In this file, we implement a utility that replaces `krun` for symbolic Michelson
tests. We use the low-level K plumbing such as `kore-exec` and `llvm-run`
directly.

Rule based IO
=============

The following module implements a wrapper around `K-IO`.
It uses strictness to allow convenient sequential composition of variable IO functions implemented there.

```k
module IO-NONFUNCTIONAL
    imports K-IO
    imports STRING
    imports INT
```

```k
    syntax KResult // Unused: TODO: Bug report
    syntax PreString ::= String
    syntax PreInt    ::= Int
```

### `open`

```k
    syntax PreInt ::= open(filename: PreString, mode: String) [seqstrict, result(String)]
    rule open(File, Mode) => #open(File, Mode)
```

### `close`

Returns `.K` on success

```k
    syntax KItem ::= close(Int)
    rule close(Fd) => #close(Fd)
```

### `read`

```k
    syntax PreString ::= read(filename: PreString)
                       | read(fd: PreInt, length: Int) [seqstrict, result(Int)]
    rule read(File) => read(open(File, "r"), 99999) // TODO: read until end of file instead of hard-coding a number
    rule read(Fd:Int, Length) => #read(Fd, Length)
```

### `write`

Returns `.K` or fails with `IOError`.

```k
    syntax KItem ::= write(fd: IOInt, contents: String)
    rule write(Fd, Content) => #write(Fd, Content)
```

### `createTempFile`

Returns `#tempFile(fileName, Fd)`

```k
    syntax PreTempFile ::= IOFile
    syntax PreTempFile ::= createTempFile(template: String)
    rule createTempFile(Template) => #mkstemp(Template)
```

### `writeTempFile`

Creates a temp file, writes contents to it, and returns the filename.

```k
    syntax PreString ::= writeTempFile(contents: PreString) [seqstrict(1), result(String)]
                       | writeTempFile(PreTempFile, contents: String) [seqstrict(1), result(IOFile)]
    rule writeTempFile(Contents) => writeTempFile(createTempFile("/tmp/kmichelson-XXXXXX"), Contents)
    rule writeTempFile(#tempFile(Filename, Fd), Content)
      => write(Fd, Content) ~> close(Fd) ~> Filename
```

### `system`

```k
    syntax PreString ::= system(command: String)
    rule system(Command) => #system(Command)
```

TODO: We'd like `#systemResult` to be of a more specific sort so we could apply
strictness instead of the following hack:

```k
    rule #systemResult(0,   StdOut, _) => StdOut
    rule #systemResult(111, StdOut, _) => StdOut // krun failures indicate that exit code is non-zero
```

```k
endmodule
```


`KORE`
======

This module defines the syntax of kore, a language used for communication
between the various K utilities.

```k
module KORE
    imports STRING-SYNTAX

    syntax KoreName ::= r"[A-Za-z'-][A-Za-z'0-9-]*" [token]
    syntax Sort ::= KoreName "{" "}"
    syntax Symbol ::= KoreName "{" Sorts "}"
    syntax Pattern ::= "\\dv" "{" Sort "}" "(" String ")"                           [klabel(\dv)]
                     | KoreName ":" Sort                                            [klabel(variable)]
                     | Symbol "(" Patterns ")"                                      [klabel(application)]
                     | "\\not" "{" Sort "}" "(" Pattern ")"                         [klabel(\not)]
                     | "inj" "{" Sort "," Sort "}" "(" Pattern ")"                  [klabel(inj)]
                     | "\\ceil" "{" Sort "," Sort "}" "(" Pattern  ")"              [klabel(\ceil)]
                     | "\\equals" "{" Sort "," Sort "}" "(" Pattern "," Pattern ")" [klabel(\equals)]
                     | "\\and" "{" Sort "}" "(" Pattern "," Pattern ")"             [klabel(\and)]
                     | "\\or" "{" Sort "}" "(" Pattern "," Pattern ")"              [klabel(\or)]
                     | "\\top" "{" Sort "}" "(" ")"                                 [klabel(\top)]
                     | "\\bottom" "{" Sort "}" "(" ")"                              [klabel(\bottom)]
                     | "\\forall" "{" Sort "}" "(" Pattern "," Pattern ")"          [klabel(\forall)]
                     | "\\exists" "{" Sort "}" "(" Pattern "," Pattern ")"          [klabel(\exists)]

    syntax Patterns ::= List{Pattern, ","} [klabel(Patterns)]
    syntax Sorts ::= List{Sort, ","}       [klabel(Sorts)]
endmodule
```

`KORE-UNPARSE`
==============

```k
module KORE-UNPARSE
    imports KORE
    imports STRING
    imports BOOL
    imports K-EQUAL

    syntax String ::= unparsePattern(Pattern) [function, functional]
    rule unparsePattern(\equals { S1 , S2 } (P1, P2)) => "\\equals{" +String unparseSort(S1) +String "," +String unparseSort(S2)  +String "} (" +String unparsePattern(P1) +String " , " +String unparsePattern(P2) +String ")"
    rule unparsePattern(Var : Sort)                   => KoreNameToString(Var) +String ":" +String unparseSort(Sort)
    rule unparsePattern(\dv { S } (Value))            => "\\dv{" +String unparseSort(S)  +String "} (\"" +String Value +String "\")"
    rule unparsePattern(\top { S } ())                => "\\top{" +String unparseSort(S)  +String "} ()"
    rule unparsePattern(\bottom { S } ())             => "\\bottom{" +String unparseSort(S)  +String "} ()"
    rule unparsePattern(inj { S1 , S2 } (P1))         => "inj{" +String unparseSort(S1) +String "," +String unparseSort(S2)  +String "} (" +String unparsePattern(P1) +String ")"
    rule unparsePattern(\ceil { S1 , S2 } (P1))       => "\\ceil{" +String unparseSort(S1) +String "," +String unparseSort(S2)  +String "} (" +String unparsePattern(P1) +String ")"
    rule unparsePattern(\not { S1 } (P1))             => "\\not{" +String unparseSort(S1) +String "} (" +String unparsePattern(P1) +String ")"
    rule unparsePattern(S(Args:Patterns))             => unparseSymbol(S) +String "(" +String unparsePatterns(Args) +String ")"
    rule unparsePattern(\and { S1 } (P1, P2))
      => "\\and{" +String unparseSort(S1) +String "} (" +String unparsePatterns(P1) +String "," +String unparsePatterns(P2) +String  ")"
    rule unparsePattern(\or { S1 } (P1, P2))
      => "\\or{" +String unparseSort(S1) +String "} (" +String unparsePatterns(P1) +String "," +String unparsePatterns(P2) +String  ")"
    rule unparsePattern(\forall  { S1 } (P1, P2)) => "\\forall {" +String unparseSort(S1) +String "} (" +String unparsePattern(P1) +String " , " +String unparsePattern(P2) +String ")"
    rule unparsePattern(\exists  { S1 } (P1, P2)) => "\\exists {" +String unparseSort(S1) +String "} (" +String unparsePattern(P1) +String " , " +String unparsePattern(P2) +String ")"

    syntax String ::= KoreNameToString(KoreName) [function, functional, hook(STRING.token2string)]

    syntax String ::= unparseSort(Sort) [function, functional]
    rule unparseSort(KoreName {}) => KoreNameToString(KoreName) +String "{}"

    syntax String ::= unparseSymbol(Symbol) [function, functional]
    rule unparseSymbol(KoreName {Sorts}) => KoreNameToString(KoreName) +String "{" +String unparseSorts(Sorts) +String "}"

    syntax String ::= unparsePatterns(Patterns) [function, functional]
    rule unparsePatterns(P, Ps) => unparsePattern(P) +String "," +String unparsePatterns(Ps) requires notBool Ps ==K .Patterns
    rule unparsePatterns(P, .Patterns) => unparsePattern(P)
    rule unparsePatterns(.Patterns) => ""

    syntax String ::= unparseSorts(Sorts) [function, functional]
    rule unparseSorts(S, Ss) => unparseSort(S) +String "," +String unparseSorts(Ss) requires notBool Ss ==K .Sorts
    rule unparseSorts(S, .Sorts) => unparseSort(S)
    rule unparseSorts(.Sorts) => ""
endmodule
```

`KORE-PARSE`
============

```k
module KORE-PARSE
    imports IO-NONFUNCTIONAL
    imports KORE
    imports K-REFLECTION

    syntax PrePattern ::= Pattern

    syntax PrePattern ::= parse(input: PreString, parser: String)
                        | parseFile(filename: PreString, parser: String) [seqstrict(1), result(String)]
    rule parse(Program, Parser) => parseFile(writeTempFile(Program), Parser)
    rule parseFile(File, Parser) => parseKore(system(Parser +String " " +String File))

    syntax PrePattern ::= parseKore(PreString) [seqstrict(1), result(String)]
    rule parseKore(String) => #parseKORE(String):Pattern

endmodule
```

`KORE-HELPERS`
==============

Various generic library functions over kore.

```k
module KORE-UTILITIES
    imports KORE
    imports K-EQUAL

    syntax Patterns ::= Patterns "+Patterns" Patterns [function, functional, left]
    rule (P1, P1s) +Patterns P2s => P1, (P1s +Patterns P2s)
    rule .Patterns +Patterns P2s =>                    P2s

    // TODO: consider adding cases for other ML operators
    syntax Patterns ::= findSubTermsByConstructor(KoreName, Pattern) [function]
    rule findSubTermsByConstructor(Ctor, Ctor { .Sorts } ( Arg, .Patterns ) ) => Arg, .Patterns
    rule findSubTermsByConstructor(Ctor, S { _ } ( Args ) ) => findSubTermsByConstructorPs(Ctor, Args) requires S =/=K Ctor
    rule findSubTermsByConstructor(Ctor, inj{ _, _ } (P) ) => findSubTermsByConstructor(Ctor, P)
    rule findSubTermsByConstructor(Ctor, \not{ _ } (P) ) => findSubTermsByConstructor(Ctor, P)
    rule findSubTermsByConstructor(  _ , \dv{ _ } (_) ) => .Patterns
    rule findSubTermsByConstructor(  _ , _ : _) => .Patterns

    syntax Patterns ::= findSubTermsByConstructorPs(KoreName, Patterns) [function, functional]
    rule findSubTermsByConstructorPs(Ctor, P, Ps) => findSubTermsByConstructor(Ctor, P) +Patterns findSubTermsByConstructorPs(Ctor, Ps)
    rule findSubTermsByConstructorPs(   _, .Patterns) => .Patterns
endmodule
```

`DRIVER`
========

```k
module DRIVER
    imports KORE-PARSE
    imports KORE-UNPARSE
    imports KORE-UTILITIES
    imports LIST
```

`kore-exec`
-----------

```k
    syntax PrePattern ::= koreExec(config: PrePattern) [seqstrict(1), result(Pattern)]
                        | koreExec(file:   PreString)  [seqstrict(1), result(String)]
    rule koreExec(Configuration) => koreExec(writeTempFile(unparsePattern(Configuration)))
    rule koreExec(File)
      => parse( system("kore-exec " +String michelsonDefinition() +String "/michelson-kompiled/definition.kore" +String
                           " --strategy all" +String
                           " --module MICHELSON" +String
                           " --pattern " +String File
                      )
              )
```

Pretty print
------------

```k
    syntax KItem ::= prettyPrint(config: PrePattern) [seqstrict(1), result(Pattern)]
                   | prettyPrint(file:   PreString)  [seqstrict(1), result(String)]
    rule prettyPrint(Configuration)
      => prettyPrint(writeTempFile(unparsePattern(Configuration)))
    rule prettyPrint(File)
      => print(system("kprint " +String michelsonDefinition() +String "/michelson-kompiled/ " +String
                    " " +String File +String
                    " true true"
               ))
```

```k
    syntax KItem ::= print(PreString) [seqstrict(1), result(String)]
    rule <k> print(Str:String) => .K ... </k>
         <out> ... .List => ListItem(Str) </out>
```

Utilities instatiated for KMichelson
------------------------------------

```k
    syntax  KoreName ::= "Lbl'-LT-'k'-GT-'" [token]
    syntax Patterns ::= getKCell(Pattern) [function]
    rule getKCell(Term) => findSubTermsByConstructor(Lbl'-LT-'k'-GT-', Term)

    syntax  KoreName ::= "Lbl'-LT-'returncode'-GT-'" [token]
    syntax Patterns ::= getReturncodeCell(Pattern) [function]
    rule getReturncodeCell(Term) => findSubTermsByConstructor(Lbl'-LT-'returncode'-GT-', Term)

    syntax PrePattern ::= parse(input: PreString)
    rule parse(Input) =>  parse(Input, driverDefiniton() +String "/driver-kompiled/parser_Pattern")

    syntax String ::= michelsonDefinition() [function, functional]
    rule [[ michelsonDefinition() => DefnDir +String "/symbolic/" ]]
         <defnDir> DefnDir </defnDir>

    syntax String ::= driverDefiniton() [function, functional]
    rule [[ driverDefiniton() => DefnDir +String "/driver/" ]]
         <defnDir> DefnDir </defnDir>
```


Main
----

We initialize the configuration with the input filename.

```k
    configuration <k> init($InputFilename) </k>
                  <success>  0 </success>               // Number of successful branches
                  <failures> 0 </failures>              // Number of failing    branches
                  <stucks>   0 </stucks>                // Number of stuck      branches
                  <defnDir> $DefnDir:String </defnDir>  // Path to kompiled definitions
                  <out stream="stdout"> .List </out>
                  <exitcode exit="0"> -1 </exitcode>
```

### Initialization

We then parse the program syntax into kore using `llvm-krun`, and then parse the kore string into a `Pattern` we then exuecute this pattern with `kore-exec`.

```k
    syntax KItem ::= init(filename: String)
    rule  <k> init(ProgramFile)
           => koreExec(initialConfiguration(ProgramFile, michelsonDefinition() +String "/michelson-kompiled/"))
              ...
          </k>

    syntax PrePattern ::= initialConfiguration(filename: PreString, definition: String) [seqstrict(1), result(String)]
    rule initialConfiguration(Filename, Definition)
      => parse( system( "llvm-krun --dry-run --directory " +String Definition
                        +String " -c PGM " +String Filename +String " Pgm prettyfile")
              )
```

### Normalization

The resulting configuration is a disjunction of constrained configurations. We handle each of these in turn.

```k
    rule <k> \or { SortGeneratedTopCell { } } (P1, P2) => P1 ~> P2 ... </k>
```

We normalize this configuration so that it is in the form of a constrained term.

```k
    syntax KoreName ::= "SortGeneratedTopCell" [token]
    rule <k> Lbl'-LT-'generatedTop'-GT-' { .Sorts } ( _ ) #as Pgm => \and { SortGeneratedTopCell { } } (Pgm, \top {SortGeneratedTopCell { }}()) ... </k>
```

We also apply associativity laws for `\and`.

```k
    rule \and { S }(\and { S } (P1, P2), P3) => \and { S }(P1, \and { S } (P2, P3)) [anywhere]
```

### Triaging

We then triage each branch according to the contents of its `<k>` and `<returncode>` cell.

```k
    syntax KoreName ::= "Lbl'-LT-'generatedTop'-GT-'" [token]
    syntax KItem ::= triage(kcell: Patterns, returncode: Patterns, config: Pattern)
    rule <k> \and { SortGeneratedTopCell { } }(Lbl'-LT-'generatedTop'-GT-' { .Sorts } (_) #as Config, _Constraints) #as ConstrainedConfiguration
          => triage(getKCell(Config), getReturncodeCell(Config), ConstrainedConfiguration)
             ...
         </k>
```

```k
    syntax KoreName ::= "SortInt" [token]
    rule <k> triage( _, \dv { SortInt { } } ( "0" ), _) => .K ... </k>
         <success> N => N +Int 1 </success>

    syntax KoreName ::= "Lbl'Hash'AssertFailed" [token] | "kseq" [token]
    rule <k> triage( kseq { .Sorts } (Lbl'Hash'AssertFailed {.Sorts } (.Patterns), _ ) , _, Config)
          => print("Failure:\n") ~> prettyPrint(Config)
             ...
         </k>
         <failures> N => N +Int 1 </failures>
    syntax KoreName ::= "Lbl'Hash'AssertFailed" [token] | "kseq" [token]

    rule <k> triage( kseq { .Sorts } (KHead, _ ), \dv { SortInt { } } ( ExitCode ) , Config)
          => print("Stuck configuration:\n") ~> prettyPrint(Config)
             ...
         </k>
         <stucks> N => N +Int 1 </stucks>
      requires ExitCode =/=K "0" andBool KHead =/=K Lbl'Hash'AssertFailed {.Sorts } (.Patterns)
```

### Finalization

Finally, we print the total number of sucessful and failed branches.

```k
    rule  <k> .K </k>
          <out> ... .List
             => ListItem(Int2String(Successes)) ListItem(" branch(es) succeeded; ")
                ListItem(Int2String(Failures))  ListItem(" branch(es) failed.\n")
          </out>
          <success> Successes </success>
          <failures> Failures </failures>
          <stucks> Stucks </stucks>
          <exitcode> -1
                  =>       #if Stucks   =/=Int 0    #then 2
                     #else #if Failures =/=Int 0    #then 1
                     #else                                0
                     #fi #fi
          </exitcode>
```

```k
endmodule
```
