```k
requires "substitution.md"

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

```k
module KORE-UNPARSE
    imports KORE
    imports STRING

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

Plumbing
========

The following module implements (non-functional) IO, calls to system, and other
the nitty-gritty details.

```k
module CELL-IO
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
    rule read(File) => read(open(File, "r"), 99999)
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

```k
module META-K
    imports CELL-IO
    imports KORE
    imports K-REFLECTION

    syntax PreString ::= parse(input: PreString, parser: String)
                       | parseFile(filename: PreString, parser: String) [seqstrict(1), result(String)]
    rule parse(Program, Parser) => parseFile(writeTempFile(Program), Parser)
    rule parseFile(File, Parser) => system(Parser +String " " +String File)

    syntax PrePattern ::= parseKore(PreString) [seqstrict(1), result(String)]
    rule parseKore(String) => #parseKORE(String):Pattern

    syntax PrePattern ::= Pattern

endmodule
```

```k
module DRIVER
    imports KORE-UNPARSE
    imports META-K
    imports LIST
```

Generic helpers

```k
    syntax Patterns ::= Patterns "+Patterns" Patterns [function, functional, left]
    rule (P1, P1s) +Patterns P2s => P1, (P1s +Patterns P2s)
    rule .Patterns +Patterns P2s =>                    P2s 

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

    syntax  KoreName ::= "Lbl'-LT-'k'-GT-'" [token]
    syntax Patterns ::= getKCell(Pattern) [function]
    rule getKCell(Term) => findSubTermsByConstructor(Lbl'-LT-'k'-GT-', Term)

    syntax  KoreName ::= "Lbl'-LT-'returncode'-GT-'" [token]
    syntax Patterns ::= getReturncodeCell(Pattern) [function]
    rule getReturncodeCell(Term) => findSubTermsByConstructor(Lbl'-LT-'returncode'-GT-', Term)
```

```k
    configuration <k> init($Input) </k>
                  <out stream="stdout"> .List </out>
                  <defnDir> $DefnDir:String </defnDir>
                  <success>  0 </success>
                  <failures> 0 </failures>
                  <exitcode exit="0"> 2 </exitcode>

    rule  <k> .K </k>
          <out> ... .List
             => ListItem(Int2String(Successes)) ListItem(" branch(es) succeeded; ")
                ListItem(Int2String(Failures))  ListItem(" branch(es) failed.\n")
          </out>
          <success> Successes </success>
          <failures> Failures </failures>
          <exitcode> 2 => #if Failures ==Int 0 #then 0 #else 1 #fi </exitcode>

    syntax PrePattern ::= initialConfiguration(filename: PreString, definition: String) [seqstrict(1), result(String)]
    // TODO: The parser here isn't generated
    rule initialConfiguration(Filename, Definition)
      => parseKore( parse( system( "llvm-krun --dry-run --directory " +String Definition
                                   +String " -c PGM " +String Filename +String " Pgm prettyfile") 
                         , koreDefinition() +String "/../../../parser_PGM"
                         )
                  )

    syntax PrePattern ::= koreExec(config: PrePattern) [seqstrict(1), result(Pattern)]
                        | koreExec(file:   PreString)  [seqstrict(1), result(String)]

    rule koreExec(Configuration) => koreExec(writeTempFile(unparsePattern(Configuration)))
    rule koreExec(File)
      => parseKore( parse( system("kore-exec " +String michelsonDefinition() +String "/michelson-kompiled/definition.kore" +String
                                      " --strategy all" +String
                                      " --module MICHELSON" +String
                                      " --pattern " +String File
                                 )
                         , koreDefinition() +String "/../../../parser_PGM"
                  )      )

    syntax String ::= michelsonDefinition() [function, functional]
    rule [[ michelsonDefinition() => DefnDir +String "/symbolic/" ]]
         <defnDir> DefnDir </defnDir>

    syntax String ::= koreDefinition() [function, functional]
    rule [[ koreDefinition() => DefnDir +String "/driver/" ]]
         <defnDir> DefnDir </defnDir>

    syntax KItem ::= init(filename: String)
    rule  <k> init(ProgramFile)
           => koreExec(initialConfiguration(ProgramFile, michelsonDefinition() +String "/michelson-kompiled/"))
              ...
          </k>

    syntax KoreName ::= "SortGeneratedTopCell" [token]
    rule <k> \or { SortGeneratedTopCell { } } (P1, P2) => P1 ~> P2 ... </k>

    rule \and { S }(\and { S } (P1, P2), P3)
      => \and { S }(P1, \and { S } (P2, P3)) [anywhere]

    rule <k> Lbl'-LT-'generatedTop'-GT-' { .Sorts } ( _ ) #as Pgm => \and { SortGeneratedTopCell { } } (Pgm, \top {SortGeneratedTopCell { }}()) ... </k>

    syntax KoreName ::= "Lbl'-LT-'generatedTop'-GT-'" [token]
    syntax KItem ::= triage(kcell: Patterns, returncode: Patterns, config: Pattern)
    rule <k> \and { SortGeneratedTopCell { } }(Lbl'-LT-'generatedTop'-GT-' { .Sorts } (_) #as Config, _Constraints) #as ConstrainedConfiguration
          => triage(getKCell(Config), getReturncodeCell(Config), ConstrainedConfiguration)
             ...
         </k>

    syntax KoreName ::= "SortInt" [token]
    rule <k> triage( _, \dv { SortInt { } } ( "0" ), _) => .K ... </k>
         <success> N => N +Int 1 </success>

    syntax KoreName ::= "Lbl'Hash'AssertFailed" [token]
                      | "kseq" [token]
    rule <k> triage( kseq { .Sorts } (Lbl'Hash'AssertFailed {.Sorts } (.Patterns), _ ) , _, _) => .K ... </k>
         <failures> N => N +Int 1 </failures>
```

```k
endmodule
```
