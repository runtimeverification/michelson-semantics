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

```k
    configuration <k> init($Input) </k>
                  <out stream="stdout"> .List </out>
                  <defnDir> $DefnDir:String </defnDir>
                  <exitcode exit="0"> 1 </exitcode>

    rule  <k> .K </k>
          <exitcode> 1 => 0 </exitcode>

    syntax KItem ::= init(filename: String)
    rule  <k> init(ProgramFile)
           => koreExec(initialConfiguration(ProgramFile, michelsonDefinition() +String "/michelson-kompiled/"))
              ...
          </k>
 
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
```

```k
endmodule
```
