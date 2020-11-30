```driver
requires "substitution.md"

module KORE
    imports STRING-SYNTAX
    imports KVAR-SYNTAX

    syntax KVar ::= r"[A-Za-z'-][A-Za-z'0-9-]*" [token]
    syntax Sort ::= KVar "{" "}"
    syntax Symbol ::= KVar "{" Sorts "}"
    syntax Pattern ::= "\\dv" "{" Sort "}" "(" String ")"                           [klabel(\dv)]
                     | KVar ":" Sort                                                [klabel(variable)]
                     | Symbol "(" Patterns ")"                                      [klabel(application)]
                     | "\\not" "{" Sort "}" "(" Pattern ")"                         [klabel(\not)]
                     | "inj" "{" Sort "," Sort "}" "(" Pattern ")"                  [klabel(inj)]
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

```driver
module KORE-UNPARSE
    imports KORE
    imports STRING

    syntax String ::= unparsePattern(Pattern) [function, functional]
    rule unparsePattern(\equals { S1 , S2 } (P1, P2)) => "\\equals{" +String unparseSort(S1) +String "," +String unparseSort(S2)  +String "} (" +String unparsePattern(P1) +String " , " +String unparsePattern(P2) +String ")"
    rule unparsePattern(KVar : Sort)                  => NameToString(KVar) +String ":" +String unparseSort(Sort)
    rule unparsePattern(\dv { S } (Value))            => "\\dv{" +String unparseSort(S)  +String "} (\"" +String Value +String "\")"
    rule unparsePattern(\top { S } ())                => "\\top{" +String unparseSort(S)  +String "} ()"
    rule unparsePattern(\bottom { S } ())                => "\\bottom{" +String unparseSort(S)  +String "} ()"
    rule unparsePattern(inj { S1 , S2 } (P1))         => "inj{" +String unparseSort(S1) +String "," +String unparseSort(S2)  +String "} (" +String unparsePattern(P1) +String ")"
    rule unparsePattern(\not { S1 } (P1))         => "\\not{" +String unparseSort(S1) +String "} (" +String unparsePattern(P1) +String ")"
    rule unparsePattern(S(Args:Patterns))             => unparseSymbol(S) +String "(" +String unparsePatterns(Args) +String ")"
    rule unparsePattern(\and { S1 } (P1, P2))
      => "\\and{" +String unparseSort(S1) +String "} (" +String unparsePatterns(P1) +String "," +String unparsePatterns(P2) +String  ")"
    rule unparsePattern(\or { S1 } (P1, P2))
      => "\\or{" +String unparseSort(S1) +String "} (" +String unparsePatterns(P1) +String "," +String unparsePatterns(P2) +String  ")"
    rule unparsePattern(\forall  { S1 } (P1, P2)) => "\\forall {" +String unparseSort(S1) +String "} (" +String unparsePattern(P1) +String " , " +String unparsePattern(P2) +String ")"
    rule unparsePattern(\exists  { S1 } (P1, P2)) => "\\exists {" +String unparseSort(S1) +String "} (" +String unparsePattern(P1) +String " , " +String unparsePattern(P2) +String ")"

    syntax String ::= NameToString(KVar) [function, functional, hook(STRING.token2string)]

    syntax String ::= unparseSort(Sort) [function, functional]
    rule unparseSort(KVar {}) => NameToString(KVar) +String "{}"

    syntax String ::= unparseSymbol(Symbol) [function, functional]
    rule unparseSymbol(KVar {Sorts}) => NameToString(KVar) +String "{" +String unparseSorts(Sorts) +String "}"

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

The following module implements IO, calls to system, and other the nitty-gritty details.

```driver
module DRIVER-HELPERS
    imports KORE
    imports KORE-UNPARSE
    imports K-IO
    imports K-REFLECTION
    imports LIST
    imports SUBSTITUTION
```

```driver
    syntax KItem ::= "init"
    configuration <k> $PGM:Pattern ~> init </k>
                  <freshVars> .K </freshVars>
                  <out stream="stdout"> .List </out>
                  <definition> $Definition:String </definition>
                  <workingDir> $WorkingDir:String </workingDir>
                  <exitcode exit="0"> 1 </exitcode>
    rule  <k> .K </k>
          <exitcode> 1 => 0 </exitcode>
```

```driver
    syntax KItem ::= triage(kcell: Patterns, config: Pattern)
```

To execute a configuration, we:

1. unparse it to a string,
2. write that it to a temporary file,
3. execute it using the `kore-exec-helper` script,
4. and unparse the kore output into a K term.

```driver
    syntax KItem ::= exec(Pattern)

    rule <k> exec(\and{_}(Pgm, \top{_}())) => exec(Pgm) ... </k>
    rule <k> exec(Config)
          => write-to-file(unparsePattern(Config), #open(WD +String "/" +String Int2String(!I) +String ".input", "w"))
          ~> kore-exec(path:                             WD +String "/" +String Int2String(!I)                       )
          ~> parseKORE
             ...
         </k>
         <workingDir> WD </workingDir>
      [owise]

    syntax KItem ::= "write-to-file" "(" contents: String "," fd: IOInt ")"
    rule <k> write-to-file(Str, Fd) => #write(Fd, Str) ~> close(Fd) ... </k>

    syntax KItem ::= "kore-exec" "(" "path:" String ")" [seqstrict, result(String)]
    rule <k> kore-exec(path: Path)
          => #system("./driver/kore-exec-helper " +String Definition +String " " +String Path)
             ...
         </k>
         <definition> Definition </definition>
    rule <k> #systemResult(0, StdOut, _) => StdOut ... </k>

    syntax KItem ::= "parseKORE"
    rule <k> S:String ~> parseKORE
          => #parseKORE(S):Pattern
             ...
         </k>

    syntax KItem ::= close(Int)
    rule <k> close(Fd) => #close(Fd) ... </k>
```

We use these tokens in the definition.

```driver
    syntax KVar ::= "SortK"                       [token]
                  | "SortKItem"                   [token]
                  | "SortValueExpr"               [token]
                  | "VarResult"                   [token]
                  | "kseq"                        [token]
                  | "dotk"                        [token]
                  | "SortGeneratedTopCell"        [token]
                  | "Lbl'-LT-'generatedTop'-GT-'" [token]
                  | "SortString"                  [token]
                  | "Lbl'Hash'failure"            [token]
                  | "Lblforallbinder"             [token]
                  | "Lblforallbinderheated"       [token]
                  | "Lblforallbindercooled"       [token]
                  | "SortExpr"                    [token]
                  | "SortBool"                    [token]
                  | "SortInt"                     [token]
                  | "Lblimplies"                  [token]
                  | "Lbland"                      [token]
                  | "Lblor"                       [token]
                  | "Lblnot"                      [token]
                  | "LbleqInt"                    [token]
                  | "LbleqBool"                   [token]
                  | "Lbl'UndsEqlsEqls'Int'Unds'"  [token]
                  | "Lbl'UndsEqlsEqls'K'Unds'"    [token]
                  | "Lbl'UndsEqlsEqls'Bool'Unds'" [token]
                  | "Lbl'-LT-'k'-GT-'"            [token]
```

```driver
    syntax Patterns ::= getKCell(Pattern) [function]
    rule getKCell(Lbl'-LT-'k'-GT-' { .Sorts } ( Arg, .Patterns ) ) => Arg, .Patterns
    rule getKCell(S { _ } ( Args ) ) => getKCellPs(Args) requires S =/=K Lbl'-LT-'k'-GT-'
    rule getKCell(inj{ _, _ } (P) ) => getKCell(P)
    rule getKCell(\not{ _ } (P) ) => getKCell(P)
    rule getKCell(\dv{ _ } (_) ) => .Patterns
    rule getKCell(_ : _) => .Patterns

    syntax Patterns ::= getKCellPs(Patterns) [function, functional]
    rule getKCellPs(P, Ps) => getKCell(P) +Patterns getKCellPs(Ps)
    rule getKCellPs(.Patterns) => .Patterns

    syntax KVar ::= "Lbl'-LT-'freshVars'-GT-'" [token]
    syntax Patterns ::= getFreshVars(Pattern) [function]
    rule getFreshVars(Lbl'-LT-'freshVars'-GT-' { .Sorts } ( Arg, .Patterns ) ) => Arg, .Patterns
    rule getFreshVars(S { _ } ( Args ) ) => getFreshVarsPs(Args) requires S =/=K Lbl'-LT-'freshVars'-GT-'
    rule getFreshVars(inj{ _, _ } (P) ) => getFreshVars(P)
    rule getFreshVars(\not{ _ } (P) ) => getFreshVars(P)
    rule getFreshVars(\and{ _ } (P1, P2) ) => getFreshVars(P1) +Patterns getFreshVars(P2)
    rule getFreshVars(\equals{ _ , _} (_, _) ) => .Patterns
    rule getFreshVars(\forall{ _ } (_, _) ) => .Patterns
    rule getFreshVars(\exists{ _ } (_, _) ) => .Patterns
    rule getFreshVars(\dv{ _ } (_) ) => .Patterns
    rule getFreshVars(\top{ _ } () ) => .Patterns
    rule getFreshVars(_ : _) => .Patterns

    syntax Patterns ::= getFreshVarsPs(Patterns) [function, functional]
    rule getFreshVarsPs(P, Ps) => getFreshVars(P) +Patterns getFreshVarsPs(Ps)
    rule getFreshVarsPs(.Patterns) => .Patterns
```

```driver
    syntax Pattern ::= setKCell(config: Pattern, kcell: Pattern) [function]
    rule setKCell(Lbl'-LT-'k'-GT-' { .Sorts } ( _, .Patterns ), KCell ) => Lbl'-LT-'k'-GT-' { .Sorts } ( KCell, .Patterns )
    rule setKCell(S { Sorts } ( Args )                 , KCell ) => S { Sorts } ( setKCellPs(Args, KCell) ) requires S =/=K Lbl'-LT-'k'-GT-'
    rule setKCell(\and { S } ( P1, P2 )                , KCell ) => \and { S } ( setKCell(P1, KCell), setKCell(P2, KCell))
    rule setKCell(\equals { S1, S2 } ( P1, P2 )        , KCell ) => \equals { S1, S2 } ( setKCell(P1, KCell), setKCell(P2, KCell))
    rule setKCell(\forall { S1 } ( P1, P2     )        , KCell ) => \forall { S1 } ( P1, setKCell(P2, KCell))
    rule setKCell(\exists { S1 } ( P1, P2     )        , KCell ) => \exists { S1 } ( P1, setKCell(P2, KCell))
    rule setKCell(inj{ S1, S2 } (P)                    , KCell ) => inj { S1, S2 } ( setKCell(P, KCell) )
    rule setKCell(\not{ S1 } (P)                       , KCell ) => \not{ S1 } ( setKCell(P, KCell) )
    rule setKCell(\top{ S1 } ()                        ,_KCell ) => \top{ S1 } ( )
    rule setKCell(\dv{ S } (P)                         ,_KCell ) => \dv{ S } (P)
    rule setKCell(S : Sort                             ,_KCell ) => S : Sort

    syntax Patterns ::= setKCellPs(config: Patterns, kcell: Pattern) [function]
    rule setKCellPs((P, Ps), KCell) => setKCell(P, KCell), setKCellPs(Ps, KCell)
    rule setKCellPs(.Patterns, _) => .Patterns
```

We use fresh variables from a domain distinct from both the Haskell backend's names, and `KVar`'s.

```driver
    syntax KVar ::= freshVariable(Int) [function]
    rule freshVariable(I) => String2KVar("VDriver" +String Int2String(I))
```

```driver
    syntax Patterns ::= Patterns "+Patterns" Patterns [function, functional, left]
    rule (P1, P1s) +Patterns P2s => P1, (P1s +Patterns P2s)
    rule .Patterns +Patterns P2s =>                    P2s
```

```driver
endmodule
```

```driver
module DRIVER
    imports DRIVER-HELPERS
```

Normalization
=============

The result of `kore-exec --search` and `krun` are of the form:

```
    {       sideconditions
    #And    Result == Configuration
    }
```

whereas, `kore-exec` accepts initial configurations for the form:

```
    {       sideconditions
    #And    Configuration
    }
```

First, we bring the configuration to the front of the conjunction:

```driver
    rule \and { S }(P, \and { S } (Lbl'-LT-'generatedTop'-GT-' { .Sorts } (_) #as Config, Ps)) => \and { S }(Config, \and { S } (P, Ps)) [anywhere]
    rule \and { S }(P, (Lbl'-LT-'generatedTop'-GT-' { .Sorts } (_) #as Config)) => \and { S }(Config, P)                                 [anywhere]
```

Next, we convert the substitution like predicate `Result == Configuration` into a constrained term:

```driver
    rule \equals { SortK { } , SortKItem { } } ( VarResult : SortK { }
                                               , kseq { .Sorts } ( inj { SortGeneratedTopCell { } , SortKItem { } }(Result)
                                                          , dotk { .Sorts } ( .Patterns )
                                                          )
                                               )
      => Result
    rule \equals { _ , _ } ( VarResult : SortGeneratedTopCell { } , Result ) => Result [anywhere]
    rule (P, (Lbl'-LT-'generatedTop'-GT-' { .Sorts } ( _ ) #as Config), Ps)
      => Config, P, Ps [anywhere]

    rule <k> Lbl'-LT-'generatedTop'-GT-' { .Sorts } ( _ ) #as Pgm => \and { SortGeneratedTopCell { } } (Pgm, \top {SortGeneratedTopCell { }}()) ... </k>
    rule <k> T:KItem
          ~> Lbl'-LT-'generatedTop'-GT-' { .Sorts } ( _ ) #as Pgm
          => ( T
            ~> \and { SortGeneratedTopCell { } } (Pgm, \top {SortGeneratedTopCell { }}())
             )
            ...
         </k>
```

Search
======

We perform a depth first search over branches:

```driver
    rule <k> \or { SortGeneratedTopCell { } }(P1, P2) => P1 ~> P2 ... </k>
```

## Triaging

For each constrained configuration, we triage according to the content of the `<k>` cell:

```driver
    rule <k> \and { SortGeneratedTopCell { } }(Lbl'-LT-'generatedTop'-GT-' { .Sorts } (_) #as Config, _Constraints) #as ConstrainedConfiguration
          => triage(getKCell(Config), ConstrainedConfiguration)
             ...
         </k>
    rule <k> \bottom{_}() => .K ... </k> // TODO: This is broken when the only result from a forall is `\bottom`
```

```driver
    rule <k> triage(_, Pgm)  ~> init => exec(Pgm) ... </k>
```

### Failure

```driver
    rule <k> triage(kseq{ .Sorts } ( Lbl'Hash'failure { .Sorts } ( \dv { SortString { } } ( Message ) ), _) , Pgm) => .K ... </k>
         <out> ... .List
            => ListItem("==== failure\n")
               ListItem(Message)             ListItem("\n")
               ListItem(unparsePattern(Pgm)) ListItem("\n")
               ListItem("\n")
         </out>
```

### Succeeded:

```driver
    rule <k> triage(dotk {.Sorts} (.Patterns), Pgm) => .K ... </k>
         <out> ... .List
            => ListItem("==== success\n")
               ListItem(unparsePattern(Pgm))
               ListItem("\n")
         </out>
```

```driver
endmodule
```
