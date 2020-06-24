```k
requires "unit-test/unit-test.md"
requires "michelson/types.md"
requires "symbolic/configuration.md"
```

```k
module SYMBOLIC-UNIT-TEST-DRIVER
  imports SYMBOLIC-UNIT-TEST-SYNTAX
  imports MICHELSON-TYPES
  imports SYMBOLIC-CONFIGURATION
  imports COLLECTIONS
  imports UNIT-TEST
  imports COLLECTIONS

  rule <k> #Init
        => #CreateSymbols
        ~> #BaseInit
        ~> #ExecutePreConditions
        ~> #LoadInputStack
        ~> #ExecuteScript
        ~> #CheckSymbolicOutput
        ~> #ExecutePostConditions
           ...
       </k>
```

```k
  rule [[ #MichelineToNative(S:SymbolicData, T, _, _) => D ]]
       <symbols> S |-> #TypedSymbol(T, D) ... </symbols>

  rule [[ #MichelineToNative(S:SymbolicData, T, _, _) => S ]]
       <symbols> Syms:Map </symbols>
    requires notBool (S in_keys(Syms))

  rule [[ #TypeData(_, S:SymbolicData, T) => #Typed(S, T) ]]
       <symbols> ... S |-> #TypedSymbol(T, _) ... </symbols>

   // Complex types..._
```

`#LiteralStackToTypesAux` Extension
-----------------------------------

We extend this typing function to handle symbolic values.

```k
  rule #LiteralStackToTypesAux(Stack_elt T S:SymbolicData ; Gs:StackElementList, PT)
    => T ; #LiteralStackToTypesAux(Gs, PT)

  rule #LiteralStackToTypesAux(Stack_elt T S:SymbolicData, PT) => T
```

`#RestoreStack` utility function
--------------------------------

```k
  syntax KItem ::= "#RestoreStack" "(" K ")"
  rule <k> #RestoreStack(Stack) => .K ... </k>
       <stack> _ => Stack </stack>
```

`invariants` group
---------------------

TODO: This is not excersized by any tests.

```k
  syntax KItem ::= #LoadInvariants(Invariants) | #LoadInvariant(Invariant)
  rule <k> invariants { } => .K ... </k>
  rule <k> invariants { I1 ; Is }
        => invariants( { I1 } ) ~> #LoadInvariants({ Is })
           ...
       </k>
  rule <k> invariants ({ I }) => .K ... </k>
  rule <k> invariants ({ Annotation Blocks }) => . ... </k>
       <invs> M => M[Annotation <- Blocks] </invs>
```

```k
  rule #Ceil(#DoCompare(@A:Int, @B:Int)) => #Ceil(@A) #And #Ceil(@B)  [anywhere, simplification]
  rule #DoCompare(I1:Int, I2:Int) <Int 0 => I1 <Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) <=Int 0 => I1 <=Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) ==Int 0 => I1 ==Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) >=Int 0 => I1 >=Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) >Int 0 => I1 >Int I2 [simplification]
```

```k
endmodule
```
