This file implements the unit test section of the .tzt format described by the
Tezos foundation
[here](https://gitlab.com/tezos/tezos/-/merge_requests/1487/diffs). This file
implements the behavior of the 'code,' 'input,' and 'output' applications
discussed in that document.

```k
requires "michelson/michelson.md"
requires "michelson/types.md"
requires "unit-test/syntax.md"
```

The unit-test semantics does not need any processing in addition to the base initialization.

```k
module UNIT-TEST-DRIVER
  imports UNIT-TEST
  rule <k> #Init
        => #UnitTestInit
        ~> #ExecuteTypedScript
        ~> #ConvertOutputStackToNative
        ~> #VerifyOutput
           ...
       </k>
endmodule
```

```k
module UNIT-TEST
  imports UNIT-TEST-SYNTAX
  imports MICHELSON
  imports MICHELSON-TYPES
```

```k
  syntax KItem ::= "#UnitTestInit"
  rule <k> #UnitTestInit
        => #BaseInit
        ~> #ConvertStackToNative
           ...
       </k>
```

During the final output comparison step we discard the type information retained
in lists. This allows us to compare lists which result from the 'MAP'
instruction correctly, since we do not presently determine a type for those
lists.

```k
  syntax Data ::= List
```

This function implements a relaxed equality check between two data elements. In
particular, it handles the wildcard matching behavior described in the .tzt
format proposal and discards list type information as discussed earlier.

```k
  syntax Bool ::= #Matches(Data, Data) [function] // Expected, Actual

  rule #TypeData(_, #Any, T) => #Typed(#Any, T)

  rule #Matches(#Any, _) => true

  rule #Matches(D1, D2) => D1 ==K D2 [owise]
  // This also covers any structurally different data. (e.g. (Left 1) vs (Right 1))

  rule #Matches(.List, .List) => true
  rule #Matches(ListItem(L1) Ls1:List, ListItem(L2) Ls2:List) => #Matches(L1, L2) andBool #Matches(Ls1, Ls2)

  rule #Matches(.Set, .Set) => true
  rule #Matches(SetItem(S1) Ss1, SetItem(S2) Ss2) => #Matches(S1, S2) andBool #Matches(Ss1, Ss2)

  rule #Matches(.Map, .Map) => true
  rule #Matches((K |-> V1) M1, (K |-> V2) M2) => #Matches(V1, V2) andBool #Matches(M1, M2)

  syntax Data ::= FailedStack

  rule #Matches(Create_contract(I1, C, O1, M1, D1), Create_contract(I2, C, O2, M2, D2)) =>
    #Matches(I1, I2) andBool
    #Matches(O1, O2) andBool
    #Matches(M1, M2) andBool
    #Matches(D1, D2)

  rule #Matches(Transfer_tokens(I1, D1, M1, A1), Transfer_tokens(I2, D2, M2, A2)) =>
    #Matches(I1, I2) andBool
    #Matches(D1, D2) andBool
    #Matches(M1, M2) andBool
    #Matches(A1, A2)

  rule #Matches(Set_delegate(I1, O1), Set_delegate(I2, O2)) =>
    #Matches(I1, I2) andBool #Matches(O1, O2)

  rule #Matches(Pair L1 R1, Pair L2 R2) => #Matches(L1, L2) andBool #Matches(R1, R2)

  rule #Matches(Some D1, Some D2) => #Matches(D1, D2)

  rule #Matches(Left D1, Left D2) => #Matches(D1, D2)
  rule #Matches(Right D1, Right D2) => #Matches(D1, D2)
```

The representation of \#Any is the same in the semantics and the concrete
syntax.

```k
  rule #MichelineToNative(#Any, _) => #Any
```

This function transforms a LiteralStack (e.g. a sequence of `Stack_elt`
productions) into a KSequence (the same format as the execution stack).

```k
  syntax K ::= #LiteralStackToSemantics(LiteralStack) [function]
  rule #LiteralStackToSemantics( { } ) => .
  rule #LiteralStackToSemantics( { L } ) => #LiteralStackToSemanticsAux(L)

  syntax K ::= #LiteralStackToSemanticsAux(StackElementList) [function]

  rule #LiteralStackToSemanticsAux( Stack_elt T D ; Gs:StackElementList) =>
       #MichelineToNative(D, T) ~> #LiteralStackToSemanticsAux(Gs)

  rule #LiteralStackToSemanticsAux(Stack_elt T D) =>
       #MichelineToNative(D, T)
```

This function transforms an expected output stack to its internal representation
(failed stacks are already in their internal representation, literals must be
transformed as in the input group).

```k
  syntax K ::= #OutputStackToSemantics(OutputStack) [function]
  rule #OutputStackToSemantics(L:LiteralStack) => #LiteralStackToSemantics(L)
  rule #OutputStackToSemantics(X:FailedStack) => X
```

Loading the input or expected output stack involves simply converting it to a
KSeq whose elements are Data in their internal representations, and then
placing that KSeq in the main execution stack configuration cell.

```k
  rule <k> input LS => .K ... </k>
       <inputstack> .K => LS </inputstack>

  rule <k> output Os => .K ... </k>
       <expected> .K => Os </expected>
```

```k
  syntax KItem ::= "#ConvertStackToNative"
  rule <k> #ConvertStackToNative => .K ... </k>
       <inputstack> Actual => #LiteralStackToSemantics(Actual) </inputstack>
       <paramtype> PT </paramtype>
```

As in the case of the contract group, loading the code group is trivial --
simply extract the block and let the main semantics handle the rest.

```k
  rule <k> code C => .K ... </k>
       <script> #NoData => C </script>
```

Type Checking Extension
-----------------------

For type-checking purposes, given an input or expected output stack, we need to
know what types are on the stack.

```k
  syntax TypeSeq ::= #LiteralStackToTypes(LiteralStack, Type) [function]

  rule #LiteralStackToTypes( { } , _) => .TypeSeq
  rule #LiteralStackToTypes( { L } , T ) => #LiteralStackToTypesAux(L, T)

  syntax TypeSeq ::= #LiteralStackToTypesAux(StackElementList, Type) [function]

  rule #LiteralStackToTypesAux( Stack_elt T D ; Gs:StackElementList, PT)
    => T ; #LiteralStackToTypesAux(Gs, PT)
    requires #Typed(D, T) :=K #TypeData(PT, D, T)

  rule #LiteralStackToTypesAux(Stack_elt T D, PT) => T
    requires #Typed(D, T) :=K #TypeData(PT, D, T)
```

### `#TypeAndExecute` function

Executing Michelson code without type information leads to non-determinism.
For example, the `CONCAT` instruction, when applied to an empty list, produces
either an empty `string` or empty `bytes`. Without knowing the type of the list,
the resulting type of value is unknown.

The K-Michelson semantics was originally written without a type system/checker.
Later, a type system was added to resolve various issues, including the one
mentioned above.

The result of type-checking a block of code produces an equivalent block where
each instruction has been wrapped in its corresponding type. These types are
unwrapped and stored in a fresh configuration cell `<stacktypes>` during
execution. This allows the oringal "type-free" semantics can be used for all
unambiguous cases while any type-dependent instructions can reference the
`<stacktypes>` cell to determine which execution path is needed.

To correctly check the typing of a unit test, we need the following info:

1. the contract parameter type --- only used in typing the `SELF` instruction
2. the input stack types --- which depend on (1) because `lambda`
3. the output stack types --- which depend on (1) for the same reason
4. a Michelson script

The `#TypeAndExecute` takes parameters 1-4, performs the type-check, and then
replaces the code in the K cell with typed version.

TODO: `#TypeAndExecute` currently is a no-op when the expected output stack is
a failed stack --- but this means that we cannot execute tests fully when we
expect failure. See note below.

```k
  syntax KItem ::= #TypeAndExecute(Block, Type, LiteralStack, OutputStack)
  syntax KItem ::= #TypeAndExecuteAux(LiteralStack, LiteralStack, TypeSeq, TypedInstruction)

  rule <k> #TypeAndExecute(B,P,IS,OS:LiteralStack)
        => #TypeAndExecuteAux(
             IS,
             OS,
             #LiteralStackToTypes(OS, P),
             #TypeInstruction(P, B, #LiteralStackToTypes(IS,P))
           )
           ...
       </k>

  // TODO: Implement a "partial" type check case
  rule <k> #TypeAndExecute(B,P,IS,OS:FailedStack) => B ... </k>
       <stack> _ => IS </stack>
       <stacktypes> _ => #LiteralStackToTypes(IS,P) </stacktypes>

  rule <k> #TypeAndExecuteAux(IS, OS, OSTypes, #TI(B, ISTypes -> OSTypes))
        => #Exec(#TI(B, ISTypes -> OSTypes)) ...
       </k>
       <stack> _ => IS </stack>
       <stacktypes> _ => ISTypes </stacktypes>
```

### `#ExecuteTypedScript`

This function executes a script in a typed fashion with an expected output
stack. Currently, it just calls `#TypeAndExecute` with the appropriate
arguments.

```k
  syntax KItem ::= "#ExecuteTypedScript"

  rule <k> #ExecuteTypedScript
        => #TypeAndExecute(B,PT,IS,OS)
        ...
       </k>
       <script> B </script>
       <paramtype> PT </paramtype>
       <inputstack> IS </inputstack>
       <expected> OS </expected>
```

`#VerifyOutput`
---------------

Once execution finishes, the output verification is simply stepping through the
KSequence and removing any elements that `#Match`. An unsuccessful unit test
will get stuck during this step, with the first sequence in the `#VerifyOutput`
production and stack cells being the expected and actual outputs respectively.

```k
  syntax KItem ::= "#ConvertOutputStackToNative"
  rule <k> #ConvertOutputStackToNative => . ... </k>
       <expected> Expected => #OutputStackToSemantics(Expected) </expected>
```

```k
  syntax KItem ::= "#VerifyOutput"
  rule <k> #VerifyOutput ... </k>
       <stack>    S2 => . ... </stack>
       <expected> S1 => . ... </expected>
    requires #Matches(S1, S2)
```

The final step when all elements of the KSequences have been exhausted is to set
the process' exit code as appropriate to indicate a successful test execution,
and then empty the k cell. In a successful unit test execution (except expected
failures), this rule is where the semantics will halt. Implicitly, this rule
also checks that `#VerifyOutput` is the last remaining production in the k cell
by excluding the normal '...' variable at the end of the K cell.

```k
  rule <k> #VerifyOutput => . ... </k>
       <stack>    . </stack>
       <expected> . </expected>
```

In the case of an expected failure, we cannot guarantee that the contents of the
K cell will be empty when the main semantics abort. However, we know that the
`#VerifyOutput` will still be in the k cell. Hence, if the main semantics abort
(by placing the Aborted production on the top of the k cell), we should find the
`#VerifyOutput` production in the K cell and pull it out.

```k
  syntax KItem ::= #FindVerifyOutput(K, KItem)
  syntax KItem ::= #NoVerifyOutput(KItem)

  rule <k> #FindVerifyOutput(#VerifyOutput ~> _, _) => #VerifyOutput ... </k>
  rule <k> #FindVerifyOutput(_:KItem ~> Rs => Rs, _) ... </k> [owise]

  rule <k> Aborted(_, _, Rk, _) #as V => #FindVerifyOutput(Rk, V) ... </k>
endmodule
```
