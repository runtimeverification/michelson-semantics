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
        ~> #ExecuteScript
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

  syntax TypeSeq ::= #LiteralStackToTypes(LiteralStack, Type) [function]

  rule #LiteralStackToTypes( { } , _) => .TypeSeq
  rule #LiteralStackToTypes( { L } , T ) => #LiteralStackToTypesAux(L, T)

  syntax TypeSeq ::= #LiteralStackToTypesAux(StackElementList, Type) [function]

  rule #LiteralStackToTypesAux( Stack_elt T D ; Gs:StackElementList, PT) => T ; #LiteralStackToTypesAux(Gs, PT)
       requires #Typed(D, T) :=K #TypeData(PT, D, T)
  rule #LiteralStackToTypesAux(Stack_elt T D, PT) => T
       requires #Typed(D, T) :=K #TypeData(PT, D, T)
```

This function transforms an expected output stack to its internal representation
(failed stacks are already in their internal representation, literals must be
transformed as in the input group).

```k
  syntax K ::= #OutputStackToSemantics(OutputStack) [function]
  rule #OutputStackToSemantics(L:LiteralStack) => #LiteralStackToSemantics(L)
  rule #OutputStackToSemantics(X:FailedStack) => X
```

Loading the input stack involves simply converting it to a KSeq whose elements
are Data in their internal representations, and then placing that KSeq in the
main execution stack configuration cell.

```k
  rule <k> input LS => .K ... </k>
       <stack> .K => LS </stack>
```

```k
  syntax KItem ::= "#ConvertStackToNative"
  rule <k> #ConvertStackToNative => .K ... </k>
       <stack> Actual => #LiteralStackToSemantics(Actual) </stack>
       <paramtype> PT </paramtype>
       <stacktypes> .TypeSeq => #LiteralStackToTypes(Actual, PT) </stacktypes>
```

Loading the expected output group is unusual because an output group will not do
anything when loaded. Instead it simply schedules the output for verification
later on, and then passes directly to the next group.

```k
  syntax KItem ::= #CheckTypes(OutputStack, Block)

  syntax KItem ::= "#VerifyOutput"
  
  rule <k> output Os => .K ... </k>
       <expected> .K => Os </expected>

//  rule <k> #LoadGroups(output Os ; (code B #as Gs)) => #CheckTypes(Os, B)
//        ~> #LoadGroups(Gs)
//        ~> #VerifyOutput(#OutputStackToSemantics(Os))
//           ...
//       </k>
//  rule <k> #LoadGroups(output Os ; ((code B ; _) #as Gs)) => #CheckTypes(Os, B) ~> #LoadGroups(Gs) ~> #VerifyOutput(#OutputStackToSemantics(Os)) ... </k>

  syntax KItem ::= #CheckTypesResult(TypeSeq, TypedInstruction)

  rule <k> #CheckTypes(LS:LiteralStack, B) => #CheckTypesResult(#LiteralStackToTypes(LS, P), #TypeInstruction(P, B, TS)) ... </k>
       <paramtype> P </paramtype>
       <stacktypes> TS </stacktypes>

  rule <k> #CheckTypes(_, _) => . ... </k> [owise]

//  rule <k> #CheckTypesResult(Os, #TI(_, Is -> Os) #as B) ~> #LoadGroups(code _) => #LoadGroups(code { #Exec(B) }) ... </k>
//       <stacktypes> Is </stacktypes>
//
//  rule <k> #CheckTypesResult(Os, #TI(_, Is -> Os) #as B) ~> #LoadGroups(code _ ; Gs) => #LoadGroups(code { #Exec(B) } ; Gs) ... </k>
//       <stacktypes> Is </stacktypes>
```

As in the case of the contract group, loading the code group is trivial -- simply
extract the block and let the main semantics handle the rest.

```k
  rule <k> code C => .K ... </k>
       <script> #NoData => C </script>
```

```k
  rule <k> #VerifyOutput ... </k>
       <expected> Expected => #LiteralStackToSemantics(Expected) </expected>
```

Once execution finishes, the output verification is simply stepping through the
KSequence and removing any elements that `#Match`. An unsuccessful unit test
will get stuck during this step, with the first sequence in the `#VerifyOutput`
production and stack cells being the expected and actual outputs respectively.

```k
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
