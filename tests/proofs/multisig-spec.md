```k
requires "lemmas.md"
```

Preamble
========

The K Framework supports different features using different included tools:

-   writing the semantics of languages and generating concrete/symbolic
    interpreters (kompile)
-   running programs using a language semantics-based interpreter (krun)
-   verifying properties about programs, or even about language semantics
    themselves (kprove)

In this file, we specify a property about the Michelson multisig contract and
verify it using the kprove tool. A kprove proof typically has three parts:

1.  some modules which specify the semantics of the language we are working
2.  some modules which specify lemmas as well as functions about our language
    that are needed for theorem proving purposes
3.  one module which specifies a set of claims that we wish to verify

In our current proof, we have all three parts:

1.  module `MICHELSON` (in file `michelson.md` defines our language semantics)
2.  module `VERIFICATION` (see below) defines lemmas and predicates about the
    Michelson language that we will use
3.  module `MULTISIG-SPEC` (see below) specifies the claims we whish to verify
    about the multisig program

```k
module VERIFICATION
  import LEMMAS // we import some basic lemmas which apply to most programs
```

We define lemmas and functions below for our specific program:

We use the `numValidSigs` function to show each iteration maintains the invariant
`VerifiedKeys + numValidSigs(RemainingSigs, RemainingKeys) >= Threshold`.

```k
  syntax Int ::= numValidSigs(sigs: InternalList, keys: InternalList) [function, smtlib(numValidSigs)]
  rule numValidSigs([ Some #Signature(_) ] ;; Sigs, [ #Key(_) ] ;; Keys) => 1 +Int numValidSigs(Sigs, Keys) [simplification, smt-lemma]
  rule numValidSigs([ None ] ;; Sigs, _K ;; Keys) => numValidSigs(Sigs, Keys) [simplification, smt-lemma]
  rule numValidSigs(_, .InternalList) => 0 [simplification, smt-lemma]
```

This lemma ensures `CHECK_SIGNATURE` always returns `True` when executing the
multisig program --- this represents that our signature list is valid.

```k
  rule <k> CHECK_SIGNATURE _A => . ... </k>
       <stack> [ key #Key(_) ]
             ; [ signature #Signature(_) ]
             ; [ bytes #Packed(_,_) ]
             ; SS
            => [ bool true ]
             ; SS
       </stack>
```

We add a few more lemmas about the `numValidSigs` function to help our prover
along. Firstly, we know that the number of valid signatures is always a
non-negative number.

```k
  rule numValidSigs(_, _) >=Int 0 => true [simplification, smt-lemma]
```

Secondly, we add a lemma about the associativity of addition, specialized to
the instance in which we need it.

```k
  rule N:Int +Int ( 1 +Int numValidSigs(L0,L)) => (N:Int +Int 1) +Int numValidSigs(L0,L) [simplification, smt-lemma]
```

```k
endmodule
```

Claim Definition
================

We proceed to define the claims we will make about the program. We will need
two claims:

1.  a claim which describes the intended behavior of the entire program
2.  a claim which describes the intended behavior of the loop

The second claim is needed to prevent symbolic execution from looping forever
while trying to summarize the behavior over the infinitude of possibile loop
iterations.

Before proceeding, we create a new module to store our claims.

```k
module MULTISIG-SPEC
  imports VERIFICATION
```

Claim Structure
---------------

The claims that we make have the following structure:

```
claim <k> InitialSourceFragment => FinalSourceFragment </k>
      <stack> InitialStack => FinalStack </stack>
      <entrypoints> %default |-> ContractParameterType </entrypoints>
      <myamount> AmountPassedToContract </myamount>
      requires Preconditions
      ensures Postconditions
```

In a claim, the different cells (e.g. the `<k>` cell) represent the state of
an abstract Michelson interpreter. Arrows inside cells describes what the
initial abstract interpreter state and the final abstract interpreter state.
Cells without arrows represent constant fragments of the abstract interpreter
state.
The claim asserts that any instance of the abstract interpreter initial state
that satisfies the preconditions, i.e., an instance of the following:

```
<k> InitialSourceFragment </k>
<stack> InitialStack  </stack>
<entrypoints> %default |-> ContractParameterType </entrypoints>
<myamount> AmountPassedToContract </myamount>
```

which satisfies the predicates `Preconditions` will always reach a final state
which is an instance of:

```
<k> FinalSourceFragment </k>
<stack> FinalStack </stack>
<entrypoints> %default |-> ContractParameterType </entrypoints>
<myamount> AmountPassedToContract </myamount>
```

which satisfies the predicates `Preconditions` and also `Postconditions`.

Main claim
----------

Our main claim asserts that the contract completes successfully when:

-   the storage contains:

    ```
    (Pair Count:Int (Pair Threshold:Int KeyList:InternalList))
    ```

-   the initial parameter value is the following (see legend below)

    ```
    Right
     (Pair
       (Pair Count:Int                           // (1)
             (Left uninterpreted(Id:Int, unit))) // (2)
       SigList:InternalList)                     // (2)
    ```

    Value (1) is a counter identical to the current value in storage.
    Value (2) are the operations to be authorized by the multisig contract.
    Value (3) is the list of the signatures passed to the contract for
    verification purposes.

In order for the contract to complete successfully, we must ensure that it does
not abort. Thus, we must avoid the following conditions that could lead to
aborting (note all locations marked in the contract):

-   (1) if a non-zero amount is passed to the contract
-   (2) if any of the first size(KeyList) signatures are invalid
-   (3) if the size(SigList) <= size(KeyList)
-   (4) if the number of signatures verified is less than Threshold
-   (5) if extra signatures were passed to the contract

We handle these issues as follows:

-   (1) is handeled by side condition (a)
-   (3), (5) by side condition (b)
-   (4) by side condition (c)
-   (2) is taken care of by the `CHECK_SIGNATURE` lemma above

The side conditions are defined as follows (and marked in the claim below):

-   (a) Amount == 0
-   (b) size(SigList) == size(KeyList)
-   (c) numValidSigs(SigList, KeyList) >= Threshold

```k
  claim <k> UNPAIR .AnnotationList ;
           IF_LEFT .AnnotationList { DROP .AnnotationList ;
                                     NIL .AnnotationList operation .AnnotationList ;
                                     PAIR .AnnotationList
                                   }
                                   { PUSH .AnnotationList mutez .AnnotationList 0 ;
                                     AMOUNT .AnnotationList ;
                                     ASSERT_CMPEQ .AnnotationList ; // ---------------------------------------------------------------- (1)
                                     SWAP .AnnotationList ;
                                     DUP .AnnotationList ;
                                     DIP .AnnotationList { SWAP .AnnotationList } ;
                                     DIP .AnnotationList { UNPAIR .AnnotationList ;
                                     DUP .AnnotationList ;
                                     SELF .AnnotationList ;
                                     ADDRESS .AnnotationList ;
                                     CHAIN_ID .AnnotationList ;
                                     PAIR .AnnotationList ;
                                     PAIR .AnnotationList ;
                                     PACK .AnnotationList ;
                                     DIP .AnnotationList
                                     { UNPAIR  .AnnotationList ;
                                       DIP .AnnotationList { SWAP .AnnotationList }
                                     } ;
                                     SWAP .AnnotationList } ;
                                     UNPAIR  .AnnotationList ;
                                     DIP .AnnotationList { SWAP .AnnotationList } ;
                                     ASSERT_CMPEQ .AnnotationList ;
                                     DIP .AnnotationList { SWAP .AnnotationList } ;
                                     UNPAIR   .AnnotationList ;
                                     DIP .AnnotationList
                                     { PUSH  .AnnotationList nat .AnnotationList 0 ;
                                       SWAP .AnnotationList ;
                                       ITER .AnnotationList
                                       {
                                         DIP .AnnotationList { SWAP .AnnotationList } ;
                                         SWAP .AnnotationList ;
                                         IF_CONS .AnnotationList
                                         { IF_SOME .AnnotationList
                                           { SWAP .AnnotationList ;
                                             DIP .AnnotationList
                                             { SWAP .AnnotationList ;
                                               DIP .AnnotationList 2 { DUP .AnnotationList 2 } ;
                                               { DUP .AnnotationList 3 ;
                                                 DIP .AnnotationList { CHECK_SIGNATURE .AnnotationList } ;
                                                 SWAP .AnnotationList ;
                                                 IF .AnnotationList { DROP .AnnotationList } { FAILWITH .AnnotationList } // ------ (2)
                                                } ;
                                               PUSH .AnnotationList nat .AnnotationList 1 ;
                                               ADD  .AnnotationList
                                             }
                                           }
                                           { SWAP .AnnotationList ; DROP .AnnotationList }
                                         }
                                         { FAIL .AnnotationList } ; // ------------------------------------------------------------ (3)
                                         SWAP .AnnotationList
                                       }
                                     } ;
                                     ASSERT_CMPLE .AnnotationList ; // ------------------------------------------------------------ (4)
                                     IF_CONS .AnnotationList { FAIL .AnnotationList } { } ; // ------------------------------------ (5)
                                     DROP .AnnotationList ;
                                     DIP .AnnotationList
                                     { UNPAIR .AnnotationList ;
                                       PUSH .AnnotationList nat .AnnotationList 1 ;
                                       ADD  .AnnotationList ;
                                       PAIR .AnnotationList
                                     } ;
                                     IF_LEFT .AnnotationList
                                     { UNIT .AnnotationList ; EXEC .AnnotationList }
                                     { DIP .AnnotationList { CAR .AnnotationList } ;
                                       SWAP .AnnotationList ;
                                       PAIR .AnnotationList ;
                                       NIL .AnnotationList operation .AnnotationList
                                     } ;
                                     PAIR .AnnotationList
                                }
      => .K
    </k>
    <stack> [ pair (or unit                                           // default operation
                       pair (pair nat                                 // counter
                                  (or (lambda unit (list operation))  // requested action
                                      pair nat                        // change keys - new threshold
                                           (list key)))               // change keys - new key list
                            (list (option signature)))
                   (pair nat                                          // stored counter
                         (pair nat                                    // threshold
                               (list key)))                           // list of keys
             // ---------------------------------------------------------------------
              Pair
                (Right
                  (Pair
                    (Pair Count:Int
                          (Left #Lambda(unit, list operation, { #Uninterpreted(Id, unit, list operation) })))
                    SigList:InternalList))
                (Pair Count:Int (Pair Threshold:Int KeyList:InternalList)) ] ; .Stack
         // -----------------------------------------------------------------
         => [ pair ( list operation ) pair nat pair nat list key Pair uninterpreted ( Id , Unit ) Pair Count +Int 1 Pair Threshold KeyList:InternalList ] ; .Stack
    </stack>
    <myamount> #Mutez(Amount:Int) </myamount>
    <currentAccount> MYADDR </currentAccount>
    <accounts>
        MYADDR |-> #Account(... entrypoints : %default |-> (or .AnnotationList unit .AnnotationList
                                               pair .AnnotationList (pair .AnnotationList nat .AnnotationList
                                                          (or .AnnotationList (lambda .AnnotationList unit .AnnotationList (list .AnnotationList operation .AnnotationList ))
                                                              pair .AnnotationList nat .AnnotationList
                                                                   (list .AnnotationList key .AnnotationList )))
                                                    (list .AnnotationList (option .AnnotationList signature .AnnotationList ))))
        ...
    </accounts>
    requires Amount ==Int 0  // --------------------------------------------------------------------------------------------------- (a)
     andBool size(SigList) ==Int size(KeyList)  // -------------------------------------------------------------------------------- (b)
     andBool numValidSigs(SigList, KeyList) >=Int Threshold // -------------------------------------------------------------------- (c)
```

Circularity
-----------

This circularity is needed to show that the invariant

`VerifiedKeys +Int size(KeyList) >=Int ?VerifiedKeysFinal`

is preserved by the loop, where `VerifiedKeys` stores the number of keys we
have already verified during loop execution and `?VerifiedKeysFinal` stores the
final number of verified keys after loop execution.

Note that variables which appear on the right-hand side only and which are
preceded by a question mark are existentially quantified; all other variables
are univerally quantified.

The initial source fragment in the circularity contains just the fragment of
the contract source code describing the loop. The `...` at the end of the
`<k>` cell means that this claim describes how an initial contract fragment
evolves that may be followed by other code (which we do not care about here).

The initial stack pattern for the circularity shows the stack at the head of
the loop. The final stack pattern shows the stack after the loop completes.

Our preconditions are similar to our main claim except that instead of
asserting that `numValidSigs() >= Threshold`, we assume that we are in
the middle of the loop and have already verified that some keys our valid,
so that we need to check:

`VerifiedKeys + numValidSigs(SigList, KeyList) >= Threshold`

where `SigList` and `KeyList` are (possibly) smaller than the ones present at
the start of the program and `VerifiedKeys` stores how many keys we have
already verified.

Here we also have some postconditions which assert claims our existentially
quantified variable `?VerifiedKeysFinal` which describes the total number of
verified keys after the loop completes. We must assert that:

```
?VerifiedKeysFinal >= Threshold
```

in order for the contract to go through. The other postconditions are helpful
for symbolic reasoning.

```k
  claim <k> ITER .AnnotationList
            {
              DIP .AnnotationList { SWAP .AnnotationList } ;
              SWAP .AnnotationList ;
              IF_CONS .AnnotationList
              { IF_SOME .AnnotationList
                { SWAP .AnnotationList ;
                  DIP .AnnotationList { SWAP .AnnotationList ;
                                        DIP .AnnotationList 2 { DUP .AnnotationList 2 } ;
                                        { DUP .AnnotationList 3 ;
                                          DIP .AnnotationList { CHECK_SIGNATURE .AnnotationList } ;
                                          SWAP .AnnotationList ;
                                          IF .AnnotationList { DROP .AnnotationList } { FAILWITH .AnnotationList } // ---- (1)
                                         } ;
                                        PUSH .AnnotationList nat .AnnotationList 1 ;
                                        ADD  .AnnotationList
                                      }
                }
                { SWAP .AnnotationList ; DROP .AnnotationList }
              }
              { FAIL .AnnotationList } ;
              SWAP .AnnotationList
            } => .K
            ...
        </k>
        <stack> [ list key KeyList:InternalList ]
              ; [ nat VerifiedKeys ]
              ; [ list option signature SigList:InternalList ]
              ; [ bytes #Packed ( pair ( pair chain_id address ) pair nat or ( lambda unit list operation ) pair nat list key , Pair Pair #ChainId ( ChainId ) MyAddr:Address Pair Count:Int Left #Lambda ( unit , list operation , { #Uninterpreted(Id, unit, list operation) } ) ) ]
              ; [ or ( lambda unit list operation ) pair nat list key Left #Lambda ( unit , list operation , { #Uninterpreted(Id, unit, list operation) } ) ]
              ; [ pair nat pair nat list key Pair Count:Int Pair Threshold:Int KeyListInit:InternalList ]
              ; .Stack
             => [ nat ?VerifiedKeysFinal ]
              ; [ list option signature .InternalList ]
              ; [ bytes #Packed ( pair ( pair chain_id address ) pair nat or ( lambda unit list operation ) pair nat list key , Pair Pair #ChainId ( ChainId ) MyAddr:Address Pair Count:Int Left #Lambda ( unit , list operation , { #Uninterpreted(Id, unit, list operation) } ) ) ]
              ; [ or ( lambda unit list operation ) pair nat list key Left #Lambda ( unit , list operation , { #Uninterpreted(Id, unit, list operation) } ) ]
              ; [ pair nat pair nat list key Pair Count:Int Pair Threshold:Int KeyListInit:InternalList ]
              ; .Stack
        </stack>
        <myamount> #Mutez(Amount:Int) </myamount>
        <currentAccount> MYADDR </currentAccount>
        <accounts>
            MYADDR |-> #Account(... entrypoints : %default |-> (or .AnnotationList unit .AnnotationList
                                                   pair .AnnotationList (pair .AnnotationList nat .AnnotationList
                                                              (or .AnnotationList (lambda .AnnotationList unit .AnnotationList (list .AnnotationList operation .AnnotationList ))
                                                                  pair .AnnotationList nat .AnnotationList
                                                                       (list .AnnotationList key .AnnotationList )))
                                                        (list .AnnotationList (option .AnnotationList signature .AnnotationList ))))
            ...
        </accounts>
    requires Amount ==Int 0
     andBool size(SigList) ==Int size(KeyList)
     andBool VerifiedKeys >=Int 0
     andBool VerifiedKeys +Int numValidSigs(SigList, KeyList) >=Int Threshold

     ensures ?VerifiedKeysFinal >=Int VerifiedKeys
     andBool ?VerifiedKeysFinal >=Int Threshold
     andBool VerifiedKeys +Int size(KeyList) >=Int ?VerifiedKeysFinal
```

```k
endmodule
```
