Michelson `LOOP` Invariant Analysis
===================================

Introduction and Preliminaries
------------------------------

We would like to define Hoare logic rules for Michelson loops, to help us ensure
that we have the proper `LOOP` invariant semantics.
Let us recall the standard C "while" loop Hoare rule:

```
      [Invariant ∧ CC ] Body [ Invariant ]
-----------------------------------------------------
[ Invariant ] while (CC) { Body } [ Invariant ∧ ¬CC ]
```

A few things to note:
1. The invariant formula must be provided by the user; in general, inferring a
  loop invariant is undecidable.

2. The invariant formula is only required to hold at designated invariant
  checkpoints.
  For C-style loops, those checkpoints should be:

  - (a) before loop execution
  - (b) after the initial execution of CC
  - (c) after each loop body iteration and CC completes

  In particular, the loop invariant is _not_ required to hold inside the loop
  body.

  Also note that the checkpoint occurring _after_ the loop CC is executed only
  matters when the CC is _effectful_, i.e. _not side-effect free_.

  Why? The invariant must hold after the loop terminates --- which can only
  happen when the CC evaluates to false --- which implies the invariant must
  hold _after_ CC evaluation.

  However, if the CC is side-effect free, then the program state before and
  after CC evaluation is identical.
  This allows for additional flexibility when picking checkpoint locations.
  In particular, the distinction between (a) and (b) evaporates.

3. In this notation, we generally assume that the loop continuation guard is
  side-effect free.
  If it is not, the meaning of `¬CC` as a predicate should be understood only
  in terms of its boolean result without any side-effects.

### Stack Notation

Since we are dealing with a stack-based language, we need a way to specify
stack shapes in our Hoare rules.
Thus, we have developed a typed stack notation.
For example, the expression below:

```
(T₁ E₁) ... (Tₖ Eₖ)
```

represents the stack with elements `E₁ ... Eₖ` such that element `Eₚ` has type
`Tₚ` for `1 ≤ p ≤ k`. To sipmlify our rule descriptions, we let variables
prefixed with two dots, e.g. `..S` represent an arbitrary stack. Thus,

`(T₁ E₁) (T₂ E₂) ..S`

represent any stack with _at least_ two elements on top.
By convention, for a stack variable `..S`, we let `..S'` represent a stack
variable of the sames shape (i.e. elements have the same type) but with
different values.

### Constraint Language

We need to represent constraints on Michelson program states. We will use:

- standard logical notation from first-order logic (FOL), i.e. `∧`,`∨`,`¬`,
  etc...
- typed stacks are understood as _predicates_ representing the actual Michelson
  program stack has the same shape as typed stack

Thus, the following would be a valid constraint:

`(int I) ..S ∧ I ≥ 5`

which specifies the stack has one integer element on top such that its value is
greater than or equal to 5.

### Program Notation

We represent program states as pairs of programs and constraints separated by
a forward slash `/`.
Thus, the following expression:

`LOOP { Body } / (bool False) ..S`

represents an input stack with a with one boolean value `False` on top
executing the program `LOOP { Body }`.

Michelson `LOOP` Semantics
--------------------------

Michelson `LOOP`s are different from C while loops.
Let us define it with small step style semantic rules:

```
LOOP { Body } / (bool False) ..S => {}                          / ..S
LOOP { Body } / (bool True)  ..S => DROP ; Body ; LOOP { Body } / ..S
```

where `{}` is the empty/terminal program (sometimes denoted `skip`) and `DROP`
is the Michelson instruction which drops the top stack element.

Note that Michelson loops have an implicit loop continuation condition, i.e.
the loop continuation condition is always that the stack top equals `True`.

### Relation to C-style While Loops

It is a useful exercise to compare the Michelson `LOOP` instruction to C-style
"while" loops to understand how we might apply standard Hoare logic rules for
C programs.

Assuming our C code has an implicit, global, typed value stack that we can
operate on called `stack`, converting the Michelson code `LOOP { Body }` to C
might look like:

```c
// 1
while (assert(stack->top->type == BOOL_TYPE), stack->top->val) {
  stack->pop();
  Body
  // 2
}
stack->pop();
```

Note that, if we view our loop this way, the actual continuation condition
check is side-effect free. This means it is enough to consider checkpoints:

- before loop execution at point (1)
- symbolically after loop iteration at point (2)

Note that, by abuse of notation, before we execut the next iteration of a
Michelson `LOOP` instruction, we will refer to the boolean element that appears
at the top of stack as its continuation condition (CC).

### Invariant Checkpoints for Michelson

To ensure soundness, we must properly convert our invariant checkpoints from
the C-style code to Michelson code so that we can do reasoning over pure
Michelson code.

Naturally, this means it is sufficient to check the invariant when:

- in the top-level proof, when we encounter the `LOOP` instruction but _before_
  descending into loop:
- in the subproof, after we have symbolically executed the loop body

This corresponds to the follwing Hoare logic rule:

```
(α)    [ (bool True) ..S ∧ Inv ] DROP ; Body [ (bool B) ..S' ∧ Inv ]
    ------------------------------------------------------------------
     [ (bool E) ..S ∧ Inv ] LOOP { Body } [ (bool False) ..S' ∧ Inv ]
```

Note that our loop rule only implicitly mentions the loop CC by referring to
the boolean element at the top of stack.
Said differently, line `(α)` amounts to a progress requirement on invariant.

*Progress Requirement*: from all states where the CC is `True` and `Inv` holds
and we execute `DROP ; Body`, we reach a state where the invariant still holds.

<!--

Deprecated
----------

```
-----------------------------------------------------------
{! EG } if (EG) { do { Body } while(CG) } { !EG }

       {Invariant /\ C } Body { Invariant }
-----------------------------------------------------------
{ EG } if (EG) { do { Body } while(CG) } { Invariant and !CG }

(1) if (EG) { (2) Body ; (3) while(CG) { Body } }

< (bool T) : (EG S) /\ Pre > LOOP { Body } < T : S' /\ Invariant /\ (!CG \/ !EG) >

< (bool T) : (EG S) /\ Pre > LOOP { Body } < T : S' /\ Post >

1. assert(Invariant)
   if (EG) {
       Body ;
       assert(Invariant)
       while(CG) {
         Body
         assert(Invariant)
       }
   }
   //    Invaraint /\ !CG
   // || Invariant /\ !EG
   Remaining

    //  Shape of the invariant can include bool at top of stack
    1.  Pre => Invariant
    2.  < T : S /\ Pre /\ Invariant /\ EG > Body < (bool T) : (B S) /\ Invariant >
    3.  < T : S        /\ Invariant /\ CG > Body < (bool T) : (B S) /\ Invariant >
    ------------------------------------------------------------------------------
    < (bool T) : (EG S) /\ Pre > LOOP { Body } < T : S' /\ Invariant /\ (!CG \/ !EG) >

    CG === ??? inside < T : S /\ ??? > Body < (bool T) : True S' >

    rule <k> LOOP A .AnnotationList Body
          => /* (1) */
             BIND { Shape } { ASSERT Predicates } ;
             LOOP .AnnotationList {
               Body
               BIND { Shape } { ASSERT Predicates } ;
               /* (2) */
               CUTPOINT(!Int, Shape) ;
               BIND { Shape } { ASSUME Predicates } ;
               /* (3) */
            }
            /* (4) */

            // (1)     // Pre => Invariant
            // (1, 2)  // < Pre /\ EG > Body < Invariant >
            // (3, 2)  // < Invariant /\ ??? >li

1'. if (EG) {
       assert(Invariant)
       Body ;
       while(CG) {
         assert(Invariant);
         Body
       }
   }
   assert(Invariant)
   //    Invaraint /\ !CG
   // || Invariant /\ !EG


    //  Shape of the invariant does not include bool at top of stack
    1.  Pre /\ EG => Invariant
    2.  < T : S /\ Pre /\ Invariant /\ EG > Body < (bool T) : (B S) /\ Invariant >
    3.  < T : S        /\ Invariant /\ CG > Body < (bool T) : (B S) /\ Invariant >
    ------------------------------------------------------------------------------
    < (bool T) : (EG S) /\ Pre > LOOP { Body } < T : S' /\ Invariant /\ (!CG \/ !EG) >

    rule <k> LOOP A .AnnotationList Body
          => /* (1) */
             LOOP .AnnotationList {
               /* (2) */
               BIND { Shape } { ASSERT Predicates } ;   // (1, 2) < Pre /\ EG > noop < Invariant >
               // Body
               // BIND { Shape } { ASSERT Predicates } ;
               //
               CUTPOINT(!Int, Shape) ;
               BIND { Shape } { ASSUME Predicates } ;   // (3, 2) < Invariant /\  CG > Body < Invariant >
               /* (3) */
               Body
             }
             /* (4) */
             BIND { Shape } { ASSERT Predicates } ;     // (1, 4) Pre /\ !EG => Inv
                                                        // (3, 4) < Invariant /\ !CG > Body < Invariant >
             ...                                        // (1, 4, ...) (3, 4, ...) < (Pre /\ !EG)  || (Invariant /\ ! CG) >

2. if (EG) {
       assert(Invariant)
       Body ;
       while(CG) {
         assert(Invariant);
         Body
       }
       assert(Invariant)
   }
   // !EG || (Invaraint /\ !CG)
   Remaining

    <Invariant /\ EG> Body <Invariant>          <Invariant /\ CG> Body <Invariant>
    -------------------------------------------------------------------------------
             LOOP { Body }     <!EG || Invariant /\ !CG>

    And then prove:
    * <!EG>              Remaining <Post>
    * <Invariant /\ !CG> Remaining <Post>

3. if (EG) {
       Body ;
       assert(Invariant)
       while(CG) {
         Body
         assert(Invariant)
       }
   }
```
-->
