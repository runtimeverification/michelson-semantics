Michelson `LOOP` Invariant Analysis
===================================

Introduction and Preliminaries
------------------------------

We would like to define Hoare logic rules for Michelson loops, to help us ensure
that we have the proper `LOOP` invariant semantics. Let us recall the standard C
"while" loop Hoare rule:

```
      [Invariant ∧ CC ] Body [ Invariant ]
-----------------------------------------------------
[ Invariant ] while (CC) { Body } [ Invariant ∧ ¬CC ]
```

A few things to note:

1.  The invariant formula must be provided by the user; in general, inferring a
    loop invariant is undecidable.

2.  The invariant formula is only required to hold at designated invariant
    checkpoints. For C-style loops, those checkpoints should be:

    a. before loop execution
    b. after the initial execution of CC
    c. after each loop body iteration and CC completes

In particular, the loop invariant is *not* required to hold inside the loop
body.

Also note that the checkpoint occurring *after* the loop CC is executed only
matters when the CC is *effectful*, i.e. *not side-effect free*.

Why? The invariant must hold after the loop terminates --- which can only happen
when the CC evaluates to false --- which implies the invariant must hold *after*
CC evaluation.

However, if the CC is side-effect free, then the program state before and after
CC evaluation is identical. This allows for additional flexibility when picking
checkpoint locations. In particular, the distinction between (a) and (b)
evaporates.

3.  In this notation, we generally assume that the loop continuation guard is
    side-effect free. If it is not, the meaning of `¬CC` as a predicate should
    be understood only in terms of its boolean result without any side-effects.

### Stack Notation

Since we are dealing with a stack-based language, we need a way to specify stack
shapes in our Hoare rules. Thus, we have developed a typed stack notation. For
example, the expression below:

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

-   standard logical notation from first-order logic (FOL), i.e. `∧`,`∨`,`¬`,
    etc...
-   typed stacks are understood as *predicates* representing the actual
    Michelson program stack has the same shape as typed stack

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

-   before loop execution at point (1)
-   symbolically after loop iteration at point (2)

Note that, by abuse of notation, before we execut the next iteration of a
Michelson `LOOP` instruction, we will refer to the boolean element that appears
at the top of stack as its continuation condition (CC).

### Invariant Checkpoints for Michelson

To ensure soundness, we must properly convert our invariant checkpoints from
the C-style code to Michelson code so that we can do reasoning over pure
Michelson code.

Naturally, this means it is sufficient to check the invariant when:

-   in the top-level proof, when we encounter the `LOOP` instruction but
    *before* descending into loop:
-   in the subproof, after we have symbolically executed the loop body

This corresponds to the following Hoare logic rule:

```
(α)  [ (bool True) ..S1 ∧ Inv(True, S1) ] DROP ; Body [ (bool B1') ..S1' ∧ Inv(B1', S1') ]
    ---------------------------------------------------------------------------------------
           [ (bool B) ..S ∧ Inv(B, S) ] LOOP { Body } [ ..S' ∧ Inv(False, S') ]
```

Note that our loop rule only implicitly mentions the loop CC by referring to
the boolean element at the top of stack.
Said differently, line `(α)` amounts to a progress requirement on invariant.

**Progress Requirement**: from all states where the CC is `True` and `Inv`
holds and we execute `DROP ; Body`, we reach a state where `Inv` still holds.

TODO:

1.  Note that C-style loops with side-effects in conditions can be trivially
    transformed into standard loops
2.  We can view the Michelson loop rule in that way --- as an instance of such a
    transformed loop

