Michelson `LOOP` Invariant Analysis
===================================

Introduction and Preliminaries
------------------------------

We would like to define Hoare logic rules for Michelson loops, to help us ensure
that we have the proper `LOOP` invariant semantics. Let us recall the standard C
"while" loop and corresponding Hoare logic loop rule.

Recall that C-style while loops have the form:

```
while (CC) {
  Body
}
```

Note that, for technical reasons, we generally assume that evaluating the
continuation condition (CC) is side-effect free.
If it is not, we can apply a simple loop transformation to make the CC
evaluation side-effect free, i.e., the loop above is equivalent to:

```
int cond = CC;
while (cond) {
  Body
  cond = CC;
}
```

Thus, without loss of generality, assume that the expression `CC` is
side-effect free.

With that detail settled, we recall the corresponding Hoare logic loop rule:

```
      [Invariant ∧ CC ] Body [ Invariant ]
-----------------------------------------------------
[ Invariant ] while (CC) { Body } [ Invariant ∧ ¬CC ]
```

A few things to note:

1.  The invariant formula must be provided by the user; in general, inferring a
    loop invariant is undecidable.

2.  The invariant formula is only required to hold at designated invariant
    checkpoints. When the expression CC is side-effect free, those checkpoints
    should be:

    a. before loop execution
    b. after each loop body iteration

    In particular, the loop invariant is *not* required to hold inside the loop
    body.

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

Assuming that:

1.  our C code has an implicit, global, typed value stack that we can operate
    on called `stack`
2.  the `stack` exposes a function `pop` which pops the top stack element and
    returns it
3.  the Michelson code `LOOP { Body }` is well-typed

converting the Michelson code `LOOP { Body }` to C might look like:

```c
while (pop(stack) == BOOL_TRUE) {
  Body
}
```

Of course, as we explained above, the continuation condition in this loop is
not amenable to static analysis because it is not side-effect free. We can
convert it into a side-effect loop using a variation of the trick that we
mentioned above:

```c
int cond = pop(stack) == BOOL_TRUE;
// 1
while (cond) {
  Body
  cond = pop(stack) == BOOL_TRUE;
  // 2
}
```

Note that, if we view our loop this way, the actual continuation condition
check is side-effect free. This means it is enough to consider checkpoints:

-   before loop execution at point (1)
-   symbolically after loop iteration at point (2)

### Hoare Loop Rule for Michelson LOOPs

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
(β)           [ (bool B) ..S ∧ Inv(B, S) ] LOOP { Body } [ ..S' ∧ Inv(False, S') ]
```

As noted above, our loop rule only implicitly mentions the CC by referring to
the boolean element at the top of stack.
Here we see that our invariant formula `Inv` is parameterized by *the value of
the stack when our current continuation is of the form `LOOP { Body }`*.
This means that our invariant formula may be parameterized by stack values
which no longer exist, as seen on the righthand side of line `(β)`, where the
invariant formula's first argument is the stack element `False` which by then
has already been popped off of the stack.
