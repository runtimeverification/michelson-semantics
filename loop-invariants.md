Michelson Loop Invariant Analysis
=================================

Introduction
------------

We would like to define Hoare logic rules for Michelson loops, to help us
ensure that we have the proper loop invariant semantics. Michelson, like other
languages, divides looping instructions into two types:

- predicate-based loops (colloquially called `while` loops)
-  sequence-based loops (colloquially called `for` loops)

For brevity and clarity, we document the different parts of the loop as we
will need to refer to them later. Let us recall the syntax of imperative-style
loops using Python's syntax as well as Michelson's loops.

### While Loops

```python
while guard:
  loop_body
```

```michelson
LOOP { loop_body }
```

```michelson
LOOP_LEFT { loop_body }
```

### For Loops

```python
for element in sequence:
  loop_body
```

```michelson
ITER { loop_body }
```

Loop Terminology
----------------

It will be helpful to define some general loop terminology based on our
examples above.

### Loop Header and Body

Syntactically, most loop constructs subdivide into two parts:

-   the _loop header_ which usually precedes the _loop body_ and describes the
    conditions for loop termination
-   the _loop body_ which is the block of the code that will be executed once
    for each loop iteration

Note that, unlike imperative-style `while` loops, Michelson `while` loop
headers do not contain their guard. Instead, the guard is set up:

-   in the expressions preceding the loop
-   inside the loop body

This relates to the point-free nature of stack-based languages like Michelson
which have: (a) no or limited usage of variables; (b) no or limited means to
create new execution contexts that do not refer to the global stack.

### Loop Guard

The most fundamental concept underlying the definition of the loop is its
_guard_, that is, the predicate that must hold true for the loop to continue
execution. For `while` loops, the guard is the predicate directly embedded in
the loop syntax. In the example above, we referred to it as `guard`. For `for`
loops, the loop guard is more implict. We can rewrite a `for` loop as a
`while` loop to make this guard explicit:

```python
index = 0
while index < len(sequence):
  element = sequence[index]
  loop_body
  index += 1
```

Here, we see that the implicit loop guard asserts that the index of our
sequence element is less than the length of the sequence.

Alternatively, we may view the loop guard as either a _continuation condition_
in the postive sense or a _termination condition_ in the negative sense.

### Loop Schema

Given the fact that the guard is somewhat abstracted in the case of
sequence-based loops, we define a related concept that we call the _loop
schema_ as follows:

-   for `while` loops, the loop schema is just the loop guard
-   for `for` loops, the loop schema is the iterated-over sequence

In either case, the loop schema defines how the loop will execute. At runtime,
the schema formal parameters are instantiated with values from the program
environment.

### Loop Object

Each loop iteration considers or focuses a particular program value. We call
this value the _loop iteration object_ or just _loop object_. For `while`
loops, this only exists in a degenerate sense as the boolean value of the loop
guard. For `for` loops, this is the current sequence element which
instantiates the loop's `element` formal parameter.

In either case, in Michelson, the loop object appears on top of the stack
either immediately before entering the loop body (in the case of `LOOP` and
`LOOP_LEFT`) or else immediately after entering the loop body (in the
case of `ITER`).

### Loop Prefix and Suffix

`For` style loops, in addition to having a loop object, also have a loop
_prefix_ and _suffix_ which refer to the subsequences of the iterated-over
sequence occuring before and after the loop object, respectively.

### Loop State

At any point of time during the execution of a loop, its _state_ is given by
the current program environment which binds its body variables to values.

### Loop Invariant

A _loop invariant_ is a formula parameterized by loop variables which
summarizes the effect of executing a loop for zero or more iterations.

In most cases, the invariant formula must be provided by the user; in general,
inferring a loop invariant is undecidable.

### Loop Invariant Checkpoints

A _loop invariant checkpoint_ or _checkpoint_, if the context is clear, refers
to the points during execution when we require that a loop invariant to hold.
They are:

1.  immediately before entering the loop
2.  before each loop iteration starts
3.  after exiting the loop

There are some points worth noting:

-   first two cases can be merged, if we consider the branching point of the
    loop, i.e. the moment when the flow of control may pass into the loop body
    or jump immediately after the loop;
-   the loop invariant is *not* required to hold inside the body.

### Loop Miscellanea

#### Alternative Loop Invariant Checkpoint Definition

One may consider instead loop invariant checkpoints occuring:

1.  before entering the loop
2.  after each loop iteration ends

This has the disadvantage that it is less natural when we consider loops
with `break` statements which allow us to exit the loop midway through a loop
iteration. In that case, it is unclear if the loop iteration has "ended."
On the other hand, the formulation above does not suffer from this ambiguity,
since it is always clear where a loop iteration may begin.

#### Predicate Loops with Effectful Guards

Note that, for technical reasons, we generally assume that evaluating the loop
guard is side-effect free. If it is not, we can always apply a simple loop
transformation to make guard evaluation side-effect free, i.e., a `while` loop
like the one above is equivalent to:

```python
guard_value = guard
while guard_value:
  loop_body
  guard_value = guard
```

Thus, without loss of generality, we assume loop guards are side-effect free.

#### Aside on C-Style For Loops

Note that C-style `for` loops, despite using the `for` keyword, are actually
predicate-based loops which do not necessarily iterate over the elements in
some sequence.

Sometimes the term `foreach` loop is used to refer to the style of
sequence-based loops that we consider above to avoid confusion with C-style
`for` loops.

Stack-based Langauge Notation
-----------------------------

It will be helpful to define some notation which precisely describes
strongly typed, stack-based languages like Michelson.

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

Hoare Rules for Imperative Loop Statements
------------------------------------------

Before considering the Hoare logic rules for Michelson loops, let us review
the rules for imperative style loop statements. We first show the Hoare logic
rule for a `while` loop:

```
      [ Invariant ∧ guard ] body [ Invariant ]
------------------------------------------------------
[ Invariant ] while guard: body [ Invariant ∧ ¬guard ]
```

Using the `for` to `while` loop encoding that we saw above, we can produce
a Hoare logic rule for `for` loops. Note that, since the loop guard mentions
a ghost variable `i` which is only in scope inside the loop.

```
[ Invariant ∧ 0 ≤ i < len(seq) ] elem = seq[i] ; body [ Invariant ]
-------------------------------------------------------------------
 [ Invariant ] for elemᵢ in seq: body [ i = len(seq) ∧ Invariant ]
```

Note that, the above Hoare rules do not properly capture loops with `break`
statements, since such statements allow for early loop termination so that
the loop guard may not necessarily become falsified.

Michelson Loop Semantics
------------------------

Michelson `LOOP`s are different from C while loops.
Let us define it with small step style semantic rules:

```
LOOP Body / (bool False) ..S => {}               / ..S
LOOP Body / (bool True)  ..S => Body ; LOOP Body / ..S
```

where `{}` is the empty/terminal program (sometimes denoted `skip`) and `DROP`
is the Michelson instruction which drops the top stack element.

`ITER` style loops have a similar definition:

```
ITER Body / (seq t {}         ) ..S => {}               /                       ..S
ITER Body / (seq t {Elt ; Seq}) ..S => Body ; ITER Body / (t Elt) (seq t {Seq}) ..S
```

Note that Michelson loops have an implicit loop guard, i.e. the loop guard
requires that the stack top equals `True` or that the stack top is a
non-empty sequence.

### Relation to C-style Loops

It is a useful exercise to compare the Michelson `LOOP` instruction to C-style
"while" loops to understand how we might apply standard Hoare logic rules for
C programs.

Assuming that:

1.  our C code has an implicit, global, typed value stack that we can operate
    on called `stack`
2.  the `stack` exposes a function `pop` which pops the top stack element and
    returns it
3.  the Michelson code `LOOP Body` is well-typed

converting the Michelson code `LOOP Body` to C might look like:

```c
while (pop(stack) == BOOL_TRUE) {
  Body
}
```

Of course, as we explained above, the loop guard here is not amenable to
static analysis because it is not side-effect free. We can convert it into a
side-effect form using a variation of the trick that we mentioned above:

```c
int cond = pop(stack) == BOOL_TRUE;
while (cond) { // 1
  Body
  cond = pop(stack) == BOOL_TRUE;
}
// 2
```

Then, by our discussion above, it is enough to consider invariant checkpoints:

-   before a loop iteration potentially executes at point (1)
-   after loop execution ends (2)

### Hoare Loop Rules for Michelson LOOPs

To ensure soundness, we must properly convert our invariant checkpoints from
the C-style code to Michelson code so that we can do reasoning over pure
Michelson code.

Naturally, this means it is sufficient to check the invariant when:

-   in the top-level proof, when we encounter the `LOOP` instruction but
    *before* descending into loop:
-   in the subproof, after we have symbolically executed the loop body

This corresponds to the following Hoare logic rule:

```
(α)    [ ..S1 ∧ Inv(True, S1) ] Body [ (bool B1') ..S1' ∧ Inv(B1', S1') ]
    ------------------------------------------------------------------------
(β)   [ (bool B) ..S ∧ Inv(B, S) ] LOOP { Body } [ ..S' ∧ Inv(False, S') ]
```

As noted above, our loop rule only implicitly mentions the guard by referring to
the boolean element at the top of stack.
Here we see that our invariant formula `Inv` is parameterized by *the value of
the stack when our current continuation is of the form `LOOP { Body }`*.
This means that our invariant formula may be parameterized by stack values
which no longer exist, as seen on the righthand side of line `(β)`, where the
invariant formula's first argument is the stack element `False` which by then
has already been popped off of the stack.

Usable Program Annotation for Verification
------------------------------------------

Due to the complexity of correctly setting up and applying formal proof rules
to real programs, hand-proofs of complex programs are typically avoided. A
more common approach is to:

1.  annotate program loops/functions/etc. with invariants;
2.  use a computer program to generate the necessary claims corresponding to
    loop invariants, functions, etc. and finally partial program correctness;
3.  use a computerized theorem prover to discharge the claims generated in
    step (2).

However, in practice, invariant annotations are not sufficient, because
invariants may need to refer to state fragments which are not explicitly named
in program code. The solution to this problem is to add _ghost variables_ and
possibly _ghost code_ which we will define below.

### Motivation

Let us first motivate this problem with two examples of Python loops and
invariants:

#### Temporally Hidden Values

Consider the following program which takes a number `num`, multiplies it
times 2, and returns that value as its result.

```python
def times2(num):
  counter = 0
  while num > 0:
    counter += 2
    num -= 1
  return counter
```

It's invariant can be written as:

```
counter = (num₀ - num) * 2
```

where `num₀` refers to the original value of `num` passed to the function.
The problem here is that the program overwrites the value of `num` during loop
execution so that we have no way to refer to its original value in the
"middle" of the loop. We say that its original value was _temporally hidden_.

#### Unnamed Values

Consider the following program which takes a non-empty list and finds its
minimum value:

```python
def minlist(seq):
  assert(len(seq) != 0)
  minimum = seq[0]
  for elt in seq:
    minimum = min(elt, minimum)
  return minimum
```

It's invariant can be written as:

```
( ∀ elt ∈ seqₚ . minimum ≤ elt ) ∧
{ ( ∃ elt ∈ seqₚ . minimum = elt ) ∨ minimum = seq[0] }
```

where `seqₚ` refers to the _loop prefix_ as defined above. Clearly, the list
prefix value is never exposed in the language.

Alternatively, we can write this as follows:

```
( ∀ j ∈ N . j < eltᵢ => minimum ≤ seq[j] ) ∧
{ ( ∃ j ∈ N . j < eltᵢ ∧ minimum = seq[j] ) ∨ minimum = seq[0] }
```

using:

-   the index of the `elt` variable `eltᵢ` and;
-   a function that returns a list value at a certain index (or) a function
    which returns a subsequence from the first element upto to some index.

Again, as above, the element index value is not exposed in the source code.

### Ghost Varibles and Ghost Code

A _ghost variable_ is a program variable that is only added for verification
purposes and is _inaccessible_ to standard program code (alternatively
_only_ accessible from our verification metalanguage).

_Ghost code_ is special code fragments whose results are only used for
verification purposes, and like ghost variables, _inaccessible_ to standard
program code.

In our two examples above, it was enough to add ghost variables `num₀` and
`seqₚ` to define our desired invariants. We conjecture that the following
ghost variables are sufficient for all loop verification purposes:

-   one ghost variable `seqₚ` for each `for` loop sequence `seq` prefix (or)
    one ghost variable `eltᵢ` for each `for` loop element variable `elt`

-   one ghost variable `v₀` for each loop variable `v` coming from a
    super-scope which stores `v`s value immediately before loop execution
