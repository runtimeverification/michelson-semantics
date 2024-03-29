User Guide
==========

Welcome to K-Michelson: a formal verification framework for Michelson using
assertions written in Michelson!

This guide assumes you have successfully installed K-Michelson according to
the instructions in [INSTALL.md](INSTALL.md).

Table of Contents
-----------------

1.  [Table of Contents](#table-of-contents)
2.  [Introduction to K-Michelson](#introduction-to-k-michelson)
3.  [Quick start and examples](#quick-start-and-examples)
4.  [Writing Your Own K-Michelson Tests](#writing-your-own-k-michelson-tests)
5.  [Cross-Validating K-Michelson](#cross-validating-k-michelson)
6.  [K-Michelson Test Grammar Reference](#k-michelson-test-grammar-reference)

Introduction to K-Michelson
---------------------------

The main purpose of this toolkit is to enable Michelson smart contract
developers to test their contracts more easily and thoroughly.

Testing with K-Michelson is easier because it is a local client with a simple
test format (`.tzt`) which is just a slight extension of Tezos scripts (`.tz`) that
you are used to writing. That means you do _not_ need to:

-   spin up a local test net;

-   define a transaction set to produce your desired blockchain state;

-   bake any blocks.

Testing with K-Michelson is more thorough because it enables you to:

-   test at a finer level of granularity than is possible with (`.tz`) tests,
    i.e., Michelson expressions that are not valid scripts can be tested;

-   test `operation` and `big_map` emitting instructions, which is difficult
    to achieve normally because we cannot `COMPARE` such values;

-   verify correctness of _concrete and symbolic_ Michelson programs.
    That is, while traditional testing allows checking pairs of input and output values,
    K-Michelson allows testing over classes of inputs (such as all integers).
    This is explained in greater detail in the [symbolic test below.](#symbolic-tests)

A secondary purpose of K-Michelson is to provide a formal, executable, and
human-readable semantics of the Michelson blockchain programming language
using the K Framework. In this sense, it acts as:

-   a programming language design aid. Since K semantics are easy to modify
    and extend, it can be used to explore modifications to the language. Since
    K semantics are exectuable, it can be used to test potential
    modifications.

-   an additional reference implementation for cross-validation testing
    purposes; by cross-validating alternate Michelson interpreter results,
    we can increase confidence in the language design correctness.

See [Cross-Validating K-Michelson](#cross-validating-k-michelson) for more
information.

## Quick start and examples

The `tzt` test format is similar to the Tezos script format.
A minimal test must include the `code`, `input` and `output` parameters.
The  `code` field specifies the Michelson expression to be tested.
The `input` field specifies the initial stack.
The `output` field specifies the expected final stack.

For example, this test asserts adding `5` to `5` produces `10`.

```tzt
# add_5_5.tzt
code { ADD } ;
input { Stack_elt int 5 ; Stack_elt int 5 } ;
output { Stack_elt int 10 }
```

These `tzt` tests we may be run with the `interpret` subcommand of the `kmich` script:

```sh
./kmich interpret add_5_5.tzt
```

When the `kmich` script finishes executing a test, it returns an exit code
indicating whether an error occurred, i.e., `0` indicates success and a
non-zero code indicates an error.

This script is a thin wrapper around K tools such as `krun`, `kprove`, and
provides a more user friendly, but constrained interface, for running tests.
To see the full range of options avaialable, run:

```sh
./kmich help
```

Additional example concrete unit tests reside in the `/tests/unit` folder in this archive.
The file `/tests/unit/concate_bytes_00.tzt` is a unit test for the `CONCAT`
instruction.

```tzt
code { CONCAT } ;
input { Stack_elt bytes 0xFF ; Stack_elt bytes 0xcd } ;
output { Stack_elt bytes 0xffcd }
```

These may be run using a similar command:

```sh
./kmich interpret tests/unit/concat_bytes_00.tzt
```

The test runner will consider a concrete test passed (i.e. return a `0` exit
code) if and only if:

1.  The test file follows the `.tzt` format properly;
2.  The `code` block is well-typed with respect to the `input` and `output`
    stacks;
3.  Given the provided `input`, after executing the `code`, the Michelson
    interpreter returns exactly the `output` stack.

#### Symbolic tests

Tests may also be run over classes of inputs instead of particular concrete values.
To do so, we may use "symbolic variables" in the `input` and `output` fields such as `$N` in the next example.
We call tests that use symbolic variables "symbolic tests", and all other tests "concrete tests".
Concrete tests are already sufficient to explore a wide range of script behaviors and can be executed much more quickly.
Symbolic tests allow us to perform a full _proof of correctness_ when required, but are much more expensive to run.

In the next test, we assert that `N + 1 > 0` for any natural number `N`:

```tzt
# n_plus_1.tzt
code { ADD ; CMPGT }
input { Stack_elt nat $N ; Stack_elt nat 1 ; Stack_elt nat 0 ; }
output { Stack_elt bool True }
```

Symbolic unit tests are run using the `kmich symbtest` subcommand:

```sh
./kmich symbtest n_plus_1.tzt
```

For tests that use loops, K-Michelson needs each loop to be annotated with a
"loop invariant". Invariants are required to hold when the program reaches the
loop, and also at the end of each loop iteration.

```tzt
input { Stack_elt nat $N0 } ;
code { INT ;
       DUP ;
       PUSH nat 0 ;
       SWAP ;
       GT ;
       LOOP @I {
           PUSH nat 1 ;
           ADD ;
           DIP {
             PUSH nat 1 ;
             SWAP ;
             SUB
           } ;
           DUP 2 ;
           GT
       } ;
       DIP { DROP }
} ;
invariant @I
  { Stack_elt bool $GUARD ; Stack_elt nat $C ; Stack_elt int $N }
  { { PUSH int $N ; PUSH nat $N0 ; INT ; CMPGE ; PUSH int $N ; GE ; AND }
  ; { PUSH int $N ; PUSH nat $N0 ; SUB ; PUSH nat $C ; INT ; CMPEQ }
  ; { PUSH int $N ; GT ; PUSH bool $GUARD ; CMPEQ }
  } ;
output { Stack_elt nat $C } ;
postcondition { { PUSH nat $N0 ; PUSH nat $C ; COMPARE ; EQ } }
```

We can use the `precondition` field (not shown above) to specify test preconditions,
i.e., a list of boolean functions that must hold before `code` executes for the test to succeed;
in particular, preconditions can constrain symbolic input values.
The  `postcondition` field specifies the test postconditions, i.e., a list of boolean
functions that must hold after `code` executes for the test to succeed;
in particular, postconditions can constrian symbolic output values.
The `invariant` field specifies loop invariants for `LOOP` and `LOOP_LEFT`; in
particular, loop invariants are necessary when looping with symbolic
values. One `invariant` field is required for each loop in the code.

Additional example symbolic unit tests reside in the `/tests/symbolic` folder in this archive.
The file `/tests/symbolic/add-party.tzt` is a unit test which tests whether
adding two numbers of opposite parity (an even and odd number) always produces
an odd number:

```tzt
code { ADD } ;
input { Stack_elt nat $I1 ; Stack_elt nat $I2 } ;
output { Stack_elt nat $I3 } ;

precondition { { PUSH nat 2 ; PUSH nat $I1 ; EDIV ;
                 IF_NONE { PUSH bool False }
                         { CDR ; PUSH nat 1 ; COMPARE ; EQ } } ;

               { PUSH nat 2 ; PUSH nat $I2 ; EDIV ;
                 IF_NONE { PUSH bool False }
                         { CDR ; PUSH nat 0 ; COMPARE ; EQ } } } ;

postcondition { { PUSH nat 2 ; PUSH nat $I3 ; EDIV ;
                  IF_NONE { PUSH bool False }
                          { CDR ; PUSH nat 1 ; COMPARE ; EQ } } }
```

These may be run similarly:

```sh
./kmich symbtest tests/symbolic/add-parity.tzt
```

Since K-Michelson is derived from the K Framework, it also allows us to use the
full power `kprove` for more advanced proofs, but a full definition of such
tests is outside the scope of this document. Consult the K Framework
documentation for more details if needed.

The test runner will consider a symbolic test passed (i.e. return a `0` exit
code) if and only if:

1.  The test file follows the extended `.tzt` format properly;
2.  The `code` block is well-typed with respect to the (possibly symbolic)
    `input` and `output` stacks;
3.  Given the provided `input` stack:
    -   each predicate `p` in the `precondition` holds;
    -   symbolically executing the `code` produces actual output stack `S`
    -   the expected `output` stack _matches_ actual output stack `S`
        with assignment α;
    -   each predicate `q` in the `postcondition`, after applying substitution
        α, holds.

Clearly, the satisfaction conditions for symbolic tests are much more complex
than concrete tests. On the flip side, symbolic tests have much more
expressive power than concrete tests; they can _prove_ correctness of a
program for _any_ valid input as opposed to just _some_ valid input.

Writing Your Own K-Michelson Tests
----------------------------------

As mentioned above, K-Michelson supports concrete and symbolic tests.
We consider both cases separately.

### Concrete Tests

Writing your own concrete tests is as simple as writing three fields:

1. an input stack
2. a code block to execute using (1)
3. an expected output stack after executing (2)

An easy way to get started is to copy the concrete test template below and
replace each instance of `FIXME` with a valid expression of the correct type
such that the resulting Michelson expression in the `code` field is well-typed
with respect to the input stack and output stack.

See the [field type reference](#field-types) for more information.

```tzt
input { FIXME } ;
code { FIXME } ;
output { FIXME }
```

For example, suppose we have the following simple concrete test:

```tzt
# simple_v1 - passing test
input { Stack_elt int 5 ; Stack_elt int 9 } ;
code { DUP ; DIP { CMPLT } ; SWAP ; IF { PUSH int 2 } { PUSH int -2 } ; ADD } ;
output { Stack_elt int 7 }
```

In words, this test does the following:

1. Check if the first stack element is less than the second;
2. If yes, return the first stack element plus two;
3. Otherwise, return the first stack element minus two.

This test passes according to our definition of successful concrete test
because, given the input stack, we observe that `5 < 9`, and so obtain the
final output stack `5 + 2` which equals `7`.

When we swap the two input stack elements (as shown below), it also succeeds:

```tzt
# simple_v2 - passing test with swapped stack
input { Stack_elt int 9 ; Stack_elt int 5 } ;
code { DUP ; DIP { CMPLT } ; SWAP ; IF { PUSH int 2 } { PUSH int -2 } ; ADD } ;
output { Stack_elt int 7 }
```

This is because, given the input stack, we see that `9 < 5` is _not_ true, and
so obtain the final output stack `9 - 2` which also equals `7`.

On the other hand, the following test fails:

```tzt
# simple_v3 - failing test
input { Stack_elt int 5 ; Stack_elt int 9 } ;
code { DUP ; DIP { CMPLT } ; SWAP ; IF { PUSH int 2 } { PUSH int -2 } ; ADD } ;
output { Stack_elt int 8 }
```

This is because, our final actual output stack is `7` but our expected output
stack is `8`.

### Symbolic Tests

For symbolic tests, the situation is slightly more complex. As in the concrete
case, the three fields above are required. However, in the case of symbolic
tests, the `input` and `output` fields are slightly more flexible, because
they support symbolic as well as concrete stacks. Before we continue, it will
be helpful to review what concrete and symbolic stacks look like and what
we can do with them.

#### Symbolic Stacks and Matching

In K-Michelson, we use the following stack notation:

```
{ Stack_elt type-1 value-1 ; Stack_elt type-2 value-2 ; ... }
```

where `value-1` occurs at the top of the stack, `value-2` is
the second from the top, etc. We divide our stacks into
concrete and symbolic variants. Let us describe them below.
As a simple example, the following stack is _concrete_:

```
# Concrete Stack A
{ Stack_elt bool true ; Stack_elt int -15 ; Stack_elt nat 87 }
```

On the other hand, the following stack is _symbolic_ (we interchangeably use
the word _abstract_):

```
# Abstract Stack B
{ Stack_elt bool $T ; Stack_elt int -15 ; Stack_elt nat $P }
```

Why? Because it has two variables, `T` and `P`. Concrete stacks have no
variables, by definition.

Furthermore, we say that a stack called `B` _matches_ a stack called `A` if
and only if:

1.  each concrete value in stack `B` is equivalent to a concrete element in
    the corresponding position in stack `A`
2.  each variable in stack `B` matches the type of the element in the
    corresponding position in stack `A`.
3.  if a variable in stack `B` occurs more than once, the corresponding values
    at each position in `A` are equivalent.

In the example above, `B` does indeed match `A` becuase:

1.  The only concrete value `-15` with type `int` occurs as position 2
    in both stack `B` and stack `A`.

2.  Variable `T` in stack `B` at position 1 has type `bool`. The concrete
    value `true` in stack `A` at position 1 also has type `bool`.
    Similarly, the variable `P` in stack `B` at position 3 has type `nat` and
    the element at position 3 in stack `A` also has type `nat`.

3.  Since no variables are duplicated in this example, this condition
    holds trivially.

#### Writing Tests by Abstraction

A common way that symbolic tests are written is through _abstraction_, that is,
we replace concrete values in the input and output stacks with more general symbolic
values. For example, suppose we want to abstract the simple concrete test we
saw above:

```tzt
# simple_v1 - passing test
input { Stack_elt int 5 ; Stack_elt int 9 } ;
code { DUP ; DIP { CMPLT } ; SWAP ; IF { PUSH int 2 } { PUSH int -2 } ; ADD } ;
output { Stack_elt int 7 }
```

##### Input Stack Abstraction

We could try abstracting the test's input stack as follows:

```tzt
# simple_v4 - failing test with abstracted input stack
input { Stack_elt int $I ; Stack_elt int $J } ;
code { DUP ; DIP { CMPLT } ; SWAP ; IF { PUSH int 2 } { PUSH int -2 } ; ADD } ;
output { Stack_elt int 7 }
```

Clearly, at this point, the test is incorrect. Why? In the abstracted stack,
information about the relationship between the two stack values is lost.
This means, for one thing, that both branches of the `IF` are possible, depending on whether `I < J` is true or false.

Additionally, we need to sure that for any two integers `I` and `J` that:

-   if `I < J` is true, then `7 = I + 2`
-   if `I < J` is _not_ true, then `7 = I - 2`

But we can find examples where this does not follow. For example, let `I`
equal `4`.

- if `J` equals `5`, then `I < J` is true but `7 != 4 + 2`
- if `J` equals `2`, then `I < J` is _not_ true but `7 != 4 - 2`

For this reason, we need to introduce _preconditions_ to constrain our input
stack. For example, we may want to reintroduce the constraint that the second
stack element is greater than the first. We can add this constraint using a
precondition:

```tzt
# simple_v5 - failing test with abstracted and constrained input stack
input { Stack_elt int $I ; Stack_elt int $J } ;
precondition {
               { PUSH int $J ; PUSH int $I ; CMPLT }
             } ;
code { DUP ; DIP { CMPLT } ; SWAP ; IF { PUSH int 2 } { PUSH int -2 } ; ADD } ;
output { Stack_elt int 7 }
```

Note that when we `PUSH` elements onto our stack, they appear in reverse
order relative to our stack notation, i.e., if we have the stack:

```
{ Stack_elt int 1 ; Stack_elt int 0 }
```

and then perform a `PUSH int 2`, we would have the result stack:

```
{ Stack_elt int 2 ; Stack_elt int 1 ; Stack_elt int 0 }
```

With this explanation, bbserve that, given this precondition, the
second branch of the `IF` expression is no longer viable.
Thus, we have simplified our test state space.
However, our test is still incorrect, since the counterexample that we saw
above where `I` equals `4` and `J` equals `5` still holds.

To resolve this problem, we also need to abstract our output stack.

##### Output Stack Abstraction

We can abstract our output stack using the same mechanism we saw for
abstracting our input stack:

```tzt
# simple_v6 - passing test with:
# - abstracted and constrained input stack
# - abstracted output stack
input { Stack_elt int $I ; Stack_elt int $J } ;
precondition {
               { PUSH int $J ; PUSH int $I ; CMPLT }
             } ;
code { DUP ; DIP { CMPLT } ; SWAP ; IF { PUSH int 2 } { PUSH int -2 } ; ADD } ;
output { Stack_elt int $K }
```

Here, the concrete value `7` was replaced by the variable `K`. Perhaps
surprisingly, this test now _passes_. Why? There is a fundamental asymmetry
between abstracting the input stack and the output stack.

When we abstract the input stack, we need to check that the test passes for
_all_ possible instances of our input variables.

When we abstract the expected output stack, we only need to check that the
test passes for _any_ possible instance of our output variables.

The upshot of all of this is: for this version of our symbolic test, the test
will pass if it well-typed, that is, if it produces a singleton stack with an
integer.

We may rightly feel that this test is too weak, since we can type-check
Michelson contracts directly using the type-checker; a complex test like
ours is entirely unnecessary.

Instead, much as we did for input stacks, we can constrain our output stack
with _postconditions_. Let's add a postcondition that makes our test more
meaningful:

```tzt
# simple_v7 - passing test with:
# - abstracted and constrained input stack
# - abstracted and constrained output stack
input { Stack_elt int $I ; Stack_elt int $J } ;
precondition {
               { PUSH int $J ; PUSH int $I ; CMPLT }
             } ;
code { DUP ; DIP { CMPLT } ; SWAP ; IF { PUSH int 2 } { PUSH int -2 } ; ADD } ;
output { Stack_elt int $K } ;
postcondition {
                { PUSH int $I ; PUSH int $K ; CMPGT }
              }
```

This postcondition (correctly) asserts that our output value `K` has a value
which is greater than our input argument `I`. It must hold because, by our
precondition, the exact value of our output stack will be `I + 2`.
When we substitute `K` for `I + 2` by stack matching, we see that `I + 2 > I`,
as required.

##### Aside on Stack Abstraction and Constraints

In general, due to the asymmetry between abstracted input and output stacks:

-   abstracting our input stack makes a test _less_ likely to pass;
-   abstracting our output stack makes a test _more_ likely to pass.

In the end, if we fully abstract both our input and output stacks with unique
and non-repeated variable names, a successful test run just means that our
Michelson expression was well-typed.

To compensate for the imprecision introduced by abstraction, we can add
constraints via preconditions and postconditions, as we did above. These
constraints reverse the trends above, that is:

-   adding more constraints via preconditions to an abstract input stack makes
    a test _more_ likely to pass:
-   adding more constraints via postconditions to an abstract output stack
    makes a test _less_ likely to pass.

One way to think about constraints is that they make an abstract stack
_less abstract_, because the constraints forbid some assignments of variables
to values that would otherwise be permitted.

For example, the abstract stack `{ Stack_elt int $I }` permits `I` to be any
integer, including, for example, `-5`. Adding the constraint
`{ PUSH int $I ; ISNAT }`
means that all negative (non-natural) values are forbidden, and thus, the
assignment of `I` to `-5` is forbidden.

#### Symbolic Tests with Loops

Note that our simple test did not have any loops. That was intentional. Adding
loops almost always complicates the analysis and verification of programs. The
problem can observed even in short programs:

```tzt
# loop-parity.tzt - test which infinitely loops
input { Stack_elt nat $N_INIT ; Stack_elt bool True } ; # N E
code { DUP ;                                            # N N E
       INT ; GT ;                                       # (N>0) N E
       LOOP { DIP { NOT } ;                             # N ¬E
              PUSH int 1 ; SWAP ; SUB ;                 # (N-1) ¬E
              DUP ;                                     # (N-1) (N-1) ¬E
              GT                                        # (N-1>0) (N-1) ¬E
            } ;                                         # N E
       DROP                                             # E
     } ;
output { Stack_elt bool $EVENNESS_RESULT }
```

The code for program is only 7 lines long.
For readability, we have annotated each line with a representation of the
stack which results from executing that line of code, where the leftmost
item represents the stack top and the rightmost item represents the stack
bottom.
The program consists of a simple loop which counts down from `N_INIT` to 0.
Each time the loop iterates once, it flips the value of the boolean `E` (for evenness) in the
second position of the input stack.

Here is our burning question: is the final value of that boolean, i.e., the
value of the variable `$EVENNESS_RESULT`, `True` or `False`?

Given just the information in the test above, it seems that there may be cases when it is `True`, and others when it is `False`.
With some careful thought, we may realize that if we additionally know whether
the value `N` is even or odd, we can compute the final value of the program.
That is, the result is `True` if `N` is even and `False` if `N` is odd.
We can express this requirement as a postcondition:

```
# loop-parity.tzt - test which infinitely loops with
# - postcondition
input { Stack_elt nat $N_INIT ; Stack_elt bool True } ; # N E
code { INT ; DUP ;                                      # N N E
       GT ;                                             # (N>0) N E
       LOOP @I { DIP { NOT } ;                          # N ¬E
                 PUSH int 1 ; SWAP ; SUB ;              # (N-1) ¬E
                 DUP ;                                  # (N-1) (N-1) ¬E
                 GT                                     # (N-1>0) (N-1) ¬E
               } ;                                      # N E
       DROP                                             # E
     } ;
output { Stack_elt bool $EVENNESS_RESULT } ;
postcondition { # EVENNESS_RESULT = (N % 2) == 0
                { PUSH nat 2 ; PUSH nat $N_INIT ;
                  EDIV ; ASSERT_SOME ; CDR ;
                  PUSH nat 0 ; CMPEQ ;
                  PUSH bool $EVENNESS_RESULT ; CMPEQ } }
```

Unfortunately, even after adding a postcondition, we still cannot complete the
proof.
This is because the K-Michelson verifies
programs using symbolic execution, i.e., by executing the program all
possible ways and checking that we get the result we want each time.
In particular, this means the framework does *not know how many* times to
execute a loop.
Depending on the initial value of variable `N`, we may need to execute the loop body 100 times, or 500.
This means we will never be sure we have executed the loop *all possible ways*, and the runner will get stuck executing the program for larger and larger values of `N`.

How can we escape this endless cycle?
It seems clear that we shouldn't need to execute the loop in all possible ways --
we can argue by induction that the post-condition holds. 
We claim that the current value of $E$ equals `True` if the difference between `N` and it's current value is even, otherwise it is `False`.
When `N`'s initial value is `0` its clear that this since `E` is initially `True`, and the body is not executed.
When `N` is greater than $0$, the body reduces the value of `N` by `1` and flips `E`.
By applying the inductive hypothesis, our claim must hold.

This is called a _loop invariant_ . It allows us to summarize the properties of an loop we care about, so that we do not need to execute it to arbitrary depth.
in a finite way.

In K-Michelson, we write loop invariants in a two step process:

1.  in the `code` field, we must annotate each symbolic loop with a unique
    name, e.g. `@MYLOOP`;
2.  for each named symbolic loop, we introduce an `invariant` field of the
    form `invariant @MYLOOP` which defines the loop invariant for loop
    `@MYLOOP` using a stack predicate list (see the
    [field types reference](#field-types) for more info).

In general, it is impossible to develop a method that will always give us the
loop invariants that we want, so writing good loop invariants will always be
somewhat of an art.

As an example, we can extend our previous test case to add a loop invariant:

```
# loop-parity.tzt - failing test with
# - postcondition
# - loop invariant
input { Stack_elt nat $N ; Stack_elt bool True } ; # N E
code { INT ; DUP ;                                 # N N E
       GT ;                                        # (N>0) N E
       LOOP @I { DIP { NOT } ;                     # N ¬E
                 PUSH int +1 ; SWAP ; SUB ;        # (N-1) ¬E
                 DUP ;                             # (N-1) (N-1) ¬E
                 GT                                # (N-1>0) (N-1) ¬E
               } ;                                 # N E
       DROP                                        # E
     } ;
output { Stack_elt bool $EVENNESS_RESULT } ;
postcondition { # EVENNESS_RESULT = (N % 2) == 0
                { PUSH nat 2 ; PUSH nat $N ;
                  EDIV ; ASSERT_SOME ; CDR ;
                  PUSH nat 0 ; CMPEQ ;
                  PUSH bool $EVENNESS_RESULT ; CMPEQ } } ;
invariant @I
          { Stack_elt bool $GUARD ; Stack_elt int $CURRENT ; Stack_elt bool $EVENNESS }
          {  # CURRENT >= 0
           ; { PUSH int 0 ; PUSH int $CURRENT ; CMPGE }

             # GUARD = CURRENT > 0
           ; { PUSH int $CURRENT ; GT ; PUSH bool $GUARD ; CMPEQ }

             # EVENNESS = (N - CURRENT) % 2 == 0
           ; { PUSH nat 2 ;
               PUSH int $CURRENT ; PUSH nat $N ; INT ; SUB ;
               EDIV ; ASSERT_SOME ; CDR ;
               PUSH nat 0 ; CMPEQ ; PUSH bool $EVENNESS ; CMPEQ }
          }
```

The invariant has three parts:

1.  a name written with an at-sign `@` followed by an alphanumeric pattern
    (above the name is `@I`)

2.  a stack pattern describing the stack shape when the looping instruction is
    encountered (above the stack pattern is
    `{ Stack_elt bool $GUARD ; Stack_elt int $CURRENT ; Stack_elt bool $EVENNESS }`)

3.  an invariant specification which is a list of Michelson blocks which
    consume an empty stack and produce a stack with a single boolean value on
    top (above the invariant specification has three clauses representing the
    predicates `CURRENT >= 0`, `GUARD = CURRENT > 0`, and
    `EVENNESS = (N - CURRENT) % 2 == 0`);
    note that invariant specification blocks may reference any variables which
    are bound by the stack pattern

The most important part is (3) whose purpose is to serve as a *finite* summary
of the infinite loop behavior.
The exact criteria that are needed for a loop invariant are beyond the scope
of this tutorial.
Intuitively, a loop invariant defines *what must be true immediately before
the loop and also after each loop iteration*.
We then can replace the loop execution by its summarized logical form in our
proof.

In the above example, we carefully chose our three predicates in our invariant
specification to precisely describe our loop behavior.
Let us consider each of them briefly below:

1.  `CURRENT >= 0` - This invariant follows from a straightforward analysis of
    the loop; since the value of `CURRENT` varies between the positive-valued
    `N` and `0`, it must always be greater than `0` during loop execution.

2.  `GUARD = CURRENT > 0` - This invariant defines the value of loop guard
    variable (i.e., the loop will continue as long as the guard is true); we
    see that the `GUARD` holds if and only `CURRENT > 0`.

3.  `EVENNESS = (N - CURRENT) % 2 == 0` - This is the key component of the
    invariant; it describes the partially computed `EVENNESS_RESULT`.
    After any number of loop iterations, `EVENNESS` will be true if and only
    if `N - CURRENT` is even.

To complete our proof, we must show that our postcondition
`EVENNESS_RESULT = (N % 2) == 0` is true.
We can do so by the following chain of deduction:

1.  Since we know that the loop will terminate when `GUARD` is false, putting
    invariants (1) and (2) together lets us determine that when the loop
    terminates, `CURRENT = 0`.

2.  By plugging this assignment into (3), we know that when the loop
    terminates, `EVENNESS = (N - 0) % 2 == 0` which reduces to
    `EVENNESS = (N % 2) == 0`.

3.  The value of `EVENNESS_RESULT` is identical to `EVENNESS` at loop
    termination, i.e., `EVENNESS_RESULT = EVENNESS`.

4.  Using the assignment dervied in (3), we can replace each occurence of
    `EVENNESS_RESULT` in our postcondition by `EVENNESS`. But then, note that
    this is exactly the fact we derived in (2).
    [QED](https://en.wikipedia.org/wiki/Q.E.D.).

##### Final Words on Loop Invariants

There is one other detail one should be aware of when write loop invariants.
In particular, one must be careful to check that the loop invariant is truly
an invariant, i.e., that the invariant always holds immediately before the
loop and after each loop iteration.
For more information on this advanced topic, see our
[primer on loop invariants](loop-invariants.md)

Cross-Validating K-Michelson
----------------------------

As part of its goal to be a human-readable language semantics and aid
programming language design, K-Michelson supports a cross-valdiation test
mode. In this mode, concrete unit tests (under `/tests/unit`) are executed
in the following manner:

1.   The unit test is executed by K-Michelson.

2.   The same unit test is executed by `tezos-client`, the reference Michelson
     interpreter.

3.   The results are compared. If they are identical, the test succeeds;
     otherwise, the test fails.

To use this mode, use the `/lib/tezos-client-unit-test` command, e.g.,

```sh
./lib/tezos-client-unit-test tests/unit/concat_bytes_00.tzt
```

Note that this mode has additional dependencies and build requirements
compared to the concrete or symbolic unit tests. See [INSTALL.md](INSTALL.md)
for details.

K-Michelson Test Grammar Reference
----------------------------------

Type (1)-(2) tests use the `.tzt` format, first defined in
[here](https://gitlab.com/tezos/tezos/-/merge_requests/1487/diffs).
The `.tzt` format is a slight extension of the `.tz` format.
For convenience, we briefly explain the `.tzt` grammar here.

The `.tzt` format can be understood as schemas applied to the
[Micheline format](http://tezos.gitlab.io/whitedoc/micheline.html).
More abstractly, it is an unordered set of typed fields that optionally
contain associated data.

#### Michelson Grammar Extensions

In the standard Michelson grammar, there are no literals for the `operation`
and `big_map` types. The `.tzt` format adds support for these literals because
tests may need to refer to these kinds of values.

1.  `operation` literals have the following form:

    - `Create_contract contract (option key_hash) mutez T byte` where
      `contract`'s storage type is `T`

    - `Transfer_tokens T mutez address byte` where the `contract` value at
       `address` has parameter type `T`

    - `Set_delegate (option key_hash) byte`

    where in each case the final `byte` argument represents a cryptographic
    nonce.

2.  `big_map` literals have the following form:

    - a natural number identifier referring to an indexed `big_map` in
      the `big_maps` field (see description below)

      Ex. `2`

    - a standard `map` literal

      Ex. `{ Elt 1 True ; Elt 3 False }` (literal of type `big_map nat bool`)

    - a pair of a `big_map` identifier and a map literal (the map literal
      represents a difference list)

      Ex. `Pair 2 { Elt 1 False }` (refers to the `big_map`
      `{ Elt 1 False ; Elt 3 False }` if `2` identifies the map
      `{ Elt 1 True  ; Elt 3 False }`)

#### Field Types

We list the set of possible types below. Note that each field can accept only
_one_ type.

-   expression - an arbitrary Michelson expression

    Ex. `{ PUSH int 1; PUSH int -2; ADD }`

    Ex. `{}`

-   stack - a typed Michelson stack which is equivalent to a list of stack
    stack elements of the form `Stack_elt type value`

    Ex. `{ Stack_elt int -1 ; Stack_elt (set int) { Elt 0 ; Elt 3 } }`

    Ex. `{ Stack_elt bool True }`

    Ex. `{}`

-   result stack - a typed Michelson stack or a stack representing a failed
    computation which comes in four forms:

    Ex. Any stack literal

    Ex. `(Failed D)` where `D` is any Michelson data literal

    Ex. `(MutezOverflow I J)` where `I` and `J` are mutez literals

    Ex. `(MutezUnderflow I J)` where `I` and `J` are mutez literals

    Ex. `(GeneralOverflow I J)` where `I` and `J` are number literals

-   type - any Michelson type

    Ex. `bool :flag`

    Ex. `pair nat int`

-   timestamp - a Michelson timestamp literal

    Ex. `0`

    Ex. `"2019-09-26T10:59:51Z"`

-   mutez - a Michelson mutez literal

    Ex. `0` (minimum value)

    Ex. `9223372036854775807` (maximum value)

-   address - a Michelson address literal

    Ex. `"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"`

    Ex. `"KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi"`

-   chain_id - a Michelson chain_id literal

    Ex. `0x00000000` (minimum value)

    Ex. `0xFFFFFFFF` (maximum value)

-   contract type map - a map of contract addresses to their parameter types
    where each map entry has the form `Contract address type`

    Ex. `{ Contract "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi" nat ;
           Contract "KT1HgAM3pNzkqd1Ps8iunMGNopFRFKHWoPdW" (list int) }`

    Ex. `{ Contract "KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi" nat }`

    Ex. `{}`

-   big_map index map - a map of natural number indices to big_map literals
    where each map entry has the form `Big_map nat type type map_literal`

    Ex. `{ Big_map 1 string int  { Elt "bar" 1 ; Elt "foo" 2 } ;
           Big_map 2 string bool { Elt "a" True ; Elt "b" False } }`

    Ex. `{}`

-   predicate list - a list of Michelson predicates where each predicate
    has the form `{ expression }` where the expression takes an empty input
    stack and produces a stack containing a single boolean

    Ex. `{ { PUSH int $I ; EQ } ;
           { PUSH list int $L ; SIZE ; GT } }`

    Ex. `{}`

    Ex. `{ { ADD ; EQ } }` (**invalid**: input stack type non-empty)

    Ex. `{ { PUSH nat 0 ; PUSH bool True } }` (**invalid**: output stack is
    not single boolean value)

-   stack predicate list - a pair of a stack binder, i.e. a fully symbolic
    stack, and a predicate list such that the bound stack variables are
    available in the predicate list

    Ex.
    ```
    { Stack_elt bool $CONT ; Stack_elt nat $N }
    { { PUSH nat $N ; INT ; GT ; PUSH bool $CONT ; CMPEQ } }
    ```
    (this stack predicate list describes, e.g., a loop variable `N` with loop
    continuation guard `CONT`, such that the loop will continue to iterate
    while `N > 0`)

#### Field List

The set of required fields and their types is listed below:

-   `code` (expression) the Michelson code to be tested

-   `input` (stack) the input stack supplied to the code in the `code` field

-   `output` (result stack) the expected output stack of the `code` field
    given the input stack defined in `input`

The format also allows for optional fields which have default values. The set
of optional fields is defined below:

-   `parameter` (type, default `unit`) defines the contract type pushed by the
    `SELF` instruction

-   `now` (timestamp, default `0`) defines the result timestamp of the `NOW`
    instruction

-   `sender` (address, default `"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"`)
    defines the result of the `SENDER` instruction

-   `source` (address, default `"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"`)
    defines the result of the `SOURCE` instruction

-   `chain_id` (chain_id, default `0x7a06a770`) defines the result of the
    `CHAIN_ID` instruction

-   `self` (address, default `"KT1BEqzn5Wx8uJrZNvuS9DVHmLvG9td3fDLi"`) defines
    the address of the contract pushed by the `SELF` instruction

-   `amount` (mutez, default `0`) defines the amount of mutez returned by the
    `AMOUNT` instruction

-   `balance` (mutez, default `0`) defines the amount of mutez returned by the
    `BALANCE` instruction

-   `other_contracts` (contract type map, default `{}`) defines the set of
    other contracts which are available for this contract to invoke.

-   `big_maps` (big_map index map, default `{}`) defines a mapping from
    natural number indices to `big_map` literals; this allows us to write
    `big_map` literals in the other fields as just an index.

    This feature is helpful when the `big_map` literal is large.

-   `precondition` (predicate list, default `{}`) defines a list of predicates
    which constrain any symbolic values in the `input` stack.

-   `postcondition` (predicate list, default `{}`) defines a list of predicates
    which constrain any symbolic values in the `output` stack.

-   `invariant @L` (stack predicate list, default `{} {}`) defines a loop
    invariant for a `LOOP` or `LOOP_LEFT` instruction annotated with a
    special `@L` annotation.

The format admits some fields which are _ignored_ for compatibility reasons.
The set of ignored fields is defined below:

-   `storage` (type) defines the storage type of the contract in `.tz` files
