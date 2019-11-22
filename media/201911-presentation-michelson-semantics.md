---
title: 'K-Michelson'
subtitle: 'Semantics of Michelson in the \K framework'
author:
-  'By Andrew Miranti and Brandon Moore.'
-  'Presentation by Rikard Hjort'
date: 'November 22, 2019'
institute:
-   Runtime Verification, Inc.
theme: metropolis
fontsize: 10pt
header-includes:
-   \usepackage{color}
-   \usepackage{fancyvrb}
-   \fvset{commandchars=\\\{\}}
-   \newcommand{\K}{$\mathbb{K}$~}
-   \newcommand{\lK}{$\mathbb{K}$}
---

Overview
--------

> 1. What is K-Michelson?
> 2. Introduction to \K
> 3. Deep-dive: What the semantics look like
> 4. Demo: Proving things

. . .

\vspace{1em}

Please ask questions as we go.

K-Michelson: Intro and road map
========================

K-Michelson
-----

> * K-Michelson is the project name for **specifying** Michelson in \lK.
> * \K is a framework for creating **executable specifications** of programming languages.
> * \K uses rewrite based semantics, very similar to the current documentation.
> * The goal is to use the executable spec to **formally verify** aspects of blockchain runtimes and smart contracts.
> * We have semantics for other blockchain languages. KEVM is mature for proving, and KWasm is being ramped up.

Design
------

A very faithful translation of Michelson spec (\K and Michelson both use rewrite semantics). Some differences:

. . .

>  - Official spec uses `iff` in side conditions, in a way that is not obviously computational. In \K we use function calls instead.
>  - Makes some state explicit that is implicit in the docs. Example: Mutez balance.
>  - Separation of typing semantics and execution semantics.

Goals
-----

- Verify contracts
- We would like to build a repository of verified code using K-Michelson. There is such a repository for KEVM:

[![](media/img/github-verified-contracts-screenshot.png)](https://github.com/runtimeverification/verified-smart-contracts)


Status
------

> * Bulk of the semantics are done.
> * Incorporating protocol changes as they happen.
> * Macros, annotation are underway, bytes operations left out for now.

. . .

### Successes

. . .

> * Performant interpreter (LLVM).
> * Can complete some non-trivial proofs (Haskell).

. . .

Both the prover and interpreter are derived from the same specification!

. . .

\center\huge DEMO!

Is it fast?
-----------

```
$contract {
   code { LEFT nat ;
          LOOP_LEFT { DUP ; DIP { CDR } ; CAR ; DUP ;
                      DIP { ADD } ; PUSH nat 1 ; SWAP ;
                      SUB ; ISNAT ;
                      IF_NONE
                        { RIGHT pair nat nat }
                        { PAIR ; LEFT nat }
                    } ;
          NIL operation ;
          PAIR } ;
   parameter nat ;
   storage nat ; } ;
...
$param 10 ^Int 5 ;     <--- 100,000 iterations.
$storage 0 ;
```

Reasonably fast!
----------------

About 5.5 seconds, roughly half spent parsing (which is multi-core).

```
$ time ./michelson.py run demo/sum-to-n.tz
  <k>
    .
  </k>
  <stack>
    Pair .List 5000050000
  </stack>
  ...
```

\vspace{1em}

12.66s user 1.14s system 248% cpu 5.566 total


Introduction to \K
=================

The Vision: Language Independence
---------------------------------

![\K Tooling Overview](media/img/k-overview.png)


\K Tooling/Languages
-------------------

### Tools

-   Parser
-   Interpreter
-   Debugger
-   Reachability Logic Prover [@stefanescu-park-yuwen-li-rosu-reachability-prover]
-   ...

. . .

### Languages

-   Java 1.4 - 2015 [@bogdanas-rosu-k-java]
-   C11 - 2015 [@hathhorn-ellison-rosu-k-c]
-   KJS - 2015 [@park-stefanescu-rosu-k-js]
-   KEVM - 2018 [@hildenbrandt-saxena-zhu-rosu-k-evm]
-   KWasm - <https://github.com/kframework/wasm-semantics>
-   KLLVM <https://github.com/kframework/llvm-semantics>
-   KX86-64 <https://github.com/kframework/X86-64-semantics>
- In progress (external groups):
   - Solidity <https://github.com/kframework/solidity-semantics>
   - Rust

Parts of a \K specification
--------------------------

A language spec in \K consists of 3 things

* Syntax
* Configuration ("state")
* Operational semantics as **rewrite rules**

\K Specifications: Syntax
------------------------

Concrete syntax built using EBNF style:

```k
  syntax Instruction ::= Block
                       | "DROP"
                       | "DUP"
                       | "SWAP"
                       | "PUSH" Type Data
                       | "SOME"
                       | "NONE" Type
                       ...

  syntax InstructionList ::= List{Instruction, ";"}

  syntax Block ::= "{" InstructionList "}"
```

\K Specifications: Configuration
-------------------------------

Tell \K about the structure of your execution state.

```k
  configuration <k> $PGM:Pgm </k>
                <stack> .K </stack>
                <paramtype> #NotSet </paramtype>
                <storagetype> #NotSet </storagetype>
                <balance> #Mutez(0) </balance>
                <amount> #Mutez(0) </amount>
                <now> #Timestamp(-1) </now>
                <myaddr> #Address(-1) </myaddr>
                <knownaddrs> .Map </knownaddrs>
                <sourceaddr> #Address(-2) </sourceaddr>
                <senderaddr> #Address(-3) </senderaddr>
                <chainid> #ChainId(0) </chainid>
```

. . .

> - `<k>` will contain the initial parsed program.
> - Based on the values in the program invocation, the other cells get populated.


\K Specifications: Transition Rules
----------------------------------

Using the above grammar and configuration:

. . .

### Push to Stack

\begin{Verbatim}[]
    rule <k> PUSH _ X => . ... </k>
         <stack> . => X ... </stack>
\end{Verbatim}

. . .

> - `=>` is the rewrite arrow.
- `_` is a wildcard (matches any value).
> - Words in all caps are variables.
> - We match on and rewrite the front of the cell contents, and `...` matches the rest of the cell.
> - We don't need to mention the cells we don't use or modify.
> - Rewriting to `.` is erasing.
> - We can rewrite several cells at once.


\K Specifications: Transition Rules
----------------------------------

### Helper functions:

\begin{Verbatim}[]
  syntax List ::= #ReverseList(List) \textcolor{blue}{[function]}

  syntax List ::= #ReverseListAux(List, List) \textcolor{blue}{[function]}

  rule #ReverseList(L) => #ReverseListAux(L, .List)
  rule #ReverseListAux(ListItem(L1) Ls, Acc) => #ReverseListAux(Ls, ListItem(L1) Acc)
  rule #ReverseListAux(.List, Acc) => Acc
\end{Verbatim}

. . .

\vspace{1em}

> - The `[function]` annotation means the rule applies regardless of context.
> - Lets you do regular functional programming.

\K Specifications: Transition Rules
----------------------------------

### Binary operators

\begin{Verbatim}[]
  rule <k> SLICE => . ... </k>
       <stack> O ~> L ~> S => Some substrString(S, O, O +Int L) ... </stack>
       \textcolor{blue}{requires} O >=Int 0
        andBool L >=Int 0
        andBool (O +Int L) <Int lengthString(S)

  rule <k> SLICE => . ... </k>
       <stack> O ~> L ~> S => None ... </stack>
       \textcolor{blue}{requires} notBool(O >=Int 0
        andBool L >=Int 0
        andBool (O +Int L) <Int lengthString(S))
\end{Verbatim}

\vspace{1em}

. . .

> - `requires` specifies side conditions.
> - \K builtin predicates and operators are not polymorphic, so they are specialized by type, e.g. `>=Int, notBool`

\K Specifications: Transition Rules
----------------------------------

### Maps

\begin{Verbatim}[]
  syntax Data ::= Map

  rule <k> EMPTY_MAP _ _ => . ... </k>
       <stack> . => \textcolor{blue}{.Map} ... </stack>

  rule <k> GET => . ... </k>
       <stack> X ~> M => Some \textcolor{blue}{\{M[X]\}:>Data} ... </stack>
       requires X in_keys(M)
\end{Verbatim}

\vspace{1em}

. . .

> - `Map` is a builtin data structure, which is an associative-commutative key-value pairs.
> - `M[X]` fetches an element in the Map.
> - `{V}:>T` is a cast of value `V` to type `T`.

\K Specifications: Transition Rules
----------------------------------

### Maps

\begin{Verbatim}[]
  rule <k> UPDATE => . ... </k>
       <stack> K ~> Some V ~> M:Map => \textcolor{blue}{M[K <- V]} ... </stack>

  rule <k> UPDATE => . ... </k>
       <stack> K ~> None ~> M:Map => \textcolor{blue}{M[K <- undef]} ... </stack>
\end{Verbatim}

\vspace{1em}

. . .

- `M[K <- V]` is a map update. The special value `undef` clears the entry.

Repo Tour
=========

Repo layout
---------

* `michelson.k`: The semantics.
* `michelson-syntax.k`: Separate module for the grammar.
* `test-michelson.k` and `test-michelson-syntax.k`: Extra syntax and semantics for unit testing.

Simple Instructions
-------------------

![](media/img/michelson-spec-simple.png)


More Complex Instructions
-------------------

![](media/img/michelson-spec-complex.png)

Proving
=======

Verifying Michelson programs
----------------------------

> 1. From the K-Michelson semantics, \K generates a parser and a deductive program verifier.
> 2. A verification claim is written like a rewrite rule. `rule A => B` should be read as "`A` will eventually always evaluate to `B`".
> 3. The automatic prover tries to construct a proof (with the help of Z3 to check constraint satisfiability) that every possible execution path starting in `A` eventually rewrites to `B`.
> 4. If prover fails, examine output. Is spec incorrect, program incorrect, or does the prover need help?

. . .

\vfill

\center\huge DEMO!

A simple proof
--------------

```
  rule <k> {  DUP ;
              DIP { CDR } ;
              CAR ;
              ADD ;
              NIL operation ; PAIR }
       => .
     </k>
     <stack> Pair A:Int B:Int
          => Pair .List (A +Int B)
     </stack>
```

\vspace{1em}

. . .

This goes through.

A bad proof
-----------

```
  rule <k> {  DUP ;
              DIP { CDR } ;
              CAR ;
              ADD ;
              NIL operation ; PAIR }
       => .
     </k>
     <stack> Pair A:Int B:Int
          => Pair .List X
     </stack>

    ensures X >Int A
```

\vspace{1em}

. . .

State a requested final property.

Prover complains
----------------

```
$ ./michelson.py prove demo/sum-simple.k
  #Not ( {
    A +Int B >Int A
  #Equals
    true
  } )
#And
    <k>
      .
    </k>
    <stack>
      Pair .List A +Int B
    </stack>
    ...
```

\vspace{1em}

. . .

It's saying, "what if *not* A + B > A"?

Fix
---

```
  rule <k> {  DUP ;
              DIP { CDR } ;
              CAR ;
              ADD ;
              NIL operation ; PAIR }
       => .
     </k>
     <stack> Pair A:Int B:Int
          => Pair .List X
     </stack>
    requires B >Int 0
    ensures X >Int A
```

\vspace{1em}

. . .

This passes.

Proving
-------

Happy to demo more for you on K proving.

Conclusion
============

Interested?
--------

Runtime Verification specializes in formalizing languages in \lK.[^1]

From a single spec you can derive a host of tools. The tools are under constant development and we can help customize them for your needs.[^2]

**If you are interested in specifying your language in \lK, talk to us.**

[rikard.hjort@runtimeverification.com](mailto:rikard.hjort@runtimeverification.com)

[^1]: We have formalized all kinds of languages: imperative, stack-based, register-based, OO, functional, logic. We have done low-level -- LLVM, EVM, Wasm -- and high-level -- C, Java, JavaScript.
[^2]:  The tools have competative performance, sometimes outperforming handwritten tools.

Questions?
======

Thanks!
-------

-   Thanks for listening!

References
----------

\tiny
