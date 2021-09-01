Michelson Syntax
================

This module declares the syntax of a K-Michelson input file. In particular, it
contains the syntax of a Michelson contract and the additional data necessary
to fully specify a contract execution.

Michelson Parser Syntax
-----------------------

This module defines the core parser of Michelson programs.
Unit test file syntax is defined as a slight extension of this file.

```k
module MICHELSON-SYNTAX
  imports MICHELSON-COMMON-SYNTAX

  syntax MichelsonBoolToken ::= "True"  [token]
                              | "False" [token]

  syntax MichelsonBytesToken ::= r"0x([0-9a-fA-F]{2})*" [token]

  syntax Groups ::= Group ";" [klabel(groupSemicolon), symbol]

  syntax TypeAnnotation ::= r":([_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]
  syntax VariableAnnotation ::= r"@(%|%%|[_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]
  syntax FieldAnnotation ::= r"%(@|[_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]

  syntax Wildcard ::= r"_" [token]

  // NB: This special token defines the syntax to be ignored by the parser
  syntax #Layout ::= r"(#.*)|[\\n \\t\\r]*" [token]
endmodule
```

Michelson Common Syntax
-----------------------

Common syntax is shared between the parser and the internal semantic
representation.

```k
module MICHELSON-COMMON-SYNTAX
  imports INT-SYNTAX
  imports STRING-SYNTAX
```

### Type Syntax

Type annotations only exist as tokens in the internal representation.

```k
  syntax TypeAnnotation [token]
  syntax VariableAnnotation [token]
  syntax FieldAnnotation [token]
  syntax Annotation ::= TypeAnnotation | VariableAnnotation | FieldAnnotation

  syntax AnnotationList ::= List{Annotation, ""}
```

Types are defined as expected.

```k
  syntax NumTypeName ::= "int" | "nat"

  syntax NullaryTypeName ::= NumTypeName
                           | "string"
                           | "bytes"
                           | "mutez"
                           | "bool"
                           | "key_hash"
                           | "timestamp"
                           | "address"
                           | "key"
                           | "unit"
                           | "signature"
                           | "operation"
                           | "chain_id"

  syntax UnaryTypeName   ::= "option"
                           | "list"
                           | "set"
                           | "contract"
                           | "ticket"

  syntax MapTypeName ::= "map" | "big_map"

  syntax BinaryTypeName  ::= "pair"
                           | "or"
                           | "lambda"
                           | MapTypeName

  syntax Type ::= NullaryTypeName AnnotationList
                | UnaryTypeName   AnnotationList Type
                | BinaryTypeName  AnnotationList Type Type
```

### Data Literal Syntax

Simple data literals have efficiently detectable normal forms at runtime.

```k
  syntax SimpleData ::= Int
  syntax SimpleData ::= String
  syntax SimpleData ::= MichelsonBool
  syntax SimpleData ::= MichelsonBytes
  syntax SimpleData ::= "Unit"
  syntax SimpleData ::= Mutez
  syntax SimpleData ::= Address
  syntax Data ::= SimpleData

  syntax Address
  syntax Mutez
```

This declaration instructs the compiler to consider SimpleData literals to be
in normal form.

```k
  syntax KResult ::= SimpleData
```

At parse time, `mutez` and `address` literals are read in as ints and strings.

```k
  syntax MutezLiteral   ::= Int
  syntax AddressLiteral ::= String
```

`bytes` and `bool` literals must be converted from Michelson to K syntax.

```k
  syntax MichelsonBytesToken [token]
  syntax MichelsonBytes ::= MichelsonBytesToken [klabel(MichelsonBytesToken), symbol, function, avoid]

  syntax MichelsonBoolToken [token]
  syntax MichelsonBool ::= MichelsonBoolToken [klabel(MichelsonBoolToken), symbol, function, avoid]
```

Simple recursive data structures are defined as expected.

```k
  syntax Pair ::= "Pair" Data Data

  syntax OrData ::= "Left" Data
                  | "Right" Data

  syntax OptionData ::= "Some" Data
                      | "None"

  syntax Data ::= Pair | OrData | OptionData
```

Collection sorts (maps, lists, and sets) are defined below.

```k
  syntax EmptyBlock ::= "{" "}"

  syntax MapEntry ::= "Elt" Data Data
  syntax MapEntryList ::= MapEntry | MapEntry ";" MapEntryList
  syntax NeMapLiteral ::= "{" MapEntryList "}"
  syntax MapLiteral ::= NeMapLiteral | EmptyBlock
  syntax Data ::= MapLiteral

  syntax DataList ::= Data | Data ";" DataList
  syntax Block ::= "{" DataList "}" | EmptyBlock
  syntax Data ::= Block
```

The `contract` literal syntax definition is somewhat redundant due to parser
limitations.

```k
  syntax CodeDecl ::= "code" Block
  syntax StorageDecl ::= "storage" Type
  syntax ParameterDecl ::= "parameter" Type
                         | "parameter" FieldAnnotation Type

  syntax Contract ::= CodeDecl ";" StorageDecl ";" ParameterDecl ";"
                    | CodeDecl ";" ParameterDecl ";" StorageDecl ";"
                    | StorageDecl ";" CodeDecl ";" ParameterDecl ";"
                    | ParameterDecl ";" CodeDecl ";" StorageDecl ";"
                    | StorageDecl ";" ParameterDecl ";" CodeDecl ";"
                    | ParameterDecl ";" StorageDecl ";" CodeDecl ";"

  syntax Contract ::= CodeDecl ";" StorageDecl ";" ParameterDecl
                    | CodeDecl ";" ParameterDecl ";" StorageDecl
                    | StorageDecl ";" CodeDecl ";" ParameterDecl
                    | ParameterDecl ";" CodeDecl ";" StorageDecl
                    | StorageDecl ";" ParameterDecl ";" CodeDecl
                    | ParameterDecl ";" StorageDecl ";" CodeDecl
```

### Instruction Syntax

Note that, because of `lambda` data literals, instructions are also data.

```k
  syntax Data ::= Instruction
```

Most instructions are defined via particular arity, except for blocks, which
are a kind of psuedo-instruction, and special instructions, i.e., instructions
with a unique arity.

```k
  syntax Instruction ::= Block
                       | NullaryInstName AnnotationList
                       | UnaryIntInstName AnnotationList Int
                       | UnaryTypeInstName AnnotationList Type
                       | UnaryBlockInstName AnnotationList Block
                       | BinaryTypeInstName AnnotationList Type Type
                       | BinaryBlockInstName AnnotationList Block Block
                       | SpecialInstruction
```

#### Nullary Instructions

```k
  syntax NullaryInstName ::= "DROP"
  syntax NullaryInstName ::= "DUP"
  syntax NullaryInstName ::= "SWAP"
  syntax NullaryInstName ::= "SOME"
  syntax NullaryInstName ::= "UNIT"
  syntax NullaryInstName ::= "PAIR"
  syntax NullaryInstName ::= "UNPAIR"
  syntax NullaryInstName ::= "CAR"
  syntax NullaryInstName ::= "CDR"
  syntax NullaryInstName ::= "CONS"
  syntax NullaryInstName ::= "SIZE"
  syntax NullaryInstName ::= "MEM"
  syntax NullaryInstName ::= "GET"
  syntax NullaryInstName ::= "UPDATE"
  syntax NullaryInstName ::= "EXEC"
  syntax NullaryInstName ::= "APPLY"
  syntax NullaryInstName ::= "FAILWITH"
  syntax NullaryInstName ::= "CAST"
  syntax NullaryInstName ::= "RENAME"
  syntax NullaryInstName ::= "CONCAT"
  syntax NullaryInstName ::= "SLICE"
  syntax NullaryInstName ::= "PACK"
  syntax NullaryInstName ::= "ADD"
  syntax NullaryInstName ::= "SUB"
  syntax NullaryInstName ::= "MUL"
  syntax NullaryInstName ::= "EDIV"
  syntax NullaryInstName ::= "ABS"
  syntax NullaryInstName ::= "ISNAT"
  syntax NullaryInstName ::= "INT"
  syntax NullaryInstName ::= "NEG"
  syntax NullaryInstName ::= "LSL"
  syntax NullaryInstName ::= "LSR"
  syntax NullaryInstName ::= "OR"
  syntax NullaryInstName ::= "AND"
  syntax NullaryInstName ::= "XOR"
  syntax NullaryInstName ::= "NOT"
  syntax NullaryInstName ::= "COMPARE"
  syntax NullaryInstName ::= "EQ"
  syntax NullaryInstName ::= "NEQ"
  syntax NullaryInstName ::= "LT"
  syntax NullaryInstName ::= "GT"
  syntax NullaryInstName ::= "LE"
  syntax NullaryInstName ::= "GE"
  syntax NullaryInstName ::= "SELF"
  syntax NullaryInstName ::= "TRANSFER_TOKENS"
  syntax NullaryInstName ::= "SET_DELEGATE"
  syntax NullaryInstName ::= "CREATE_ACCOUNT"
  syntax NullaryInstName ::= "IMPLICIT_ACCOUNT"
  syntax NullaryInstName ::= "NOW"
  syntax NullaryInstName ::= "CHAIN_ID"
  syntax NullaryInstName ::= "AMOUNT"
  syntax NullaryInstName ::= "BALANCE"
  syntax NullaryInstName ::= "CHECK_SIGNATURE"
  syntax NullaryInstName ::= "BLAKE2B"
  syntax NullaryInstName ::= "SHA256"
  syntax NullaryInstName ::= "SHA512"
  syntax NullaryInstName ::= "HASH_KEY"
  syntax NullaryInstName ::= "STEPS_TO_QUOTA"
  syntax NullaryInstName ::= "SOURCE"
  syntax NullaryInstName ::= "SENDER"
  syntax NullaryInstName ::= "ADDRESS"
  syntax NullaryInstName ::= "TICKET"
  syntax NullaryInstName ::= "READ_TICKET"
  syntax NullaryInstName ::= "JOIN_TICKETS"
  syntax NullaryInstName ::= "SPLIT_TICKET"
```

#### Unary Instructions

```k
  syntax UnaryIntInstName ::= "DROP"
  syntax UnaryIntInstName ::= "DIG"
  syntax UnaryIntInstName ::= "DUG"

  syntax UnaryTypeInstName ::= "NONE"
  syntax UnaryTypeInstName ::= "LEFT"
  syntax UnaryTypeInstName ::= "RIGHT"
  syntax UnaryTypeInstName ::= "NIL"
  syntax UnaryTypeInstName ::= "EMPTY_SET"
  syntax UnaryTypeInstName ::= "UNPACK"
  syntax UnaryTypeInstName ::= "CONTRACT"

  syntax UnaryBlockInstName ::= "MAP"
  syntax UnaryBlockInstName ::= "ITER"
  syntax UnaryBlockInstName ::= "LOOP"
  syntax UnaryBlockInstName ::= "LOOP_LEFT"
  syntax UnaryBlockInstName ::= "DIP"
```

#### Binary Instructions

```k
  syntax BinaryTypeInstName ::= "EMPTY_MAP"
  syntax BinaryTypeInstName ::= "EMPTY_BIG_MAP"

  syntax BinaryBlockInstName ::= "IF_NONE"
  syntax BinaryBlockInstName ::= "IF_LEFT"
  syntax BinaryBlockInstName ::= "IF_CONS"
  syntax BinaryBlockInstName ::= "IF"
```

#### Special Instructions

```k
  syntax SpecialInstruction ::= "PUSH" AnnotationList Type Data
  syntax SpecialInstruction ::= "LAMBDA" AnnotationList Type Type Block
  syntax SpecialInstruction ::= "CREATE_CONTRACT" AnnotationList "{" Contract "}"
  syntax SpecialInstruction ::= "DIP" AnnotationList Int Block
```

#### Debugging Instructions

The following instructions are an extension of the core Michelson instruction
set used for debugging purposes.

```k
  syntax Instruction ::= "STOP"
  syntax Instruction ::= "PAUSE"
  syntax Instruction ::= PAUSE(String)
  syntax Instruction ::= TRACE(String)
```

### Macro Syntax

All macros are instructions.

```k
  syntax Instruction ::= Macro
```

Some macros have syntax that can be validated at parse-time. Other macros need
additional runtime processing. The following macros require such processing.

```k
  syntax DIPMacro ::= r"DII+P" [token]
  syntax DUPMacro ::= r"DUU+P" [token]
  syntax CDARMacro ::= r"C[A,D]{2,}R" [token]
  syntax SetCDARMacro ::= r"SET_C[AD]{2,}R" [token]
  syntax MapCDARMacro ::= r"MAP_C[AD]{2,}R" [token]

  syntax NullaryMacroToken ::= DUPMacro
                             | CDARMacro
                             | SetCDARMacro

  syntax UnaryMacroToken ::= DIPMacro
                           | MapCDARMacro

  syntax Macro ::= NullaryMacroToken AnnotationList
  syntax Macro ::= UnaryMacroToken AnnotationList Block
```

Note: these macros require special runtime processing but are currently
_unimplemented_. Trying to use them will result in a parse error.

```disabled
  syntax NullaryMacroToken ::= PairMacro | UnpairMacro
  syntax PairMacro ::= r"P[AIP]+R" [token]
  syntax UnpairMacro ::= r"UNP[AIP]+R" [token]
```

The following macros can be validated at parse-time.

```k
  syntax Macro ::= "DUP" AnnotationList Int
  syntax Macro ::= "CMPEQ" AnnotationList
  syntax Macro ::= "CMPNEQ" AnnotationList
  syntax Macro ::= "CMPLT" AnnotationList
  syntax Macro ::= "CMPGT" AnnotationList
  syntax Macro ::= "CMPLE" AnnotationList
  syntax Macro ::= "CMPGE" AnnotationList
  syntax Macro ::= "IFEQ" AnnotationList Block Block
  syntax Macro ::= "IFNEQ" AnnotationList Block Block
  syntax Macro ::= "IFLT" AnnotationList Block Block
  syntax Macro ::= "IFGT" AnnotationList Block Block
  syntax Macro ::= "IFLE" AnnotationList Block Block
  syntax Macro ::= "IFGE" AnnotationList Block Block
  syntax Macro ::= "IFCMPEQ" AnnotationList Block Block
  syntax Macro ::= "IFCMPNEQ" AnnotationList Block Block
  syntax Macro ::= "IFCMPLT" AnnotationList Block Block
  syntax Macro ::= "IFCMPGT" AnnotationList Block Block
  syntax Macro ::= "IFCMPLE" AnnotationList Block Block
  syntax Macro ::= "IFCMPGE" AnnotationList Block Block
  syntax Macro ::= "FAIL" AnnotationList
  syntax Macro ::= "ASSERT" AnnotationList
  syntax Macro ::= "ASSERT_EQ" AnnotationList
  syntax Macro ::= "ASSERT_NEQ" AnnotationList
  syntax Macro ::= "ASSERT_LT" AnnotationList
  syntax Macro ::= "ASSERT_LE" AnnotationList
  syntax Macro ::= "ASSERT_GT" AnnotationList
  syntax Macro ::= "ASSERT_GE" AnnotationList
  syntax Macro ::= "ASSERT_CMPEQ" AnnotationList
  syntax Macro ::= "ASSERT_CMPNEQ" AnnotationList
  syntax Macro ::= "ASSERT_CMPLT" AnnotationList
  syntax Macro ::= "ASSERT_CMPLE" AnnotationList
  syntax Macro ::= "ASSERT_CMPGT" AnnotationList
  syntax Macro ::= "ASSERT_CMPGE" AnnotationList
  syntax Macro ::= "ASSERT_NONE" AnnotationList
  syntax Macro ::= "ASSERT_SOME" AnnotationList
  syntax Macro ::= "ASSERT_LEFT" AnnotationList
  syntax Macro ::= "ASSERT_RIGHT" AnnotationList
  syntax Macro ::= "IF_SOME" AnnotationList Block Block
  syntax Macro ::= "IF_RIGHT" AnnotationList Block Block
  syntax Macro ::= "SET_CAR" AnnotationList
  syntax Macro ::= "SET_CDR" AnnotationList
  syntax Macro ::= "MAP_CAR" AnnotationList Block
  syntax Macro ::= "MAP_CDR" AnnotationList Block
```

### Primitive Applications

We do not correctly parse Micheline primitive applications; we currently work
around the issue by allowing extra parens around potential primitive arguments.

```k
  syntax OptionData ::= "(" OptionData ")" [bracket]
  syntax Data       ::= "(" Data ")"       [bracket]
  syntax Type       ::= "(" Type ")"       [bracket]
```

### Input File Syntax

At the top level, input files consist of groups which specify:

-   parameters affecting the operation of the Michelson interpreter
-   parameters affecting the execution of Michelson unit tests

```k
  syntax Groups ::= Group
                  | Group ";" Groups
  syntax Pgm ::= Groups
```

#### Group Syntax

Typically, but not always, one loading group defined here corresponds to the
action of one instruction.

```k
  syntax ContractGroup ::= "contract" "{" Contract "}"
  syntax NowGroup ::= "now" Int
  syntax SenderGroup ::= "sender" String
  syntax SourceGroup ::= "source" String
  syntax ChainGroup ::= "chain_id" MichelsonBytes
  syntax SelfGroup ::= "self" String
  syntax AmountGroup ::= "amount" Int
  syntax BalanceGroup ::= "balance" Int
  syntax ParameterValueGroup ::= "parameter_value" Data
  syntax StorageValueGroup ::= "storage_value" Data

  syntax ContractsGroup ::= "other_contracts" "{" OtherContractsMapEntryList "}"
  syntax OtherContractsMapEntry ::= "Contract" String Type
  syntax OtherContractsMapEntryList ::= List{OtherContractsMapEntry, ";"} [klabel(OtherContractsMapEntryList)]

  syntax BigMapGroup ::= "big_maps" "{" BigMapEntryList "}"
  syntax BigMapEntry ::= "Big_map" Int Type Type NeMapLiteral
                       | "Big_map" Int Type Type EmptyBlock
  syntax BigMapEntryList ::= List{BigMapEntry, ";"} [klabel(BigMapEntryList)]

  syntax Group ::= ContractGroup
                 | ParameterValueGroup
                 | StorageValueGroup
                 | NowGroup
                 | SenderGroup
                 | SourceGroup
                 | ChainGroup
                 | SelfGroup
                 | AmountGroup
                 | BalanceGroup
                 | ContractsGroup
                 | BigMapGroup
```

```k
endmodule
```

Michelson Internal Syntax
=========================

Michelson internal syntax describes an extension of the base Michelson grammar
to allow for defining values of types which are unrepresentable in the
standard Michelson syntax.

```k
module MICHELSON-INTERNAL-SYNTAX
  imports MICHELSON-COMMON-SYNTAX
```

Operation literals
------------------

Here we define literals of type `operation` which correspond to values
produced by the `CREATE_CONTRACT`, `TRANSFER_TOKENS`, and `SET_DELEGATE`
instructions.

```k
  syntax SimpleData ::= BlockchainOperation
```

1.  Values produced by the `CREATE_CONTRACT` instruction:

    ```k
    syntax BlockchainOperation ::=
      "Create_contract" "{" Contract "}" OptionData MutezLiteral Data Data
    ```

    - Contract (`contract`) The source code of the contract to originate; the
      type of this contract determines storage type.
    - Delegate (`option key_hash`) An optional delegate specified by key hash.
    - Initial Balance (`mutez`) An initial balance to transfer to the new
      contract.
    - Initial Storage (`T`) An initial storage value of the correct type.
    - Nonce (`byte`): A unique cryptographic nonce for this operation.

2.  Values produced by the `TRANSFER_TOKENS` instruction:

    ```k
    syntax BlockchainOperation ::= "Transfer_tokens" Data MutezLiteral AddressLiteral Data
    ```

    - Parameter (`T`) The parameter passed to the contract being invoked
      (or `Unit`, if the target contract is an implicit account).
    - Amount (`mutez`) The amount of mutez to transfer to the target contract.
    - Address (`address`) The address of the target contract.
    - Nonce (`byte`): A unique cryptographic nonce for this operation.

3.  Values produced by the `SET_DELEGATE` instruction:

    ```k
    syntax BlockchainOperation ::= "Set_delegate" OptionData Data
    ```

    - Delegate (`option key_hash`) An optional new delegate specified by
      key hash; if `None`, then this operation clears the current delegate.
    - Nonce (`byte`): A unique cryptographic nonce for this operation.

Failed Stack Literals
---------------------

We need to represent the stack of Michelson code when execution fails.
We have failed stack literals for the following exceptional conditions:

1.  Exceuting a `FAILWITH` instruction; its argument is the value passed to
    `FAILWITH`.

    ```k
    syntax FailedStack ::= "(" "Failed" Data ")"
    ```

2.  Executing an instruction that causes a `mutez` value to overflow
    (producing a mutez value greater than 2^63-1); its arguments are the
    mutez inputs to the instruction which overflowed.

    ```k
    syntax FailedStack ::= "(" "MutezOverflow" Int Int ")"
    ```

3.  Executing an instruction that causes a `mutez` value to underflow
    (producing a mutez value less than 0); its arguments are the mutez inputs
    to the instruction which underflowed.

    ```k
    syntax FailedStack ::= "(" "MutezUnderflow" Int Int ")"
    ```

4.  Executing an instruction which produces a non-mutez out-of-bound value
    (e.g. the `LSL` and `LSR` instructions); its arguments are the numeric
    inputs to the overflowing instruction.

    ```k
    syntax FailedStack ::= "(" "GeneralOverflow" Int Int ")"
    ```

Michelson Assertions
--------------------

When running Michelson unit tests, we store pre- and post-conditions as lists
of Michelson sequences that produce a stack of the form `Stack_elt bool`.

```k
  syntax BlockList ::= List{Block, ";"} [klabel(BlockList)]
```

Trailing semicolons
-------------------

Trailing semicolons are optional removed:

```k
  syntax Groups ::= groupSemicolon(Group) [klabel(groupSemicolon), symbol]
  rule groupSemicolon(G) => G [anywhere]
```

```k
endmodule
```

Unit Test Syntax
================

This module extends the Michelson script grammar to the `.tzt` [unit test
grammar](https://gitlab.com/tezos/tezos/-/merge_requests/1487/diffs).

```k
module UNIT-TEST-SYNTAX
  imports UNIT-TEST-COMMON-SYNTAX
  imports MICHELSON-SYNTAX

endmodule
```

```k
module UNIT-TEST-COMMON-SYNTAX
  imports MICHELSON-COMMON-SYNTAX
  imports MICHELSON-INTERNAL-SYNTAX
```

Michelson Stack Syntax
----------------------

A stack is a Micheline sequence whose elements have the form `Stack_elt ty x`
where `x` is a Michelson value and `ty` is its type, e.g.

`{ Stack_elt bool True ; Stack_elt nat 42 }`

is a stack of length 2 whose top element is `True` and bottom element `42`.
We require that input stacks are well-formed. However, output stacks include
the degenerate case of failed stacks.

```k
  syntax StackElementLiteral ::= "Stack_elt" Type Data
  syntax StackElementList ::= List{ StackElementLiteral, ";" } [klabel(StackElementList)]
  syntax LiteralStack ::= "{" StackElementList "}"

  syntax OutputStack ::= LiteralStack | FailedStack
```

The wildcard pattern `_` can be used to omit part of the output stack. This is
typically used to omit the cryprographic nonce in `operation` values.

```k
  syntax Wildcard [token]
  syntax Data ::= Wildcard
```

Unit Test Groups
----------------

Unit test files allow the following additional groups.

### Mandatory Groups

-  `input` the initial stack to pass to the `code` block or instruction.

    ```k
    syntax Group ::= InputGroup
    syntax InputGroup ::= "input" LiteralStack
    ```

-   `code` the instruction or instruction sequence to test.

    ```k
    syntax Group ::= CodeGroup
    syntax CodeGroup ::= CodeDecl
    ```

-   `output` the expected output stack.

    ```k
    syntax Group ::= OutputGroup
    syntax OutputGroup ::= "output" OutputStack
    ```

### Optional Groups

-   `test` wraps tests allowing for multiple tests in one file

    ```k
    syntax Group ::= TestGroup
    syntax TestGroup ::= "test" TestName "{" Groups "}"
    syntax TestName ::= String
    ```

-   `parameter` (default `unit`) the type of the parameter of the contract
    pushed by the `SELF` instruction.

    ```k
    syntax Group ::= ParameterGroup
    syntax ParameterGroup ::= ParameterDecl
    ```

-   `import` groups map test fragments in files to strings for future usage

    ```k
    syntax Group ::= ImportGroup
    syntax ImportGroup ::= "import" String "as" String
    ```

-   `include` groups load either imported test fragments or raw files

    ```k
    syntax Group ::= IncludeGroup
    syntax IncludeGroup ::= "include" String
                          | "include-file" String
    ```

```k
endmodule
```

Symbolic Unit Test Syntax
=========================

We further extend the unit-test syntax with additional groups for representing
pre- and post-conditions as well as invariants to enable verification.

```k
module SYMBOLIC-UNIT-TEST-SYNTAX
  imports UNIT-TEST-SYNTAX
  imports SYMBOLIC-UNIT-TEST-COMMON-SYNTAX

  syntax SymbolicData ::= r"$[_a-zA-Z][_0-9a-zA-Z]*" [token]
endmodule
```

```k
module SYMBOLIC-UNIT-TEST-COMMON-SYNTAX
  imports UNIT-TEST-COMMON-SYNTAX
```

We extend `Data` to allow symbolic variables.

```k
  syntax SymbolicData [token]
  syntax Data ::= SymbolicData
```

We represent pre- and post-conditions as lists of blocks, where each block
contains a well-typed Michelson expression that consumes an empty input stack
and produces an output stack of type `Stack_elt bool _`.

```k
  syntax PreconditionGroup ::= "precondition" "{" BlockList "}"
  syntax PostconditionGroup ::= "postcondition" "{" BlockList "}"
  syntax Group ::= PreconditionGroup | PostconditionGroup
```

An invariant is an annotated pair of a stack shape (represented as a
`LiteralStack`) and a list of predicates (represented as `BlockList`).
The annotation determines to which looping instruction sequence (i.e. `LOOP`,
`LOOP_LEFT`, or `ITER`) an invariant corresponds.
When the program reaches the head of an annotated loop, we check that the actual
stack matches the specified stack shape.
Any symbolic variables in the stack shape are bound to the corresponding values
in the actual stack.
The predicate list represents invariants that must hold over all bound symbolic
variables.


```k
  syntax InvariantsGroup ::= "invariant" VariableAnnotation Invariant
  syntax Invariant ::= LiteralStack "{" BlockList "}"
  syntax Group ::= InvariantsGroup
```

```k
endmodule
```
