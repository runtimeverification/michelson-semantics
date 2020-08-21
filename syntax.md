Michelson Syntax
================

This module declares the syntax of a K-Michelson input file. In particular, it
contains the syntax of a Michelson contract and the additional data necessary to
fully specify a contract execution.

```k
module MICHELSON-SYNTAX
  imports MICHELSON-COMMON-SYNTAX
  imports MICHELSON-MACRO-SYNTAX

  syntax Groups ::= Group ";" [klabel(groupSemicolon), symbol]

  syntax TypeAnnotation ::= r":([_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]
  syntax VariableAnnotation ::= r"@(%|%%|[_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]
  syntax FieldAnnotation ::= r"%(@|[_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]
endmodule
```

```k
module MICHELSON-COMMON-SYNTAX
  imports INT-SYNTAX
  imports STRING-SYNTAX
```

Here we declare many of the data sorts we will use.  Note that, in this module at least, some of these sorts are empty (e.g. ContractData) - as they should not be in an input file.

```k
  // Sorts
  syntax SimpleData
  syntax KResult ::= SimpleData
  syntax Data
  syntax Instruction
  syntax Type
  syntax Contract
  syntax Type
  syntax SimpleType
  syntax Address
  syntax ContractData
  syntax Mutez
  syntax KeyHash
  syntax ChainId
  syntax Timestamp
  syntax Key
  syntax Signature
```

An element in a map in Michelson is specified in the form `Elt Key Value`. A map literal is a sequence of these pairs.

```k
  syntax MapEntry ::= "Elt" Data Data
```

Here we define the three sequence sorts in Michelson.  Note that these sorts cover only *nonempty* sequences - the empty sequence is defined separately to avoid parsing ambiguities.

```k
  syntax Data ::= Instruction
  syntax DataList ::= Data | Data ";" DataList

  syntax MapEntryList ::= MapEntry | MapEntry ";" MapEntryList
```

[//]: # (What about sets?)

Here we define annotations.  Michelson actually has more stringent requirements for annotation lists to be well formed, but we do not yet enforce these requirements as annotations do very little in an execution semantics.  It is possible to fully specify the real requirements in the K grammar, and indeed an older version of the semantics did so.  However, the number of productions and rules necessary came at an unacceptable performance penalty when compared to the minimal benefit gained by rejecting such contracts.

```k
  syntax TypeAnnotation     [token]
  syntax VariableAnnotation [token]
  syntax FieldAnnotation    [token]
  syntax Annotation ::= TypeAnnotation | VariableAnnotation | FieldAnnotation

  syntax AnnotationList ::= List{Annotation, ""}
```

The bytes literal is expressed here.  We accept mixed type bytes of the form `0xaAfF01` but standardize them to all lowercase during load time.

```k
  syntax MBytesLiteral ::= r"0x([0-9a-fA-F]{2})*" [token]
  syntax MBytes ::= MBytesLiteral [klabel(MBytesLiteral), symbol, function, avoid]
```

K boolean values use all lowercase `true` and `false` - hence we need to add tokens for Michelson bools.
They are converted to K booleans immediately after parsing by function rules.

```k
  syntax Data ::= MichelsonBool [klabel(MichelsonBool), symbol, function, avoid]
  syntax MichelsonBool ::= "True" [token]
                         | "False" [token]
```

Here we specify the various complex types offered by Michelson, making the best possible use of K sorts.

```k
  syntax Data ::= Pair | OrData | OptionData
  syntax Pair ::= "Pair" Data Data [seqstrict]

  syntax OrData ::= "Left" Data [seqstrict]
                  | "Right" Data [seqstrict]

  syntax OptionData ::= "Some" Data [seqstrict]
                      | "None"
```

Here we specify the various forms of sequence literals in Michelson, including Map and List literals, and blocks.  The former two are converted to K's hooked sorts during load time.

```k
  syntax MapLiteral ::= "{" MapEntryList "}"

  syntax EmptyBlock ::= "{" "}"

  syntax Block ::= "{" DataList "}"
                 | EmptyBlock

  syntax SequenceData ::= MapLiteral | Block
  syntax Data ::= SequenceData
```

Here we define the simple data literals.

```k
  syntax SimpleData ::= Int
  syntax SimpleData ::= String
  syntax SimpleData ::= MBytes
  syntax SimpleData ::= "Unit"
  syntax SimpleData ::= Timestamp
  syntax SimpleData ::= ChainId
  syntax SimpleData ::= KeyHash
  syntax SimpleData ::= Mutez
  syntax SimpleData ::= Address
  syntax SimpleData ::= ContractData
  syntax SimpleData ::= Key
  syntax SimpleData ::= Signature

  syntax Data ::= SimpleData
```

Mutez and Addresses are specified in Michelson contracts as the simpler Int and String types, and need to be wrapped during load time so they retain their type information.

```k
  syntax Mutez ::= Int
  syntax Address ::= String
```

K offers the bracket attribute for productions that should not actually be retained in the AST, but instead simply give information on disambiguating the AST (such as parentheses).

```k
  syntax OptionData ::= "(" OptionData ")" [bracket]
  syntax Data ::= "(" Data ")" [bracket]
  syntax Type ::= "(" Type ")" [bracket] // Technically incorrect due to rule about primitive app right inside a sequence.  Need to split out Wrapped/Unwrapped sort.
```

[//]: # (If you want to forbid { (prim arg) }, you should probably have a complete intermediate representation corresponding to Micheline.)

Michelson types (like other Micheline primitives) have optional annotations.

```k
  syntax NumTypeName ::= "int"
                       | "nat"

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

  syntax BinaryTypeName  ::= "pair"
                           | "or"
                           | "lambda"
                           | "map"
                           | "big_map"

  syntax Type ::= NullaryTypeName AnnotationList
                | UnaryTypeName   AnnotationList Type
                | BinaryTypeName  AnnotationList Type Type
```

We now specify the MICHELSON instruction set.

```k
  syntax Instruction ::= Block

  syntax Instruction ::= "DROP" AnnotationList
  syntax Instruction ::= "DROP" AnnotationList Int
  syntax Instruction ::= "DIG" AnnotationList Int
  syntax Instruction ::= "DUG" AnnotationList Int
  syntax Instruction ::= "DUP" AnnotationList
  syntax Instruction ::= "SWAP" AnnotationList
  syntax Instruction ::= "PUSH" AnnotationList Type Data
  syntax Instruction ::= "SOME" AnnotationList
  syntax Instruction ::= "NONE" AnnotationList Type
  syntax Instruction ::= "UNIT" AnnotationList
  syntax Instruction ::= "IF_NONE" AnnotationList Block Block
  syntax Instruction ::= "PAIR" AnnotationList
  syntax Instruction ::= "UNPAIR" AnnotationList
  syntax Instruction ::= "CAR" AnnotationList
  syntax Instruction ::= "CDR" AnnotationList
  syntax Instruction ::= "LEFT" AnnotationList Type
  syntax Instruction ::= "RIGHT" AnnotationList Type
  syntax Instruction ::= "IF_LEFT" AnnotationList Block Block
  syntax Instruction ::= "NIL" AnnotationList Type
  syntax Instruction ::= "CONS" AnnotationList
  syntax Instruction ::= "IF_CONS" AnnotationList Block Block
  syntax Instruction ::= "SIZE" AnnotationList
  syntax Instruction ::= "EMPTY_SET" AnnotationList Type
  syntax Instruction ::= "EMPTY_MAP" AnnotationList Type Type
  syntax Instruction ::= "EMPTY_BIG_MAP" AnnotationList Type Type
  syntax Instruction ::= "MAP" AnnotationList Block
  syntax Instruction ::= "ITER" AnnotationList Block
  syntax Instruction ::= "MEM" AnnotationList
  syntax Instruction ::= "GET" AnnotationList
  syntax Instruction ::= "UPDATE" AnnotationList
  syntax Instruction ::= "IF" AnnotationList Block Block
  syntax Instruction ::= "LOOP" AnnotationList Block
  syntax Instruction ::= "LOOP_LEFT" AnnotationList Block
  syntax Instruction ::= "LAMBDA" AnnotationList Type Type Block
  syntax Instruction ::= "EXEC" AnnotationList
  syntax Instruction ::= "APPLY" AnnotationList
  syntax Instruction ::= "DIP" AnnotationList Block
  syntax Instruction ::= "DIP" AnnotationList Int Block
  syntax Instruction ::= "FAILWITH" AnnotationList
  syntax Instruction ::= "CAST" AnnotationList
  syntax Instruction ::= "RENAME" AnnotationList
  syntax Instruction ::= "CONCAT" AnnotationList
  syntax Instruction ::= "SLICE" AnnotationList
  syntax Instruction ::= "PACK" AnnotationList
  syntax Instruction ::= "UNPACK" AnnotationList Type
  syntax Instruction ::= "ADD" AnnotationList
  syntax Instruction ::= "SUB" AnnotationList
  syntax Instruction ::= "MUL" AnnotationList
  syntax Instruction ::= "EDIV" AnnotationList
  syntax Instruction ::= "ABS" AnnotationList
  syntax Instruction ::= "ISNAT" AnnotationList
  syntax Instruction ::= "INT" AnnotationList
  syntax Instruction ::= "NEG" AnnotationList
  syntax Instruction ::= "LSL" AnnotationList
  syntax Instruction ::= "LSR" AnnotationList
  syntax Instruction ::= "OR" AnnotationList
  syntax Instruction ::= "AND" AnnotationList
  syntax Instruction ::= "XOR" AnnotationList
  syntax Instruction ::= "NOT" AnnotationList
  syntax Instruction ::= "COMPARE" AnnotationList
  syntax Instruction ::= "EQ" AnnotationList
  syntax Instruction ::= "NEQ" AnnotationList
  syntax Instruction ::= "LT" AnnotationList
  syntax Instruction ::= "GT" AnnotationList
  syntax Instruction ::= "LE" AnnotationList
  syntax Instruction ::= "GE" AnnotationList
  syntax Instruction ::= "SELF" AnnotationList
  syntax Instruction ::= "CONTRACT" AnnotationList Type
  syntax Instruction ::= "TRANSFER_TOKENS" AnnotationList
  syntax Instruction ::= "SET_DELEGATE" AnnotationList
  syntax Instruction ::= "CREATE_ACCOUNT" AnnotationList
  syntax Instruction ::= "IMPLICIT_ACCOUNT" AnnotationList
  syntax Instruction ::= "NOW" AnnotationList
  syntax Instruction ::= "CHAIN_ID" AnnotationList
  syntax Instruction ::= "AMOUNT" AnnotationList
  syntax Instruction ::= "BALANCE" AnnotationList
  syntax Instruction ::= "CHECK_SIGNATURE" AnnotationList
  syntax Instruction ::= "BLAKE2B" AnnotationList
  syntax Instruction ::= "SHA256" AnnotationList
  syntax Instruction ::= "SHA512" AnnotationList
  syntax Instruction ::= "HASH_KEY" AnnotationList
  syntax Instruction ::= "STEPS_TO_QUOTA" AnnotationList
  syntax Instruction ::= "SOURCE" AnnotationList
  syntax Instruction ::= "SENDER" AnnotationList
  syntax Instruction ::= "ADDRESS" AnnotationList
  syntax Instruction ::= "CREATE_CONTRACT" AnnotationList "{" Contract "}"
```

The following instructions are an extension of the core Michelson instruction
set used for debugging purposes.

```k
  syntax Instruction ::= "STOP"
  syntax Instruction ::= "PAUSE"
  syntax Instruction ::= PAUSE(String)
  syntax Instruction ::= TRACE(String)
```


Here we specify the different formats a Michelson Contract may take.  These will be converted to the first format (`Code ; Storage ; Parameter ;`) by macros immediately after parsing.

```k
  syntax CodeDecl ::= "code" Block
  syntax StorageDecl ::= "storage" Type
  syntax ParameterDecl ::= "parameter" Type

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

[//]: # (I suggest to rename "Contract" into "Script"; in Tezos, "contract" usually means everything that is stored at a given address: this includes the script but also the storage and the balance.)

These sorts construct a mapping from Addresses to Types which will specify which contracts are available for this contract to access with the `CONTRACT T` instruction. In principle, any contract on the blockchain should be so accessible, but in practice this would be infeasible and needlessly overcomplicate using the semantics.

```k
  syntax OtherContractsMapEntry ::= "Elt" String Type
  syntax OtherContractsMapEntryList ::= List{OtherContractsMapEntry, ";"} [klabel(OtherContractsMapEntryList)]
```

These sorts construct a mapping from Ints to big\_maps and specify the contents of any big\_maps identified by their index in the storage field.

```k
  syntax BigMapEntry ::= "Big_map" Int Type Type MapLiteral
                       | "Big_map" Int Type Type EmptyBlock
  syntax BigMapEntryList ::= List{BigMapEntry, ";"} [klabel(BigMapEntryList)]
```

These sorts define the *Loading Groups* for the contract.  Loading groups specify information about the contract execution.  They intentionally look like Micheline primitive applications.  A program in the Michelson semantics consists of a sequence of loading groups separated by semicolons.  The order of these groups does not matter as the sequence is sorted before loading occurs.

- Contract specifies the Michelson contract to execute.
- Now specifies the timestamp output by the `NOW` instruction.
- Sender specifies the address output by the `SENDER` instruction.
- Source specifies the address output by the `SOURCE` instruction.
- Chain specifies the chain\_id output by the `CHAIN_ID` function.
- Self specifies the address of the contract output by the `SELF` instruction (the parameter type from the contract group specifies its type).
- Amount specifies the mutez quantity output by the `AMOUNT` instruction.
- Balance specifies the mutez quantity output by the `BALANCE` instruction.
- Contracts specifies the other smart contracts this execution knows about and might query with the `CONTRACT` instruction.
- ParameterValue specifies the data passed to this execution as a parameter.
- StorageValue specifies the data passed to this execution as its last storage value.
- BigMap specifies the big\_map data stored at each big\_map index.


[//]: # (Are ParameterValue and StorageValue ever used?)

```k
  syntax ContractGroup ::= "contract" "{" Contract "}"
  syntax NowGroup ::= "now" Int
  syntax SenderGroup ::= "sender" String
  syntax SourceGroup ::= "source" String
  syntax ChainGroup ::= "chain_id" MBytes
  syntax SelfGroup ::= "self" String
  syntax AmountGroup ::= "amount" Int
  syntax BalanceGroup ::= "balance" Int
  syntax ContractsGroup ::= "other_contracts" "{" OtherContractsMapEntryList "}"
  syntax ParameterValueGroup ::= "parameter_value" Data
  syntax StorageValueGroup ::= "storage_value" Data
  syntax BigMapGroup ::= "big_maps" "{" BigMapEntryList "}"

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

  syntax Groups ::= Group
                  | Group ";" Groups
```

Programs consist of sequences of these groups, potentially with an extra
semicolon on the end.

Note that for the default semantics, the `contract`, `parameter`, and `storage`
groups are required; all other groups are optional.  Accordingly, no empty
sequence of groups exists in the parser, since at least three groups must be
present for an execution to work.

```k
  syntax Pgm ::= Groups
endmodule
```

We list Macros separately, although in practice macros should not exist by this
point (since the external parser eliminates them).
We keep them in the grammar for future work.

```k
module MICHELSON-MACRO-SYNTAX
  imports MICHELSON-COMMON-SYNTAX

  syntax Macro
  syntax Instruction ::= Macro

  syntax DIPMacro ::= r"DII+P" [token]
  syntax DUPMacro ::= r"DUU+P" [token]
  syntax PairMacro ::= r"P[AIP]+R" [token] // This regex needs to be far more complex, but not sure how much K actually supports. P(\left=A|P(\left)(\right))(\right=I|P(\left)(\right))R
  syntax UnpairMacro ::= r"UNP[AIP]+R" [token] // Same as above. UNP(\left=A|P(\left)(\right))(\right=I|P(\left)(\right))R
  syntax CDARMacro ::= r"C[A,D]{2,}R" [token]
  syntax SetCDARMacro ::= r"SET_C[AD]+R" [token]

  syntax Macro ::= DIPMacro AnnotationList Block
  syntax Macro ::= DUPMacro AnnotationList
  syntax Macro ::= PairMacro AnnotationList
  syntax Macro ::= UnpairMacro AnnotationList
  syntax Macro ::= CDARMacro AnnotationList
  syntax Macro ::= SetCDARMacro AnnotationList

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
endmodule
```

Michelson Internal Syntax
=========================

```k
module MICHELSON-INTERNAL-SYNTAX
  imports MICHELSON-COMMON-SYNTAX

```

This extends the normal Michelson syntax to describe Blockchain operations
and error conditions. This module should remain separate from MICHELSON-SYNTAX
to prevent normal contracts from using these productions, which are intended
only for unit tests. However, this cannot be merged into the UNIT-TEST-SYNTAX
module, as the MICHELSON module needs to be able to use them, and MICHELSON
should not depend upon UNIT-TEST-SYNTAX.


Operation literals
------------------

Here we define literals of type `operation`.
They are not representable in standard Michelson code;
instead, they can only be pushed on the stack by the Michelson
`CREATE_CONTRACT`, `TRANSFER_TOKENS`, and `SET_DELEGATE` instructions.

1. We need an `operation` literal to create a new contract:

```k
  syntax BlockchainOperation ::=
    "Create_contract" "(" Int "," Contract "," OptionData "," Mutez "," Data ")"
```

- Nonce (`int`): A cryptographic nonce attached to each new `operation` literal
  created; no two operations created separately will ever share the same nonce
- Contract (`contract`): The source code of the contract to originate. The type
  of this contract will determine the expected type of the initial storage
- Delegate (`option key_hash`): An optional delegate specified by key hash
- Initial Balance (`mutez`): An initial balance to transfer to the new contract
- Initial Storage (`T`): An initial storage value, expected to be the same type
  as specified in the originated contract
[//]: # (A code together with parameter and storage types is usually called a "script")

2. We need an `operation` literal to transfer mutez to a contract (including
   the case of invoking a contract):

```k
  syntax BlockchainOperation ::=
    "Transfer_tokens" "(" Int "," Data "," Mutez "," Address ")"
```

- Nonce (`int`): A cryptographic nonce attached to each new `ooeration` literal
  created; no two operations created separately will ever share the same nonce
- Parameter (`T`): The parameter passed to the contract being invoked
  (or Unit, if the target contract is an implicit account)
- Amount (`mutez`): The quantity of mutez to transfer to the target contract
- Address (`address`): The address of the target contract

3. We need an `operation` literal to set the delegate of an account.

```k
  syntax BlockchainOperation ::= "Set_delegate" "(" Int "," OptionData ")"
```

- Nonce (`int`): A cryptographic nonce attached to each new `operation` literal
  created; no two operations created separately will ever share the same nonce
- Delegate (`option key_hash`): An optional new delegate specified by key hash;
  if `None`, then this operation clears the current delegate of the contract

We need to be able to represent `operation` literals stored on the stack.
Thus, we make `BlockchainOperation` a subsort of `Data`.

```k
  syntax SimpleData ::= BlockchainOperation
```

Failed Stack Literals
---------------------

We need to represent the stack of Michelson code that fails to execute properly.

1. We need to represent a stack resulting from a `FAILWITH` instruction. Its
   argument is the element on top of the stack when `FAILWITH` was executed.

```k
  syntax FailedStack ::= "(" "Failed" Data ")"
```

2. We need to represent a stack resulting from an operation on mutez which
   would have created a mutez value greater than the maximum representable
   value of 2^63 - 1. Its arguments are the two input mutez values involved in
   the operation.

```k
  syntax FailedStack ::= "(" "MutezOverflow" Int Int ")"
```

3. Similarly, we need to represent a stack resulting from an operation on mutez
   which would have created a mutez value less than the minimum representable
   value of 0. Its arguments are the two input mutez values involved in the
   operation.

```k
  syntax FailedStack ::= "(" "MutezUnderflow" Int Int ")"
```

4. This production represents a stack resulting from a non-mutez overflow, such
   as those produced by the `LSL` and `LSR` instructions when their shift
   argument is greater than 256.

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

This module extends the Michelson contract format to include the .tzt unit
testing format proposed by the Tezos foundation
[here](https://gitlab.com/tezos/tezos/-/merge_requests/1487/diffs).
The first module, `UNIT-TEST-SYNTAX` attempts to follow the proposal as closely
as possible. The second, `SYMBOLIC-UNIT-TEST-SYNTAX` further extends the syntax
with contructs needed for symbolic tests.

Concrete Unit Test Syntax
=========================

In order to test Michelson code at a finer level than a full smart
contract script, the following extension of the Michelson language can
be used. It adds syntax to specify an instruction (or sequence of
instructions) to test, a concrete input stack and the expected output
stack.

These unit tests can be useful for both smart contract developers that
need to independently test various parts of the smart contracts they
develop and to the developers of new implementations of the Michelson
interpreter that need to check that their new implementations behave
as the reference implementation by passing a conformance test suite.

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

Syntax of concrete stacks
-------------------------

A concrete stack is written as a Micheline sequence whose elements are
of the form ``Stack_elt ty x`` where ``x`` is a Michelson value and
``ty`` is its type. For example, ``{ Stack_elt bool True ; Stack_elt
nat 42 }`` is a concrete stack of length 2 whose top element is the
boolean ``True`` and the bottom element is the natural number ``42``.

```k
  syntax StackElementLiteral ::= "Stack_elt" Type Data
  syntax StackElementList ::= List{ StackElementLiteral, ";" } [klabel(StackElementList)]
  syntax LiteralStack ::= "{" StackElementList "}"
```

While the input stack must be a full literal, the output stack may be either a
literal or a failed stack (see `michelson-internal-syntax.k` for details),
indicating that the code snippet should fail in a specific way during execution.

```k
  syntax OutputStack ::= LiteralStack | FailedStack
```

The wildcard pattern `_` can be used to omit part of the output stack. This
is typically used to omit the cryprographic nonce in values of type `operation`.

TODO: Parsing `_`s is somewhat tricky in K so we use `#Any` instead for now.

```k
  syntax Data ::= "#Any"
```

Unit test groups
----------------

Unit test files allow the following additional groups, in any order:

`input` (mandatory): the input stack is the initial stack to pass to the Code
block or instruction.

```k
  syntax Group ::= InputGroup
  syntax InputGroup ::= "input" LiteralStack
```

`code` (mandatory): the instruction or sequence of instructions to test.
We promote `CodeDecl` from the contract syntax to a `Group`.

```k
  syntax Group ::= CodeGroup
  syntax CodeGroup ::= CodeDecl
```

`output` (mandatory): the expected output stack:

```k
  syntax Group ::= OutputGroup
  syntax OutputGroup ::= "output" OutputStack
```

`parameter` (optional, defaults to `unit`): the type of the parameter of the
contract pushed by the `SELF` instruction.

```k
  syntax Group ::= ParameterGroup
  syntax ParameterGroup ::= ParameterDecl
```

Besides these, the `now`, `sender`, `source`, `chain_id`, `self`, `amount`, and
`balance`, `other_contracts`, and `big_maps` groups are defined in
[michelson/syntax.md](michelson/syntax.md)

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
