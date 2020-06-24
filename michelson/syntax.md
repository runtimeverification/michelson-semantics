This module declares the syntax of a K-Michelson input file.  In particular, it contains the syntax of a Michelson contract and the additional data necessary to fully specify a contract execution.

```k
module MICHELSON-SYNTAX
  imports INT-SYNTAX
  imports STRING-SYNTAX
```

Here we declare many of the data sorts we will use.  Note that, in this module at least, some of these sorts are empty (e.g. ContractData) - as they should not be in an input file.

```k
  // Sorts
  syntax SimpleData
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
  syntax TypeAnnotation ::= r":([_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]
  syntax VariableAnnotation ::= r"@(%|%%|[_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]
  syntax FieldAnnotation ::= r"%(@|[_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]
  syntax Annotation ::= TypeAnnotation | VariableAnnotation | FieldAnnotation

  syntax AnnotationList ::= List{Annotation, ""}
```

The bytes literal is expressed here.  We accept mixed type bytes of the form `0xaAfF01` but standardize them to all lowercase during load time.

```k
  syntax MBytesLiteral ::= r"0x([0-9a-fA-F]{2})*" [token]
```

K boolean values use all lowercase `true` and `false` - hence we need to add tokens for Michelson bools.
They are converted to K booleans immediately after parsing by function rules.

```k
  syntax MichelsonBool ::= "True" [token]
                         | "False" [token]
```

Here we specify the various complex types offered by Michelson, making the best possible use of K sorts.

```k
  syntax Pair ::= "Pair" Data Data

  syntax LeftData ::= "Left" Data
  syntax RightData ::= "Right" Data

  syntax OrData ::= LeftData | RightData

  syntax OptionData ::= "Some" Data
                      | "None"

  syntax ApplicationData ::= Pair | OrData | OptionData
  syntax Data ::= ApplicationData
```

[//]: # (What is the role of `ApplicationData`? grepping it does not return much result)

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
  syntax MBytes ::= MBytesLiteral [klabel(MBytesLiteral), symbol, function, avoid]

  syntax SimpleData ::= Int
  syntax SimpleData ::= String
  syntax Data ::= MichelsonBool [klabel(MichelsonBool), symbol, function, avoid]
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

In Michelson a simple type can be any of the following list, followed by an optional AnnotationList.

```k
  syntax UnannotatedSimpleType ::= "int"
                                 | "nat"
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

  syntax SimpleType ::= UnannotatedSimpleType AnnotationList
```

Note the positioning of the AnnotationList.

```k
  syntax Type ::= SimpleType
                | "pair" AnnotationList Type Type
                | "option" AnnotationList Type
                | "list" AnnotationList Type
                | "set" AnnotationList Type
                | "contract" AnnotationList Type
                | "or" AnnotationList Type Type
                | "lambda" AnnotationList Type Type
                | "map" AnnotationList Type Type
                | "big_map" AnnotationList Type Type
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

We list Macros separately, although in practice macros should not exist by this point (since the external parser eliminates them), we keep them in the grammar for future work.

```k
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
  syntax OtherContractsMapEntryList ::= OtherContractsMapEntry | OtherContractsMapEntry ";" OtherContractsMapEntryList
  syntax OtherContractsMap ::= EmptyBlock | "{" OtherContractsMapEntryList "}"
```

These sorts construct a mapping from Ints to big\_maps and specify the contents of any big\_maps identified by their index in the storage field.

```k
  syntax BigMapEntry ::= "Big_map" Int Type Type MapLiteral
                       | "Big_map" Int Type Type EmptyBlock
  syntax BigMapEntryList ::= BigMapEntry | BigMapEntry ";" BigMapEntryList
  syntax BigMapMap ::= EmptyBlock | "{" BigMapEntryList "}"
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
  syntax ContractsGroup ::= "other_contracts" OtherContractsMap
  syntax ParameterValueGroup ::= "parameter_value" Data
  syntax StorageValueGroup ::= "storage_value" Data
  syntax BigMapGroup ::= "big_maps" BigMapMap

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

  syntax Groups ::= Group | Group ";" Groups | Group ";"
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
