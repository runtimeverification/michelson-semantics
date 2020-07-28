# Micheline Format

```k
module MICHELINE-COMMON-SYNTAX
  imports INT-SYNTAX
  imports STRING-SYNTAX
```

## Micheline Node Types

Micheline has five node types.

```k
  syntax MichelineNode ::= Int
                         | String
                         | MichelineBytes
                         | PrimitiveNode
                         | SequenceNode

  syntax BytesToken
  syntax MichelineBytes ::= BytesToken

  syntax Primitive

  syntax PrimitiveNode ::= Primitive
                         | PrimitiveApplication
  syntax PrimitiveApplication ::= Primitive PrimitiveArgs
  syntax PrimitiveArgs ::= NeList{PrimitiveArg, ""} [klabel(PrimitiveArgs), symbol]

  syntax SequenceNode ::= "{" MichelineNodes "}"

  syntax EmptyMichelineNodes          // ::= "" [klabel(.MichelineNodes), symbol]
  syntax EmptyMichelineNodesSemiColon // ::= ";"
  syntax NeMichelineNodes ::= MichelineNode EmptyMichelineNodes          [klabel(MichelineNodesCons), symbol]
                            | MichelineNode EmptyMichelineNodesSemiColon [klabel(MichelineNodesCons)]
                            | MichelineNode ";" NeMichelineNodes         [klabel(MichelineNodesCons)]
  syntax MichelineNodes   ::= NeMichelineNodes
                            | EmptyMichelineNodes

  syntax PrimitiveArg ::= NodeArg
                        | Annotation

  syntax NodeArg ::= Int
                   | String
                   | MichelineBytes
                   | SequenceNode
                   | Primitive
                   | "(" PrimitiveApplication ")"

  syntax Annotation
endmodule
```

```k
module MICHELINE-TO-MICHELSON-COMMON-SYNTAX
  imports MICHELINE-COMMON-SYNTAX

  // Annotations
  syntax Annotation ::= VarAnnotation | TypeAnnotation | FieldAnnotation
  syntax VarAnnotation   [token]
  syntax TypeAnnotation  [token]
  syntax FieldAnnotation [token]

  syntax Primitive ::= Type | Instruction | Macro | Field | MichelsonData | InternalData

  // Types
  // Simple types
  syntax Type ::= "int"
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
                | "chain_id"
  // Recursive types
                | "pair"
                | "option"
                | "list"
                | "set"
                | "or"
                | "lambda"
                | "map"
  // Non-pushable simple types
                | "operation"
                | "contract"
  // Non-pushable recurisve types
                | "big_map"

  // Instructions
  // Control structures
  syntax Instruction ::= "FAILWITH"
                       | "IF"
                       | "LOOP"
                       | "LOOP_LEFT"
                       | "DIP"
                       | "EXEC"
                       | "APPLY"
  // Stack operations
                       | "DROP"
                       | "DUP"
                       | "SWAP"
                       | "DIG"
                       | "DUG"
                       | "PUSH"
                       | "UNIT"
                       | "LAMBDA"
  // Generic comparison
                       | "EQ"
                       | "NEQ"
                       | "LT"
                       | "GT"
                       | "LE"
                       | "GE"
  // Boolean operations
                       | "OR"
                       | "AND"
                       | "XOR"
                       | "NOT"
  // Number operations
                       | "NEG"
                       | "ABS"
                       | "ISNAT"
                       | "INT"
                       | "ADD"
                       | "SUB"
                       | "MUL"
                       | "EDIV"
                       | "COMPARE"
  // Nubmer bitwise operations
                       | "OR"
                       | "AND"
                       | "XOR"
                       | "NOT"
                       | "LSL"
                       | "LSR"
  // String operations
                       | "CONCAT"
                       | "SIZE"
                       | "SLICE"
                       | "COMPARE"
  // Pair operations
                       | "PAIR"
                       | "CAR"
                       | "CDR"
                       | "COMPARE"
  // Set operations
                       | "EMPTY_SET"
                       | "MEM"
                       | "UPDATE"
                       | "ITER"
                       | "SIZE"
  // Map operations
                       | "EMPTY_MAP"
                       | "GET"
                       | "MEM"
                       | "UPDATE"
                       | "MAP"
                       | "ITER"
                       | "SIZE"
  // Big_map operations
                       | "EMPTY_BIG_MAP"
                       | "GET"
                       | "MEM"
                       | "UPDATE"
  // Option operations
                       | "SOME"
                       | "NONE"
                       | "IF_NONE"
  // Union operations
                       | "LEFT"
                       | "RIGHT"
                       | "IF_LEFT"
  // List operations
                       | "CONS"
                       | "NIL"
                       | "IF_CONS"
                       | "MAP"
                       | "SIZE"
                       | "ITER"
  // Timestamp operations
                       | "ADD"
                       | "SUB"
                       | "COMPARE"
  // Mutez operations
                       | "ADD"
                       | "SUB"
                       | "MUL"
                       | "EDIV"
                       | "COMPARE"
  // Contract operations
                       | "CREATE_CONTRACT"
                       | "TRANSFER_TOKENS"
                       | "SET_DELEGATE"
                       | "BALANCE"
                       | "ADDRESS"
                       | "CONTRACT"
                       | "SOURCE"
                       | "SENDER"
                       | "SELF"
                       | "AMOUNT"
                       | "IMPLICIT_ACCOUNT"
  // Special operations
                       | "NOW"
                       | "CHAIN_ID"
  // Byte operations
                       | "PACK"
                       | "UNPACK"
                       | "CONCAT"
                       | "SIZE"
                       | "SLICE"
                       | "COMPARE"
  // Cryptographic operations
                       | "HASH_KEY"
                       | "BLAKE2B"
                       | "SHA256"
                       | "SHA512"
                       | "CHECK_SIGNATURE"
                       | "COMPARE"
  // No-op type-transforming instructions
                       | "CAST"
                       | "RENAME"
  // Deprecated instructions
                       | "CREATE_CONTRACT"
                       | "CREATE_ACCOUNT"
                       | "STEPS_TO_QUOTA"
  // Extended instructions
                       | "STOP"
                       | "PAUSE"

  // Macros
  // Comparison macros
  syntax Macro ::= "CMPEQ"
                 | "CMPNEQ"
                 | "CMPLT"
                 | "CMPGT"
                 | "CMPLE"
                 | "CMPGE"
                 | "IFEQ"
                 | "IFNEQ"
                 | "IFLT"
                 | "IFGT"
                 | "IFLE"
                 | "IFGE"
                 | "IFCMPEQ"
                 | "IFCMPNEQ"
                 | "IFCMPLT"
                 | "IFCMPGT"
                 | "IFCMPLE"
                 | "IFCMPGE"
  // Failure and assertion macros
                 | "FAIL"
                 | "ASSERT"
                 | "ASSERT_EQ"
                 | "ASSERT_NEQ"
                 | "ASSERT_LT"
                 | "ASSERT_LE"
                 | "ASSERT_GT"
                 | "ASSERT_GE"
                 | "ASSERT_CMPEQ"
                 | "ASSERT_CMPNEQ"
                 | "ASSERT_CMPLT"
                 | "ASSERT_CMPLE"
                 | "ASSERT_CMPGT"
                 | "ASSERT_CMPGE"
                 | "ASSERT_NONE"
                 | "ASSERT_SOME"
                 | "ASSERT_LEFT"
                 | "ASSERT_RIGHT"
  // Syntactic conveniences
                 | "IF_SOME"
                 | "IF_RIGHT"
                 | DIPMacro
                 | DUPMacro
                 | PAIRMacro
                 | UNPAIRMacro
                 | CADRMacro
                 | SETCADRMacro
                 | MAPCADRMacro

  syntax DIPMacro     [token]
  syntax DUPMacro     [token]
  syntax PAIRMacro    [token]
  syntax UNPAIRMacro  [token]
  syntax CADRMacro    [token]
  syntax SETCADRMacro [token]
  syntax MAPCADRMacro [token]

  // Michelson fields
  syntax Field ::= "code"
                 | "storage"
                 | "parameter"
  // Michelson TZT fields
                 | "contract"
                 | "now"
                 | "sender"
                 | "source"
                 | "chain_id"
                 | "self"
                 | "amount"
                 | "balance"
                 | "other_contracts"
                 | "big_maps"
                 | "input"
                 | "output"
  // Michelson TZT extended fields
                 | "parameter_value"
                 | "storage_value"
  // Michelson symbolic TZT
                 | "precondition"
                 | "postcondition"
                 | "invariant"

  // Michelson data constructors (arguments to PUSH or Stack_elt)

  syntax SymbolicPrimitive [token]
  syntax AnyToken          [token]

  // Booleans
  syntax MichelsonData ::= "True"
                         | "False"
  // Maps
                         | "Elt"
  // Options
                         | "Some"
                         | "None"
  // Unions
                         | "Left"
                         | "Right"
  // Pairs
                         | "Pair"
  // Unit
                         | "Unit"

  // Michelson internal data constructors (used in TZT files)
  // Big_maps
  syntax InternalData ::= "Big_map"
  // Operations
                        | "Transfer_token"
                        | "Set_delegate"
                        | "Create_contract"
  // Stacks
                        | "Stack_elt"
                        | "Failed"
                        | "MutezOverflow"
                        | "MutezUnderflow"
                        | "GeneralOverflow"
  // Any
                        | AnyToken
  // Michelson extended internal data constructors
                        | "#Any"
                        | SymbolicPrimitive
endmodule
```

```k
module MICHELSON-PARSER-SYNTAX
  imports MICHELINE-TO-MICHELSON-COMMON-SYNTAX

  syntax EmptyMichelineNodes          ::= ""  [klabel(.MichelineNodes), symbol]
  syntax EmptyMichelineNodesSemiColon ::= ";" [klabel(.MichelineNodes), symbol]

  syntax BytesToken         ::= r"0x[a-fA-F0-9]*"                     [token]

  syntax VarAnnotation      ::= r"@(%|%%|[_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]
  syntax TypeAnnotation     ::= r":([_a-zA-Z][_0-9a-zA-Z\\.]*)?"      [token]
  syntax FieldAnnotation    ::= r"%(@|[_a-zA-Z][_0-9a-zA-Z\\.]*)?"    [token]

  syntax DIPMacro           ::= r"DII+P"                              [token]
  syntax DUPMacro           ::= r"DUU+P"                              [token]
  syntax PAIRMacro          ::= r"P[AIP]+R"                           [token]
  syntax UNPAIRMacro        ::= r"UNP[AIP]+R"                         [token]
  syntax CADRMacro          ::= r"C[A,D]{2,}R"                        [token]
  syntax SETCADRMacro       ::= r"SET_C[AD]+R"                        [token]
  syntax MAPCADRMacro       ::= r"MAP_C[AD]+R"                        [token]

  syntax AnyToken           ::= "_"                                   [token]
  syntax SymbolicPrimitive  ::= r"\\$[a-zA-Z_0-9]+"                   [token]
endmodule
```

```k
module MICHELSON-PARSER
  imports MICHELINE-TO-MICHELSON-COMMON-SYNTAX
  imports MICHELSON-PARSER-INTERNAL

  rule NodesToIR()                       => .MichelineIRNodes
  rule NodesToIR(N Ns)                   => NodeToIR(N) NodesToIRAux(Ns)
  rule NodesToIRAux(; Ns:MichelineNodes) => NodesToIR(Ns)
  rule NodesToIRAux()                    => .MichelineIRNodes

  rule NodeToIR(P:Primitive)      => NodeToPrim(P, NoArgs)
  rule NodeToIR(P:Primitive Args) => NodeToPrim(P, Args)
endmodule

module MICHELINE-INTERNAL-REPRESENTATION
  imports MICHELINE-TO-MICHELSON-COMMON-SYNTAX
  imports INT
  imports STRING
  imports BYTES

  syntax MichelineIRNode ::= Int
                           | String
                           | Bytes
                           | Prim(String, AnnotationData, MichelineIRNodes)
                           | Seq(MichelineIRNodes)

  syntax MichelineIRNodes ::= List{MichelineIRNode, ""}

  syntax AnnotationData      ::= VarAnnotationList TypeAnnotationList FieldAnnotationList
  syntax VarAnnotationList   ::= List{VarAnnotation,  ""}
  syntax TypeAnnotationList  ::= List{TypeAnnotation, ""}
  syntax FieldAnnotationList ::= List{TypeAnnotation, ""}
endmodule

module MICHELSON-PARSER-INTERNAL
  imports MICHELINE-TO-MICHELSON-COMMON-SYNTAX
  imports MICHELINE-INTERNAL-REPRESENTATION

  configuration <k> $PGM:MichelineNodes </k>

  syntax MichelineIRNodes ::= NodesToIR(MichelineNodes)
                            | NodesToIRAux(MichelineNodesAux)
  syntax MichelineIRNode  ::= NodeToIR(MichelineNode)
                            | NodeToPrim(Primitive, OptPrimitiveArgs)

  syntax OptPrimitiveArgs ::= "NoArgs"
                            | PrimitiveArgs
endmodule
```

1. Micheline-Like    - all primitives are well-defined
2. MichelineIR       - all primitives take correct number of args / annotations
3. TypedMichelsonIR  - convert IR to typed version --- progress guaranteed

