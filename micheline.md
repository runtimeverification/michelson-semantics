# Micheline Format

```k
module MICHELINE-COMMON-SYNTAX
  syntax BytesToken
  syntax Primitive
  syntax Annotation
endmodule
```

```k
module MICHELSON-PARSER-SYNTAX
  imports MICHELINE-TO-MICHELSON-COMMON-SYNTAX
  imports INT-SYNTAX
  imports STRING-SYNTAX

  syntax Pgm ::= MichelineNodes [klabel(MichelsonPgm), symbol]

  syntax SequenceNode         ::= "{" MichelineNodes "}"    [klabel(SeqNodeCtor), symbol]
  syntax PrimitiveApplication ::= Primitive NePrimitiveArgs [klabel(AppNodeCtor), symbol]

  syntax MichelineNode  ::= MichelineNode0       [klabel(Node0),      symbol]
  syntax MichelineNode0 ::= Int                  [klabel(IntNode),    symbol]
                          | String               [klabel(StringNode), symbol]
                          | BytesToken           [klabel(BytesNode),  symbol]
                          | SequenceNode         [klabel(SeqNode),    symbol]
                          | Primitive            [klabel(PrimNode),   symbol]
                          | PrimitiveApplication [klabel(AppNode),    symbol]

  syntax PrimitiveArg  ::= PrimitiveArg0                [klabel(Node0),      symbol]
                         | Annotation                   [klabel(AnnotArg),   symbol]
  syntax PrimitiveArg0 ::= Int                          [klabel(IntNode),    symbol]
                         | String                       [klabel(StringNode), symbol]
                         | BytesToken                   [klabel(BytesNode),  symbol]
                         | SequenceNode                 [klabel(SeqNode),    symbol]
                         | Primitive                    [klabel(PrimNode),   symbol]
                         | "(" PrimitiveApplication ")" [klabel(AppNode),    symbol]

  // Micheline Nodes
  syntax EmptyMichelineNodes   ::= ""  [klabel(.PrimArgs), symbol]
  syntax EmptyMichelineNodesSC ::= ";" [klabel(.PrimArgs), symbol]
  syntax NeMichelineNodes ::= MichelineNode EmptyMichelineNodes   [klabel(PrimArgsCons), symbol]
                            | MichelineNode EmptyMichelineNodesSC [klabel(PrimArgsCons), symbol]
                            | MichelineNode ";" NeMichelineNodes  [klabel(PrimArgsCons), symbol]
  syntax MichelineNodes   ::= NeMichelineNodes                    [klabel(PrimArgs),     symbol]
                            | EmptyMichelineNodes                 [klabel(PrimArgs),     symbol]

  // Primtive Args
  syntax EmptyPrimitiveArgs ::= "" [klabel(.PrimArgs), symbol]
  syntax NePrimitiveArgs ::= PrimitiveArg EmptyPrimitiveArgs [klabel(PrimArgsCons), symbol]
                           | PrimitiveArg NePrimitiveArgs    [klabel(PrimArgsCons), symbol]
  syntax PrimitiveArgs   ::= NePrimitiveArgs                 [klabel(PrimArgs),     symbol]
                           | EmptyPrimitiveArgs              [klabel(PrimArgs),     symbol]

  // Tokens
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
module MICHELINE-INTERNAL-SYNTAX
  imports MICHELINE-TO-MICHELSON-COMMON-SYNTAX
  imports INT-SYNTAX
  imports STRING-SYNTAX

  syntax SequenceNode         ::= "{" PrimitiveArgs "}"   [klabel(SeqNodeCtor), symbol]
  syntax PrimitiveApplication ::= Primitive PrimitiveArgs [klabel(AppNodeCtor), symbol]

  syntax MichelineNode ::= Int                  [klabel(IntNode),    symbol]
                         | String               [klabel(StringNode), symbol]
                         | BytesToken           [klabel(BytesNode),  symbol]
                         | SequenceNode         [klabel(SeqNode),    symbol]
                         | Primitive            [klabel(PrimNode),   symbol]
                         | PrimitiveApplication [klabel(AppNode),    symbol]

  syntax PrimitiveArg ::= MichelineNode [klabel(Node0),    symbol]
                        | Annotation    [klabel(AnnotArg), symbol]

  syntax EmptyPrimitiveArgs ::= ".PrimitiveArgs" [klabel(.PrimArgs), symbol]
  syntax NePrimitiveArgs ::= PrimitiveArg EmptyPrimitiveArgs [klabel(PrimArgsCons), symbol]
                           | PrimitiveArg NePrimitiveArgs    [klabel(PrimArgsCons)        ]
  syntax PrimitiveArgs   ::= NePrimitiveArgs                 [klabel(PrimArgs),     symbol]
                           | EmptyPrimitiveArgs              [klabel(PrimArgs)            ]
endmodule
```

```k
module MICHELSON-PARSER-INTERNAL
  imports MICHELINE-TO-MICHELSON-COMMON-SYNTAX
  imports MICHELINE-INTERNAL-SYNTAX
  imports INT
  imports STRING
  imports BYTES

  syntax Pgm ::= PrimitiveArgs [klabel(MichelsonPgm), symbol]
  configuration <k> $PGM:Pgm </k>

  syntax MichelineNode   ::= MichelineIRNode
  syntax MichelineIRNode ::= Int
                           | String
                           | Bytes
                           | Inst(Instruction, AnnotationData, MichelineIRNodes)
                           | Macro(Macro, AnnotationData, MichelineIRNodes)
                           | Field(Field, MichelineIRNodes)
                           | Data(MichelsonData, AnnotationData, MichelineIRNodes)
                           | Seq(MichelineIRNodes)

  syntax MichelineIRNodes ::= List{MichelineIRNode, ""}

  syntax AnnotationData      ::= VarAnnotationList TypeAnnotationList FieldAnnotationList
  syntax VarAnnotationList   ::= List{VarAnnotation,   ";"}
  syntax TypeAnnotationList  ::= List{TypeAnnotation,  ";"}
  syntax FieldAnnotationList ::= List{FieldAnnotation, ";"}

  syntax AnnotationData  ::= "noAnnotData" [function, functional]
  rule noAnnotData => .VarAnnotationList .TypeAnnotationList .FieldAnnotationList

  /*
  syntax PrimArgData ::= #PAD(AnnotationData, PrimitiveArgs)
                       | toPrimArgData(PrimitiveArgs)              [function]
                       | toPrimArgData(PrimitiveArgs, PrimArgData) [function]

  rule toPrimArgData(Args) => toPrimArgData(Args, #PAD(noAnnotData, .PrimitiveArgs))
  rule toPrimArgData(.PrimitiveArgs, PAD) => PAD
  rule toPrimArgData(A:VarAnnotation   Rest, #PAD(VAs TAs FAs, Args)) => toPrimArgData(Rest, #PAD(VAs ; A TAs FAs, Args))
  rule toPrimArgData(A:TypeAnnotation  Rest, #PAD(VAs TAs FAs, Args)) => toPrimArgData(Rest, #PAD(VAs TAs ; A FAs, Args))
  rule toPrimArgData(A:FieldAnnotation Rest, #PAD(VAs TAs FAs, Args)) => toPrimArgData(Rest, #PAD(VAs TAs FAs ; A, Args))
  rule toPrimArgData(N:MichelineNode   Rest, #PAD(VAs TAs FAs, Args)) => toPrimArgData(Rest, #PAD(VAs TAs FAs, Args N))
  */
endmodule

module MICHELSON-PARSER
  imports MICHELINE-TO-MICHELSON-COMMON-SYNTAX
  imports MICHELSON-PARSER-INTERNAL

  // syntax MichelineIRNode ::= NodeToPrim(Primitive, PrimArgData)

  // rule (N:Primitive):MichelineNode => NodeToPrim(N, #PAD(noAnnotData, .PrimitiveArgs)) [anywhere]
  // rule N:Primitive Args       => NodeToPrim(N, toPrimArgData(wrapArgs(Args)))   [anywhere]

  // rule NodeToPrim(I:NullaryInst, (Annots, NoArgs)) => Inst(I,NoAnnots,.MichelineIRNodes)
  // rule NodeToPrim(I:UnaryInst,   Arg)    => Inst(I,
  // rule NodeToPrim(I:UnaryInst, NoArgs) => ...
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

  syntax Primitive ::= Type | Instruction | Macro | Field | MichelsonData

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

  syntax Instruction ::= NullaryInst
                       | UnaryInst
                       | BinaryInst
                       | TernaryInst
  // Instructions
  // Control structures
  syntax NullaryInst ::= "FAILWITH"
  syntax BinaryInst  ::= "IF"
  syntax UnaryInst   ::= "LOOP"
  syntax UnaryInst   ::= "LOOP_LEFT"
  syntax BinaryInst  ::= "DIP"
  syntax NullaryInst ::= "EXEC"
  syntax NullaryInst ::= "APPLY"
  // Stack operations
  syntax UnaryInst   ::= "DROP"
  syntax NullaryInst ::= "DUP"
  syntax NullaryInst ::= "SWAP"
  syntax UnaryInst   ::= "DIG"
  syntax UnaryInst   ::= "DUG"
  syntax BinaryInst  ::= "PUSH"
  syntax NullaryInst ::= "UNIT"
  syntax TernaryInst ::= "LAMBDA"
  // Generic comparison
  syntax NullaryInst ::= "EQ"
  syntax NullaryInst ::= "NEQ"
  syntax NullaryInst ::= "LT"
  syntax NullaryInst ::= "GT"
  syntax NullaryInst ::= "LE"
  syntax NullaryInst ::= "GE"
  // Boolean operations
  syntax NullaryInst ::= "OR"
  syntax NullaryInst ::= "AND"
  syntax NullaryInst ::= "XOR"
  syntax NullaryInst ::= "NOT"
  // Number operations
  syntax NullaryInst ::= "NEG"
  syntax NullaryInst ::= "ABS"
  syntax NullaryInst ::= "ISNAT"
  syntax NullaryInst ::= "INT"
  syntax NullaryInst ::= "ADD"
  syntax NullaryInst ::= "SUB"
  syntax NullaryInst ::= "MUL"
  syntax NullaryInst ::= "EDIV"
  syntax NullaryInst ::= "COMPARE"
  // Nubmer bitwise operations
  syntax NullaryInst ::= "OR"
  syntax NullaryInst ::= "AND"
  syntax NullaryInst ::= "XOR"
  syntax NullaryInst ::= "NOT"
  syntax NullaryInst ::= "LSL"
  syntax NullaryInst ::= "LSR"
  // String operations
  syntax NullaryInst ::= "CONCAT"
  syntax NullaryInst ::= "SIZE"
  syntax NullaryInst ::= "SLICE"
  syntax NullaryInst ::= "COMPARE"
  // Pair operations
  syntax NullaryInst ::= "PAIR"
  syntax NullaryInst ::= "CAR"
  syntax NullaryInst ::= "CDR"
  syntax NullaryInst ::= "COMPARE"
  // Set operations
  syntax NullaryInst ::= "EMPTY_SET"
  syntax NullaryInst ::= "MEM"
  syntax NullaryInst ::= "UPDATE"
  syntax NullaryInst ::= "ITER"
  syntax NullaryInst ::= "SIZE"
  // Map operations
  syntax NullaryInst ::= "EMPTY_MAP"
  syntax NullaryInst ::= "GET"
  syntax NullaryInst ::= "MEM"
  syntax NullaryInst ::= "UPDATE"
  syntax NullaryInst ::= "MAP"
  syntax NullaryInst ::= "ITER"
  syntax NullaryInst ::= "SIZE"
  // Big_map operations
  syntax NullaryInst ::= "EMPTY_BIG_MAP"
  syntax NullaryInst ::= "GET"
  syntax NullaryInst ::= "MEM"
  syntax NullaryInst ::= "UPDATE"
  // Option operations
  syntax NullaryInst ::= "SOME"
  syntax NullaryInst ::= "NONE"
  syntax BinaryInst  ::= "IF_NONE"
  // Union operations
  syntax NullaryInst ::= "LEFT"
  syntax NullaryInst ::= "RIGHT"
  syntax BinaryInst  ::= "IF_LEFT"
  // List operations
  syntax NullaryInst ::= "CONS"
  syntax NullaryInst ::= "NIL"
  syntax BinaryInst  ::= "IF_CONS"
  syntax NullaryInst ::= "MAP"
  syntax NullaryInst ::= "SIZE"
  syntax NullaryInst ::= "ITER"
  // Timestamp operations
  syntax NullaryInst ::= "ADD"
  syntax NullaryInst ::= "SUB"
  syntax NullaryInst ::= "COMPARE"
  // Mutez operations
  syntax NullaryInst ::= "ADD"
  syntax NullaryInst ::= "SUB"
  syntax NullaryInst ::= "MUL"
  syntax NullaryInst ::= "EDIV"
  syntax NullaryInst ::= "COMPARE"
  // Contract operations
  syntax TernaryInst ::= "CREATE_CONTRACT"
  syntax NullaryInst ::= "TRANSFER_TOKENS"
  syntax NullaryInst ::= "SET_DELEGATE"
  syntax NullaryInst ::= "BALANCE"
  syntax NullaryInst ::= "ADDRESS"
  syntax UnaryInst   ::= "CONTRACT"
  syntax NullaryInst ::= "SOURCE"
  syntax NullaryInst ::= "SENDER"
  syntax NullaryInst ::= "SELF"
  syntax NullaryInst ::= "AMOUNT"
  syntax NullaryInst ::= "IMPLICIT_ACCOUNT"
  // Special operations
  syntax NullaryInst ::= "NOW"
  syntax NullaryInst ::= "CHAIN_ID"
  // Byte operations
  syntax NullaryInst ::= "PACK"
  syntax UnaryInst   ::= "UNPACK"
  syntax NullaryInst ::= "CONCAT"
  syntax NullaryInst ::= "SIZE"
  syntax NullaryInst ::= "SLICE"
  syntax NullaryInst ::= "COMPARE"
  // Cryptographic operations
  syntax NullaryInst ::= "HASH_KEY"
  syntax NullaryInst ::= "BLAKE2B"
  syntax NullaryInst ::= "SHA256"
  syntax NullaryInst ::= "SHA512"
  syntax NullaryInst ::= "CHECK_SIGNATURE"
  syntax NullaryInst ::= "COMPARE"
  // No-op type-transforming instructions
  syntax UnaryInst   ::= "CAST"
  syntax UnaryInst   ::= "RENAME"
  // Deprecated instructions
  syntax UnaryInst   ::= "CREATE_CONTRACT"
  syntax NullaryInst ::= "CREATE_ACCOUNT"
  syntax NullaryInst ::= "STEPS_TO_QUOTA"
  // Extended instructions
  syntax NullaryInst ::= "STOP"
  syntax NullaryInst ::= "PAUSE"

  // DIG, DUG, DROP also have nullary versions

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
  syntax MichelsonData ::= InternalData
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
