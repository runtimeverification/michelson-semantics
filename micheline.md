# Micheline Format

We parse and validate Michelson in the following fashion:

1.  Parse initial program text as K-flavored Micheline AST (K-MICHELINE-CONCRETE-SYNTAX).
    K-flavored Micheline is different from Micheline in three respects:

    - no whitespace sensitivity
    - only admits primitives known to K-Michelson as instructions, macros, fields, or data constructors
    - only admits annotations known to K-Michelson

2.  Use internal hooks to re-interpret AST as abstract K-flavored Micheline AST (K-MICHELINE-ABSTRACT-SYNTAX)

    - collapse nested primitive application `"(" PrimitiveApplication ")"` and `PrimitiveApplication`
    - collapse Micheline nodes `MichelineNode` into subsort of primitive arguments `PrimitiveArg`
    - collapse Micheline node lists `MichelineNodes` into primitive argument lists `PrimitiveArgs`

    Note that in the abstract AST, ill-formed Micheline expressions are constructible.
    However, we only use this formart as a temporary container and immediately apply the next processing step.

3.  Transform via rewrite rules the abstract Micheline AST into K-flavored Micheline IR (K-MICHELINE-IR)

    - primitive argument arity is validated
    - annotation kind/arity is validated
    - macros are expanded

    Note special rules handle arity checking for macros and optional argument instructions `DROP` and `DIP`
    (and possibly `DIG` and `DUG` --- which, by default, would be either `SWAP` or a no-op).

4.  Apply type-checking pass that transforms the K-flavored Micheline IR into a typed K-Michelson IR.

```k
module K-MICHELINE-COMMON-SYNTAX
  syntax BytesToken
  syntax Primitive
  syntax Annotation
endmodule
```

```k
module K-MICHELINE-CONCRETE-SYNTAX
  imports K-MICHELINE-TO-MICHELSON-COMMON-SYNTAX
  imports INT-SYNTAX
  imports STRING-SYNTAX

  syntax Pgm ::= NeMichelineNodes [klabel(MichelsonPgm), symbol]

  syntax SequenceNode         ::= "{" EmptyMichelineNodes "}" [klabel(SeqNodeCtor), symbol]
                                | "{"    NeMichelineNodes "}" [klabel(SeqNodeCtor), symbol]
  syntax PrimitiveApplication ::= Primitive NePrimitiveArgs   [klabel(AppNodeCtor), symbol]

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

  // Primtive Args
  syntax EmptyPrimitiveArgs ::= "" [klabel(.PrimArgs), symbol]
  syntax NePrimitiveArgs ::= PrimitiveArg EmptyPrimitiveArgs [klabel(PrimArgsCons), symbol]
                           | PrimitiveArg NePrimitiveArgs    [klabel(PrimArgsCons), symbol]

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
module K-MICHELINE-ABSTRACT-SYNTAX
  imports K-MICHELINE-TO-MICHELSON-COMMON-SYNTAX
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

  syntax PrimitiveArgs  ::= ".PrimitiveArgs"           [klabel(.PrimArgs), symbol]
                          | PrimitiveArg PrimitiveArgs [klabel(PrimArgsCons), symbol, right]
endmodule
```

```k
module K-MICHELINE-IR
  imports K-MICHELINE-TO-MICHELSON-COMMON-SYNTAX
  imports K-MICHELINE-ABSTRACT-SYNTAX
  imports INT
  imports STRING
  imports BYTES

  syntax MichelineNode   ::= MichelineIRNode
  syntax MichelineIRNode ::= Int
                           | String
                           | Bytes
                           | Inst(Instruction, AnnotationData, MichelineIRNodes)
                           | Macro(Macro, AnnotationData, MichelineIRNodes)
                           | Data(MichelsonData, AnnotationData, MichelineIRNodes)
                           | Field(Field, MichelineIRNodes)
                           | Seq(MichelineIRNodes)

  syntax MichelineIRNodes ::= List{MichelineIRNode, ""}

  syntax AnnotationData      ::= VarAnnotationList TypeAnnotationList FieldAnnotationList
  syntax VarAnnotationList   ::= List{VarAnnotation,   ";"}
  syntax TypeAnnotationList  ::= List{TypeAnnotation,  ";"}
  syntax FieldAnnotationList ::= List{FieldAnnotation, ";"}

  syntax AnnotationData  ::= "noAnnotData" [function, functional]
  rule noAnnotData => .VarAnnotationList .TypeAnnotationList .FieldAnnotationList
endmodule
```

```k
module K-MICHELINE-IR-TRANSLATION
  imports K-MICHELINE-IR

  syntax MichelineIRNode ::= NodeToPrim(Primitive, PrimArgData)

  // miscellaneous helper functions
  syntax PrimArgData ::= #PAD(AnnotationData, PrimitiveArgs)

  syntax PrimArgData ::= toPrimArgData(PrimitiveArgs)              [function]
                       | toPrimArgData(PrimitiveArgs, PrimArgData) [function]

  rule toPrimArgData(Args) => toPrimArgData(Args, #PAD(noAnnotData, .PrimitiveArgs))
  // TODO: reverse the argument list before returning it because it is flipped
  rule toPrimArgData(.PrimitiveArgs, PAD) => PAD
  rule toPrimArgData(A:VarAnnotation   Rest, #PAD(VAs TAs FAs, Args)) => toPrimArgData(Rest, #PAD(VAs ; A TAs FAs, Args))
  rule toPrimArgData(A:TypeAnnotation  Rest, #PAD(VAs TAs FAs, Args)) => toPrimArgData(Rest, #PAD(VAs TAs ; A FAs, Args))
  rule toPrimArgData(A:FieldAnnotation Rest, #PAD(VAs TAs FAs, Args)) => toPrimArgData(Rest, #PAD(VAs TAs FAs ; A, Args))
  rule toPrimArgData(N:MichelineNode   Rest, #PAD(VAs TAs FAs, Args)) => toPrimArgData(Rest, #PAD(VAs TAs FAs, N Args))
endmodule
```

```k
module MICHELSON-PARSER
  imports K-MICHELINE-TO-MICHELSON-COMMON-SYNTAX
  imports K-MICHELINE-IR-TRANSLATION

  syntax Pgm ::= PrimitiveArgs [klabel(MichelsonPgm), symbol]
  configuration <k> $PGM:Pgm </k>
endmodule
```

```k
module K-MICHELINE-TO-MICHELSON-COMMON-SYNTAX
  imports K-MICHELINE-COMMON-SYNTAX

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
