# Micheline Format

```k
module MICHELINE-COMMON-SYNTAX
  imports INT-SYNTAX
  imports STRING-SYNTAX
```

## Micheline Node Types

Micheline has five node types.

```k
  syntax MichelineNode
  //                       ::= Int
  //                         | String
  //                         | MichelineBytes
  //                         | SequenceNode
  //                         | Primitive
  //                         | PrimitiveApplication

  syntax BytesToken
  syntax MichelineBytes ::= BytesToken

  syntax Primitive

  syntax PrimitiveApplication ::= Primitive PrimitiveArgs

  syntax SequenceNode

  syntax PrimitiveArgs

  syntax PrimitiveArg ::= Int
                        | String
                        | MichelineBytes
                        | SequenceNode
                        | Primitive
                     // | "(" PrimitiveApplication ")"
                        | Annotation


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

  syntax Pgm ::= MichelineNodes [klabel(Injection2), symbol]

  syntax MichelineNode ::= Int
                         | String
                         | MichelineBytes
                         | SequenceNode
                         | Primitive
                         | PrimitiveApplication [klabel(PrimitiveApplication), symbol]

  syntax SequenceNode ::= "{" MichelineNodes "}" [klabel(SequenceNode), symbol]

  //
  syntax EmptyMichelineNodes          ::= ""  [klabel(.PrimitiveArgs), symbol]
  syntax EmptyMichelineNodesSemiColon ::= ";" [klabel(.PrimitiveArgs), symbol]
  syntax NeMichelineNodes ::= MichelineNode EmptyMichelineNodes          [klabel(PrimitiveArgs), symbol]
                            | MichelineNode EmptyMichelineNodesSemiColon [klabel(PrimitiveArgs), symbol]
                            | MichelineNode ";" NeMichelineNodes         [klabel(PrimitiveArgs), symbol]
  syntax MichelineNodes   ::= NeMichelineNodes                           [klabel(Injection), symbol]
                            | EmptyMichelineNodes                        [klabel(Injection), symbol]

  //
  syntax EmptyPrimitiveArgs ::= "" [klabel(.PrimitiveArgs), symbol]
  syntax NePrimitiveArgs  ::= PrimitiveArg EmptyPrimitiveArgs  [klabel(PrimitiveArgs), symbol]
                              | PrimitiveArg NePrimitiveArgs     [klabel(PrimitiveArgs), symbol]
  syntax PrimitiveArgs ::= NePrimitiveArgs                     [klabel(Injection), symbol]
                         | EmptyPrimitiveArgs                  [klabel(Injection), symbol]

  syntax PrimitiveArg ::= "(" PrimitiveApplication ")" [klabel(PrimitiveApplication), symbol]

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

  // syntax MichelineNode ::= NodeToPrim(Primitive, PrimArgData)

  // rule (N:Primitive):MichelineNode => (0):MichelineNode [anywhere]

  // rule (N:Primitive):MichelineNode => NodeToPrim(N, #PAD(noAnnotData, .WrappedArgs)) [anywhere]
  // rule N:Primitive Args       => NodeToPrim(N, toPrimArgData(wrapArgs(Args)))   [anywhere]

  // rule NodeToPrim(I:NullaryInst, (Annots, NoArgs)) => Inst(I,NoAnnots,.MichelineIRNodes)
  // rule NodeToPrim(I:UnaryInst,   Arg)    => Inst(I,
  // rule NodeToPrim(I:UnaryInst, NoArgs) => ...
endmodule

module MICHELINE-INTERNAL-REPRESENTATION
  imports MICHELINE-TO-MICHELSON-COMMON-SYNTAX
  imports INT
  imports STRING
  imports BYTES

  syntax MichelineNode ::= Int
                         | String
                         | MichelineBytes
                         | SequenceNode
                         | Primitive
                         | PrimitiveApplication [klabel(PrimitiveApplication), symbol]

  syntax SequenceNode ::= "{" PrimitiveArgs "}" [klabel(SequenceNode), symbol]

  syntax PrimitiveArg ::= MichelineNode


  syntax EmptyPrimitiveArgs ::= ".PrimitiveArgs" [klabel(.PrimitiveArgs), symbol]
  syntax NePrimitiveArgs  ::= PrimitiveArg EmptyPrimitiveArgs  [klabel(PrimitiveArgs), symbol]
                              | PrimitiveArg NePrimitiveArgs     [klabel(PrimitiveArgs)]
  syntax PrimitiveArgs ::= NePrimitiveArgs                     [klabel(Injection), symbol]
                         | EmptyPrimitiveArgs                  [klabel(Injection)]


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
endmodule

module MICHELSON-PARSER-INTERNAL
  imports MICHELINE-TO-MICHELSON-COMMON-SYNTAX
  imports MICHELINE-INTERNAL-REPRESENTATION

  syntax Pgm ::= PrimitiveArgs [klabel(Injection2), symbol]
  configuration <k> $PGM:Pgm </k>

  /*
  syntax WrappedArg  ::= Arg(PrimitiveArg)
  syntax WrappedArgs ::= List{WrappedArg, ""}
                       | wrapArgs(PrimitiveArgs) [function]

  rule wrapArgs(A Args) => Arg(A) wrapArgs(Args)
  rule wrapArgs(A)      => Arg(A)

  syntax PrimArgData ::= #PAD(AnnotationData, WrappedArgs)
                       | toPrimArgData(WrappedArgs)              [function]
                       | toPrimArgData(WrappedArgs, PrimArgData) [function]

  rule toPrimArgData(Args) => toPrimArgData(Args, #PAD(noAnnotData, .WrappedArgs))

  rule toPrimArgData(.WrappedArgs, PAD) => PAD

  rule toPrimArgData(Arg(A:VarAnnotation)   Rest, #PAD(VAs TAs FAs, Args)) => toPrimArgData(Rest, #PAD(VAs ; A TAs FAs, Args))
  rule toPrimArgData(Arg(A:TypeAnnotation)  Rest, #PAD(VAs TAs FAs, Args)) => toPrimArgData(Rest, #PAD(VAs TAs ; A FAs, Args))
  rule toPrimArgData(Arg(A:FieldAnnotation) Rest, #PAD(VAs TAs FAs, Args)) => toPrimArgData(Rest, #PAD(VAs TAs FAs ; A, Args))
  rule toPrimArgData(Arg(N:NodeArg)         Rest, #PAD(VAs TAs FAs, Args)) => toPrimArgData(Rest, #PAD(VAs TAs FAs, Args Arg(N)))
  */
endmodule
```

1. Micheline-Like    - all primitives are well-defined
2. MichelineIR       - all primitives take correct number of args / annotations
3. TypedMichelsonIR  - convert IR to typed version --- progress guaranteed

