# Micheline Format

We parse and validate Michelson in the following fashion:

1.  Parse initial program text as K-flavored Micheline AST (K-MICHELINE-CONCRETE-SYNTAX).
    K-flavored Micheline is different from Micheline in three respects:

    - no whitespace sensitivity
    - only admits primitives known to K-Michelson as instructions, macros, fields, or data constructors
    - only admits annotations known to K-Michelson
    - lexer limitation means annotations with a number immediately following the sigil are tokenized as
      two separate tokens instead of a single annotation (`@1` tokenizes as `@` and `1`)

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

  syntax MichelineNode  ::= MichelineNode0       [klabel(Node),       symbol]
  syntax MichelineNode0 ::= Int                  [klabel(IntNode),    symbol]
                          | String               [klabel(StringNode), symbol]
                          | BytesToken           [klabel(BytesNode),  symbol]
                          | SequenceNode         [klabel(SeqNode),    symbol]
                          | Primitive            [klabel(PrimNode),   symbol]
                          | PrimitiveApplication [klabel(AppNode),    symbol]

  syntax PrimitiveArg  ::= PrimitiveArg0                [klabel(Node),       symbol]
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
  imports MAP

  // Abstract
  syntax SequenceNode         ::= SeqNodeCtor(AbsPrimitiveArgs)           [klabel(SeqNodeCtor), symbol]
  syntax PrimitiveApplication ::= AppNodeCtor(Primitive,AbsPrimitiveArgs) [klabel(AppNodeCtor), symbol]

  syntax AbsMichelineNode ::= IntNode(Int)               [klabel(IntNode),    symbol]
                         | StringNode(String)            [klabel(StringNode), symbol]
                         | BytesNode(BytesToken)         [klabel(BytesNode),  symbol]
                         | SeqNode(SequenceNode)         [klabel(SeqNode),    symbol]
                         | PrimNode(Primitive)           [klabel(PrimNode),   symbol]
                         | AppNode(PrimitiveApplication) [klabel(AppNode),    symbol]

  syntax AbsPrimitiveArg ::= Node(AbsMichelineNode) [klabel(Node),     symbol]
                        | AnnotArg(Annotation)   [klabel(AnnotArg), symbol]

  syntax AbsPrimitiveArgs ::= ".AbsPrimitiveArgs"               [klabel(.PrimArgs),    symbol       ]
                         | AbsPrimitiveArg "|" AbsPrimitiveArgs [klabel(PrimArgsCons), symbol, right]

  // Semantics
  syntax AbsMichelineNode ::= MichelineNode
  syntax AbsPrimitiveArg  ::= PrimitiveArg

  syntax MichelineNodes ::= MichelineNode ";" MichelineNodes
                          | ".Nodes"
  syntax PrimitiveArgs ::= List{PrimitiveArg,""}

  syntax MichelineNode ::= Int
                         | String
                         | BytesToken
                         | "{" MichelineNodes "}"
                         | Primitive Map PrimitiveArgs

  syntax PrimitiveArg ::= MichelineNode
                        | Annotation

  rule IntNode(I)    => I [anywhere]
  rule StringNode(S) => S [anywhere]
  rule BytesNode(B)  => B [anywhere]
  rule AnnotArg(A)   => A [anywhere]
  rule Node(N)       => N [anywhere]
endmodule
```

```k
module K-MICHELINE-PRIMITIVE
  imports K-MICHELINE-ABSTRACT-SYNTAX
  imports INT
  imports STRING
  imports BYTES

  rule SeqNode(S)    => "{" toNodes(S) "}" [anywhere]

  // Internal Primitive Definition
  // Primitive has three arguments
  //
  // 1. Primitive name
  // 2. `Map` argument has type: AnnotationType |-> AnnotationList
  // 3. Primitive argument list (possibly empty)
  //
  syntax AnnotationType ::= "#@" | "#:" | "#%" | "#!"
  syntax AnnotationList ::= List{Annotation, ";"}

  // Internal Primitive Conversion
  syntax MichelineNode ::= toPrim(Primitive, PrimArgData)

  rule PrimNode(N:Primitive)                  => toPrim(N, #PAD(newAnnotMap, .AbsPrimitiveArgs)) [anywhere]
  rule AppNode(AppNodeCtor(N:Primitive,Args)) => toPrim(N, toPrimArgData(Args))                  [anywhere]
  rule toPrim(P,#PAD(Annots, Args))           => P Annots Args                                   [anywhere]

  // Auxiliary Functions
  // list helpers
  syntax MichelineNodes ::= toNodes(AbsPrimitiveArgs) [function]
  rule toNodes(N:AbsMichelineNode | Args) => N ; toNodes(Args)
  rule toNodes(.AbsPrimitiveArgs)         => .Nodes

  syntax AnnotationList ::= revAnnots(AnnotationList)                 [function, functional]
                          | revAnnots(AnnotationList, AnnotationList) [function, functional]
  rule revAnnots(As) => revAnnots(As, .AnnotationList)
  rule revAnnots(A ; As, As') => revAnnots(As, A ; As')
  rule revAnnots(.AnnotationList, As') => As'

  syntax PrimitiveArgs ::= revArgs(PrimitiveArgs)                [function, functional]
                         | revArgs(PrimitiveArgs, PrimitiveArgs) [function, functional]
  rule revArgs(As) => revArgs(As, .PrimitiveArgs)
  rule revArgs(A | As:PrimitiveArgs, As':PrimitiveArgs) => revArgs(As, A | As')
  rule revArgs(      .PrimitiveArgs, As':PrimitiveArgs) => As'

  // map helpers
  syntax Map ::= "newAnnotMap" [function, functional]
  rule newAnnotMap => #@ |-> .AnnotationList
                      #: |-> .AnnotationList
                      #% |-> .AnnotationList
                      #! |-> .AnnotationList

  // Primitive argument annotation separator
  syntax PrimArgData ::= #PAD(Map, PrimitiveArgs)

  syntax PrimArgData ::= toPrimArgData(PrimitiveArgs)              [function]
                       | toPrimArgData(PrimitiveArgs, PrimArgData) [function]

  rule toPrimArgData(Others) => toPrimArgData(Others, #PAD(newAnnotMap, .PrimitiveArgs))
  rule toPrimArgData(A:VarAnnotation   Rest, #PAD(Annots #@ |-> As,       Others))
    => toPrimArgData(Rest,                   #PAD(Annots #@ |-> A ; As,   Others))
  rule toPrimArgData(A:TypeAnnotation  Rest, #PAD(Annots #: |-> As,       Others))
    => toPrimArgData(Rest,                   #PAD(Annots #: |-> A ; As,   Others))
  rule toPrimArgData(A:FieldAnnotation Rest, #PAD(Annots #% |-> As,       Others))
    => toPrimArgData(Rest,                   #PAD(Annots #% |-> A ; As,   Others))
  rule toPrimArgData(N:MichelineNode   Rest, #PAD(Annots,                 Others))
    => toPrimArgData(Rest,                   #PAD(Annots,               N Others))
  rule toPrimArgData(.PrimitiveArgs, #PAD(#@ |-> VAs #: |-> TAs #% |-> FAs #! |-> LAs, Others))
    => #PAD(#@ |-> revAnnots(VAs)
            #: |-> revAnnots(TAs)
            #% |-> revAnnots(FAs)
            #! |-> revAnnots(LAs),
            revArgs(Others))
endmodule
```

```k
module MICHELSON-PARSER
  imports K-MICHELINE-PRIMITIVE

  syntax Pgm ::= PrimitiveArgs [klabel(MichelsonPgm), symbol]
  configuration <k> $PGM:Pgm </k>
endmodule
```

### Notes about annotations and argument arity.

#### Non-annotation argument arity:

Possible exception: `DUP` may have unary macro version.

-   Types: fixed
-   Instructions: fixed except `DIG`, `DUG`, and `DROP` have 0 or 1 and `DIP` has 1 or 2
-   Macros: fixed
-   Fields: fixed at 1
-   Data: fixed

#### Annotation argument arity:

Note: fields and data do NOT accept annotations.

##### Variable annotation argument arity:

-   Types: 0
-   Instructions:
    - 0 - `DROP` `SWAP` `DIG` `DUG` `IF_NONE` `IF_LEFT` `IF_CONS` `ITER` `IF` `LOOP` `LOOP_LEFT` `DIP` `FAILWITH`
    - 1 - default
    - 2 - `CREATE_ACCOUNT` `CREATE_CONTRACT`
-   Macros
    - 0 - default
    - 1 - `DUP` `CADR` `CMP{OP}` `SET_CADR` `MAP_CADR` `PAIR`
    - n - `UNPAIR`

##### Type annotation argument arity:

-   Types: 1
-   Instructions:
    - 0 - default
    - 1 - `CAST` `UNIT` `PAIR` `SOME` `NONE` `LEFT` `RIGHT` `NIL` `EMPTY_SET` `EMPTY_MAP` `EMPTY_BIG_MAP`
-   Macros
    - 0 - default

##### Field annotation argument arity:

-   Types: fixed
-   Instructions:
    - 0 - default
    - 1 - `CDR` `CAR`
    - 2 - `PAIR` `LEFT` `RIGHT`
-   Macros:
    - 0 - default
    - 1 - `CADR` `SET_CADR` `MAP_CADR`
    - n - `PAIR`

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
  // Pushable types
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
                | "pair"
                | "option"
                | "list"
                | "set"
                | "or"
                | "lambda"
                | "map"
  // Non-pushable types
                | "operation"
                | "contract"
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

```k
module K-MICHELSON-PRIMTIVE-ARITY
  imports K-MICHELINE-TO-MICHELSON-COMMON-SYNTAX
  imports INT

  syntax IntOrAny ::= Int
                    | "AnyInt"

  syntax ArgType ::= "IntArg"
                   | "DataArg"
                   | "CodeArg"
                   | "TypeArg"

  syntax ArgTypeSpec ::= List{ArgType, ""}
                       | "Special"

  syntax ArityData ::= Arity(varAnnotation:   IntOrAny,
                             typeAnnotation:  Int,
                             fieldAnnotation: IntOrAny,
                             loopAnnotation:  Int,
                             argTypes:        ArgTypeSpec)

  syntax MichelsonContext ::= "FieldCon"
                            | "TypeCon"
                            | "InstCon"
                            | "DataCon"

  syntax ContextualArityData    ::= MichelsonContext "|->" ArityData
  syntax ContextualArityDataMap ::= List{ContextualArityData, ""}

  syntax ArityData ::= arity(Primitive) [function, functional]

  rule arity(int)       => Arity(0, 0, 1, 0, 0)
  rule arity(nat)       => Arity(0, 0, 1, 0, 0)
  rule arity(string)    => Arity(0, 0, 1, 0, 0)
  rule arity(bytes)     => Arity(0, 0, 1, 0, 0)
  rule arity(mutez)     => Arity(0, 0, 1, 0, 0)
  rule arity(bool)      => Arity(0, 0, 1, 0, 0)
  rule arity(key_hash)  => Arity(0, 0, 1, 0, 0)
  rule arity(timestamp) => Arity(0, 0, 1, 0, 0)
  rule arity(address)   => Arity(0, 0, 1, 0, 0)
  rule arity(key)       => Arity(0, 0, 1, 0, 0)
  rule arity(unit)      => Arity(0, 0, 1, 0, 0)
  rule arity(signature) => Arity(0, 0, 1, 0, 0)
  rule arity(operation) => Arity(0, 0, 1, 0, 0)
  rule arity(chain_id)  => Arity(0, 0, 1, 0, 0) // ambiguous with field name
  rule arity(contract)  => Arity(0, 0, 1, 0, 0) // ambiguous with field name

  rule arity(option)    => Arity(1, 0, 1, 0, 0)
  rule arity(list)      => Arity(1, 0, 1, 0, 0)
  rule arity(set)       => Arity(1, 0, 1, 0, 0)

  rule arity(map)       => Arity(2, 0, 1, 0, 0)
  rule arity(big_map)   => Arity(2, 0, 1, 0, 0)
  rule arity(lambda)    => Arity(2, 0, 1, 0, 0)

  rule arity(pair)      => Arity(2, 0, 1, 2, 0)
  rule arity(or)        => Arity(2, 0, 1, 2, 0)


  // One loop annotation
  // No variable annotations
  rule arity(ITER)             => Arity(1, 0, 0, 0, 1)
  rule arity(LOOP)             => Arity(1, 0, 0, 0, 1)
  rule arity(LOOP_LEFT)        => Arity(1, 0, 0, 0, 1)
  // One variable annotation
  rule arity(EXEC)             => Arity(0, 1, 0, 0, 1)
  rule arity(MAP)              => Arity(1, 1, 0, 0, 1)

  // No loop annotations
  // No variable annotations
  rule arity(DROP)             => Arity(1, 0, 0, 0, 0) // also nullary version
  rule arity(DIG)              => Arity(1, 0, 0, 0, 0) // also nullary version
  rule arity(DUG)              => Arity(1, 0, 0, 0, 0) // also nullary version
  rule arity(DIP)              => Arity(2, 0, 0, 0, 0) // also unary version
  rule arity(SWAP)             => Arity(0, 0, 0, 0, 0)
  rule arity(FAILWITH)         => Arity(0, 0, 0, 0, 0)
  rule arity(IF_NONE)          => Arity(2, 0, 0, 0, 0)
  rule arity(IF_LEFT)          => Arity(2, 0, 0, 0, 0)
  rule arity(IF_CONS)          => Arity(2, 0, 0, 0, 0)
  rule arity(IF)               => Arity(2, 0, 0, 0, 0)

  // Two variable annotations
  rule arity(CREATE_ACCOUNT)   => Arity(0, 2, 0, 0, 0)
  rule arity(CREATE_CONTRACT)  => Arity(1, 2, 0, 0, 0)

  // "PUSH"-like instructions: one variable annotation, one type annotation
  rule arity(UNIT)             => Arity(0, 1, 1, 0, 0)
  rule arity(SOME)             => Arity(0, 1, 1, 0, 0)
  rule arity(CONS)             => Arity(0, 1, 1, 0, 0)
  rule arity(NONE)             => Arity(1, 1, 1, 0, 0)
  rule arity(NIL)              => Arity(1, 1, 1, 0, 0)
  rule arity(EMPTY_SET)        => Arity(1, 1, 1, 0, 0)
  rule arity(EMPTY_MAP)        => Arity(2, 1, 1, 0, 0)
  rule arity(EMPTY_BIG_MAP)    => Arity(2, 1, 1, 0, 0)

  // One variable annotation, one type annotation, and two field annotations
  rule arity(PAIR)             => Arity(0, 1, 1, 2, 0)
  rule arity(LEFT)             => Arity(1, 1, 1, 2, 0)
  rule arity(RIGHT)            => Arity(1, 1, 1, 2, 0)

  // Nullary, one variable annotation ONLY
  rule arity(APPLY)            => Arity(0, 1, 0, 0, 0)
  rule arity(DUP)              => Arity(0, 1, 0, 0, 0) // MAYBE: also unary macro version?
  rule arity(EQ)               => Arity(0, 1, 0, 0, 0)
  rule arity(NEQ)              => Arity(0, 1, 0, 0, 0)
  rule arity(LT)               => Arity(0, 1, 0, 0, 0)
  rule arity(GT)               => Arity(0, 1, 0, 0, 0)
  rule arity(LE)               => Arity(0, 1, 0, 0, 0)
  rule arity(GE)               => Arity(0, 1, 0, 0, 0)
  rule arity(OR)               => Arity(0, 1, 0, 0, 0)
  rule arity(AND)              => Arity(0, 1, 0, 0, 0)
  rule arity(XOR)              => Arity(0, 1, 0, 0, 0)
  rule arity(NOT)              => Arity(0, 1, 0, 0, 0)
  rule arity(NEG)              => Arity(0, 1, 0, 0, 0)
  rule arity(ABS)              => Arity(0, 1, 0, 0, 0)
  rule arity(ISNAT)            => Arity(0, 1, 0, 0, 0)
  rule arity(INT)              => Arity(0, 1, 0, 0, 0)
  rule arity(ADD)              => Arity(0, 1, 0, 0, 0)
  rule arity(SUB)              => Arity(0, 1, 0, 0, 0)
  rule arity(MUL)              => Arity(0, 1, 0, 0, 0)
  rule arity(EDIV)             => Arity(0, 1, 0, 0, 0)
  rule arity(COMPARE)          => Arity(0, 1, 0, 0, 0)
  rule arity(LSL)              => Arity(0, 1, 0, 0, 0)
  rule arity(LSR)              => Arity(0, 1, 0, 0, 0)
  rule arity(CONCAT)           => Arity(0, 1, 0, 0, 0)
  rule arity(SIZE)             => Arity(0, 1, 0, 0, 0)
  rule arity(SLICE)            => Arity(0, 1, 0, 0, 0)
  rule arity(CAR)              => Arity(0, 1, 0, 0, 0)
  rule arity(CDR)              => Arity(0, 1, 0, 0, 0)
  rule arity(MEM)              => Arity(0, 1, 0, 0, 0)
  rule arity(UPDATE)           => Arity(0, 1, 0, 0, 0)
  rule arity(GET)              => Arity(0, 1, 0, 0, 0)
  rule arity(TRANSFER_TOKENS)  => Arity(0, 1, 0, 0, 0)
  rule arity(SET_DELEGATE)     => Arity(0, 1, 0, 0, 0)
  rule arity(BALANCE)          => Arity(0, 1, 0, 0, 0)
  rule arity(ADDRESS)          => Arity(0, 1, 0, 0, 0)
  rule arity(SOURCE)           => Arity(0, 1, 0, 0, 0)
  rule arity(SENDER)           => Arity(0, 1, 0, 0, 0)
  rule arity(SELF)             => Arity(0, 1, 0, 0, 0)
  rule arity(AMOUNT)           => Arity(0, 1, 0, 0, 0)
  rule arity(IMPLICIT_ACCOUNT) => Arity(0, 1, 0, 0, 0)
  rule arity(NOW)              => Arity(0, 1, 0, 0, 0)
  rule arity(CHAIN_ID)         => Arity(0, 1, 0, 0, 0)
  rule arity(PACK)             => Arity(0, 1, 0, 0, 0)
  rule arity(HASH_KEY)         => Arity(0, 1, 0, 0, 0)
  rule arity(BLAKE2B)          => Arity(0, 1, 0, 0, 0)
  rule arity(SHA256)           => Arity(0, 1, 0, 0, 0)
  rule arity(SHA512)           => Arity(0, 1, 0, 0, 0)
  rule arity(CHECK_SIGNATURE)  => Arity(0, 1, 0, 0, 0)
  rule arity(CAST)             => Arity(0, 1, 0, 0, 0)
  rule arity(RENAME)           => Arity(0, 1, 0, 0, 0)
  rule arity(STEPS_TO_QUOTA)   => Arity(0, 1, 0, 0, 0)
  rule arity(STOP)             => Arity(0, 1, 0, 0, 0)
  rule arity(PAUSE)            => Arity(0, 1, 0, 0, 0)

  // Non-nullary, one variable annotation ONLY
  rule arity(CONTRACT)         => Arity(1, 1, 0, 0, 0)
  rule arity(UNPACK)           => Arity(1, 1, 0, 0, 0)
  rule arity(PUSH)             => Arity(2, 1, 0, 0, 0)
  rule arity(LAMBDA)           => Arity(3, 1, 0, 0, 0)

  rule arity(CMPEQ)         => Arity(0, 1, 0, 0, 0)
  rule arity(CMPNEQ)        => Arity(0, 1, 0, 0, 0)
  rule arity(CMPLT)         => Arity(0, 1, 0, 0, 0)
  rule arity(CMPGT)         => Arity(0, 1, 0, 0, 0)
  rule arity(CMPLE)         => Arity(0, 1, 0, 0, 0)
  rule arity(CMPGE)         => Arity(0, 1, 0, 0, 0)

  rule arity(IFEQ)          => Arity(2, 0, 0, 0, 0)
  rule arity(IFNEQ)         => Arity(2, 0, 0, 0, 0)
  rule arity(IFLT)          => Arity(2, 0, 0, 0, 0)
  rule arity(IFGT)          => Arity(2, 0, 0, 0, 0)
  rule arity(IFLE)          => Arity(2, 0, 0, 0, 0)
  rule arity(IFGE)          => Arity(2, 0, 0, 0, 0)
  rule arity(IFCMPEQ)       => Arity(2, 0, 0, 0, 0)
  rule arity(IFCMPNEQ)      => Arity(2, 0, 0, 0, 0)
  rule arity(IFCMPLT)       => Arity(2, 0, 0, 0, 0)
  rule arity(IFCMPGT)       => Arity(2, 0, 0, 0, 0)
  rule arity(IFCMPLE)       => Arity(2, 0, 0, 0, 0)
  rule arity(IFCMPGE)       => Arity(2, 0, 0, 0, 0)
  rule arity(IF_SOME)       => Arity(2, 0, 0, 0, 0)
  rule arity(IF_RIGHT)      => Arity(2, 0, 0, 0, 0)

  rule arity(FAIL)          => Arity(0, 0, 0, 0, 0)
  rule arity(ASSERT)        => Arity(0, 0, 0, 0, 0)
  rule arity(ASSERT_EQ)     => Arity(0, 0, 0, 0, 0)
  rule arity(ASSERT_NEQ)    => Arity(0, 0, 0, 0, 0)
  rule arity(ASSERT_LT)     => Arity(0, 0, 0, 0, 0)
  rule arity(ASSERT_LE)     => Arity(0, 0, 0, 0, 0)
  rule arity(ASSERT_GT)     => Arity(0, 0, 0, 0, 0)
  rule arity(ASSERT_GE)     => Arity(0, 0, 0, 0, 0)
  rule arity(ASSERT_CMPEQ)  => Arity(0, 0, 0, 0, 0)
  rule arity(ASSERT_CMPNEQ) => Arity(0, 0, 0, 0, 0)
  rule arity(ASSERT_CMPLT)  => Arity(0, 0, 0, 0, 0)
  rule arity(ASSERT_CMPLE)  => Arity(0, 0, 0, 0, 0)
  rule arity(ASSERT_CMPGT)  => Arity(0, 0, 0, 0, 0)
  rule arity(ASSERT_CMPGE)  => Arity(0, 0, 0, 0, 0)
  rule arity(ASSERT_NONE)   => Arity(0, 0, 0, 0, 0)
  rule arity(ASSERT_SOME)   => Arity(0, 0, 0, 0, 0)
  rule arity(ASSERT_LEFT)   => Arity(0, 0, 0, 0, 0)
  rule arity(ASSERT_RIGHT)  => Arity(0, 0, 0, 0, 0)

  // CADR macros
  rule arity(CADRMacro)     => Arity(0, 1, 0, 1, 0)
  rule arity(SETCADRMacro)  => Arity(0, 1, 0, 1, 0)
  rule arity(MAPCADRMacro)  => Arity(1, 1, 0, 1, 0)

  // PAIR macros
  rule arity(PAIRMacro)     => Arity(0, 1,   0, Any, 0)
  rule arity(UNPAIRMacro)   => Arity(0, Any, 0, 0,   0)

  // legacy macros?
  rule arity(DUPMacro)      => Arity(0, 1, 0, 0, 0)
  rule arity(DIPMacro)      => Arity(1, 0, 0, 0, 0)

  // all fields accept one argument and NO annotations
  rule arity(code)            => Arity(1, 0, 0, 0, 0)
  rule arity(storage)         => Arity(1, 0, 0, 0, 0)
  rule arity(parameter)       => Arity(1, 0, 0, 0, 0)
  rule arity(now)             => Arity(1, 0, 0, 0, 0)
  rule arity(sender)          => Arity(1, 0, 0, 0, 0)
  rule arity(source)          => Arity(1, 0, 0, 0, 0)
  rule arity(self)            => Arity(1, 0, 0, 0, 0)
  rule arity(amount)          => Arity(1, 0, 0, 0, 0)
  rule arity(balance)         => Arity(1, 0, 0, 0, 0)
  rule arity(other_contracts) => Arity(1, 0, 0, 0, 0)
  rule arity(big_maps)        => Arity(1, 0, 0, 0, 0)
  rule arity(input)           => Arity(1, 0, 0, 0, 0)
  rule arity(output)          => Arity(1, 0, 0, 0, 0)
  rule arity(parameter_value) => Arity(1, 0, 0, 0, 0)
  rule arity(storage_value)   => Arity(1, 0, 0, 0, 0)
  rule arity(precondition)    => Arity(1, 0, 0, 0, 0)
  rule arity(postcondition)   => Arity(1, 0, 0, 0, 0)
  rule arity(invariant)       => Arity(1, 0, 0, 0, 0)
  // ambiguous with type names
  rule arity(chain_id)        => Arity(1, 0, 0, 0, 0)
  rule arity(contract)        => Arity(1, 0, 0, 0, 0)


  // All Michelson data literals accept no annotations
  // Basic Michelson data literals
  rule arity(Unit)              => Arity(0, 0, 0, 0, 0)
  rule arity(True)              => Arity(0, 0, 0, 0, 0)
  rule arity(False)             => Arity(0, 0, 0, 0, 0)
  rule arity(Some)              => Arity(1, 0, 0, 0, 0)
  rule arity(None)              => Arity(1, 0, 0, 0, 0)
  rule arity(Left)              => Arity(1, 0, 0, 0, 0)
  rule arity(Right)             => Arity(1, 0, 0, 0, 0)
  rule arity(Elt)               => Arity(2, 0, 0, 0, 0)
  rule arity(Pair)              => Arity(2, 0, 0, 0, 0)

  // Michelson stack literals
  rule arity(Stack_elt)         => Arity(2, 0, 0, 0, 0)
  rule arity(Failed)            => Arity(1, 0, 0, 0, 0)
  rule arity(MutezOverflow)     => Arity(2, 0, 0, 0, 0)
  rule arity(MutezUnderflow)    => Arity(2, 0, 0, 0, 0)
  rule arity(GeneralOverflow)   => Arity(2, 0, 0, 0, 0)

  // Michelson operation literals
  rule arity(Transfer_token)    => Arity(4, 0, 0, 0, 0)
  rule arity(Create_contract)   => Arity(5, 0, 0, 0, 0)
  rule arity(Set_delegate)      => Arity(2, 0, 0, 0, 0)

  // Michelson big_map literals
  rule arity(Big_map)           => Arity(4, 0, 0, 0, 0)

  // Special literals
  rule arity(AnyToken)          => Arity(0, 0, 0, 0, 0)
  rule arity(#Any)              => Arity(0, 0, 0, 0, 0)
  rule arity(SymbolicPrimitive) => Arity(0, 0, 0, 0, 0)
endmodule
```
