Compatability Layer for Tezos Client
====================================

For cross-validating this interpreter with the Tezos Client, we need a
compatability layer to translate inputs/outputs back and forth.

Michelson Unparser
------------------

The unparser converts from an internal representation to Michelson source.

```k
requires "json.md"
requires "michelson.md"

module MICHELSON-UNPARSER
  imports MICHELSON-MACRO-SYNTAX
  imports UNIT-TEST-COMMON-SYNTAX
  imports MICHELSON-COMMON

  // We add sorts here to simplify when terms are primitive arguments
  syntax ApplicationData ::= Pair | OrData | OptionData
  syntax Data ::= ApplicationData

  // Michelson IR Unparsing Entrypoint
  syntax String ::= #unparse(K) [function]
  rule #unparse(O) => #doUnparse(O, false)

  // First argument is term to unparse, second arg is true iff term is a primitive argument
  syntax String ::= #doUnparse(K, Bool) [function]

  rule #doUnparse(Elt D1:Data D2:Data, false) =>
    "Elt " +String
    #doUnparse(D1, true)
    +String " " +String
    #doUnparse(D2, true)

  rule #doUnparse(D1:Data ; DL:DataList, _) =>
    #doUnparse(D1, false) +String
    " ; " +String
    #doUnparse(DL, false)

  rule #doUnparse(M1:MapEntry ; ML:MapEntryList, _) =>
    #doUnparse(M1, false) +String
    " ; " +String
    #doUnparse(ML, false)

  syntax String ::= #unparseTypeAnnotation(TypeAnnotation) [function, functional, hook(STRING.token2string)]
  syntax String ::= #unparseVariableAnnotation(VariableAnnotation) [function, functional, hook(STRING.token2string)]
  syntax String ::= #unparseFieldAnnotation(FieldAnnotation) [function, functional, hook(STRING.token2string)]

  rule #doUnparse(A, _) => #unparseTypeAnnotation(A)
  rule #doUnparse(A, _) => #unparseVariableAnnotation(A)
  rule #doUnparse(A, _) => #unparseFieldAnnotation(A)

  rule #doUnparse(.AnnotationList, _) => ""

  rule #doUnparse(A:Annotation AL:AnnotationList, _) =>
    #doUnparse(A, false) +String
    " " +String
    #doUnparse(AL, false)


  rule #doUnparse(Pair D1 D2, false) =>
    "Pair " +String
    #doUnparse(D1, true) +String
    " " +String
    #doUnparse(D2, true)

  rule #doUnparse(Left D, false) => "Left " +String #doUnparse(D, true)
  rule #doUnparse(Right D, false) => "Right " +String #doUnparse(D, true)

  rule #doUnparse(Some D, false) => "Some " +String #doUnparse(D, true)
  rule #doUnparse(None, false) => "None"

  rule #doUnparse(D:ApplicationData, true) => "(" +String #doUnparse(D, false) +String ")"

  rule #doUnparse({ ML:MapEntryList }, _) => "{" +String #doUnparse(ML, false) +String "}"
  rule #doUnparse({ DL:DataList }, _) => "{" +String #doUnparse(DL, false) +String "}"
  rule #doUnparse({ }, _) => "{ }"

  rule #doUnparse(I:Int, _) => Int2String(I)
  rule #doUnparse(S:String, _) => "\"" +String S +String "\""

  syntax String ::= #unparseBytes(Bytes) [function, functional, hook(MICHELSON.tohexstring)]

  rule #doUnparse(M, _) => #unparseBytes(M)

  rule #doUnparse(true, _) => "True"
  rule #doUnparse(false, _) => "False"

  rule #doUnparse(True, _) => "True"
  rule #doUnparse(False, _) => "False"

  rule #doUnparse(Unit, _) => "Unit"

  rule #doUnparse(#Timestamp(I), _) => #doUnparse(I, false)
  rule #doUnparse(#ChainId(I), _) => #doUnparse(I, false)
  rule #doUnparse(#KeyHash(S), _) => #doUnparse(S, false)
  rule #doUnparse(#Mutez(I), _) => #doUnparse(I, false)
  rule #doUnparse(#Address(S), _) => #doUnparse(S, false)
  rule #doUnparse(#Contract(A, _), _) => #doUnparse(A, false)
  rule #doUnparse(#Key(S), _) => #doUnparse(S, false)
  rule #doUnparse(#Signature(S), _) => #doUnparse(S, false)

  // Unparse types.

  rule #doUnparse(int, _) => "int"
  rule #doUnparse(nat, _) => "nat"
  rule #doUnparse(string, _) => "string"
  rule #doUnparse(bytes, _) => "bytes"
  rule #doUnparse(mutez, _) => "mutez"
  rule #doUnparse(bool, _) => "bool"
  rule #doUnparse(key_hash, _) => "key_hash"
  rule #doUnparse(timestamp, _) => "timestamp"
  rule #doUnparse(address, _) => "address"
  rule #doUnparse(key, _) => "key"
  rule #doUnparse(unit, _) => "unit"
  rule #doUnparse(signature, _) => "signature"
  rule #doUnparse(operation, _) => "operation"
  rule #doUnparse(chain_id, _) => "chain_id"
  rule #doUnparse(option, _) => "option"
  rule #doUnparse(list, _) => "list"
  rule #doUnparse(set, _) => "set"
  rule #doUnparse(contract, _) => "contract"
  rule #doUnparse(pair, _) => "pair"
  rule #doUnparse(or, _) => "or"
  rule #doUnparse(lambda, _) => "lambda"
  rule #doUnparse(map, _) => "map"
  rule #doUnparse(big_map, _) => "big_map"

  rule #doUnparse(T:NullaryTypeName (A AL):AnnotationList, false) =>
    #doUnparse(T, false) +String
    " " +String
    #doUnparse(A AL, false)

  rule #doUnparse(T:NullaryTypeName (A AL):AnnotationList, true) =>
    "(" +String #doUnparse(T (A AL), false) +String ")"

  rule #doUnparse(T:NullaryTypeName .AnnotationList, _) => #doUnparse(T, false)

  rule #doUnparse(TyName:UnaryTypeName AL:AnnotationList T, false) =>
    #doUnparse(TyName, false) +String
    " " +String
    #doUnparse(AL, false) +String
    " " +String
    #doUnparse(T, true)

  rule #doUnparse(TyName:UnaryTypeName AL:AnnotationList T, true) =>
    "(" +String #doUnparse(TyName AL T, false) +String ")"

  rule #doUnparse(TyName:BinaryTypeName AL:AnnotationList T1 T2, false) =>
    #doUnparse(TyName, false) +String
    " " +String
    #doUnparse(AL, false) +String
    " " +String
    #doUnparse(T1, true) +String
    " " +String
    #doUnparse(T2, true)

  rule #doUnparse(TyName:BinaryTypeName AL:AnnotationList T1 T2, true) =>
    "(" +String #doUnparse(TyName AL T1 T2, false) +String ")"

  rule #doUnparse(DROP AList, _) => "DROP" +String #doUnparse(AList, false)
  rule #doUnparse(DROP AList I:Int, _) => "DROP" +String #doUnparse(AList, false) +String " " +String #doUnparse(I, true)
  rule #doUnparse(DIG AList I, _) => "DIG" +String #doUnparse(AList, false) +String " " +String #doUnparse(I, true)
  rule #doUnparse(DUG AList I, _) => "DUG" +String #doUnparse(AList, false) +String " " +String #doUnparse(I, true)
  rule #doUnparse(DUP AList, _) => "DUP" +String #doUnparse(AList, false)
  rule #doUnparse(SWAP AList, _) => "SWAP" +String #doUnparse(AList, false)
  rule #doUnparse(PUSH AList T D, _) => "PUSH" +String #doUnparse(AList, false) +String " " +String #doUnparse(T, true) +String " " +String #doUnparse(D, true)
  rule #doUnparse(SOME AList, _) => "SOME" +String #doUnparse(AList, false)
  rule #doUnparse(NONE AList T, _) => "NONE" +String #doUnparse(AList, false) +String " " +String #doUnparse(T, true)
  rule #doUnparse(UNIT AList, _) => "UNIT" +String #doUnparse(AList, false)
  rule #doUnparse(IF_NONE AList B1 B2, _) => "IF_NONE" +String #doUnparse(AList, false) +String " " +String #doUnparse(B1, true) +String " " +String #doUnparse(B2, true)
  rule #doUnparse(PAIR AList, _) => "PAIR" +String #doUnparse(AList, false)
  rule #doUnparse(UNPAIR AList, _) => "UNPAIR" +String #doUnparse(AList, false)
  rule #doUnparse(CAR AList, _) => "CAR" +String #doUnparse(AList, false)
  rule #doUnparse(CDR AList, _) => "CDR" +String #doUnparse(AList, false)
  rule #doUnparse(LEFT AList T, _) => "LEFT" +String #doUnparse(AList, false) +String " " +String #doUnparse(T, true)
  rule #doUnparse(RIGHT AList T, _) => "RIGHT" +String #doUnparse(AList, false) +String " " +String #doUnparse(T, true)
  rule #doUnparse(IF_LEFT AList B1 B2, _) => "IF_LEFT" +String #doUnparse(AList, false) +String " " +String #doUnparse(B1, true) +String " " +String #doUnparse(B2, true)
  rule #doUnparse(IF_RIGHT AList B1 B2, _) => "IF_RIGHT" +String #doUnparse(AList, false) +String " " +String #doUnparse(B1, true) +String " " +String #doUnparse(B2, true)
  rule #doUnparse(NIL AList T, _) => "NIL" +String #doUnparse(AList, false) +String " " +String #doUnparse(T, true)
  rule #doUnparse(CONS AList, _) => "CONS" +String #doUnparse(AList, false)
  rule #doUnparse(IF_CONS AList B1 B2, _) => "IF_CONS" +String #doUnparse(AList, false) +String " " +String #doUnparse(B1, true) +String " " +String #doUnparse(B2, true)
  rule #doUnparse(SIZE AList, _) => "SIZE" +String #doUnparse(AList, false)
  rule #doUnparse(EMPTY_SET AList T, _) => "EMPTY_SET" +String #doUnparse(AList, false) +String " " +String #doUnparse(T, true)
  rule #doUnparse(EMPTY_MAP AList T1 T2, _) => "EMPTY_MAP" +String #doUnparse(AList, false) +String " " +String #doUnparse(T1, true) +String " " +String #doUnparse(T2, true)
  rule #doUnparse(EMPTY_BIG_MAP AList T1 T2, _) => "EMPTY_BIG_MAP" +String #doUnparse(AList, false) +String " " +String #doUnparse(T1, true) +String " " +String #doUnparse(T2, true)
  rule #doUnparse(MAP AList B, _) => "MAP" +String #doUnparse(AList, false) +String " " +String #doUnparse(B, true)
  rule #doUnparse(ITER AList B, _) => "ITER" +String #doUnparse(AList, false) +String " " +String #doUnparse(B, true)
  rule #doUnparse(MEM AList, _) => "MEM" +String #doUnparse(AList, false)
  rule #doUnparse(GET AList, _) => "GET" +String #doUnparse(AList, false)
  rule #doUnparse(UPDATE AList, _) => "UPDATE" +String #doUnparse(AList, false)
  rule #doUnparse(IF AList B1 B2, _) => "IF" +String #doUnparse(AList, false) +String " " +String #doUnparse(B1, true) +String " " +String #doUnparse(B2, true)
  rule #doUnparse(LOOP AList B, _) => "LOOP" +String #doUnparse(AList, false) +String " " +String #doUnparse(B, true)
  rule #doUnparse(LOOP_LEFT AList B, _) => "LOOP_LEFT" +String #doUnparse(AList, false) +String " " +String #doUnparse(B, true)
  rule #doUnparse(LAMBDA AList T1 T2 B, _) => "LAMBDA" +String #doUnparse(AList, false) +String " " +String #doUnparse(T1, true) +String " " +String #doUnparse(T2, true) +String " " +String #doUnparse(B, true)
  rule #doUnparse(EXEC AList, _) => "EXEC" +String #doUnparse(AList, false)
  rule #doUnparse(APPLY AList, _) => "APPLY" +String #doUnparse(AList, false)
  rule #doUnparse(DIP AList B, _) => "DIP" +String #doUnparse(AList, false) +String " " +String #doUnparse(B, true)
  rule #doUnparse(DIP AList I:Int B, _) => "DIP" +String #doUnparse(AList, false) +String " " +String #doUnparse(I, true) +String " " +String #doUnparse(B, true)
  rule #doUnparse(FAILWITH AList, _) => "FAILWITH" +String #doUnparse(AList, false)
  rule #doUnparse(CAST AList, _) => "CAST" +String #doUnparse(AList, false)
  rule #doUnparse(RENAME AList, _) => "RENAME" +String #doUnparse(AList, false)
  rule #doUnparse(CONCAT AList, _) => "CONCAT" +String #doUnparse(AList, false)
  rule #doUnparse(SLICE AList, _) => "SLICE" +String #doUnparse(AList, false)
  rule #doUnparse(PACK AList, _) => "PACK" +String #doUnparse(AList, false)
  rule #doUnparse(UNPACK AList T, _) => "UNPACK" +String #doUnparse(AList, false) +String " " +String #doUnparse(T, true)
  rule #doUnparse(ADD AList, _) => "ADD" +String #doUnparse(AList, false)
  rule #doUnparse(SUB AList, _) => "SUB" +String #doUnparse(AList, false)
  rule #doUnparse(MUL AList, _) => "MUL" +String #doUnparse(AList, false)
  rule #doUnparse(EDIV AList, _) => "EDIV" +String #doUnparse(AList, false)
  rule #doUnparse(ABS AList, _) => "ABS" +String #doUnparse(AList, false)
  rule #doUnparse(ISNAT AList, _) => "ISNAT" +String #doUnparse(AList, false)
  rule #doUnparse(INT AList, _) => "INT" +String #doUnparse(AList, false)
  rule #doUnparse(NEG AList, _) => "NEG" +String #doUnparse(AList, false)
  rule #doUnparse(LSL AList, _) => "LSL" +String #doUnparse(AList, false)
  rule #doUnparse(LSR AList, _) => "LSR" +String #doUnparse(AList, false)
  rule #doUnparse(OR AList, _) => "OR" +String #doUnparse(AList, false)
  rule #doUnparse(AND AList, _) => "AND" +String #doUnparse(AList, false)
  rule #doUnparse(XOR AList, _) => "XOR" +String #doUnparse(AList, false)
  rule #doUnparse(NOT AList, _) => "NOT" +String #doUnparse(AList, false)
  rule #doUnparse(COMPARE AList, _) => "COMPARE" +String #doUnparse(AList, false)
  rule #doUnparse(EQ AList, _) => "EQ" +String #doUnparse(AList, false)
  rule #doUnparse(NEQ AList, _) => "NEQ" +String #doUnparse(AList, false)
  rule #doUnparse(LT AList, _) => "LT" +String #doUnparse(AList, false)
  rule #doUnparse(GT AList, _) => "GT" +String #doUnparse(AList, false)
  rule #doUnparse(LE AList, _) => "LE" +String #doUnparse(AList, false)
  rule #doUnparse(GE AList, _) => "GE" +String #doUnparse(AList, false)
  rule #doUnparse(SELF AList, _) => "SELF" +String #doUnparse(AList, false)
  rule #doUnparse(CONTRACT AList T, _) => "CONTRACT" +String #doUnparse(AList, false) +String " " +String #doUnparse(T, true)
  rule #doUnparse(TRANSFER_TOKENS AList, _) => "TRANSFER_TOKENS" +String #doUnparse(AList, false)
  rule #doUnparse(SET_DELEGATE AList, _) => "SET_DELEGATE" +String #doUnparse(AList, false)
  rule #doUnparse(CREATE_ACCOUNT AList, _) => "CREATE_ACCOUNT" +String #doUnparse(AList, false)
  rule #doUnparse(IMPLICIT_ACCOUNT AList, _) => "IMPLICIT_ACCOUNT" +String #doUnparse(AList, false)
  rule #doUnparse(NOW AList, _) => "NOW" +String #doUnparse(AList, false)
  rule #doUnparse(CHAIN_ID AList, _) => "CHAIN_ID" +String #doUnparse(AList, false)
  rule #doUnparse(AMOUNT AList, _) => "AMOUNT" +String #doUnparse(AList, false)
  rule #doUnparse(BALANCE AList, _) => "BALANCE" +String #doUnparse(AList, false)
  rule #doUnparse(CHECK_SIGNATURE AList, _) => "CHECK_SIGNATURE" +String #doUnparse(AList, false)
  rule #doUnparse(BLAKE2B AList, _) => "BLAKE2B" +String #doUnparse(AList, false)
  rule #doUnparse(SHA256 AList, _) => "SHA256" +String #doUnparse(AList, false)
  rule #doUnparse(SHA512 AList, _) => "SHA512" +String #doUnparse(AList, false)
  rule #doUnparse(HASH_KEY AList, _) => "HASH_KEY" +String #doUnparse(AList, false)
  rule #doUnparse(STEPS_TO_QUOTA AList, _) => "STEPS_TO_QUOTA" +String #doUnparse(AList, false)
  rule #doUnparse(SOURCE AList, _) => "SOURCE" +String #doUnparse(AList, false)
  rule #doUnparse(SENDER AList, _) => "SENDER" +String #doUnparse(AList, false)
  rule #doUnparse(ADDRESS AList, _) => "ADDRESS" +String #doUnparse(AList, false)
  rule #doUnparse(CMPEQ AList, _) => "CMPEQ" +String #doUnparse(AList, false)
  rule #doUnparse(CMPNEQ AList, _) => "CMPNEQ" +String #doUnparse(AList, false)
  rule #doUnparse(CMPLT AList, _) => "CMPLT" +String #doUnparse(AList, false)
  rule #doUnparse(CMPGT AList, _) => "CMPGT" +String #doUnparse(AList, false)
  rule #doUnparse(CMPLE AList, _) => "CMPLE" +String #doUnparse(AList, false)
  rule #doUnparse(CMPGE AList, _) => "CMPGE" +String #doUnparse(AList, false)
  rule #doUnparse(IFEQ AList B1 B2, _) => "IFEQ" +String #doUnparse(AList, false) +String " " +String #doUnparse(B1, true) +String " " +String #doUnparse(B2, true)
  rule #doUnparse(IFNEQ AList B1 B2, _) => "IFNEQ" +String #doUnparse(AList, false) +String " " +String #doUnparse(B1, true) +String " " +String #doUnparse(B2, true)
  rule #doUnparse(IFLT AList B1 B2, _) => "IFLT" +String #doUnparse(AList, false) +String " " +String #doUnparse(B1, true) +String " " +String #doUnparse(B2, true)
  rule #doUnparse(IFGT AList B1 B2, _) => "IFGT" +String #doUnparse(AList, false) +String " " +String #doUnparse(B1, true) +String " " +String #doUnparse(B2, true)
  rule #doUnparse(IFLE AList B1 B2, _) => "IFLE" +String #doUnparse(AList, false) +String " " +String #doUnparse(B1, true) +String " " +String #doUnparse(B2, true)
  rule #doUnparse(IFGE AList B1 B2, _) => "IFGE" +String #doUnparse(AList, false) +String " " +String #doUnparse(B1, true) +String " " +String #doUnparse(B2, true)
  rule #doUnparse(IFCMPEQ AList B1 B2, _) => "IFCMPEQ" +String #doUnparse(AList, false) +String " " +String #doUnparse(B1, true) +String " " +String #doUnparse(B2, true)
  rule #doUnparse(IFCMPNEQ AList B1 B2, _) => "IFCMPNEQ" +String #doUnparse(AList, false) +String " " +String #doUnparse(B1, true) +String " " +String #doUnparse(B2, true)
  rule #doUnparse(IFCMPLT AList B1 B2, _) => "IFCMPLT" +String #doUnparse(AList, false) +String " " +String #doUnparse(B1, true) +String " " +String #doUnparse(B2, true)
  rule #doUnparse(IFCMPGT AList B1 B2, _) => "IFCMPGT" +String #doUnparse(AList, false) +String " " +String #doUnparse(B1, true) +String " " +String #doUnparse(B2, true)
  rule #doUnparse(IFCMPLE AList B1 B2, _) => "IFCMPLE" +String #doUnparse(AList, false) +String " " +String #doUnparse(B1, true) +String " " +String #doUnparse(B2, true)
  rule #doUnparse(IFCMPGE AList B1 B2, _) => "IFCMPGE" +String #doUnparse(AList, false) +String " " +String #doUnparse(B1, true) +String " " +String #doUnparse(B2, true)
  rule #doUnparse(FAIL AList, _) => "FAIL" +String #doUnparse(AList, false)
  rule #doUnparse(ASSERT AList, _) => "ASSERT" +String #doUnparse(AList, false)
  rule #doUnparse(ASSERT_EQ AList, _) => "ASSERT_EQ" +String #doUnparse(AList, false)
  rule #doUnparse(ASSERT_NEQ AList, _) => "ASSERT_NEQ" +String #doUnparse(AList, false)
  rule #doUnparse(ASSERT_LT AList, _) => "ASSERT_LT" +String #doUnparse(AList, false)
  rule #doUnparse(ASSERT_LE AList, _) => "ASSERT_LE" +String #doUnparse(AList, false)
  rule #doUnparse(ASSERT_GT AList, _) => "ASSERT_GT" +String #doUnparse(AList, false)
  rule #doUnparse(ASSERT_GE AList, _) => "ASSERT_GE" +String #doUnparse(AList, false)
  rule #doUnparse(ASSERT_CMPEQ AList, _) => "ASSERT_CMPEQ" +String #doUnparse(AList, false)
  rule #doUnparse(ASSERT_CMPNEQ AList, _) => "ASSERT_CMPNEQ" +String #doUnparse(AList, false)
  rule #doUnparse(ASSERT_CMPLT AList, _) => "ASSERT_CMPLT" +String #doUnparse(AList, false)
  rule #doUnparse(ASSERT_CMPLE AList, _) => "ASSERT_CMPLE" +String #doUnparse(AList, false)
  rule #doUnparse(ASSERT_CMPGT AList, _) => "ASSERT_CMPGT" +String #doUnparse(AList, false)
  rule #doUnparse(ASSERT_CMPGE AList, _) => "ASSERT_CMPGE" +String #doUnparse(AList, false)
  rule #doUnparse(ASSERT_NONE AList, _) => "ASSERT_NONE" +String #doUnparse(AList, false)
  rule #doUnparse(ASSERT_SOME AList, _) => "ASSERT_SOME" +String #doUnparse(AList, false)
  rule #doUnparse(ASSERT_LEFT AList, _) => "ASSERT_LEFT" +String #doUnparse(AList, false)
  rule #doUnparse(ASSERT_RIGHT AList, _) => "ASSERT_RIGHT" +String #doUnparse(AList, false)
  rule #doUnparse(IF_SOME AList B1 B2, _) => "IF_SOME" +String #doUnparse(AList, false) +String " " +String #doUnparse(B1, true) +String " " +String #doUnparse(B2, true)
  rule #doUnparse(SET_CAR AList, _) => "SET_CAR" +String #doUnparse(AList, false)
  rule #doUnparse(SET_CDR AList, _) => "SET_CDR" +String #doUnparse(AList, false)
  rule #doUnparse(CREATE_CONTRACT AList { C }, _) => "CREATE_CONTRACT" +String #doUnparse(AList, false) +String " {" +String #doUnparse(C, true) +String "}"

  syntax String ::= #DIPMacroToString(DIPMacro) [function, hook(STRING.token2string)]
  syntax String ::= #DUPMacroToString(DUPMacro) [function, hook(STRING.token2string)]
  syntax String ::= #PairMacroToString(PairMacro) [function, hook(STRING.token2string)]
  syntax String ::= #UnpairMacroToString(UnpairMacro) [function, hook(STRING.token2string)]
  syntax String ::= #CDARMacroToString(CDARMacro) [function, hook(STRING.token2string)]
  syntax String ::= #SetCDARMacroToString(SetCDARMacro) [function, hook(STRING.token2string)]

  rule #doUnparse(D:DIPMacro A B, _) => #DIPMacroToString(D) +String
                                        " " +String
                                        #doUnparse(A, false) +String
                                        " " +String
                                        #doUnparse(B, false)

  rule #doUnparse(D:DUPMacro A, _) => #DUPMacroToString(D) +String
                                      " " +String
                                      #doUnparse(A, false)

  rule #doUnparse(P:PairMacro A, _) => #PairMacroToString(P) +String
                                       " " +String
                                       #doUnparse(A, false)

  rule #doUnparse(U:UnpairMacro A, _) => #UnpairMacroToString(U) +String
                                         " " +String
                                         #doUnparse(A, false)

  rule #doUnparse(C:CDARMacro A, _) => #CDARMacroToString(C) +String
                                       " " +String
                                       #doUnparse(A, false)

  rule #doUnparse(C:SetCDARMacro A, _) => #SetCDARMacroToString(C) +String
                                          " " +String
                                          #doUnparse(A, false)

  rule #doUnparse(storage T, _) => "storage " +String #doUnparse(T, true)
  rule #doUnparse(parameter T, _) => "parameter " +String #doUnparse(T, true)

  rule #doUnparse((C:CodeDecl ; S:StorageDecl ; P:ParameterDecl):Contract, _) =>
    #doUnparse(C, false) +String
    "; " +String
    #doUnparse(S, false) +String
    "; " +String
    #doUnparse(P, false)

  rule #doUnparse((C:CodeDecl ; S:StorageDecl ; P:ParameterDecl ;):Contract, _) =>
    #doUnparse(C, false) +String
    "; " +String
    #doUnparse(S, false) +String
    "; " +String
    #doUnparse(P, false) +String
    ";"

  rule #doUnparse(Elt S:String T:Type, _) =>
    "Elt " +String
    #doUnparse(S, false) +String
    " " +String
    #doUnparse(T, false)

  rule #doUnparse(O:OtherContractsMapEntry ; O':OtherContractsMapEntry ; Os:OtherContractsMapEntryList, _) =>
    #doUnparse(O, false) +String
    "; " +String
    #doUnparse(O' ; Os, false)

  rule #doUnparse(O:OtherContractsMapEntry ; .OtherContractsMapEntryList, _) => #doUnparse(O, false)

  rule #doUnparse(.OtherContractsMapEntryList, _) => ""

  rule #doUnparse(Big_map I T1 T2 { }, _) =>
    "Big_map" +String
    #doUnparse(I, false) +String
    " " +String
    #doUnparse(T1, true) +String
    " " +String
    #doUnparse(T2, true) +String
    " { }"

  rule #doUnparse(Big_map I T1 T2 ML:MapLiteral, _) =>
    "Big_map" +String
    #doUnparse(I, false) +String
    " " +String
    #doUnparse(T1, true) +String
    " " +String
    #doUnparse(T2, true) +String
    " " +String
    #doUnparse(ML, false)

  rule #doUnparse(O:BigMapEntry ; O':BigMapEntry ; Os:BigMapEntryList, _) =>
    #doUnparse(O, false) +String
    "; " +String
    #doUnparse(O' ; Os, false)

  rule #doUnparse(O:BigMapEntry ; .BigMapEntryList, _) => #doUnparse(O, false)

  rule #doUnparse(.BigMapEntryList, _) => ""

  rule #doUnparse(contract { C }, _) => "contract { " +String #doUnparse(C, false) +String "}"
  rule #doUnparse(now I, _) => "now " +String #doUnparse(I, false)
  rule #doUnparse(sender S, _) => "sender " +String #doUnparse(S, false)
  rule #doUnparse(source S, _) => "source " +String #doUnparse(S, false)
  rule #doUnparse((chain_id S):Group, _) => "chain_id " +String #doUnparse(S, false)
  rule #doUnparse(self S, _) => "self " +String #doUnparse(S, false)
  rule #doUnparse(amount S, _) => "amount " +String #doUnparse(S, false)
  rule #doUnparse(balance S, _) => "balance " +String #doUnparse(S, false)
  rule #doUnparse(other_contracts { S }, _) => "other_contracts {" +String #doUnparse(S, false) +String "}"
  rule #doUnparse(parameter_value S, _) => "parameter_value " +String #doUnparse(S, false)
  rule #doUnparse(storage_value S, _) => "storage_value " +String #doUnparse(S, false)
  rule #doUnparse(big_maps { S }, _) => "big_maps {" +String #doUnparse(S, false) +String "}"

  rule #doUnparse(G:Group ; Gs:Groups, _) => #doUnparse(G, false) +String ";\n" +String #doUnparse(Gs, false)

  rule #doUnparse(Stack_elt T D, _) =>
    "Stack_elt " +String
    #doUnparse(T, true) +String
    " " +String
    #doUnparse(D, true)

  rule #doUnparse(S:StackElementLiteral ; Ss:StackElementList, _) =>
    #doUnparse(S, false) +String
    "; " +String
    #doUnparse(Ss, false)
    requires notBool Ss ==K .StackElementList

  rule #doUnparse(S:StackElementLiteral ; .StackElementList, _) => #doUnparse(S, false)

  rule #doUnparse(.StackElementList, _) => ""

  rule #doUnparse({ S:StackElementList }, _) =>
    "{" +String
    #doUnparse(S, false) +String
    "}"

  rule #doUnparse(Create_contract(I, C, O, M, D), _) =>
    "Create_contract (" +String
    #doUnparse(I, true) +String
    ", " +String
    #doUnparse(C, true) +String
    ", " +String
    #doUnparse(O, true) +String
    ", " +String
    #doUnparse(M, true) +String
    ", " +String
    #doUnparse(D, true) +String
    ")"

  rule #doUnparse(Transfer_tokens(I, O, M, A), _) =>
    "Transfer_tokens (" +String
    #doUnparse(I, true) +String
    ", " +String
    #doUnparse(O, true) +String
    ", " +String
    #doUnparse(M, true) +String
    ", " +String
    #doUnparse(A, true) +String
    ")"

  rule #doUnparse(Set_delegate(I, O), _) =>
    "Set_delegate (" +String
    #doUnparse(I, true) +String
    ", " +String
    #doUnparse(O, true) +String
    ")"

  rule #doUnparse(( Failed D ), _) => "( Failed " +String #doUnparse(D, true) +String ")"

  rule #doUnparse(( MutezOverflow I1 I2 ), _) =>
    "( MutezOverflow " +String
    #doUnparse(I1, true) +String
    " " +String
    #doUnparse(I2, true) +String
    ")"

  rule #doUnparse(( MutezUnderflow I1 I2 ), _) =>
    "( MutezUnderflow " +String
    #doUnparse(I1, true) +String
    " " +String
    #doUnparse(I2, true) +String
    ")"

  rule #doUnparse(( GeneralOverflow I1 I2 ), _) =>
    "( GeneralOverflow " +String
    #doUnparse(I1, true) +String
    " " +String
    #doUnparse(I2, true) +String
    ")"

  rule #doUnparse(code I, _) => "code " +String #doUnparse(I, true)
  rule #doUnparse(input C, _) => "input " +String #doUnparse(C, true)
  rule #doUnparse(output LS, _) => "output " +String #doUnparse(LS, true)

  rule #doUnparse(#Any, _) => "#Any"
endmodule
```

Common
------

Here are common files shared between different compatibility semantics.

```k

module COMPAT-COMMON
  imports UNIT-TEST-COMMON-SYNTAX
  imports MICHELSON-UNPARSER

  configuration <k> $PGM:Pgm </k>
                <knownaddrs> .Map </knownaddrs>
                <out stream="stdout"> .List </out>
                <returncode exit=""> 1 </returncode>
```

When the `<k>` cell is empty, we consider execution successful

```k
  rule <k> .K </k>
       <returncode> 1 => 0 </returncode>
endmodule
```

Contract Expander
-----------------

```k
module CONTRACT-EXPANDER-SYNTAX
  imports UNIT-TEST-SYNTAX
endmodule

module CONTRACT-EXPANDER
  imports COMPAT-COMMON

  syntax Block ::= #StackEltToPush(StackElementLiteral) [function]
  rule #StackEltToPush(Stack_elt (contract _:AnnotationList T:Type) D:Data) => { PUSH .AnnotationList address .AnnotationList D ;
                                                                                 CONTRACT .AnnotationList T ;
                                                                                 IF_SOME .AnnotationList { } { UNIT .AnnotationList ; FAILWITH .AnnotationList } }
  rule #StackEltToPush(Stack_elt T D) => { PUSH .AnnotationList T D } [owise]

  syntax Block ::= #StackToPush(LiteralStack) [function]

  syntax Block ::= #StackToPushAux(StackElementList, Block) [function]
  rule #StackToPush( { Se } ) => #StackToPushAux(Se, { DROP .AnnotationList 0 })
  rule #StackToPushAux(Se ; Ls, { I:DataList })  => #StackToPushAux(Ls, { #StackEltToPush(Se) ; I }) requires notBool Ls ==K .StackElementList
  rule #StackToPushAux(Se, { I:DataList })  => { #StackEltToPush(Se) ; I }
  rule #StackToPushAux(.StackElementList, { I:DataList }) => { I }

  syntax Contract ::= #FillTemplateContract(Block, Block, Type) [function]

  syntax Bool ::= #CodeBlockEndsWithFail(Block) [function]
  rule #CodeBlockEndsWithFail( { } ) => false
  rule #CodeBlockEndsWithFail( { { Is } } ) => #CodeBlockEndsWithFail( { Is } )
  rule #CodeBlockEndsWithFail( { I:Instruction } ) => ((FAILWITH _) :=K I) [owise]
  rule #CodeBlockEndsWithFail( { _:Instruction ; Is } ) => #CodeBlockEndsWithFail( { Is } )

  rule #FillTemplateContract(PushBlock, CodeBlock, ParamType) =>
  parameter ParamType ; storage unit .AnnotationList ; code { DROP .AnnotationList ; PushBlock ; CodeBlock ; UNIT #token("@exitToken", "VariableAnnotation") ; FAILWITH .AnnotationList }  ;
  requires notBool #CodeBlockEndsWithFail(CodeBlock)

  rule #FillTemplateContract(PushBlock, CodeBlock, ParamType) =>
  parameter ParamType ; storage unit .AnnotationList ; code { DROP .AnnotationList ; PushBlock ; CodeBlock }  ;
  requires #CodeBlockEndsWithFail(CodeBlock)

  syntax LiteralStack ::= #FindInputGroup(Groups) [function]
  rule #FindInputGroup(input T) => T
  rule #FindInputGroup(input T ; _) => T
  rule #FindInputGroup(_ ; G) => #FindInputGroup(G) [owise]

  syntax Block ::= #FindCodeGroup(Groups) [function]
  rule #FindCodeGroup(code T) => { T }
  rule #FindCodeGroup(code T ; _) => { T }
  rule #FindCodeGroup(_ ; G) => #FindCodeGroup(G) [owise]

  syntax Type ::= #FindParamType(Groups) [function]
  rule #FindParamType(parameter T) => T
  rule #FindParamType(parameter T ; Rs) => T
  rule #FindParamType(_:Group) => unit .AnnotationList [owise]
  rule #FindParamType(_ ; Rs) => #FindParamType(Rs) [owise]

  rule <k> G:Groups => . </k>
       <out> ... .List => ListItem(#unparse(#FillTemplateContract(#StackToPush(#FindInputGroup(G)), #FindCodeGroup(G), #FindParamType(G)))) </out>
endmodule
```

Extractor
---------

```k
module EXTRACTOR-SYNTAX
  imports UNIT-TEST-SYNTAX
endmodule

module EXTRACTOR
  imports COMPAT-COMMON
  imports JSON

  syntax JSONKey ::= #KeyFromGroup(Group) [function]

  rule #KeyFromGroup(G:ContractGroup) => "contract"
  rule #KeyFromGroup(G:NowGroup) => "now"
  rule #KeyFromGroup(G:SenderGroup) => "sender"
  rule #KeyFromGroup(G:SourceGroup) => "source"
  rule #KeyFromGroup(G:ChainGroup) => "chain_id"
  rule #KeyFromGroup(G:SelfGroup) => "self"
  rule #KeyFromGroup(G:AmountGroup) => "amount"
  rule #KeyFromGroup(G:BalanceGroup) => "balance"
  rule #KeyFromGroup(G:BigMapGroup) => "big_maps"
  rule #KeyFromGroup(G:ContractsGroup) => "other_contracts"
  rule #KeyFromGroup(G:CodeGroup) => "code"
  rule #KeyFromGroup(G:InputGroup) => "input"
  rule #KeyFromGroup(G:OutputGroup) => "output"
  rule #KeyFromGroup(G:ParameterDecl) => "parameter"
  rule #KeyFromGroup(G:ParameterValueGroup) => "parameter_value"
  rule #KeyFromGroup(G:StorageValueGroup) => "storage_value"

  syntax K ::= #GroupContent(Group) [function]
  rule #GroupContent(contract { C }) => C
  rule #GroupContent(now C) => C
  rule #GroupContent(sender C) => C
  rule #GroupContent(source C) => C
  rule #GroupContent(chain_id C) => C
  rule #GroupContent(self C) => C
  rule #GroupContent(amount C) => C
  rule #GroupContent(balance C) => C
  rule #GroupContent(other_contracts { C }) => C
  rule #GroupContent(code C) => C
  rule #GroupContent(input C) => C
  rule #GroupContent(output C) => C
  rule #GroupContent(parameter C) => C
  rule #GroupContent(big_maps { C }) => C
  rule #GroupContent(parameter_value C) => C
  rule #GroupContent(storage_value C) => C

  syntax JSON  ::= #GroupsToJson(Groups)      [function]
  syntax JSONs ::= #GroupsToJsonInner(Groups) [function]
  syntax JSON  ::= #GroupToJson(Group)        [function]
  syntax JSON  ::= #GroupToJson(Group,Bool)   [function]

  rule #GroupsToJson(Gs) => { #GroupsToJsonInner(Gs) }

  rule #GroupsToJsonInner(G ; Gs) => #GroupToJson(G) , #GroupsToJsonInner(Gs)
  rule #GroupsToJsonInner(G)      => #GroupToJson(G)

  // check if we need to add braces around our output group
  rule #GroupToJson(G) => #GroupToJson(G, #KeyFromGroup(G) in SetItem("other_contracts") SetItem("big_maps"))

  rule #GroupToJson(G, false) => #KeyFromGroup(G) :             #unparse(#GroupContent(G))
  rule #GroupToJson(G, true)  => #KeyFromGroup(G) : "{" +String #unparse(#GroupContent(G)) +String "}"

  rule <k> Gs:Groups => . </k>
       <out> ... .List => ListItem(JSON2String(#GroupsToJson(Gs))) </out>
       <returncode> 1 => 0 </returncode>
endmodule
```

Input Creator
-------------

```k
module INPUT-CREATOR-SYNTAX
  imports UNIT-TEST-SYNTAX
endmodule

module INPUT-CREATOR
  imports COMPAT-COMMON

  syntax Data ::= #DataForType(Type) [function]
  rule #DataForType(int _) => 0
  rule #DataForType(nat _) => 0
  rule #DataForType(string _) => ""
  rule #DataForType(bytes _) => 0x
  rule #DataForType(mutez _) => 0
  rule #DataForType(bool _) => true
  rule #DataForType(key_hash _) => "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"
  rule #DataForType(timestamp _) => 0
  rule #DataForType(address _) => "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"

//  rule #DataForType(key _)
  rule #DataForType(unit _) => Unit
//  rule #DataForType(signature _)
//  rule #DataForType(operation _)
//  rule #DataForType(chain_id _)

  rule #DataForType(pair _ T1:Type T2:Type) => Pair #DataForType(T1) #DataForType(T2)

  rule #DataForType(option _ _) => None
  rule #DataForType(list _ _) => { }
  rule #DataForType(set _ _) => { }
//  rule #DataForType(contract _ _) => None
  rule #DataForType(or _ T1 _) => #DataForType(T1)
  rule #DataForType(lambda _ _ T2) => { DROP .AnnotationList ; PUSH .AnnotationList T2 #DataForType(T2) }
  rule #DataForType(map _ _ _) => { }
  rule #DataForType(big_map _ _ _) => { }

  syntax Type ::= #FindParamType(Pgm) [function]
  rule #FindParamType(parameter T) => T
  rule #FindParamType(parameter T ; Rs) => T
  rule #FindParamType(_:Group) => unit .AnnotationList [owise]
  rule #FindParamType(_ ; Rs) => #FindParamType(Rs) [owise]

  rule <k> G:Pgm => . </k>
       <out> ... .List => ListItem(#unparse(#DataForType(#FindParamType(G)))) </out>
endmodule
```

Output Compare
--------------

```k
module OUTPUT-COMPARE-COMMON-SYNTAX
  imports UNIT-TEST-COMMON-SYNTAX
  syntax RealOutputGroup ::= "real_output" OutputStack
  syntax Group ::= RealOutputGroup
endmodule

module OUTPUT-COMPARE-SYNTAX
  imports UNIT-TEST-SYNTAX
  imports OUTPUT-COMPARE-COMMON-SYNTAX
endmodule

module OUTPUT-COMPARE
  imports OUTPUT-COMPARE-COMMON-SYNTAX
  imports COMPAT-COMMON
  imports K-REFLECTION
  imports MATCHER

  syntax String ::= #decodeBinaryRepresentation(Bytes) [function, hook(MICHELSON.decode)]
  syntax BlockchainOperation ::= #parseOperation(String) [function]
  rule #parseOperation(S) => #parseKORE(S)

  syntax KItem ::= #CheckOutput(OutputStack, OutputStack) // Expected, Actual

  rule #MichelineToNative(B:Bytes, operation _, KnownAddrs, BigMaps)
    => #MichelineToNative(#parseOperation(#decodeBinaryRepresentation(B)),
                          operation .AnnotationList,
                          KnownAddrs,
                          BigMaps)

  syntax KItem ::= "#Failed"

  rule <k> #CheckOutput( X:FailedStack , X:FailedStack ) => . ... </k>

  rule <k> #CheckOutput( { .StackElementList } , { .StackElementList } ) => . ... </k>

  rule <k> #CheckOutput( { Stack_elt ET ED } , { Stack_elt AT AD } )
        => .
           ...
       </k>
       <knownaddrs> KnownAddrs </knownaddrs>
    requires #Matches(#MichelineToNative(ED, ET, KnownAddrs, .Map),
                      #MichelineToNative(AD, AT, KnownAddrs, .Map))

  rule <k> #CheckOutput( { Stack_elt ET ED } , { Stack_elt AT AD } )
        => #Failed
           ...
       </k>
       <knownaddrs> KnownAddrs </knownaddrs>
       <out> ...
             .List
          => ListItem("Mismatch - Expected: " +String #unparse(#MichelineToNative(ED, ET, KnownAddrs, .Map)) +String
                                  " Actual: " +String #unparse(#MichelineToNative(AD, AT, KnownAddrs, .Map))
                     )
       </out> [owise]

  rule <k> #CheckOutput( { Stack_elt ET ED ; Es } , { Stack_elt AT AD ; As } )
        => #CheckOutput( { Es } , { As } )
           ...
       </k>
       <knownaddrs> KnownAddrs </knownaddrs>
    requires #Matches(#MichelineToNative(ED, ET, KnownAddrs, .Map), #MichelineToNative(AD, AT, KnownAddrs, .Map))

  rule <k> #CheckOutput( { Stack_elt ET ED ; Es } , { Stack_elt AT AD ; As } )
        => #Failed
           ...
       </k>
       <knownaddrs> KnownAddrs </knownaddrs>
       <out> ...
             .List
          => ListItem("Mismatch - Expected: " +String #unparse(#MichelineToNative(ED, ET, KnownAddrs, .Map))
                          +String " Actual: " +String #unparse(#MichelineToNative(AD, AT, KnownAddrs, .Map))
                     )
       </out> [owise]

  rule <k> other_contracts { M } ; Gs => Gs </k>
       <knownaddrs> _ => #OtherContractsMapEntryListToKMap(M) </knownaddrs>

  rule <k> real_output AOS ; output EOS => #CheckOutput(EOS, AOS) ... </k>
endmodule
```
