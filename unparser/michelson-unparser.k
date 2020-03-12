requires "../michelson-syntax.k"
requires "../michelson-common.k"

module MICHELSON-UNPARSER-SYNTAX
  imports MICHELSON-SYNTAX
endmodule

module MICHELSON-UNPARSER
  imports MICHELSON-UNPARSER-SYNTAX
  imports MICHELSON-COMMON
  imports DOMAINS

  syntax String ::= #unparse(K) [function]
  syntax String ::= #doUnparse(K, Bool) [function]

  rule #unparse(O) => #doUnparse(O, false)


  // Unparse Data.
  rule #doUnparse(Elt D1:Data D2:Data, false) => 
    "Elt " +String 
    #doUnparse(D1, true) 
    +String " " +String 
    #doUnparse(D2, true) 

  rule #doUnparse(D1:Data ; DL:Data, _) => 
    #doUnparse(D1, false) +String 
    " ; " +String 
    #doUnparse(DL, false)

  rule #doUnparse(M1:MapEntry ; ML:MapEntryList, _) => 
    #doUnparse(M1, false) +String 
    " ; " +String 
    #doUnparse(ML, false)

  rule #doUnparse(I1:Instruction ; IL:InstructionList, _) => 
    #doUnparse(I1, false) +String 
    " ; " +String 
    #doUnparse(IL, false)
  
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
  rule #doUnparse({ IL:InstructionList }, _) => "{" +String #doUnparse(IL, false) +String "}"
  rule #doUnparse({ }, _) => "{ }"

  rule #doUnparse(I:Int, _) => Int2String(I)
  rule #doUnparse(S:String, _) => "\"" +String S +String "\""

  syntax String ::= #unparseMBytesLiteral(MBytesLiteral) [function, functional, hook(STRING.token2string)]

  rule #doUnparse(M, _) => #unparseMBytesLiteral(M)

  rule #doUnparse(True, _) => "True"
  rule #doUnparse(False, _) => "False"

  rule #doUnparse(Unit, _) => "Unit"

  rule #doUnparse(#Timestamp(I), _) => #doUnparse(I, false)
  rule #doUnparse(#ChainId(I), _) => "0x" +String Base2String(I, 16)
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

  rule #doUnparse(T:UnannotatedSimpleType (A AL):AnnotationList, false) => 
    #doUnparse(T, false) +String 
    " " +String
    #doUnparse(A AL, false)

  rule #doUnparse(T:UnannotatedSimpleType (A AL):AnnotationList, true) => 
    "(" +String #doUnparse(T (A AL), false) +String ")" 

  rule #doUnparse(T:UnannotatedSimpleType .AnnotationList, _) => #doUnparse(T, false)

  rule #doUnparse(pair AL T1 T2, false) =>
    "pair " +String
    #doUnparse(AL, false) +String
    " " +String
    #doUnparse(T1, true) +String
    " " +String
    #doUnparse(T2, true)

  rule #doUnparse(pair AL T1 T2, true) => 
    "(" +String #doUnparse(pair AL T1 T2, false) +String ")"

  rule #doUnparse(option AL T, false) => 
    "option " +String
    #doUnparse(AL, false) +String
    " " +String
    #doUnparse(T, true)

  rule #doUnparse(option AL T, true) => "(" +String #doUnparse(option AL T, false) +String ")"



  configuration <k> #unparse($PGM:K) </k>
endmodule
