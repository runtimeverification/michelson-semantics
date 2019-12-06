module MICHELSON-SYNTAX
  imports INT-SYNTAX // TODO maybe use unsigned int so the parser can catch int/nat issues at parse time?
  imports STRING-SYNTAX
  imports BOOL-SYNTAX


  // Sorts
  syntax Data
  syntax Instruction
  syntax ComparableType
  syntax Type
  syntax Contract
  syntax ComparableType
  syntax SimpleComparableType

  syntax MapEntry ::= "Elt" Data Data

  syntax DataList ::= List{Data, ";"}
  syntax MapEntryList ::= List{MapEntry, ";"}
  syntax InstructionList ::= List{Instruction, ";"}

  syntax NoAnnotation ::= "" [klabel(noannotation)]
  syntax TypeAnnotation ::= r":([_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]
  syntax VariableAnnotation ::= r"@(%|%%|[_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]
  syntax FieldAnnotation ::= r"%(@|[_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]

  syntax TypeAnnotationList ::= List{TypeAnnotation, " "}
  syntax VariableAnnotationList ::= List{VariableAnnotation, " "}
  syntax FieldAnnotationList ::= List{FieldAnnotation, " "}

  syntax InstructionAnnotationList ::= TypeAnnotationList VariableAnnotationList FieldAnnotationList [klabel(annotations)]
                                     | TypeAnnotationList FieldAnnotationList VariableAnnotationList [klabel(annotations)]
                                     | FieldAnnotationList TypeAnnotationList VariableAnnotationList [klabel(annotations)]
                                     | VariableAnnotationList TypeAnnotationList FieldAnnotationList [klabel(annotations)]
                                     | FieldAnnotationList VariableAnnotationList TypeAnnotationList [klabel(annotations)]
                                     | VariableAnnotationList FieldAnnotationList TypeAnnotationList [klabel(annotations)]

  syntax BytesLiteral ::= r"0x([0-9a-fA-F]{2})+" [token]

  syntax MichelsonBool ::= "True" [token]
                         | "False" [token] // These just get macro'd to the proper K types.
 
  syntax Pair ::= "Pair" Data Data 

  syntax LeftData ::= "Left" Data
  syntax RightData ::= "Right" Data
  syntax OrData ::= LeftData | RightData

  syntax OptionData ::= "Some" Data
                      | "None"  

  syntax Data ::= Int 
                | String 
                | MichelsonBool
                | BytesLiteral
                | "Unit" 
                | Pair
                | OrData
                | OptionData
                | "{" MapEntryList "}"
                | "{" DataList "}"
                | Block

  syntax Data ::= "(" Data ")" [bracket]
  syntax Type ::= "(" Type ")" [bracket] // Technically incorrect due to rule about primitive app right inside a sequence.  Need to split out Wrapped/Unwrapped sort. 

  syntax ComparableType ::= SimpleComparableType 
                          | ComparablePair

  syntax UnannotatedSimpleComparableType ::= "int" 
	  		        | "nat" 
			        | "string" 
          			| "bytes" 
			        | "mutez" 
			        | "bool" 
			        | "key_hash" 
			        | "timestamp" 
			        | "address"   

  syntax SimpleComparableType ::= UnannotatedSimpleComparableType AnnotatedTypeList

  syntax UnannotatedSimpleNoncomparableType ::= "key" 
                    		   | "unit" 
                   		   | "signature"
		                   | "operation"

  syntax SimpleNoncomparableType ::= UnannotatedSimpleNoncomparableType AnnotatedTypeList


  syntax AnnotatedTypeList ::= TypeAnnotationList FieldAnnotationList | FieldAnnotationList TypeAnnotationList

  syntax NormalPair ::= "pair" AnnotatedTypeList Type Type [klabel(ampair)]
  syntax ComparablePair ::= "pair" AnnotatedTypeList SimpleComparableType ComparableType [prefer, klabel(ampair)]

  syntax Type ::= ComparableType
                | SimpleNoncomparableType
		| NormalPair
                | "option" AnnotatedTypeList Type
		| "list" AnnotatedTypeList Type
		| "set" AnnotatedTypeList ComparableType
		| "contract" AnnotatedTypeList Type
                | "or" AnnotatedTypeList Type Type
		| "lambda" AnnotatedTypeList Type Type
		| "map" AnnotatedTypeList ComparableType Type
		| "big_map" AnnotatedTypeList ComparableType Type

  syntax Block ::= "{" InstructionList "}"
                 | "{" "}"

  syntax DIPMacro ::= r"DII+P" [token]
  syntax DUPMacro ::= r"DUU+P" [token]
  syntax PairMacro ::= r"P[AIP]+R" [token] // This regex needs to be far more complex, but not sure how much K actually supports. P(\left=A|P(\left)(\right))(\right=I|P(\left)(\right))R
  syntax UnpairMacro ::= r"UNP[AIP]+R" [token] // Same as above. UNP(\left=A|P(\left)(\right))(\right=I|P(\left)(\right))R
  syntax CDARMacro ::= r"C[AD]+R" [token]
  syntax SetCDARMacro ::= r"SET_C[AD]+R" [token]

  syntax ContractBlock ::= "{" Contract "}"

  syntax Instruction ::= "DROP" InstructionAnnotationList
  syntax Instruction ::= "DROP" InstructionAnnotationList Int
  syntax Instruction ::= "DIG" InstructionAnnotationList Int
  syntax Instruction ::= "DUG" InstructionAnnotationList Int
  syntax Instruction ::= "DUP" InstructionAnnotationList 
  syntax Instruction ::= "SWAP" InstructionAnnotationList 
  syntax Instruction ::= "PUSH" InstructionAnnotationList Type Data
  syntax Instruction ::= "SOME" InstructionAnnotationList 
  syntax Instruction ::= "NONE" InstructionAnnotationList Type
  syntax Instruction ::= "UNIT" InstructionAnnotationList 
  syntax Instruction ::= "IF_NONE" InstructionAnnotationList Block Block
  syntax Instruction ::= "PAIR" InstructionAnnotationList
  syntax Instruction ::= "UNPAIR" InstructionAnnotationList
  syntax Instruction ::= "CAR" InstructionAnnotationList 
  syntax Instruction ::= "CDR" InstructionAnnotationList 
  syntax Instruction ::= "LEFT" InstructionAnnotationList Type
  syntax Instruction ::= "RIGHT" InstructionAnnotationList Type
  syntax Instruction ::= "IF_LEFT" InstructionAnnotationList Block Block
  syntax Instruction ::= "IF_RIGHT" InstructionAnnotationList Block Block
  syntax Instruction ::= "NIL" InstructionAnnotationList Type
  syntax Instruction ::= "CONS" InstructionAnnotationList 
  syntax Instruction ::= "IF_CONS" InstructionAnnotationList Block Block
  syntax Instruction ::= "SIZE" InstructionAnnotationList 
  syntax Instruction ::= "EMPTY_SET" InstructionAnnotationList ComparableType
  syntax Instruction ::= "EMPTY_MAP" InstructionAnnotationList ComparableType Type
  syntax Instruction ::= "EMPTY_BIG_MAP" InstructionAnnotationList ComparableType Type
  syntax Instruction ::= "MAP" InstructionAnnotationList Block
  syntax Instruction ::= "ITER" InstructionAnnotationList Block
  syntax Instruction ::= "MEM" InstructionAnnotationList 
  syntax Instruction ::= "GET" InstructionAnnotationList 
  syntax Instruction ::= "UPDATE" InstructionAnnotationList 
  syntax Instruction ::= "IF" InstructionAnnotationList Block Block
  syntax Instruction ::= "LOOP" InstructionAnnotationList Block
  syntax Instruction ::= "LOOP_LEFT" InstructionAnnotationList Block
  syntax Instruction ::= "LAMBDA" InstructionAnnotationList Type Type Block
  syntax Instruction ::= "EXEC" InstructionAnnotationList 
  syntax Instruction ::= "APPLY" InstructionAnnotationList 
  syntax Instruction ::= "DIP" InstructionAnnotationList Block
  syntax Instruction ::= "DIP" InstructionAnnotationList Int Block
  syntax Instruction ::= "FAILWITH" InstructionAnnotationList 
  syntax Instruction ::= "CAST" InstructionAnnotationList 
  syntax Instruction ::= "RENAME" InstructionAnnotationList 
  syntax Instruction ::= "CONCAT" InstructionAnnotationList 
  syntax Instruction ::= "SLICE" InstructionAnnotationList 
  syntax Instruction ::= "PACK" InstructionAnnotationList 
  syntax Instruction ::= "UNPACK" InstructionAnnotationList Type
  syntax Instruction ::= "ADD" InstructionAnnotationList 
  syntax Instruction ::= "SUB" InstructionAnnotationList 
  syntax Instruction ::= "MUL" InstructionAnnotationList 
  syntax Instruction ::= "EDIV" InstructionAnnotationList 
  syntax Instruction ::= "ABS" InstructionAnnotationList 
  syntax Instruction ::= "ISNAT" InstructionAnnotationList 
  syntax Instruction ::= "INT" InstructionAnnotationList 
  syntax Instruction ::= "NEG" InstructionAnnotationList 
  syntax Instruction ::= "LSL" InstructionAnnotationList 
  syntax Instruction ::= "LSR" InstructionAnnotationList 
  syntax Instruction ::= "OR" InstructionAnnotationList 
  syntax Instruction ::= "AND" InstructionAnnotationList 
  syntax Instruction ::= "XOR" InstructionAnnotationList 
  syntax Instruction ::= "NOT" InstructionAnnotationList 
  syntax Instruction ::= "COMPARE" InstructionAnnotationList 
  syntax Instruction ::= "EQ" InstructionAnnotationList 
  syntax Instruction ::= "NEQ" InstructionAnnotationList 
  syntax Instruction ::= "LT" InstructionAnnotationList 
  syntax Instruction ::= "GT" InstructionAnnotationList 
  syntax Instruction ::= "LE" InstructionAnnotationList 
  syntax Instruction ::= "GE" InstructionAnnotationList 
  syntax Instruction ::= "SELF" InstructionAnnotationList 
  syntax Instruction ::= "CONTRACT" InstructionAnnotationList Type
  syntax Instruction ::= "TRANSFER_TOKENS" InstructionAnnotationList 
  syntax Instruction ::= "SET_DELEGATE" InstructionAnnotationList 
  syntax Instruction ::= "CREATE_ACCOUNT" InstructionAnnotationList 
  syntax Instruction ::= "IMPLICIT_ACCOUNT" InstructionAnnotationList 
  syntax Instruction ::= "NOW" InstructionAnnotationList 
  syntax Instruction ::= "CHAIN_ID" InstructionAnnotationList 
  syntax Instruction ::= "AMOUNT" InstructionAnnotationList 
  syntax Instruction ::= "BALANCE" InstructionAnnotationList 
  syntax Instruction ::= "CHECK_SIGNATURE" InstructionAnnotationList 
  syntax Instruction ::= "BLAKE2B" InstructionAnnotationList 
  syntax Instruction ::= "SHA256" InstructionAnnotationList 
  syntax Instruction ::= "SHA512" InstructionAnnotationList 
  syntax Instruction ::= "HASH_KEY" InstructionAnnotationList 
  syntax Instruction ::= "STEPS_TO_QUOTA" InstructionAnnotationList 
  syntax Instruction ::= "SOURCE" InstructionAnnotationList 
  syntax Instruction ::= "SENDER" InstructionAnnotationList 
  syntax Instruction ::= "ADDRESS" InstructionAnnotationList 
  syntax Instruction ::= "CMPEQ" InstructionAnnotationList 
  syntax Instruction ::= "CMPNEQ" InstructionAnnotationList 
  syntax Instruction ::= "CMPLT" InstructionAnnotationList 
  syntax Instruction ::= "CMPGT" InstructionAnnotationList 
  syntax Instruction ::= "CMPLE" InstructionAnnotationList 
  syntax Instruction ::= "CMPGE" InstructionAnnotationList 
  syntax Instruction ::= "IFEQ" InstructionAnnotationList Block Block
  syntax Instruction ::= "IFNEQ" InstructionAnnotationList Block Block
  syntax Instruction ::= "IFLT" InstructionAnnotationList Block Block
  syntax Instruction ::= "IFGT" InstructionAnnotationList Block Block
  syntax Instruction ::= "IFLE" InstructionAnnotationList Block Block
  syntax Instruction ::= "IFGE" InstructionAnnotationList Block Block
  syntax Instruction ::= "IFCMPEQ" InstructionAnnotationList Block Block
  syntax Instruction ::= "IFCMPNEQ" InstructionAnnotationList Block Block
  syntax Instruction ::= "IFCMPLT" InstructionAnnotationList Block Block
  syntax Instruction ::= "IFCMPGT" InstructionAnnotationList Block Block
  syntax Instruction ::= "IFCMPLE" InstructionAnnotationList Block Block
  syntax Instruction ::= "IFCMPGE" InstructionAnnotationList Block Block
  syntax Instruction ::= "FAIL" InstructionAnnotationList 
  syntax Instruction ::= "ASSERT" InstructionAnnotationList 
  syntax Instruction ::= "ASSERT_EQ" InstructionAnnotationList 
  syntax Instruction ::= "ASSERT_NEQ" InstructionAnnotationList 
  syntax Instruction ::= "ASSERT_LT" InstructionAnnotationList 
  syntax Instruction ::= "ASSERT_LE" InstructionAnnotationList 
  syntax Instruction ::= "ASSERT_GT" InstructionAnnotationList 
  syntax Instruction ::= "ASSERT_GE" InstructionAnnotationList 
  syntax Instruction ::= "ASSERT_CMPEQ" InstructionAnnotationList 
  syntax Instruction ::= "ASSERT_CMPNEQ" InstructionAnnotationList 
  syntax Instruction ::= "ASSERT_CMPLT" InstructionAnnotationList 
  syntax Instruction ::= "ASSERT_CMPLE" InstructionAnnotationList 
  syntax Instruction ::= "ASSERT_CMPGT" InstructionAnnotationList 
  syntax Instruction ::= "ASSERT_CMPGE" InstructionAnnotationList 
  syntax Instruction ::= "ASSERT_NONE" InstructionAnnotationList 
  syntax Instruction ::= "ASSERT_SOME" InstructionAnnotationList 
  syntax Instruction ::= "ASSERT_LEFT" InstructionAnnotationList 
  syntax Instruction ::= "ASSERT_RIGHT" InstructionAnnotationList 
  syntax Instruction ::= "IF_SOME" InstructionAnnotationList Block Block
  syntax Instruction ::= "SET_CAR" InstructionAnnotationList 
  syntax Instruction ::= "SET_CDR" InstructionAnnotationList 
  syntax Instruction ::= "CREATE_CONTRACT" InstructionAnnotationList ContractBlock 

 
  syntax CodeDecl ::= "code" Block ";"
  syntax StorageDecl ::= "storage" Type ";"
  syntax ParameterDecl ::= "parameter" Type ";"

  syntax Contract ::= CodeDecl StorageDecl ParameterDecl
                    | CodeDecl ParameterDecl StorageDecl
                    | StorageDecl CodeDecl ParameterDecl
                    | ParameterDecl CodeDecl StorageDecl
                    | StorageDecl ParameterDecl CodeDecl
                    | ParameterDecl StorageDecl CodeDecl

  syntax ContractInput ::= #ContractInput(String, Type)
  syntax ContractInputList ::= List{ContractInput, " "} 

  syntax Pgm ::= "$contract" "{" Contract "}" ";" 
                 "$paramtype" Type ";" 
                 "$storagetype" Type ";" 
                 "$balance" Int ";" 
                 "$amount" Int ";" 
                 "$now" Int ";" 
                 "$myaddr" String ";" 
                 "$knownaddrs" ContractInputList ";" 
                 "$sourceaddr" String ";" 
                 "$senderaddr" String ";" 
                 "$param" Data  ";" 
                 "$storage" Data ";" 
endmodule
