require "michelson-annotation-syntax.k"

module MICHELSON-SYNTAX
  imports INT-SYNTAX // TODO maybe use unsigned int so the parser can catch int/nat issues at parse time?
  imports STRING-SYNTAX
  imports BOOL-SYNTAX
  imports MICHELSON-ANNOTATION-SYNTAX


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

  syntax InstructionAnnotationList ::= TypeAnnotationList VariableAnnotationList FieldAnnotationList
                                     | TypeAnnotationList FieldAnnotationList VariableAnnotationList
                                     | FieldAnnotationList TypeAnnotationList VariableAnnotationList
                                     | VariableAnnotationList TypeAnnotationList FieldAnnotationList
                                     | FieldAnnotationList VariableAnnotationList TypeAnnotationList
                                     | VariableAnnotationList FieldAnnotationList TypeAnnotationList

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
                | Instruction

  syntax Block ::= "{" InstructionList "}"
                 | "{" "}"

  syntax DIPMacro ::= r"DII+P" [token]
  syntax DUPMacro ::= r"DUU+P" [token]
  syntax PairMacro ::= r"P[AIP]+R" [token] // This regex needs to be far more complex, but not sure how much K actually supports. P(\left=A|P(\left)(\right))(\right=I|P(\left)(\right))R
  syntax UnpairMacro ::= r"UNP[AIP]+R" [token] // Same as above. UNP(\left=A|P(\left)(\right))(\right=I|P(\left)(\right))R
  syntax CDARMacro ::= r"C[AD]+R" [token]
  syntax SetCDARMacro ::= r"SET_C[AD]+R" [token]

  syntax Instruction ::= "DROP" T0V0F0 
  syntax Instruction ::= "DUP" T0V1F0 
  syntax Instruction ::= "SWAP" T0V0F0 
  syntax Instruction ::= "PUSH" T0V1F0 Type Data
  syntax Instruction ::= "SOME" T1V1F0 
  syntax Instruction ::= "NONE" T1V1F0 Type
  syntax Instruction ::= "UNIT" T1V1F0 
  syntax Instruction ::= "IF_NONE" T0V0F0 Block Block
  syntax Instruction ::= "PAIR" T1V1F2 
  syntax Instruction ::= "CAR" T0V1F1 
  syntax Instruction ::= "CDR" T0V1F1 
  syntax Instruction ::= "LEFT" T1V1F2 Type
  syntax Instruction ::= "RIGHT" T1V1F2 Type
  syntax Instruction ::= "IF_LEFT" T0V0F0 Block Block
  syntax Instruction ::= "IF_RIGHT" T0V0F0 Block Block
  syntax Instruction ::= "NIL" T1V1F0 Type
  syntax Instruction ::= "CONS" T0V1F0 
  syntax Instruction ::= "IF_CONS" T0V0F0 Block Block
  syntax Instruction ::= "SIZE" T0V1F0 
  syntax Instruction ::= "EMPTY_SET" T1V1F0 ComparableType
  syntax Instruction ::= "EMPTY_MAP" T1V1F0 ComparableType Type
  syntax Instruction ::= "EMPTY_BIG_MAP" T1V1F0 ComparableType Type
  syntax Instruction ::= "MAP" T0V1F0 Block
  syntax Instruction ::= "ITER" T0V0F0 Block
  syntax Instruction ::= "MEM" T0V1F0 
  syntax Instruction ::= "GET" T0V1F0 
  syntax Instruction ::= "UPDATE" T0V1F0 
  syntax Instruction ::= "IF" T0V0F0 Block Block
  syntax Instruction ::= "LOOP" T0V0F0 Block
  syntax Instruction ::= "LOOP_LEFT" T0V0F0 Block
  syntax Instruction ::= "LAMBDA" T0V1F0 Type Type Block
  syntax Instruction ::= "EXEC" T0V1F0 
  syntax Instruction ::= "APPLY" T0V0F0 
  syntax Instruction ::= "DIP" T0V0F0 Block
  syntax Instruction ::= "FAILWITH" T0V0F0 
  syntax Instruction ::= "CAST" T1V1F0 
  syntax Instruction ::= "RENAME" T0V1F0 
  syntax Instruction ::= "CONCAT" T0V1F0 
  syntax Instruction ::= "SLICE" T0V0F0 
  syntax Instruction ::= "PACK" T0V0F0 
  syntax Instruction ::= "UNPACK" T0V0F0 
  syntax Instruction ::= "ADD" T0V1F0 
  syntax Instruction ::= "SUB" T0V1F0 
  syntax Instruction ::= "MUL" T0V1F0 
  syntax Instruction ::= "EDIV" T0V1F0 
  syntax Instruction ::= "ABS" T0V1F0 
  syntax Instruction ::= "ISNAT" T0V1F0 
  syntax Instruction ::= "INT" T0V1F0 
  syntax Instruction ::= "NEG" T0V1F0 
  syntax Instruction ::= "LSL" T0V1F0 
  syntax Instruction ::= "LSR" T0V1F0 
  syntax Instruction ::= "OR" T0V1F0 
  syntax Instruction ::= "AND" T0V1F0 
  syntax Instruction ::= "XOR" T0V1F0 
  syntax Instruction ::= "NOT" T0V1F0 
  syntax Instruction ::= "COMPARE" T0V1F0 
  syntax Instruction ::= "EQ" T0V1F0 
  syntax Instruction ::= "NEQ" T0V1F0 
  syntax Instruction ::= "LT" T0V1F0 
  syntax Instruction ::= "GT" T0V1F0 
  syntax Instruction ::= "LE" T0V1F0 
  syntax Instruction ::= "GE" T0V1F0 
  syntax Instruction ::= "SELF" T0V1F0 
  syntax Instruction ::= "CONTRACT" T0V1F0 Type
  syntax Instruction ::= "TRANSFER_TOKENS" T0V0F0 
  syntax Instruction ::= "SET_DELEGATE" T0V1F0 
  syntax Instruction ::= "CREATE_ACCOUNT" T0V2F0 
  syntax Instruction ::= "IMPLICIT_ACCOUNT" T0V1F0 
  syntax Instruction ::= "NOW" T0V1F0 
  syntax Instruction ::= "CHAIN_ID" T0V1F0 
  syntax Instruction ::= "AMOUNT" T0V1F0 
  syntax Instruction ::= "BALANCE" T0V1F0 
  syntax Instruction ::= "CHECK_SIGNATURE" T0V1F0 
  syntax Instruction ::= "BLAKE2B" T0V1F0 
  syntax Instruction ::= "SHA256" T0V0F0 
  syntax Instruction ::= "SHA512" T0V0F0 
  syntax Instruction ::= "HASH_KEY" T0V1F0 
  syntax Instruction ::= "STEPS_TO_QUOTA" T0V1F0 
  syntax Instruction ::= "SOURCE" T0V1F0 
  syntax Instruction ::= "SENDER" T0V1F0 
  syntax Instruction ::= "ADDRESS" T0V1F0 
  syntax Instruction ::= "CMPEQ" T0V0F0 
  syntax Instruction ::= "CMPNEQ" T0V0F0 
  syntax Instruction ::= "CMPLT" T0V0F0 
  syntax Instruction ::= "CMPGT" T0V0F0 
  syntax Instruction ::= "CMPLE" T0V0F0 
  syntax Instruction ::= "CMPGE" T0V0F0 
  syntax Instruction ::= "IFEQ" T0V0F0 Block Block
  syntax Instruction ::= "IFNEQ" T0V0F0 Block Block
  syntax Instruction ::= "IFLT" T0V0F0 Block Block
  syntax Instruction ::= "IFGT" T0V0F0 Block Block
  syntax Instruction ::= "IFLE" T0V0F0 Block Block
  syntax Instruction ::= "IFGE" T0V0F0 Block Block
  syntax Instruction ::= "IFCMPEQ" T0V0F0 Block Block
  syntax Instruction ::= "IFCMPNEQ" T0V0F0 Block Block
  syntax Instruction ::= "IFCMPLT" T0V0F0 Block Block
  syntax Instruction ::= "IFCMPGT" T0V0F0 Block Block
  syntax Instruction ::= "IFCMPLE" T0V0F0 Block Block
  syntax Instruction ::= "IFCMPGE" T0V0F0 Block Block
  syntax Instruction ::= "FAIL" T0V0F0 
  syntax Instruction ::= "ASSERT" T0V0F0 
  syntax Instruction ::= "ASSERT_EQ" T0V0F0 
  syntax Instruction ::= "ASSERT_NEQ" T0V0F0 
  syntax Instruction ::= "ASSERT_LT" T0V0F0 
  syntax Instruction ::= "ASSERT_LE" T0V0F0 
  syntax Instruction ::= "ASSERT_GT" T0V0F0 
  syntax Instruction ::= "ASSERT_GE" T0V0F0 
  syntax Instruction ::= "ASSERT_CMPEQ" T0V0F0 
  syntax Instruction ::= "ASSERT_CMPNEQ" T0V0F0 
  syntax Instruction ::= "ASSERT_CMPLT" T0V0F0 
  syntax Instruction ::= "ASSERT_CMPLE" T0V0F0 
  syntax Instruction ::= "ASSERT_CMPGT" T0V0F0 
  syntax Instruction ::= "ASSERT_CMPGE" T0V0F0 
  syntax Instruction ::= "ASSERT_NONE" T0V0F0 
  syntax Instruction ::= "ASSERT_SOME" T0V0F0 
  syntax Instruction ::= "ASSERT_LEFT" T0V0F0 
  syntax Instruction ::= "ASSERT_RIGHT" T0V0F0 
  syntax Instruction ::= "IF_SOME" T0V0F0 Block Block
  syntax Instruction ::= "SET_CAR" T0V0F0 
  syntax Instruction ::= "SET_CDR" T0V0F0 
  syntax Instruction ::= "CREATE_CONTRACT" T0V2F0 Block 

  syntax Type
  syntax SimpleNoncomparableType
  syntax ComparableType
  syntax SimpleComparableType

  syntax NormalPair ::= "pair" T1 Type Type [klabel(ampair)]
  syntax ComparablePair ::= "pair" T1 SimpleComparableType ComparableType [prefer, klabel(ampair)]

  syntax Type ::= ComparableType
                | SimpleNoncomparableType
		| NormalPair
                | "option" T1 Type
		| "list" T1 Type
		| "set" T1 ComparableType
		| "contract" T1 Type
                | "or" T1 Type Type
		| "lambda" T1 Type Type
		| "map" T1 ComparableType Type
		| "big_map" T1 ComparableType Type
 
  
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

  syntax SimpleComparableType ::= UnannotatedSimpleComparableType T1

  syntax UnannotatedSimpleNoncomparableType ::= "key" 
                    		   | "unit" 
                   		   | "signature"
		                   | "operation"

  syntax SimpleNoncomparableType ::= UnannotatedSimpleNoncomparableType T1


  syntax CodeDecl ::= "code" Block ";"
  syntax StorageDecl ::= "storage" Type ";"
  syntax ParameterDecl ::= "parameter" Type ";"

  syntax Contract ::= CodeDecl StorageDecl ParameterDecl
                    | CodeDecl ParameterDecl StorageDecl
                    | StorageDecl CodeDecl ParameterDecl
                    | ParameterDecl CodeDecl StorageDecl
                    | StorageDecl ParameterDecl CodeDecl
                    | ParameterDecl StorageDecl CodeDecl

  syntax ContractInput ::= #ContractInput(Int, Type)
  syntax ContractInputList ::= List{ContractInput, " "} 

  syntax Pgm ::= "$contract" "{" Contract "}" ";" 
                 "$paramtype" Type ";" 
                 "$storagetype" Type ";" 
                 "$balance" Int ";" 
                 "$amount" Int ";" 
                 "$now" Int ";" 
                 "$myaddr" Int ";" 
                 "$knownaddrs" ContractInputList ";" 
                 "$sourceaddr" Int ";" 
                 "$senderaddr" Int ";" 
                 "$param" Data  ";" 
                 "$storage" Data ";" 
endmodule
