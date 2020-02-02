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
  syntax Address
  syntax ContractData
  syntax Mutez
  syntax KeyHash
  syntax ChainId
  syntax Timestamp
  syntax Key
  syntax Signature

  syntax MapEntry ::= "Elt" Data Data

  syntax DataList ::= Data | Data ";" DataList
  syntax MapEntryList ::= MapEntry | MapEntry ";" MapEntryList
  syntax InstructionList ::= Instruction | Instruction ";" InstructionList

  syntax TypeAnnotation ::= r":([_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]
  syntax VariableAnnotation ::= r"@(%|%%|[_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]
  syntax FieldAnnotation ::= r"%(@|[_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]
  syntax Annotation ::= TypeAnnotation | VariableAnnotation | FieldAnnotation

  syntax AnnotationList ::= List{Annotation, ""}

  syntax MBytesLiteral ::= r"0x([0-9a-fA-F]{2})*" [token]

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
                | MBytesLiteral
                | "Unit" 
                | Pair
                | OrData
                | OptionData
                | "{" MapEntryList "}"
                | "{" DataList "}"
                | Block

  syntax Data ::= Timestamp 
  syntax Data ::= ChainId
  syntax Data ::= KeyHash
  syntax Data ::= Mutez
  syntax Data ::= Address
  syntax Data ::= ContractData
  syntax Data ::= Key
  syntax Data ::= Signature
  
  syntax Mutez ::= Int
  syntax Address ::= String
 
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

  syntax SimpleComparableType ::= UnannotatedSimpleComparableType AnnotationList

  syntax UnannotatedSimpleNoncomparableType ::= "key" 
                    		   | "unit" 
                   		   | "signature"
		                   | "operation"
                                   | "chain_id"

  syntax SimpleNoncomparableType ::= UnannotatedSimpleNoncomparableType AnnotationList


  syntax AnnotationList 

  syntax NormalPair ::= "pair" AnnotationList Type Type [klabel(ampair)]
  syntax ComparablePair ::= "pair" AnnotationList SimpleComparableType ComparableType [prefer, klabel(ampair)]

  syntax ComparableType ::= "(" ComparableType ")" [bracket]
 
  syntax Type ::= ComparableType
                | SimpleNoncomparableType
		| NormalPair
                | "option" AnnotationList Type
		| "list" AnnotationList Type
		| "set" AnnotationList ComparableType
		| "contract" AnnotationList Type
                | "or" AnnotationList Type Type
		| "lambda" AnnotationList Type Type
		| "map" AnnotationList ComparableType Type
		| "big_map" AnnotationList ComparableType Type

  syntax EmptyBlock ::= "{" "}"

  syntax Block ::= "{" InstructionList "}"
                 | "{" InstructionList ";" "}"
                 | EmptyBlock

//  syntax DIPMacro ::= r"DII+P" [token]
//  syntax DUPMacro ::= r"DUU+P" [token]
//  syntax PairMacro ::= r"P[AIP]+R" [token] // This regex needs to be far more complex, but not sure how much K actually supports. P(\left=A|P(\left)(\right))(\right=I|P(\left)(\right))R
//  syntax UnpairMacro ::= r"UNP[AIP]+R" [token] // Same as above. UNP(\left=A|P(\left)(\right))(\right=I|P(\left)(\right))R
  syntax CDARMacro ::= r"C[A,D]{2,}R" [token]
//  syntax SetCDARMacro ::= r"SET_C[AD]+R" [token]

  syntax ContractBlock ::= "{" Contract "}"

  syntax Instruction ::= Block

  syntax Instruction ::= CDARMacro AnnotationList
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
  syntax Instruction ::= "IF_RIGHT" AnnotationList Block Block
  syntax Instruction ::= "NIL" AnnotationList Type
  syntax Instruction ::= "CONS" AnnotationList 
  syntax Instruction ::= "IF_CONS" AnnotationList Block Block
  syntax Instruction ::= "SIZE" AnnotationList 
  syntax Instruction ::= "EMPTY_SET" AnnotationList ComparableType
  syntax Instruction ::= "EMPTY_MAP" AnnotationList ComparableType Type
  syntax Instruction ::= "EMPTY_BIG_MAP" AnnotationList ComparableType Type
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
  syntax Instruction ::= "CMPEQ" AnnotationList 
  syntax Instruction ::= "CMPNEQ" AnnotationList 
  syntax Instruction ::= "CMPLT" AnnotationList 
  syntax Instruction ::= "CMPGT" AnnotationList 
  syntax Instruction ::= "CMPLE" AnnotationList 
  syntax Instruction ::= "CMPGE" AnnotationList 
  syntax Instruction ::= "IFEQ" AnnotationList Block Block
  syntax Instruction ::= "IFNEQ" AnnotationList Block Block
  syntax Instruction ::= "IFLT" AnnotationList Block Block
  syntax Instruction ::= "IFGT" AnnotationList Block Block
  syntax Instruction ::= "IFLE" AnnotationList Block Block
  syntax Instruction ::= "IFGE" AnnotationList Block Block
  syntax Instruction ::= "IFCMPEQ" AnnotationList Block Block
  syntax Instruction ::= "IFCMPNEQ" AnnotationList Block Block
  syntax Instruction ::= "IFCMPLT" AnnotationList Block Block
  syntax Instruction ::= "IFCMPGT" AnnotationList Block Block
  syntax Instruction ::= "IFCMPLE" AnnotationList Block Block
  syntax Instruction ::= "IFCMPGE" AnnotationList Block Block
  syntax Instruction ::= "FAIL" AnnotationList 
  syntax Instruction ::= "ASSERT" AnnotationList 
  syntax Instruction ::= "ASSERT_EQ" AnnotationList 
  syntax Instruction ::= "ASSERT_NEQ" AnnotationList 
  syntax Instruction ::= "ASSERT_LT" AnnotationList 
  syntax Instruction ::= "ASSERT_LE" AnnotationList 
  syntax Instruction ::= "ASSERT_GT" AnnotationList 
  syntax Instruction ::= "ASSERT_GE" AnnotationList 
  syntax Instruction ::= "ASSERT_CMPEQ" AnnotationList 
  syntax Instruction ::= "ASSERT_CMPNEQ" AnnotationList 
  syntax Instruction ::= "ASSERT_CMPLT" AnnotationList 
  syntax Instruction ::= "ASSERT_CMPLE" AnnotationList 
  syntax Instruction ::= "ASSERT_CMPGT" AnnotationList 
  syntax Instruction ::= "ASSERT_CMPGE" AnnotationList 
  syntax Instruction ::= "ASSERT_NONE" AnnotationList 
  syntax Instruction ::= "ASSERT_SOME" AnnotationList 
  syntax Instruction ::= "ASSERT_LEFT" AnnotationList 
  syntax Instruction ::= "ASSERT_RIGHT" AnnotationList 
  syntax Instruction ::= "IF_SOME" AnnotationList Block Block
  syntax Instruction ::= "SET_CAR" AnnotationList 
  syntax Instruction ::= "SET_CDR" AnnotationList 
  syntax Instruction ::= "CREATE_CONTRACT" AnnotationList ContractBlock 

 
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

  syntax ContractInput ::= #ContractInput(String, Type)
  syntax ContractInputList ::= List{ContractInput, " "} 

  syntax Pgm ::= "$contract" "{" Contract "}" ";" 
                 "$balance" Int ";" 
                 "$amount" Int ";" 
                 "$now" Int ";" 
                 "$myaddr" String ";" 
                 "$knownaddrs" ContractInputList ";" 
                 "$sourceaddr" String ";" 
                 "$senderaddr" String ";" 
                 "$param" Data  ";" 
                 "$storage" Data ";"

  syntax Pgm ::= "$contract" "{" Contract "}" ";" 
                 "$param" Data  ";" 
                 "$storage" Data ";"
endmodule
