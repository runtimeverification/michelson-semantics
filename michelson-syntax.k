module MICHELSON-SYNTAX
  imports INT-SYNTAX // TODO maybe use unsigned int so the parser can catch int/nat issues at parse time?
  imports STRING-SYNTAX
  imports BOOL-SYNTAX


  // Sorts
  syntax Data
  syntax Instruction
  syntax ComparableType
  syntax Type
  syntax TypeAnnotation

  syntax MapEntry ::= "Elt" Data Data

  syntax DataList ::= List{Data, ";"}
  syntax MapEntryList ::= List{MapEntry, ";"}
  syntax InstructionList ::= List{Instruction, ";"}

  syntax BytesLiteral ::= r"0x([0-9a-fA-F]{2})+" [token]
  syntax Data ::= Int 
                | String 
                | Bool
                | BytesLiteral
                | "Unit" 
                | "Pair" Data Data
                | "Left" Data
                | "Right" Data
                | "Some" Data
                | "None" 
                | "{" DataList "}"
                | "{" MapEntry "}"
                | Instruction

  syntax VariableAnnotation ::= r"@[a-z0-9_]" [token] 

  syntax Data ::= VariableAnnotation Data

  syntax Block ::= "{" InstructionList "}"

  syntax DIPMacro ::= r"DII+P" [token]
  syntax DUPMacro ::= r"DUU+P" [token]
  syntax PairMacro ::= r"P[AIP]+R" [token] // This regex needs to be far more complex, but not sure how much K actually supports. P(\left=A|P(\left)(\right))(\right=I|P(\left)(\right))R
  syntax UnpairMacro ::= r"UNP[AIP]+R" [token] // Same as above. UNP(\left=A|P(\left)(\right))(\right=I|P(\left)(\right))R
  syntax CDARMacro ::= r"C[AD]+R" [token]
  syntax SetCDARMacro ::= r"SET_C[AD]+R" [token]


  syntax Instruction ::= Block
                       | "DROP" 
                       | "DUP" 
                       | "SWAP"
                       | "PUSH" Type Data
                       | "SOME" 
                       | "NONE" Type
                       | "UNIT" 
                       | "IF_NONE" Block Block
                       | "PAIR" 
                       | "CAR" 
                       | "CDR" 
                       | "LEFT" Type
                       | "RIGHT" Type
                       | "IF_LEFT" Block Block
                       | "IF_RIGHT" Block Block
                       | "NIL" Type
                       | "CONS" 
                       | "IF_CONS" Block Block
                       | "SIZE" 
                       | "EMPTY_SET" ComparableType
                       | "EMPTY_MAP" ComparableType
                       | "MAP" Block
                       | "ITER" Block
                       | "MEM" 
                       | "GET" 
                       | "UPDATE" 
                       | "IF" Block Block
                       | "LOOP" Block
                       | "LOOP_LEFT" Block
                       | "LAMBDA" Type Type Block
                       | "EXEC" 
                       | "DIP" Block
                       | "FAILWITH" Data
                       | "CAST" 
                       | "RENAME" 
                       | "CONCAT" 
 		       | "SLICE" 
       		       | "PACK" 
		       | "UNPACK" 
		       | "ADD" 
		       | "SUB" 
		       | "MUL" 
		       | "EDIV" 
		       | "ABS" 
		       | "NEG" 
		       | "LSL" 
		       | "LSR" 
		       | "OR" 
		       | "AND" 
		       | "XOR" 
		       | "NOT" 
		       | "COMPARE" 
		       | "EQ" 
		       | "NEQ" 
		       | "LT" 
		       | "GT" 
		       | "LE" 
		       | "GE" 
		       | "SELF" 
		       | "CONTRACT" Type
		       | "TRANSFER_TOKENS" 
	               | "SET_DELEGATE" 
		       | "CREATE_ACCOUNT" 
		       | "CREATE_CONTRACT" Block
		       | "IMPLICIT_ACCOUNT" 
		       | "NOW" 
		       | "AMOUNT" 
		       | "BALANCE" 
		       | "CHECK_SIGNATURE" 
		       | "BLAKE2B" 
		       | "SHA256" 
		       | "SHA512" 
		       | "HASH_KEY" 
		       | "STEPS_TO_QUOTA" 
		       | "SOURCE" 
		       | "SENDER" 
		       | "ADDRESS" 
                       // Macros
                       // Comparison:
                       | "CMPEQ" 
                       | "CMPNEQ" 
                       | "CMPLT" 
                       | "CMPGT" 
                       | "CMPLE"
                       | "CMPGE"
                       | "IFEQ" Block Block
                       | "IFNEQ" Block Block
                       | "IFLT" Block Block
                       | "IFGT" Block Block
                       | "IFLE" Block Block
                       | "IFGE" Block Block
                       | "IFCMPEQ" Block Block
                       | "IFCMPNEQ" Block Block
                       | "IFCMPLT" Block Block
                       | "IFCMPGT" Block Block
                       | "IFCMPLE" Block Block
                       | "IFCMPGE" Block Block
                       // Failure:
                       | "FAIL"
                       // Assertion:
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
                       | DIPMacro Block
                       | DUPMacro
                       | PairMacro
                       | UnpairMacro
                       | CDARMacro
                       | "IF_SOME" Block Block
                       | "SET_CAR" 
                       | "SET_CDR"



  syntax Type ::= ComparableType
		| "key" 
		| "unit" 
		| "signature" 
		| "option" Type
		| "list" Type
		| "set" ComparableType
		| "operation" 
		| "contract" Type
		| "pair" Type Type
		| "or" Type Type
		| "lambda" Type Type
		| "map" ComparableType Type
		| "big_map" ComparableType Type
  
  syntax Type ::= "(" Type ")" [bracket] // Technically incorrect due to rule about primitive app right inside a sequence.  Need to split out Wrapped/Unwrapped sort.
  
  syntax ComparableType ::= "int" 
			  | "nat" 
			  | "string" 
			  | "bytes" 
			  | "mutez" 
			  | "bool" 
			  | "key_hash" 
			  | "timestamp" 
			  | "address" 

  

  syntax TypeAnnotation ::= r":[a-z0-9_]+" [token] 
  syntax Type ::= Type TypeAnnotation

  syntax CodeDecl ::= "code" Block ";"
  syntax StorageDecl ::= "storage" Type ";"
  syntax ParameterDecl ::= "parameter" Type ";"

  syntax Contract ::= CodeDecl StorageDecl ParameterDecl
                    | CodeDecl ParameterDecl StorageDecl
                    | StorageDecl CodeDecl ParameterDecl
                    | ParameterDecl CodeDecl StorageDecl
                    | StorageDecl ParameterDecl CodeDecl
                    | ParameterDecl StorageDecl CodeDecl

  syntax Pgm ::= "{" Contract "}" Data Data
endmodule
