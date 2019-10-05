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
                | "Unit" [token]
                | "Pair" Data Data
                | "Left" Data
                | "Right" Data
                | "Some" Data
                | "None" [token]
                | "{" DataList "}"
                | "{" MapEntry "}"
                | Instruction

  syntax VariableAnnotation ::= r"@[a-z0-9_]" [token]

  syntax Data ::= VariableAnnotation Data

  syntax Block ::= "{" InstructionList "}"

  syntax Instruction ::= Block
                       | "DROP" [token]
                       | "DUP" [token]
                       | "PUSH" Type Data
                       | "SOME" [token]
                       | "NONE" Type
                       | "UNIT" [token]
                       | "IF_NONE" Block Block
                       | "PAIR" [token]
                       | "CAR" [token]
                       | "CDR" [token]
                       | "LEFT" Type
                       | "RIGHT" Type
                       | "IF_LEFT" Block Block
                       | "NIL" Type
                       | "CONS" [token]
                       | "IF_CONS" Block Block
                       | "SIZE" [token]
                       | "EMPTY_SET" ComparableType
                       | "EMPTY_MAP" ComparableType
                       | "MAP" Block
                       | "ITER" Block
                       | "MEM" [token]
                       | "GET" [token]
                       | "UPDATE" [token]
                       | "IF" Block Block
                       | "LOOP" Block
                       | "LOOP_LEFT" Block
                       | "LAMBDA" Type Type Block
                       | "EXEC" [token]
                       | "DIP" Block
                       | "FAILWITH" Data
                       | "CAST" [token]
                       | "RENAME" [token]
                       | "CONCAT" [token]
 		       | "SLICE" [token]
       		       | "PACK" [token]
		       | "UNPACK" [token]
		       | "ADD" [token]
		       | "SUB" [token]
		       | "MUL" [token]
		       | "EDIV" [token]
		       | "ABS" [token]
		       | "NEG" [token]
		       | "LSL" [token]
		       | "LSR" [token]
		       | "OR" [token]
		       | "AND" [token]
		       | "XOR" [token]
		       | "NOT" [token]
		       | "COMPARE" [token]
		       | "EQ" [token]
		       | "NEQ" [token]
		       | "LT" [token]
		       | "GT" [token]
		       | "LE" [token]
		       | "GE" [token]
		       | "SELF" [token]
		       | "CONTRACT" Type
		       | "TRANSFER_TOKENS" [token]
	               | "SET_DELEGATE" [token]
		       | "CREATE_ACCOUNT" [token]
		       | "CREATE_CONTRACT" Block
		       | "IMPLICIT_ACCOUNT" [token]
		       | "NOW" [token]
		       | "AMOUNT" [token]
		       | "BALANCE" [token]
		       | "CHECK_SIGNATURE" [token]
		       | "BLAKE2B" [token]
		       | "SHA256" [token]
		       | "SHA512" [token]
		       | "HASH_KEY" [token]
		       | "STEPS_TO_QUOTA" [token]
		       | "SOURCE" [token]
		       | "SENDER" [token]
		       | "ADDRESS" [token]

  syntax Type ::= ComparableType
		| "key" [token]
		| "unit" [token]
		| "signature" [token]
		| "option" Type
		| "list" Type
		| "set" ComparableType
		| "operation" [token]
		| "contract" Type
		| "pair" Type Type
		| "or" Type Type
		| "lambda" Type Type
		| "map" ComparableType Type
		| "big_map" ComparableType Type
  
  syntax ComparableType ::= "int" [token]
			  | "nat" [token]
			  | "string" [token]
			  | "bytes" [token]
			  | "mutez" [token]
			  | "bool" [token]
			  | "key_hash" [token]
			  | "timestamp" [token]
			  | "address" [token]

  

  syntax TypeAnnotation ::= ":[a-z0-9_]+" [token]
  syntax Type ::= Type TypeAnnotation

  syntax CodeDecl ::= "code" Block ";"
  syntax StorageDecl ::= "storage" Type ";"
  syntax ParameterDecl ::= "parameter" Type ";"
  syntax Contract ::= CodeDecl StorageDecl ParameterDecl

  syntax Pgm ::= "{" Contract "}" Data Data

endmodule

module MICHELSON
  imports MICHELSON-SYNTAX
  imports DOMAINS

  rule <k> { code B:Block ; _:StorageDecl _:ParameterDecl } P:Data S:Data => B </k> // Untyped semantics disregard everything but the code block.
       <Stack> .List => ListItem(Pair P S) </Stack>

  configuration <k> $PGM:Pgm </k>
                <Stack> .List </Stack>
endmodule
