requires "michelson.k"
requires "michelson-syntax.k"

module TEST-MICHELSON-SYNTAX
  imports MICHELSON-SYNTAX
  imports MICHELSON
  imports DOMAINS

  syntax Data ::= List | Set | Map

  syntax KItem ::= Data

  syntax Optional ::= "$Contracts" ContractInputList
                    | "$ParamType" Type

  syntax OptionalGroup ::= List{Optional,";"}

  syntax Pgm ::= "$Instructions" "{" InstructionList "}" ";"
                 "$Stack" List ";" // Basic unit test block.
               |  "$Instructions" "{" InstructionList "}" ";"
                 "$Stack" List ";" 
                 OptionalGroup ";"

  syntax Pgm ::= Pgm "$ExpectedOutput" Data ";"
endmodule 
