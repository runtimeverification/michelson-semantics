requires "michelson.k"
requires "michelson-syntax.k"

module TEST-MICHELSON-SYNTAX
  imports MICHELSON-SYNTAX-BASE
  imports MICHELSON
  imports DOMAINS

  syntax Data ::= List | Set | Map

  syntax KItem ::= Data

  syntax Optional ::= "$Contracts" ContractInputList
                    | "$ParamType" Type

  syntax OptionalGroup ::= List{Optional,";"}

  syntax Pgm ::= "$Instructions" Block ";"
                 "$Stack" List ";" // Basic unit test block.
               |  "$Instructions" Block ";"
                 "$Stack" List ";" 
                 OptionalGroup ";"
endmodule 
