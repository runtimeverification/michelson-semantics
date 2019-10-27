requires "michelson.k"
requires "michelson-syntax.k"

module TEST-MICHELSON-SYNTAX
  imports MICHELSON-SYNTAX
  imports MICHELSON
  imports DOMAINS

  syntax Data ::= List | Set | Map

  syntax KItem ::= Data

  syntax Pgm ::= "$Instructions" Block ";"
                 "$Stack" List ";" // Basic unit test block.
endmodule 
