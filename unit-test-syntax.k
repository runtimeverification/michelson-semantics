requires "michelson.k"
requires "michelson-syntax.k"

module UNIT-TEST-SYNTAX
  imports MICHELSON-SYNTAX
  imports MICHELSON-OPERATIONS

  syntax StackElement ::= "Stack_elt" Type Data
  syntax StackElementList ::= StackElement | StackElement ";" StackElementList
  syntax LiteralStack ::= EmptyBlock | "{" StackElementList "}"
  syntax FailedStack ::= "(" "Failed" Data ")"
  syntax OutputStack ::= LiteralStack | FailedStack

  syntax TestCode ::= Instruction | Block

  syntax InputGroup ::= "input" LiteralStack
  syntax CodeGroup ::= "code" TestCode
  syntax OutputGroup ::= "output" OutputStack

  syntax Group ::= InputGroup | CodeGroup | OutputGroup | ParameterDecl
endmodule 
