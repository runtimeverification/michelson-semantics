```k
requires "unit-test-syntax.k"

module SYMBOLIC-UNIT-TEST-SYNTAX
  imports UNIT-TEST-SYNTAX

  syntax BlockList ::= Block | Block ";" BlockList

  syntax Blocks ::= EmptyBlock | "{" BlockList "}"

  syntax PreconditionGroup ::= "precondition" Blocks
  syntax PostconditionGroup ::= "postcondition" Blocks

  syntax Group ::= PreconditionGroup | PostconditionGroup

  syntax SymbolicData ::= r"$[_a-zA-Z][_0-9a-zA-Z]*" [token]

  syntax Data ::= SymbolicData
endmodule
```
