```k
requires "unit-test-syntax.k"

module SYMBOLIC-UNIT-TEST-SYNTAX
  imports UNIT-TEST-SYNTAX

  syntax BlockList ::= Block | Block ";" BlockList
  syntax Blocks ::= EmptyBlock | "{" BlockList "}"

  syntax Invariants ::= EmptyBlock | "{" InvariantList "}"
  syntax InvariantList ::= Invariant | Invariant ";" InvariantList
  syntax Invariant ::= VariableAnnotation Blocks

  syntax PreconditionGroup ::= "precondition" Blocks
  syntax PostconditionGroup ::= "postcondition" Blocks
  syntax InvariantsGroup ::= "invariants" Invariants

  syntax Group ::= PreconditionGroup | PostconditionGroup | InvariantsGroup
  syntax SymbolicData ::= r"$[_a-zA-Z][_0-9a-zA-Z]*" [token]

  syntax Data ::= SymbolicData
endmodule
```
