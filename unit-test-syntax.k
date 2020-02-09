requires "michelson-operations-syntax.k"
requires "michelson-syntax.k"

module UNIT-TEST-SYNTAX
  imports MICHELSON-SYNTAX
  imports MICHELSON-OPERATIONS-SYNTAX

  syntax StackElement ::= "Stack_elt" Type Data
  syntax StackElementList ::= StackElement | StackElement ";" StackElementList
  syntax LiteralStack ::= EmptyBlock | "{" StackElementList "}"

  syntax OtherContractsMapEntry ::= "Elt" String Type
  syntax OtherContractsMapEntryList ::= OtherContractsMapEntry | OtherContractsMapEntry ";" OtherContractsMapEntryList

  syntax OtherContractsMap ::= EmptyBlock | "{" OtherContractsMapEntryList "}"

  syntax FailedStack ::= "(" "Failed" Data ")"

  syntax TestCode ::= Instruction | Block

  syntax OutputStack ::= LiteralStack | FailedStack

  syntax InputGroup ::= "input" LiteralStack
  syntax CodeGroup ::= "code" TestCode
  syntax OutputGroup ::= "output" OutputStack

  syntax NowGroup ::= "now" Int
  syntax SenderGroup ::= "sender" String
  syntax SourceGroup ::= "source" String
  syntax ChainGroup ::= "chain_id" MBytesLiteral 
  syntax SelfGroup ::= "self" String
  syntax ParameterGroup ::= ParameterDecl
  syntax AmountGroup ::= "amount" Int
  syntax BalanceGroup ::= "balance" Int
  syntax ContractsGroup ::= "other_contracts" OtherContractsMap

  syntax Group ::= InputGroup
                 | CodeGroup
                 | OutputGroup
                 | NowGroup
                 | SenderGroup
                 | SourceGroup
                 | ChainGroup
                 | SelfGroup
                 | ParameterGroup
                 | AmountGroup
                 | BalanceGroup
                 | ContractsGroup

  syntax Groups ::= Group | Group ";" Groups
  syntax Pgm ::= Groups
endmodule 
