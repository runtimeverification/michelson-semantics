requires "michelson-syntax.k"

module MICHELSON-COMMON
  imports MICHELSON-SYNTAX
  imports DOMAINS-SYNTAX

  syntax Type ::= "#NotSet"
  syntax Data ::= "#NoData"

  syntax Address ::= #Address(String)
  syntax ContractData ::= #Contract(Address, Type)
  syntax Mutez ::= #Mutez(Int)
  syntax KeyHash ::= #KeyHash(String)
  syntax ChainId ::= #ChainId(Int)
  syntax Timestamp ::= #Timestamp(Int) 
  syntax Key ::= #Key(String)
  syntax Signature ::= #Signature(String)

  syntax OperationNonce ::= #Nonce(Int)

  syntax Data ::= Timestamp 
  syntax Data ::= ChainId
  syntax Data ::= KeyHash
  syntax Data ::= Mutez
  syntax Data ::= Address
  syntax Data ::= ContractData
  syntax Data ::= Key
  syntax Data ::= Signature

  syntax Data ::= Bool

  syntax Group ::= ParameterDecl
  syntax Group ::= StorageDecl

  syntax KItem ::= #LoadGroups(Groups)
  syntax Int ::= #GroupOrder(Group) [function]

  syntax Int ::= "#GroupOrderMax" [function]
  rule #GroupOrderMax => 1000

  rule True => true [macro]
  rule False => false [macro]
endmodule
