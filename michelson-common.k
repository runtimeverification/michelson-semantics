requires "michelson-syntax.k"

module MICHELSON-COMMON
  imports MICHELSON-SYNTAX
  imports DOMAINS

  syntax Type ::= "#NotSet"

  syntax Address ::= #Address(String)
  syntax ContractData ::= #Contract(Address, Type)
  syntax Mutez ::= #Mutez(Int)
  syntax KeyHash ::= #KeyHash(String)
  syntax ChainId ::= #ChainId(Int)
  syntax Timestamp ::= #Timestamp(Int) 
  syntax Key ::= #Key(String)
  syntax Signature ::= #Signature(String)

  syntax Data ::= Timestamp 
  syntax Data ::= ChainId
  syntax Data ::= KeyHash
  syntax Data ::= Mutez
  syntax Data ::= Address
  syntax Data ::= ContractData
  syntax Data ::= Key
  syntax Data ::= Signature

  rule True => true [macro]
  rule False => false [macro]
endmodule
