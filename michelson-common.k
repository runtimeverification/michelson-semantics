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

  syntax MBytes ::= MBytesLiteral 
                  | #Packed(Data)
                  | #SHA256(MBytes) 
                  | #SHA512(MBytes)

  syntax Data ::= Timestamp 
  syntax Data ::= ChainId
  syntax Data ::= KeyHash
  syntax Data ::= Mutez
  syntax Data ::= Address
  syntax Data ::= ContractData
  syntax Data ::= Key
  syntax Data ::= Signature
  syntax Data ::= Bool
  syntax Data ::= #Lambda(Type, Type, Block)
  syntax Data ::= MBytes

  syntax Group ::= ParameterDecl
  syntax Group ::= StorageDecl

  syntax KItem ::= #LoadGroups(Groups)
  syntax Int ::= #GroupOrder(Group) [function]

  syntax Int ::= "#GroupOrderMax" [function]
  rule #GroupOrderMax => 1000

  rule True => true [macro]
  rule False => false [macro]

  rule code B ; storage St ; parameter Pt => code B ; storage St ; parameter Pt ; [macro]
  rule code B ; parameter Pt ; storage St => code B ; storage St ; parameter Pt ; [macro]
  rule storage St ; code B ; parameter Pt => code B ; storage St ; parameter Pt ; [macro]
  rule parameter Pt ; code B ; storage St => code B ; storage St ; parameter Pt ; [macro]
  rule storage St ; parameter Pt ; code B => code B ; storage St ; parameter Pt ; [macro]
  rule parameter Pt ; storage St ; code B => code B ; storage St ; parameter Pt ; [macro]

  rule code B ; parameter Pt ; storage St ; => code B ; storage St ; parameter Pt ; [macro]
  rule storage St ; code B ; parameter Pt ; => code B ; storage St ; parameter Pt ; [macro]
  rule parameter Pt ; code B ; storage St ; => code B ; storage St ; parameter Pt ; [macro]
  rule storage St ; parameter Pt ; code B ; => code B ; storage St ; parameter Pt ; [macro]
  rule parameter Pt ; storage St ; code B ; => code B ; storage St ; parameter Pt ; [macro]
endmodule
