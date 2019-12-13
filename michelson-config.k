requires "michelson-syntax.k"

module MICHELSON-CONFIG
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

  configuration <k> $PGM:Pgm </k>
                <stack> .K </stack>
                <paramtype> #NotSet </paramtype>
                <storagetype> #NotSet </storagetype>
                <balance> #Mutez(0) </balance>
                <amount> #Mutez(0) </amount>
                <now> #Timestamp(-1) </now>
                <myaddr> #Address("InvalidMyAddr") </myaddr>
                <knownaddrs> .Map </knownaddrs>
                <sourceaddr> #Address("InvalidSourceAddr") </sourceaddr>
                <senderaddr> #Address("InvalidSenderAddr") </senderaddr>
                <chainid> #ChainId(0) </chainid>
endmodule
