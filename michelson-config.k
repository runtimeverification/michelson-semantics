requires "michelson-syntax.k"

module MICHELSON-CONFIG
  imports MICHELSON-SYNTAX
  imports DOMAINS

  syntax Type ::= "#NotSet"

  syntax Address ::= #Address(Int)
  syntax ContractData ::= #Contract(Address, Type)
  syntax Mutez ::= #Mutez(Int)
  syntax KeyHash ::= #KeyHash(Int)
  syntax ChainId ::= #ChainId(Int)
  syntax Timestamp ::= #Timestamp(Int) 

  syntax Data ::= Timestamp 
  syntax Data ::= ChainId
  syntax Data ::= KeyHash
  syntax Data ::= Mutez
  syntax Data ::= Address
  syntax Data ::= ContractData

  configuration <k> $PGM:Pgm </k>
                <stack> .K </stack>
                <paramtype> #NotSet </paramtype>
                <storagetype> #NotSet </storagetype>
                <balance> #Mutez(0) </balance>
                <amount> #Mutez(0) </amount>
                <now> #Timestamp(-1) </now>
                <myaddr> #Address(-1) </myaddr>
                <knownaddrs> .Map </knownaddrs>
                <sourceaddr> #Address(-2) </sourceaddr>
                <senderaddr> #Address(-3) </senderaddr>
                <chainid> #ChainId(0) </chainid>
endmodule
