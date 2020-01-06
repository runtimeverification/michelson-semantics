requires "michelson-syntax.k"
requires "michelson-common.k"

module MICHELSON-CONFIG
  imports MICHELSON-SYNTAX
  imports MICHELSON-COMMON
  imports DOMAINS

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
