requires "michelson-syntax.k"
requires "michelson-common.k"

module MICHELSON-CONFIG
  imports MICHELSON-SYNTAX
  imports MICHELSON-COMMON
  imports DOMAINS

  rule <k> Gs:Group ; => Gs </k> [owise]

  configuration <michelsonTop>
                  <k> $PGM:Pgm </k>
                  <stack> .K </stack>
                  <paramtype> #NotSet </paramtype>
                  <paramvalue> #NoData </paramvalue>
                  <storagetype> #NotSet </storagetype>
                  <storagevalue> #NoData </storagevalue>
                  <mybalance> #Mutez(0) </mybalance>
                  <myamount> #Mutez(0) </myamount>
                  <mynow> #Timestamp(0) </mynow>
                  <myaddr> #Address("InvalidMyAddr") </myaddr>
                  <knownaddrs> .Map </knownaddrs>
                  <sourceaddr> #Address("InvalidSourceAddr") </sourceaddr>
                  <senderaddr> #Address("InvalidSenderAddr") </senderaddr>
                  <mychainid> #ChainId(0) </mychainid>
                  <nonce> #Nonce(0) </nonce>
                  <bigmaps> .Map </bigmaps>
                  <returncode exit=""> 1 </returncode>
                </michelsonTop>
endmodule
