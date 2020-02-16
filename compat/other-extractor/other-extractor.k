requires "../../unit-test-syntax.k"
requires "../../michelson-config.k"

module OTHER-EXTRACTOR
  imports UNIT-TEST-SYNTAX
  imports MICHELSON-CONFIG

  syntax OtherContractsMap ::= #FindContractsGroup(Groups) [function]
  rule #FindContractsGroup(other_contracts M) => M
  rule #FindContractsGroup(other_contracts M ; _) => M
  rule #FindContractsGroup(_:Group) => { } [owise]
  rule #FindContractsGroup(_ ; G) => #FindContractsGroup(G) [owise]

  rule <k> G1 ; Gs => #FindContractsGroup(G1 ; Gs) </k>
       <returncode> _ => 0 </returncode>
endmodule
