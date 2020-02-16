requires "../../unit-test-syntax.k"
requires "../../michelson-config.k"

module OUTPUT-EXTRACTOR
  imports UNIT-TEST-SYNTAX
  imports MICHELSON-CONFIG

  syntax OutputGroup ::= #FindOutputGroup(Groups) [function]
  rule #FindOutputGroup(output T) => output T
  rule #FindOutputGroup(output T ; _) => output T
  rule #FindOutputGroup(_ ; G) => #FindOutputGroup(G) [owise]

  rule <k> G1 ; Gs => #FindOutputGroup(G1 ; Gs) </k>
       <returncode> _ => 0 </returncode>
endmodule
