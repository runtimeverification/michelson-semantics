requires "../michelson-syntax.k"
requires "michelson-unparser.k"

module MICHELSON-UNPARSER-TEST-SYNTAX
  imports MICHELSON-SYNTAX
endmodule

module MICHELSON-UNPARSER-TEST
  imports MICHELSON-UNPARSER
  configuration <k> #unparse($PGM:K) </k>
endmodule
