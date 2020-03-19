requires "../michelson-syntax.k"
requires "michelson-unparser.k"

module MICHELSON-UNPARSER-TEST-SYNTAX
  imports MICHELSON-SYNTAX
endmodule

module MICHELSON-UNPARSER-TEST
  imports MICHELSON-UNPARSER
  imports DOMAINS

  syntax KItem ::= #OutputUnparse(K)

  rule <k> #OutputUnparse(T) => . </k>
       <out> _ => ListItem(#unparse(T)) </out>

  configuration <k> #OutputUnparse($PGM:K) </k>
                <out stream="stdout"> .List </out>
endmodule
