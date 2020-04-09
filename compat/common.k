requires "unit-test.k"
requires "michelson-unparser.k"

module COMPAT-COMMON
  imports UNIT-TEST
  imports MICHELSON-UNPARSER

  rule G:Group ; => G [anywhere]

  configuration <michelsonTop/> <out stream="stdout"> .List </out>
endmodule
