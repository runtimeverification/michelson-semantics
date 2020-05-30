requires "unit-test.k"
requires "michelson-unparser.k"

module COMPAT-COMMON
  imports UNIT-TEST
  imports MICHELSON-UNPARSER

  configuration <michelsonTop/> <out stream="stdout"> .List </out>
endmodule
