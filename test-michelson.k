requires "test-michelson-syntax.k"
requires "michelson.k"

module TEST-MICHELSON
  imports TEST-MICHELSON-SYNTAX
  imports MICHELSON

  rule <k> $Instructions B ; $Stack L ; => B </k>
       <stack> Stack(_ => L) </stack> [structural]
endmodule
