requires "test-michelson-syntax.k"
requires "michelson.k"

module TEST-MICHELSON
  imports TEST-MICHELSON-SYNTAX
  imports MICHELSON

  rule <k> $Instructions B ; $Stack L ; => B </k>
       <stack> Stack(_ => L) </stack> [structural]

    rule <k> $Instructions B ; $Stack L ; OL ; => OL ~> B </k>
         <stack> Stack(_ => L) </stack> [structural]

    rule O:Optional ; OL => O ~> OL [structural]
    rule .OptionalGroup => . [structural]
    rule $Contracts CL => #ReadKnownAddrs(CL) [structural]
    
    rule <k> $ParamType T => . ... </k>
         <paramtype> _ => T </paramtype> [structural]

endmodule
