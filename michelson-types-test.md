```k
requires "michelson-types.k"
requires "michelson-config.k"

module MICHELSON-TYPES-TEST-SYNTAX
  imports MICHELSON-SYNTAX
endmodule

module MICHELSON-TYPES-TEST
  imports MICHELSON-CONFIG
  imports MICHELSON-TYPES

  rule <k> contract { code B ; storage St ; parameter Pt ; } => #TypeInstruction(Pt, B, pair .AnnotationList Pt St) </k>
endmodule
```
