Michelson Program Verification Lemmas
=====================================

In order to verify Michelson program correctness, we need lemmas which guide
our symbolic computation/proof search.

This module defines relevant lemmas.

```k
requires "michelson.md"
```

```k
module LEMMAS
  imports MICHELSON

  rule X /Int 1          => X        [simplification]
  rule X *Int 1          => X        [simplification]
  rule X +Int (0 -Int Y) => X -Int Y [simplification]

endmodule
```
