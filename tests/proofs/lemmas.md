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

  rule #Ceil(#DoCompare(@A:Int, @B:Int)) => #Ceil(@A) #And #Ceil(@B)
    [anywhere, simplification]

  rule #MichelineToNative(M:Map, map _ _ _, _, _) => M
    [simplification]

  rule size(L:InternalList) >=Int 0 => true [simplification, smt-lemma]
endmodule
```
