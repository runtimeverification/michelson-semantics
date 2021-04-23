```k
requires "dexter-compiled.md"
module DEXTER-SPEC
  imports DEXTER-COMPILED
  imports DEXTER-VERIFICATION

  claim <k> now 0 => . ... </k>
        <mynow> #Timestamp(0) </mynow>

  claim <k> #dexterCode => _ ... </k>

endmodule
```
