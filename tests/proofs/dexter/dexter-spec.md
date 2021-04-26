```k
module DEXTER-SPEC
  imports DEXTER-VERIFICATION

  claim <k> now 0 => . ... </k>
        <mynow> #Timestamp(0) </mynow>

  claim <k> (now 0 => .) ~> #dexterCode ... </k>
        <mynow> #Timestamp(0) </mynow>

endmodule
```
