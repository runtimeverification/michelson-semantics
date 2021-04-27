```k
module DEXTER-SPEC
  imports DEXTER-VERIFICATION

  claim <k> now 0 => . ... </k>
        <mynow> #Timestamp(0) </mynow>

  claim <k> (now 0 => .) ~> #dexterCode(true) ... </k>
        <mynow> #Timestamp(0) </mynow>

  claim <k> (now 0 => .) ~> #dexterCode(false) ... </k>
        <mynow> #Timestamp(0) </mynow>

  claim <k> (now 0 => .) ~> #dexterCode(_) ... </k>
        <mynow> #Timestamp(0) </mynow>

endmodule
```
