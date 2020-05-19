```k
requires "unit-test.k"

module SYMBOLIC-CONFIGURATION
  imports UNIT-TEST
  configuration <michelsonTop/>
                <symbolsLoaded> false </symbolsLoaded>
                <symbols> .Map </symbols>
                <preconditions> .List </preconditions>
                <postconditions> .List </postconditions>
                <invariants> .Map </invariants>
endmodule
```
