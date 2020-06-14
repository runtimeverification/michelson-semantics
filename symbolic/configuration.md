```k
requires "unit-test/unit-test.md"

module SYMBOLIC-CONFIGURATION
  imports UNIT-TEST
  configuration <michelsonTop/>
                <symbolsLoaded> false </symbolsLoaded>
                <symbols> .Map </symbols>
                <preconditions> .List </preconditions>
                <postconditions> .List </postconditions>
                <invariants> .Map </invariants>
                <guards> .Map </guards>
                <assumeFailed> false </assumeFailed>
endmodule
```
