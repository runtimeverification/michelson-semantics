```k
requires "unit-test/unit-test.md"

module SYMBOLIC-CONFIGURATION
  imports UNIT-TEST
  imports SYMBOLIC-UNIT-TEST-SYNTAX

  configuration <michelsonTop/>
                <symbols> .Map </symbols>
                <pre> {}:Blocks </pre>
                <post> {}:Blocks </post>
                <invs> .Map </invs>
                <assumeFailed> false </assumeFailed>
endmodule
```
