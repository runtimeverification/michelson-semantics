```k
requires "unit-test/unit-test.md"

module SYMBOLIC-CONFIGURATION
  imports UNIT-TEST
  imports SYMBOLIC-UNIT-TEST-SYNTAX

  configuration <michelsonTop/>
                <pre> {}:Blocks </pre>
                <post> {}:Blocks </post>
                <invs> .Map </invs>
endmodule
```
