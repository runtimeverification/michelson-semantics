```k
requires "lemmas.md"
requires "pretty-syntax.md"
```

```k
module VERIFICATION
  imports LEMMAS
  imports MICHELSON-PRETTY-SYNTAX

  syntax Data ::= "#testCode" [macro]
  rule #testCode => { PUSH int 5 ;
                      DUP ;
                      DIG 1 ; 
                      ADD ;
                      NONE operation ;
                      DROP ;
                      DIP { EMPTY_MAP timestamp key_hash ; DROP } ;
                      NONE map (set mutez) address ;
                      IF_NONE { } { } ;
                      PUSH bool True ;
                      DROP
                      }

endmodule
```


                       | BinaryBlockInstName AnnotationList Block Block

```k
module PRETTY-SPEC
  imports VERIFICATION
```

```k
  claim <k> #testCode => .K </k>
        <stack> .Stack => [ int 10 ] </stack>
```

```k
endmodule
```
