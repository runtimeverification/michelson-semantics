```k
requires "../../../pretty-syntax.md"
module LQT-TOKEN-COMPILED
  imports MICHELSON-PRETTY-SYNTAX
  imports MICHELSON
```

# Purpose

This module contains the compiled LQT token code in the `#lqtTokenCode` macro.
We have pasted the code verbatim:
(https://gitlab.com/dexter2tz/dexter2tz/-/blob/d98643881fe14996803997f1283e84ebd2067e35/lqt_fa12.mligo.tz)

# The Liquidity Baking Smart Contract Code

```k
  syntax Data ::= "#lqtTokenCode" [macro]
  // ------------------------------------
```

## Annotations and Addresses

Annotations are arbitrary tokens.
The set of permissible annotations is infinite.
Since we work with specific code that contains a finite number of annotations, we can represent each and give it a macro for wrapping it in the `#token(...)` production.

```k
  syntax FieldAnnotation ::= "%default"        [token]
                           | "%transfer"       [token]
                           | "%approve"        [token]
                           | "%mintOrBurn"     [token]
                           | "%getAllowance"   [token]
                           | "%getBalance"     [token]
                           | "%getTotalSupply" [token]
```

Each Tezos account has an address. We make a macro which stores the null address to simplify our proof scripts.

```k
  rule #lqtTokenCode
    => { DUP ;
         CDR ;
         PUSH mutez 0 ;
         AMOUNT ;
         COMPARE ;
         NEQ ;
         IF { PUSH string "DontSendTez" ; FAILWITH } {} ;
         SWAP ;
         CAR ;
         IF_LEFT
           { IF_LEFT
               { IF_LEFT
                   { SWAP ;
                     DUP ;
                     DUG 2 ;
                     CDR ;
                     CAR ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     CAR ;
                     SENDER ;
                     PAIR ;
                     PUSH nat 0 ;
                     DIG 3 ;
                     DUP ;
                     DUG 4 ;
                     CDR ;
                     COMPARE ;
                     GT ;
                     PUSH nat 0 ;
                     DIG 3 ;
                     DUP ;
                     DUG 4 ;
                     DIG 3 ;
                     DUP ;
                     DUG 4 ;
                     GET ;
                     IF_NONE { PUSH nat 0 } {} ;
                     COMPARE ;
                     GT ;
                     AND ;
                     IF { PUSH string "UnsafeAllowanceChange" ; FAILWITH } {} ;
                     DIG 3 ;
                     DUP ;
                     DUG 4 ;
                     CDR ;
                     CDR ;
                     DIG 2 ;
                     DIG 3 ;
                     CDR ;
                     PUSH nat 0 ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     COMPARE ;
                     EQ ;
                     IF { DROP ; NONE nat } { SOME } ;
                     DIG 3 ;
                     UPDATE ;
                     PAIR ;
                     SWAP ;
                     CAR ;
                     PAIR ;
                     NIL operation ;
                     PAIR }
                   { SWAP ;
                     DUP ;
                     DIG 2 ;
                     NIL operation ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     CDR ;
                     PUSH mutez 0 ;
                     DIG 4 ;
                     CDR ;
                     CAR ;
                     DIG 4 ;
                     CAR ;
                     GET ;
                     IF_NONE { PUSH nat 0 } {} ;
                     TRANSFER_TOKENS ;
                     CONS ;
                     PAIR } }
               { IF_LEFT
                   { SWAP ;
                     DUP ;
                     DIG 2 ;
                     NIL operation ;
                     SWAP ;
                     DUP ;
                     DUG 2 ;
                     CDR ;
                     PUSH mutez 0 ;
                     DIG 4 ;
                     CAR ;
                     DIG 4 ;
                     CAR ;
                     GET ;
                     IF_NONE { PUSH nat 0 } {} ;
                     TRANSFER_TOKENS ;
                     CONS ;
                     PAIR }
                   { SWAP ;
                     DUP ;
                     DIG 2 ;
                     NIL operation ;
                     SWAP ;
                     CDR ;
                     PUSH mutez 0 ;
                     DIG 3 ;
                     CDR ;
                     CDR ;
                     CDR ;
                     TRANSFER_TOKENS ;
                     CONS ;
                     PAIR } } }
           { IF_LEFT
               { SWAP ;
                 DUP ;
                 DUG 2 ;
                 CDR ;
                 CDR ;
                 CAR ;
                 SENDER ;
                 COMPARE ;
                 NEQ ;
                 IF { PUSH string "OnlyAdmin" ; FAILWITH } {} ;
                 DUP ;
                 CAR ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 CAR ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 CDR ;
                 GET ;
                 IF_NONE { PUSH nat 0 } {} ;
                 ADD ;
                 ISNAT ;
                 IF_NONE
                   { PUSH string "Cannot burn more than the target's balance." ; FAILWITH }
                   {} ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CAR ;
                 DIG 3 ;
                 DUP ;
                 DUG 4 ;
                 CDR ;
                 CDR ;
                 CDR ;
                 ADD ;
                 ABS ;
                 DIG 3 ;
                 DUP ;
                 DUG 4 ;
                 CDR ;
                 DIG 4 ;
                 CAR ;
                 PUSH nat 0 ;
                 DIG 4 ;
                 DUP ;
                 DUG 5 ;
                 COMPARE ;
                 EQ ;
                 IF { DIG 3 ; DROP ; NONE nat } { DIG 3 ; SOME } ;
                 DIG 4 ;
                 CDR ;
                 UPDATE ;
                 PAIR ;
                 DUP ;
                 DUG 2 ;
                 CDR ;
                 CDR ;
                 CAR ;
                 PAIR ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 CDR ;
                 CAR ;
                 PAIR ;
                 SWAP ;
                 CAR ;
                 PAIR ;
                 NIL operation ;
                 PAIR }
               { SWAP ;
                 DUP ;
                 DUG 2 ;
                 CDR ;
                 CAR ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 CAR ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 CAR ;
                 SENDER ;
                 COMPARE ;
                 EQ ;
                 IF { SWAP }
                    { SENDER ;
                      DIG 3 ;
                      DUP ;
                      DUG 4 ;
                      CAR ;
                      PAIR ;
                      DIG 3 ;
                      DUP ;
                      DUG 4 ;
                      CDR ;
                      CDR ;
                      DIG 3 ;
                      DUP ;
                      DUG 4 ;
                      DIG 2 ;
                      DUP ;
                      DUG 3 ;
                      GET ;
                      IF_NONE { PUSH nat 0 } {} ;
                      SUB ;
                      ISNAT ;
                      IF_NONE { PUSH string "NotEnoughAllowance" ; FAILWITH } {} ;
                      DIG 3 ;
                      PUSH nat 0 ;
                      DIG 2 ;
                      DUP ;
                      DUG 3 ;
                      COMPARE ;
                      EQ ;
                      IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                      DIG 2 ;
                      UPDATE } ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 CDR ;
                 CDR ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 DIG 4 ;
                 DUP ;
                 DUG 5 ;
                 CAR ;
                 GET ;
                 IF_NONE { PUSH nat 0 } {} ;
                 SUB ;
                 ISNAT ;
                 IF_NONE { PUSH string "NotEnoughBalance" ; FAILWITH } {} ;
                 DIG 2 ;
                 PUSH nat 0 ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 COMPARE ;
                 EQ ;
                 IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                 DIG 3 ;
                 DUP ;
                 DUG 4 ;
                 CAR ;
                 UPDATE ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 CDR ;
                 CDR ;
                 SWAP ;
                 DUP ;
                 DUG 2 ;
                 DIG 4 ;
                 DUP ;
                 DUG 5 ;
                 CDR ;
                 CAR ;
                 GET ;
                 IF_NONE { PUSH nat 0 } {} ;
                 ADD ;
                 SWAP ;
                 PUSH nat 0 ;
                 DIG 2 ;
                 DUP ;
                 DUG 3 ;
                 COMPARE ;
                 EQ ;
                 IF { SWAP ; DROP ; NONE nat } { SWAP ; SOME } ;
                 DIG 3 ;
                 CDR ;
                 CAR ;
                 UPDATE ;
                 DIG 2 ;
                 CDR ;
                 SWAP ;
                 PAIR ;
                 DUP ;
                 CDR ;
                 CDR ;
                 DIG 2 ;
                 PAIR ;
                 SWAP ;
                 CAR ;
                 PAIR ;
                 NIL operation ;
                 PAIR } } }
```

```k
endmodule
```
