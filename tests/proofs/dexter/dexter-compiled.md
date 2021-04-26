```k
requires "../../../syntax.md"
module DEXTER-COMPILED
  imports MICHELSON-INTERNAL-SYNTAX
```

# Purpose

This module contains the compiled Dexter code for version FA1.2 and FA2.
The macros `#dexterCode` and `#dexterCodeFA2` translate to the respective Michelson code.
We have pasted the code verbatim:
https://gitlab.com/dexter2tz/dexter2tz/-/blob/8a5792a56e0143042926c3ca8bff7d7068a541c3/dexter.mligo.tz
https://gitlab.com/dexter2tz/dexter2tz/-/blob/8a5792a56e0143042926c3ca8bff7d7068a541c3/dexter.fa2.mligo.tz

# The Dexter Smart Contract Code

```k
  syntax Data ::= "#dexterCode" | "#dexterCodeFA2"

  rule #dexterCode
    => { DUP ;
         CDR ;
         SWAP ;
         CAR ;
         IF_LEFT
           { IF_LEFT
               { IF_LEFT
                   { IF_LEFT
                       { DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         SWAP ;
                         DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         SWAP ;
                         DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         DIG 4 ;
                         DUP ;
                         DUG 5 ;
                         CDR ;
                         CDR ;
                         CDR ;
                         CAR ;
                         IF { DROP 5 ; PUSH nat 2 ; FAILWITH }
                            { SWAP ;
                              NOW ;
                              COMPARE ;
                              GE ;
                              IF { DROP 4 ; PUSH nat 3 ; FAILWITH }
                                 { PUSH mutez 1 ;
                                   DIG 4 ;
                                   DUP ;
                                   DUG 5 ;
                                   CDR ;
                                   CAR ;
                                   EDIV ;
                                   IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                                   CAR ;
                                   AMOUNT ;
                                   PUSH mutez 1 ;
                                   SWAP ;
                                   EDIV ;
                                   IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                                   CAR ;
                                   SWAP ;
                                   DUP ;
                                   DUG 2 ;
                                   DIG 6 ;
                                   DUP ;
                                   DUG 7 ;
                                   CDR ;
                                   CDR ;
                                   CAR ;
                                   DIG 2 ;
                                   DUP ;
                                   DUG 3 ;
                                   MUL ;
                                   EDIV ;
                                   IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                                   CAR ;
                                   DIG 2 ;
                                   DIG 6 ;
                                   DUP ;
                                   DUG 7 ;
                                   CAR ;
                                   DIG 3 ;
                                   MUL ;
                                   EDIV ;
                                   IF_NONE
                                     { PUSH string "DIV by 0" ; FAILWITH }
                                     { DUP ;
                                       CDR ;
                                       SWAP ;
                                       CAR ;
                                       PUSH nat 0 ;
                                       DIG 2 ;
                                       COMPARE ;
                                       EQ ;
                                       IF {} { PUSH nat 1 ; ADD } } ;
                                   DIG 2 ;
                                   SWAP ;
                                   DUP ;
                                   DUG 2 ;
                                   COMPARE ;
                                   GT ;
                                   IF { DROP 5 ; PUSH nat 4 ; FAILWITH }
                                      { DUG 2 ;
                                        DUP ;
                                        DUG 3 ;
                                        COMPARE ;
                                        LT ;
                                        IF { DROP 4 ; PUSH nat 5 ; FAILWITH }
                                           { DIG 3 ;
                                             DUP ;
                                             DUG 4 ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             DIG 2 ;
                                             DUP ;
                                             DUG 3 ;
                                             DIG 5 ;
                                             DUP ;
                                             DUG 6 ;
                                             CDR ;
                                             CDR ;
                                             CAR ;
                                             ADD ;
                                             PAIR ;
                                             DIG 4 ;
                                             DUP ;
                                             DUG 5 ;
                                             CDR ;
                                             CAR ;
                                             PAIR ;
                                             DIG 4 ;
                                             DUP ;
                                             DUG 5 ;
                                             CAR ;
                                             PAIR ;
                                             CDR ;
                                             SWAP ;
                                             DUP ;
                                             DUG 2 ;
                                             DIG 5 ;
                                             DUP ;
                                             DUG 6 ;
                                             CAR ;
                                             ADD ;
                                             PAIR ;
                                             DUP ;
                                             CDR ;
                                             CDR ;
                                             AMOUNT ;
                                             DIG 6 ;
                                             CDR ;
                                             CAR ;
                                             ADD ;
                                             PAIR ;
                                             SWAP ;
                                             CAR ;
                                             PAIR ;
                                             SWAP ;
                                             SELF ;
                                             ADDRESS ;
                                             PAIR ;
                                             SENDER ;
                                             PAIR ;
                                             SWAP ;
                                             DUP ;
                                             DUG 2 ;
                                             SWAP ;
                                             DUP ;
                                             CDR ;
                                             SWAP ;
                                             CAR ;
                                             SWAP ;
                                             DUP ;
                                             CDR ;
                                             SWAP ;
                                             CAR ;
                                             DIG 3 ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CAR ;
                                             CONTRACT %transfer (pair address (pair address nat)) ;
                                             IF_NONE { PUSH nat 0 ; FAILWITH } {} ;
                                             PUSH mutez 0 ;
                                             DIG 3 ;
                                             DIG 3 ;
                                             PAIR ;
                                             DIG 3 ;
                                             PAIR ;
                                             TRANSFER_TOKENS ;
                                             DIG 2 ;
                                             INT ;
                                             DIG 3 ;
                                             PAIR ;
                                             DIG 2 ;
                                             DUP ;
                                             DUG 3 ;
                                             SWAP ;
                                             DUP ;
                                             CDR ;
                                             SWAP ;
                                             CAR ;
                                             DIG 2 ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CONTRACT %mintOrBurn (pair (int %quantity) (address %target)) ;
                                             IF_NONE { PUSH nat 12 ; FAILWITH } {} ;
                                             PUSH mutez 0 ;
                                             DIG 2 ;
                                             DIG 3 ;
                                             PAIR ;
                                             TRANSFER_TOKENS ;
                                             DIG 2 ;
                                             NIL operation ;
                                             DIG 2 ;
                                             CONS ;
                                             DIG 2 ;
                                             CONS ;
                                             PAIR } } } } }
                       { DROP ;
                         DUP ;
                         CDR ;
                         CDR ;
                         CDR ;
                         CAR ;
                         IF { DROP ; PUSH nat 2 ; FAILWITH }
                            { DUP ;
                              CDR ;
                              CDR ;
                              AMOUNT ;
                              DIG 2 ;
                              DUP ;
                              DUG 3 ;
                              CDR ;
                              CAR ;
                              ADD ;
                              PAIR ;
                              SWAP ;
                              CAR ;
                              PAIR ;
                              NIL operation ;
                              PAIR } } }
                   { IF_LEFT
                       { DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         SWAP ;
                         DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         SWAP ;
                         DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         SWAP ;
                         DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         DIG 5 ;
                         DUP ;
                         DUG 6 ;
                         CDR ;
                         CDR ;
                         CDR ;
                         CAR ;
                         IF { DROP 6 ; PUSH nat 2 ; FAILWITH }
                            { SWAP ;
                              NOW ;
                              COMPARE ;
                              GE ;
                              IF { DROP 5 ; PUSH nat 3 ; FAILWITH }
                                 { PUSH mutez 0 ;
                                   AMOUNT ;
                                   COMPARE ;
                                   GT ;
                                   IF { DROP 5 ; PUSH nat 10 ; FAILWITH }
                                      { DIG 4 ;
                                        DUP ;
                                        DUG 5 ;
                                        CDR ;
                                        CDR ;
                                        CAR ;
                                        PUSH mutez 1 ;
                                        DIG 6 ;
                                        DUP ;
                                        DUG 7 ;
                                        CDR ;
                                        CAR ;
                                        EDIV ;
                                        IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                                        CAR ;
                                        DIG 4 ;
                                        DUP ;
                                        DUG 5 ;
                                        MUL ;
                                        EDIV ;
                                        IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                                        CAR ;
                                        PUSH mutez 1 ;
                                        SWAP ;
                                        MUL ;
                                        DIG 5 ;
                                        DUP ;
                                        DUG 6 ;
                                        CDR ;
                                        CDR ;
                                        CAR ;
                                        DIG 6 ;
                                        DUP ;
                                        DUG 7 ;
                                        CAR ;
                                        DIG 5 ;
                                        DUP ;
                                        DUG 6 ;
                                        MUL ;
                                        EDIV ;
                                        IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                                        CAR ;
                                        DIG 3 ;
                                        DIG 2 ;
                                        DUP ;
                                        DUG 3 ;
                                        COMPARE ;
                                        LT ;
                                        IF { DROP 6 ; PUSH nat 11 ; FAILWITH }
                                           { DIG 2 ;
                                             SWAP ;
                                             DUP ;
                                             DUG 2 ;
                                             COMPARE ;
                                             LT ;
                                             IF { DROP 5 ; PUSH nat 13 ; FAILWITH }
                                                { DIG 2 ;
                                                  DUP ;
                                                  DUG 3 ;
                                                  DIG 5 ;
                                                  DUP ;
                                                  DUG 6 ;
                                                  CDR ;
                                                  CDR ;
                                                  CAR ;
                                                  SUB ;
                                                  ISNAT ;
                                                  IF_NONE { PUSH nat 14 ; FAILWITH } {} ;
                                                  SWAP ;
                                                  DUP ;
                                                  DUG 2 ;
                                                  DIG 6 ;
                                                  DUP ;
                                                  DUG 7 ;
                                                  CAR ;
                                                  SUB ;
                                                  ISNAT ;
                                                  IF_NONE { PUSH nat 15 ; FAILWITH } {} ;
                                                  DIG 4 ;
                                                  PUSH int 0 ;
                                                  SUB ;
                                                  SENDER ;
                                                  PAIR ;
                                                  DIG 6 ;
                                                  DUP ;
                                                  DUG 7 ;
                                                  SWAP ;
                                                  DUP ;
                                                  CDR ;
                                                  SWAP ;
                                                  CAR ;
                                                  DIG 2 ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CONTRACT %mintOrBurn (pair (int %quantity) (address %target)) ;
                                                  IF_NONE { PUSH nat 12 ; FAILWITH } {} ;
                                                  PUSH mutez 0 ;
                                                  DIG 2 ;
                                                  DIG 3 ;
                                                  PAIR ;
                                                  TRANSFER_TOKENS ;
                                                  DIG 3 ;
                                                  DIG 5 ;
                                                  DUP ;
                                                  DUG 6 ;
                                                  PAIR ;
                                                  SELF ;
                                                  ADDRESS ;
                                                  PAIR ;
                                                  DIG 6 ;
                                                  DUP ;
                                                  DUG 7 ;
                                                  SWAP ;
                                                  DUP ;
                                                  CDR ;
                                                  SWAP ;
                                                  CAR ;
                                                  SWAP ;
                                                  DUP ;
                                                  CDR ;
                                                  SWAP ;
                                                  CAR ;
                                                  DIG 3 ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CAR ;
                                                  CONTRACT %transfer (pair address (pair address nat)) ;
                                                  IF_NONE { PUSH nat 0 ; FAILWITH } {} ;
                                                  PUSH mutez 0 ;
                                                  DIG 3 ;
                                                  DIG 3 ;
                                                  PAIR ;
                                                  DIG 3 ;
                                                  PAIR ;
                                                  TRANSFER_TOKENS ;
                                                  DIG 4 ;
                                                  DUP ;
                                                  DUG 5 ;
                                                  DIG 6 ;
                                                  CONTRACT unit ;
                                                  IF_NONE { PUSH nat 9 ; FAILWITH } {} ;
                                                  SWAP ;
                                                  PUSH unit Unit ;
                                                  TRANSFER_TOKENS ;
                                                  DIG 6 ;
                                                  DUP ;
                                                  DUG 7 ;
                                                  CDR ;
                                                  CDR ;
                                                  DIG 6 ;
                                                  DIG 7 ;
                                                  DUP ;
                                                  DUG 8 ;
                                                  CDR ;
                                                  CAR ;
                                                  SUB ;
                                                  PAIR ;
                                                  DIG 6 ;
                                                  CAR ;
                                                  PAIR ;
                                                  DUP ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  DIG 6 ;
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
                                                  CDR ;
                                                  DIG 4 ;
                                                  PAIR ;
                                                  NIL operation ;
                                                  DIG 2 ;
                                                  CONS ;
                                                  DIG 2 ;
                                                  CONS ;
                                                  DIG 2 ;
                                                  CONS ;
                                                  PAIR } } } } } }
                       { DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         DIG 2 ;
                         DUP ;
                         DUG 3 ;
                         CDR ;
                         CDR ;
                         CDR ;
                         CAR ;
                         IF { DROP 3 ; PUSH nat 2 ; FAILWITH }
                            { PUSH mutez 0 ;
                              AMOUNT ;
                              COMPARE ;
                              GT ;
                              IF { DROP 3 ; PUSH nat 10 ; FAILWITH }
                                 { DIG 2 ;
                                   DUP ;
                                   DUG 3 ;
                                   CDR ;
                                   CDR ;
                                   CDR ;
                                   CDR ;
                                   CDR ;
                                   CAR ;
                                   SENDER ;
                                   COMPARE ;
                                   NEQ ;
                                   IF { DROP 3 ; PUSH nat 20 ; FAILWITH }
                                      { DIG 2 ;
                                        DUP ;
                                        DUG 3 ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CAR ;
                                        IF { DROP 3 ; PUSH nat 22 ; FAILWITH }
                                           { DIG 2 ;
                                             DUP ;
                                             DUG 3 ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             DIG 2 ;
                                             PAIR ;
                                             DIG 2 ;
                                             DUP ;
                                             DUG 3 ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CAR ;
                                             PAIR ;
                                             DIG 2 ;
                                             DUP ;
                                             DUG 3 ;
                                             CDR ;
                                             CDR ;
                                             CAR ;
                                             PAIR ;
                                             DIG 2 ;
                                             DUP ;
                                             DUG 3 ;
                                             CDR ;
                                             CAR ;
                                             PAIR ;
                                             DIG 2 ;
                                             CAR ;
                                             PAIR ;
                                             NIL operation ;
                                             DIG 2 ;
                                             SET_DELEGATE ;
                                             CONS ;
                                             PAIR } } } } } } }
               { IF_LEFT
                   { IF_LEFT
                       { SWAP ;
                         DUP ;
                         DUG 2 ;
                         CDR ;
                         CDR ;
                         CDR ;
                         CAR ;
                         IF { DROP 2 ; PUSH nat 2 ; FAILWITH }
                            { PUSH mutez 0 ;
                              AMOUNT ;
                              COMPARE ;
                              GT ;
                              IF { DROP 2 ; PUSH nat 10 ; FAILWITH }
                                 { SWAP ;
                                   DUP ;
                                   DUG 2 ;
                                   CDR ;
                                   CDR ;
                                   CDR ;
                                   CDR ;
                                   CDR ;
                                   CAR ;
                                   SENDER ;
                                   COMPARE ;
                                   NEQ ;
                                   IF { DROP 2 ; PUSH nat 23 ; FAILWITH }
                                      { PUSH address "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" ;
                                        DIG 2 ;
                                        DUP ;
                                        DUG 3 ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        COMPARE ;
                                        NEQ ;
                                        IF { DROP 2 ; PUSH nat 24 ; FAILWITH }
                                           { SWAP ;
                                             DUP ;
                                             DUG 2 ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CAR ;
                                             PAIR ;
                                             SWAP ;
                                             DUP ;
                                             DUG 2 ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CAR ;
                                             PAIR ;
                                             SWAP ;
                                             DUP ;
                                             DUG 2 ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CAR ;
                                             PAIR ;
                                             SWAP ;
                                             DUP ;
                                             DUG 2 ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CAR ;
                                             PAIR ;
                                             SWAP ;
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
                                             PAIR } } } } }
                       { SWAP ;
                         DUP ;
                         DUG 2 ;
                         CDR ;
                         CDR ;
                         CDR ;
                         CAR ;
                         IF { DROP 2 ; PUSH nat 2 ; FAILWITH }
                            { PUSH mutez 0 ;
                              AMOUNT ;
                              COMPARE ;
                              GT ;
                              IF { DROP 2 ; PUSH nat 10 ; FAILWITH }
                                 { SWAP ;
                                   DUP ;
                                   DUG 2 ;
                                   CDR ;
                                   CDR ;
                                   CDR ;
                                   CDR ;
                                   CDR ;
                                   CAR ;
                                   SENDER ;
                                   COMPARE ;
                                   NEQ ;
                                   IF { DROP 2 ; PUSH nat 21 ; FAILWITH }
                                      { SWAP ;
                                        DUP ;
                                        DUG 2 ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        SWAP ;
                                        PAIR ;
                                        SWAP ;
                                        DUP ;
                                        DUG 2 ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CAR ;
                                        PAIR ;
                                        SWAP ;
                                        DUP ;
                                        DUG 2 ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CAR ;
                                        PAIR ;
                                        SWAP ;
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
                                        PAIR } } } } }
                   { IF_LEFT
                       { DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         SWAP ;
                         DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         SWAP ;
                         DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         SWAP ;
                         DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         DIG 4 ;
                         CONTRACT %xtzToToken
                           (pair (address %to) (pair (nat %minTokensBought) (timestamp %deadline))) ;
                         IF_NONE { PUSH nat 31 ; FAILWITH } {} ;
                         DIG 5 ;
                         DUP ;
                         DUG 6 ;
                         CDR ;
                         CDR ;
                         CDR ;
                         CAR ;
                         IF { DROP 6 ; PUSH nat 2 ; FAILWITH }
                            { PUSH mutez 0 ;
                              AMOUNT ;
                              COMPARE ;
                              GT ;
                              IF { DROP 6 ; PUSH nat 10 ; FAILWITH }
                                 { DIG 2 ;
                                   DUP ;
                                   DUG 3 ;
                                   NOW ;
                                   COMPARE ;
                                   GE ;
                                   IF { DROP 6 ; PUSH nat 3 ; FAILWITH }
                                      { PUSH nat 997 ;
                                        DIG 2 ;
                                        DUP ;
                                        DUG 3 ;
                                        MUL ;
                                        PUSH nat 1000 ;
                                        DIG 7 ;
                                        DUP ;
                                        DUG 8 ;
                                        CAR ;
                                        MUL ;
                                        ADD ;
                                        PUSH mutez 1 ;
                                        DIG 7 ;
                                        DUP ;
                                        DUG 8 ;
                                        CDR ;
                                        CAR ;
                                        EDIV ;
                                        IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                                        CAR ;
                                        PUSH nat 997 ;
                                        DIG 4 ;
                                        DUP ;
                                        DUG 5 ;
                                        MUL ;
                                        MUL ;
                                        EDIV ;
                                        IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                                        CAR ;
                                        PUSH mutez 1 ;
                                        SWAP ;
                                        MUL ;
                                        DIG 6 ;
                                        DUP ;
                                        DUG 7 ;
                                        CDR ;
                                        DIG 3 ;
                                        DUP ;
                                        DUG 4 ;
                                        DIG 8 ;
                                        DUP ;
                                        DUG 9 ;
                                        CAR ;
                                        ADD ;
                                        PAIR ;
                                        DUP ;
                                        CDR ;
                                        CDR ;
                                        DIG 2 ;
                                        DUP ;
                                        DUG 3 ;
                                        DIG 9 ;
                                        CDR ;
                                        CAR ;
                                        SUB ;
                                        PAIR ;
                                        SWAP ;
                                        CAR ;
                                        PAIR ;
                                        DIG 3 ;
                                        SELF ;
                                        ADDRESS ;
                                        PAIR ;
                                        SENDER ;
                                        PAIR ;
                                        SWAP ;
                                        DUP ;
                                        DUG 2 ;
                                        SWAP ;
                                        DUP ;
                                        CDR ;
                                        SWAP ;
                                        CAR ;
                                        SWAP ;
                                        DUP ;
                                        CDR ;
                                        SWAP ;
                                        CAR ;
                                        DIG 3 ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CAR ;
                                        CONTRACT %transfer (pair address (pair address nat)) ;
                                        IF_NONE { PUSH nat 0 ; FAILWITH } {} ;
                                        PUSH mutez 0 ;
                                        DIG 3 ;
                                        DIG 3 ;
                                        PAIR ;
                                        DIG 3 ;
                                        PAIR ;
                                        TRANSFER_TOKENS ;
                                        DIG 3 ;
                                        DIG 3 ;
                                        DIG 4 ;
                                        DIG 6 ;
                                        PAIR ;
                                        DIG 5 ;
                                        PAIR ;
                                        TRANSFER_TOKENS ;
                                        DIG 2 ;
                                        NIL operation ;
                                        DIG 2 ;
                                        CONS ;
                                        DIG 2 ;
                                        CONS ;
                                        PAIR } } } }
                       { DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         SWAP ;
                         DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         SWAP ;
                         DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         DIG 4 ;
                         DUP ;
                         DUG 5 ;
                         CDR ;
                         CDR ;
                         CDR ;
                         CAR ;
                         IF { DROP 5 ; PUSH nat 2 ; FAILWITH }
                            { SWAP ;
                              NOW ;
                              COMPARE ;
                              GE ;
                              IF { DROP 4 ; PUSH nat 3 ; FAILWITH }
                                 { PUSH mutez 0 ;
                                   AMOUNT ;
                                   COMPARE ;
                                   GT ;
                                   IF { DROP 4 ; PUSH nat 10 ; FAILWITH }
                                      { PUSH nat 997 ;
                                        DIG 2 ;
                                        DUP ;
                                        DUG 3 ;
                                        MUL ;
                                        PUSH nat 1000 ;
                                        DIG 5 ;
                                        DUP ;
                                        DUG 6 ;
                                        CAR ;
                                        MUL ;
                                        ADD ;
                                        PUSH mutez 1 ;
                                        DIG 5 ;
                                        DUP ;
                                        DUG 6 ;
                                        CDR ;
                                        CAR ;
                                        EDIV ;
                                        IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                                        CAR ;
                                        PUSH nat 997 ;
                                        DIG 4 ;
                                        DUP ;
                                        DUG 5 ;
                                        MUL ;
                                        MUL ;
                                        EDIV ;
                                        IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                                        CAR ;
                                        PUSH mutez 1 ;
                                        SWAP ;
                                        MUL ;
                                        DUP ;
                                        DUG 2 ;
                                        COMPARE ;
                                        LT ;
                                        IF { DROP ; PUSH nat 8 ; FAILWITH } {} ;
                                        SWAP ;
                                        DUP ;
                                        DUG 2 ;
                                        SELF ;
                                        ADDRESS ;
                                        PAIR ;
                                        SENDER ;
                                        PAIR ;
                                        DIG 4 ;
                                        DUP ;
                                        DUG 5 ;
                                        SWAP ;
                                        DUP ;
                                        CDR ;
                                        SWAP ;
                                        CAR ;
                                        SWAP ;
                                        DUP ;
                                        CDR ;
                                        SWAP ;
                                        CAR ;
                                        DIG 3 ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CAR ;
                                        CONTRACT %transfer (pair address (pair address nat)) ;
                                        IF_NONE { PUSH nat 0 ; FAILWITH } {} ;
                                        PUSH mutez 0 ;
                                        DIG 3 ;
                                        DIG 3 ;
                                        PAIR ;
                                        DIG 3 ;
                                        PAIR ;
                                        TRANSFER_TOKENS ;
                                        SWAP ;
                                        DUP ;
                                        DUG 2 ;
                                        DIG 4 ;
                                        CONTRACT unit ;
                                        IF_NONE { PUSH nat 9 ; FAILWITH } {} ;
                                        SWAP ;
                                        PUSH unit Unit ;
                                        TRANSFER_TOKENS ;
                                        DIG 4 ;
                                        DUP ;
                                        DUG 5 ;
                                        CDR ;
                                        DIG 4 ;
                                        DIG 5 ;
                                        DUP ;
                                        DUG 6 ;
                                        CAR ;
                                        ADD ;
                                        PAIR ;
                                        DUP ;
                                        CDR ;
                                        CDR ;
                                        DIG 4 ;
                                        DIG 5 ;
                                        CDR ;
                                        CAR ;
                                        SUB ;
                                        PAIR ;
                                        SWAP ;
                                        CAR ;
                                        PAIR ;
                                        NIL operation ;
                                        DIG 2 ;
                                        CONS ;
                                        DIG 2 ;
                                        CONS ;
                                        PAIR } } } } } } }
           { IF_LEFT
               { IF_LEFT
                   { DROP ;
                     SOURCE ;
                     SENDER ;
                     COMPARE ;
                     NEQ ;
                     IF { DROP ; PUSH nat 25 ; FAILWITH }
                        { PUSH mutez 0 ;
                          AMOUNT ;
                          COMPARE ;
                          GT ;
                          IF { DROP ; PUSH nat 10 ; FAILWITH }
                             { DUP ;
                               CDR ;
                               CDR ;
                               CDR ;
                               CAR ;
                               IF { DROP ; PUSH nat 33 ; FAILWITH }
                                  { SELF %updateTokenPoolInternal ;
                                    SWAP ;
                                    DUP ;
                                    DUG 2 ;
                                    CDR ;
                                    CDR ;
                                    CDR ;
                                    CDR ;
                                    CDR ;
                                    CDR ;
                                    CAR ;
                                    CONTRACT %getBalance (pair address (contract nat)) ;
                                    IF_NONE { PUSH nat 28 ; FAILWITH } {} ;
                                    PUSH mutez 0 ;
                                    DIG 2 ;
                                    SELF ;
                                    ADDRESS ;
                                    PAIR ;
                                    TRANSFER_TOKENS ;
                                    SWAP ;
                                    DUP ;
                                    DUG 2 ;
                                    CDR ;
                                    CDR ;
                                    CDR ;
                                    CDR ;
                                    PUSH bool True ;
                                    PAIR ;
                                    DIG 2 ;
                                    DUP ;
                                    DUG 3 ;
                                    CDR ;
                                    CDR ;
                                    CAR ;
                                    PAIR ;
                                    DIG 2 ;
                                    DUP ;
                                    DUG 3 ;
                                    CDR ;
                                    CAR ;
                                    PAIR ;
                                    DIG 2 ;
                                    CAR ;
                                    PAIR ;
                                    NIL operation ;
                                    DIG 2 ;
                                    CONS ;
                                    PAIR } } } }
                   { SWAP ;
                     DUP ;
                     DUG 2 ;
                     CDR ;
                     CDR ;
                     CDR ;
                     CDR ;
                     CDR ;
                     CDR ;
                     CAR ;
                     SENDER ;
                     COMPARE ;
                     NEQ ;
                     DIG 2 ;
                     DUP ;
                     DUG 3 ;
                     CDR ;
                     CDR ;
                     CDR ;
                     CAR ;
                     NOT ;
                     OR ;
                     IF { DROP 2 ; PUSH nat 29 ; FAILWITH }
                        { PUSH mutez 0 ;
                          AMOUNT ;
                          COMPARE ;
                          GT ;
                          IF { DROP 2 ; PUSH nat 10 ; FAILWITH }
                             { SWAP ;
                               CDR ;
                               SWAP ;
                               PAIR ;
                               DUP ;
                               CDR ;
                               CDR ;
                               CDR ;
                               CDR ;
                               PUSH bool False ;
                               PAIR ;
                               SWAP ;
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
                               PAIR } } } }
               { DUP ;
                 CDR ;
                 SWAP ;
                 CAR ;
                 SWAP ;
                 DUP ;
                 CDR ;
                 SWAP ;
                 CAR ;
                 DIG 3 ;
                 DUP ;
                 DUG 4 ;
                 CDR ;
                 CDR ;
                 CDR ;
                 CAR ;
                 IF { DROP 4 ; PUSH nat 2 ; FAILWITH }
                    { SWAP ;
                      NOW ;
                      COMPARE ;
                      GE ;
                      IF { DROP 3 ; PUSH nat 3 ; FAILWITH }
                         { PUSH mutez 1 ;
                           DIG 3 ;
                           DUP ;
                           DUG 4 ;
                           CDR ;
                           CAR ;
                           EDIV ;
                           IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                           CAR ;
                           AMOUNT ;
                           PUSH mutez 1 ;
                           SWAP ;
                           EDIV ;
                           IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                           CAR ;
                           PUSH nat 997 ;
                           SWAP ;
                           DUP ;
                           DUG 2 ;
                           MUL ;
                           PUSH nat 1000 ;
                           DIG 3 ;
                           MUL ;
                           ADD ;
                           DIG 4 ;
                           DUP ;
                           DUG 5 ;
                           CAR ;
                           PUSH nat 997 ;
                           DIG 3 ;
                           MUL ;
                           MUL ;
                           EDIV ;
                           IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                           CAR ;
                           DUP ;
                           DUG 2 ;
                           COMPARE ;
                           LT ;
                           IF { DROP ; PUSH nat 18 ; FAILWITH } {} ;
                           DUP ;
                           DIG 3 ;
                           DUP ;
                           DUG 4 ;
                           CAR ;
                           SUB ;
                           ISNAT ;
                           IF_NONE { PUSH nat 19 ; FAILWITH } {} ;
                           DIG 3 ;
                           DUP ;
                           DUG 4 ;
                           CDR ;
                           CDR ;
                           AMOUNT ;
                           DIG 5 ;
                           DUP ;
                           DUG 6 ;
                           CDR ;
                           CAR ;
                           ADD ;
                           PAIR ;
                           DIG 4 ;
                           CAR ;
                           PAIR ;
                           CDR ;
                           SWAP ;
                           PAIR ;
                           SWAP ;
                           DIG 2 ;
                           PAIR ;
                           SELF ;
                           ADDRESS ;
                           PAIR ;
                           SWAP ;
                           DUP ;
                           DUG 2 ;
                           SWAP ;
                           DUP ;
                           CDR ;
                           SWAP ;
                           CAR ;
                           SWAP ;
                           DUP ;
                           CDR ;
                           SWAP ;
                           CAR ;
                           DIG 3 ;
                           CDR ;
                           CDR ;
                           CDR ;
                           CDR ;
                           CDR ;
                           CDR ;
                           CAR ;
                           CONTRACT %transfer (pair address (pair address nat)) ;
                           IF_NONE { PUSH nat 0 ; FAILWITH } {} ;
                           PUSH mutez 0 ;
                           DIG 3 ;
                           DIG 3 ;
                           PAIR ;
                           DIG 3 ;
                           PAIR ;
                           TRANSFER_TOKENS ;
                           SWAP ;
                           NIL operation ;
                           DIG 2 ;
                           CONS ;
                           PAIR } } } } }
       [macro]

  rule #dexterCodeFA2
    => { DUP ;
         CDR ;
         SWAP ;
         CAR ;
         IF_LEFT
           { IF_LEFT
               { IF_LEFT
                   { IF_LEFT
                       { DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         SWAP ;
                         DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         SWAP ;
                         DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         DIG 4 ;
                         DUP ;
                         DUG 5 ;
                         CDR ;
                         CDR ;
                         CDR ;
                         CAR ;
                         IF { DROP 5 ; PUSH nat 2 ; FAILWITH }
                            { SWAP ;
                              NOW ;
                              COMPARE ;
                              GE ;
                              IF { DROP 4 ; PUSH nat 3 ; FAILWITH }
                                 { PUSH mutez 1 ;
                                   DIG 4 ;
                                   DUP ;
                                   DUG 5 ;
                                   CDR ;
                                   CAR ;
                                   EDIV ;
                                   IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                                   CAR ;
                                   AMOUNT ;
                                   PUSH mutez 1 ;
                                   SWAP ;
                                   EDIV ;
                                   IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                                   CAR ;
                                   SWAP ;
                                   DUP ;
                                   DUG 2 ;
                                   DIG 6 ;
                                   DUP ;
                                   DUG 7 ;
                                   CDR ;
                                   CDR ;
                                   CAR ;
                                   DIG 2 ;
                                   DUP ;
                                   DUG 3 ;
                                   MUL ;
                                   EDIV ;
                                   IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                                   CAR ;
                                   DIG 2 ;
                                   DIG 6 ;
                                   DUP ;
                                   DUG 7 ;
                                   CAR ;
                                   DIG 3 ;
                                   MUL ;
                                   EDIV ;
                                   IF_NONE
                                     { PUSH string "DIV by 0" ; FAILWITH }
                                     { DUP ;
                                       CDR ;
                                       SWAP ;
                                       CAR ;
                                       PUSH nat 0 ;
                                       DIG 2 ;
                                       COMPARE ;
                                       EQ ;
                                       IF {} { PUSH nat 1 ; ADD } } ;
                                   DIG 2 ;
                                   SWAP ;
                                   DUP ;
                                   DUG 2 ;
                                   COMPARE ;
                                   GT ;
                                   IF { DROP 5 ; PUSH nat 4 ; FAILWITH }
                                      { DUG 2 ;
                                        DUP ;
                                        DUG 3 ;
                                        COMPARE ;
                                        LT ;
                                        IF { DROP 4 ; PUSH nat 5 ; FAILWITH }
                                           { DIG 3 ;
                                             DUP ;
                                             DUG 4 ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             DIG 2 ;
                                             DUP ;
                                             DUG 3 ;
                                             DIG 5 ;
                                             DUP ;
                                             DUG 6 ;
                                             CDR ;
                                             CDR ;
                                             CAR ;
                                             ADD ;
                                             PAIR ;
                                             DIG 4 ;
                                             DUP ;
                                             DUG 5 ;
                                             CDR ;
                                             CAR ;
                                             PAIR ;
                                             DIG 4 ;
                                             DUP ;
                                             DUG 5 ;
                                             CAR ;
                                             PAIR ;
                                             CDR ;
                                             SWAP ;
                                             DUP ;
                                             DUG 2 ;
                                             DIG 5 ;
                                             DUP ;
                                             DUG 6 ;
                                             CAR ;
                                             ADD ;
                                             PAIR ;
                                             DUP ;
                                             CDR ;
                                             CDR ;
                                             AMOUNT ;
                                             DIG 6 ;
                                             CDR ;
                                             CAR ;
                                             ADD ;
                                             PAIR ;
                                             SWAP ;
                                             CAR ;
                                             PAIR ;
                                             SWAP ;
                                             SELF ;
                                             ADDRESS ;
                                             PAIR ;
                                             SENDER ;
                                             PAIR ;
                                             SWAP ;
                                             DUP ;
                                             DUG 2 ;
                                             SWAP ;
                                             DUP ;
                                             CDR ;
                                             SWAP ;
                                             CAR ;
                                             SWAP ;
                                             DUP ;
                                             CDR ;
                                             SWAP ;
                                             CAR ;
                                             DIG 3 ;
                                             DUP ;
                                             DUG 4 ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CAR ;
                                             CONTRACT %transfer (list (pair address (list (pair address (pair nat nat))))) ;
                                             IF_NONE { PUSH nat 0 ; FAILWITH } {} ;
                                             PUSH mutez 0 ;
                                             NIL (pair address (list (pair address (pair nat nat)))) ;
                                             NIL (pair address (pair nat nat)) ;
                                             DIG 5 ;
                                             DIG 7 ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CAR ;
                                             PAIR ;
                                             DIG 5 ;
                                             PAIR ;
                                             CONS ;
                                             DIG 4 ;
                                             PAIR ;
                                             CONS ;
                                             TRANSFER_TOKENS ;
                                             DIG 2 ;
                                             INT ;
                                             DIG 3 ;
                                             PAIR ;
                                             DIG 2 ;
                                             DUP ;
                                             DUG 3 ;
                                             SWAP ;
                                             DUP ;
                                             CDR ;
                                             SWAP ;
                                             CAR ;
                                             DIG 2 ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CONTRACT %mintOrBurn (pair (int %quantity) (address %target)) ;
                                             IF_NONE { PUSH nat 12 ; FAILWITH } {} ;
                                             PUSH mutez 0 ;
                                             DIG 2 ;
                                             DIG 3 ;
                                             PAIR ;
                                             TRANSFER_TOKENS ;
                                             DIG 2 ;
                                             NIL operation ;
                                             DIG 2 ;
                                             CONS ;
                                             DIG 2 ;
                                             CONS ;
                                             PAIR } } } } }
                       { DROP ;
                         DUP ;
                         CDR ;
                         CDR ;
                         CDR ;
                         CAR ;
                         IF { DROP ; PUSH nat 2 ; FAILWITH }
                            { DUP ;
                              CDR ;
                              CDR ;
                              AMOUNT ;
                              DIG 2 ;
                              DUP ;
                              DUG 3 ;
                              CDR ;
                              CAR ;
                              ADD ;
                              PAIR ;
                              SWAP ;
                              CAR ;
                              PAIR ;
                              NIL operation ;
                              PAIR } } }
                   { IF_LEFT
                       { DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         SWAP ;
                         DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         SWAP ;
                         DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         SWAP ;
                         DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         DIG 5 ;
                         DUP ;
                         DUG 6 ;
                         CDR ;
                         CDR ;
                         CDR ;
                         CAR ;
                         IF { DROP 6 ; PUSH nat 2 ; FAILWITH }
                            { SWAP ;
                              NOW ;
                              COMPARE ;
                              GE ;
                              IF { DROP 5 ; PUSH nat 3 ; FAILWITH }
                                 { PUSH mutez 0 ;
                                   AMOUNT ;
                                   COMPARE ;
                                   GT ;
                                   IF { DROP 5 ; PUSH nat 10 ; FAILWITH }
                                      { DIG 4 ;
                                        DUP ;
                                        DUG 5 ;
                                        CDR ;
                                        CDR ;
                                        CAR ;
                                        PUSH mutez 1 ;
                                        DIG 6 ;
                                        DUP ;
                                        DUG 7 ;
                                        CDR ;
                                        CAR ;
                                        EDIV ;
                                        IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                                        CAR ;
                                        DIG 4 ;
                                        DUP ;
                                        DUG 5 ;
                                        MUL ;
                                        EDIV ;
                                        IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                                        CAR ;
                                        PUSH mutez 1 ;
                                        SWAP ;
                                        MUL ;
                                        DIG 5 ;
                                        DUP ;
                                        DUG 6 ;
                                        CDR ;
                                        CDR ;
                                        CAR ;
                                        DIG 6 ;
                                        DUP ;
                                        DUG 7 ;
                                        CAR ;
                                        DIG 5 ;
                                        DUP ;
                                        DUG 6 ;
                                        MUL ;
                                        EDIV ;
                                        IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                                        CAR ;
                                        DIG 3 ;
                                        DIG 2 ;
                                        DUP ;
                                        DUG 3 ;
                                        COMPARE ;
                                        LT ;
                                        IF { DROP 6 ; PUSH nat 11 ; FAILWITH }
                                           { DIG 2 ;
                                             SWAP ;
                                             DUP ;
                                             DUG 2 ;
                                             COMPARE ;
                                             LT ;
                                             IF { DROP 5 ; PUSH nat 13 ; FAILWITH }
                                                { DIG 2 ;
                                                  DUP ;
                                                  DUG 3 ;
                                                  DIG 5 ;
                                                  DUP ;
                                                  DUG 6 ;
                                                  CDR ;
                                                  CDR ;
                                                  CAR ;
                                                  SUB ;
                                                  ISNAT ;
                                                  IF_NONE { PUSH nat 14 ; FAILWITH } {} ;
                                                  SWAP ;
                                                  DUP ;
                                                  DUG 2 ;
                                                  DIG 6 ;
                                                  DUP ;
                                                  DUG 7 ;
                                                  CAR ;
                                                  SUB ;
                                                  ISNAT ;
                                                  IF_NONE { PUSH nat 15 ; FAILWITH } {} ;
                                                  DIG 4 ;
                                                  PUSH int 0 ;
                                                  SUB ;
                                                  SENDER ;
                                                  PAIR ;
                                                  DIG 6 ;
                                                  DUP ;
                                                  DUG 7 ;
                                                  SWAP ;
                                                  DUP ;
                                                  CDR ;
                                                  SWAP ;
                                                  CAR ;
                                                  DIG 2 ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CONTRACT %mintOrBurn (pair (int %quantity) (address %target)) ;
                                                  IF_NONE { PUSH nat 12 ; FAILWITH } {} ;
                                                  PUSH mutez 0 ;
                                                  DIG 2 ;
                                                  DIG 3 ;
                                                  PAIR ;
                                                  TRANSFER_TOKENS ;
                                                  DIG 3 ;
                                                  DIG 5 ;
                                                  DUP ;
                                                  DUG 6 ;
                                                  PAIR ;
                                                  SELF ;
                                                  ADDRESS ;
                                                  PAIR ;
                                                  DIG 6 ;
                                                  DUP ;
                                                  DUG 7 ;
                                                  SWAP ;
                                                  DUP ;
                                                  CDR ;
                                                  SWAP ;
                                                  CAR ;
                                                  SWAP ;
                                                  DUP ;
                                                  CDR ;
                                                  SWAP ;
                                                  CAR ;
                                                  DIG 3 ;
                                                  DUP ;
                                                  DUG 4 ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CAR ;
                                                  CONTRACT %transfer (list (pair address (list (pair address (pair nat nat))))) ;
                                                  IF_NONE { PUSH nat 0 ; FAILWITH } {} ;
                                                  PUSH mutez 0 ;
                                                  NIL (pair address (list (pair address (pair nat nat)))) ;
                                                  NIL (pair address (pair nat nat)) ;
                                                  DIG 5 ;
                                                  DIG 7 ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  CAR ;
                                                  PAIR ;
                                                  DIG 5 ;
                                                  PAIR ;
                                                  CONS ;
                                                  DIG 4 ;
                                                  PAIR ;
                                                  CONS ;
                                                  TRANSFER_TOKENS ;
                                                  DIG 4 ;
                                                  DUP ;
                                                  DUG 5 ;
                                                  DIG 6 ;
                                                  CONTRACT unit ;
                                                  IF_NONE { PUSH nat 9 ; FAILWITH } {} ;
                                                  SWAP ;
                                                  PUSH unit Unit ;
                                                  TRANSFER_TOKENS ;
                                                  DIG 6 ;
                                                  DUP ;
                                                  DUG 7 ;
                                                  CDR ;
                                                  CDR ;
                                                  DIG 6 ;
                                                  DIG 7 ;
                                                  DUP ;
                                                  DUG 8 ;
                                                  CDR ;
                                                  CAR ;
                                                  SUB ;
                                                  PAIR ;
                                                  DIG 6 ;
                                                  CAR ;
                                                  PAIR ;
                                                  DUP ;
                                                  CDR ;
                                                  CDR ;
                                                  CDR ;
                                                  DIG 6 ;
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
                                                  CDR ;
                                                  DIG 4 ;
                                                  PAIR ;
                                                  NIL operation ;
                                                  DIG 2 ;
                                                  CONS ;
                                                  DIG 2 ;
                                                  CONS ;
                                                  DIG 2 ;
                                                  CONS ;
                                                  PAIR } } } } } }
                       { DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         DIG 2 ;
                         DUP ;
                         DUG 3 ;
                         CDR ;
                         CDR ;
                         CDR ;
                         CAR ;
                         IF { DROP 3 ; PUSH nat 2 ; FAILWITH }
                            { PUSH mutez 0 ;
                              AMOUNT ;
                              COMPARE ;
                              GT ;
                              IF { DROP 3 ; PUSH nat 10 ; FAILWITH }
                                 { DIG 2 ;
                                   DUP ;
                                   DUG 3 ;
                                   CDR ;
                                   CDR ;
                                   CDR ;
                                   CDR ;
                                   CDR ;
                                   CAR ;
                                   SENDER ;
                                   COMPARE ;
                                   NEQ ;
                                   IF { DROP 3 ; PUSH nat 20 ; FAILWITH }
                                      { DIG 2 ;
                                        DUP ;
                                        DUG 3 ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CAR ;
                                        IF { DROP 3 ; PUSH nat 22 ; FAILWITH }
                                           { DIG 2 ;
                                             DUP ;
                                             DUG 3 ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             DIG 2 ;
                                             PAIR ;
                                             DIG 2 ;
                                             DUP ;
                                             DUG 3 ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CAR ;
                                             PAIR ;
                                             DIG 2 ;
                                             DUP ;
                                             DUG 3 ;
                                             CDR ;
                                             CDR ;
                                             CAR ;
                                             PAIR ;
                                             DIG 2 ;
                                             DUP ;
                                             DUG 3 ;
                                             CDR ;
                                             CAR ;
                                             PAIR ;
                                             DIG 2 ;
                                             CAR ;
                                             PAIR ;
                                             NIL operation ;
                                             DIG 2 ;
                                             SET_DELEGATE ;
                                             CONS ;
                                             PAIR } } } } } } }
               { IF_LEFT
                   { IF_LEFT
                       { SWAP ;
                         DUP ;
                         DUG 2 ;
                         CDR ;
                         CDR ;
                         CDR ;
                         CAR ;
                         IF { DROP 2 ; PUSH nat 2 ; FAILWITH }
                            { PUSH mutez 0 ;
                              AMOUNT ;
                              COMPARE ;
                              GT ;
                              IF { DROP 2 ; PUSH nat 10 ; FAILWITH }
                                 { SWAP ;
                                   DUP ;
                                   DUG 2 ;
                                   CDR ;
                                   CDR ;
                                   CDR ;
                                   CDR ;
                                   CDR ;
                                   CAR ;
                                   SENDER ;
                                   COMPARE ;
                                   NEQ ;
                                   IF { DROP 2 ; PUSH nat 23 ; FAILWITH }
                                      { PUSH address "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU" ;
                                        DIG 2 ;
                                        DUP ;
                                        DUG 3 ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        COMPARE ;
                                        NEQ ;
                                        IF { DROP 2 ; PUSH nat 24 ; FAILWITH }
                                           { SWAP ;
                                             DUP ;
                                             DUG 2 ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CAR ;
                                             PAIR ;
                                             SWAP ;
                                             DUP ;
                                             DUG 2 ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CAR ;
                                             PAIR ;
                                             SWAP ;
                                             DUP ;
                                             DUG 2 ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CAR ;
                                             PAIR ;
                                             SWAP ;
                                             DUP ;
                                             DUG 2 ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CAR ;
                                             PAIR ;
                                             SWAP ;
                                             DUP ;
                                             DUG 2 ;
                                             CDR ;
                                             CDR ;
                                             CDR ;
                                             CAR ;
                                             PAIR ;
                                             SWAP ;
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
                                             PAIR } } } } }
                       { SWAP ;
                         DUP ;
                         DUG 2 ;
                         CDR ;
                         CDR ;
                         CDR ;
                         CAR ;
                         IF { DROP 2 ; PUSH nat 2 ; FAILWITH }
                            { PUSH mutez 0 ;
                              AMOUNT ;
                              COMPARE ;
                              GT ;
                              IF { DROP 2 ; PUSH nat 10 ; FAILWITH }
                                 { SWAP ;
                                   DUP ;
                                   DUG 2 ;
                                   CDR ;
                                   CDR ;
                                   CDR ;
                                   CDR ;
                                   CDR ;
                                   CAR ;
                                   SENDER ;
                                   COMPARE ;
                                   NEQ ;
                                   IF { DROP 2 ; PUSH nat 21 ; FAILWITH }
                                      { SWAP ;
                                        DUP ;
                                        DUG 2 ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        SWAP ;
                                        PAIR ;
                                        SWAP ;
                                        DUP ;
                                        DUG 2 ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CAR ;
                                        PAIR ;
                                        SWAP ;
                                        DUP ;
                                        DUG 2 ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CAR ;
                                        PAIR ;
                                        SWAP ;
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
                                        PAIR } } } } }
                   { IF_LEFT
                       { DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         SWAP ;
                         DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         SWAP ;
                         DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         SWAP ;
                         DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         DIG 4 ;
                         CONTRACT %xtzToToken
                           (pair (address %to) (pair (nat %minTokensBought) (timestamp %deadline))) ;
                         IF_NONE { PUSH nat 31 ; FAILWITH } {} ;
                         DIG 5 ;
                         DUP ;
                         DUG 6 ;
                         CDR ;
                         CDR ;
                         CDR ;
                         CAR ;
                         IF { DROP 6 ; PUSH nat 2 ; FAILWITH }
                            { PUSH mutez 0 ;
                              AMOUNT ;
                              COMPARE ;
                              GT ;
                              IF { DROP 6 ; PUSH nat 10 ; FAILWITH }
                                 { DIG 2 ;
                                   DUP ;
                                   DUG 3 ;
                                   NOW ;
                                   COMPARE ;
                                   GE ;
                                   IF { DROP 6 ; PUSH nat 3 ; FAILWITH }
                                      { PUSH nat 997 ;
                                        DIG 2 ;
                                        DUP ;
                                        DUG 3 ;
                                        MUL ;
                                        PUSH nat 1000 ;
                                        DIG 7 ;
                                        DUP ;
                                        DUG 8 ;
                                        CAR ;
                                        MUL ;
                                        ADD ;
                                        PUSH mutez 1 ;
                                        DIG 7 ;
                                        DUP ;
                                        DUG 8 ;
                                        CDR ;
                                        CAR ;
                                        EDIV ;
                                        IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                                        CAR ;
                                        PUSH nat 997 ;
                                        DIG 4 ;
                                        DUP ;
                                        DUG 5 ;
                                        MUL ;
                                        MUL ;
                                        EDIV ;
                                        IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                                        CAR ;
                                        PUSH mutez 1 ;
                                        SWAP ;
                                        MUL ;
                                        DIG 6 ;
                                        DUP ;
                                        DUG 7 ;
                                        CDR ;
                                        DIG 3 ;
                                        DUP ;
                                        DUG 4 ;
                                        DIG 8 ;
                                        DUP ;
                                        DUG 9 ;
                                        CAR ;
                                        ADD ;
                                        PAIR ;
                                        DUP ;
                                        CDR ;
                                        CDR ;
                                        DIG 2 ;
                                        DUP ;
                                        DUG 3 ;
                                        DIG 9 ;
                                        CDR ;
                                        CAR ;
                                        SUB ;
                                        PAIR ;
                                        SWAP ;
                                        CAR ;
                                        PAIR ;
                                        DIG 3 ;
                                        SELF ;
                                        ADDRESS ;
                                        PAIR ;
                                        SENDER ;
                                        PAIR ;
                                        SWAP ;
                                        DUP ;
                                        DUG 2 ;
                                        SWAP ;
                                        DUP ;
                                        CDR ;
                                        SWAP ;
                                        CAR ;
                                        SWAP ;
                                        DUP ;
                                        CDR ;
                                        SWAP ;
                                        CAR ;
                                        DIG 3 ;
                                        DUP ;
                                        DUG 4 ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CAR ;
                                        CONTRACT %transfer (list (pair address (list (pair address (pair nat nat))))) ;
                                        IF_NONE { PUSH nat 0 ; FAILWITH } {} ;
                                        PUSH mutez 0 ;
                                        NIL (pair address (list (pair address (pair nat nat)))) ;
                                        NIL (pair address (pair nat nat)) ;
                                        DIG 5 ;
                                        DIG 7 ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CAR ;
                                        PAIR ;
                                        DIG 5 ;
                                        PAIR ;
                                        CONS ;
                                        DIG 4 ;
                                        PAIR ;
                                        CONS ;
                                        TRANSFER_TOKENS ;
                                        DIG 3 ;
                                        DIG 3 ;
                                        DIG 4 ;
                                        DIG 6 ;
                                        PAIR ;
                                        DIG 5 ;
                                        PAIR ;
                                        TRANSFER_TOKENS ;
                                        DIG 2 ;
                                        NIL operation ;
                                        DIG 2 ;
                                        CONS ;
                                        DIG 2 ;
                                        CONS ;
                                        PAIR } } } }
                       { DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         SWAP ;
                         DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         SWAP ;
                         DUP ;
                         CDR ;
                         SWAP ;
                         CAR ;
                         DIG 4 ;
                         DUP ;
                         DUG 5 ;
                         CDR ;
                         CDR ;
                         CDR ;
                         CAR ;
                         IF { DROP 5 ; PUSH nat 2 ; FAILWITH }
                            { SWAP ;
                              NOW ;
                              COMPARE ;
                              GE ;
                              IF { DROP 4 ; PUSH nat 3 ; FAILWITH }
                                 { PUSH mutez 0 ;
                                   AMOUNT ;
                                   COMPARE ;
                                   GT ;
                                   IF { DROP 4 ; PUSH nat 10 ; FAILWITH }
                                      { PUSH nat 997 ;
                                        DIG 2 ;
                                        DUP ;
                                        DUG 3 ;
                                        MUL ;
                                        PUSH nat 1000 ;
                                        DIG 5 ;
                                        DUP ;
                                        DUG 6 ;
                                        CAR ;
                                        MUL ;
                                        ADD ;
                                        PUSH mutez 1 ;
                                        DIG 5 ;
                                        DUP ;
                                        DUG 6 ;
                                        CDR ;
                                        CAR ;
                                        EDIV ;
                                        IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                                        CAR ;
                                        PUSH nat 997 ;
                                        DIG 4 ;
                                        DUP ;
                                        DUG 5 ;
                                        MUL ;
                                        MUL ;
                                        EDIV ;
                                        IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                                        CAR ;
                                        PUSH mutez 1 ;
                                        SWAP ;
                                        MUL ;
                                        DUP ;
                                        DUG 2 ;
                                        COMPARE ;
                                        LT ;
                                        IF { DROP ; PUSH nat 8 ; FAILWITH } {} ;
                                        SWAP ;
                                        DUP ;
                                        DUG 2 ;
                                        SELF ;
                                        ADDRESS ;
                                        PAIR ;
                                        SENDER ;
                                        PAIR ;
                                        DIG 4 ;
                                        DUP ;
                                        DUG 5 ;
                                        SWAP ;
                                        DUP ;
                                        CDR ;
                                        SWAP ;
                                        CAR ;
                                        SWAP ;
                                        DUP ;
                                        CDR ;
                                        SWAP ;
                                        CAR ;
                                        DIG 3 ;
                                        DUP ;
                                        DUG 4 ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CAR ;
                                        CONTRACT %transfer (list (pair address (list (pair address (pair nat nat))))) ;
                                        IF_NONE { PUSH nat 0 ; FAILWITH } {} ;
                                        PUSH mutez 0 ;
                                        NIL (pair address (list (pair address (pair nat nat)))) ;
                                        NIL (pair address (pair nat nat)) ;
                                        DIG 5 ;
                                        DIG 7 ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CDR ;
                                        CAR ;
                                        PAIR ;
                                        DIG 5 ;
                                        PAIR ;
                                        CONS ;
                                        DIG 4 ;
                                        PAIR ;
                                        CONS ;
                                        TRANSFER_TOKENS ;
                                        SWAP ;
                                        DUP ;
                                        DUG 2 ;
                                        DIG 4 ;
                                        CONTRACT unit ;
                                        IF_NONE { PUSH nat 9 ; FAILWITH } {} ;
                                        SWAP ;
                                        PUSH unit Unit ;
                                        TRANSFER_TOKENS ;
                                        DIG 4 ;
                                        DUP ;
                                        DUG 5 ;
                                        CDR ;
                                        DIG 4 ;
                                        DIG 5 ;
                                        DUP ;
                                        DUG 6 ;
                                        CAR ;
                                        ADD ;
                                        PAIR ;
                                        DUP ;
                                        CDR ;
                                        CDR ;
                                        DIG 4 ;
                                        DIG 5 ;
                                        CDR ;
                                        CAR ;
                                        SUB ;
                                        PAIR ;
                                        SWAP ;
                                        CAR ;
                                        PAIR ;
                                        NIL operation ;
                                        DIG 2 ;
                                        CONS ;
                                        DIG 2 ;
                                        CONS ;
                                        PAIR } } } } } } }
           { IF_LEFT
               { IF_LEFT
                   { DROP ;
                     SOURCE ;
                     SENDER ;
                     COMPARE ;
                     NEQ ;
                     IF { DROP ; PUSH nat 25 ; FAILWITH }
                        { PUSH mutez 0 ;
                          AMOUNT ;
                          COMPARE ;
                          GT ;
                          IF { DROP ; PUSH nat 10 ; FAILWITH }
                             { DUP ;
                               CDR ;
                               CDR ;
                               CDR ;
                               CAR ;
                               IF { DROP ; PUSH nat 33 ; FAILWITH }
                                  { SELF %updateTokenPoolInternal ;
                                    SWAP ;
                                    DUP ;
                                    DUG 2 ;
                                    CDR ;
                                    CDR ;
                                    CDR ;
                                    CDR ;
                                    CDR ;
                                    CDR ;
                                    CAR ;
                                    CONTRACT %balance_of
                                      (pair (list (pair address nat)) (contract (list (pair (pair address nat) nat)))) ;
                                    IF_NONE { PUSH nat 28 ; FAILWITH } {} ;
                                    PUSH mutez 0 ;
                                    DIG 2 ;
                                    NIL (pair address nat) ;
                                    DIG 4 ;
                                    DUP ;
                                    DUG 5 ;
                                    CDR ;
                                    CDR ;
                                    CDR ;
                                    CDR ;
                                    CDR ;
                                    CDR ;
                                    CDR ;
                                    CAR ;
                                    SELF ;
                                    ADDRESS ;
                                    PAIR ;
                                    CONS ;
                                    PAIR ;
                                    TRANSFER_TOKENS ;
                                    SWAP ;
                                    DUP ;
                                    DUG 2 ;
                                    CDR ;
                                    CDR ;
                                    CDR ;
                                    CDR ;
                                    PUSH bool True ;
                                    PAIR ;
                                    DIG 2 ;
                                    DUP ;
                                    DUG 3 ;
                                    CDR ;
                                    CDR ;
                                    CAR ;
                                    PAIR ;
                                    DIG 2 ;
                                    DUP ;
                                    DUG 3 ;
                                    CDR ;
                                    CAR ;
                                    PAIR ;
                                    DIG 2 ;
                                    CAR ;
                                    PAIR ;
                                    NIL operation ;
                                    DIG 2 ;
                                    CONS ;
                                    PAIR } } } }
                   { SWAP ;
                     DUP ;
                     DUG 2 ;
                     CDR ;
                     CDR ;
                     CDR ;
                     CDR ;
                     CDR ;
                     CDR ;
                     CAR ;
                     SENDER ;
                     COMPARE ;
                     NEQ ;
                     DIG 2 ;
                     DUP ;
                     DUG 3 ;
                     CDR ;
                     CDR ;
                     CDR ;
                     CAR ;
                     NOT ;
                     OR ;
                     IF { DROP 2 ; PUSH nat 29 ; FAILWITH }
                        { PUSH mutez 0 ;
                          AMOUNT ;
                          COMPARE ;
                          GT ;
                          IF { DROP 2 ; PUSH nat 10 ; FAILWITH }
                             { IF_CONS { SWAP ; DROP ; CDR } { PUSH nat 32 ; FAILWITH } ;
                               SWAP ;
                               CDR ;
                               SWAP ;
                               PAIR ;
                               DUP ;
                               CDR ;
                               CDR ;
                               CDR ;
                               CDR ;
                               PUSH bool False ;
                               PAIR ;
                               SWAP ;
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
                               PAIR } } } }
               { DUP ;
                 CDR ;
                 SWAP ;
                 CAR ;
                 SWAP ;
                 DUP ;
                 CDR ;
                 SWAP ;
                 CAR ;
                 DIG 3 ;
                 DUP ;
                 DUG 4 ;
                 CDR ;
                 CDR ;
                 CDR ;
                 CAR ;
                 IF { DROP 4 ; PUSH nat 2 ; FAILWITH }
                    { SWAP ;
                      NOW ;
                      COMPARE ;
                      GE ;
                      IF { DROP 3 ; PUSH nat 3 ; FAILWITH }
                         { PUSH mutez 1 ;
                           DIG 3 ;
                           DUP ;
                           DUG 4 ;
                           CDR ;
                           CAR ;
                           EDIV ;
                           IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                           CAR ;
                           AMOUNT ;
                           PUSH mutez 1 ;
                           SWAP ;
                           EDIV ;
                           IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                           CAR ;
                           PUSH nat 997 ;
                           SWAP ;
                           DUP ;
                           DUG 2 ;
                           MUL ;
                           PUSH nat 1000 ;
                           DIG 3 ;
                           MUL ;
                           ADD ;
                           DIG 4 ;
                           DUP ;
                           DUG 5 ;
                           CAR ;
                           PUSH nat 997 ;
                           DIG 3 ;
                           MUL ;
                           MUL ;
                           EDIV ;
                           IF_NONE { PUSH string "DIV by 0" ; FAILWITH } {} ;
                           CAR ;
                           DUP ;
                           DUG 2 ;
                           COMPARE ;
                           LT ;
                           IF { DROP ; PUSH nat 18 ; FAILWITH } {} ;
                           DUP ;
                           DIG 3 ;
                           DUP ;
                           DUG 4 ;
                           CAR ;
                           SUB ;
                           ISNAT ;
                           IF_NONE { PUSH nat 19 ; FAILWITH } {} ;
                           DIG 3 ;
                           DUP ;
                           DUG 4 ;
                           CDR ;
                           CDR ;
                           AMOUNT ;
                           DIG 5 ;
                           DUP ;
                           DUG 6 ;
                           CDR ;
                           CAR ;
                           ADD ;
                           PAIR ;
                           DIG 4 ;
                           CAR ;
                           PAIR ;
                           CDR ;
                           SWAP ;
                           PAIR ;
                           SWAP ;
                           DIG 2 ;
                           PAIR ;
                           SELF ;
                           ADDRESS ;
                           PAIR ;
                           SWAP ;
                           DUP ;
                           DUG 2 ;
                           SWAP ;
                           DUP ;
                           CDR ;
                           SWAP ;
                           CAR ;
                           SWAP ;
                           DUP ;
                           CDR ;
                           SWAP ;
                           CAR ;
                           DIG 3 ;
                           DUP ;
                           DUG 4 ;
                           CDR ;
                           CDR ;
                           CDR ;
                           CDR ;
                           CDR ;
                           CDR ;
                           CAR ;
                           CONTRACT %transfer (list (pair address (list (pair address (pair nat nat))))) ;
                           IF_NONE { PUSH nat 0 ; FAILWITH } {} ;
                           PUSH mutez 0 ;
                           NIL (pair address (list (pair address (pair nat nat)))) ;
                           NIL (pair address (pair nat nat)) ;
                           DIG 5 ;
                           DIG 7 ;
                           CDR ;
                           CDR ;
                           CDR ;
                           CDR ;
                           CDR ;
                           CDR ;
                           CDR ;
                           CAR ;
                           PAIR ;
                           DIG 5 ;
                           PAIR ;
                           CONS ;
                           DIG 4 ;
                           PAIR ;
                           CONS ;
                           TRANSFER_TOKENS ;
                           SWAP ;
                           NIL operation ;
                           DIG 2 ;
                           CONS ;
                           PAIR } } } } }
       [macro]
```

# Translation

To support the verbatim representation, we need to add some syntactic sugar.
The parsed internal representation of Michelson contains nodes like explicit empty lists, e.g. `.AnnotationList`, and tokens to be wrapped using the `#token(...)` production.
Here we implement a simplified parser that introduces what is necessary to paste the contract code directly.
All the rules are macros, and they insert the `.AnnotationList` and `#token(...)` elements where necessary.

# Instructions

## Pretty Nullary Instructions

The macro processor has problems inferring the type of nullary instructions.
We get around this problem by explicitly declaring every `NullaryInstName` an `Instruction`, and giving each a separate macro.

```k
  syntax Instruction ::= "DROP"
                       | "DUP"
                       | "SWAP"
                       | "SOME"
                       | "UNIT"
                       | "PAIR"
                       | "UNPAIR"
                       | "CAR"
                       | "CDR"
                       | "CONS"
                       | "SIZE"
                       | "MEM"
                       | "GET"
                       | "UPDATE"
                       | "EXEC"
                       | "APPLY"
                       | "FAILWITH"
                       | "CAST"
                       | "RENAME"
                       | "CONCAT"
                       | "SLICE"
                       | "PACK"
                       | "ADD"
                       | "SUB"
                       | "MUL"
                       | "EDIV"
                       | "ABS"
                       | "ISNAT"
                       | "INT"
                       | "NEG"
                       | "LSL"
                       | "LSR"
                       | "OR"
                       | "AND"
                       | "XOR"
                       | "NOT"
                       | "COMPARE"
                       | "EQ"
                       | "NEQ"
                       | "LT"
                       | "GT"
                       | "LE"
                       | "GE"
                       | "SELF"
                       | "TRANSFER_TOKENS"
                       | "SET_DELEGATE"
                       | "CREATE_ACCOUNT"
                       | "IMPLICIT_ACCOUNT"
                       | "NOW"
                       | "CHAIN_ID"
                       | "AMOUNT"
                       | "BALANCE"
                       | "CHECK_SIGNATURE"
                       | "BLAKE2B"
                       | "SHA256"
                       | "SHA512"
                       | "HASH_KEY"
                       | "STEPS_TO_QUOTA"
                       | "SOURCE"
                       | "SENDER"
                       | "ADDRESS"
                       | "SWAP"

  rule DROP             => DROP             .AnnotationList [macro]
  rule DUP              => DUP              .AnnotationList [macro]
  rule SWAP             => SWAP             .AnnotationList [macro]
  rule SOME             => SOME             .AnnotationList [macro]
  rule UNIT             => UNIT             .AnnotationList [macro]
  rule PAIR             => PAIR             .AnnotationList [macro]
  rule UNPAIR           => UNPAIR           .AnnotationList [macro]
  rule CAR              => CAR              .AnnotationList [macro]
  rule CDR              => CDR              .AnnotationList [macro]
  rule CONS             => CONS             .AnnotationList [macro]
  rule SIZE             => SIZE             .AnnotationList [macro]
  rule MEM              => MEM              .AnnotationList [macro]
  rule GET              => GET              .AnnotationList [macro]
  rule UPDATE           => UPDATE           .AnnotationList [macro]
  rule EXEC             => EXEC             .AnnotationList [macro]
  rule APPLY            => APPLY            .AnnotationList [macro]
  rule FAILWITH         => FAILWITH         .AnnotationList [macro]
  rule CAST             => CAST             .AnnotationList [macro]
  rule RENAME           => RENAME           .AnnotationList [macro]
  rule CONCAT           => CONCAT           .AnnotationList [macro]
  rule SLICE            => SLICE            .AnnotationList [macro]
  rule PACK             => PACK             .AnnotationList [macro]
  rule ADD              => ADD              .AnnotationList [macro]
  rule SUB              => SUB              .AnnotationList [macro]
  rule MUL              => MUL              .AnnotationList [macro]
  rule EDIV             => EDIV             .AnnotationList [macro]
  rule ABS              => ABS              .AnnotationList [macro]
  rule ISNAT            => ISNAT            .AnnotationList [macro]
  rule INT              => INT              .AnnotationList [macro]
  rule NEG              => NEG              .AnnotationList [macro]
  rule LSL              => LSL              .AnnotationList [macro]
  rule LSR              => LSR              .AnnotationList [macro]
  rule OR               => OR               .AnnotationList [macro]
  rule AND              => AND              .AnnotationList [macro]
  rule XOR              => XOR              .AnnotationList [macro]
  rule NOT              => NOT              .AnnotationList [macro]
  rule COMPARE          => COMPARE          .AnnotationList [macro]
  rule EQ               => EQ               .AnnotationList [macro]
  rule NEQ              => NEQ              .AnnotationList [macro]
  rule LT               => LT               .AnnotationList [macro]
  rule GT               => GT               .AnnotationList [macro]
  rule LE               => LE               .AnnotationList [macro]
  rule GE               => GE               .AnnotationList [macro]
  rule SELF             => SELF             .AnnotationList [macro]
  rule TRANSFER_TOKENS  => TRANSFER_TOKENS  .AnnotationList [macro]
  rule SET_DELEGATE     => SET_DELEGATE     .AnnotationList [macro]
  rule CREATE_ACCOUNT   => CREATE_ACCOUNT   .AnnotationList [macro]
  rule IMPLICIT_ACCOUNT => IMPLICIT_ACCOUNT .AnnotationList [macro]
  rule NOW              => NOW              .AnnotationList [macro]
  rule CHAIN_ID         => CHAIN_ID         .AnnotationList [macro]
  rule AMOUNT           => AMOUNT           .AnnotationList [macro]
  rule BALANCE          => BALANCE          .AnnotationList [macro]
  rule CHECK_SIGNATURE  => CHECK_SIGNATURE  .AnnotationList [macro]
  rule BLAKE2B          => BLAKE2B          .AnnotationList [macro]
  rule SHA256           => SHA256           .AnnotationList [macro]
  rule SHA512           => SHA512           .AnnotationList [macro]
  rule HASH_KEY         => HASH_KEY         .AnnotationList [macro]
  rule STEPS_TO_QUOTA   => STEPS_TO_QUOTA   .AnnotationList [macro]
  rule SOURCE           => SOURCE           .AnnotationList [macro]
  rule SENDER           => SENDER           .AnnotationList [macro]
  rule ADDRESS          => ADDRESS          .AnnotationList [macro]
  rule SWAP             => SWAP             .AnnotationList [macro]
```

## Other Pretty Instructions

```k
  syntax Instruction ::= UnaryIntInstName Int
                       | UnaryTypeInstName Type
                       | UnaryBlockInstName Block
                       | BinaryTypeInstName Type Type
                       | BinaryBlockInstName Block Block

  rule I:UnaryIntInstName    P:Int             => I .AnnotationList P     [macro]
  rule I:UnaryTypeInstName   P:Type            => I .AnnotationList P     [macro]
  rule I:UnaryBlockInstName  P:Block           => I .AnnotationList P     [macro]
  rule I:BinaryTypeInstName  P1:Type  P2:Type  => I .AnnotationList P1 P2 [macro]
  rule I:BinaryBlockInstName P1:Block P2:Block => I .AnnotationList P1 P2 [macro]

  syntax SpecialInstruction ::= "PUSH" Type Data
                              | "LAMBDA" Type Type Block
                              | "CREATE_CONTRACT" "{" Contract "}"
                              | "DIP" Int Block

  rule PUSH T:Type D:Data => PUSH .AnnotationList T D [macro]
  rule LAMBDA T1:Type T2:Type B:Block => LAMBDA .AnnotationList T1 T2 B [macro]
  rule CREATE_CONTRACT { C } => CREATE_CONTRACT .AnnotationList { C } [macro]
  rule DIP I:Int B:Block => DIP .AnnotationList I B [macro]
```

# Types

## Pretty Nullary Types

The macro processor has problems inferring the type of nullary types.
We get around this problem by explicitly declaring every `NullaryTypeName` a `Type`, and giving each a separate macro.

```k
  syntax Type ::= "int"
                | "nat"
                | "string"
                | "bytes"
                | "mutez"
                | "bool"
                | "key_hash"
                | "timestamp"
                | "address"
                | "key"
                | "unit"
                | "signature"
                | "operation"
                | "chain_id"

  rule int          => int        .AnnotationList [macro]
  rule nat          => nat        .AnnotationList [macro]
  rule string       => string     .AnnotationList [macro]
  rule bytes        => bytes      .AnnotationList [macro]
  rule mutez        => mutez      .AnnotationList [macro]
  rule bool         => bool       .AnnotationList [macro]
  rule key_hash     => key_hash   .AnnotationList [macro]
  rule timestamp    => timestamp  .AnnotationList [macro]
  rule address      => address    .AnnotationList [macro]
  rule key          => key        .AnnotationList [macro]
  rule unit         => unit       .AnnotationList [macro]
  rule signature    => signature  .AnnotationList [macro]
  rule operation    => operation  .AnnotationList [macro]
  rule chain_id     => chain_id   .AnnotationList [macro]
```

## Other Pretty Types

```k
  syntax Type ::= UnaryTypeName   Type
                | BinaryTypeName  Type Type

  rule N:UnaryTypeName  T     => N .AnnotationList T     [macro]
  rule N:BinaryTypeName T1 T2 => N .AnnotationList T1 T2 [macro]
```

# Annotations

Annotations are arbitrary tokens.
The set of permissible annotations is infinite.
Since we work with specific code that contains a finite number of annotations, we can represent each and give it a macro for wrapping it in the `#token(...)` production.

```k
  syntax Annotation ::= "%balance_of"
                      | "%deadline"
                      | "%getBalance"
                      | "%minTokensBought"
                      | "%mintOrBurn"
                      | "%quantity"
                      | "%target"
                      | "%to"
                      | "%transfer"
                      | "%updateTokenPoolInternal"
                      | "%xtzToToken"

  rule %balance_of              => #token("%balance_of"             , "FieldAnnotation") [macro]
  rule %deadline                => #token("%deadline"               , "FieldAnnotation") [macro]
  rule %getBalance              => #token("%getBalance"             , "FieldAnnotation") [macro]
  rule %minTokensBought         => #token("%minTokensBought"        , "FieldAnnotation") [macro]
  rule %mintOrBurn              => #token("%mintOrBurn"             , "FieldAnnotation") [macro]
  rule %quantity                => #token("%quantity"               , "FieldAnnotation") [macro]
  rule %target                  => #token("%target"                 , "FieldAnnotation") [macro]
  rule %to                      => #token("%to"                     , "FieldAnnotation") [macro]
  rule %transfer                => #token("%transfer"               , "FieldAnnotation") [macro]
  rule %updateTokenPoolInternal => #token("%updateTokenPoolInternal", "FieldAnnotation") [macro]
  rule %xtzToToken              => #token("%xtzToToken"             , "FieldAnnotation") [macro]
```

# Literals

The boolean literals from Michelson needs to be wrapped in `#token`, since they are not part of the usual internal syntax.

```k
  syntax MichelsonBool ::= "True" | "False"
  rule True  => #token("True" , "MichelsonBoolToken") [macro]
  rule False => #token("False", "MichelsonBoolToken") [macro]
```

```k
endmodule
```
