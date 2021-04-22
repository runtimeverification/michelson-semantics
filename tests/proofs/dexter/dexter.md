# Dexter Verification

## Prologue

Our verification subject is the Michelson code corresponding to the LIGO [Dexter 2 contract](https://gitlab.com/dexter2tz/dexter2tz) at [commit id 8a5792a5](https://gitlab.com/dexter2tz/dexter2tz/-/tree/8a5792a56e0143042926c3ca8bff7d7068a541c3).

The goal of this project is to produce:

-   a series of proofs which specify that the intended behavior of each individual LIGO function is correct (which implies that the LIGO-to-Michelson compilation process is also correct in this case)
-   a series of proofs which demonstate high-level invariants over sequences of contract calls hold (e.g. it is not possible to produce a profit by exploiting rounding errors)

In this project, we will model the entrypoints in Dexter contract code and extract their high-level properties.
Note that we will base our high-level descriptions of each function of the LIGO contract code, while our verification will be performed at the the level of the compiled Michelson versions.

We begin start our verification project by opening a new module context in which our verification will be performed.

```k
requires "../lemmas.md"
module DEXTER-VERIFICATION-SYNTAX
  imports MICHELSON-INTERNAL-SYNTAX
```

# Instructions

## Pretty Nullary Instructions

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
                | UnaryTypeName   Type
                | BinaryTypeName  Type Type

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

  rule N:UnaryTypeName  T     => N .AnnotationList T     [macro]
  rule N:BinaryTypeName T1 T2 => N .AnnotationList T1 T2 [macro]

  syntax MichelsonBool ::= "True" | "False"

  rule True  => #token("True" , "MichelsonBoolToken") [macro]
  rule False => #token("False", "MichelsonBoolToken") [macro]
```

## Annotations

```k
  syntax Annotation ::= "%deadline"
                      | "%getBalance"
                      | "%minTokensBought"
                      | "%mintOrBurn"
                      | "%quantity"
                      | "%target"
                      | "%to"
                      | "%transfer"
                      | "%updateTokenPoolInternal"
                      | "%xtzToToken"

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

## The Dexter Smart Contract Code

```k
  syntax Data ::= "#dexterCode"

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
```

```k
endmodule
```

```k
module DEXTER-VERIFICATION
  imports DEXTER-VERIFICATION-SYNTAX
  imports LEMMAS
```

## Terminology Prerequisites

### Entrypoints

Note that we are slightly abusing language in the preceding sentence because Michelson has no native notion of separate contract function entrypoints.
Instead, different entrypoints are modelled by making the global contract input parameter a disjoint union of the parameter types for each possible entrypoint, and then dispatching to different code blocks based on which entry of the disjoint union was selected.
Due to Michelon's design, this means that each entrypoint can be understood to have the following typing:

```
entrypoint_input_type * storage_type -> (operation list) * storage_type
```

where:

1.  each `entrypoint_input_type` is a member of the disjoint union which is the global contract parameter type;
2.  each `operation` is a callback to be placed in a FIFO queue of callbacks that are executed once the current contract execution terminates.

By abuse of notation, we identify an entrypoint's typing rule with its corresponding `entrypoint_input_type`, since the other parts of the typing rule are constant.
We may abbreviate `entrypoint_input_type` to just `input` when it is clear from context.

### Storage Type

In our proofs, we will use the following abstract representation of the Dexter contract storage state.
For simplicity, we assume the that the `tokenId` field is always present, though the verification of the FA12 version of the contract will not touch this field.

```k
configuration <dexterTop>
                <michelsonTop/>
                <storage>
                  <tokenPool>               0                                                </tokenPool>
                  <xtzPool>                 #Mutez(0)                                        </xtzPool>
                  <selfIsUpdatingTokenPool> false                                            </selfIsUpdatingTokenPool>
                  <freezeBaker>             false                                            </freezeBaker>
                  <manager>                 #Address("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU") </manager>
                  <lqtTotal>                0                                                </lqtTotal>
                  <tokenAddress>            #Address("")                                     </tokenAddress>
                  <lqtAddress>              #Address("")                                     </lqtAddress>
                  <tokenId>                 0                                                </tokenId>
                </storage>
              </dexterTop>
```

## Entrypoint Summaries

We define our list of entrypoints below.
Each entrypoint is given a unique abstract parameter type that we use to simplify our proof structure.

```k
  syntax EntryPointParams
```

1.  [dexter.mligo.tz](https://gitlab.com/dexter2tz/dexter2tz/-/blob/8a5792a56e0143042926c3ca8bff7d7068a541c3/dexter.mligo.tz)

    1.  `add_liquidity`: Allows callers to "mint" liquidity in excahnge for tez.
        Caller gains liquidity equal to `tez * storage.lqtTotal / storage.xtzPool`.

        -   Input:

            ```
            type add_liquidity =
            { owner : address ;
              minLqtMinted : nat ;
              maxTokensDeposited : nat ;
              deadline : timestamp ;
            }
            ```

        -   Storage updates:

            ```
            Storage( lqtTotal:  LqtTotal  => LqtTotal  + lqt_minted ;
                     tokenPool: TokenPool => TokenPool + tokens_deposited ;
                     xtzPool:   XtzPool   => XtzPool   + Tezos.amount
                   )
            ```

        -   Operations:

            1. self call to `transfer` entrypoint: Send tokens from sender to self.
            2. self call to `mintOrBurn` entrypoint: Adds liquidity for the sender.

        -   Preconditions

            1.  the token pool _is_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
            2.  the deadline has not passed (i.e. the `Tezos.now >= input.deadline`)
            3.  the tez transferred is less than `input.maxTokensDeposited`
            4.  the liquidity minted is more than `input.minLqtMinted`

    2.  `remove_liquidity`

    3.  `set_baker`

        -   Input:

            ```
            type set_baker =
              { baker : key_hash option ;
                freezeBaker : bool ;
              }
            ```

        -   Output:

            ```
            ( [ set_delegate(baker) ], { storage with freezeBaker = freezeBaker } )
            ```

        -   Summary: The contract sets its delegate to the value of `baker` (and optionally freezes the baker to that particular value) if the following conditions are satisfied:

            1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
            2.  exactly 0 tez was transferred to this contract when it was invoked
            3.  the txn sender is the `storage.manager`
            4.  the baker is _not_ already frozen

    4.  `set_manager`

        -   Input:

            ```
            type set_manager = address // named new_manager
            ```

        -   Output:

            ```
            ( [], { storage with manager = new_manager } )
            ```

        -   Summary: The contract sets its manager to the provided manager address if the following conditions are satisfied:

            1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
            2.  exactly 0 tez was transferred to this contract when it was invoked
            3.  the txn sender is the `storage.manager`

    5.  `set_lqt_address`

        -   Input:

            ```
            type set_lqt_address = address // named lqtAddress
            ```

        -   Output:

            ```
            ( [], { storage with lqtAddress = lqtAddress } )
            ```

        -   Summary: The contract sets its liquidity pool adddress to the provided address if the following conditions are satisifed:

            1.  the token pool is _not_ currently updating (i.e. `storage.selfIsUpdatingTokenPool = false`)
            2.  exactly 0 tez was transferred to this contract when it was invoked
            3.  the txn sender is the `storage.manager`
            4.  the liquidity pool address has already been set (i.e. `storage.lqtAddress1 != tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU`)

    6.  `default_`
    7.  `update_token_pool`
    8.  `xtz_to_token`
    9.  `token_to_xtz`
    10. `token_to_token`

2.  [lqt_fa12.mligo.tz](https://gitlab.com/dexter2tz/dexter2tz/-/blob/8a5792a56e0143042926c3ca8bff7d7068a541c3/lqt_fa12.mligo.tz)

    Note that, in this case, we do not need to verify this implementation in particular; it is sufficient that we can model the behavior of an arbitrary contract which conforms to the FA1.2 standard.
    Such a contract will have the following entry points:

    1.  `transfer`
    2.  `approve`
    3.  `mintOrBurn`
    4.  `getAllowance`
    5.  `getBalance`
    6.  `getTotalSupply`

As reference materials for understanding the contract intent, we will consult:

1.  The [Dexter 2 origination script](https://gitlab.com/dexter2tz/dexter2tz/-/blob/master/origination.sh)
2.  The [LIGO documentation](https://ligolang.org/docs/intro/introduction)
3.  The [Michelson documentation](http://tezos.gitlab.io/008/michelson.html)
4.  The [FA 1.2 standard](https://gitlab.com/tzip/tzip/blob/master/proposals/tzip-7/tzip-7.md)
5.  The [FA 2 standard](https://gitlab.com/tzip/tzip/-/blob/master/proposals/tzip-12/tzip-12.md)

## State Abstraction

We use a few helper routines to convert between our abstract and concrete proof state.
We first define functions which build our parameter and our storage types.

```k
  syntax TypeName ::= #DexterParamType(Bool)                [function, functional]
                    | #DexterVersionSpecificParamType(Bool) [function, functional]
  // -----------------------------------------------------------------------------
  rule #DexterParamType(IsFA2)
    => (or
          (or
             (or
                (or (pair address                        // addLiquidity
                       pair nat
                         pair nat timestamp)
                    unit)                                // default
                (or (pair address                        // removeLiquidity
                       pair nat
                         pair mutez
                           pair nat timestamp)
                    (pair option key_hash bool)))        // setBaker
             (or
                (or address                              // setLqtAddress
                    address)                             // setManager
                (or (pair address                        // tokenToToken
                       pair nat
                         pair address
                           pair nat timestamp)
                    (pair address                        // tokenToXtz
                       pair nat
                         pair mutez timestamp))))
          (or
             (or unit                                    // updateTokenPool
                 #DexterVersionSpecificParamType(IsFA2)) // updateTokenPoolInternal
             (pair address                               // xtzToToken
                pair nat timestamp)))

  rule #DexterVersionSpecificParamType(true)  => list pair pair address nat nat
  rule #DexterVersionSpecificParamType(false) => nat

  syntax TypeName ::= #DexterStorageType(Bool)                [function, functional]
                    | #DexterVersionSpecificStorageType(Bool) [function, functional]
  // -------------------------------------------------------------------------------
  rule #DexterStorageType(IsFA2)
    => pair nat
         pair mutez
           pair nat
             pair bool
               pair bool
                 pair address
                   pair address
                     #DexterVersionSpecificStorageType(IsFA2)

  rule #DexterVersionSpecificStorageType(true)  => pair nat address
  rule #DexterVersionSpecificStorageType(false) => address
```

We also define a functions that serialize and deserialize our abstract parameters and state.

```k
  syntax Data ::= #LoadDexterParams(EntryPointParams) [function, functional, no-evaluators]
  // --------------------------------------------------------------------------------------
  // FIXME

  syntax KItem ::= #loadDexterState(Bool, EntryPointParams)
  // ------------------------------------------------------
  rule <k> #loadDexterState(IsFA2, Params) => . ... </k>
       <stack> .Stack
            => [ pair #DexterParamType(IsFA2) #DexterStorageType(IsFA2)
                 Pair #LoadDexterParams(Params)
                   Pair TokenPool
                     Pair XTZPool
                       Pair LQTTotal
                         Pair IsUpdatingTokenPool
                           Pair IsBakerFrozen
                             Pair Manager
                               Pair TokenAddress
                                 #if IsFA2
                                   #then Pair TokenId LQTAddress
                                   #else LQTAddress
                                 #fi ]
       </stack>
       <tokenPool>               TokenPool           </tokenPool>
       <xtzPool>                 XTZPool             </xtzPool>
       <lqtTotal>                LQTTotal            </lqtTotal>
       <selfIsUpdatingTokenPool> IsUpdatingTokenPool </selfIsUpdatingTokenPool>
       <freezeBaker>             IsBakerFrozen       </freezeBaker>
       <manager>                 Manager             </manager>
       <tokenAddress>            TokenAddress        </tokenAddress>
       <tokenId>                 TokenId             </tokenId>
       <lqtAddress>              LQTAddress          </lqtAddress>

  syntax KItem ::= #storeDexterState(Bool)
                 | #storeDexterState(Bool, Data)
  // -------------------------------------------
  rule <k> #storeDexterState(IsFA2) => #storeDexterState(IsFA2, VersionSpecificData) ... </k>
       <stack> [ pair list operation StorageType:TypeName
                 Pair _OpList
                   Pair TokenPool
                     Pair XTZPool
                       Pair LQTTotal
                         Pair IsUpdatingTokenPool
                           Pair IsBakerFrozen
                             Pair Manager
                               Pair TokenContract
                                 VersionSpecificData ]
       </stack>
       <tokenPool>               _ => TokenPool           </tokenPool>
       <xtzPool>                 _ => XTZPool             </xtzPool>
       <lqtTotal>                _ => LQTTotal            </lqtTotal>
       <selfIsUpdatingTokenPool> _ => IsUpdatingTokenPool </selfIsUpdatingTokenPool>
       <freezeBaker>             _ => IsBakerFrozen       </freezeBaker>
       <manager>                 _ => Manager             </manager>
       <tokenAddress>            _ => TokenContract       </tokenAddress>
    requires StorageType ==K #DexterStorageType(IsFA2)

  rule <k> #storeDexterState(IsFA2, Pair TokenId LQTContract) => .K ... </k>
       <tokenId>    _ => TokenId     </tokenId>
       <lqtAddress> _ => LQTContract </lqtAddress>
    requires IsFA2

  rule <k> #storeDexterState(IsFA2, LQTContract) => .K ... </k>
       <lqtAddress> _ => LQTContract </lqtAddress>
    requires notBool IsFA2
```

## Epilogue

We close out our module context now, which contains all of the information necessary to complete our proof.

```k
endmodule
```
