# Translation

To support the verbatim representation, we need to add some syntactic sugar.
The parsed internal representation of Michelson contains nodes like explicit empty lists, e.g. `.AnnotationList`, and tokens to be wrapped using the `#token(...)` production.
Here we implement a simplified parser that introduces what is necessary to paste the contract code directly.
All the rules are macros, and they insert the `.AnnotationList` and `#token(...)` elements where necessary.

## Instructions

### Pretty Nullary Instructions

The macro processor has problems inferring the type of nullary instructions.
We get around this problem by explicitly declaring every `NullaryInstName` an `Instruction`, and giving each a separate macro.

```k
require "syntax.md"

module MICHELSON-PRETTY
  
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

### Other Pretty Instructions

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

## Types

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

## Literals

The boolean literals from Michelson needs to be wrapped in `#token`, since they are not part of the usual internal syntax.

```k
  syntax MichelsonBool ::= "True" | "False"
  rule True  => #token("True" , "MichelsonBoolToken") [macro]
  rule False => #token("False", "MichelsonBoolToken") [macro]
```

```k
endmodule
```
