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
requires "syntax.md"

module MICHELSON-PRETTY-SYNTAX
  imports MICHELSON-INTERNAL-SYNTAX
  imports BOOL-SYNTAX

  syntax Instruction ::= "DROP" [macro]
                       | "DUP" [macro]
                       | "SWAP" [macro]
                       | "SOME" [macro]
                       | "UNIT" [macro]
                       | "PAIR" [macro]
                       | "UNPAIR" [macro]
                       | "CAR" [macro]
                       | "CDR" [macro]
                       | "CONS" [macro]
                       | "SIZE" [macro]
                       | "MEM" [macro]
                       | "GET" [macro]
                       | "UPDATE" [macro]
                       | "EXEC" [macro]
                       | "APPLY" [macro]
                       | "FAILWITH" [macro]
                       | "CAST" [macro]
                       | "RENAME" [macro]
                       | "CONCAT" [macro]
                       | "SLICE" [macro]
                       | "PACK" [macro]
                       | "ADD" [macro]
                       | "SUB" [macro]
                       | "MUL" [macro]
                       | "EDIV" [macro]
                       | "ABS" [macro]
                       | "ISNAT" [macro]
                       | "INT" [macro]
                       | "NEG" [macro]
                       | "LSL" [macro]
                       | "LSR" [macro]
                       | "OR" [macro]
                       | "AND" [macro]
                       | "XOR" [macro]
                       | "NOT" [macro]
                       | "COMPARE" [macro]
                       | "EQ" [macro]
                       | "NEQ" [macro]
                       | "LT" [macro]
                       | "GT" [macro]
                       | "LE" [macro]
                       | "GE" [macro]
                       | "SELF" [macro]
                       | "TRANSFER_TOKENS" [macro]
                       | "SET_DELEGATE" [macro]
                       | "CREATE_ACCOUNT" [macro]
                       | "IMPLICIT_ACCOUNT" [macro]
                       | "NOW" [macro]
                       | "CHAIN_ID" [macro]
                       | "AMOUNT" [macro]
                       | "BALANCE" [macro]
                       | "CHECK_SIGNATURE" [macro]
                       | "BLAKE2B" [macro]
                       | "SHA256" [macro]
                       | "SHA512" [macro]
                       | "HASH_KEY" [macro]
                       | "STEPS_TO_QUOTA" [macro]
                       | "SOURCE" [macro]
                       | "SENDER" [macro]
                       | "ADDRESS" [macro]
                       | "SWAP" [macro]

  rule (DROP):Instruction             => DROP             .AnnotationList
  rule (DUP):Instruction              => DUP              .AnnotationList
  rule (SWAP):Instruction             => SWAP             .AnnotationList
  rule (SOME):Instruction             => SOME             .AnnotationList
  rule (UNIT):Instruction             => UNIT             .AnnotationList
  rule (PAIR):Instruction             => PAIR             .AnnotationList
  rule (UNPAIR):Instruction           => UNPAIR           .AnnotationList
  rule (CAR):Instruction              => CAR              .AnnotationList
  rule (CDR):Instruction              => CDR              .AnnotationList
  rule (CONS):Instruction             => CONS             .AnnotationList
  rule (SIZE):Instruction             => SIZE             .AnnotationList
  rule (MEM):Instruction              => MEM              .AnnotationList
  rule (GET):Instruction              => GET              .AnnotationList
  rule (UPDATE):Instruction           => UPDATE           .AnnotationList
  rule (EXEC):Instruction             => EXEC             .AnnotationList
  rule (APPLY):Instruction            => APPLY            .AnnotationList
  rule (FAILWITH):Instruction         => FAILWITH         .AnnotationList
  rule (CAST):Instruction             => CAST             .AnnotationList
  rule (RENAME):Instruction           => RENAME           .AnnotationList
  rule (CONCAT):Instruction           => CONCAT           .AnnotationList
  rule (SLICE):Instruction            => SLICE            .AnnotationList
  rule (PACK):Instruction             => PACK             .AnnotationList
  rule (ADD):Instruction              => ADD              .AnnotationList
  rule (SUB):Instruction              => SUB              .AnnotationList
  rule (MUL):Instruction              => MUL              .AnnotationList
  rule (EDIV):Instruction             => EDIV             .AnnotationList
  rule (ABS):Instruction              => ABS              .AnnotationList
  rule (ISNAT):Instruction            => ISNAT            .AnnotationList
  rule (INT):Instruction              => INT              .AnnotationList
  rule (NEG):Instruction              => NEG              .AnnotationList
  rule (LSL):Instruction              => LSL              .AnnotationList
  rule (LSR):Instruction              => LSR              .AnnotationList
  rule (OR):Instruction               => OR               .AnnotationList
  rule (AND):Instruction              => AND              .AnnotationList
  rule (XOR):Instruction              => XOR              .AnnotationList
  rule (NOT):Instruction              => NOT              .AnnotationList
  rule (COMPARE):Instruction          => COMPARE          .AnnotationList
  rule (EQ):Instruction               => EQ               .AnnotationList
  rule (NEQ):Instruction              => NEQ              .AnnotationList
  rule (LT):Instruction               => LT               .AnnotationList
  rule (GT):Instruction               => GT               .AnnotationList
  rule (LE):Instruction               => LE               .AnnotationList
  rule (GE):Instruction               => GE               .AnnotationList
  rule (SELF):Instruction             => SELF             .AnnotationList
  rule (TRANSFER_TOKENS):Instruction  => TRANSFER_TOKENS  .AnnotationList
  rule (SET_DELEGATE):Instruction     => SET_DELEGATE     .AnnotationList
  rule (CREATE_ACCOUNT):Instruction   => CREATE_ACCOUNT   .AnnotationList
  rule (IMPLICIT_ACCOUNT):Instruction => IMPLICIT_ACCOUNT .AnnotationList
  rule (NOW):Instruction              => NOW              .AnnotationList
  rule (CHAIN_ID):Instruction         => CHAIN_ID         .AnnotationList
  rule (AMOUNT):Instruction           => AMOUNT           .AnnotationList
  rule (BALANCE):Instruction          => BALANCE          .AnnotationList
  rule (CHECK_SIGNATURE):Instruction  => CHECK_SIGNATURE  .AnnotationList
  rule (BLAKE2B):Instruction          => BLAKE2B          .AnnotationList
  rule (SHA256):Instruction           => SHA256           .AnnotationList
  rule (SHA512):Instruction           => SHA512           .AnnotationList
  rule (HASH_KEY):Instruction         => HASH_KEY         .AnnotationList
  rule (STEPS_TO_QUOTA):Instruction   => STEPS_TO_QUOTA   .AnnotationList
  rule (SOURCE):Instruction           => SOURCE           .AnnotationList
  rule (SENDER):Instruction           => SENDER           .AnnotationList
  rule (ADDRESS):Instruction          => ADDRESS          .AnnotationList
  rule (SWAP):Instruction             => SWAP             .AnnotationList
```

### Other Pretty Instructions

```k
  syntax Instruction ::= UnaryIntInstName Int [macro]
                       | UnaryTypeInstName Type [macro]
                       | UnaryBlockInstName Block [macro]
                       | BinaryTypeInstName Type Type [macro]
                       | BinaryBlockInstName Block Block [macro]

  rule I:UnaryIntInstName    P:Int             => I .AnnotationList P
  rule I:UnaryTypeInstName   P:Type            => I .AnnotationList P
  rule I:UnaryBlockInstName  P:Block           => I .AnnotationList P
  rule I:BinaryTypeInstName  P1:Type  P2:Type  => I .AnnotationList P1 P2
  rule I:BinaryBlockInstName P1:Block P2:Block => I .AnnotationList P1 P2

  syntax SpecialInstruction ::= "PUSH" Type Data [macro]
                              | "LAMBDA" Type Type Block [macro]
                              | "CREATE_CONTRACT" "{" Contract "}" [macro]
                              | "DIP" Int Block [macro]

  rule PUSH T:Type D:Data => PUSH .AnnotationList T D
  rule LAMBDA T1:Type T2:Type B:Block => LAMBDA .AnnotationList T1 T2 B
  rule CREATE_CONTRACT { C } => CREATE_CONTRACT .AnnotationList { C }
  rule DIP I:Int B:Block => DIP .AnnotationList I B
```

## Types

## Pretty Nullary Types

The macro processor has problems inferring the type of nullary types.
We get around this problem by explicitly declaring every `NullaryTypeName` a `Type`, and giving each a separate macro.

```k
  syntax Type ::= "int" [macro]
                | "nat" [macro]
                | "string" [macro]
                | "bytes" [macro]
                | "mutez" [macro]
                | "bool" [macro]
                | "key_hash" [macro]
                | "timestamp" [macro]
                | "address" [macro]
                | "key" [macro]
                | "unit" [macro]
                | "signature" [macro]
                | "operation" [macro]
                | "chain_id" [macro]

  rule (int):Type          => int        .AnnotationList
  rule (nat):Type          => nat        .AnnotationList
  rule (string):Type       => string     .AnnotationList
  rule (bytes):Type        => bytes      .AnnotationList
  rule (mutez):Type        => mutez      .AnnotationList
  rule (bool):Type         => bool       .AnnotationList
  rule (key_hash):Type     => key_hash   .AnnotationList
  rule (timestamp):Type    => timestamp  .AnnotationList
  rule (address):Type      => address    .AnnotationList
  rule (key):Type          => key        .AnnotationList
  rule (unit):Type         => unit       .AnnotationList
  rule (signature):Type    => signature  .AnnotationList
  rule (operation):Type    => operation  .AnnotationList
  rule (chain_id):Type     => chain_id   .AnnotationList
```

## Other Pretty Types

```k
  syntax Type ::= UnaryTypeName   Type [macro]
                | BinaryTypeName  Type Type [macro]
                | BinaryPlusTypeName Type TypeList [macro]

  rule N:UnaryTypeName      T     => N .AnnotationList T
  rule N:BinaryTypeName     T1 T2 => N .AnnotationList T1 T2
  rule N:BinaryPlusTypeName T TL => N .AnnotationList T TL
```

## Literals

The boolean literals from Michelson needs to be wrapped in `#token`, since they are not part of the usual internal syntax.

```k
  syntax MichelsonBool ::= Bool
  syntax MichelsonBool ::= "True"  [macro]
                         | "False" [macro]

  rule True  => true
  rule False => false
```

```k
endmodule
```
