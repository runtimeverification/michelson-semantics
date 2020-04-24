This file contains the productions for the internal representation of certain types of Michelson data in K.  These are shared among a number of different modules which should not depend on one another, and so is kept separate.  Furthermore, it contains a number of macro rules which standardize the representations of certain productions (e.g. by reordering them and adding/removing extra semicolons).

```k
requires "michelson-syntax.k"

module MICHELSON-COMMON
  imports MICHELSON-SYNTAX
  imports DOMAINS-SYNTAX
  imports BYTES
```

These are general "Unset" values for Data and Type productions, used when there should be information in a cell or production, but there isn't.  For example, `<parametertype>` is set to `#NotSet` until the `parameter` group is loaded.

** `#NoData` should not be confused with `None` - the former is invalid data and should never occur on the Michelson stack, the latter is perfectly legitimate Michelson data representing an `option` type which happens to be empty.  **

```k
  syntax Type ::= "#NotSet"
  syntax Data ::= "#NoData"
```

These productions wrap the literal data of certain Michelson types, attaching a minimal amount of type information to data elements on the stack which allows K to select the proper rules.  For example, the `ADD` instruction uses the presence of the `#Timestamp` wrapper to distinguish between additions involving timestamps and naturals.

```k
  syntax Address ::= #Address(String)
  syntax ContractData ::= #Contract(Address, Type)
  syntax Mutez ::= #Mutez(Int)
  syntax KeyHash ::= #KeyHash(String)
  syntax ChainId ::= #ChainId(MBytes)
  syntax Timestamp ::= #Timestamp(Int)
  syntax Key ::= #Key(String)
  syntax Signature ::= #Signature(String)
  syntax OperationNonce ::= #Nonce(Int)
```

The K specification of the Michelson Bytes type is incomplete due to the lack a formal specification or even documentation of the `PACK` and `UNPACK` instructions.  Thus, the best we can do for now is wrap packed data with a production which allows us to axiomatize `PACK ; UNPACK _` as an identity operation.  We give the various cryptographic operations a similar treatment for now.

```k
  syntax MBytes ::= MBytesLiteral
                  | #Packed(Data)
                  | #Blake2B(MBytes)
                  | #SHA256(MBytes)
                  | #SHA512(MBytes)
```

We extend the Data sort with the internal K representations of any Michelson data that does not directly map into K, such as those in the productions above.

```k
  syntax Data ::= Timestamp
  syntax Data ::= ChainId
  syntax Data ::= KeyHash
  syntax Data ::= Mutez
  syntax Data ::= Address
  syntax Data ::= ContractData
  syntax Data ::= Key
  syntax Data ::= Signature
  syntax Data ::= Bool
  syntax Data ::= #Lambda(Type, Type, Block)
  syntax Data ::= MBytes
```

We specify that both `parameter T` and `storage T` productions are also groups (see `michelson-syntax.md` for a description of groups).

```k
  syntax Group ::= ParameterDecl
  syntax Group ::= StorageDecl
```

The `#LoadGroups` production is used during contract loading.  See `michelson.k` for a full description of its behavior.

```k
  syntax KItem ::= #LoadGroups(Groups)
```

Michelson bools are of the form (True/False), but K bools are of the form (true/false).  We convert them here so we can define rewrite rules over the K bool sort.

```k
  rule `MichelsonBool`(True) => true
  rule `MichelsonBool`(False) => false
```

Michelson byte literals are given by their hexadecimal representation with the prefix "Ox".
Since the K byte literal has a different representation, we convert here from one to the other.

```k
  syntax MBytes ::= Bytes

  syntax Bytes ::= #MBytesLiteralToBytes(MBytesLiteral) [function, hook(BYTES.hexstring2bytes)]
  rule `MBytesLiteral`(M) => #MBytesLiteralToBytes(M)
```

A Michelson contract consists of three [primitive applications](https://tezos.gitlab.io/whitedoc/michelson.html#primitive-applications) `code`, `storage` and `parameter` in any order, separated by semicolons and with or without an extra semicolon at the end.  We standardize this format with these eleven 'anywhere' rules here.

```k
  rule code B ; storage St ; parameter Pt => code B ; storage St ; parameter Pt ; [anywhere]
  rule code B ; parameter Pt ; storage St => code B ; storage St ; parameter Pt ; [anywhere]
  rule storage St ; code B ; parameter Pt => code B ; storage St ; parameter Pt ; [anywhere]
  rule parameter Pt ; code B ; storage St => code B ; storage St ; parameter Pt ; [anywhere]
  rule storage St ; parameter Pt ; code B => code B ; storage St ; parameter Pt ; [anywhere]
  rule parameter Pt ; storage St ; code B => code B ; storage St ; parameter Pt ; [anywhere]

  rule code B ; parameter Pt ; storage St ; => code B ; storage St ; parameter Pt ; [anywhere]
  rule storage St ; code B ; parameter Pt ; => code B ; storage St ; parameter Pt ; [anywhere]
  rule parameter Pt ; code B ; storage St ; => code B ; storage St ; parameter Pt ; [anywhere]
  rule storage St ; parameter Pt ; code B ; => code B ; storage St ; parameter Pt ; [anywhere]
  rule parameter Pt ; storage St ; code B ; => code B ; storage St ; parameter Pt ; [anywhere]
endmodule
```
