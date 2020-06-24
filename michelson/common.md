This file contains the productions for the internal representation of certain
types of Michelson data in K. These are shared among a number of different
modules which should not depend on one another, and so is kept separate.
Furthermore, it contains a number of macro rules which standardize the
representations of certain productions (e.g. by reordering them and
adding/removing extra semicolons).

```k
requires "michelson/internal-syntax.md"

module MICHELSON-COMMON
  imports MICHELSON-INTERNAL-SYNTAX
  imports BYTES
  imports DOMAINS
  imports COLLECTIONS
```

The internal representation of Michelson sets, lists and maps are simply K sets,
lists and maps respectively.

```k
  syntax Data ::= Set | Map | List
```

The internal representation of typed data in Michelson.

```k
  syntax TypedData ::= #Typed(Data, Type)
  syntax Data ::= TypedData
```

These are general "Unset" values for Data and Type productions, used when there
should be information in a cell or production, but there isn't. For example,
`<parametertype>` is set to `#NotSet` until the `parameter` group is loaded.

**`#NoData` should not be confused with `None` - the former is invalid data and
should never occur on the Michelson stack, the latter is perfectly legitimate
Michelson data representing an `option` type which happens to be empty.**

```k
  syntax Type ::= "#NotSet"
  syntax Data ::= "#NoData"
```

These productions wrap the literal data of certain Michelson types, attaching a
minimal amount of type information to data elements on the stack which allows K
to select the proper rules. For example, the `ADD` instruction uses the presence
of the `#Timestamp` wrapper to distinguish between additions involving
timestamps and naturals.

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

The K specification of the Michelson Bytes type is incomplete due to the lack a
formal specification or even documentation of the `PACK` and `UNPACK`
instructions. Thus, the best we can do for now is wrap packed data with a
production which allows us to axiomatize `PACK ; UNPACK _` as an identity
operation. We give the various cryptographic operations a similar treatment for
now.

```k
  syntax MBytes ::= MBytesLiteral
                  | #Packed(Data)
                  | #Blake2B(MBytes)
                  | #SHA256(MBytes)
                  | #SHA512(MBytes)
```

We extend the Data sort with the internal K representations of any Michelson
data that does not directly map into K, such as those in the productions above.

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

We specify that both `parameter T` and `storage T` productions are also groups
(see `michelson-syntax.md` for a description of groups).

```k
  syntax Group ::= ParameterDecl
  syntax Group ::= StorageDecl
```

Michelson bools are of the form (True/False), but K bools are of the form
(true/false). We convert them here so we can define rewrite rules over the K
bool sort.

```k
  rule `MichelsonBool`(True) => true
  rule `MichelsonBool`(False) => false
```

These rules define what constitutes a legal mutez value, allowing us to
represent mutez overflow.

```k
  syntax Int ::= "#MutezOverflowLimit" [function]
  rule #MutezOverflowLimit => 2 ^Int 63 // Signed 64 bit integers.

  syntax Bool ::= #IsLegalMutezValue(Int) [function]
  rule #IsLegalMutezValue(I) => I >=Int 0 andBool I <Int #MutezOverflowLimit
```

Michelson byte literals are given by their hexadecimal representation with the
prefix "Ox". Since the K byte literal has a different representation, we convert
here from one to the other.

```k
  syntax MBytes ::= Bytes

  syntax Bytes ::= #MBytesLiteralToBytes(MBytesLiteral) [function, hook(BYTES.hexstring2bytes)]
  rule `MBytesLiteral`(M) => #MBytesLiteralToBytes(M)
```

A Michelson contract consists of three [primitive
applications](https://tezos.gitlab.io/whitedoc/michelson.html#primitive-applications)
`code`, `storage` and `parameter` in any order, separated by semicolons and with
or without an extra semicolon at the end. We standardize this format with these
eleven 'anywhere' rules here.

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
```

This function is similar to `#MichelineToNative` but for contract maps.

```k
  syntax Map ::= #OtherContractsMapToKMap(OtherContractsMap) [function]
  syntax Map ::= #OtherContractsMapEntryListToKMap(OtherContractsMapEntryList) [function]
  rule #OtherContractsMapToKMap({ }) => .Map
  rule #OtherContractsMapToKMap({ EL }) => #OtherContractsMapEntryListToKMap(EL)
  rule #OtherContractsMapEntryListToKMap( Elt A T ) => #Address(A) |-> #Contract(#Address(A), T)
  rule #OtherContractsMapEntryListToKMap( Elt A T ; Rs ) => #Address(A) |-> #Contract(#Address(A), T) #OtherContractsMapEntryListToKMap(Rs)
```

This function transforms a Michelson data element from its Micheline
representation to its internal K representation given the data and its real
Michelson type. It performs some basic sanity checks on the data passed (that it
is of the correct sort, for example). but otherwise does not attempt to do a
real typecheck.

```k
  syntax DataOrSeq ::= Data | DataList | MapEntryList // Can't subsort DataList to Data, as that would form a cycle.
  syntax Data ::= #MichelineToNative(DataOrSeq, Type, Map, Map) [function]
```

### Converting String-Based Datatypes

Michelson has several datatypes whose values are given as Micheline Strings.
These include:

- address
- key
- key_hash
- signature
- string
- timestamp (in its human-readable form)

We delegate these datatypes validations to their own functions.

```k
  rule #MichelineToNative(S:String, key_hash _,  KnownAddrs, BigMaps) => #ParseKeyHash(S)
  rule #MichelineToNative(S:String, address _,   KnownAddrs, BigMaps) => #ParseAddress(S)
  rule #MichelineToNative(S:String, key _,       KnownAddrs, BigMaps) => #ParseKey(S)
  rule #MichelineToNative(S:String, signature _, KnownAddrs, BigMaps) => #ParseSignature(S)
  rule #MichelineToNative(S:String, timestamp _, KnownAddrs, BigMaps) => #ParseTimestamp(S)
```

A simple hook to return the Unix epoch representation of a timestamp passed to
the semantics in an ISO8601 format string. See time.cpp for its implementation.
The semantics accept timestamps in one of two formats:

1. An ISO-8601 string.
2. A unix timestamp

```k
  syntax Int ::= #ISO2Epoch(String) [function, hook(TIME.ISO2Epoch)]
  syntax Timestamp ::= #ParseTimestamp(String) [function]
  rule #ParseTimestamp(S) => #Timestamp(#ISO2Epoch(S)) requires findString(S, "Z", 0) >=Int 0
  rule #ParseTimestamp(S) => #Timestamp(String2Int(S)) requires findString(S, "Z", 0) <Int 0
```

The other string based datatypes have stubs for their validation functions.

```k
  syntax KeyHash ::= #ParseKeyHash(String) [function]
  rule #ParseKeyHash(S) => #KeyHash(S)

  syntax Address ::= #ParseAddress(String) [function]
  rule #ParseAddress(S) => #Address(S)

  syntax Key ::= #ParseKey(String) [function]
  rule #ParseKey(S) => #Key(S)

  syntax Signature ::= #ParseSignature(String) [function]
  rule #ParseSignature(S) => #Signature(S)
```

Note that timestamps have an optimized form based on integers.
We convert that form here.

```k
  rule #MichelineToNative(I:Int, timestamp _, KnownAddrs, BigMaps) => #Timestamp(I)
```

### Converting Simple Datatypes

A ChainId is simply a specially tagged MBytes.

```k
  rule #MichelineToNative(H:MBytes, chain_id _, KnownAddrs, BigMaps) => #ChainId(H)
```

An int can simply be represented directly as a K int. Nats get an additional
sanity check to avoid negative nats.

```k
  rule #MichelineToNative(I:Int, int _, KnownAddrs, BigMaps) => I
  rule #MichelineToNative(I:Int, nat _, KnownAddrs, BigMaps) => I requires I >=Int 0
```

Strings, like ints, represent themselves.

```k
  rule #MichelineToNative(S:String, string _, KnownAddrs, BigMaps) => S
```

MBytes conversion is done by the function rule.

```k
  rule #MichelineToNative(B:MBytes, bytes _, KnownAddrs, BigMaps) => B
```

Mutez is simply a specially tagged int - we also sanity check the int to ensure
that it is in bounds.

```k
  rule #MichelineToNative(I:Int, mutez _, KnownAddrs, BigMaps) => #Mutez(I) requires #IsLegalMutezValue(I)
```

K's function expansion step has already handled converting Michelsons
"True/False" booleans into "true/false" K bools, so we don't need to do anything
special with them here.

```k
  rule #MichelineToNative(B:Bool, bool _, KnownAddrs, BigMaps) => B
```

The Unit token represents itself.

```k
  rule #MichelineToNative(Unit, unit _, KnownAddrs, BigMaps) => Unit
```

### Converting Complex Datatypes

We recursively convert the contents of pairs, ors and options, if applicable.

```k
  rule #MichelineToNative(Pair A B, pair _ T1:Type T2:Type, KnownAddrs, BigMaps) =>
       Pair #MichelineToNative(A, T1, KnownAddrs, BigMaps) #MichelineToNative(B, T2, KnownAddrs, BigMaps)

  rule #MichelineToNative(Some V, option _ T, KnownAddrs, BigMaps) => Some #MichelineToNative(V, T, KnownAddrs, BigMaps)
  rule #MichelineToNative(None, option _:AnnotationList _, KnownAddrs, BigMaps) => None

  rule #MichelineToNative(Left V, or _:AnnotationList TL:Type _:Type,  KnownAddrs, BigMaps) => Left #MichelineToNative(V, TL, KnownAddrs, BigMaps)
  rule #MichelineToNative(Right V, or _:AnnotationList _:Type TR:Type, KnownAddrs, BigMaps) => Right #MichelineToNative(V, TR, KnownAddrs, BigMaps)
```

We wrap Lambdas appropriately and save their type information.  Note that we do *not* recurse into the Block.

```k
  rule #MichelineToNative(B:Block, lambda _:AnnotationList T1 T2, KnownAddrs, BigMaps) => #Lambda(T1, T2, B)
```

Collections are converted one element at a time. We need to handle the cases of
0 and 1 length lists separately due to parsing ambiguities between a size 1
element list, and another embedded list.

```k
  rule #MichelineToNative({ }, list _ _, KnownAddrs, BigMaps) => .List
  rule #MichelineToNative({ D1:Data }, list _ T, KnownAddrs, BigMaps) => ListItem(#MichelineToNative(D1, T, KnownAddrs, BigMaps))
  rule #MichelineToNative({ D1 ; DL:DataList }, list _ T, KnownAddrs, BigMaps) => #MichelineToNative(D1 ; DL, list .AnnotationList T, KnownAddrs, BigMaps)

  rule #MichelineToNative(D1:Data ; D2:Data, list _ T, KnownAddrs, BigMaps) =>
       ListItem(#MichelineToNative(D1, T, KnownAddrs, BigMaps)) ListItem(#MichelineToNative(D2, T, KnownAddrs, BigMaps))

  rule #MichelineToNative(D1:Data ; D2:Data ; DL:DataList, list _ T, KnownAddrs, BigMaps) =>
       ListItem(#MichelineToNative(D1, T, KnownAddrs, BigMaps)) {#MichelineToNative(D2 ; DL, list .AnnotationList T, KnownAddrs, BigMaps)}:>List
```

Sets are handled essentially the same way as lists, with the same caveat about
needing to handle 3 cases (0-Size sets, 1-Size sets, and otherwise).

```k
  rule #MichelineToNative({ }, set _ _, KnownAddrs, BigMaps) => .Set
  rule #MichelineToNative({ D:Data }, set _ T, KnownAddrs, BigMaps) => SetItem(#MichelineToNative(D, T, KnownAddrs, BigMaps))
  rule #MichelineToNative({ D1 ; DL:DataList }, set _ T, KnownAddrs, BigMaps) => #MichelineToNative(D1 ; DL, set .AnnotationList T, KnownAddrs, BigMaps)

  rule #MichelineToNative(D1:Data ; D2:Data, set _ T, KnownAddrs, BigMaps) =>
       SetItem(#MichelineToNative(D1, T, KnownAddrs, BigMaps)) SetItem(#MichelineToNative(D2, T, KnownAddrs, BigMaps))

  rule #MichelineToNative(D1:Data ; D2:Data ; DL:DataList, set _ T, KnownAddrs, BigMaps) =>
       SetItem(#MichelineToNative(D1, T, KnownAddrs, BigMaps)) {#MichelineToNative(D2 ; DL, set .AnnotationList T, KnownAddrs,BigMaps)}:>Set
```

Maps and `big_map`s do not have the same parsing ambiguity, so we do not need to
handle the case of size 1 maps separately. Note that, internally, no difference
exists between maps and `big_map`s in K-Michelson.

```k
  rule #MichelineToNative({ }, map _ _ _, KnownAddrs, BigMaps) => .Map
  rule #MichelineToNative({ M:MapEntryList }, map _:AnnotationList KT VT, KnownAddrs, BigMaps) =>
       #MichelineToNative(M, map .AnnotationList KT VT, KnownAddrs, BigMaps)

  rule #MichelineToNative(Elt K V ; ML, map _:AnnotationList KT VT, KnownAddrs, BigMaps) =>
       ({#MichelineToNative(ML, map .AnnotationList KT VT, KnownAddrs, BigMaps)}:>Map)[#MichelineToNative(K, KT, KnownAddrs, BigMaps) <- #MichelineToNative(V, VT, KnownAddrs, BigMaps)]

  rule #MichelineToNative(Elt K V, map _:AnnotationList KT VT, KnownAddrs, BigMaps) =>
       #MichelineToNative(K, KT, KnownAddrs, BigMaps) |-> #MichelineToNative(V, VT, KnownAddrs, BigMaps)

  rule #MichelineToNative({ }, big_map _:AnnotationList K V, KnownAddrs, BigMaps) => .Map

  rule #MichelineToNative({ M:MapEntryList }, big_map _:AnnotationList K V, KnownAddrs, BigMaps) =>
       #MichelineToNative({ M:MapEntryList }, map .AnnotationList K V, KnownAddrs, BigMaps)  // We handle big_map literals as maps.
```

We construct a contract datatype from its string address and type. Note that,
for convenience, we do not enforce that this address exists in the
`other_contracts` map!

```k
  rule #MichelineToNative(S:String, contract _ T, KnownAddrs, BigMaps) => #Contract(#ParseAddress(S), T)

  rule #MichelineToNative(#Typed(D, T), T, KnownAddrs, BigMaps) => #MichelineToNative(D, T, KnownAddrs, BigMaps)
```

These rules exists for the unit testing semantics - operations are not legal
data literals in normal Michelson. Note that we need to recurse into the data
elements.

```k
  rule #MichelineToNative(Create_contract(N, C, O, M, S), operation _, KnownAddrs, BigMaps) =>
       Create_contract(
           N,
           C,
           {#MichelineToNative(O, (option .AnnotationList  key_hash .AnnotationList), KnownAddrs, BigMaps)}:>OptionData,
           {#MichelineToNative(M, mutez .AnnotationList, KnownAddrs, BigMaps)}:>Mutez,
           #MichelineToNative(S, #StorageTypeFromContract(C), KnownAddrs, BigMaps)
       )

  rule #MichelineToNative(Set_delegate(N, K), operation _, KnownAddrs, BigMaps) =>
       Set_delegate(N, {#MichelineToNative(K, (option .AnnotationList key_hash .AnnotationList), KnownAddrs, BigMaps)}:>OptionData)

  syntax Type ::= #StorageTypeFromContract(Contract) [function]
  rule #StorageTypeFromContract(code _ ; storage S ; parameter _ ;) => S
```

These rules use the K syntax for a `withConfig` function. This attribute allows
a function read-only access to the K configuration, and is necessary here to
avoid needing an explicit configuration argument in all other rules. We need the
configuration because we need to be able to construct a `contract` value from an
address specified in the `Transfer_tokens` production, and thus need to perform
a contract lookup.

```k
  syntax Type ::= #TypeFromOtherContract(ContractData) [function]
  rule #TypeFromOtherContract(#Contract(_, T)) => T

  rule #MichelineToNative(Transfer_tokens(N, P, M, A), operation _, KnownAddrs, BigMaps)
          => Transfer_tokens(N,
              #MichelineToNative(
                  P,
                  #TypeFromOtherContract({KnownAddrs[#MichelineToNative(A, address .AnnotationList, KnownAddrs, BigMaps)]}:>ContractData),
                  KnownAddrs,
                  BigMaps
              ),
              {#MichelineToNative(M, mutez .AnnotationList, KnownAddrs, BigMaps)}:>Mutez,
              {#MichelineToNative(A, address .AnnotationList, KnownAddrs, BigMaps)}:>Address
          )
       requires #MichelineToNative(A, address .AnnotationList, KnownAddrs, BigMaps) in_keys(KnownAddrs)
```

We extract a `big_map` by index from the bigmaps map. Note that these have
already been converted to K-internal form, so there is no need to recurse here.

```k
  rule #MichelineToNative(I:Int, big_map _:AnnotationList K V, KnownAddrs, BigMaps) => {BigMaps[I]}:>Data
```

```k
endmodule
```
