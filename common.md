```k
requires "syntax.md"
```

Michelson Internal Representation
=================================

The Michelson external syntax needs to be converted into an internal
representation. We define all our internal representations below.

```k
module MICHELSON-COMMON
  imports SYMBOLIC-UNIT-TEST-COMMON-SYNTAX
  imports BYTES
  imports DOMAINS
  imports COLLECTIONS
```

Type Representation
-------------------

We represent types internally without their annotations.

NOTE: This simplification breaks the semantics of some instructions relating
to contracts with entrypoints. We may fix this issue in a later release.

```k
  syntax TypeName ::= NullaryTypeName
                    | UnaryTypeName TypeName
                    | BinaryTypeName TypeName TypeName

  syntax TypeName ::= #Name(Type) [function, functional]
  // ---------------------------------------------------
  rule #Name(T:NullaryTypeName _:AnnotationList) => T
  rule #Name(T:UnaryTypeName _:AnnotationList ArgT) => T #Name(ArgT)
  rule #Name(T:BinaryTypeName _:AnnotationList ArgT1 ArgT2) => T #Name(ArgT1) #Name(ArgT2)

  syntax Type     ::= #Type(TypeName) [function, functional]
  // -------------------------------------------------------
  rule #Type(T:NullaryTypeName) => T .AnnotationList
  rule #Type(T:UnaryTypeName ArgT) => T .AnnotationList #Type(ArgT)
  rule #Type(T:BinaryTypeName ArgT1 ArgT2) => T .AnnotationList #Type(ArgT1) #Type(ArgT2)
```

Value Representation
--------------------

We represent Michelson values by constructors which wrap K primitive types.

```k
  syntax Address ::= #Address(String)
  syntax ContractData ::= #Contract(Address, Type)
  syntax Mutez ::= #Mutez(Int)
  syntax KeyHash ::= #KeyHash(String)
  syntax ChainId ::= #ChainId(MichelsonBytes)
  syntax Timestamp ::= #Timestamp(Int)
  syntax Key ::= #Key(String)
  syntax Signature ::= #Signature(String)
  syntax OperationNonce ::= #Nonce(Int)
  syntax LambdaData ::= #Lambda(TypeName, TypeName, Block)


  syntax SimpleData ::= LambdaData
                      | Timestamp
                      | ChainId
                      | KeyHash
                      | ContractData
                      | Key
                      | Signature
```

We represent the values of byte operations as special constructors.

```k
  syntax MichelsonBytes ::= #Packed(TypeName,Data)
                          | #Blake2B(MichelsonBytes)
                          | #SHA256(MichelsonBytes)
                          | #SHA512(MichelsonBytes)
```

We represent values of collection types (lists, sets, maps) as follows:

```k
  syntax WrappedData  ::= "[" Data "]"
  syntax InternalList ::= List{WrappedData, ";;"}

  syntax Int ::= size(InternalList, Int) [function, functional]
  // ----------------------------------------------------------
  rule size(_ ;; L,        S) => size(L, S +Int 1)
  rule size(.InternalList, S) => S

  syntax SimpleData ::= Set | Map | InternalList
```

The internal representation of mutez is bounded.

```k
  syntax Int ::= "#MutezOverflowLimit" [function]
  // --------------------------------------------
  rule #MutezOverflowLimit => 2 ^Int 63 // Signed 64 bit integers.

  syntax Bool ::= #IsLegalMutezValue(Int) [function]
  // -----------------------------------------------
  rule #IsLegalMutezValue(I) => I >=Int 0 andBool I <Int #MutezOverflowLimit
```

The following representation is used by our legacy type checker.

```k
  syntax TypedData ::= #Typed(Data, Type)
  syntax Data ::= TypedData
```

## Literal Token to Value Conversion

Michelson bool literals are represented internally by the K `Bool` type.

```k
  syntax MichelsonBool ::= Bool
  rule `MichelsonBoolToken`(#token("True",  "MichelsonBoolToken")) => true
  rule `MichelsonBoolToken`(#token("False", "MichelsonBoolToken")) => false
```

Michelson byte literals are represented internally by the K `Bytes` type.

```k
  syntax MichelsonBytes ::= Bytes
  rule `MichelsonBytesToken`(M) => #ParseBytes(stripFirst(#MichelsonBytesTokenToString(M), 2), .Bytes)

  syntax String ::= #MichelsonBytesTokenToString(MichelsonBytesToken) [function, hook(STRING.token2string)]

  syntax Bytes ::= #ParseBytes(String, Bytes) [function]
  // ---------------------------------------------------
  rule #ParseBytes("", ByteStr) => ByteStr
  rule #ParseBytes(Hex, ByteStr)
    => #ParseBytes(stripFirst(Hex, 2),
                   ByteStr
            +Bytes Int2Bytes(1, String2Base(substrString(Hex, 0, 2), 16), BE))
    requires Hex =/=String ""

  syntax String ::= stripFirst(String, Int) [function]
  // -------------------------------------------------
  rule stripFirst(S, I) => substrString(S, I, lengthString(S))
```

Macros must be converted into strings for further processing.

```k
  syntax String ::= #DIPMacroToString(DIPMacro) [function, hook(STRING.token2string)]
  syntax String ::= #DUPMacroToString(DUPMacro) [function, hook(STRING.token2string)]
  syntax String ::= #CDARMacroToString(CDARMacro) [function, hook(STRING.token2string)]
  syntax String ::= #SetCDARMacroToString(SetCDARMacro) [function, hook(STRING.token2string)]
  syntax String ::= #MapCDARMacroToString(MapCDARMacro) [function, hook(STRING.token2string)]
```

These macro conversion functions are currently disabled.

```disabled
  syntax String ::= #PairMacroToString(PairMacro) [function, hook(STRING.token2string)]
  syntax String ::= #UnpairMacroToString(UnpairMacro) [function, hook(STRING.token2string)]
```

Stack Representation
--------------------

We represent the Michelson stack using the `InternalStack` type which contains
the representation of standard stacks as well as failed stacks.

```k
  syntax StackElement ::= "[" TypeName Data "]"
  syntax Stack ::= List{StackElement, ";"} [klabel(Stack)]
  syntax InternalStack ::= Stack | FailedStack

  syntax InternalStack ::= #LiteralStackToStack(OutputStack) [function]
  syntax Stack ::= #LiteralStackToStack(StackElementList, Stack) [function]
  // ----------------------------------------------------------------------
  rule #LiteralStackToStack(FS:FailedStack) => FS
  rule #LiteralStackToStack({ LS }) => #LiteralStackToStack(LS, .Stack)
  rule #LiteralStackToStack(Stack_elt T D ; LS, SS) => #LiteralStackToStack(LS, [#Name(T) D] ; SS)
  rule #LiteralStackToStack(.StackElementList, SS) => reverseStack(SS)

  syntax Stack ::= reverseStack( Stack )        [function]
                 | reverseStack( Stack, Stack ) [function]
  // -----------------------------------------------------
  rule reverseStack( EL )          => reverseStack( EL, .Stack  )
  rule reverseStack( E ; EL, EL' ) => reverseStack( EL, E ; EL' )
  rule reverseStack( .Stack, EL' ) => EL'
```

Miscellaneous Internal Representations
--------------------------------------

The internal represetnation of unset type and data values.

```k
  syntax PreType ::= "#NotSet" | Type
  syntax PreData ::= "#NoData" | Data
```

We specify that both `parameter T` and `storage T` productions are also groups
(see `michelson-syntax.md` for a description of groups).

```k
  syntax Group ::= ParameterDecl
  syntax Group ::= StorageDecl
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

Michelson Internal Representation Conversion
--------------------------------------------

This function converts the `other_contracts` field into its internal
represetation.

```k
  syntax Map ::= #OtherContractsMapEntryListToKMap(OtherContractsMapEntryList) [function]
  rule #OtherContractsMapEntryListToKMap( .OtherContractsMapEntryList ) => .Map
  rule #OtherContractsMapEntryListToKMap( Elt A T ; Rs ) => #Address(A) |-> #Contract(#Address(A), T) #OtherContractsMapEntryListToKMap(Rs)
```

This function converts all other datatypes into their internal represenations.

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
  rule #MichelineToNative(S:String, key_hash _,  _KnownAddrs, _BigMaps) => #ParseKeyHash(S)
  rule #MichelineToNative(S:String, address _,   _KnownAddrs, _BigMaps) => #ParseAddress(S)
  rule #MichelineToNative(S:String, key _,       _KnownAddrs, _BigMaps) => #ParseKey(S)
  rule #MichelineToNative(S:String, signature _, _KnownAddrs, _BigMaps) => #ParseSignature(S)
  rule #MichelineToNative(S:String, timestamp _, _KnownAddrs, _BigMaps) => #ParseTimestamp(S)
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
  rule #MichelineToNative(I:Int, timestamp _, _KnownAddrs, _BigMaps) => #Timestamp(I)
```

### Converting Simple Datatypes

A ChainId is simply a specially tagged MichelsonBytes.

```k
  rule #MichelineToNative(H:MichelsonBytes, chain_id _, _KnownAddrs, _BigMaps) => #ChainId(H)
```

An int can simply be represented directly as a K int. Nats get an additional
sanity check to avoid negative nats.

```k
  rule #MichelineToNative(I:Int, int _, _KnownAddrs, _BigMaps) => I
  rule #MichelineToNative(I:Int, nat _, _KnownAddrs, _BigMaps) => I requires I >=Int 0
```

Strings, like ints, represent themselves.

```k
  rule #MichelineToNative(S:String, string _, _KnownAddrs, _BigMaps) => S
```

MichelsonBytes conversion is done by the function rule.

```k
  rule #MichelineToNative(B:MichelsonBytes, bytes _, _KnownAddrs, _BigMaps) => B
```

Mutez is simply a specially tagged int - we also sanity check the int to ensure
that it is in bounds.

```k
  rule #MichelineToNative(I:Int, mutez _, _KnownAddrs, _BigMaps) => #Mutez(I) requires #IsLegalMutezValue(I)
```

K's function expansion step has already handled converting Michelsons
"True/False" booleans into "true/false" K bools, so we don't need to do anything
special with them here.

```k
  rule #MichelineToNative(B:Bool, bool _, _KnownAddrs, _BigMaps) => B
```

The Unit token represents itself.

```k
  rule #MichelineToNative(Unit, unit _, _KnownAddrs, _BigMaps) => Unit
```

### Converting Complex Datatypes

We recursively convert the contents of pairs, ors and options, if applicable.

```k
  rule #MichelineToNative(Pair A B, pair _ T1:Type T2:Type, KnownAddrs, BigMaps) =>
       Pair #MichelineToNative(A, T1, KnownAddrs, BigMaps) #MichelineToNative(B, T2, KnownAddrs, BigMaps)

  rule #MichelineToNative(Some V, option _ T, KnownAddrs, BigMaps) => Some #MichelineToNative(V, T, KnownAddrs, BigMaps)
  rule #MichelineToNative(None, option _:AnnotationList _, _KnownAddrs, _BigMaps) => None

  rule #MichelineToNative(Left V, or _:AnnotationList TL:Type _:Type,  KnownAddrs, BigMaps) => Left #MichelineToNative(V, TL, KnownAddrs, BigMaps)
  rule #MichelineToNative(Right V, or _:AnnotationList _:Type TR:Type, KnownAddrs, BigMaps) => Right #MichelineToNative(V, TR, KnownAddrs, BigMaps)
```

We wrap Lambdas appropriately and save their type information.  Note that we do *not* recurse into the Block.

```k
  rule #MichelineToNative(B:Block, lambda _:AnnotationList T1 T2, _KnownAddrs, _BigMaps) => #Lambda(#Name(T1), #Name(T2), B)
```

Collections are converted one element at a time. We need to handle the cases of
0 and 1 length lists separately due to parsing ambiguities between a size 1
element list, and another embedded list.

```k
  rule #MichelineToNative({ },                  list _ _, _KnownAddrs, _BigMaps) => .InternalList
  rule #MichelineToNative({ D1:Data },          list _ T,  KnownAddrs, BigMaps) => [ #MichelineToNative(D1, T, KnownAddrs, BigMaps) ] ;; .InternalList
  rule #MichelineToNative({ D1 ; DL:DataList }, list _ T,  KnownAddrs, BigMaps) => #MichelineToNativeList(D1 ; DL, list .AnnotationList T, KnownAddrs, BigMaps)

  syntax InternalList ::= #MichelineToNativeList(DataList, Type, Map, Map) [function]
  // --------------------------------------------------------------------------------
  rule #MichelineToNativeList(D1:Data ; D2:Data, list _ T, KnownAddrs, BigMaps) =>
       [ #MichelineToNative(D1, T, KnownAddrs, BigMaps) ]  ;; [ #MichelineToNative(D2, T, KnownAddrs, BigMaps) ]

  rule #MichelineToNativeList(D1:Data ; D2:Data ; DL:DataList, list _ T, KnownAddrs, BigMaps) =>
       [ #MichelineToNative(D1, T, KnownAddrs, BigMaps) ] ;; #MichelineToNativeList(D2 ; DL, list .AnnotationList T, KnownAddrs, BigMaps)
```

Sets are handled essentially the same way as lists, with the same caveat about
needing to handle 3 cases (0-Size sets, 1-Size sets, and otherwise).

```k
  rule #MichelineToNative({ }, set _ _, _KnownAddrs, _BigMaps) => .Set
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
  rule #MichelineToNative({ }, map _ _ _, _KnownAddrs, _BigMaps) => .Map
  rule #MichelineToNative({ M:MapEntryList }, map _:AnnotationList KT VT, KnownAddrs, BigMaps) =>
       #MichelineToNative(M, map .AnnotationList KT VT, KnownAddrs, BigMaps)

  rule #MichelineToNative(Elt K V ; ML, map _:AnnotationList KT VT, KnownAddrs, BigMaps) =>
       ({#MichelineToNative(ML, map .AnnotationList KT VT, KnownAddrs, BigMaps)}:>Map)[#MichelineToNative(K, KT, KnownAddrs, BigMaps) <- #MichelineToNative(V, VT, KnownAddrs, BigMaps)]

  rule #MichelineToNative(Elt K V, map _:AnnotationList KT VT, KnownAddrs, BigMaps) =>
       #MichelineToNative(K, KT, KnownAddrs, BigMaps) |-> #MichelineToNative(V, VT, KnownAddrs, BigMaps)

  rule #MichelineToNative({ }, big_map _:AnnotationList _K _V, _KnownAddrs, _BigMaps) => .Map

  rule #MichelineToNative({ M:MapEntryList }, big_map _:AnnotationList K V, KnownAddrs, BigMaps) =>
       #MichelineToNative({ M:MapEntryList }, map .AnnotationList K V, KnownAddrs, BigMaps)  // We handle big_map literals as maps.
```

We construct a contract datatype from its string address and type. Note that,
for convenience, we do not enforce that this address exists in the
`other_contracts` map!

```k
  rule #MichelineToNative(S:String, contract _ T, _KnownAddrs, _BigMaps) => #Contract(#ParseAddress(S), T)

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
  rule #MichelineToNative(I:Int, big_map _:AnnotationList _K _V, _KnownAddrs, BigMaps) => {BigMaps[I]}:>Data
```

The wildcard value maps to itself.

```k
  rule #MichelineToNative(WC:Wildcard, _, _, _) => WC
```

```k
endmodule
```
