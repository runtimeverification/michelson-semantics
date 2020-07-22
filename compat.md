Compatability Layer for Tezos Client
====================================

For cross-validating this interpreter with teh Tezos Client, we need a compatability layer to translate inputs/outputs back and forth.

Common
------

```k
requires "json.k"
requires "unit-test/unit-test.md"
requires "michelson-unparser.md"

module COMPAT-COMMON
  imports UNIT-TEST-SYNTAX
  imports MICHELSON-UNPARSER

  configuration <k> $PGM:Pgm </k>
                <knownaddrs> .Map </knownaddrs>
                <out stream="stdout"> .List </out>
                <returncode exit=""> 1 </returncode>
```

When the `<k>` cell is empty, we consider execution successful

```k
  rule <k> .K </k>
       <returncode> 1 => 0 </returncode>
endmodule
```

Contract Expander
-----------------

```k
module CONTRACT-EXPANDER-SYNTAX
  imports UNIT-TEST-SYNTAX
endmodule

module CONTRACT-EXPANDER
  imports CONTRACT-EXPANDER-SYNTAX
  imports COMPAT-COMMON

  syntax Block ::= #StackEltToPush(StackElement) [function]
  rule #StackEltToPush(Stack_elt (contract _:AnnotationList T:Type) D:Data) => { PUSH .AnnotationList address .AnnotationList D ;
                                                                                 CONTRACT .AnnotationList T ;
                                                                                 IF_SOME .AnnotationList { } { UNIT .AnnotationList ; FAILWITH .AnnotationList } }
  rule #StackEltToPush(Stack_elt T D) => { PUSH .AnnotationList T D } [owise]

  syntax Block ::= #StackToPush(LiteralStack) [function]

  syntax Block ::= #StackToPushAux(StackElementList, Block) [function]
  rule #StackToPush( { Se } ) => #StackToPushAux(Se, { DROP .AnnotationList 0 })
  rule #StackToPushAux(Se ; Ls, { I:DataList })  => #StackToPushAux(Ls, { #StackEltToPush(Se) ; I }) requires notBool Ls ==K .StackElementList
  rule #StackToPushAux(Se, { I:DataList })  => { #StackEltToPush(Se) ; I }
  rule #StackToPushAux(.StackElementList, { I:DataList }) => { I }

  syntax Contract ::= #FillTemplateContract(Block, Block, Type) [function]

  syntax Bool ::= #CodeBlockEndsWithFail(Block) [function]
  rule #CodeBlockEndsWithFail( { } ) => false
  rule #CodeBlockEndsWithFail( { { Is } } ) => #CodeBlockEndsWithFail( { Is } )
  rule #CodeBlockEndsWithFail( { I:Instruction } ) => ((FAILWITH _) :=K I) [owise]
  rule #CodeBlockEndsWithFail( { _:Instruction ; Is } ) => #CodeBlockEndsWithFail( { Is } )

  rule #FillTemplateContract(PushBlock, CodeBlock, ParamType) =>
  parameter ParamType ; storage unit .AnnotationList ; code { DROP .AnnotationList ; PushBlock ; CodeBlock ; UNIT @exitToken ; FAILWITH .AnnotationList }  ;
  requires notBool #CodeBlockEndsWithFail(CodeBlock)

  rule #FillTemplateContract(PushBlock, CodeBlock, ParamType) =>
  parameter ParamType ; storage unit .AnnotationList ; code { DROP .AnnotationList ; PushBlock ; CodeBlock }  ;
  requires #CodeBlockEndsWithFail(CodeBlock)

  syntax LiteralStack ::= #FindInputGroup(Groups) [function]
  rule #FindInputGroup(input T) => T
  rule #FindInputGroup(input T;) => T
  rule #FindInputGroup(input T ; _) => T
  rule #FindInputGroup(_ ; G) => #FindInputGroup(G) [owise]

  syntax Block ::= #FindCodeGroup(Groups) [function]
  rule #FindCodeGroup(code T) => { T }
  rule #FindCodeGroup(code T;) => { T }
  rule #FindCodeGroup(code T ; _) => { T }
  rule #FindCodeGroup(_ ; G) => #FindCodeGroup(G) [owise]

  syntax Type ::= #FindParamType(Groups) [function]
  rule #FindParamType(parameter T) => T
  rule #FindParamType(parameter T;) => T
  rule #FindParamType(parameter T ; Rs) => T
  rule #FindParamType(_:Group) => unit .AnnotationList [owise]
  rule #FindParamType(_:Group ;) => unit .AnnotationList [owise]
  rule #FindParamType(_ ; Rs) => #FindParamType(Rs) [owise]

  rule <k> G:Groups => . </k>
       <out> ... .List => ListItem(#unparse(#FillTemplateContract(#StackToPush(#FindInputGroup(G)), #FindCodeGroup(G), #FindParamType(G)))) </out>
endmodule
```

Extractor
---------

```k
module EXTRACTOR-SYNTAX
  imports UNIT-TEST-SYNTAX
endmodule

module EXTRACTOR
  imports COMPAT-COMMON
  imports EXTRACTOR-SYNTAX
  imports JSON

  syntax JSONKey ::= #KeyFromGroup(Group) [function]

  rule #KeyFromGroup(G:ContractGroup) => "contract"
  rule #KeyFromGroup(G:NowGroup) => "now"
  rule #KeyFromGroup(G:SenderGroup) => "sender"
  rule #KeyFromGroup(G:SourceGroup) => "source"
  rule #KeyFromGroup(G:ChainGroup) => "chain_id"
  rule #KeyFromGroup(G:SelfGroup) => "self"
  rule #KeyFromGroup(G:AmountGroup) => "amount"
  rule #KeyFromGroup(G:BalanceGroup) => "balance"
  rule #KeyFromGroup(G:BigMapGroup) => "big_maps"
  rule #KeyFromGroup(G:ContractsGroup) => "other_contracts"
  rule #KeyFromGroup(G:CodeGroup) => "code"
  rule #KeyFromGroup(G:InputGroup) => "input"
  rule #KeyFromGroup(G:OutputGroup) => "output"
  rule #KeyFromGroup(G:ParameterDecl) => "parameter"
  rule #KeyFromGroup(G:ParameterValueGroup) => "parameter_value"
  rule #KeyFromGroup(G:StorageValueGroup) => "storage_value"

  syntax K ::= #GroupContent(Group) [function]
  rule #GroupContent(contract { C }) => C
  rule #GroupContent(now C) => C
  rule #GroupContent(sender C) => C
  rule #GroupContent(source C) => C
  rule #GroupContent(chain_id C) => C
  rule #GroupContent(self C) => C
  rule #GroupContent(amount C) => C
  rule #GroupContent(balance C) => C
  rule #GroupContent(other_contracts { C }) => C
  rule #GroupContent(code C) => C
  rule #GroupContent(input C) => C
  rule #GroupContent(output C) => C
  rule #GroupContent(parameter C) => C
  rule #GroupContent(big_maps { C }) => C
  rule #GroupContent(parameter_value C) => C
  rule #GroupContent(storage_value C) => C

  syntax JSON  ::= #GroupsToJson(Groups)      [function]
  syntax JSONs ::= #GroupsToJsonInner(Groups) [function]
  syntax JSON  ::= #GroupToJson(Group)        [function]
  syntax JSON  ::= #GroupToJson(Group,Bool)   [function]

  rule #GroupsToJson(Gs) => { #GroupsToJsonInner(Gs) }

  rule #GroupsToJsonInner(G ; Gs) => #GroupToJson(G) , #GroupsToJsonInner(Gs)
  rule #GroupsToJsonInner(G)      => #GroupToJson(G)
  rule #GroupsToJsonInner(G ;)    => #GroupToJson(G)

  // check if we need to add braces around our output group
  rule #GroupToJson(G) => #GroupToJson(G, #KeyFromGroup(G) in SetItem("other_contracts") SetItem("big_maps"))

  rule #GroupToJson(G, false) => #KeyFromGroup(G) :             #unparse(#GroupContent(G))
  rule #GroupToJson(G, true)  => #KeyFromGroup(G) : "{" +String #unparse(#GroupContent(G)) +String "}"

  rule <k> Gs:Groups => . </k>
       <out> ... .List => ListItem(JSON2String(#GroupsToJson(Gs))) </out>
       <returncode> 1 => 0 </returncode>
endmodule
```

Input Creator
-------------

```k
module INPUT-CREATOR-SYNTAX
  imports UNIT-TEST-SYNTAX
endmodule

module INPUT-CREATOR
  imports INPUT-CREATOR-SYNTAX
  imports COMPAT-COMMON

  syntax Data ::= #DataForType(Type) [function]
  rule #DataForType(int _) => 0
  rule #DataForType(nat _) => 0
  rule #DataForType(string _) => ""
  rule #DataForType(bytes _) => 0x
  rule #DataForType(mutez _) => 0
  rule #DataForType(bool _) => true
  rule #DataForType(key_hash _) => "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"
  rule #DataForType(timestamp _) => 0
  rule #DataForType(address _) => "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV"

//  rule #DataForType(key _)
  rule #DataForType(unit _) => Unit
//  rule #DataForType(signature _)
//  rule #DataForType(operation _)
//  rule #DataForType(chain_id _)

  rule #DataForType(pair _ T1:Type T2:Type) => Pair #DataForType(T1) #DataForType(T2)

  rule #DataForType(option _ _) => None
  rule #DataForType(list _ _) => { }
  rule #DataForType(set _ _) => { }
//  rule #DataForType(contract _ _) => None
  rule #DataForType(or _ T1 _) => #DataForType(T1)
  rule #DataForType(lambda _ _ T2) => { DROP .AnnotationList ; PUSH .AnnotationList T2 #DataForType(T2) }
  rule #DataForType(map _ _ _) => { }
  rule #DataForType(big_map _ _ _) => { }

  syntax Type ::= #FindParamType(Pgm) [function]
  rule #FindParamType(parameter T) => T
  rule #FindParamType(parameter T;) => T
  rule #FindParamType(parameter T ; Rs) => T
  rule #FindParamType(_:Group) => unit .AnnotationList [owise]
  rule #FindParamType(_:Group;) => unit .AnnotationList [owise]
  rule #FindParamType(_ ; Rs) => #FindParamType(Rs) [owise]

  rule <k> G:Pgm => . </k>
       <out> ... .List => ListItem(#unparse(#DataForType(#FindParamType(G)))) </out>
endmodule
```

Output Compare
--------------

```k
module OUTPUT-COMPARE-SYNTAX
  imports UNIT-TEST-SYNTAX
  syntax RealOutputGroup ::= "real_output" OutputStack
  syntax Group ::= RealOutputGroup
endmodule

module OUTPUT-COMPARE
  imports OUTPUT-COMPARE-SYNTAX
  imports COMPAT-COMMON
  imports K-REFLECTION
  imports MATCHER

  syntax String ::= #decodeBinaryRepresentation(Bytes) [function, hook(MICHELSON.decode)]
  syntax BlockchainOperation ::= #parseOperation(String) [function]
  rule #parseOperation(S) => #parseKORE(S)

  syntax KItem ::= #CheckOutput(OutputStack, OutputStack) // Expected, Actual

  rule #MichelineToNative(B:Bytes, operation _, KnownAddrs, BigMaps)
    => #MichelineToNative(#parseOperation(#decodeBinaryRepresentation(B)),
                          operation .AnnotationList,
                          KnownAddrs,
                          BigMaps)

  syntax KItem ::= "#Failed"

  rule <k> #CheckOutput( X:FailedStack , X:FailedStack ) => . ... </k>

  rule <k> #CheckOutput( { .StackElementList } , { .StackElementList } ) => . ... </k>

  rule <k> #CheckOutput( { Stack_elt ET ED } , { Stack_elt AT AD } )
        => .
           ...
       </k>
       <knownaddrs> KnownAddrs </knownaddrs>
    requires #Matches(#MichelineToNative(ED, ET, KnownAddrs, .Map),
                      #MichelineToNative(AD, AT, KnownAddrs, .Map))

  rule <k> #CheckOutput( { Stack_elt ET ED } , { Stack_elt AT AD } )
        => #Failed
           ...
       </k>
       <knownaddrs> KnownAddrs </knownaddrs>
       <out> ...
             .List
          => ListItem("Mismatch - Expected: " +String #unparse(#MichelineToNative(ED, ET, KnownAddrs, .Map)) +String
                                  " Actual: " +String #unparse(#MichelineToNative(AD, AT, KnownAddrs, .Map))
                     )
       </out> [owise]

  rule <k> #CheckOutput( { Stack_elt ET ED ; Es } , { Stack_elt AT AD ; As } )
        => #CheckOutput( { Es } , { As } )
           ...
       </k>
       <knownaddrs> KnownAddrs </knownaddrs>
    requires #Matches(#MichelineToNative(ED, ET, KnownAddrs, .Map), #MichelineToNative(AD, AT, KnownAddrs, .Map))

  rule <k> #CheckOutput( { Stack_elt ET ED ; Es } , { Stack_elt AT AD ; As } )
        => #Failed
           ...
       </k>
       <knownaddrs> KnownAddrs </knownaddrs>
       <out> ...
             .List
          => ListItem("Mismatch - Expected: " +String #unparse(#MichelineToNative(ED, ET, KnownAddrs, .Map))
                          +String " Actual: " +String #unparse(#MichelineToNative(AD, AT, KnownAddrs, .Map))
                     )
       </out> [owise]

  rule <k> other_contracts { M }; Gs => Gs </k>
       <knownaddrs> _ => #OtherContractsMapEntryListToKMap(M) </knownaddrs>

  rule <k> real_output AOS ; output EOS ; => #CheckOutput(EOS, AOS) ... </k>
endmodule
```
