requires "michelson-syntax.k"
requires "michelson-config.k"
requires "michelson-internal-syntax.k"

module MICHELSON
  imports MICHELSON-SYNTAX
  imports MICHELSON-CONFIG
  imports MICHELSON-INTERNAL-SYNTAX
  imports DOMAINS 
  imports COLLECTIONS

  syntax String ::= #ToLowerCase(String) [function, functional, hook(STRING.tolowercase)]

  syntax Data ::= List | Set | Map
  syntax Data ::= #Lambda(Type, Type, Block)

  syntax OperationNonce ::= #NextNonce(OperationNonce) [function]
  rule #NextNonce(#Nonce(I)) => #Nonce(I +Int 1)

  syntax Data ::= #ConcreteArgToSemantics(Data, Type) [function] 

  syntax KeyHash ::= #ParseKeyHash(String) [function]
  rule #ParseKeyHash(S) => #KeyHash(S)

  syntax Int ::= #ISO2Epoch(String) [function, hook(TIME.ISO2Epoch)]

  syntax Timestamp ::= #ParseTimestamp(String) [function]
  rule #ParseTimestamp(S) => #Timestamp(#ISO2Epoch(S)) requires findString(S, "Z", 0) >=Int 0
  rule #ParseTimestamp(S) => #Timestamp(String2Int(S)) requires findString(S, "Z", 0) <Int 0

  syntax Address ::= #ParseAddress(String) [function]
  rule #ParseAddress(S) => #Address(S)

  syntax Key ::= #ParseKey(String) [function]
  rule #ParseKey(S) => #Key(S)

  syntax Signature ::= #ParseSignature(String) [function]
  rule #ParseSignature(S) => #Signature(S)

  syntax String ::= #CDARToString(CDARMacro) [function, functional, hook(STRING.token2string)]
  syntax String ::= #MBytesToString(MBytesLiteral) [function, functional, hook(STRING.token2string)]

  syntax String ::= #MBytesContent(MBytesLiteral) [function, functional]
  rule #MBytesContent(H) => substrString(#MBytesToString(H), 2, lengthString(#MBytesToString(H)))

  syntax Int ::= #MBytesToInt(MBytesLiteral) [function]

  rule #MBytesToInt(H) => String2Base(#MBytesContent(H), 16) requires lengthString(#MBytesContent(H)) >Int 0
  rule #MBytesToInt(H) => 0 requires lengthString(#MBytesContent(H)) ==Int 0

  syntax MBytesLiteral ::= #StringToMBytes(String) [function, hook(STRING.string2token)]

  syntax MBytesLiteral ::= #IntToMBytes(Int) [function]
  rule #IntToMBytes(I) => #StringToMBytes("0x" +String Base2String(I, 16))

  rule #ConcreteArgToSemantics(H:MBytesLiteral, chain_id _) => #ChainId(#MBytesToInt(H))
  rule #ConcreteArgToSemantics(I:Int, int _) => I
  rule #ConcreteArgToSemantics(I:Int, nat _) => I requires I >=Int 0
  rule #ConcreteArgToSemantics(S:String, string _) => S
  rule #ConcreteArgToSemantics(B:MBytesLiteral, bytes _) => #StringToMBytes(#ToLowerCase(#MBytesToString(B)))
  rule #ConcreteArgToSemantics(I:Int, mutez _) => #Mutez(I)
  rule #ConcreteArgToSemantics(B:Bool, bool _) => B
  rule #ConcreteArgToSemantics(S:String, key_hash _) => #ParseKeyHash(S)
  rule #ConcreteArgToSemantics(I:Int, timestamp _) => #Timestamp(I)
  rule #ConcreteArgToSemantics(S:String, timestamp _) => #ParseTimestamp(S)
  rule #ConcreteArgToSemantics(S:String, address _) => #ParseAddress(S)
  rule #ConcreteArgToSemantics(S:String, key _) => #ParseKey(S)
  rule #ConcreteArgToSemantics(Unit, unit _) => Unit
  rule #ConcreteArgToSemantics(S:String, signature _) => #ParseSignature(S)
  
  rule #ConcreteArgToSemantics(Pair A B, pair _ T1:Type T2:Type) => 
       Pair #ConcreteArgToSemantics(A, T1) #ConcreteArgToSemantics(B, T2)

  rule #ConcreteArgToSemantics(Some V, option _ T) => Some #ConcreteArgToSemantics(V, T)
  rule #ConcreteArgToSemantics(None, option _:AnnotationList _) => None

  syntax Data ::= #ConcreteArgToSemanticsList(DataList, Type, List) [function]

  rule #ConcreteArgToSemantics({ }, list _ _) => .List

  rule #ConcreteArgToSemantics({ DL:DataList }, list _ T) =>
       #ConcreteArgToSemanticsList(DL, T, .List)

  rule #ConcreteArgToSemanticsList(D1 ; DL, T, L) =>
       #ConcreteArgToSemanticsList(DL, T, ListItem(#ConcreteArgToSemantics(D1, T)) L)

  syntax List ::= #ReverseList(List) [function]
  syntax List ::= #ReverseListAux(List, List) [function]

  rule #ReverseList(L) => #ReverseListAux(L, .List)
  rule #ReverseListAux(ListItem(L1) Ls, Acc) => #ReverseListAux(Ls, ListItem(L1) Acc)
  rule #ReverseListAux(.List, Acc) => Acc

  rule #ConcreteArgToSemanticsList(D1, T, L) => #ReverseList(ListItem(#ConcreteArgToSemantics(D1, T)) L)

  syntax Data ::= #ConcreteArgToSemanticsSet(DataList, Type, Set) [function]

  rule #ConcreteArgToSemantics({ }, set _ _) => .Set

  rule #ConcreteArgToSemantics({ DL:DataList }, set _ T) =>
       #ConcreteArgToSemanticsSet(DL, T, .Set)

  rule #ConcreteArgToSemanticsSet(D1 ; DL, T, S) =>
       #ConcreteArgToSemanticsSet(DL, T, SetItem(#ConcreteArgToSemantics(D1, T)) S)

  rule #ConcreteArgToSemanticsSet(D, T, S) => SetItem(#ConcreteArgToSemantics(D, T)) S

  rule #ConcreteArgToSemantics(S:String, contract _ T) => #Contract(#ParseAddress(S), T)
  
  rule #ConcreteArgToSemantics(Left V, or _:AnnotationList TL:Type _:Type) => Left #ConcreteArgToSemantics(V, TL)
  rule #ConcreteArgToSemantics(Right V, or _:AnnotationList _:Type TR:Type) => Right #ConcreteArgToSemantics(V, TR)

  rule #ConcreteArgToSemantics(B:Block, lambda _:AnnotationList T1 T2) => #Lambda(T1, T2, B)

  syntax Data ::= #ConcreteArgToSemanticsMap(MapEntryList, Type, Type, Map) [function]

  rule #ConcreteArgToSemantics({ }, map _ K V) => .Map

  rule #ConcreteArgToSemantics({ M:MapEntryList }, map _:AnnotationList K V) =>
       #ConcreteArgToSemanticsMap(M, K, V, .Map)

  rule #ConcreteArgToSemanticsMap(Elt K V ; ML, KT, VT, M) =>
       #ConcreteArgToSemanticsMap(ML, KT, VT, M[#ConcreteArgToSemantics(K, KT) <- #ConcreteArgToSemantics(V, VT)])

  rule #ConcreteArgToSemanticsMap(Elt K V, KT, VT, M) => M[#ConcreteArgToSemantics(K, KT) <- #ConcreteArgToSemantics(V, VT)]

  rule #ConcreteArgToSemantics({ }, big_map _:AnnotationList K V) => .Map

  rule #ConcreteArgToSemantics({ M:MapEntryList }, big_map _:AnnotationList K V) =>
       #ConcreteArgToSemanticsMap(M, K, V, .Map)

  syntax Type ::= #ParameterTypeFromContract(Contract) [function]
  rule #ParameterTypeFromContract(code _ ; storage _ ; parameter P ;) => P

  syntax Type ::= #StorageTypeFromContract(Contract) [function]
  rule #StorageTypeFromContract(code _ ; storage P ; parameter _ ;) => P

  rule #ConcreteArgToSemantics(Create_contract(N, C, O, M, S), operation _) =>
       Create_contract(
           N, 
           C, 
           {#ConcreteArgToSemantics(O, (option .AnnotationList  key_hash .AnnotationList))}:>OptionData,
           {#ConcreteArgToSemantics(M, mutez .AnnotationList)}:>Mutez, 
           #ConcreteArgToSemantics(S, #StorageTypeFromContract(C))
       )

  rule #ConcreteArgToSemantics(Set_delegate(N, K), operation _) => 
       Set_delegate(N, {#ConcreteArgToSemantics(K, (option .AnnotationList key_hash .AnnotationList))}:>OptionData)

  syntax Type ::= #TypeFromContract(ContractData) [function]

  rule #TypeFromContract(#Contract(_, T)) => T

  rule [[ #ConcreteArgToSemantics(Transfer_tokens(N, P, M, A), operation _) 
          => Transfer_tokens(N, 
              #ConcreteArgToSemantics(
                  P, 
                  #TypeFromContract({Known[#ConcreteArgToSemantics(A, address .AnnotationList)]}:>ContractData)
              ), 
              {#ConcreteArgToSemantics(M, mutez .AnnotationList)}:>Mutez, 
              {#ConcreteArgToSemantics(A, address .AnnotationList)}:>Address
          ) ]]
       <knownaddrs> Known </knownaddrs>
       requires #ConcreteArgToSemantics(A, address .AnnotationList) in_keys(Known)

  rule [[ #ConcreteArgToSemantics(I:Int, big_map _:AnnotationList K V) => {M[I]}:>Data ]]
       <bigmaps> M:Map </bigmaps>

  rule code B ; storage St ; parameter Pt => code B ; storage St ; parameter Pt ; [macro]
  rule code B ; parameter Pt ; storage St => code B ; storage St ; parameter Pt ; [macro]
  rule storage St ; code B ; parameter Pt => code B ; storage St ; parameter Pt ; [macro]
  rule parameter Pt ; code B ; storage St => code B ; storage St ; parameter Pt ; [macro]
  rule storage St ; parameter Pt ; code B => code B ; storage St ; parameter Pt ; [macro]
  rule parameter Pt ; storage St ; code B => code B ; storage St ; parameter Pt ; [macro]

  rule code B ; parameter Pt ; storage St ; => code B ; storage St ; parameter Pt ; [macro]
  rule storage St ; code B ; parameter Pt ; => code B ; storage St ; parameter Pt ; [macro]
  rule parameter Pt ; code B ; storage St ; => code B ; storage St ; parameter Pt ; [macro]
  rule storage St ; parameter Pt ; code B ; => code B ; storage St ; parameter Pt ; [macro]
  rule parameter Pt ; storage St ; code B ; => code B ; storage St ; parameter Pt ; [macro]

//  syntax Int ::= #GroupOrder(Group) [function] // In michelson-common.k
  rule #GroupOrder(_:ContractGroup) => #GroupOrderMax
  rule #GroupOrder(_:ParameterValueGroup) => #GroupOrderMax -Int 1
  rule #GroupOrder(_:StorageValueGroup) => #GroupOrderMax -Int 2

  rule #GroupOrder(_:NowGroup) => 0
  rule #GroupOrder(_:SenderGroup) => 1
  rule #GroupOrder(_:SourceGroup) => 2
  rule #GroupOrder(_:ChainGroup) => 3
  rule #GroupOrder(_:SelfGroup) => 4
  rule #GroupOrder(_:AmountGroup) => 5
  rule #GroupOrder(_:BalanceGroup) => 6
  rule #GroupOrder(_:BigMapGroup) => 7
  rule #GroupOrder(_:ContractsGroup) => 8
  rule #GroupOrder(_:ParameterDecl) => 9
  rule #GroupOrder(_:StorageDecl) => 10

  syntax Group ::= #GetMinOfTwoGroups(Group, Group) [function]
  rule #GetMinOfTwoGroups(G1, G2) => G1 requires #GroupOrder(G1) <Int #GroupOrder(G2)
  rule #GetMinOfTwoGroups(G1, G2) => G2 [owise]

  syntax Group ::= #GetMinGroup(Groups) [function]
  rule #GetMinGroup(G:Group) => G
  rule #GetMinGroup(G ; Gs) => #GetMinGroupAux(Gs, G)

  syntax Group ::= #GetMinGroupAux(Groups, Group) [function]
  rule #GetMinGroupAux(G1, G2) => #GetMinOfTwoGroups(G1, G2)
  rule #GetMinGroupAux(G1 ; Gs, G2) => #GetMinGroupAux(Gs, #GetMinOfTwoGroups(G1, G2))

  syntax Groups ::= #RemoveGroup(Groups, Group) [function]
  rule #RemoveGroup(G1:Group ; G2:Group, G:Group) => G1 ; G2 requires G =/=K G1 andBool G =/=K G2
  rule #RemoveGroup(G1:Group ; G2:Group, G1) => G2
  rule #RemoveGroup(G1:Group ; G2:Group, G2) => G1
  rule #RemoveGroup(G1:Group ; G2:Group ; Gs:Groups, G1) => G2 ; Gs
  rule #RemoveGroup(G ; Gs:Groups, Go:Group) => G ; #RemoveGroup(Gs, Go) [owise]

  syntax Groups ::= #ReorderGroups(Groups) [function, functional]
  rule #ReorderGroups(G:Group) => G
  rule #ReorderGroups(G ; Gs) => #GetMinGroup(G ; Gs) ; #ReorderGroups(#RemoveGroup(G ; Gs, #GetMinGroup(G ; Gs))) 

  syntax Contract ::= #FindContract(Groups) [function]
  rule #FindContract(contract { C }) => C
  rule #FindContract(contract { C } ; _) => C
  rule #FindContract(_ ; Gs) => #FindContract(Gs) [owise]

  syntax Group ::= #MakeParameterGroup(Groups) [function]
  rule #MakeParameterGroup(G) => parameter #ParameterTypeFromContract(#FindContract(G))

  syntax Group ::= #MakeStorageGroup(Groups) [function]
  rule #MakeStorageGroup(G) => storage #StorageTypeFromContract(#FindContract(G))

  syntax Bool ::= #HasContract(Groups) [function]
  rule #HasContract(contract { C }) => true
  rule #HasContract(contract { C } ; _) => true
  rule #HasContract(_ ; Gs) => #HasContract(Gs) [owise]
  rule #HasContract(_:Group) => false [owise]

  syntax Groups ::= #ExtendGroups(Groups) [function]

  rule #ExtendGroups(Gs) => #MakeParameterGroup(Gs) ; #MakeStorageGroup(Gs) ; Gs requires #HasContract(Gs)
  rule #ExtendGroups(Gs) => Gs [owise]

//  syntax KItem ::= #LoadGroups(Groups) // In michelson-common.k

  rule <k> G:Groups => #LoadGroups(#ReorderGroups(#ExtendGroups(G))) </k> [owise] 

  rule <k> #LoadGroups(now I ; Gs => Gs) </k>
       <mynow> #Timestamp(0 => I) </mynow>

  rule <k> #LoadGroups(sender A ; Gs => Gs) </k>
       <senderaddr> #Address("InvalidSenderAddr" => A) </senderaddr>

  rule <k> #LoadGroups(source A ; Gs => Gs) </k>
       <sourceaddr> #Address("InvalidSourceAddr" => A) </sourceaddr>

  rule <k> #LoadGroups(chain_id M ; Gs => Gs) </k>
       <mychainid> #ChainId(0 => #MBytesToInt(M)) </mychainid>

  rule <k> #LoadGroups(self A ; Gs => Gs) </k>
       <myaddr> #Address("InvalidMyAddr" => A) </myaddr>

  syntax Int ::= "#MutezOverflowLimit" [function]
  rule #MutezOverflowLimit => 2 ^Int 63 // Signed 64 bit integers.

  syntax Bool ::= #IsLegalMutezValue(Int) [function]
  rule #IsLegalMutezValue(I) => I >=Int 0 andBool I <Int #MutezOverflowLimit

  rule <k> #LoadGroups(amount I ; Gs => Gs) </k>
       <myamount> #Mutez(0 => I) </myamount>
       requires #IsLegalMutezValue(I)

  rule <k> #LoadGroups(balance I ; Gs => Gs) </k>
       <mybalance> #Mutez(0 => I) </mybalance>
       requires #IsLegalMutezValue(I)

  syntax Map ::= #OtherContractsMapToKMap(OtherContractsMap) [function]
  syntax Map ::= #OtherContractsMapEntryListToKMap(OtherContractsMapEntryList) [function]
  rule #OtherContractsMapToKMap({ }) => .Map
  rule #OtherContractsMapToKMap({ EL }) => #OtherContractsMapEntryListToKMap(EL)
  rule #OtherContractsMapEntryListToKMap( Elt A T ) => #Address(A) |-> #Contract(#Address(A), T)
  rule #OtherContractsMapEntryListToKMap( Elt A T ; Rs ) => #Address(A) |-> #Contract(#Address(A), T) #OtherContractsMapEntryListToKMap(Rs)

  rule <k> #LoadGroups(other_contracts M ; Gs => Gs) </k>
       <knownaddrs> .Map => #OtherContractsMapToKMap(M) </knownaddrs>

  rule <k> #LoadGroups(parameter T ; Gs => Gs) </k>
       <paramtype> #NotSet => T </paramtype>

  rule <k> #LoadGroups(storage T ; Gs => Gs) </k>
       <storagetype> #NotSet => T </storagetype>

  syntax Map ::= #BigMapsToKMap(BigMapMap) [function]
  syntax Map ::= #BigMapsEntryListToKMap(BigMapEntryList) [function] 
  syntax Map ::= #BigMapsEntryToKMap(BigMapEntry) [function]

  rule #BigMapsToKMap({ }) => .Map
  rule #BigMapsToKMap({ EL }) => #BigMapsEntryListToKMap(EL)

  rule #BigMapsEntryListToKMap(E) => #BigMapsEntryToKMap(E)
  rule #BigMapsEntryListToKMap(E ; Es) => #BigMapsEntryToKMap(E) #BigMapsEntryListToKMap(Es)
 
  rule #BigMapsEntryToKMap(Big_map I T1 T2 { }) => 
    I |-> #ConcreteArgToSemantics({ }, big_map .AnnotationList T1 T2)

  rule #BigMapsEntryToKMap(Big_map I T1 T2 ML:MapLiteral) => 
    I |-> #ConcreteArgToSemantics(ML, big_map .AnnotationList T1 T2)

  rule <k> #LoadGroups(big_maps M ; Gs => Gs) </k>
       <bigmaps> .Map => #BigMapsToKMap(M) </bigmaps>

  rule <k> #LoadGroups(parameter_value D ; Gs => Gs) </k>
       <paramtype> T </paramtype>
       <paramvalue> #NoData => #ConcreteArgToSemantics(D, T) </paramvalue>

  rule <k> #LoadGroups(storage_value D ; Gs => Gs) </k>
       <storagetype> T </storagetype>
       <storagevalue> #NoData => #ConcreteArgToSemantics(D, T) </storagevalue>

  rule <k> #LoadGroups(contract { code C ; storage _ ; parameter _ }) => C </k>
       <stack> . => Pair P S </stack>
       <paramvalue> P </paramvalue>
       <storagevalue> S </storagevalue>
       <returncode> _ => 0 </returncode>

  rule I:Instruction ; Is:InstructionList => I ~> Is 
  rule {} => .K [structrual]
  rule { Is:InstructionList } => Is 
  rule { Is:InstructionList ; } => { Is } [macro]

  syntax KItem ::= #HandleAnnotations(AnnotationList)
  rule #HandleAnnotations(_) => . 

  syntax Error ::= Aborted(String, KItem, K, K)

  // Core Instructioons
  //// Control Structures
  rule <k> FAILWITH A ~> Rk => #HandleAnnotations(A) ~> Aborted("FAILWITH instruction reached", D, Rk, Rs) </k>
       <stack> D ~> Rs => ( Failed D ) </stack>
       <returncode> _ => 1 </returncode>

  rule <k> IF A BT BF => #HandleAnnotations(A) ~> BT ... </k>
       <stack> true => . ... </stack>

  rule <k> IF A BT BF => #HandleAnnotations(A) ~> BF ... </k>
       <stack> false => . ... </stack>

  rule <k> LOOP A B => #HandleAnnotations(A) ~> B ~> LOOP .AnnotationList B ... </k>
       <stack> true => . ... </stack>

  rule <k> LOOP A B => #HandleAnnotations(A) ... </k>
       <stack> false => . ... </stack>

  rule <k> LOOP_LEFT A B => #HandleAnnotations(A) ~> B ~> LOOP_LEFT .AnnotationList B ... </k>
       <stack> Left D => D ... </stack>

  rule <k> LOOP_LEFT A B => #HandleAnnotations(A) ... </k>
       <stack> Right D => D ... </stack>

  syntax KItem ::= #Push(Data)
  rule <k> #Push(D) => . ... </k>
       <stack> . => D ... </stack> 

  rule <k> DIP A B => #HandleAnnotations(A) ~> B ~> #Push(D) ... </k>
       <stack> D:Data => . ... </stack>

  rule <k> DIP A 0 B => #HandleAnnotations(A) ~> B ... </k>

  rule <k> DIP A I B => #HandleAnnotations(A) ~> DIP .AnnotationList { DIP .AnnotationList  I -Int 1 B } ... </k>
       requires I >Int 0

  syntax KItem ::= #ReturnStack(K)

  rule <k> #ReturnStack(Ls) => . ... </k>
       <stack> R:Data => R ~> Ls </stack>

  rule <k> EXEC B => #HandleAnnotations(B) ~> C ~> #ReturnStack(Rs) ... </k>
       <stack> A:Data ~> #Lambda(_, _, C):Data ~> Rs:K => A </stack> 

  rule <k> APPLY A => #HandleAnnotations(A) ... </k>
       <stack> D:Data ~> #Lambda((pair _:AnnotationList T0 T1):Type, T2, { C } ) => #Lambda(T1, T2, { PUSH .AnnotationList T0 D ; PAIR .AnnotationList ; { C } } ) ... </stack> 

  ////Stack operations

  rule <k> DROP A =>  #HandleAnnotations(A) ... </k>
       <stack> _:Data => . ... </stack>

  rule <k> DROP A I => #HandleAnnotations(A) ~> DROP .AnnotationList ~> DROP .AnnotationList I -Int 1 ... </k> 
       requires I >Int 0

  rule <k> DROP A 0 => #HandleAnnotations(A) ... </k> 


  rule <k> DUP A => #HandleAnnotations(A) ... </k>
       <stack> X:Data => X ~> X ... </stack>

  rule <k> SWAP A => #HandleAnnotations(A) ... </k>
       <stack> X:Data ~> Y:Data => Y ~> X ... </stack>

  syntax KItem ::= #DoDig(Int, K, OptionData)

  rule <k> DIG A I => #HandleAnnotations(A) ~> #DoDig(I, .K, None) ... </k>
       <stack> S </stack> 

  rule <k> #DoDig(I, A, None) => #DoDig(I -Int 1, F ~> A, None) ... </k>
       <stack> F:Data => . ... </stack>
       requires I >Int 0 

  rule <k> #DoDig(0, A, None) => #DoDig(-1, A, Some F) ... </k>
       <stack> F:Data => . ... </stack> 

  rule <k> #DoDig(-1, F:Data ~> A, Some T) => #DoDig(-1, A, Some T) ... </k>
       <stack> . => F ... </stack> 

  rule <k> #DoDig(-1, .K, Some T) => . ... </k>
       <stack> . => T ... </stack>

  syntax KItem ::= #DoDug(Int, K, Data)

  rule <k> DUG A I => #HandleAnnotations(A) ~> #DoDug(I, .K, T) ... </k>
       <stack> T => .K ... </stack> 

  rule <k> #DoDug(I, S, R) => #DoDug(I -Int 1, T ~> S, R) ... </k>
       <stack> T:Data => .K ... </stack>
       requires I >Int 0 

  rule <k> #DoDug(0, S, R) => #DoDug(-1, S, R) ... </k>
       <stack> .K => R ... </stack> 

  rule <k> #DoDug(-1, T:Data ~> S, R) => #DoDug(-1, S, R) ... </k>
       <stack> .K => T ... </stack> 

  rule <k> #DoDug(-1, .K, _) => .K ... </k>

  rule <k> PUSH A T X => #HandleAnnotations(A) ... </k>
       <stack> . => #ConcreteArgToSemantics(X, T) ... </stack>

  rule <k> UNIT A => #HandleAnnotations(A) ... </k>
       <stack> . => Unit ... </stack>

  rule <k> LAMBDA A T1 T2 C => #HandleAnnotations(A) ... </k>
       <stack> . => #Lambda(T1, T2, C) ... </stack>

  //// Generic Comparisons

  rule <k> EQ A => #HandleAnnotations(A) ... </k>
       <stack> I => I ==Int 0 ... </stack>

  rule <k> NEQ A => #HandleAnnotations(A) ... </k>
       <stack> I => I =/=Int 0 ... </stack>

  rule <k> LT A => #HandleAnnotations(A) ... </k>
       <stack> I => I <Int 0 ... </stack>

  rule <k> GT A => #HandleAnnotations(A) ... </k>
       <stack> I => I >Int 0 ... </stack>

  rule <k> LE A => #HandleAnnotations(A) ... </k>
       <stack> I => I <=Int 0 ... </stack>

  rule <k> GE A => #HandleAnnotations(A) ... </k>
       <stack> I => I >=Int 0 ... </stack>

  // Operations
  //// Operations on booleans
  rule <k> OR A => #HandleAnnotations(A) ... </k>
       <stack> B1 ~> B2 => B1 orBool B2 ...  </stack>

  rule <k> AND A => #HandleAnnotations(A) ... </k>
       <stack> B1 ~> B2 => B1 andBool B2 ... </stack>

  rule <k> XOR A => #HandleAnnotations(A) ... </k>
       <stack> B1 ~> B2 => B1 xorBool B2 ... </stack>

  rule <k> NOT A => #HandleAnnotations(A) ... </k>
       <stack> B => notBool B ... </stack>

  //// Operations on integers and natural numbers 
  rule <k> NEG A => #HandleAnnotations(A) ... </k>
       <stack> I => 0 -Int I ... </stack>

  rule <k> ABS A => #HandleAnnotations(A) ... </k>
       <stack> I => absInt(I) ... </stack>

  rule <k> ISNAT A => #HandleAnnotations(A) ... </k>
       <stack> I => Some I ... </stack>
       requires I >=Int 0 

  rule <k> ISNAT A => #HandleAnnotations(A) ... </k>
       <stack> I => None ... </stack>
       requires I <Int 0 

  rule <k> INT A => #HandleAnnotations(A) ... </k>
       <stack> I:Int ... </stack>

  rule <k> ADD A => #HandleAnnotations(A) ... </k>
       <stack> I1 ~> I2 => I1 +Int I2 ... </stack>

  rule <k> SUB A => #HandleAnnotations(A) ... </k>
       <stack> I1 ~> I2 => I1 -Int I2 ... </stack>

  rule <k> MUL A => #HandleAnnotations(A) ... </k>
       <stack> I1 ~> I2 => I1 *Int I2 ... </stack>

  rule <k> EDIV A => #HandleAnnotations(A) ... </k>
       <stack> I1:Int ~> 0 => None ... </stack>
       // Could combine this rule with the Mutez one but probably a disadvantage in readability.

  rule <k> EDIV A  => #HandleAnnotations(A) ... </k>
       <stack> I1 ~> I2 => Some (Pair (I1 /Int I2) (I1 %Int I2)) ... </stack>
       requires I2 =/=Int 0

  rule <k> OR A => #HandleAnnotations(A)  ... </k>
       <stack> I1 ~> I2 => I1 |Int I2 ... </stack>

  rule <k> AND A => #HandleAnnotations(A) ... </k>
       <stack> I1 ~> I2 => I1 &Int I2 ... </stack>

  rule <k> XOR A => #HandleAnnotations(A) ... </k>
       <stack> I1 ~> I2 => I1 xorInt I2 ... </stack>

  rule <k> NOT A => #HandleAnnotations(A) ... </k>
       <stack> I => ~Int I ... </stack>

  rule <k> LSL A => #HandleAnnotations(A) ... </k>
       <stack> X ~> S => X <<Int S ... </stack>
       requires S <=Int 256

  rule <k> LSL A ~> Rk => #HandleAnnotations(A) ~> Aborted("LSL out of range", S, Rk, Rs) </k>
       <stack> C:Int ~> S:Int ~> Rs => ( GeneralOverflow C S )  </stack>
       requires S >Int 256

  rule <k> LSR A => #HandleAnnotations(A) ... </k>
       <stack> X ~> S => X >>Int S ... </stack>
       requires S <=Int 256

  rule <k> LSR A ~> Rk => #HandleAnnotations(A) ~> Aborted("LSR out of range", S, Rk, Rs) </k>
       <stack> C:Int ~> S:Int ~> Rs => ( GeneralOverflow C S ) </stack>
       requires S >Int 256

  syntax Int ::= #DoCompare(Data, Data) [function]

  rule #DoCompare(I1:Int, I2:Int) => -1 requires I1 <Int I2
  rule #DoCompare(I1:Int, I2:Int) => 0 requires I1 ==Int I2
  rule #DoCompare(I1:Int, I2:Int) => 1 requires I1 >Int I2

  rule #DoCompare(S1:String, S2:String) => -1 requires S1 <String S2
  rule #DoCompare(S1:String, S2:String) => 0 requires S1 ==String S2
  rule #DoCompare(S1:String, S2:String) => 1 requires S1 >String S2

  rule #DoCompare((Pair A1 A2), (Pair B1 B2)) => -1                 requires #DoCompare(A1, B1) ==Int -1
  rule #DoCompare((Pair A1 A2), (Pair B1 B2)) => #DoCompare(A2, B2) requires #DoCompare(A1, B1) ==Int 0
  rule #DoCompare((Pair A1 A2), (Pair B1 B2)) => 1                  requires #DoCompare(A1, B1) ==Int 1

  rule <k> COMPARE A => #HandleAnnotations(A) ... </k>
       <stack> V1 ~> V2 => #DoCompare(V1, V2) ... </stack>

  syntax String ::= #ConcatAll(List, String) [function]

  rule #ConcatAll(.List, A) => A
  rule #ConcatAll(ListItem(S1) DL, A) => #ConcatAll(DL, A +String S1)
  rule #ConcatAll(ListItem(B1:MBytesLiteral) DL, A) => #ConcatAll(DL, A +String #MBytesContent(B1)) requires lengthString(A) >Int 0

  //// Operations on strings
  rule <k> CONCAT A => #HandleAnnotations(A) ... </k>
       <stack> S1 ~> S2 => S1 +String S2 ... </stack>

  rule <k> CONCAT A => #HandleAnnotations(A) ... </k>
       <stack> .List => "" ... </stack>

  rule <k> CONCAT A => #HandleAnnotations(A) ... </k>
       <stack> L => #ConcatAll(L, "") ... </stack>

  rule <k> SIZE A => #HandleAnnotations(A) ... </k>
       <stack> S => lengthString(S) ... </stack> 

  syntax OptionData ::= #SliceString(String, Int, Int) [function]

  rule #SliceString(S, O, L) => Some substrString(S, O, O +Int L)
  requires O >=Int 0 andBool L >=Int 0 andBool (O +Int L) <=Int lengthString(S)

  rule #SliceString(S, O, L) => None [owise]

  rule <k> SLICE A => #HandleAnnotations(A) ... </k>
       <stack> O ~> L ~> S => #SliceString(S, O, L)  ... </stack>

  //// Operations on pairs
  rule <k> PAIR A => #HandleAnnotations(A) ... </k>
       <stack> L ~> R => Pair L R ... </stack>

  rule <k> CAR A => #HandleAnnotations(A) ... </k>
       <stack> Pair L _ => L ... </stack>

  rule <k> CDR A => #HandleAnnotations(A) ... </k>
       <stack> Pair _ R => R ... </stack>

  //// Operations on sets
  syntax Data ::= Set

  rule <k> EMPTY_SET A _ => #HandleAnnotations(A) ... </k>
       <stack> . => .Set ... </stack>

  rule <k> MEM A => #HandleAnnotations(A) ... </k> 
       <stack> X ~> S:Set => X in S ... </stack>

  // True to insert, False to remove.

  rule <k> UPDATE A => #HandleAnnotations(A) ... </k>
       <stack> D ~> true ~> S => SetItem(D) S ... </stack>

  rule <k> UPDATE A => #HandleAnnotations(A) ... </k>
       <stack> D ~> false ~> SetItem(D) S => S ... </stack>

  rule <k> UPDATE A => #HandleAnnotations(A) ... </k>
       <stack> (D ~> false => .) ~> S:Set ... </stack>
       requires notBool(D in S)

  syntax Data ::= #MinimalElement(List) [function]
  syntax Data ::= #MinimalElementAux(List, Data) [function]

  rule #MinimalElement(ListItem(H) L) => #MinimalElementAux(L, H)
  rule #MinimalElementAux(.List, M) => M
  rule #MinimalElementAux(ListItem(H) L, M) => #MinimalElementAux(L, M) requires #DoCompare(M, H) <=Int 0
  rule #MinimalElementAux(ListItem(H) L, M) => #MinimalElementAux(L, H) requires #DoCompare(M, H) ==Int 1

  rule <k> ITER A _ => #HandleAnnotations(A) ... </k>
       <stack> .Set => . ... </stack>

  rule <k> ITER A B => #HandleAnnotations(A) ~> B ~> #Push(S -Set SetItem(#MinimalElement(Set2List(S)))) ~> ITER .AnnotationList B ... </k>
       <stack> S => #MinimalElement(Set2List(S)) ... </stack>
       requires size(S) >Int 0

  rule <k> SIZE A => #HandleAnnotations(A) ... </k>
       <stack> S:Set => size(S) ... </stack>

  //// Operations on maps
  rule <k> EMPTY_MAP A _ _ => #HandleAnnotations(A) ... </k>
       <stack> . => .Map ... </stack>

  rule <k> GET A => #HandleAnnotations(A) ... </k>
       <stack> X ~> M => None ... </stack>
       requires notBool(X in_keys(M))

  syntax OptionData ::= #LookupMap(Map, Data) [function]

  rule #LookupMap(M, X) => Some {M[X]}:>Data requires X in_keys(M)
  rule #LookupMap(M, X) => None requires notBool(X in_keys(M))

  rule <k> MEM A => #HandleAnnotations(A) ~> . ... </k>
       <stack> X ~> M => X in_keys(M) ... </stack>

  rule <k> GET A => #HandleAnnotations(A) ... </k>
       <stack> X ~> M => #LookupMap(M, X) ... </stack>

  rule <k> UPDATE A => #HandleAnnotations(A)  ... </k>
       <stack> K ~> Some V ~> M:Map => M[K <- V] ... </stack>

  rule <k> UPDATE A => #HandleAnnotations(A)  ... </k>
       <stack> K ~> None ~> M:Map => M[K <- undef] ... </stack>

  syntax KItem ::= #PerformMap(Map, Map, Block)

  rule <k> MAP A B => #HandleAnnotations(A) ~> #PerformMap(M, .Map, B) ... </k>
       <stack> M => . ... </stack>

  syntax KItem ::= #PopNewVal(Data)

  rule <k> #PopNewVal(K) ~> #PerformMap(M1, M2, B) => #PerformMap(M1, M2[K <- V], B) ... </k>
       <stack> V => . ... </stack>

  syntax Data ::= #MinimalKey(Map) [function]
  rule #MinimalKey(M) => #MinimalElement(keys_list(M))

  rule <k> #PerformMap(M1, M2, B) => B ~> #PopNewVal(#MinimalKey(M1)) 
        ~> #PerformMap(M1[#MinimalKey(M1) <- undef], M2, B) ... </k>
       <stack> . => Pair #MinimalKey(M1) {M1[#MinimalKey(M1)]}:>Data ... </stack>
       requires size(M1) >Int 0

  rule <k> #PerformMap(.Map, M, _) => . ... </k>
       <stack> . => M ... </stack>

  rule <k> ITER A B => #HandleAnnotations(A)  ... </k>
       <stack> .Map => . ... </stack>

  rule <k> ITER A B => #HandleAnnotations(A) ~> B ~> #Push(M[#MinimalKey(M) <- undef]) ~> ITER .AnnotationList B ... </k>
       <stack> M:Map => Pair #MinimalKey(M) {M[#MinimalKey(M)]}:>Data ... </stack>
       requires size(M) >Int 0

  rule <k> SIZE A => #HandleAnnotations(A)  ... </k>
       <stack> M:Map => size(M) ... </stack>

  //// Operations on big maps

  rule <k> EMPTY_BIG_MAP A _ _ => #HandleAnnotations(A)  ... </k>
       <stack> . => .Map ... </stack>

  // Same as maps

  //// Operations on optional values

  rule <k> SOME A => #HandleAnnotations(A)  ... </k>
       <stack> X => Some X ... </stack>

  rule <k> NONE A _ => #HandleAnnotations(A)  ... </k>
       <stack> . => None ... </stack>

  rule <k> IF_NONE A BT BF => #HandleAnnotations(A) ~> BT ... </k>
       <stack> None => . ... </stack>

  rule <k> IF_NONE A BT BF => #HandleAnnotations(A) ~> BF ... </k>
       <stack> Some V => V ... </stack>

  //// Operations on unions
  rule <k> LEFT A _ => #HandleAnnotations(A)  ... </k>
       <stack> X:Data => Left X ... </stack>

  rule <k> RIGHT A _:Type => #HandleAnnotations(A) ... </k>
       <stack> X:Data => Right X ... </stack>

  rule <k> IF_LEFT A BT BF => #HandleAnnotations(A) ~> BT ... </k>
       <stack> Left V => V ... </stack>

  rule <k> IF_LEFT A BT BF => #HandleAnnotations(A) ~> BF ... </k>
       <stack> Right V => V ... </stack>

  rule <k> IF_RIGHT A BT BF => #HandleAnnotations(A) ~> BT ... </k>
       <stack> Right V => V ... </stack>

  rule <k> IF_RIGHT A BT BF => #HandleAnnotations(A) ~> BF ... </k>
       <stack> Left V => V ... </stack>

  //// Operations on lists
 
  syntax Data ::= List

  rule <k> CONS A => #HandleAnnotations(A)  ... </k>
       <stack> V ~> L => ListItem(V) L ... </stack>
  
  rule <k> NIL A _ => #HandleAnnotations(A)  ... </k> 
       <stack> . => .List ... </stack>

  rule <k> IF_CONS A BT BF => #HandleAnnotations(A) ~> BT ... </k>
       <stack> ListItem(L1) Ls => L1 ~> Ls ... </stack>

  rule <k> IF_CONS A BT BF => #HandleAnnotations(A) ~> BF ... </k>
       <stack> .List => . ... </stack>

  syntax KItem ::= #PerformMapList(List, List, Block)

  rule <k> MAP A B => #HandleAnnotations(A) ~> #PerformMapList(Ls, .List, B) ... </k>
       <stack> Ls => . ... </stack>

  syntax KItem ::= #AddToList(List, List, Block)

  rule <k> #PerformMapList(.List, Acc, B) => . ... </k>
       <stack> . => #ReverseList(Acc) ... </stack>

  rule <k> #PerformMapList(ListItem(L) Ls, Acc, B) => B ~> #AddToList(Ls, Acc, B) ... </k>
       <stack> . => L ... </stack>

  rule <k> #AddToList(Ls, Acc, B) => #PerformMapList(Ls, ListItem(L) Acc, B) ... </k>
       <stack> L => . ... </stack>

  rule <k> SIZE A => #HandleAnnotations(A)  ... </k>
       <stack> L:List => size(L) ... </stack>

  rule <k> ITER A B =>  #HandleAnnotations(A) ~>. ... </k>
       <stack> .List => . ... </stack>

  rule <k> ITER A B => #HandleAnnotations(A) ~> B ~> #Push(Ls) ~> ITER .AnnotationList B ... </k>
       <stack> ListItem(L) Ls => L ... </stack>

  // Domain Specific operations
  //// Operations on timestamps
  rule <k> ADD A => . ... </k>
       <stack> #Timestamp(I1) ~> I2 => #Timestamp(I1 +Int I2) ... </stack>

  rule <k> ADD A => . ... </k>
       <stack> I1 ~> #Timestamp(I2) => #Timestamp(I1 +Int I2) ... </stack>

  rule <k> SUB A => . ... </k>
       <stack> #Timestamp(I1) ~> I2 => #Timestamp(I1 -Int I2) ... </stack>

  rule <k> SUB A => . ... </k>
       <stack> #Timestamp(I1) ~> #Timestamp(I2) => I1 -Int I2 ... </stack>

  rule #DoCompare(#Timestamp(I1), #Timestamp(I2)) => #DoCompare(I1, I2)

  
  syntax Bool ::= #IsKeyHashOption(OptionData) [function]
  rule #IsKeyHashOption(Some K:KeyHash) => true
  rule #IsKeyHashOption(None) => true
  rule #IsKeyHashOption(Some _) => false [owise]

  rule <k> CREATE_CONTRACT A:AnnotationList { C } => . ... </k>
       <stack> Delegate:OptionData ~> Initial:Mutez ~> Stor:Data => Create_contract(O, C, Delegate, Initial, Stor) ~> #Address("@Address(" +String Int2String(!_:Int) +String ")") ... </stack>
       <nonce> #Nonce(O) => #NextNonce(#Nonce(O)) </nonce>
       requires #IsKeyHashOption(Delegate)

  rule <k> TRANSFER_TOKENS _ => . ... </k>
       <stack> D ~> M ~> #Contract(A, _) => Transfer_tokens(O, D, M, A) ... </stack>
       <nonce> #Nonce(O) => #NextNonce(#Nonce(O)) </nonce>

  rule <k> SET_DELEGATE A => . ... </k>
       <stack> D => Set_delegate(O, D) ... </stack>
       <nonce> #Nonce(O) => #NextNonce(#Nonce(O)) </nonce>
       requires #IsKeyHashOption(D)

  rule <k> BALANCE A => . ... </k>
       <stack> . => B ... </stack>
       <mybalance> B </mybalance>

  rule <k> ADDRESS Ann => . ... </k>
       <stack> #Contract(A, _) => A ... </stack>

  syntax Type ::= #TypeFromContractStruct(Data) [function]
  rule #TypeFromContractStruct(#Contract(_, T)) => T

  rule <k> CONTRACT _ T => . ... </k>
       <stack> A => Some {M[A]}:>Data ... </stack>
       <knownaddrs> M </knownaddrs>
       requires A in_keys(M) andBool #TypeFromContractStruct({M[A]}:>Data) ==K T
       
  rule <k> CONTRACT _ T => . ... </k>
       <stack> A:Address => None ... </stack>
       <knownaddrs> M </knownaddrs> [owise]

  rule <k> SOURCE Ann => . ... </k>
       <stack> . => A ... </stack>
       <sourceaddr> A </sourceaddr>

  rule <k> SENDER Ann => . ... </k>
       <stack> . => A ... </stack>
       <senderaddr> A </senderaddr>

  rule <k> SELF Ann => . ... </k>
       <stack> . => #Contract(A, T) ... </stack>
       <paramtype> T </paramtype>
       <myaddr> A </myaddr>

  rule <k> AMOUNT Ann => . ... </k>
       <stack> . => M ... </stack>
       <myamount> M </myamount>

  rule <k> IMPLICIT_ACCOUNT Ann => . ... </k>
       <stack> #KeyHash(A) => #Contract(#Address(A), unit .AnnotationList) ... </stack>

  //// Special Operations
  rule <k> CHAIN_ID A => . ... </k>
       <stack> . => C ... </stack>
       <mychainid> C </mychainid>

  rule <k> NOW A => . ... </k> 
       <stack> . => N ... </stack>
       <mynow> N </mynow>


  //// Operations on MBytes, stubbed for now because of the lack of a documented bytes format.

  syntax MBytes ::= MBytesLiteral | #Packed(Data)

  syntax Data ::= MBytes

  rule <k> PACK A => #HandleAnnotations(A) ... </k>
       <stack> T => #Packed(T) ... </stack>

  rule <k> UNPACK A _ => #HandleAnnotations(A) ... </k>
       <stack> #Packed(T) => T ... </stack>

  rule <k> CONCAT A => #HandleAnnotations(A) ... </k>
       <stack> B1:MBytesLiteral ~> B2:MBytesLiteral => #StringToMBytes("0x" +String #MBytesContent(B1) +String #MBytesContent(B2)) ... </stack>

  rule <k> SIZE A => #HandleAnnotations(A) ... </k>
       <stack> B:MBytesLiteral => lengthString(#MBytesContent(B)) /Int 2 ... </stack> 

  syntax OptionData ::= #OptionBytesFromOptionContent(Data) [function]
  rule #OptionBytesFromOptionContent(None) => None
  rule #OptionBytesFromOptionContent(Some S:String) => Some #StringToMBytes("0x" +String S) 

  rule <k> SLICE A => #HandleAnnotations(A) ... </k>
       <stack> O:Int ~> L:Int ~> B:MBytesLiteral => #OptionBytesFromOptionContent(#SliceString(#MBytesContent(B), 2 *Int O, 2 *Int L))  ... </stack>

  rule #DoCompare(B1:MBytesLiteral, B2:MBytesLiteral) => #DoCompare(#MBytesContent(B1), #MBytesContent(B2)) 

  //// Cryptographic primitives

  syntax String ::= #Blake2BKeyHash(String) [function] // TODO: Blake2B crypto hook.
  rule #Blake2BKeyHash(S) => S

  rule <k> HASH_KEY A => #HandleAnnotations(A) ... </k>
       <stack> #Key(S) => #KeyHash(#Blake2BKeyHash(S)) ... </stack> 
  
  syntax MBytes ::= #SHA256(MBytes) | #SHA512(MBytes)

  rule <k> SHA256 A => #HandleAnnotations(A) ... </k>
       <stack> B:MBytes => #SHA256(B) ... </stack>
       
  rule <k> SHA512 A => #HandleAnnotations(A) ... </k>
       <stack> B:MBytes => #SHA512(B) ... </stack>

  syntax MBytes ::= #SignedMBytes(Key, Signature, MBytes)

/*  rule <k> CHECK_SIGNATURE A => #HandleAnnotations(A) ... </k>
       <stack> #Key(K) ~> #Signature(S) ~> #SignedMBytes(#Key(K), #Signature(S), _) => true ... </stack>

  rule <k> CHECK_SIGNATURE A => #HandleAnnotations(A) ... </k>
       <stack> #Key(_) ~> #Signature(_) ~> _:MBytes => false ... </stack> [owise]*/ // TODO: Bug - The haskell backend does not support distinguishing these rules.

  rule #DoCompare(#KeyHash(S1), #KeyHash(S2)) => #DoCompare(S1, S2)

  //// Operations on Mutez

  syntax Data ::= Mutez

  syntax KItem ::= #ValidateMutezAndPush(Mutez, Int, Int)

  syntax FailedStack ::= #FailureFromMutezValue(Mutez, Int, Int) [function]
  rule #FailureFromMutezValue(#Mutez(I), I1, I2) => ( MutezOverflow I1 I2 ) requires I >=Int #MutezOverflowLimit
  rule #FailureFromMutezValue(#Mutez(I), I1, I2) => ( MutezUnderflow I1 I2 ) requires I <Int 0


  rule <k> #ValidateMutezAndPush(#Mutez(I), _, _) => . ... </k>
       <stack> . => #Mutez(I) ... </stack>
       requires #IsLegalMutezValue(I) 

  rule <k> #ValidateMutezAndPush(#Mutez(I), I1, I2) ~> Rk => Aborted("Mutez out of bounds", I, Rk, Rs) </k>
       <stack> Rs => #FailureFromMutezValue(#Mutez(I), I1, I2) </stack>
       requires notBool #IsLegalMutezValue(I) 

  rule <k> ADD A => #ValidateMutezAndPush(#Mutez(I1 +Int I2), I1, I2) ~> #HandleAnnotations(A) ... </k>
       <stack> #Mutez(I1) ~> #Mutez(I2) => . ... </stack>

  rule <k> SUB A => #ValidateMutezAndPush(#Mutez(I1 -Int I2), I1, I2) ~> #HandleAnnotations(A) ... </k>
       <stack> #Mutez(I1) ~> #Mutez(I2) => . ... </stack>

  rule <k> MUL A => #ValidateMutezAndPush(#Mutez(I1 *Int I2), I1, I2) ~> #HandleAnnotations(A) ... </k>
       <stack> #Mutez(I1) ~> I2 => . ... </stack>

  rule <k> MUL A => #ValidateMutezAndPush(#Mutez(I1 *Int I2), I1, I2) ~> #HandleAnnotations(A) ... </k>
       <stack> I1 ~> #Mutez(I2) => . ... </stack>

  rule <k> EDIV A => #HandleAnnotations(A) ... </k>
       <stack> #Mutez(I1) ~> #Mutez(0) => None ... </stack>

  rule <k> EDIV A => #HandleAnnotations(A) ... </k>
       <stack> #Mutez(I1) ~> 0 => None ... </stack>

  rule <k> EDIV A => #HandleAnnotations(A) ... </k>
       <stack> #Mutez(I1) ~> #Mutez(I2) => Some (Pair (I1 /Int I2) #Mutez(I1 %Int I2)) ... </stack>
       requires I2 >Int 0

  rule <k> EDIV A => #HandleAnnotations(A) ... </k>
       <stack> #Mutez(I1) ~> I2 => Some (Pair #Mutez(I1 /Int I2) #Mutez(I1 %Int I2)) </stack>
       requires I2 >Int 0

  rule #DoCompare(#Mutez(I1), #Mutez(I2)) => #DoCompare(I1, I2)

  syntax K ::= #ListToKSeq(List) [function]
  syntax K ::= #ListToKSeqAux(List, K) [function]

  rule #ListToKSeq(L) => #ListToKSeqAux(#ReverseList(L), .K)
  rule #ListToKSeqAux(ListItem(O) L, S) => #ListToKSeqAux(L, O ~> S)
  rule #ListToKSeqAux(.List, S) => S
endmodule
