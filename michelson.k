requires "michelson-syntax.k"
requires "michelson-config.k"

module MICHELSON-OPERATIONS
  imports MICHELSON-SYNTAX
  imports MICHELSON-CONFIG

  syntax BlockchainOperation ::= "Create_contract" "(" Int "," Contract "," OptionData "," Mutez "," Data ")"
  syntax BlockchainOperation ::= "Transfer_tokens" "(" Int "," Data "," Mutez "," Address ")"
  syntax BlockchainOperation ::= "Set_delegate" "(" Int "," OptionData ")"

  syntax Data ::= BlockchainOperation
endmodule

module MICHELSON
  imports MICHELSON-SYNTAX
  imports MICHELSON-CONFIG
  imports MICHELSON-OPERATIONS
  imports DOMAINS 
  imports COLLECTIONS

  syntax Data ::= Bool

  syntax OperationNonce ::= #NextNonce(OperationNonce) [function]
  rule #NextNonce(#Nonce(I)) => #Nonce(I +Int 1)

  syntax KItem ::= #FailedStack(Data)

  syntax KItem ::= #ReadKnownAddrs(ContractInputList)
  syntax KItem ::= #LoadContract(Contract)
  syntax KItem ::= #InitializeStack(Data, Data)

  syntax KItem ::= #HandleAnnotations(AnnotationList)

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
  syntax Int ::= #MBytesToInt(MBytesLiteral) [function]

  rule #MBytesToInt(H) => String2Base(substrString(#MBytesToString(H), 2, lengthString(#MBytesToString(H))), 16) requires lengthString(#MBytesToString(H)) >Int 2 
  rule #MBytesToInt(H) => 0 requires lengthString(#MBytesToString(H)) <=Int 2

  rule #ConcreteArgToSemantics(H:MBytesLiteral, chain_id _) => #ChainId(#MBytesToInt(H))
  rule #ConcreteArgToSemantics(I:Int, int _) => I
  rule #ConcreteArgToSemantics(I:Int, nat _) => I requires I >=Int 0
  rule #ConcreteArgToSemantics(S:String, string _) => S
  rule #ConcreteArgToSemantics(B:MBytesLiteral, bytes _) => B
  rule #ConcreteArgToSemantics(I:Int, mutez _) => #Mutez(I)
  rule #ConcreteArgToSemantics(B:Bool, bool _) => B
  rule #ConcreteArgToSemantics(S:String, key_hash _) => #ParseKeyHash(S)
  rule #ConcreteArgToSemantics(S:String, timestamp _) => #ParseTimestamp(S)
  rule #ConcreteArgToSemantics(I:Int, timestamp _) => #Timestamp(I)
  rule #ConcreteArgToSemantics(I:Int, address _) => #Timestamp(I)
  rule #ConcreteArgToSemantics(S:String, timestamp _) => #ParseTimestamp(S)
  rule #ConcreteArgToSemantics(S:String, address _) => #ParseAddress(S)
  rule #ConcreteArgToSemantics(S:String, key _) => #ParseKey(S)
  rule #ConcreteArgToSemantics(Unit, unit _) => Unit
  rule #ConcreteArgToSemantics(S:String, signature _) => #ParseSignature(S)
  
  rule #ConcreteArgToSemantics(Pair A B, pair _ T1:Type T2:Type) => 
       Pair #ConcreteArgToSemantics(A, T1) #ConcreteArgToSemantics(B, T2)

  rule #ConcreteArgToSemantics(Pair A B, pair _ T1:SimpleComparableType T2:SimpleComparableType) => 
       Pair #ConcreteArgToSemantics(A, T1) #ConcreteArgToSemantics(B, T2)

  rule #ConcreteArgToSemantics(Some V, option _ T) => Some #ConcreteArgToSemantics(V, T)
  rule #ConcreteArgToSemantics(None, option _:AnnotationList _) => None

  syntax Data ::= #ConcreteArgToSemanticsList(DataList, Type, List) [function]

  rule #ConcreteArgToSemantics({ }, list _ _) => .List

  rule #ConcreteArgToSemantics({ DL:DataList }, list _ T) =>
       #ConcreteArgToSemanticsList(DL, T, .List)

  rule #ConcreteArgToSemanticsList(D1 ; DL, T, L) =>
       #ConcreteArgToSemanticsList(DL, T, ListItem(#ConcreteArgToSemantics(D1, T)) L)

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

  rule #ConcreteArgToSemantics({ }, map _ K V) => #Map(.Map, K, V)

  rule #ConcreteArgToSemantics({ M:MapEntryList }, map _:AnnotationList K V) =>
       #ConcreteArgToSemanticsMap(M, K, V, .Map)

  rule #ConcreteArgToSemanticsMap(Elt K V ; ML, KT, VT, M) =>
       #ConcreteArgToSemanticsMap(ML, KT, VT, M[#ConcreteArgToSemantics(K, KT) <- #ConcreteArgToSemantics(V, VT)])

  rule #ConcreteArgToSemanticsMap(Elt K V, KT, VT, M) => #Map(M[#ConcreteArgToSemantics(K, KT) <- #ConcreteArgToSemantics(V, VT)], KT, VT)

  rule #ConcreteArgToSemantics({ }, big_map _:AnnotationList K V) => #Map(.Map, K, V)

  rule #ConcreteArgToSemantics({ M:MapEntryList }, big_map _:AnnotationList K V) =>
       #ConcreteArgToSemanticsMap(M, K, V, .Map)

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

  rule <k> $contract { code C ; storage St ; parameter Pt ; } ;
           $balance B ;
           $amount Amt ;
           $now N ;
           $myaddr Addr ;
           $knownaddrs AddrList ;
           $sourceaddr SourceAddr ;
           $senderaddr SenderAddr ;
           $param P ;
           $storage S ; => #ReadKnownAddrs(AddrList) ~> #InitializeStack(P, S) ~> C ... </k> 
       <stack> . </stack>
       <balance> #Mutez(_ => B) </balance>
       <amount> #Mutez(_ => Amt) </amount>
       <now> #Timestamp(_ => N) </now>
       <myaddr> #Address(_ => Addr) </myaddr>
       <paramtype> _ => Pt </paramtype>
       <storagetype> _ => St </storagetype>
       <sourceaddr> #Address(_ => SourceAddr) </sourceaddr>
       <senderaddr> #Address(_ => SenderAddr) </senderaddr> [structural]

  rule <k> $contract { code B ; storage St ; parameter Pt ;} ;
           $param P ;
           $storage S ; => #InitializeStack(P, S) ~> B ... </k> 
       <paramtype> _ => Pt </paramtype>
       <storagetype> _ => St </storagetype> [structural]

  rule <k> #ReadKnownAddrs(#ContractInput(A, T) Cs => Cs) ... </k>
       <knownaddrs> M => M[#Address(A) <- #Contract(#Address(A), T)] </knownaddrs> [structural]

  rule #ReadKnownAddrs( .ContractInputList ) => . [structural]

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


  syntax Type ::= #StorageTypeFromContract(Contract) [function]

  rule #StorageTypeFromContract(code _ ; storage P ; parameter _ ;) => P

  rule <k> #InitializeStack(P, S) => . ... </k>
       <stack> . => Pair #ConcreteArgToSemantics(P, Pt) #ConcreteArgToSemantics(S, St) </stack>
       <paramtype> Pt </paramtype>
       <storagetype> St </storagetype>

  rule I:Instruction ; Is:InstructionList => I ~> Is [structural]
  rule {} => .K [structrual]
  rule { Is:InstructionList } => Is [structural]
  rule { Is:InstructionList ; } => Is [structural]


  rule #HandleAnnotations(_) => . [structural]

  syntax Error ::= Aborted(String, KItem, K, K)

  // Core Instructioons
  //// Control Structures
  rule <k> FAILWITH A ~> Rk => #HandleAnnotations(A) ~> Aborted("FAILWITH instruction reached", D, Rk, Rs) </k>
       <stack> D ~> Rs => #FailedStack(D) </stack>

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
       <stack> . => D ... </stack> [structural]

  rule <k> DIP A B => #HandleAnnotations(A) ~> B ~> #Push(D) ... </k>
       <stack> D:Data => . ... </stack>

  rule <k> DIP A 0 B => #HandleAnnotations(A) ~> B ... </k>

  rule <k> DIP A I B => #HandleAnnotations(A) ~> DIP .AnnotationList { DIP .AnnotationList  I -Int 1 B } ... </k>
       requires I >Int 0

  syntax Data ::= #Lambda(Type, Type, Block)
 
  syntax KItem ::= #ReturnStack(K)

  rule <k> #ReturnStack(Ls) => . ... </k>
       <stack> R:Data => R ~> Ls </stack>

  rule <k> EXEC B => #HandleAnnotations(B) ~> C ~> #ReturnStack(Rs) ... </k>
       <stack> A:Data ~> #Lambda(_, _, C):Data ~> Rs:K => A </stack> 

  rule <k> APPLY A => #HandleAnnotations(A) ... </k>
       <stack> D:Data ~> #Lambda((pair _:AnnotationList T0 T1):Type, T2, { C } ) => #Lambda(T1, T2, { PUSH .AnnotationList T0 D ; PAIR .AnnotationList ; C } ) ... </stack> 

  rule <k> APPLY A => #HandleAnnotations(A) ... </k>
       <stack> D:Data ~> #Lambda((pair _:AnnotationList T0 T1):ComparableType, T2, { C } ) => #Lambda(T1, T2, { PUSH .AnnotationList T0 D ; PAIR .AnnotationList ; C }) ... </stack>

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
       <stack> S </stack> [structural]

  rule <k> #DoDig(I, A, None) => #DoDig(I -Int 1, F ~> A, None) ... </k>
       <stack> F:Data => . ... </stack>
       requires I >Int 0 [structural]

  rule <k> #DoDig(0, A, None) => #DoDig(-1, A, Some F) ... </k>
       <stack> F:Data => . ... </stack> [structural]

  rule <k> #DoDig(-1, F:Data ~> A, Some T) => #DoDig(-1, A, Some T) ... </k>
       <stack> . => F ... </stack> [structural]

  rule <k> #DoDig(-1, .K, Some T) => . ... </k>
       <stack> . => T ... </stack>

  syntax KItem ::= #DoDug(Int, K, Data)

  rule <k> DUG A I => #HandleAnnotations(A) ~> #DoDug(I, .K, T) ... </k>
       <stack> T => .K ... </stack> [structural]

  rule <k> #DoDug(I, S, R) => #DoDug(I -Int 1, T ~> S, R) ... </k>
       <stack> T:Data => .K ... </stack>
       requires I >Int 0 [structural]

  rule <k> #DoDug(0, S, R) => #DoDug(-1, S, R) ... </k>
       <stack> .K => R ... </stack> [structural]

  rule <k> #DoDug(-1, T:Data ~> S, R) => #DoDug(-1, S, R) ... </k>
       <stack> .K => T ... </stack> [structural]

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
       <stack> _:Data ~> S:Int ~> Rs => #FailedStack(S) </stack>
       requires S >Int 256

  rule <k> LSR A => #HandleAnnotations(A) ... </k>
       <stack> X ~> S => X >>Int S ... </stack>
       requires S <=Int 256

  rule <k> LSR A ~> Rk => #HandleAnnotations(A) ~> Aborted("LSR out of range", S, Rk, Rs) </k>
       <stack> _:Data ~> S:Int ~> Rs => #FailedStack(S) </stack>
       requires S >Int 256

  syntax Int ::= #DoCompare(Data, Data) [function]

  rule #DoCompare(I1:Int, I2:Int) => -1 requires I1 <Int I2
  rule #DoCompare(I1:Int, I2:Int) => 0 requires I1 ==Int I2
  rule #DoCompare(I1:Int, I2:Int) => 1 requires I1 >Int I2

  rule #DoCompare(S1:String, S2:String) => -1 requires S1 <String S2
  rule #DoCompare(S1:String, S2:String) => 0 requires S1 ==String S2
  rule #DoCompare(S1:String, S2:String) => 1 requires S1 >String S2


  rule <k> COMPARE A => #HandleAnnotations(A) ... </k>
       <stack> V1 ~> V2 => #DoCompare(V1, V2) ... </stack>

  syntax String ::= #ConcatAll(List, String) [function]

  rule #ConcatAll(.List, A) => A
  rule #ConcatAll(ListItem(S1) DL, A) => #ConcatAll(DL, A +String S1)

  //// Operations on strings
  rule <k> CONCAT A => #HandleAnnotations(A) ... </k>
       <stack> S1 ~> S2 => S1 +String S2 ... </stack>

  rule <k> CONCAT A => #HandleAnnotations(A) ... </k>
       <stack> L => #ConcatAll(L, "") ... </stack>

  rule <k> SIZE A => #HandleAnnotations(A) ... </k>
       <stack> S => lengthString(S) ... </stack> 

  rule <k> SLICE A => #HandleAnnotations(A) ... </k>
       <stack> O ~> L ~> S => Some substrString(S, O, O +Int L) ... </stack>
       requires O >=Int 0 andBool L >=Int 0 andBool (O +Int L) <Int lengthString(S)

  rule <k> SLICE A => #HandleAnnotations(A) ... </k>
       <stack> O ~> L ~> S => None ... </stack>
       requires notBool(O >=Int 0 andBool L >=Int 0 andBool (O +Int L) <Int lengthString(S))


  //// Operations on pairs
  rule <k> PAIR A => #HandleAnnotations(A) ... </k>
       <stack> L ~> R => Pair L R ... </stack>

  rule <k> UNPAIR A => #HandleAnnotations(A) ... </k>
       <stack> Pair L R => L ~> R ... </stack>

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
  syntax MichelsonMap ::= #Map(Map, Type, Type) // Key, Value
  syntax Data ::= MichelsonMap

  rule <k> EMPTY_MAP A KT VT => #HandleAnnotations(A) ... </k>
       <stack> . => #Map(.Map, KT, VT) ... </stack>

  rule <k> GET A => #HandleAnnotations(A) ... </k>
       <stack> X ~> #Map(M, _, _) => None ... </stack>
       requires notBool(X in_keys(M))

  syntax OptionData ::= #LookupMap(MichelsonMap, Data) [function]

  rule #LookupMap(#Map(M, T0, T1), X) => Some {M[X]}:>Data requires X in_keys(M)
  rule #LookupMap(#Map(M, T0, T1), X) => None requires notBool(X in_keys(M))

  rule <k> MEM A => #HandleAnnotations(A) ~> . ... </k>
       <stack> X ~> #Map(M, _, _) => X in_keys(M) ... </stack>

  rule <k> GET A => #HandleAnnotations(A) ... </k>
       <stack> X ~> M => #LookupMap(M, X) ... </stack>

  rule <k> UPDATE A => #HandleAnnotations(A)  ... </k>
       <stack> K ~> Some V ~> #Map(M:Map, T1, T2) => #Map(M[K <- V], T1, T2) ... </stack>

  rule <k> UPDATE A => #HandleAnnotations(A)  ... </k>
       <stack> K ~> None ~> #Map(M:Map, T1, T2) => #Map(M[K <- undef], T1, T2) ... </stack>

  syntax KItem ::= #PerformMap(MichelsonMap, MichelsonMap, Block)

  rule <k> MAP A B => #HandleAnnotations(A) ~> #PerformMap(#Map(M, T1, T2), #Map(.Map, T1, T2), B) ... </k>
       <stack> #Map(M, T1, T2) => . ... </stack>

  syntax KItem ::= #PopNewVal(Data)

  rule <k> #PopNewVal(K) ~> #PerformMap(#Map(M1, T1, T2), #Map(M2, T1, T2), B) => #PerformMap(#Map(M1, T1, T2), #Map(M2[K <- V], T1, T2), B) ... </k>
       <stack> V => . ... </stack>

  syntax Data ::= #MinimalKey(Map) [function]
  rule #MinimalKey(M) => #MinimalElement(keys_list(M))

  rule <k> #PerformMap(#Map(M1, T1, T2), M2, B) => B ~> #PopNewVal(#MinimalKey(M1)) 
        ~> #PerformMap(#Map(M1[#MinimalKey(M1) <- undef], T1, T2), M2, B) ... </k>
       <stack> . => Pair #MinimalKey(M1) {M1[#MinimalKey(M1)]}:>Data ... </stack>
       requires size(M1) >Int 0

  rule <k> #PerformMap(#Map(.Map, _, _), M, _) => . ... </k>
       <stack> . => M ... </stack>

  rule <k> ITER A B => #HandleAnnotations(A)  ... </k>
       <stack> #Map(.Map, _, _) => .K ... </stack>

  rule <k> ITER A B => #HandleAnnotations(A) ~> B ~> #Push(#Map(M[#MinimalKey(M) <- undef], T1, T2)) ~> ITER .AnnotationList B ... </k>
       <stack> #Map(M, T1, T2) => Pair #MinimalKey(M) {M[#MinimalKey(M)]}:>Data ... </stack>
       requires size(M) >Int 0

  rule <k> SIZE A => #HandleAnnotations(A)  ... </k>
       <stack> #Map(M, T1, T2) => size(M) ... </stack>

  //// Operations on big maps

  rule <k> EMPTY_BIG_MAP A T1 T2 => #HandleAnnotations(A)  ... </k>
       <stack> . => #Map(.Map, T1, T2) ... </stack>

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

  syntax List ::= #ReverseList(List) [function]
  syntax List ::= #ReverseListAux(List, List) [function]

  rule #ReverseList(L) => #ReverseListAux(L, .List)
  rule #ReverseListAux(ListItem(L1) Ls, Acc) => #ReverseListAux(Ls, ListItem(L1) Acc)
  rule #ReverseListAux(.List, Acc) => Acc

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
       <stack> Delegate:OptionData ~> Initial:Mutez ~> Stor:Data => Create_contract(O, C, Delegate, Initial, Stor) ~> #Address(Int2String(!_:Int)) ... </stack>
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
       <balance> B </balance>

  rule <k> ADDRESS Ann => . ... </k>
       <stack> #Contract(A, _) => A ... </stack>

  rule <k> CONTRACT Ann T => . ... </k>
       <stack> A => Some {M[A]}:>Data ... </stack>
       <knownaddrs> M </knownaddrs>
       requires A in_keys(M)
       
  rule <k> CONTRACT Ann _ => . ... </k>
       <stack> A:Address => None ... </stack>
       <knownaddrs> M </knownaddrs>
       requires notBool(A in_keys(M))

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
       <amount> M </amount>

  rule <k> IMPLICIT_ACCOUNT Ann => . ... </k>
       <stack> #KeyHash(A) => #Contract(#Address(A), unit .AnnotationList) ... </stack>

  //// Special Operations
  rule <k> CHAIN_ID A => . ... </k>
       <stack> . => C ... </stack>
       <chainid> C </chainid>

  rule <k> NOW A => . ... </k> 
       <stack> . => N ... </stack>
       <now> N </now>


  //// Operations on MBytes, stubbed for now because of the lack of a documented bytes format.

  syntax MBytes ::= MBytesLiteral | #Packed(Data) | "#UnknownMBytes"

  syntax Data ::= MBytes

  rule <k> PACK A => #HandleAnnotations(A) ... </k>
       <stack> T => #Packed(T) ... </stack>

  rule <k> UNPACK A _ => #HandleAnnotations(A) ... </k>
       <stack> #Packed(T) => T ... </stack>

  rule <k> CONCAT A => #HandleAnnotations(A) ... </k>
       <stack> B1:MBytes ~> B2:MBytes => #UnknownMBytes ... </stack>

  rule <k> SIZE A => #HandleAnnotations(A) ... </k>
       <stack> B:MBytes => 0 ... </stack> // Would like to make this a ?_ variable but llvm doesn't support those.

  rule <k> SLICE A => #HandleAnnotations(A) ... </k>
       <stack> _:Int ~> _:Int ~> B:MBytes => Some #UnknownMBytes ... </stack>

  rule #DoCompare(B1:MBytes, B2:MBytes) => 0 // I can't see a reasonable way of doing this.

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

  rule <k> CHECK_SIGNATURE A => #HandleAnnotations(A) ... </k>
       <stack> #Key(K) ~> #Signature(S) ~> #SignedMBytes(#Key(K), #Signature(S), _) => true ... </stack>

  rule <k> CHECK_SIGNATURE A => #HandleAnnotations(A) ... </k>
       <stack> #Key(_) ~> #Signature(_) ~> _:MBytes => false ... </stack> [owise]

  rule #DoCompare(#KeyHash(S1), #KeyHash(S2)) => #DoCompare(S1, S2)

  //// Operations on Mutez

  syntax Data ::= Mutez

  syntax Int ::= "#MutezOverflowLimit" [function]
  rule #MutezOverflowLimit => 2 ^Int 63 // Signed 64 bit integers.

  syntax Bool ::= #IsLegalMutezValue(Int) [function]
  rule #IsLegalMutezValue(I) => I >=Int 0 andBool I <Int #MutezOverflowLimit

  syntax KItem ::= #ValidateMutezAndPush(Mutez)

  rule <k> #ValidateMutezAndPush(#Mutez(I)) => . ... </k>
       <stack> . => #Mutez(I) ... </stack>
       requires #IsLegalMutezValue(I) [structural]

  rule <k> #ValidateMutezAndPush(#Mutez(I)) ~> Rk => Aborted("Mutez out of bounds", I, Rk, Rs) </k>
       <stack> Rs => #FailedStack(I) </stack>
       requires notBool #IsLegalMutezValue(I) [structural]

  rule <k> ADD A => #ValidateMutezAndPush(#Mutez(I1 +Int I2)) ~> #HandleAnnotations(A) ... </k>
       <stack> #Mutez(I1) ~> #Mutez(I2) => . ... </stack>

  rule <k> SUB A => #ValidateMutezAndPush(#Mutez(I1 -Int I2)) ~> #HandleAnnotations(A) ... </k>
       <stack> #Mutez(I1) ~> #Mutez(I2) => . ... </stack>

  rule <k> MUL A => #ValidateMutezAndPush(#Mutez(I1 *Int I2)) ~> #HandleAnnotations(A) ... </k>
       <stack> #Mutez(I1) ~> I2 => . ... </stack>

  rule <k> MUL A => #ValidateMutezAndPush(#Mutez(I1 *Int I2)) ~> #HandleAnnotations(A) ... </k>
       <stack> I1 ~> #Mutez(I2) => . ... </stack>

  rule <k> EDIV A => #HandleAnnotations(A) ... </k>
       <stack> #Mutez(I1) ~> #Mutez(0) => None ... </stack>

  rule <k> EDIV A => #HandleAnnotations(A) ... </k>
       <stack> #Mutez(I1) ~> 0 => None ... </stack>

  rule <k> EDIV A => #HandleAnnotations(A) ... </k>
       <stack> #Mutez(I1) ~> #Mutez(I2) => Some (Pair #Mutez(I1 /Int I2) #Mutez(I1 %Int I2)) ... </stack>
       requires I2 >Int 0

  rule <k> EDIV A => #HandleAnnotations(A) ... </k>
       <stack> #Mutez(I1) ~> I2 => Some (Pair #Mutez(I1 /Int I2) #Mutez(I1 %Int I2)) </stack>
       requires I2 >Int 0

  rule #DoCompare(#Mutez(I1), #Mutez(I2)) => #DoCompare(I1, I2)

  rule CMPEQ _ => COMPARE .AnnotationList ~> EQ .AnnotationList
  rule CMPNEQ _ => COMPARE .AnnotationList ~> NEQ .AnnotationList
  rule CMPLT _ => COMPARE .AnnotationList ~> LT .AnnotationList
  rule CMPGT _ => COMPARE .AnnotationList ~> GT .AnnotationList
  rule CMPLE _ => COMPARE .AnnotationList ~> LE .AnnotationList
  rule CMPGE _ => COMPARE .AnnotationList ~> GE .AnnotationList

  rule IFEQ _ BT BF => EQ .AnnotationList ~> IF .AnnotationList BT BF
  rule IFNEQ _ BT BF => NEQ .AnnotationList ~> IF .AnnotationList BT BF
  rule IFLT _ BT BF => LT .AnnotationList ~> IF .AnnotationList BT BF
  rule IFGT _ BT BF => GT .AnnotationList ~> IF .AnnotationList BT BF
  rule IFLE _ BT BF => LE .AnnotationList ~> IF .AnnotationList BT BF
  rule IFGE _ BT BF => GE .AnnotationList ~> IF .AnnotationList BT BF

  rule IFCMPEQ _ BT BF => COMPARE .AnnotationList ~> EQ .AnnotationList ~> IF .AnnotationList BT BF
  rule IFCMPNEQ _ BT BF => COMPARE .AnnotationList ~> NEQ .AnnotationList ~> IF .AnnotationList BT BF
  rule IFCMPLT _ BT BF => COMPARE .AnnotationList ~> LT .AnnotationList ~> IF .AnnotationList BT BF
  rule IFCMPGT _ BT BF => COMPARE .AnnotationList ~> GT .AnnotationList ~> IF .AnnotationList BT BF
  rule IFCMPLE _ BT BF => COMPARE .AnnotationList ~> LE .AnnotationList ~> IF .AnnotationList BT BF
  rule IFCMPGE _ BT BF => COMPARE .AnnotationList ~> GE .AnnotationList ~> IF .AnnotationList BT BF

  rule FAIL _ => UNIT .AnnotationList ~> FAILWITH .AnnotationList

  rule ASSERT _ => IF .AnnotationList {} {FAIL .AnnotationList }

  rule ASSERT_EQ _ => IFEQ .AnnotationList {} {FAIL .AnnotationList }
  rule ASSERT_NEQ _ => IFNEQ .AnnotationList {} {FAIL .AnnotationList }
  rule ASSERT_LT _ => IFLT .AnnotationList {} {FAIL .AnnotationList }
  rule ASSERT_GT _ => IFGT .AnnotationList {} {FAIL .AnnotationList }
  rule ASSERT_LE _ => IFLE .AnnotationList {} {FAIL .AnnotationList }
  rule ASSERT_GE _ => IFGE .AnnotationList {} {FAIL .AnnotationList }

  rule ASSERT_CMPEQ _ => IFCMPEQ .AnnotationList {} {FAIL .AnnotationList }
  rule ASSERT_CMPNEQ _ => IFCMPNEQ .AnnotationList {} {FAIL .AnnotationList }
  rule ASSERT_CMPLT _ => IFCMPLT .AnnotationList {} {FAIL .AnnotationList }
  rule ASSERT_CMPGT _ => IFCMPGT .AnnotationList {} {FAIL .AnnotationList }
  rule ASSERT_CMPLE _ => IFCMPLE .AnnotationList {} {FAIL .AnnotationList }
  rule ASSERT_CMPGE _ => IFCMPGE .AnnotationList {} {FAIL .AnnotationList }

  rule ASSERT_NONE _ => IF_NONE .AnnotationList {} {FAIL .AnnotationList }

  rule ASSERT_SOME _ => IF_NONE .AnnotationList {FAIL .AnnotationList } {} // TODO: Add annotations
  rule ASSERT_LEFT _ => IF_LEFT .AnnotationList {} {FAIL .AnnotationList } //  ^
  rule ASSERT_RIGHT _ => IF_LEFT .AnnotationList {FAIL .AnnotationList } {} // ^

  rule IF_SOME _ BT BF => IF_NONE .AnnotationList BF BT

  syntax KItem ::= #DoCDARMacro(String)

  rule C:CDARMacro _ => #DoCDARMacro(substrString(#CDARToString(C), 1, lengthString(#CDARToString(C)) -Int 1))

  rule #DoCDARMacro("") => . 
  rule <k> #DoCDARMacro(S => substrString(S, 1, lengthString(S))) ... </k>
       <stack> Pair A _ => A ... </stack> requires lengthString(S) >Int 0 andBool substrString(S, 0, 1) ==String "A"

  rule <k> #DoCDARMacro(S => substrString(S, 1, lengthString(S))) ... </k>
       <stack> Pair _ B => B ... </stack> requires lengthString(S) >Int 0 andBool substrString(S, 0, 1) ==String "D"


  syntax K ::= #ListToKSeq(List) [function]
  syntax K ::= #ListToKSeqAux(List, K) [function]

  rule #ListToKSeq(L) => #ListToKSeqAux(#ReverseList(L), .K)
  rule #ListToKSeqAux(ListItem(O) L, S) => #ListToKSeqAux(L, O ~> S)
  rule #ListToKSeqAux(.List, S) => S
endmodule
