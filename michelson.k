requires "michelson-syntax.k"
requires "michelson-config.k"

module MICHELSON
  imports MICHELSON-SYNTAX-BASE
  imports MICHELSON-CONFIG
  imports DOMAINS 
  imports COLLECTIONS

  syntax Data ::= Bool

  rule True => true [macro]
  rule False => false [macro]

  syntax KItem ::= "FailedStack"

  syntax KItem ::= #ReadKnownAddrs(ContractInputList)
  syntax KItem ::= #LoadContract(Contract)

  syntax KItem ::= #HandleAnnotations(InstructionAnnotationList)

  syntax InstructionAnnotationList ::= AnnotationList AnnotationList AnnotationList [symbol, klabel(annotationlists)]
  syntax AnnotatedTypeList ::= AnnotationList AnnotationList [symbol, klabel(annotatedtypelist)]

  // Reorder annotation lists to T V F format to make future rule matching easier.
  rule T:TypeAnnotationList F:FieldAnnotationList V:VariableAnnotationList => T V F [macro]
  rule F:FieldAnnotationList T:TypeAnnotationList V:VariableAnnotationList => T V F [macro]
  rule F:FieldAnnotationList V:VariableAnnotationList T:TypeAnnotationList => T V F [macro]
  rule V:VariableAnnotationList T:TypeAnnotationList F:FieldAnnotationList => T V F [macro]
  rule V:VariableAnnotationList F:FieldAnnotationList T:TypeAnnotationList => T V F [macro]


  syntax Data ::= #ConcreteArgToSemantics(Data, Type) [function] 

  syntax KeyHash ::= #ParseKeyHash(String) [function]
  rule #ParseKeyHash(S) => #KeyHash(S)

  syntax Timestamp ::= #ParseTimestamp(String) [function]
  rule #ParseTimestamp(S) => #Timestamp(String2Int(S)) // For the time being we support only unix times.
  

  syntax Address ::= #ParseAddress(String) [function]
  rule #ParseAddress(S) => #Address(S)

  syntax Key ::= #ParseKey(String) [function]
  rule #ParseKey(S) => #Key(S)

  syntax Signature ::= #ParseSignature(String) [function]
  rule #ParseSignature(S) => #Signature(S)

  rule #ConcreteArgToSemantics(I:Int, int _) => I
  rule #ConcreteArgToSemantics(I:Int, nat _) => I requires I >=Int 0
  rule #ConcreteArgToSemantics(S:String, string _) => S
  rule #ConcreteArgToSemantics(B:BytesLiteral, bytes _) => B
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
  // I do not believe you can input an operation to a contract, so skipping that one.
  
  rule #ConcreteArgToSemantics(Pair A B, pair _ T1:Type T2:Type) => 
       Pair #ConcreteArgToSemantics(A, T1) #ConcreteArgToSemantics(B, T2)

  rule #ConcreteArgToSemantics(Pair A B, pair _ T1:SimpleComparableType T2:SimpleComparableType) => 
       Pair #ConcreteArgToSemantics(A, T1) #ConcreteArgToSemantics(B, T2)

  rule #ConcreteArgToSemantics(Some V, option _ T) => Some #ConcreteArgToSemantics(V, T)
  rule #ConcreteArgToSemantics(None, option _:AnnotatedTypeList _) => None

  syntax Data ::= #ConcreteArgToSemanticsList(DataList, Type, List) [function]

  rule #ConcreteArgToSemantics({ DL:DataList }, list _ T) =>
       #ConcreteArgToSemanticsList(DL, T, .List)

  rule #ConcreteArgToSemanticsList(D1 ; DL, T, L) =>
       #ConcreteArgToSemanticsList(DL, T, ListItem(#ConcreteArgToSemantics(D1, T)) L)

  rule #ConcreteArgToSemanticsList(.DataList, _, L) => #ReverseList(L)

  syntax Data ::= #ConcreteArgToSemanticsSet(DataList, Type, Set) [function]

  rule #ConcreteArgToSemantics({ DL:DataList }, set _ T) =>
       #ConcreteArgToSemanticsSet(DL, T, .Set)

  rule #ConcreteArgToSemanticsSet(D1 ; DL, T, S) =>
       #ConcreteArgToSemanticsSet(DL, T, SetItem(#ConcreteArgToSemantics(D1, T)) S)

  rule #ConcreteArgToSemanticsSet(.DataList, _, S) => S

  rule #ConcreteArgToSemantics(S:String, contract _ T) => #Contract(#ParseAddress(S), T)
  
  rule #ConcreteArgToSemantics(Left V, or _:AnnotatedTypeList TL:Type _:Type) => Left #ConcreteArgToSemantics(V, TL)
  rule #ConcreteArgToSemantics(Right V, or _:AnnotatedTypeList _:Type TR:Type) => Right #ConcreteArgToSemantics(V, TR)

  rule #ConcreteArgToSemantics(B:Block, lambda _:AnnotatedTypeList T1 T2) => #Lambda(T1, T2, B)

  syntax Data ::= #ConcreteArgToSemanticsMap(MapEntryList, Type, Type, Map) [function]

  rule #ConcreteArgToSemantics({ M:MapEntryList }, map _:AnnotatedTypeList K V) =>
       #ConcreteArgToSemanticsMap(M, K, V, .Map)

  rule #ConcreteArgToSemanticsMap(Elt K V ; ML , KT, VT, M) =>
       #ConcreteArgToSemanticsMap(ML , KT, VT, M[#ConcreteArgToSemantics(K, KT) <- #ConcreteArgToSemantics(V, VT)])

  rule #ConcreteArgToSemanticsMap(.MapEntryList, KT, VT, M) => #Map(M, KT, VT)

  rule #ConcreteArgToSemantics({ M:MapEntryList }, big_map _:AnnotatedTypeList K V) =>
       #ConcreteArgToSemanticsMap(M, K, V, .Map)


  rule <k> $contract {C} ;
           $paramtype Pt ;
           $storagetype St ;
           $balance B ;
           $amount Amt ;
           $now N ;
           $myaddr Addr ;
           $knownaddrs AddrList ;
           $sourceaddr SourceAddr ;
           $senderaddr SenderAddr ;
           $param P ;
           $storage S ; => #ReadKnownAddrs(AddrList) ~> #LoadContract(C) </k> 
       <stack> . => Pair #ConcreteArgToSemantics(P, Pt) #ConcreteArgToSemantics(S, St) </stack>
       <paramtype> _ => Pt </paramtype>
       <storagetype> _ => St </storagetype>
       <balance> #Mutez(_ => B) </balance>
       <amount> #Mutez(_ => Amt) </amount>
       <now> #Timestamp(_ => N) </now>
       <myaddr> #Address(_ => Addr) </myaddr>
       <sourceaddr> #Address(_ => SourceAddr) </sourceaddr>
       <senderaddr> #Address(_ => SenderAddr) </senderaddr> [structural]

  rule <k> #ReadKnownAddrs(#ContractInput(A, T) Cs => Cs) ... </k>
       <knownaddrs> M => M[#Address(A) <- #Contract(#Address(A), T)] </knownaddrs> [structural]

  rule #ReadKnownAddrs( .ContractInputList ) => . [structural]

  rule <k> #LoadContract(code B ; storage St ; parameter Pt ;) => B </k> 
       <paramtype> Pt </paramtype>
       <storagetype> St </storagetype> [structural]

  rule <k> #LoadContract(code B ; parameter Pt; storage St ;) => B </k> 
       <paramtype> Pt </paramtype>
       <storagetype> St </storagetype> [structural]

  rule <k> #LoadContract(storage St ; code B ; parameter Pt ;) => B </k> 
       <paramtype> Pt </paramtype>
       <storagetype> St </storagetype> [structural]

  rule <k> #LoadContract(parameter Pt ; code B ; storage St ;) => B </k> 
       <paramtype> Pt </paramtype>
       <storagetype> St </storagetype> [structural]

  rule <k> #LoadContract(storage St ; parameter Pt ; code B ;) => B </k> 
       <paramtype> Pt </paramtype>
       <storagetype> St </storagetype> [structural]

  rule <k> #LoadContract(parameter Pt ; storage St ; code B ;) => B </k> 
       <paramtype> Pt </paramtype>
       <storagetype> St </storagetype> [structural]

  rule I:Instruction ; Is:InstructionList => I ~> Is [structural]
  rule .InstructionList => .K [structural]
  rule {} => .K [structrual]
  rule { Is:InstructionList } => Is [structural]

  rule #HandleAnnotations(_) => . [structural]

  syntax Error ::= Aborted(String, KItem)

  // Core Instructioons
  //// Control Structures
  rule <k> FAILWITH A ~> _ => #HandleAnnotations(A) ~> Aborted("FAILWITH instruction reached", D) </k>
       <stack> D ~> _ => FailedStack </stack>

  rule <k> IF A BT BF => #HandleAnnotations(A) ~> BT ... </k>
       <stack> true => . ... </stack>

  rule <k> IF A BT BF => #HandleAnnotations(A) ~> BF ... </k>
       <stack> false => . ... </stack>

  rule <k> LOOP A B => #HandleAnnotations(A) ~> B ~> LOOP NoAnnotations B ... </k>
       <stack> true => . ... </stack>

  rule <k> LOOP A B => #HandleAnnotations(A) ... </k>
       <stack> false => . ... </stack>

  rule <k> LOOP_LEFT A B => #HandleAnnotations(A) ~> B ~> LOOP_LEFT NoAnnotations B ... </k>
       <stack> Left D => D ... </stack>

  rule <k> LOOP_LEFT A B => #HandleAnnotations(A) ... </k>
       <stack> Right D => D ... </stack>

  syntax KItem ::= #Push(Data)
  rule <k> #Push(D) => . ... </k>
       <stack> . => D ... </stack> [structural]

  rule <k> DIP A B => #HandleAnnotations(A) ~> B ~> #Push(D) ... </k>
       <stack> D:Data => . ... </stack>

  rule <k> DIP A 0 B => #HandleAnnotations(A) ~> B ... </k>

  syntax InstructionAnnotationList ::= "NoAnnotations"

  rule NoAnnotations => .TypeAnnotationList .VariableAnnotationList .FieldAnnotationList [macro]

  rule <k> DIP A I B => #HandleAnnotations(A) ~> DIP NoAnnotations { DIP NoAnnotations  I -Int 1 B } ... </k>
       requires I >Int 0

  syntax Data ::= #Lambda(Type, Type, Block)
 
  syntax KItem ::= #ReturnStack(K)

  rule <k> #ReturnStack(Ls) => . ... </k>
       <stack> R:Data => R ~> Ls </stack>

  rule <k> EXEC B => #HandleAnnotations(B) ~> C ~> #ReturnStack(Rs) ... </k>
       <stack> A:Data ~> #Lambda(_, _, C):Data ~> Rs:K => A </stack> 

  rule <k> APPLY A => #HandleAnnotations(A) ... </k>
       <stack> D:Data ~> #Lambda((pair _:AnnotatedTypeList T0 T1):Type, T2, { C } ) => #Lambda(T1, T2, { PUSH NoAnnotations T0 D ; PAIR NoAnnotations ; C } ) ... </stack> 

  rule <k> APPLY A => #HandleAnnotations(A) ... </k>
       <stack> D:Data ~> #Lambda((pair _:AnnotatedTypeList T0 T1):ComparableType, T2, { C } ) => #Lambda(T1, T2, { PUSH NoAnnotations T0 D ; PAIR NoAnnotations ; C }) ... </stack>

  ////Stack operations

  rule <k> DROP A =>  #HandleAnnotations(A) ... </k>
       <stack> _:Data => . ... </stack>

  rule <k> DROP A I => #HandleAnnotations(A) ~> DROP NoAnnotations ~> DROP NoAnnotations I -Int 1 ... </k> 
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

  rule <k> LSL A ~> _ => #HandleAnnotations(A) ~> Aborted("LSL out of range", S) </k>
       <stack> _:Data ~> S:Int ~> _ => FailedStack </stack>
       requires S >Int 256

  rule <k> LSR A => #HandleAnnotations(A) ... </k>
       <stack> X ~> S => X >>Int S ... </stack>
       requires S <=Int 256

  rule <k> LSR A ~> _ => #HandleAnnotations(A) ~> Aborted("LSR out of range", S) </k>
       <stack> _:Data ~> S:Int ~> _ => FailedStack </stack>
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

  rule <k> ITER A B => #HandleAnnotations(A) ~> B ~> #Push(S -Set SetItem(#MinimalElement(Set2List(S)))) ~> ITER NoAnnotations B ... </k>
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

  rule #LookupMap(#Map(M, T0, T1), X) => Some {M[X]}:>Data requires X in_keys(M) [concrete]
  rule #LookupMap(#Map(M, T0, T1), X) => None requires notBool(X in_keys(M)) [concrete]

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

  rule <k> ITER A B => #HandleAnnotations(A) ~> B ~> #Push(#Map(M[#MinimalKey(M) <- undef], T1, T2)) ~> ITER NoAnnotations B ... </k>
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

  rule <k> ITER A B => #HandleAnnotations(A) ~> B ~> #Push(Ls) ~> ITER NoAnnotations B ... </k>
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

  syntax BlockchainOperation ::= #CreateContract(Contract, OptionData, Mutez, Data)

  syntax Data ::= BlockchainOperation

  rule <k> CREATE_CONTRACT A:InstructionAnnotationList { C } => . ... </k>
       <stack> Delegate:OptionData ~> Initial:Mutez ~> Stor:Data => #CreateContract(C, Delegate, Initial, Stor) ~> #Address(Int2String(!_:Int)) ... </stack>
       requires #IsKeyHashOption(Delegate)

  syntax BlockchainOperation ::= #TransferTokens(Data, Mutez, ContractData)

  rule <k> TRANSFER_TOKENS A => . ... </k>
       <stack> D ~> M ~> C => #TransferTokens(D, M, C) ... </stack>

  syntax BlockchainOperation ::= #SetDelegate(OptionData)

  rule <k> SET_DELEGATE A => . ... </k>
       <stack> O => #SetDelegate(O) ... </stack>
       requires #IsKeyHashOption(O)

  rule <k> BALANCE A => . ... </k>
       <stack> . => B ... </stack>
       <balance> B </balance>

  rule <k> ADDRESS Ann => . ... </k>
       <stack> #Contract(A, _) => A ... </stack>

  rule <k> CONTRACT Ann T => . ... </k>
       <stack> A => Some #Contract(A, T) ... </stack>
       <knownaddrs> ... A |-> #Contract(A, T) ... </knownaddrs>
       
  rule <k> CONTRACT Ann _ => . ... </k>
       <stack> A:Address => None ... </stack> [owise]

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
       <stack> #KeyHash(A) => #Contract(#Address(A), unit .TypeAnnotationList .FieldAnnotationList) ... </stack>

  //// Special Operations
  rule <k> CHAIN_ID A => . ... </k>
       <stack> . => C ... </stack>
       <chainid> C </chainid>

  rule <k> NOW A => . ... </k> 
       <stack> . => N ... </stack>
       <now> N </now>


  //// Operations on Bytes, stubbed for now because of the lack of a documented bytes format.

  syntax Bytes ::= BytesLiteral | #Packed(Data) | "#UnknownBytes"

  syntax Data ::= Bytes

  rule <k> PACK A => #HandleAnnotations(A) ... </k>
       <stack> T => #Packed(T) ... </stack>

  rule <k> UNPACK A _ => #HandleAnnotations(A) ... </k>
       <stack> #Packed(T) => T ... </stack>

  rule <k> CONCAT A => #HandleAnnotations(A) ... </k>
       <stack> B1:Bytes ~> B2:Bytes => #UnknownBytes ... </stack>

  rule <k> SIZE A => #HandleAnnotations(A) ... </k>
       <stack> B:Bytes => 0 ... </stack> // Would like to make this a ?_ variable but llvm doesn't support those.

  rule <k> SLICE A => #HandleAnnotations(A) ... </k>
       <stack> _:Int ~> _:Int ~> B:Bytes => Some #UnknownBytes ... </stack>

  rule #DoCompare(B1:Bytes, B2:Bytes) => 0 // I can't see a reasonable way of doing this.

  //// Cryptographic primitives

  syntax String ::= #Blake2BKeyHash(String) [function] // TODO: Blake2B crypto hook.
  syntax String ::= #Blake2BHash(String)
  rule #Blake2BKeyHash(S) => #Blake2BHash(S)

  rule <k> HASH_KEY A => #HandleAnnotations(A) ... </k>
       <stack> #Key(S) => #KeyHash(#Blake2BKeyHash(S)) ... </stack> 
  
  syntax Bytes ::= #SHA256(Bytes) | #SHA512(Bytes)

  rule <k> SHA256 A => #HandleAnnotations(A) ... </k>
       <stack> B:Bytes => #SHA256(B) ... </stack>
       
  rule <k> SHA512 A => #HandleAnnotations(A) ... </k>
       <stack> B:Bytes => #SHA512(B) ... </stack>

  syntax Bytes ::= #SignedBytes(Key, Signature, Bytes)

  rule <k> CHECK_SIGNATURE A => #HandleAnnotations(A) ... </k>
       <stack> #Key(K) ~> #Signature(S) ~> #SignedBytes(#Key(K), #Signature(S), _) => true ... </stack>

  rule <k> CHECK_SIGNATURE A => #HandleAnnotations(A) ... </k>
       <stack> #Key(_) ~> #Signature(_) ~> _:Bytes => false ... </stack> [owise]

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

  rule <k> #ValidateMutezAndPush(#Mutez(I)) ~> _ => Aborted("Mutez out of bounds", I) </k>
       <stack> _ => FailedStack </stack>
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

  rule CMPEQ _ => COMPARE NoAnnotations ~> EQ NoAnnotations
  rule CMPNEQ _ => COMPARE NoAnnotations ~> NEQ NoAnnotations
  rule CMPLT _ => COMPARE NoAnnotations ~> LT NoAnnotations
  rule CMPGT _ => COMPARE NoAnnotations ~> GT NoAnnotations
  rule CMPLE _ => COMPARE NoAnnotations ~> LE NoAnnotations
  rule CMPGE _ => COMPARE NoAnnotations ~> GE NoAnnotations

  rule IFEQ _ BT BF => EQ NoAnnotations ~> IF NoAnnotations BT BF
  rule IFNEQ _ BT BF => NEQ NoAnnotations ~> IF NoAnnotations BT BF
  rule IFLT _ BT BF => LT NoAnnotations ~> IF NoAnnotations BT BF
  rule IFGT _ BT BF => GT NoAnnotations ~> IF NoAnnotations BT BF
  rule IFLE _ BT BF => LE NoAnnotations ~> IF NoAnnotations BT BF
  rule IFGE _ BT BF => GE NoAnnotations ~> IF NoAnnotations BT BF

  rule IFCMPEQ _ BT BF => COMPARE NoAnnotations ~> EQ NoAnnotations ~> IF NoAnnotations BT BF
  rule IFCMPNEQ _ BT BF => COMPARE NoAnnotations ~> NEQ NoAnnotations ~> IF NoAnnotations BT BF
  rule IFCMPLT _ BT BF => COMPARE NoAnnotations ~> LT NoAnnotations ~> IF NoAnnotations BT BF
  rule IFCMPGT _ BT BF => COMPARE NoAnnotations ~> GT NoAnnotations ~> IF NoAnnotations BT BF
  rule IFCMPLE _ BT BF => COMPARE NoAnnotations ~> LE NoAnnotations ~> IF NoAnnotations BT BF
  rule IFCMPGE _ BT BF => COMPARE NoAnnotations ~> GE NoAnnotations ~> IF NoAnnotations BT BF

  rule FAIL _ => UNIT NoAnnotations ~> FAILWITH NoAnnotations

  rule ASSERT _ => IF NoAnnotations {} {FAIL NoAnnotations}

  rule ASSERT_EQ _ => IFEQ NoAnnotations {} {FAIL NoAnnotations}
  rule ASSERT_NEQ _ => IFNEQ NoAnnotations {} {FAIL NoAnnotations}
  rule ASSERT_LT _ => IFLT NoAnnotations {} {FAIL NoAnnotations}
  rule ASSERT_GT _ => IFGT NoAnnotations {} {FAIL NoAnnotations}
  rule ASSERT_LE _ => IFLE NoAnnotations {} {FAIL NoAnnotations}
  rule ASSERT_GE _ => IFGE NoAnnotations {} {FAIL NoAnnotations}

  rule ASSERT_CMPEQ _ => IFCMPEQ NoAnnotations {} {FAIL NoAnnotations}
  rule ASSERT_CMPNEQ _ => IFCMPNEQ NoAnnotations {} {FAIL NoAnnotations}
  rule ASSERT_CMPLT _ => IFCMPLT NoAnnotations {} {FAIL NoAnnotations}
  rule ASSERT_CMPGT _ => IFCMPGT NoAnnotations {} {FAIL NoAnnotations}
  rule ASSERT_CMPLE _ => IFCMPLE NoAnnotations {} {FAIL NoAnnotations}
  rule ASSERT_CMPGE _ => IFCMPGE NoAnnotations {} {FAIL NoAnnotations}

  rule ASSERT_NONE _ => IF_NONE NoAnnotations {} {FAIL NoAnnotations}

  rule ASSERT_SOME _ => IF_NONE NoAnnotations {FAIL NoAnnotations} {} // TODO: Add annotations
  rule ASSERT_LEFT _ => IF_LEFT NoAnnotations {} {FAIL NoAnnotations} //  ^
  rule ASSERT_RIGHT _ => IF_LEFT NoAnnotations {FAIL NoAnnotations} {} // ^
endmodule
