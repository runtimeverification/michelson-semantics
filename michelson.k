require "michelson-syntax.k"

module MICHELSON
  imports MICHELSON-SYNTAX
  imports DOMAINS 
  imports COLLECTIONS

  syntax Data ::= Bool

  rule True => true [macro]
  rule False => false [macro]

  syntax StackSort ::= Stack(List)
                     | "FailedStack"

  syntax ContractData ::= #Contract(Address, Type)
  syntax Data ::= ContractData


  syntax KItem ::= #ReadKnownAddrs(ContractInputList)
  syntax KItem ::= #LoadContract(Contract)
  
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
       <stack> _ => Stack(ListItem(Pair P S)) </stack>
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

  syntax Error ::= Aborted(String, KItem)

  // Core Instructioons
  //// Control Structures
  rule <k> FAILWITH ~> _ => Aborted("FAILWITH instruction reached", D) </k>
       <stack> Stack(ListItem(D) _) => FailedStack </stack>

  rule <k> IF BT BF => BT ... </k>
       <stack> Stack(ListItem(true) Ls => Ls) </stack>

  rule <k> IF BT BF => BF ... </k>
       <stack> Stack(ListItem(false) Ls => Ls) </stack>

  rule <k> LOOP B => B ~> LOOP B ... </k>
       <stack> Stack(ListItem(true) Ls => Ls) </stack>

  rule <k> LOOP B => .K ... </k>
       <stack> Stack(ListItem(false) Ls => Ls) </stack>

  rule <k> LOOP_LEFT B => B ~> LOOP_LEFT B ... </k>
       <stack> Stack(ListItem(Left A) Ls => ListItem(A) Ls) </stack>

  rule <k> LOOP_LEFT B => .K ... </k>
       <stack> Stack(ListItem(Right A) Ls => ListItem(A) Ls) </stack>

  syntax KItem ::= #Push(Data)
  rule <k> #Push(D) => . ... </k>
       <stack> Stack(Ls => ListItem(D) Ls) </stack> [structural]

  rule <k> DIP B => B ~> #Push(D) ... </k>
       <stack> Stack((ListItem(D) => .List) _) </stack>

  syntax Data ::= #Lambda(Type, Type, Block)
 
  syntax KItem ::= #ReturnStack(StackSort)

  rule <k> #ReturnStack(Stack(Ls)) => . ... </k>
       <stack> Stack(ListItem(R) => ListItem(R) Ls) </stack>

  rule <k> EXEC => C ~> #ReturnStack(Stack(Ls)) ... </k>
       <stack> Stack(ListItem(A) ListItem(#Lambda(_, _, C)) Ls => ListItem(A)) </stack> 

  rule <k> APPLY => . ... </k>
       <stack> Stack((ListItem(D) ListItem(#Lambda(pair T0 T1, T2, C)) => ListItem(#Lambda(T1, T2, { PUSH T0 D ; PAIR ; { C } }))) _) </stack>

  ////Stack operations

  rule <k> DROP => . ... </k>
       <stack> Stack((ListItem(_) => .List) _) </stack>

  rule <k> DUP => . ... </k>
       <stack> Stack((ListItem(X) => ListItem(X) ListItem(X)) _) </stack>

  rule <k> SWAP => . ... </k>
       <stack> Stack((ListItem(X) ListItem(Y) => ListItem(Y) ListItem(X)) _) </stack>

  rule <k> PUSH _ X => . ... </k>
       <stack> Stack((.List => ListItem(X)) _) </stack>

  rule <k> UNIT => . ... </k>
       <stack> Stack((.List => ListItem(Unit)) _) </stack>

  rule <k> LAMBDA T1 T2 C => . ... </k>
       <stack> Stack((.List => ListItem(#Lambda(T1, T2, C))) _) </stack>

  //// Generic Comparisons

  rule <k> EQ => . ... </k>
       <stack> Stack(ListItem(I) Ls => ListItem(I ==Int 0) Ls) </stack>

  rule <k> NEQ => . ... </k>
       <stack> Stack(ListItem(I) Ls => ListItem(I =/=Int 0) Ls) </stack>

  rule <k> LT => . ... </k>
       <stack> Stack(ListItem(I) Ls => ListItem(I <Int 0) Ls) </stack>

  rule <k> GT => . ... </k>
       <stack> Stack(ListItem(I) Ls => ListItem(I >Int 0) Ls) </stack>

  rule <k> LE => . ... </k>
       <stack> Stack(ListItem(I) Ls => ListItem(I <=Int 0) Ls) </stack>

  rule <k> GE => . ... </k>
       <stack> Stack(ListItem(I) Ls => ListItem(I >=Int 0) Ls) </stack>

  // Operations
  //// Operations on booleans

  rule <k> OR => . ... </k>
       <stack> Stack(ListItem(B1:Bool) ListItem(B2:Bool) Ls => ListItem(B1 orBool B2) Ls) </stack>

  rule <k> AND => . ... </k>
       <stack> Stack(ListItem(B1:Bool) ListItem(B2:Bool) Ls => ListItem(B1 andBool B2) Ls) </stack>

  rule <k> XOR => . ... </k>
       <stack> Stack(ListItem(B1:Bool) ListItem(B2:Bool) Ls => ListItem(B1 xorBool B2) Ls) </stack>

  rule <k> NOT => . ... </k>
       <stack> Stack(ListItem(B:Bool) Ls => ListItem(notBool B) Ls) </stack>

  //// Operations on integers and natural numbers
  
  rule <k> NEG => . ... </k>
       <stack> Stack(ListItem(I) Ls => ListItem(0 -Int I) Ls) </stack>

  rule <k> ABS => . ... </k>
       <stack> Stack(ListItem(I) Ls => ListItem(absInt(I)) Ls) </stack>

  rule <k> ISNAT => . ... </k>
       <stack> Stack(ListItem(I => Some I) _) </stack>
       requires I >=Int 0 

  rule <k> ISNAT => . ... </k>
       <stack> Stack(ListItem(I => None) _) </stack>
       requires I <Int 0 

  rule <k> ADD => . ... </k>
       <stack> Stack(ListItem(I1) ListItem(I2) Ls => ListItem(I1 +Int I2) Ls) </stack>

  rule <k> SUB => . ... </k>
       <stack> Stack(ListItem(I1) ListItem(I2) Ls => ListItem(I1 -Int I2) Ls) </stack>

  rule <k> MUL => . ... </k>
       <stack> Stack(ListItem(I1) ListItem(I2) Ls => ListItem(I1 *Int I2) Ls) </stack>

  rule <k> EDIV => . ... </k>
       <stack> Stack(ListItem(I1:Int) ListItem(0) Ls => ListItem(None) Ls) </stack>
       // Could combine this rule with the Mutez one but probably a disadvantage in readability.

  rule <k> EDIV => . ... </k>
       <stack> Stack(ListItem(I1) ListItem(I2) Ls => ListItem(Some (Pair (I1 /Int I2) (I1 %Int I2))) Ls) </stack>
       requires I2 =/=Int 0

  rule <k> OR => . ... </k>
       <stack> Stack(ListItem(I1:Int) ListItem(I2:Int) Ls => ListItem(I1 |Int I2) Ls) </stack>

  rule <k> AND => . ... </k>
       <stack> Stack(ListItem(I1:Int) ListItem(I2:Int) Ls => ListItem(I1 &Int I2) Ls) </stack>

  rule <k> XOR => . ... </k>
       <stack> Stack(ListItem(I1:Int) ListItem(I2:Int) Ls => ListItem(I1 xorInt I2) Ls) </stack>

  rule <k> NOT => . ... </k>
       <stack> Stack(ListItem(I:Int) Ls => ListItem(~Int I) Ls) </stack>

  rule <k> LSL => . ... </k>
       <stack> Stack(ListItem(X) ListItem(S) Ls => ListItem(X <<Int S) Ls) </stack>
       requires S <=Int 256

  rule <k> LSL ~> _ => Aborted("LSL out of range", S) </k>
       <stack> Stack(ListItem(_) ListItem(S) _) => FailedStack </stack>
       requires S >Int 256

  rule <k> LSR => . ... </k>
       <stack> Stack(ListItem(X) ListItem(S) Ls => ListItem(X >>Int S) Ls) </stack>
       requires S <=Int 256

  rule <k> LSR ~> _ => Aborted("LSR out of range", S) </k>
       <stack> Stack(ListItem(_) ListItem(S) _) => FailedStack </stack>
       requires S >Int 256

  syntax Int ::= #DoCompare(Data, Data) [function]

  rule #DoCompare(I1:Int, I2:Int) => -1 requires I1 <Int I2
  rule #DoCompare(I1:Int, I2:Int) => 0 requires I1 ==Int I2
  rule #DoCompare(I1:Int, I2:Int) => 1 requires I1 >Int I2

  rule #DoCompare(S1:String, S2:String) => -1 requires S1 <String S2
  rule #DoCompare(S1:String, S2:String) => 0 requires S1 ==String S2
  rule #DoCompare(S1:String, S2:String) => 1 requires S1 >String S2


  rule <k> COMPARE => . ... </k>
       <stack> Stack(ListItem(V1) ListItem(V2) Ls => ListItem(#DoCompare(V1, V2)) Ls) </stack>

  syntax String ::= #ConcatAll(List, String) [function]

  rule #ConcatAll(.List, A) => A
  rule #ConcatAll(ListItem(S1) DL, A) => #ConcatAll(DL, A +String S1)

  //// Operations on strings
  rule <k> CONCAT => . ... </k>
       <stack> Stack(ListItem(S1) ListItem(S2) Ls => ListItem(S1 +String S2) Ls) </stack>

  rule <k> CONCAT => . ... </k>
       <stack> Stack(ListItem(L => #ConcatAll(L, "")) _) </stack>

  rule <k> SIZE => . ... </k>
       <stack> Stack(ListItem(S => lengthString(S)) _) </stack> 

  rule <k> SLICE => . ... </k>
       <stack> Stack((ListItem(O) ListItem(L) ListItem(S) => ListItem(Some substrString(S, O, O +Int L))) _) </stack>
       requires O >=Int 0 andBool L >=Int 0 andBool (O +Int L) <Int lengthString(S)

  rule <k> SLICE => . ... </k>
       <stack> Stack((ListItem(O) ListItem(L) ListItem(S) => ListItem(None)) _) </stack>
       requires notBool(O >=Int 0 andBool L >=Int 0 andBool (O +Int L) <Int lengthString(S))


  //// Operations on pairs
  rule <k> PAIR => . ... </k>
       <stack> Stack((ListItem(L) ListItem(R) => ListItem(Pair L R)) _) </stack>

  rule <k> CAR => . ... </k>
       <stack> Stack(ListItem(Pair L _ => L) _) </stack>

  rule <k> CDR => . ... </k>
       <stack> Stack(ListItem(Pair _ R => R) _) </stack>

  //// Operations on sets
  syntax Data ::= Set

  rule <k> EMPTY_SET _ => . ... </k>
       <stack> Stack((.List => ListItem(.Set)) _) </stack>

  rule <k> MEM => . ... </k> 
       <stack> Stack(((ListItem(X:Data) ListItem(S:Set)) => ListItem(X in S)) _) </stack>

  syntax KItem ::= #InsertIntoSet(Data, DataList, DataList, List) | #ContinueInsert(Data, DataList, DataList, List)

  // True to insert, False to remove.

  rule <k> UPDATE => . ... </k>
       <stack> Stack((ListItem(D) ListItem(true) ListItem(S:Set) => ListItem(SetItem(D) S)) _) </stack>

  rule <k> UPDATE => . ... </k>
       <stack> Stack((ListItem(D) ListItem(false) ListItem(SetItem(D) S) => ListItem(S)) _) </stack>

  rule <k> UPDATE => . ... </k>
       <stack> Stack((ListItem(D) ListItem(false) => .List) ListItem(S:Set) _) </stack>
       requires notBool(D in S)

  syntax Data ::= #MinimalElement(List) [function]
  syntax Data ::= #MinimalElementAux(List, Data) [function]

  rule #MinimalElement(ListItem(H) L) => #MinimalElementAux(L, H)
  rule #MinimalElementAux(.List, M) => M
  rule #MinimalElementAux(ListItem(H) L, M) => #MinimalElementAux(L, M) requires #DoCompare(M, H) <=Int 0
  rule #MinimalElementAux(ListItem(H) L, M) => #MinimalElementAux(L, H) requires #DoCompare(M, H) ==Int 1

  rule <k> ITER B => . ... </k>
       <stack> Stack((ListItem(.Set) => .List) _) </stack>

  rule <k> ITER B => B ~> #Push(S -Set SetItem(#MinimalElement(Set2List(S)))) ~> ITER B ... </k>
       <stack> Stack((ListItem(S:Set) => ListItem(#MinimalElement(Set2List(S)))) _) </stack>
       requires size(S) >Int 0

  rule <k> SIZE => . ... </k>
       <stack> Stack((ListItem(S:Set) => ListItem(size(S))) _) </stack>

  //// Operations on maps

  syntax Data ::= Map

  rule <k> EMPTY_MAP _ _ => . ... </k>
       <stack> Stack((.List => ListItem(.Map)) _) </stack>

  rule <k> GET => . ... </k>
       <stack> Stack((ListItem(X) ListItem(M) => ListItem(None)) _) </stack>
       requires notBool(X in_keys(M))

  rule <k> GET => . ... </k>
       <stack> Stack((ListItem(X) ListItem(M) => ListItem(Some {M[X]}:>Data)) _) </stack>
       requires X in_keys(M)

  rule <k> MEM => . ... </k>
       <stack> Stack((ListItem(X) ListItem(M) => ListItem(X in_keys(M))) _) </stack>

  rule <k> UPDATE => . ... </k>
       <stack> Stack((ListItem(K) ListItem(Some V) ListItem(M:Map) => ListItem(M[K <- V])) _) </stack>

  rule <k> UPDATE => . ... </k>
       <stack> Stack((ListItem(K) ListItem(None) ListItem(M:Map) => ListItem(M[K <- undef])) _) </stack>

  syntax KItem ::= #PerformMap(Map, Map, Block, List)

  rule <k> MAP B => #PerformMap(M, .Map, B, Rs) ... </k>
       <stack> Stack((ListItem(M) => .List) Rs) </stack>

  syntax KItem ::= #PopNewVal(Data)

  rule <k> #PopNewVal(K) ~> #PerformMap(M1, M2, B, Rs) => #PerformMap(M1, M2[K <- V], B, Rs) ... </k>
       <stack> Stack((ListItem(V) => .List) _) </stack>

  syntax Data ::= #MinimalKey(Map) [function]
  rule #MinimalKey(M) => #MinimalElement(keys_list(M))

  rule <k> #PerformMap(M1, M2, B, Rs) => B ~> #PopNewVal(#MinimalKey(M1)) 
        ~> #PerformMap(M1[#MinimalKey(M1) <- undef], M2, B, Rs) ... </k>
       <stack> Stack((.List => ListItem(Pair #MinimalKey(M1) {M1[#MinimalKey(M1)]}:>Data)) _) </stack>
       requires size(M1) >Int 0

  rule <k> #PerformMap(.Map, M, _, Rs) => . ... </k>
       <stack> Stack(_ => ListItem(M) Rs) </stack>

  rule <k> ITER B => . ... </k>
       <stack> Stack((ListItem(.Map) => .List) _) </stack>

  rule <k> ITER B => B ~> #Push(M[#MinimalKey(M) <- undef]) ~> ITER B ... </k>
       <stack> Stack(ListItem(M => Pair #MinimalKey(M) {M[#MinimalKey(M)]}:>Data) _) </stack>
       requires size(M) >Int 0

  rule <k> SIZE => . ... </k>
       <stack> Stack(ListItem(M:Map => size(M)) _) </stack>

  //// Operations on big maps

  rule <k> EMPTY_BIG_MAP _ _ => . ... </k>
       <stack> Stack((.List => ListItem(.Map)) _) </stack>

  // Same as maps

  //// Operations on optional values

  rule <k> SOME => . ... </k>
       <stack> Stack(ListItem(X => Some X) _) </stack>

  rule <k> NONE _ => . ... </k>
       <stack> Stack((.List => ListItem(None)) _) </stack>

  rule <k> IF_NONE BT BF => BT ... </k>
       <stack> Stack((ListItem(None) => .List) _) </stack>

  rule <k> IF_NONE BT BF => BF ... </k>
       <stack> Stack(ListItem(Some V => V) _) </stack>

  //// Operations on unions
  rule <k> LEFT _ => . ... </k>
       <stack> Stack((ListItem(X) => ListItem(Left X)) _) </stack>

  rule <k> RIGHT _ => . ... </k>
       <stack> Stack((ListItem(X) => ListItem(Right X)) _) </stack>

  rule <k> IF_LEFT BT BF => BT ... </k>
       <stack> Stack(ListItem(Left V => V) _) </stack>

  rule <k> IF_LEFT BT BF => BF ... </k>
       <stack> Stack(ListItem(Right V => V) _) </stack>

  rule <k> IF_RIGHT BT BF => BT ... </k>
       <stack> Stack(ListItem(Right V => V) _) </stack>

  rule <k> IF_RIGHT BT BF => BF ... </k>
       <stack> Stack(ListItem(Left V => V) _) </stack>

  //// Operations on lists
 
  syntax Data ::= List

  rule <k> CONS => . ... </k>
       <stack> Stack((ListItem(V) ListItem(L) => ListItem(ListItem(V) L)) _) </stack>
  
  rule <k> NIL _ => . ... </k> 
       <stack> Stack((.List => ListItem(.List)) _) </stack>

  rule <k> IF_CONS BT BF => BT ... </k>
       <stack> Stack((ListItem(ListItem(L1) Ls) => ListItem(L1) ListItem(Ls)) _) </stack>

  rule <k> IF_CONS BT BF => BF ... </k>
       <stack> Stack((ListItem(.List) => .List) _) </stack>

  syntax KItem ::= #PerformMapList(List, List, Block)

  rule <k> MAP B => #PerformMapList(Ls, .List, B) ... </k>
       <stack> Stack((ListItem(Ls) => .List) _) </stack>

  syntax KItem ::= #AddToList(List, List, Block)

  syntax List ::= #ReverseList(List) [function]
  syntax List ::= #ReverseListAux(List, List) [function]

  rule #ReverseList(L) => #ReverseListAux(L, .List)
  rule #ReverseListAux(ListItem(L1) Ls, Acc) => #ReverseListAux(Ls, ListItem(L1) Acc)
  rule #ReverseListAux(.List, Acc) => Acc

  rule <k> #PerformMapList(.List, Acc, B) => . ... </k>
       <stack> Stack((.List => ListItem(#ReverseList(Acc))) _) </stack>

  rule <k> #PerformMapList(ListItem(L) Ls, Acc, B) => B ~> #AddToList(Ls, Acc, B) ... </k>
       <stack> Stack((.List => ListItem(L)) _) </stack>

  rule <k> #AddToList(Ls, Acc, B) => #PerformMapList(Ls, ListItem(L) Acc, B) ... </k>
       <stack> Stack((ListItem(L) => .List) _) </stack>

  rule <k> SIZE => . ... </k>
       <stack> Stack(ListItem(L:List => size(L)) _) </stack>

  rule <k> ITER B => . ... </k>
       <stack> Stack((ListItem(.List) => .List) _) </stack>

  rule <k> ITER B => B ~> #Push(Ls) ~> ITER B ... </k>
       <stack> Stack((ListItem(ListItem(L) Ls) => ListItem(L)) _) </stack>

  // Domain Specific operations
  //// Operations on timestamps

  syntax Timestamp ::= #Timestamp(Int) 

  syntax Data ::= Timestamp 

  rule <k> ADD => . ... </k>
       <stack> Stack((ListItem(#Timestamp(I1)) ListItem(I2) => ListItem(#Timestamp(I1 +Int I2))) _) </stack>

  rule <k> ADD => . ... </k>
       <stack> Stack((ListItem(I1) ListItem(#Timestamp(I2)) => ListItem(#Timestamp(I1 +Int I2))) _) </stack>

  rule <k> SUB => . ... </k>
       <stack> Stack((ListItem(#Timestamp(I1)) ListItem(I2) => ListItem(#Timestamp(I1 -Int I2))) _) </stack>

  rule <k> SUB => . ... </k>
       <stack> Stack((ListItem(#Timestamp(I1)) ListItem(#Timestamp(I2)) => ListItem(I1 -Int I2)) _) </stack>

  rule #DoCompare(#Timestamp(I1), #Timestamp(I2)) => #DoCompare(I1, I2)

  syntax Address ::= #Address(Int)

  syntax Mutez ::= #Mutez(Int)
  syntax KeyHash ::= #KeyHash(Int)

  syntax Data ::= KeyHash
  syntax Data ::= Mutez
  syntax Data ::= Address

  syntax Bool ::= #IsKeyHashOption(OptionData) [function]
  rule #IsKeyHashOption(Some K:KeyHash) => true
  rule #IsKeyHashOption(None) => true
  rule #IsKeyHashOption(_) => false [owise]

  syntax BlockchainOperation ::= #CreateContract(Contract, OptionData, Mutez, Data)

  syntax Data ::= BlockchainOperation

  rule <k> CREATE_CONTRACT { C } => . ... </k>
       <stack> Stack((ListItem(Delegate:OptionData)
                ListItem(Initial:Mutez)
                ListItem(Stor:Data)
                =>
                ListItem(#CreateContract(C, Delegate, Initial, Stor))
                ListItem(#Address(!_:Int))) _) </stack>
                requires #IsKeyHashOption(Delegate)


  syntax BlockchainOperation ::= #TransferTokens(Data, Mutez, ContractData)

  rule <k> TRANSFER_TOKENS => . ... </k>
       <stack> Stack((ListItem(D) ListItem(M:Mutez) ListItem(C:ContractData) => ListItem(#TransferTokens(D, M, C))) _) </stack>

  syntax BlockchainOperation ::= #SetDelegate(OptionData)

  rule <k> SET_DELEGATE => . ... </k>
       <stack> Stack((ListItem(O:OptionData => #SetDelegate(O))) _) </stack>
       requires #IsKeyHashOption(O)

  rule <k> BALANCE => . ... </k>
       <stack> Stack((.List => ListItem(B)) _) </stack>
       <balance> B </balance>

  rule <k> ADDRESS => . ... </k>
       <stack> Stack(ListItem(#Contract(A, _) => A) _) </stack>

  rule <k> CONTRACT T => . ... </k>
       <stack> Stack(ListItem(A => Some #Contract(A, T)) _) </stack>
       <knownaddrs> ... A |-> #Contract(A, T) ... </knownaddrs>
       
  rule <k> CONTRACT _ => . ... </k>
       <stack> Stack(ListItem(A => None) _) </stack> [owise]

  rule <k> SOURCE => . ... </k>
       <stack> Stack((.List => ListItem(A)) _) </stack>
       <sourceaddr> A:Address </sourceaddr>

  rule <k> SENDER => . ... </k>
       <stack> Stack((.List => ListItem(A)) _) </stack>
       <senderaddr> A:Address </senderaddr>

  rule <k> SELF => . ... </k>
       <stack> Stack((.List => ListItem(#Contract(A, T))) _) </stack>
       <paramtype> T </paramtype>
       <myaddr> A </myaddr>

  rule <k> AMOUNT => . ... </k>
       <stack> Stack((.List => ListItem(A)) _) </stack>
       <amount> A:Mutez </amount>

  rule <k> IMPLICIT_ACCOUNT => . ... </k>
       <stack> Stack(ListItem(#KeyHash(A) => #Contract(#Address(A), unit)) _)  </stack>

  //// Special Operations
  syntax ChainId ::= #ChainId(Int)
  syntax Data ::= ChainId

  rule <k> CHAIN_ID => . ... </k>
       <stack> Stack((.List => ListItem(C)) _) </stack>
       <chainid> C:ChainId </chainid>

  rule <k> NOW => . ... </k> 
       <stack> Stack((.List => ListItem(N)) _) </stack>
       <now> N:Timestamp </now>

  //// Operations on Mutez

  syntax Data ::= Mutez

  syntax Int ::= "#MutezOverflowLimit" [function]
  rule #MutezOverflowLimit => 2 ^Int 63 // Signed 64 bit integers.

  syntax KItem ::= #ValidateMutezAndPush(Mutez)

  rule <k> #ValidateMutezAndPush(#Mutez(I)) => . ... </k>
       <stack> Stack((.List => ListItem(#Mutez(I))) _) </stack>
       requires I >=Int 0 andBool I <Int #MutezOverflowLimit [structural]

  rule <k> #ValidateMutezAndPush(#Mutez(I)) ~> _ => Aborted("Mutez out of bounds", I) </k>
       <stack> Stack(_) => FailedStack </stack>
       requires notBool(I >=Int 0 andBool I <Int #MutezOverflowLimit) [structural]

  rule <k> ADD => #ValidateMutezAndPush(#Mutez(I1 +Int I2)) ... </k>
       <stack> Stack((ListItem(#Mutez(I1)) ListItem(#Mutez(I2)) => .List) _) </stack>

  rule <k> SUB => #ValidateMutezAndPush(#Mutez(I1 -Int I2)) ... </k>
       <stack> Stack((ListItem(#Mutez(I1)) ListItem(#Mutez(I2)) => .List) _) </stack>

  rule <k> MUL => #ValidateMutezAndPush(#Mutez(I1 *Int I2)) ... </k>
       <stack> Stack((ListItem(#Mutez(I1)) ListItem(I2) => .List) _) </stack>

  rule <k> MUL => #ValidateMutezAndPush(#Mutez(I1 *Int I2)) ... </k>
       <stack> Stack((ListItem(I1) ListItem(#Mutez(I2)) => .List) _) </stack>

  rule <k> EDIV => . ... </k>
       <stack> Stack((ListItem(#Mutez(I1)) ListItem(#Mutez(0)) => ListItem(None)) _) </stack>

  rule <k> EDIV => . ... </k>
       <stack> Stack((ListItem(#Mutez(I1)) ListItem(0) => ListItem(None)) _) </stack>

  rule <k> EDIV => . ... </k>
       <stack> Stack((ListItem(#Mutez(I1)) ListItem(#Mutez(I2)) => 
               ListItem(Some (Pair #Mutez(I1 /Int I2) #Mutez(I1 %Int I2)))) _) </stack>
       requires I2 >Int 0

  rule <k> EDIV => . ... </k>
       <stack> Stack((ListItem(#Mutez(I1)) ListItem(I2) => 
               ListItem(Some (Pair #Mutez(I1 /Int I2) #Mutez(I1 %Int I2)))) _) </stack>
       requires I2 >Int 0

  rule #DoCompare(#Mutez(I1), #Mutez(I2)) => #DoCompare(I1, I2)

//  syntax Int ::= "#NotSet"

  syntax Type ::= "#NotSet"

  configuration <k> $PGM:Pgm </k>
                <paramtype> #NotSet </paramtype>
                <storagetype> #NotSet </storagetype>
                <stack> Stack(.List) </stack>
                <balance> #Mutez(0) </balance>
                <amount> #Mutez(0) </amount>
                <now> #Timestamp(-1) </now>
                <myaddr> #Address(-1) </myaddr>
                <knownaddrs> .Map </knownaddrs>
                <sourceaddr> #Address(-2) </sourceaddr>
                <senderaddr> #Address(-3) </senderaddr>
                <chainid> #ChainId(0) </chainid>
endmodule
