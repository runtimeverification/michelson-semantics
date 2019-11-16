require "michelson-syntax.k"

module MICHELSON
  imports MICHELSON-SYNTAX
  imports DOMAINS 
  imports COLLECTIONS

  syntax Data ::= Bool

  rule True => true [macro]
  rule False => false [macro]

  syntax KItem ::= "FailedStack"

  syntax ContractData ::= #Contract(Address, Type)
  syntax Address ::= #Address(Int)
  syntax Mutez ::= #Mutez(Int)
  syntax KeyHash ::= #KeyHash(Int)
  syntax ChainId ::= #ChainId(Int)
  syntax Timestamp ::= #Timestamp(Int) 

  syntax Data ::= Timestamp 
  syntax Data ::= ChainId
  syntax Data ::= KeyHash
  syntax Data ::= Mutez
  syntax Data ::= Address
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
       <stack> . => Pair P S </stack>
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
       <stack> D ~> _ => FailedStack </stack>

  rule <k> IF BT BF => BT ... </k>
       <stack> true => . ... </stack>

  rule <k> IF BT BF => BF ... </k>
       <stack> false => . ... </stack>

  rule <k> LOOP B => B ~> LOOP B ... </k>
       <stack> true => . ... </stack>

  rule <k> LOOP B => .K ... </k>
       <stack> false => . ... </stack>

  rule <k> LOOP_LEFT B => B ~> LOOP_LEFT B ... </k>
       <stack> Left A => A ... </stack>

  rule <k> LOOP_LEFT B => .K ... </k>
       <stack> Right A => A ... </stack>

  syntax KItem ::= #Push(Data)
  rule <k> #Push(D) => . ... </k>
       <stack> . => D ... </stack> [structural]

  rule <k> DIP B => B ~> #Push(D) ... </k>
       <stack> D:Data => . ... </stack>

  syntax Data ::= #Lambda(Type, Type, Block)
 
  syntax KItem ::= #ReturnStack(K)

  rule <k> #ReturnStack(Ls) => . ... </k>
       <stack> R:Data => R ~> Ls </stack>

  rule <k> EXEC => C ~> #ReturnStack(Rs) ... </k>
       <stack> A:Data ~> #Lambda(_, _, C):Data ~> Rs:K => A </stack> 

  rule <k> APPLY => . ... </k>
       <stack> D:Data ~> #Lambda((pair T0 T1):Type, T2, C) => #Lambda(T1, T2, { PUSH T0 D ; PAIR ; { C } }) ... </stack>

  rule <k> APPLY => . ... </k>
       <stack> D:Data ~> #Lambda((pair T0 T1):ComparableType, T2, C) => #Lambda(T1, T2, { PUSH T0 D ; PAIR ; { C } }) ... </stack>

  ////Stack operations

  rule <k> DROP => . ... </k>
       <stack> _:Data => . ... </stack>

  rule <k> DUP => . ... </k>
       <stack> X:Data => X ~> X ... </stack>

  rule <k> SWAP => . ... </k>
       <stack> X:Data ~> Y:Data => Y ~> X ... </stack>

  rule <k> PUSH _ X => . ... </k>
       <stack> . => X ... </stack>

  rule <k> UNIT => . ... </k>
       <stack> . => Unit ... </stack>

  rule <k> LAMBDA T1 T2 C => . ... </k>
       <stack> . => #Lambda(T1, T2, C) ... </stack>

  //// Generic Comparisons

  rule <k> EQ => . ... </k>
       <stack> I => I ==Int 0 ... </stack>

  rule <k> NEQ => . ... </k>
       <stack> I => I =/=Int 0 ... </stack>

  rule <k> LT => . ... </k>
       <stack> I => I <Int 0 ... </stack>

  rule <k> GT => . ... </k>
       <stack> I => I >Int 0 ... </stack>

  rule <k> LE => . ... </k>
       <stack> I => I <=Int 0 ... </stack>

  rule <k> GE => . ... </k>
       <stack> I => I >=Int 0 ... </stack>

  // Operations
  //// Operations on booleans
  rule <k> OR => . ... </k>
       <stack> B1 ~> B2 => B1 orBool B2 ...  </stack>

  rule <k> AND => . ... </k>
       <stack> B1 ~> B2 => B1 andBool B2 ... </stack>

  rule <k> XOR => . ... </k>
       <stack> B1 ~> B2 => B1 xorBool B2 ... </stack>

  rule <k> NOT => . ... </k>
       <stack> B => notBool B ... </stack>

  //// Operations on integers and natural numbers 
  rule <k> NEG => . ... </k>
       <stack> I => 0 -Int I ... </stack>

  rule <k> ABS => . ... </k>
       <stack> I => absInt(I) ... </stack>

  rule <k> ISNAT => . ... </k>
       <stack> I => Some I ... </stack>
       requires I >=Int 0 

  rule <k> ISNAT => . ... </k>
       <stack> I => None ... </stack>
       requires I <Int 0 

  rule <k> INT => . ... </k>
       <stack> I:Int ... </stack>

  rule <k> ADD => . ... </k>
       <stack> I1 ~> I2 => I1 +Int I2 ... </stack>

  rule <k> SUB => . ... </k>
       <stack> I1 ~> I2 => I1 -Int I2 ... </stack>

  rule <k> MUL => . ... </k>
       <stack> I1 ~> I2 => I1 *Int I2 ... </stack>

  rule <k> EDIV => . ... </k>
       <stack> I1:Int ~> 0 => None ... </stack>
       // Could combine this rule with the Mutez one but probably a disadvantage in readability.

  rule <k> EDIV => . ... </k>
       <stack> I1 ~> I2 => Some (Pair (I1 /Int I2) (I1 %Int I2)) ... </stack>
       requires I2 =/=Int 0

  rule <k> OR => . ... </k>
       <stack> I1 ~> I2 => I1 |Int I2 ... </stack>

  rule <k> AND => . ... </k>
       <stack> I1 ~> I2 => I1 &Int I2 ... </stack>

  rule <k> XOR => . ... </k>
       <stack> I1 ~> I2 => I1 xorInt I2 ... </stack>

  rule <k> NOT => . ... </k>
       <stack> I => ~Int I ... </stack>

  rule <k> LSL => . ... </k>
       <stack> X ~> S => X <<Int S ... </stack>
       requires S <=Int 256

  rule <k> LSL ~> _ => Aborted("LSL out of range", S) </k>
       <stack> _:Data ~> S:Int ~> _ => FailedStack </stack>
       requires S >Int 256

  rule <k> LSR => . ... </k>
       <stack> X ~> S => X >>Int S ... </stack>
       requires S <=Int 256

  rule <k> LSR ~> _ => Aborted("LSR out of range", S) </k>
       <stack> _:Data ~> S:Int ~> _ => FailedStack </stack>
       requires S >Int 256

  syntax Int ::= #DoCompare(Data, Data) [function]

  rule #DoCompare(I1:Int, I2:Int) => -1 requires I1 <Int I2
  rule #DoCompare(I1:Int, I2:Int) => 0 requires I1 ==Int I2
  rule #DoCompare(I1:Int, I2:Int) => 1 requires I1 >Int I2

  rule #DoCompare(S1:String, S2:String) => -1 requires S1 <String S2
  rule #DoCompare(S1:String, S2:String) => 0 requires S1 ==String S2
  rule #DoCompare(S1:String, S2:String) => 1 requires S1 >String S2


  rule <k> COMPARE => . ... </k>
       <stack> V1 ~> V2 => #DoCompare(V1, V2) ... </stack>

  syntax String ::= #ConcatAll(List, String) [function]

  rule #ConcatAll(.List, A) => A
  rule #ConcatAll(ListItem(S1) DL, A) => #ConcatAll(DL, A +String S1)

  //// Operations on strings
  rule <k> CONCAT => . ... </k>
       <stack> S1 ~> S2 => S1 +String S2 ... </stack>

  rule <k> CONCAT => . ... </k>
       <stack> L => #ConcatAll(L, "") ... </stack>

  rule <k> SIZE => . ... </k>
       <stack> S => lengthString(S) ... </stack> 

  rule <k> SLICE => . ... </k>
       <stack> O ~> L ~> S => Some substrString(S, O, O +Int L) ... </stack>
       requires O >=Int 0 andBool L >=Int 0 andBool (O +Int L) <Int lengthString(S)

  rule <k> SLICE => . ... </k>
       <stack> O ~> L ~> S => None ... </stack>
       requires notBool(O >=Int 0 andBool L >=Int 0 andBool (O +Int L) <Int lengthString(S))


  //// Operations on pairs
  rule <k> PAIR => . ... </k>
       <stack> L ~> R => Pair L R ... </stack>

  rule <k> CAR => . ... </k>
       <stack> Pair L _ => L ... </stack>

  rule <k> CDR => . ... </k>
       <stack> Pair _ R => R ... </stack>

  //// Operations on sets
  syntax Data ::= Set

  rule <k> EMPTY_SET _ => . ... </k>
       <stack> . => .Set ... </stack>

  rule <k> MEM => . ... </k> 
       <stack> X ~> S:Set => X in S ... </stack>

  // True to insert, False to remove.

  rule <k> UPDATE => . ... </k>
       <stack> D ~> true ~> S => SetItem(D) S ... </stack>

  rule <k> UPDATE => . ... </k>
       <stack> D ~> false ~> SetItem(D) S => S ... </stack>

  rule <k> UPDATE => . ... </k>
       <stack> (D ~> false => .) ~> S:Set ... </stack>
       requires notBool(D in S)

  syntax Data ::= #MinimalElement(List) [function]
  syntax Data ::= #MinimalElementAux(List, Data) [function]

  rule #MinimalElement(ListItem(H) L) => #MinimalElementAux(L, H)
  rule #MinimalElementAux(.List, M) => M
  rule #MinimalElementAux(ListItem(H) L, M) => #MinimalElementAux(L, M) requires #DoCompare(M, H) <=Int 0
  rule #MinimalElementAux(ListItem(H) L, M) => #MinimalElementAux(L, H) requires #DoCompare(M, H) ==Int 1

  rule <k> ITER B => . ... </k>
       <stack> .Set => . ... </stack>

  rule <k> ITER B => B ~> #Push(S -Set SetItem(#MinimalElement(Set2List(S)))) ~> ITER B ... </k>
       <stack> S => #MinimalElement(Set2List(S)) ... </stack>
       requires size(S) >Int 0

  rule <k> SIZE => . ... </k>
       <stack> S:Set => size(S) ... </stack>

  //// Operations on maps
  syntax Data ::= Map

  rule <k> EMPTY_MAP _ _ => . ... </k>
       <stack> . => .Map ... </stack>

  rule <k> GET => . ... </k>
       <stack> X ~> M => None ... </stack>
       requires notBool(X in_keys(M))

  rule <k> GET => . ... </k>
       <stack> X ~> M => Some {M[X]}:>Data ... </stack>
       requires X in_keys(M)

  rule <k> MEM => . ... </k>
       <stack> X ~> M => X in_keys(M) ... </stack>

  rule <k> UPDATE => . ... </k>
       <stack> K ~> Some V ~> M:Map => M[K <- V] ... </stack>

  rule <k> UPDATE => . ... </k>
       <stack> K ~> None ~> M:Map => M[K <- undef] ... </stack>

  syntax KItem ::= #PerformMap(Map, Map, Block)

  rule <k> MAP B => #PerformMap(M, .Map, B) ... </k>
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

  rule <k> ITER B => . ... </k>
       <stack> .Map => .K ... </stack>

  rule <k> ITER B => B ~> #Push(M[#MinimalKey(M) <- undef]) ~> ITER B ... </k>
       <stack> M => Pair #MinimalKey(M) {M[#MinimalKey(M)]}:>Data ... </stack>
       requires size(M) >Int 0

  rule <k> SIZE => . ... </k>
       <stack> M:Map => size(M) ... </stack>

  //// Operations on big maps

  rule <k> EMPTY_BIG_MAP _ _ => . ... </k>
       <stack> . => .Map ... </stack>

  // Same as maps

  //// Operations on optional values

  rule <k> SOME => . ... </k>
       <stack> X => Some X ... </stack>

  rule <k> NONE _ => . ... </k>
       <stack> . => None ... </stack>

  rule <k> IF_NONE BT BF => BT ... </k>
       <stack> None => . ... </stack>

  rule <k> IF_NONE BT BF => BF ... </k>
       <stack> Some V => V ... </stack>

  //// Operations on unions
  rule <k> LEFT _ => . ... </k>
       <stack> X:Data => Left X ... </stack>

  rule <k> RIGHT _:Type => .K ... </k>
       <stack> X:Data => Right X ... </stack>

  rule <k> IF_LEFT BT BF => BT ... </k>
       <stack> Left V => V ... </stack>

  rule <k> IF_LEFT BT BF => BF ... </k>
       <stack> Right V => V ... </stack>

  rule <k> IF_RIGHT BT BF => BT ... </k>
       <stack> Right V => V ... </stack>

  rule <k> IF_RIGHT BT BF => BF ... </k>
       <stack> Left V => V ... </stack>

  //// Operations on lists
 
  syntax Data ::= List

  rule <k> CONS => . ... </k>
       <stack> V ~> L => ListItem(V) L ... </stack>
  
  rule <k> NIL _ => . ... </k> 
       <stack> . => .List ... </stack>

  rule <k> IF_CONS BT BF => BT ... </k>
       <stack> ListItem(L1) Ls => L1 ~> Ls ... </stack>

  rule <k> IF_CONS BT BF => BF ... </k>
       <stack> .List => . ... </stack>

  syntax KItem ::= #PerformMapList(List, List, Block)

  rule <k> MAP B => #PerformMapList(Ls, .List, B) ... </k>
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

  rule <k> SIZE => . ... </k>
       <stack> L:List => size(L) ... </stack>

  rule <k> ITER B => . ... </k>
       <stack> .List => . ... </stack>

  rule <k> ITER B => B ~> #Push(Ls) ~> ITER B ... </k>
       <stack> ListItem(L) Ls => L ... </stack>

  // Domain Specific operations
  //// Operations on timestamps
  rule <k> ADD => . ... </k>
       <stack> #Timestamp(I1) ~> I2 => #Timestamp(I1 +Int I2) ... </stack>

  rule <k> ADD => . ... </k>
       <stack> I1 ~> #Timestamp(I2) => #Timestamp(I1 +Int I2) ... </stack>

  rule <k> SUB => . ... </k>
       <stack> #Timestamp(I1) ~> I2 => #Timestamp(I1 -Int I2) ... </stack>

  rule <k> SUB => . ... </k>
       <stack> #Timestamp(I1) ~> #Timestamp(I2) => I1 -Int I2 ... </stack>

  rule #DoCompare(#Timestamp(I1), #Timestamp(I2)) => #DoCompare(I1, I2)

  
  syntax Bool ::= #IsKeyHashOption(OptionData) [function]
  rule #IsKeyHashOption(Some K:KeyHash) => true
  rule #IsKeyHashOption(None) => true
  rule #IsKeyHashOption(Some _) => false [owise]

  syntax BlockchainOperation ::= #CreateContract(Contract, OptionData, Mutez, Data)

  syntax Data ::= BlockchainOperation

  rule <k> CREATE_CONTRACT { C } => . ... </k>
       <stack> Delegate:OptionData ~> Initial:Mutez ~> Stor:Data => #CreateContract(C, Delegate, Initial, Stor) ~> #Address(!_:Int) ... </stack>
       requires #IsKeyHashOption(Delegate)

  syntax BlockchainOperation ::= #TransferTokens(Data, Mutez, ContractData)

  rule <k> TRANSFER_TOKENS => . ... </k>
       <stack> D ~> M ~> C => #TransferTokens(D, M, C) ... </stack>

  syntax BlockchainOperation ::= #SetDelegate(OptionData)

  rule <k> SET_DELEGATE => . ... </k>
       <stack> O => #SetDelegate(O) ... </stack>
       requires #IsKeyHashOption(O)

  rule <k> BALANCE => . ... </k>
       <stack> . => B ... </stack>
       <balance> B </balance>

  rule <k> ADDRESS => . ... </k>
       <stack> #Contract(A, _) => A ... </stack>

  rule <k> CONTRACT T => . ... </k>
       <stack> A => Some #Contract(A, T) ... </stack>
       <knownaddrs> ... A |-> #Contract(A, T) ... </knownaddrs>
       
  rule <k> CONTRACT _ => . ... </k>
       <stack> A:Address => None ... </stack> [owise]

  rule <k> SOURCE => . ... </k>
       <stack> . => A ... </stack>
       <sourceaddr> A </sourceaddr>

  rule <k> SENDER => . ... </k>
       <stack> . => A ... </stack>
       <senderaddr> A </senderaddr>

  rule <k> SELF => . ... </k>
       <stack> . => #Contract(A, T) ... </stack>
       <paramtype> T </paramtype>
       <myaddr> A </myaddr>

  rule <k> AMOUNT => . ... </k>
       <stack> . => M ... </stack>
       <amount> M </amount>

  rule <k> IMPLICIT_ACCOUNT => . ... </k>
       <stack> #KeyHash(A) => #Contract(#Address(A), unit) ... </stack>

  //// Special Operations
  rule <k> CHAIN_ID => . ... </k>
       <stack> . => C ... </stack>
       <chainid> C </chainid>

  rule <k> NOW => . ... </k> 
       <stack> . => N ... </stack>
       <now> N </now>

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

  rule <k> ADD => #ValidateMutezAndPush(#Mutez(I1 +Int I2)) ... </k>
       <stack> #Mutez(I1) ~> #Mutez(I2) => . ... </stack>

  rule <k> SUB => #ValidateMutezAndPush(#Mutez(I1 -Int I2)) ... </k>
       <stack> #Mutez(I1) ~> #Mutez(I2) => . ... </stack>

  rule <k> MUL => #ValidateMutezAndPush(#Mutez(I1 *Int I2)) ... </k>
       <stack> #Mutez(I1) ~> I2 => . ... </stack>

  rule <k> MUL => #ValidateMutezAndPush(#Mutez(I1 *Int I2)) ... </k>
       <stack> I1 ~> #Mutez(I2) => . ... </stack>

  rule <k> EDIV => . ... </k>
       <stack> #Mutez(I1) ~> #Mutez(0) => None ... </stack>

  rule <k> EDIV => . ... </k>
       <stack> #Mutez(I1) ~> 0 => None ... </stack>

  rule <k> EDIV => . ... </k>
       <stack> #Mutez(I1) ~> #Mutez(I2) => Some (Pair #Mutez(I1 /Int I2) #Mutez(I1 %Int I2)) ... </stack>
       requires I2 >Int 0

  rule <k> EDIV => . ... </k>
       <stack> #Mutez(I1) ~> I2 => Some (Pair #Mutez(I1 /Int I2) #Mutez(I1 %Int I2)) </stack>
       requires I2 >Int 0

  rule #DoCompare(#Mutez(I1), #Mutez(I2)) => #DoCompare(I1, I2)

  syntax Type ::= "#NotSet"
/*
  rule CMPEQ => COMPARE ~> EQ
  rule CMPNEQ => COMPARE ~> NEQ
  rule CMPLT => COMPARE ~> LT
  rule CMPGT => COMPARE ~> GT
  rule CMPLE => COMPARE ~> LE
  rule CMPGE => COMPARE ~> GE

  rule IFEQ BT BF => EQ ~> IF BT BF
  rule IFNEQ BT BF => NEQ ~> IF BT BF
  rule IFLT BT BF => LT ~> IF BT BF
  rule IFGT BT BF => GT ~> IF BT BF
  rule IFLE BT BF => LE ~> IF BT BF
  rule IFGE BT BF => GE ~> IF BT BF

  rule IFCMPEQ BT BF => COMPARE ~> EQ ~> IF BT BF
  rule IFCMPNEQ BT BF => COMPARE ~> NEQ ~> IF BT BF
  rule IFCMPLT BT BF => COMPARE ~> LT ~> IF BT BF
  rule IFCMPGT BT BF => COMPARE ~> GT ~> IF BT BF
  rule IFCMPLE BT BF => COMPARE ~> LE ~> IF BT BF
  rule IFCMPGE BT BF => COMPARE ~> GE ~> IF BT BF

  rule FAIL => UNIT ~> FAILWITH

  rule ASSERT => IF {} {FAIL}

  rule ASSERT_EQ => IFEQ {} {FAIL}
  rule ASSERT_NEQ => IFNEQ {} {FAIL}
  rule ASSERT_LT => IFLT {} {FAIL}
  rule ASSERT_GT => IFGT {} {FAIL}
  rule ASSERT_LE => IFLE {} {FAIL}
  rule ASSERT_GE => IFGE {} {FAIL}

  rule ASSERT_CMPEQ => IFCMPEQ {} {FAIL}
  rule ASSERT_CMPNEQ => IFCMPNEQ {} {FAIL}
  rule ASSERT_CMPLT => IFCMPLT {} {FAIL}
  rule ASSERT_CMPGT => IFCMPGT {} {FAIL}
  rule ASSERT_CMPLE => IFCMPLE {} {FAIL}
  rule ASSERT_CMPGE => IFCMPGE {} {FAIL}

  rule ASSERT_NONE => IF_NONE {} {FAIL}

  rule ASSERT_SOME => IF_NONE {FAIL} {} // TODO: Add annotations
  rule ASSERT_LEFT => IF_LEFT {} {FAIL} //  ^
  rule ASSERT_RIGHT => IF_LEFT {FAIL} {} // ^
*/


  configuration <k> $PGM:Pgm </k>
                <stack> .K </stack>
                <paramtype> #NotSet </paramtype>
                <storagetype> #NotSet </storagetype>
                <balance> #Mutez(0) </balance>
                <amount> #Mutez(0) </amount>
                <now> #Timestamp(-1) </now>
                <myaddr> #Address(-1) </myaddr>
                <knownaddrs> .Map </knownaddrs>
                <sourceaddr> #Address(-2) </sourceaddr>
                <senderaddr> #Address(-3) </senderaddr>
                <chainid> #ChainId(0) </chainid>
endmodule
