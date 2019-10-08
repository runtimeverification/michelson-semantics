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

  rule I:Instruction ; Is:InstructionList => I ~> Is [structural]
  rule .InstructionList => .K [structural]
  rule {} => .K [structrual]
  rule { Is:InstructionList } => Is [structural]

  rule <k> { code B:Block ; _:StorageDecl _:ParameterDecl } P:Data S:Data => B </k> // Untyped semantics disregard everything but the code block.
       <stack> Stack(.List => ListItem(Pair P S)) </stack>

  rule <k> { code B:Block ; _:ParameterDecl _:StorageDecl } P:Data S:Data => B </k> // Untyped semantics disregard everything but the code block.
       <stack> Stack(.List => ListItem(Pair P S)) </stack>

  rule <k> { _:StorageDecl code B:Block ; _:ParameterDecl } P:Data S:Data => B </k> 
       <stack> Stack(.List => ListItem(Pair P S)) </stack>

  rule <k> { _:ParameterDecl code B:Block ; _:StorageDecl } P:Data S:Data => B </k> 
       <stack> Stack(.List => ListItem(Pair P S)) </stack>

  rule <k> { _:StorageDecl _:ParameterDecl code B:Block ; } P:Data S:Data => B </k> 
       <stack> Stack(.List => ListItem(Pair P S)) </stack>

  rule <k> { _:ParameterDecl _:StorageDecl code B:Block ; } P:Data S:Data => B </k> 
       <stack> Stack(.List => ListItem(Pair P S)) </stack>

  syntax Error ::= Aborted(Data)

  // Core Instructioons
  //// Control Structures
  rule <k> FAILWITH D ~> _ => Aborted(D) </k>
       <stack> _ => FailedStack </stack>

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

  syntax K ::= #Push(Data)
  rule <k> #Push(D) => . ... </k>
       <stack> Stack(Ls => ListItem(D) Ls) </stack>

  rule <k> DIP B => B ~> #Push(D) ... </k>
       <stack> Stack((ListItem(D) => .List) _) </stack>

  syntax Data ::= #Lambda(Type, Type, Block)
 
  syntax K ::= #ReturnStack(StackSort)

  rule <k> #ReturnStack(Stack(Ls)) => . ... </k>
       <stack> Stack(ListItem(R) => ListItem(R) Ls) </stack>

  rule <k> EXEC => C ~> #ReturnStack(Stack(Ls)) ... </k>
       <stack> Stack(ListItem(A) ListItem(#Lambda(_, _, C)) Ls => ListItem(A)) </stack> 

  ////Stack operations

  rule <k> DROP => . ... </k>
       <stack> Stack(ListItem(_) Ls => Ls) </stack>

  rule <k> DUP => . ... </k>
       <stack> Stack(ListItem(X) Ls => ListItem(X) ListItem(X) Ls) </stack>

  rule <k> SWAP => . ... </k>
       <stack> Stack(ListItem(X) ListItem(Y) Ls => ListItem(Y) ListItem(X) Ls) </stack>

  rule <k> PUSH _ X => . ... </k>
       <stack> Stack(Ls => ListItem(X) Ls) </stack>

  rule <k> UNIT => . ... </k>
       <stack> Stack(Ls => ListItem(Unit) Ls) </stack>

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

  rule <k> ADD => . ... </k>
       <stack> Stack(ListItem(I1) ListItem(I2) Ls => ListItem(I1 +Int I2) Ls) </stack>

  rule <k> SUB => . ... </k>
       <stack> Stack(ListItem(I1) ListItem(I2) Ls => ListItem(I1 -Int I2) Ls) </stack>

  rule <k> MUL => . ... </k>
       <stack> Stack(ListItem(I1) ListItem(I2) Ls => ListItem(I1 *Int I2) Ls) </stack>

  rule <k> EDIV => . ... </k>
       <stack> Stack(ListItem(I1) ListItem(0) Ls => ListItem(None) Ls) </stack>

  rule <k> EDIV => . ... </k>
       <stack> Stack(ListItem(I1) ListItem(I2) Ls => ListItem(Some (Pair (I1 /Int I2) (I2 %Int I2))) Ls) </stack>
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

  rule <k> LSL ~> _ => Aborted("LSL out of range") </k>
       <stack> Stack(ListItem(_) ListItem(S) _) => FailedStack </stack>
       requires S >Int 256

  rule <k> LSR => . ... </k>
       <stack> Stack(ListItem(X) ListItem(S) Ls => ListItem(X >>Int S) Ls) </stack>
       requires S <=Int 256

  rule <k> LSR ~> _ => Aborted("LSR out of range") </k>
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

  syntax String ::= #ConcatAll(DataList, String) [function]

  rule #ConcatAll(.DataList, A) => A
  rule #ConcatAll(S1:String ; DL:DataList, A) => #ConcatAll(DL, A +String S1)

  //// Operations on strings
  rule <k> CONCAT => . ... </k>
       <stack> Stack(ListItem(S1) ListItem(S2) Ls => ListItem(S1 +String S2) Ls) </stack>

  rule <k> CONCAT => . ... </k>
       <stack> Stack(ListItem({ D:DataList }) Ls => ListItem(#ConcatAll(D, "")) Ls) </stack>

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

  syntax K ::= #InsertIntoSet(Data, DataList, DataList, List) | #ContinueInsert(Data, DataList, DataList, List)

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

  rule <k> ITER B => B ~> ITER B ... </k>
       <stack> Stack((ListItem(S:Set) => ListItem(#MinimalElement(Set2List(S))) ListItem(S -Set SetItem(#MinimalElement(Set2List(S))))) _) </stack>
       requires size(S) >Int 0

  rule <k> SIZE => . ... </k>
       <stack> Stack((ListItem(S:Set) => ListItem(size(S))) _) </stack>

  //// Operations on maps

  rule <k> EMPTY_MAP _ _ => . ... </k>
       <stack> Stack((.List => ListItem(.Map)) _) </stack>

  syntax K ::= #DoGet(Data, DataList)


  //// Operations on unions
  rule <k> LEFT _ => . ... </k>
       <stack> Stack((ListItem(X) => ListItem(Left X)) _) </stack>

  rule <k> RIGHT _ => . ... </k>
       <stack> Stack((ListItem(X) => ListItem(Right X)) _) </stack>

  //// Operations on lists
  rule <k> NIL _ => . ... </k> 
       <stack> Stack((.List => ListItem({ .DataList })) _) </stack>

  configuration <k> $PGM:Pgm </k>
                <stack> Stack(.List) </stack>
endmodule
