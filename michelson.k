require "michelson-syntax.k"

module MICHELSON
  imports MICHELSON-SYNTAX
  imports DOMAINS  

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
       <stack> Stack(ListItem(D) Ls => Ls) </stack>

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

  rule <k> COMPARE => . ... </k>
       <stack> Stack(ListItem(I1) ListItem(I2) Ls => ListItem(-1) Ls) </stack>
       requires I1 <Int I2

  rule <k> COMPARE => . ... </k>
       <stack> Stack(ListItem(I1) ListItem(I2) Ls => ListItem(0) Ls) </stack>
       requires I1 ==Int I2

  rule <k> COMPARE => . ... </k>
       <stack> Stack(ListItem(I1) ListItem(I2) Ls => ListItem(1) Ls) </stack>
       requires I1 >Int I2

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

  rule <k> COMPARE => . ... </k>
       <stack> Stack((ListItem(S1) ListItem(S2) => ListItem(-1)) _) </stack>
       requires S1 <String S2

  rule <k> COMPARE => . ... </k>
       <stack> Stack((ListItem(S1) ListItem(S2) => ListItem(0)) _) </stack>
       requires S2 ==String S2

  rule <k> COMPARE => . ... </k>
       <stack> Stack((ListItem(S1) ListItem(S2) => ListItem(1)) _) </stack>
       requires S1 >String S2

  //// Operations on pairs
  rule <k> PAIR => . ... </k>
       <stack> Stack((ListItem(L) ListItem(R) => ListItem(Pair L R)) _) </stack>

  rule <k> CAR => . ... </k>
       <stack> Stack(ListItem(Pair L _ => L) _) </stack>

  rule <k> CDR => . ... </k>
       <stack> Stack(ListItem(Pair _ R => R) _) </stack>

  //// Operations on sets
  rule <k> EMPTY_SET _ => . ... </k>
       <stack> Stack((.List => ListItem({.DataList})) _) </stack>

  rule <k> MEM => . ... </k> 
       <stack> Stack((ListItem(X) ListItem({.DataList}) => ListItem(false)) _) </stack>

  syntax K ::= #ReplaceStack(StackSort)
  rule <k> #ReplaceStack(S) => . ... </k>
       <stack> _ => S </stack>

  syntax K ::= #ContinueMEM(StackSort)

  rule <k> MEM => COMPARE ~> #ContinueMEM(Stack(ListItem(X) ListItem({ DL:DataList }) S)) ... </k> 
       <stack> Stack(ListItem(X) ListItem({ D:Data ; DL:DataList }) S) => Stack(ListItem(X) ListItem(D))  </stack>

  rule <k> #ContinueMEM(S) => MEM ... </k> 
       <stack> Stack(ListItem(1)) => S </stack>

  rule <k> #ContinueMEM(Stack(ListItem(_) ListItem(_) S)) => . ... </k>
       <stack> Stack(ListItem(0) => ListItem(true) S) </stack>

  rule <k> #ContinueMEM(Stack(ListItem(_) ListItem(_) S)) => . ... </k>
       <stack> Stack(ListItem(-1) => ListItem(false) S) </stack>

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
