requires "../../.build/michelson.k"

module LEMMAS
  imports MICHELSON

  syntax Instruction ::= "#AssumeTrue"

  rule <k> #AssumeTrue => . ... </k>
       <stack> true => . ... </stack>

  rule <k> #AssumeTrue ~> _:K => . </k>
       <stack> false ~> _ => . </stack>

  syntax Instruction ::= "#AssertTrue"

  rule <k> #AssertTrue => . ... </k>
       <stack> true => . ... </stack>

  rule #Ceil(#DoCompare(@A:Int, @B:Int)) => #Ceil(@A) #And #Ceil(@B)  [anywhere, simplification]

  rule #DoCompare(I1:Int, I2:Int) ==Int 0 => I1 ==Int I2 [simplification]
endmodule

