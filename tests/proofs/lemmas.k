requires "unit-test.k"

module LEMMAS
  imports UNIT-TEST

  rule #Ceil(#DoCompare(@A:Int, @B:Int)) => #Ceil(@A) #And #Ceil(@B)  [anywhere, simplification]

  rule #ConcreteArgToSemantics(M:Map, map _ _ _) => M [simplification]

  rule S:String in_keys ((M:Map)[S:String <- _]) => true [simplification]
  rule ((M:Map)[S:String <- I:Int][S]) => I [simplification]
  rule ((M:Map)[S <- _][S <- J]) => M[S <- J] [simplification]

  rule #DoCompare(I1:Int, I2:Int) <Int 0 => I1 <Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) ==Int 0 => I1 ==Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) >Int 0 => I1 >Int I2 [simplification]
endmodule

