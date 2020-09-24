requires "../../michelson.md"

module LEMMAS
  imports MICHELSON

  rule #Ceil(#DoCompare(@A:Int, @B:Int)) => #Ceil(@A) #And #Ceil(@B)  [anywhere, simplification]
  rule #MichelineToNative(M:Map, map _ _ _, _, _) => M [simplification]
endmodule

