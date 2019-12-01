requires "../../michelson.k"

module VOTE-LEMMAS
  imports MICHELSON

  syntax Bool ::= IsInt(KItem) [function]

  rule IsInt(_:Int) => true
  rule IsInt(_) => false [owise]
endmodule

