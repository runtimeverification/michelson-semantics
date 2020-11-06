requires "lemmas.md"

module VERIFICATION
  imports LEMMAS
endmodule

module LISTSUM-SPEC
  imports VERIFICATION

  rule <k>  { ITER .AnnotationList { DROP .AnnotationList ; PUSH .AnnotationList nat .AnnotationList 1 ; ADD .AnnotationList } } => .K </k>
       <stack> [ list nat NatList:InternalList ] ; [ nat 0 ] => [ nat ?N:Int ] </stack>
     requires isValue(list nat, NatList)
     ensures  ?N ==Int size(NatList, 0)

endmodule

