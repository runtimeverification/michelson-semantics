requires "../../unit-test-syntax.k"

module UNIT-TEST-EXPANDER
  imports UNIT-TEST-SYNTAX

  syntax Block ::= #StackToPush(LiteralStack) [function]
  rule #StackToPush( { } ) => { }

  syntax StackElementList ::= #ReverseStackElementList(StackElementList) [function, functional]
  syntax StackElementList ::= #ReverseStackElementListAux(StackElementList, StackElementList) [function]
  rule #ReverseStackElementList(S:StackElement) => S
  rule #ReverseStackElementList(S:StackElement ; Se:StackElementList) => #ReverseStackElementListAux(Se, S)
  rule #ReverseStackElementListAux(S:StackElement ; Se:StackElementList, O) => #ReverseStackElementListAux(Se, S ; O)
  rule #ReverseStackElementListAux(S:StackElement, O) => S ; O


  syntax Block ::= #StackToPushAux(StackElementList, Block) [function]
  rule #StackToPush( { Se } ) => #StackToPushAux(#ReverseStackElementList(Se), { DROP .AnnotationList 0 })
  rule #StackToPushAux(Stack_elt T D ; Ls, { I })  => #StackToPushAux(Ls, { PUSH .AnnotationList T D ; I })
  rule #StackToPushAux(Stack_elt T D, { I })  => { PUSH .AnnotationList T D ; I }


  syntax Contract ::= #FillTemplateContract(Block, Block) [function]
  
  rule #FillTemplateContract(PushBlock, CodeBlock) =>
  parameter unit .AnnotationList ; storage unit .AnnotationList ; code { DROP .AnnotationList ; PushBlock ; CodeBlock ; UNIT @exitToken ; FAILWITH .AnnotationList } ; 

  syntax LiteralStack ::= #FindInputGroup(Groups) [function]
  rule #FindInputGroup(input T) => T
  rule #FindInputGroup(input T ; _) => T
  rule #FindInputGroup(_ ; G) => #FindInputGroup(G) [owise]

  syntax Block ::= #FindCodeGroup(Groups) [function]
  rule #FindCodeGroup(code T) => { T }
  rule #FindCodeGroup(code T ; _) => { T } 
  rule #FindCodeGroup(_ ; G) => #FindCodeGroup(G) [owise]

  rule <k> G:Groups => #FillTemplateContract(#StackToPush(#FindInputGroup(G)), #FindCodeGroup(G)) </k>
endmodule
