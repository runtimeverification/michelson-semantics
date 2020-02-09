requires "../../unit-test-syntax.k"
requires "../../michelson-config.k"

module UNIT-TEST-EXPANDER
  imports UNIT-TEST-SYNTAX
  imports MICHELSON-CONFIG

  syntax Block ::= #StackToPush(LiteralStack) [function]
  rule #StackToPush( { } ) => { }

  syntax Block ::= #StackToPushAux(StackElementList, Block) [function]
  rule #StackToPush( { Se } ) => #StackToPushAux(Se, { DROP .AnnotationList 0 })
  rule #StackToPushAux(Stack_elt T D ; Ls, { I:InstructionList })  => #StackToPushAux(Ls, { PUSH .AnnotationList T D ; I })
  rule #StackToPushAux(Stack_elt T D, { I:InstructionList })  => { PUSH .AnnotationList T D ; I }


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
