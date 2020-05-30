```k
requires "common.k"

module CONTRACT-EXPANDER-SYNTAX
  imports UNIT-TEST-SYNTAX
endmodule

module CONTRACT-EXPANDER
  imports CONTRACT-EXPANDER-SYNTAX
  imports COMPAT-COMMON

  syntax Block ::= #StackEltToPush(StackElement) [function]
  rule #StackEltToPush(Stack_elt (contract _:AnnotationList T:Type) D:Data) => { PUSH .AnnotationList address .AnnotationList D ;
                                                                                 CONTRACT .AnnotationList T ;
                                                                                 IF_SOME .AnnotationList { } { UNIT .AnnotationList ; FAILWITH .AnnotationList } }
  rule #StackEltToPush(Stack_elt T D) => { PUSH .AnnotationList T D } [owise]

  syntax Block ::= #StackToPush(LiteralStack) [function]
  rule #StackToPush( { } ) => { }

  syntax Block ::= #StackToPushAux(StackElementList, Block) [function]
  rule #StackToPush( { Se } ) => #StackToPushAux(Se, { DROP .AnnotationList 0 })
  rule #StackToPushAux(Se ; Ls, { I:DataList })  => #StackToPushAux(Ls, { #StackEltToPush(Se) ; I })
  rule #StackToPushAux(Se, { I:DataList })  => { #StackEltToPush(Se) ; I }

  syntax Contract ::= #FillTemplateContract(Block, Block, Type) [function]

  syntax Bool ::= #CodeBlockEndsWithFail(Block) [function]
  rule #CodeBlockEndsWithFail( { } ) => false
  rule #CodeBlockEndsWithFail( { { Is } } ) => #CodeBlockEndsWithFail( { Is } )
  rule #CodeBlockEndsWithFail( { I:Instruction } ) => ((FAILWITH _) :=K I) [owise]
  rule #CodeBlockEndsWithFail( { _:Instruction ; Is } ) => #CodeBlockEndsWithFail( { Is } )

  rule #FillTemplateContract(PushBlock, CodeBlock, ParamType) =>
  parameter ParamType ; storage unit .AnnotationList ; code { DROP .AnnotationList ; PushBlock ; CodeBlock ; UNIT @exitToken ; FAILWITH .AnnotationList }  ;
  requires notBool #CodeBlockEndsWithFail(CodeBlock)

  rule #FillTemplateContract(PushBlock, CodeBlock, ParamType) =>
  parameter ParamType ; storage unit .AnnotationList ; code { DROP .AnnotationList ; PushBlock ; CodeBlock }  ;
  requires #CodeBlockEndsWithFail(CodeBlock)

  syntax LiteralStack ::= #FindInputGroup(Groups) [function]
  rule #FindInputGroup(input T) => T
  rule #FindInputGroup(input T;) => T
  rule #FindInputGroup(input T ; _) => T
  rule #FindInputGroup(_ ; G) => #FindInputGroup(G) [owise]

  syntax Block ::= #FindCodeGroup(Groups) [function]
  rule #FindCodeGroup(code T) => { T }
  rule #FindCodeGroup(code T;) => { T }
  rule #FindCodeGroup(code T ; _) => { T }
  rule #FindCodeGroup(_ ; G) => #FindCodeGroup(G) [owise]

  syntax Type ::= #FindParamType(Groups) [function]
  rule #FindParamType(parameter T) => T
  rule #FindParamType(parameter T;) => T
  rule #FindParamType(parameter T ; Rs) => T
  rule #FindParamType(_:Group) => unit .AnnotationList [owise]
  rule #FindParamType(_:Group ;) => unit .AnnotationList [owise]
  rule #FindParamType(_ ; Rs) => #FindParamType(Rs) [owise]

  rule <k> G:Groups => . </k>
       <out> ... .List => ListItem(#unparse(#FillTemplateContract(#StackToPush(#FindInputGroup(G)), #FindCodeGroup(G), #FindParamType(G)))) </out>
       <returncode> _ => 0 </returncode>
endmodule
```
