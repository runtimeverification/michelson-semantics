```k
requires "symbolic-configuration.k"
requires "symbolic-unit-test-syntax.k"

module MICHELSON-CLAIMS
  imports SYMBOLIC-CONFIGURATION 
  imports SYMBOLIC-UNIT-TEST-SYNTAX

  syntax Antecedent
  syntax Consequent

  syntax Inherits ::= "#Inherit"

  syntax Antecedent ::= Inherits
  syntax Consequent ::= Inherits

  syntax Antecedent ::= #Precondition(List)
  syntax Consequent ::= #Postcondition(List)

  syntax Antecedent ::= #LoopTermination(TypedInstruction, Invariant)

  syntax Antecedent ::= Invariant
  syntax Consequent ::= Invariant

  syntax Claim ::= Antecedent "->" Consequent

  syntax ClaimNode ::= #ClaimNode(Claim, TypedInstruction, ClaimNodes)
  syntax ClaimNodes ::= List{ClaimNode, ""}

  syntax ClaimNode ::= #GetClaims(Block) [function]
  syntax ClaimNodes ::= #GetSubClaims(Block) [function]

  rule [[ #GetClaims({ #Exec(Is) } #as B) => #ClaimNode(#Precondition(Pr) -> #Postcondition(Po), Is, #GetSubClaims(B)) ]]
       <preconditions> Pr </preconditions>
       <postconditions> Po </postconditions>

  syntax CTypedInstructions ::= List{TypedInstruction, ":"}

  syntax CTypedInstructions ::= #CTypedToList(TypedInstructionList) [function]

  rule #CTypedToList(T:TypedInstruction) => T
  rule #CTypedToList(T ; Ts) => T : #CTypedToList(Ts)

  rule #GetSubClaims({ }) => .ClaimNodes
  rule #GetSubClaims({ #Exec(Is) }) => #GetSubClaimsAux(#CTypedToList(Is), #Inherit, .CTypedInstructions, .ClaimNodes)

  syntax ClaimNodes ::= #GetSubClaimsAux(CTypedInstructions, Antecedent, CTypedInstructions, ClaimNodes)  [function]

  syntax VariableAnnotation ::= #GetLoopInvariantName(AnnotationList) [function]
  rule #GetLoopInvariantName(V:VariableAnnotation Rs) => V
  rule #GetLoopInvariantName(_:Annotation Rs) => #GetLoopInvariant(Rs) [owise]

  syntax Invariant ::= #GetLoopInvariant(AnnotationList) [function]
  syntax Invariant ::= #GetLoopInvariantAux(VariableAnnotation) [function]
  rule #GetLoopInvariant(A) => #GetLoopInvariantAux(#GetLoopInvariantName(A))
  rule [[ #GetLoopInvariantAux(V) => M[V] ]]
       <invariants> M </invariants>

  syntax Antecedent ::= #GetPostLoopAntecedent(TypedInstruction, AnnotationList) [function]
  rule #GetPostLoopAntecedent(TI, AL) => #LoopTermination(TI, #GetLoopInvariant(AL))

  syntax CTypedInstructions ::= #ReverseCTyped(CTypedInstructions) [function]
  syntax CTypedInstructions ::= #ReverseCTypedAux(CTypedInstructions, CTypedInstructions) [function]

  rule #ReverseCTyped(C) => #ReverseCTypedAux(C, .CTypedInstructions) 
  rule #ReverseCTypedAux(C1 : CS1, CS2) => #ReverseCTypedAux(CS1, C1 : CS2)
  rule #ReverseCTypedAux(.CTypedInstructions, _) => .CTypedInstructions

  syntax TypedInstructionList ::= #ToInstructionList(CTypedInstructions, TypeSeq) [function]

  rule #ToInstructionList(C1 : C2, _) => C1 ; #ToInstructionList(C2)
  rule #ToInstructionList(C1:TypedInstruction, _) => C1
  rule #ToInstructionList(.CTypedInstructions, T1) => #TI({ }, T1 -> T1)

  syntax TypedInstruction ::= #ComposeTypedInstructions(TypedInstructionList) [function]

  rule #GetSubClaimsAux(I : Rs, A, Is, CN) => #GetSubClaimsAux(Rs, A, I : Is, CN) [owise]

  rule #GetSubClaimsAux((#TI(LOOP A B #Or LOOP_LEFT A B #Or MAP A B #Or ITER A B, T1 -> T2) #as I) : Rs, AN, Is, CN) => 
       #GetSubClaimsAux(Rs, #GetPostLoopAntecedent(I, A), .CTypedInstructions, CN
                        #ClaimNode(AN -> #GetLoopInvariant(A), #ComposeTypedInstructions(#ToInstructionList(#ReverseCTyped(Is), )), .ClaimNodes)
                        #ClaimNode(#GetLoopInvariant(A) -> #GetLoopInvariant(A), I, #GetSubClaims(B)))

  rule #GetSubClaimsAux(.ClaimNodes, AN, Is, CN) => CN #ClaimNode(AN -> #Inherit, #TypeInstruction({ #Exec(#ReverseCTyped(Is)) }), .ClaimNodes)
endmodule
```
