```k
requires "michelson/common.md"

module MICHELSON-TYPES
  imports MICHELSON-COMMON
  imports DOMAINS

  syntax TypeContext ::= Type // Parameter type.

  syntax TypeSeq ::= List{Type, ";"}
  syntax TypeTransition ::= TypeSeq "->" TypeInput


  syntax FailureType ::= "#ContractFailed"

  syntax TypeError ::= "#InternalError"

  syntax TypeInput ::= TypeSeq | TypeError
  syntax TypeResult ::= TypeTransition | TypeError | FailureType

  syntax TypedInstruction ::= #TypeInstruction(TypeContext, Instruction, TypeSeq) [function, functional]
  syntax TypedInstructions ::= #TypeInstructions(TypeContext, DataList, TypeInput) [function, functional]

  syntax TypedInstruction ::= #TI(Instruction, TypeResult)
  syntax TypedInstructions ::= #TIs(TypedInstructionList, TypeResult)

  syntax TypedInstructionList ::= TypedInstruction ";" TypedInstructionList | TypedInstruction
                                | #Remaining(DataList)

  syntax TypeError ::= #InvalidTypeForInstruction(Instruction, TypeSeq)
                     | #IncompatibleTypesForBranch(Instruction, TypeInput, TypeInput)
                     | #UnexpectedMacro(Macro, TypeSeq)
                     | "#UnexpectedFailureType"

  syntax TypeInput ::= #EndType(TypeResult) [function, functional]

  rule #EndType(TE:TypeError) => TE
  rule #EndType(_:TypeSeq -> TR) => TR
  rule #EndType(#ContractFailed) => #UnexpectedFailureType

  syntax TypeResult ::= #MergeResults(TypeSeq, TypeResult) [function, functional]
  rule #MergeResults(_, E:TypeError) => E
  rule #MergeResults(TS, #ContractFailed) => #ContractFailed
  rule #MergeResults(TS, _ -> TD) => TS -> TD

  syntax MaybeData ::= #TypeData(TypeContext, Data, Type) [function, functional]
  syntax MaybeData ::= #CheckInnerData(Data, Type, List) [function, functional]

  syntax TypeError ::= #MistypedData(Data, Type)
  rule #TypeData(C, D, T) => #MistypedData(D, T) [owise]

  syntax TypedData ::= #Typed(Data, Type)

  syntax MaybeData ::= TypedData | TypeError

  syntax Data ::= TypedData

  rule #TypeData(_, Unit, unit _) => #Typed(Unit, unit .AnnotationList)

  rule #TypeData(_, (_:MBytes) #as D, (chain_id _) #as T) => #Typed(D, T)
  rule #TypeData(_, (_:Int) #as D, (int _) #as T) => #Typed(D, T)
  rule #TypeData(_, (I:Int) #as D, (nat _) #as T) => #Typed(D, T) requires I >=Int 0
  rule #TypeData(_, (_:String) #as D, (string _) #as T) => #Typed(D, T)
  rule #TypeData(_, (_:MBytes) #as D, (bytes _) #as T) => #Typed(D, T)
  rule #TypeData(_, (I:Int) #as D, (mutez _) #as T) => #Typed(D, T)
       requires #IsLegalMutezValue(I)
  rule #TypeData(_, (_:Bool) #as D, (bool _) #as T) => #Typed(D, T)
  rule #TypeData(_, (_:String) #as D, (key_hash _) #as T) => #Typed(D, T)
  rule #TypeData(_, (_:Int) #as D, (timestamp _) #as T) => #Typed(D, T)
  rule #TypeData(_, (_:String) #as D, (timestamp _) #as T) => #Typed(D, T)
  rule #TypeData(_, (_:String) #as D, (address _) #as T) => #Typed(D, T)
  rule #TypeData(_, (_:String) #as D, (key _) #as T) => #Typed(D, T)
  rule #TypeData(_, (_:String) #as D, (signature _) #as T) => #Typed(D, T)

  rule #TypeData(C, (Pair VL VR) #as D, (pair _ TL TR) #as T) => #CheckInnerData(D, T, ListItem(#TypeData(C, VL, TL)) ListItem(#TypeData(C, VR, TR)))

  rule #TypeData(C, (Some V) #as D, (option _ TI) #as T) => #CheckInnerData(D, T, ListItem(#TypeData(C, V, TI)))
  rule #TypeData(C, None #as D, (option _ TI) #as T) => #Typed(D, T)

  rule #TypeData(C, (Left V) #as D, (or _ TI _) #as T) => #CheckInnerData(D, T, ListItem(#TypeData(C, V, TI)))
  rule #TypeData(C, (Right V) #as D, (or _ _ TI) #as T) => #CheckInnerData(D, T, ListItem(#TypeData(C, V, TI)))

  rule #TypeData(C, D:String, (contract _ _) #as T) => #Typed(D, T)

  syntax Bool ::= #AllWellTyped(List) [function, functional]
  rule #AllWellTyped(ListItem(#Typed(_, _)) L) => #AllWellTyped(L)
  rule #AllWellTyped(ListItem(_) L) => false [owise]
  rule #AllWellTyped(.List) => true

  syntax TypeError ::= #MistypedInnerData(List)

  rule #CheckInnerData(D, T, L) => #Typed(D, T) requires #AllWellTyped(L)
  rule #CheckInnerData(_, _, L) => #MistypedInnerData(L) [owise]

  syntax MaybeData ::= #TypeLambdaAux(TypedInstruction, Type, Type) [function, functional]
  syntax TypeError ::= #IllTypedLambda(TypedInstruction, Type, Type)

  rule #TypeData(C, B:Block, lambda _ T1 T2) => #TypeLambdaAux(#TypeInstruction(C, B, T1), T1, T2)

  rule #TypeData(C, #Lambda(T1, T2, B), lambda _ T1 T2) => #TypeLambdaAux(#TypeInstruction(C, B, T1), T1, T2)

  rule #TypeLambdaAux((#TI(_, T1:Type ; .TypeSeq -> T2:Type ; .TypeSeq) #as B), T1:Type, T2:Type) => #Typed({ #Exec(B) }, lambda .AnnotationList T1 T2)
  rule #TypeLambdaAux(T, T1, T2) => #IllTypedLambda(T, T1, T2) [owise]

  syntax DataList ::= #Exec(TypedInstructionList)

  rule #TypeData(C, { } #as D, (list _ _) #as T) => #Typed(D, T)
  rule #TypeData(C, { } #as D, (set _ _) #as T) => #Typed(D, T)
  rule #TypeData(C, { } #as D, (map _ _ _) #as T) => #Typed(D, T)
  rule #TypeData(C, { } #as D, (big_map _ _ _) #as T) => #Typed(D, T)

  syntax List ::= #TypeCheckListElements(TypeContext, DataList, Type) [function, functional]

  rule #TypeCheckListElements(C, D:Data, T) => ListItem(#TypeData(C, D, T))
  rule #TypeCheckListElements(C, D ; DL, T) => ListItem(#TypeData(C, D, T)) #TypeCheckListElements(C, DL, T)

  rule #TypeData(C, { DL } #as D, (list _ TI) #as T) => #CheckInnerData(D, T, #TypeCheckListElements(C, DL, TI))
  rule #TypeData(C, { DL } #as D, (set _ TI) #as T) => #CheckInnerData(D, T, #TypeCheckListElements(C, DL, TI))

  syntax List ::= #TypeCheckMapElements(TypeContext, MapEntryList, Type, Type) [function, functional]
  rule #TypeCheckMapElements(C, Elt K V, KT, VT) => ListItem(#TypeData(C, K, KT)) ListItem(#TypeData(C, V, VT))
  rule #TypeCheckMapElements(C, Elt K V ; Rs, KT, VT) => ListItem(#TypeData(C, K, KT)) ListItem(#TypeData(C, V, VT)) #TypeCheckMapElements(C, Rs, KT, VT)

  rule #TypeData(C, { DL } #as D, (map _ KT VT) #as T) => #CheckInnerData(D, T, #TypeCheckMapElements(C, DL, KT, VT))
  rule #TypeData(C, { DL } #as D, (big_map _ KT VT) #as T) => #CheckInnerData(D, T, #TypeCheckMapElements(C, DL, KT, VT))

  rule #TypeData(C, I:Int, (big_map _ KT VT) #as T) => #Typed(I, T)

  rule #TypeData(C, (Create_contract(_, _, _, _, _) #as D), (operation _) #as T) => #Typed(D, T)
  rule #TypeData(C, (Transfer_tokens(_, _, _, _) #as D), (operation _) #as T) => #Typed(D, T)
  rule #TypeData(C, (Set_delegate(_, _) #as D), (operation _) #as T) => #Typed(D, T)

  rule #TypeInstruction(C, { }, TS) => #TI({ }, TS -> TS)
  rule #TypeInstruction(C, { Is:DataList }, TS) => #fun(#TIs(Is2, TR) => #TI({ #Exec(Is2) }, TR))(#TypeInstructions(C, Is, TS))

  rule #TypeInstructions(C, Is, TE:TypeError) => #TIs(#Remaining(Is), TE)

  syntax TypeError ::= #SequenceError(TypeInput)

  rule #TypeInstructions(_, Is, TR) => #TIs(#Remaining(Is), #SequenceError(TR)) [owise]



  rule #TypeInstructions(C, I1 ; Is, Input:TypeSeq) => #fun(#TI(_, TR1) #as T => #fun(#TIs(Ts2, TR2) => #TIs(T ; Ts2, #MergeResults(Input, TR2)))(#TypeInstructions(C, Is, #EndType(TR1))))(#TypeInstruction(C, I1, Input))

  rule #TypeInstructions(C, I:Instruction, TS:TypeSeq) => #fun(#TI(I2, TR) => #TIs(#TI(I2, TR), TR))(#TypeInstruction(C, I, TS))

  rule #TypeInstruction(C, (DROP _) #as I, (_ ; Rs) #as T1) => #TI(I, T1 -> Rs)
  rule #TypeInstruction(C, (DROP _ N) #as I, T1) => #TI(I, T1 -> #DropFirst(T1, N))

  syntax TypeInput ::= #DropFirst(TypeSeq, Int) [function, functional]
  syntax TypeError ::= #InvalidDropCount(TypeSeq, Int)

  rule #DropFirst(TS, I) => #InvalidDropCount(TS, I) [owise]
  rule #DropFirst(TS, 0) => TS
  rule #DropFirst(_ ; TS, I) => #DropFirst(TS, I -Int 1) requires I >Int 0


  rule #TypeInstruction(C, (DIG _ N) #as I, T1) => #TI(I, T1 -> #DigType(T1, N))

  syntax TypeError ::= #InvalidDigCount(TypeSeq, Int)

  syntax TypeInput ::= #DigType(TypeSeq, Int) [function, functional]
  rule #DigType(TS, I) => #DigTypeAux(TS, I, #GetTypeN(TS, I))

  syntax TypeInput ::= #DigTypeAux(TypeSeq, Int, MaybeType) [function, functional]
  rule #DigTypeAux(TS, I, T:Type) => T ; #RemoveN(TS, I)
  rule #DigTypeAux(TS, I, #NoType) => #InvalidDigCount(TS, I)

  syntax MaybeType ::= Type | "#NoType"

  syntax MaybeType ::= #GetTypeN(TypeSeq, Int) [function, functional]
  rule #GetTypeN(T1 ; _, 0) => T1
  rule #GetTypeN(_ ; Ts, I) => #GetTypeN(Ts, I -Int 1) requires I >Int 0
  rule #GetTypeN(_, _) => #NoType [owise]

  syntax TypeSeq ::= #RemoveN(TypeSeq, Int) [function, functional]
  rule #RemoveN(T1 ; Ts, 0) => Ts
  rule #RemoveN(T1 ; Ts, I) => T1 ; #RemoveN(Ts, I -Int 1) requires I >Int 0
  rule #RemoveN(Ts, I) => Ts [owise] // This rule is unreachable.

  syntax TypeSeq ::= #RemoveFirstN(TypeSeq, Int) [function, functional]
  rule #RemoveFirstN(Ts, 0) => Ts
  rule #RemoveFirstN(T1 ; Ts, I) => #RemoveFirstN(Ts, I -Int 1) requires I >Int 0
  rule #RemoveFirstN(Ts, I) => Ts [owise] // This rule is unreachable.



  rule #TypeInstruction(C, (DUG _ N) #as I, T1) => #TI(I, T1 -> #DoDug(T1, N))

  syntax Int ::= #LengthTypeSeq(TypeSeq) [function, functional]
  rule #LengthTypeSeq(.TypeSeq) => 0
  rule #LengthTypeSeq(_ ; Ts) => 1 +Int #LengthTypeSeq(Ts)

  syntax TypeError ::= #InvalidDugCount(TypeSeq, Int)

  syntax TypeInput ::= #DoDug(TypeSeq, Int) [function, functional]
  rule #DoDug(TS, I) => #InvalidDugCount(TS, I) requires #LengthTypeSeq(TS) <=Int I orBool
                                                           I <Int 0
  rule #DoDug(TS, 0) => TS
  rule #DoDug(T1 ; TS, I) => #DoDugAux(TS, I, T1)

  syntax TypeSeq ::= #DoDugAux(TypeSeq, Int, Type) [function, functional]
  rule #DoDugAux(_, I, T) => .TypeSeq requires I <Int 0 // This pattern is unreachable.
  rule #DoDugAux(TS, 0, T) => T ; TS
  rule #DoDugAux(T1 ; TS, I, T) => T1 ; #DoDugAux(TS, I -Int 1, T)

  rule #TypeInstruction(C, (DUP _) #as I, T1 ; Ts) => #TI(I, T1 ; Ts -> T1 ; T1 ; Ts)
  rule #TypeInstruction(C, (SWAP _) #as I, T1 ; T2 ; Ts) => #TI(I, T1 ; T2 ; Ts -> T2 ; T1 ; Ts)

  syntax TypedInstruction ::= #PushAux(Instruction, MaybeData, TypeSeq) [function, functional]
  syntax TypeError ::= #InvalidPush(Instruction, TypeError)

  rule #PushAux(I, _, _) => #TI(I, #InternalError) [owise]
  rule #PushAux(I, TE:TypeError, _) => #TI(I, #InvalidPush(I, TE))
  rule #PushAux(PUSH _ T _, TD:TypedData, Ts) => #TI(PUSH .AnnotationList T TD, Ts -> T ; Ts)

  rule #TypeInstruction(C, (PUSH _ T D) #as I, Ts) => #PushAux(I, #TypeData(C, D, T), Ts)

  rule #TypeInstruction(C, (SOME _) #as I, (T1 ; Ts) #as OS) => #TI(I, OS -> option .AnnotationList T1 ; Ts)
  rule #TypeInstruction(C, (NONE _ T) #as I, Ts) => #TI(I, Ts -> option .AnnotationList T ; Ts)

  rule #TypeInstruction(C, (UNIT _) #as I, Ts) => #TI(I, Ts -> unit .AnnotationList ; Ts)

  syntax TypedInstruction ::= #UnifyBranches(Instruction, TypedInstruction, TypedInstruction, TypeSeq) [function, functional]

  syntax TypeError ::= #MultipleTypeErrors(TypeError, TypeError)

  rule #TypeInstruction(C, (IF_NONE A1 B1 B2) #as I, (option A2 T1 ; Ts1) #as Os) =>
       #UnifyBranches(I, #TypeInstruction(C, B1, Ts1), #TypeInstruction(C, B2, T1 ; Ts1), Os)

  syntax Instruction ::= #SubTypedBranches(Instruction, Block, Block) [function, functional]
  syntax Instruction ::= #InvalidBranchInstruction(Instruction)
  rule #SubTypedBranches(IF_NONE _ _ _, B1, B2)  => IF_NONE  .AnnotationList B1 B2
  rule #SubTypedBranches(IF_LEFT _ _ _, B1, B2)  => IF_LEFT  .AnnotationList B1 B2
  rule #SubTypedBranches(IF_RIGHT _ _ _, B1, B2) => IF_RIGHT .AnnotationList B1 B2
  rule #SubTypedBranches(IF_CONS _ _ _, B1, B2)  => IF_CONS  .AnnotationList B1 B2
  rule #SubTypedBranches(IF      _ _ _, B1, B2)  => IF       .AnnotationList B1 B2
  rule #SubTypedBranches(I, _, _) => #InvalidBranchInstruction(I) [owise]

  syntax TypeError ::= #IllegalBranchInstruction(Instruction) // Internal error.

  syntax TypedInstruction ::= #MakeTypedBranch(Instruction, TypeTransition) [function, functional]
  rule #MakeTypedBranch(I, T) => #TI(I, T) [owise]
  rule #MakeTypedBranch(#InvalidBranchInstruction(I), _) => #TI(I, #IllegalBranchInstruction(I))

  rule #UnifyBranches(I, #TI(_, _ -> D:TypeSeq) #as B1, #TI(_, _ -> D:TypeSeq) #as B2, Os) =>
       #MakeTypedBranch(#SubTypedBranches(I, { #Exec(B1) }, { #Exec(B2) }), Os -> D)

  rule #UnifyBranches(I, #TI(_, _ -> V1:TypeSeq), #TI(_, _ -> V2:TypeSeq), _) => #TI(I, #IncompatibleTypesForBranch(I, V1, V2)) requires V1 =/=K V2

  rule #UnifyBranches(I, #TI(_, #ContractFailed) #as B1, #TI(_, _ -> D:TypeSeq) #as B2, Os) =>
       #MakeTypedBranch(#SubTypedBranches(I, { #Exec(B1) }, { #Exec(B2) }), Os -> D)

  rule #UnifyBranches(I, #TI(_, _ -> D:TypeSeq) #as B1, #TI(_, #ContractFailed) #as B2, Os) =>
       #MakeTypedBranch(#SubTypedBranches(I, { #Exec(B1) }, { #Exec(B2) }), Os -> D)

  rule #UnifyBranches(I, #TI(_, #ContractFailed) #as B1, #TI(_, #ContractFailed) #as B2, Os) => #TI(I, #UnexpectedFailureType)

  rule #UnifyBranches(I, #TI(_, TE:TypeError), #TI(_, _ -> _), _) => #TI(I, TE)
  rule #UnifyBranches(I, #TI(_, _ -> _), #TI(_, TE:TypeError), _) => #TI(I, TE)
  rule #UnifyBranches(I, #TI(_, TE1:TypeError), #TI(_, TE2:TypeError), _) => #TI(I, #MultipleTypeErrors(TE1, TE2))
  rule #UnifyBranches(I, _, _, _) => #TI(I, #InternalError) [owise] // Unreachable.

  rule #TypeInstruction(C, (PAIR _) #as I, (T1 ; T2 ; Ts) #as Os) => #TI(I, Os -> pair .AnnotationList T1 T2 ; Ts)

  rule #TypeInstruction(C, (UNPAIR _) #as I, (pair _ T1 T2 ; Ts) #as Os) => #TI(I, Os -> T1 ; T2 ; Ts)

  rule #TypeInstruction(C, (CAR _) #as I, (pair _ T1 _ ; Ts) #as Os) => #TI(I, Os -> T1 ; Ts)
  rule #TypeInstruction(C, (CDR _) #as I, (pair _ _ T2 ; Ts) #as Os) => #TI(I, Os -> T2 ; Ts)

  rule #TypeInstruction(C, (LEFT _ TR) #as I, (TL ; Ts) #as OS) => #TI(I, OS -> (or .AnnotationList TL TR) ; Ts)
  rule #TypeInstruction(C, (RIGHT _ TL) #as I, (TR ; Ts) #as OS) => #TI(I, OS -> (or .AnnotationList TL TR) ; Ts)

  rule #TypeInstruction(C, (IF_LEFT _ BL BR) #as I, ((or _ TL TR) ; Ts) #as OS) => #UnifyBranches(I, #TypeInstruction(C, BL, TL ; Ts), #TypeInstruction(C, BR, TR ; Ts), OS)
  rule #TypeInstruction(C, (IF_RIGHT _ BL BR) #as I, ((or _ TR TL) ; Ts) #as OS) => #UnifyBranches(I, #TypeInstruction(C, BL, TL ; Ts), #TypeInstruction(C, BR, TR ; Ts), OS)


  rule #TypeInstruction(C, (NIL _ T) #as I, Ts) => #TI(I, Ts -> (list .AnnotationList T) ; Ts)
  rule #TypeInstruction(C, (CONS _) #as I, (T ; list _ T ; Ts) #as OS) => #TI(I, OS -> list .AnnotationList T ; Ts)

  rule #TypeInstruction(C, (IF_CONS _ B1 B2) #as I, (list _ T ; Ts) #as OS) => #UnifyBranches(I, #TypeInstruction(C, B1, T ; list .AnnotationList T ; Ts), #TypeInstruction(C, B2, Ts), OS)

  rule #TypeInstruction(C, (SIZE _) #as I, (list _ _ ; Ts) #as OS) => #TI(I, OS -> nat .AnnotationList ; Ts)
  rule #TypeInstruction(C, (SIZE _) #as I, (set _ _ ; Ts) #as OS) => #TI(I, OS -> nat .AnnotationList ; Ts)
  rule #TypeInstruction(C, (SIZE _) #as I, (map _ _ _ ; Ts) #as OS) => #TI(I, OS -> nat .AnnotationList ; Ts)
  rule #TypeInstruction(C, (SIZE _) #as I, (string _ ; Ts) #as OS) => #TI(I, OS -> nat .AnnotationList ; Ts)
  rule #TypeInstruction(C, (SIZE _) #as I, (bytes _ ; Ts) #as OS) => #TI(I, OS -> nat .AnnotationList ; Ts)

  rule #TypeInstruction(C, (EMPTY_SET _ T) #as I, Ts) => #TI(I, Ts -> set .AnnotationList T ; Ts)
  rule #TypeInstruction(C, (EMPTY_MAP _ KT VT) #as I, Ts) => #TI(I, Ts -> map .AnnotationList KT VT ; Ts)
  rule #TypeInstruction(C, (EMPTY_BIG_MAP _ KT VT) #as I, Ts) => #TI(I, Ts -> big_map .AnnotationList KT VT ; Ts)

  rule #TypeInstruction(C, I, T) => #TI(I, #InvalidTypeForInstruction(I, T)) [owise]

  syntax TypeError ::= #InvalidPostIterationStack(Instruction, TypeSeq, TypeSeq)

  syntax TypedInstruction ::= #MapAux(Instruction, TypedInstruction, TypeSeq) [function, functional]
  rule #MapAux(MAP _ _, (#TI(_, _ -> N ; Ts) #as B), (list _ T ; Ts) #as OS) => #TI(MAP .AnnotationList { #Exec(B) }, OS -> (list .AnnotationList N) ; Ts)
  rule #MapAux(MAP _ _, (#TI(_, _ -> VT ; Ts) #as B), (map _ KT _ ; Ts) #as OS) => #TI(MAP .AnnotationList  { #Exec(B) }, OS -> (map .AnnotationList KT VT) ; Ts)

  rule #MapAux(I, (#TI(_, _ -> (N ; Ts1) #as DS) #as B), (_ ; Ts2) #as OS) => #TI(I, #InvalidPostIterationStack(I, OS, DS)) requires Ts1 =/=K Ts2
  rule #MapAux(I, #TI(_, #ContractFailed),  _) => #TI(I, #UnexpectedFailureType)
  rule #MapAux(I, #TI(_, TE:TypeError),  _) => #TI(I, TE)
  rule #MapAux(I, _, _) => #TI(I, #InternalError) [owise] // Unreachable

  rule #TypeInstruction(C, (MAP _ B) #as I, (list _ T ; Ts) #as OS) => #MapAux(I, #TypeInstruction(C, B, T ; Ts), OS)
  rule #TypeInstruction(C, (MAP _ B) #as I, (map _ KT VT ; Ts) #as OS) => #MapAux(I, #TypeInstruction(C, B, (pair .AnnotationList KT VT) ; Ts), OS)


  syntax TypedInstruction ::= #IterAux(Instruction, TypedInstruction, TypeSeq) [function, functional]
  rule #IterAux(ITER _ _, (#TI(_, _ -> Ts) #as B), (_ ; Ts) #as OS) => #TI(ITER .AnnotationList { #Exec(B) }, OS -> Ts)

  rule #IterAux(I, (#TI(_, _ -> Ts1) #as B), (_ ; Ts2) #as OS) => #TI(I, #InvalidPostIterationStack(I, OS, Ts1)) requires Ts1 =/=K Ts2
  rule #IterAux(ITER _ _, #TI(_, #ContractFailed) #as B,  (_ ; Ts) #as OS) => #TI(ITER .AnnotationList { #Exec(B) }, OS -> Ts)
  rule #IterAux(I, #TI(_, TE:TypeError),  _) => #TI(I, TE)
  rule #IterAux(I, _, _) => #TI(I, #InternalError) [owise] // Unreachable

  rule #TypeInstruction(C, (ITER _ B) #as I, (list _ T ; Ts) #as OS) => #IterAux(I, #TypeInstruction(C, B, T ; Ts), OS)
  rule #TypeInstruction(C, (ITER _ B) #as I, (set _ T ; Ts) #as OS) => #IterAux(I, #TypeInstruction(C, B, T ; Ts), OS)
  rule #TypeInstruction(C, (ITER _ B) #as I, (map _ KT VT ; Ts) #as OS) => #IterAux(I, #TypeInstruction(C, B, (pair .AnnotationList KT VT) ; Ts), OS)

  rule #TypeInstruction(C, (MEM _) #as I, (KT ; map _ KT VT ; Ts) #as OS) => #TI(I, OS -> bool .AnnotationList ; Ts)
  rule #TypeInstruction(C, (MEM _) #as I, (KT ; big_map _ KT VT ; Ts) #as OS) => #TI(I, OS -> bool .AnnotationList ; Ts)
  rule #TypeInstruction(C, (MEM _) #as I, (T ; set _ T ; Ts) #as OS) => #TI(I, OS -> bool .AnnotationList ; Ts)

  rule #TypeInstruction(C, (GET _) #as I, (KT ; map _ KT VT ; Ts) #as OS) => #TI(I, OS -> option .AnnotationList VT ; Ts)
  rule #TypeInstruction(C, (GET _) #as I, (KT ; big_map _ KT VT ; Ts) #as OS) => #TI(I, OS -> option .AnnotationList VT ; Ts)


  rule #TypeInstruction(C, (UPDATE _) #as I, (KT ; option _ VT ; map _ KT VT ; Ts) #as OS) => #TI(I, OS -> map .AnnotationList KT VT ; Ts)
  rule #TypeInstruction(C, (UPDATE _) #as I, (KT ; option _ VT ; big_map _ KT VT ; Ts) #as OS) => #TI(I, OS -> big_map .AnnotationList KT VT ; Ts)
  rule #TypeInstruction(C, (UPDATE _) #as I, (T ; bool _ ; set _ T ; Ts) #as OS) => #TI(I, OS -> set .AnnotationList T ; Ts)

  rule #TypeInstruction(C, (IF _ BL BR) #as I, (bool _ ; Ts) #as OS) => #UnifyBranches(I, #TypeInstruction(C, BL, Ts), #TypeInstruction(C, BR, Ts), OS)

  syntax TypedInstruction ::= #LoopAux(Instruction, TypedInstruction, TypeSeq) [function, functional]

  rule #LoopAux(LOOP _ _, #TI(_, Ts -> (bool _) ; Ts) #as B, ((bool _) ; Ts) #as OS) => #TI((LOOP .AnnotationList { #Exec(B) }), OS -> Ts)
  rule #LoopAux(I, #TI(_, _ -> Ts1), Ts2) => #TI(I, #InvalidPostIterationStack(I, Ts1, Ts2)) requires Ts1 =/=K Ts2
  rule #LoopAux(I, #TI(_, #ContractFailed) #as B, ((bool _) ; Ts) #as OS) => #TI((LOOP .AnnotationList { #Exec(B) }), OS -> Ts)
  rule #LoopAux(I, #TI(_, TE:TypeError),  _) => #TI(I, TE)
  rule #LoopAux(I, _, _) => #TI(I, #InternalError) [owise] // Unreachable

  rule #TypeInstruction(C, (LOOP _ B) #as I, ((bool _) ; Ts) #as OS) => #LoopAux(I, #TypeInstruction(C, B, Ts), OS)

  syntax TypedInstruction ::= #LoopLeftAux(Instruction, TypedInstruction, TypeSeq) [function, functional]

  rule #LoopLeftAux(LOOP_LEFT _ _, #TI(_, TL ; Ts -> (or _ TL TR) ; Ts) #as B, ((or _ TL TR) ; Ts) #as OS) => #TI((LOOP_LEFT .AnnotationList { #Exec(B) }), OS -> TR ; Ts)
  rule #LoopLeftAux(I, #TI(_, _ -> Ts1), Ts2) => #TI(I, #InvalidPostIterationStack(I, Ts1, Ts2)) requires Ts1 =/=K Ts2
  rule #LoopLeftAux(I, #TI(_, #ContractFailed) #as B,  ((or _ TL TR) ; Ts) #as OS) => #TI((LOOP_LEFT .AnnotationList { #Exec(B) }), OS -> TR ; Ts)
  rule #LoopLeftAux(I, #TI(_, TE:TypeError),  _) => #TI(I, TE)
  rule #LoopLeftAux(I, _, _) => #TI(I, #InternalError) [owise]

  rule #TypeInstruction(C, (LOOP_LEFT _ B) #as I, (or _ TL _ ; Ts) #as OS) => #LoopLeftAux(I, #TypeInstruction(C, B, TL ; Ts), OS)

  syntax TypedInstruction ::= #LambdaAux(Instruction, TypedInstruction, TypeSeq) [function, functional]

  syntax TypeError ::= #IllTypedLambdaInst(TypedInstruction, Type, Type, Bool, Bool)

  rule #LambdaAux(LAMBDA _ T1:Type T2:Type _, #TI(_, T1:Type ; .TypeSeq -> T2:Type ; .TypeSeq) #as B, OS) => #TI(LAMBDA .AnnotationList T1 T2 { #Exec(B) }, OS -> lambda .AnnotationList T1 T2 ; OS)
  rule #LambdaAux(LAMBDA _ T1 T2 _ #as I, #TI(_, T3 -> T4) #as B, _) => #TI(I, #IllTypedLambdaInst(B, T1, T2, (T1 ; .TypeSeq) ==K T3, (T2 ; .TypeSeq) ==K T4))
  requires (T1 ; .TypeSeq) =/=K T3 orBool (T2 ; .TypeSeq) =/=K T4

  syntax TypeError ::= #LambdaError(Instruction, TypedInstruction, TypeSeq)

  rule #LambdaAux(I, T, Ts) => #TI(I, #LambdaError(I, T, Ts)) [owise]

  rule #TypeInstruction(C, (LAMBDA _ T1 T2 B) #as I, Os) => #LambdaAux(I, #TypeInstruction(C, B, T1), Os)

  rule #TypeInstruction(C, (EXEC _) #as I, (T1 ; lambda _ T1 T2 ; Ts) #as OS) => #TI(I, OS -> T2 ; Ts)

  rule #TypeInstruction(C, (APPLY _) #as I, (TL ; lambda _ (pair .AnnotationList TL TR) T2 ; Ts) #as OS) => #TI(I, OS -> lambda .AnnotationList TR T2 ; Ts)

  syntax TypeError ::= #InvalidDIP(Int)

  syntax TypeInput ::= #FirstN(TypeSeq, Int) [function, functional]

  rule #FirstN(TS, N) => #FirstNAux(TS, N, .TypeSeq)

  syntax TypeSeq ::= #ReverseTypeSeq(TypeSeq) [function, functional]
  rule #ReverseTypeSeq(T) => #ReverseTypeSeqAux(T, .TypeSeq)

  syntax TypeSeq ::= #ReverseTypeSeqAux(TypeSeq, TypeSeq) [function, functional]
  rule #ReverseTypeSeqAux(T ; Ts1, Ts2) => #ReverseTypeSeqAux(Ts1, T ; Ts2)
  rule #ReverseTypeSeqAux(.TypeSeq, Ts) => Ts

  syntax TypeInput ::= #FirstNAux(TypeSeq, Int, TypeInput) [function, functional]
  rule #FirstNAux(_, I, _) => #InternalError                               requires I <Int 0
  rule #FirstNAux(_, 0, TE:TypeError) => TE
  rule #FirstNAux(_, 0, TS:TypeSeq) => #ReverseTypeSeq(TS)
  rule #FirstNAux(T ; Ts, I, R:TypeSeq) => #FirstNAux(Ts, I -Int 1, T ; R) requires I >Int 0
  rule #FirstNAux(.TypeSeq, I, _) => #InvalidDIP(I)                        requires I >Int 0

  rule #TypeInstruction(C, DIP A B, OS) => #TypeInstruction(C, DIP A 1 B, OS)

  syntax TypeInput ::= #MakeConcat(TypeInput, TypeInput) [function, functional]
  rule #MakeConcat(TE:TypeError, _) => TE
  rule #MakeConcat(_:TypeSeq, TE:TypeError) => TE
  rule #MakeConcat(T1:TypeSeq, T2:TypeSeq) => #Concat(T1, T2)

  syntax TypeResult ::= #MakeTransition(TypeInput, TypeInput) [function, functional]
  rule #MakeTransition(TE:TypeError, _) => TE
  rule #MakeTransition(_:TypeSeq, TE:TypeError) => TE
  rule #MakeTransition(T1:TypeSeq, T2:TypeSeq) => T1 -> T2



  syntax TypeSeq ::= #Concat(TypeSeq, TypeSeq) [function, functional]
  rule #Concat(TS1, TS2) => #ConcatAux(#ReverseTypeSeq(TS1), TS2)


  syntax TypeSeq ::= #ConcatAux(TypeSeq, TypeSeq) [function, functional]
  rule #ConcatAux(.TypeSeq, TS) => TS
  rule #ConcatAux(T ; TS1, TS2) => #ConcatAux(TS1, T ; TS2)

  syntax TypedInstruction ::= #DIPAux(Instruction, TypedInstruction, TypeSeq) [function, functional]

  syntax TypeError ::= #DIPError(TypedInstruction, TypeSeq)

  rule #DIPAux(DIP _ N _, #TI(_, Ts1 -> Ts2) #as B, OS) => #TI(DIP .AnnotationList N { #Exec(B) }, #MakeTransition(OS, #MakeConcat(#FirstN(OS, N), Ts2)))
  requires #RemoveFirstN(OS, N) ==K Ts1
  rule #DIPAux(I, TI, OS) => #TI(I, #DIPError(TI, OS)) [owise] // TODO: Better error messages.

  rule #TypeInstruction(C, (DIP _ N B) #as I, OS) => #DIPAux(I, #TypeInstruction(C, B, #RemoveFirstN(OS, N)), OS)

  rule #TypeInstruction(C, (FAILWITH _) #as I, _) => #TI(I, #ContractFailed)
//  rule #TypeInstruction(C, (CAST _) #as I, Ts) => #TI(I, Ts -> Ts)
//  rule #TypeInstruction(C, (RENAME _) #as I, Ts) => #TI(I, Ts -> Ts)

  rule #TypeInstruction(C, (CONCAT _) #as I, (string _ ; string _ ; Ts) #as OS) => #TI(I, OS -> (string .AnnotationList ; Ts))
  rule #TypeInstruction(C, (CONCAT _) #as I, (bytes _ ; bytes _ ; Ts) #as OS) => #TI(I, OS -> (bytes .AnnotationList ; Ts))

  rule #TypeInstruction(C, (CONCAT _) #as I, (list _ string _ ; Ts) #as OS) => #TI(I, OS -> (string .AnnotationList ; Ts))
  rule #TypeInstruction(C, (CONCAT _) #as I,  (list _ bytes _ ; Ts) #as OS) => #TI(I, OS -> (bytes .AnnotationList ; Ts))



  rule #TypeInstruction(C, (SLICE _) #as I, (nat _ ; nat _ ; string _ ; Ts) #as OS) => #TI(I, OS -> (option .AnnotationList string .AnnotationList ; Ts))
  rule #TypeInstruction(C, (SLICE _) #as I, (nat _ ; nat _ ; bytes _ ; Ts) #as OS) => #TI(I, OS -> (option .AnnotationList bytes .AnnotationList ; Ts))

  rule #TypeInstruction(C, (PACK _) #as I, (T ; Ts) #as OS) => #TI(I, OS -> bytes .AnnotationList ; Ts)
  rule #TypeInstruction(C, (UNPACK _ T) #as I, (bytes _ ; Ts) #as OS) => #TI(I, OS -> (option .AnnotationList T) ; Ts)

  rule #TypeInstruction(C, (ADD _) #as I, (nat _ ; nat _ ; Ts) #as OS) => #TI(I, OS -> nat .AnnotationList ; Ts)
  rule #TypeInstruction(C, (ADD _) #as I, (nat _ ; int _ ; Ts) #as OS) => #TI(I, OS -> int .AnnotationList ; Ts)
  rule #TypeInstruction(C, (ADD _) #as I, (int _ ; nat _ ; Ts) #as OS) => #TI(I, OS -> int .AnnotationList ; Ts)
  rule #TypeInstruction(C, (ADD _) #as I, (int _ ; int _ ; Ts) #as OS) => #TI(I, OS -> int .AnnotationList ; Ts)
  rule #TypeInstruction(C, (ADD _) #as I, (int _ ; timestamp _ ; Ts) #as OS) => #TI(I, OS -> timestamp .AnnotationList ; Ts)
  rule #TypeInstruction(C, (ADD _) #as I, (timestamp _ ; int _ ; Ts) #as OS) => #TI(I, OS -> timestamp .AnnotationList ; Ts)
  rule #TypeInstruction(C, (ADD _) #as I, (mutez _ ; mutez _ ; Ts) #as OS) => #TI(I, OS -> mutez .AnnotationList ; Ts)

  rule #TypeInstruction(C, (SUB _) #as I, (nat _ ; nat _ ; Ts) #as OS) => #TI(I, OS -> int .AnnotationList ; Ts)
  rule #TypeInstruction(C, (SUB _) #as I, (nat _ ; int _ ; Ts) #as OS) => #TI(I, OS -> int .AnnotationList ; Ts)
  rule #TypeInstruction(C, (SUB _) #as I, (int _ ; nat _ ; Ts) #as OS) => #TI(I, OS -> int .AnnotationList ; Ts)
  rule #TypeInstruction(C, (SUB _) #as I, (int _ ; int _ ; Ts) #as OS) => #TI(I, OS -> int .AnnotationList ; Ts)
  rule #TypeInstruction(C, (SUB _) #as I, (timestamp _ ; int _ ; Ts) #as OS) => #TI(I, OS -> timestamp .AnnotationList ; Ts)
  rule #TypeInstruction(C, (SUB _) #as I, (timestamp _ ; timestamp _ ; Ts) #as OS) => #TI(I, OS -> int .AnnotationList ; Ts)
  rule #TypeInstruction(C, (SUB _) #as I, (mutez _ ; mutez _ ; Ts) #as OS) => #TI(I, OS -> mutez .AnnotationList ; Ts)

  rule #TypeInstruction(C, (MUL _) #as I, (nat _ ; nat _ ; Ts) #as OS) => #TI(I, OS -> nat .AnnotationList ; Ts)
  rule #TypeInstruction(C, (MUL _) #as I, (nat _ ; int _ ; Ts) #as OS) => #TI(I, OS -> int .AnnotationList ; Ts)
  rule #TypeInstruction(C, (MUL _) #as I, (int _ ; nat _ ; Ts) #as OS) => #TI(I, OS -> int .AnnotationList ; Ts)
  rule #TypeInstruction(C, (MUL _) #as I, (int _ ; int _ ; Ts) #as OS) => #TI(I, OS -> int .AnnotationList ; Ts)
  rule #TypeInstruction(C, (MUL _) #as I, (mutez _ ; nat _ ; Ts) #as OS) => #TI(I, OS -> mutez .AnnotationList ; Ts)
  rule #TypeInstruction(C, (MUL _) #as I, (nat _ ; mutez _ ; Ts) #as OS) => #TI(I, OS -> mutez .AnnotationList ; Ts)

  rule #TypeInstruction(C, (EDIV _) #as I, (nat _ ; nat _ ; Ts) #as OS) => #TI(I, OS -> option .AnnotationList pair .AnnotationList nat .AnnotationList nat .AnnotationList ; Ts)
  rule #TypeInstruction(C, (EDIV _) #as I, (int _ ; nat _ ; Ts) #as OS) => #TI(I, OS -> option .AnnotationList pair .AnnotationList int .AnnotationList nat .AnnotationList ; Ts)
  rule #TypeInstruction(C, (EDIV _) #as I, (nat _ ; int _ ; Ts) #as OS) => #TI(I, OS -> option .AnnotationList pair .AnnotationList int .AnnotationList nat .AnnotationList ; Ts)
  rule #TypeInstruction(C, (EDIV _) #as I, (int _ ; int _ ; Ts) #as OS) => #TI(I, OS -> option .AnnotationList pair .AnnotationList int .AnnotationList nat .AnnotationList ; Ts)
  rule #TypeInstruction(C, (EDIV _) #as I, (mutez _ ; nat _ ; Ts) #as OS) => #TI(I, OS -> option .AnnotationList pair .AnnotationList mutez .AnnotationList mutez .AnnotationList ; Ts)
  rule #TypeInstruction(C, (EDIV _) #as I, (mutez _ ; mutez _ ; Ts) #as OS) => #TI(I, OS -> option .AnnotationList pair .AnnotationList nat .AnnotationList mutez .AnnotationList ; Ts)

  rule #TypeInstruction(C, (ABS _) #as I, (int _ ; Ts) #as OS) => #TI(I, OS -> (nat .AnnotationList ; Ts))
  rule #TypeInstruction(C, (ISNAT _) #as I, (int _ ; Ts) #as OS) => #TI(I, OS -> (option .AnnotationList nat .AnnotationList ; Ts))
  rule #TypeInstruction(C, (INT _) #as I, (nat _ ; Ts) #as OS) => #TI(I, OS -> (int .AnnotationList ; Ts))
  rule #TypeInstruction(C, (NEG _) #as I, (int _ ; Ts) #as OS) => #TI(I, OS -> (int .AnnotationList ; Ts))
  rule #TypeInstruction(C, (NEG _) #as I, (nat _ ; Ts) #as OS) => #TI(I, OS -> (int .AnnotationList ; Ts))

  rule #TypeInstruction(C, (LSL _) #as I, (nat _ ; nat _ ; Ts) #as OS) => #TI(I, OS -> (nat .AnnotationList ; Ts))
  rule #TypeInstruction(C, (LSR _) #as I, (nat _ ; nat _ ; Ts) #as OS) => #TI(I, OS -> (nat .AnnotationList ; Ts))

  rule #TypeInstruction(C, (OR _) #as I, (nat _ ; nat _ ; Ts) #as OS) => #TI(I, OS -> (nat .AnnotationList ; Ts))
  rule #TypeInstruction(C, (OR _) #as I, (bool _ ; bool _ ; Ts) #as OS) => #TI(I, OS -> (bool .AnnotationList ; Ts))

  rule #TypeInstruction(C, (AND _) #as I, (nat _ ; nat _ ; Ts) #as OS) => #TI(I, OS -> (nat .AnnotationList ; Ts))
  rule #TypeInstruction(C, (AND _) #as I, (int _ ; nat _ ; Ts) #as OS) => #TI(I, OS -> (nat .AnnotationList ; Ts))
  rule #TypeInstruction(C, (AND _) #as I, (bool _ ; bool _ ; Ts) #as OS) => #TI(I, OS -> (bool .AnnotationList ; Ts))

  rule #TypeInstruction(C, (XOR _) #as I, (nat _ ; nat _ ; Ts) #as OS) => #TI(I, OS -> (nat .AnnotationList ; Ts))
  rule #TypeInstruction(C, (XOR _) #as I, (bool _ ; bool _ ; Ts) #as OS) => #TI(I, OS -> (bool .AnnotationList ; Ts))

  rule #TypeInstruction(C, (NOT _) #as I, (nat _ ; Ts) #as OS) => #TI(I, OS -> (int .AnnotationList ; Ts))
  rule #TypeInstruction(C, (NOT _) #as I, (int _ ; Ts) #as OS) => #TI(I, OS -> (int .AnnotationList ; Ts))
  rule #TypeInstruction(C, (NOT _) #as I, (bool _ ; Ts) #as OS) => #TI(I, OS -> (bool .AnnotationList ; Ts))

  rule #TypeInstruction(C, (COMPARE _) #as I, (nat _ ; nat _ ; Ts) #as OS) => #TI(I, OS -> (int .AnnotationList ; Ts))
  rule #TypeInstruction(C, (COMPARE _) #as I, (bool _ ; bool _ ; Ts) #as OS) => #TI(I, OS -> (int .AnnotationList ; Ts))
  rule #TypeInstruction(C, (COMPARE _) #as I, (int _ ; int _ ; Ts) #as OS) => #TI(I, OS -> (int .AnnotationList ; Ts))
  rule #TypeInstruction(C, (COMPARE _) #as I, (string _ ; string _ ; Ts) #as OS) => #TI(I, OS -> (int .AnnotationList ; Ts))
  rule #TypeInstruction(C, (COMPARE _) #as I, (pair _ A B ; pair _ A B ; Ts) #as OS) => #TI(I, OS -> (int .AnnotationList ; Ts))
  rule #TypeInstruction(C, (COMPARE _) #as I, (timestamp _ ; timestamp _ ; Ts) #as OS) => #TI(I, OS -> (int .AnnotationList ; Ts))
  rule #TypeInstruction(C, (COMPARE _) #as I, (mutez _ ; mutez _ ; Ts) #as OS) => #TI(I, OS -> (int .AnnotationList ; Ts))
  rule #TypeInstruction(C, (COMPARE _) #as I, (bytes _ ; bytes _ ; Ts) #as OS) => #TI(I, OS -> (int .AnnotationList ; Ts))
  rule #TypeInstruction(C, (COMPARE _) #as I, (key_hash _ ; key_hash _ ; Ts) #as OS) => #TI(I, OS -> (int .AnnotationList ; Ts))

  rule #TypeInstruction(C, (EQ _) #as I, (int _ ; Ts) #as OS) => #TI(I, OS -> (bool .AnnotationList ; Ts))
  rule #TypeInstruction(C, (NEQ _) #as I, (int _ ; Ts) #as OS) => #TI(I, OS -> (bool .AnnotationList ; Ts))
  rule #TypeInstruction(C, (LT _) #as I, (int _ ; Ts) #as OS) => #TI(I, OS -> (bool .AnnotationList ; Ts))
  rule #TypeInstruction(C, (GT _) #as I, (int _ ; Ts) #as OS) => #TI(I, OS -> (bool .AnnotationList ; Ts))
  rule #TypeInstruction(C, (LE _) #as I, (int _ ; Ts) #as OS) => #TI(I, OS -> (bool .AnnotationList ; Ts))
  rule #TypeInstruction(C, (GE _) #as I, (int _ ; Ts) #as OS) => #TI(I, OS -> (bool .AnnotationList ; Ts))

  rule #TypeInstruction(C, (SELF _) #as I, OS) => #TI(I, OS -> (contract .AnnotationList C) ; OS)

  rule #TypeInstruction(C, (CONTRACT _ T) #as I, ((address _) ; Ts) #as OS) => #TI(I, OS -> (option .AnnotationList contract .AnnotationList T) ; Ts)

  rule #TypeInstruction(C, (TRANSFER_TOKENS _) #as I, (T ; mutez _ ; contract _ T ; Ts) #as OS) => #TI(I, OS -> (operation .AnnotationList) ; Ts)
  rule #TypeInstruction(C, (SET_DELEGATE _) #as I, (option _ key_hash _ ; Ts) #as OS) => #TI(I, OS -> (operation .AnnotationList) ; Ts)

  rule #TypeInstruction(C, (NOW _) #as I, OS) => #TI(I, OS -> (timestamp .AnnotationList) ; OS)
  rule #TypeInstruction(C, (CHAIN_ID _) #as I, OS) => #TI(I, OS -> (chain_id .AnnotationList) ; OS)
  rule #TypeInstruction(C, (AMOUNT _) #as I, OS) => #TI(I, OS -> (mutez .AnnotationList) ; OS)
  rule #TypeInstruction(C, (BALANCE _) #as I, OS) => #TI(I, OS -> (mutez .AnnotationList) ; OS)
  rule #TypeInstruction(C, (CHECK_SIGNATURE _) #as I, (key _ ; signature _ ; bytes _ ; Ts) #as OS) => #TI(I, OS -> bool .AnnotationList ; Ts)

  rule #TypeInstruction(C, (BLAKE2B _) #as I, (bytes _ ; Ts) #as OS) => #TI(I, OS -> OS)
  rule #TypeInstruction(C, (SHA256 _) #as I, (bytes _ ; Ts) #as OS) => #TI(I, OS -> OS)
  rule #TypeInstruction(C, (SHA512 _) #as I, (bytes _ ; Ts) #as OS) => #TI(I, OS -> OS)

  rule #TypeInstruction(C, (HASH_KEY _) #as I, (key _ ; Ts) #as OS) => #TI(I, OS -> (key_hash .AnnotationList ; Ts))

  rule #TypeInstruction(C, (SOURCE _) #as I, OS) => #TI(I, OS -> (address .AnnotationList) ; OS)
  rule #TypeInstruction(C, (SENDER _) #as I, OS) => #TI(I, OS -> (address .AnnotationList) ; OS)
  rule #TypeInstruction(C, (ADDRESS _) #as I, (contract _ _ ; Ts) #as OS) => #TI(I, OS -> (address .AnnotationList) ; Ts)

  syntax TypedInstruction ::= #CreateContractAux(Instruction, TypedInstruction, TypeSeq) [function, functional]

  syntax TypeError ::= #CreateContractError(TypedInstruction, TypeSeq)

  rule #CreateContractAux((CREATE_CONTRACT _ { code B ; storage St ; parameter Pt ; }) ,
                          #TI(_, (pair _ Pt St) ; .TypeSeq -> (pair _ (list _ operation _) St) ; .TypeSeq ), (option _ key_hash _ ; mutez _ ; St ; Ts) #as OS) =>
       #TI(CREATE_CONTRACT .AnnotationList { code B ; storage St ; parameter Pt ; }, OS -> operation .AnnotationList ; address .AnnotationList ; Ts)
  rule #CreateContractAux(I, TI, TS) => #TI(I, #CreateContractError(TI, TS)) [owise]

  rule #TypeInstruction(C, (CREATE_CONTRACT _ { code B ; storage St ; parameter Pt ; }) #as I, OS) => #CreateContractAux(I, #TypeInstruction(Pt, B, pair .AnnotationList Pt St), OS)

  rule #TypeInstruction(C, (IMPLICIT_ACCOUNT _) #as I, (key_hash _ ; Ts) #as OS) => #TI(I, OS -> (contract .AnnotationList unit .AnnotationList ; Ts))
endmodule
```
