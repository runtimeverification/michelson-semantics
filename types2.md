```k
requires "common.md"
```

Static Type Semantics
=====================

```k
module STATIC-TYPE-SEMANTICS
  imports MICHELSON-COMMON
```

```k
  syntax TypeStack ::= List{TypeName, ";"}
  syntax MaybeTypeStack ::= TypeStack
                          | "Failed"
```

Operations on type stacks
-------------------------

```k
  syntax TypeName  ::= getIndex(TypeStack, Int) [function]
  // -----------------------------------------------------
  rule getIndex(T ; _ , 0) => T
  rule getIndex(_ ; TS, N) => getIndex(TS, N -Int 1) requires N >Int 1

  syntax TypeStack ::= append(TypeStack, TypeStack) [function]
                     | revAppend(TypeStack, TypeStack) [function]
  // ------------------------------------------------------------
  rule append(T ; TS,     TS') => append(TS, T ; TS')
  rule append(.TypeStack, TS') => TS'

  rule revAppend(TS, T' ; TS') => revAppend(T' ; TS, TS')
  rule revAppend(TS, .TypeSeq) => TS

  syntax TypeStack ::= putIndex(Type, TypeStack, Int) [function]
                     | putIndex2(Type, TypeStack, TypeStack, Int) [function]
  // -----------------------------------------------------------------------
  rule putIndex(T, TS, N) => putIndex2(T, TS, N, .TypeStack)
  rule putIndex2(T, T' ; TS, TS', 0) => putIndex2(T, TS, T' ; TS, N -Int 1) requires N >Int 1
  rule putIndex2(T,      TS, TS', N) => revAppend(T ; TS, TS')

  syntax TypeStack ::= delIndex(TypeStack, Int) [function]
                     | delIndex2(TypeStack, TypeStack, Int) [function]
  // -----------------------------------------------------------------
  rule delIndex(TS, N)            => delIndex2(TS, N, .TypeStack)
  rule delIndex2(T' ; TS, N, TS') => delIndex2(TS, N -Int 1, T' ; TS) requires N >Int 1
  rule delIndex2(TS,      0, TS') => revAppend(T ; TS, TS')

  syntax TypeStack ::= delFirstN(TypeStack, Int) [function]
  // ------------------------------------------------------
  rule delFirstN(_ ; TS, N) => delFirstN(TS, N -Int 1) requires N >Int 1
  rule delFirstN(TS,     0) => TS

  syntax TypeStack ::= takeFirstN(TypeStack, Int) [function]
                     | takeFirstN'(TypeStack, TypeStack, Int) [function]
  // -------------------------------------------------------------------
  rule takeFirstN(TS, N) => takeFirstN'(TS, .TypeStack, N)
  rule takeFirstN'(T ; TS, 'TS, N) => takeFirstN'(TS, T ; 'TS, N -Int 1) requires N >Int 1
  rule takeFirstN'(_,      'TS, 0) => revAppend(.TypeStack, 'TS)

  syntax Bool ::= UnifyWithFailed?(MaybeTypeStack, MaybeTypeStack) [function]
  // ------------------------------------------------------------------------
  rule UnifyWithFailed?(TS:TypeStack, TS':TypeStack) => TS ==K TS'
  rule UnifyWithFailed?(Failed,       _            ) => true
  rule UnifyWithFailed?(_,            Failed       ) => true

  syntax Bool ::= UnifyNoFailed?(TypeStack, TypeStack) [function]
  // ------------------------------------------------------------
  rule UnifyNoFailed?(TS:TypeStack, TS':TypeStack) => TS ==K TS'

  syntax MaybeTypeStack ::= Unify(MaybeTypeStack, MaybeTypeStack) [function]
  // -----------------------------------------------------------------------
  rule Unify(TS:TypeStack, TS':TypeStack) => TS requires UnifyNoFailed?(TS,TS')
  rule Unify(Failed,       TS:TypeStack ) => TS
  rule Unify(TS:TypeStack, Failed       ) => TS
  rule Unify(Failed,       Failed       ) => Failed
```

Type Stack Operations
---------------------

```k
  syntax Instruction ::= APPEND(TypeStack)
                       | REPLACE(TypeStack)
                       | CHECK(TypeStack)
                       | SAVE_CHECK(TypeStack, DataList)
                       | MAP_MAP_CHECK(TypeName, TypeStack)
                       | MAP_LIST_CHECK(TypeStack)

  rule <type> APPEND(TS') => .K ... </type>
       <tstack> TS => append(TS',TS) </tstack>

  rule <type> REPLACE(TS') => .K ... </type>
       <tstack> TS => TS' </tstack>

  rule <type> CHECK(TS') => .K ... </type>
       <tstack> Unify(TS,TS') </tstack>
    requires UnifyWithFailed(TS, TS')

  rule <type> SAVE_CHECK(TS', IS) => IS ~> CHECK(TS) ... </type>
       <tstack> TS => TS' </tstack>

  rule <type> MAP_MAP_CHECK(key_ty, TS') => .K ... </type>
       <tstack> val_ty ; TS => map key_ty val_ty ; TS </tstack>
    requires UnifyNoFailed(TS, TS')

  rule <type> MAP_LIST_CHECK(TS') => .K ... </type>
       <tstack> val_ty ; TS => list val_ty ; TS </tstack>
    requires UnifyNoFailed(TS, TS')
```

Control Structures
------------------

### Instruction Sequencing

```k
  rule <type> I:Instruction ; Is => I ~> Is </type> [structural]
  rule <type> {}                 => .K      </type> [structrual]
  rule <type> { Is:DataList }    => Is      </type> [structural]
```

### User-defined Exceptions

```k
  rule <type> FAILWITH A => .K ... </type>
       <tstack> T ; TS => Failed </tstack>
```

### Conditionals

```k
  rule <type> IF A BT BF => BT ~> SAVE_CHECK(TS, BF) ... </type>
       <tstack> bool ; TS => TS </tstack>
```

### Loops

```k
  rule <type> LOOP A B => B ~> CHECK(bool ; TS) ~> REPLACE(TS) ... </type>
       <tstack> bool ; TS => TS </tstack>

  rule <type> LOOP_LEFT A B => B ~> CHECK(or LX RX ; TS) ~> REPLACE(TS) ... </type>
       <tstack> or LX RX ; TS => LX ; TS </tstack>
```

### Stack Manipulation

```k
  rule <type> DIP A B => DIP A 1 B ... </type>
  rule <type> DIP A N B => B ~> APPEND(takeFirstN(TS)) ... </type>
       <tstack> TS => delFirstN(TS) </tstack>

  rule <type> DROP A => DROP A 1 ... </type>
  rule <type> DROP A I => .K ...  </type>
       <tstack> TS => delFirstN(TS) </tstack>

  rule <type> DUP A => .K ... </type>
       <tstack> X ; TS => X ; X ; TS </tstack>

  rule <type> SWAP A => .K ... </type>
       <tstack> X ; Y ; TS => Y ; X ; TS </tstack>

  rule <type> DIG A N => ... </type>
       <tstack> T ; TS => putIndex(T,TS,N)  </tstack>

  rule <type> DUG A N => .K ~> DUG_DOWN(N, .Stack, T) ... </type>
       <tstack> T ; TS => getIndex(TS, N) ; delIndex(TS, N) </tstack>
```

#### `PUSH`-like Instructions

```k
  rule <type> PUSH A T X => .K ... </type>
       <tstack> TS => #Name(T) ; TS </tstack>

  rule <type> UNIT A => .K ... </type>
       <tstack> TS => unit ; TS </tstack>

  rule <type> LAMBDA A T1 T2 C => .K ... </type>
       <tstack> TS => (lambda #Name(T1) #Name(T2)) ; TS </tstack>
```

### Lambda Evaluation

```k
  rule <type> EXEC B => C ~> CHECK(T2) ~> REPLACE(T2 ; TS) ... </type>
       <tstack> T1 ; lambda T1 T2 ; TS => T2 </tstack>

  rule <type> APPLY A => .K ... </type>
       <tstack> T0 ; (lambda (pair T0 T1) T2) ; TS => lambda T1 T2 ; TS </tstack>
```

Core Operations
---------------

### Generic Comparison

```k
  rule <type> EQ A => .K ... </type>
       <tstack> int ; TS => bool ; TS </tstack>

  rule <type> NEQ A => .K ... </type>
       <tstack> int ; TS => bool ; TS </tstack>

  rule <type> LT A => .K ... </type>
       <tstack> int ; TS => bool ; TS </tstack>

  rule <type> GT A => .K ... </type>
       <tstack> int ; TS => bool ; TS </tstack>

  rule <type> LE A => .K ... </type>
       <tstack> int ; TS => bool ; TS </tstack>

  rule <type> GE A => .K ... </type>
       <tstack> int ; TS => bool ; TS </tstack>
```

### Boolean Operations

```k
  rule <type> OR A => .K ... </type>
       <tstack> bool ; bool ; TS => bool ; TS </tstack>

  rule <type> AND A => .K ... </type>
       <tstack> bool ; bool ; TS => bool ; TS </tstack>

  rule <type> XOR A => .K ... </type>
       <tstack> bool ; bool ; TS => bool ; TS </tstack>

  rule <type> NOT A => .K ... </type>
       <tstack> bool ; TS => bool ; TS </tstack>
```

### Integer and Natural Operations

```k
  rule <type> NEG A => .K ... </type>
       <tstack> N:NumTypeName ; TS => int ; TS </tstack>

  rule <type> ABS A => .K ... </type>
       <tstack> int ; TS => nat ; TS </tstack>

  rule <type> ISNAT A => .K ... </type>
       <tstack> int ; TS => option nat ; TS </tstack>

  rule <type> INT A => .K ... </type>
       <tstack> nat ; TS => int ; TS </tstack>

  rule <type> ADD A => .K ... </type>
       <tstack> T1:NumTypeName ; T2:NumTypeName ; TS => BinOpNumType(T1,T2) ; TS </tstack>

  rule <type> SUB A => .K ... </type>
       <tstack> T1:NumTypeName ; T2:NumTypeName ; TS => int ; TS </tstack>

  rule <type> MUL A => .K ... </type>
       <tstack> T1:NumTypeName ; T2:NumTypeName ; TS => BinOpNumType(T1,T2) ; TS </tstack>

  rule <type> EDIV A  => .K ... </type>
       <tstack> T1:NumTypeName ; T2:NumTypeName ; TS => option (pair BinOpNumType(T1,T2) nat)) ; TS </tstack>

  rule <type> OR A => .K  ... </type>
       <tstack> nat ; nat ; TS => nat ; TS </tstack>

  rule <type> AND A => .K ... </type>
       <tstack> T1:NumTypeName ; nat ; TS => nat ; TS </tstack>

  rule <type> XOR A => .K ... </type>
       <tstack> nat ; nat ; TS => nat ; TS </tstack>

  rule <type> NOT A => .K ... </type>
       <tstack> T1:NumTypeName ; TS => int ; TS </tstack>

  rule <type> LSL A => .K ... </type>
       <tstack> nat ; nat ; TS => nat ; TS </tstack>

  rule <type> LSR A => .K ... </type>
       <tstack> nat ; nat ; TS => nat ; TS </tstack>
```

### `COMPARE` Instruction

```k
  rule <type> COMPARE A => .K ... </type>
       <tstack> TY V1 ; TY ; TS => int ; TS </tstack>
    requires #IsComparable(TY)
```

### String Operations

```k
  rule <type> CONCAT A => .K ... </type>
       <tstack> string ; string ; TS => string ; TS </tstack>

  rule <type> CONCAT A => .K ... </type>
       <tstack> list string ; TS => string ; TS </tstack>

  rule <type> SIZE A => .K ... </type>
       <tstack> string ; TS => nat ; TS </tstack>

  rule <type> SLICE A => .K ... </type>
       <tstack> nat ; nat ; string ; TS => option string ; TS </tstack>
```

### Bytes Operations

```k
  rule <type> PACK A => .K ... </type>
       <tstack> T ; TS => bytes ; TS </tstack>

  rule <type> UNPACK A T => .K ... </type>
       <tstack> bytes ; TS => option T ; TS </tstack>

  rule <type> CONCAT A => .K ... </type>
       <tstack> bytes ; bytes ; TS => bytes ; TS </tstack>

  rule <type> CONCAT A => .K ... </type>
       <tstack> list bytes ; TS => bytes ; TS </tstack>

  rule <type> SIZE A => .K ... </type>
       <tstack> bytes ; TS => nat ; TS </tstack>

  rule <type> SLICE A => .K ... </type>
       <tstack> nat ; nat ; bytes ; TS => option bytes ; TS </tstack>
```

### Pair Operations

  rule <type> PAIR A => .K ... </type>
       <tstack> LTy ; RTy ; TS => Pair LTy RTy ; TS </tstack>

  rule <type> UNPAIR A => .K ... </type>
       <tstack> pair LTy RTy ; TS => LTy ; RTy ; TS </tstack>

  rule <type> CAR A => .K ... </type>
       <tstack> pair LTy RTy ; TS => LTy ; TS </tstack>

  rule <type> CDR A => .K ... </type>
       <tstack> pair LTy RTy ; TS => RTy ; TS </tstack>

### Set Operations

```k
  rule <type> EMPTY_SET A T:Type => .K ... </type>
       <tstack> TS => set #Name(T) ; TS </tstack>

  rule <type> MEM A => .K ... </type>
       <tstack> T ; set T ; TS => bool ; TS </tstack>

  rule <type> UPDATE A => .K ... </type>
       <tstack> T ; bool ; set ; TS => set T ; TS </tstack>

  rule <type> SIZE A => .K ... </type>
       <tstack> set _ ; TS => nat ; TS </tstack>

  rule <type> ITER A B => B ~> CHECK(TS) ...  </type>
       <tstack> set T ; TS => T ; TS </tstack>
```

### Shared Map/Big Map Operations

```k
   rule <type> GET A => .K ... </type>
        <tstack> KT ; MT:MapTypeName KT VT ; TS => option VT ; TS </tstack>

  rule <type> MEM A => .K ~> . ... </type>
       <tstack> KT ; MT:MapTypeName KT VT ; TS => bool ; TS </tstack>

  rule <type> UPDATE A => .K  ... </type>
       <tstack> KT ; option VT ; MT:MapTypeName KT VT ; TS => MT KT VT ; TS </tstack>

  rule <type> UPDATE A => .K  ... </type>
       <tstack> KT ; option VT ; MT:MapTypeName KT VT ; TS => MT KT VT ; TS </tstack>
```

### Map Specific Operations

```k
  rule <type> EMPTY_MAP A KT VT => .K ... </type>
       <tstack> TS => #Name(map A KT VT) ; TS </tstack>

  rule <type> SIZE A => .K  ... </type>
       <tstack> map KT VT ; TS => nat ; TS </tstack>

  rule <type> MAP A B => B ~> MAP_MAP_CHECK(KT,TS) ...  </type>
       <tstack> map KT VT ; TS => pair KT VT ; TS </tstack>

  rule <type> ITER A B => B ~> CHECK(TS) ... </type>
       <tstack> map KT VT ; TS => pair KT VT ; TS </tstack>
```

### Big Map Specific Operations

```k
  rule <type> EMPTY_BIG_MAP A KT VT => .K ... </type>
       <tstack> TS => #Name(big_map A KT VT) ; TS </tstack>
```

### Option Operations

```k
  rule <type> SOME A => .K  ... </type>
       <tstack> T ; TS => option T ; TS </tstack>

  rule <type> NONE A T:Type => .K  ... </type>
       <tstack> TS => option #Name(T) ; TS </tstack>

  rule <type> IF_NONE A BT BF => BT ~> SAVE_CHECK(option T ; TS, BF) ... </type>
       <tstack> option T ; TS => TS </tstack>
```

### Union Operations

```k
  rule <type> LEFT A RTy => .K  ... </type>
       <tstack> LTy ; TS => or LTy #Name(RTy) ; TS </tstack>

  rule <type> RIGHT A LTy => .K ... </type>
       <tstack> RTy ; TS => or #Name(LTy) RTy ; TS </tstack>

  rule <type> IF_LEFT A BT BF => BT ~> SAVE_CHECK(RTy ; TS, BF) ... </type>
       <tstack> or LTy RTy ; TS => LTy ; TS </tstack>
```

### List Operations

```k
  rule <type> CONS A => .K  ... </type>
       <tstack> T ; list T ; TS => list T ; TS </tstack>

  rule <type> NIL A T => .K  ... </type>
       <tstack> TS => list T ; TS </tstack>

  rule <type> IF_CONS A BT BF => BT ~> SAVE_CHECK(TS), BF) ... </type>
       <tstack> list T ; TS => T ; list T ; TS </tstack>

  rule <type> SIZE A => .K ... </type>
       <tstack> list T ; TS => nat ; TS </tstack>

  rule <type> ITER A B => B ~> CHECK(TS) ... </type>
       <tstack> list T ; TS => T ; TS </tstack>

  rule <type> MAP A B => MAP_LIST_CHECK(TS) ... </type>
       <tstack> list T ; TS => T ; TS </tstack>
```

Domain Specific operations
--------------------------

### Timestamp Operations

```k
  rule <type> ADD A => . ... </type>
       <tstack> timestamp ; int ; TS => timestamp ; TS </tstack>

  rule <type> ADD A => . ... </type>
       <tstack> int ; timestamp ; TS => timestamp ; TS </tstack>

  rule <type> SUB A => . ... </type>
       <tstack> timestamp ; int ; TS => timestamp ; TS </tstack>

  rule <type> SUB A => . ... </type>
       <tstack> timestamp ; timestamp ; TS => int ; TS </tstack>
```

### Blockchain Operations

```k
  rule <type> CREATE_CONTRACT A { C } => . ... </type>
       <tstack> option key_hash ; mutez ; T ; TS => operation ; address ; TS </tstack>

  rule <type> TRANSFER_TOKENS _ => . ... </type>
       <tstack> T ; mutez ; contract T ; TS => operation ; TS </tstack>

  rule <type> SET_DELEGATE A => . ... </type>
       <tstack> option key_hash ; TS => operation ; TS </tstack>

  rule <type> CONTRACT _ T => . ... </type>
       <tstack> address ; TS => option contract T ; TS </tstack>

  rule <type> IMPLICIT_ACCOUNT Ann => . ... </type>
       <tstack> key_hash ; TS => contract unit ; TS </tstack>

  rule <type> BALANCE A => . ... </type>
       <tstack> TS => mutez ; TS </tstack>

  rule <type> ADDRESS Ann => . ... </type>
       <tstack> contract T ; TS => address ; TS </tstack>

  rule <type> SOURCE Ann => . ... </type>
       <tstack> TS => address ; TS </tstack>

  rule <type> SENDER Ann => . ... </type>
       <tstack> TS => address ; TS </tstack>

  rule <type> SELF Ann => . ... </type>
       <tstack> TS => contract T ; TS </tstack>
       <paramtype> T </paramtype>

  rule <type> AMOUNT Ann => . ... </type>
       <tstack> TS => mutez ; TS </tstack>

  rule <type> CHAIN_ID A => . ... </type>
       <tstack> TS => chain_id ; TS </tstack>

  rule <type> NOW A => . ... </type>
       <tstack> TS => timestamp ; TS </tstack>
```

### Cryptographic Operations

```k
  rule <type> HASH_KEY A => .K ... </type>
       <tstack> key ; TS => key_hash ; TS </tstack>

  rule <type> BLAKE2B A => .K ... </type>
       <tstack> bytes ; TS => bytes ; TS </tstack>

  rule <type> SHA256 A => .K ... </type>
       <tstack> bytes ; TS => bytes ; TS </tstack>

  rule <type> SHA512 A => .K ... </type>
       <tstack> bytes ; TS => bytes ; TS </tstack>

  rule <type> CHECK_SIGNATURE A => .K ... </type>
       <tstack> key ; signature ; bytes ; TS => bool ; TS </tstack>
```

### Mutez Operations

```k
  rule <type> ADD A => .K ...  </type>
       <tstack> mutez ; mutez ; TS => TS </tstack>

  rule <type> SUB A => .K ...  </type>
       <tstack> mutez ; mutez ; TS => TS </tstack>

  rule <type> MUL A => .K ...  </type>
       <tstack> mutez ; nat ; TS => TS </tstack>

  rule <type> MUL A => .K ...  </type>
       <tstack> nat ; mutez ; TS => TS </tstack>

  rule <type> EDIV A => .K ... </type>
       <tstack> mutez ; mutez ; TS => option pair nat mutez ; TS </tstack>

  rule <type> EDIV A => .K ... </type>
       <tstack> mutez ; nat ; TS => option pair mutez mutez ; TS </tstack>
```

Debugging Operations
--------------------

```k
  rule <type> PAUSE    => .K ... </type>
  rule <type> STOP     => .K ... </type>
  rule <type> PAUSE(S) => .K ... </type>
  rule <type> TRACE(S) => .K ... </type>
```

```k
endmodule
```
