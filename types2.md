Static Type Semantics
=====================

```k
module STATIC-TYPE-SEMANTICS
```

```k
  rule <type> I:Instruction ; Is => I ~> Is </type> [structural]
  rule <type> {}                 => .K      </type> [structrual]
  rule <type> { Is:DataList }    => Is      </type> [structural]
```

Control Structures
------------------

### User-defined Exceptions

The `FAILWITH` instruction lets users terminate execution at any point.

```k
  rule <type> FAILWITH A => .K ... </type>
       <tstack> T D:Data ; SS => ( Failed D ) </tstack>
```

### Conditionals

```k
  rule <type> IF A BT BF => .K ~> BT ... </type>
       <tstack> bool ; SS => SS </tstack>

  rule <type> IF A BT BF => .K ~> BF ... </type>
       <tstack> bool ; SS => SS </tstack>
```

### Loops

Here we handle concrete loop semantics.

```k
  rule <type> LOOP .AnnotationList B
        => B ~> LOOP .AnnotationList B
           ...
       </type>
       <tstack> bool ; SS => SS </tstack>
  rule <type> LOOP .AnnotationList B => .K ... </type>
       <tstack> bool ; SS => SS </tstack>
```

```k
  rule <type> LOOP_LEFT .AnnotationList B
        => B
        ~> LOOP_LEFT .AnnotationList B
           ...
        </type>
       <tstack> [ (or LX RX) Left D ] ; SS
           =>  [ LX D ] ; SS
       </tstack>
  rule <type> LOOP_LEFT .AnnotationList B => .K ... </type>
       <tstack> [ (or LX RX) Right D ] ; SS
            => [ RX D ] ; SS
       </tstack>
```

### Stack Manipulation

It is sometimes useful to create "pseudo-instructions" like this to schedule
operations to happen in the future.

```k
  syntax Instruction ::= #Push(TypeName,Data)
  rule <type> #Push(T,D) => . ... </type>
       <tstack> SS => [ T D ] ; SS </tstack>
```

The `DIP` instruction uses the `#Push` pseudo-instruction to replace the
element it pops off for its block.

```k
  rule <type> DIP A B => .K ~> B ~> #Push(T,D) ... </type>
       <tstack> [ T D ] ; SS => SS </tstack>

  rule <type> DIP A 0 B => .K ~> B ... </type>

  rule <type> DIP A N B
         => .K
         ~> DIP .AnnotationList { DIP .AnnotationList N -Int 1 B }
            ...
       </type>
    requires N >Int 0
```

```k
  rule <type> DROP A =>  .K ... </type>
       <tstack> _:StackElement ; SS => SS </tstack>

  rule <type> DROP A I
        => .K
        ~> DROP .AnnotationList
        ~> DROP .AnnotationList I -Int 1
           ...
       </type>
    requires I >Int 0

  rule <type> DROP A 0 => .K ... </type>
```

```k
  rule <type> DUP A => .K ... </type>
       <tstack> X:StackElement ; SS => X ; X ; SS </tstack>

  rule <type> SWAP A => .K ... </type>
       <tstack> X:StackElement ; Y:StackElement ; SS
            => Y ; X ; SS
       </tstack>
```

```k
  rule <type> DIG A N => .K ~> DIG_DOWN(N, .Stack) ... </type>

  syntax Instruction ::= "DIG_DOWN" "(" Int "," Stack ")"
                       | "DIG_UP" "(" Stack "," StackElement ")"
  // -----------------------------------------------------------
  rule <type> DIG_DOWN(N, A) => DIG_DOWN(N -Int 1, F ; A) ... </type>
       <tstack> F ; SS => SS </tstack>
    requires N >Int 0

  rule <type> DIG_DOWN(0, A) => DIG_UP(A, F) ... </type>
       <tstack> F ; SS => SS </tstack>

  rule <type> DIG_UP(F ; A, T) => DIG_UP(A, T) ... </type>
       <tstack> SS => F ; SS </tstack>

  rule <type> DIG_UP(.Stack, T) => . ... </type>
       <tstack> SS => T ; SS </tstack>

  rule <type> DUG A N => .K ~> DUG_DOWN(N, .Stack, T) ... </type>
       <tstack> T ; SS => SS </tstack>

  syntax Instruction ::= "DUG_DOWN" "(" Int "," Stack "," StackElement ")"
                       | "DUG_UP" "(" K ")"
  // ---------------------------------------------------------------------
  rule <type> DUG_DOWN(N, S, R) => DUG_DOWN(N -Int 1, T ; S, R) ... </type>
       <tstack> T ; SS => SS </tstack>
    requires N >Int 0

  rule <type> DUG_DOWN(0, S, R) => DUG_UP(S) ... </type>
       <tstack> SS => R ; SS </tstack>

  rule <type> DUG_UP(T:StackElement ; S) => DUG_UP(S) ... </type>
       <tstack> SS => T ; SS </tstack>

  rule <type> DUG_UP(.Stack) => .K ... </type>
```

#### `PUSH`-like Instructions

```k
  rule <type> PUSH A T X => .K ... </type>
       <tstack> SS => #Name(T) ; SS </tstack>

  rule <type> UNIT A => .K ... </type>
       <tstack> SS => unit ; SS </tstack>

  rule <type> LAMBDA A T1 T2 C => .K ... </type>
       <tstack> SS => (lambda #Name(T1) #Name(T2)) ; SS </tstack>
```

### Lambda Evaluation

```k
  rule <type> EXEC B => C ~> #ReturnStack(SS) ... </type>
       <tstack> T1 ; lambda T1 T2 ; SS => T2 </tstack>

  rule <type> APPLY A => .K ... </type>
       <tstack> T0 ; (lambda (pair T0 T1) T2) ; SS => lambda T1 T2 ; SS </tstack>
```

Core Operations
---------------

### Generic Comparison

```k
  rule <type> EQ A => .K ... </type>
       <tstack> int ; SS => bool ; SS </tstack>

  rule <type> NEQ A => .K ... </type>
       <tstack> int ; SS => bool ; SS </tstack>

  rule <type> LT A => .K ... </type>
       <tstack> int ; SS => bool ; SS </tstack>

  rule <type> GT A => .K ... </type>
       <tstack> int ; SS => bool ; SS </tstack>

  rule <type> LE A => .K ... </type>
       <tstack> int ; SS => bool ; SS </tstack>

  rule <type> GE A => .K ... </type>
       <tstack> int ; SS => bool ; SS </tstack>
```

### Boolean Operations

```k
  rule <type> OR A => .K ... </type>
       <tstack> bool ; bool ; SS => bool ; SS </tstack>

  rule <type> AND A => .K ... </type>
       <tstack> bool ; bool ; SS => bool ; SS </tstack>

  rule <type> XOR A => .K ... </type>
       <tstack> bool ; bool ; SS => bool ; SS </tstack>

  rule <type> NOT A => .K ... </type>
       <tstack> bool ; SS => bool ; SS </tstack>
```

### Integer and Natural Operations

```k
  rule <type> NEG A => .K ... </type>
       <tstack> N:NumTypeName ; SS => int ; SS </tstack>

  rule <type> ABS A => .K ... </type>
       <tstack> int ; SS => nat ; SS </tstack>

  rule <type> ISNAT A => .K ... </type>
       <tstack> int ; SS => option nat ; SS </tstack>

  rule <type> INT A => .K ... </type>
       <tstack> nat ; SS => int ; SS </tstack>

  rule <type> ADD A => .K ... </type>
       <tstack> T1:NumTypeName ; T2:NumTypeName ; SS => BinOpNumType(T1,T2) ; SS </tstack>

  rule <type> SUB A => .K ... </type>
       <tstack> T1:NumTypeName ; T2:NumTypeName ; SS => int ; SS </tstack>

  rule <type> MUL A => .K ... </type>
       <tstack> T1:NumTypeName ; T2:NumTypeName ; SS => BinOpNumType(T1,T2) ; SS </tstack>

  rule <type> EDIV A  => .K ... </type>
       <tstack> T1:NumTypeName ; T2:NumTypeName ; SS => option (pair BinOpNumType(T1,T2) nat)) ; SS </tstack>

  rule <type> OR A => .K  ... </type>
       <tstack> nat ; nat ; SS => nat ; SS </tstack>

  rule <type> AND A => .K ... </type>
       <tstack> T1:NumTypeName ; nat ; SS => nat ; SS </tstack>

  rule <type> XOR A => .K ... </type>
       <tstack> nat ; nat ; SS => nat ; SS </tstack>

  rule <type> NOT A => .K ... </type>
       <tstack> T1:NumTypeName ; SS => int ; SS </tstack>

  rule <type> LSL A => .K ... </type>
       <tstack> nat ; nat ; SS => nat ; SS </tstack>

  rule <type> LSR A => .K ... </type>
       <tstack> nat ; nat ; SS => nat ; SS </tstack>
```

### `COMPARE` Instruction

```k
  rule <type> COMPARE A => .K ... </type>
       <tstack> TY V1 ; TY ; SS => int ; SS </tstack>
    requires #IsComparable(TY)
```

### String Operations

```k
  rule <type> CONCAT A => .K ... </type>
       <tstack> string ; string ; SS => string ; SS </tstack>

  rule <type> CONCAT A => .K ... </type>
       <tstack> list string ; SS => string ; SS </tstack>

  rule <type> SIZE A => .K ... </type>
       <tstack> string ; SS => nat ; SS </tstack>

  rule <type> SLICE A => .K ... </type>
       <tstack> nat ; nat ; string ; SS => option string ; SS </tstack>
```

### Bytes Operations

```k
  rule <type> PACK A => .K ... </type>
       <tstack> T ; SS => bytes ; SS </tstack>

  rule <type> UNPACK A T => .K ... </type>
       <tstack> bytes ; SS => option T ; SS </tstack>

  rule <type> CONCAT A => .K ... </type>
       <tstack> bytes ; bytes ; SS => bytes ; SS </tstack>

  rule <type> CONCAT A => .K ... </type>
       <tstack> list bytes ; SS => bytes ; SS </tstack>

  rule <type> SIZE A => .K ... </type>
       <tstack> bytes ; SS => nat ; SS </tstack>

  rule <type> SLICE A => .K ... </type>
       <tstack> nat ; nat ; bytes ; SS => option bytes ; SS </tstack>
```

### Pair Operations

  rule <type> PAIR A => .K ... </type>
       <tstack> LTy ; RTy ; SS => Pair LTy RTy ; SS </tstack> 

  rule <type> UNPAIR A => .K ... </type>
       <tstack> pair LTy RTy ; SS => LTy ; RTy ; SS </tstack> 

  rule <type> CAR A => .K ... </type>
       <tstack> pair LTy RTy ; SS => LTy ; SS </tstack> 

  rule <type> CDR A => .K ... </type>
       <tstack> pair LTy RTy ; SS => RTy ; SS </tstack>

### Set Operations

```k
  rule <type> EMPTY_SET A T:Type => .K ... </type>
       <tstack> SS => set #Name(T) ; SS </tstack>

  rule <type> MEM A => .K ... </type>
       <tstack> T ; set T ; SS => bool ; SS </tstack>

  rule <type> UPDATE A => .K ... </type>
       <tstack> T ; bool ; set ; SS => set T ; SS </tstack>

  rule <type> SIZE A => .K ... </type>
       <tstack> set _ ; SS => nat ; SS </tstack>
```

```k
  rule <type> ITER A B => .K ...  </type>
       <tstack> [set T S] ; SS => [T #MinimalElement(Set2List(S))] ; SS </tstack>
```

### Shared Map/Big Map Operations

```k
   rule <type> GET A => .K ... </type>
        <tstack> KT ; MT:MapTypeName KT VT ; SS => option VT ; SS </tstack>

  rule <type> MEM A => .K ~> . ... </type>
       <tstack> KT ; MT:MapTypeName KT VT ; SS => bool ; SS </tstack>

  rule <type> UPDATE A => .K  ... </type>
       <tstack> KT ; option VT ; MT:MapTypeName KT VT ; SS => MT KT VT ; SS </tstack>

  rule <type> UPDATE A => .K  ... </type>
       <tstack> KT ; option VT ; MT:MapTypeName KT VT ; SS => MT KT VT ; SS </tstack>
```

### Map Specific Operations

```k
  rule <type> EMPTY_MAP A KT VT => .K ... </type>
       <tstack> SS => #Name(map A KT VT) ; SS </tstack>

  rule <type> SIZE A => .K  ... </type>
       <tstack> map KT VT ; SS => nat ; SS </tstack>
```

```k
  rule <type> MAP A B => .K ~> #DoMap(#MapOpInfo(KT, VT, NoneType, M, .Map, B)) ...  </type>
       <tstack> map KT VT ; SS => SS </tstack>
```

  rule <type> ITER A B => .K  ... </type>
       <tstack> map KT VT ; SS => SS </tstack>
```

### Big Map Specific Operations

```k
  rule <type> EMPTY_BIG_MAP A KT VT => .K ... </type>
       <tstack> SS => #Name(big_map A KT VT) ; SS </tstack>
```

### Option Operations

```k
  rule <type> SOME A => .K  ... </type>
       <tstack> T ; SS => option T ; SS </tstack>

  rule <type> NONE A T:Type => .K  ... </type>
       <tstack> SS => option #Name(T) ; SS </tstack>

  rule <type> IF_NONE A BT BF => .K ~> BT ... </type>
       <tstack> option T ; SS => SS </tstack>

  rule <type> IF_NONE A BT BF => .K ~> BF ... </type>
       <tstack> option T ; SS => T ; SS </tstack>
```

### Union Operations

```k
  rule <type> LEFT A RTy => .K  ... </type>
       <tstack> LTy ; SS => or LTy #Name(RTy) ; SS </tstack>

  rule <type> RIGHT A LTy => .K ... </type>
       <tstack> RTy ; SS => or #Name(LTy) RTy ; SS </tstack>

  rule <type> IF_LEFT A BT BF => .K ~> BT ... </type>
       <tstack> or LTy RTy ; SS => LTy ; SS </tstack>

  rule <type> IF_LEFT A BT BF => .K ~> BF ... </type>
       <tstack> or LTy RTy ; SS => RTy ; SS </tstack>
```

### List Operations

```k
  rule <type> CONS A => .K  ... </type>
       <tstack> T ; list T ; SS => list T ; SS </tstack>

  rule <type> NIL A T => .K  ... </type>
       <tstack> SS => list T ; SS </tstack>

  rule <type> IF_CONS A BT BF => .K ~> BT ... </type>
       <tstack> list T ; SS => [T L1] ; list T ; SS </tstack>

  rule <type> IF_CONS A BT BF => .K ~> BF ... </type>
       <tstack> list T ; SS => SS </tstack>

  rule <type> SIZE A => .K  ... </type>
       <tstack> list T ; SS => nat ; SS </tstack>

  rule <type> ITER A B =>  .K ~>. ... </type>
       <tstack> list T ; SS => SS </tstack>

  rule <type> ITER A B => .K ...  </type>
       <tstack> list T ; SS => [T E] ; SS </tstack>
```

The `MAP` operation over `list`s is defined in terms of a helper function.

```k
  rule <type> MAP A B => #DoMap(T, NoneType, Ls, .List, B) ...  </type>
       <tstack> list T ; SS => SS </tstack>
```

Domain Specific operations
--------------------------

### Timestamp Operations

```k
  rule <type> ADD A => . ... </type>
       <tstack> timestamp ; int ; SS => timestamp ; SS </tstack>

  rule <type> ADD A => . ... </type>
       <tstack> int ; timestamp ; SS => timestamp ; SS </tstack>

  rule <type> SUB A => . ... </type>
       <tstack> timestamp ; int ; SS => timestamp ; SS </tstack>

  rule <type> SUB A => . ... </type>
       <tstack> timestamp ; timestamp ; SS => int ; SS </tstack>
```

### Blockchain Operations

```k
  rule <type> CREATE_CONTRACT A:AnnotationList { C } => . ... </type>
       <tstack> option key_hash ; mutez ; T ; SS => operation ; address ; SS </tstack>

  rule <type> TRANSFER_TOKENS _ => . ... </type>
       <tstack> T ; mutez ; contract T ; SS => operation ; SS </tstack>

  rule <type> SET_DELEGATE A => . ... </type>
       <tstack> option key_hash ; SS => operation ; SS </tstack>
```

```k
  rule <type> CONTRACT _ T => . ... </type>
       <tstack> address ; SS => option contract T ; SS </tstack>

  rule <type> IMPLICIT_ACCOUNT Ann => . ... </type>
       <tstack> key_hash ; SS => contract unit ; SS </tstack>
```

```k
  rule <type> BALANCE A => . ... </type>
       <tstack> SS => mutez ; SS </tstack>

  rule <type> ADDRESS Ann => . ... </type>
       <tstack> contract T ; SS => address ; SS </tstack>

  rule <type> SOURCE Ann => . ... </type>
       <tstack> SS => address ; SS </tstack>

  rule <type> SENDER Ann => . ... </type>
       <tstack> SS => address ; SS </tstack>

  rule <type> SELF Ann => . ... </type>
       <tstack> SS => contract T ; SS </tstack>
       <paramtype> T </paramtype>

  rule <type> AMOUNT Ann => . ... </type>
       <tstack> SS => mutez ; SS </tstack>

  rule <type> CHAIN_ID A => . ... </type>
       <tstack> SS => chain_id ; SS </tstack>

  rule <type> NOW A => . ... </type>
       <tstack> SS => timestamp ; SS </tstack>
```

### Cryptographic Operations

The cryptographic operations are simply stubbed for now.

```k
  rule <type> HASH_KEY A => .K ... </type>
       <tstack> key ; SS => key_hash ; SS </tstack>

  rule <type> BLAKE2B A => .K ... </type>
       <tstack> bytes ; SS => bytes ; SS </tstack>

  rule <type> SHA256 A => .K ... </type>
       <tstack> bytes ; SS => bytes ; SS </tstack>

  rule <type> SHA512 A => .K ... </type>
       <tstack> bytes ; SS => bytes ; SS </tstack>

  rule <type> CHECK_SIGNATURE A => .K ... </type>
       <tstack> key ; signature ; bytes ; SS => bool ; SS </tstack>
```

### Mutez Operations

```k
  rule <type> ADD A => .K ...  </type>
       <tstack> mutez ; mutez ; SS => SS </tstack> 

  rule <type> SUB A => .K ...  </type>
       <tstack> mutez ; mutez ; SS => SS </tstack> 

  rule <type> MUL A => .K ...  </type>
       <tstack> mutez ; nat ; SS => SS </tstack> 

  rule <type> MUL A => .K ...  </type>
       <tstack> nat ; mutez ; SS => SS </tstack> 

  rule <type> EDIV A => .K ... </type>
       <tstack> mutez ; mutez ; SS => option pair nat mutez ; SS </tstack>

  rule <type> EDIV A => .K ... </type>
       <tstack> mutez ; nat ; SS => option pair mutez mutez ; SS </tstack>
```

Debugging Operations
--------------------

```k
  rule <type> PAUSE    => .K ... </type>
  rule <type> STOP     => .K ... </type>
  rule <type> PAUSE(S) => .K ... </type>
  rule <type> TRACE(S) => .K ... </type>
```
