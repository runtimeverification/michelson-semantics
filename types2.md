Execution Semantics
===================

When the `<type>` cell is empty, we consider execution successful.

We handle typed instruction wrappers and blocks here.

```k
  rule <type> I:Instruction ; Is => I ~> Is </type> [structural]
  rule <type> {}                 => .K      </type> [structrual]
  rule <type> { Is:DataList }    => Is      </type> [structural]
```

For now, annotations are ignored.

```k
  syntax Instruction ::= #HandleAnnotations(AnnotationList)
  rule #HandleAnnotations(_) => .
```

Control Structures
------------------

### User-defined Exceptions

The `FAILWITH` instruction lets users terminate execution at any point.

```k
  rule <type> FAILWITH A ~> Rk
        => #HandleAnnotations(A)
        ~> Aborted("FAILWITH instruction reached", D, Rk, Rs)
        ~> Rk
       </type>
       <tstack> [ T D:Data ] ; Rs => ( Failed D ) </tstack>
```

`Aborted()` contains error information when a contract fails at runtime.

It then consumes the rest of the program:

```k
  rule <type> Aborted(_, _, _, _) ~> (_:DataList => .K) ... </type>
  rule <type> Aborted(_, _, _, _) ~> (_:Data => .K) ... </type>
```

Currently, if a program aborts due to the FAILWITH instruction, we throw away
the abortion debug info:

```k
  rule <type> (Aborted(_, _, _, _) => .K) ~> #ExecutePostConditions ... </type>
```

### Conditionals

The control flow instruction's implementations in K should look extremely
similar to their formal description in the [Michelson
documentation](https://tezos.gitlab.io/whitedoc/michelson.html#control-structures).
Keeping this similarity, unless absolutely prevented for performance or K style
reasons, was a major design goal of the semantics.

```k
  rule <type> IF A BT BF => #HandleAnnotations(A) ~> BT ... </type>
       <tstack> [ bool true  ] ; SS => SS </tstack>

  rule <type> IF A BT BF => #HandleAnnotations(A) ~> BF ... </type>
       <tstack> [ bool false ] ; SS => SS </tstack>
```

### Loops

Here we handle concrete loop semantics.

```k
  rule <type> LOOP .AnnotationList B
        => B ~> LOOP .AnnotationList B
           ...
       </type>
       <tstack> [ bool true  ] ; SS => SS </tstack>
  rule <type> LOOP .AnnotationList B => .K ... </type>
       <tstack> [ bool false ] ; SS => SS </tstack>
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

Here we handle symbolic loop semantics.

```symbolic
  rule <type> LOOP A .AnnotationList Body
        => CUTPOINT(!Id, Invariant) ;
           LOOP .AnnotationList {
             Body ;
             CUTPOINT(!Id, Invariant)
           }
           ...
       </type>
       <invs> A |-> Invariant ... </invs>

  rule <type> LOOP_LEFT A .AnnotationList Body
        => CUTPOINT(!Id, Invariant) ;
           LOOP_LEFT .AnnotationList {
             Body ;
             CUTPOINT(!Id, Invariant)
           }
           ...
       </type>
       <invs> A |-> Invariant ... </invs>
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
  rule <type> DIP A B => #HandleAnnotations(A) ~> B ~> #Push(T,D) ... </type>
       <tstack> [ T D ] ; SS => SS </tstack>

  rule <type> DIP A 0 B => #HandleAnnotations(A) ~> B ... </type>

  rule <type> DIP A N B
         => #HandleAnnotations(A)
         ~> DIP .AnnotationList { DIP .AnnotationList N -Int 1 B }
            ...
       </type>
    requires N >Int 0
```

This pseudo-instruction implements the behavior of restoring the previous stack
when a lambda completes execution.

```k
  syntax Instruction ::= #ReturnStack(Stack)

  rule <type> #ReturnStack(SS) => . ... </type>
       <tstack> E ; _ => E ; SS </tstack>
```

`DROP n` is implemented in a recursive style, like in the Michelson
documentation.

```k
  rule <type> DROP A =>  #HandleAnnotations(A) ... </type>
       <tstack> _:StackElement ; SS => SS </tstack>

  rule <type> DROP A I
        => #HandleAnnotations(A)
        ~> DROP .AnnotationList
        ~> DROP .AnnotationList I -Int 1
           ...
       </type>
    requires I >Int 0

  rule <type> DROP A 0 => #HandleAnnotations(A) ... </type>
```

`DUP` and `SWAP` are essentially lifted directly from the docs.

```k
  rule <type> DUP A => #HandleAnnotations(A) ... </type>
       <tstack> X:StackElement ; SS => X ; X ; SS </tstack>

  rule <type> SWAP A => #HandleAnnotations(A) ... </type>
       <tstack> X:StackElement ; Y:StackElement ; SS
            => Y ; X ; SS
       </tstack>
```

`DIG n` and `DUG n` are both implemented using two internal instructions:
`X_DOWN` and `X_UP` which descend down to the `n`th stack position and then
climb back up, respectively.

```k
  rule <type> DIG A N => #HandleAnnotations(A) ~> DIG_DOWN(N, .Stack) ... </type>

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

  rule <type> DUG A N => #HandleAnnotations(A) ~> DUG_DOWN(N, .Stack, T) ... </type>
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

`PUSH` puts its syntactic argument on the stack *when it is a `Value`*.

```k
  rule <type> PUSH A T X => #HandleAnnotations(A) ... </type>
       <tstack> SS => [ #Name(T) X ] ; SS
       </tstack>
    requires isValue(X)
```

If it is not a `Value`, `PUSH` converts its argument to a `Value`, either by
converting the parse-time representation to an internal one or else by looking
up/creating a new symbol in the symbol table.

```k
  rule <type> PUSH A T (X => #MichelineToNative(X, T, .Map, .Map)) ... </type>
    requires notBool isValue(X)
     andBool notBool isSymbolicData(X)
```

```symbolic
  rule <type> PUSH A T (X:SymbolicData => D)  ... </type>
       <symbols> X |-> #TypedSymbol(#Name(T), D) ... </symbols>
  rule <type> (.K => #CreateSymbol(X, T)) ~> PUSH A T X:SymbolicData  ... </type>
       <symbols> Symbols  </symbols>
    requires notBool X in_keys(Symbols)
```

`UNIT` and `LAMBDA` are specialized versions of `PUSH`.

```k
  rule <type> UNIT A => #HandleAnnotations(A) ... </type>
       <tstack> SS => [ unit  Unit] ; SS </tstack>

  rule <type> LAMBDA A T1 T2 C => #HandleAnnotations(A) ... </type>
       <tstack> SS
            => [ (lambda #Name(T1) #Name(T2)) L ] ; SS
       </tstack>
```

### Lambda Evaluation
 An `EXEC` instruction replaces the stack and schedules the restoration of the
old stack after the completion of the lambda code.

```k
  rule <type> EXEC B => #HandleAnnotations(B) ~> C ~> #ReturnStack(SS) ... </type>
       <tstack> [ T1 D ]
             ; [ (lambda T1 T2) L ]
             ; SS
            => [ T1 D ]
       </tstack>
```

`APPLY` demonstrates why lambdas have their type information preserved, as
otherwise we would be unable to produce an appropriate `PUSH` instruction for
the expanded lambda.

```k
  rule <type> APPLY A => #HandleAnnotations(A) ... </type>
       <tstack> [ T0 D ] ;
               [ (lambda (pair T0 T1) T2)
                 L
               ] ;
               SS
            => [ (lambda T1 T2) L
               ] ;
               SS
       </tstack>
```

Core Operations
---------------

### Generic Comparison

```k
  rule <type> EQ A => #HandleAnnotations(A) ... </type>
       <tstack> [int ; SS => [ bool I ==Int 0 ] ; SS </tstack>

  rule <type> NEQ A => #HandleAnnotations(A) ... </type>
       <tstack> [int ; SS => [ bool I =/=Int 0 ] ; SS </tstack>

  rule <type> LT A => #HandleAnnotations(A) ... </type>
       <tstack> [int ; SS => [ bool I <Int 0 ] ; SS </tstack>

  rule <type> GT A => #HandleAnnotations(A) ... </type>
       <tstack> [int ; SS => [ bool I >Int 0 ] ; SS </tstack>

  rule <type> LE A => #HandleAnnotations(A) ... </type>
       <tstack> [int ; SS => [ bool I <=Int 0 ] ; SS </tstack>

  rule <type> GE A => #HandleAnnotations(A) ... </type>
       <tstack> [int ; SS => [ bool I >=Int 0 ] ; SS </tstack>
```

```k
    rule A  >Int B => notBool( A <=Int B ) [simplification]
    rule A >=Int B => notBool( A  <Int B ) [simplification]
```

### Boolean Operations

```k
  rule <type> OR A => #HandleAnnotations(A) ... </type>
       <tstack> [ bool B1 ]
             ; [ bool B2 ]
             ; SS
            => [ bool (B1 orBool B2) ] ; SS
       </tstack>

  rule <type> AND A => #HandleAnnotations(A) ... </type>
       <tstack> [ bool B1 ]
             ; [ bool B2 ]
             ; SS
            => [ bool (B1 andBool B2) ] ; SS
       </tstack>

  rule <type> XOR A => #HandleAnnotations(A) ... </type>
       <tstack> [ bool B1 ]
             ; [ bool B2 ]
             ; SS
             => [ bool (B1 xorBool B2) ] ; SS
       </tstack>

  rule <type> NOT A => #HandleAnnotations(A) ... </type>
       <tstack> [ bool B ] ; SS
            => [ bool (notBool B) ] ; SS
       </tstack>
```

### Integer and Natural Operations

Michelson `int` and `nat` datatypes are both represnted using the K `Int` type.
These operations map directly to their K equivalents.

```k
  rule <type> NEG A => #HandleAnnotations(A) ... </type>
       <tstack> [ N:NumTypeName I ] ; SS => [int ; SS </tstack>

  rule <type> ABS A => #HandleAnnotations(A) ... </type>
       <tstack> [int ; SS => [ nat absInt(I) ] ; SS </tstack>

  rule <type> ISNAT A => #HandleAnnotations(A) ... </type>
       <tstack> [int ; SS => [ (option nat) Some I ] ; SS </tstack>
       requires I >=Int 0

  rule <type> ISNAT A => #HandleAnnotations(A) ... </type>
       <tstack> [int ; SS => [ (option nat) None ] ; SS </tstack>
       requires I <Int 0

  rule <type> INT A => #HandleAnnotations(A) ... </type>
       <tstack> [ (nat =>int ; SS </tstack>

  rule <type> ADD A => #HandleAnnotations(A) ... </type>
       <tstack> [ T1:NumTypeName I1 ] ;
               [ T2:NumTypeName I2 ] ;
               SS
            => [ BinOpNumType(T1,T2) I1 +Int I2 ] ;
               SS
       </tstack>

  rule <type> SUB A => #HandleAnnotations(A) ... </type>
       <tstack> [ T1:NumTypeName I1 ] ;
               [ T2:NumTypeName I2 ] ;
               SS
            => [int ;
               SS
       </tstack>

  rule <type> MUL A => #HandleAnnotations(A) ... </type>
       <tstack> [ T1:NumTypeName I1 ] ;
               [ T2:NumTypeName I2 ] ;
               SS
            => [ BinOpNumType(T1,T2) I1 *Int I2 ] ;
               SS
       </tstack>

  rule <type> EDIV A => #HandleAnnotations(A) ... </type>
       <tstack> [ T1:NumTypeName I1:Int ] ;
               [ T2:NumTypeName 0 ] ;
               SS
            => [ (option (pair BinOpNumType(T1,T2) nat)) None ] ;
               SS
       </tstack>

  rule <type> EDIV A  => #HandleAnnotations(A) ... </type>
       <tstack> [ T1:NumTypeName I1:Int ] ;
               [ T2:NumTypeName I2:Int ] ;
               SS
            => [ (option (pair BinOpNumType(T1,T2) nat))
                   Some P ] ;
               SS
       </tstack>
       requires I2 =/=Int 0

  syntax NumTypeName ::= BinOpNumType(NumTypeName, NumTypeName) [function, functional]
  rule BinOpNumType(N:NumTypeName, int) => int
  rule BinOpNumType(int, N:NumTypeName) => int
  rule BinOpNumType(nat, nat) => nat
```

Bitwise operations on Michelson `int`s map directly onto K `Int` functions.
The `LSL` and `LSR` operations produce exceptions when their shift arugment
overflows.

```k
  rule <type> OR A => #HandleAnnotations(A)  ... </type>
       <tstack> [ nat I1 ] ; [ nat I2 ] ; SS => [ nat I1 |Int I2 ] ; SS </tstack>

  rule <type> AND A => #HandleAnnotations(A) ... </type>
       <tstack> [ T1:NumTypeName I1 ] ; [ nat I2 ] ; SS => [ nat I1 &Int I2 ] ; SS </tstack>

  rule <type> XOR A => #HandleAnnotations(A) ... </type>
       <tstack> [ nat I1 ] ; [ nat I2 ] ; SS => [ nat I1 xorInt I2 ] ; SS </tstack>

  rule <type> NOT A => #HandleAnnotations(A) ... </type>
       <tstack> [ T1:NumTypeName I ] ; SS => [int ; SS </tstack>

  rule <type> LSL A => #HandleAnnotations(A) ... </type>
       <tstack> [ nat X ] ; [ nat S ] ; SS => [ nat X <<Int S ] ; SS </tstack>
    requires S <=Int 256

  rule <type> LSL A ~> Rk
        => #HandleAnnotations(A)
        ~> Aborted("LSL out of range", S, Rk, Rs)
        ~> Rk
        </type>
       <tstack> [ nat C:Int ] ; [ nat S:Int ] ; Rs => ( GeneralOverflow C S ) </tstack>
    requires S >Int 256

  rule <type> LSR A => #HandleAnnotations(A) ... </type>
       <tstack> [ nat X ] ; [ nat S ] ; SS => [ nat X >>Int S ] ; SS </tstack>
    requires S <=Int 256

  rule <type> LSR A ~> Rk
        => #HandleAnnotations(A)
        ~> Aborted("LSR out of range", S, Rk, Rs)
        ~> Rk
        </type>
       <tstack> [ nat X ] ; [ nat S ] ; Rs => ( GeneralOverflow X S ) </tstack>
       requires S >Int 256
```

### `COMPARE` Instruction

The `COMPARE` instruction is defined over all comparable datatypes.

```k
  rule <type> COMPARE A => #HandleAnnotations(A) ... </type>
       <tstack> [ TY V1 ] ; [ TY V2 ] ; SS => [int ; SS </tstack>
    requires #IsComparable(TY)

  syntax Bool ::= #IsComparable(TypeName) [function, functional]
  // Comparables
  rule #IsComparable(NumberType) => true
  rule #IsComparable(string) => true
  rule #IsComparable(bytes) => true
  rule #IsComparable(mutez) => true
  rule #IsComparable(bool) => true
  rule #IsComparable(key_hash) => true
  rule #IsComparable(timestamp) => true
  rule #IsComparable(address) => true
  rule #IsComparable(pair T1 T2) => #IsComparable(T1) andBool #IsComparable(T2)

  // Nullary Incomparables
  rule #IsComparable(key) => false
  rule #IsComparable(unit) => false
  rule #IsComparable(signature) => false
  rule #IsComparable(operation) => false
  rule #IsComparable(chain_id) => false

  // Unary Incomparables
  rule #IsComparable(option _) => false
  rule #IsComparable(list _) => false
  rule #IsComparable(set _) => false
  rule #IsComparable(contract _) => false

  // Bianry Incomparables
  rule #IsComparable(or _ _) => false
  rule #IsComparable(lambda _ _) => false
  rule #IsComparable(map _ _) => false
  rule #IsComparable(big_map _ _) => false
```

We define `COMPARE` in terms of a `#DoCompare` function.

```k
  syntax Int ::= #DoCompare(Data, Data) [function, functional]

  rule #DoCompare(true, true) => 0
  rule #DoCompare(false, false) => 0
  rule #DoCompare(false, true) => -1
  rule #DoCompare(true, false) => 1

  rule #DoCompare(I1:Int, I2:Int) => -1 requires I1 <Int I2
  rule #DoCompare(I1:Int, I2:Int) => 0 requires I1 ==Int I2
  rule #DoCompare(I1:Int, I2:Int) => 1 requires I1 >Int I2

  rule #DoCompare(S1:String, S2:String) => -1 requires S1 <String S2
  rule #DoCompare(S1:String, S2:String) => 0 requires S1 ==String S2
  rule #DoCompare(S1:String, S2:String) => 1 requires S1 >String S2

  rule #DoCompare(P, P) => -1                 requires #DoCompare(A1, B1) ==Int -1 rule #DoCompare(P, P) => #DoCompare(A2, B2) requires #DoCompare(A1, B1) ==Int 0 rule #DoCompare(P, P) => 1                  requires #DoCompare(A1, B1) ==Int 1 
  rule #DoCompare(B1:Bytes, B2:Bytes) => #DoCompare(Bytes2Int(B1, BE, Unsigned), Bytes2Int(B2, BE, Unsigned))
  rule #DoCompare(#KeyHash(S1), #KeyHash(S2)) => #DoCompare(S1, S2)
  rule #DoCompare(V, V) => #DoCompare(I1, I2)
  rule #DoCompare(#Timestamp(I1), #Timestamp(I2)) => #DoCompare(I1, I2)
  rule #DoCompare(#Address(S1), #Address(S2)) => #DoCompare(S1, S2)
```

The `#DoCompare` function requires additional lemmas for symbolic execution.

```symbolic
  rule #DoCompare(I1:Bool, I2:Bool) <Int 0  => (I1 ==Bool false) andBool (I2 ==Bool true)                       [simplification]
  rule #DoCompare(I1:Bool, I2:Bool) <=Int 0 => ((I1 ==Bool false) andBool (I2 ==Bool true)) orBool I1 ==Bool I2 [simplification]
  rule #DoCompare(I1:Bool, I2:Bool) ==Int 0 => I1 ==Bool I2                                                     [simplification]
  rule #DoCompare(I1:Bool, I2:Bool) >=Int 0 => ((I1 ==Bool true) andBool (I2 ==Bool false)) orBool I1 ==Bool I2 [simplification]
  rule #DoCompare(I1:Bool, I2:Bool) >Int 0  => (I1 ==Bool true) andBool (I2 ==Bool false)                       [simplification]

  rule #DoCompare(I1:Int, I2:Int) <Int 0  => I1 <Int I2  [simplification]
  rule #DoCompare(I1:Int, I2:Int) <=Int 0 => I1 <=Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) ==Int 0 => I1 ==Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) >=Int 0 => I1 >=Int I2 [simplification]
  rule #DoCompare(I1:Int, I2:Int) >Int 0  => I1 >Int I2  [simplification]

  rule #DoCompare(I1:String, I2:String) <Int 0  => I1 <String I2  [simplification]
  rule #DoCompare(I1:String, I2:String) <=Int 0 => I1 <=String I2 [simplification]
  rule #DoCompare(I1:String, I2:String) ==Int 0 => I1 ==String I2 [simplification]
  rule #DoCompare(I1:String, I2:String) >=Int 0 => I1 >=String I2 [simplification]
  rule #DoCompare(I1:String, I2:String) >Int 0  => I1 >String I2  [simplification]

  // TODO: at some point this rule should be builtin
  rule X ==String X => true [simplification]
```

### String Operations

```k
  syntax String ::= #ConcatStrings(List, String) [function]
  rule #ConcatStrings(.List, A) => A
  rule #ConcatStrings(ListItem(S1) DL, A) => #ConcatStrings(DL, A +String S1)

  rule <type> CONCAT A => #HandleAnnotations(A) ... </type>
       <tstack> [string S1] ; [string S2] ; SS => [string S1 +String S2] ; SS </tstack>

  rule <type> CONCAT A => #HandleAnnotations(A) ... </type>
       <tstack> [(list string) L] ; SS => [string #ConcatStrings(L, "")] ; SS </tstack>

  rule <type> SIZE A => #HandleAnnotations(A) ... </type>
       <tstack> [string S] ; SS => [nat lengthString(S)] ; SS </tstack>
```

The actual out of bounds conditions here are determined by experimentation.
Earlier versions of the semantics didn't check if O was in bounds, resulting in
`Slice("", 0, 0) => Some ""` rather than the correct
`#SliceString("", 0, 0) => None`

```k
  rule <type> SLICE A => #HandleAnnotations(A) ... </type>
       <tstack> [nat O] ; [nat L] ; [string S] ; SS => [option string #SliceString(S, O, L)] ; SS </tstack>

  syntax OptionData ::= #SliceString(String, Int, Int) [function]

  rule #SliceString(S, O, L) => Some substrString(S, O, O +Int L)
    requires O >=Int 0
     andBool L >=Int 0
     andBool O <Int lengthString(S)
     andBool (O +Int L) <=Int lengthString(S)

  rule #SliceString(S, O, L) => None [owise]
```

### Bytes Operations

The bytes instructions have a stubbed implementation for the time being, since
the actual serialization format is not formally unspecified.

```k
  rule <type> PACK A => #HandleAnnotations(A) ... </type>
       <tstack> [T V] ; SS => [bytes #Packed(T,V)] ; SS </tstack>

  rule <type> UNPACK A _ => #HandleAnnotations(A) ... </type>
       <tstack> [bytes #Packed(T,V)] ; SS => [option T Some V] ; SS </tstack>
```

The `CONCAT` operation over two bytes is relatively straightforward since we
already have helper functions to extract bytes content.

```k
  rule <type> CONCAT A => #HandleAnnotations(A) ... </type>
       <tstack> [bytes B1] ; [bytes B2] ; SS => [bytes B1 +Bytes B2] ; SS </tstack>
```

`CONCAT` over lists of bytes is somewhat more involved, since we need to
distinguish this case from lists of strings.

```k
  rule <type> CONCAT A => #HandleAnnotations(A) ... </type>
       <tstack> [(list bytes) L] ; SS => [bytes #ConcatBytes(L, .Bytes)] ; SS </tstack>

  syntax Bytes ::= #ConcatBytes(List, Bytes) [function]
  rule #ConcatBytes(.List, A) => A
  rule #ConcatBytes(ListItem(B) DL, A) => #ConcatBytes(DL, A +Bytes B)
```

`SIZE` is relatively simple, except that we must remember to divide by two,
since bytes length is measured in terms of number of bytes, not characters in
the hex string.

```k
  rule <type> SIZE A => #HandleAnnotations(A) ... </type>
       <tstack> [bytes B] ; SS => [nat lengthBytes(B)] ; SS </tstack>
```

The remaining operations are defined in terms of the same operations on
strings, allowing for code reuse.

```k
  rule <type> SLICE A => #HandleAnnotations(A) ... </type>
       <tstack> [nat O:Int] ; [nat L:Int] ; [bytes B:Bytes] ; SS => [option bytes #SliceBytes(B, O, L)] ; SS </tstack>

  syntax OptionData ::= #SliceBytes(Bytes, Int, Int) [function]
  // ----------------------------------------------------------
  rule #SliceBytes(S, O, L) => Some substrBytes(S, O, O +Int L)
    requires O >=Int 0
     andBool L >=Int 0
     andBool O <Int lengthBytes(S)
     andBool (O +Int L) <=Int lengthBytes(S)

  rule #SliceBytes(S, O, L) => None [owise]
```

###P PAIR A => #HandleAnnotations(A) ... </type> <tstack> [LTy L] ; [RTy R] ; SS => P ; SS </tstack> 
  rule <type> UNPAIR A => #HandleAnnotations(A) ... </type>
       <tstack> P ; SS => [LTy L] ; [RTy R] ; SS </tstack> 
  rule <type> CAR A => #HandleAnnotations(A) ... </type>
       <tstack> P ; SS => [LTy L] ; SS </tstack> 
  rule <type> CDR A => #HandleAnnotations(A) ... </type>
       <tstack> P ; SS => [RTy R] ; SS </tstack> ```

### Set Operations

```k
  rule <type> EMPTY_SET A T:Type => #HandleAnnotations(A) ... </type>
       <tstack> SS => [set #Name(T) .Set] ; SS </tstack>

  rule <type> MEM A => #HandleAnnotations(A) ... </type>
       <tstack> [T X] ; [set T S:Set] ; SS => [bool X in S] ; SS </tstack>

  // True to insert, False to remove.
  rule <type> UPDATE A => #HandleAnnotations(A) ... </type>
       <tstack> [T D] ; [bool true] ; [set T S:Set] ; SS => [set T (SetItem(D) S)] ; SS </tstack>

  rule <type> UPDATE A => #HandleAnnotations(A) ... </type>
       <tstack> [T D] ; [bool false] ; [set T SetItem(D) S] ; SS => [set T S] ; SS </tstack>

  rule <type> UPDATE A => #HandleAnnotations(A) ... </type>
       <tstack> [T D] ; [bool false] ; [set T S:Set] ; SS => [set T S:Set] ; SS </tstack>
       requires notBool(D in S)

  rule <type> SIZE A => #HandleAnnotations(A) ... </type>
       <tstack> [set _ S:Set] ; SS => [nat size(S)] ; SS </tstack>
```

Note that, according to the Michelson documentation, set iteration order is
actually defined (the set is iterated over in ascending order).
For simplicity we implement this by repeatedly selecting the minimal element.

```k
  rule <type> ITER A _ => #HandleAnnotations(A) ... </type>
       <tstack> [set _ .Set] ; SS => SS </tstack>

  rule <type> ITER A B
        => #HandleAnnotations(A)
        ~> B
        ~> #Push(set T,S -Set SetItem(#MinimalElement(Set2List(S))))
        ~> ITER .AnnotationList B
        ...
        </type>
       <tstack> [set T S] ; SS => [T #MinimalElement(Set2List(S))] ; SS </tstack>
    requires size(S) >Int 0

  syntax Data ::= #MinimalElement(List) [function]
  syntax Data ::= #MinimalElementAux(List, Data) [function]

  rule #MinimalElement(ListItem(H) L) => #MinimalElementAux(L, H)
  rule #MinimalElementAux(.List, M) => M
  rule #MinimalElementAux(ListItem(H) L, M)
    => #MinimalElementAux(L, M) requires #DoCompare(M, H) <=Int 0
  rule #MinimalElementAux(ListItem(H) L, M)
    => #MinimalElementAux(L, H) requires #DoCompare(M, H) ==Int 1
```

### Shared Map/Big Map Operations

Internally, we represent `map`s and `big_map`s identically using K maps.
For this reason, many map operations share an identical representation upto
typing (shared operations use a generic `MapTypeName`).

```k
  rule <type> GET A => #HandleAnnotations(A) ... </type>
       <tstack> [KT X] ; [MT:MapTypeName KT VT M] ; SS => [option VT None] ; SS </tstack>
    requires isValue(X)
     andBool notBool(X in_keys(M))
```

```concrete
   rule <type> GET A => #HandleAnnotations(A) ... </type>
        <tstack> [KT X] ; [MT:MapTypeName KT VT M] ; SS => [option VT Some {M[X]}:>Data] ; SS </tstack>
     requires isValue(X)
      andBool X in_keys(M)
```

```symbolic
  rule <type> GET A
        => #HandleAnnotations(A)
        ~> #Assume(?Val == #MakeFresh(#Type(VT)))
        ~> #Assume(M[X] == ?Val)
           ...
       </type>
       <tstack> [KT X] ; [MT:MapTypeName KT VT M] ; SS => [option VT Some ?Val] ; SS </tstack>
    requires X in_keys(M)

  rule K1 in_keys(M:Map[ K2 <- _ ]) => K1 ==K K2 orBool K1 in_keys(M) [simplification]
```

```k
  rule <type> MEM A => #HandleAnnotations(A) ~> . ... </type>
       <tstack> [KT X] ; [MT:MapTypeName KT VT M] ; SS => [bool X in_keys(M)] ; SS </tstack>

  rule <type> UPDATE A => #HandleAnnotations(A)  ... </type>
       <tstack> [KT K] ; [option VT Some V] ; [MT:MapTypeName KT VT M:Map] ; SS => [MT KT VT M[K <- V]] ; SS </tstack>

  rule <type> UPDATE A => #HandleAnnotations(A)  ... </type>
       <tstack> [KT K] ; [option VT None] ; [MT:MapTypeName KT VT M:Map] ; SS => [MT KT VT M[K <- undef]] ; SS </tstack>
```

### Map Specific Operations

```k
  rule <type> EMPTY_MAP A KT VT => #HandleAnnotations(A) ... </type>
       <tstack> SS => [#Name(map A KT VT) .Map] ; SS </tstack>

  rule <type> SIZE A => #HandleAnnotations(A)  ... </type>
       <tstack> [map KT VT M:Map] ; SS => [nat size(M)] ; SS </tstack>
```

The `MAP` operation over maps is defined via psuedoinstruction `#DoMap`.

```k
  rule <type> MAP A B
        => #HandleAnnotations(A)
        ~> #DoMap(#MapOpInfo(KT, VT, NoneType, M, .Map, B))
           ...
       </type>
       <tstack> [map KT VT M] ; SS => SS </tstack>
```

`#DoMap` takes a `MapOpInfo` struct that holds the original map and newly built
map, as well as typing information. The map instruction proceeds as follows.
Let the map have `p` entries in sorted order:

```
{ Elt key₁ val₁ ; Elt key₂ val₂ ; ... ; Elt keyₚ valₚ }
```

We apply the map `code`, replacing the map on top of the stack with each
map entry in sorted order, and then build the new map by pairing each key
with the corresponding `code` generated value (with a possibly new type),
as described by the `MAP` typing rule below.

```
          Γ ⊢ code :: ( pair key_ty val_ty1 ) : A ⇒ val_ty2 : A
:------------------------------------------------------------------------
      Γ ⊢ MAP code :: map key_ty val_ty1 : A ⇒ map key_ty val_ty2 : A
```

```k
  syntax MapOpInfo ::= #MapOpInfo(keyType     :TypeName,
                                  origValType :TypeName,
                                  newValType  :MaybeTypeName,
                                  origMap     :Map,
                                  newMap      :Map,
                                  mapBody     :Block)

  syntax Instruction ::= #DoMap(MapOpInfo)
  // -------------------------------------
  rule <type> #DoMap(#MapOpInfo(KT, VT, NVT, M1, M2, B))
        => B
        ~> #DoMapAux(#MinKey(M1),
                     #MapOpInfo(KT, VT, NVT, M1[#MinKey(M1) <- undef], M2, B))
           ...
       </type>
       <tstack> SS
            => [pair KT VTP {M1[#MinKey(M1)]}:>Data] ; SS
       </tstack>
    requires size(M1) >Int 0

  rule <type> #DoMap(#MapOpInfo(KT, VT, NVT, .Map, M, _)) => .K ... </type>
       <tstack> SS => [map KT #DefaultType(NVT,VT) M] ; SS </tstack>

  syntax Instruction ::= #DoMapAux(Data, MapOpInfo)
  // ----------------------------------------------
  rule <type> #DoMapAux(K, #MapOpInfo(KT, VT, NVT, M1, M2, B))
        => #DoMap(#MapOpInfo(KT, VT, NVT', M1, M2[K <- V], B))
        ...
       </type>
       <tstack> [NVT' V] ; SS => SS </tstack>
    requires #CompatibleTypes(NVT,NVT')

  syntax Data ::= #MinKey(Map) [function]
  // ------------------------------------
  rule #MinKey(M) => #MinimalElement(keys_list(M))
```

We define auxiliary functions for computing the result type of `MAP`.

```k
  syntax MaybeTypeName ::= TypeName
                         | "NoneType"

  syntax Bool ::= #CompatibleTypes(MaybeTypeName, TypeName) [function]
  // -----------------------------------------------------------------
  rule #CompatibleTypes(NoneType,T) => true
  rule #CompatibleTypes(T1:TypeName, T2) => T1 ==K T2

  syntax TypeName ::= #DefaultType(MaybeTypeName, TypeName) [function]
  // -----------------------------------------------------------------
  rule #DefaultType(T:TypeName, _) => T
  rule #DefaultType(NoneType, T) => T
```

`ITER` is relatively easy to implement using a straightforward recursive style,
since it does not need to track the new map while keeping it off the stack.

```k
  rule <type> ITER A B => #HandleAnnotations(A)  ... </type>
       <tstack> [map KT VT .Map] ; SS => SS </tstack>

  rule <type> ITER A B
        => #HandleAnnotations(A)
        ~> B
        ~> #Push(map KT VT, M[#MinKey(M) <- undef])
        ~> ITER .AnnotationList B
           ...
       </type>
       <tstack> [map KT VT M:Map] ; SS
            => [pair KT VTP {M[#MinKey(M)]}:>Data] ; SS </tstack>
    requires size(M) >Int 0
```

### Big Map Specific Operations

```k
  rule <type> EMPTY_BIG_MAP A KT VT => #HandleAnnotations(A) ... </type>
       <tstack> SS => [#Name(big_map A KT VT) .Map] ; SS </tstack>
```

### Option Operations

```k
  rule <type> SOME A => #HandleAnnotations(A)  ... </type>
       <tstack> [T X] ; SS => [option T Some X] ; SS </tstack>

  rule <type> NONE A T:Type => #HandleAnnotations(A)  ... </type>
       <tstack> SS => [option #Name(T) None] ; SS </tstack>

  rule <type> IF_NONE A BT BF => #HandleAnnotations(A) ~> BT ... </type>
       <tstack> [option T None] ; SS => SS </tstack>

  rule <type> IF_NONE A BT BF => #HandleAnnotations(A) ~> BF ... </type>
       <tstack> [option T Some V] ; SS => [T V] ; SS </tstack>
```

### Union Operations

```k
  rule <type> LEFT A RTy:Type => #HandleAnnotations(A)  ... </type>
       <tstack> [LTy X:Data] ; SS => [or LTy #Name(RTy) Left X] ; SS </tstack>

  rule <type> RIGHT A LTy:Type => #HandleAnnotations(A) ... </type>
       <tstack> [RTy X:Data] ; SS => [or #Name(LTy) RTy Right X] ; SS </tstack>

  rule <type> IF_LEFT A BT BF => #HandleAnnotations(A) ~> BT ... </type>
       <tstack> [or LTy RTy Left V] ; SS => [LTy V] ; SS </tstack>

  rule <type> IF_LEFT A BT BF => #HandleAnnotations(A) ~> BF ... </type>
       <tstack> [or LTy RTy Right V] ; SS => [RTy V] ; SS </tstack>
```

### List Operations

```k
  rule <type> CONS A => #HandleAnnotations(A)  ... </type>
       <tstack> [T V] ; [list T L:List] ; SS => [list T ListItem(V) L] ; SS </tstack>

  rule <type> NIL A T => #HandleAnnotations(A)  ... </type>
       <tstack> SS => [list #Name(T) .List] ; SS </tstack>

  rule <type> IF_CONS A BT BF => #HandleAnnotations(A) ~> BT ... </type>
       <tstack> [list T ListItem(L1) Ls] ; SS => [T L1] ; [list T Ls] ; SS </tstack>

  rule <type> IF_CONS A BT BF => #HandleAnnotations(A) ~> BF ... </type>
       <tstack> [list T .List ] ; SS => SS </tstack>

  rule <type> SIZE A => #HandleAnnotations(A)  ... </type>
       <tstack> [list T L:List] ; SS => [nat size(L)] ; SS </tstack>

  rule <type> ITER A B =>  #HandleAnnotations(A) ~>. ... </type>
       <tstack> [list T .List] ; SS => SS </tstack>

  rule <type> ITER A B
        => #HandleAnnotations(A)
        ~> B
        ~> #Push(list T,Ls)
        ~> ITER .AnnotationList B
           ...
       </type>
       <tstack> [list T ListItem(E) Ls] ; SS => [T E] ; SS </tstack>
```

The `MAP` operation over `list`s is defined in terms of a helper function.

```k
  rule <type> MAP A B
        => #HandleAnnotations(A)
        ~> #DoMap(T, NoneType, Ls, .List, B)
           ...
       </type>
       <tstack> [list T Ls] ; SS => SS </tstack>

  syntax Instruction ::= #DoMap(TypeName, MaybeTypeName, List, List, Block)
                       | #DoMapAux(TypeName, MaybeTypeName, List, List, Block)
  // -------------------------------------------------------------------------
  rule <type> #DoMap(T, NT, .List, Acc, B) => .K ... </type>
       <tstack> SS => [list #DefaultType(NT,T) #ReverseList(Acc)] ; SS </tstack>

  rule <type> #DoMap(T, NT, ListItem(E) Ls, Acc, B)
        => B
        ~> #DoMapAux(T, NT, Ls, Acc, B)
           ...
       </type>
       <tstack> SS => [T E] ; SS </tstack>

  rule <type> #DoMapAux(T, NT, Ls, Acc, B)
        => #DoMap(T, NT', Ls, ListItem(E) Acc, B)
           ...
       </type>
       <tstack> [NT' E] ; SS => SS </tstack>
    requires #CompatibleTypes(NT,NT')

  syntax List ::= #ReverseList(List) [function]
  syntax List ::= #ReverseListAux(List, List) [function]
  // ---------------------------------------------------
  rule #ReverseList(L) => #ReverseListAux(L, .List)
  rule #ReverseListAux(ListItem(L1) Ls, Acc)
    => #ReverseListAux(Ls, ListItem(L1) Acc)
  rule #ReverseListAux(.List, Acc) => Acc
```

Domain Specific operations
--------------------------

### Timestamp Operations

Timestamps are simply wrapped ints in Michelson, so the implementation of
simple arithmetic over them is straightforward. The differing argument types
however forces us to use two rules for each operation.

```k
  rule <type> ADD A => . ... </type>
       <tstack> [timestamp #Timestamp(I1)] ; int ; SS => [timestamp #Timestamp(I1 +Int I2)] ; SS </tstack>

  rule <type> ADD A => . ... </type>
       <tstack> int ; [timestamp #Timestamp(I2)] ; SS => [timestamp #Timestamp(I1 +Int I2)] ; SS </tstack>

  rule <type> SUB A => . ... </type>
       <tstack> [timestamp #Timestamp(I1)] ; int ; SS => [timestamp #Timestamp(I1 -Int I2)] ; SS </tstack>

  rule <type> SUB A => . ... </type>
       <tstack> [timestamp #Timestamp(I1)] ; [timestamp #Timestamp(I2)] ; SS => int ; SS </tstack>
```

### Blockchain Operations

```k
  rule <type> CREATE_CONTRACT A:AnnotationList { C } => . ... </type>
       <tstack> [option key_hash Delegate:OptionData] ;
               mutez
               ; [T Storage:Data] ;
               SS
            => [operation Create_contract(O, C, Delegate, Initial, Storage)] ;
               [address #Address("@Address(" +String Int2String(!_:Int) +String ")")] ;
               SS
       </tstack>
       <nonce> #Nonce(O) => #NextNonce(#Nonce(O)) </nonce>

  rule <type> TRANSFER_TOKENS _ => . ... </type>
       <tstack> [T D] ; mutez
       ; [contract T #Contract(A, _)] ; SS => [operation Transfer_tokens(O, D, M, A)] ; SS
       </tstack>
       <nonce> #Nonce(O) => #NextNonce(#Nonce(O)) </nonce>

  rule <type> SET_DELEGATE A => . ... </type>
       <tstack> [option key_hash D] ; SS => [operation Set_delegate(O, D)] ; SS </tstack>
       <nonce> #Nonce(O) => #NextNonce(#Nonce(O)) </nonce>
```

Each `operation` value must have a unique nonce.
The nonce generation process is unspecified by the Michelson semantics.
We implement it here.

```k
  syntax OperationNonce ::= #NextNonce(OperationNonce) [function]
  rule #NextNonce(#Nonce(I)) => #Nonce(I +Int 1)
```

These instructions push fresh `contract` literals on the stack corresponding
to the given addresses/key hashes.

```k
  rule <type> CONTRACT _ T => . ... </type>
       <tstack> [address A] ; SS => [option contract #Name(T) Some {M[A]}:>Data] ; SS </tstack>
       <knownaddrs> M </knownaddrs>
    requires A in_keys(M)
     andBool #TypeFromContractStruct({M[A]}:>Data) ==K T

  rule <type> CONTRACT _ T => . ... </type>
       <tstack> [address A:Address] ; SS => [option contract #Name(T) None] ; SS </tstack>
       <knownaddrs> M </knownaddrs> [owise]

  rule <type> IMPLICIT_ACCOUNT Ann => . ... </type>
       <tstack> [key_hash #KeyHash(A)] ; SS
            => [contract unit #Contract(#Address(A), unit .AnnotationList)] ; SS
       </tstack>

  syntax Type ::= #TypeFromContractStruct(Data) [function]
  rule #TypeFromContractStruct(#Contract(_, T)) => T
```

These instructions push blockchain state on the stack.

```k
  rule <type> BALANCE A => . ... </type>
       <tstack> SS => mutez
       ; SS </tstack> <mybalance> B </mybalance>

  rule <type> ADDRESS Ann => . ... </type>
       <tstack> [contract T #Contract(A, _)] ; SS => [address A] ; SS </tstack>

  rule <type> SOURCE Ann => . ... </type>
       <tstack> SS => [address A] ; SS </tstack>
       <sourceaddr> A </sourceaddr>

  rule <type> SENDER Ann => . ... </type>
       <tstack> SS => [address A] ; SS </tstack>
       <senderaddr> A </senderaddr>

  rule <type> SELF Ann => . ... </type>
       <tstack> SS => [contract #Name(T) #Contract(A, T)] ; SS </tstack>
       <paramtype> T </paramtype>
       <myaddr> A </myaddr>

  rule <type> AMOUNT Ann => . ... </type>
       <tstack> SS => mutez
       ; SS </tstack> <myamount> M </myamount>

  rule <type> CHAIN_ID A => . ... </type>
       <tstack> SS => [chain_id C] ; SS </tstack>
       <mychainid> C </mychainid>

  rule <type> NOW A => . ... </type>
       <tstack> SS => [timestamp N] ; SS </tstack>
       <mynow> N </mynow>
```

### Cryptographic Operations

The cryptographic operations are simply stubbed for now.

```k
  syntax String ::= #Blake2BKeyHash(String) [function]
  rule #Blake2BKeyHash(S) => S

  rule <type> HASH_KEY A => #HandleAnnotations(A) ... </type>
       <tstack> [key #Key(S)] ; SS => [key_hash #KeyHash(#Blake2BKeyHash(S))] ; SS </tstack>

  rule <type> BLAKE2B A => #HandleAnnotations(A) ... </type>
       <tstack> [bytes B:MBytes] ; SS => [bytes #Blake2B(B)] ; SS </tstack>

  rule <type> SHA256 A => #HandleAnnotations(A) ... </type>
       <tstack> [bytes B:MBytes] ; SS => [bytes #SHA256(B)] ; SS </tstack>

  rule <type> SHA512 A => #HandleAnnotations(A) ... </type>
       <tstack> [bytes B:MBytes] ; SS => [bytes #SHA512(B)] ; SS </tstack>

  syntax MBytes ::= #SignedMBytes(Key, Signature, MBytes)

  /*
  // FIXME: The haskell backend does not support distinguishing these rules.
  rule <type> CHECK_SIGNATURE A => #HandleAnnotations(A) ... </type>
       <tstack> #Key(K)
            ~> #Signature(S)
            ~> #SignedMBytes(#Key(K), #Signature(S), _)
            => true
               ...
       </tstack>

  rule <type> CHECK_SIGNATURE A => #HandleAnnotations(A) ... </type>
       <tstack> #Key(_)
            ~> #Signature(_)
            ~> _:MBytes
            => false
               ...
       </tstack> [owise]
  */
```

### Mutez Operations

Mutez operations need to check their results since `mutez` is not an unlimited
precision type.
This internal instruction checks and produces the appropriate error case if the
value is invalid.

```k
  syntax Instruction ::= #ValidateMutezAndPush(Mutez, Int, Int)
  // ----------------------------------------------------------
  rule <type> #ValidateMutezAndPush(V, _, _) => . ... </type>
       <tstack> SS => mutez
       ; SS </tstack> requires #IsLegalMutezValue(I)

  rule <type> #ValidateMutezAndPush(V, I1, I2) ~> Rk
        => Aborted("Mutez out of bounds", I, Rk, Rs) ~> Rk </type>
       <tstack> Rs => #FailureFromMutezValue(V, I1, I2) </tstack>
    requires notBool #IsLegalMutezValue(I)

  syntax FailedStack ::= #FailureFromMutezValue(Mutez, Int, Int) [function]
  // ----------------------------------------------------------------------
  rule #FailureFromMutezValue(V, I1, I2)
    => ( MutezOverflow I1 I2 ) requires I >=Int #MutezOverflowLimit
  rule #FailureFromMutezValue(V, I1, I2)
    => ( MutezUnderflow I1 I2 ) requires I <Int 0
```

Other than the mutez validation step, these arithmetic rules are essentially
identical to those defined over integers.

```k
  rule <type> ADD A
        => #ValidateMutezAndPush(V, I1, I2)
        ~> #HandleAnnotations(A)
           ...
       </type>
       <tstack> mutez
       ; mutez ; SS => SS </tstack> 
  rule <type> SUB A
        => #ValidateMutezAndPush(V, I1, I2)
        ~> #HandleAnnotations(A)
           ...
       </type>
       <tstack> mutez
       ; mutez ; SS => SS </tstack> 
  rule <type> MUL A
        => #ValidateMutezAndPush(V, I1, I2)
        ~> #HandleAnnotations(A)
           ...
       </type>
       <tstack> mutez
       ; [nat I2] ; SS => SS </tstack> 
  rule <type> MUL A
        => #ValidateMutezAndPush(V, I1, I2)
        ~> #HandleAnnotations(A)
           ...
       </type>
       <tstack> [nat I1] ; mutez
       ; SS => SS </tstack> 
  rule <type> EDIV A => #HandleAnnotations(A) ... </type>
       <tstack> mutez
       ; mutez ; SS => [option pair nat mutez None] ; SS </tstack> 
  rule <type> EDIV A => #HandleAnnotations(A) ... </type>
       <tstack> mutez
       ; [nat 0] ; SS => [option pair mutez mutez None] ; SS </tstack> 
  rule <type> EDIV A => #HandleAnnotations(A) ... </type>
       <tstack> mutez
       ; mutez
               ; SS
            => [option pair nat mutez Some P] ; SS
       </tstack>
    requires I2 >Int 0

  rule <type> EDIV A => #HandleAnnotations(A) ... </type>
       <tstack> mutez
       ; [nat I2] ;
               SS
            => [option pair mutez mutez Some P] ; SS
       </tstack>
       requires I2 >Int 0
```

Debugging Operations
--------------------

We introduce several pseudo-instructions that are used for debugging:

-   `STOP` is an instruction that cannot be evaluated and causes the program to
    get stuck
-   `PAUSE` non-determinstically chooses to either do nothing or else `STOP`;
    it optionally traces at its pause point.
-   `TRACE` appends its string content to the `<trace>` cell as a debugging aid
    for complex programs.

```k
  rule <type> PAUSE    => .K                ... </type>
  rule <type> PAUSE    => STOP              ... </type>
  rule <type> PAUSE(S) => TRACE(S) ~> PAUSE ... </type>
  rule <type> TRACE(S) => .K ... </type>
       <trace> K:K => (K ~> S) </trace>
```
