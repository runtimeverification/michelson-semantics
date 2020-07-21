# Micheline Format

Micheline is a data serialization format similar to JSON or XML. Like other
formats, various sub-languages may be constructed on top of Micheline by
applying a schema which restricts the set of well-formed expressions.

The syntax of the programming language Michelson can be defined as a
sublanguage of Micheline.
Using the markdown selectors feature of K, we define two syntaxes:

1.  we define the Michelson syntax using the selector `k`.
2.  we define the Micheline syntax using the `k` selector and additionally the
    `micheline` selector.

Micheline expressions (called nodes) have an extremely sinmple syntax that we
define below.

```k
module MICHELINE-COMMON-SYNTAX
  imports INT
  imports STRING
```

## Micheline Node Types

Micheline has five node types: integers, strings, byte strings, primitives,
and sequences.

```k
  syntax MichelineNode ::= Int
                         | String
                         | MichelineBytes
                         | PrimitiveNode
                         | SequenceNode
```

We now give the syntactic definition of each node type individually.

1.  Integers - we import the K builtin --- this is slightly incorrect, because K
    admits an initial `+` which Micheline rejects

2.  Double-quoted strings - we import K builtin --- this is also slightly
    incorrect, because K's escape sequences differ in two ways:
    - K accepts escape `\b` and rejects escape `\f`; Micheline is opposite
    - K accepts numerical escape codes for arbitrary Unicode codepoints

3.  Byte strings using hexadecimal notation with the prefix `0x`

    ```k
    syntax BytesToken
    syntax MichelineBytes ::= BytesToken // [klabel(BytesToInternal), symbol, function, avoid]
    ```

4.  Primitives applications---which are primtives (unquoted alphanumeric strings
    with underscores) followed by a possibly empty primitive arguemnt list (we
    will come back to define primtive arguments later)

    ```k
    syntax PrimitiveToken
    syntax Primitive ::= PrimitiveToken // [klabel(PrimitiveToInternal), symbol, function, avoid]
    ```

    ```k
    syntax PrimitiveNode ::= Primitive
                           | PrimitiveApplication
    syntax PrimitiveApplication ::= Primitive Primitive PrimitiveArgs
    syntax PrimitiveArgs ::= List{PrimitiveArg, ""}
    ```

5.  Sequences of nodes surrounded by curly braces (`{` and `}`) and
    delimited by semi-colons (`;`) where the last element may be
    optionally followed by a semi-colon

    ```k
    syntax SequenceNode ::= "{" MichelineNodes "}"
    syntax SequenceNode ::= "{" MichelineNodes ";" "}"
    syntax MichelineNodes ::= List{MichelineNode, ";"}
    ```

## Primitive Arguments

A primtive argument is just like a Micheline node (with two exceptions). With
respect to literals and sequences, they are identical.

```k
  syntax PrimitiveArg ::= Int
                        | String
                        | MichelineBytes
                        | SequenceNode
```

Here we see there are two distinctions:
1. Non-empty primtive applications must be wrapped in parentheses;
2. Primitive arguments include a new syntactic category called _annotations_.

```k
  syntax PrimitiveArg ::= Primitive
                        | "(" PrimitiveApplication ")"
                        | Annotation
```

### Micheline Annotations

An annotation is essentially a piece of metadata attached to a primitive
application. They are defined as a special character (`@`, `:`, `$`, `&`, `%`,
`!`, `?`) followed by any number of alphanumeric characters, underscores (`_`),
periods (`.`), percent signs (`%`), and at-signs (`@`).

```k
  syntax AnnotationToken
  syntax Annotation ::= AnnotationToken // [klabel(AnnotationToInternal), symbol, function, avoid]
```

This concludes the definition of the Micheline data serialization format.

```k
endmodule
```

```k
module MICHELINE-SYNTAX
  imports MICHELINE-COMMON-SYNTAX

  syntax BytesToken      ::= r"0x([0-9a-fA-F]{2})*"       [token]
  syntax AnnotationToken ::= r"[@:$&%!?][_0-9a-zA-Z.%@]*" [token]
  syntax PrimitiveToken  ::= r"[a-zA-Z_0-9]+"             [token]
endmodule
```

```k
module MICHELINE-INTERNAL-SYNTAX
  imports MICHELINE-COMMON-SYNTAX
  imports BYTES

  syntax MichelineBytes   ::= Bytes
  syntax Annotation       ::= Annot( String )
  syntax PrimitiveNode    ::= Prim( String, PrimitiveArgs )


  syntax MichelineNodes ::= NodesToInternal ( MichelineNodes )
  syntax MichelineNode  ::= NodeToInternal  ( MichelineNode )
  syntax PrimitiveArgs  ::= ArgsToInternal  ( PrimitiveArgs )
  syntax PrimitiveArg   ::= ArgToInternal   ( PrimitiveArg )

  // configuration <k> NodeToInternal($PGM:MichelineNode) </k>
  configuration <k> $PGM:MichelineNode </k>
endmodule
```

## Micheline Surface Syntax to K-Internal Syntax

The K framework provides builtins for various operations including integers,
strings, bytes, maps, etc... The issue is: different languages may use a
notation for their builtin types that differs from K's notation. When that
happens, we prefer to convert the surface language syntax into a K builtin
representation for code reuse and performance reasons.

The internal syntax should be separated into its own module so that we can
use the surface syntax for our parser and import the separate internal
syntax module into our semantic definitions.

In this case, since Micheline is just a data format with no builtin notion
of semantic evaluation, we prefer to drop the `-SYNTAX` postfix, and in
the tradition of other K data structure modules, name the module after the
data structure which it defines. This module's only responsibility will be
to convert the surface representation into the K internal representation.

Here we import our syntax module as well as builtin depdencies.

```k
module MICHELINE-PARSER
  imports MICHELINE-SYNTAX
  imports MICHELINE-INTERNAL-SYNTAX
```

<!--
```k
  rule `BytesToInternal`(B)      => ConvertBytesAux(BytesTokenToString(B))
  rule `AnnotationToInternal`(A) => Annot(AnnotTokenToString(A))
  rule `PrimitiveToInternal`(P)  => Prim(PrimTokenToString(P))
```
-->

```k
  syntax String ::= BytesTokenToString ( BytesToken )      [function, functional, hook(STRING.token2string)]
  syntax String ::= AnnotTokenToString ( AnnotationToken ) [function, functional, hook(STRING.token2string)]
  syntax String ::= PrimTokenToString ( PrimitiveToken )   [function, functional, hook(STRING.token2string)]
```

```k
  rule NodeToInternal( I:Int ) => I
  rule NodeToInternal( S:String ) => S
  rule NodeToInternal( B:BytesToken ) => ConvertBytesAux( BytesTokenToString( B ) )
  rule NodeToInternal( P:PrimitiveToken ) => Prim( PrimTokenToString( P ), .PrimitiveArgs )
  rule NodeToInternal( P:PrimitiveToken P':PrimitiveToken Args:PrimitiveArgs)
    => Prim( PrimTokenToString( P ), ArgsToInternal( P' Args ))
  rule NodeToInternal( { Nodes:MichelineNodes ; } ) => { NodesToInternal(Nodes) }
  rule NodeToInternal( { Nodes:MichelineNodes   } ) => { NodesToInternal(Nodes) }

  rule NodesToInternal( Node:MichelineNode ; Nodes:MichelineNodes ) => NodeToInternal( Node ) ; NodesToInternal( Nodes )
  rule NodesToInternal( .MichelineNodes )                           => .MichelineNodes

  rule ArgsToInternal( P:PrimitiveArg Rest:PrimitiveArgs ) => ArgToInternal( P ) ArgsToInternal( Rest )
  rule ArgsToInternal( .PrimitiveArgs )                    => .PrimitiveArgs

  rule ArgToInternal( I:Int ) => I
  rule ArgToInternal( S:String ) => S
  rule ArgToInternal( B:BytesToken ) => NodeToInternal( B )
  rule ArgToInternal( S:SequenceNode ) => NodeToInternal( S )
  rule ArgToInternal( P:PrimitiveToken ) => NodeToInternal( P )
  rule ArgToInternal( ( App:PrimitiveApplication ) ) => NodeToInternal( App )
  rule ArgToInternal( A:AnnotationToken ) => Annot( AnnotTokenToString( A ) )
```

```k
  syntax Bytes ::= ConvertBytesAux ( String ) [function]
  rule ConvertBytesAux(ByteStr) =>
         Int2Bytes(((lengthString(ByteStr) -Int 2) /Int 2),           // byte sequence length
                   String2Base(                                       // integer to convert
                     substrString(ByteStr, 2, lengthString(ByteStr)),
                     16),
                   BE)                                                // integer is big-endian
```

```k
endmodule
```

```k
module MICHELINE
  imports MICHELINE-PARSER
endmodule
```

## From Micheline to Michelson

The Michelson language can be viewed as schema applied to Micheline where:

-   Micheline primitives are used for types (e.g. `int`) and instructions
    (e.g. `ADD`)
-   Micheline sequences describe instruction blocks (e.g. `{ ADD ; MUL }`)
-   Primitive applications are used to define:
    1. type constructors (`option`, `list`, `set`, `map`, `big_map`)
    2. compound instructions (e.g. `IF <true-branch> <false-branch>`)
-   Annotations are used as type and value decorators
-   Only expressions that satisfy a typing relation are permitted.

Given their similarity, one possible approach to parse Michelson is to first
parse using the program as a Micheline expression and then apply a type
checking process. However, for efficiency reasons, we choose to parse Michelson
using a dedicated syntax.

<!--
```k
module UNPARSED-MICHELSON-SYNTAX
  imports EXTERNAL-MICHELINE-SYNTAX
```

To define the Michelson syntax proper, we restrict the type of possible
primitives to those shown below.

### Instructions

```k
  syntax Primitive ::= InstructionPrimitive
  syntax InstructionPrimitive ::= "DROP"
                                | "DROP"
                                | "DIG"
                                | "DUG"
                                | "DUP"
                                | "SWAP"
                                | "PUSH"
                                | "SOME"
                                | "NONE"
                                | "UNIT"
                                | "IF_NONE"
                                | "PAIR"
                                | "UNPAIR"
                                | "CAR"
                                | "CDR"
                                | "LEFT"
                                | "RIGHT"
                                | "IF_LEFT"
                                | "NIL"
                                | "CONS"
                                | "IF_CONS"
                                | "SIZE"
                                | "EMPTY_SET"
                                | "EMPTY_MAP"
                                | "EMPTY_BIG_MAP"
                                | "MAP"
                                | "ITER"
                                | "MEM"
                                | "GET"
                                | "UPDATE"
                                | "IF"
                                | "LOOP"
                                | "LOOP_LEFT"
                                | "LAMBDA"
                                | "EXEC"
                                | "APPLY"
                                | "DIP"
                                | "DIP"
                                | "FAILWITH"
                                | "CAST"
                                | "RENAME"
                                | "CONCAT"
                                | "SLICE"
                                | "PACK"
                                | "UNPACK"
                                | "ADD"
                                | "SUB"
                                | "MUL"
                                | "EDIV"
                                | "ABS"
                                | "ISNAT"
                                | "INT"
                                | "NEG"
                                | "LSL"
                                | "LSR"
                                | "OR"
                                | "AND"
                                | "XOR"
                                | "NOT"
                                | "COMPARE"
                                | "EQ"
                                | "NEQ"
                                | "LT"
                                | "GT"
                                | "LE"
                                | "GE"
                                | "SELF"
                                | "CONTRACT"
                                | "TRANSFER_TOKENS"
                                | "SET_DELEGATE"
                                | "CREATE_ACCOUNT"
                                | "IMPLICIT_ACCOUNT"
                                | "NOW"
                                | "CHAIN_ID"
                                | "AMOUNT"
                                | "BALANCE"
                                | "CHECK_SIGNATURE"
                                | "BLAKE2B"
                                | "SHA256"
                                | "SHA512"
                                | "HASH_KEY"
                                | "STEPS_TO_QUOTA"
                                | "SOURCE"
                                | "SENDER"
                                | "ADDRESS"
                                | "CREATE_CONTRACT"
```

### Types

```k
syntax Primitive ::= TypePrimitive
syntax TypePrimitive ::= "int"
                       | "nat"
                       | "string"
                       | "bytes"
                       | "mutez"
                       | "bool"
                       | "key_hash"
                       | "timestamp"
                       | "address"
                       | "key"
                       | "unit"
                       | "signature"
                       | "operation"
                       | "chain_id"
                       | "pair"
                       | "option"
                       | "list"
                       | "set"
                       | "contract"
                       | "or"
                       | "lambda"
                       | "map"
                       | "big_map"
```

### Script Fields

```k
syntax Primitive ::= ScriptFieldPrimitive
syntax ScriptFieldPrimitive ::= "parameter"
                              | "storage"
                              | "code"
```

### Michelson Annotations

```k
syntax MichelineAnnotation ::= FieldAnnotation    [klabel(FieldAnnotCtor), symbol, function, avoid]
                             | TypeAnnotation     [klabel(TypeAnnotCtor),  symbol, function, avoid]
                             | VariableAnnotation [klabel(VarAnnotCtor),   symbol, function, avoid]
syntax FieldAnnotation    ::= r"%(@|[_a-zA-Z][_0-9a-zA-Z\\.]*)?"    [token]
syntax TypeAnnotation     ::= r":([_a-zA-Z][_0-9a-zA-Z\\.]*)?"      [token]
syntax VariableAnnotation ::= r"@(%|%%|[_a-zA-Z][_0-9a-zA-Z\\.]*)?" [token]
```

```k
endmodule
```

### Putting it All Together

```k
module UNPARSED-MICHELSON
  imports MICHELINE
  imports UNPARSED-MICHELSON-SYNTAX
  imports MICHELSON-INTERNAL-REPRESENTATION

  rule `FieldAnnotCtor`(A) => FieldAnnot(MichelineAnnotToString(A))
  rule `TypeAnnotCtor`(A)  => TypeAnnot(MichelineAnnotToString(A))
  rule `VarAnnotCtor`(A)   => VarAnnot(MichelineAnnotToString(A))
endmodule
```

```k
module MICHELSON-INTERNAL-REPRESENTATION
  imports STRING

  syntax FieldAnnotation    ::= FieldAnnot(String)
  syntax TypeAnnotation     ::= TypeAnnot(String)
  syntax VariableAnnotation ::= VarAnnot(String)
endmodule
```

This concludes the syntactic definition of Michelson programs.

## Parser Limitations

This parser implements the full Micheline specification with the exception of
its indentation rules for primtive applications and sequences. Thus, strictly
speaking, this parser accepts a superset of all Micheline terms.
-->
