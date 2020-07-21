# Micheline Format

Micheline is a data serialization format similar to JSON or XML. Like other
formats, various sub-languages may be constructed on top of Micheline by
applying a schema which restricts the set of well-formed expressions.

Micheline expressions (called nodes) have an extremely sinmple syntax that we
define below.

```k
module MICHELINE-SYNTAX
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
    syntax MichelineBytes        ::= MichelineBytesLiteral  [klabel(BytesLiteral2Bytes), symbol, function, avoid]
    syntax MichelineBytesLiteral ::= r"0x([0-9a-fA-F]{2})*" [token]
    ```

4.  Primitives applications---which are primtives (unquoted alphanumeric strings
    with underscores) followed by a possibly empty primitive arguemnt list (we
    will come back to define primtive arguments later)

    ```k
    syntax PrimitiveNode ::= Primitive
                           | Primitive PrimitiveArgList
    syntax Primitive ::= r"[a-zA-Z_0-9]+" [token]
    syntax PrimitiveArgList ::= PrimitiveArg
                              | PrimitiveArg PrimitiveArgList
    ```

5.  Sequences of nodes surrounded by curly braces (`{` and `}`) and
    delimited by semi-colons (`;`) where the last element may be
    optionally followed by a semi-colon

    ```k
    syntax SequenceNode ::= "{" MichelineNodes "}"
    syntax MichelineNodes ::= MichelineNode
                            | MichelineNode ";"
                            | MichelineNode ";" MichelineNodes
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
                        | "(" Primitive PrimitiveArgList ")"
                        | MichelineAnnotation
```

### Micheline Annotations

An annotation is essentially a piece of metadata attached to a primitive
application. They are defined as a special character (`@`, `:`, `$`, `&`, `%`,
`!`, `?`) followed by any number of alphanumeric characters, underscores (`_`),
periods (`.`), percent signs (`%`), and at-signs (`@`).

```k
  syntax MichelineAnnotation ::= r"[@:$&%!?][_0-9a-zA-Z.%@]*" [token]
```

This concludes the definition of the Micheline data serialization format.

```k
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
module MICHELINE
  imports MICHELINE-SYNTAX
  imports BYTES
```

We enrich our syntactic categories with the builtin sorts.

```k
  syntax MichelineBytes ::= Bytes
```

K provides a builtin which can convert a token into a string. We will use this
feature to parse LiteralNode tokens into strings and then convert those strings
into the appropriate datatype inside K.

```k
  syntax String ::= BytesLiteral2String ( MichelineBytesLiteral ) [function, functional, hook(STRING.token2string)]
```

We now fulfill our promise to explain the implementation details referenced in
the original `LiteralNode` syntax declaration. The `klabel` attributes are used
to define custom sort injections from `Int/String/BytesNode` into `LiteralNode`
which are actually conversion functions. We define their rules here:

```k
  rule `BytesLiteral2Bytes`(B) => ConvertBytesAux(BytesLiteral2String(B))
```

We use an auxiliary rule to simplify our definition:

```k
  syntax Bytes ::= ConvertBytesAux ( String ) [function]

  rule ConvertBytesAux(ByteStr) =>
         Int2Bytes(((lengthString(ByteStr) -Int 2) /Int 2),           // byte sequence length
                   String2Base(                                       // integer to convert
                     substrString(ByteStr, 2, lengthString(ByteStr)),
                     16),
                   BE)                                                // integer is big-endian
```

This concludes our Micheline definition.

```k
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

## Parser Limitations

This parser implements the full Micheline specification with the exception of
its indentation rules for primtive applications and sequences. Thus, strictly
speaking, this parser accepts a superset of all Micheline terms.
