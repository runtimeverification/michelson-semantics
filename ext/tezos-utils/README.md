# Tezos Utils

This is a collection of utilities for working with Tezos which exists as a CLI wrapper around pytezos.
Currently, the following tools are included:

-   convert - converts Michelson contract literals/expressions between different formats

    input formats: Michelson, JSON

    output formats: Michelson, JSON, dot

## Installation

This tool has system and Python dependencies.

On Ubuntu Linux derivatives, the system dependencies include:

- libsodium-dev
- libsecp256k1-dev
- libgmp-dev
- graphviz

For other distributions, you will need to find the appropriate packages.

The Python dependencies are all available available via PyPI packages and can be installed via `pip`:

-   click
-   graphviz
-   pytezos (known to work with version 3.2.11)

## CFG Generation

Using the dot output generator, one can build a CFG for a Michelson script.
By way of example, we show one of the smallest well-typed Michelson contracts possible:

```
parameter unit ; storage unit ; code { DROP ; UNIT ; NIL operation ; PAIR }
```

Note that the contract above exists in file `tests/unit.tz`.
We can draw a CFG for this by doing the following (using the `dot` tool from the `graphviz` toolkit):

```
tezos-utils convert -i michelson -o dot tests/unit.tz unit.dot
dot -Tpng unit.dot > unit.png
```

The first command reads file `unit.tz` to produce `unit.dot`.
The second command renders the DOT file into a nice png image.

We can shorten the invocation to a single command using the special file name `-` which represents stdin/stdout:

```
tezos-utils convert -i michelson -o dot tests/unit.tz - | dot -Tpng > unit.png
```

We can also draw CFGs for arbitrary Michelson code fragments, e.g. suppose we have:

```
IF { PUSH int 5 } { PUSH int -3 } ; ADD
```

stored in the file `tests/if.tz`.
We can render a CFG for it in the exact same way:

```
tezos-utils convert -i michelson -o dot tests/if.tz - | dot -Tpng > if.png
```
