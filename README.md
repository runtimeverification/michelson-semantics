K-Michelson: A Michelson Semantics
==================================

Introduction
------------

K-Michelson is:

-   a formal, executable, and human-readable semantics of the Michelson
    blockchain programming language using the K framework;

-   a unit test framework for Michelson programs that allows testing program
    behavior at a finer granularity than contract invocation;

-   a formal verification framework for Michelson programs using a slight
    extension of Michelson as an assertion language.

### Goals

-   Serve as a human-readable and executable reference document specifying how
    Michelson programs should operate.

-   Provide tools for testing and verifying Michelson programs operate as
    designed.

### Non-Goals

-   Provide a complete formal model of the Tezos blockchain. In particular, we
    do _not_ model things like: networking and peer-to-peer communication,
    block baking or validation, transaction pools, protocol amendment, etc.

-   Replace existing Tezos CLI tools. Developers should still use
    `tezos-client` for interfacing with Tezos nodes, `tezos-node` for running
    your own nodes, etc.

Getting Started
---------------

See the [user guide](USER_GUIDE.md) for installation and usage instructions.

Project Structure
-----------------

The project has a very simple layout. Source files live at the project root.

-   [syntax.md](./syntax.md) specifies the syntax of Michelson and other input
    data

-   [common.md](./common.md) specifies common infrastructure used in the type
    checker and the core semantics

-   [types.md](./types.md) contains a rudimentary type-checker for Michelson

-   [michelson.md](./michelson.md) specifies:

    * the configuration (state representation) of Michelson programs (module
      `MICHELSON-CONFIG`)
    * the core semantic rules of Michelson as well as semantic extensions to
      support unit testing (module `MICHELSON`)

-   [compat.md](./compat.md) is a compatability layer between K-Michelson and
    the Tezos Reference client used for cross-validation testing

Other directories include:

- `/ext` - Git submodules for our direct dependencies
- `/hooks` - C++ definitions which implement some low-level operations
- `/lib` - scripts used by the test harness
- `/media` - source code for presentations about K-Michelson
- `/tests` - source code for all tests

See the tests [README](./tests/README.md) for more information on test formats
and organization.
