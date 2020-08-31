K-Michelson Semantics
=====================

Getting Started
---------------

See the [user guide](USER_GUIDE.md) for installation and usage instructions.

Project Structure
-----------------

The project has a very simple layout. Source files live at the project root:

- [syntax.md](./syntax.md) specifies the syntax of Michelson and other input
  data

- [common.md](./common.md) specifies common infrastructure used in the type
  checker and the core semantics

- [types.md](./types.md) contains a rudimentary type-checker for Michelson

- [michelson.md](./michelson.md) specifies:

  * the configuration (state representation) of Michelson programs (module
    `MICHELSON-CONFIGURATION`)
  * the core semantic rules of Michelson as well as semantic extensions to
    support unit testing (module `MICHELSON`)

- [compat.md](./compat.md) is a compatability layer between KMichelson and the
  Tezos Reference client used for doing cross-validation between the two.

Other directories include:

- `/ext` - Git submodules for our direct dependencies
- `/hooks` - C++ definitions which implement some low-level operations
- `/lib` - scripts used by the test harness
- `/media` - source code for presentations about K-Michelson
- `/tests` - source code for all tests

See the tests [README](./tests/README.md) for more information on test formats
and organization.
