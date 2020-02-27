# K-Michelson Semantics

## Building

First, initialize the submodules using

    git submodule update --init --recursive

This will check out a copy of k in the `ext/k` subdirectory.  As a first time setup step, cd into this directory and execute `mvn package` to build k.  Then, `source common.sh` in the main directory to add the copy of k to the PATH variable.

Afterwards, simply executing 

    ./build.sh 

This script can be found in the top level repository directory, and will build the semantics for the llvm backend of K.  

After building, executing `./run-tests.sh` will run the unit test suite.  Presently, only building for the llvm backend is automated through this script, however building for the haskell backend is also possible through the command

    kompile --backend haskell unit-test.k

However, this will disable certain llvm-specific hooks, such as timestamp parsing.

The OCaml and Java backends are not presently officially supported.

After building, one can use the `./run.sh` script to pass an appropriate file to the semantics for execution.
    

## Using the semantics

The semantics accept Michelson contracts or unit tests in a variant of the format discussed [here](https://gitlab.com/tezos/tezos/-/merge_requests/1487/diffs).  The primary change can be seen in the addition of the `contract`, `parameter_value` and `storage_value` primitive applications, which capture the remaining initial state of a Michelson contract execution.  Either these three primitive applications, or the `code`, `input` and `output` applications detailed in the Unit Test syntax must be present in any input file for the semantics.  The former results in a full contract execution, the latter in the execution of a small snippet.

As an example, here is a contract input file implementing a sum-to-n program:

    parameter_value 300000 ;
    storage_value 0 ;
    contract { 
      storage nat ; 
      parameter nat ; 
      code { LEFT nat ; 
             LOOP_LEFT { DUP ;
                         DIP { CDR } ;
                         CAR ;
                         DUP ;
                         DIP { ADD } ;
                         PUSH nat 1 ;
                         SWAP ; 
                         SUB ;
                         ISNAT ;
                         IF_NONE { RIGHT (pair nat nat) } { PAIR ; LEFT nat } } ; NIL operation; PAIR } }

This contract computes the sum of 1 to its parameter value, plus its storage value, and places the result in its storage.

As an example of a unit test format file, here is a test for the `DIG` instruction:


    code { DIG 1 } ; 
    input { Stack_elt int 1 ; Stack_elt int 2 ; Stack_elt int 3 ; Stack_elt int 4 ; Stack_elt int 5 ; Stack_elt int 6 } ;
    output { Stack_elt int 2 ; Stack_elt int 1 ; Stack_elt int 3 ; Stack_elt int 4 ; Stack_elt int 5 ; Stack_elt int 6 }

Note that the unit test format allows the user to specify an entire input and output stack, rather than using the normal michelson parameter/storage system.

## Project Structure

`michelson-syntax.k` - This file contains the specification for the syntax of a Michelson contract and the other input data.

`michelson-config.k` - This file describes the template state of a michelson contract.

`michelson-common.k` - This file creates most of the K-Michelson internal datatypes.

`michelson.k` - This file specifies the semantics of the Michelson language as rewrite rules over the syntax, configuration and datatypes defined in the previous files.

`unit-test.k` and `unit-test-syntax.k` extend the semantics and syntax of the Michelson language to include unit testing facilities, such as the ability to specify an initial and final stack, and to check that the final stack matches the expected result.

`time.cpp` - This file implements a hook on the LLVM backend to perform translation from an ISO-8601 timestamp human readable timestamp to a unix timestamp, used by the K semantics internally.

`tests/unit` contains the unit testing suite used to verify the correctnes of the semantics.

`tests/contracts` contains complete contracts used for miscellaneous purposes, such as performance testing.

`tests/obsolete` contains contracts in obsolete formats no longer accepted by the current semantics that have not yet been translated.

`tests/proofs` contains verification specs - TODO: These have not been updated for the new configuration yet.
