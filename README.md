First, install ninja-build, and then run 

git submodule update --init

This will clone and build K and kninja/, once that completes, ./build.py should build the semantics and run the unit tests.  From there

./michelson.py run $FILE will run a unit test of your choice.  Alternatively,

kompile --backend llvm michelson.k

Will compile only the main semantics, excluding the tests.  Then krun will work on the appropriate file:

krun --parser ./parser.sh $FILE.tz

Parser presently removes single line comments and annotations (annotation support is coming soon).

Credits: KNinja from nishantjr
