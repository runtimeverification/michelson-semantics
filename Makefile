# Settings
# --------

DEPS_DIR      := ext
LIB_DIR       := lib
BUILD_DIR     := .build
SUBDEFN_DIR   := .
DEFN_BASE_DIR := $(BUILD_DIR)/defn
DEFN_DIR      := $(DEFN_BASE_DIR)/$(SUBDEFN_DIR)
BUILD_LOCAL   := $(abspath $(BUILD_DIR)/local)
LOCAL_LIB     := $(BUILD_LOCAL)/lib

K_SUBMODULE     := $(DEPS_DIR)/k
TEZOS_SUBMODULE := $(DEPS_DIR)/tezos

ifneq (,$(wildcard $(K_SUBMODULE)/k-distribution/target/release/k/bin/*))
    K_RELEASE ?= $(abspath $(K_SUBMODULE)/k-distribution/target/release/k)
else
    K_RELEASE ?= $(dir $(shell which kompile))..
endif
K_BIN := $(K_RELEASE)/bin
K_LIB := $(K_RELEASE)/lib/kframework
export K_RELEASE

LIBRARY_PATH       := $(LOCAL_LIB)
C_INCLUDE_PATH     += :$(BUILD_LOCAL)/include
CPLUS_INCLUDE_PATH += :$(BUILD_LOCAL)/include
ifeq ($(shell uname -s),Darwin) # on OSX add brew include path
    C_INCLUDE_PATH     += :/usr/local/include
    CPLUS_INCLUDE_PATH += :/usr/local/include
endif
PATH               := $(K_BIN):$(TEZOS_SUBMODULE):$(PATH)
PYTHONPATH         := $(K_LIB):$(PYTHONPATH)

export LIBRARY_PATH
export C_INCLUDE_PATH
export CPLUS_INCLUDE_PATH
export PATH
export PYTHONPATH

# if K_OPTS is undefined, up the default heap size for kompile
ifeq "$(origin K_OPTS)" "undefined"
K_OPTS := -Xmx8G
endif
export K_OPTS

.PHONY: all clean distclean clean-tests                                                             \
        deps deps-k deps-tezos                                                                      \
        defn defn-k defn-compat                                                                     \
        defn-llvm defn-prove defn-symbolic                                                          \
        defn-contract-expander defn-extractor defn-input-creator defn-output-compare                \
        build build-k build-compat                                                                  \
        build-llvm build-prove build-symbolic build-dexter                                          \
        build-contract-expander build-extractor build-input-creator build-output-compare            \
        test test-unit test-unit-failing test-cross test-cross-faling                               \
        test-prove test-prove-failing test-symbolic test-symbolic-failing                           \
        dexter-prove

.SECONDARY:

all: build

clean-tests:
	git clean -dffx -- tests
	rm -rf run-tezos.timestamp fix-address.timestamp

clean: clean-tests
	rm -rf $(DEFN_BASE_DIR)

distclean: clean
	rm -rf $(BUILD_DIR)

# Dependencies
# ------------

K_JAR := $(K_SUBMODULE)/k-distribution/target/release/k/lib/java/kernel-1.0-SNAPSHOT.jar

deps: deps-k deps-tezos
deps-k: $(K_JAR)

$(TEZOS_SUBMODULE)/make.timestamp:
	./build-deps-tezos.sh
	touch $@

ifneq ($(RELEASE),)
    K_BUILD_TYPE         := FastBuild
    SEMANTICS_BUILD_TYPE := Release
else
    K_BUILD_TYPE         := FastBuild
    SEMANTICS_BUILD_TYPE := Debug
endif

$(K_JAR):
	cd $(K_SUBMODULE) && mvn package -DskipTests -U -Dproject.build.type=$(K_BUILD_TYPE)

deps-tezos: $(TEZOS_SUBMODULE)/make.timestamp

# Building
# --------

SOURCE_FILES       := compat    \
                      common    \
                      michelson \
                      syntax    \
                      types
EXTRA_SOURCE_FILES :=
ALL_FILES          := $(patsubst %, %.md, $(SOURCE_FILES) $(EXTRA_SOURCE_FILES))

tangle_haskell := k | symbolic
tangle_llvm    := k | concrete

HOOK_NAMESPACES := TIME

# NOTE: -W useless-rule check is quite expensive (increasing compilation times by several times), use -Wno useless-rule unless this check is needed!!!
KOMPILE_OPTS += --hook-namespaces "$(HOOK_NAMESPACES)" --gen-bison-parser --emit-json -w all -Wno unused-symbol -Wno useless-rule

ifneq (,$(RELEASE))
    KOMPILE_OPTS += -O3
endif

CPP_FILES := hooks/time.cpp

LLVM_KOMPILE_OPTS += -L$(LOCAL_LIB) -I$(K_RELEASE)/include/kllvm \
                     $(abspath $(CPP_FILES))                     \
                     -std=c++14

ifeq (,$(RELEASE))
    LLVM_KOMPILE_OPTS += -g
endif

KOMPILE_LLVM = kompile --debug --backend llvm --md-selector "$(tangle_llvm)" \
               $(KOMPILE_OPTS)                                               \
               $(addprefix -ccopt ,$(LLVM_KOMPILE_OPTS))

HASKELL_KOMPILE_OPTS +=

KOMPILE_HASKELL = kompile --debug --backend haskell --md-selector "$(tangle_haskell)" \
                  $(KOMPILE_OPTS)                                                     \
                  $(HASKELL_KOMPILE_OPTS)

defn:        defn-k defn-compat
defn-k:      defn-llvm defn-prove defn-symbolic
defn-compat: defn-contract-expander defn-extractor defn-input-creator defn-output-compare

build:        build-k build-compat
build-k:      build-llvm build-prove build-symbolic
build-compat: build-contract-expander build-extractor build-input-creator build-output-compare

# LLVM

llvm_dir           := $(DEFN_DIR)/llvm
llvm_files         := $(ALL_FILES)
llvm_main_file     := michelson
llvm_main_module   := MICHELSON
llvm_syntax_module := UNIT-TEST-SYNTAX
llvm_kompiled      := $(llvm_dir)/$(notdir $(llvm_main_file))-kompiled/interpreter

defn-llvm:  $(llvm_files)
build-llvm: $(llvm_kompiled)

$(llvm_kompiled): $(llvm_files)
	$(KOMPILE_LLVM) $(llvm_main_file).md                  \
	                --directory $(llvm_dir) -I $(CURDIR)  \
	                --main-module $(llvm_main_module)     \
	                --syntax-module $(llvm_syntax_module)

### Prove

prove_dir           := $(DEFN_DIR)/prove
prove_files         := $(ALL_FILES) pretty-syntax.md
prove_main_file     := michelson
prove_main_module   := MICHELSON
prove_syntax_module := UNIT-TEST-SYNTAX
prove_kompiled      := $(prove_dir)/$(notdir $(prove_main_file))-kompiled/definition.kore

defn-prove:  $(prove_files)
build-prove: $(prove_kompiled)

$(prove_kompiled): $(prove_files)
	$(KOMPILE_HASKELL) $(prove_main_file).md                  \
	                   --directory $(prove_dir) -I $(CURDIR)  \
	                   --main-module $(prove_main_module)     \
	                   --syntax-module $(prove_syntax_module)

### Dexter
dexter_dir           := $(DEFN_DIR)/dexter
dexter_main_file     := dexter
dexter_main_file_loc := tests/proofs/dexter/$(dexter_main_file)
dexter_files         := $(prove_files) $(dexter_main_file_loc).md tests/proofs/dexter/dexter-compiled.md
dexter_main_module   := DEXTER-VERIFICATION
dexter_syntax_module := DEXTER-VERIFICATION-SYNTAX
dexter_kompiled      := $(dexter_dir)/$(notdir $(dexter_main_file))-kompiled/definition.kore

defn-dexter:  $(dexter_files)
build-dexter: $(dexter_kompiled)

$(dexter_kompiled): $(dexter_files)
	$(KOMPILE_HASKELL) $(dexter_main_file_loc).md              \
	                   --directory $(dexter_dir) -I $(CURDIR)  \
	                   --main-module $(dexter_main_module)     \
	                   --syntax-module $(dexter_syntax_module)

### Symbolic Driver

driver_dir            := $(DEFN_DIR)/driver
driver_files          := lib/driver.md
driver_main_file      := lib/driver
driver_main_module    := DRIVER
driver_syntax_module  := KORE
driver_kompiled       := $(driver_dir)/$(notdir $(driver_main_file))-kompiled/interpreter
driver_pattern_parser := $(driver_dir)/$(notdir $(driver_main_file))-kompiled/parser_Pattern

defn-driver:  $(driver_files)
build-driver: $(driver_kompiled) $(driver_pattern_parser)

$(driver_kompiled): tangle_haskell := k | driver
$(driver_kompiled): $(driver_files)
	$(KOMPILE_LLVM) $(driver_main_file).md                  \
	                --directory $(driver_dir) -I $(CURDIR)  \
	                --main-module $(driver_main_module)     \
	                --syntax-module $(driver_syntax_module)

$(driver_pattern_parser): $(driver_files) $(driver_kompiled)
	kast --gen-parser --directory $(driver_dir) --sort Pattern $@

### Symbolic

symbolic_dir           := $(DEFN_DIR)/symbolic
symbolic_files         := $(ALL_FILES)
symbolic_main_file     := michelson
symbolic_main_module   := MICHELSON
symbolic_syntax_module := SYMBOLIC-UNIT-TEST-SYNTAX
symbolic_kompiled      := $(symbolic_dir)/$(notdir $(symbolic_main_file))-kompiled/definition.kore

defn-symbolic:  $(symbolic_files)
build-symbolic: $(symbolic_kompiled) build-driver

$(symbolic_kompiled): tangle_haskell := k | symbolic | internalized-rl
$(symbolic_kompiled): $(symbolic_files)
	$(KOMPILE_HASKELL) $(symbolic_main_file).md                  \
	                   --directory $(symbolic_dir) -I $(CURDIR)  \
	                   --main-module $(symbolic_main_module)     \
	                   --syntax-module $(symbolic_syntax_module)

# Compat Contract Expander

contract_expander_dir           := $(DEFN_DIR)/contract-expander
contract_expander_files         := $(ALL_FILES)
contract_expander_main_file     := compat
contract_expander_main_module   := CONTRACT-EXPANDER
contract_expander_syntax_module := $(contract_expander_main_module)-SYNTAX
contract_expander_kompiled      := $(contract_expander_dir)/$(notdir $(contract_expander_main_file))-kompiled/interpreter

defn-contract-expander:  $(contract_expander_files)
build-contract-expander: $(contract_expander_kompiled)

$(contract_expander_kompiled): $(contract_expander_files)
	$(KOMPILE_LLVM) $(contract_expander_main_file).md                  \
	                --directory $(contract_expander_dir) -I $(CURDIR)  \
	                --main-module $(contract_expander_main_module)     \
	                --syntax-module $(contract_expander_syntax_module)

# Compat Extractor

extractor_dir           := $(DEFN_DIR)/extractor
extractor_files         := $(ALL_FILES)
extractor_main_file     := compat
extractor_main_module   := EXTRACTOR
extractor_syntax_module := $(extractor_main_module)-SYNTAX
extractor_kompiled      := $(extractor_dir)/$(notdir $(extractor_main_file))-kompiled/interpreter

defn-extractor:  $(extractor_files)
build-extractor: $(extractor_kompiled)

$(extractor_kompiled): $(extractor_files)
	$(KOMPILE_LLVM) $(extractor_main_file).md                  \
	                --directory $(extractor_dir) -I $(CURDIR)  \
	                --main-module $(extractor_main_module)     \
	                --syntax-module $(extractor_syntax_module)

# Compat Input Creator

input_creator_dir           := $(DEFN_DIR)/input-creator
input_creator_files         := $(ALL_FILES)
input_creator_main_file     := compat
input_creator_main_module   := INPUT-CREATOR
input_creator_syntax_module := $(input_creator_main_module)-SYNTAX
input_creator_kompiled      := $(input_creator_dir)/$(notdir $(input_creator_main_file))-kompiled/interpreter

defn-input-creator:  $(input_creator_files)
build-input-creator: $(input_creator_kompiled)

$(input_creator_kompiled): $(input_creator_files)
	$(KOMPILE_LLVM) $(input_creator_main_file).md                  \
	                --directory $(input_creator_dir) -I $(CURDIR)  \
	                --main-module $(input_creator_main_module)     \
	                --syntax-module $(input_creator_syntax_module)

# Compat Output Compare

output_compare_dir           := $(DEFN_DIR)/output-compare
output_compare_files         := $(ALL_FILES)
output_compare_main_file     := compat
output_compare_main_module   := OUTPUT-COMPARE
output_compare_syntax_module := $(output_compare_main_module)-SYNTAX
output_compare_kompiled      := $(output_compare_dir)/$(notdir $(output_compare_main_file))-kompiled/interpreter

defn-output-compare:  $(output_compare_files)
build-output-compare: $(output_compare_kompiled)

$(output_compare_kompiled): $(output_compare_files)
	$(KOMPILE_LLVM) $(output_compare_main_file).md                  \
	                --directory $(output_compare_dir) -I $(CURDIR)  \
	                --main-module $(output_compare_main_module)     \
	                --syntax-module $(output_compare_syntax_module)

# Tests
# -----

TEST  := ./kmich
CHECK := git --no-pager diff --no-index --ignore-all-space -R

KPROVE_MODULE := VERIFICATION
KPROVE_OPTIONS :=

test: test-unit test-cross test-prove

# Unit

unit_tests         := $(wildcard tests/unit/*.tzt) $(wildcard tests/macros/*.tzt) $(wildcard tests/multi/*.tzt)
unit_tests_failing := $(shell cat tests/failing.unit)
unit_tests_passing := $(filter-out $(unit_tests_failing), $(unit_tests))

test-unit:         $(unit_tests_passing:=.unit)
test-unit-failing: $(unit_tests_failing:=.unit)

tests/%.unit: tests/% $(llvm_kompiled)
	$(TEST) interpret --backend llvm $< --output-file /dev/null

# symbolic

symbolic_tests         := $(shell find tests/symbolic -type f -name '*.tzt')
symbolic_tests_failing := $(shell cat tests/failing.symbolic)
symbolic_tests_passing := $(filter-out $(symbolic_tests_failing), $(symbolic_tests))

test-symbolic:         $(symbolic_tests_passing:=.symbolic)
test-symbolic-failing: $(symbolic_tests_failing:=.symbolic)

EXPECTED_EXITCODE = 0
tests/symbolic/%.fail.tzt.symbolic:  EXPECTED_EXITCODE = 1
tests/symbolic/%.stuck.tzt.symbolic: EXPECTED_EXITCODE = 2

tests/%.symbolic: tests/% build-symbolic
	$(LIB_DIR)/check-exit-code $(EXPECTED_EXITCODE) $(TEST) symbtest $< > /dev/null

# Cross Validation

cross_tests         := $(wildcard tests/unit/*.tzt) $(wildcard tests/macros/*.tzt)
cross_tests_failing := $(shell cat tests/failing.cross)
cross_tests_passing := $(filter-out $(cross_tests_failing), $(cross_tests))

test-cross:         $(cross_tests_passing:=.cross)
test-cross-failing: $(cross_tests_failing:=.cross)

tests/%.cross: tests/% $(input_creator_kompiled) $(extractor_kompiled) $(contract_expander_kompiled) $(output_compare_kompiled)
	$(LIB_DIR)/tezos-client-unit-test $< > $@

# Prove

prove_tests         := $(wildcard tests/proofs/*-spec.md) $(wildcard tests/proofs/*-spec.k)
prove_tests_failing := $(shell cat tests/failing.prove)
prove_tests_passing := $(filter-out $(prove_tests_failing), $(prove_tests))

tests/proofs/multisig-spec.md.prove: export KORE_EXEC_OPTS=--smt-timeout 10000

test-prove:         $(prove_tests_passing:=.prove)
test-prove-failing: $(prove_tests_failing:=.prove)

tests/%.prove: tests/% $(prove_kompiled)
	$(TEST) prove --backend prove $< $(KPROVE_MODULE) $(KPROVE_OPTIONS)

# Dexter proofs

dexter_spec_modules = DEXTER-SPEC                          \
					  DEXTER-TOKENTOXTZ-SPEC               \
                      DEXTER-ADDLIQUIDITY-NEGATIVE-SPEC    \
                      DEXTER-ADDLIQUIDITY-POSITIVE-SPEC    \
                      DEXTER-DEFAULT-SPEC                  \
                      DEXTER-REMOVELIQUIDITY-NEGATIVE-SPEC \
                      DEXTER-REMOVELIQUIDITY-POSITIVE-SPEC \
                      DEXTER-REMOVELIQUIDITY-SPEC          \
                      DEXTER-SETBAKER-SPEC                 \
                      DEXTER-SETLQTADDRESS-SPEC            \
                      DEXTER-SETMANAGER-SPEC               \
                      DEXTER-UPDATETOKENPOOL-SPEC          \

dexter_spec_file := tests/proofs/dexter/dexter-spec.md

dexter-prove: $(dexter_spec_modules:%=dexter-prove_%)

dexter-prove_%:
	$(MAKE) $(dexter_spec_file).dexter_prove \
  KPROVE_MODULE=DEXTER-VERIFICATION        \
  KPROVE_OPTIONS="$(KPROVE_OPTIONS) --spec-module $*"

tests/%.dexter_prove: tests/% $(dexter_kompiled)
	$(TEST) prove --backend prove --backend-dir $(dexter_dir) $< $(KPROVE_MODULE) $(KPROVE_OPTIONS)
