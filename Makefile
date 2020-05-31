# Settings
# --------

DEPS_DIR      := ext
BUILD_DIR     := .build
SUBDEFN_DIR   := .
DEFN_BASE_DIR := $(BUILD_DIR)/defn
DEFN_DIR      := $(DEFN_BASE_DIR)/$(SUBDEFN_DIR)
BUILD_LOCAL   := $(abspath $(BUILD_DIR)/local)
LOCAL_LIB     := $(BUILD_LOCAL)/lib

K_SUBMODULE             := $(DEPS_DIR)/k
TEZOS_SUBMODULE         := $(DEPS_DIR)/tezos
PANDOC_TANGLE_SUBMODULE := $(DEPS_DIR)/pandoc-tangle

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
PATH               := $(K_BIN):$(TEZOS_SUBMODULE):$(PATH)

export LIBRARY_PATH
export C_INCLUDE_PATH
export CPLUS_INCLUDE_PATH
export PATH

.PHONY: all clean distclean clean-tests                                                             \
        deps deps-k deps-tezos deps-tangle                                                          \
        defn defn-llvm                                                                              \
        build build-k build-compat                                                                  \
        build-llvm build-prove build-symbolic                                                       \
        build-contract-expander build-extractor build-input-creator build-output-compare            \
        test test-unit test-unit-failing test-cross test-cross-faling test-prove test-prove-failing
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

deps: deps-k deps-tezos deps-tangle
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

TANGLER  := $(PANDOC_TANGLE_SUBMODULE)/tangle.lua
LUA_PATH := $(PANDOC_TANGLE_SUBMODULE)/?.lua;;
export TANGLER
export LUA_PATH

deps-tangle: $(TANGLER)

# Building
# --------

SOURCE_FILES       := compat                    \
                      michelson-common          \
                      michelson-config          \
                      michelson-internal-syntax \
                      michelson                 \
                      michelson-syntax          \
                      michelson-types           \
                      michelson-unparser        \
                      symbolic-configuration    \
                      symbolic-unit-test        \
                      symbolic-unit-test-syntax \
                      unit-test                 \
                      unit-test-syntax
EXTRA_SOURCE_FILES :=
ALL_FILES          := $(patsubst %, %.k, $(SOURCE_FILES) $(EXTRA_SOURCE_FILES))

tangle_selector := .k

HOOK_NAMESPACES := TIME MICHELSON

KOMPILE_OPTS += --hook-namespaces "$(HOOK_NAMESPACES)" --gen-bison-parser

ifneq (,$(RELEASE))
    KOMPILE_OPTS += -O3
endif

CPP_FILES := hex.cpp time.cpp decode.cpp

LLVM_KOMPILE_OPTS += -L$(LOCAL_LIB) -I$(K_RELEASE)/include/kllvm \
                     $(abspath $(CPP_FILES))                     \
                     -std=c++14

ifeq (,$(RELEASE))
    LLVM_KOMPILE_OPTS += -g
endif

KOMPILE_LLVM := kompile --debug --backend llvm            \
                $(KOMPILE_OPTS)                           \
                $(addprefix -ccopt ,$(LLVM_KOMPILE_OPTS))

HASKELL_KOMPILE_OPTS +=

KOMPILE_HASKELL := kompile --debug --backend haskell \
                   $(KOMPILE_OPTS)                   \
                   $(HASKELL_KOMPILE_OPTS)

defn:        defn-k defn-compat
defn-k:      defn-llvm defn-prove defn-symbolic
defn-compat: defn-contract-expander defn-extractor defn-input-creator defn-output-compare

build:        build-k build-compat
build-k:      build-llvm build-prove build-symbolic
build-compat: build-contract-expander build-extractor build-input-creator build-output-compare

# LLVM

llvm_dir           := $(DEFN_DIR)/llvm
llvm_files         := $(patsubst %, $(llvm_dir)/%, $(ALL_FILES))
llvm_main_file     := unit-test
llvm_main_module   := UNIT-TEST
llvm_syntax_module := $(llvm_main_module)-SYNTAX
llvm_kompiled      := $(llvm_dir)/$(llvm_main_file)-kompiled/interpreter

defn-llvm:  $(llvm_files)
build-llvm: $(llvm_kompiled)

$(llvm_dir)/%.k: %.md $(TANGLER)
	@mkdir -p $(llvm_dir)
	pandoc --from markdown --to "$(TANGLER)" --metadata=code:"$(tangle_selector)" $< > $@

$(llvm_kompiled): $(llvm_files)
	$(KOMPILE_LLVM) $(llvm_dir)/$(llvm_main_file).k        \
	                --directory $(llvm_dir) -I $(llvm_dir) \
	                --main-module $(llvm_main_module)      \
	                --syntax-module $(llvm_syntax_module)

### Prove

prove_dir           := $(DEFN_DIR)/prove
prove_files         := $(patsubst %, $(prove_dir)/%, $(ALL_FILES))
prove_main_file     := unit-test
prove_main_module   := UNIT-TEST
prove_syntax_module := $(prove_main_module)-SYNTAX
prove_kompiled      := $(prove_dir)/$(prove_main_file)-kompiled/definition.kore

defn-prove:  $(prove_files)
build-prove: $(prove_kompiled)

$(prove_dir)/%.k: %.md $(TANGLER)
	@mkdir -p $(prove_dir)
	pandoc --from markdown --to "$(TANGLER)" --metadata=code:"$(tangle_selector)" $< > $@

$(prove_kompiled): $(prove_files)
	$(KOMPILE_HASKELL) $(prove_dir)/$(prove_main_file).k        \
	                   --directory $(prove_dir) -I $(prove_dir) \
	                   --main-module $(prove_main_module)       \
	                   --syntax-module $(prove_syntax_module)

### Symbolic

symbolic_dir           := $(DEFN_DIR)/symbolic
symbolic_files         := $(patsubst %, $(symbolic_dir)/%, $(ALL_FILES))
symbolic_main_file     := symbolic-unit-test
symbolic_main_module   := SYMBOLIC-UNIT-TEST
symbolic_syntax_module := $(symbolic_main_module)-SYNTAX
symbolic_kompiled      := $(symbolic_dir)/$(symbolic_main_file)-kompiled/definition.kore

defn-symbolic:  $(symbolic_files)
build-symbolic: $(symbolic_kompiled)

$(symbolic_dir)/%.k: %.md $(TANGLER)
	@mkdir -p $(symbolic_dir)
	pandoc --from markdown --to "$(TANGLER)" --metadata=code:"$(tangle_selector)" $< > $@

$(symbolic_kompiled): $(symbolic_files)
	$(KOMPILE_HASKELL) $(symbolic_dir)/$(symbolic_main_file).k        \
	                   --directory $(symbolic_dir) -I $(symbolic_dir) \
	                   --main-module $(symbolic_main_module)          \
	                   --syntax-module $(symbolic_syntax_module)

# Compat Contract Expander

contract_expander_dir           := $(DEFN_DIR)/contract-expander
contract_expander_files         := $(patsubst %, $(contract_expander_dir)/%, $(ALL_FILES))
contract_expander_main_file     := compat
contract_expander_main_module   := CONTRACT-EXPANDER
contract_expander_syntax_module := $(contract_expander_main_module)-SYNTAX
contract_expander_kompiled      := $(contract_expander_dir)/$(contract_expander_main_file)-kompiled/interpreter

defn-contract-expander:  $(contract_expander_files)
build-contract-expander: $(contract_expander_kompiled)

$(contract_expander_dir)/%.k: %.md $(TANGLER)
	@mkdir -p $(contract_expander_dir)
	pandoc --from markdown --to "$(TANGLER)" --metadata=code:"$(tangle_selector)" $< > $@

$(contract_expander_kompiled): $(contract_expander_files)
	$(KOMPILE_LLVM) $(contract_expander_dir)/$(contract_expander_main_file).k        \
	                --directory $(contract_expander_dir) -I $(contract_expander_dir) \
	                --main-module $(contract_expander_main_module)                   \
	                --syntax-module $(contract_expander_syntax_module)

# Compat Extractor

extractor_dir           := $(DEFN_DIR)/extractor
extractor_files         := $(patsubst %, $(extractor_dir)/%, $(ALL_FILES))
extractor_main_file     := compat
extractor_main_module   := EXTRACTOR
extractor_syntax_module := $(extractor_main_module)-SYNTAX
extractor_kompiled      := $(extractor_dir)/$(extractor_main_file)-kompiled/interpreter

defn-extractor:  $(extractor_files)
build-extractor: $(extractor_kompiled)

$(extractor_dir)/%.k: %.md $(TANGLER)
	@mkdir -p $(extractor_dir)
	pandoc --from markdown --to "$(TANGLER)" --metadata=code:"$(tangle_selector)" $< > $@

$(extractor_kompiled): $(extractor_files)
	$(KOMPILE_LLVM) $(extractor_dir)/$(extractor_main_file).k        \
	                --directory $(extractor_dir) -I $(extractor_dir) \
	                --main-module $(extractor_main_module)           \
	                --syntax-module $(extractor_syntax_module)

# Compat Input Creator

input_creator_dir           := $(DEFN_DIR)/input-creator
input_creator_files         := $(patsubst %, $(input_creator_dir)/%, $(ALL_FILES))
input_creator_main_file     := compat
input_creator_main_module   := INPUT-CREATOR
input_creator_syntax_module := $(input_creator_main_module)-SYNTAX
input_creator_kompiled      := $(input_creator_dir)/$(input_creator_main_file)-kompiled/interpreter

defn-input-creator:  $(input_creator_files)
build-input-creator: $(input_creator_kompiled)

$(input_creator_dir)/%.k: %.md $(TANGLER)
	@mkdir -p $(input_creator_dir)
	pandoc --from markdown --to "$(TANGLER)" --metadata=code:"$(tangle_selector)" $< > $@

$(input_creator_kompiled): $(input_creator_files)
	$(KOMPILE_LLVM) $(input_creator_dir)/$(input_creator_main_file).k        \
	                --directory $(input_creator_dir) -I $(input_creator_dir) \
	                --main-module $(input_creator_main_module)               \
	                --syntax-module $(input_creator_syntax_module)

# Compat Output Compare

output_compare_dir           := $(DEFN_DIR)/output-compare
output_compare_files         := $(patsubst %, $(output_compare_dir)/%, $(ALL_FILES))
output_compare_main_file     := compat
output_compare_main_module   := OUTPUT-COMPARE
output_compare_syntax_module := $(output_compare_main_module)-SYNTAX
output_compare_kompiled      := $(output_compare_dir)/$(output_compare_main_file)-kompiled/interpreter

defn-output-compare:  $(output_compare_files)
build-output-compare: $(output_compare_kompiled)

$(output_compare_dir)/%.k: %.md $(TANGLER)
	@mkdir -p $(output_compare_dir)
	pandoc --from markdown --to "$(TANGLER)" --metadata=code:"$(tangle_selector)" $< > $@

$(output_compare_kompiled): $(output_compare_files)
	$(KOMPILE_LLVM) $(output_compare_dir)/$(output_compare_main_file).k        \
	                --directory $(output_compare_dir) -I $(output_compare_dir) \
	                --main-module $(output_compare_main_module)                \
	                --syntax-module $(output_compare_syntax_module)

# Tests
# -----

TEST  := ./kmich
CHECK := git --no-pager diff --no-index --ignore-all-space -R

KPROVE_MODULE := VERIFICATION

test: test-unit test-cross test-prove

# Unit

unit_tests         := $(wildcard tests/unit/*.tzt)
unit_tests_failing := $(shell cat tests/failing.unit)
unit_tests_passing := $(filter-out $(unit_tests_failing), $(unit_tests))

test-unit:         $(unit_tests_passing:=.run)
test-unit-failing: $(unit_tests_failing:=.run)

tests/%.run: tests/% $(llvm_kompiled)
	$(TEST) interpret --backend llvm $< --output-file /dev/null

# Cross Validation

cross_tests         := $(wildcard tests/unit/*.tzt)
cross_tests_failing := $(shell cat tests/failing.cross)
cross_tests_passing := $(filter-out $(cross_tests_failing), $(cross_tests))

test-cross:         $(cross_tests_passing:=.cross)
test-cross-failing: $(cross_tests_failing:=.cross)

tests/%.cross: tests/%.output $(output_compare_kompiled)
	$(TEST) interpret --backend output-compare $< --output-file /dev/null > $@

$(cross_tests_passing:=.output): run-tezos.timestamp

run-tezos.timestamp: $(cross_tests_passing) $(cross_tests_passing:=.expanded) $(cross_tests_passing:=.input) $(cross_tests_passing:=.extracted)
	./run-with-tezos.sh run-tezos
	touch $@

tests/%.expanded: tests/%.address $(contract_expander_kompiled)
	$(TEST) interpret --backend contract-expander $< --output-file /dev/null > $@

$(cross_tests_passing:=.address): fix-address.timestamp

fix-address.timestamp: $(cross_tests_passing) $(cross_tests_passing:=.extracted) $(cross_tests_passing:=.input)
	./run-with-tezos.sh fix-address
	touch $@

tests/%.extracted: tests/% $(extractor_kompiled)
	$(TEST) interpret --backend extractor $< --output-file /dev/null > $@

tests/%.input: tests/% $(input_creator_kompiled)
	$(TEST) interpret --backend input-creator $< --output-file /dev/null > $@

# Prove

prove_tests         := $(wildcard tests/proofs/*-spec.k)
prove_tests_failing := $(shell cat tests/failing.prove)
prove_tests_passing := $(filter-out $(prove_tests_failing), $(prove_tests))

test-prove:         $(prove_tests_passing:=.prove)
test-prove-failing: $(prove_tests_failing:=.prove)

tests/%.prove: tests/% $(prove_kompiled)
	$(TEST) prove --backend prove $< $(KPROVE_MODULE)
