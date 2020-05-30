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
PATH               := $(K_BIN):$(PATH)

export LIBRARY_PATH
export C_INCLUDE_PATH
export CPLUS_INCLUDE_PATH
export PATH

.PHONY: all clean distclean                \
        deps deps-k deps-tezos deps-tangle \
        defn defn-llvm                     \
        build build-llvm build-compat
.SECONDARY:

all: build

clean:
	rm -rf $(DEFN_BASE_DIR)

distclean:
	rm -rf $(BUILD_DIR)

# K Dependencies
# --------------

K_JAR := $(K_SUBMODULE)/k-distribution/target/release/k/lib/java/kernel-1.0-SNAPSHOT.jar

deps: deps-k deps-tezos deps-tangle
deps-k: $(K_JAR)

$(TEZOS_SUBMODULE)/make.timestamp:
	./build-deps-tezos.sh
	touch $(TEZOS_SUBMODULE)/make.timestamp

ifneq ($(RELEASE),)
    K_BUILD_TYPE         := FastBuild
    SEMANTICS_BUILD_TYPE := Release
    KOMPILE_OPTS         += -O3
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

SOURCE_FILES       := michelson-common          \
                      michelson-config          \
                      michelson-internal-syntax \
                      michelson                 \
                      michelson-syntax          \
                      michelson-types           \
                      symbolic-configuration    \
                      symbolic-unit-test        \
                      symbolic-unit-test-syntax \
                      unit-test                 \
                      unit-test-syntax
EXTRA_SOURCE_FILES :=
ALL_FILES          := $(patsubst %, %.k, $(SOURCE_FILES) $(EXTRA_SOURCE_FILES))

# Tangler for *.md files

tangle_selector := .k

hook_namespaces := TIME MICHELSON

defn:  defn-llvm defn-prove defn-symbolic
build: build-llvm build-prove build-symbolic build-compat

build-compat:
	./compat/build.sh

# LLVM

llvm_dir             := $(DEFN_DIR)/llvm
llvm_files           := $(patsubst %, $(llvm_dir)/%, $(ALL_FILES))
llvm_main_file       := unit-test
llvm_main_module     := UNIT-TEST
llvm_syntax_module   := $(llvm_main_module)
llvm_kompiled        := $(llvm_dir)/$(llvm_main_file)-kompiled/interpreter

CPP_FILES := hex.cpp time.cpp

LLVM_KOMPILE_OPTS := -L$(LOCAL_LIB) -I$(K_RELEASE)/include/kllvm \
                     $(abspath $(CPP_FILES))                     \
                     -std=c++14

ifeq (,$(RELEASE))
    LLVM_KOMPILE_OPTS += -g
endif

defn-llvm:  $(llvm_files)
build-llvm: $(llvm_kompiled)

$(llvm_dir)/%.k: %.md $(TANGLER)
	@mkdir -p $(llvm_dir)
	pandoc --from markdown --to "$(TANGLER)" --metadata=code:"$(tangle_selector)" $< > $@

$(llvm_kompiled): $(llvm_files)
	kompile --debug --backend llvm $(llvm_dir)/$(llvm_main_file).k                  \
	        --directory $(llvm_dir) -I $(llvm_dir)                                  \
	        --main-module $(llvm_main_module) --syntax-module $(llvm_syntax_module) \
	        --hook-namespaces "$(hook_namespaces)"                                  \
	        $(KOMPILE_OPTS)                                                         \
	        $(addprefix -ccopt ,$(LLVM_KOMPILE_OPTS))

### Prove

prove_dir           := $(DEFN_DIR)/prove
prove_files         := $(patsubst %, $(prove_dir)/%, $(ALL_FILES))
prove_main_file     := unit-test
prove_main_module   := UNIT-TEST
prove_syntax_module := $(prove_main_module)
prove_kompiled      := $(prove_dir)/$(prove_main_file)-kompiled/definition.kore

defn-prove:  $(prove_files)
build-prove: $(prove_kompiled)

$(prove_dir)/%.k: %.md $(TANGLER)
	@mkdir -p $(prove_dir)
	pandoc --from markdown --to "$(TANGLER)" --metadata=code:"$(tangle_selector)" $< > $@

$(prove_kompiled): $(prove_files)
	kompile --debug --backend prove $(prove_dir)/$(prove_main_file).k                 \
	        --directory $(prove_dir) -I $(prove_dir)                                  \
	        --main-module $(prove_main_module) --syntax-module $(prove_syntax_module) \
	        --hook-namespaces "$(hook_namespaces)"                                    \
	        $(KOMPILE_OPTS)

### Symbolic

symbolic_dir           := $(DEFN_DIR)/symbolic
symbolic_files         := $(patsubst %, $(symbolic_dir)/%, $(ALL_FILES))
symbolic_main_file     := symbolic-unit-test
symbolic_main_module   := SYMBOLIC-UNIT-TEST
symbolic_syntax_module := $(symbolic_main_module)
symbolic_kompiled      := $(symbolic_dir)/$(symbolic_main_file)-kompiled/definition.kore

defn-symbolic:  $(symbolic_files)
build-symbolic: $(symbolic_kompiled)

$(symbolic_dir)/%.k: %.md $(TANGLER)
	@mkdir -p $(symbolic_dir)
	pandoc --from markdown --to "$(TANGLER)" --metadata=code:"$(tangle_selector)" $< > $@

$(symbolic_kompiled): $(symbolic_files)
	kompile --debug --backend symbolic $(symbolic_dir)/$(symbolic_main_file).k              \
	        --directory $(symbolic_dir) -I $(symbolic_dir)                                  \
	        --main-module $(symbolic_main_module) --syntax-module $(symbolic_syntax_module) \
	        --hook-namespaces "$(hook_namespaces)"                                          \
	        $(KOMPILE_OPTS)
