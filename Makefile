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

export MAIN_DEFN_FILE

# Tangler for *.md files

tangle_selector := .k

defn:  defn-llvm defn-haskel
build: build-llvm build-haskell build-compat

build-compat:
	./compat/build.sh

# LLVM

llvm_dir           := $(DEFN_DIR)/llvm
llvm_files         := $(patsubst %, $(llvm_dir)/%, $(ALL_FILES))
llvm_main_file     := unit-test
llvm_main_module   := UNIT-TEST
llvm_syntax_module := $(llvm_main_module)
llvm_kompiled      := $(llvm_dir)/$(llvm_main_file)-kompiled/interpreter

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
	        $(KOMPILE_OPTS)                                                         \
	        $(addprefix -ccopt ,$(LLVM_KOMPILE_OPTS))

### Haskell

haskell_dir           := $(DEFN_DIR)/haskell
haskell_files         := $(patsubst %, $(haskell_dir)/%, $(ALL_FILES))
haskell_main_file     := symbolic-unit-test
haskell_main_module   := SYMBOLIC-UNIT-TEST
haskell_syntax_module := $(haskell_main_module)
haskell_kompiled      := $(haskell_dir)/$(haskell_main_file)-kompiled/definition.kore

defn-haskell:  $(haskell_files)
build-haskell: $(haskell_kompiled)

$(haskell_dir)/%.k: %.md $(TANGLER)
	@mkdir -p $(haskell_dir)
	pandoc --from markdown --to "$(TANGLER)" --metadata=code:"$(tangle_selector)" $< > $@

$(haskell_kompiled): $(haskell_files)
	kompile --debug --backend haskell $(haskell_dir)/$(haskell_main_file).k               \
	        --directory $(haskell_dir) -I $(haskell_dir)                                  \
	        --main-module $(haskell_main_module) --syntax-module $(haskell_syntax_module) \
	        $(KOMPILE_OPTS)
