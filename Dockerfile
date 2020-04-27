FROM runtimeverificationinc/ubuntu:bionic

# add opam repository
RUN    apt-get update                                   \
    && apt-get install --yes software-properties-common \
    && add-apt-repository ppa:avsm/ppa

# install dependencies
RUN    apt-get update                \
    && apt-get upgrade --yes         \
    && apt-get install --yes         \
            autoconf                 \
            bison                    \
            clang-8                  \
            cmake                    \
            curl                     \
            flex                     \
            gcc                      \
            jq                       \
            libboost-test-dev        \
            libcrypto++-dev          \
            libffi-dev               \
            libgflags-dev            \
            libjemalloc-dev          \
            libmpfr-dev              \
            libprocps-dev            \
            libprotobuf-dev          \
            libsecp256k1-dev         \
            libssl-dev               \
            libtool                  \
            libyaml-dev              \
            lld-8                    \
            llvm-8-tools             \
            make                     \
            maven                    \
            netcat-openbsd           \
            opam                     \
            openjdk-11-jdk           \
            pandoc                   \
            pcregrep                 \
            pkg-config               \
            protobuf-compiler        \
            python3                  \
            python-pygments          \
            python-recommonmark      \
            python-sphinx            \
            rapidjson-dev            \
            time                     \
            zlib1g-dev

ADD ext/k/haskell-backend/src/main/native/haskell-backend/scripts/install-stack.sh /.install-stack/
RUN /.install-stack/install-stack.sh

RUN    git clone 'https://github.com/z3prover/z3' --branch=z3-4.6.0 \
    && cd z3                                                        \
    && python scripts/mk_make.py                                    \
    && cd build                                                     \
    && make -j8                                                     \
    && make install                                                 \
    && cd ../..                                                     \
    && rm -rf z3

USER user:user

ENV LC_ALL=C.UTF-8
ADD --chown=user:user ext/k/haskell-backend/src/main/native/haskell-backend/stack.yaml /home/user/.tmp-haskell/
ADD --chown=user:user ext/k/haskell-backend/src/main/native/haskell-backend/kore/package.yaml /home/user/.tmp-haskell/kore/
RUN    cd /home/user/.tmp-haskell \
    && stack build --only-snapshot

ADD ext/k/pom.xml /home/user/.tmp-maven/
ADD ext/k/ktree/pom.xml /home/user/.tmp-maven/ktree/
ADD ext/k/llvm-backend/pom.xml /home/user/.tmp-maven/llvm-backend/
ADD ext/k/llvm-backend/src/main/native/llvm-backend/matching/pom.xml /home/user/.tmp-maven/llvm-backend/src/main/native/llvm-backend/matching/
ADD ext/k/haskell-backend/pom.xml /home/user/.tmp-maven/haskell-backend/
ADD ext/k/ocaml-backend/pom.xml /home/user/.tmp-maven/ocaml-backend/
ADD ext/k/kernel/pom.xml /home/user/.tmp-maven/kernel/
ADD ext/k/java-backend/pom.xml /home/user/.tmp-maven/java-backend/
ADD ext/k/k-distribution/pom.xml /home/user/.tmp-maven/k-distribution/
ADD ext/k/kore/pom.xml /home/user/.tmp-maven/kore/
RUN    cd /home/user/.tmp-maven \
    && mvn dependency:go-offline

ENV LD_LIBRARY_PATH=/usr/local/lib
ENV PATH=/home/user/.local/bin:$PATH

RUN mkdir -p /home/user/.ssh
ADD --chown=user:user ssh/config /home/user/.ssh/
RUN    chmod go-rwx -R /home/user/.ssh                                \
    && git config --global user.email "admin@runtimeverification.com" \
    && git config --global user.name  "RV Jenkins"
