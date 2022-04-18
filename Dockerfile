ARG K_COMMIT
FROM runtimeverificationinc/kframework-k:ubuntu-focal-${K_COMMIT}

RUN    apt-get update                \
    && apt-get upgrade --yes         \
    && apt-get install --yes         \
                autoconf             \
                cmake                \
                curl                 \
                jq                   \
                libcrypto++-dev      \
                libev-dev            \
                libhidapi-dev        \
                libprocps-dev        \
                libsecp256k1-dev     \
                libsodium-dev        \
                libssl-dev           \
                npm                  \
                opam                 \
                pandoc               \
                pcregrep             \
                pkg-config           \
                python3              \
                python3-pip          \
                python3-recommonmark \
                python-pygments      \
                sphinx-common        \
                zlib1g-dev

RUN    git clone 'https://github.com/z3prover/z3' --branch=z3-4.8.15 \
    && cd z3                                                         \
    && python scripts/mk_make.py                                     \
    && cd build                                                      \
    && make -j8                                                      \
    && make install                                                  \
    && cd ../..                                                      \
    && rm -rf z3

RUN curl -sL https://deb.nodesource.com/setup_14.x | bash -
RUN    apt-get update               \
    && apt-get upgrade --yes        \
    && apt-get install --yes nodejs

RUN pip3 install click graphviz pytezos==3.2.11

ARG USER_ID=1000
ARG GROUP_ID=1000
RUN groupadd -g $GROUP_ID user && useradd -m -u $USER_ID -s /bin/sh -g user user

USER user:user
WORKDIR /home/user

# setup rust with correct version
RUN curl -sL https://sh.rustup.rs/rustup-init.sh | bash -s -- --profile minimal --default-toolchain 1.52.1 -y
ENV PATH="/home/user/.cargo/bin/:${PATH}"

RUN    git config --global user.email 'admin@runtimeverification.com' \
    && git config --global user.name  'RV Jenkins'                    \
    && mkdir -p ~/.ssh                                                \
    && echo 'host github.com'                       > ~/.ssh/config   \
    && echo '    hostname github.com'              >> ~/.ssh/config   \
    && echo '    user git'                         >> ~/.ssh/config   \
    && echo '    identityagent SSH_AUTH_SOCK'      >> ~/.ssh/config   \
    && echo '    stricthostkeychecking accept-new' >> ~/.ssh/config   \
    && chmod go-rwx -R ~/.ssh
