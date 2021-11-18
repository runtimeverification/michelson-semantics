ARG K_COMMIT
FROM runtimeverificationinc/kframework-k:ubuntu-focal-${K_COMMIT}

RUN    apt-get update                \
    && apt-get upgrade --yes         \
    && apt-get install --yes         \
                cargo                \
                cmake                \
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
                python-pygments      \
                python3-recommonmark \
                sphinx-common

RUN    git clone 'https://github.com/z3prover/z3' --branch=z3-4.8.11 \
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

RUN    git config --global user.email 'admin@runtimeverification.com' \
    && git config --global user.name  'RV Jenkins'                    \
    && mkdir -p ~/.ssh                                                \
    && echo 'host github.com'                       > ~/.ssh/config   \
    && echo '    hostname github.com'              >> ~/.ssh/config   \
    && echo '    user git'                         >> ~/.ssh/config   \
    && echo '    identityagent SSH_AUTH_SOCK'      >> ~/.ssh/config   \
    && echo '    stricthostkeychecking accept-new' >> ~/.ssh/config   \
    && chmod go-rwx -R ~/.ssh
