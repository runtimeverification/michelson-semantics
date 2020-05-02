ARG K_COMMIT
FROM runtimeverificationinc/kframework-k:ubuntu-bionic-${K_COMMIT}

RUN    sudo apt-get update                                   \
    && sudo apt-get upgrade --yes                            \
    && sudo apt-get install --yes software-properties-common \
    && sudo add-apt-repository ppa:avsm/ppa

RUN    sudo apt-get update              \
    && sudo apt-get upgrade --yes       \
    && sudo apt-get install --yes       \
                    cmake               \
                    jq                  \
                    libcrypto++-dev     \
                    libprocps-dev       \
                    libsecp256k1-dev    \
                    libssl-dev          \
                    opam                \
                    pandoc              \
                    pcregrep            \
                    pkg-config          \
                    python3             \
                    python-pygments     \
                    python-recommonmark \
                    python-sphinx

RUN mkdir -p /home/user/.ssh
ADD --chown=user:user ssh/config /home/user/.ssh/
RUN    chmod go-rwx -R /home/user/.ssh                                \
    && git config --global user.email 'admin@runtimeverification.com' \
    && git config --global user.name  'RV Jenkins'

ENV OPAMROOT=/home/user/.opam
