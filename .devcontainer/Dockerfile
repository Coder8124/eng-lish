FROM mcr.microsoft.com/devcontainers/rust:1-bookworm

ARG LLVM_VERSION=21

# inkwell is pinned to one LLVM major version (feature llvm21-1), so the
# container installs that exact toolchain from apt.llvm.org rather than
# whatever Debian ships.
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        ca-certificates gnupg lsb-release software-properties-common wget \
    && wget -qO /tmp/llvm.sh https://apt.llvm.org/llvm.sh \
    && chmod +x /tmp/llvm.sh \
    && /tmp/llvm.sh ${LLVM_VERSION} \
    && apt-get install -y --no-install-recommends \
        llvm-${LLVM_VERSION}-dev \
        libpolly-${LLVM_VERSION}-dev \
        clang-${LLVM_VERSION} \
        libzstd-dev \
    && ln -sf /usr/bin/clang-${LLVM_VERSION} /usr/local/bin/clang \
    && rm -rf /var/lib/apt/lists/* /tmp/llvm.sh

ENV LLVM_SYS_211_PREFIX=/usr/lib/llvm-${LLVM_VERSION}
ENV PATH=/usr/lib/llvm-${LLVM_VERSION}/bin:${PATH}
