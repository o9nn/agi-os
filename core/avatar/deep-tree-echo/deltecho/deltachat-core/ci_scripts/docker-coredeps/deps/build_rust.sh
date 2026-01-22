#!/bin/bash
set -e -x
curl https://sh.rustup.rs -sSf | sh -s -- --default-toolchain nightly-2019-03-23 -y
export PATH=/root/.cargo/bin:$PATH
rustc --version
rm -rf /root/.rustup/toolchains/nightly-2019-03-23-x86_64-unknown-linux-gnu/share/