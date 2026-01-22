#!/bin/bash
set -ex
pushd "$(dirname "$0")"
[ ! $EMSDK ] && pushd emsdk && source ./emsdk_env.sh && popd
pushd ../..
emconfigure ./configure --disable-editline --disable-sat-solver --disable-java-bindings --disable-python-bindings --disable-pcre2
emmake make clean
emmake make
popd
./link-parser/build.sh
popd