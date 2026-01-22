#!/bin/bash
set -ex
pushd "$(dirname "$0")"
[ ! $EMSDK ] && pushd ../emsdk && source ./emsdk_env.sh && popd
rm -rf dist
mkdir dist
cp ../../../link-parser/link_parser-command-line.o command-line.bc
cp ../../../link-parser/link_parser-lg_readline.o lg_readline.bc
cp ../../../link-parser/link_parser-parser-utilities.o parser-utilities.bc
cp ../../../link-parser/link_parser-link-parser.o link-parser.bc
emcc -O3 link-parser.bc command-line.bc lg_readline.bc parser-utilities.bc \
../../../link-grammar/.libs/liblink-grammar.a \
--pre-js pre.js \
-s WASM=1 \
-s ALLOW_MEMORY_GROWTH=1 \
-o dist/link-parser.js
rm link-parser.bc
cp -r ../../../data dist/data
cp bin.js package.json README.md dist
pushd dist && npm link && popd
echo The needs of the many outweigh the needs of the few. | link-parser
pushd dist && npm unlink && popd
popd