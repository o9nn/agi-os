#! /usr/bin/env bash
set -eux
cd "$(dirname "${BASH_SOURCE[0]}")"
echo -n "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=" > assets-hash.txt
if [ -w /dev/tty ] 2>/dev/null; then
HASH=$(nix build ..
else
HASH=$(nix build ..
fi
echo -n $HASH > assets-hash.txt