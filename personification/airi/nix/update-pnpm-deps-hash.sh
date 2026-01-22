#! /usr/bin/env bash
set -eux
cd "$(dirname "${BASH_SOURCE[0]}")"
:> pnpm-deps-hash.txt
if [ -w /dev/tty ] 2>/dev/null; then
  HASH=$(nix build ..
else
  HASH=$(nix build ..
fi
echo -n $HASH > pnpm-deps-hash.txt