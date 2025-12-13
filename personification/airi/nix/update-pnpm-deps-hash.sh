#! /usr/bin/env bash

set -eux

cd "$(dirname "${BASH_SOURCE[0]}")"
:> pnpm-deps-hash.txt # Clear hash to trigger rebuild
# Redirect stderr to stdout for grep while keeping printing on stderr
# In CI environments, /dev/tty may not be available, so we use stderr instead
if [ -w /dev/tty ] 2>/dev/null; then
  HASH=$(nix build ..#airi.pnpmDeps 2> >(tee /dev/tty >&2) | grep -oP 'got: +\K\S+')
else
  HASH=$(nix build ..#airi.pnpmDeps 2>&1 | grep -oP 'got: +\K\S+')
fi
echo -n $HASH > pnpm-deps-hash.txt
