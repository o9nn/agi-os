#!/bin/bash
set -e -x
export PATH=$PATH:$HOME/.cargo/bin
git clone https://github.com/rpgp/rpgp.git
cd rpgp/pgp-ffi
make install