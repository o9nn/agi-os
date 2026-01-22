#!/bin/sh
if [ ! -d "build-aux" ]; then
    mkdir build-aux
fi
touch build-aux/config.rpath
autoreconf -vif
echo
echo "Now run ./configure [--prefix=/your/prefix] [--with-guile-site=yes]"