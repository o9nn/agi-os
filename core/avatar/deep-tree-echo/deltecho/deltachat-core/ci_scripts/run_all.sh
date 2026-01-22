#!/bin/bash
set -e -x
export NINJA_BUILD_DIR=.docker-corebuild
export TOXWORKDIR=.docker-tox
[ -d "$NINJA_BUILD_DIR" ] && rm -rf "$NINJA_BUILD_DIR"
meson $MESONARGS -Drpgp=true "$NINJA_BUILD_DIR" .
pushd $NINJA_BUILD_DIR 
ninja
ninja install
ldconfig -v
popd
export PATH=$PATH:/opt/python/cp35-cp35m/bin
export PYTHONDONTWRITEBYTECODE=1
pushd /bin
ln -s /opt/python/cp27-cp27m/bin/python2.7
ln -s /opt/python/cp36-cp36m/bin/python3.6
ln -s /opt/python/cp37-cp37m/bin/python3.7
popd
if [ -n "$TESTS" ]; then 
    pushd $NINJA_BUILD_DIR
    popd
    echo ----------------
    echo run python tests
    echo ----------------
    pushd python 
    rm -rf tests/__pycache__
    rm -rf src/deltachat/__pycache__
    export PYTHONDONTWRITEBYTECODE=1
    tox --workdir "$TOXWORKDIR" -e py27,py35,py36,py37
    popd
fi
if [ -n "$DOCS" ]; then 
    echo -----------------------
    echo generating python docs
    echo -----------------------
    (cd python && tox --workdir "$TOXWORKDIR" -e doc) 
fi