#!/usr/bin/bash
set -e
export PATH=/usr/bin:$PATH
SOURCE_PATH="$1"
CURRENT_PACKAGES_DIR="$2"
pushd ${SOURCE_PATH}
make PREFIX=${CURRENT_PACKAGES_DIR}
make install PREFIX=${CURRENT_PACKAGES_DIR}
popd