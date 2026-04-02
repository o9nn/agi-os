#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -lt 1 ]; then
    echo "Usage: $(basename "$0") <source-subdir>" >&2
    exit 1
fi

SOURCE_SUBDIR="$1"
PACKAGE_DIR="$(pwd)"
DEBIAN_DIR="$PACKAGE_DIR/debian"
PACKAGE_NAME="$(basename "$PACKAGE_DIR")"
REPO_ROOT="$(cd "$PACKAGE_DIR/../../../.." && pwd)"
SOURCE_PATH="$REPO_ROOT/$SOURCE_SUBDIR"

if [ ! -d "$DEBIAN_DIR" ]; then
    echo "ERROR: Debian packaging directory not found at $DEBIAN_DIR" >&2
    exit 1
fi

if [ ! -d "$SOURCE_PATH" ]; then
    echo "ERROR: Source directory not found at $SOURCE_PATH" >&2
    exit 1
fi

SOURCE_PACKAGE="$(sed -n 's/^Source:[[:space:]]*//p' "$DEBIAN_DIR/control" | head -n1)"
if [ -z "$SOURCE_PACKAGE" ]; then
    SOURCE_PACKAGE="$PACKAGE_NAME"
fi

VERSION="$(dpkg-parsechangelog -l "$DEBIAN_DIR/changelog" -S Version 2>/dev/null || true)"
if [ -z "$VERSION" ]; then
    VERSION="$(sed -n '1s/.*(\([^)]*\)).*/\1/p' "$DEBIAN_DIR/changelog")"
fi
UPSTREAM_VERSION="${VERSION%%-*}"

WORK_DIR="$PACKAGE_DIR/build"
BUILD_DIR="$WORK_DIR/${SOURCE_PACKAGE}-${UPSTREAM_VERSION}"
TARBALL="$WORK_DIR/${SOURCE_PACKAGE}_${UPSTREAM_VERSION}.orig.tar.gz"

python3 - "$BUILD_DIR" "$TARBALL" <<'PY'
from pathlib import Path
import shutil
import sys
for raw in sys.argv[1:]:
    p = Path(raw)
    if p.exists():
        if p.is_dir():
            shutil.rmtree(p)
        else:
            p.unlink()
PY

mkdir -p "$WORK_DIR"

if [ "$SOURCE_SUBDIR" = "." ]; then
    tar czf "$TARBALL"         --exclude-vcs         --exclude='./build'         --exclude='./.git'         --exclude='./*.o'         --exclude='./*.a'         --exclude='./*.so'         --exclude='./*.pyc'         --exclude='./__pycache__'         --transform "s,^\.,${SOURCE_PACKAGE}-${UPSTREAM_VERSION},"         -C "$REPO_ROOT" .
else
    tar czf "$TARBALL"         --exclude-vcs         --exclude='build'         --exclude='*.o'         --exclude='*.a'         --exclude='*.so'         --exclude='*.pyc'         --exclude='__pycache__'         --transform "s,^${SOURCE_SUBDIR},${SOURCE_PACKAGE}-${UPSTREAM_VERSION},"         -C "$REPO_ROOT" "$SOURCE_SUBDIR"
fi

tar xzf "$TARBALL" -C "$WORK_DIR"
cp -a "$DEBIAN_DIR" "$BUILD_DIR/"

echo "=========================================="
echo "Prepared Debian source package"
echo "  Package dir : $PACKAGE_DIR"
echo "  Source      : $SOURCE_SUBDIR"
echo "  Build dir   : $BUILD_DIR"
echo "  Tarball     : $TARBALL"
echo "=========================================="
echo "Next steps:"
echo "  cd "$BUILD_DIR""
echo "  dpkg-buildpackage -rfakeroot -us -uc"
