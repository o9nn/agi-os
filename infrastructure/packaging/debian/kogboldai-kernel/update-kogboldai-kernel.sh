#!/bin/bash
set -e
PACKAGE_NAME="kogboldai-kernel"
VERSION="1.0.0"
REPO_URL="https://github.com/cogpy/KogboldAI.git"
BRANCH="united"
echo "========================================="
echo "Updating $PACKAGE_NAME package"
echo "========================================="
if [ ! -d "KogboldAI" ]; then
echo "Cloning KogboldAI repository..."
git clone --depth 1 --branch "$BRANCH" "$REPO_URL" KogboldAI
else
echo "Updating existing KogboldAI repository..."
cd KogboldAI
git fetch origin "$BRANCH"
git checkout "$BRANCH"
git pull
cd ..
fi
SOURCE_DIR="${PACKAGE_NAME}-${VERSION}"
rm -rf "$SOURCE_DIR"
mkdir -p "$SOURCE_DIR"
echo "Copying kernel source files..."
cp -r KogboldAI/kernel/* "$SOURCE_DIR/"
echo "Copying debian packaging files..."
cp -r debian "$SOURCE_DIR/"
echo "Creating source tarball..."
tar czf "${PACKAGE_NAME}_${VERSION}.orig.tar.gz" "$SOURCE_DIR"
echo "========================================="
echo "Package: $PACKAGE_NAME"
echo "Version: $VERSION"
echo "Source: $SOURCE_DIR"
echo "Tarball: ${PACKAGE_NAME}_${VERSION}.orig.tar.gz"
echo "========================================="