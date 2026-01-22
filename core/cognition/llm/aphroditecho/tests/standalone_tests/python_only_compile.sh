#!/bin/bash
set -e
set -x
cd /aphrodite-workspace/
pip3 uninstall -y aphrodite
mv test_docs/aphrodite ./aphrodite
apt remove --purge build-essential -y
apt autoremove -y
echo 'import os; os.system("touch /tmp/changed.file")' >> aphrodite/__init__.py
APHRODITE_TEST_USE_PRECOMPILED_NIGHTLY_WHEEL=1 APHRODITE_USE_PRECOMPILED=1 pip3 install -vvv -e .
python3 -c 'import aphrodite'
if [ ! -f /tmp/changed.file ]; then
echo "changed.file was not created, python only compilation failed"
exit 1
fi