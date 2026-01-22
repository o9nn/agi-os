#!/usr/bin/env bash
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR
set -eu
if [[ "${SLOW_TESTS:-0}" == 1 ]]; then
python $SCRIPT_DIR/../../../scripts/fetch_server_test_models.py
fi
if [ $
then
if [[ "${SLOW_TESTS:-0}" == 1 ]]; then
pytest -v -x
else
pytest -v -x -m "not slow"
fi
else
pytest "$@"
fi