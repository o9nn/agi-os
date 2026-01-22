#!/usr/bin/env bash
set -eu
if [ $
then
echo "usage:   $0 path_to_build_binary [path_to_temp_folder]"
echo "example: $0 ../../build/bin ../../tmp"
exit 1
fi
if [ $
then
TMP_DIR=$2
else
TMP_DIR=/tmp
fi
set -x
SPLIT=$1/llama-gguf-split
MAIN=$1/llama-cli
WORK_PATH=$TMP_DIR/gguf-split
ROOT_DIR=$(realpath $(dirname $0)/../../)
mkdir -p "$WORK_PATH"
rm -f $WORK_PATH/ggml-model-split*.gguf $WORK_PATH/ggml-model-merge*.gguf
(
cd $WORK_PATH
"$ROOT_DIR"/scripts/hf.sh --repo ggml-org/gemma-1.1-2b-it-Q8_0-GGUF --file gemma-1.1-2b-it.Q8_0.gguf
)
echo PASS
$SPLIT --split-max-tensors 28  $WORK_PATH/gemma-1.1-2b-it.Q8_0.gguf $WORK_PATH/ggml-model-split
echo PASS
echo
$MAIN -no-cnv --model $WORK_PATH/ggml-model-split-00001-of-00006.gguf --n-predict 32
echo PASS
echo
$SPLIT --merge $WORK_PATH/ggml-model-split-00001-of-00006.gguf $WORK_PATH/ggml-model-merge.gguf
echo PASS
echo
$MAIN -no-cnv --model $WORK_PATH/ggml-model-merge.gguf --n-predict 32
echo PASS
echo
$SPLIT --split-max-tensors 32 --no-tensor-first-split $WORK_PATH/ggml-model-merge.gguf $WORK_PATH/ggml-model-split-32-tensors
echo PASS
echo
$MAIN -no-cnv --model $WORK_PATH/ggml-model-split-32-tensors-00001-of-00007.gguf --n-predict 32
echo PASS
echo
$SPLIT --split-max-size 2G $WORK_PATH/ggml-model-merge.gguf $WORK_PATH/ggml-model-split-2G
echo PASS
echo
$MAIN -no-cnv --model $WORK_PATH/ggml-model-split-2G-00001-of-00002.gguf --n-predict 32
echo PASS
echo
rm -f $WORK_PATH/ggml-model-split*.gguf $WORK_PATH/ggml-model-merge*.gguf