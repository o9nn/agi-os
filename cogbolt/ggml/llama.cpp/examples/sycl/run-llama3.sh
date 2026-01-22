#!/usr/bin/env bash
source /opt/intel/oneapi/setvars.sh
INPUT_PROMPT="Building a website can be done in 10 simple steps:\nStep 1:"
MODEL_FILE=models/Meta-Llama-3.1-8B-Instruct-Q4_K_M.gguf
NGL=99
CONTEXT=4096
if [ $
GGML_SYCL_DEVICE=$1
echo "Using $GGML_SYCL_DEVICE as the main GPU"
ZES_ENABLE_SYSMAN=1 ./build/bin/llama-cli -m ${MODEL_FILE} -p "${INPUT_PROMPT}" -n 400 -e -ngl ${NGL} -c ${CONTEXT} -mg $GGML_SYCL_DEVICE -sm none
else
ZES_ENABLE_SYSMAN=1 ./build/bin/llama-cli -m ${MODEL_FILE} -p "${INPUT_PROMPT}" -n 400 -e -ngl ${NGL} -c ${CONTEXT}
fi