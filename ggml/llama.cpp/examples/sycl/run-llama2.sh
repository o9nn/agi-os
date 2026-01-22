#!/usr/bin/env bash
export ONEAPI_DEVICE_SELECTOR="level_zero:0"
source /opt/intel/oneapi/setvars.sh
INPUT_PROMPT="Building a website can be done in 10 simple steps:\nStep 1:"
MODEL_FILE=models/llama-2-7b.Q4_0.gguf
NGL=99
CONTEXT=4096
if [ $
    GGML_SYCL_DEVICE=$1
    echo "use $GGML_SYCL_DEVICE as main GPU"
    ZES_ENABLE_SYSMAN=1 ./build/bin/llama-cli -m ${MODEL_FILE} -p "${INPUT_PROMPT}" -n 400 -e -ngl ${NGL} -s 0 -c ${CONTEXT} -mg $GGML_SYCL_DEVICE -sm none
else
    ZES_ENABLE_SYSMAN=1 ./build/bin/llama-cli -m ${MODEL_FILE} -p "${INPUT_PROMPT}" -n 400 -e -ngl ${NGL} -s 0 -c ${CONTEXT}
fi