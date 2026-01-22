#!/usr/bin/env bash
mkdir -p build
cd build
source /opt/intel/oneapi/setvars.sh
cmake .. -DGGML_SYCL=ON -DCMAKE_C_COMPILER=icx -DCMAKE_CXX_COMPILER=icpx -DLLAMA_CURL=OFF
cmake --build . --config Release -j -v