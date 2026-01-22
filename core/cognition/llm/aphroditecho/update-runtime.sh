#!/bin/bash
wget -qO- https://github.com/mamba-org/micromamba-releases/releases/download/1.5.8-0/micromamba-linux-64.tar.bz2 | tar -xvj bin/micromamba
if [ ! -f "conda/envs/aphrodite-runtime/bin/python" ]; then
 bin/micromamba create --no-shortcuts -r conda -n aphrodite-runtime -f environment.yaml -y
fi
bin/micromamba create --no-shortcuts -r conda -n aphrodite-runtime -f environment.yaml -y
bin/micromamba run -r conda -n aphrodite-runtime pip install -r requirements/cuda.txt
export CUDA_HOME=$(realpath $(find ./ -type d -path "*/conda/envs/aphrodite-runtime" 2>/dev/null | head -n 1))
export PATH=$CUDA_HOME/bin:$PATH
bin/micromamba run -r conda -n aphrodite-runtime pip install -ve .