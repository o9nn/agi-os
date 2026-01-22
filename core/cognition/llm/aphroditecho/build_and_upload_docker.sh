#!/bin/bash
set -e
MAX_JOBS=64
NVCC_THREADS=64
CUDA_VERSION=12.8.1
TORCH_CUDA_ARCH_LIST="7.0 7.5 8.0 8.6 8.9 9.0 10.0 10.1 12.0+PTX"
while [[ "$
    case $1 in
        --max_jobs) MAX_JOBS="$2"; shift ;;
        --nvcc_threads) NVCC_THREADS="$2"; shift ;;
        --cuda_version) CUDA_VERSION="$2"; shift ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
    shift
done
DOCKER_BUILDKIT=1 docker build . --target build --tag alpindale/aphrodite-build \
    --build-arg CUDA_VERSION=$CUDA_VERSION \
    --build-arg max_jobs=$MAX_JOBS \
    --build-arg nvcc_threads=$NVCC_THREADS
docker run -d --name aphrodite-build-container alpindale/aphrodite-build tail -f /dev/null
mkdir -p dist
docker cp aphrodite-build-container:/workspace/dist .
docker stop aphrodite-build-container && docker rm aphrodite-build-container
DOCKER_BUILDKIT=1 docker build -f Dockerfile . --target aphrodite-openai --tag alpindale/aphrodite-openai \
    --build-arg CUDA_VERSION=$CUDA_VERSION \
    --build-arg max_jobs=$MAX_JOBS \
    --build-arg nvcc_threads=$NVCC_THREADS
commit=$(git rev-parse --short HEAD)
docker tag alpindale/aphrodite-openai alpindale/aphrodite-openai:${commit}
docker push alpindale/aphrodite-openai:${commit}
docker tag alpindale/aphrodite-openai alpindale/aphrodite-openai:latest
docker push alpindale/aphrodite-openai:latest
echo "Build and upload completed successfully!"