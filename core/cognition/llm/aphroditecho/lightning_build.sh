#!/bin/bash
set -e
echo "🚀 Aphrodite Engine - Lightning Studios A100 Build"
echo "=================================================="
export APHRODITE_TARGET_DEVICE=cuda
export CMAKE_BUILD_TYPE=Release
export MAX_JOBS=64
export CCACHE_MAXSIZE=100G
export CUDA_VISIBLE_DEVICES=0
export PATH=/usr/local/cuda/bin:$PATH
export LD_LIBRARY_PATH=/usr/local/cuda/lib64:$LD_LIBRARY_PATH
echo "🔍 Verifying CUDA environment..."
nvidia-smi
nvcc --version
python --version
echo "💻 System resources:"
nproc
free -h
df -h /
echo "📦 Installing optimized build dependencies..."
python -m pip install --upgrade pip wheel setuptools
pip install ninja cmake
echo "🧹 Pre-build cleanup..."
pip cache purge
sudo apt-get clean || true
echo "🏗️ Starting Aphrodite Engine build..."
echo "Estimated time: 2-4 hours on A100 (vs 34+ hours on standard hardware)"
echo "Build steps: 347 total"
start_time=$(date +%s)
timeout 14400 pip install -e . --timeout 7200 --verbose 2>&1 | tee build.log || {
    echo "❌ Build failed or timed out after 4 hours"
    echo "📊 Partial build statistics:"
    grep -i "step\|progress\|%" build.log | tail -10 || true
    exit 1
}
end_time=$(date +%s)
build_duration=$((end_time - start_time))
build_hours=$((build_duration / 3600))
build_minutes=$(((build_duration % 3600) / 60))
echo "✅ Build completed successfully!"
echo "⏱️ Total build time: ${build_hours}h ${build_minutes}m"
echo "🚀 Ready for Deep Tree Echo integration testing"
echo "🔍 Verifying installation..."
python -c "import aphrodite; print(f'Aphrodite version: {aphrodite.__version__}')" || {
    echo "⚠️ Installation verification failed"
    exit 1
}
echo "🧪 Running smoke tests..."
python -c "from aphrodite import LLM, SamplingParams; print('Core imports successful')" || {
    echo "⚠️ Core imports failed"
    exit 1
}
echo "🎉 Lightning Studios A100 build complete and verified!"
echo "💡 Build performance: ~${build_hours}h ${build_minutes}m (vs 34+ hours on standard hardware)"
echo "📈 Performance improvement: ~$(echo "$((3400 / (build_duration / 60)))" | cut -c1-2)x faster"