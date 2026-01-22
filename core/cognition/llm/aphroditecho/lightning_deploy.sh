#!/bin/bash
set -e
echo "🏠 Personal Developer Pro - Aphrodite Engine Lightning AI Deployment"
echo "=================================================================="
echo "💰 Cost Optimization: Enabled"
echo "⏱️ Auto-shutdown: 30 minutes idle"
git clone https://github.com/EchoCog/aphroditecho.git /tmp/aphroditecho
cd /tmp/aphroditecho
export APHRODITE_TARGET_DEVICE=cuda
export CMAKE_BUILD_TYPE=Release
export MAX_JOBS=16
export CCACHE_MAXSIZE=30G
echo "🔍 Environment verification:"
nvidia-smi
python --version
nvcc --version
echo "🏗️ Starting build process..."
time ./lightning_build.sh
echo "📦 Creating deployment artifacts..."
mkdir -p /tmp/artifacts
cp -r dist/ /tmp/artifacts/ || true
cp build.log /tmp/artifacts/ || true
echo "✅ Deployment complete - artifacts in /tmp/artifacts/"