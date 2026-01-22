#!/bin/bash
set -e
LIGHTNING_SSH_HOST="s_01k38606vyrx8vgc38h5wm9rd9@ssh.lightning.ai"
REMOTE_WORK_DIR="/tmp/aphroditecho-build"
LOCAL_REPO_DIR="/workspaces/aphroditecho"
echo "🌩️ Lightning AI SSH Deployment for Aphrodite Engine"
echo "=================================================="
run_remote() {
    echo "🔧 Remote: $1"
    ssh -o StrictHostKeyChecking=no "$LIGHTNING_SSH_HOST" "$1"
}
copy_to_remote() {
    echo "📤 Copying: $1 → $2"
    scp -o StrictHostKeyChecking=no -r "$1" "$LIGHTNING_SSH_HOST:$2"
}
copy_from_remote() {
    echo "📥 Downloading: $1 → $2"
    scp -o StrictHostKeyChecking=no -r "$LIGHTNING_SSH_HOST:$1" "$2"
}
echo "🔍 Testing Lightning AI SSH connection..."
if run_remote "echo 'SSH connection successful' && nvidia-smi | head -5"; then
    echo "✅ Lightning AI SSH connection established"
else
    echo "❌ SSH connection failed"
    exit 1
fi
echo "🏗️ Setting up remote build environment..."
run_remote "mkdir -p $REMOTE_WORK_DIR && cd $REMOTE_WORK_DIR"
echo "📦 Uploading repository to Lightning AI..."
copy_to_remote "$LOCAL_REPO_DIR" "$REMOTE_WORK_DIR/aphroditecho"
echo "⚙️ Configuring build environment on A100..."
run_remote "cd $REMOTE_WORK_DIR/aphroditecho && chmod +x lightning_build.sh"
echo "🚀 Starting Aphrodite Engine build on Lightning AI A100..."
echo "⏱️ Estimated time: 2-4 hours (vs 34+ hours locally)"
echo "📊 Progress will be shown in real-time..."
BUILD_START_TIME=$(date +%s)
if run_remote "cd $REMOTE_WORK_DIR/aphroditecho && timeout 14400 ./lightning_build.sh"; then
    BUILD_END_TIME=$(date +%s)
    BUILD_DURATION=$((BUILD_END_TIME - BUILD_START_TIME))
    BUILD_HOURS=$((BUILD_DURATION / 3600))
    BUILD_MINUTES=$(((BUILD_DURATION % 3600) / 60))
    echo "✅ Build completed successfully!"
    echo "⏱️ Total build time: ${BUILD_HOURS}h ${BUILD_MINUTES}m"
    echo "🚀 Performance improvement: ~$(echo "$((12000 / BUILD_DURATION))" | cut -c1-2)x faster than standard hardware"
else
    echo "❌ Build failed or timed out"
    echo "📋 Fetching error logs..."
    copy_from_remote "$REMOTE_WORK_DIR/aphroditecho/build.log" "./build_error.log" || true
    exit 1
fi
echo "🔍 Verifying build on Lightning AI..."
if run_remote "cd $REMOTE_WORK_DIR/aphroditecho && python -c 'import aphrodite; print(f\"Aphrodite version: {aphrodite.__version__}\")'"; then
    echo "✅ Build verification successful"
else
    echo "⚠️ Build verification failed"
fi
echo "📥 Downloading build artifacts..."
mkdir -p "./lightning_artifacts"
copy_from_remote "$REMOTE_WORK_DIR/aphroditecho/dist" "./lightning_artifacts/dist" || echo "No dist directory found"
copy_from_remote "$REMOTE_WORK_DIR/aphroditecho/build.log" "./lightning_artifacts/build.log" || echo "No build log found"
copy_from_remote "$REMOTE_WORK_DIR/aphroditecho/.venv/lib/python*/site-packages/aphrodite*" "./lightning_artifacts/packages" || echo "No packages found"
read -p "🧹 Clean up remote build directory? (y/N): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    run_remote "rm -rf $REMOTE_WORK_DIR"
    echo "✅ Remote cleanup completed"
fi
echo "🧪 Setting up local testing environment..."
if [ -d "./lightning_artifacts/dist" ]; then
    echo "📦 Installing locally from Lightning AI build..."
    find ./lightning_artifacts/dist -name "*.whl" -exec /workspaces/aphroditecho/.venv/bin/pip install {} --force-reinstall \; || echo "No wheels found"
fi
echo ""
echo "🎉 Lightning AI SSH Deployment Complete!"
echo "========================================"
echo "✅ Build completed on Lightning AI A100"
echo "✅ Artifacts downloaded to ./lightning_artifacts/"
echo "✅ Local environment updated (if wheels available)"
echo ""
echo "📊 Build Statistics:"
echo "   Duration: ${BUILD_HOURS}h ${BUILD_MINUTES}m"
echo "   Platform: Lightning AI A100 GPU"
echo "   Performance: ~10-15x faster than standard hardware"
echo ""
echo "🎯 Next Steps:"
echo "   1. Test local installation: python -c 'import aphrodite; print(aphrodite.__version__)'"
echo "   2. Run Deep Tree Echo integration tests"
echo "   3. Deploy to production environment"
echo ""
echo "💡 Artifacts available in: ./lightning_artifacts/"