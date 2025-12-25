#!/bin/bash
# AGI-OS Native Code Build Script
# Builds all native code integration components

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
NATIVE_CODE_DIR="$PROJECT_ROOT/external/native-code"
BUILD_DIR="$PROJECT_ROOT/build/native-code"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Parse arguments
BUILD_TYPE="Release"
BUILD_TESTS=OFF
INSTALL_PREFIX="/usr/local"
JOBS=$(nproc)

while [[ $# -gt 0 ]]; do
    case $1 in
        --debug)
            BUILD_TYPE="Debug"
            shift
            ;;
        --tests)
            BUILD_TESTS=ON
            shift
            ;;
        --prefix=*)
            INSTALL_PREFIX="${1#*=}"
            shift
            ;;
        -j*)
            JOBS="${1#-j}"
            shift
            ;;
        --help)
            echo "Usage: $0 [options]"
            echo ""
            echo "Options:"
            echo "  --debug         Build with debug symbols"
            echo "  --tests         Build and run tests"
            echo "  --prefix=PATH   Installation prefix (default: /usr/local)"
            echo "  -jN             Number of parallel jobs (default: nproc)"
            echo "  --help          Show this help message"
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            exit 1
            ;;
    esac
done

log_info "AGI-OS Native Code Build"
log_info "========================"
log_info "Build type: $BUILD_TYPE"
log_info "Install prefix: $INSTALL_PREFIX"
log_info "Parallel jobs: $JOBS"
log_info "Build tests: $BUILD_TESTS"
echo ""

# Create build directory
mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

# Build OpenNARS
log_info "Building OpenNARS for Applications..."
if [ -d "$NATIVE_CODE_DIR/opennars" ]; then
    mkdir -p opennars && cd opennars
    cmake "$NATIVE_CODE_DIR/opennars" \
        -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
        -DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX \
        -DOPENNARS_BUILD_TESTS=$BUILD_TESTS
    make -j$JOBS
    log_success "OpenNARS built successfully"
    cd ..
else
    log_warning "OpenNARS source not found, skipping"
fi

# Build GGML
log_info "Building GGML tensor library..."
if [ -d "$NATIVE_CODE_DIR/ggml-core" ]; then
    mkdir -p ggml && cd ggml
    cmake "$NATIVE_CODE_DIR/ggml-core" \
        -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
        -DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX \
        -DGGML_BUILD_TESTS=$BUILD_TESTS
    make -j$JOBS
    log_success "GGML built successfully"
    cd ..
else
    log_warning "GGML source not found, skipping"
fi

# Build Cognitive Synergy Bridge and Integrations
log_info "Building Cognitive Synergy Bridge..."
mkdir -p integrations && cd integrations
cmake "$NATIVE_CODE_DIR/integrations" \
    -DCMAKE_BUILD_TYPE=$BUILD_TYPE \
    -DCMAKE_INSTALL_PREFIX=$INSTALL_PREFIX \
    -DBUILD_NARS_INTEGRATION=ON \
    -DBUILD_GGML_INTEGRATION=ON \
    -DBUILD_METTA_INTEGRATION=ON

make -j$JOBS
log_success "Cognitive Synergy Bridge built successfully"
cd ..

# Run tests if requested
if [ "$BUILD_TESTS" = "ON" ]; then
    log_info "Running tests..."
    ctest --output-on-failure
fi

# Summary
echo ""
log_success "Build complete!"
log_info "Build artifacts are in: $BUILD_DIR"
echo ""
log_info "To install, run:"
echo "  cd $BUILD_DIR && sudo make install"
