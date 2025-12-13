#!/bin/bash
#
# AGI-OS Comprehensive Build Script
# Builds all layers in correct dependency order:
# Layer 0: Build Tools (MIG)
# Layer 1: Cognumach (Microkernel)
# Layer 2: HurdCog (Cognitive OS)
# Layer 3: OpenCog Collection (AGI Framework)
#

set -e

# Configuration
BUILD_DIR="${BUILD_DIR:-.}"
INSTALL_PREFIX="${INSTALL_PREFIX:-/usr/local}"
CMAKE_BUILD_TYPE="${CMAKE_BUILD_TYPE:-Release}"
PARALLEL_JOBS="${PARALLEL_JOBS:-$(nproc)}"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
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

# Parse command line arguments
BUILD_COGNUMACH=0
BUILD_HURDCOG=0
BUILD_OCC=1
BUILD_COGBOLT=1

while [[ $# -gt 0 ]]; do
    case $1 in
        --help)
            echo "AGI-OS Build Script"
            echo ""
            echo "Usage: ./build-agi-os.sh [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --cognumach       Build Cognumach microkernel"
            echo "  --hurdcog         Build HurdCog OS (implies --cognumach)"
            echo "  --all             Build all components"
            echo "  --occ-only        Build only OpenCog Collection"
            echo "  --prefix PATH     Set installation prefix (default: /usr/local)"
            echo "  --jobs N          Set parallel jobs (default: nproc)"
            echo "  --help            Display this help message"
            echo ""
            echo "Examples:"
            echo "  ./build-agi-os.sh                    # Build OpenCog and CogBolt"
            echo "  ./build-agi-os.sh --all              # Build everything"
            echo "  ./build-agi-os.sh --occ-only         # Build only OpenCog"
            echo "  ./build-agi-os.sh --prefix /opt/agi  # Custom install location"
            exit 0
            ;;
        --cognumach)
            BUILD_COGNUMACH=1
            shift
            ;;
        --hurdcog)
            BUILD_HURDCOG=1
            BUILD_COGNUMACH=1
            shift
            ;;
        --all)
            BUILD_COGNUMACH=1
            BUILD_HURDCOG=1
            BUILD_OCC=1
            BUILD_COGBOLT=1
            shift
            ;;
        --occ-only)
            BUILD_COGNUMACH=0
            BUILD_HURDCOG=0
            BUILD_OCC=1
            BUILD_COGBOLT=0
            shift
            ;;
        --prefix)
            INSTALL_PREFIX="$2"
            shift 2
            ;;
        --jobs)
            PARALLEL_JOBS="$2"
            shift 2
            ;;
        *)
            log_error "Unknown option: $1"
            exit 1
            ;;
    esac
done

log_info "AGI-OS Build Configuration:"
log_info "  Build directory: $BUILD_DIR"
log_info "  Install prefix: $INSTALL_PREFIX"
log_info "  Build type: $CMAKE_BUILD_TYPE"
log_info "  Parallel jobs: $PARALLEL_JOBS"
log_info "  Build Cognumach: $BUILD_COGNUMACH"
log_info "  Build HurdCog: $BUILD_HURDCOG"
log_info "  Build OCC: $BUILD_OCC"
log_info "  Build CogBolt: $BUILD_COGBOLT"
log_info ""

# Layer 0: Build Tools - MIG (Mach Interface Generator)
if [ $BUILD_COGNUMACH -eq 1 ] || [ $BUILD_HURDCOG -eq 1 ]; then
    log_info "Layer 0: Building MIG (Mach Interface Generator)..."
    
    if [ -d "core/microkernel/cognumach/mig" ]; then
        log_info "  MIG found in Cognumach"
    else
        log_warning "  MIG not found - will be built with Cognumach"
    fi
fi

# Layer 1: Cognumach Microkernel
if [ $BUILD_COGNUMACH -eq 1 ]; then
    log_info "Layer 1: Building Cognumach Microkernel..."
    
    if [ ! -d "core/microkernel/cognumach" ]; then
        log_error "Cognumach directory not found at core/microkernel/cognumach"
        exit 1
    fi
    
    cd core/microkernel/cognumach
    
    if ! command -v autoreconf &> /dev/null; then
        log_error "autoreconf not found. Please install autotools."
        exit 1
    fi
    
    if [ ! -f "configure" ]; then
        log_info "  Running autoreconf..."
        autoreconf -fi || {
            log_error "autoreconf failed"
            exit 1
        }
    fi
    
    mkdir -p build
    cd build
    
    ../configure --prefix="$INSTALL_PREFIX" || {
        log_error "Cognumach configure failed"
        exit 1
    }
    
    make -j$PARALLEL_JOBS || {
        log_error "Cognumach build failed"
        exit 1
    }
    
    make install || {
        log_error "Cognumach install failed"
        exit 1
    }
    
    log_success "Cognumach built and installed"
    cd ../../../..
fi

# Layer 2: HurdCog Operating System
if [ $BUILD_HURDCOG -eq 1 ]; then
    log_info "Layer 2: Building HurdCog Operating System..."
    
    if [ ! -d "core/os/hurdcog" ]; then
        log_error "HurdCog directory not found at core/os/hurdcog"
        exit 1
    fi
    
    cd core/os/hurdcog
    
    if ! command -v autoreconf &> /dev/null; then
        log_error "autoreconf not found. Please install autotools."
        exit 1
    fi
    
    if [ ! -f "configure" ]; then
        log_info "  Running autoreconf..."
        autoreconf -fi || {
            log_error "autoreconf failed"
            exit 1
        }
    fi
    
    mkdir -p build
    cd build
    
    ../configure --prefix="$INSTALL_PREFIX" || {
        log_error "HurdCog configure failed"
        exit 1
    }
    
    make -j$PARALLEL_JOBS || {
        log_error "HurdCog build failed"
        exit 1
    }
    
    make install || {
        log_error "HurdCog install failed"
        exit 1
    }
    
    log_success "HurdCog built and installed"
    cd ../../../..
fi

# Layer 3: OpenCog Collection
if [ $BUILD_OCC -eq 1 ]; then
    log_info "Layer 3: Building OpenCog Collection..."
    
    mkdir -p "$BUILD_DIR/occ-build"
    cd "$BUILD_DIR/occ-build"
    
    log_info "  Configuring CMake..."
    cmake -DCMAKE_BUILD_TYPE="$CMAKE_BUILD_TYPE" \
           -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
           -DBUILD_COGUTIL=ON \
           -DBUILD_ATOMSPACE=ON \
           -DBUILD_ATOMSPACE_STORAGE=ON \
           -DBUILD_COGSERVER=ON \
           -DBUILD_LEARN=ON \
           -DBUILD_AGENTS=ON \
           -DBUILD_ATTENTION=ON \
           -DBUILD_ASMOSES=ON \
           -DBUILD_ATOMSPACE_EXTENSIONS=ON \
           -DBUILD_ATOMSPACE_ACCELERATOR=ON \
           -DBUILD_AGENTIC_CHATBOTS=ON \
           .. || {
        log_error "CMake configuration failed"
        exit 1
    }
    
    log_info "  Building OpenCog Collection..."
    make -j$PARALLEL_JOBS || {
        log_error "OpenCog Collection build failed"
        exit 1
    }
    
    log_info "  Installing OpenCog Collection..."
    make install || {
        log_error "OpenCog Collection install failed"
        exit 1
    }
    
    log_success "OpenCog Collection built and installed"
    cd ../..
fi

# Layer 4: CogBolt AI-Powered IDE Core
if [ $BUILD_COGBOLT -eq 1 ]; then
    log_info "Layer 4: Building CogBolt AI-Powered IDE Core..."
    
    if [ ! -d "cogbolt" ]; then
        log_error "CogBolt directory not found at cogbolt/"
        exit 1
    fi
    
    mkdir -p "$BUILD_DIR/cogbolt-build"
    cd "$BUILD_DIR/cogbolt-build"
    
    log_info "  Configuring CogBolt..."
    cmake "$BUILD_DIR/../cogbolt" \
           -DCMAKE_BUILD_TYPE="$CMAKE_BUILD_TYPE" \
           -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" || {
        log_error "CogBolt CMake configuration failed"
        exit 1
    }
    
    log_info "  Building CogBolt..."
    make -j$PARALLEL_JOBS || {
        log_error "CogBolt build failed"
        exit 1
    }
    
    log_info "  Installing CogBolt..."
    make install || {
        log_error "CogBolt install failed"
        exit 1
    }
    
    log_success "CogBolt built and installed"
    cd ../..
fi

# Summary
log_success "AGI-OS build completed successfully!"
log_info ""
log_info "Installation summary:"
log_info "  Cognumach: $BUILD_COGNUMACH"
log_info "  HurdCog: $BUILD_HURDCOG"
log_info "  OpenCog Collection: $BUILD_OCC"
log_info "  CogBolt IDE Core: $BUILD_COGBOLT"
log_info "  Install prefix: $INSTALL_PREFIX"
log_info ""
log_info "To use the installed components, add to your environment:"
log_info "  export PATH=$INSTALL_PREFIX/bin:\$PATH"
log_info "  export LD_LIBRARY_PATH=$INSTALL_PREFIX/lib:\$LD_LIBRARY_PATH"
log_info "  export PKG_CONFIG_PATH=$INSTALL_PREFIX/lib/pkgconfig:\$PKG_CONFIG_PATH"
