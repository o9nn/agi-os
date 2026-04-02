#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_ROOT="${BUILD_DIR:-$REPO_ROOT/build}"
INSTALL_PREFIX="${INSTALL_PREFIX:-/usr/local}"
CMAKE_BUILD_TYPE="${CMAKE_BUILD_TYPE:-Release}"
PARALLEL_JOBS="${PARALLEL_JOBS:-$(nproc)}"

RED='[0;31m'
GREEN='[0;32m'
YELLOW='[1;33m'
BLUE='[0;34m'
NC='[0m'

log_info() { echo -e "${BLUE}[INFO]${NC} $*"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $*"; }
log_warning() { echo -e "${YELLOW}[WARNING]${NC} $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $*"; }

BUILD_MIG=0
BUILD_COGNUMACH=0
BUILD_HURDCOG=0
BUILD_OCC=1
BUILD_COGBOLT=1

usage() {
    cat <<USAGE
AGI-OS Build Script

Usage: ./build-agi-os.sh [OPTIONS]

Options:
  --mig              Build the unified MIG toolchain
  --mig-only         Build only MIG
  --cognumach        Build CogNUMach (implies --mig)
  --hurdcog          Build HurdCog (implies --cognumach and --mig)
  --occ              Build only the OpenCog collection layer
  --occ-only         Build only the OpenCog collection layer
  --cogbolt          Build only CogBolt
  --cogbolt-only     Build only CogBolt
  --all              Build MIG, CogNUMach, HurdCog, OCC, and CogBolt
  --prefix PATH      Installation prefix (default: /usr/local)
  --jobs N           Parallel jobs (default: nproc)
  --help             Display this help text
USAGE
}

resolve_mig_source() {
    local candidate
    for candidate in         "$REPO_ROOT/core/microkernel/cognumach/mig"         "$REPO_ROOT/core/microkernel/mig"         "$REPO_ROOT/core/os/hurdcog/mig"; do
        if [ -d "$candidate" ] && [ -f "$candidate/configure.ac" ]; then
            printf '%s
' "$candidate"
            return 0
        fi
    done
    return 1
}

build_autotools_component() {
    local name="$1"
    local source_dir="$2"
    shift 2
    local build_dir="$source_dir/build"

    if [ ! -d "$source_dir" ]; then
        log_error "$name source directory not found at $source_dir"
        return 1
    fi

    mkdir -p "$build_dir"
    pushd "$source_dir" >/dev/null
    if [ ! -f configure ]; then
        log_info "Running autoreconf for $name"
        autoreconf -fi
    fi
    popd >/dev/null

    pushd "$build_dir" >/dev/null
    log_info "Configuring $name"
    ../configure --prefix="$INSTALL_PREFIX" "$@"
    log_info "Building $name"
    make -j"$PARALLEL_JOBS"
    log_info "Installing $name"
    make install
    popd >/dev/null

    log_success "$name built and installed"
}

build_mig() {
    local mig_source
    local mig_wrapper_dir="$REPO_ROOT/build-tools/mig"
    local mig_build_dir="$BUILD_ROOT/mig"

    if [ -f "$mig_wrapper_dir/CMakeLists.txt" ]; then
        log_info "Layer 0: Building MIG via unified wrapper"
        cmake -S "$mig_wrapper_dir" -B "$mig_build_dir" \
            -DCMAKE_BUILD_TYPE="$CMAKE_BUILD_TYPE" \
            -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX"
        cmake --build "$mig_build_dir" --parallel "$PARALLEL_JOBS"
        cmake --install "$mig_build_dir"
        export PATH="$INSTALL_PREFIX/bin:$PATH"
        log_success "MIG built and installed"
        return 0
    fi

    mig_source="$(resolve_mig_source)" || {
        log_error "Unable to locate MIG source tree"
        exit 1
    }

    log_warning "Unified MIG wrapper not found; falling back to raw autotools source at $mig_source"
    build_autotools_component "MIG" "$mig_source"
    export PATH="$INSTALL_PREFIX/bin:$PATH"
}

build_occ() {
    local occ_build_dir="$BUILD_ROOT/occ"
    mkdir -p "$occ_build_dir"

    log_info "Layer 3: Configuring OpenCog Collection"
    cmake -S "$REPO_ROOT" -B "$occ_build_dir"         -DCMAKE_BUILD_TYPE="$CMAKE_BUILD_TYPE"         -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX"         -DBUILD_INFERNO_KERNEL=OFF         -DBUILD_VORTEX_MORPHULE_EGREGORE=OFF         -DBUILD_ECHO_ANGEL=OFF         -DBUILD_MIG=OFF         -DBUILD_COGNUMACH=OFF         -DBUILD_HURDCOG=OFF         -DBUILD_COGUTIL=ON         -DBUILD_ATOMSPACE=ON         -DBUILD_ATOMSPACE_STORAGE=ON         -DBUILD_COGSERVER=ON         -DBUILD_LEARN=ON         -DBUILD_AGENTS=ON         -DBUILD_ATTENTION=ON         -DBUILD_ASMOSES=ON         -DBUILD_MOSES=ON         -DBUILD_URE=ON         -DBUILD_PLN=ON         -DBUILD_MINER=ON         -DBUILD_UNIFY=ON         -DBUILD_SPACETIME=ON         -DBUILD_GENERATE=ON         -DBUILD_LG_ATOMESE=ON         -DBUILD_RELEX=OFF         -DBUILD_ATOMSPACE_EXTENSIONS=ON         -DBUILD_ATOMSPACE_ACCELERATOR=ON         -DBUILD_AGENTIC_CHATBOTS=OFF         -DBUILD_APHRODITECHO=ON         -DBUILD_DELTECHO=OFF         -DBUILD_ATOMSPACE_9P=ON         -DBUILD_PLN_9P=ON         -DBUILD_ECAN_9P=ON         -DBUILD_DISTRIBUTED_COGNITION=ON         -DBUILD_KOBOLDCPP_COG=ON         -DBUILD_COGBOLT=OFF

    log_info "Building OpenCog Collection"
    cmake --build "$occ_build_dir" --parallel "$PARALLEL_JOBS"
    log_info "Installing OpenCog Collection"
    cmake --install "$occ_build_dir"
    log_success "OpenCog Collection built and installed"
}

build_cogbolt() {
    local cogbolt_build_dir="$BUILD_ROOT/cogbolt"
    if [ ! -d "$REPO_ROOT/cogbolt" ]; then
        log_error "CogBolt directory not found at $REPO_ROOT/cogbolt"
        exit 1
    fi
    log_info "Layer 4: Building CogBolt"
    cmake -S "$REPO_ROOT/cogbolt" -B "$cogbolt_build_dir"         -DCMAKE_BUILD_TYPE="$CMAKE_BUILD_TYPE"         -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX"
    cmake --build "$cogbolt_build_dir" --parallel "$PARALLEL_JOBS"
    cmake --install "$cogbolt_build_dir"
    log_success "CogBolt built and installed"
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --help)
            usage
            exit 0
            ;;
        --mig|--mig-only)
            BUILD_MIG=1
            BUILD_COGNUMACH=0
            BUILD_HURDCOG=0
            BUILD_OCC=0
            BUILD_COGBOLT=0
            shift
            ;;
        --cognumach)
            BUILD_MIG=1
            BUILD_COGNUMACH=1
            BUILD_HURDCOG=0
            BUILD_OCC=0
            BUILD_COGBOLT=0
            shift
            ;;
        --hurdcog)
            BUILD_MIG=1
            BUILD_COGNUMACH=1
            BUILD_HURDCOG=1
            BUILD_OCC=0
            BUILD_COGBOLT=0
            shift
            ;;
        --occ|--occ-only)
            BUILD_MIG=0
            BUILD_COGNUMACH=0
            BUILD_HURDCOG=0
            BUILD_OCC=1
            BUILD_COGBOLT=0
            shift
            ;;
        --cogbolt|--cogbolt-only)
            BUILD_MIG=0
            BUILD_COGNUMACH=0
            BUILD_HURDCOG=0
            BUILD_OCC=0
            BUILD_COGBOLT=1
            shift
            ;;
        --all)
            BUILD_MIG=1
            BUILD_COGNUMACH=1
            BUILD_HURDCOG=1
            BUILD_OCC=1
            BUILD_COGBOLT=1
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
            usage
            exit 1
            ;;
    esac
done

mkdir -p "$BUILD_ROOT"

log_info "AGI-OS build configuration"
log_info "  Repository root : $REPO_ROOT"
log_info "  Build root      : $BUILD_ROOT"
log_info "  Install prefix  : $INSTALL_PREFIX"
log_info "  Build type      : $CMAKE_BUILD_TYPE"
log_info "  Parallel jobs   : $PARALLEL_JOBS"
log_info "  Build MIG       : $BUILD_MIG"
log_info "  Build CogNUMach : $BUILD_COGNUMACH"
log_info "  Build HurdCog   : $BUILD_HURDCOG"
log_info "  Build OCC       : $BUILD_OCC"
log_info "  Build CogBolt   : $BUILD_COGBOLT"

if [ "$BUILD_MIG" -eq 1 ] || [ "$BUILD_COGNUMACH" -eq 1 ] || [ "$BUILD_HURDCOG" -eq 1 ]; then
    build_mig
fi

if [ "$BUILD_COGNUMACH" -eq 1 ]; then
    build_autotools_component "CogNUMach" "$REPO_ROOT/core/microkernel/cognumach"
fi

if [ "$BUILD_HURDCOG" -eq 1 ]; then
    build_autotools_component "HurdCog" "$REPO_ROOT/core/os/hurdcog"
fi

if [ "$BUILD_OCC" -eq 1 ]; then
    build_occ
fi

if [ "$BUILD_COGBOLT" -eq 1 ]; then
    build_cogbolt
fi

log_success "AGI-OS build orchestration completed"
log_info "Add the installed binaries to your environment if needed:"
log_info "  export PATH=$INSTALL_PREFIX/bin:\$PATH"
log_info "  export LD_LIBRARY_PATH=$INSTALL_PREFIX/lib:\$LD_LIBRARY_PATH"
log_info "  export PKG_CONFIG_PATH=$INSTALL_PREFIX/lib/pkgconfig:\$PKG_CONFIG_PATH"
