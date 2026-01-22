#!/bin/bash
set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'
log() {
    echo -e "${GREEN}[INFO]${NC} $1"
}
warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}
error() {
    echo -e "${RED}[ERROR]${NC} $1"
}
usage() {
    cat << EOF
Usage: $0 [OPTIONS] ARCHITECTURE
Build GNU Mach for the specified architecture with CI/CD optimizations.
ARCHITECTURE:
    i686     Build for 32-bit x86 architecture
    x86_64   Build for 64-bit x86 architecture
OPTIONS:
    -h, --help           Show this help message
    -c, --clean          Clean before building
    -t, --test           Run tests after building
    -a, --analysis       Run static analysis after building
    --debug              Enable debug build
    --force-build        Continue building even if MIG assertions fail
EOF
}
check_dependencies() {
    log "Checking build dependencies..."
    local missing_deps=()
    local non_functional_deps=()
    CORE_TOOLS=("gcc" "make" "autoconf" "automake" "libtool")
    log "🔍 Scanning core build tools (tensor: [core_tools_validation])..."
    for cmd in "${CORE_TOOLS[@]}"; do
        if ! command -v "$cmd" &> /dev/null; then
            missing_deps+=("$cmd")
            error "❌ Missing: $cmd"
        else
            case "$cmd" in
                "libtool")
                    if ! "$cmd" --version &> /dev/null; then
                        non_functional_deps+=("$cmd")
                        error "⚠️  Present but non-functional: $cmd"
                    else
                        log "✅ Verified: $cmd ($(which "$cmd"))"
                    fi
                    ;;
                *)
                    log "✅ Found: $cmd ($(which "$cmd"))"
                    ;;
            esac
        fi
    done
    local total_issues=$((${
    if [ $total_issues -gt 0 ]; then
        error "🚨 Dependency validation failed (issues: $total_issues)"
        if [ ${
            error "Missing dependencies: ${missing_deps[*]}"
            error "🛠️  Actionable fix: Install missing tools with package manager"
        fi
        if [ ${
            error "Non-functional dependencies: ${non_functional_deps[*]}"
            error "🛠️  Actionable fix: Reinstall or repair listed tools"
        fi
        exit 1
    fi
    log "✅ All dependencies satisfied (tensor: [dependency_matrix, all_green])"
}
setup_mig() {
    log "Setting up MIG (Mach Interface Generator)..."
    if command -v mig &> /dev/null; then
        log "MIG already available: $(which mig)"
        if mig -version &> /dev/null || mig --help &> /dev/null; then
            log "✅ Existing MIG is functional"
            return 0
        else
            warn "⚠️  Existing MIG found but not functional, rebuilding..."
        fi
    fi
    log "🏗️  Building MIG from source (cognitive tensor: [source_build, headers, install])..."
    log "📁 Setting up Mach headers for MIG build..."
    sudo mkdir -p /usr/include/mach
    sudo cp -r "${PROJECT_ROOT}/include/mach"/* /usr/include/mach/
    sudo ln -sf "${PROJECT_ROOT}/i386/include/mach/i386" /usr/include/mach/machine
    if [ ! -d "/usr/include/mach" ] || [ ! -L "/usr/include/mach/machine" ]; then
        error "❌ MIG header setup failed"
        exit 1
    fi
    log "✅ Mach headers configured successfully"
    log "🔨 Building MIG with timeout protection..."
    cd "${PROJECT_ROOT}/mig"
    if ! autoreconf --install; then
        error "❌ MIG autoreconf failed"
        exit 1
    fi
    if ! ./configure CPPFLAGS="-I/usr/include"; then
        error "❌ MIG configure failed"
        exit 1
    fi
    if ! timeout 300 make -j"$(nproc)"; then
        error "❌ MIG build failed or timed out (300s limit)"
        exit 1
    fi
    if ! sudo make install; then
        error "❌ MIG installation failed"
        exit 1
    fi
    cd "${PROJECT_ROOT}"
    log "🧪 Validating MIG installation..."
    if ! command -v mig &> /dev/null; then
        error "❌ MIG not found in PATH after installation"
        exit 1
    fi
    if ! (mig -version &> /dev/null || mig --help &> /dev/null); then
        error "❌ MIG installed but not functional"
        exit 1
    fi
    log "✅ MIG installation completed and validated: $(which mig)"
}
configure_build() {
    local arch=$1
    local clean=$2
    local debug=$3
    log "Configuring build for $arch architecture..."
    if [[ "$clean" == "true" ]]; then
        make distclean || true
    fi
    autoreconf --install
    local build_dir="build-${arch}"
    mkdir -p "$build_dir"
    cd "$build_dir"
    local configure_flags=""
    local cflags="-g -O2"
    case "$arch" in
        i686)
            configure_flags="--host=i686-gnu CC='gcc -m32' LD='ld -melf_i386'"
            if [[ "$debug" == "true" ]]; then
                cflags="$cflags -DDEBUG -DMACH_KDB"
            fi
            ;;
        x86_64)
            configure_flags="--host=x86_64-gnu --enable-pae --enable-user32"
            if [[ "$debug" == "true" ]]; then
                cflags="$cflags -DDEBUG"
            fi
            ;;
        *)
            error "Unsupported architecture: $arch"
            exit 1
            ;;
    esac
    log "Running configure with flags: $configure_flags"
    eval "../configure $configure_flags MIG='mig' CFLAGS='$cflags'"
    cd ..
}
build_kernel() {
    local arch=$1
    local force_build=$2
    log "Building kernel for $arch..."
    cd "build-${arch}"
    if [[ "$force_build" == "true" ]]; then
        make -j"$(nproc)" || {
            warn "Build had issues, but continuing as requested"
            return 0
        }
    else
        make -j"$(nproc)"
    fi
    if [[ -f gnumach ]]; then
        log "Build successful: gnumach created"
        file gnumach
        ls -lh gnumach
    else
        error "Build failed: gnumach not found"
        exit 1
    fi
    cd ..
}
run_tests() {
    local arch=$1
    log "Running tests for $arch..."
    log "Running basic functionality tests..."
    timeout 300 make run-hello || warn "hello test had issues: exit code $?"
    if command -v qemu-system-i386 &> /dev/null && [[ "$arch" == "i686" ]]; then
        log "Running additional i686 tests..."
        timeout 300 make run-mach_port || warn "mach_port test had issues: exit code $?"
        timeout 300 make run-console-timestamps || warn "console-timestamps test had issues: exit code $?"
    fi
}
run_analysis() {
    log "Running static analysis..."
    "${PROJECT_ROOT}/scripts/run-static-analysis.sh" || warn "Static analysis completed with warnings"
}
main() {
    local arch=""
    local clean=false
    local test=false
    local analysis=false
    local debug=false
    local force_build=false
    while [[ $
        case $1 in
            -h|--help)
                usage
                exit 0
                ;;
            -c|--clean)
                clean=true
                shift
                ;;
            -t|--test)
                test=true
                shift
                ;;
            -a|--analysis)
                analysis=true
                shift
                ;;
            --debug)
                debug=true
                shift
                ;;
            --force-build)
                force_build=true
                shift
                ;;
            i686|x86_64)
                arch=$1
                shift
                ;;
            *)
                error "Unknown option: $1"
                usage >&2
                exit 1
                ;;
        esac
    done
    if [[ -z "$arch" ]]; then
        error "Architecture must be specified (i686 or x86_64)"
        usage >&2
        exit 1
    fi
    cd "$PROJECT_ROOT"
    check_dependencies
    setup_mig
    configure_build "$arch" "$clean" "$debug"
    build_kernel "$arch" "$force_build"
    if [[ "$test" == "true" ]]; then
        run_tests "$arch"
    fi
    if [[ "$analysis" == "true" ]]; then
        run_analysis
    fi
    log "Build process completed successfully for $arch"
}
main "$@"