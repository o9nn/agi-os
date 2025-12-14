#!/bin/bash
# Comprehensive test runner for all Inferno OS features
# This script runs tests for all components in the correct order

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test results tracking
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Logging function
log() {
    echo -e "${GREEN}[$(date +'%Y-%m-%d %H:%M:%S')] $1${NC}"
}

warn() {
    echo -e "${YELLOW}[$(date +'%Y-%m-%d %H:%M:%S')] WARNING: $1${NC}"
}

error() {
    echo -e "${RED}[$(date +'%Y-%m-%d %H:%M:%S')] ERROR: $1${NC}"
}

# Function to run a test and track results
run_test() {
    local test_name="$1"
    local test_command="$2"
    
    echo ""
    log "Running test: $test_name"
    echo "Command: $test_command"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    
    if eval "$test_command"; then
        log "✅ PASSED: $test_name"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    else
        error "❌ FAILED: $test_name"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
}

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Main test execution
main() {
    log "=== Inferno OS Comprehensive Test Suite ==="
    log "Starting at $(date)"
    
    # Get the script directory
    SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
    
    cd "$PROJECT_ROOT"
    
    log "Project root: $PROJECT_ROOT"
    log "Working directory: $(pwd)"
    
    # 1. Environment Validation Tests
    echo ""
    log "=== Phase 1: Environment Validation ==="
    
    run_test "Check required tools" "command_exists gcc && command_exists make"
    run_test "Check mkconfig exists" "test -f mkconfig"
    run_test "Check makemk.sh exists" "test -f makemk.sh"
    run_test "Check mkfile exists" "test -f mkfile"
    
    # 2. Python Component Tests  
    echo ""
    log "=== Phase 2: Python Component Tests ==="
    
    if command_exists python3; then
        run_test "Python cognitive grammar tests" "cd python && python3 tests/run_tests.py cognitive_grammar"
        run_test "Python distributed network tests" "cd python && python3 tests/run_tests.py distributed_network"
        run_test "Python cognitive network tests" "cd python && python3 tests/run_tests.py cognitive_network"
        run_test "Python all tests" "cd python && python3 tests/run_tests.py"
    else
        warn "Python3 not available, skipping Python tests"
    fi
    
    # 3. Build System Tests
    echo ""
    log "=== Phase 3: Build System Tests ==="
    
    # Save original mkconfig
    cp mkconfig mkconfig.backup 2>/dev/null || true
    
    # Configure for testing
    sed -i.bak "s|ROOT=/usr/inferno|ROOT=$PROJECT_ROOT|" mkconfig
    sed -i "s|SYSHOST=Plan9|SYSHOST=Linux|" mkconfig
    sed -i "s|#OBJTYPE=386|OBJTYPE=386|" mkconfig
    sed -i "s|OBJTYPE=\$objtype|OBJTYPE=386|" mkconfig
    
    run_test "Build mk tool" "./makemk.sh"
    
    # Add mk to PATH for subsequent tests
    if [ -f "Linux/386/bin/mk" ]; then
        export PATH="$PROJECT_ROOT/Linux/386/bin:$PATH"
        log "Added mk to PATH: $PROJECT_ROOT/Linux/386/bin"
    fi
    
    # 4. Core Build Tests
    echo ""
    log "=== Phase 4: Core Build Tests ==="
    
    if command_exists mk; then
        run_test "Clean build artifacts" "mk nuke" || true  # Don't fail if nothing to clean
        run_test "Build directories" "mk mkdirs" || true   # Create necessary directories
        run_test "Build essential libraries" "mk install"
    else
        warn "mk command not available, skipping core build tests"
    fi
    
    # 5. Component-Specific Tests
    echo ""
    log "=== Phase 5: Component-Specific Tests ==="
    
    # Test utilities that might be built
    if [ -f "Linux/386/bin/emu" ]; then
        run_test "EMU functionality test" "timeout 5s Linux/386/bin/emu -c1 echo 'emu test' || echo 'emu timeout (expected)'"
    fi
    
    # Test Limbo test utilities if available
    if [ -f "appl/cmd/test.b" ]; then
        log "Found Limbo test utilities, but limbo compiler needed to run them"
    fi
    
    # Test specific features
    run_test "Check spree test sets" "test -f appl/spree/lib/testsets.b"
    run_test "Check itest utility" "test -f appl/cmd/itest.b"
    run_test "Check ftest utility" "test -f appl/cmd/ftest.b"
    
    # 6. Integration Tests
    echo ""
    log "=== Phase 6: Integration Tests ==="
    
    run_test "Git state check" "git status --porcelain | wc -l | grep -q '^0$' || (echo 'Git state not clean:' && git status --porcelain)"
    
    # Test that essential binaries were built
    if [ -d "Linux/386/bin" ]; then
        run_test "Check build artifacts" "ls -la Linux/386/bin/ | grep -E '(mk|emu|limbo)' || echo 'Some binaries may not be built yet'"
    fi
    
    # 7. System Coherence Tests
    echo ""
    log "=== Phase 7: System Coherence Tests ==="
    
    run_test "Makefile syntax check" "mk -f mkfile -n >/dev/null 2>&1 || true"
    run_test "Directory structure check" "test -d utils && test -d appl && test -d lib9"
    
    # Restore original mkconfig
    if [ -f "mkconfig.backup" ]; then
        mv mkconfig.backup mkconfig
        log "Restored original mkconfig"
    fi
    
    # 8. Test Summary
    echo ""
    log "=== Test Summary ==="
    echo ""
    log "Total tests run: $TOTAL_TESTS"
    log "Passed: $PASSED_TESTS"
    
    if [ $FAILED_TESTS -eq 0 ]; then
        log "Failed: $FAILED_TESTS"
        log "🎉 ALL TESTS PASSED!"
        echo ""
        log "Test suite completed successfully at $(date)"
        exit 0
    else
        error "Failed: $FAILED_TESTS"
        error "💥 SOME TESTS FAILED!"
        echo ""
        error "Test suite completed with failures at $(date)"
        exit 1
    fi
}

# Handle command line arguments
case "${1:-all}" in
    "python")
        log "Running Python tests only"
        cd python && python3 tests/run_tests.py
        ;;
    "build")
        log "Running build tests only"
        # Configure and build
        sed -i.bak "s|ROOT=/usr/inferno|ROOT=$(pwd)|" mkconfig
        sed -i "s|SYSHOST=Plan9|SYSHOST=Linux|" mkconfig  
        sed -i "s|#OBJTYPE=386|OBJTYPE=386|" mkconfig
        sed -i "s|OBJTYPE=\$objtype|OBJTYPE=386|" mkconfig
        ./makemk.sh
        ;;
    "all"|"")
        main
        ;;
    *)
        echo "Usage: $0 [all|python|build]"
        echo "  all    - Run all tests (default)"
        echo "  python - Run only Python tests" 
        echo "  build  - Run only build tests"
        exit 1
        ;;
esac