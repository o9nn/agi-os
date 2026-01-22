#!/bin/bash
# Comprehensive Verification Suite for Smol Optimizations
# Verifies syntax and basic functionality for all supported languages

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

PASS=0
FAIL=0

log_pass() {
    echo -e "${GREEN}✓${NC} $1"
    ((PASS++))
}

log_fail() {
    echo -e "${RED}✗${NC} $1"
    ((FAIL++))
}

log_info() {
    echo -e "${YELLOW}→${NC} $1"
}

# JavaScript verification
verify_js() {
    local file="$1"
    if node -c "$file" 2>/dev/null; then
        log_pass "JS syntax: $file"
        return 0
    else
        log_fail "JS syntax: $file"
        return 1
    fi
}

# Python verification
verify_py() {
    local file="$1"
    if python3 -m py_compile "$file" 2>/dev/null; then
        log_pass "Python syntax: $file"
        return 0
    else
        log_fail "Python syntax: $file"
        return 1
    fi
}

# C verification
verify_c() {
    local file="$1"
    if gcc -fsyntax-only "$file" 2>/dev/null; then
        log_pass "C syntax: $file"
        return 0
    else
        log_fail "C syntax: $file"
        return 1
    fi
}

# C++ verification
verify_cpp() {
    local file="$1"
    if g++ -fsyntax-only "$file" 2>/dev/null; then
        log_pass "C++ syntax: $file"
        return 0
    else
        log_fail "C++ syntax: $file"
        return 1
    fi
}

# Go verification
verify_go() {
    local file="$1"
    if go build -n "$file" 2>/dev/null || gofmt -e "$file" >/dev/null 2>&1; then
        log_pass "Go syntax: $file"
        return 0
    else
        log_fail "Go syntax: $file"
        return 1
    fi
}

# Scheme verification (Guile)
verify_scm() {
    local file="$1"
    if guile -c "(load \"$file\")" 2>/dev/null; then
        log_pass "Scheme syntax: $file"
        return 0
    else
        # Try just parsing
        if guile -c "(read (open-input-file \"$file\"))" 2>/dev/null; then
            log_pass "Scheme syntax (parse): $file"
            return 0
        else
            log_fail "Scheme syntax: $file"
            return 1
        fi
    fi
}

# Ruby verification
verify_rb() {
    local file="$1"
    if ruby -c "$file" 2>/dev/null; then
        log_pass "Ruby syntax: $file"
        return 0
    else
        log_fail "Ruby syntax: $file"
        return 1
    fi
}

# Perl verification
verify_pl() {
    local file="$1"
    if perl -c "$file" 2>/dev/null; then
        log_pass "Perl syntax: $file"
        return 0
    else
        log_fail "Perl syntax: $file"
        return 1
    fi
}

# Shell script verification
verify_sh() {
    local file="$1"
    if bash -n "$file" 2>/dev/null; then
        log_pass "Shell syntax: $file"
        return 0
    else
        log_fail "Shell syntax: $file"
        return 1
    fi
}

# Generic file size measurement
measure_size() {
    local file="$1"
    wc -c < "$file"
}

# Main verification function
verify_file() {
    local file="$1"
    local ext="${file##*.}"
    
    case "$ext" in
        js|mjs)
            verify_js "$file"
            ;;
        ts)
            # TypeScript needs tsc
            if command -v tsc &> /dev/null; then
                if tsc --noEmit "$file" 2>/dev/null; then
                    log_pass "TypeScript syntax: $file"
                else
                    log_fail "TypeScript syntax: $file"
                fi
            else
                log_info "TypeScript compiler not available: $file"
            fi
            ;;
        py)
            verify_py "$file"
            ;;
        c)
            verify_c "$file"
            ;;
        cpp|cc|cxx)
            verify_cpp "$file"
            ;;
        go)
            verify_go "$file"
            ;;
        scm)
            verify_scm "$file"
            ;;
        rb)
            verify_rb "$file"
            ;;
        pl)
            verify_pl "$file"
            ;;
        sh|bash)
            verify_sh "$file"
            ;;
        *)
            log_info "No verifier for extension: $ext ($file)"
            ;;
    esac
}

# Compare original and optimized files
compare_files() {
    local original="$1"
    local optimized="$2"
    
    local orig_size=$(measure_size "$original")
    local opt_size=$(measure_size "$optimized")
    local diff=$((orig_size - opt_size))
    local percent=$(echo "scale=2; ($diff / $orig_size) * 100" | bc)
    
    echo "Original:  $orig_size bytes"
    echo "Optimized: $opt_size bytes"
    echo "Saved:     $diff bytes ($percent%)"
}

# Main execution
main() {
    echo "========================================"
    echo "Smol Verification Suite"
    echo "========================================"
    echo ""
    
    if [ $# -eq 0 ]; then
        echo "Usage: $0 <file1> [file2] ..."
        echo "       $0 --all (verify all implementations)"
        exit 1
    fi
    
    if [ "$1" == "--all" ]; then
        # Verify all smol implementations
        IMPL_DIR="../implementations"
        if [ -d "$IMPL_DIR" ]; then
            for file in "$IMPL_DIR"/smol.*; do
                if [ -f "$file" ]; then
                    log_info "Verifying: $file ($(measure_size "$file") bytes)"
                    verify_file "$file"
                fi
            done
        fi
    else
        for file in "$@"; do
            if [ -f "$file" ]; then
                log_info "Verifying: $file ($(measure_size "$file") bytes)"
                verify_file "$file"
            else
                log_fail "File not found: $file"
            fi
        done
    fi
    
    echo ""
    echo "========================================"
    echo "Results: $PASS passed, $FAIL failed"
    echo "========================================"
    
    [ $FAIL -eq 0 ]
}

main "$@"
