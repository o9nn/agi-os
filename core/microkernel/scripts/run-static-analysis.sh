#!/bin/bash
set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
OUTPUT_DIR="${PROJECT_ROOT}/analysis-reports"
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'
usage() {
    cat << EOF
Usage: $0 [OPTIONS]
Run static analysis tools on GNU Mach codebase with debugging focus.
OPTIONS:
    -h, --help              Show this help message
    -o, --output DIR        Output directory for reports (default: ./analysis-reports)
    --debug-focus          Focus analysis on debugging-related code
    --timestamp-focus      Focus analysis on timestamp functionality
    --security-focus       Focus analysis on security infrastructure
EOF
}
DEBUG_FOCUS=false
TIMESTAMP_FOCUS=false
SECURITY_FOCUS=false
while [[ $
    case $1 in
        -h|--help)
            usage
            exit 0
            ;;
        -o|--output)
            OUTPUT_DIR="$2"
            shift 2
            ;;
        --debug-focus)
            DEBUG_FOCUS=true
            shift
            ;;
        --timestamp-focus)
            TIMESTAMP_FOCUS=true
            shift
            ;;
        --security-focus)
            SECURITY_FOCUS=true
            shift
            ;;
        *)
            echo "Unknown option: $1" >&2
            usage >&2
            exit 1
            ;;
    esac
done
cd "$PROJECT_ROOT"
mkdir -p "$OUTPUT_DIR"
echo -e "${BLUE}=== GNU Mach Enhanced Static Analysis ===${NC}"
echo "Running static analysis tools to identify code quality issues..."
echo "Project: $PROJECT_ROOT"
echo "Output:  $OUTPUT_DIR"
echo
check_tool() {
    if ! command -v "$1" &> /dev/null; then
        echo -e "${YELLOW}Warning: $1 not found. Please install it for complete analysis.${NC}"
        return 1
    fi
    return 0
}
if [[ "$DEBUG_FOCUS" == "true" ]]; then
    FILE_PATTERNS="ddb/ kern/debug.c kern/printf.c i386/i386/db_*.c i386/i386/kttd_*.c"
    echo -e "${YELLOW}Focus: Debugging infrastructure${NC}"
elif [[ "$TIMESTAMP_FOCUS" == "true" ]]; then
    FILE_PATTERNS="kern/printf.c tests/test-console-timestamp.c"
    echo -e "${YELLOW}Focus: Timestamp functionality${NC}"
elif [[ "$SECURITY_FOCUS" == "true" ]]; then
    FILE_PATTERNS="kern/security_monitor.c kern/cfi_integrity.c include/mach/mach_security.h include/mach/mach_safety.h i386/i386/trap.c vm/vm_kern.c"
    echo -e "${YELLOW}Focus: Security infrastructure${NC}"
else
    FILE_PATTERNS="kern/ ddb/ device/ ipc/ vm/ i386/"
    echo -e "${YELLOW}Scope: Full codebase analysis${NC}"
fi
if [ ! -x ./configure ]; then
    echo "'configure' not found. Bootstrapping autotools (autoreconf)..."
    if check_tool autoreconf; then
        autoreconf -fi || true
    else
        echo "Warning: autoreconf not available; skipping configure generation."
    fi
fi
if check_tool cppcheck; then
    echo -e "${BLUE}=== Running cppcheck ===${NC}"
    cppcheck --enable=all --error-exitcode=0 \
             --suppress=missingIncludeSystem \
             --suppress=unmatchedSuppression \
             --inline-suppr \
             -I include -I i386/include -I x86_64/include \
             $FILE_PATTERNS 2>&1 | tee "$OUTPUT_DIR/cppcheck-report.txt"
    echo -e "${GREEN}Cppcheck report saved to $OUTPUT_DIR/cppcheck-report.txt${NC}"
    echo
fi
if check_tool clang; then
    echo -e "${BLUE}=== Running clang static analyzer ===${NC}"
    mkdir -p "$OUTPUT_DIR/build-analyze"
    cd "$OUTPUT_DIR/build-analyze"
    if check_tool scan-build; then
        if [ -x "$PROJECT_ROOT/configure" ]; then
            scan-build "$PROJECT_ROOT/configure" --host=i686-gnu || true
            scan-build -o scan-results make -j"$(nproc)" || true
            echo -e "${GREEN}Clang static analyzer results saved in $OUTPUT_DIR/build-analyze/scan-results/${NC}"
        else
            echo "Skipping scan-build configure step (no configure script)."
        fi
    else
        echo -e "${YELLOW}scan-build not found. Install clang-tools for static analysis.${NC}"
    fi
    cd "$PROJECT_ROOT"
    echo
fi
if [[ "$DEBUG_FOCUS" == "true" || "$TIMESTAMP_FOCUS" == "true" || "$SECURITY_FOCUS" == "true" ]]; then
    echo -e "${BLUE}=== Running specialized checks ===${NC}"
    {
        if [[ "$SECURITY_FOCUS" == "true" ]]; then
            echo "=== GNU Mach Security Analysis Report ==="
        else
            echo "=== GNU Mach Debugging Analysis Report ==="
        fi
        echo "Generated: $(date)"
        echo
        echo "=== Console Timestamp Implementation ==="
        echo "Files with timestamp functionality:"
        grep -r "console_timestamp" . --include="*.c" --include="*.h" || echo "None found"
        echo
        echo "=== Printf Usage Analysis ==="
        echo "Total printf calls:"
        grep -r "printf\s*(" . --include="*.c" | wc -l || echo "0"
        echo
        echo "=== Error Handling Patterns ==="
        echo "Panic calls:"
        grep -r "panic\s*(" . --include="*.c" | wc -l || echo "0"
        echo
    } > "$OUTPUT_DIR/debug-analysis.txt"
    echo -e "${GREEN}Debug analysis report saved to $OUTPUT_DIR/debug-analysis.txt${NC}"
fi
echo -e "${BLUE}=== Checking for compiler warnings ===${NC}"
echo "Building with enhanced warnings to identify issues..."
mkdir -p "$OUTPUT_DIR/build-warnings"
cd "$OUTPUT_DIR/build-warnings"
if [ -x "$PROJECT_ROOT/configure" ]; then
    "$PROJECT_ROOT/configure" --host=i686-gnu --enable-kdb CFLAGS="-g -O2 -Wall -Wextra" 2>&1 | tee ../configure-warnings.txt || true
    make -j"$(nproc)" 2>&1 | tee ../compiler-warnings.txt || true
else
    echo "No configure script; skipping build with warnings." | tee ../compiler-warnings.txt
fi
cd "$PROJECT_ROOT"
echo -e "${GREEN}Compiler warnings saved to $OUTPUT_DIR/compiler-warnings.txt${NC}"
echo
echo -e "${GREEN}=== Static analysis complete ===${NC}"
echo "Review the following files for issues:"
echo "  - $OUTPUT_DIR/cppcheck-report.txt"
echo "  - $OUTPUT_DIR/compiler-warnings.txt"
echo "  - $OUTPUT_DIR/build-analyze/scan-results/ (if clang analyzer was run)"
if [[ "$DEBUG_FOCUS" == "true" || "$TIMESTAMP_FOCUS" == "true" || "$SECURITY_FOCUS" == "true" ]]; then
    echo "  - $OUTPUT_DIR/debug-analysis.txt"
fi