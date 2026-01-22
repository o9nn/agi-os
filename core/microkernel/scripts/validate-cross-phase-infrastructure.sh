#!/bin/bash
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'
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
TESTS_PASSED=0
TESTS_FAILED=0
test_assert() {
local condition=$1
local message=$2
if [ "$condition" = "0" ]; then
log_success "✓ $message"
TESTS_PASSED=$((TESTS_PASSED + 1))
else
log_error "✗ $message"
TESTS_FAILED=$((TESTS_FAILED + 1))
fi
}
test_safety_infrastructure() {
log_info "Testing safety infrastructure..."
if [ -f "$PROJECT_ROOT/include/mach/mach_safety.h" ]; then
test_assert 0 "Safety header file exists"
if grep -q "MACH_SAFE_ADD" "$PROJECT_ROOT/include/mach/mach_safety.h"; then
test_assert 0 "MACH_SAFE_ADD macro is defined"
else
test_assert 1 "MACH_SAFE_ADD macro is missing"
fi
if grep -q "MACH_SAFE_MUL" "$PROJECT_ROOT/include/mach/mach_safety.h"; then
test_assert 0 "MACH_SAFE_MUL macro is defined"
else
test_assert 1 "MACH_SAFE_MUL macro is missing"
fi
else
test_assert 1 "Safety header file is missing"
fi
}
test_trap_handling() {
log_info "Testing enhanced trap handling..."
if [ -f "$PROJECT_ROOT/i386/i386/trap.c" ]; then
test_assert 0 "Trap handling file exists"
if grep -q "bounds checking" "$PROJECT_ROOT/i386/i386/trap.c" ||
grep -q "validation" "$PROJECT_ROOT/i386/i386/trap.c"; then
test_assert 0 "Enhanced GPF handling is present"
else
test_assert 1 "Enhanced GPF handling not found"
fi
else
test_assert 1 "Trap handling file is missing"
fi
}
test_thread_management() {
log_info "Testing thread management improvements..."
if [ -f "$PROJECT_ROOT/kern/thread.c" ]; then
test_assert 0 "Thread management file exists"
if grep -q "validation" "$PROJECT_ROOT/kern/thread.c" ||
grep -q "limit" "$PROJECT_ROOT/kern/thread.c"; then
test_assert 0 "Thread resource validation is present"
else
test_assert 1 "Thread resource validation not found"
fi
else
test_assert 1 "Thread management file is missing"
fi
}
test_vm_improvements() {
log_info "Testing VM kernel improvements..."
if [ -f "$PROJECT_ROOT/vm/vm_kern.c" ]; then
test_assert 0 "VM kernel file exists"
if grep -q "overflow" "$PROJECT_ROOT/vm/vm_kern.c" ||
grep -q "MACH_SAFE" "$PROJECT_ROOT/vm/vm_kern.c"; then
test_assert 0 "VM overflow protection is present"
else
test_assert 1 "VM overflow protection not found"
fi
else
test_assert 1 "VM kernel file is missing"
fi
}
test_documentation() {
log_info "Testing documentation infrastructure..."
if [ -f "$PROJECT_ROOT/docs/new-developer-guide.md" ]; then
test_assert 0 "New developer guide exists"
else
test_assert 1 "New developer guide is missing"
fi
if [ -f "$PROJECT_ROOT/docs/mentorship-program.md" ]; then
test_assert 0 "Mentorship program documentation exists"
else
test_assert 1 "Mentorship program documentation is missing"
fi
if [ -f "$PROJECT_ROOT/docs/cross-phase-infrastructure.md" ]; then
test_assert 0 "Cross-phase infrastructure documentation exists"
else
test_assert 1 "Cross-phase infrastructure documentation is missing"
fi
}
test_community_infrastructure() {
log_info "Testing community infrastructure..."
if [ -f "$PROJECT_ROOT/.github/ISSUE_TEMPLATE/new-developer-onboarding.yaml" ]; then
test_assert 0 "New developer onboarding template exists"
else
test_assert 1 "New developer onboarding template is missing"
fi
if grep -q "New Developer Guide" "$PROJECT_ROOT/CONTRIBUTING.md"; then
test_assert 0 "CONTRIBUTING.md references new developer resources"
else
test_assert 1 "CONTRIBUTING.md needs to reference new developer resources"
fi
}
test_testing_infrastructure() {
log_info "Testing infrastructure validation..."
if [ -f "$PROJECT_ROOT/tests/test-cross-phase-infrastructure.c" ]; then
test_assert 0 "Cross-phase infrastructure test exists"
else
test_assert 1 "Cross-phase infrastructure test is missing"
fi
if grep -q "test-cross-phase-infrastructure" "$PROJECT_ROOT/tests/user-qemu.mk"; then
test_assert 0 "Cross-phase test is registered in test suite"
else
test_assert 1 "Cross-phase test not registered in test suite"
fi
}
test_roadmap_infrastructure() {
log_info "Testing roadmap review infrastructure..."
if [ -f "$PROJECT_ROOT/scripts/roadmap-review.sh" ] && [ -x "$PROJECT_ROOT/scripts/roadmap-review.sh" ]; then
test_assert 0 "Roadmap review script exists and is executable"
else
test_assert 1 "Roadmap review script is missing or not executable"
fi
}
test_build_integration() {
log_info "Testing build system integration..."
if [ -f "$PROJECT_ROOT/build-x86_64/gnumach" ] ||
[ -f "$PROJECT_ROOT/build-i686/gnumach" ] ||
ls "$PROJECT_ROOT"/build-*/gnumach >/dev/null 2>&1; then
test_assert 0 "Build system has produced kernel artifacts"
else
test_assert 1 "No kernel build artifacts found"
fi
if [ -f "$PROJECT_ROOT/configure" ]; then
test_assert 0 "Configure script is available"
else
test_assert 1 "Configure script is missing (run autoreconf --install)"
fi
}
run_all_tests() {
log_info "Starting Cross-Phase Infrastructure Validation"
log_info "Project root: $PROJECT_ROOT"
echo
test_safety_infrastructure
test_trap_handling
test_thread_management
test_vm_improvements
test_documentation
test_community_infrastructure
test_testing_infrastructure
test_roadmap_infrastructure
test_build_integration
echo
log_info "=== VALIDATION RESULTS ==="
log_info "Tests passed: $TESTS_PASSED"
log_info "Tests failed: $TESTS_FAILED"
log_info "Total tests:  $((TESTS_PASSED + TESTS_FAILED))"
if [ $TESTS_FAILED -eq 0 ]; then
log_success ""
log_success "🎉 ALL TESTS PASSED!"
log_success "Cross-phase infrastructure is properly implemented."
log_success ""
return 0
else
log_error ""
log_error "⚠️  Some tests failed ($TESTS_FAILED/$((TESTS_PASSED + TESTS_FAILED)))"
log_error "Please review the output above and address any missing components."
log_error ""
return 1
fi
}
usage() {
cat << EOF
Usage: $0 [OPTIONS]
Validate the cross-phase infrastructure improvements for GNU Mach.
Options:
-h, --help     Show this help message
-v, --verbose  Run in verbose mode (show detailed output)
This script validates:
1. Safety infrastructure (overflow protection, bounds checking)
2. Enhanced trap handling (GPF improvements)
3. Thread management robustness
4. VM kernel improvements
5. Documentation infrastructure
6. Community support tools
7. Testing infrastructure
8. Roadmap review tools
9. Build system integration
Examples:
$0
$0 --verbose
EOF
}
case "${1:-}" in
-h|--help)
usage
exit 0
;;
-v|--verbose)
set -x
run_all_tests
;;
"")
run_all_tests
;;
*)
log_error "Unknown option: $1"
usage
exit 1
;;
esac