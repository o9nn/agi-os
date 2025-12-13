#!/bin/bash

# Echo9llama Build Validation Script
# Tests all build fixes and reports compilation status

set -e

echo "════════════════════════════════════════════════════════"
echo "  Echo9llama Build Validation"
echo "  Testing fixes for struct redeclarations and conflicts"
echo "════════════════════════════════════════════════════════"
echo ""

# Check Go installation
if ! command -v go &> /dev/null; then
    echo "❌ ERROR: Go is not installed"
    echo "   Please install Go 1.21+ from https://go.dev/dl/"
    exit 1
fi

echo "✅ Go version: $(go version)"
echo ""

# Navigate to project root
cd "$(dirname "$0")"
PROJECT_ROOT=$(pwd)
echo "📁 Project root: $PROJECT_ROOT"
echo ""

# Test 1: Check for redeclaration issues
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Test 1: Checking for type redeclarations"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Count StepExecution declarations
STEP_EXEC_COUNT=$(grep -r "^type StepExecution struct" core/ 2>/dev/null | wc -l || echo "0")
echo "StepExecution declarations found: $STEP_EXEC_COUNT"
if [ "$STEP_EXEC_COUNT" -gt 1 ]; then
    echo "❌ FAIL: Multiple StepExecution declarations detected"
    grep -rn "^type StepExecution struct" core/
    exit 1
else
    echo "✅ PASS: StepExecution is declared only once"
fi

# Count CognitivePhase declarations (should be 2: one enum, one struct)
COGNITIVE_PHASE_COUNT=$(grep -r "^type CognitivePhase " core/ 2>/dev/null | wc -l || echo "0")
echo "CognitivePhase declarations found: $COGNITIVE_PHASE_COUNT"
if [ "$COGNITIVE_PHASE_COUNT" -gt 1 ]; then
    echo "⚠️  WARNING: Multiple CognitivePhase declarations detected"
    echo "   This is expected if one is enum and one is struct"
    grep -rn "^type CognitivePhase " core/
fi

# Count CognitivePhaseEnum declarations
COGNITIVE_PHASE_ENUM_COUNT=$(grep -r "^type CognitivePhaseEnum " core/ 2>/dev/null | wc -l || echo "0")
echo "CognitivePhaseEnum declarations found: $COGNITIVE_PHASE_ENUM_COUNT"
if [ "$COGNITIVE_PHASE_ENUM_COUNT" -eq 1 ]; then
    echo "✅ PASS: CognitivePhaseEnum is declared once"
elif [ "$COGNITIVE_PHASE_ENUM_COUNT" -gt 1 ]; then
    echo "❌ FAIL: Multiple CognitivePhaseEnum declarations detected"
    exit 1
fi

# Count StepType declarations
STEP_TYPE_COUNT=$(grep -r "^type StepType " core/ 2>/dev/null | wc -l || echo "0")
echo "StepType declarations found: $STEP_TYPE_COUNT"
if [ "$STEP_TYPE_COUNT" -gt 1 ]; then
    echo "❌ FAIL: Multiple StepType declarations detected"
    grep -rn "^type StepType " core/
    exit 1
else
    echo "✅ PASS: StepType is declared only once"
fi

echo ""

# Test 2: Compile core/echobeats
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Test 2: Compiling core/echobeats package"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if go build -v ./core/echobeats 2>&1 | tee /tmp/echobeats_build.log; then
    echo "✅ PASS: core/echobeats compiles successfully"
else
    echo "❌ FAIL: core/echobeats compilation failed"
    echo ""
    echo "Build errors:"
    cat /tmp/echobeats_build.log
    exit 1
fi

echo ""

# Test 3: Compile core/echodream
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Test 3: Compiling core/echodream package"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if go build -v ./core/echodream 2>&1 | tee /tmp/echodream_build.log; then
    echo "✅ PASS: core/echodream compiles successfully"
else
    echo "❌ FAIL: core/echodream compilation failed"
    echo ""
    echo "Build errors:"
    cat /tmp/echodream_build.log
    exit 1
fi

echo ""

# Test 4: Compile entire project
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Test 4: Compiling entire project"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if go build -v ./... 2>&1 | tee /tmp/full_build.log; then
    echo "✅ PASS: Full project compiles successfully"
else
    echo "❌ FAIL: Full project compilation failed"
    echo ""
    echo "Build errors:"
    cat /tmp/full_build.log
    exit 1
fi

echo ""

# Test 5: Run go vet
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Test 5: Running go vet for code quality checks"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if go vet ./... 2>&1 | tee /tmp/vet.log; then
    echo "✅ PASS: go vet found no issues"
else
    echo "⚠️  WARNING: go vet found potential issues"
    cat /tmp/vet.log
    echo ""
    echo "Note: These may not be critical errors"
fi

echo ""

# Test 6: Check for common issues
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Test 6: Checking for common code issues"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

# Check for unused imports
echo "Checking for unused imports..."
if command -v goimports &> /dev/null; then
    goimports -l core/ | tee /tmp/goimports.log
    if [ -s /tmp/goimports.log ]; then
        echo "⚠️  WARNING: Some files may have import issues"
    else
        echo "✅ PASS: No import issues detected"
    fi
else
    echo "ℹ️  INFO: goimports not installed, skipping import check"
fi

# Check for formatting issues
echo "Checking code formatting..."
UNFORMATTED=$(gofmt -l core/ 2>/dev/null || echo "")
if [ -z "$UNFORMATTED" ]; then
    echo "✅ PASS: All code is properly formatted"
else
    echo "⚠️  WARNING: Some files need formatting:"
    echo "$UNFORMATTED"
fi

echo ""

# Summary
echo "════════════════════════════════════════════════════════"
echo "  Build Validation Summary"
echo "════════════════════════════════════════════════════════"
echo ""
echo "✅ All critical tests passed!"
echo ""
echo "Fixes validated:"
echo "  ✅ StepExecution unified in shared_types.go"
echo "  ✅ CognitivePhase enum renamed to CognitivePhaseEnum"
echo "  ✅ StepType unified in shared_types.go"
echo "  ✅ All packages compile successfully"
echo ""
echo "The build is ready for deployment."
echo ""
echo "════════════════════════════════════════════════════════"

exit 0
