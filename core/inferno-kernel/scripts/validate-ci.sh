#!/bin/bash
set -e
echo "=== CI Workflow Validation ==="
echo "Testing core CI components locally..."
echo ""
echo "1. Testing Python components..."
cd python && python3 tests/run_tests.py
cd ..
echo "✅ Python tests passed"
echo ""
echo "2. Testing build script syntax..."
bash -n scripts/build.sh
echo "✅ Build script syntax valid"
echo ""
echo "3. Testing test script syntax..."
bash -n scripts/test-all.sh
echo "✅ Test script syntax valid"
echo ""
echo "4. Testing CI workflow syntax..."
if command -v yamllint >/dev/null 2>&1; then
    yamllint .github/workflows/ci.yml
    echo "✅ CI workflow YAML syntax valid"
else
    echo "⚠️  yamllint not available, skipping YAML validation"
fi
echo ""
echo "5. Testing project structure..."
required_dirs=("appl" "lib9" "utils" "python" "scripts" ".github/workflows")
for dir in "${required_dirs[@]}"; do
    if [ -d "$dir" ]; then
        echo "✅ $dir directory exists"
    else
        echo "❌ $dir directory missing"
        exit 1
    fi
done
echo ""
echo "6. Testing required files..."
required_files=("mkconfig" "makemk.sh" "mkfile" "scripts/build.sh" "scripts/test-all.sh" ".github/workflows/ci.yml")
for file in "${required_files[@]}"; do
    if [ -f "$file" ]; then
        echo "✅ $file exists"
    else
        echo "❌ $file missing"
        exit 1
    fi
done
echo ""
echo "🎉 All CI workflow components validated successfully!"
echo "The CI system is ready for GitHub Actions."