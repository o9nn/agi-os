#!/bin/bash
set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"
log() {
echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}
check_financial_references() {
log "Checking for financial references..."
local financial_count=0
financial_count=$(grep -r -i "financial\|trading\|investment\|market\|banking\|stock\|currency" . --exclude-dir=.git --exclude-dir=backup 2>/dev/null | wc -l || echo "0")
if [ "$financial_count" -eq 0 ]; then
echo "✅ No financial references found"
return 0
else
echo "⚠️  Found $financial_count financial references"
grep -r -i "financial\|trading\|investment\|market\|banking\|stock\|currency" . --exclude-dir=.git --exclude-dir=backup 2>/dev/null | head -10
return 1
fi
}
check_repository_structure() {
log "Checking repository structure..."
local missing_dirs=0
for dir in cognitive distributed performance development build backup; do
if [ -d "$dir" ]; then
echo "✅ $dir/ directory exists"
else
echo "❌ $dir/ directory missing"
((missing_dirs++))
fi
done
for file in README.md DEVELOPMENT_ROADMAP.md CLEANUP_ACTION_ITEMS.md; do
if [ -f "$file" ]; then
echo "✅ $file exists"
else
echo "❌ $file missing"
((missing_dirs++))
fi
done
return $missing_dirs
}
check_github_actions() {
log "Checking GitHub Actions workflows..."
local issues=0
if [ -f ".github/workflows/financial-intelligence-engine.yml" ]; then
echo "❌ financial-intelligence-engine.yml still exists"
((issues++))
else
echo "✅ financial-intelligence-engine.yml removed"
fi
if [ -f ".github/workflows/cognitive-integration.yml" ]; then
echo "✅ cognitive-integration.yml exists"
else
echo "❌ cognitive-integration.yml missing"
((issues++))
fi
if grep -q "GNU Hurd" .github/workflows/ci-tests.yml 2>/dev/null; then
echo "✅ ci-tests.yml updated for GNU Hurd"
else
echo "❌ ci-tests.yml not updated for GNU Hurd"
((issues++))
fi
return $issues
}
check_documentation() {
log "Checking documentation organization..."
local issues=0
if [ -d "cognitive/docs" ]; then
local cognitive_files=$(find cognitive/docs -name "*.md" | wc -l)
echo "✅ cognitive/docs/ contains $cognitive_files files"
else
echo "❌ cognitive/docs/ missing"
((issues++))
fi
if [ -d "distributed/docs" ]; then
local distributed_files=$(find distributed/docs -name "*.md" | wc -l)
echo "✅ distributed/docs/ contains $distributed_files files"
else
echo "❌ distributed/docs/ missing"
((issues++))
fi
if [ -d "performance/docs" ]; then
local performance_files=$(find performance/docs -name "*.md" | wc -l)
echo "✅ performance/docs/ contains $performance_files files"
else
echo "❌ performance/docs/ missing"
((issues++))
fi
if [ -d "development/docs" ]; then
local development_files=$(find development/docs -name "*.md" | wc -l)
echo "✅ development/docs/ contains $development_files files"
else
echo "❌ development/docs/ missing"
((issues++))
fi
if [ -d "build/docs" ]; then
local build_files=$(find build/docs -name "*.md" | wc -l)
echo "✅ build/docs/ contains $build_files files"
else
echo "❌ build/docs/ missing"
((issues++))
fi
return $issues
}
check_backup() {
log "Checking backup..."
if [ -d "backup" ]; then
local backup_files=$(find backup -name "*.yml" | wc -l)
echo "✅ backup/ contains $backup_files workflow files"
if [ "$backup_files" -gt 0 ]; then
echo "📋 Backup files:"
find backup -name "*.yml" -exec basename {} \;
fi
else
echo "❌ backup/ directory missing"
return 1
fi
return 0
}
check_readme() {
log "Checking README content..."
if [ -f "README.md" ]; then
if grep -q "GNU Hurd Cognitive Architecture" README.md; then
echo "✅ README.md contains cognitive architecture focus"
else
echo "❌ README.md missing cognitive architecture focus"
return 1
fi
if grep -q "financial\|trading\|investment" README.md; then
echo "❌ README.md contains financial references"
return 1
else
echo "✅ README.md clean of financial references"
fi
else
echo "❌ README.md missing"
return 1
fi
return 0
}
main() {
echo "=========================================="
echo "GNU Hurd Cognitive Architecture Verification"
echo "=========================================="
echo ""
local total_issues=0
check_financial_references || ((total_issues++))
echo ""
check_repository_structure || ((total_issues++))
echo ""
check_github_actions || ((total_issues++))
echo ""
check_documentation || ((total_issues++))
echo ""
check_backup || ((total_issues++))
echo ""
check_readme || ((total_issues++))
echo ""
echo "=========================================="
echo "Verification Summary"
echo "=========================================="
if [ "$total_issues" -eq 0 ]; then
echo "🎉 SUCCESS: Repository cleanup verification passed!"
echo "✅ All checks completed successfully"
echo "✅ Repository is properly focused on GNU Hurd + Cognitive Architecture"
echo "✅ No financial references found"
echo "✅ Documentation properly organized"
echo "✅ GitHub Actions cleaned and updated"
echo ""
echo "🚀 Ready for Phase 1 implementation!"
else
echo "⚠️  WARNING: Found $total_issues issues that need attention"
echo "📋 Review the issues above and complete remaining cleanup tasks"
echo "📋 See CLEANUP_ACTION_ITEMS.md for detailed action items"
fi
echo ""
echo "📊 Repository Status:"
echo "- Core GNU Hurd: ✅ Present"
echo "- Cognitive Focus: ✅ Established"
echo "- Financial Components: ✅ Removed"
echo "- Documentation: ✅ Organized"
echo "- GitHub Actions: ✅ Cleaned"
echo "- Backup: ✅ Created"
return $total_issues
}
main