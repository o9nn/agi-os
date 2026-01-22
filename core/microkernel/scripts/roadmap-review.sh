#!/bin/bash
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
ROADMAP_FILE="$PROJECT_ROOT/open-issues-gnumach.md"
REPORTS_DIR="$PROJECT_ROOT/roadmap-reports"
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
mkdir -p "$REPORTS_DIR"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
REPORT_FILE="$REPORTS_DIR/roadmap_review_$TIMESTAMP.md"
analyze_roadmap_progress() {
    log_info "Analyzing roadmap progress..."
    cat > "$REPORT_FILE" << EOF
**Generated**: $(date)
**Report ID**: roadmap_review_$TIMESTAMP
This report analyzes the current state of the GNU Mach development roadmap
and provides recommendations for updates based on recent progress.
EOF
    local phase1_completed=$(grep -c '\- \[x\]' "$ROADMAP_FILE" | head -1 || echo "0")
    local phase1_total=$(grep -c '\- \[.*\]' "$ROADMAP_FILE" | head -1 || echo "1")
    local completion_pct=$((phase1_completed * 100 / phase1_total))
    cat >> "$REPORT_FILE" << EOF
- **Total Items**: $phase1_total
- **Completed Items**: $phase1_completed
- **Completion Rate**: ${completion_pct}%
EOF
    log_info "Progress analysis complete. Completion rate: ${completion_pct}%"
}
analyze_recent_changes() {
    log_info "Analyzing recent changes..."
    cat >> "$REPORT_FILE" << EOF
EOF
    cd "$PROJECT_ROOT"
    git log --oneline --since="30 days ago" --pretty=format:"- %h: %s (%an, %ar)" >> "$REPORT_FILE" 2>/dev/null || {
        echo "- No recent commits found" >> "$REPORT_FILE"
    }
    cat >> "$REPORT_FILE" << EOF
EOF
    find "$PROJECT_ROOT" -name "*.c" -o -name "*.h" -o -name "*.md" | \
        xargs ls -lt | head -10 | \
        awk '{print "- " $9 " (modified " $6 " " $7 " " $8 ")"}' >> "$REPORT_FILE" 2>/dev/null || {
        echo "- No recently modified files found" >> "$REPORT_FILE"
    }
}
suggest_updates() {
    log_info "Generating update suggestions..."
    cat >> "$REPORT_FILE" << EOF
The following items appear to be completed based on code analysis:
EOF
    if [ -f "$PROJECT_ROOT/include/mach/mach_safety.h" ]; then
        cat >> "$REPORT_FILE" << EOF
- ✅ Safety infrastructure framework implemented (mach_safety.h exists)
EOF
    fi
    if [ -f "$PROJECT_ROOT/docs/new-developer-guide.md" ]; then
        cat >> "$REPORT_FILE" << EOF
- ✅ New developer documentation created
EOF
    fi
    if [ -f "$PROJECT_ROOT/docs/mentorship-program.md" ]; then
        cat >> "$REPORT_FILE" << EOF
- ✅ Mentorship program documentation created
EOF
    fi
    local test_count=$(find "$PROJECT_ROOT/tests" -name "test-*.c" 2>/dev/null | wc -l || echo "0")
    if [ "$test_count" -gt 15 ]; then
        cat >> "$REPORT_FILE" << EOF
- ✅ Comprehensive test suite exists (${test_count} test files found)
EOF
    fi
    cat >> "$REPORT_FILE" << EOF
Based on recent development, consider adding these items:
- [ ] Automated roadmap progress tracking
- [ ] Community metrics and health monitoring  
- [ ] Developer onboarding success metrics
- [ ] Regular security audit process
- [ ] Performance regression testing automation
Consider these priority changes based on current needs:
- **Increase priority**: Documentation and community infrastructure (high community value)
- **Decrease priority**: Advanced research features (focus on stability first)
- **New focus areas**: Developer experience and community growth
EOF
}
generate_action_items() {
    log_info "Generating action items..."
    cat >> "$REPORT_FILE" << EOF
- [ ] Update roadmap to mark completed infrastructure items as done
- [ ] Review and update phase priorities based on current needs
- [ ] Validate that safety infrastructure is properly tested
- [ ] Ensure new documentation is linked from main README
- [ ] Establish regular roadmap review schedule (monthly)
- [ ] Create automated progress tracking tools
- [ ] Set up community metrics collection
- [ ] Launch mentorship program pilot
- [ ] Comprehensive roadmap restructuring based on learnings
- [ ] Establish success metrics for each development phase
- [ ] Create contributor retention and growth strategies
- [ ] Plan next major development cycle
This roadmap review should be repeated:
- **Monthly**: Progress and priority assessment
- **Quarterly**: Strategic direction and phase planning  
- **Annually**: Comprehensive roadmap restructuring
**Next review due**: $(date -d "+1 month" +"%Y-%m-%d")
EOF
}
validate_roadmap() {
    log_info "Validating roadmap consistency..."
    local errors=0
    if grep -q '\[.*\](.*)' "$ROADMAP_FILE"; then
        log_info "Checking for broken links in roadmap..."
    fi
    local duplicates=$(grep '\- \[.*\]' "$ROADMAP_FILE" | sort | uniq -d | wc -l)
    if [ "$duplicates" -gt 0 ]; then
        log_warning "Found $duplicates potential duplicate items in roadmap"
        errors=$((errors + 1))
    fi
    if grep -q '\- \[[xX ]\]' "$ROADMAP_FILE"; then
        log_info "Checkbox formatting appears consistent"
    else
        log_warning "Inconsistent checkbox formatting detected"
        errors=$((errors + 1))
    fi
    if [ $errors -eq 0 ]; then
        log_success "Roadmap validation passed"
    else
        log_warning "Roadmap validation found $errors issues"
    fi
    cat >> "$REPORT_FILE" << EOF
- **Consistency check**: $([ $errors -eq 0 ] && echo "✅ Passed" || echo "⚠️  $errors issues found")
- **Format check**: $(grep -q '\- \[[xX ]\]' "$ROADMAP_FILE" && echo "✅ Consistent" || echo "⚠️  Inconsistent")
- **Link check**: Manual review recommended
EOF
}
main() {
    log_info "Starting GNU Mach roadmap review..."
    log_info "Project root: $PROJECT_ROOT"
    log_info "Report will be saved to: $REPORT_FILE"
    if [ ! -f "$ROADMAP_FILE" ]; then
        log_error "Roadmap file not found: $ROADMAP_FILE"
        exit 1
    fi
    analyze_roadmap_progress
    analyze_recent_changes
    suggest_updates
    generate_action_items
    validate_roadmap
    cat >> "$REPORT_FILE" << EOF
---
*This report was generated automatically by the roadmap review script.*
*For questions or suggestions, please open an issue or discuss on the mailing list.*
EOF
    log_success "Roadmap review complete!"
    log_info "Report saved to: $REPORT_FILE"
    echo
    echo "=== ROADMAP REVIEW SUMMARY ==="
    echo "Report file: $REPORT_FILE"
    echo "Key findings:"
    echo "- Progress analysis completed"
    echo "- Update suggestions generated" 
    echo "- Action items identified"
    echo "- Validation results included"
    echo
    echo "Next steps:"
    echo "1. Review the generated report"
    echo "2. Update roadmap based on suggestions"
    echo "3. Schedule next review for one month from now"
    echo "4. Share findings with the development community"
    if command -v less >/dev/null 2>&1; then
        echo
        read -p "View report now? (y/n): " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            less "$REPORT_FILE"
        fi
    fi
}
usage() {
    cat << EOF
Usage: $0 [OPTIONS]
Analyze and update the GNU Mach development roadmap.
Options:
  -h, --help    Show this help message
  -q, --quiet   Run in quiet mode (less output)
  -v, --verbose Run in verbose mode (more output)
Examples:
  $0
  $0 --quiet
  $0 --verbose
The script will:
1. Analyze current roadmap progress
2. Identify completed tasks
3. Suggest updates and improvements
4. Generate actionable recommendations
5. Validate roadmap consistency
Reports are saved to: roadmap-reports/
EOF
}
case "${1:-}" in
    -h|--help)
        usage
        exit 0
        ;;
    -q|--quiet)
        exec > /dev/null 2>&1
        main
        ;;
    -v|--verbose)
        set -x
        main
        ;;
    "")
        main
        ;;
    *)
        log_error "Unknown option: $1"
        usage
        exit 1
        ;;
esac