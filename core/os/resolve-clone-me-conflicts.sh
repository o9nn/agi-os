#!/bin/bash
set -euo pipefail
log() {
echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}
error_exit() {
log "ERROR: $1"
exit 1
}
if ! git rev-parse --git-dir >/dev/null 2>&1; then
error_exit "Not in a git repository"
fi
if ! git ls-files -u >/dev/null 2>&1 || [ ! -f .git/MERGE_HEAD ]; then
error_exit "No merge in progress. Start the merge first with: git merge master --allow-unrelated-histories"
fi
log "Starting automated conflict resolution for clone-me merge..."
conflicted_files=$(git diff --name-only --diff-filter=U)
if [ -z "$conflicted_files" ]; then
log "No conflicts found. Merge may already be resolved."
exit 0
fi
log "Found conflicted files:"
echo "$conflicted_files"
resolve_keep_head() {
local file="$1"
log "Resolving $file - keeping clone-me version"
git checkout --ours "$file"
git add "$file"
}
resolve_keep_master() {
local file="$1"
log "Resolving $file - keeping master version"
git checkout --theirs "$file"
git add "$file"
}
resolve_manual() {
local file="$1"
log "Manual resolution required for $file"
case "$file" in
"Makefile")
log "Attempting intelligent merge of Makefile..."
{
echo "
echo "
git show :3:"$file" 2>/dev/null || true
echo ""
echo "
git show :2:"$file" 2>/dev/null || true
} > "${file}.merged"
sed -e '/^<<<<<<<\|^======\|^>>>>>>>/d' \
-e '/^$/N;/^\n$/d' \
"${file}.merged" > "$file"
rm -f "${file}.merged"
git add "$file"
;;
"external/README.md")
log "Merging documentation from both branches..."
{
echo "
echo ""
echo "This directory contains external repository clones and related documentation."
echo ""
echo "
git show :3:"$file" 2>/dev/null | tail -n +2 || true
echo ""
echo "
git show :2:"$file" 2>/dev/null | tail -n +2 || true
} > "${file}.merged"
sed -e '/^<<<<<<<\|^======\|^>>>>>>>/d' \
-e '/^$/N;/^\n$/d' \
"${file}.merged" > "$file"
rm -f "${file}.merged"
git add "$file"
;;
*)
log "Unknown file for manual resolution: $file"
log "Please resolve manually and run: git add $file"
return 1
;;
esac
}
for file in $conflicted_files; do
case "$file" in
".github/scripts/"*.py)
resolve_keep_head "$file"
;;
".github/workflows/"*.yml)
resolve_keep_head "$file"
;;
".github/scripts/requirements.txt")
resolve_keep_head "$file"
;;
"clone-repos.sh")
resolve_keep_head "$file"
;;
"Makefile"|"external/README.md")
resolve_manual "$file" || {
log "Manual resolution failed for $file"
log "Please resolve manually and run: git add $file"
continue
}
;;
*)
log "Unknown conflict file: $file"
log "Defaulting to keep clone-me version"
resolve_keep_head "$file"
;;
esac
done
remaining_conflicts=$(git diff --name-only --diff-filter=U)
if [ -n "$remaining_conflicts" ]; then
log "WARNING: Some conflicts still need manual resolution:"
echo "$remaining_conflicts"
log ""
log "After resolving manually, run:"
log "  git add <filename>"
log "  git commit"
exit 1
fi
log "All conflicts resolved successfully!"
log ""
log "Next steps:"
log "1. Review the changes: git diff --cached"
log "2. Commit the merge: git commit"
log "3. Push the changes: git push -u origin clone-me"
log ""
log "Recommended commit message:"
cat << 'EOF'
Merge master into clone-me - resolve conflicts
- Resolved conflicts in GitHub workflows and scripts
- Preserved automation enhancements from clone-me branch
- Merged documentation from both branches
- Updated build configuration
Fixes conflicts in:
- .github/scripts/ (Python automation scripts)
- .github/workflows/ (GitHub Actions)
- clone-repos.sh (enhanced repository cloning)
- Makefile (build configuration)
- external/README.md (documentation)
EOF