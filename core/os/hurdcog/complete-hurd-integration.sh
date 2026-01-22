#!/bin/bash
set -e
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"
log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1"
}
error_exit() {
    log "ERROR: $1"
    exit 1
}
integrate_repo() {
    local repo_url="$1"
    local target_dir="$2"
    local temp_name="$3"
    local merge_strategy="$4"
    log "Integrating $repo_url into $target_dir (strategy: $merge_strategy)"
    if [ -d "$target_dir" ] && [ "$merge_strategy" = "skip_if_exists" ]; then
        log "Skipping $temp_name - target directory $target_dir already exists"
        return 0
    fi
    temp_dir="/tmp/hurd-integration-$$-$(date +%s)"
    mkdir -p "$temp_dir"
    log "Cloning $repo_url to temporary location..."
    if ! git clone --progress "$repo_url" "$temp_dir/$temp_name"; then
        rm -rf "$temp_dir"
        error_exit "Failed to clone $repo_url"
    fi
    log "Removing .git directory from $temp_name..."
    rm -rf "$temp_dir/$temp_name/.git"
    find "$temp_dir/$temp_name" -name ".gitignore" -delete
    find "$temp_dir/$temp_name" -name ".gitmodules" -delete
    mkdir -p "$(dirname "$target_dir")"
    case "$merge_strategy" in
        "replace")
            log "Replacing existing content in $target_dir..."
            rm -rf "$target_dir"
            mv "$temp_dir/$temp_name" "$target_dir"
            ;;
        "merge")
            log "Merging content into $target_dir..."
            if [ -d "$target_dir" ]; then
                cp -r "$temp_dir/$temp_name/"* "$target_dir/" || error_exit "Failed to merge content into $target_dir"
            else
                mv "$temp_dir/$temp_name" "$target_dir"
            fi
            ;;
        *)
            mv "$temp_dir/$temp_name" "$target_dir"
            ;;
    esac
    rm -rf "$temp_dir"
    log "Successfully integrated $temp_name into $target_dir"
}
update_build_system() {
    log "Updating build system for integrated components..."
    if ! grep -q "hurd-ecosystem" .gitignore 2>/dev/null; then
        echo "
        echo "!hurd-ecosystem/" >> .gitignore
    fi
    for component_dir in hurd-ecosystem/*/; do
        if [ -d "$component_dir" ] && [ ! -f "$component_dir/Makefile" ]; then
            cat > "$component_dir/Makefile" << 'EOF'
dir := $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))
makemode := server
include $(addprefix $(dir)/,$(addsuffix /Makefile,$(subdirs)))
include ../Makeconf
EOF
            log "Created Makefile for $component_dir"
        fi
    done
}
log "Starting complete GNU Hurd ecosystem integration..."
if ! curl -s --connect-timeout 5 https://git.savannah.gnu.org/ > /dev/null; then
    error_exit "Cannot reach git.savannah.gnu.org - network access required for repository cloning"
fi
log "Phase 1: Integrating standard repositories..."
declare -A standard_repos=(
    ["https://git.savannah.gnu.org/git/hurd/gnumach.git"]="hurd-ecosystem/kernel/gnumach:replace"
    ["https://git.savannah.gnu.org/git/hurd/mig.git"]="hurd-ecosystem/tools/mig:replace"
    ["https://git.savannah.gnu.org/git/hurd/incubator.git"]="hurd-ecosystem/experimental/incubator:replace"
    ["https://git.savannah.gnu.org/git/hurd/libpthread.git"]="hurd-ecosystem/libraries/libpthread:replace"
    ["https://git.savannah.gnu.org/git/hurd/unionfs.git"]="hurd-ecosystem/servers/unionfs:replace"
    ["https://git.savannah.gnu.org/git/hurd/viengoos.git"]="hurd-ecosystem/experimental/viengoos:replace"
    ["https://git.savannah.gnu.org/git/hurd/web.git"]="hurd-ecosystem/documentation/web:replace"
)
for repo_url in "${!standard_repos[@]}"; do
    IFS=':' read -r target_dir strategy <<< "${standard_repos[$repo_url]}"
    temp_name=$(basename "$repo_url" .git)
    integrate_repo "$repo_url" "$target_dir" "$temp_name" "$strategy"
done
log "Phase 2: Handling procfs integration..."
integrate_repo "https://git.savannah.gnu.org/git/hurd/procfs.git" "hurd-ecosystem/servers/procfs-upstream" "procfs" "replace"
log "Phase 3: Handling main hurd repository integration..."
integrate_repo "https://git.savannah.gnu.org/git/hurd/hurd.git" "hurd-ecosystem/reference/hurd-upstream" "hurd" "replace"
log "Creating integration notes for hurd.git..."
cat > hurd-ecosystem/reference/HURD_INTEGRATION_NOTES.md << 'EOF'
The main hurd.git repository has been cloned to this location for reference.
The repository root already contains the core Hurd implementation.
1. **Compare content**: Identify differences between upstream and current
2. **Selective merge**: Integrate only missing or updated components
3. **Conflict resolution**: Handle any conflicting implementations
4. **Build system**: Ensure unified build system works
- Review differences between hurd-ecosystem/reference/hurd-upstream/ and repository root
- Merge any missing components or updates
- Resolve conflicts in overlapping files
- Test unified build system
- Server implementations in root vs upstream
- Library versions and updates  
- Build system changes
- Documentation updates
EOF
log "Phase 4: Handling glibc (large repository)..."
log "Cloning glibc to hurd-ecosystem/libraries/glibc (this may take a while)..."
integrate_repo "https://git.savannah.gnu.org/git/hurd/glibc.git" "hurd-ecosystem/libraries/glibc" "glibc" "replace"
log "Phase 5: Updating build system integration..."
update_build_system
log "Phase 6: Creating integration summary..."
cat > hurd-ecosystem/INTEGRATION_COMPLETE.md << EOF
$(date)
- ✅ **gnumach** → hurd-ecosystem/kernel/gnumach/
- ✅ **mig** → hurd-ecosystem/tools/mig/
- ✅ **incubator** → hurd-ecosystem/experimental/incubator/
- ✅ **libpthread** → hurd-ecosystem/libraries/libpthread/
- ✅ **unionfs** → hurd-ecosystem/servers/unionfs/
- ✅ **viengoos** → hurd-ecosystem/experimental/viengoos/
- ✅ **web** → hurd-ecosystem/documentation/web/
- ✅ **procfs** → hurd-ecosystem/servers/procfs-upstream/
- ✅ **glibc** → hurd-ecosystem/libraries/glibc/
- ⚠️ **hurd** → hurd-ecosystem/reference/hurd-upstream/ (requires manual review)
- All .git directories removed
- Components integrated into monorepo structure
- Build system integration started
- Documentation preserved and enhanced
1. Review hurd.git integration notes in hurd-ecosystem/reference/
2. Test unified build system: \`make\`
3. Resolve any build conflicts
4. Update cross-references in documentation
5. Test component interactions
\`\`\`
$(find hurd-ecosystem -type d | head -20)
...
\`\`\`
- Updated .gitignore for hurd-ecosystem/
- Created component Makefiles where needed
- Integration with existing Makeconf system
- Compare hurd-ecosystem/reference/hurd-upstream/ with repository root
- Merge any missing upstream updates
- Resolve conflicts in overlapping components
- Verify build system compatibility
EOF
log "Creating final status file..."
echo "$(date): Complete GNU Hurd ecosystem integration finished successfully" > hurd-ecosystem/INTEGRATION_STATUS.txt
echo "Manual review required for hurd.git integration" >> hurd-ecosystem/INTEGRATION_STATUS.txt
log "Integration completed successfully!"
log "Total disk usage:"
du -sh hurd-ecosystem/
log ""
log "⚠️  IMPORTANT: Manual steps required:"
log "1. Review hurd-ecosystem/reference/HURD_INTEGRATION_NOTES.md"
log "2. Test build: make"
log "3. Review integration summary: hurd-ecosystem/INTEGRATION_COMPLETE.md"
log ""
log "Integration script completed at $(date)"