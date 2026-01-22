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
log "Starting Unicorn-Forest repository integration for monorepo..."
mkdir -p external/unicorn-forest-repos || error_exit "Failed to create unicorn-forest-repos directory"
TEMP_DIR=$(mktemp -d)
trap "rm -rf $TEMP_DIR" EXIT
repos=(
    "gnumach:gnumach"
    "hurd:hurd"
    "libpthread:libpthread"
    "incubator:incubator"
    "mig:mig"
    "procfs:procfs"
    "unionfs:unionfs"
    "viengoos:viengoos"
    "web:web"
    "glibc:glibc"
    "bash:bash"
    "h:h"
)
for repo in "${repos[@]}"; do
    IFS=':' read -r repo_name dir_name <<< "$repo"
    log "Integrating Unicorn-Forest/$repo_name into monorepo as $dir_name..."
    target_dir="external/unicorn-forest-repos/$dir_name"
    if [ -d "$target_dir" ] && [ "$(ls -A "$target_dir" 2>/dev/null)" ]; then
        log "$dir_name already integrated, skipping..."
        continue
    fi
    temp_repo="$TEMP_DIR/$repo_name"
    if ! git clone --progress "https://github.com/Unicorn-Forest/$repo_name.git" "$temp_repo"; then
        log "WARNING: Failed to clone Unicorn-Forest/$repo_name, skipping..."
        continue
    fi
    rm -rf "$temp_repo/.git"
    mkdir -p "$target_dir"
    cp -r "$temp_repo"/* "$target_dir/" 2>/dev/null || log "No files to copy from $repo_name"
    cp -r "$temp_repo"/.[^.]* "$target_dir/" 2>/dev/null || log "No hidden files to copy from $repo_name"
    log "Successfully integrated $repo_name into monorepo"
done
log "Unicorn-Forest repository integration completed!"
log "All repositories from issues
log "No .git directories or submodules were created - all content is directly integrated."
echo "$(date): Unicorn-Forest repositories successfully integrated as monorepo" > external/UNICORN_FOREST_STATUS.txt
echo "Repository mapping (monorepo integration):" >> external/UNICORN_FOREST_STATUS.txt
echo "Issue
echo "Issue
echo "Issue
echo "Issue
echo "Issue
echo "Issue
echo "Issue
echo "Issue
echo "Issue
echo "Issue
echo "Additional: bash → external/unicorn-forest-repos/bash" >> external/UNICORN_FOREST_STATUS.txt
echo "Additional: h → external/unicorn-forest-repos/h" >> external/UNICORN_FOREST_STATUS.txt
echo "" >> external/UNICORN_FOREST_STATUS.txt
echo "MONOREPO STRUCTURE: All repositories integrated without .git headers" >> external/UNICORN_FOREST_STATUS.txt
echo "NO SUBMODULES: Content copied directly into main repository" >> external/UNICORN_FOREST_STATUS.txt
log "Status file created: external/UNICORN_FOREST_STATUS.txt"
log "Status file created: external/UNICORN_FOREST_STATUS.txt"