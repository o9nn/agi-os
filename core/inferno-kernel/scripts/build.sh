#!/bin/bash
# Inferno OS build script following the exact sequence from the issue
# This script replicates the successful build sequence provided

set -e

# Colors for output  
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log() {
    echo -e "${GREEN}[BUILD] $1${NC}"
}

error() {
    echo -e "${RED}[ERROR] $1${NC}"
}

warn() {
    echo -e "${YELLOW}[WARN] $1${NC}"
}

main() {
    log "Starting Inferno OS build sequence..."
    
    # Get current directory (should be project root)
    PROJECT_ROOT="$(pwd)"
    log "Project root: $PROJECT_ROOT"
    
    # Step 1: Update git repository (skip for CI, but show command)
    log "Step 1: Git update (skipped in CI environment)"
    # git pull && git submodule update --init --recursive
    
    # Step 1.5: Set up directory structure
    log "Step 1.5: Setting up directory structure..."
    mkdir -p Linux/386/{bin,lib,include}
    
    # Copy include files from existing structure
    if [ -d "Inferno/386/include" ]; then
        cp -r Inferno/386/include/* Linux/386/include/ 2>/dev/null || true
        log "Copied include files from Inferno/386/include"
    fi
    
    # Step 2: Configure mkconfig with proper paths
    log "Step 2: Configuring mkconfig..."
    
    # Create backup
    cp mkconfig mkconfig.original 2>/dev/null || true
    
    # Apply the same sed commands from the issue
    sed -i "s|ROOT=/usr/inferno|ROOT=$PROJECT_ROOT|" mkconfig
    sed -i 's|SYSHOST=Plan9|SYSHOST=Linux|' mkconfig
    sed -i 's|#OBJTYPE=386|OBJTYPE=386|' mkconfig
    sed -i 's|OBJTYPE=\$objtype|OBJTYPE=386|' mkconfig
    
    log "Updated mkconfig:"
    grep -E "(ROOT=|SYSHOST=|OBJTYPE=)" mkconfig
    
    # Step 3: Build mk command
    log "Step 3: Building mk command with makemk.sh..."
    ./makemk.sh
    
    # Step 4: Restore any version-controlled binaries
    log "Step 4: Restoring version-controlled binaries..."
    git restore Linux/386/bin/data2c Linux/386/bin/iyacc Linux/386/bin/mkext 2>/dev/null || true
    
    # Step 5: Set up PATH
    log "Step 5: Setting up PATH..."
    export PATH="$PROJECT_ROOT/Linux/386/bin:$PATH"
    log "PATH updated: Linux/386/bin added"
    
    # Verify mk is available
    if command -v mk >/dev/null 2>&1; then
        log "mk command is available at: $(which mk)"
    else
        error "mk command not found in PATH"
        return 1
    fi
    
    # Step 6: Clean and build
    log "Step 6: Building system (mk install)..."
    mk nuke || warn "mk nuke failed (may be expected if nothing to clean)"
    mk install
    
    # Step 7: Verify git state is clean (as in the original sequence)
    log "Step 7: Verifying git state..."
    
    if git diff --quiet; then
        log "Git working directory is clean"
    else
        error "Git working directory was not clean in the end"
        git diff --name-only
        return 1
    fi
    
    if git diff --cached --quiet; then
        log "Git index has no uncommitted changes"  
    else
        error "Git index had uncommitted changes in the end"
        git diff --cached --name-only
        return 1
    fi
    
    if git ls-files --others --exclude-standard | grep -q .; then
        error "Untracked files were present in the end"
        git ls-files --others --exclude-standard
        return 1
    else
        log "Git state was clean in the end"
    fi
    
    # Step 8: Verify build results
    log "Step 8: Verifying build results..."
    
    if [ -f "Linux/386/bin/mk" ]; then
        log "✅ mk binary built successfully"
    fi
    
    if [ -f "Linux/386/bin/emu" ]; then
        log "✅ emu binary built successfully"
    fi
    
    if [ -f "Linux/386/bin/limbo" ]; then
        log "✅ limbo binary built successfully"
    fi
    
    # Show what was built
    log "Built binaries:"
    ls -la Linux/386/bin/ 2>/dev/null | head -20 || warn "No build artifacts found"
    
    # Restore original mkconfig
    if [ -f "mkconfig.original" ]; then
        mv mkconfig.original mkconfig
        log "Restored original mkconfig"
    fi
    
    log "🎉 Build completed successfully!"
}

# Run main function
main "$@"