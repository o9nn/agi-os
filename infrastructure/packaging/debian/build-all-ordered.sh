#!/bin/bash
#
# AGI-OS Debian Package Build Script (Dependency Order)
# Builds all debian packages in correct dependency order
#

set -e

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $1"; }
log_warning() { echo -e "${YELLOW}[WARNING]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="${BUILD_DIR:-/tmp/agi-os-deb-build}"
OUTPUT_DIR="${OUTPUT_DIR:-$SCRIPT_DIR/packages}"

mkdir -p "$BUILD_DIR"
mkdir -p "$OUTPUT_DIR"

log_info "AGI-OS Debian Package Build"
log_info "  Build directory: $BUILD_DIR"
log_info "  Output directory: $OUTPUT_DIR"
log_info ""

# Build packages in dependency order
PACKAGES=(
    # Layer 0: Inferno Kernel
    "inferno-kernel"
    
    # Layer 0.5: Build Tools
    "cognumach-mig"
    
    # Layer 1: Microkernel
    "cognumach"
    "cognumach-cognitive-scheduler"
    
    # Layer 2: Operating System
    "hurdcog"
    "hurdcog-cogkernel-core"
    "hurdcog-machspace"
    "hurdcog-atomspace-bridge"
    "hurdcog-occ-bridge"
    
    # Layer 2.5: CogPlan9 Operating System
    "cogplan9"
    
    # Layer 3.1: Foundation
    "cogutil"
    
    # Layer 3.2: Hypergraph Database
    "atomspace"
    
    # Layer 3.3: Storage Layer (CRITICAL)
    "atomspace-storage"
    
    # Layer 3.4: Storage Backends
    "atomspace-rocks"
    "atomspace-pgres"
    "atomspace-dht"
    "atomspace-ipfs"
    "atomspace-restful"
    "atomspace-metta"
    
    # Layer 3.5: Network Services
    "cogserver"
    "atomspace-cog"
    
    # Layer 3.6: Reasoning
    "unify"
    "ure"
    "pln"
    "spacetime"
    
    # Layer 3.7: Attention
    "attention"
    
    # Layer 3.8: Learning
    "learn"
    "miner"
    "asmoses"
    
    # Layer 3.9: Language
    "link-grammar"
    "lg-atomese"
    
    # Layer 3.10: Agents
    "agents"
    
    # Layer 3.11: Generation
    "generate"
    
    # Layer 3.12: Specialized
    "agi-bio"
    
    # Layer 3.13: Extensions
    "atomspace-9p"
    "pln-9p"
    "ecan-9p"
    
    # Layer 3.14: Distributed Cognition
    "das"
    
    # Layer 4: Integration
    "cognitive-grip"
    
    # Meta-packages
    "agi-os-unified"
    "agi-os-cognitive-init"
    "agi-os-monitoring"
)

BUILT_COUNT=0
FAILED_COUNT=0
SKIPPED_COUNT=0

for package in "${PACKAGES[@]}"; do
    log_info "Building package: $package"
    
    if [ ! -d "$SCRIPT_DIR/$package" ]; then
        log_warning "  Package directory not found, skipping: $package"
        ((SKIPPED_COUNT++))
        continue
    fi
    
    if [ ! -f "$SCRIPT_DIR/$package/control" ]; then
        log_warning "  No control file found, skipping: $package"
        ((SKIPPED_COUNT++))
        continue
    fi
    
    # Build package
    cd "$SCRIPT_DIR/$package"
    
    if dpkg-buildpackage -us -uc -b 2>&1 | tee "$BUILD_DIR/$package.log"; then
        log_success "  Built: $package"
        ((BUILT_COUNT++))
        
        # Move .deb files to output directory
        mv ../*.deb "$OUTPUT_DIR/" 2>/dev/null || true
        mv ../*.changes "$OUTPUT_DIR/" 2>/dev/null || true
        mv ../*.buildinfo "$OUTPUT_DIR/" 2>/dev/null || true
    else
        log_error "  Failed to build: $package"
        log_error "  See log: $BUILD_DIR/$package.log"
        ((FAILED_COUNT++))
        
        if [ "${STOP_ON_ERROR:-0}" = "1" ]; then
            log_error "Stopping due to build failure"
            exit 1
        fi
    fi
    
    cd "$SCRIPT_DIR"
done

log_info ""
log_info "Build Summary:"
log_info "  Built: $BUILT_COUNT"
log_info "  Failed: $FAILED_COUNT"
log_info "  Skipped: $SKIPPED_COUNT"
log_info "  Total: ${#PACKAGES[@]}"
log_info ""

if [ $FAILED_COUNT -eq 0 ]; then
    log_success "All packages built successfully!"
    log_info "Packages available in: $OUTPUT_DIR"
else
    log_warning "Some packages failed to build"
    log_info "Check logs in: $BUILD_DIR"
fi
