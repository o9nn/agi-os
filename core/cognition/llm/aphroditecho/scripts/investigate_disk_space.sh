#!/bin/bash

# GitHub Actions Disk Space Investigation Script
# This script implements comprehensive disk space analysis for GitHub Actions runners
# to diagnose and address space usage issues like the reported 51GB usage problem.

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
TEMP_DIR="/tmp/disk_analysis_$$"
OUTPUT_FILE="${TEMP_DIR}/disk_analysis_report.txt"
MAX_DISPLAY_ITEMS=20

# Create temporary directory for analysis
mkdir -p "$TEMP_DIR"

# Function to print colored output
print_header() {
    echo -e "\n${BLUE}========================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}========================================${NC}"
}

print_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

print_error() {
    echo -e "${RED}❌ $1${NC}"
}

# Function to log output to both console and file
log_output() {
    tee -a "$OUTPUT_FILE"
}

# Function to analyze directory sizes safely
analyze_directory() {
    local dir="$1"
    local description="$2"
    local max_depth="${3:-1}"
    
    print_header "$description"
    echo "Analyzing directory: $dir (max depth: $max_depth)" | log_output
    
    if [ -d "$dir" ]; then
        echo "Directory exists, analyzing..." | log_output
        # Use timeout to prevent hanging on problematic directories
        timeout 60 sudo du -h --max-depth="$max_depth" "$dir" 2>/dev/null | \
            sort -rh | head -"$MAX_DISPLAY_ITEMS" | log_output || \
            echo "Analysis timed out or failed for $dir" | log_output
    else
        echo "Directory $dir does not exist" | log_output
    fi
    echo "" | log_output
}

# Function to check Docker usage
check_docker_usage() {
    print_header "Docker Space Analysis"
    
    if command -v docker >/dev/null 2>&1; then
        print_success "Docker is installed, checking usage..."
        
        echo "Docker system disk usage:" | log_output
        timeout 30 docker system df 2>/dev/null | log_output || \
            echo "Docker system df failed or timed out" | log_output
        
        echo "" | log_output
        echo "Docker images (top $MAX_DISPLAY_ITEMS):" | log_output
        timeout 30 docker images --format "table {{.Repository}}\t{{.Tag}}\t{{.Size}}" 2>/dev/null | \
            head -$((MAX_DISPLAY_ITEMS + 1)) | log_output || \
            echo "Docker images listing failed or timed out" | log_output
    else
        print_warning "Docker not installed or not running"
        echo "Docker not available" | log_output
    fi
    echo "" | log_output
}

# Function to check snap packages
check_snap_packages() {
    print_header "Snap Packages Analysis"
    
    if command -v snap >/dev/null 2>&1; then
        print_success "Snap is available, checking packages..."
        
        echo "Snap directory sizes:" | log_output
        analyze_directory "/snap" "Snap Packages Directory Analysis" 2
        
        echo "Installed snap packages:" | log_output
        timeout 30 snap list 2>/dev/null | head -$MAX_DISPLAY_ITEMS | log_output || \
            echo "Snap list failed or timed out" | log_output
    else
        print_warning "Snap not available on this system"
        echo "Snap not available" | log_output
    fi
    echo "" | log_output
}

# Function to check package manager caches
check_package_caches() {
    print_header "Package Manager Caches"
    
    # APT cache
    echo "APT cache analysis:" | log_output
    analyze_directory "/var/cache/apt" "APT Cache" 2
    analyze_directory "/var/lib/apt/lists" "APT Lists" 1
    
    # User cache directories
    echo "User cache directories:" | log_output
    for user_home in /home/* /root; do
        if [ -d "$user_home/.cache" ]; then
            echo "Cache for $(basename "$user_home"):" | log_output
            timeout 30 sudo du -sh "$user_home/.cache" 2>/dev/null | log_output || \
                echo "Failed to analyze cache for $(basename "$user_home")" | log_output
        fi
    done
    
    # Python pip cache
    echo "Python pip caches:" | log_output
    for cache_dir in /root/.cache/pip /home/*/.cache/pip; do
        if [ -d "$cache_dir" ]; then
            timeout 30 sudo du -sh "$cache_dir" 2>/dev/null | log_output || true
        fi
    done
    
    echo "" | log_output
}

# Function to check workspace usage
check_workspace_usage() {
    print_header "Workspace Analysis"
    
    echo "GitHub Actions workspace analysis:" | log_output
    if [ -d "/home/runner/work" ]; then
        analyze_directory "/home/runner/work" "GitHub Actions Workspace" 3
    else
        echo "GitHub Actions workspace not found" | log_output
    fi
    
    # Current working directory analysis
    echo "Current working directory analysis:" | log_output
    echo "Current directory: $(pwd)" | log_output
    timeout 60 du -h --max-depth=2 . 2>/dev/null | sort -rh | head -$MAX_DISPLAY_ITEMS | log_output || \
        echo "Current directory analysis failed" | log_output
    
    echo "" | log_output
}

# Function to check system-wide largest directories
check_system_overview() {
    print_header "System-Wide Disk Usage Overview"
    
    echo "Filesystem usage:" | log_output
    df -h | log_output
    echo "" | log_output
    
    echo "Largest directories from root (this may take a while):" | log_output
    timeout 300 sudo du -h --max-depth=1 / 2>/dev/null | \
        sort -rh | head -$MAX_DISPLAY_ITEMS | log_output || \
        echo "Root directory analysis timed out (this is normal for large filesystems)" | log_output
    
    echo "" | log_output
}

# Function to check pre-installed software
check_preinstalled_software() {
    print_header "Pre-installed Software Analysis"
    
    echo "/usr/local/ analysis:" | log_output
    analyze_directory "/usr/local" "Pre-installed Software (/usr/local)" 2
    
    echo "/opt/ analysis:" | log_output  
    analyze_directory "/opt" "Optional Software (/opt)" 2
    
    # Check for common development tools
    echo "Common development tools:" | log_output
    for tool_dir in /usr/local/lib/android /usr/local/share/powershell /usr/local/lib/node_modules \
                    /usr/share/dotnet /usr/local/.ghcup /usr/local/lib/heroku; do
        if [ -d "$tool_dir" ]; then
            timeout 30 sudo du -sh "$tool_dir" 2>/dev/null | log_output || true
        fi
    done
    
    echo "" | log_output
}

# Function to generate cleanup recommendations
generate_recommendations() {
    print_header "Cleanup Recommendations"
    
    echo "Based on the analysis above, here are recommended cleanup actions:" | log_output
    echo "" | log_output
    
    echo "1. SAFE CLEANUP COMMANDS (can be run without risk):" | log_output
    echo "   # Clean APT cache" | log_output
    echo "   sudo apt-get clean" | log_output
    echo "   sudo apt-get autoclean" | log_output
    echo "" | log_output
    echo "   # Clean Docker (if Docker is used)" | log_output
    echo "   docker system prune -f" | log_output
    echo "   docker image prune -f" | log_output
    echo "" | log_output
    echo "   # Clean pip cache" | log_output
    echo "   pip cache purge" | log_output
    echo "" | log_output
    
    echo "2. AGGRESSIVE CLEANUP (for GitHub Actions runners only):" | log_output
    echo "   # Remove large pre-installed software (use with caution)" | log_output
    echo "   sudo rm -rf /usr/share/dotnet" | log_output
    echo "   sudo rm -rf /opt/ghc" | log_output
    echo "   sudo rm -rf /usr/local/share/boost" | log_output
    echo "   sudo rm -rf \"\$AGENT_TOOLSDIRECTORY\"" | log_output
    echo "" | log_output
    
    echo "3. TEMPORARY FILE CLEANUP:" | log_output
    echo "   # Clean temporary files" | log_output
    echo "   sudo find /tmp -type f -atime +1 -delete" | log_output
    echo "   sudo find /var/tmp -type f -atime +1 -delete" | log_output
    echo "" | log_output
    
    echo "4. BUILD-SPECIFIC CLEANUP (for this repository):" | log_output
    echo "   # Clean CUDA compilation artifacts" | log_output
    echo "   find /tmp -name \"*.fatbin.c\" -delete" | log_output
    echo "   find /tmp -name \"*.cudafe*\" -delete" | log_output
    echo "   find /tmp -name \"tmpxft_*\" -delete" | log_output
    echo "" | log_output
    echo "   # Clean build directories" | log_output
    echo "   rm -rf build/" | log_output
    echo "   rm -rf dist/" | log_output
    echo "   ccache -C  # Clear compilation cache" | log_output
    echo "" | log_output
}

# Function to estimate space that could be freed
estimate_cleanup_space() {
    print_header "Estimated Cleanup Space"
    
    local total_estimated=0
    
    # APT cache
    if [ -d "/var/cache/apt" ]; then
        local apt_size=$(sudo du -sb /var/cache/apt 2>/dev/null | cut -f1)
        if [ -n "$apt_size" ] && [ "$apt_size" -gt 0 ]; then
            echo "APT cache: $(numfmt --to=iec-i --suffix=B $apt_size)" | log_output
            total_estimated=$((total_estimated + apt_size))
        fi
    fi
    
    # Docker
    if command -v docker >/dev/null 2>&1; then
        local docker_reclaimable=$(docker system df --format "table {{.Reclaimable}}" 2>/dev/null | tail -n +2 | head -1 || echo "0B")
        echo "Docker reclaimable: $docker_reclaimable" | log_output
    fi
    
    # Temp directories
    local temp_size=$(sudo find /tmp /var/tmp -type f -size +1M 2>/dev/null | xargs -r sudo du -cb | tail -1 | cut -f1 || echo "0")
    if [ "$temp_size" -gt 0 ]; then
        echo "Large temporary files: $(numfmt --to=iec-i --suffix=B $temp_size)" | log_output
        total_estimated=$((total_estimated + temp_size))
    fi
    
    if [ "$total_estimated" -gt 0 ]; then
        echo "Estimated total reclaimable space: $(numfmt --to=iec-i --suffix=B $total_estimated)" | log_output
    else
        echo "Could not estimate reclaimable space accurately" | log_output
    fi
    
    echo "" | log_output
}

# Function to create summary
create_summary() {
    print_header "Analysis Summary"
    
    local current_usage=$(df / | awk 'NR==2 {print $3}')
    local available_space=$(df / | awk 'NR==2 {print $4}')
    local usage_percentage=$(df / | awk 'NR==2 {print $5}')
    
    echo "Current disk usage summary:" | log_output
    echo "  Used space: $(numfmt --to=iec-i --suffix=B $((current_usage * 1024)))" | log_output
    echo "  Available space: $(numfmt --to=iec-i --suffix=B $((available_space * 1024)))" | log_output
    echo "  Usage percentage: $usage_percentage" | log_output
    echo "" | log_output
    
    # Determine if this is concerning
    local usage_number=$(echo "$usage_percentage" | sed 's/%//')
    if [ "$usage_number" -gt 80 ]; then
        print_error "HIGH DISK USAGE WARNING: $usage_percentage used"
        echo "❌ CRITICAL: Disk usage is very high at $usage_percentage" | log_output
    elif [ "$usage_number" -gt 60 ]; then
        print_warning "Moderate disk usage: $usage_percentage used"
        echo "⚠️  WARNING: Disk usage is moderately high at $usage_percentage" | log_output
    else
        print_success "Disk usage appears normal: $usage_percentage used"
        echo "✅ OK: Disk usage is within normal range at $usage_percentage" | log_output
    fi
    
    echo "" | log_output
    echo "Full analysis report saved to: $OUTPUT_FILE" | log_output
}

# Main execution function
main() {
    print_header "GitHub Actions Disk Space Investigation"
    echo "Starting comprehensive disk space analysis..." | log_output
    echo "Analysis started at: $(date)" | log_output
    echo "" | log_output
    
    # Run all analysis functions
    check_system_overview
    check_preinstalled_software
    check_docker_usage
    check_package_caches
    check_snap_packages
    check_workspace_usage
    estimate_cleanup_space
    generate_recommendations
    create_summary
    
    echo "Analysis completed at: $(date)" | log_output
    
    # Display the report location
    print_success "Analysis complete! Full report available at: $OUTPUT_FILE"
    
    # Show a summary of the most critical information
    echo ""
    print_header "Quick Summary"
    df -h /
    echo ""
    
    # Cleanup temp directory but keep the report
    # mv "$OUTPUT_FILE" "$(pwd)/disk_analysis_report_$(date +%Y%m%d_%H%M%S).txt"
    # rmdir "$TEMP_DIR"
}

# Execute main function
main "$@"