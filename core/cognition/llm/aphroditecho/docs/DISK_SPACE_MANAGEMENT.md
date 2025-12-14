# 💾 GitHub Actions Disk Space Management Guide

This document provides comprehensive guidance for managing disk space issues in GitHub Actions runners, particularly addressing the 51GB usage problems encountered during CUDA compilation and large-scale builds.

## 🚨 Problem Overview

GitHub Actions runners come with significant pre-installed software that can consume up to 51GB of disk space, including:

- **Pre-installed software**: Multiple versions of Python, Node.js, .NET, Android SDKs, databases
- **Development tools**: Docker images, language runtimes, build tools
- **Cache directories**: Package manager caches, compilation artifacts
- **System files**: Snap packages, system libraries, documentation

This can cause build failures, especially for complex projects like Aphrodite Engine that require CUDA compilation with large temporary files.

## 🔍 Investigation Tools

### Quick Investigation Script

Use our comprehensive disk space investigation script:

```bash
# Make the script executable
chmod +x scripts/investigate_disk_space.sh

# Run standard investigation
./scripts/investigate_disk_space.sh
```

### GitHub Actions Workflow

Trigger the disk space investigation workflow:

```bash
# Using GitHub CLI
gh workflow run disk-space-investigation.yml \
  -f investigation_level=comprehensive \
  -f include_cleanup=false \
  -f runner_type=ubuntu-latest
```

Or trigger manually through the GitHub Actions web interface.

### Manual Investigation Commands

#### 1. System Overview
```bash
# Check filesystem usage
df -h

# Find largest directories from root
sudo du -h --max-depth=1 / 2>/dev/null | sort -rh | head -20
```

#### 2. Pre-installed Software Analysis
```bash
# Check /usr/local/ usage
du -sh /usr/local/* 2>/dev/null | sort -rh | head -10

# Check /opt/ usage  
du -sh /opt/* 2>/dev/null | sort -rh | head -10
```

#### 3. Docker Analysis
```bash
# Docker system usage
docker system df 2>/dev/null || echo "Docker not running"

# List Docker images with sizes
docker images --format "table {{.Repository}}\t{{.Tag}}\t{{.Size}}" 2>/dev/null | head -20
```

#### 4. Package Manager Caches
```bash
# APT cache
du -sh /var/cache/apt 2>/dev/null
du -sh /var/lib/apt/lists 2>/dev/null

# User cache directories
du -sh ~/.cache 2>/dev/null
```

#### 5. Snap Packages
```bash
# Snap directory usage
du -sh /snap/* 2>/dev/null | sort -rh | head -10

# List installed snap packages
snap list 2>/dev/null | head -20
```

#### 6. Workspace Analysis
```bash
# GitHub Actions workspace
du -sh /home/runner/work/* 2>/dev/null | sort -rh
```

## 🧹 Cleanup Strategies

### Level 1: Safe Cleanup (Always Safe)

```bash
# Clean package manager caches
sudo apt-get clean
sudo apt-get autoclean
pip cache purge

# Clean Docker (if used)
docker system prune -f
docker image prune -f

# Clean temporary files
sudo find /tmp -type f -atime +1 -delete
sudo find /var/tmp -type f -atime +1 -delete
```

### Level 2: Moderate Cleanup (Usually Safe for CI)

```bash
# Remove unused packages
sudo apt-get autoremove -y

# Clean build artifacts
rm -rf build/
rm -rf dist/
ccache -C  # Clear compilation cache
```

### Level 3: Aggressive Cleanup (GitHub Actions Runners Only)

⚠️ **WARNING**: Only use on disposable GitHub Actions runners, never on persistent infrastructure.

```bash
# Remove large pre-installed software
sudo rm -rf /usr/share/dotnet          # .NET SDK (~1-2GB)
sudo rm -rf /opt/ghc                   # Haskell GHC (~2-3GB)
sudo rm -rf /usr/local/share/boost     # Boost libraries (~1-2GB)
sudo rm -rf "$AGENT_TOOLSDIRECTORY"    # GitHub Actions tools (~5-10GB)
sudo rm -rf /usr/local/lib/android     # Android SDK (~10-15GB)
sudo rm -rf /usr/local/share/powershell # PowerShell (~500MB)
sudo rm -rf /usr/local/lib/node_modules # Global npm modules (~1-2GB)
```

### Level 4: Build-Specific Cleanup (Aphrodite Engine)

```bash
# CUDA compilation artifacts
find /tmp -name "*.fatbin.c" -delete
find /tmp -name "*.cudafe*" -delete  
find /tmp -name "tmpxft_*" -delete
find /tmp -name "*.stub.c" -delete
find /tmp -name "cc*.s" -delete

# Build directory cleanup
find build/ -name "*.o" -delete
find build/ -name "*.obj" -delete
find build/ -name "*.tmp" -delete

# Remove precompiled CUDA kernels
rm -rf kernels/xqa/cubin/
find kernels/ -name "*.cubin" -delete
find kernels/ -name "*.fatbin" -delete
```

## 🏗️ Build Optimization Strategies

### 1. Reduce Parallel Compilation

For disk-intensive builds, reduce parallelism to manage space usage:

```yaml
env:
  MAX_JOBS: 1  # Reduced from default 8
  CCACHE_MAXSIZE: 5G  # Reduced cache size
```

### 2. Use tmpfs for Build Artifacts

Create build environment in memory-backed filesystem:

```bash
# Create tmpfs build directory
sudo mkdir -p /dev/shm/build_env
python -m venv /dev/shm/build_env
source /dev/shm/build_env/bin/activate
```

### 3. Continuous Cleanup During Build

Monitor and clean artifacts during compilation:

```bash
# Background cleanup process
(while true; do 
  find /tmp -name "tmpxft_*" -mmin +0.5 -delete 2>/dev/null || true
  find build/ -name "*.o" -mmin +1 -delete 2>/dev/null || true
  sleep 30
done) &
CLEANUP_PID=$!

# Your build process here
make -j1

# Stop cleanup
kill $CLEANUP_PID 2>/dev/null || true
```

### 4. Selective CUDA Architecture Compilation

Limit CUDA architectures to reduce compilation space:

```bash
export TORCH_CUDA_ARCH_LIST="8.0"  # Only compile for specific architecture
export NVCC_PREPEND_FLAGS="-ccbin=ccache"  # Use ccache for NVCC
```

## 📊 Monitoring and Alerting

### Pre-build Space Verification

```bash
# Check available space before build
AVAILABLE_GB=$(df / | awk 'NR==2 {print int($4/1024/1024)}')
echo "Available disk space: ${AVAILABLE_GB}GB"

if [ "$AVAILABLE_GB" -lt 15 ]; then
  echo "ERROR: Insufficient disk space for CUDA compilation"
  exit 1
fi
```

### Real-time Monitoring

```bash
# Monitor disk usage during build
watch -n 30 'df -h | grep -E "/$|/tmp"'

# Or continuous monitoring with alerts
while true; do
  usage=$(df / | awk 'NR==2 {print int($5)}')
  if [ "$usage" -gt 85 ]; then
    echo "WARNING: Disk usage at ${usage}%"
  fi
  sleep 60
done
```

## 🔧 GitHub Actions Workflow Integration

### Pre-build Cleanup Step

```yaml
- name: 🗂️ Free up disk space
  run: |
    echo "Initial disk usage:"
    df -h
    
    # Run investigation if available
    if [ -f "scripts/investigate_disk_space.sh" ]; then
      chmod +x scripts/investigate_disk_space.sh
      ./scripts/investigate_disk_space.sh
    fi
    
    # Aggressive cleanup for GitHub Actions
    sudo rm -rf /usr/share/dotnet
    sudo rm -rf /opt/ghc
    sudo rm -rf /usr/local/share/boost
    sudo rm -rf "$AGENT_TOOLSDIRECTORY"
    
    echo "Disk usage after cleanup:"
    df -h
```

### Build Monitoring Step

```yaml
- name: 🏗️ Build with Space Management
  run: |
    # Monitor space during build
    (while true; do
      df -h | grep -E "/$|/tmp"
      find /tmp -name "tmpxft_*" -mmin +0.5 -delete 2>/dev/null || true
      sleep 30
    done) &
    MONITOR_PID=$!
    
    # Your build process
    python setup.py build_ext --inplace
    
    # Stop monitoring
    kill $MONITOR_PID 2>/dev/null || true
```

## 📈 Expected Space Savings

| Cleanup Level | Typical Space Freed | Risk Level |
|---------------|-------------------|------------|
| Safe Cleanup | 1-3 GB | None |
| Moderate Cleanup | 2-5 GB | Low |
| Aggressive Cleanup | 15-30 GB | Medium (CI only) |
| Build-specific | 5-15 GB | Low |

## 🚨 Troubleshooting Common Issues

### Issue: "No space left on device" during CUDA compilation

**Solution**:
1. Run aggressive cleanup before build
2. Reduce MAX_JOBS to 1
3. Use tmpfs for build artifacts
4. Limit CUDA architectures

### Issue: Build fails after cleanup

**Solution**:
1. Check if required tools were accidentally removed
2. Reinstall essential build dependencies
3. Use more conservative cleanup level

### Issue: Intermittent space issues

**Solution**:
1. Implement continuous monitoring
2. Add background cleanup during build
3. Use space verification before critical steps

## 🔗 Related Files

- [`scripts/investigate_disk_space.sh`](../scripts/investigate_disk_space.sh) - Main investigation script
- [`.github/workflows/disk-space-investigation.yml`](../.github/workflows/disk-space-investigation.yml) - Investigation workflow
- [`.github/workflows/build-engine.yml`](../.github/workflows/build-engine.yml) - Build workflow with space management

## 📞 Support

For disk space issues:

1. Run the investigation workflow with `comprehensive` level
2. Check the generated reports in workflow artifacts
3. Apply appropriate cleanup level based on findings
4. Monitor space usage during subsequent builds

Remember: GitHub Actions runners are ephemeral, so aggressive cleanup is safe and recommended for CI/CD workflows.