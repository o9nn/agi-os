# Inferno Kernel Debian Packaging for AGI-OS

## Overview

This document describes the Debian packaging for the new Inferno kernel components and 9P-enabled cognitive services in AGI-OS.

## New Packages

### Inferno Kernel Packages

1. **libinferno-kernel1** - Inferno kernel runtime library
2. **libinferno-kernel-dev** - Development files for Inferno kernel
3. **inferno-dis-vm** - Dis Virtual Machine for Limbo bytecode
4. **inferno-9p-tools** - 9P protocol tools and utilities

### 9P-Enabled Cognitive Packages

5. **libatomspace-9p1** - AtomSpace as 9P file servers
6. **libatomspace-9p-dev** - Development files for AtomSpace-9P
7. **atomspace-9p-tools** - AtomSpace-9P utilities

8. **libpln-9p1** - PLN reasoning via 9P protocol
9. **libpln-9p-dev** - Development files for PLN-9P
10. **pln-9p-tools** - PLN-9P utilities

11. **libecan-9p1** - ECAN attention via 9P protocol
12. **libecan-9p-dev** - Development files for ECAN-9P
13. **ecan-9p-tools** - ECAN-9P utilities

14. **libdistributed-cognition1** - Distributed cognition framework
15. **libdistributed-cognition-dev** - Development files
16. **distributed-cognition-tools** - Distributed cognition utilities

## Build Order

Packages must be built in dependency order:

### Stage 1: Inferno Kernel (Foundation)
```bash
cd infrastructure/packaging/debian/inferno-kernel
dpkg-buildpackage -us -uc -b
```

### Stage 2: AtomSpace-9P (Depends on Inferno + AtomSpace)
```bash
cd infrastructure/packaging/debian/atomspace-9p
dpkg-buildpackage -us -uc -b
```

### Stage 3: PLN-9P and ECAN-9P (Depend on AtomSpace-9P)
```bash
cd infrastructure/packaging/debian/pln-9p
dpkg-buildpackage -us -uc -b

cd infrastructure/packaging/debian/ecan-9p
dpkg-buildpackage -us -uc -b
```

### Stage 4: Distributed Cognition (Depends on All Above)
```bash
cd infrastructure/packaging/debian/distributed-cognition
dpkg-buildpackage -us -uc -b
```

## Package Dependencies

### Inferno Kernel
```
libinferno-kernel1
  ├─> libc6
  ├─> libpthread-stubs0
  └─> (no OpenCog dependencies)

libinferno-kernel-dev
  └─> libinferno-kernel1
```

### AtomSpace-9P
```
libatomspace-9p1
  ├─> libinferno-kernel1
  ├─> libcogutil
  └─> libatomspace

libatomspace-9p-dev
  ├─> libatomspace-9p1
  ├─> libinferno-kernel-dev
  ├─> libcogutil-dev
  └─> libatomspace-dev
```

## Installation

### Install Inferno Kernel
```bash
sudo dpkg -i libinferno-kernel1_1.0.0-1_amd64.deb
sudo dpkg -i libinferno-kernel-dev_1.0.0-1_amd64.deb
sudo dpkg -i inferno-dis-vm_1.0.0-1_amd64.deb
sudo dpkg -i inferno-9p-tools_1.0.0-1_amd64.deb
```

### Install 9P-Enabled Components
```bash
sudo dpkg -i libatomspace-9p1_1.0.0-1_amd64.deb
sudo dpkg -i libpln-9p1_1.0.0-1_amd64.deb
sudo dpkg -i libecan-9p1_1.0.0-1_amd64.deb
sudo dpkg -i libdistributed-cognition1_1.0.0-1_amd64.deb
```

## Testing

### Test Inferno Kernel
```bash
# Check library installation
ldconfig -p | grep inferno

# Check headers
ls /usr/include/inferno-kernel/

# Test Dis VM
dis-vm --version
```

### Test AtomSpace-9P
```bash
# Start AtomSpace 9P server
atomspace-9p-server &

# Mount AtomSpace namespace
mkdir -p /mnt/atomspace
mount -t 9p localhost:9999 /mnt/atomspace

# Access atoms
ls /mnt/atomspace/nodes/ConceptNode/
cat /mnt/atomspace/nodes/ConceptNode/cat/tv
```

## Status

### Completed
- ✅ Package structure defined
- ✅ Control files created
- ✅ Dependency chains mapped

### In Progress
- 🚧 Creating debian/rules files
- 🚧 Creating debian/changelog files
- 🚧 Testing package builds

### Planned
- 📋 Complete all debian/ files
- 📋 Build and test packages
- 📋 Upload to repository
- 📋 Integration testing

---

**Version**: 1.0  
**Date**: December 13, 2025  
**Status**: Package definitions complete, build infrastructure in progress
