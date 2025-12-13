# HurdCog-Inferno Integration

## Overview

This document describes the integration of HurdCog (GNU Hurd + Cognitive Extensions) with the Inferno kernel infrastructure, enabling cognitive translators and semantic filesystem operations via 9P protocol.

## Integration Points

### 1. Mach IPC → 9P Protocol Bridge

HurdCog uses Mach IPC for inter-process communication. We create a bridge to expose Mach ports as 9P file servers:

```
Mach Port → 9P File Server
├── Port rights → File permissions
├── Port messages → File read/write
├── Port sets → Directory structures
└── Port notifications → File events
```

### 2. Translators as 9P Servers

HurdCog translators are filesystem servers. We enhance them to speak 9P protocol:

```
Traditional Translator:
  Mach Port → Translator → Filesystem Operations

9P-Enhanced Translator:
  9P Client → Translator → Filesystem Operations
           ↓
  Mach Port (for backward compatibility)
```

### 3. Cognitive Filesystem Namespace

Integrate cognitive operations into the HurdCog filesystem namespace:

```
/hurd/
  ├── trans/                       # Traditional translators
  │   ├── ext2fs
  │   ├── nfs
  │   └── pflocal
  ├── cognitive/                   # Cognitive translators (NEW)
  │   ├── atomspace                # AtomSpace translator
  │   ├── pln                      # PLN reasoning translator
  │   ├── attention                # Attention mechanism translator
  │   └── pattern-matcher          # Pattern matching translator
  └── 9p/                          # 9P mount points (NEW)
      ├── local/                   # Local 9P servers
      └── remote/                  # Remote 9P servers
```

## Implementation

### Phase 1: 9P Protocol Support in HurdCog

Add 9P protocol support to HurdCog's networking stack:

**Files to create**:
- `core/os/hurdcog/trans/9p-server.c` - 9P server translator
- `core/os/hurdcog/trans/9p-client.c` - 9P client translator
- `core/os/hurdcog/lib9p/` - 9P protocol library

### Phase 2: Cognitive Translators

Create cognitive translators that expose OpenCog components:

**AtomSpace Translator** (`core/os/hurdcog/trans/atomspace.c`):
```c
/* Expose AtomSpace as filesystem */
/hurd/cognitive/atomspace/
  ├── nodes/
  │   └── ConceptNode/
  │       └── cat/
  │           ├── tv           # Truth value
  │           ├── attention    # Attention value
  │           └── links        # Links
  └── links/
      └── InheritanceLink/
```

**PLN Translator** (`core/os/hurdcog/trans/pln.c`):
```c
/* Expose PLN reasoning as filesystem */
/hurd/cognitive/pln/
  ├── deduction/
  ├── induction/
  ├── abduction/
  └── forward-chainer/
```

**Attention Translator** (`core/os/hurdcog/trans/attention.c`):
```c
/* Expose ECAN attention as filesystem */
/hurd/cognitive/attention/
  ├── active/              # High STI atoms
  ├── dormant/             # Low STI atoms
  └── ctl                  # Control interface
```

### Phase 3: Mach-9P Bridge

Create a bridge between Mach IPC and 9P protocol:

**Files to create**:
- `core/os/hurdcog/lib9p/mach-9p-bridge.c`
- `core/os/hurdcog/lib9p/port-to-file.c`
- `core/os/hurdcog/lib9p/message-to-rpc.c`

**Key Functions**:
```c
/* Convert Mach port to 9P file descriptor */
int mach_port_to_9p_fd(mach_port_t port);

/* Convert 9P file descriptor to Mach port */
mach_port_t fd_9p_to_mach_port(int fd);

/* Bridge Mach message to 9P RPC */
int mach_msg_to_9p_rpc(mach_msg_header_t *msg, struct Fcall *fcall);
```

### Phase 4: Integration with CogNumach

Ensure CogNumach microkernel supports cognitive scheduling primitives:

**Files to modify**:
- `core/microkernel/cognumach/kern/sched_prim.c` - Add attention-based scheduling
- `core/microkernel/cognumach/kern/priority.c` - Add STI/LTI priority mapping
- `core/microkernel/cognumach/ipc/ipc_mqueue.c` - Add cognitive message queues

## Build Integration

### CMakeLists.txt Updates

Add HurdCog-Inferno integration to build system:

```cmake
# core/os/hurdcog/CMakeLists.txt (if using CMake wrapper)
IF(BUILD_INFERNO_INTEGRATION)
    add_subdirectory(lib9p)
    add_subdirectory(trans/cognitive)
ENDIF()
```

### Autotools Updates

Add to `core/os/hurdcog/Makefile.am`:

```makefile
if BUILD_INFERNO_INTEGRATION
SUBDIRS += lib9p trans/cognitive
endif
```

## Testing

### Unit Tests

```bash
# Test 9P protocol in HurdCog
./test-hurdcog-9p

# Test cognitive translators
./test-cognitive-translators

# Test Mach-9P bridge
./test-mach-9p-bridge
```

### Integration Tests

```bash
# Mount AtomSpace via HurdCog translator
settrans -a /hurd/cognitive/atomspace /hurd/trans/atomspace

# Access atoms via filesystem
ls /hurd/cognitive/atomspace/nodes/ConceptNode/
cat /hurd/cognitive/atomspace/nodes/ConceptNode/cat/tv

# Perform reasoning via filesystem
echo "cat mammal" > /hurd/cognitive/pln/deduction/premises
cat /hurd/cognitive/pln/deduction/conclusion
```

## Status

### Completed
- ✅ Architecture design
- ✅ Integration plan

### In Progress
- 🚧 9P protocol support in HurdCog
- 🚧 Cognitive translators
- 🚧 Mach-9P bridge

### Planned
- 📋 Complete implementation
- 📋 Testing and validation
- 📋 Documentation
- 📋 Performance optimization

## References

- [GNU Hurd Translators](https://www.gnu.org/software/hurd/hurd/translator.html)
- [Mach IPC](https://www.gnu.org/software/hurd/gnumach-doc/Mach-IPC.html)
- [9P Protocol](https://en.wikipedia.org/wiki/9P_(protocol))
- [Inferno OS](http://www.vitanuova.com/inferno/)

---

**Version**: 1.0  
**Date**: December 13, 2025  
**Status**: Design phase complete, implementation in progress
