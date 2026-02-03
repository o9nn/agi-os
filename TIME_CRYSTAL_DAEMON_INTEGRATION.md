# Time Crystal Daemon Integration

This document describes the integration of the Time Crystal Daemon into the agi-os cognitive operating system.

## Overview

The Time Crystal Daemon provides a **deterministic cognitive service** with **natural language access** through an LLM interface. It implements hierarchical temporal organization based on Nanobrain time crystal models, creating a multi-scale cognitive architecture.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         agi-os                                   │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────────┐    ┌─────────────────┐    ┌─────────────┐ │
│  │   User/Tech     │───▶│  LLM Interface  │───▶│  IDL Layer  │ │
│  │  (Natural Lang) │    │   (Sidecar)     │    │  (Typed)    │ │
│  └─────────────────┘    └─────────────────┘    └──────┬──────┘ │
│                                                       │         │
│  ┌────────────────────────────────────────────────────▼───────┐ │
│  │                  Time Crystal Daemon                        │ │
│  │  ┌──────────────────────────────────────────────────────┐  │ │
│  │  │              Time Crystal Hierarchy                   │  │ │
│  │  │  Level 0: Quantum (1μs)  ────────────────────────▶   │  │ │
│  │  │  Level 1: Protein (8ms)  ────────────────────────▶   │  │ │
│  │  │  Level 2: Ion (26ms)     ────────────────────────▶   │  │ │
│  │  │  ...                                                  │  │ │
│  │  │  Level 11: Homeostatic (1hr) ────────────────────▶   │  │ │
│  │  └──────────────────────────────────────────────────────┘  │ │
│  │                           │                                 │ │
│  │  ┌────────────────────────▼─────────────────────────────┐  │ │
│  │  │                    AtomSpace                          │  │ │
│  │  │  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐  │  │ │
│  │  │  │   PLN   │  │  MOSES  │  │ Pattern │  │ Attn    │  │  │ │
│  │  │  │ Module  │  │ Module  │  │ Module  │  │ Module  │  │  │ │
│  │  │  └─────────┘  └─────────┘  └─────────┘  └─────────┘  │  │ │
│  │  └──────────────────────────────────────────────────────┘  │ │
│  └────────────────────────────────────────────────────────────┘ │
│                                                                  │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │                 Cognitive Kernels (GGML)                    │ │
│  │  cognitive_kernels.h  ◀──▶  time_crystal_integration.h     │ │
│  └────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────┘
```

## Integration Points

### 1. Cognitive Kernels (`core/os/cogkernel/`)

The Time Crystal Daemon integrates with the existing cognitive kernel infrastructure:

- **`cognitive_kernels.h`**: Base cognitive operations
- **`time_crystal_integration.h`**: Time crystal-aware extensions

```c
// Execute attention focus at a specific time crystal level
cognitive_result_t result = tc_execute_at_level(
    TC_LEVEL_DENDRITIC_INTEGRATION,  // Level 5: 160ms
    attention_focus,
    input_tensor,
    config
);
```

### 2. Inferno Kernel (`core/inferno-kernel/`)

The daemon builds on the OpenCog-Inferno kernel architecture:

- AtomSpace implementation from `opencog-inferno-kernel` skill
- Cognitive process management
- Distributed inference patterns

### 3. HurdCog (`core/os/hurdcog/`)

Integration with the HurdCog cognitive operating system:

- Shared cognitive kernel infrastructure
- MIG interface definitions
- AtomSpace bridge

### 4. CognuMach (`core/microkernel/cognumach/`)

Integration with the CognuMach microkernel:

- IPC for daemon communication
- Memory management for AtomSpace
- Process scheduling for cognitive modules

## File Locations

| Component | Location |
|-----------|----------|
| Skill Package | `skills/time-crystal-daemon/` |
| Integration Header | `core/os/cogkernel/time_crystal_integration.h` |
| Daemon Template | `skills/time-crystal-daemon/templates/daemon/` |
| LLM Interface | `skills/time-crystal-daemon/templates/llm_interface/` |
| IDL Specification | `skills/time-crystal-daemon/references/idl_specification.md` |

## Usage

### Starting the Daemon

```bash
# From agi-os root
python skills/time-crystal-daemon/templates/daemon/time_crystal_daemon.py \
    --socket /tmp/tc_daemon.sock
```

### Starting the LLM Interface

```bash
python skills/time-crystal-daemon/templates/llm_interface/llm_sidecar.py
```

### Programmatic Access

```python
import socket
import json

def send_command(method, params=None):
    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    sock.connect("/tmp/tc_daemon.sock")
    
    request = {
        'jsonrpc': '2.0',
        'id': 1,
        'method': method,
        'params': params or {}
    }
    
    sock.sendall(json.dumps(request).encode() + b'\n')
    response = sock.recv(4096)
    sock.close()
    
    return json.loads(response)

# Get daemon status
status = send_command('get_status')

# Get time crystal hierarchy
hierarchy = send_command('get_tc_hierarchy')

# Run diagnostics
diagnostics = send_command('diagnose', {'scope': 'all'})
```

## Design Principles

1. **Deterministic Core**: The daemon never uses LLM for decisions; all cognition is deterministic
2. **Typed Interface**: All commands validated against IDL schema before execution
3. **Fail-Safe**: LLM interface can fail without affecting the daemon
4. **Hierarchical Time**: Cognition organized by temporal scale (12 levels)
5. **Auditable**: All commands logged and reviewable

## Related Documentation

- `INFERNO_KERNEL_INTEGRATION_PLAN.md`: OpenCog-Inferno integration
- `COGNITIVE_SYNERGY_INTEGRATION.md`: Cognitive synergy patterns
- `CONSCIOUSNESS_LAYER_INTEGRATION.md`: Consciousness layer design
- `skills/time-crystal-daemon/SKILL.md`: Full skill documentation
