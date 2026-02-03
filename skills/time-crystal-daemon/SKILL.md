---
name: time-crystal-daemon
description: Build and operate time crystal daemons with LLM interfaces. Use for creating deterministic cognitive services with natural language access, implementing hierarchical temporal architectures based on Nanobrain time crystal models, and exposing OpenCog-style cognitive kernels through typed IDL interfaces.
---

# Time Crystal Daemon

This skill enables the creation of **time crystal daemons**—deterministic cognitive services organized around hierarchical temporal oscillators—with **LLM interfaces** that provide natural language access while maintaining the integrity of the deterministic core.

## Core Architecture

The system follows a **sidecar pattern**:

1. **Time Crystal Daemon (Core)**: A deterministic, long-running process implementing the cognitive kernel
2. **LLM Interface (Sidecar)**: A natural language translator that compiles user intent into typed IDL commands
3. **Interface Definition Language (IDL)**: The typed command surface mediating all interactions

The LLM acts as a **compiler, not an oracle**—it translates intent into explicit, reviewable plans but never makes decisions.

## Quick Start

### 1. Start the Daemon

```bash
# Copy templates to your project
cp -r /home/ubuntu/skills/time-crystal-daemon/templates/daemon /path/to/project/
cp -r /home/ubuntu/skills/time-crystal-daemon/templates/llm_interface /path/to/project/

# Start the daemon
python /path/to/project/daemon/time_crystal_daemon.py --socket /tmp/tc_daemon.sock
```

### 2. Start the LLM Interface

```bash
# In another terminal
python /path/to/project/llm_interface/llm_sidecar.py
```

### 3. Interact via Natural Language

```
> What's the status?
> List all modules
> Run diagnostics
> Show time crystal hierarchy
```

## Composed Architecture: Self-Weaving Daemon

This skill supports an advanced **composed architecture** that implements the skill expression:

```
/time-crystal-daemon ( (/o9c -> /topology-weaver [ /opencog-inferno-kernel | /time-crystal-neuron ]) )
```

This creates a **self-weaving cognitive fabric** that dynamically generates and refines its own neural architecture.

### Pipeline Stages

| Stage | Skill | Function |
|-------|-------|----------|
| 1 | `/topology-weaver` | Extracts terminology from parallel inputs and generates contextually-tagged neural topology |
| 2 | `/o9c` | Applies recursive self-improvement, converging to a fixed-point architecture |
| 3 | `/time-crystal-daemon` | Executes the evolved topology with hierarchical temporal organization |

### Running the Self-Weaving Daemon

```bash
# Start the self-weaving daemon
python /home/ubuntu/skills/time-crystal-daemon/templates/composed/self_weaving_daemon.py

# Or save the evolved topology without running
python templates/composed/self_weaving_daemon.py --save-topology evolved.yaml
```

### o9c Kernel Integration

The o9c kernel implements Marduk's recursive transformation:

```
T(system) = marduk(hypergauge(sys-n(system)))
```

Where:
- **sys-n**: Analyzes hierarchical structure (rooted trees, A000081 sequence)
- **hypergauge-orbifold**: Interprets as geometric manifold with singularities
- **marduk-persona**: Transforms via over-engineering and indirect orchestration

The kernel converges to a **fixed point** where `T(o9c) = o9c`.

### Topology Weaver Integration

The topology weaver extracts terminology from:
- `/opencog-inferno-kernel`: Atoms, AtomSpace, PLN, pattern matching
- `/time-crystal-neuron`: Temporal hierarchy, oscillators, phase coupling

And maps them to neural components using analogy patterns:

| Source Concept | Neural Component | Tag |
|----------------|------------------|-----|
| Atom | Embedding | `discrete_feature` |
| AtomSpace | Attention | `hypergraph_attention` |
| Pattern Matching | Cross-Attention | `pattern_matcher` |
| PLN Inference | Linear | `probabilistic_reasoner` |
| Time Crystal | LayerNorm | `temporal_oscillator` |
| Global Rhythm | Norm | `level_9_rhythm` |

## Access Levels

| Level | Description | Commands |
|-------|-------------|----------|
| **Technician** | Safe, read-only access | `get_status`, `list_modules`, `diagnose`, `trace_atom`, `get_tc_hierarchy`, `explain_decision` |
| **Engineer** | Full access with mutations | All technician commands + `pause_module`, `resume_module`, `set_attention`, `inject_atom`, `set_tc_phase` |

Switch levels in the CLI:
```
/engineer  - Switch to engineer mode
/tech      - Switch to technician mode
```

## Time Crystal Hierarchy

The daemon implements a 12-level temporal hierarchy based on Nanobrain Fig 7.15:

| Level | Name | Period | Domain |
|-------|------|--------|--------|
| 0 | quantum_resonance | 1μs | Quantum effects |
| 1 | protein_dynamics | 8ms | Protein channels |
| 2 | ion_channel_gating | 26ms | Ion channels |
| 3 | membrane_dynamics | 52ms | Membrane |
| 4 | axon_initial_segment | 110ms | AIS |
| 5 | dendritic_integration | 160ms | Dendrites |
| 6 | synaptic_plasticity | 250ms | Synapses |
| 7 | soma_processing | 330ms | Soma |
| 8 | network_synchronization | 500ms | Network |
| 9 | global_rhythm | 1s | Global |
| 10 | circadian_modulation | 1min | Circadian |
| 11 | homeostatic_regulation | 1hr | Homeostatic |

Each cognitive module operates at specific TC levels, creating a hierarchical processing architecture.

## IDL Commands

Read the full IDL specification: `references/idl_specification.md`

### Example: Diagnose and Fix

```python
# User: "Why is PLN using so much attention?"

# LLM compiles to:
commands = [
    {"method": "get_module", "params": {"module_id": "pln"}},
    {"method": "diagnose", "params": {"scope": "module", "target": "pln"}}
]

# Daemon executes deterministically
# LLM narrates the structured response
```

## Using with OpenAI

For production use with OpenAI-powered compilation:

```python
from llm_interface.openai_compiler import OpenAICompiler

compiler = OpenAICompiler(model="gpt-4.1-mini")
plan = compiler.compile("Pause the PLN module", access_level="engineer")
# Returns validated IDL command plan
```

## Bundled Resources

### Core Daemon
- **`templates/daemon/`**: Time Crystal Daemon implementation
- **`templates/llm_interface/`**: LLM sidecar and OpenAI compiler
- **`references/idl_specification.md`**: Full IDL specification
- **`scripts/run_daemon.sh`**: Convenience script to start the daemon

### Composed Architecture
- **`templates/o9c/`**: o9c self-referential cognitive kernel
- **`templates/topology_weaver/`**: Topology weaver with parallel inputs
- **`templates/composed/`**: Self-weaving daemon combining all components

## Integration with agi-os

This skill integrates with the agi-os cognitive operating system:

1. The daemon implements the `opencog-inferno-kernel` cognitive kernel interface
2. The time crystal hierarchy maps to `time-crystal-neuron` temporal structures
3. The IDL provides the "ghost in the shell" addressability for cognitive processes
4. The o9c kernel enables recursive self-improvement
5. The topology weaver generates architectures from conceptual frameworks

## Design Principles

1. **Deterministic Core**: The daemon never uses LLM for decisions
2. **Typed Interface**: All commands validated against IDL schema
3. **Fail-Safe**: LLM interface can fail without affecting daemon
4. **Auditable**: All commands logged and reviewable
5. **Hierarchical Time**: Cognition organized by temporal scale
6. **Self-Weaving**: Architecture can evolve through o9c transformation
7. **Conceptual Grounding**: Neural topology derived from domain concepts
