# AGI-OS Build Dependency Order

This document defines the canonical build dependency order for all AGI-OS components. Components must be built in stage order; within a stage, components can be built in parallel.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│ Stage 10: agi-os-unified (Complete AGI Operating System)        │
├─────────────────────────────────────────────────────────────────┤
│ Stage 9:  opencog (Meta-package: all OCC components)            │
├─────────────────────────────────────────────────────────────────┤
│ Stage 8:  moses, asmoses, agi-bio, vision, aphroditecho         │
├─────────────────────────────────────────────────────────────────┤
│ Stage 7:  lg-atomese, relex, das-atomspace, deltecho            │
├─────────────────────────────────────────────────────────────────┤
│ Stage 6:  learn, generate, cogbolt, das, hyperon-metta          │
├─────────────────────────────────────────────────────────────────┤
│ Stage 5:  attention, pln, miner, unify, spacetime,              │
│           cognitive-grip, hurdcog-atomspace-bridge,              │
│           koboldcpp-cog                                          │
├─────────────────────────────────────────────────────────────────┤
│ Stage 4.5: hurdcog, hurdcog-cogkernel-core, hurdcog-machspace,  │
│            hurdcog-occ-bridge                                    │
├─────────────────────────────────────────────────────────────────┤
│ Stage 4:  cogserver, ure, atomspace-9p                          │
├─────────────────────────────────────────────────────────────────┤
│ Stage 3:  atomspace-cog, atomspace-rocks, atomspace-pgres,      │
│           atomspace-storage, cogcities-kernel, node-llama-cog    │
├─────────────────────────────────────────────────────────────────┤
│ Stage 2:  atomspace, cognumach-cognitive-scheduler, cogplan9,    │
│           d81p9p9                                                │
├─────────────────────────────────────────────────────────────────┤
│ Stage 1:  cogutil, ggml-tensor, opennars-native, webvm          │
├─────────────────────────────────────────────────────────────────┤
│ Stage 0:  cognumach (microkernel), inferno-kernel               │
│           + MIG (Mach Interface Generator - build tool)          │
└─────────────────────────────────────────────────────────────────┘
```

## MIG Build Dependency Locations

MIG (Mach Interface Generator) is a critical build tool required by both CogNUMach and HurdCog. It exists in multiple locations:

| Location | Role |
|----------|------|
| `build-tools/mig/CMakeLists.txt` | **Unified build entry point** (new) |
| `core/microkernel/cognumach/mig/` | Primary MIG source |
| `core/microkernel/mig/` | Mirror of cognumach MIG |
| `core/os/hurdcog/mig.backup/` | HurdCog backup copy |
| `core/os/mig.backup/` | Root-level backup copy |
| `core/os/hurdcog/external/hurd-repos/mig` | External reference |

The unified `build-tools/mig/CMakeLists.txt` automatically locates MIG from these candidates and builds it as a shared dependency.

## Key Dependency Chains

**CogServer chain**: cogutil → atomspace → atomspace-storage → cogserver

**PLN reasoning chain**: cogutil → atomspace → ure → pln

**HurdCog chain**: cognumach + MIG → hurdcog → hurdcog-cogkernel-core / hurdcog-machspace

**9P cognitive chain**: inferno-kernel → atomspace-9p → pln-9p / ecan-9p

**LLM integration chain**: ggml-tensor → node-llama-cog / aphroditecho

## Build Commands

```bash
# Full build (all layers)
./build-agi-os.sh --all

# Layer-specific builds
./build-agi-os.sh --cognumach    # Layer 1: Microkernel
./build-agi-os.sh --hurdcog      # Layer 2: Cognitive OS
./build-agi-os.sh --occ          # Layer 3: OpenCog Collection
./build-agi-os.sh --cogbolt      # Layer 4: IDE Core

# Debian package build order
cd infrastructure/packaging/debian
bash resolve-dependencies.sh order
```

## Debian Packaging

All 51 packages are validated. Run the validation:

```bash
cd infrastructure/packaging/debian
bash validate-packaging.sh
bash resolve-dependencies.sh check
```
