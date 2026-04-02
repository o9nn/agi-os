# AGI-OS Build Dependency Order

This document defines the **canonical AGI-OS build graph** for the unified monorepo. The goal is to keep **MIG**, **CogNUMach**, **HurdCog**, and the **OpenCog Collection** aligned as one coherent dependency topology rather than as loosely related subsystems.

## Architecture Overview

```text
┌─────────────────────────────────────────────────────────────────┐
│ Stage 10: agi-os-unified (planned meta-package)                │
├─────────────────────────────────────────────────────────────────┤
│ Stage 9:  opencog                                              │
├─────────────────────────────────────────────────────────────────┤
│ Stage 8:  moses, asmoses, agi-bio, vision                      │
├─────────────────────────────────────────────────────────────────┤
│ Stage 7:  aphroditecho, lg-atomese, relex,                     │
│           das-atomspace, deltecho                              │
├─────────────────────────────────────────────────────────────────┤
│ Stage 6:  learn, generate, cogbolt, das, hyperon-metta         │
├─────────────────────────────────────────────────────────────────┤
│ Stage 5:  attention, pln, miner, unify, spacetime,             │
│           cognitive-grip, hurdcog-atomspace-bridge,            │
│           koboldcpp-cog                                        │
├─────────────────────────────────────────────────────────────────┤
│ Stage 4.5: hurdcog, hurdcog-cogkernel-core,                    │
│            hurdcog-machspace, hurdcog-occ-bridge               │
├─────────────────────────────────────────────────────────────────┤
│ Stage 4:  cogserver, ure, atomspace-9p                         │
├─────────────────────────────────────────────────────────────────┤
│ Stage 3:  atomspace-storage, atomspace-cog,                    │
│           atomspace-rocks, atomspace-pgres,                    │
│           node-llama-cog, cogcities-kernel                     │
├─────────────────────────────────────────────────────────────────┤
│ Stage 2:  atomspace, cognumach-cognitive-scheduler,            │
│           cogplan9, d81p9p9                                    │
├─────────────────────────────────────────────────────────────────┤
│ Stage 1:  cogutil, ggml-tensor, opennars-native, webvm         │
├─────────────────────────────────────────────────────────────────┤
│ Stage 0.5: cognumach                                           │
├─────────────────────────────────────────────────────────────────┤
│ Stage 0:  mig, inferno-kernel                                  │
└─────────────────────────────────────────────────────────────────┘
```

## MIG Build Dependency Locations

MIG is the **bootstrap interface compiler** required before the microkernel and Hurd layers can be built. In the monorepo, the relevant locations are:

| Location | Role |
|---|---|
| `build-tools/mig/CMakeLists.txt` | Unified root-level build entry point |
| `core/microkernel/cognumach/mig/` | Canonical primary MIG source |
| `core/microkernel/mig/` | Mirror/fallback MIG source |
| `core/os/hurdcog/mig` | Symlink to the shared CogNUMach MIG tree |
| `core/os/hurdcog/mig.backup/` | Historical backup copy retained for recovery |

The root build graph now resolves MIG centrally instead of relying on duplicated subsystem-local assumptions.

## Key Dependency Chains

| Chain | Meaning |
|---|---|
| `mig → cognumach → hurdcog` | Bootstrap path from interface compiler to cognitive OS |
| `cogutil → atomspace → atomspace-storage → cogserver` | Core OpenCog runtime path |
| `cogutil → atomspace → ure → pln` | Symbolic reasoning path |
| `ggml-tensor → node-llama-cog / aphroditecho` | LLM orchestration path |
| `atomspace + hurdcog → cognitive-grip bridges` | Cross-layer AGI-OS integration path |

## Build Commands

```bash
# Full build (all implemented layers)
./build-agi-os.sh --all

# Layer-specific builds
./build-agi-os.sh --mig
./build-agi-os.sh --cognumach
./build-agi-os.sh --hurdcog
./build-agi-os.sh --occ
./build-agi-os.sh --cogbolt

# Debian package dependency order
cd infrastructure/packaging/debian
bash resolve-dependencies.sh order
```

## Debian Packaging Status

The packaging tree now distinguishes between **realized package directories** and **planned future packages** in the dependency graph.

```bash
cd infrastructure/packaging/debian
bash validate-packaging.sh
bash resolve-dependencies.sh check
bash resolve-dependencies.sh makefile
bash resolve-dependencies.sh cmake
```

The `validate-packaging.sh` script checks structural completeness of package directories that exist in the repository, while `resolve-dependencies.sh` models the broader AGI-OS package graph and reports which future package directories are still planned rather than materialized.
