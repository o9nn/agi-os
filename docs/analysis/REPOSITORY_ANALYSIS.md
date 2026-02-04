# AGI-OS Repository Analysis Report

## Executive Summary

The agi-os repository contains **124,901 files** totaling approximately **5.5GB**. Analysis reveals significant opportunities for optimization through deduplication, archival of test data, and reorganization for improved cognitive grip.

## Size Analysis by Directory

| Directory | Size | Purpose | Action |
|-----------|------|---------|--------|
| `core/` | 2.3GB | Core cognitive systems | **Keep** - reorganize |
| `external/` | 1.1GB | External dependencies | **Archive** test-datasets |
| `personification/` | 645MB | Avatar/VTuber systems | **Keep** - consolidate |
| `cogbolt/` | 363MB | Bolt.new integration | **Keep** |
| `consciousness/` | 278MB | Consciousness layer | **Keep** |
| `ggml/` | 265MB | GGML inference | **Deduplicate** with cogbolt |
| `archive/` | 259MB | Archived experiments | **Review** for removal |

## Large Files Identified

### Critical Bloat (>50MB each)

| File | Size | Assessment |
|------|------|------------|
| `external/archive/experimental/tinycog/data/shape_predictor_68_face_landmarks.dat` | 96MB | **REMOVE** - binary model, use git-lfs or download |
| `consciousness/models/stories15M.gguf` | 94MB | **KEEP** - required model |
| `external/archive/test-datasets/pln/tuffy/smokes/tests/03-08-14/atomspace.scm` | 81MB | **ARCHIVE** - test data |
| `external/archive/experimental/tinycog/data/shape_predictor_26_face_landmarks.dat` | 50MB | **REMOVE** - binary model |
| `core/cognition/learning/learn/attic/run-ull-2019/mini-fuzz/dict.db` | 46MB | **ARCHIVE** - old experiment |
| `core/cognition/foundation/learn/attic/run-ull-2019/mini-fuzz/dict.db` | 46MB | **DUPLICATE** - remove |

### Duplicate Content Detected

**Major Duplication**: `core/cognition/learning/` and `core/cognition/foundation/` contain near-identical `learn/attic/` directories (~400MB duplicated).

**Recommendation**: Consolidate into single location with symlinks.

## Root-Level Markdown Proliferation

**54 markdown files** at repository root create cognitive overload:

| Category | Count | Examples |
|----------|-------|----------|
| Integration Reports | 15 | `*_INTEGRATION*.md` |
| Implementation Docs | 12 | `*_IMPLEMENTATION*.md` |
| Build/Packaging | 8 | `BUILD_*.md`, `DEBIAN_*.md` |
| Analysis/Planning | 10 | `*_ANALYSIS.md`, `STEP*.md` |
| Architecture | 5 | `*_ARCHITECTURE*.md` |
| Miscellaneous | 4 | `README.md`, `IMPROVEMENTS.md` |

**Recommendation**: Move to `docs/` with categorical subdirectories.

## Proposed Directory Structure

```
agi-os/
├── README.md                    # Single entry point
├── docs/                        # All documentation
│   ├── architecture/            # Architecture docs
│   ├── integration/             # Integration reports
│   ├── implementation/          # Implementation guides
│   └── analysis/                # Analysis reports
├── core/                        # Core systems (reorganized)
│   ├── kernel/                  # Unified kernel (inferno + micro)
│   ├── cognition/               # Cognitive modules
│   ├── os/                      # OS layer (hurdcog)
│   └── avatar/                  # Personification
├── packages/                    # Debian packages
├── external/                    # External deps (trimmed)
├── skills/                      # Manus skills
└── tests/                       # Consolidated tests
```

## Cognitive Grip Optimization

### Current Issues

1. **Flat hierarchy**: 54 root markdown files overwhelm navigation
2. **Duplicate code**: learning/foundation duplication wastes space
3. **Binary bloat**: Large model files should use git-lfs
4. **Scattered tests**: Test data spread across multiple locations
5. **Naming inconsistency**: Mix of SCREAMING_CASE and kebab-case

### Proposed Actions

1. **Consolidate documentation** → `docs/` with categories
2. **Deduplicate learn/attic** → Single canonical location
3. **Archive test-datasets** → Separate repository or git-lfs
4. **Unify kernels** → Merge inferno-kernel and microkernel concepts
5. **Standardize naming** → Consistent kebab-case for files

## Integration Points Requiring Attention

| Component | Location | Integration Status |
|-----------|----------|-------------------|
| OpenCog (occ) | `core/cognition/` | Partial |
| HurdCog | `core/os/` | Needs unification |
| CognuMach | `core/microkernel/` | Needs integration |
| Time Crystal Daemon | `skills/time-crystal-daemon/` | New - integrate |
| Inferno Kernel | `core/inferno-kernel/` | Merge with microkernel |

## Recommended Cleanup Script

```bash
# Remove duplicate learn/attic
rm -rf core/cognition/foundation/learn/attic

# Archive large test datasets
mkdir -p .archive/test-datasets
mv external/archive/test-datasets .archive/

# Remove binary models (use git-lfs instead)
rm external/archive/experimental/tinycog/data/*.dat

# Consolidate documentation
mkdir -p docs/{architecture,integration,implementation,analysis}
mv *_INTEGRATION*.md docs/integration/
mv *_IMPLEMENTATION*.md docs/implementation/
mv *_ARCHITECTURE*.md docs/architecture/
mv *_ANALYSIS*.md docs/analysis/
```

## Space Recovery Estimate

| Action | Space Saved |
|--------|-------------|
| Remove duplicate learn/attic | ~400MB |
| Archive test-datasets | ~300MB |
| Remove binary models | ~150MB |
| **Total** | **~850MB** |
