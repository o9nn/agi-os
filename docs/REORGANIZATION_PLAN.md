# AGI-OS Repository Reorganization Plan

## Vision: Optimal Cognitive Grip Architecture

This plan transforms agi-os from a sprawling 125K-file repository into a **cognitively coherent** structure optimized for:

1. **Navigability**: Find anything in ≤3 clicks
2. **Comprehensibility**: Structure reflects conceptual hierarchy
3. **Maintainability**: Clear ownership and dependency paths
4. **Extensibility**: New components slot in naturally

## Phase 1: Documentation Consolidation

### Current State
- 54 markdown files at root level
- Cognitive overload, no clear entry point
- Mix of reports, guides, and architecture docs

### Target State
```
docs/
├── README.md                    # Documentation index
├── architecture/                # System architecture
│   ├── overview.md              # High-level architecture
│   ├── cognitive-synergy.md     # Cognitive integration
│   ├── time-crystal.md          # Temporal hierarchy
│   └── diagrams/                # Architecture diagrams
├── integration/                 # Integration reports
│   ├── opencog.md               # OpenCog integration
│   ├── hurdcog.md               # HurdCog integration
│   ├── cognumach.md             # CognuMach integration
│   └── consciousness.md         # Consciousness layer
├── implementation/              # Implementation guides
│   ├── build-guide.md           # Build instructions
│   ├── debian-packaging.md      # Packaging guide
│   └── step-by-step/            # Phased implementation
├── analysis/                    # Analysis reports
│   ├── dependency-map.md        # Dependency analysis
│   ├── vulnerability.md         # Security analysis
│   └── optimization.md          # Performance analysis
└── reports/                     # Historical reports
    └── [archived reports]
```

### Migration Map

| Source File | Destination |
|-------------|-------------|
| `AGI-OS-INTEGRATION.md` | `docs/integration/overview.md` |
| `BUILD_DEPENDENCY_*.md` | `docs/analysis/dependency-map.md` |
| `COGNITIVE_SYNERGY_*.md` | `docs/architecture/cognitive-synergy.md` |
| `DEBIAN_PACKAGING*.md` | `docs/implementation/debian-packaging.md` |
| `*_IMPLEMENTATION*.md` | `docs/implementation/` |
| `*_ARCHITECTURE*.md` | `docs/architecture/` |
| `*_ANALYSIS.md` | `docs/analysis/` |
| `*_REPORT.md` | `docs/reports/` |

## Phase 2: Core System Unification

### Current State
```
core/
├── cognition/       # 1.7GB - cognitive modules
├── os/              # 329MB - OS layer
├── avatar/          # 282MB - personification
├── inferno-kernel/  # 71MB - Inferno-based kernel
├── microkernel/     # 32MB - CognuMach microkernel
└── integration/     # 128KB - integration code
```

### Target State
```
core/
├── kernel/                      # Unified cognitive kernel
│   ├── inferno/                 # Inferno kernel components
│   ├── cognumach/               # CognuMach microkernel
│   ├── time-crystal/            # Time crystal daemon
│   └── integration/             # Kernel integration layer
├── cognition/                   # Cognitive modules (deduplicated)
│   ├── atomspace/               # Knowledge representation
│   ├── reasoning/               # PLN, inference
│   ├── learning/                # MOSES, learning (single copy)
│   ├── attention/               # ECAN, attention
│   ├── language/                # NLP, grammar
│   └── perception/              # Sensory processing
├── os/                          # Operating system layer
│   ├── hurdcog/                 # HurdCog cognitive OS
│   ├── cogkernel/               # Cognitive kernel interface
│   └── services/                # System services
└── avatar/                      # Personification layer
    ├── deep-tree-echo/          # Delta Echo system
    ├── neuro/                   # Neuro-Sama persona
    └── marduk/                  # Marduk persona
```

### Key Changes

1. **Merge inferno-kernel + microkernel** → `core/kernel/`
2. **Deduplicate learning/foundation** → Single `core/cognition/learning/`
3. **Add persona modules** → `core/avatar/{neuro,marduk}/`
4. **Integrate time-crystal-daemon** → `core/kernel/time-crystal/`

## Phase 3: External Dependencies Cleanup

### Current State
- `external/archive/` contains 800MB of test data and experiments
- Binary model files bloating repository

### Target State
```
external/
├── native-code/                 # Native dependencies (keep)
│   ├── das/                     # Distributed AtomSpace
│   ├── metta-wam/               # MeTTa runtime
│   └── hyperon/                 # Hyperon components
└── README.md                    # External deps documentation

.gitattributes                   # LFS tracking for large files
```

### Cleanup Actions

1. **Remove** `external/archive/test-datasets/` (294MB) → Use git-lfs or separate repo
2. **Remove** `external/archive/hardware-specific/` (148MB) → Raspberry Pi tools not needed
3. **Remove** `external/archive/experimental/tinycog/data/*.dat` (146MB) → Binary models
4. **Archive** `external/archive/applications/` (59MB) → Separate repo

## Phase 4: Skills Integration

### Current State
- `skills/time-crystal-daemon/` exists but not integrated
- No persona skills in repository

### Target State
```
skills/
├── time-crystal-daemon/         # Time crystal daemon skill
│   ├── templates/
│   │   ├── daemon/              # Core daemon
│   │   ├── llm_interface/       # LLM sidecar
│   │   ├── o9c/                 # Self-referential kernel
│   │   ├── topology_weaver/     # Topology generation
│   │   ├── composed/            # Self-weaving daemon
│   │   └── persona/             # NEW: Persona integration
│   └── SKILL.md
├── marduk-persona/              # Marduk persona skill
├── agent-neuro/                 # Neuro persona skill
└── skill-infinity/              # Meta-learning skill
```

## Phase 5: Root Level Cleanup

### Target Root Structure
```
agi-os/
├── README.md                    # Single entry point
├── CONTRIBUTING.md              # Contribution guide
├── LICENSE                      # License file
├── Makefile                     # Build orchestration
├── .gitattributes               # LFS configuration
├── core/                        # Core systems
├── docs/                        # Documentation
├── packages/                    # Debian packages
├── external/                    # External dependencies
├── skills/                      # Manus skills
├── infrastructure/              # CI/CD, deployment
└── tests/                       # Consolidated tests
```

## Implementation Script

```bash
#!/bin/bash
# AGI-OS Repository Reorganization Script

set -e
cd /home/ubuntu/agi-os

# Phase 1: Create documentation structure
mkdir -p docs/{architecture,integration,implementation,analysis,reports}

# Move architecture docs
mv COGNITIVE_SYNERGY_*.md docs/architecture/ 2>/dev/null || true
mv *_ARCHITECTURE*.md docs/architecture/ 2>/dev/null || true
mv VORTEX_*.md docs/architecture/ 2>/dev/null || true
mv composed-architecture-o9c.md docs/architecture/ 2>/dev/null || true
mv time-crystal-daemon-architecture.md docs/architecture/ 2>/dev/null || true

# Move integration docs
mv *_INTEGRATION*.md docs/integration/ 2>/dev/null || true
mv INTEGRATION_*.md docs/integration/ 2>/dev/null || true

# Move implementation docs
mv *_IMPLEMENTATION*.md docs/implementation/ 2>/dev/null || true
mv STEP*.md docs/implementation/ 2>/dev/null || true
mv PHASE*.md docs/implementation/ 2>/dev/null || true
mv BUILD_*.md docs/implementation/ 2>/dev/null || true
mv DEBIAN_*.md docs/implementation/ 2>/dev/null || true

# Move analysis docs
mv *_ANALYSIS*.md docs/analysis/ 2>/dev/null || true
mv ERRORS_*.md docs/analysis/ 2>/dev/null || true
mv ISSUE_*.md docs/analysis/ 2>/dev/null || true
mv VULNERABILITY_*.md docs/analysis/ 2>/dev/null || true

# Move reports
mv *_REPORT*.md docs/reports/ 2>/dev/null || true
mv *_SUMMARY*.md docs/reports/ 2>/dev/null || true

# Phase 2: Deduplicate cognition
rm -rf core/cognition/foundation/learn/attic 2>/dev/null || true

# Phase 3: Create unified kernel structure
mkdir -p core/kernel/{inferno,cognumach,time-crystal,integration}
cp -r skills/time-crystal-daemon/templates/* core/kernel/time-crystal/ 2>/dev/null || true

# Phase 4: Archive large test datasets
mkdir -p .archive
mv external/archive/test-datasets .archive/ 2>/dev/null || true
mv external/archive/hardware-specific .archive/ 2>/dev/null || true

# Phase 5: Remove binary bloat
rm -f external/archive/experimental/tinycog/data/*.dat 2>/dev/null || true

echo "Reorganization complete!"
```

## Cognitive Grip Metrics

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| Root files | 54 | 5 | 91% reduction |
| Max depth to any doc | 1 | 3 | Structured |
| Duplicate code | ~400MB | 0 | 100% removed |
| Binary bloat | ~600MB | 0 | 100% removed |
| Total size | ~5.5GB | ~4.0GB | 27% reduction |

## Verification Checklist

- [ ] All documentation accessible from `docs/README.md`
- [ ] No duplicate `learn/attic` directories
- [ ] No binary model files in repository
- [ ] Time crystal daemon integrated in `core/kernel/`
- [ ] Persona modules in `core/avatar/`
- [ ] Root level clean (≤10 files)
- [ ] All tests pass after reorganization
