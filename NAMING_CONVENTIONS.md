# AGI-OS Naming Conventions

**Date:** December 14, 2025  
**Author:** AGI-OS Architecture Team

## Overview

This document establishes the official naming conventions for all AGI-OS components, reflecting their etymologies and ensuring consistency across documentation, code, and build systems.

## Core Component Names

### CoGNUMach (Cognitive GNU Mach Microkernel)

**Etymology**: **Co**gnitive **GNU** **Mach** - A cognitive adaptation of the GNU Mach microkernel.

**Official Names**:
- **Full Name**: CoGNUMach Cognitive Microkernel
- **Short Name**: CoGNUMach
- **Directory**: `cognumach` or `CoGNUMach` (context-dependent)
- **Code Prefix**: `cognumach_` or `CoGNUMach`
- **Package Name**: `cognumach`

**Usage Examples**:
- Documentation: "CoGNUMach provides cognitive scheduling at the microkernel level"
- Code: `cognumach_cognitive_scheduler.h`
- Build: `BUILD_COGNUMACH`
- Debian: `cognumach`, `cognumach-dev`

### CoGNUHurd (Cognitive GNU Hurd Operating System)

**Etymology**: **Co**gnitive **GNU** **Hurd** - A cognitive adaptation of the GNU Hurd operating system.

**Official Names**:
- **Full Name**: CoGNUHurd Cognitive Operating System
- **Short Name**: CoGNUHurd
- **Directory**: `hurdcog` or `CoGNUHurd` (transitioning to CoGNUHurd)
- **Code Prefix**: `hurdcog_` or `CoGNUHurd_` (transitioning)
- **Package Name**: `hurdcog` (legacy) → `cognuhurd` (preferred)

**Usage Examples**:
- Documentation: "CoGNUHurd extends GNU Hurd with cognitive translators"
- Code: `hurdcog_atomspace_bridge.h` (legacy) → `cognuhurd_atomspace_bridge.h` (preferred)
- Build: `BUILD_HURDCOG` → `BUILD_COGNUHURD`
- Debian: `hurdcog` → `cognuhurd`

**Note**: The repository currently uses `hurdcog` extensively. We maintain backward compatibility while transitioning to `CoGNUHurd`.

### InFernOKern (Inferno Cognitive Kernel)

**Etymology**: **In**ferno **Fern**o **K**ern(el) - The Inferno operating system kernel adapted for cognitive operations.

**Official Names**:
- **Full Name**: InFernOKern Distributed Cognitive Kernel
- **Short Name**: InFernOKern (stylized) or Inferno (common usage)
- **Directory**: `inferno-kernel` or `InFernOKern`
- **Code Prefix**: `inferno_` or `InFernOKern_`
- **Package Name**: `inferno-kernel`

**Usage Examples**:
- Documentation: "InFernOKern provides the foundational 9P protocol layer"
- Code: `inferno_9p_server.h`
- Build: `BUILD_INFERNO_KERNEL`
- Debian: `inferno-kernel`

**Note**: "Inferno" is widely recognized and can be used interchangeably with "InFernOKern" in documentation. The stylized form emphasizes the kernel aspect.

## Additional Component Names

### CogPlan9 (Cognitive Plan 9)

**Etymology**: **Cog**nitive **Plan 9** - A cognitive adaptation of Plan 9 from Bell Labs.

**Official Names**:
- **Full Name**: CogPlan9 Cognitive Operating System
- **Short Name**: CogPlan9
- **Directory**: `cogplan9`
- **Code Prefix**: `cogplan9_`
- **Package Name**: `cogplan9`

### DAS (Distributed AtomSpace)

**Etymology**: **D**istributed **A**tom**S**pace - SingularityNET's distributed hypergraph database.

**Official Names**:
- **Full Name**: Distributed AtomSpace (DAS)
- **Short Name**: DAS
- **Directory**: `das`
- **Code Prefix**: `das_`
- **Package Name**: `das`

### OCC (OpenCog Collection)

**Etymology**: **O**pen**C**og **C**ollection - The complete set of OpenCog repositories.

**Official Names**:
- **Full Name**: OpenCog Collection (OCC)
- **Short Name**: OCC or OpenCog
- **Directory**: `core/cognition/foundation/`
- **Code Prefix**: `opencog_` or component-specific
- **Package Names**: `cogutil`, `atomspace`, `cogserver`, etc.

## Naming Patterns

### Directory Names

**Pattern**: Use lowercase with hyphens for multi-word names.

Examples:
- `core/microkernel/cognumach/`
- `core/os/hurdcog/` (legacy) → `core/os/cognuhurd/` (preferred)
- `core/inferno-kernel/`
- `core/os/cogplan9/`
- `core/cognition/distributed/das/`

### File Names

**Pattern**: Use lowercase with underscores for C/C++ files, hyphens for scripts.

Examples:
- `cognumach_scheduler.c`
- `hurdcog_bridge.h` → `cognuhurd_bridge.h`
- `inferno_9p_server.c`
- `build-cognumach.sh`

### Code Identifiers

**Pattern**: Use component prefix with underscore separation.

Examples:
```c
// CoGNUMach
cognumach_init();
struct cognumach_task;

// CoGNUHurd
hurdcog_translator_init();  // legacy
cognuhurd_translator_init(); // preferred

// InFernOKern
inferno_9p_mount();
```

### CMake Variables

**Pattern**: Use UPPERCASE with underscores.

Examples:
```cmake
BUILD_COGNUMACH
BUILD_HURDCOG  # legacy
BUILD_COGNUHURD  # preferred
BUILD_INFERNO_KERNEL
BUILD_COGPLAN9
BUILD_DAS
```

### Debian Package Names

**Pattern**: Use lowercase with hyphens, follow Debian naming conventions.

Examples:
- `cognumach`, `cognumach-dev`
- `hurdcog`, `hurdcog-dev` (legacy)
- `cognuhurd`, `cognuhurd-dev` (preferred)
- `inferno-kernel`, `inferno-kernel-dev`
- `cogplan9`, `cogplan9-dev`
- `das`, `das-dev`

## Documentation Style

### Headers and Titles

Use the official full names in headers:

```markdown
# CoGNUMach Cognitive Microkernel

# CoGNUHurd Cognitive Operating System

# InFernOKern Distributed Cognitive Kernel
```

### Body Text

Use short names in body text for readability:

> CoGNUMach provides cognitive scheduling at the microkernel level, while CoGNUHurd extends this with cognitive translators. InFernOKern (or simply Inferno) provides the foundational 9P protocol layer.

### Code Blocks

Use appropriate prefixes in code examples:

```c
#include <cognumach/scheduler.h>
#include <cognuhurd/translator.h>
#include <inferno/9p.h>
```

## Transition Strategy

### Phase 1: Documentation Update (Current)
- Update all markdown files with new naming conventions
- Create this NAMING_CONVENTIONS.md document
- Update README files

### Phase 2: Code Comments and Headers
- Update code comments to use new naming
- Update header file documentation
- Maintain backward compatibility

### Phase 3: Directory Renaming (Careful)
- Consider renaming `hurdcog` → `cognuhurd`
- Use symlinks for backward compatibility
- Update build system references

### Phase 4: Code Refactoring (Long-term)
- Gradually transition function names
- Maintain legacy aliases
- Update all references

## Backward Compatibility

To ensure smooth transition, we maintain compatibility:

1. **Legacy Names**: Keep existing `hurdcog` references functional
2. **Aliases**: Provide type aliases and function wrappers
3. **Documentation**: Note both old and new names during transition
4. **Symlinks**: Use filesystem symlinks where appropriate

## Summary Table

| Component | Etymology | Official Name | Directory | Code Prefix | Package |
|-----------|-----------|---------------|-----------|-------------|---------|
| Microkernel | Cognitive GNU Mach | CoGNUMach | `cognumach` | `cognumach_` | `cognumach` |
| OS Layer | Cognitive GNU Hurd | CoGNUHurd | `hurdcog`→`cognuhurd` | `hurdcog_`→`cognuhurd_` | `hurdcog`→`cognuhurd` |
| Kernel | Inferno Ferno Kern | InFernOKern / Inferno | `inferno-kernel` | `inferno_` | `inferno-kernel` |
| Plan9 OS | Cognitive Plan 9 | CogPlan9 | `cogplan9` | `cogplan9_` | `cogplan9` |
| Distributed | Distributed AtomSpace | DAS | `das` | `das_` | `das` |
| Framework | OpenCog Collection | OCC / OpenCog | `foundation` | `opencog_` | various |

## References

- GNU Mach: https://www.gnu.org/software/hurd/microkernel/mach/gnumach.html
- GNU Hurd: https://www.gnu.org/software/hurd/
- Inferno: http://www.vitanuova.com/inferno/
- Plan 9: https://9p.io/plan9/
- OpenCog: https://opencog.org/
- DAS: https://github.com/singnet/das
