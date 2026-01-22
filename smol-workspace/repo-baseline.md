# AGI-OS Repository Baseline Measurements

## Repository Overview
- **Total Size**: 7.0 GB
- **Total Files**: ~100,000+ files

## Code Files by Extension (Top Categories)

| Extension | File Count | Description |
|-----------|------------|-------------|
| .c | 17,314 | C source files |
| .h | 16,404 | C/C++ header files |
| .json | 7,233 | JSON configuration/data |
| .md | 4,721 | Markdown documentation |
| .py | 4,342 | Python source files |
| .png | 3,456 | Image assets |
| .ts | 2,931 | TypeScript source files |
| .txt | 2,660 | Text files |
| .scm | 2,594 | Scheme source files |
| .cpp | 1,969 | C++ source files |
| .b | 1,471 | Limbo (Inferno) source files |
| .js | 1,259 | JavaScript source files |
| .jl | 606 | Julia source files |
| .go | 656 | Go source files |
| .sh | 1,239 | Shell scripts |

## Total Bytes by Major Code Types

| Type | Total Bytes | File Count |
|------|-------------|------------|
| .scm | 242,226,910 | 2,594 |
| .js | 43,611,054 | 1,259 |
| .c | 27,625,270 | 17,314 |
| .h | 24,480,841 | 16,404 |
| .b | 13,015,862 | 1,471 |
| .py | 8,179,004 | 4,342 |
| .jl | 5,852,475 | 606 |
| .go | 5,159,014 | 656 |
| .sh | 3,691,725 | 1,239 |
| .ts | 3,079,180 | 2,931 |
| .cpp | 997,900 | 1,969 |

## Primary Optimization Targets

### 1. Smol Protocol Implementations (implementations/)
These are the core smol agent implementations in various languages:

| File | Bytes | Language |
|------|-------|----------|
| smol.echo.lisp | 2,446 | Echo Lisp |
| smol.scm | 3,062 | Scheme |
| smol-prolog.pl | 3,146 | Prolog |
| smol.b | 3,929 | Limbo |
| smol.lisp | 4,100 | Common Lisp |
| smol.rb | 4,208 | Ruby |
| smol.rkt | 4,277 | Racket |
| smol.ml | 4,487 | OCaml |
| smol.el | 4,532 | Emacs Lisp |
| smol.raku | 4,642 | Raku |
| smol.c | 4,763 | C |
| smol-perl.pl | 4,840 | Perl |
| smol.jl | 4,943 | Julia |
| smol.go | 4,983 | Go |
| smol.py | 5,027 | Python |
| smol.cpp | 5,057 | C++ |
| smol.hs | 5,149 | Haskell |
| smol.zig | 5,614 | Zig |
| **TOTAL** | **79,205** | 18 files |

### 2. src/ Directory (Core Agent)
| File | Bytes | Description |
|------|-------|-------------|
| smallest-agent.js | 803 | Minified agent |
| smallest-agent.commented.js | 2,480 | Commented version |
| agent.ts | 2,298 | Original TypeScript |
| **TOTAL** | **5,581** | 3 files |

## Optimization Strategy

1. **Phase 1**: Optimize src/smallest-agent.js (already at 803 bytes, target <795)
2. **Phase 2**: Optimize all implementations/ smol files (79,205 bytes total)
3. **Phase 3**: Identify and optimize high-impact project-specific files
4. **Phase 4**: Apply language-specific minification across the codebase

## Current Progress

- src/smallest-agent.js: 803 → 795 bytes (v6) = **8 bytes saved (1.0%)**
