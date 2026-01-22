# Smol Agent Protocol Implementations

This directory contains implementations of the Smol Agent Protocol (Code Minimalization as Constraint Optimization) in various programming languages.

## Overview

The Smol Agent Protocol is a systematic approach to code minimization that treats the problem as a constraint optimization challenge:

- **Objective Function**: Minimize `size(code)` in bytes
- **Hard Constraint**: `functionality(optimized_code) == functionality(original_code)`

## Available Implementations

### Lisp Family
- **smol.scm** - Guile Scheme implementation
- **smol.lisp** - Common Lisp implementation
- **smol.el** - Emacs Lisp implementation
- **smol.echo.lisp** - Echo Lisp implementation (https://github.com/o9nn/echolisp)
- **smol.rkt** - Racket implementation

### Logic Programming
- **smol-prolog.pl** - Prolog implementation

### Systems Programming
- **smol.c** - C implementation
- **smol.cpp** - C++ implementation
- **smol.go** - Go implementation
- **smol.zig** - Zig implementation
- **smol.b** - Limbo implementation (Inferno OS)

### Scripting Languages
- **smol-perl.pl** - Perl implementation
- **smol.raku** - Raku (Perl 6) implementation
- **smol.py** - Python implementation
- **smol.rb** - Ruby implementation
- **smol.jl** - Julia implementation

### Functional Programming
- **smol.hs** - Haskell implementation
- **smol.ml** - OCaml implementation

### Esoteric/Historical
- **smol.forth** - Forth implementation

### Declarative Formats
- **smol.json** - JSON representation
- **smol.yaml** - YAML configuration format
- **smol.toml** - TOML configuration format
- **smol.xml** - XML representation

### AI Markup Language
- **smol.aiml** - AIML (Artificial Intelligence Markup Language) implementation with pattern matching and decision logic

### Automation & CI/CD
- **GitHub Actions** - Two workflow implementations in `.github/workflows/`:
  - **smol-optimize.yml** - Standard iterative optimization workflow
  - **smol-recursive.yml** - Recursive AIML-based optimization with threshold convergence

### Documentation Formats
- **smol.ascii** - ASCII art diagram of the protocol flow
- **smol.math.md** - Complete mathematical formulation with proofs

## Core Algorithm

Each implementation follows the same constraint optimization loop:

1. **Baseline Establishment**
   - Measure initial code size
   - Document functionality requirements
   - Run tests to verify current behavior

2. **Iterative Optimization Loop**
   - Apply transformations:
     - Syntax compaction (remove whitespace, shorten identifiers)
     - Statement reduction (arrow functions, comma operators)
     - Structural optimization (hoist constants, inline variables)
     - Semantic equivalence (shorter patterns)
   - Verify functionality preservation
   - Measure new size
   - Decision rule: Accept only if functionality preserved AND size reduced

3. **Convergence**
   - Continue until no more optimizations possible
   - Report final size and reduction percentage

## Key Principles

All implementations adhere to these principles:

1. **Functionality is Sacred** - Never sacrifice working code for size
2. **Measure Everything** - Every change must be measured in bytes
3. **Verify Continuously** - Test after every transformation
4. **Version Iteratively** - Keep track of each optimization attempt
5. **Embrace Reversibility** - Be ready to revert any change
6. **Converge Systematically** - Stop when no more byte savings found

## Usage Examples

### Scheme (Guile)
```scheme
(use-modules (smol-agent))
(minimize-code "path/to/file.js")
```

### Common Lisp
```lisp
(load "smol.lisp")
(smol-agent:minimize-code "path/to/file.js")
```

### Go
```bash
go run smol.go path/to/file.js
```

### Python
```bash
python smol.py path/to/file.js
```

### C
```bash
gcc smol.c -o smol
./smol path/to/file.js
```

### AIML (Artificial Intelligence Markup Language)
```xml
<!-- Use with an AIML interpreter like Program AB or similar -->
<!-- Load smol.aiml into your AIML bot -->
Human: START OPTIMIZATION src/example.js
Bot: [Executes optimization protocol]

<!-- Query patterns -->
Human: SMOL HELP
Human: SMOL PRINCIPLES
Human: OPTIMIZATION OBJECTIVE
```

### GitHub Actions

#### Standard Optimization Workflow
```yaml
# Trigger manually from GitHub Actions UI
# Navigate to: Actions → Smol Protocol - Code Optimization → Run workflow

# Or via GitHub CLI
gh workflow run smol-optimize.yml \
  -f target_file=src/smallest-agent.js \
  -f max_iterations=20 \
  -f test_command="npm test"
```

#### Recursive AIML-Based Optimization
```yaml
# Trigger manually from GitHub Actions UI  
# Navigate to: Actions → Smol Protocol - Recursive AIML-Based Optimization → Run workflow

# Or via GitHub CLI
gh workflow run smol-recursive.yml \
  -f target_file=src/smallest-agent.js \
  -f threshold_bytes=1 \
  -f max_iterations=50 \
  -f test_command="npm test"
```

**GitHub Actions Features:**
- **smol-optimize.yml**: Standard iterative optimization with simple transformations
- **smol-recursive.yml**: Advanced recursive optimization implementing AIML patterns as workflow steps
  - Recursive loop with convergence detection
  - Threshold-based termination
  - Version tracking for all iterations
  - Comprehensive AIML pattern implementation (Phase 1, 2, 3)
  - Detailed optimization reports

## Decision Rule

The optimization decision rule is consistent across all implementations:

```
if functionality_preserved AND size_reduced:
    return ACCEPT  # Continue optimizing
elif functionality_preserved AND size_same:
    return NEUTRAL  # Try different optimization
else:
    return REJECT  # Revert changes
```

## Mathematical Formulation

This is a constrained optimization problem:

```
minimize: f(x) where f(x) = size(code)
subject to: g(x) = 0 where g(x) = functionality(original) - functionality(optimized)
```

## Testing

Each implementation includes:
- Syntax verification using `node -c`
- Functional testing using `npm test`
- Size measurement in bytes
- Iterative version tracking

## License

These implementations are provided as reference implementations of the Smol Agent Protocol for educational and research purposes.
