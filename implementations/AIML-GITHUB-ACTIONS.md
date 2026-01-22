# Smol Protocol: AIML and GitHub Actions Implementations

This document provides detailed information about the AIML and GitHub Actions implementations of the Smol Protocol.

## Overview

The Smol Protocol has been implemented in three new formats:

1. **AIML (Artificial Intelligence Markup Language)** - Pattern-matching based implementation
2. **GitHub Actions - Standard Workflow** - Simple iterative optimization
3. **GitHub Actions - Recursive AIML-Based** - Advanced recursive optimization with AIML patterns

## 1. AIML Implementation

### File Location
`implementations/smol.aiml`

### Description
The AIML implementation uses pattern-matching categories to define the optimization protocol as a conversational AI system. It implements all three phases of the protocol as AIML patterns that can be triggered through natural language interactions.

### Key Features
- Pattern-based phase execution (Phase 1, 2, 3)
- Decision rule logic as conditional templates
- Transformation categories (syntax compaction, statement reduction, etc.)
- Verification protocol patterns
- Convergence checking
- Metrics calculation

### AIML Patterns Included

#### Entry Points
- `START OPTIMIZATION *` - Begin optimization for a target file
- `SMOL HELP` - Display usage information
- `SMOL PRINCIPLES` - Show core principles

#### Phase Patterns
- `PHASE 1 BASELINE ESTABLISHMENT`
- `PHASE 2 OPTIMIZATION LOOP`
- `PHASE 3 FINALIZATION`

#### Transformation Patterns
- `SYNTAX COMPACTION` - Priority 1 transformations
- `STATEMENT REDUCTION` - Priority 2 transformations
- `STRUCTURAL OPTIMIZATION` - Priority 3 transformations
- `SEMANTIC EQUIVALENCE` - Priority 4 transformations

#### Decision Patterns
- `MAKE DECISION` - Evaluate transformation results
- `DECISION ACCEPT` - Accept and continue
- `DECISION NEUTRAL` - Try different approach
- `DECISION REJECT SIZE` - Revert size increase
- `DECISION REJECT FUNCTIONALITY` - Immediate revert

#### Utility Patterns
- `VERIFY FUNCTIONALITY` - Run verification protocol
- `CHECK CONVERGENCE` - Determine if optimization should continue
- `CALCULATE METRICS` - Compute optimization metrics
- `SUCCESS METRICS` - Display success criteria

### Usage with AIML Interpreters

The AIML file can be loaded into any AIML 2.0 compatible interpreter:

**Program AB:**
```bash
# Copy smol.aiml to Program AB bots directory
cp implementations/smol.aiml /path/to/programab/bots/smol/aiml/

# Start Program AB and interact
Human: START OPTIMIZATION src/example.js
Bot: [Executes optimization protocol]
```

**PyAIML:**
```python
import aiml

kernel = aiml.Kernel()
kernel.learn("implementations/smol.aiml")

response = kernel.respond("START OPTIMIZATION src/example.js")
print(response)
```

### Example Conversation Flow

```
Human: SMOL HELP
Bot: Smol Agent Protocol - Code Minimalization as Constraint Optimization
     Usage: START OPTIMIZATION [filepath]
     
     The protocol will:
     1. Establish baseline measurements
     2. Apply iterative transformations
     3. Verify functionality preservation
     4. Converge to minimal size
     5. Generate final report