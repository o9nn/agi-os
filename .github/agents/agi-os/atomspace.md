# AtomSpace Knowledge Representation - Extended Definition

## Overview

The AtomSpace is a weighted, labeled hypergraph for knowledge representation. It is the core data structure used throughout AGI-OS for representing system state, cognitive knowledge, and relationships.

## Core Concepts

### Atoms

- **Nodes**: Represent concepts, entities, or values
- **Links**: Represent relationships between nodes

### Truth Values

Probabilistic strength and confidence:
- **Strength**: Probability (0.0 to 1.0)
- **Confidence**: Certainty (0.0 to 1.0)

### Attention Values

Importance measures:
- **STI** (Short-Term Importance): Immediate relevance
- **LTI** (Long-Term Importance): Historical significance
- **VLTI** (Very Long-Term Importance): Permanent importance

## Usage

```cpp
#include <opencog/atomspace/AtomSpace.h>

AtomSpace as;
Handle h = as.add_node(CONCEPT_NODE, "AI");
h->setTruthValue(SimpleTruthValue::createTV(0.9, 0.8));
```

## References

- AtomSpace Documentation: https://wiki.opencog.org/w/AtomSpace
- Location: `core/cognition/foundation/atomspace/`
