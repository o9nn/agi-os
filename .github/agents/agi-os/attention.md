# Attention Allocation Mechanisms - Extended Definition

## Overview

Economic Attention Networks (ECAN) manage attention allocation across AGI-OS, from process scheduling in the microkernel to inference focus in reasoning systems.

## Attention Values

- **STI** (Short-Term Importance): 0-1000, immediate relevance
- **LTI** (Long-Term Importance): 0-1000, historical significance
- **VLTI** (Very Long-Term Importance): 0-1000, permanent importance

## Mechanisms

1. **Spreading**: Attention spreads through links
2. **Rent**: Atoms pay rent based on STI
3. **Forgetting**: Low-attention atoms forgotten
4. **Hebbian Learning**: Strengthen co-activated links

## Usage in AGI-OS

- **Microkernel**: Process scheduling based on STI
- **OS**: Resource allocation based on attention
- **Cognition**: Inference focus based on attention

## References

- ECAN Documentation: https://wiki.opencog.org/w/ECAN
- Location: `core/cognition/attention/ecan/`
