# AGI-OS Native Code Examples

This directory contains example programs demonstrating the use of the AGI-OS Cognitive Synergy Bridge and its integration layers.

## Building the Examples

To build the examples, ensure you have GCC and the required dependencies installed, then run:

```bash
make
```

## Running the Demo

After building, run the cognitive demonstration:

```bash
./cognitive_demo
```

## Example Programs

### cognitive_demo.c

This comprehensive demo showcases the integration of multiple cognitive computing systems through the Cognitive Synergy Bridge. The demo includes demonstrations of AtomSpace operations for knowledge representation, NARS integration for non-axiomatic reasoning, GGML tensor operations for neural processing, MeTTa integration for meta-programming, and cognitive synergy where all systems work together.

## API Overview

The Cognitive Synergy Bridge provides a unified C API for cognitive computing. The main components include the core bridge API (`cognitive_synergy_bridge.h`) for context management and AtomSpace operations, the NARS integration (`nars_integration.h`) for non-axiomatic reasoning, the GGML integration (`ggml_integration.h`) for tensor operations and neural processing, and the MeTTa integration (`metta_integration.h`) for Hyperon MeTTa runtime.

## License

GPL-3.0 - See the main repository LICENSE file for details.
