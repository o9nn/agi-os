# AGI-OS Native Code Integration

This directory contains external cognitive computing repositories integrated into AGI-OS for native code execution and cognitive synergy.

## Integrated Components

### 1. Hyperon (MeTTa Language)
**Source:** [trueagi-io/hyperon-experimental](https://github.com/trueagi-io/hyperon-experimental)

MeTTa (Meta Type Talk) is the next-generation Atomese language for OpenCog Hyperon. It provides:
- Rust-based implementation with Python bindings
- Type-theoretic foundations
- Meta-programming capabilities
- Integration with Distributed AtomSpace (DAS)

**Build:** Requires Rust/Cargo
```bash
cd hyperon && cargo build --release
```

### 2. MeTTa-WAM (Warren Abstract Machine)
**Source:** [trueagi-io/metta-wam](https://github.com/trueagi-io/metta-wam)

Efficient MeTTa interpreter/transpiler targeting the Warren Abstract Machine:
- Prolog-style execution model
- Optimized pattern matching
- SWI-Prolog integration

### 3. DAS (Distributed AtomSpace)
**Source:** [singnet/das](https://github.com/singnet/das)

Scalable distributed knowledge representation:
- Distributed query API
- MeTTa integration
- Redis/MongoDB backends
- gRPC communication

### 4. OpenNARS for Applications
**Source:** [opennars/OpenNARS-for-Applications](https://github.com/opennars/OpenNARS-for-Applications)

Non-Axiomatic Reasoning System in pure C:
- Real-time reasoning
- Minimal dependencies
- Event-driven architecture
- NAL (Non-Axiomatic Logic) implementation

**Build:**
```bash
cd opennars && ./build.sh
```

### 5. GGML (Tensor Library)
**Source:** [ggml-org/ggml](https://github.com/ggml-org/ggml)

Lightweight tensor library for machine learning:
- CPU-optimized operations
- CUDA/OpenCL support
- Quantization support
- Used by llama.cpp

**Build:**
```bash
cd ggml-core && mkdir build && cd build && cmake .. && make
```

### 6. Soar Cognitive Architecture (Patterns)
**Source:** [SoarGroup/Soar](https://github.com/SoarGroup/Soar)

General cognitive architecture patterns:
- Production system
- Working memory
- Long-term memory
- Chunking/learning

### 7. Inferno OS Components
**Source:** [inferno-os/inferno-os](https://github.com/inferno-os/inferno-os)

Distributed OS primitives:
- 9P protocol implementation
- Limbo language runtime
- Namespace management
- Distributed computing patterns

## Cognitive Synergy Bridge

The `cognitive_synergy_bridge.h` header provides a unified C API for:
- Knowledge representation (AtomSpace)
- Reasoning (NARS/PLN)
- Neural processing (GGML)
- MeTTa execution
- Distributed queries (DAS)
- Cognitive cycles (Soar-inspired)

## Building

### Prerequisites
- CMake 3.16+
- C11/C++17 compiler
- Rust (for Hyperon)
- Python 3.8+ (for DAS)

### Build Commands
```bash
mkdir build && cd build
cmake .. -DBUILD_OPENNARS=ON -DBUILD_GGML=ON
make -j$(nproc)
```

### Build Options
| Option | Default | Description |
|--------|---------|-------------|
| BUILD_HYPERON | ON | Build Hyperon MeTTa |
| BUILD_OPENNARS | ON | Build OpenNARS |
| BUILD_GGML | ON | Build GGML tensors |
| BUILD_DAS | ON | Build DAS integration |
| BUILD_SOAR_PATTERNS | OFF | Build Soar patterns |
| BUILD_INFERNO_COMPONENTS | OFF | Build Inferno components |

## Integration with AGI-OS

These components integrate with the AGI-OS cognitive architecture:

```
┌─────────────────────────────────────────────────────────────┐
│                      AGI-OS Core                            │
├─────────────────────────────────────────────────────────────┤
│  CogNumach (Microkernel)  │  HurdCog (OS Services)          │
├─────────────────────────────────────────────────────────────┤
│              Cognitive Synergy Bridge                       │
├──────────┬──────────┬──────────┬──────────┬────────────────┤
│ Hyperon  │ OpenNARS │   GGML   │   DAS    │ Inferno/9P     │
│ (MeTTa)  │ (NARS)   │ (Tensor) │ (Dist.)  │ (Distributed)  │
└──────────┴──────────┴──────────┴──────────┴────────────────┘
```

## License

Each component retains its original license:
- Hyperon: MIT
- OpenNARS: MIT
- GGML: MIT
- DAS: Apache 2.0
- Soar: BSD
- Inferno: MIT/LPL

The integration code (CMakeLists.txt, cognitive_synergy_bridge.h) is GPL-3.0.
