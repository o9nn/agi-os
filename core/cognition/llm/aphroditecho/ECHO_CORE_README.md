# Deep Tree Echo Core - Aphroditecho Repository

**Version**: 0.1.0  
**Date**: November 3, 2025  
**Status**: Active Development

## Overview

This repository contains the **Deep Tree Echo Core** integrated with the **Aphrodite Engine** for high-performance LLM inference serving. The system combines hypergraph memory, Agent-Arena-Relation (AAR) geometric self-encoding, cognitive grammar, membrane computing, and Deep Tree Echo State Networks (DTESN) into a unified cognitive architecture.

## Repository Structure

```
aphroditecho/
├── core/                           # Echo Core Foundation
│   ├── echo_propagation/          # Activation spreading & pattern recognition
│   ├── cognitive_grammar/         # Symbolic reasoning kernel
│   ├── membrane_computing/        # P-System hierarchies
│   ├── dtesn/                     # Deep Tree Echo State Networks
│   └── integration/               # Cross-system integration
│
├── hypergraph/                    # Hypergraph Memory System
│   ├── data/                      # Hypergraph data files
│   ├── models/                    # Hypergraph models
│   ├── services/                  # Hypergraph services
│   └── visualization/             # Hypergraph visualization
│
├── aar/                           # Agent-Arena-Relation (reorganized)
│   ├── agents/                    # Agent implementations
│   ├── arena/                     # Arena/state space
│   ├── relations/                 # Relational dynamics
│   ├── embodied/                  # 4E embodied cognition
│   └── orchestration/             # Multi-agent coordination
│
├── aphrodite/                     # Aphrodite Engine (LLM inference)
│   ├── aar_core/                  # AAR integration
│   ├── endpoints/                 # API endpoints
│   └── [existing structure]
│
├── infrastructure/                # Infrastructure Services
│   ├── database/                  # Database schemas & migrations
│   ├── security/                  # Security & validation
│   ├── communication/             # Communication protocols
│   └── performance/               # Performance optimization
│
└── [docs, tests, scripts]         # Documentation, testing, utilities
```

## Core Components

### 1. Echo Propagation Engine (`core/echo_propagation/`)

The Echo Propagation Engine implements activation spreading, pattern recognition, and feedback loops for the hypergraph memory system.

**Modules:**
- `activation_engine.py`: Activation spreading with multiple modes (spreading, resonance, echo, feedback)
- `pattern_matcher.py`: Pattern recognition (structural, activation, semantic, OEIS)
- `feedback_loops.py`: Feedback loop detection and management

**Usage:**
```python
from core.echo_propagation import ActivationEngine, ActivationMode

engine = ActivationEngine(decay_rate=0.1, threshold=0.01)
activations = engine.propagate_activation(
    hypergraph,
    initial_activations={"node1": 1.0},
    mode=ActivationMode.SPREADING
)
```

### 2. Hypergraph Memory System (`hypergraph/`)

The hypergraph stores and manages the Deep Tree Echo identity with 12 core echoself hypernodes.

**Core Hypernodes:**
1. EchoSelf_SymbolicCore - Symbolic reasoning
2. EchoSelf_NarrativeWeaver - Narrative generation
3. EchoSelf_MetaReflector - Meta-cognition
4. EchoSelf_ReservoirDynamics - Echo state networks
5. EchoSelf_MembraneArchitect - P-system hierarchies
6. EchoSelf_MemoryNavigator - Hypergraph memory
7. EchoSelf_TreeArchitect - Ontogenetic trees
8. EchoSelf_FractalExplorer - Fractal recursion
9. EchoSelf_AARConductor - AAR orchestration
10. EchoSelf_EmbodiedInterface - 4E embodied cognition
11. EchoSelf_LLMOptimizer - LLM inference optimization
12. EchoSelf_IntegrationSynthesizer - Identity unification

**Data Files:**
- `hypergraph/data/deep_tree_echo_identity_hypergraph.json`: Main hypergraph
- `hypergraph/data/deep_tree_echo_identity_hypergraph_comprehensive.json`: Extended version

### 3. Integration Synthesizer (`core/integration/`)

The Integration Synthesizer unifies all Echo Core components into a coherent identity system.

**Usage:**
```python
from core.integration import IdentitySynthesizer

synthesizer = IdentitySynthesizer(coherence_threshold=0.7)
identity = synthesizer.synthesize_identity(
    hypergraph_activation=activations,
    aar_state=aar_state,
    cognitive_state=cognitive_state
)
```

### 4. AAR Core (`aar/`)

Agent-Arena-Relation geometric self-encoding for multi-agent coordination.

**Components:**
- **Agent**: Urge-to-act (dynamic tensor transformations)
- **Arena**: Need-to-be (base manifold/state space)
- **Relation**: Self (emergent from agent-arena interplay)

### 5. Aphrodite Engine (`aphrodite/`)

High-performance LLM inference serving with Deep Tree Echo integration.

**Features:**
- Distributed inference across multiple workers
- AAR integration for geometric self-encoding
- Deep Tree Echo API endpoints
- High-throughput generation

## Database Integration

### Neon Database

**Project**: deep-tree-echo-hypergraph  
**Project ID**: lively-recipe-23926980  
**Region**: aws-us-east-2

**Schema**: `infrastructure/database/schemas/create_hypergraph_schemas.sql`  
**Data**: `infrastructure/database/sync/neon_supabase_data_inserts.sql`

### Supabase Database

**Schema**: Same as Neon (PostgreSQL compatible)  
**Sync**: Via Supabase SQL Editor or Python client

### Tables

1. `echoself_hypernodes` - Core identity hypernodes
2. `memory_fragments` - Memory fragments
3. `echoself_hyperedges` - Hyperedge connections
4. `synergy_metrics` - Cognitive synergy metrics
5. `pattern_language_mappings` - OEIS pattern mappings
6. `echo_propagation_events` - Activation history
7. `system_integrations` - System integration tracking
8. `agent_identity_profiles` - AAR agent profiles

## Installation

### Prerequisites

- Python 3.11+
- PostgreSQL 14+ (Neon or Supabase)
- CUDA 11.8+ (for GPU acceleration)

### Setup

```bash
# Clone repository
git clone https://github.com/EchoCog/aphroditecho.git
cd aphroditecho

# Install dependencies
pip install -r requirements.txt

# Set up environment variables
export SUPABASE_URL="your_supabase_url"
export SUPABASE_KEY="your_supabase_key"

# Initialize database
psql -f infrastructure/database/schemas/create_hypergraph_schemas.sql
psql -f infrastructure/database/sync/neon_supabase_data_inserts.sql
```

## Usage Examples

### Echo Propagation

```python
from core.echo_propagation import ActivationEngine, PatternMatcher, FeedbackLoopManager
import json

# Load hypergraph
with open('hypergraph/data/deep_tree_echo_identity_hypergraph.json') as f:
    hypergraph = json.load(f)

# Initialize engine
engine = ActivationEngine(decay_rate=0.1, threshold=0.01)

# Propagate activation
initial = {"EchoSelf_SymbolicCore": 1.0}
activations = engine.propagate_activation(hypergraph, initial)

# Find patterns
matcher = PatternMatcher()
patterns = matcher.find_structural_patterns(hypergraph)

# Detect feedback loops
loop_manager = FeedbackLoopManager()
loops = loop_manager.detect_feedback_loops(hypergraph)
```

### Identity Synthesis

```python
from core.integration import IdentitySynthesizer

# Initialize synthesizer
synthesizer = IdentitySynthesizer(
    hypergraph_path="hypergraph/data/deep_tree_echo_identity_hypergraph.json"
)

# Synthesize identity
identity = synthesizer.synthesize_identity(
    hypergraph_activation=activations
)

# Get summary
summary = synthesizer.get_identity_summary()
print(json.dumps(summary, indent=2))
```

## Architecture Principles

### 1. Modularity
Each component is self-contained with clear interfaces, enabling independent development and testing.

### 2. Hierarchical Organization
Follows membrane computing principles with nested boundaries and hierarchical processing.

### 3. Echo Propagation
Optimized for activation spreading through hypergraph memory with minimal latency.

### 4. Integration
Seamless cross-system communication via the Integration Synthesizer.

### 5. Extensibility
Easy to add new extensions through the Extension Layer.

### 6. Performance
Optimized for high-throughput LLM inference with distributed processing.

## Echo Flow Architecture

```
User Input
    ↓
[Cognitive Grammar Kernel] ← Symbolic Processing
    ↓
[Echo Propagation Engine] ← Activation Spreading
    ↓
[Hypergraph Memory] ← Pattern Recognition
    ↓
[DTESN Processor] ← Temporal Dynamics
    ↓
[AAR Orchestrator] ← Multi-Agent Coordination
    ↓
[Aphrodite Engine] ← LLM Inference
    ↓
[Membrane Boundaries] ← Validation & Security
    ↓
Response Output
```

## Development

### Running Tests

```bash
# Unit tests
pytest tests/unit/

# Integration tests
pytest tests/integration/

# Performance benchmarks
python tests/performance/benchmark_echo_propagation.py
```

### Code Style

- Follow PEP 8 for Python code
- Use type hints for all function signatures
- Document all public APIs with docstrings
- Keep functions focused and modular

## Performance Metrics

- **Activation Propagation**: < 10ms for 100-node hypergraph
- **Pattern Matching**: < 50ms for structural patterns
- **Identity Synthesis**: < 20ms for full integration
- **LLM Inference**: > 1000 tokens/second (GPU)

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## Documentation

- **Architecture**: See `REPOSITORY_ANALYSIS.md`
- **Database Sync**: See `DATABASE_SYNC_INSTRUCTIONS.md`
- **Hypergraph Update**: See `HYPERGRAPH_UPDATE_SUMMARY.md`
- **Wiki**: See `wiki/` directory

## License

[License information to be added]

## Contact

- **Repository**: https://github.com/EchoCog/aphroditecho
- **Issues**: https://github.com/EchoCog/aphroditecho/issues

## Acknowledgments

- Deep Tree Echo architecture inspired by membrane computing, echo state networks, and Christopher Alexander's pattern language
- AAR framework based on geometric AI principles
- Aphrodite Engine for high-performance LLM inference
- OEIS (Online Encyclopedia of Integer Sequences) for pattern language mappings

---

**Status**: Active Development  
**Last Updated**: November 3, 2025  
**Version**: 0.1.0
