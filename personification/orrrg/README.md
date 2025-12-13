# ORRRG - Hierarchical Autognosis System

ORRRG implements **hierarchical self-image building** - a breakthrough capability that enables the system to understand, monitor, and optimize its own cognitive processes.

## Features

- **Hierarchical Self-Modeling**: Multi-level self-image construction with recursive awareness
- **Meta-Cognitive Insights**: Higher-order reasoning about own cognitive processes
- **Self-Optimization**: Autonomous improvement based on introspective analysis
- **Behavioral Pattern Detection**: Identification of system behavioral trends
- **Confidence Scoring**: Uncertainty quantification for self-understanding

## Installation

```bash
cd orrrg
pip install -e .
```

## Quick Start

### CLI Usage

```bash
# Basic autognosis status
orrrg autognosis

# Detailed report
orrrg autognosis report

# Self-awareness analysis
orrrg autognosis insights
```

### Python API

```python
from core import SelfOrganizingCore

# Initialize ORRRG with autognosis
soc = SelfOrganizingCore()
await soc.initialize()

# Get autognosis status
status = soc.get_autognosis_status()

# Run manual cycle
cycle_results = await soc.autognosis.run_autognosis_cycle(soc)

# Access current self-images
self_images = soc.autognosis.current_self_images
```

## Architecture

The autognosis system operates through four interconnected layers:

1. **Self-Monitoring Layer** - Continuous observation of system states
2. **Self-Modeling Layer** - Hierarchical self-image construction
3. **Meta-Cognitive Layer** - Higher-order reasoning and insight generation
4. **Self-Optimization Layer** - Adaptive improvements based on self-insights

## Documentation

See [AUTOGNOSIS.md](../.github/agents/AUTOGNOSIS.md) for detailed documentation.

## License

MIT
