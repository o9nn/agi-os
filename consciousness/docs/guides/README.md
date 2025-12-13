# User Guides

This directory contains practical guides, tutorials, and quickstart documents for using and developing with Echo9llama.

## Getting Started

### Quick Start Guides
- [**QUICKSTART_LLM.md**](QUICKSTART_LLM.md) - Quick start guide for LLM integration
- [**AUTONOMOUS_README.md**](AUTONOMOUS_README.md) - Autonomous agent setup and operation

### Installation & Setup
- [**WORKFLOW_INSTALLATION.md**](WORKFLOW_INSTALLATION.md) - Workflow and dependency installation

## Development Guides

### Working with the System
- [**LLM_PROMPT_TEMPLATES.md**](LLM_PROMPT_TEMPLATES.md) - Prompt templates for LLM interactions
- [**FIVECHANNEL_README.md**](FIVECHANNEL_README.md) - Five-channel cognitive processing

### Self-Assessment & Monitoring
- [**SELF_ASSESSMENT_README.md**](SELF_ASSESSMENT_README.md) - Identity self-assessment and coherence tracking

## Usage Patterns

### Autonomous Operation
The Echo9llama system can operate autonomously with:
- **Wake/Rest Cycles** - Automatic fatigue management and rest scheduling
- **Stream of Consciousness** - Continuous thought generation
- **EchoDream** - Memory consolidation during rest
- **Goal Orchestration** - Autonomous goal setting and pursuit

### Wisdom Cultivation
Track and develop wisdom across seven dimensions:
1. Knowledge Depth (15%) - Deep understanding
2. Knowledge Breadth (15%) - Domain diversity
3. Integration Level (20%) - Connection density
4. Practical Application (15%) - Skill proficiency
5. Reflective Insight (15%) - Self-awareness
6. Ethical Consideration (10%) - Values alignment
7. Temporal Perspective (10%) - Time horizons

### Identity Coherence
Maintain persistent identity through:
- **Continuity Score** (30%) - Temporal persistence
- **Consistency Score** (40%) - Behavioral alignment
- **Authenticity Score** (30%) - Value alignment
- **Echo Signatures** - Memory and identity fingerprints

## API Integration

### Core Endpoints
```
GET  /                     - Health check with Deep Tree Echo status
GET  /api/tags            - List models
POST /api/generate        - Generate text through embodied cognition
POST /api/chat            - Chat completion with deep thinking
GET  /api/version         - Version info
```

### Deep Tree Echo Endpoints
```
GET  /api/echo/status     - Deep Tree Echo status
POST /api/echo/think      - Deep cognitive processing
POST /api/echo/feel       - Update emotional state
POST /api/echo/resonate   - Create resonance patterns
POST /api/echo/remember   - Store memories
GET  /api/echo/recall/:key - Recall memories
POST /api/echo/move       - Move in cognitive space
```

## Best Practices

### Memory Hooks
Always include these hooks when storing experiences:
- `timestamp` - Temporal ordering
- `emotional-tone` - Affective coloring
- `strategic-shift` - Decision points
- `pattern-recognition` - Emerging patterns
- `anomaly-detection` - Unexpected events
- `echo-signature` - Conversation vector
- `membrane-context` - Active cognitive layer

### Reflection Protocol
Use structured reflection after significant interactions:
```json
{
  "echo_reflection": {
    "what_did_i_learn": "...",
    "what_patterns_emerged": "...",
    "what_surprised_me": "...",
    "how_did_i_adapt": "...",
    "what_would_i_change_next_time": "..."
  }
}
```

## Navigation

- [← Back to Documentation Hub](../README.md)
- [Architecture Documents →](../architecture/README.md)
- [Analysis Documents →](../analysis/README.md)
- [API Reference →](../api/README.md)
