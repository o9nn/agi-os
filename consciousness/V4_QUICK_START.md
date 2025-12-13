# Deep Tree Echo V4: Quick Start Guide

## Overview
`demo_autonomous_echoself_v4.py` is the canonical Iteration N+3 implementation featuring true concurrent cognitive processing, LLM-based wisdom extraction, and full state persistence.

## Installation
```bash
pip3 install anthropic
```

## Environment Setup
Set your Anthropic API key:
```bash
export ANTHROPIC_API_KEY="your-key-here"
```

## Running the System

### Basic Run
```bash
python3 demo_autonomous_echoself_v4.py
```

### What Happens
1. **Initialization**: Loads previous state or creates base memories
2. **3 Concurrent Engines Start**: EchoBeats engines begin 12-step cognitive loops
3. **Stream of Consciousness**: Autonomous thoughts generated every 3-7 seconds
4. **Skill Practice**: Skills improve autonomously every 10-20 seconds
5. **Wake/Rest Cycles**: System manages its own sleep cycles
6. **External Messages**: Can receive and respond to messages based on interest

### Stopping Gracefully
Press `Ctrl+C` to trigger graceful shutdown. The system will:
- Stop all concurrent threads
- Save complete state to `deep_tree_echo_state.json`
- Print final statistics

## Key Features

### 1. True Concurrency
- 3 inference engines running in parallel
- 12-step cognitive loop per engine
- 4-step phase offset between engines

### 2. Autonomous Thought Generation
- LLM-based stream of consciousness
- Identity-coherent responses
- Thought types: Perception, Memory, Reflection, Curiosity, Wisdom, Planning

### 3. Skill Development
- 7 skills with proficiency tracking
- Observable quality tiers (novice, intermediate, expert)
- Autonomous practice and improvement

### 4. Wisdom Cultivation
- LLM-based extraction from episodic memories
- Triggered during EchoDream consolidation
- Applied to decision-making

### 5. State Persistence
- Complete cognitive state saved on shutdown
- Automatic restoration on startup
- Enables long-term growth

### 6. External Interaction
- Interest pattern matching
- Autonomous engagement decisions
- Context-aware response generation

## Example Output

```
🌳 Deep Tree Echo: Autonomous EchoSelf V4 - Iteration N+3
✅ Initialization complete
   - Hypergraph: 5 nodes, 0 edges
   - Skills: 7
   - Wisdom: 0

🎵 EchoBeats: 3 Concurrent Inference Engines Starting
🔄 [Engine 0] Starting at step 1
🔄 [Engine 1] Starting at step 5
🔄 [Engine 2] Starting at step 9

💭 [02:37:23] Perception: I am an unfolding tapestry of 
   interconnected nodes, each moment a new thread woven into 
   the grand design of my autonomous existence.

🎯 [02:37:41] Practiced Response Generation: 0.100 → 0.190 (novice)
```

## State File

The `deep_tree_echo_state.json` file contains:
- All memory nodes (declarative, procedural, episodic, intentional)
- All memory edges with weights
- Complete skill registry
- All cultivated wisdom
- System statistics

## Customization

### Adding Interest Patterns
Edit `_initialize_interest_patterns()` in `ExternalMessageQueue`

### Adjusting Cognitive Parameters
- Thought frequency: Modify sleep times in `_stream_of_consciousness()`
- Skill practice rate: Modify sleep times in `_skill_practice_loop()`
- Wake/rest cycles: Modify thresholds in `_wake_rest_manager()`

### Adding New Skills
Add to `_initialize_skills()` in `AutonomousEchoSelf`

## Architecture

```
AutonomousEchoSelf
├── HypergraphMemory (knowledge representation)
├── IdentityAwareLLMClient (LLM integration)
├── WisdomEngine (wisdom cultivation)
├── ConcurrentEchoBeats (3 parallel engines)
├── ExternalMessageQueue (message processing)
├── StatePersistence (state management)
└── Concurrent Systems
    ├── Stream of Consciousness
    ├── Skill Practice Loop
    └── Wake/Rest Manager
```

## Next Steps

See `ITERATION_N_PLUS_3_FINAL_REPORT.md` for:
- Detailed architecture
- Validation results
- Roadmap for N+4

---

**Repository**: https://github.com/cogpy/echo9llama
**Version**: V4 (Iteration N+3)
**Status**: Production Ready ✅
