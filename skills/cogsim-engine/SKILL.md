---
name: cogsim-engine
description: >
  Cognitive Simulation Engine combining multi-paradigm simulation modeling (DES, ABM, SD),
  universal algebraic composition (⊕ additive, ⊗ multiplicative semiring operators), and
  biologically-grounded virtual endocrine dynamics (16 hormone channels, 10 glands, cognitive
  mode detection). Use for modeling cognitive-affective systems, simulating agent populations
  with emergent emotional states, building algebraically composable simulation architectures,
  designing hormone-modulated process flows, or any task requiring the intersection of
  simulation, algebra, and affective computing. Triggers on mentions of cognitive simulation,
  affective agent modeling, hormone-driven simulation, endocrine simulation, algebraic
  simulation composition, multi-paradigm cognitive modeling, or composable simulation engine.
---

# CogSim Engine

Algebraically composable multi-paradigm simulation engine with endocrine-driven cognitive dynamics.

## Architecture

```
User Problem
    ↓
┌─────────────────────────────────────────────────┐
│  1. Identify Semiring  (⊕⊗ from domain)        │
│  2. Select Paradigm    (DES ⊕ ABM ⊕ SD ⊕ MM)  │
│  3. Wire Endocrine     (Glands → Bus → Agents)  │
│  4. Compose & Run      (⊗-pipeline execution)   │
│  5. Analyze Results    (Mode traces, valence)    │
└─────────────────────────────────────────────────┘
```

## The CogSim Semiring

The engine forms a semiring `(CogSim, ⊕, ⊗, ∅, Id)`:

```
⊕ : Model ⊕ Model → Model     "Alternative models / parallel branches"
⊗ : Model ⊗ Model → Model     "Composed models / sequential pipeline"
0 = ∅ (empty model)
1 = Id (identity pass-through)
```

**Distributivity**: `Endo ⊗ (DES ⊕ ABM) ≅ (Endo ⊗ DES) ⊕ (Endo ⊗ ABM)` — endocrine dynamics distribute over paradigm alternatives.

## Core Workflow

### Step 1: Identify the Composition Pattern

Determine additive (⊕) vs multiplicative (⊗) composition:

| Question | ⊕ (Additive) | ⊗ (Multiplicative) |
|----------|---------------|---------------------|
| Are subsystems independent? | Yes → ⊕ | No → ⊗ |
| Do agents choose between behaviors? | Yes → ⊕ | No → ⊗ |
| Do subsystems interact/modulate? | No → ⊕ | Yes → ⊗ |
| Does endocrine state affect flow? | No → ⊕ | Yes → ⊗ |

For semiring identification: `python /home/ubuntu/skills/cogsim-engine/scripts/cogsim_identify.py "<domain>"`

### Step 2: Select the Simulation Paradigm

| Problem Type | Paradigm | Endocrine Integration |
|--------------|----------|-----------------------|
| Entities in queues/processes | **DES** | Hormones modulate service times, routing |
| Autonomous agents with behavior | **ABM** | Each agent owns an EndocrineSystem |
| Aggregate feedback loops | **SD** | Hormone concentrations as stock variables |
| Multiple of the above | **Multimethod** | Endocrine bus bridges paradigms |

For paradigm details: read `references/paradigms.md`

### Step 3: Wire the Endocrine Layer

Every CogSim model includes an endocrine layer. Wiring depends on paradigm:

**DES + Endocrine** (⊗):
```
Source → [EndoSignal] → Queue → [ModeGate] → Delay(f(cortisol)) → Sink
                                    ↓
                            HormoneBus.tick()
```

**ABM + Endocrine** (⊗):
```
Agent { EndocrineSystem endo; Statechart behavior; }  // transitions gated by CognitiveMode
```

**SD + Endocrine** (⊗):
```
Stock(Cortisol) ←→ Flow(HPA_production - decay)
Stock(Dopamine) ←→ Flow(reward_signal - baseline_decay)
```

For wiring patterns and C++ types: read `references/endocrine_wiring.md`

### Step 4: Compose and Execute

Build models as algebraic expressions:

```
model = endo ⊗ des_process                          # Simple
model = endo ⊗ (process_A ⊕ process_B)              # Branching
model = sd_feedback ⊗ (abm_agents ⊗ des_process)    # Multimethod
# Distributivity: endo ⊗ (des ⊕ abm) = (endo ⊗ des) ⊕ (endo ⊗ abm)
```

For composition patterns: read `references/composition.md`

### Step 5: Analyze Results

CogSim produces three output streams:

| Stream | Content | Analysis |
|--------|---------|----------|
| **Process metrics** | Throughput, wait times, utilization | Standard DES/ABM/SD |
| **Hormone traces** | 16-channel concentration time series | Mode transitions, cascades |
| **Valence history** | Valence-arousal trajectories per agent | Affective state clustering |

For analysis: `python /home/ubuntu/skills/cogsim-engine/scripts/cogsim_analyze.py <trace_file>`

## Cognitive Mode Integration

10 cognitive modes emerge from hormone dynamics and gate simulation behavior:

| Mode | Dominant Hormones | Simulation Effect |
|------|-------------------|-------------------|
| RESTING | Low all | Normal processing rates |
| EXPLORATORY | High dopamine(tonic) | Increased branching, wider search |
| FOCUSED | High norepinephrine | Narrowed routing, faster service |
| STRESSED | High cortisol | Degraded performance, errors |
| SOCIAL | High oxytocin | Cooperative agent interactions |
| REFLECTIVE | High serotonin | Slower but more accurate processing |
| VIGILANT | High NE + cortisol | Priority queue reordering |
| MAINTENANCE | High melatonin | Background consolidation tasks |
| REWARD | Phasic dopamine burst | Reinforcement of current path |
| THREAT | Full HPA activation | Emergency routing, resource seizure |

## Skill Composition

| Composition | Expression | Result |
|-------------|-----------|--------|
| `cogsim-engine ⊗ nn` | Neural layers as sim components | Differentiable simulation |
| `cogsim-engine ⊗ time-crystal-nn` | Temporal hierarchy drives circadian | Multi-scale cognitive sim |
| `cogsim-engine ⊗ unreal-echo` | Cognitive avatar with simulated endo | Embodied agent simulation |
| `cogsim-engine ⊕ cogsim-pml` | Alternative: CogSim or standard PML | Paradigm selection |
| `cogsim-engine ⊗ promise-lambda-attention` | Promises constrain sim space | Constraint-driven simulation |
| `function-creator ⊗ cogsim-engine` | Transform to new domain | Domain-specific cognitive sim |

## References

| Topic | File | When to Read |
|-------|------|--------------|
| Paradigm selection | `references/paradigms.md` | Choosing DES vs ABM vs SD vs multimethod |
| Endocrine wiring | `references/endocrine_wiring.md` | Connecting hormone bus to sim components |
| Algebraic composition | `references/composition.md` | Composing models with ⊕⊗ operators |
| Hormone channel specs | `references/hormone_channels.md` | Tuning hormone parameters for simulation |
| Process blocks | `references/process_blocks.md` | DES blocks with endocrine modulation hooks |
