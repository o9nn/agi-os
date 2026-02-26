# Process Blocks with Endocrine Modulation

DES process blocks enhanced with hormone-driven cognitive dynamics.

## Endocrine-Modulated Blocks

### EndoSource

Generates entities with arrival rate modulated by T3/T4 (global processing rate).

```
arrival_rate = base_rate * (0.5 + T3_T4)
```

When T3/T4 is at baseline (0.5), rate equals base_rate. Higher T3/T4 increases throughput.

### EndoQueue

Queue with priority reordering based on cognitive mode:

| Mode | Queue Behavior |
|------|---------------|
| RESTING | FIFO (default) |
| VIGILANT | Priority: urgent entities first |
| THREAT | Priority: emergency entities only |
| SOCIAL | FIFO with cooperative yielding |
| STRESSED | Random (degraded ordering) |

### EndoDelay

Delay duration modulated by hormone concentrations:

```
effective_delay = base_delay
    * (1.0 + cortisol * 0.5)       // stress increases delay
    * (1.0 - dopamine_tonic * 0.3) // motivation decreases delay
    * (0.5 + T3_T4)               // global rate scaling
    * (1.0 + serotonin * 0.2)     // patience increases thoroughness
```

### EndoService

Combined EndoQueue + EndoDelay with resource modulation:

- Resource efficiency = `1.0 - cortisol * 0.3 + dopamine_tonic * 0.2`
- When MAINTENANCE mode: resources enter repair cycle (temporarily unavailable)
- When THREAT mode: all available resources seized immediately

### EndoSelectOutput

Routing gated by cognitive mode (see mode-gated routing in `endocrine_wiring.md`).

Additional modulation:
- Norepinephrine > 0.6: bias toward cautious/safe routes
- Dopamine(phasic) > 0.5: bias toward novel/exploratory routes
- Serotonin > 0.6: bias toward established/proven routes

### EndoSeize / EndoRelease

Resource acquisition with urgency scaling:

```
urgency = base_priority
    + norepinephrine * 2.0   // arousal increases urgency
    + cortisol * 1.5         // stress increases urgency
    - serotonin * 0.5        // patience decreases urgency
```

During THREAT mode: preemptive seizure (can interrupt lower-priority holders).

### EndoSink

Entity destruction with valence tagging:

```
on_entity_exit(entity) {
    ValenceSignature vs = compute_valence(entity);
    valence_memory.tag(entity.id, vs);
    // Successful completion → positive valence → dopamine signal
    if (entity.completed_successfully)
        endo.signal_event(GOAL_ACHIEVED, 0.6);
    // Failed/timed-out → negative valence → stress signal
    else
        endo.signal_event(ERROR_DETECTED, 0.4);
}
```

## Composition Patterns

### Linear Pipeline (⊗)

```
EndoSource ⊗ EndoQueue ⊗ EndoDelay ⊗ EndoSink
```

All blocks share one hormone bus. Mode changes propagate through the pipeline.

### Branching (⊕)

```
EndoSource ⊗ EndoSelectOutput ⊗ (PathA ⊕ PathB ⊕ PathC) ⊗ EndoSink
```

Mode selects among alternative paths. Each path may have different endocrine sensitivity.

### Parallel Resources (⊕)

```
EndoSource ⊗ EndoSeize ⊗ (ResourceA ⊕ ResourceB) ⊗ EndoRelease ⊗ EndoSink
```

Resources are alternatives. Hormone state influences which resource is preferred.

### Feedback Loop (⊗ with cycle)

```
EndoSource ⊗ Process ⊗ QualityCheck ⊗ [pass: EndoSink ⊕ fail: Process]
```

Failed quality checks re-enter the process. Each failure triggers ERROR_DETECTED, raising cortisol and degrading subsequent performance — a realistic stress spiral.
