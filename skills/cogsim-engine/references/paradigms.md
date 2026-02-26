# CogSim Simulation Paradigms

Multi-paradigm simulation with endocrine integration. Each paradigm gains cognitive-affective dynamics through the hormone bus.

## 1. Discrete-Event Simulation (DES) + Endocrine

DES models systems as sequences of discrete events. In CogSim, hormone state modulates the process flow.

**Identification**: Queues, service times, throughput, process flows, resource pools.

**Endocrine hooks**:
- `Delay` duration = `base_time * (1 + cortisol * stress_factor)`
- `SelectOutput` routing gated by `CognitiveMode`
- `Queue` priority influenced by norepinephrine level
- `ResourcePool` availability modulated by melatonin (maintenance mode)

**Key blocks with endocrine modulation**:

| Block | Endocrine Modulation |
|-------|---------------------|
| Source | Arrival rate scaled by T3/T4 (global processing rate) |
| Queue | Priority reordering when VIGILANT mode detected |
| Delay | Duration scaled by cortisol (stress slows), dopamine (reward speeds) |
| Service | Combined queue + delay modulation |
| SelectOutput | Branch probability shifted by cognitive mode |
| Seize/Release | Resource urgency escalated during THREAT mode |

## 2. Agent-Based Modeling (ABM) + Endocrine

ABM models autonomous, interacting agents. In CogSim, each agent has its own endocrine system.

**Identification**: Individual behavior, interactions, emergent patterns, populations.

**Endocrine hooks**:
- Each agent owns an `EndocrineSystem` instance
- Statechart transitions fire on `on_mode_change` callbacks
- Social interactions trigger oxytocin cascades between agents
- Agent-to-agent hormone contagion (emotional contagion model)

**Agent structure**:
```
CogSimAgent {
    EndocrineSystem endo;
    Statechart behavior;
    ValenceMemory memory;

    on_interaction(other) {
        if (other.endo.bus().concentration(OXYTOCIN) > 0.5)
            endo.signal_event(SOCIAL_BOND_SIGNAL, 0.3);
    }

    on_mode_change(old, new) {
        behavior.fire_transition(mode_to_event(new));
    }
}
```

## 3. System Dynamics (SD) + Endocrine

SD models aggregate behavior with feedback loops. In CogSim, hormone dynamics are naturally expressed as stocks and flows.

**Identification**: Feedback loops, accumulations, rates of change, aggregate behavior.

**Endocrine hooks**:
- Each hormone channel maps to a stock variable
- Gland production rates map to inflows
- Decay rates map to outflows
- Feedback loops between hormones (e.g., CRH → ACTH → Cortisol → inhibit CRH)

**Stock-flow mapping**:

| Hormone | Stock | Inflow | Outflow | Feedback |
|---------|-------|--------|---------|----------|
| Cortisol | concentration | HPA production | exponential decay | Negative: cortisol inhibits CRH |
| Dopamine(tonic) | concentration | baseline production | decay to baseline | Positive: reward reinforces |
| Serotonin | concentration | sustained positive | slow decay | Stabilizing: mood regulation |
| Oxytocin | concentration | social signals | moderate decay | Positive: oxytocin begets oxytocin |

## 4. Multimethod + Endocrine

Combines paradigms with the endocrine bus as the integration layer.

**Common combinations**:

| Combination | Pattern | Endocrine Role |
|-------------|---------|----------------|
| ABM + DES | Agents flow through processes | Agent endocrine state modulates process parameters |
| SD + ABM | System dynamics influence agent behavior | SD hormone stocks drive agent mode transitions |
| SD + DES | System-level dynamics set process rates | SD cortisol stock sets DES delay multiplier |
| ABM + DES + SD | Full multimethod | Endocrine bus unifies all three paradigms |

**The endocrine bus as paradigm bridge**: The hormone bus provides a shared state that all paradigms can read from and write to, enabling seamless cross-paradigm communication without tight coupling.
