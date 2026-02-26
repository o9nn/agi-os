# Hormone Channel Specifications for Simulation

Tuning guide for the 16 hormone channels in CogSim simulation contexts.

## Channel Parameters

Each channel has configurable parameters that affect simulation dynamics:

| ID | Hormone | Half-Life | Baseline | Simulation Role |
|----|---------|-----------|----------|-----------------|
| 0 | CRH | 5 ticks | 0.05 | Initiates stress cascade; triggers emergency routing |
| 1 | ACTH | 10 | 0.05 | Relays stress signal; amplifies HPA response |
| 2 | Cortisol | 30 | 0.15 | Resource mobilization; increases delay times under stress |
| 3 | Dopamine (tonic) | 20 | 0.3 | Baseline motivation; decreases delay times |
| 4 | Dopamine (phasic) | 3 | 0.0 | Reward prediction error; reinforces successful paths |
| 5 | Serotonin | 50 | 0.4 | Patience/accuracy tradeoff; increases quality at cost of speed |
| 6 | Norepinephrine | 8 | 0.1 | Arousal/vigilance; narrows attention, reorders queues |
| 7 | Oxytocin | 15 | 0.1 | Trust/bonding; enables cooperative resource sharing |
| 8 | T3/T4 | 100 | 0.5 | Global processing rate governor; scales all timing |
| 9 | Melatonin | 12 | 0.0 | Circadian maintenance; triggers consolidation phases |
| 10 | Insulin | 10 | 0.2 | Energy conservation; reduces throughput to save resources |
| 11 | Glucagon | 8 | 0.1 | Energy mobilization; increases throughput temporarily |
| 12 | IL-6 | 20 | 0.05 | System health signal; triggers error recovery routines |
| 13 | Anandamide | 6 | 0.1 | Noise reduction; dampens spurious signals |
| 14-15 | Reserved | 10 | 0.0 | Extension slots for domain-specific hormones |

## Tuning for Simulation Scenarios

### High-Throughput Processing

```
T3/T4 baseline: 0.7 (faster global rate)
Dopamine(tonic) baseline: 0.5 (higher motivation)
Cortisol half-life: 50 (slower stress buildup)
```

### Stress-Resilient System

```
Serotonin baseline: 0.6 (higher patience)
Cortisol half-life: 15 (faster stress recovery)
Anandamide baseline: 0.2 (more noise dampening)
```

### Social/Cooperative Agents

```
Oxytocin baseline: 0.3 (higher trust baseline)
Oxytocin half-life: 25 (longer bonding duration)
Serotonin baseline: 0.5 (stable mood)
```

### Emergency Response

```
Norepinephrine half-life: 4 (faster arousal response)
CRH half-life: 3 (rapid stress initiation)
Glucagon baseline: 0.2 (ready energy mobilization)
```

## Decay Dynamics

Each hormone decays exponentially toward its baseline:

```
concentration(t+1) = baseline + (concentration(t) - baseline) * decay_factor
decay_factor = exp(-ln(2) / half_life)
```

**Short half-life** (3-8 ticks): Rapid response, quick recovery. Use for signals that must be transient (phasic dopamine, CRH).

**Medium half-life** (10-20 ticks): Moderate persistence. Use for states that should last several simulation steps (norepinephrine, oxytocin).

**Long half-life** (30-100 ticks): Slow dynamics. Use for background states that change gradually (cortisol, serotonin, T3/T4).

## Mode Prototype Centroids

Each cognitive mode is defined by a prototype vector in 16D hormone space. Mode classification uses nearest-centroid:

```
RESTING:     [all near baseline]
EXPLORATORY: [DA_tonic=0.6, 5HT=0.4, NE=0.2]
FOCUSED:     [NE=0.6, Cort=0.3, DA_tonic=0.4]
STRESSED:    [Cort=0.7, NE=0.6, CRH=0.4, ACTH=0.4]
SOCIAL:      [OXY=0.7, 5HT=0.5, DA_tonic=0.4]
REFLECTIVE:  [5HT=0.7, NE=0.1, DA_tonic=0.3]
VIGILANT:    [NE=0.8, Cort=0.4]
MAINTENANCE: [MEL=0.8, 5HT=0.3]
REWARD:      [DA_phasic=0.8, DA_tonic=0.5]
THREAT:      [Cort=0.9, NE=0.8, CRH=0.7, ACTH=0.6]
```

Adjust prototypes to shift mode boundaries for domain-specific simulation behavior.
