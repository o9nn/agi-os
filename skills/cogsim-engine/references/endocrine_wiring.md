# Endocrine Wiring Patterns

How to connect the Virtual Endocrine System to simulation components.

## Table of Contents

1. [Core Types](#core-types)
2. [Wiring Pattern: DES](#wiring-pattern-des)
3. [Wiring Pattern: ABM](#wiring-pattern-abm)
4. [Wiring Pattern: SD](#wiring-pattern-sd)
5. [Wiring Pattern: Multimethod](#wiring-pattern-multimethod)
6. [Event Mapping](#event-mapping)
7. [Mode-Gated Routing](#mode-gated-routing)

---

## Core Types

The endocrine system uses these core types (from `virtual-endocrine-system`):

```cpp
// 8-byte valence tag (Russell's circumplex)
struct ValenceSignature { float valence; float arousal; };

// 64-byte SIMD-aligned hormone snapshot
struct EndocrineState { std::array<float, 16> concentrations; };

// 10 emergent cognitive modes
enum class CognitiveMode : uint8_t {
    RESTING, EXPLORATORY, FOCUSED, STRESSED, SOCIAL,
    REFLECTIVE, VIGILANT, MAINTENANCE, REWARD, THREAT
};

// 16 hormone channels
enum class HormoneId : uint8_t {
    CRH, ACTH, CORTISOL, DOPAMINE_TONIC, DOPAMINE_PHASIC,
    SEROTONIN, NOREPINEPHRINE, OXYTOCIN, T3_T4, MELATONIN,
    INSULIN, GLUCAGON, IL6, ANANDAMIDE, RESERVED_1, RESERVED_2
};
```

## Wiring Pattern: DES

Connect endocrine system to DES process blocks:

```cpp
class EndoModulatedDelay {
    EndocrineSystem& endo_;
    float base_delay_;

public:
    float compute_delay() const {
        float cortisol = endo_.bus().concentration(HormoneId::CORTISOL);
        float dopamine = endo_.bus().concentration(HormoneId::DOPAMINE_TONIC);
        // Stress increases delay, motivation decreases it
        return base_delay_ * (1.0f + cortisol * 0.5f) * (1.0f - dopamine * 0.3f);
    }
};

class EndoModulatedRouter {
    EndocrineSystem& endo_;

public:
    int select_output() const {
        CognitiveMode mode = endo_.bus().current_mode();
        switch (mode) {
            case CognitiveMode::THREAT:    return 0;  // Emergency path
            case CognitiveMode::FOCUSED:   return 1;  // Optimized path
            case CognitiveMode::EXPLORATORY: return 2; // Discovery path
            default:                       return 1;  // Default path
        }
    }
};
```

## Wiring Pattern: ABM

Each agent owns an endocrine system:

```cpp
class CogSimAgent {
    EndocrineSystem endo_;
    // Statechart transitions gated by mode
    void on_tick(float dt) {
        endo_.tick(dt);
        CognitiveMode mode = endo_.bus().current_mode();
        update_behavior(mode);
    }

    void on_interact(CogSimAgent& other) {
        // Emotional contagion: oxytocin spreads
        float their_oxy = other.endo_.bus().concentration(HormoneId::OXYTOCIN);
        if (their_oxy > 0.4f) {
            endo_.signal_event(EndocrineEvent::SOCIAL_BOND_SIGNAL, their_oxy * 0.5f);
        }
        // Stress contagion: cortisol spreads
        float their_cort = other.endo_.bus().concentration(HormoneId::CORTISOL);
        if (their_cort > 0.6f) {
            endo_.signal_event(EndocrineEvent::THREAT_DETECTED, their_cort * 0.3f);
        }
    }
};
```

## Wiring Pattern: SD

Map hormone dynamics to stock-flow equations:

```
d(Cortisol)/dt = HPA_production_rate - decay_rate * Cortisol
    where HPA_production_rate = f(CRH, ACTH, stress_input)
    and   decay_rate = ln(2) / half_life_cortisol

d(Dopamine_tonic)/dt = baseline_production + reward_signal - decay_rate * Dopamine_tonic
    where baseline_production = homeostatic_setpoint * restoration_rate

d(Serotonin)/dt = sustained_positive_rate - decay_rate * Serotonin
    where sustained_positive_rate increases with prolonged positive valence
```

## Wiring Pattern: Multimethod

The hormone bus bridges paradigms:

```
┌──────────────┐     ┌──────────────┐     ┌──────────────┐
│   SD Layer   │     │  Hormone Bus │     │  DES Layer   │
│ (stocks/flows)│────▶│  (16 channels)│────▶│ (process flow)│
└──────────────┘     └──────┬───────┘     └──────────────┘
                            │
                     ┌──────▼───────┐
                     │  ABM Layer   │
                     │ (agent states)│
                     └──────────────┘
```

SD writes aggregate hormone levels → Bus broadcasts → DES reads for modulation → ABM agents read for behavior.

## Event Mapping

Map simulation events to endocrine events:

| Simulation Event | EndocrineEvent | Typical Intensity |
|-----------------|----------------|-------------------|
| Entity enters emergency queue | THREAT_DETECTED | 0.7 |
| Task completed successfully | GOAL_ACHIEVED | 0.8 |
| Agent meets another agent | SOCIAL_BOND_SIGNAL | 0.3-0.6 |
| Resource pool exhausted | RESOURCE_DEPLETED | 0.9 |
| New entity type encountered | NOVELTY_ENCOUNTERED | 0.5 |
| Queue overflow / error | ERROR_DETECTED | 0.6 |
| High noise in measurements | NOISE_EXCESSIVE | 0.4 |
| Conflict between agents | CONFLICT_DETECTED | 0.5 |

## Mode-Gated Routing

Use cognitive mode to gate simulation decisions:

```cpp
// In SelectOutput block
int route(CognitiveMode mode) {
    // Polynomial pattern: mode selects among alternatives (⊕)
    switch (mode) {
        case CognitiveMode::THREAT:
            return EMERGENCY_ROUTE;      // Fast, resource-intensive
        case CognitiveMode::FOCUSED:
            return OPTIMIZED_ROUTE;      // Efficient, narrow
        case CognitiveMode::EXPLORATORY:
            return DISCOVERY_ROUTE;      // Broad, experimental
        case CognitiveMode::STRESSED:
            return DEGRADED_ROUTE;       // Error-prone, slow
        case CognitiveMode::SOCIAL:
            return COOPERATIVE_ROUTE;    // Shared resources
        default:
            return DEFAULT_ROUTE;
    }
}
```

This is a **polynomial** (⊕-dominant) pattern: the mode selects among independent route alternatives.
