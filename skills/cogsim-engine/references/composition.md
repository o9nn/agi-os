# Algebraic Composition Patterns

How to compose CogSim models using ⊕ (additive) and ⊗ (multiplicative) operators.

## Table of Contents

1. [The CogSim Semiring](#the-cogsim-semiring)
2. [Model Composition Patterns](#model-composition-patterns)
3. [Paradigm Composition](#paradigm-composition)
4. [Endocrine Composition](#endocrine-composition)
5. [Skill Composition](#skill-composition)
6. [Higher-Order Patterns](#higher-order-patterns)

---

## The CogSim Semiring

```
(CogSim, ⊕, ⊗, ∅, Id)

⊕ : Alternative / parallel / independent models
⊗ : Composed / sequential / interacting models
∅ : Empty model (no entities, no dynamics)
Id: Identity model (pass-through, no transformation)
```

**Eight laws hold**:
1. `A ⊕ B ≅ B ⊕ A` — model alternatives are symmetric
2. `(A ⊕ B) ⊕ C ≅ A ⊕ (B ⊕ C)` — alternatives associate
3. `A ⊕ ∅ ≅ A` — adding empty model changes nothing
4. `A ⊗ B ≅ B ⊗ A` — composition commutes (when symmetric)
5. `(A ⊗ B) ⊗ C ≅ A ⊗ (B ⊗ C)` — composition associates
6. `A ⊗ Id ≅ A` — composing with identity changes nothing
7. `A ⊗ (B ⊕ C) ≅ (A⊗B) ⊕ (A⊗C)` — **distributivity**
8. `A ⊗ ∅ ≅ ∅` — composing with empty annihilates

## Model Composition Patterns

### Pattern 1: Additive (⊕) — Independent Alternatives

```
model = process_A ⊕ process_B ⊕ process_C
```

Entities choose one path. Subsystems run independently. Output is union of results.

**Use when**: Multiple independent process variants exist. Agent selects behavior based on type or mode.

### Pattern 2: Multiplicative (⊗) — Sequential Pipeline

```
model = intake ⊗ process ⊗ quality_check ⊗ output
```

Each stage feeds the next. All stages must execute. Output is the composed transformation.

**Use when**: Stages depend on each other. Endocrine state propagates through pipeline.

### Pattern 3: Polynomial of Tensors — Choose Among Interactions

```
model = (endo ⊗ des) ⊕ (endo ⊗ abm) ⊕ (endo ⊗ sd)
```

Choose one of several composed models. Each alternative is internally multiplicative.

**Use when**: Different simulation approaches are valid. Select based on problem characteristics.

### Pattern 4: Tensor of Polynomials — Interact Among Choices

```
model = (process_A ⊕ process_B) ⊗ (resource_X ⊕ resource_Y) ⊗ endo
```

All choices interact. Combinatorial: each combination of alternatives is explored.

**Use when**: Multiple dimensions of variation must be jointly considered.

### Pattern 5: Factoring via Distributivity

```
# Expanded (⊕-dominant):
model = (endo ⊗ des_fast) ⊕ (endo ⊗ des_slow) ⊕ (endo ⊗ des_normal)

# Factored (⊗-dominant):
model = endo ⊗ (des_fast ⊕ des_slow ⊕ des_normal)
```

Factoring extracts shared components. Use to avoid redundant endocrine system instantiation.

## Paradigm Composition

| Expression | Meaning |
|-----------|---------|
| `DES ⊕ ABM` | Choose DES or ABM based on problem type |
| `DES ⊗ ABM` | Agents flow through DES process (multimethod) |
| `SD ⊗ DES` | SD sets parameters, DES executes process |
| `SD ⊗ ABM` | SD drives aggregate dynamics, ABM models individuals |
| `(SD ⊗ ABM) ⊗ DES` | Full multimethod: SD → ABM → DES pipeline |
| `Endo ⊗ (DES ⊕ ABM ⊕ SD)` | Endocrine layer applied to any paradigm |

## Endocrine Composition

### Single Endocrine System (⊗)

```
model = EndocrineSystem ⊗ SimulationModel
```

One shared hormone bus modulates the entire model. Global cognitive mode affects all components.

### Per-Agent Endocrine (⊕ of ⊗)

```
model = ⊕ᵢ (Endoᵢ ⊗ Agentᵢ)
```

Each agent has its own endocrine system. Polynomial: agents are independent alternatives. Within each agent, endocrine and behavior are multiplicatively composed.

### Hierarchical Endocrine (⊗ of ⊗)

```
model = GlobalEndo ⊗ (⊕ᵢ (LocalEndoᵢ ⊗ Agentᵢ))
```

Global endocrine sets macro-level mood. Local endocrine systems respond to individual events. Global modulates local via hormone bus broadcast.

### Hormone Channel Composition

Individual hormone channels compose:

```
HPA = CRH ⊗ ACTH ⊗ Cortisol       # Multiplicative cascade
Mood = Serotonin ⊕ Dopamine(tonic)  # Additive alternatives
Arousal = Norepinephrine ⊗ Cortisol  # Multiplicative interaction
```

## Skill Composition

| Expression | Result |
|-----------|--------|
| `cogsim-engine ⊗ nn` | Neural network layers as simulation components |
| `cogsim-engine ⊗ time-crystal-nn` | Temporal hierarchy drives circadian gland |
| `cogsim-engine ⊗ unreal-echo` | Cognitive avatar with simulated endocrine |
| `cogsim-engine ⊕ cogsim-pml` | Choose CogSim or standard PML |
| `cogsim-engine ⊗ promise-lambda-attention` | Promises constrain simulation space |
| `cogsim-engine ⊗ topology-weaver` | Map sim architecture to neural topology |
| `function-creator ⊗ cogsim-engine` | Transform CogSim to new domain |
| `cogsim-engine ⊗ virtual-endocrine-system` | Direct VES integration (identity) |
| `cogsim-engine ⊗ circled-operators` | Explicit algebraic reasoning layer |
| `cogsim-engine ⊗ anylogic-modeler` | AnyLogic-specific implementation |

## Higher-Order Patterns

### The Simulation Semiring of Semirings

Semirings of simulation models themselves form a semiring:

```
⊕ : Take the direct sum of two simulation semirings
⊗ : Take the tensor product of two simulation semirings
0 : The trivial simulation (no entities, no time)
1 : The identity simulation (entities pass through unchanged)
```

### Fixed Points

The free monoid of CogSim models:

```
CogSim* = Id ⊕ M ⊕ (M⊗M) ⊕ (M⊗M⊗M) ⊕ ...
```

This represents all possible pipeline lengths — from identity (do nothing) through arbitrarily deep compositions.

### Polynomial Hierarchy of Models

```
Level 0: Constants (fixed parameter models)
Level 1: CogSim[x] — models parameterized by one variable
Level 2: CogSim[x][y] — models parameterized by two variables
Level n: Iterated parameterization
```
