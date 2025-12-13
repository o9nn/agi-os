# AGI-OS Formal Specification

**Version:** 1.0  
**Date:** December 12, 2025  
**Authors:** AGI-OS Development Team

## Overview

This directory contains the complete formal specification of the AGI-OS (Autonomous General Intelligence Operating System) architecture using Z++ notation. The specification provides a rigorous, mathematically precise definition of the system's data structures, state, operations, and external integration contracts.

## Purpose

The formal specification serves multiple purposes:

1. **Precise Documentation:** Unambiguous definition of system behavior
2. **Design Verification:** Mathematical proof that operations maintain invariants
3. **Implementation Guide:** Reference specification for developers
4. **Contract Definition:** Clear interfaces for external integrations
5. **Safety Analysis:** Formal reasoning about system properties

## Specification Files

### 1. `data_model.zpp` - Data Layer Formalization

**Purpose:** Defines the core data structures and types used throughout AGI-OS.

**Key Components:**
- **Base Types:** AtomID, TypeID, Probability, Confidence, Timestamp
- **Truth Values:** SimpleTruthValue, CountTruthValue, IndefiniteTruthValue
- **Attention Values:** STI, LTI, VLTI for economic attention networks
- **Atom Type System:** Hierarchical type system for nodes and links
- **Atoms:** Core knowledge representation (Node, Link)
- **Values:** Typed data attached to atoms (FloatValue, StringValue, TensorValue)
- **AtomSpace:** Hypergraph database with indices
- **Attentional Focus:** Working memory with STI-based selection
- **Pattern Matching:** Query language and solutions
- **Task Scheduling:** Priority-based cognitive task queue
- **Event System:** Priority-based event bus

**Relationship to Code:**
- Maps directly to C++ classes in `core/cognition/foundation/atomspace/`
- Corresponds to Python bindings in AtomSpace API
- Reflects Scheme interface defined in `.scm` files

### 2. `system_state.zpp` - System State Schemas

**Purpose:** Defines the complete state of AGI-OS across all architectural layers.

**Key Components:**

#### Layer 1: Microkernel (CogNumach)
- **ProcessState:** Process management with cognitive STI
- **MemoryRegion:** Memory management with permissions
- **IPCPort:** Inter-process communication ports
- **CogNumachState:** Complete microkernel state

#### Layer 2: Operating System (HurdCog)
- **Translator:** User-space servers (filesystem, network)
- **FileDescriptor:** File access tracking
- **FileSystemEntry:** File metadata and permissions
- **CognitiveCache:** Semantic caching with STI-based eviction
- **HurdCogState:** Complete OS state

#### Layer 3: Cognitive Framework (OpenCog)
- **AtomSpaceState:** Knowledge base with attention and queries
- **CogServerState:** Network server state
- **PLNState:** Probabilistic logic reasoning
- **UREState:** Unified rule engine
- **MOSESState:** Program synthesis/evolution
- **PatternMinerState:** Pattern discovery
- **ECANState:** Economic attention networks

#### Layer 4: Development Tools (CogBolt)
- **CogBoltState:** AI-powered IDE state

#### System-Wide State
- **AGIOSSystemState:** Complete integrated system state
- **InitialState:** Post-boot state
- **SteadyStateInvariant:** Invariants that always hold
- **DegradedState:** Graceful degradation
- **ShutdownState:** Safe shutdown state

**Relationship to Code:**
- Reflects `core/agi_boot.py` boot sequence
- Maps to `core/agi_scheduler.py` task management
- Corresponds to `core/integration/cognitive-grip/` integration layer

### 3. `operations.zpp` - Operation Specifications

**Purpose:** Defines all operations that transform system state while maintaining invariants.

**Key Operations:**

#### AtomSpace Operations
- **AddNode:** Create new node in knowledge base
- **AddLink:** Create new link between atoms
- **RemoveAtom:** Delete atom (with cascade handling)
- **SetTruthValue:** Update probabilistic truth value
- **SetAttentionValue:** Update attention (STI/LTI)

#### Query Operations
- **ExecuteQuery:** Pattern matching with variables
- **GetIncomingSet:** Find links pointing to atom
- **GetAtomsByType:** Type-based retrieval

#### Attention Operations (ECAN)
- **SpreadAttention:** Propagate STI to neighbors
- **CollectRent:** Decay STI values over time
- **UpdateAttentionalFocus:** Rebuild working memory

#### Reasoning Operations (PLN/URE)
- **ApplyInferenceRule:** Single inference step
- **ForwardChain:** Forward chaining inference
- **BackwardChain:** Goal-directed proof search

#### Learning Operations (MOSES)
- **EvolvePrograms:** Genetic programming evolution
- **MinePatterns:** Frequent pattern discovery

#### Persistence Operations
- **StoreAtoms:** Write to storage backend
- **LoadAtoms:** Read from storage backend

#### Network Operations
- **HandleClientRequest:** Process CogServer request
- **AcceptConnection:** New client connection
- **CloseConnection:** Disconnect client

#### System Operations
- **BootSystem:** Initialize from power-on
- **ShutdownSystem:** Graceful shutdown
- **CognitiveLoop:** Main processing loop

**Relationship to Code:**
- Specifies operations in `core/cognition/foundation/atomspace/opencog/atomspace/`
- Defines behavior of `core/cognition/network/cogserver/`
- Formalizes `core/agi_scheduler.py` and `core/agi_event_bus.py`

### 4. `integrations.zpp` - External Integration Contracts

**Purpose:** Defines contracts for external system integration and APIs.

**Key Integration Points:**

#### LLM Integration
- **LLMModel:** External language model configuration
- **LLMPrompt:** Prompt construction with AtomSpace context
- **LLMResponse:** Structured response parsing
- **InvokeLLM:** Call external LLM service
- **ParseLLMOutput:** Convert LLM output to atoms

#### Storage Backends
- **RocksDBBackend:** Key-value store integration
- **PostgreSQLBackend:** Relational database persistence
- **IPFSBackend:** Content-addressed distributed storage

#### Network Protocols
- **MCP (Model Context Protocol):** Anthropic's standard
- **RESTful HTTP API:** Web service interface
- **MachIPC:** Microkernel message passing

#### Translator Interface
- **TranslatorOpen:** Hurd filesystem access
- **TranslatorRead:** Read via translator
- **TranslatorWrite:** Write via translator

#### Cognitive Cache
- **UpdateCognitiveCache:** STI-based cache update
- **EvictFromCognitiveCache:** Remove low-STI entries

#### IDE Integration
- **LSP (Language Server Protocol):** Code completion, hover
- **HandleLSPCompletion:** AI-powered completions

#### Event-Driven
- **WebhookEvent:** External event reception
- **ProcessWebhook:** Verified webhook handling

**Relationship to Code:**
- Defines CogServer API in `core/cognition/network/cogserver/`
- Specifies storage backends in `core/cognition/foundation/atomspace-storage/`
- Documents MCP implementation in `examples/mcp/`

## Z++ Notation Guide

### Basic Syntax

```z++
(* Comments use Pascal-style notation *)

(* Type definitions *)
TypeName == BaseType
TypeName == { x: Type | constraint }

(* Schemas *)
schema SchemaName
  field1: Type1
  field2: Type2
where
  (* Invariants and constraints *)
  field1 > 0
  field2 ∈ SomeSet
end

(* Operations *)
schema OperationName
  ΔStateName          (* State changes *)
  ΞStateName          (* State unchanged - read-only *)
  input_param: Type
  output_param!: Type (* ! indicates output *)
where
  (* Pre-conditions *)
  input_param > 0
  
  (* Post-conditions *)
  output_param! = compute(input_param)
  
  (* State updates *)
  field' = new_value  (* ' indicates "after" state *)
end
```

### Key Symbols

| Symbol | Meaning | Example |
|--------|---------|---------|
| `ℕ` | Natural numbers (0, 1, 2, ...) | `count: ℕ` |
| `ℕ₁` | Positive naturals (1, 2, 3, ...) | `size: ℕ₁` |
| `ℤ` | Integers (..., -1, 0, 1, ...) | `sti: ℤ` |
| `ℝ` | Real numbers | `probability: ℝ` |
| `ℝ⁺` | Positive reals | `rate: ℝ⁺` |
| `𝔹` | Boolean (true, false) | `flag: 𝔹` |
| `ℙ` | Power set (set of all subsets) | `atoms: ℙ AtomID` |
| `↦` | Maplet (map from ... to) | `id ↦ atom` |
| `→` | Total function | `f: A → B` |
| `↛` | Partial function | `f: A ↛ B` |
| `⊕` | Function override | `map ⊕ {key ↦ value}` |
| `⩤` | Domain subtraction | `{1, 2} ⩤ map` |
| `seq` | Sequence | `seq ℕ` |
| `⟨⟩` | Empty sequence | `empty = ⟨⟩` |
| `::=` | Free type definition | `Type ::= A \| B` |
| `∈` | Element of | `x ∈ S` |
| `∉` | Not element of | `x ∉ S` |
| `⊆` | Subset | `A ⊆ B` |
| `∪` | Union | `A ∪ B` |
| `∩` | Intersection | `A ∩ B` |
| `\` | Set difference | `A \ B` |
| `∅` | Empty set | `S = ∅` |
| `∀` | For all | `∀ x: S • P(x)` |
| `∃` | There exists | `∃ x: S • P(x)` |
| `∧` | Logical AND | `P ∧ Q` |
| `∨` | Logical OR | `P ∨ Q` |
| `¬` | Logical NOT | `¬P` |
| `⟹` | Implies | `P ⟹ Q` |
| `#` | Cardinality/size | `#set` |
| `dom` | Domain of function | `dom f` |
| `ran` | Range of function | `ran f` |
| `Δ` | State change | `ΔState` |
| `Ξ` | State unchanged | `ΞState` |
| `'` | After state | `field'` |

## Reading the Specification

### Understanding Schemas

A **schema** defines a state or operation with:
1. **Declarations:** Field names and types
2. **Predicates:** Invariants that must hold (in `where` clause)

Example:
```z++
schema Node
  extends Atom
  name: AtomName
where
  atom_type.type_class = NodeType
  name ≠ ""
end
```

This says: "A Node is an Atom with a name field. The atom's type class must be NodeType, and the name cannot be empty."

### Understanding Operations

An **operation schema** specifies:
1. **State changes:** `ΔState` (modifies) or `ΞState` (read-only)
2. **Inputs:** Regular parameters
3. **Outputs:** Parameters with `!` suffix
4. **Pre-conditions:** What must be true before the operation
5. **Post-conditions:** What will be true after the operation

Example:
```z++
schema AddNode
  ΔAtomSpaceState
  node_type: TypeID
  node_name: AtomName
  result!: AtomID
where
  (* Pre-condition *)
  ¬read_only
  
  (* Post-condition *)
  result! ∈ dom atoms'
  atoms'(result!).name = node_name
end
```

This says: "AddNode modifies the AtomSpace. It takes a type and name as input. It requires the space not be read-only. After execution, the result will be in the atoms map with the specified name."

### State Change Notation

- `field` - Value before operation
- `field'` - Value after operation
- `Δ` prefix - Operation modifies this state
- `Ξ` prefix - Operation does not modify this state

Example:
```z++
atom_count' = atom_count + 1
```
"The atom count after the operation equals the count before plus one."

## Verification Properties

The formal specification enables verification of key properties:

### 1. Safety Properties

**Referential Integrity:**
```z++
∀ aid: dom atomspace.atoms | atomspace.atoms(aid) ∈ Link •
  ∀ out_id: atomspace.atoms(aid).outgoing •
    out_id ∈ dom atomspace.atoms
```
"Every atom referenced by a link must exist in the atomspace."

**No Cycles:**
```z++
∀ aid: dom atomspace.atoms | atomspace.atoms(aid) ∈ Link •
  aid ∉ atomspace.atoms(aid).outgoing
```
"An atom cannot directly link to itself."

**Attention Conservation:**
```z++
(∑ aid: dom atomspace.atoms • 
  atomspace.atoms(aid).attention_value.sti.sti) ≤ ecan.sti_funds_total
```
"Total STI never exceeds the system's STI budget."

### 2. Liveness Properties

**Progress:**
```z++
∀ task: task_queue.tasks • 
  eventually(task.state = Completed ∨ task.state = Cancelled)
```
"Every task eventually completes or is cancelled."

**Event Processing:**
```z++
#event_queue.events > 0 ⟹ eventually(#event_queue.events' < #event_queue.events)
```
"If there are events in the queue, eventually the queue size decreases."

### 3. Consistency Properties

**Type Consistency:**
```z++
∀ aid: dom atomspace.atoms •
  atomspace.atoms(aid).atom_type.type_id ∈ dom type_index ∧
  aid ∈ type_index(atomspace.atoms(aid).atom_type.type_id)
```
"Type index is consistent with atom types."

**Index Consistency:**
```z++
∀ target: AtomID; link_set: ℙ AtomID |
  target ∈ dom incoming_index ∧ incoming_index(target) = link_set •
  ∀ link_id: link_set •
    target ∈ atomspace.atoms(link_id).outgoing
```
"Incoming index correctly tracks which links point to each atom."

## Implementation Guidelines

### For Developers

1. **Read data_model.zpp first:** Understand core types and structures
2. **Review system_state.zpp:** See how your component fits into overall state
3. **Study relevant operations.zpp:** Understand operation contracts
4. **Check integrations.zpp:** Learn external API contracts

### Implementing Operations

When implementing an operation specified here:

1. **Check pre-conditions:** Validate inputs match pre-conditions
2. **Maintain invariants:** Ensure all invariants hold after operation
3. **Update indices:** Keep all indices consistent
4. **Handle errors:** Return appropriate error codes when pre-conditions fail
5. **Test post-conditions:** Verify outputs match post-conditions

### Example: Implementing AddNode

```cpp
// From operations.zpp: AddNode specification
Handle AtomSpace::add_node(Type node_type, const std::string& node_name) {
    // Pre-condition: not read-only
    if (read_only) {
        throw RuntimeException("AtomSpace is read-only");
    }
    
    // Pre-condition: type must be valid
    if (!is_valid_type(node_type)) {
        throw InvalidTypeException(node_type);
    }
    
    // Check if node already exists (from spec)
    Handle existing = get_node(node_type, node_name);
    if (existing != Handle::UNDEFINED) {
        return existing;  // Return existing node
    }
    
    // Create new node (follows spec)
    UUID new_uuid = generate_uuid();
    Handle new_handle = Handle(new_uuid);
    
    Node* node = new Node(node_type, node_name);
    node->set_truth_value(default_tv);
    node->set_attention_value(default_av);
    
    // Update state (as per spec)
    atoms[new_handle] = node;
    name_index[{node_type, node_name}] = new_handle;
    type_index[node_type].insert(new_handle);
    atom_count++;
    
    // Post-condition: result is in atoms map
    assert(atoms.find(new_handle) != atoms.end());
    
    return new_handle;
}
```

## Relationship to Actual Codebase

### Core Mapping

| Specification | Implementation |
|---------------|----------------|
| `AtomSpace` schema | `core/cognition/foundation/atomspace/opencog/atomspace/AtomSpace.h` |
| `Node`, `Link` | `opencog/atoms/base/Node.h`, `Link.h` |
| `TruthValue` | `opencog/truthvalue/TruthValue.h` |
| `AttentionValue` | `opencog/attentionbank/AttentionValue.h` |
| `CogServer` | `core/cognition/network/cogserver/` |
| `PLN` operations | `core/cognition/reasoning/pln/` |
| `MOSES` operations | `core/cognition/learning/moses/` |
| `ECAN` operations | `core/cognition/attention/ecan/` |

### Python Bindings

The Python API reflects the formal specification:

```python
# Corresponds to AddNode operation
atomspace.add_node(types.ConceptNode, "example")

# Corresponds to ExecuteQuery operation
results = atomspace.execute_query(pattern)

# Corresponds to SetAttentionValue operation
atom.sti = 100
```

### Scheme Interface

The Scheme API also follows the specification:

```scheme
;; Corresponds to AddNode operation
(cog-new-node 'ConceptNode "example")

;; Corresponds to SetTruthValue operation
(cog-set-tv! atom (stv 0.8 0.9))

;; Corresponds to ExecuteQuery operation
(cog-execute! (Bind pattern))
```

## Future Extensions

### Planned Additions

1. **Distributed AtomSpace:** Formalize MachSpace protocol for distributed knowledge
2. **Neural-Symbolic Integration:** Specify hybrid neural network + symbolic reasoning
3. **Self-Modification:** Formalize safe self-modifying system operations
4. **Meta-Learning:** Specify learning to learn operations

### Contributing

To extend the formal specification:

1. Follow existing Z++ notation conventions
2. Define clear pre-conditions and post-conditions
3. Maintain consistency with existing schemas
4. Verify new operations preserve invariants
5. Add examples and explanations

## References

### Z++ and Formal Methods

- **Z Notation:** ISO/IEC 13568:2002 standard
- **Z++:** Object-oriented extension of Z
- **Formal Methods:** Rigorous mathematical specification techniques

### AGI-OS Documentation

- `../../architecture_overview.md` - High-level architecture with Mermaid diagrams
- `../../README-AGI-OS.md` - Project overview and quick start
- `../../AGI-OS-INTEGRATION.md` - Integration architecture details

### OpenCog Resources

- **OpenCog Wiki:** https://wiki.opencog.org/
- **AtomSpace Documentation:** https://wiki.opencog.org/w/AtomSpace
- **Pattern Matcher:** https://wiki.opencog.org/w/Pattern_Matcher
- **PLN Book:** "Probabilistic Logic Networks" by Goertzel et al.

## Validation and Testing

### Specification Consistency

The specification has been validated for:

1. **Syntactic Correctness:** All schemas are well-formed Z++
2. **Type Consistency:** All type references are defined
3. **Operation Completeness:** All state transitions are covered
4. **Invariant Preservation:** Operations maintain invariants

### Test Generation

The formal specification can be used to generate test cases:

```python
# Generate test for AddNode operation
def test_add_node_creates_new_node():
    """Test AddNode post-condition: result in atoms map"""
    atomspace = AtomSpace()
    
    # Pre-condition: atomspace not read-only
    assert not atomspace.read_only
    
    # Execute operation
    result = atomspace.add_node(types.ConceptNode, "test")
    
    # Post-condition: result is valid handle
    assert result is not None
    assert result in atomspace
    
    # Post-condition: atom has correct name
    assert atomspace[result].name == "test"
```

## Conclusion

This formal specification provides a mathematically rigorous foundation for AGI-OS development. It serves as:

- **Design document** for architecture decisions
- **Implementation guide** for developers
- **Verification tool** for correctness
- **Communication medium** for precise technical discussion

By maintaining consistency between specification and implementation, we ensure the system behaves correctly and predictably across all layers of the cognitive architecture.

---

**Version:** 1.0  
**Last Updated:** December 12, 2025  
**Maintained By:** AGI-OS Development Team  
**License:** Documentation under CC-BY-SA-4.0
