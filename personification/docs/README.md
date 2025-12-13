# Moeru AI Monorepo - Technical Documentation

This directory contains comprehensive technical architecture documentation and formal specifications for the Moeru AI ecosystem.

## 📚 Documentation Structure

### 1. Architecture Overview
**File**: `architecture_overview.md`

A comprehensive technical architecture document with detailed Mermaid diagrams covering:

- **System Architecture**: High-level component diagrams showing all major systems and their interactions
- **Technology Stack**: Distribution of TypeScript/Node.js, Rust, and Go projects
- **Core Projects Deep Dive**: Detailed architecture for AIRI, xsAI, ortts, and inventory
- **Data Flow Diagrams**: Visual representations of key workflows (chat flow, model download, events)
- **Integration Boundaries**: External service connections and API integrations
- **Deployment Architecture**: Multi-tier deployment model
- **Technology Rationale**: Why specific technologies were chosen for each component
- **Security & Performance**: Considerations for scalability, security, and optimization
- **Development Workflow**: Monorepo structure and development practices

**Use Cases**:
- Understanding the overall system architecture
- Onboarding new developers
- Planning new features and integrations
- System design reviews
- DevOps and deployment planning

### 2. Formal Specifications (Z++)

The following files contain rigorous formal specifications using Z++ notation:

#### `data_model.zpp`
Formal specification of all data structures and entities:

- **Basic Types**: Primitives like UUID, TIMESTAMP, URL, JSON
- **Enumerations**: ProviderType, ModelType, ServiceStatus, MessageRole, etc.
- **Core Entities**: 
  - Session management (UserSession, WebSocketConnection)
  - AI models (ModelConfig, ModelMetadata, CatalogEntry)
  - Chat system (ChatMessage, ToolCall, ConversationContext)
  - TTS (TTSRequest, TTSResponse)
  - Service states (Discord, Telegram, Twitter, Minecraft, Factorio)
  - Event system (Event, EventHandler, EventQueue)
- **Data Relationships**: Formal constraints ensuring data consistency

**Use Cases**:
- Understanding data structures and their relationships
- Validating data model integrity
- Generating database schemas
- API contract definition
- Type system validation

#### `system_state.zpp`
Complete system state specifications with invariants:

- **AIRI Server State**: Session, connection, and module management
- **Integration Services**: State for all platform integrations
- **xsAI SDK State**: Provider management and request tracking
- **Service States**: ortts, inventory, unspeech, demodel, MCP launcher
- **Event System State**: Event processing and handler registry
- **Global System State**: Complete state with cross-service invariants
- **Initialization**: Initial state conditions

**Use Cases**:
- Understanding system state management
- Identifying state invariants for testing
- Debugging state-related issues
- Validating system consistency
- Designing state persistence strategies

#### `operations.zpp`
Formal specifications of all system operations:

- **Authentication & Connection**: Session and WebSocket operations
- **Conversation Management**: Starting conversations and adding messages
- **AI Operations**: Text generation, streaming, embeddings, tool calling
- **TTS Operations**: Request submission, processing, and aggregation
- **Model Catalog**: Listing, querying, and syncing models
- **Model Download**: Queue, start, progress, and completion operations
- **MCP Operations**: Server launching and stopping
- **Event Operations**: Publishing, processing, and handler registration
- **Composite Flows**: End-to-end workflows (chat flow, model download flow)

**Use Cases**:
- Understanding operation preconditions and postconditions
- Generating test cases from specifications
- API documentation generation
- Validating operation correctness
- Identifying edge cases and error conditions

#### `integrations.zpp`
External service integration contracts:

- **LLM Providers**: OpenAI, Anthropic, DeepSeek, Groq
- **Local Models**: Ollama, Transformers.js
- **Model Repositories**: HuggingFace Hub, Ollama Registry
- **Social Platforms**: Discord, Telegram, Twitter/X
- **Gaming Platforms**: Minecraft RCON, Factorio RCON
- **TTS Services**: ElevenLabs, Azure Speech
- **Authentication**: OAuth2, API Key
- **Error Handling**: Retry strategies, circuit breakers
- **Load Balancing**: Multi-provider fallback, connection pooling

**Use Cases**:
- Understanding external API contracts
- Implementing new provider integrations
- API client development
- Error handling strategy
- Rate limiting and retry logic
- Integration testing

## 🎯 How to Use This Documentation

### For New Developers
1. Start with `architecture_overview.md` to understand the big picture
2. Review `data_model.zpp` to learn about core data structures
3. Study `system_state.zpp` to understand how state is managed
4. Explore `operations.zpp` for specific functionality you'll work on

### For System Architects
1. Use `architecture_overview.md` for system design discussions
2. Reference formal specifications for architectural decisions
3. Validate new designs against existing invariants
4. Use Mermaid diagrams in presentations and documentation

### For API Developers
1. Study relevant sections in `integrations.zpp` for API contracts
2. Use `operations.zpp` to understand operation requirements
3. Reference `data_model.zpp` for request/response structures
4. Validate implementations against formal specifications

### For QA/Testing
1. Use `operations.zpp` to derive test cases
2. Reference preconditions and postconditions for test scenarios
3. Validate invariants in `system_state.zpp` during testing
4. Use integration contracts for external service mocking

### For DevOps
1. Use deployment architecture from `architecture_overview.md`
2. Understand service dependencies and communication patterns
3. Reference port configurations and service endpoints
4. Plan monitoring based on system state specifications

## 📖 Z++ Notation Guide

### Basic Constructs

```z++
(* Schema Definition *)
schema EntityName
  field1: Type1
  field2: Type2
  
  (* Invariants *)
  field1 ≠ ""
  field2 > 0
end EntityName
```

### Common Symbols

- `ℕ` - Natural numbers (non-negative integers)
- `ℤ` - Integers
- `ℝ` - Real numbers
- `BOOLEAN` - Boolean values (true/false)
- `seq T` - Sequence of type T
- `⇸` - Partial function
- `∈` - Element of (membership)
- `∉` - Not element of
- `⊆` - Subset of
- `∪` - Union
- `∩` - Intersection
- `∅` - Empty set
- `∀` - For all (universal quantifier)
- `∃` - There exists (existential quantifier)
- `⇒` - Implies
- `⇔` - If and only if
- `∧` - And
- `∨` - Or
- `¬` - Not

### Operation Schemas

```z++
(* State-changing operation *)
schema OperationName
  ΔSystemState  (* State changes *)
  input?: Type  (* Input parameter *)
  output!: Type (* Output parameter *)
  
  (* Preconditions *)
  pre input? ≠ ""
  
  (* Postconditions *)
  post output! = result
end OperationName

(* Read-only operation *)
schema QueryOperation
  ΞSystemState  (* State unchanged *)
  input?: Type
  output!: Type
end QueryOperation
```

## 🔍 Verification & Validation

### Invariant Checking
The formal specifications include invariants that should be validated:

1. **Data Model Invariants**: Constraints on individual entities
2. **System State Invariants**: Cross-entity consistency rules
3. **Operation Preconditions**: Requirements before operations execute
4. **Operation Postconditions**: Guaranteed results after operations

### Automated Verification (Future)
These specifications can be used with formal verification tools:

- **Type Checking**: Validate type consistency
- **Invariant Checking**: Ensure invariants hold in all states
- **Model Checking**: Verify temporal properties
- **Proof Obligations**: Generate and prove correctness conditions

## 🔄 Maintenance

### Updating Documentation

When making architectural changes:

1. **Update Architecture Overview**: Modify Mermaid diagrams and descriptions
2. **Update Data Model**: Add/modify schemas in `data_model.zpp`
3. **Update System State**: Reflect state changes in `system_state.zpp`
4. **Update Operations**: Add new operations or modify existing ones
5. **Update Integrations**: Add new external service contracts

### Validation Process

Before finalizing changes:

1. Verify all invariants are consistent
2. Ensure operations respect state transitions
3. Check that integration contracts match actual APIs
4. Validate that diagrams reflect implementation
5. Review with team for accuracy

## 📊 Metrics & Statistics

### Documentation Coverage

- **4 main specification files**: data_model, system_state, operations, integrations
- **65+ formal schemas**: Covering all major system components
- **18 Mermaid diagrams**: Visual architecture representation
- **30+ operations**: With formal pre/post-conditions
- **20+ integration contracts**: For external services

### Lines of Specification

- `architecture_overview.md`: ~700 lines, 17.6 KB
- `data_model.zpp`: ~450 lines, 12 KB
- `system_state.zpp`: ~600 lines, 17 KB
- `operations.zpp`: ~700 lines, 20 KB
- `integrations.zpp`: ~600 lines, 16.6 KB
- **Total**: ~3,050 lines of documentation and formal specifications

## 🤝 Contributing

When contributing to this documentation:

1. Follow the existing Z++ notation style
2. Include invariants for all new schemas
3. Document preconditions and postconditions for operations
4. Update Mermaid diagrams when architecture changes
5. Maintain consistency across all specification files
6. Add comments explaining complex invariants

## 📝 License

This documentation follows the same license as the Moeru AI monorepo. See individual project LICENSE files for details.

## 🔗 Related Resources

- [Moeru AI Monorepo README](../README.md)
- [AIRI Project Documentation](../airi/docs/)
- [xsAI SDK Documentation](../xsai/docs/)
- [Individual Project READMEs](../)

## ❓ Questions & Support

For questions about this documentation:

- Open an issue in the repository
- Join the Discord server (see main README)
- Refer to individual project documentation for project-specific questions

---

**Last Updated**: December 2025  
**Maintained By**: Moeru AI Team
