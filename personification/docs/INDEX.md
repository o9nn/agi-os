# Moeru AI Monorepo - Documentation Index

## 📖 Navigation Guide

This is the central index for all Moeru AI technical documentation and formal specifications.

## 🚀 Quick Start

### New to the Project?
Start here: **[SUMMARY.md](SUMMARY.md)** → **[architecture_overview.md](architecture_overview.md)**

### Need API Contracts?
Go to: **[integrations.zpp](integrations.zpp)**

### Looking for Data Structures?
See: **[data_model.zpp](data_model.zpp)**

### Want to Understand Operations?
Check: **[operations.zpp](operations.zpp)**

## 📚 Complete Documentation Map

### 🌟 Start Here
- **[SUMMARY.md](SUMMARY.md)** - Quick reference guide with tables and metrics
  - System architecture at a glance
  - Technology stack breakdown
  - Key data flows
  - Common operations examples
  - Learning paths

### 📐 Architecture Documentation
- **[architecture_overview.md](architecture_overview.md)** - Comprehensive technical architecture
  - 18 Mermaid diagrams showing system components
  - Deep dives into AIRI, xsAI, ortts, inventory
  - Data flow diagrams
  - Deployment architecture
  - Technology rationale
  - Security and scalability
  
### 🔷 Formal Specifications (Z++)

#### Core Specifications
1. **[data_model.zpp](data_model.zpp)** - Data structures and entities
   - 40+ formal schemas
   - Type definitions and enumerations
   - Entity relationships
   - Data constraints and invariants
   
2. **[system_state.zpp](system_state.zpp)** - Complete system state
   - Service states (AIRI, xsAI, ortts, inventory, etc.)
   - Global system state
   - State invariants
   - Initialization conditions
   
3. **[operations.zpp](operations.zpp)** - System operations
   - 30+ operations with contracts
   - Pre/post-conditions
   - State transitions
   - Composite workflows
   
4. **[integrations.zpp](integrations.zpp)** - External service contracts
   - 20+ integration contracts
   - LLM providers (OpenAI, Anthropic, DeepSeek, Groq)
   - Local models (Ollama, Transformers.js)
   - Social platforms (Discord, Telegram, Twitter)
   - Gaming (Minecraft, Factorio)
   - Error handling patterns

### 📖 Usage Guides
- **[README.md](README.md)** - Comprehensive documentation guide
  - How to use this documentation
  - Z++ notation guide
  - Use cases by role (developer, architect, QA, DevOps)
  - Verification strategies
  - Maintenance procedures

## 🎯 Find What You Need

### By Role

#### 👨‍💻 **Software Developer**
1. Start: [SUMMARY.md](SUMMARY.md) → [architecture_overview.md](architecture_overview.md)
2. Deep Dive: [data_model.zpp](data_model.zpp) → [operations.zpp](operations.zpp)
3. Reference: [integrations.zpp](integrations.zpp)

#### 🏗️ **System Architect**
1. Overview: [architecture_overview.md](architecture_overview.md)
2. Formal Model: [system_state.zpp](system_state.zpp)
3. Constraints: All `.zpp` files for invariants

#### 🔍 **QA Engineer**
1. Operations: [operations.zpp](operations.zpp) for test cases
2. States: [system_state.zpp](system_state.zpp) for invariant testing
3. Contracts: [integrations.zpp](integrations.zpp) for mocking

#### 🚀 **DevOps Engineer**
1. Deployment: [architecture_overview.md](architecture_overview.md) - Deployment section
2. Services: [system_state.zpp](system_state.zpp) for service configs
3. Monitoring: [SUMMARY.md](SUMMARY.md) for metrics

#### 📝 **Technical Writer**
1. Start: [README.md](README.md)
2. Content: All documentation files
3. Examples: [SUMMARY.md](SUMMARY.md) for code examples

### By Topic

#### 🤖 **AI & LLM**
- Architecture: [architecture_overview.md](architecture_overview.md#2-xsai---extra-small-ai-sdk)
- Data: [data_model.zpp](data_model.zpp) - AI Model Entities
- Operations: [operations.zpp](operations.zpp) - xsAI SDK Operations
- Integrations: [integrations.zpp](integrations.zpp) - LLM Provider Integrations

#### 🔊 **Text-to-Speech**
- Architecture: [architecture_overview.md](architecture_overview.md#3-ortts---onnx-runtime-tts-server)
- Data: [data_model.zpp](data_model.zpp) - TTS Entities
- State: [system_state.zpp](system_state.zpp) - ortts & unspeech
- Operations: [operations.zpp](operations.zpp) - TTS Operations
- Integrations: [integrations.zpp](integrations.zpp) - TTS Service Integrations

#### 💬 **Chat & Conversations**
- Data: [data_model.zpp](data_model.zpp) - Chat & Message Entities
- Operations: [operations.zpp](operations.zpp) - Conversation Operations
- Flow: [architecture_overview.md](architecture_overview.md#airi-text-to-speech-flow)

#### 🎮 **Gaming Integrations**
- Integrations: [integrations.zpp](integrations.zpp) - Gaming Platform Integrations
- State: [system_state.zpp](system_state.zpp) - Minecraft & Factorio State

#### 📦 **Model Management**
- Architecture: [architecture_overview.md](architecture_overview.md#4-inventory---model-catalog-service)
- Data: [data_model.zpp](data_model.zpp) - AI Model Entities
- State: [system_state.zpp](system_state.zpp) - Inventory & Demodel
- Operations: [operations.zpp](operations.zpp) - Model Catalog & Download
- Flow: [architecture_overview.md](architecture_overview.md#model-download--catalog-flow)

#### 🔌 **Service Integration**
- Architecture: [architecture_overview.md](architecture_overview.md#integration-boundaries)
- Data: [data_model.zpp](data_model.zpp) - Service & Integration Entities
- State: [system_state.zpp](system_state.zpp) - Integration Services State
- Contracts: [integrations.zpp](integrations.zpp) - All integration contracts

#### 📡 **Events & Messaging**
- Architecture: [architecture_overview.md](architecture_overview.md#event-driven-architecture-eventa)
- Data: [data_model.zpp](data_model.zpp) - Event System Entities
- State: [system_state.zpp](system_state.zpp) - Eventa State
- Operations: [operations.zpp](operations.zpp) - Event Operations

#### 🔐 **Authentication & Security**
- Architecture: [architecture_overview.md](architecture_overview.md#security-considerations)
- Data: [data_model.zpp](data_model.zpp) - UserSession
- Operations: [operations.zpp](operations.zpp) - Authentication Operations
- Integrations: [integrations.zpp](integrations.zpp) - Auth Contracts

## 📊 Documentation Statistics

| File | Lines | Size | Schemas/Sections |
|------|-------|------|------------------|
| architecture_overview.md | 674 | 18 KB | 18 diagrams |
| data_model.zpp | 484 | 12 KB | 40+ schemas |
| system_state.zpp | 592 | 18 KB | 15+ states |
| operations.zpp | 779 | 21 KB | 30+ operations |
| integrations.zpp | 673 | 17 KB | 20+ contracts |
| README.md | 299 | 11 KB | Usage guide |
| SUMMARY.md | 306 | 8.8 KB | Quick reference |
| **Total** | **3,807** | **105 KB** | **125+ items** |

## 🔍 Search Tips

### Finding Specific Schemas
```bash
# Search for a specific schema
grep -n "schema SchemaName" *.zpp

# Search for operations related to a topic
grep -n "schema.*Chat" operations.zpp

# Find all invariants
grep -n "Invariants\|invariant" *.zpp
```

### Finding Diagrams
```bash
# List all Mermaid diagrams
grep -n "```mermaid" architecture_overview.md
```

### Finding Integrations
```bash
# List all integration contracts
grep -n "schema.*Contract" integrations.zpp
```

## 🧭 Navigation Patterns

### Understanding a Component
1. **Architecture** → Read overview section
2. **Data Model** → Find relevant schemas
3. **State** → Understand runtime state
4. **Operations** → Learn how it works
5. **Integrations** → Check external dependencies

### Implementing a Feature
1. **Operations** → Find related operations
2. **Data Model** → Understand data structures
3. **Integrations** → Check external APIs
4. **State** → Verify state transitions
5. **Architecture** → Understand context

### Debugging an Issue
1. **State** → Check state invariants
2. **Operations** → Verify pre/post-conditions
3. **Integrations** → Check external contracts
4. **Data Model** → Validate data constraints

## 🔗 External Links

### Repository Links
- [Main Repository](../)
- [AIRI Project](../airi/)
- [xsAI SDK](../xsai/)
- [ortts Server](../ortts/)
- [Inventory Service](../inventory/)

### Project Resources
- [Moeru AI Organization](https://github.com/moeru-ai)
- [Discord Community](https://discord.gg/TgQ3Cu2F7A)
- [Website](https://moeru-ai.github.io)

## 📝 Quick Command Reference

### Reading Documentation
```bash
# View architecture overview
cat docs/architecture_overview.md | less

# View data model
cat docs/data_model.zpp | less

# Search for specific term
grep -r "YourSearchTerm" docs/

# Count schemas
grep -c "^schema " docs/*.zpp
```

### Generating Outputs
```bash
# Extract all schema names
grep "^schema " docs/*.zpp | cut -d: -f2 | sort

# List all operations
grep "^schema " docs/operations.zpp | grep -v "^  "

# Export diagrams (requires mermaid-cli)
# mmdc -i docs/architecture_overview.md -o diagrams/
```

## 🎓 Recommended Learning Order

### Level 1: Beginner (1-2 hours)
1. [SUMMARY.md](SUMMARY.md) - 15 min
2. [architecture_overview.md](architecture_overview.md) - Main sections - 30 min
3. [data_model.zpp](data_model.zpp) - Basic types and core entities - 30 min
4. [README.md](README.md) - Z++ notation guide - 15 min

### Level 2: Intermediate (3-5 hours)
1. [architecture_overview.md](architecture_overview.md) - Complete read - 1 hour
2. [data_model.zpp](data_model.zpp) - All schemas - 1 hour
3. [system_state.zpp](system_state.zpp) - Service states - 1 hour
4. [operations.zpp](operations.zpp) - Common operations - 1 hour
5. [integrations.zpp](integrations.zpp) - Key contracts - 1 hour

### Level 3: Advanced (10+ hours)
1. All documentation files - Complete study
2. Trace through composite operations
3. Understand all invariants
4. Map specifications to implementation
5. Contribute improvements

## ✅ Verification Checklist

When using these specifications:

- [ ] Read relevant architecture sections
- [ ] Understand data model schemas
- [ ] Check state invariants
- [ ] Verify operation contracts
- [ ] Review integration contracts
- [ ] Validate against implementation
- [ ] Update specifications if needed

## 🤝 Contributing

To add or update documentation:

1. Update relevant `.zpp` or `.md` files
2. Maintain consistent notation
3. Add/update invariants
4. Update diagrams if needed
5. Update this index if adding files
6. Run verification checks

## 📅 Version History

- **v1.0** (December 2025) - Initial comprehensive documentation release
  - Architecture overview with 18 Mermaid diagrams
  - Complete Z++ formal specifications (4 files)
  - Usage guides and quick reference
  - 3,807 lines of documentation

---

**Last Updated**: December 2025  
**Version**: 1.0  
**Status**: ✅ Complete  
**Maintained By**: Moeru AI Team

**Need help?** Check [README.md](README.md) or open an issue in the repository.
