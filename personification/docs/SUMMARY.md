# Moeru AI Monorepo - Documentation Summary

## 📋 Quick Reference

This document provides a high-level overview of the Moeru AI monorepo architecture and formal specifications.

## 🏗️ System Architecture At a Glance

### Core Systems

| System | Technology | Purpose | Port/Protocol |
|--------|-----------|---------|---------------|
| **AIRI Server** | TypeScript/Node.js | AI companion runtime, WebSocket server | WebSocket |
| **xsAI SDK** | TypeScript | Multi-provider AI SDK | Library |
| **ortts** | Rust | ONNX-based TTS inference server | HTTP (8080) |
| **inventory** | Go | Model catalog service | gRPC + HTTP |
| **unspeech** | Go | TTS service aggregator | HTTP |
| **demodel** | Go | Accelerated model downloader | HTTP |
| **mcp-launcher** | Go | MCP server management | HTTP |
| **eventa** | TypeScript | Type-safe event system | Library |

### Integration Services

| Service | Platform | Technology | Purpose |
|---------|----------|-----------|---------|
| **Discord Bot** | Discord | TypeScript | Discord voice/text integration |
| **Telegram Bot** | Telegram | TypeScript | Telegram messaging |
| **Twitter Service** | Twitter/X | TypeScript | Social media integration |
| **Minecraft** | Minecraft | TypeScript | Game world interaction |
| **Factorio** | Factorio | TypeScript | Factory game integration |
| **Echo Service** | Internal | TypeScript | Test/development service |

## 📊 Technology Stack

### TypeScript/Node.js (60%)
- **Frontend**: AIRI web, Tamagotchi desktop (Vue.js, Tauri)
- **Backend**: Server runtime, integration services
- **SDKs**: xsAI, eventa, xsai-transformers
- **Tools**: deditor, velin, hfup, gpuu

### Rust (15%)
- **Services**: ortts TTS server
- **Plugins**: Tauri desktop plugins (audio VAD, transcription, MCP)

### Go (25%)
- **Services**: inventory, unspeech, demodel, mcp-launcher
- **Architecture**: gRPC, microservices

## 🔄 Key Data Flows

### 1. Chat Flow
```
User Input → AIRI UI → WebSocket → AIRI Server → xsAI → LLM Provider
                                                    ↓
User ← Audio ← TTS (ortts/unspeech) ← Text ← AIRI Server
```

### 2. Model Management Flow
```
Client → inventory (list models) → HuggingFace/Ollama
Client → demodel (download) → HuggingFace Hub → Local Storage
```

### 3. Event Flow
```
Event Source → eventa Event Bus → Event Router → Handlers
```

## 📁 Specification Files

### data_model.zpp (12 KB)
**Defines**: 40+ data schemas covering:
- Sessions & connections
- AI models & metadata
- Messages & conversations
- TTS requests/responses
- Service states
- Events & handlers

**Key Schemas**:
- `UserSession`, `WebSocketConnection`
- `ModelConfig`, `ModelMetadata`, `CatalogEntry`
- `ChatMessage`, `ConversationContext`
- `TTSRequest`, `TTSResponse`
- `ModulePeer`, `EventHandler`

### system_state.zpp (17 KB)
**Defines**: Complete system state with invariants:
- AIRI server state
- Integration services state
- xsAI SDK state
- Service states (ortts, inventory, unspeech, demodel, MCP)
- Event system state
- Global system state with cross-service invariants

**Key Invariants**:
- Session authentication requirements
- Connection state consistency
- Port conflict prevention
- Service availability rules
- Data consistency constraints

### operations.zpp (20 KB)
**Defines**: 30+ operations with pre/post-conditions:
- Authentication & session management
- WebSocket connection handling
- Conversation management
- AI text generation & streaming
- TTS request processing
- Model catalog operations
- Download management
- MCP server lifecycle
- Event publishing & processing

**Operation Types**:
- State-changing (Δ)
- Read-only (Ξ)
- Composite workflows

### integrations.zpp (16.6 KB)
**Defines**: 20+ external integration contracts:
- **LLM Providers**: OpenAI, Anthropic, DeepSeek, Groq
- **Local Models**: Ollama, Transformers.js
- **Repositories**: HuggingFace Hub, Ollama Registry
- **Social**: Discord, Telegram, Twitter/X
- **Gaming**: Minecraft RCON, Factorio RCON
- **TTS**: ElevenLabs, Azure Speech
- **Auth**: OAuth2, API Key
- **Patterns**: Retry, circuit breaker, load balancing

## 🎯 Key Architectural Patterns

### 1. Microservices Architecture
- Independent services with clear boundaries
- gRPC for internal communication
- HTTP/REST for external APIs
- WebSocket for real-time client connections

### 2. Event-Driven Architecture
- Type-safe event bus (eventa)
- Decoupled service communication
- Cross-boundary event propagation

### 3. Provider Abstraction
- Unified interface for multiple AI providers
- Fallback and load balancing
- Local and cloud provider support

### 4. Module System
- Dynamic module registration
- Capability-based permissions
- WebSocket-based module communication

### 5. Connection Pooling & Circuit Breaking
- Connection reuse for efficiency
- Automatic failure detection
- Graceful degradation

## 🔐 Security Features

- Token-based authentication for AIRI server
- API key management for external services
- Secure WebSocket connections (WSS)
- gRPC with TLS support
- Rate limiting per service
- Self-hosted option for data privacy

## 📈 Scalability Considerations

### Horizontal Scaling
- Stateless Go services (inventory, unspeech, demodel)
- Load balancing ready
- Independent service scaling

### Caching Strategy
- Model metadata caching (inventory)
- Response caching (xsAI)
- Embedding caching
- Audio output caching potential

### Performance Optimizations
- ONNX Runtime for TTS
- WebGPU for browser ML
- Parallel downloads (demodel)
- Turbo for monorepo builds
- Connection pooling

## 🚀 Deployment Options

### Self-Hosted
- Complete data ownership
- Local model execution
- Minimal external dependencies

### Cloud
- Scalable infrastructure
- Cloud LLM providers
- Managed services

### Hybrid
- Local TTS inference
- Cloud LLM APIs
- Self-hosted bot services

## 📊 System Metrics

### Specifications
- **4** main specification files
- **65+** formal schemas
- **30+** operations with contracts
- **20+** integration contracts
- **18** Mermaid diagrams
- **3,050** lines of specifications

### Codebase (Approximate)
- **25+** projects in monorepo
- **3** programming languages (TypeScript, Rust, Go)
- **10+** integration services
- **6** core AI infrastructure services

## 🔧 Common Operations

### Starting a Chat Session
```
1. AuthenticateSession (token) → sessionId
2. EstablishConnection (session) → connectionId  
3. StartConversation (userId, model) → conversationId
4. AddMessage (conversationId, userMessage)
5. GenerateText (messages, model) → response
6. ProcessTTSRequest (response.text) → audio
```

### Downloading a Model
```
1. ListModels (modelType, provider) → models
2. GetModelMetadata (modelId) → metadata
3. QueueDownload (modelId, sourceUrl) → taskId
4. StartDownload (taskId) → downloading
5. UpdateDownloadProgress (taskId, bytes)
6. CompleteDownload (taskId, success) → completed
```

### Publishing an Event
```
1. RegisterEventHandler (eventType, callback) → handlerId
2. PublishEvent (event) → queued
3. ProcessEvent → handlers invoked
```

## 📚 Documentation Navigation

- **[Architecture Overview](architecture_overview.md)**: Complete system architecture with diagrams
- **[Data Model](data_model.zpp)**: Formal data structure specifications
- **[System State](system_state.zpp)**: Complete state management specification
- **[Operations](operations.zpp)**: Operation contracts with pre/post-conditions
- **[Integrations](integrations.zpp)**: External service integration contracts
- **[README](README.md)**: Detailed documentation guide

## 🎓 Learning Path

### Beginner
1. Read Architecture Overview
2. Understand core systems (AIRI, xsAI)
3. Review basic data models
4. Explore simple operations

### Intermediate
1. Study system state management
2. Understand operation contracts
3. Learn integration patterns
4. Review service architectures

### Advanced
1. Deep dive into formal specifications
2. Understand global invariants
3. Study composite operations
4. Master integration contracts
5. Contribute to specifications

## 🔄 Maintenance Schedule

### Regular Updates
- **After major features**: Update affected specifications
- **Architecture changes**: Update overview and diagrams
- **New integrations**: Add to integrations.zpp
- **API changes**: Update operation contracts

### Quarterly Reviews
- Validate specifications against implementation
- Update metrics and statistics
- Review and improve invariants
- Enhance documentation clarity

## 📞 Contact & Support

- **Repository**: [github.com/moeru-ai](https://github.com/moeru-ai)
- **Discord**: Join for community support
- **Issues**: Report documentation issues on GitHub
- **Website**: [moeru-ai.github.io](https://moeru-ai.github.io)

---

**Version**: 1.0  
**Last Updated**: December 2025  
**Status**: ✅ Complete  
**Maintained By**: Moeru AI Team
