# Moeru AI Monorepo - Technical Architecture Overview

## Executive Summary

The Moeru AI monorepo is a comprehensive collection of AI-powered applications and services focused on creating intelligent virtual companions and AI tooling infrastructure. The architecture is organized as a polyglot monorepo with projects in TypeScript/JavaScript, Rust, and Go, each serving distinct but interconnected purposes in the ecosystem.

## System Architecture

### High-Level Component Diagram

```mermaid
graph TB
    subgraph "Frontend Applications"
        AIRI_WEB[AIRI Web App]
        AIRI_DESKTOP[AIRI Desktop/Tamagotchi]
        CHAT[WebXR Chat UI]
        DEDITOR[Dataset Editor]
        HF_INSPECTOR[HuggingFace Inspector]
    end

    subgraph "Core AI Infrastructure"
        XSAI[xsAI SDK]
        STD[Standard Library]
        INVENTORY[Model Inventory Service]
        ORTTS[ONNX TTS Server]
        UNSPEECH[TTS Aggregator Service]
        DEMODEL[Model Downloader]
    end

    subgraph "Integration Services"
        DISCORD[Discord Bot Service]
        TELEGRAM[Telegram Bot Service]
        TWITTER[Twitter Service]
        MINECRAFT[Minecraft Integration]
        FACTORIO[Factorio AI]
        ECHO[Echo Service]
    end

    subgraph "Infrastructure & Tools"
        EVENTA[Eventa Event System]
        MCP_LAUNCHER[MCP Launcher]
        VELIN[Prompt Orchestration]
        GPUU[WebGPU Utilities]
    end

    subgraph "External Services"
        LLM_APIS[LLM APIs<br/>OpenAI, Claude, etc.]
        TTS_APIS[TTS APIs]
        HUGGINGFACE[HuggingFace Hub]
        OLLAMA[Ollama]
        DISCORD_API[Discord API]
        TELEGRAM_API[Telegram API]
    end

    AIRI_WEB --> XSAI
    AIRI_DESKTOP --> XSAI
    AIRI_WEB --> EVENTA
    AIRI_DESKTOP --> EVENTA
    
    XSAI --> LLM_APIS
    XSAI --> ORTTS
    XSAI --> UNSPEECH
    
    INVENTORY --> HUGGINGFACE
    INVENTORY --> OLLAMA
    DEMODEL --> HUGGINGFACE
    DEMODEL --> OLLAMA
    
    DISCORD --> DISCORD_API
    TELEGRAM --> TELEGRAM_API
    TWITTER --> XSAI
    
    ORTTS --> HUGGINGFACE
    UNSPEECH --> TTS_APIS
    
    MINECRAFT --> AIRI_WEB
    FACTORIO --> XSAI
```

### Technology Stack Distribution

```mermaid
graph LR
    subgraph "TypeScript/Node.js Ecosystem"
        TS1[AIRI - AI Companion]
        TS2[xsAI - AI SDK]
        TS3[eventa - Event System]
        TS4[xsai-transformers]
        TS5[xsai-use - Framework Bindings]
        TS6[deditor - Dataset Editor]
        TS7[velin - Prompt Orchestration]
        TS8[chat - WebXR UI]
    end

    subgraph "Rust Ecosystem"
        RUST1[ortts - TTS Server]
        RUST2[AIRI Tauri Plugins]
    end

    subgraph "Go Ecosystem"
        GO1[inventory - Model Catalog]
        GO2[unspeech - TTS Service]
        GO3[demodel - Model Downloader]
        GO4[mcp-launcher - MCP Manager]
    end

    style TS1 fill:#3178c6
    style TS2 fill:#3178c6
    style TS3 fill:#3178c6
    style TS4 fill:#3178c6
    style TS5 fill:#3178c6
    style TS6 fill:#3178c6
    style TS7 fill:#3178c6
    style TS8 fill:#3178c6
    style RUST1 fill:#ce422b
    style RUST2 fill:#ce422b
    style GO1 fill:#00add8
    style GO2 fill:#00add8
    style GO3 fill:#00add8
    style GO4 fill:#00add8
```

## Core Projects Deep Dive

### 1. AIRI - AI Companion System

**Technology**: TypeScript, Rust (Tauri plugins), Vue.js  
**Type**: Monorepo with multiple packages and services

#### Architecture

```mermaid
graph TB
    subgraph "AIRI Applications"
        WEB[stage-web<br/>Web Application]
        TAMAGOTCHI[stage-tamagotchi<br/>Desktop App]
    end

    subgraph "Runtime Layer"
        SERVER_RUNTIME[server-runtime<br/>WebSocket Server]
        SERVER_SDK[server-sdk<br/>Client SDK]
        SERVER_SHARED[server-shared<br/>Shared Types]
    end

    subgraph "Integration Services"
        DISCORD_SVC[discord-bot]
        TELEGRAM_SVC[telegram-bot]
        TWITTER_SVC[twitter-service]
        MINECRAFT_SVC[minecraft]
        ECHO_SVC[echo-service]
    end

    subgraph "Rust Plugins (Tauri)"
        AUDIO_VAD[audio-vad-ort]
        AUDIO_TRANS[audio-transcription-ort]
        MCP_PLUGIN[mcp]
        RDEV[rdev]
        WINDOW_ROUTER[window-router-link]
    end

    WEB --> SERVER_SDK
    TAMAGOTCHI --> SERVER_SDK
    SERVER_SDK --> SERVER_RUNTIME
    
    SERVER_RUNTIME --> DISCORD_SVC
    SERVER_RUNTIME --> TELEGRAM_SVC
    SERVER_RUNTIME --> TWITTER_SVC
    SERVER_RUNTIME --> MINECRAFT_SVC
    SERVER_RUNTIME --> ECHO_SVC
    
    TAMAGOTCHI --> AUDIO_VAD
    TAMAGOTCHI --> AUDIO_TRANS
    TAMAGOTCHI --> MCP_PLUGIN
    TAMAGOTCHI --> RDEV
```

#### Key Components

- **stage-web**: Browser-based interface for AIRI
- **stage-tamagotchi**: Desktop application using Tauri
- **server-runtime**: WebSocket-based real-time communication server
- **Integration Services**: Bot services for various platforms
- **Rust Plugins**: Native capabilities (audio processing, VAD, transcription)

### 2. xsAI - Extra-Small AI SDK

**Technology**: TypeScript  
**Type**: Modular AI SDK monorepo

#### Architecture

```mermaid
graph TB
    subgraph "Core Packages"
        MODEL[model<br/>Model Abstraction]
        SHARED[shared<br/>Common Utilities]
        SHARED_CHAT[shared-chat<br/>Chat Utilities]
    end

    subgraph "Generation Packages"
        GEN_TEXT[generate-text]
        GEN_SPEECH[generate-speech]
        GEN_IMAGE[generate-image]
        GEN_OBJECT[generate-object]
        GEN_TRANS[generate-transcription]
        STREAM_TEXT[stream-text]
        STREAM_TRANS[stream-transcription]
    end

    subgraph "Utility Packages"
        EMBED[embed<br/>Embeddings]
        TOOL[tool<br/>Tool Calling]
        UTILS_CHAT[utils-chat]
        UTILS_REASONING[utils-reasoning]
    end

    subgraph "Provider Extensions"
        PROVIDERS[providers<br/>Base Providers]
        PROVIDERS_CLOUD[providers-cloud<br/>Cloud APIs]
        PROVIDERS_LOCAL[providers-local<br/>Local Models]
        TRANSFORMERS[xsai-transformers<br/>Transformers.js]
    end

    MODEL --> SHARED
    SHARED_CHAT --> SHARED
    
    GEN_TEXT --> MODEL
    GEN_TEXT --> SHARED_CHAT
    GEN_SPEECH --> MODEL
    GEN_IMAGE --> MODEL
    GEN_OBJECT --> MODEL
    GEN_TRANS --> MODEL
    STREAM_TEXT --> GEN_TEXT
    STREAM_TRANS --> GEN_TRANS
    
    EMBED --> MODEL
    TOOL --> MODEL
    UTILS_CHAT --> SHARED_CHAT
    
    PROVIDERS --> MODEL
    PROVIDERS_CLOUD --> PROVIDERS
    PROVIDERS_LOCAL --> PROVIDERS
    TRANSFORMERS --> PROVIDERS_LOCAL
```

#### Provider Architecture

```mermaid
graph LR
    subgraph "Cloud Providers"
        OPENAI[OpenAI]
        ANTHROPIC[Claude]
        DEEPSEEK[DeepSeek]
        GROQ[Groq]
        GOOGLE[Google]
    end

    subgraph "Local Providers"
        OLLAMA_P[Ollama]
        TRANSFORMERS_P[Transformers.js]
        LLAMACPP[llama.cpp]
    end

    subgraph "xsAI SDK"
        PROVIDER_INTERFACE[Provider Interface]
        MODEL_LAYER[Model Layer]
    end

    OPENAI --> PROVIDER_INTERFACE
    ANTHROPIC --> PROVIDER_INTERFACE
    DEEPSEEK --> PROVIDER_INTERFACE
    GROQ --> PROVIDER_INTERFACE
    GOOGLE --> PROVIDER_INTERFACE
    OLLAMA_P --> PROVIDER_INTERFACE
    TRANSFORMERS_P --> PROVIDER_INTERFACE
    LLAMACPP --> PROVIDER_INTERFACE
    
    PROVIDER_INTERFACE --> MODEL_LAYER
```

### 3. ortts - ONNX Runtime TTS Server

**Technology**: Rust  
**Type**: HTTP API server for text-to-speech

#### Architecture

```mermaid
graph TB
    subgraph "HTTP Server Layer"
        ROUTES[Route Handlers]
        OPENAPI[OpenAPI Spec]
    end

    subgraph "Core Processing"
        SERVER_CORE[Server Core]
        ONNX_WRAPPER[ONNX Wrapper]
    end

    subgraph "Model Layer"
        CHATTERBOX[Chatterbox Multilingual Model]
        ONNX_RT[ONNX Runtime]
    end

    subgraph "Text Processing"
        JIEBA[Jieba Tokenizer]
        KAKASI[Kakasi Japanese]
        UNICODE_NORM[Unicode Normalization]
    end

    CLIENT[HTTP Client] --> ROUTES
    ROUTES --> SERVER_CORE
    SERVER_CORE --> ONNX_WRAPPER
    ONNX_WRAPPER --> CHATTERBOX
    CHATTERBOX --> ONNX_RT
    
    SERVER_CORE --> JIEBA
    SERVER_CORE --> KAKASI
    SERVER_CORE --> UNICODE_NORM
```

### 4. inventory - Model Catalog Service

**Technology**: Go  
**Type**: gRPC service for model metadata management

#### Architecture

```mermaid
graph TB
    subgraph "API Layer"
        GRPC_SERVER[gRPC Server]
        HTTP_GATEWAY[gRPC-Gateway<br/>HTTP/REST]
        OPENAPI_DOC[OpenAPI Documentation]
    end

    subgraph "Service Layer"
        INVENTORY_SVC[Inventory Service]
        CRON[Scheduled Jobs]
    end

    subgraph "Integration Layer"
        HF_CLIENT[HuggingFace Client]
        OLLAMA_CLIENT[Ollama Client]
        VLLM_CLIENT[vLLM Client]
    end

    subgraph "External Services"
        HUGGINGFACE_API[HuggingFace API]
        OLLAMA_API[Ollama API]
        VLLM_API[vLLM API]
    end

    GRPC_CLIENT[gRPC Client] --> GRPC_SERVER
    HTTP_CLIENT[HTTP Client] --> HTTP_GATEWAY
    HTTP_GATEWAY --> GRPC_SERVER
    
    GRPC_SERVER --> INVENTORY_SVC
    CRON --> INVENTORY_SVC
    
    INVENTORY_SVC --> HF_CLIENT
    INVENTORY_SVC --> OLLAMA_CLIENT
    INVENTORY_SVC --> VLLM_CLIENT
    
    HF_CLIENT --> HUGGINGFACE_API
    OLLAMA_CLIENT --> OLLAMA_API
    VLLM_CLIENT --> VLLM_API
```

## Data Flow Diagrams

### AIRI Text-to-Speech Flow

```mermaid
sequenceDiagram
    participant User
    participant AIRI_UI as AIRI UI
    participant Server as AIRI Server
    participant xsAI
    participant LLM as LLM Provider
    participant TTS as ortts/unspeech
    
    User->>AIRI_UI: Voice/Text Input
    AIRI_UI->>Server: WebSocket Message
    Server->>xsAI: Generate Text Request
    xsAI->>LLM: API Call
    LLM-->>xsAI: Text Response
    xsAI-->>Server: Generated Text
    Server->>TTS: TTS Request
    TTS-->>Server: Audio Data
    Server-->>AIRI_UI: WebSocket Audio Stream
    AIRI_UI-->>User: Play Audio
```

### Model Download & Catalog Flow

```mermaid
sequenceDiagram
    participant Client
    participant Inventory as Inventory Service
    participant Demodel as Demodel Service
    participant HF as HuggingFace
    participant Ollama
    
    Client->>Inventory: List Available Models
    Inventory->>HF: Query Models
    Inventory->>Ollama: Query Models
    HF-->>Inventory: Model Metadata
    Ollama-->>Inventory: Model Metadata
    Inventory-->>Client: Unified Model List
    
    Client->>Demodel: Download Model
    Demodel->>HF: Accelerated Download
    HF-->>Demodel: Model Files
    Demodel-->>Client: Download Complete
```

### Event-Driven Architecture (eventa)

```mermaid
graph LR
    subgraph "Event Sources"
        WEB_WORKER[Web Worker]
        WEBSOCKET[WebSocket]
        ELECTRON_IPC[Electron IPC]
        HTTP_RPC[HTTP/RPC]
    end

    subgraph "eventa Event System"
        EVENT_BUS[Type-Safe Event Bus]
        EVENT_ROUTER[Event Router]
    end

    subgraph "Event Handlers"
        HANDLER1[Handler 1]
        HANDLER2[Handler 2]
        HANDLER3[Handler 3]
    end

    WEB_WORKER --> EVENT_BUS
    WEBSOCKET --> EVENT_BUS
    ELECTRON_IPC --> EVENT_BUS
    HTTP_RPC --> EVENT_BUS
    
    EVENT_BUS --> EVENT_ROUTER
    
    EVENT_ROUTER --> HANDLER1
    EVENT_ROUTER --> HANDLER2
    EVENT_ROUTER --> HANDLER3
```

## Integration Boundaries

### External Service Integrations

```mermaid
graph TB
    subgraph "Moeru AI Platform"
        CORE[Core Services]
    end

    subgraph "LLM Providers"
        OPENAI_EXT[OpenAI API]
        CLAUDE_EXT[Anthropic Claude]
        DEEPSEEK_EXT[DeepSeek]
        GROQ_EXT[Groq]
    end

    subgraph "Model Repositories"
        HF_EXT[HuggingFace Hub]
        OLLAMA_EXT[Ollama Registry]
    end

    subgraph "Communication Platforms"
        DISCORD_EXT[Discord]
        TELEGRAM_EXT[Telegram]
        TWITTER_EXT[Twitter/X]
    end

    subgraph "Gaming Platforms"
        MINECRAFT_EXT[Minecraft Server]
        FACTORIO_EXT[Factorio RCON]
    end

    CORE --> OPENAI_EXT
    CORE --> CLAUDE_EXT
    CORE --> DEEPSEEK_EXT
    CORE --> GROQ_EXT
    
    CORE --> HF_EXT
    CORE --> OLLAMA_EXT
    
    CORE --> DISCORD_EXT
    CORE --> TELEGRAM_EXT
    CORE --> TWITTER_EXT
    
    CORE --> MINECRAFT_EXT
    CORE --> FACTORIO_EXT
```

## Deployment Architecture

```mermaid
graph TB
    subgraph "Client Tier"
        BROWSER[Web Browser]
        DESKTOP_APP[Desktop Application]
        MOBILE[Mobile PWA]
    end

    subgraph "Application Tier"
        WEB_SERVER[Web Server<br/>Node.js]
        WEBSOCKET_SERVER[WebSocket Server<br/>AIRI Runtime]
    end

    subgraph "Service Tier"
        ORTTS_SVC[ortts TTS Service<br/>Rust]
        INVENTORY_SVC[Inventory Service<br/>Go/gRPC]
        UNSPEECH_SVC[Unspeech Service<br/>Go]
        DEMODEL_SVC[Demodel Service<br/>Go]
    end

    subgraph "Bot Services"
        DISCORD_BOT[Discord Bot]
        TELEGRAM_BOT[Telegram Bot]
        TWITTER_BOT[Twitter Service]
    end

    subgraph "External Dependencies"
        LLM_CLOUD[LLM Cloud APIs]
        HF_CLOUD[HuggingFace Cloud]
        SOCIAL_APIS[Social Platform APIs]
    end

    BROWSER --> WEB_SERVER
    DESKTOP_APP --> WEBSOCKET_SERVER
    MOBILE --> WEB_SERVER
    
    WEB_SERVER --> WEBSOCKET_SERVER
    WEBSOCKET_SERVER --> ORTTS_SVC
    WEBSOCKET_SERVER --> INVENTORY_SVC
    WEBSOCKET_SERVER --> UNSPEECH_SVC
    
    WEBSOCKET_SERVER --> DISCORD_BOT
    WEBSOCKET_SERVER --> TELEGRAM_BOT
    WEBSOCKET_SERVER --> TWITTER_BOT
    
    ORTTS_SVC --> HF_CLOUD
    INVENTORY_SVC --> HF_CLOUD
    DEMODEL_SVC --> HF_CLOUD
    
    WEBSOCKET_SERVER --> LLM_CLOUD
    DISCORD_BOT --> SOCIAL_APIS
    TELEGRAM_BOT --> SOCIAL_APIS
    TWITTER_BOT --> SOCIAL_APIS
```

## Technology Choices and Rationale

### TypeScript/Node.js Projects
- **Rationale**: Rapid development, extensive ecosystem, web compatibility
- **Used for**: UI applications, SDK development, event systems
- **Key libraries**: Vue.js, Vite, Turbo (monorepo), pnpm (package management)

### Rust Projects
- **Rationale**: Performance, safety, native capabilities
- **Used for**: TTS inference server, Tauri desktop plugins
- **Key libraries**: Axum (HTTP server), ONNX Runtime, Tauri

### Go Projects
- **Rationale**: Excellent for network services, gRPC support, simplicity
- **Used for**: Microservices, API gateways, model management
- **Key libraries**: gRPC, Echo, Fx (dependency injection)

## Shared Infrastructure Components

### eventa - Event System
- Type-safe event-driven architecture
- Cross-boundary communication (Web Workers, WebSocket, IPC)
- Used across multiple projects for decoupled communication

### Standard Library (std)
- Shared standards and utilities for Moeru AI projects
- Common patterns and best practices

### MCP Launcher
- Model Context Protocol (MCP) server management
- Simplifies integration of MCP-compatible tools
- Docker-like experience for MCP servers

## Security Considerations

### Authentication & Authorization
- Token-based authentication in AIRI server runtime
- API key management for external services
- Secure credential storage

### Data Privacy
- Self-hosted option for complete data ownership
- Local model execution capabilities (via Ollama, Transformers.js)
- Minimal external API dependencies when using local models

### Network Security
- WebSocket secure connections (WSS)
- gRPC with TLS support
- Rate limiting and request validation

## Scalability & Performance

### Horizontal Scaling
- Stateless service design (inventory, unspeech, demodel)
- Load balancing ready for Go services
- Independent scaling of TTS and LLM inference

### Caching Strategy
- Model metadata caching in inventory service
- Audio output caching potential
- Response caching in xsAI SDK

### Performance Optimizations
- ONNX Runtime for efficient TTS inference
- WebGPU support for browser-based ML
- Parallel model downloads in demodel
- Turbo for efficient monorepo builds

## Development Workflow

### Monorepo Structure
- Independent project development
- Shared tooling and configurations
- Cross-project dependency management
- Unified version control

### Build System
- TypeScript projects: Vite, Turbo, tsdown
- Rust projects: Cargo workspaces
- Go projects: Go modules
- Per-project build scripts with monorepo orchestration

### Testing Strategy
- Unit tests per package
- Integration tests for service boundaries
- E2E tests for AIRI applications
- CI/CD pipeline considerations

## Future Architecture Considerations

### Potential Enhancements
- Kubernetes deployment manifests
- Service mesh for microservice communication
- Distributed tracing implementation
- Centralized configuration management
- Database layer for persistent storage
- Message queue for async job processing

### Extension Points
- Plugin system for AIRI extensions
- Custom provider support in xsAI
- Additional game integrations
- Enhanced RAG capabilities
- Multi-modal model support

## Conclusion

The Moeru AI monorepo represents a comprehensive ecosystem for AI-powered virtual companions and AI infrastructure. The architecture is designed for:

1. **Modularity**: Each project serves a specific purpose and can be used independently
2. **Interoperability**: Projects work together through well-defined interfaces
3. **Flexibility**: Support for both cloud and local execution
4. **Extensibility**: Plugin systems and provider abstractions allow easy extension
5. **Performance**: Use of efficient languages and runtimes where needed
6. **Developer Experience**: Modern tooling and clear project boundaries

The polyglot approach leverages the strengths of each language while maintaining clear separation of concerns and integration points.
