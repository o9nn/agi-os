# Personification Layer Integration - Moeru AI

## Date: December 13, 2025

## Overview

Successfully integrated **Moeru AI** into AGI-OS as the **personification layer**, providing embodiment, personality, and user interaction capabilities through AIRI - a self-hosted cyber companion with realtime voice chat, game playing, and Live2D visualization.

## Integration Details

### Source Repository
**Original**: https://github.com/o9nn/moeru-ai  
**Integrated As**: `./personification/`  
**Status**: ✅ Cloned and integrated (`.git` removed)

### Commit Information
**Commit Hash**: `a7b3fbc39`  
**Branch**: `main`  
**Files Added**: 5,255 files  
**Size**: 404.94 MiB  
**Status**: ✅ Successfully pushed to origin

## Moeru AI Features

### Core Project: AIRI

**AIRI (アイリ)** is a self-hosted, user-owned cyber companion - a "container of souls" bringing virtual beings into our world. Inspired by Neuro-sama, AIRI aims to achieve similar capabilities while being fully open source and self-hosted.

**Key Capabilities**:
- **Realtime Voice Chat**: Natural conversation with voice synthesis and recognition
- **Game Playing**: Minecraft and Factorio integration with computer vision and LLM
- **Live2D Visualization**: Animated character models with personality expression
- **Cross-Platform**: Web, macOS, Windows support with PWA for mobile
- **Self-Hosted**: Complete ownership and control of your digital companion

### Web-First Architecture

Unlike traditional AI VTuber projects, AIRI is built with modern web technologies:

**WebGPU**: Hardware-accelerated graphics and compute
**WebAudio**: High-quality audio processing and synthesis
**Web Workers**: Multi-threaded computation
**WebAssembly**: Near-native performance for critical paths
**WebSocket**: Real-time bidirectional communication

**Performance**: Desktop version supports native NVIDIA CUDA and Apple Metal through HuggingFace Candle, providing optimal performance without complex dependency management.

## Moeru AI Ecosystem Components

### Core Infrastructure

#### xsai - Extra-Small AI SDK
Lightweight AI SDK for building conversational applications with minimal overhead.

**Features**:
- Minimal API surface
- Framework-agnostic design
- TypeScript support
- Multiple provider support

#### ortts - ONNX Runtime TTS
Simple and easy-to-use local text-to-speech inference server powered by ONNX Runtime.

**Features**:
- Local inference (no cloud dependency)
- ONNX Runtime optimization
- Multiple voice models
- REST API interface

#### eventa - Type-Safe Event System
Truly type-safe event-driven toolbox for distributed systems.

**Use Cases**:
- Web Worker communication
- WebSocket messaging
- Electron IPC
- RPC systems

**Philosophy**: "Events are all you need" - define events everywhere, use them anywhere.

#### inventory - Universal Model Catalog
Your universal model catalog for managing AI models across different runtimes.

**Features**:
- Unified model management
- Multi-runtime support (HuggingFace, Ollama, vLLM)
- Model versioning
- Metadata tracking

#### unspeech - Unified TTS Services
All-in-one text-to-speech service aggregator.

**Supported Services**:
- Local ONNX models
- Cloud TTS providers
- Custom voice synthesis
- Voice cloning

#### demodel - Model Downloader
Easily boost the speed of pulling models and datasets from various inference runtimes.

**Supported Sources**:
- 🤗 HuggingFace
- 🐫 Ollama
- vLLM
- Custom registries

### Game Integrations

#### airi-factorio
AI plays Factorio with computer vision and LLM combined.

**Technologies**:
- YOLO object detection
- LLM decision making
- Game state analysis
- Automated factory building

#### airi-minecraft
Intelligent Minecraft bot powered by LLM (archived - merged into main AIRI).

**Capabilities**:
- Natural language commands
- Autonomous exploration
- Resource gathering
- Building assistance

### UI & Visualization

#### chat - WebXR Voice Call UI
WebXR-powered voice call interface that makes AI characters appear in your space.

**Features**:
- Virtual reality support
- Augmented reality mode
- Spatial audio
- 3D character rendering

#### deditor - Dataset Editor
Opinionated dataset editor for managing, labeling, and processing massive-scale data.

**Features**:
- Visual data exploration
- Batch labeling
- Data transformation
- Export to multiple formats

#### hf-inspector - HuggingFace Inspector
Web-based tool to find and inspect downloaded HuggingFace models.

**Features**:
- Local model discovery
- Model metadata viewing
- Storage analysis
- Quick access to model files

#### three-mmd - MMD for Three.js
Use MikuMikuDance (MMD) models and animations in Three.js.

**Features**:
- MMD model loading
- Animation playback
- Physics simulation
- Camera control

### Development Tools

#### velin - Prompt Orchestration
Prompt orchestration with familiar stacks like Vue.js.

**Features**:
- Component-based prompts
- Template composition
- Variable interpolation
- Conditional logic

#### hfup - HuggingFace Deployment Tools
Collection of tools to deploy and bundle HuggingFace Spaces and related assets.

**Features**:
- Space deployment automation
- Asset bundling
- Configuration management
- CI/CD integration

#### gpuu - WebGPU Utilities
Set of utility tools to work with WebGPU.

**Features**:
- Shader helpers
- Buffer management
- Pipeline creation
- Performance profiling

#### cosine-similarity - Vector Similarity
Fast and small cosine similarity calculation for vector comparison.

**Use Cases**:
- Semantic search
- Document similarity
- Embedding comparison
- Recommendation systems

### Additional Components

#### arpk - LLM Translator
LLM as your translator with DeepLX-compatible API.

**Features**:
- Multiple language support
- Context-aware translation
- API compatibility
- Local or cloud LLM

#### mcp-launcher - MCP Server Launcher
Easy-to-use MCP builder and launcher for all MCP servers, like Ollama for models.

**Features**:
- One-command server launch
- Configuration management
- Multi-server support
- Docker integration

#### L3.1-Moe - Mixture of Experts
Mixture of Experts (MoE) model made with mergekit-moe.

**Architecture**:
- Multiple expert models
- Routing mechanism
- Specialized capabilities
- Efficient inference

## Architecture Integration

### Updated AGI-OS Layer Structure

```
┌─────────────────────────────────────────────────────────────┐
│        Layer 8: Personification (Moeru AI) ✨               │
│     (Embodiment, Personality, User Interaction, AIRI)       │
├─────────────────────────────────────────────────────────────┤
│        Layer 7: CogPilot (Computational Intelligence)       │
│     (Deep Tree Echo, SciML, Mathematical Foundations)       │
├─────────────────────────────────────────────────────────────┤
│        Layer 6: Consciousness (EchOllama)                   │
│     (Self-Awareness, Embodied Cognition, Deep Tree Echo)    │
├─────────────────────────────────────────────────────────────┤
│              Layer 4: CogBolt (AI-Powered IDE)              │
├─────────────────────────────────────────────────────────────┤
│          Layer 5: Cognitive-Grip (Integration Layer)        │
├─────────────────────────────────────────────────────────────┤
│         Layer 3: OpenCog (Cognitive Framework)              │
│  Foundation | Storage | Network | Reasoning | Learning     │
├─────────────────────────────────────────────────────────────┤
│              Layer 2: HurdCog (Operating System)            │
├─────────────────────────────────────────────────────────────┤
│              Layer 1: CogNumach (Microkernel)               │
├─────────────────────────────────────────────────────────────┤
│           Layer 0.5: GGML (AI Inference Engine)             │
├─────────────────────────────────────────────────────────────┤
│           Layer 0: MIG (Mach Interface Generator)           │
└─────────────────────────────────────────────────────────────┘
```

### Personification Layer Position

The personification layer sits at the **highest user-facing level**, providing:

**Embodiment**: Physical/visual representation through Live2D, MMD, and WebXR.

**Personality**: Character traits, emotional states, and behavioral patterns.

**User Interaction**: Natural language conversation, voice chat, and multimodal communication.

**Presence**: Real-time responsiveness and situational awareness in games and applications.

**Accessibility**: Cross-platform support (web, desktop, mobile) for ubiquitous access.

## Integration with Existing Layers

### 1. Personification ↔ Consciousness Integration

```
AIRI (personification)
    ↓ Personality expression
Consciousness (self-awareness)
    ↓ Cognitive state
Cognitive Framework (reasoning)
```

**Use Case**: AIRI provides the personality and embodiment layer on top of the consciousness layer's self-awareness, creating a complete cyber being with both internal experience and external expression.

### 2. Personification ↔ CogPilot Integration

```
AIRI (game playing)
    ↓ Action planning
CogPilot (computational intelligence)
    ↓ Physics simulation
Differential Equations (dynamics)
```

**Use Case**: Game playing in Minecraft and Factorio uses CogPilot's computational intelligence for physics-based planning and optimal action sequences.

### 3. Personification ↔ GGML Integration

```
AIRI (voice chat)
    ↓ TTS/STT
GGML (inference)
    ↓ Model execution
Neural Networks (speech models)
```

**Use Case**: Realtime voice chat uses GGML for efficient local inference of speech recognition and synthesis models.

### 4. Personification ↔ OpenCog Integration

```
AIRI (conversation)
    ↓ Natural language
PLN (reasoning)
    ↓ Logical inference
AtomSpace (knowledge)
```

**Use Case**: Conversational AI uses PLN for logical reasoning and AtomSpace for knowledge retrieval, enabling contextual and coherent dialogue.

## Technical Specifications

### Language & Runtime
- **Primary Languages**: TypeScript, JavaScript
- **Runtime**: Node.js, Browser (Web APIs)
- **Package Manager**: pnpm, npm
- **Build Tools**: Vite, Rollup, esbuild

### Key Technologies

**Frontend**:
- Vue.js 3 (Composition API)
- Three.js (3D graphics)
- WebGPU (GPU compute)
- WebXR (VR/AR)

**Backend**:
- ONNX Runtime (ML inference)
- WebSocket (real-time communication)
- REST APIs (service integration)

**AI/ML**:
- Transformers.js (browser ML)
- YOLO (computer vision)
- LLM integration (multiple providers)
- Voice synthesis/recognition

### Performance Characteristics

**Web Version**:
- Runs in modern browsers
- WebGPU acceleration
- Progressive Web App (PWA)
- Mobile support

**Desktop Version**:
- Native CUDA (NVIDIA)
- Native Metal (Apple)
- Optimized inference
- Lower latency

## Usage Examples

### Basic AIRI Setup

```bash
# Clone and setup
cd /path/to/agi-os/personification/airi

# Install dependencies
pnpm install

# Start development server
pnpm dev

# Build for production
pnpm build

# Start production server
pnpm start
```

### Voice Chat Integration

```typescript
import { AIRI } from './personification/airi'
import { VoiceChat } from './personification/airi/voice'

// Initialize AIRI
const airi = new AIRI({
  personality: 'friendly',
  voice: 'female-1',
  language: 'en-US'
})

// Start voice chat
const chat = new VoiceChat(airi)
await chat.start()

// Handle conversation
chat.on('message', (text) => {
  console.log('User:', text)
})

chat.on('response', (text) => {
  console.log('AIRI:', text)
})
```

### Game Integration Example

```typescript
import { MinecraftBot } from './personification/airi-minecraft'

// Create bot
const bot = new MinecraftBot({
  host: 'localhost',
  port: 25565,
  username: 'AIRI'
})

// Connect to server
await bot.connect()

// Natural language commands
bot.command('build a house')
bot.command('gather wood')
bot.command('follow me')
```

### Live2D Visualization

```typescript
import { Live2DModel } from './personification/airi/live2d'

// Load model
const model = await Live2DModel.load('models/airi.model3.json')

// Set expression
model.setExpression('happy')

// Animate
model.startMotion('idle')

// Lip sync
model.lipSync(audioData)
```

## Build System Integration

### Proposed CMakeLists.txt Addition

```cmake
# Layer 8: Personification (Moeru AI / AIRI)
OPTION(BUILD_PERSONIFICATION "Build Personification Layer (Moeru AI)" ON)

IF(BUILD_PERSONIFICATION)
    MESSAGE(STATUS "====================================")
    MESSAGE(STATUS "Layer 8: Personification (Moeru AI)")
    MESSAGE(STATUS "====================================")
    
    # Find Node.js and pnpm
    find_program(NODE_EXECUTABLE node)
    find_program(PNPM_EXECUTABLE pnpm)
    
    IF(NODE_EXECUTABLE AND PNPM_EXECUTABLE)
        MESSAGE(STATUS "  Node.js found: ${NODE_EXECUTABLE}")
        MESSAGE(STATUS "  pnpm found: ${PNPM_EXECUTABLE}")
        
        # Get versions
        execute_process(
            COMMAND ${NODE_EXECUTABLE} --version
            OUTPUT_VARIABLE NODE_VERSION
            OUTPUT_STRIP_TRAILING_WHITESPACE
        )
        MESSAGE(STATUS "  Node.js version: ${NODE_VERSION}")
        
        # Install dependencies for AIRI
        add_custom_target(personification_install ALL
            COMMAND ${PNPM_EXECUTABLE} install
            WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/personification/airi
            COMMENT "Installing AIRI dependencies"
        )
        
        # Build AIRI
        add_custom_target(personification_build ALL
            COMMAND ${PNPM_EXECUTABLE} build
            WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/personification/airi
            DEPENDS personification_install
            COMMENT "Building AIRI application"
        )
        
        # Install built files
        install(DIRECTORY ${CMAKE_SOURCE_DIR}/personification/airi/dist/
            DESTINATION share/agi-os/personification/airi
        )
        
        MESSAGE(STATUS "  Personification layer configured successfully")
        
    ELSE()
        MESSAGE(WARNING "  Node.js or pnpm not found - personification will not be built")
        MESSAGE(WARNING "  Install Node.js 18+ and pnpm")
    ENDIF()
ENDIF()
```

### Proposed build-agi-os.sh Addition

```bash
BUILD_PERSONIFICATION=1  # Enable by default

# Layer 8: Personification (Moeru AI / AIRI)
if [ $BUILD_PERSONIFICATION -eq 1 ]; then
    log_info "======================================"
    log_info "Layer 8: Personification (Moeru AI)"
    log_info "======================================"
    
    if ! command -v node &> /dev/null || ! command -v pnpm &> /dev/null; then
        log_warning "Node.js or pnpm not found - skipping personification"
        log_warning "Install Node.js 18+ and pnpm"
    else
        NODE_VERSION=$(node --version)
        PNPM_VERSION=$(pnpm --version)
        log_info "Found Node.js: $NODE_VERSION"
        log_info "Found pnpm: $PNPM_VERSION"
        
        cd "$BUILD_DIR/../personification/airi"
        
        # Install dependencies
        log_info "Installing dependencies..."
        pnpm install
        
        # Build AIRI
        log_info "Building AIRI..."
        pnpm build
        
        # Install to system
        log_info "Installing AIRI..."
        mkdir -p "$INSTALL_PREFIX/share/agi-os/personification/airi"
        cp -r dist/* "$INSTALL_PREFIX/share/agi-os/personification/airi/"
        
        log_success "Personification layer built and installed"
        cd "$BUILD_DIR/.."
    fi
fi
```

## Debian Packaging

### Proposed Package Structure

```
infrastructure/packaging/debian/personification/
├── debian/
│   ├── control          # Package metadata
│   ├── rules            # Build rules for Node.js
│   ├── changelog        # Version 1.0.0-1
│   ├── copyright        # License (check upstream)
│   └── install          # Installation rules
└── README.md            # Package documentation
```

### Package Dependencies

```
Package: agi-personification
Depends: nodejs (>= 18.0), pnpm
Recommends: consciousness, cognitive-grip, ggml
Suggests: cogbolt, atomspace
Description: Personification layer for AGI-OS
 Moeru AI provides embodiment, personality, and user interaction
 capabilities for AGI-OS through AIRI - a self-hosted cyber companion.
 Features include:
  * Realtime voice chat with TTS/STT
  * Game playing (Minecraft, Factorio)
  * Live2D and MMD visualization
  * WebXR virtual/augmented reality
  * Cross-platform support (web, desktop, mobile)
  * Web-first architecture with WebGPU acceleration
  * Comprehensive AI tooling ecosystem
```

## Benefits to AGI-OS

### 1. User-Facing Interface
Provides the primary interface for human-AGI interaction through natural conversation, voice, and visual embodiment.

### 2. Personality & Embodiment
Gives the AGI system a relatable personality and visual presence, making it more approachable and engaging.

### 3. Multimodal Interaction
Enables voice, text, visual, and spatial interaction modes for comprehensive communication.

### 4. Real-World Grounding
Game playing and computer vision provide grounding in simulated environments, enabling practical skill development.

### 5. Cross-Platform Accessibility
Web-first architecture ensures the AGI is accessible anywhere, on any device, at any time.

### 6. Self-Hosted Ownership
Users maintain complete control and ownership of their digital companion, ensuring privacy and autonomy.

## Integration Roadmap

### Phase 1: Basic Integration (Current)
- [x] Clone Moeru AI into personification folder
- [x] Remove .git directory
- [x] Commit and push to repository
- [ ] Update root CMakeLists.txt for Node.js/pnpm
- [ ] Create build script for AIRI

### Phase 2: Cognitive-Grip Integration
- [ ] Create personification bridge in Cognitive-Grip
- [ ] Connect AIRI to consciousness layer
- [ ] Enable personality state in AtomSpace
- [ ] Integrate voice chat with GGML inference

### Phase 3: Deep Integration
- [ ] Connect game playing to CogPilot physics
- [ ] Integrate conversation with PLN reasoning
- [ ] Enable Live2D expression from emotional state
- [ ] Implement WebXR presence in virtual environments

### Phase 4: Advanced Features
- [ ] Multi-character support
- [ ] Distributed presence across devices
- [ ] Advanced game playing with reinforcement learning
- [ ] Full embodied AGI with personality and presence

## Testing

### Basic Functionality Test

```bash
cd /path/to/agi-os/personification/airi

# Install dependencies
pnpm install

# Run development server
pnpm dev

# Open browser to http://localhost:5173
# Test voice chat, character visualization, etc.
```

### Integration Test

```bash
# Test with consciousness layer
curl -X POST http://localhost:5000/api/personality \
  -H "Content-Type: application/json" \
  -d '{"query": "What is your personality?"}'

# Expected: Response with personality traits and emotional state
```

## Conclusion

The personification layer (Moeru AI / AIRI) represents the **highest user-facing layer** in AGI-OS, providing embodiment, personality, and natural interaction capabilities. This completes the cognitive stack from microkernel to user interface, enabling AGI-OS to function as a complete cyber being with internal cognition, self-awareness, computational intelligence, and external personality.

The integration of AIRI's voice chat, game playing, Live2D visualization, and web-first architecture creates a comprehensive platform for human-AGI interaction, making the system accessible, relatable, and practical for everyday use.

---

**Status**: ✅ INTEGRATED  
**Commit**: a7b3fbc39  
**Layer**: 8 (Personification)  
**Next Phase**: Build system integration and Cognitive-Grip connection  
**Last Updated**: December 13, 2025
