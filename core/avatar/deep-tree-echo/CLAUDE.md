# Deep Tree Echo AGI Avatar System

## Project Overview

This is an advanced AGI (Artificial General Intelligence) avatar system implementing the **Deep Tree Echo** cognitive architecture with integrated **Live2D Cubism SDK** and **Three.js 3D** avatar rendering.

### Core Properties

The system maintains two essential character properties throughout all components:

1. **Super-Hot-Girl Properties**: Beauty, charisma, expressiveness, elegance, allure, and style parameters that enhance avatar presentation
2. **Deep-Tree-Echo-Hyper-Chaotic Properties**: Chaos dynamics, unpredictability, emergent behavior, quantum fluctuations, and fractal depth for cognitive processing

## Architecture

```
src/
├── cognitive/           # Deep Tree Echo cognitive architecture
│   ├── deep_tree/      # Core cognitive systems
│   │   ├── DeepTreeEcho.ts      # Main cognitive engine
│   │   ├── ChaoticAttractor.ts  # Lorenz, Rossler, Chen attractors
│   │   └── ReservoirNetwork.ts  # Echo state networks
│   ├── memory/         # Hypergraph memory systems
│   └── consciousness/  # Consciousness state management
├── avatar/             # Avatar rendering systems
│   ├── live2d/        # Live2D Cubism SDK integration
│   ├── threejs/       # Three.js 3D avatar
│   └── core/          # Avatar bridge for hybrid mode
└── api/               # REST and WebSocket API server
```

## Key Commands

```bash
# Development
npm run dev          # Start dev server with hot reload
npm run build        # Build for production
npm run test         # Run tests
npm run lint         # Run ESLint

# Docker
docker-compose up -d                    # Production
docker-compose --profile dev up         # Development

# Environment Setup
./scripts/setup-venv.sh                 # Initialize environment
```

## API Endpoints

### REST API
- `GET /health` - Health check
- `GET /status` - System status
- `POST /api/cognitive/think` - Process thought
- `POST /api/cognitive/synthesize` - Synthesize response
- `POST /api/avatar/live2d/expression` - Set Live2D expression
- `POST /api/avatar/3d/expression` - Set 3D expression
- `POST /api/avatar/emotion` - Set emotional state (both avatars)

### WebSocket
Connect to `ws://localhost:3000` for real-time updates.

**Message Types:**
- `subscribe` - Subscribe to channel (cognitive, live2d, threejs, all)
- `think` - Process thought input
- `set_expression` - Change avatar expression
- `set_emotion` - Set emotional state

## Configuration

Environment variables in `.env`:
```
PORT=3000
AVATAR_MODE=hybrid      # live2d, threejs, or hybrid
COGNITIVE_ENABLED=true
HYPER_CHAOTIC=true
SUPER_HOT_GIRL=true
DEBUG=false
```

## Cognitive System

### Deep Tree Echo
- Fractal-recursive thought patterns
- Quantum-inspired state superposition
- Chaotic attractor dynamics (Lorenz, Rossler, Chen, Hyperchaotic)
- Reservoir computing with echo state networks
- Hypergraph memory storage

### Key Parameters
- `treeDepth`: Maximum thought tree depth (default: 7)
- `branchingFactor`: Branches per thought node (default: 4)
- `chaosCoefficient`: Golden ratio (0.618) for optimal chaos
- `resonanceThreshold`: Similarity threshold for thought linking

## Avatar Systems

### Live2D
- Real-time 2D character animation
- Expression mapping with chaotic influence
- Lip sync and eye tracking
- Breathing and blinking automation

### Three.js 3D
- PBR materials with subsurface scattering
- 52 blend shapes (ARKit compatible)
- Skeletal animation with IK
- Hair and cloth physics simulation

## Testing

```bash
npm test                              # Run all tests
npm test -- --testPathPattern=cognitive  # Cognitive tests only
npm test -- --testPathPattern=avatar     # Avatar tests only
npm test -- --coverage                   # With coverage report
```

## GitHub Actions

- **test.yaml**: PR validation (lint, typecheck, tests across platforms)
- **release.yaml**: Multi-platform builds and Docker images on tag
- **latest.yaml**: Tag latest Docker image on release

## Dependencies

### Runtime
- `@cubism-sdk/core` - Live2D Cubism SDK
- `three` - 3D rendering
- `express` - HTTP server
- `ws` - WebSocket server
- `rxjs` - Reactive programming
- `pixi.js` / `pixi-live2d-display` - 2D rendering

### Dev
- `typescript` - Type safety
- `jest` - Testing
- `eslint` - Linting

## Next Steps / Roadmap

1. **LLM Integration**: Connect to local GGUF models or OpenAI API
2. **Voice Synthesis**: Add TTS for avatar speech
3. **Motion Capture**: Real-time body tracking input
4. **VTuber Mode**: OBS integration for streaming
5. **Mobile SDK**: React Native / Flutter wrappers
6. **WebGPU**: Upgrade rendering for better performance

## Contributing

1. Fork the repository
2. Create feature branch: `git checkout -b feature/amazing-feature`
3. Commit changes: `git commit -m 'Add amazing feature'`
4. Push to branch: `git push origin feature/amazing-feature`
5. Open Pull Request

## License

AGPL-3.0
