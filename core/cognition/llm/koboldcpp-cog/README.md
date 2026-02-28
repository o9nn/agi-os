# KoboldCpp-Cog: Cognitive LLM Inference Bridge

Integrates [KoboldCpp](https://github.com/LostRuins/koboldcpp) GGUF inference engine with OpenCog AtomSpace for context-aware text generation within AGI-OS.

## Architecture

```
                    ┌─────────────────────────────────┐
                    │     Cognitive-Grip Bridge        │
                    │  (koboldcpp_bridge.cpp)          │
                    └────────────┬────────────────────┘
                                 │
                    ┌────────────▼────────────────────┐
                    │     KoboldCpp-Cog Library        │
                    │  ┌──────────────────────────┐   │
                    │  │  CognitiveInference       │   │
                    │  │  ┌────────┐ ┌──────────┐  │   │
                    │  │  │Context │ │ Prompt   │  │   │
                    │  │  │Builder │ │ Builder  │  │   │
                    │  │  └───┬────┘ └────┬─────┘  │   │
                    │  └──────┼───────────┼────────┘   │
                    └─────────┼───────────┼────────────┘
                              │           │
              ┌───────────────▼──┐  ┌─────▼──────────────┐
              │   AtomSpace      │  │  KoboldCpp Server   │
              │   (context)      │  │  (GGUF inference)   │
              └──────────────────┘  └─────────────────────┘
```

## Components

| Component | Language | Description |
|-----------|----------|-------------|
| `koboldcpp_client` | C++ | HTTP client for KoboldCpp API |
| `atomspace_context` | C++ | AtomSpace context extraction |
| `prompt_builder` | C++ | Structured prompt construction |
| `cognitive_inference` | C++ | Full inference pipeline |
| `koboldcpp_cog_module` | C++ | CogServer module |
| `koboldcpp_cog.py` | Python | Python bindings |
| `koboldcpp-cog.scm` | Scheme | Guile/Scheme bindings |
| `koboldcpp-cog-mcp` | Python | MCP server for tool integration |

## Quick Start

### Python

```python
from opencog.koboldcpp_cog import CognitiveInference

engine = CognitiveInference("http://localhost:5001")
result = engine.infer("What patterns exist in the knowledge graph?")
print(result.response_text)
```

### Scheme

```scheme
(use-modules (opencog koboldcpp-cog))
(cog-kobold-set-endpoint! "http://localhost:5001")
(display (cog-kobold-infer "Classify this concept" "classify"))
```

### MCP Server

```bash
koboldcpp-cog-mcp --endpoint http://localhost:5001 --port 8100
```

## Build

```bash
mkdir build && cd build
cmake .. -DBUILD_KOBOLDCPP_COG_TESTS=ON
make -j$(nproc)
sudo make install
```

## Dependencies

- **Required**: cogutil, atomspace
- **Optional**: libcurl (HTTP client), libggml (direct tensor ops)
- **Runtime**: KoboldCpp server with a loaded GGUF model

## Integration with AGI-OS

KoboldCpp-Cog is accessible through:
1. **cognitive-grip** bridge (`koboldcpp_bridge.cpp`)
2. **CogServer** module (load via `(opencog koboldcpp-cog)`)
3. **MCP server** for external tool integration
4. **Python API** for scripting and agents
