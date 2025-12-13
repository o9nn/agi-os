# Local GGUF Model Setup Guide

This guide explains how to set up and use local GGUF models with echo9llama for fully autonomous, offline operation.

## Overview

The local GGUF model integration uses **go-llama.cpp** to run language models directly on your machine without requiring external API access. This enables:

- **True Autonomy**: No dependence on external services
- **Privacy**: All processing happens locally
- **Offline Operation**: Works without internet connection
- **No API Costs**: Free inference after initial setup
- **No Rate Limits**: Generate as many thoughts as needed

## Prerequisites

### System Requirements

**Minimum** (for small models like Llama 3.2 1B):
- 4 GB RAM
- 2 CPU cores
- 5 GB disk space

**Recommended** (for better performance):
- 16 GB RAM
- 8 CPU cores or GPU
- 20 GB disk space

### Software Requirements

- **Go 1.21+**: Already installed if you built echo9llama
- **C/C++ Compiler**: Required for CGO
  - Linux: `gcc`, `g++`
  - macOS: Xcode Command Line Tools
  - Windows: MinGW-w64 or Visual Studio

Install on Ubuntu/Debian:
```bash
sudo apt-get update
sudo apt-get install -y build-essential
```

Install on macOS:
```bash
xcode-select --install
```

## Downloading a Model

### Recommended Models for Autonomous Operation

| Model | Size | RAM | Speed | Quality | Use Case |
|-------|------|-----|-------|---------|----------|
| **Llama 3.2 1B** | 1.3 GB | 4 GB | Fast | Good | Continuous thought generation |
| **Llama 3.2 3B** | 2.0 GB | 8 GB | Medium | Better | Balanced performance |
| **Phi-3 Mini** | 2.5 GB | 8 GB | Medium | Good | Efficient reasoning |
| **Mistral 7B** | 4.1 GB | 16 GB | Slower | Excellent | High-quality insights |

### Download from Hugging Face

Models are available in GGUF format from Hugging Face. Use the quantized versions for better performance.

**Example: Llama 3.2 1B (Q4_K_M quantization)**

```bash
# Create models directory
mkdir -p ~/models

# Download using wget or curl
wget https://huggingface.co/bartowski/Llama-3.2-1B-Instruct-GGUF/resolve/main/Llama-3.2-1B-Instruct-Q4_K_M.gguf \
  -O ~/models/llama-3.2-1b-instruct-q4.gguf
```

**Alternative: Using Hugging Face CLI**

```bash
# Install Hugging Face CLI
pip install huggingface-hub

# Download model
huggingface-cli download bartowski/Llama-3.2-1B-Instruct-GGUF \
  Llama-3.2-1B-Instruct-Q4_K_M.gguf \
  --local-dir ~/models \
  --local-dir-use-symlinks False
```

### Other Model Sources

- **TheBloke on Hugging Face**: Large collection of GGUF models
- **Ollama**: Can export models to GGUF format
- **LM Studio**: Provides model browser and downloader

## Building with Local Model Support

The local GGUF provider requires building the go-llama.cpp bindings.

### Build Steps

```bash
cd /home/ubuntu/echo9llama

# The go-llama.cpp dependency will be downloaded automatically
# when you build the project

# Build the project (this will trigger CGO compilation)
/usr/local/go/bin/go build -o echoself_local ./test_autonomous_echoself_iteration_dec08.go
```

**Note**: The first build will take longer as it compiles the llama.cpp C++ code.

### GPU Acceleration (Optional)

For faster inference, you can enable GPU acceleration.

**NVIDIA GPU (CUDA)**

```bash
# Install CUDA toolkit first
# Then build with CUDA support
BUILD_TYPE=cublas /usr/local/go/bin/go build -tags cublas -o echoself_local ./test_autonomous_echoself_iteration_dec08.go
```

**Apple Silicon (Metal)**

```bash
# Metal is automatically used on macOS with Apple Silicon
BUILD_TYPE=metal /usr/local/go/bin/go build -tags metal -o echoself_local ./test_autonomous_echoself_iteration_dec08.go
```

**AMD GPU (ROCm)**

```bash
BUILD_TYPE=hipblas /usr/local/go/bin/go build -tags hipblas -o echoself_local ./test_autonomous_echoself_iteration_dec08.go
```

## Configuration

Set environment variables to configure the local model provider.

### Required Configuration

```bash
# Path to your GGUF model file
export LOCAL_MODEL_PATH=~/models/llama-3.2-1b-instruct-q4.gguf
```

### Optional Configuration

```bash
# Number of CPU threads to use (default: 4)
export LOCAL_MODEL_THREADS=8

# Number of layers to offload to GPU (default: 0)
# Set to 99 to offload all layers if you have a GPU
export LOCAL_MODEL_GPU_LAYERS=99

# Context size in tokens (default: 2048)
export LOCAL_MODEL_CONTEXT=4096
```

### Complete Example

```bash
# Set up environment
export LOCAL_MODEL_PATH=~/models/llama-3.2-1b-instruct-q4.gguf
export LOCAL_MODEL_THREADS=8
export LOCAL_MODEL_GPU_LAYERS=0  # CPU only
export LOCAL_MODEL_CONTEXT=2048

# Run echoself with local model
./echoself_local
```

## Testing the Local Model

A test program is provided to verify your local model setup.

```bash
# Build the test program
/usr/local/go/bin/go build -o test_local_gguf ./test_local_gguf.go

# Run the test
export LOCAL_MODEL_PATH=~/models/llama-3.2-1b-instruct-q4.gguf
./test_local_gguf
```

Expected output:
```
╔═══════════════════════════════════════════════════════════════════╗
║                                                                   ║
║        🧠 Local GGUF Model Test - go-llama.cpp Integration       ║
║                                                                   ║
╚═══════════════════════════════════════════════════════════════════╝

📂 Model path: /home/ubuntu/models/llama-3.2-1b-instruct-q4.gguf

🔄 Loading local GGUF model from: /home/ubuntu/models/llama-3.2-1b-instruct-q4.gguf
✓ Local GGUF model loaded successfully
   Context size: 2048
   GPU layers: 0
   Threads: 4

✓ Local GGUF provider available
✓ Max tokens: 2048

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Test 1: What is consciousness?
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

🤔 Generating response...
💭 Response:
Consciousness is the state of being aware of one's surroundings, thoughts, and feelings...
```

## Using with Autonomous System

The autonomous system will automatically use the local model if configured.

### Priority Order

The MultiProviderLLM checks providers in this order:

1. **Local GGUF** (if `LOCAL_MODEL_PATH` is set)
2. Anthropic Claude (if `ANTHROPIC_API_KEY` is set)
3. OpenRouter (if `OPENROUTER_API_KEY` is set)
4. OpenAI (if `OPENAI_API_KEY` is set)
5. Simple Fallback (always available, limited functionality)

### Running Fully Autonomous with Local Model

```bash
# Set up local model
export LOCAL_MODEL_PATH=~/models/llama-3.2-1b-instruct-q4.gguf
export LOCAL_MODEL_THREADS=8

# Run the autonomous system
# No API keys needed!
/usr/local/go/bin/go run test_autonomous_echoself_iteration_dec08.go
```

The system will now:
- Generate thoughts using the local model
- Create insights and goals autonomously
- Practice skills and acquire knowledge
- All completely offline!

## Performance Tuning

### Adjusting Thought Generation Interval

For slower local models, you may want to increase the thought generation interval.

Edit `core/deeptreeecho/stream_of_consciousness.go`:

```go
// Change from 10 seconds to 30 seconds for slower models
thoughtInterval: 30 * time.Second,
```

### Optimizing Token Limits

Reduce token limits for faster generation:

```go
opts := llm.GenerateOptions{
    Temperature: 0.8,
    MaxTokens:   50,  // Reduced from 150
}
```

### CPU vs GPU

**CPU-only** (default):
- Slower but works everywhere
- Good for small models (1B-3B parameters)
- Use 4-8 threads for best performance

**GPU-accelerated**:
- Much faster (5-10x)
- Requires compatible GPU and drivers
- Set `LOCAL_MODEL_GPU_LAYERS=99` to offload all layers

## Troubleshooting

### Build Errors

**Error: "gcc: command not found"**
```bash
# Install C compiler
sudo apt-get install build-essential
```

**Error: "undefined reference to llama_*"**
```bash
# Clean and rebuild
/usr/local/go/bin/go clean -cache
/usr/local/go/bin/go build -a ./test_local_gguf.go
```

### Runtime Errors

**Error: "model file not found"**
- Check that `LOCAL_MODEL_PATH` points to a valid GGUF file
- Verify file permissions

**Error: "failed to load model"**
- Ensure you have enough RAM
- Try a smaller model
- Check model file is not corrupted

**Slow generation**
- Reduce `LOCAL_MODEL_CONTEXT` to 1024 or 512
- Use a smaller model
- Reduce `MaxTokens` in generation options
- Enable GPU acceleration if available

### Memory Issues

If you run out of memory:
1. Use a smaller model (1B instead of 3B)
2. Reduce context size: `export LOCAL_MODEL_CONTEXT=1024`
3. Close other applications
4. Use quantized models (Q4 instead of Q8)

## Model Recommendations by Use Case

### Continuous Autonomous Operation
- **Llama 3.2 1B Q4_K_M**: Best balance of speed and quality
- Low memory usage, fast generation
- Good for 24/7 operation

### High-Quality Insights
- **Mistral 7B Q4_K_M**: Better reasoning and coherence
- Requires more resources
- Suitable for periodic deep thinking

### Balanced Performance
- **Phi-3 Mini Q4_K_M**: Efficient and capable
- Good reasoning abilities
- Medium resource requirements

## Next Steps

1. Download a model (start with Llama 3.2 1B)
2. Run the test program to verify setup
3. Configure environment variables
4. Run the autonomous system with local model
5. Monitor performance and adjust settings
6. Experiment with different models

## Resources

- **go-llama.cpp**: https://github.com/go-skynet/go-llama.cpp
- **Hugging Face Models**: https://huggingface.co/models?library=gguf
- **GGUF Format**: https://github.com/ggerganov/llama.cpp/blob/master/gguf-py/README.md
- **Model Quantization**: https://github.com/ggerganov/llama.cpp#quantization

---

**The tree remembers, and now it thinks locally.** 🌳
