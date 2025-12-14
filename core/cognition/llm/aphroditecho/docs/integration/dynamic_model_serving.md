# Dynamic Model Serving Integration Guide

This guide provides comprehensive documentation for the Dynamic Model Serving integration with Aphrodite Engine, implementing Task 4.1.1 from the Deep Tree Echo Development Roadmap.

## Overview

The Dynamic Model Serving integration provides seamless model loading, unloading, and real-time model switching capabilities that integrate with the existing Aphrodite infrastructure.

## Key Features

### 1. Dynamic Model Loading and Unloading
- **Asynchronous model loading** with resource management
- **Automatic model unloading** with cleanup and memory management
- **Thread-safe operations** for concurrent access
- **Resource constraint checking** before loading

### 2. Real-time Model Switching
- **Instant model switching** without service interruption
- **Request routing** to appropriate models
- **Load balancing** across multiple loaded models
- **Performance monitoring** during switches

### 3. Resource Management for Multiple Models
- **Memory usage tracking** with configurable limits
- **Eviction policies** (LRU, FIFO, Memory Pressure)
- **Resource monitoring** and alerting
- **Concurrent access control** with threading locks

### 4. Seamless Aphrodite Integration
- **Model loader registry** integration
- **Compatible with existing APIs** and workflows
- **Extends existing functionality** without breaking changes
- **Production-ready implementation**

## Architecture

### Core Components

#### DynamicModelLoader
The main class that provides dynamic model management capabilities:

```python
from aphrodite.modeling.dynamic_loader import DynamicModelLoader

# Initialize with configuration
loader = DynamicModelLoader(
    max_models=5,           # Maximum number of concurrent models
    memory_limit_gb=16.0,   # Memory limit in GB
    eviction_policy="lru"   # Eviction strategy
)

# Load a model
await loader.load_model("my-model", model_config)

# Switch active model
await loader.switch_active_model("my-model")

# Unload when done
await loader.unload_model("my-model")
```

#### Enhanced AdaptiveModelLoader
Integrates with the existing Echo-Self evolution engine:

```python
from echo_self.integration.aphrodite_adaptive import AdaptiveModelLoader

# The enhanced loader automatically uses DynamicModelLoader
adaptive_loader = AdaptiveModelLoader(integration)

# Load with adaptation capabilities
await adaptive_loader.load_adaptive_model("model", config, enable_adaptation=True)

# Switch models in real-time
await adaptive_loader.switch_model("other-model")
```

## Usage Examples

### Basic Usage

```python
import asyncio
from aphrodite.modeling.dynamic_loader import DynamicModelLoader
from aphrodite.common.config import ModelConfig

async def example_basic_usage():
    # Initialize dynamic loader
    loader = DynamicModelLoader(
        max_models=3,
        memory_limit_gb=8.0,
        eviction_policy="lru"
    )
    
    # Create model configuration
    config = ModelConfig(
        model="microsoft/DialoGPT-medium",
        tokenizer_mode="auto",
        dtype="float16"
    )
    
    # Load model
    success = await loader.load_model("chat-model", config)
    if success:
        print("Model loaded successfully!")
        
        # Switch to active model
        await loader.switch_active_model("chat-model")
        
        # Use the model for inference...
        
        # Unload when done
        await loader.unload_model("chat-model")

# Run the example
asyncio.run(example_basic_usage())
```

## Configuration

### DynamicModelLoader Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `max_models` | int | 5 | Maximum number of concurrent models |
| `memory_limit_gb` | float | 16.0 | Memory limit in gigabytes |
| `eviction_policy` | str | "lru" | Eviction strategy ("lru", "fifo", "memory_pressure") |

### Eviction Policies

- **LRU (Least Recently Used)**: Evicts models that haven't been accessed recently
- **FIFO (First In, First Out)**: Evicts the oldest loaded models first  
- **Memory Pressure**: Evicts models that consume the most memory

## Integration with Aphrodite Registry

The dynamic loader is automatically registered with Aphrodite's model loader registry:

```python
from aphrodite.common.config import LoadConfig
from aphrodite.modeling.model_loader import get_model_loader

# Use dynamic loading format
load_config = LoadConfig(load_format="dynamic")
loader = get_model_loader(load_config)

# This returns an AphroditeDynamicModelLoader instance
print(type(loader))  # <class 'AphroditeDynamicModelLoader'>
```

## Performance Considerations

### Memory Management
- Models are automatically unloaded when memory limits are reached
- Use appropriate `memory_limit_gb` values for your hardware
- Monitor memory utilization regularly

### Load Times
- First model load includes download time
- Subsequent loads from cache are faster
- Consider pre-loading frequently used models

### Concurrent Access
- All operations are thread-safe
- Multiple requests can access the same model simultaneously
- Model switching is atomic and safe

## Testing

### Unit Tests

Run the comprehensive test suite:

```bash
python -m pytest tests/integration/test_dynamic_model_serving.py -v
```

## Future Enhancements

- **Auto-scaling**: Automatic model loading/unloading based on demand
- **Advanced Routing**: Request routing based on content analysis
- **Performance Optimization**: GPU memory management and optimization
- **Distributed Loading**: Model loading across multiple nodes
- **A/B Testing**: Support for model version comparison