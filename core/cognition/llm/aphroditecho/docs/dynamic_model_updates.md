# Dynamic Model Updates Documentation

This document describes the Dynamic Model Updates functionality implemented for Task 4.1.2 of the Deep Tree Echo roadmap.

## Overview

The Dynamic Model Updates system provides:

- **Online Model Parameter Updates**: Modify model parameters without service interruption
- **Incremental Learning Capabilities**: Adaptive learning with performance feedback
- **Model Versioning and Rollback**: Safe deployment with automatic recovery
- **DTESN Integration**: Enhanced learning using Echo.Kern cognitive algorithms

## Architecture

### Core Components

1. **DynamicModelManager** (`aphrodite/dynamic_model_manager.py`)
   - Manages model versions and parameter updates
   - Handles checkpoints and rollback mechanisms
   - Monitors performance and triggers automatic recovery

2. **OpenAI Serving Interface** (`aphrodite/endpoints/openai/serving_dynamic_updates.py`)
   - Provides REST API endpoints for dynamic updates
   - Integrates with existing Aphrodite serving infrastructure
   - Maintains OpenAI-compatible request/response format

3. **DTESN Integration Bridge** (`aphrodite/dtesn_integration.py`)
   - Connects with Echo.Kern adaptive learning system
   - Implements cognitive learning algorithms (Hebbian, STDP, BCM, Reinforcement)
   - Provides enhanced parameter updates with intelligent algorithm selection

## API Endpoints

All endpoints are available when `APHRODITE_ALLOW_RUNTIME_LORA_UPDATING` is enabled.

### POST `/v1/models/incremental_update`

Apply incremental parameter updates to the model.

**Request Body:**
```json
{
  "parameter_name": "layer1.weight",
  "update_data": [0.01, 0.02, 0.03],
  "learning_rate": 0.01,
  "update_type": "additive",
  "metadata": {"context": "performance_improvement"}
}
```

**Response:**
```json
{
  "success": true,
  "message": "Successfully applied incremental update to layer1.weight",
  "data": {
    "update_id": "update_1_1756835128",
    "update_count": 5,
    "pre_metrics": {"accuracy": 0.85, "latency_ms": 100.0},
    "post_metrics": {"accuracy": 0.87, "latency_ms": 95.0}
  }
}
```

### POST `/v1/models/create_version`

Create a new model version checkpoint.

**Request Body:**
```json
{
  "description": "Checkpoint after training batch 100"
}
```

**Response:**
```json
{
  "success": true,
  "message": "Successfully created model version: v5_1756835128",
  "data": {
    "version_id": "v5_1756835128"
  }
}
```

### POST `/v1/models/rollback`

Rollback to a specific model version.

**Request Body:**
```json
{
  "version_id": "v3_1756835100"
}
```

**Response:**
```json
{
  "success": true,
  "message": "Successfully rolled back to version: v3_1756835100",
  "data": {
    "rolled_back_to": "v3_1756835100",
    "timestamp": 1756835100.0,
    "description": "Stable checkpoint"
  }
}
```

### GET `/v1/models/versions`

List all available model versions.

**Response:**
```json
{
  "versions": [
    {
      "version_id": "v5_1756835128",
      "timestamp": 1756835128.0,
      "description": "Latest checkpoint",
      "is_active": true,
      "performance_metrics": {"accuracy": 0.89, "latency_ms": 90.0}
    },
    {
      "version_id": "v4_1756835100",
      "timestamp": 1756835100.0,
      "description": "Previous stable",
      "is_active": false,
      "performance_metrics": {"accuracy": 0.87, "latency_ms": 95.0}
    }
  ],
  "total_count": 2
}
```

### GET `/v1/models/status`

Get current status of dynamic model updates.

**Response:**
```json
{
  "current_version": "v5_1756835128",
  "total_versions": 5,
  "total_updates": 25,
  "config": {
    "max_versions": 10,
    "checkpoint_interval": 10,
    "auto_rollback_threshold": 0.1,
    "incremental_learning_enabled": true,
    "versioning_enabled": true
  },
  "recent_performance": [
    {"accuracy": 0.89, "latency_ms": 90.0, "timestamp": 1756835128.0},
    {"accuracy": 0.87, "latency_ms": 95.0, "timestamp": 1756835120.0}
  ]
}
```

## Configuration

### DynamicUpdateConfig

Configure dynamic model update behavior:

```python
from aphrodite.dynamic_model_manager import DynamicUpdateConfig

config = DynamicUpdateConfig(
    max_versions=10,                    # Maximum versions to keep
    checkpoint_interval=100,            # Updates between auto-checkpoints
    auto_rollback_threshold=0.1,        # Performance degradation threshold
    enable_incremental_learning=True,   # Enable adaptive learning
    enable_versioning=True,             # Enable version management
    backup_dir="/tmp/model_checkpoints" # Checkpoint storage directory
)
```

### DTESNLearningConfig

Configure DTESN cognitive learning integration:

```python
from aphrodite.dtesn_integration import DTESNLearningConfig

dtesn_config = DTESNLearningConfig(
    learning_rate=0.01,           # Base learning rate
    adaptation_rate=0.001,        # Adaptation speed
    max_iterations=100,           # Maximum learning iterations
    convergence_threshold=1e-4,   # Convergence criteria
    enable_plasticity=True,       # Enable synaptic plasticity
    enable_homeostasis=True,      # Enable homeostatic regulation
    batch_size=32,               # Learning batch size
    reservoir_size=1000          # DTESN reservoir size
)
```

## Usage Examples

### Basic Parameter Update

```python
import asyncio
from aphrodite.dynamic_model_manager import DynamicModelManager, IncrementalUpdateRequest
import torch

# Initialize manager
manager = DynamicModelManager(engine_client, model_config)

# Create initial version
version_id = await manager.create_initial_version("Initial model")

# Apply parameter update
request = IncrementalUpdateRequest(
    parameter_name="transformer.layer1.attention.weight",
    update_data=torch.randn(768, 768) * 0.01,
    learning_rate=0.01,
    update_type="additive"
)

result = await manager.apply_incremental_update(request)
print(f"Update successful: {result['success']}")
```

### Enhanced Learning with DTESN

```python
from aphrodite.dtesn_integration import DTESNDynamicIntegration

# Initialize DTESN integration
dtesn_integration = DTESNDynamicIntegration(
    dynamic_manager=manager,
    dtesn_config=dtesn_config
)

# Apply enhanced update with performance context
result = await dtesn_integration.enhanced_incremental_update(
    parameter_name="transformer.layer2.mlp.weight",
    update_data=torch.randn(768, 3072) * 0.01,
    performance_context={
        "accuracy_change": 0.05,    # 5% improvement
        "latency_change": -10       # 10ms reduction
    }
)
```

### Version Management

```python
# Create checkpoint
checkpoint_id = await manager.create_version("Post-training checkpoint")

# List all versions
versions = manager.list_versions()
for version in versions:
    print(f"{version['version_id']}: {version['description']}")

# Rollback if needed
result = await manager.rollback_to_version(checkpoint_id)
if result["success"]:
    print(f"Rolled back to: {result['rolled_back_to']}")
```

## Performance Monitoring

The system continuously monitors model performance and can automatically rollback degraded updates:

- **Accuracy Degradation**: Monitors accuracy changes
- **Latency Increase**: Tracks inference latency
- **Throughput Reduction**: Monitors request throughput
- **Memory Usage**: Tracks memory consumption

Automatic rollback triggers when performance degrades beyond the configured threshold.

## DTESN Learning Algorithms

The system adaptively selects learning algorithms based on performance feedback:

1. **Hebbian Learning** (High Performance): Strengthens connections that fire together
2. **STDP** (Moderate Performance): Timing-dependent synaptic plasticity
3. **BCM Rule** (Poor Performance): Sliding threshold for modification
4. **Reinforcement Learning** (Very Poor Performance): Reward-modulated updates

## Safety Features

1. **Automatic Rollback**: Reverts changes on performance degradation
2. **Version Limits**: Prevents excessive version accumulation
3. **Backup Storage**: Persistent checkpoint storage
4. **Error Recovery**: Graceful handling of update failures
5. **Performance Monitoring**: Continuous health checks

## Integration Points

### Existing LoRA Infrastructure

The system builds on Aphrodite's proven LoRA adapter infrastructure:
- Leverages existing hot-swapping capabilities
- Maintains compatibility with current model loading
- Uses established error handling patterns

### Echo.Kern DTESN System

Integration with the Echo.Kern cognitive computing system:
- Accesses DTESN adaptive learning algorithms
- Utilizes Echo State Network reservoirs
- Implements cognitive feedback loops

### OpenAI API Compatibility

Maintains compatibility with OpenAI API standards:
- Consistent request/response formats
- Standard error handling
- Authentication integration

## Testing

Comprehensive test suite covers:

- **Unit Tests**: Individual component functionality
- **Integration Tests**: End-to-end workflows
- **Performance Tests**: Rollback and recovery scenarios
- **API Tests**: REST endpoint validation
- **DTESN Tests**: Cognitive learning integration

Run tests with:
```bash
pytest tests/test_dynamic_model_updates.py -v
```

## Troubleshooting

### Common Issues

1. **Update Fails**: Check parameter name validity and data format
2. **Rollback Triggers**: Review performance metrics and threshold settings
3. **Version Limits**: Adjust `max_versions` configuration
4. **DTESN Unavailable**: Verify Echo.Kern library installation

### Debug Information

Enable detailed logging:
```python
import logging
logging.getLogger('aphrodite.dynamic_model_manager').setLevel(logging.DEBUG)
```

### Monitoring

Check system status:
```bash
curl -X GET http://localhost:2242/v1/models/status
```

## Performance Considerations

- **Memory Usage**: Versions consume additional memory
- **Update Latency**: Parameter updates add minimal overhead
- **Checkpoint Frequency**: Balance safety vs. performance
- **DTESN Processing**: Cognitive algorithms add computation cost

## Future Enhancements

Planned improvements include:

1. **Distributed Updates**: Multi-node parameter synchronization
2. **Advanced Algorithms**: Additional DTESN learning rules
3. **Compression**: Checkpoint size optimization
4. **Analytics**: Enhanced performance visualization
5. **Integration**: Extended ecosystem compatibility