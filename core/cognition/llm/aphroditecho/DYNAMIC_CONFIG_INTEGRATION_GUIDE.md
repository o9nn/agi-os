# Dynamic Configuration Integration Guide

Quick start guide for integrating dynamic configuration management into your DTESN applications.

## Quick Start

### 1. Enable Dynamic Configuration

```python
from aphrodite.endpoints.deep_tree_echo import (
    DTESNProcessor,
    get_dynamic_config_manager,
    initialize_dynamic_config_manager
)

# Initialize with dynamic configuration enabled
processor = DTESNProcessor(
    config=your_initial_config,
    enable_dynamic_config=True  # Enables hot configuration updates
)
```

### 2. Access Configuration Manager

```python
# Get the global configuration manager
manager = get_dynamic_config_manager()

# Check current configuration
current_config = manager.current_config
print(f"Current ESN reservoir size: {current_config.esn_reservoir_size}")
```

### 3. Update Configuration at Runtime

```python
from aphrodite.endpoints.deep_tree_echo import ConfigurationUpdateRequest

# Update a single parameter
update_request = ConfigurationUpdateRequest(
    parameter_path="esn_reservoir_size",
    new_value=1024,
    description="Increase capacity for high-load processing"
)

result = await manager.update_parameter(update_request)
if result["success"]:
    print(f"✅ Updated {result['parameter']} to {result['new_value']}")
else:
    print(f"❌ Update failed: {result['error']}")
```

## FastAPI Integration

### Add Configuration Routes to Your App

```python
from fastapi import FastAPI
from aphrodite.endpoints.deep_tree_echo import config_router

app = FastAPI()

# Include configuration management endpoints
app.include_router(config_router)

# Your configuration is now available at /v1/config/*
```

### Test Configuration API

```bash
# Get current configuration
curl http://localhost:8000/v1/config/current

# Update a parameter
curl -X POST http://localhost:8000/v1/config/update \
     -H 'Content-Type: application/json' \
     -d '{"parameter": "esn_reservoir_size", "value": 1024}'

# Check configuration health
curl http://localhost:8000/v1/config/health
```

## Configuration Callbacks

### Register for Configuration Updates

```python
# Register callback for configuration changes
async def on_config_update(new_config):
    print(f"Configuration updated!")
    print(f"New ESN reservoir size: {new_config.esn_reservoir_size}")
    
    # Update your application state
    await update_processing_pipeline(new_config)

manager.register_update_callback(on_config_update)
```

## Environment Management

### Set Environment

```python
from aphrodite.endpoints.deep_tree_echo import ConfigurationEnvironment

# Set environment (affects validation and behavior)
manager.set_environment(ConfigurationEnvironment.PRODUCTION)
```

### Environment-Specific Updates

```bash
# Set environment via API
curl -X POST http://localhost:8000/v1/config/environment \
     -H 'Content-Type: application/json' \
     -d '{"environment": "production"}'
```

## Configuration Rollback

### Manual Rollback

```python
# Get available snapshots
snapshots = manager.get_snapshots()
print(f"Available snapshots: {len(snapshots)}")

# Rollback to previous snapshot
if snapshots:
    previous_snapshot = snapshots[-2]["snapshot_id"]
    result = await manager.rollback_to_snapshot(previous_snapshot)
    
    if result["success"]:
        print(f"✅ Rolled back to {previous_snapshot}")
    else:
        print(f"❌ Rollback failed: {result['error']}")
```

### API Rollback

```bash
# Rollback via API
curl -X POST http://localhost:8000/v1/config/rollback \
     -H 'Content-Type: application/json' \
     -d '{"snapshot_id": "snapshot_1699123456_0001"}'
```

## Validation

### Validate Before Applying

```python
# Test configuration without applying changes
validate_request = ConfigurationUpdateRequest(
    parameter_path="max_membrane_depth",
    new_value=16,
    validate_only=True  # Only validate, don't apply
)

result = await manager.update_parameter(validate_request)
if result["success"]:
    print("✅ Validation passed")
else:
    print(f"❌ Validation failed: {result['validation_errors']}")
```

### API Validation

```bash
# Validate configuration via API
curl -X POST http://localhost:8000/v1/config/validate \
     -H 'Content-Type: application/json' \
     -d '{"parameter": "max_membrane_depth", "value": 16}'
```

## Batch Updates

### Atomic Configuration Changes

```python
# Update multiple parameters atomically
batch_updates = [
    ConfigurationUpdateRequest("max_membrane_depth", 8),
    ConfigurationUpdateRequest("esn_reservoir_size", 2048),
    ConfigurationUpdateRequest("cache_ttl_seconds", 600)
]

batch_result = await manager.update_multiple_parameters(batch_updates)
if batch_result["success"]:
    print(f"✅ Updated {len(batch_result['updated_parameters'])} parameters")
    for update in batch_result['updated_parameters']:
        print(f"  - {update['parameter']}: {update['old_value']} → {update['new_value']}")
else:
    print(f"❌ Batch update failed: {batch_result['error']}")
```

## Monitoring & Health Checks

### Check Configuration Status

```python
# Get detailed status
status = manager.get_current_status()
print(f"Environment: {status['environment']}")
print(f"Total snapshots: {status['total_snapshots']}")
print(f"Registered callbacks: {status['registered_callbacks']}")
```

### Health Check

```bash
# Monitor configuration health
curl http://localhost:8000/v1/config/health
```

## Best Practices

### 1. Always Validate First

```python
# Good practice: validate before applying
validate_first = ConfigurationUpdateRequest(
    parameter_path="esn_reservoir_size",
    new_value=4096,
    validate_only=True
)

validation_result = await manager.update_parameter(validate_first)
if validation_result["success"]:
    # Apply the update
    actual_update = ConfigurationUpdateRequest(
        parameter_path="esn_reservoir_size",
        new_value=4096,
        description="Validated update for increased capacity"
    )
    await manager.update_parameter(actual_update)
```

### 2. Use Batch Updates for Related Parameters

```python
# Good: Update related parameters together
related_updates = [
    ConfigurationUpdateRequest("max_membrane_depth", 8),
    ConfigurationUpdateRequest("esn_reservoir_size", 1024),  # Appropriate for depth 8
    ConfigurationUpdateRequest("bseries_max_order", 16)
]
await manager.update_multiple_parameters(related_updates)
```

### 3. Handle Configuration Failures Gracefully

```python
async def safe_config_update(parameter, value):
    """Safely update configuration with error handling."""
    try:
        request = ConfigurationUpdateRequest(
            parameter_path=parameter,
            new_value=value,
            description=f"Safe update of {parameter}"
        )
        
        result = await manager.update_parameter(request)
        
        if result["success"]:
            logger.info(f"Successfully updated {parameter} to {value}")
            return True
        else:
            logger.error(f"Failed to update {parameter}: {result['error']}")
            return False
            
    except Exception as e:
        logger.error(f"Configuration update exception: {e}")
        return False
```

### 4. Monitor Configuration Changes

```python
# Set up logging for configuration changes
import logging

# Enable configuration manager logging
config_logger = logging.getLogger('aphrodite.endpoints.deep_tree_echo.dynamic_config_manager')
config_logger.setLevel(logging.INFO)

# Register monitoring callback
async def config_monitoring_callback(new_config):
    """Monitor and log configuration changes."""
    logger.info(f"Configuration updated: {new_config.dict()}")
    
    # Send metrics to monitoring system
    metrics.gauge('dtesn.esn_reservoir_size', new_config.esn_reservoir_size)
    metrics.gauge('dtesn.max_membrane_depth', new_config.max_membrane_depth)

manager.register_update_callback(config_monitoring_callback)
```

## Troubleshooting

### Common Issues

**Issue**: Configuration updates fail with validation errors
```python
# Solution: Check parameter constraints
schema_response = requests.get('http://localhost:8000/v1/config/schema')
schema = schema_response.json()
print("Parameter constraints:", schema['parameter_info'])
```

**Issue**: Rollback fails
```python
# Solution: Check available snapshots
snapshots = manager.get_snapshots()
print(f"Available snapshots: {[s['snapshot_id'] for s in snapshots]}")
```

**Issue**: Callbacks not working
```python
# Solution: Check callback registration
status = manager.get_current_status()
print(f"Registered callbacks: {status['registered_callbacks']}")
```

### Debug Logging

```python
import logging

# Enable debug logging
logging.basicConfig(level=logging.DEBUG)
config_logger = logging.getLogger('aphrodite.endpoints.deep_tree_echo.dynamic_config_manager')
config_logger.setLevel(logging.DEBUG)
```

## Configuration Parameters Reference

| Parameter | Type | Range | Default | Description |
|-----------|------|-------|---------|-------------|
| `max_membrane_depth` | int | 1-64 | 8 | DTESN membrane hierarchy depth |
| `esn_reservoir_size` | int | 64-16384 | 1024 | Echo State Network reservoir size |
| `bseries_max_order` | int | 1-32 | 16 | B-Series computation maximum order |
| `enable_caching` | bool | true/false | true | Server-side response caching |
| `cache_ttl_seconds` | int | 1-86400 | 300 | Cache time-to-live in seconds |
| `enable_performance_monitoring` | bool | true/false | true | Performance monitoring middleware |

## Next Steps

1. **Read the full documentation**: [dynamic_configuration_management.md](docs/dynamic_configuration_management.md)
2. **Run the demo**: `python demo_dynamic_config_management.py`
3. **Test the API**: Start your server and test configuration endpoints
4. **Set up monitoring**: Implement configuration change monitoring in your application
5. **Configure environments**: Set up proper environments for dev/staging/production

---

**Need Help?** Check the troubleshooting section above or review the comprehensive documentation for advanced usage patterns.