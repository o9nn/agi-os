# Deep Tree Echo FastAPI Endpoints

This module provides server-side rendering (SSR) FastAPI endpoints for Deep Tree Echo System Network (DTESN) processing integrated with the Aphrodite Engine.

## Architecture

The implementation follows the FastAPI application factory pattern with server-side rendering focus:

```
aphrodite/endpoints/deep_tree_echo/
├── __init__.py              # Module exports
├── app_factory.py           # FastAPI application factory
├── config.py                # Configuration management
├── middleware.py            # Request/response middleware
├── routes.py                # API route handlers (ENHANCED)
└── dtesn_processor.py       # DTESN processing integration (ENHANCED)
```

## Features

- ✅ **Server-side rendering** with no client dependencies
- ✅ **FastAPI application factory** for configurable app creation
- ✅ **DTESN processing integration** with echo.kern components
- ✅ **Performance monitoring** middleware
- ✅ **Environment-based configuration** management
- ✅ **Comprehensive testing** with pytest
- ✅ **Production-ready** architecture

### Enhanced Features (Phase 5.1.2)

- ✅ **Advanced route handlers** with batch processing and streaming
- ✅ **Engine integration** with Aphrodite Engine data fetching
- ✅ **Enhanced response serialization** with performance metrics
- ✅ **Comprehensive monitoring** endpoints for server-side metrics
- ✅ **Real-time streaming** via Server-Sent Events
- ✅ **Input validation** and output sanitization

## Quick Start

```python
from aphrodite.endpoints.deep_tree_echo import create_app
from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig

# Create configuration
config = DTESNConfig(
    max_membrane_depth=6,
    esn_reservoir_size=1024,
    enable_caching=True
)

# Create FastAPI app
app = create_app(config=config)

# Run with uvicorn
import uvicorn
uvicorn.run(app, host="0.0.0.0", port=8000)
```

## API Endpoints

### Core Processing Endpoints

| Endpoint | Method | Description | Features |
|----------|--------|-------------|----------|
| `/health` | GET | Health check endpoint | Basic service status |
| `/deep_tree_echo/` | GET | Service information | Enhanced with engine integration |

### DTESN Processing Endpoints

| Endpoint | Method | Description | Features |
|----------|--------|-------------|----------|
| `/deep_tree_echo/process` | POST | Process input through DTESN | Enhanced with performance metrics |
| `/deep_tree_echo/batch_process` | POST | Batch process multiple inputs | Parallel/sequential processing |
| `/deep_tree_echo/stream_process` | POST | Real-time streaming processing | Server-Sent Events |

### Information & Monitoring Endpoints  

| Endpoint | Method | Description | Features |
|----------|--------|-------------|----------|
| `/deep_tree_echo/status` | GET | Enhanced system status | Engine integration status |
| `/deep_tree_echo/membrane_info` | GET | P-System membrane info | Enhanced with performance data |
| `/deep_tree_echo/esn_state` | GET | ESN reservoir state | Enhanced with optimization features |
| `/deep_tree_echo/engine_integration` | GET | Engine integration details | Comprehensive integration status |
| `/deep_tree_echo/performance_metrics` | GET | Performance monitoring | Server-side metrics |

### Request/Response Models

#### Enhanced DTESNRequest
```python
{
    "input_data": "string",
    "membrane_depth": 4,        # Range: 1-16
    "esn_size": 512,           # Range: 32-4096  
    "processing_mode": "server_side",  # server_side|streaming|batch
    "include_intermediate": false,
    "output_format": "json"    # json|compressed|streaming
}
```

#### Batch Processing Request
```python
{
    "inputs": ["string1", "string2", ...],  # Max 100 items
    "membrane_depth": 4,
    "esn_size": 512,
    "parallel_processing": true,
    "max_batch_size": 10
}
```

#### Enhanced Response Format
```python
{
    "status": "success",
    "result": { ... },
    "processing_time_ms": 123.45,
    "membrane_layers": 4,
    "server_rendered": true,
    "engine_integration": {
        "engine_available": true,
        "model_config": { ... },
        "integration_capabilities": { ... }
    },
    "performance_metrics": {
        "total_processing_time_ms": 125.67,
        "dtesn_processing_time_ms": 123.45,
        "overhead_ms": 2.22,
        "throughput_chars_per_second": 156.78
    }
}
```

## Configuration

Configuration can be set via environment variables:

```bash
export DTESN_MAX_MEMBRANE_DEPTH=6
export DTESN_ESN_RESERVOIR_SIZE=1024
export DTESN_ENABLE_CACHING=true
export DTESN_ENABLE_PERFORMANCE_MONITORING=true
export DTESN_CACHE_TTL_SECONDS=300
```

## Integration with echo.kern

The implementation integrates with echo.kern DTESN components when available:

- **P-System Membranes**: Hierarchical membrane computing with engine enhancements
- **Echo State Networks**: Reservoir computing with temporal dynamics and engine integration
- **B-Series Computation**: Rooted tree enumeration and differential computation
- **OEIS A000081**: Mathematical foundation compliance with advanced features

### Enhanced Integration Features

- **Engine Context Fetching**: Retrieves data from Aphrodite Engine components
- **Model Configuration Integration**: Incorporates model settings into processing
- **Advanced Error Handling**: Comprehensive error recovery and reporting
- **Performance Optimization**: Server-side caching and optimization strategies

## Testing

### Running Enhanced Tests

```bash
# Run all tests
pytest tests/endpoints/test_deep_tree_echo.py -v

# Run specific test categories
pytest tests/endpoints/test_deep_tree_echo.py::TestDeepTreeEchoEndpoints::test_batch_process_endpoint -v
pytest tests/endpoints/test_deep_tree_echo.py::TestDeepTreeEchoEndpoints::test_stream_process_endpoint -v
pytest tests/endpoints/test_deep_tree_echo.py::TestDeepTreeEchoEndpoints::test_engine_integration_endpoint -v
```

### Test Coverage

The enhanced test suite covers:
- ✅ All original functionality
- ✅ Enhanced route handlers with engine integration
- ✅ Batch processing capabilities
- ✅ Streaming responses
- ✅ Performance monitoring
- ✅ Input validation
- ✅ Error handling

## Performance

### Server-Side Optimizations

- **Batch Processing**: Efficient parallel/sequential batch processing
- **Streaming Responses**: Real-time updates via Server-Sent Events
- **Caching**: Configurable response caching with TTL
- **Performance Monitoring**: Detailed metrics collection
- **Engine Integration**: Optimized data fetching from Aphrodite Engine

### Performance Metrics

The enhanced implementation provides comprehensive metrics:

```python
{
    "service_metrics": {
        "uptime_seconds": 12345,
        "processing_mode": "server_side",
        "optimization_level": "production"
    },
    "dtesn_performance": {
        "max_membrane_depth": 8,
        "max_esn_size": 1024,
        "estimated_throughput": "varies_by_input_size"
    },
    "server_optimization": {
        "caching_enabled": true,
        "batch_processing": true,
        "streaming_support": true
    }
}
```

## Production Deployment

### Docker Configuration

```dockerfile
# Enhanced configuration for production
ENV DTESN_MAX_MEMBRANE_DEPTH=8
ENV DTESN_ESN_RESERVOIR_SIZE=1024
ENV DTESN_ENABLE_CACHING=true
ENV DTESN_ENABLE_PERFORMANCE_MONITORING=true
ENV DTESN_CACHE_TTL_SECONDS=300

EXPOSE 8000
CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8000"]
```

### Load Balancing

The enhanced endpoints support horizontal scaling:

- **Stateless Design**: All endpoints are stateless for easy scaling
- **Batch Processing**: Efficient resource utilization
- **Caching**: Reduces load through intelligent caching
- **Monitoring**: Built-in performance metrics for load balancer decisions

## Example Usage

### Single Processing
```python
import httpx

response = httpx.post("http://localhost:8000/deep_tree_echo/process", json={
    "input_data": "Process this through DTESN",
    "membrane_depth": 4,
    "esn_size": 512,
    "output_format": "json"
})

result = response.json()
print(f"Processing time: {result['processing_time_ms']}ms")
print(f"Engine integration: {result['engine_integration']}")
```

### Batch Processing
```python
import httpx

response = httpx.post("http://localhost:8000/deep_tree_echo/batch_process", json={
    "inputs": ["input1", "input2", "input3"],
    "parallel_processing": True,
    "max_batch_size": 5
})

result = response.json()
print(f"Processed {result['successful_count']} items successfully")
```

### Streaming Processing
```python
import httpx

with httpx.stream("POST", "http://localhost:8000/deep_tree_echo/stream_process", 
                  json={"input_data": "Stream this"}) as response:
    for line in response.iter_lines():
        if line.startswith("data: "):
            data = json.loads(line[6:])
            print(f"Status: {data.get('status')}")
```

## License

This module is part of the Aphrodite Engine project and follows the same licensing terms.

## Quick Start

```python
from aphrodite.endpoints.deep_tree_echo import create_app
from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig

# Create configuration
config = DTESNConfig(
    max_membrane_depth=6,
    esn_reservoir_size=1024,
    enable_caching=True
)

# Create FastAPI app
app = create_app(config=config)

# Run with uvicorn
import uvicorn
uvicorn.run(app, host="0.0.0.0", port=8000)
```

## API Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/health` | GET | Health check endpoint |
| `/deep_tree_echo/` | GET | Service information |
| `/deep_tree_echo/process` | POST | Process input through DTESN |
| `/deep_tree_echo/status` | GET | System status |
| `/deep_tree_echo/membrane_info` | GET | P-System membrane info |
| `/deep_tree_echo/esn_state` | GET | ESN reservoir state |

## Configuration

Configuration can be set via environment variables:

```bash
export DTESN_MAX_MEMBRANE_DEPTH=6
export DTESN_ESN_RESERVOIR_SIZE=1024
export DTESN_ENABLE_CACHING=true
export DTESN_ENABLE_PERFORMANCE_MONITORING=true
```

## Integration with echo.kern

The implementation integrates with echo.kern DTESN components when available:

- **P-System Membranes**: Hierarchical membrane computing
- **Echo State Networks**: Reservoir computing with temporal dynamics  
- **B-Series Computation**: Rooted tree enumeration and differential computation
- **OEIS A000081**: Mathematical foundation compliance

The implementation requires echo.kern components to be available for full DTESN functionality.

## Testing

Run tests with pytest:

```bash
pytest tests/endpoints/test_deep_tree_echo.py -v
```

## Performance

The implementation includes:

- Request/response timing middleware
- Server-side caching support
- Performance metrics collection
- Memory-efficient processing pipelines

## Production Deployment

The endpoints are production-ready with:

- Comprehensive error handling
- Input validation and sanitization
- Security middleware
- Monitoring and observability
- Health check endpoints
- Configuration management

## Example Usage

See `usage_example_deep_tree_echo.py` and `demo_deep_tree_echo_endpoints.py` for comprehensive usage examples.

## License

This module is part of the Aphrodite Engine project and follows the same license terms.