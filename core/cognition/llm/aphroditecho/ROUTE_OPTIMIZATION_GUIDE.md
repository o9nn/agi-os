# Advanced Route Optimization Guide

This guide covers the advanced route optimization system implemented for the Aphrodite Engine API server to achieve consistent sub-100ms response times.

## Overview

The route optimization system consists of three main middleware components:

1. **Caching Middleware** - Route-specific caching strategies
2. **Compression Middleware** - Response compression and optimization  
3. **Preprocessing Middleware** - Request preprocessing and validation

These components work together to minimize latency and improve API throughput.

## Quick Start

### Enable Route Optimizations

The route optimizations are automatically applied when starting the Aphrodite API server:

```bash
# Use balanced optimization (default)
aphrodite run model-name --optimization-level balanced

# Use high-performance optimization  
aphrodite run model-name --optimization-level high

# Use minimal optimization
aphrodite run model-name --optimization-level minimal
```

### Configuration Presets

#### High Performance (`--optimization-level high`)
- **Target Response Time**: 50ms
- **Cache Size**: 2000 entries
- **Rate Limit**: 120 requests/minute
- **Compression**: Aggressive (min 200 bytes)
- **Best for**: Production environments with consistent workloads

#### Balanced (`--optimization-level balanced`) 
- **Target Response Time**: 100ms
- **Cache Size**: 1000 entries
- **Rate Limit**: 60 requests/minute  
- **Compression**: Standard (min 500 bytes)
- **Best for**: General purpose usage

#### Minimal (`--optimization-level minimal`)
- **Target Response Time**: 200ms
- **Caching**: Disabled
- **Compression**: Light (min 1000 bytes)
- **Best for**: Development and resource-constrained environments

## Architecture

### Middleware Stack Order

The middleware is applied in the following order (outermost to innermost):

```
Request → Preprocessing → Caching → Compression → Core Handler
Response ← Preprocessing ← Caching ← Compression ← Core Handler
```

### Request Flow

1. **Preprocessing**: Validates request size, applies rate limiting, optimizes JSON
2. **Caching**: Checks cache for existing response, generates cache key
3. **Compression**: Selects compression algorithm based on client capabilities  
4. **Core Handler**: Processes request if not cached
5. **Compression**: Compresses response if enabled and beneficial
6. **Caching**: Stores response in cache with appropriate TTL
7. **Preprocessing**: Adds performance monitoring headers

## Caching Strategy

### Cache Key Generation

Cache keys are generated using:
- HTTP method (GET, POST, etc.)
- Request path
- Query parameters (sorted)
- Request body (for POST requests, JSON normalized)
- Authentication header (hashed)

### TTL Configuration

Different endpoints have optimized TTL values:

| Endpoint | TTL | Reasoning |
|----------|-----|-----------|
| `/v1/models` | 1 hour | Model list rarely changes |
| `/v1/embeddings` | 5 minutes | Semi-static content |  
| `/health` | 30 seconds | Fast health checks |
| `/v1/chat/completions` | 60 seconds | Dynamic but can cache deterministic requests |

### Deterministic Request Caching

For POST endpoints like `/v1/chat/completions`, caching is enabled for deterministic requests:
- `temperature=0` (deterministic sampling)
- Same model and parameters
- No streaming responses

## Compression Strategy

### Algorithm Selection

The system supports multiple compression algorithms:

1. **gzip** - Best compatibility, good compression
2. **deflate** - Faster compression, smaller overhead

Algorithm selection is based on client's `Accept-Encoding` header.

### Compression Criteria

Responses are compressed when:
- Status code is 2xx
- Content-Type is compressible (JSON, HTML, text)
- Response size exceeds minimum threshold
- Client supports compression
- Not already compressed

### Size Thresholds

- **High Performance**: 200 bytes minimum
- **Balanced**: 500 bytes minimum  
- **Minimal**: 1000 bytes minimum

## Rate Limiting

### Token Bucket Algorithm

Uses token bucket rate limiting with:
- Configurable requests per minute
- Burst allowance for traffic spikes
- Per-IP address tracking
- Route-specific limits

### Rate Limits by Endpoint

| Endpoint | Requests/Min (Balanced) | Reasoning |
|----------|------------------------|-----------|
| `/v1/chat/completions` | 30 | Resource intensive |
| `/v1/completions` | 30 | Resource intensive |
| `/v1/embeddings` | 120 | Lighter computation |
| `/health` | 300 | Monitoring endpoint |

## Performance Monitoring

### Response Time Headers

All responses include performance timing:

```
X-Process-Time: 45.23ms
```

### Metrics Endpoint

Access performance metrics at `/metrics`:

```json
{
  "optimization_status": {
    "caching_enabled": true,
    "compression_enabled": true, 
    "preprocessing_enabled": true
  },
  "target_response_time_ms": 100,
  "cache_stats": {
    "cache_hits": 1250,
    "cache_misses": 180,
    "hit_rate": 0.874
  }
}
```

### Slow Request Logging

Requests exceeding target response time are automatically logged:

```
WARNING Slow response: /v1/chat/completions took 125.43ms (target: 100ms)
```

## Advanced Configuration

### Custom Configuration

Create custom optimization configuration in code:

```python
from aphrodite.endpoints.route_optimizer import (
    RouteOptimizationConfig, CacheConfig, CompressionConfig
)

# Custom cache configuration
cache_config = CacheConfig(
    backend="memory",
    max_cache_size=5000,
    default_ttl=300,
    route_ttl={
        "/custom/endpoint": 1800  # 30 minutes
    }
)

# Custom compression configuration  
compression_config = CompressionConfig(
    min_size=100,
    compression_level=9,  # Maximum compression
    algorithms=["gzip", "brotli"]  # Add brotli support
)

# Apply custom configuration
config = RouteOptimizationConfig(
    cache_config=cache_config,
    compression_config=compression_config,
    target_response_time_ms=75
)
```

### Environment Variables

Configure optimization via environment variables:

```bash
export APHRODITE_OPTIMIZATION_LEVEL=high
export APHRODITE_CACHE_SIZE=2000
export APHRODITE_COMPRESSION_LEVEL=6
export APHRODITE_RATE_LIMIT_RPM=120
```

## Troubleshooting

### High Cache Miss Rate

**Problem**: Cache hit rate < 50%
**Solutions**:
- Increase cache size
- Adjust TTL values
- Check for non-deterministic request patterns
- Verify cache key generation

### Response Time Above Target

**Problem**: Responses consistently > target time
**Solutions**:
- Enable higher optimization level
- Increase cache size and TTL
- Check rate limiting configuration
- Profile request processing pipeline

### Memory Usage Issues

**Problem**: High memory consumption
**Solutions**:
- Reduce cache size
- Lower compression level
- Disable caching for large responses
- Use minimal optimization level

### Compression Not Working

**Problem**: Responses not being compressed
**Solutions**:
- Check client `Accept-Encoding` headers
- Verify response size exceeds minimum threshold
- Ensure content type is compressible
- Check for existing compression

## Performance Benchmarks

### Expected Improvements

With route optimizations enabled:

| Scenario | Without Optimization | With Optimization | Improvement |
|----------|---------------------|------------------|-------------|
| Cached GET /v1/models | 50-100ms | 5-10ms | 80-90% faster |
| Deterministic completions | 200-500ms | 10-20ms | 95%+ faster |
| Large JSON responses | Network limited | 30-70% smaller | Faster transfer |
| Health checks | 20-50ms | 5-15ms | 60-70% faster |

### Target Compliance

The system is designed to achieve:
- **95%+ of requests** under target response time
- **80%+ cache hit rate** for cacheable endpoints  
- **30-70% size reduction** for compressed responses
- **Zero service degradation** from rate limiting under normal load

## Integration with Deep Tree Echo

The route optimization integrates seamlessly with DTESN components:

- **Echo.Kern Processing**: Optimized caching for membrane computing results
- **AAR Orchestration**: Rate limiting prevents agent arena overload
- **Virtual Body Mappings**: Compression reduces sensor data transfer overhead
- **Proprioceptive Feedback**: Caching accelerates feedback loop processing

## Best Practices

### Development

1. **Test with realistic payloads** - Use production-like request sizes
2. **Monitor cache hit rates** - Aim for >80% on cacheable endpoints
3. **Profile slow requests** - Investigate responses >target time
4. **Validate compression** - Ensure meaningful size reduction

### Production

1. **Start with balanced config** - Upgrade to high performance as needed
2. **Monitor memory usage** - Cache size vs available memory
3. **Set up alerting** - For slow requests and cache performance
4. **Regular cleanup** - Cache eviction and rate limit bucket cleanup

### Security

1. **Sanitize cache keys** - Ensure no sensitive data in cache keys
2. **Rate limit by IP** - Prevent abuse and DoS attacks  
3. **Validate compressed data** - Prevent compression bombs
4. **Monitor cache poisoning** - Verify cache content integrity

## API Reference

### CacheConfig

```python
@dataclass
class CacheConfig:
    backend: str = "memory"                    # Cache backend type
    default_ttl: int = 300                     # Default TTL in seconds
    max_cache_size: int = 1000                 # Maximum cache entries
    route_ttl: Dict[str, int] = ...            # Per-route TTL settings
    exclude_routes: set = ...                  # Routes to skip caching
    cache_methods: set = {"GET"}               # HTTP methods to cache
    cache_deterministic_posts: bool = True     # Cache deterministic POSTs
```

### CompressionConfig  

```python
@dataclass
class CompressionConfig:
    min_size: int = 500                        # Minimum size to compress
    compression_level: int = 6                 # Compression level (1-9)
    algorithms: list = ["gzip", "deflate"]     # Supported algorithms
    compressible_types: Set[str] = ...         # Content types to compress
    exclude_routes: Set[str] = set()           # Routes to skip compression
    enable_streaming: bool = True              # Enable streaming compression
```

### PreprocessingConfig

```python
@dataclass
class PreprocessingConfig:
    enable_validation: bool = True             # Enable request validation
    enable_rate_limiting: bool = True          # Enable rate limiting
    rate_limit: RateLimitConfig = ...          # Rate limiting configuration
    max_body_size: int = 1024 * 1024          # Max request body size
    request_timeout: float = 30.0              # Request timeout
    exclude_routes: Set[str] = ...             # Routes to skip preprocessing
    enable_size_optimization: bool = True      # Enable request optimization
```

## Conclusion

The advanced route optimization system provides a comprehensive solution for achieving sub-100ms API response times while maintaining system stability and security. The multi-layered approach with intelligent caching, compression, and preprocessing ensures optimal performance across different use cases and load patterns.

For support or advanced configuration needs, refer to the DTESN integration documentation or create an issue in the project repository.