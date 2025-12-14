# Server-Side Streaming Response Guide

This guide documents the enhanced server-side streaming capabilities implemented for Deep Tree Echo System Network (DTESN) processing, designed to prevent timeouts and efficiently handle large datasets.

## Overview

The streaming implementation provides three specialized endpoints optimized for different use cases:

1. **Enhanced Stream Processing** (`/stream_process`) - General-purpose streaming with timeout prevention
2. **Chunked Streaming** (`/stream_chunks`) - Configurable chunking with compression support
3. **Large Dataset Streaming** (`/stream_large_dataset`) - Aggressive optimization for datasets >1MB

## Features

### Timeout Prevention
- **Automatic Heartbeats**: Prevents server timeouts during long-running operations
- **Adaptive Intervals**: 25-second intervals for chunks, 20-second for large datasets
- **Pre-processing Alerts**: Heartbeat for operations processing >50KB data

### Large Dataset Optimization
- **Adaptive Chunking**: Dynamic chunk sizing based on dataset characteristics
- **Compression Support**: Configurable levels (0=none, 1=light, 2=medium, 3=aggressive)
- **Minimal Overhead**: Shortened field names and optimized serialization

### Enhanced Error Handling
- **Graceful Recovery**: Recoverable error indicators for client retry logic
- **Client Disconnect Handling**: Proper cleanup when clients disconnect
- **Comprehensive Logging**: Detailed error tracking and performance metrics

## Endpoint Usage

### 1. Enhanced Stream Processing

```http
POST /deep_tree_echo/stream_process
Content-Type: application/json

{
  "input_data": "your data here",
  "membrane_depth": 4,
  "esn_size": 256,
  "processing_mode": "streaming"
}
```

**Features:**
- Automatic heartbeat for datasets >50KB
- Enhanced metadata with processing estimates
- Server-side rendering indicators

### 2. Chunked Streaming

```http
POST /deep_tree_echo/stream_chunks?chunk_size=1024&enable_compression=true&timeout_prevention=true
Content-Type: application/json

{
  "input_data": "your data here",
  "membrane_depth": 3,
  "esn_size": 512
}
```

**Parameters:**
- `chunk_size` (128-8192): Size of processing chunks
- `enable_compression` (boolean): Enable response compression
- `timeout_prevention` (boolean): Enable heartbeat mechanism

**Features:**
- Intelligent backpressure control
- Adaptive delay based on dataset size
- Compressed responses for efficiency

### 3. Large Dataset Streaming

```http
POST /deep_tree_echo/stream_large_dataset?max_chunk_size=4096&compression_level=2
Content-Type: application/json

{
  "input_data": "very large dataset here...",
  "membrane_depth": 5,
  "esn_size": 1024
}
```

**Parameters:**
- `max_chunk_size` (512-16384): Maximum chunk size
- `compression_level` (0-3): Compression level (0=none, 3=max)

**Features:**
- Aggressive throughput optimization
- Enhanced timeout prevention (20s heartbeats)
- Minimal serialization overhead
- **NEW**: Hybrid compression strategy (gzip + zlib)
- **NEW**: Progressive rendering optimization

### 4. Bandwidth-Optimized Streaming (NEW)

```http
POST /deep_tree_echo/stream_optimized?bandwidth_hint=auto&adaptive_compression=true&enable_progressive_rendering=true
Content-Type: application/json

{
  "input_data": "your data here...",
  "membrane_depth": 4,
  "esn_size": 512
}
```

**Parameters:**
- `bandwidth_hint` (low/medium/high/auto): Network bandwidth optimization hint
- `adaptive_compression` (boolean): Enable dynamic compression adjustment
- `enable_progressive_rendering` (boolean): Enable progressive content delivery

**Features:**
- **Bandwidth-aware optimization**: Adapts chunk size and compression based on network conditions
- **Progressive rendering**: Streams complex JSON progressively for faster client parsing
- **Adaptive compression**: Intelligently selects compression algorithm (gzip vs zlib) based on content
- **Resume capability**: Supports checkpointing and resume for interrupted transfers
- **Enhanced error recovery**: Graceful degradation with retry hints

## Response Format

### Standard Streaming Events

```
data: {"type": "metadata", "request_id": "stream_123", ...}

data: {"type": "chunk", "chunk_index": 0, "progress": 0.1, ...}

data: {"type": "heartbeat", "timestamp": 1640995200, "progress": 0.5}

data: {"type": "completion", "total_chunks": 10, "duration_ms": 2500}
```

### Large Dataset Events

```
event: metadata
data: {"type": "large_dataset_metadata", ...}

event: heartbeat  
data: {"type": "large_dataset_heartbeat", "progress": 0.3, ...}

event: chunk
data: {"type": "compressed_chunk", "data": "789abc...", "index": 5}

event: completion
data: {"type": "large_dataset_completion", "throughput_mb_per_sec": 15.2}
```

## Performance Characteristics

### Standard Streaming
- **First Byte**: <500ms
- **Throughput**: >1000 bytes/sec
- **Memory**: Constant regardless of input size
- **Heartbeat**: Every 25 seconds for long operations

### Large Dataset Streaming
- **First Byte**: <1000ms
- **Throughput**: >50MB/sec optimized
- **Memory**: Minimal buffer with aggressive compression
- **Heartbeat**: Every 20 seconds with progress estimates
- **NEW**: Hybrid compression reduces overhead by 20-40%

### Bandwidth-Optimized Streaming (NEW)
- **First Byte**: <300ms (adaptive)
- **Throughput**: Adaptive (0.5-50MB/sec based on bandwidth hint)
- **Memory**: Intelligent buffering (2KB-32KB based on network conditions)
- **Compression Ratio**: 0.3-0.8 (content-aware optimization)
- **Progressive Rendering**: 50%+ faster client parsing for complex data

## Error Handling

### Recoverable Errors
```json
{
  "type": "error",
  "error": "Processing timeout on chunk 5", 
  "recoverable": true,
  "chunk_index": 5,
  "progress": 0.5
}
```

### Fatal Errors
```json
{
  "type": "large_dataset_error",
  "error": "Invalid compression level",
  "recoverable": false,
  "timestamp": 1640995200
}
```

## Best Practices

### Client Implementation
1. **Parse Events**: Handle different event types appropriately
2. **Implement Retry**: Use `recoverable` flag for retry logic
3. **Monitor Progress**: Track progress from heartbeat messages
4. **Handle Compression**: Decompress compressed responses when needed

### Server Configuration
1. **Choose Appropriate Endpoint**: Use large dataset endpoint for >1MB data
2. **Optimize Chunk Size**: Balance between memory and responsiveness
3. **Configure Compression**: Higher levels for bandwidth-constrained environments
4. **Monitor Performance**: Use provided metrics for optimization

### Example Client Code (JavaScript)

```javascript
const eventSource = new EventSource('/deep_tree_echo/stream_large_dataset?compression_level=1');

eventSource.addEventListener('metadata', (event) => {
  const data = JSON.parse(event.data);
  console.log('Stream started:', data.request_id);
});

eventSource.addEventListener('heartbeat', (event) => {
  const data = JSON.parse(event.data);
  console.log('Progress:', Math.round(data.progress * 100) + '%');
});

eventSource.addEventListener('chunk', (event) => {
  const data = JSON.parse(event.data);
  if (data.type === 'compressed_chunk') {
    // Decompress data.data from hex
    const compressed = new Uint8Array(data.data.match(/.{2}/g).map(h => parseInt(h, 16)));
    // Process decompressed chunk...
  }
});

eventSource.addEventListener('completion', (event) => {
  const data = JSON.parse(event.data);
  console.log('Completed:', data.throughput_mb_per_sec + ' MB/s');
  eventSource.close();
});

eventSource.addEventListener('error', (event) => {
  const data = JSON.parse(event.data);
  if (data.recoverable) {
    // Implement retry logic
    setTimeout(() => retryFromProgress(data.progress), 1000);
  } else {
    console.error('Fatal error:', data.error);
    eventSource.close();
  }
});
```

## Testing

Run the enhanced performance tests to verify streaming functionality:

```bash
pytest tests/endpoints/test_performance_integration.py::TestBackendPerformance::test_enhanced_chunked_streaming_performance -v
pytest tests/endpoints/test_performance_integration.py::TestBackendPerformance::test_large_dataset_streaming_performance -v
pytest tests/endpoints/test_performance_integration.py::TestBackendPerformance::test_streaming_timeout_prevention -v
```

## Monitoring

### Key Metrics
- **Time to First Byte**: Should be <500ms for standard, <1000ms for large datasets
- **Throughput**: Monitor bytes/sec and MB/sec rates
- **Error Rate**: Track recoverable vs fatal error ratios
- **Heartbeat Frequency**: Ensure proper timeout prevention

### Headers for Monitoring
- `X-Server-Rendered: true` - Confirms server-side processing
- `X-Large-Dataset-Optimized: true` - Indicates optimization mode
- `X-Compression-Level: N` - Shows compression setting
- `X-Timeout-Prevention: enhanced` - Confirms timeout prevention

This implementation ensures efficient streaming of large responses without timeouts while maintaining optimal server-side performance.