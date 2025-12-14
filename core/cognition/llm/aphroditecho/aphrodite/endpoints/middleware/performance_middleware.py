"""
Enhanced performance monitoring and profiling middleware for FastAPI applications.

Provides comprehensive performance metrics, profiling, and real-time monitoring
with integration to existing performance monitoring systems.
"""

import time
import threading
from collections import deque, defaultdict
from dataclasses import dataclass, asdict
from typing import Any, Callable, Dict, List, Optional
from datetime import datetime, timedelta
import statistics

from fastapi import Request, Response
from starlette.middleware.base import BaseHTTPMiddleware
from loguru import logger

try:
    import psutil
    HAS_PSUTIL = True
except ImportError:
    HAS_PSUTIL = False

try:
    import GPUtil
    HAS_GPUTIL = True
except ImportError:
    HAS_GPUTIL = False


@dataclass
class PerformanceMetrics:
    """Performance metrics for a request."""
    request_id: str
    endpoint: str
    method: str
    status_code: int
    processing_time_ms: float
    memory_usage_mb: float
    cpu_percent: float
    gpu_usage_percent: Optional[float]
    concurrent_requests: int
    queue_time_ms: float
    db_time_ms: float
    cache_hit: bool
    bytes_sent: int
    bytes_received: int
    timestamp: datetime
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization."""
        result = asdict(self)
        result['timestamp'] = self.timestamp.isoformat()
        return result


@dataclass
class SystemMetrics:
    """System-level performance metrics."""
    cpu_percent: float
    memory_percent: float
    disk_usage_percent: float
    gpu_usage_percent: Optional[float] = None
    gpu_memory_percent: Optional[float] = None
    network_bytes_sent: int = 0
    network_bytes_recv: int = 0
    active_connections: int = 0
    timestamp: datetime = None
    
    def __post_init__(self):
        if self.timestamp is None:
            self.timestamp = datetime.utcnow()


class PerformanceProfiler:
    """Real-time performance profiler with metrics collection."""
    
    def __init__(self, max_history: int = 1000):
        self.max_history = max_history
        self.request_metrics: deque = deque(maxlen=max_history)
        self.system_metrics: deque = deque(maxlen=max_history)
        self.endpoint_stats: Dict[str, List[float]] = defaultdict(list)
        self.concurrent_requests = 0
        self.request_queue = deque()
        self.lock = threading.RLock()
        
        # Start system monitoring thread
        self.monitoring_active = True
        self.monitor_thread = threading.Thread(target=self._monitor_system, daemon=True)
        self.monitor_thread.start()
        
    def _monitor_system(self):
        """Monitor system metrics in background thread."""
        while self.monitoring_active:
            try:
                metrics = self._collect_system_metrics()
                with self.lock:
                    self.system_metrics.append(metrics)
                time.sleep(5)  # Collect every 5 seconds
            except Exception as e:
                logger.error(f"System monitoring error: {e}")
                time.sleep(10)  # Back off on errors
    
    def _collect_system_metrics(self) -> SystemMetrics:
        """Collect current system performance metrics."""
        metrics = SystemMetrics(
            cpu_percent=0.0,
            memory_percent=0.0,
            disk_usage_percent=0.0,
            timestamp=datetime.utcnow()
        )
        
        if HAS_PSUTIL:
            try:
                # CPU and memory
                metrics.cpu_percent = psutil.cpu_percent(interval=0.1)
                memory = psutil.virtual_memory()
                metrics.memory_percent = memory.percent
                
                # Disk usage
                disk = psutil.disk_usage('/')
                metrics.disk_usage_percent = (disk.used / disk.total) * 100
                
                # Network stats
                network = psutil.net_io_counters()
                metrics.network_bytes_sent = network.bytes_sent
                metrics.network_bytes_recv = network.bytes_recv
                
                # Connection count
                metrics.active_connections = len(psutil.net_connections())
                
            except Exception as e:
                logger.warning(f"Failed to collect psutil metrics: {e}")
        
        if HAS_GPUTIL:
            try:
                gpus = GPUtil.getGPUs()
                if gpus:
                    gpu = gpus[0]  # Use first GPU
                    metrics.gpu_usage_percent = gpu.load * 100
                    metrics.gpu_memory_percent = gpu.memoryUtil * 100
            except Exception as e:
                logger.warning(f"Failed to collect GPU metrics: {e}")
        
        return metrics
    
    def record_request(self, metrics: PerformanceMetrics):
        """Record performance metrics for a request."""
        with self.lock:
            self.request_metrics.append(metrics)
            
            # Update endpoint statistics
            self.endpoint_stats[f"{metrics.method} {metrics.endpoint}"].append(
                metrics.processing_time_ms
            )
            
            # Limit per-endpoint history
            if len(self.endpoint_stats[f"{metrics.method} {metrics.endpoint}"]) > 100:
                self.endpoint_stats[f"{metrics.method} {metrics.endpoint}"].pop(0)
    
    def get_endpoint_statistics(self, endpoint: str) -> Dict[str, float]:
        """Get statistical analysis for an endpoint."""
        with self.lock:
            times = self.endpoint_stats.get(endpoint, [])
            
            if not times:
                return {}
            
            return {
                "count": len(times),
                "mean_ms": statistics.mean(times),
                "median_ms": statistics.median(times),
                "min_ms": min(times),
                "max_ms": max(times),
                "std_dev_ms": statistics.stdev(times) if len(times) > 1 else 0.0,
                "p95_ms": self._percentile(times, 95),
                "p99_ms": self._percentile(times, 99)
            }
    
    def get_system_health(self) -> Dict[str, Any]:
        """Get current system health status."""
        with self.lock:
            if not self.system_metrics:
                return {"status": "no_data"}
            
            latest = self.system_metrics[-1]
            recent_requests = [m for m in self.request_metrics 
                             if (datetime.utcnow() - m.timestamp).total_seconds() < 300]
            
            health = {
                "status": "healthy",
                "timestamp": latest.timestamp.isoformat(),
                "system": {
                    "cpu_percent": latest.cpu_percent,
                    "memory_percent": latest.memory_percent,
                    "disk_usage_percent": latest.disk_usage_percent,
                    "gpu_usage_percent": latest.gpu_usage_percent,
                    "gpu_memory_percent": latest.gpu_memory_percent,
                    "active_connections": latest.active_connections
                },
                "requests": {
                    "count_last_5min": len(recent_requests),
                    "concurrent_active": self.concurrent_requests,
                    "avg_response_time_ms": statistics.mean([r.processing_time_ms for r in recent_requests]) if recent_requests else 0
                }
            }
            
            # Determine health status
            if (latest.cpu_percent > 90 or 
                latest.memory_percent > 95 or 
                latest.disk_usage_percent > 95):
                health["status"] = "critical"
            elif (latest.cpu_percent > 70 or 
                  latest.memory_percent > 80 or 
                  latest.disk_usage_percent > 80):
                health["status"] = "warning"
            
            return health
    
    def get_performance_trends(self, minutes: int = 30) -> Dict[str, List[Dict[str, Any]]]:
        """Get performance trends over time."""
        cutoff = datetime.utcnow() - timedelta(minutes=minutes)
        
        with self.lock:
            recent_system = [m for m in self.system_metrics if m.timestamp > cutoff]
            recent_requests = [m for m in self.request_metrics if m.timestamp > cutoff]
            
            return {
                "system_metrics": [
                    {
                        "timestamp": m.timestamp.isoformat(),
                        "cpu_percent": m.cpu_percent,
                        "memory_percent": m.memory_percent,
                        "disk_usage_percent": m.disk_usage_percent,
                        "gpu_usage_percent": m.gpu_usage_percent
                    } for m in recent_system
                ],
                "request_metrics": [
                    {
                        "timestamp": m.timestamp.isoformat(),
                        "processing_time_ms": m.processing_time_ms,
                        "endpoint": m.endpoint,
                        "status_code": m.status_code
                    } for m in recent_requests
                ]
            }
    
    @staticmethod
    def _percentile(data: List[float], percentile: int) -> float:
        """Calculate percentile of data."""
        if not data:
            return 0.0
        
        sorted_data = sorted(data)
        index = (percentile / 100) * (len(sorted_data) - 1)
        
        if index.is_integer():
            return sorted_data[int(index)]
        else:
            lower = sorted_data[int(index)]
            upper = sorted_data[int(index) + 1]
            return lower + (upper - lower) * (index - int(index))
    
    def shutdown(self):
        """Shutdown the profiler."""
        self.monitoring_active = False
        if self.monitor_thread.is_alive():
            self.monitor_thread.join(timeout=5)


class EnhancedPerformanceMonitoringMiddleware(BaseHTTPMiddleware):
    """
    Enhanced performance monitoring middleware with real-time profiling,
    system metrics, and comprehensive performance analysis.
    """
    
    def __init__(
        self, 
        app, 
        profiler: Optional[PerformanceProfiler] = None,
        enable_profiling: bool = True,
        slow_request_threshold_ms: float = 1000.0,
        enable_detailed_metrics: bool = True
    ):
        """Initialize enhanced performance monitoring middleware."""
        super().__init__(app)
        self.profiler = profiler or PerformanceProfiler()
        self.enable_profiling = enable_profiling
        self.slow_request_threshold_ms = slow_request_threshold_ms
        self.enable_detailed_metrics = enable_detailed_metrics
        
        # Performance counters
        self._request_counter = 0
        self._error_counter = 0
        self._slow_request_counter = 0
        
        logger.info("EnhancedPerformanceMonitoringMiddleware initialized with real-time profiling")
    
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """Monitor request performance with comprehensive metrics collection."""
        
        start_time = time.time()
        start_memory = self._get_memory_usage()
        start_cpu = self._get_cpu_usage()
        
        # Track concurrent requests
        self.profiler.concurrent_requests += 1
        request_id = getattr(request.state, 'request_id', f"perf_{int(time.time() * 1000000)}")
        
        try:
            # Measure queue time (if available from previous middleware)
            queue_time_ms = 0.0
            if hasattr(request.state, 'queue_start_time'):
                queue_time_ms = (start_time - request.state.queue_start_time) * 1000
            
            # Process request
            response = await call_next(request)
            
            # Calculate metrics
            processing_time = time.time() - start_time
            processing_time_ms = processing_time * 1000
            
            # Collect detailed metrics
            end_memory = self._get_memory_usage()
            end_cpu = self._get_cpu_usage()
            
            # Get response size
            bytes_sent = 0
            if hasattr(response, 'body'):
                bytes_sent = len(response.body) if response.body else 0
            elif response.headers.get('content-length'):
                bytes_sent = int(response.headers['content-length'])
            
            # Get request size
            bytes_received = int(request.headers.get('content-length', '0'))
            
            # Check for cache hit (from cache middleware)
            cache_hit = getattr(request.state, 'cache_hit', False)
            
            # Measure database time (if available from DTESN middleware)
            db_time_ms = getattr(request.state, 'db_time_ms', 0.0)
            
            # Create performance metrics
            metrics = PerformanceMetrics(
                request_id=request_id,
                endpoint=request.url.path,
                method=request.method,
                status_code=response.status_code,
                processing_time_ms=processing_time_ms,
                memory_usage_mb=end_memory,
                cpu_percent=(end_cpu + start_cpu) / 2,  # Average
                gpu_usage_percent=self._get_gpu_usage(),
                concurrent_requests=self.profiler.concurrent_requests,
                queue_time_ms=queue_time_ms,
                db_time_ms=db_time_ms,
                cache_hit=cache_hit,
                bytes_sent=bytes_sent,
                bytes_received=bytes_received,
                timestamp=datetime.utcnow()
            )
            
            # Record metrics
            if self.enable_profiling:
                self.profiler.record_request(metrics)
            
            # Update counters
            self._request_counter += 1
            if response.status_code >= 400:
                self._error_counter += 1
            if processing_time_ms > self.slow_request_threshold_ms:
                self._slow_request_counter += 1
            
            # Add performance headers
            response.headers["X-Process-Time-Ms"] = f"{processing_time_ms:.2f}"
            response.headers["X-Memory-Usage-Mb"] = f"{end_memory:.2f}"
            response.headers["X-Concurrent-Requests"] = str(self.profiler.concurrent_requests)
            response.headers["X-Cache-Hit"] = "true" if cache_hit else "false"
            
            if queue_time_ms > 0:
                response.headers["X-Queue-Time-Ms"] = f"{queue_time_ms:.2f}"
            
            if self.enable_detailed_metrics:
                response.headers["X-CPU-Percent"] = f"{metrics.cpu_percent:.1f}"
                if metrics.gpu_usage_percent is not None:
                    response.headers["X-GPU-Percent"] = f"{metrics.gpu_usage_percent:.1f}"
                response.headers["X-Bytes-Sent"] = str(bytes_sent)
                response.headers["X-Bytes-Received"] = str(bytes_received)
            
            # Log slow requests
            if processing_time_ms > self.slow_request_threshold_ms:
                logger.warning(
                    f"Slow request detected: {request.method} {request.url.path} "
                    f"took {processing_time_ms:.2f}ms (threshold: {self.slow_request_threshold_ms}ms)"
                )
            
            # Log performance summary
            logger.info(
                f"Performance: {request.method} {request.url.path} "
                f"{processing_time_ms:.2f}ms {response.status_code} "
                f"[mem: {end_memory:.1f}MB, concurrent: {self.profiler.concurrent_requests}]"
            )
            
            return response
            
        except Exception as e:
            self._error_counter += 1
            processing_time_ms = (time.time() - start_time) * 1000
            
            logger.error(
                f"Performance monitoring error for {request.method} {request.url.path}: {e} "
                f"(processing time: {processing_time_ms:.2f}ms)"
            )
            raise
        
        finally:
            # Always decrement concurrent counter
            self.profiler.concurrent_requests -= 1
    
    def _get_memory_usage(self) -> float:
        """Get current memory usage in MB."""
        if HAS_PSUTIL:
            try:
                process = psutil.Process()
                return process.memory_info().rss / 1024 / 1024
            except Exception:
                pass
        return 0.0
    
    def _get_cpu_usage(self) -> float:
        """Get current CPU usage percentage."""
        if HAS_PSUTIL:
            try:
                return psutil.cpu_percent(interval=0.01)
            except Exception:
                pass
        return 0.0
    
    def _get_gpu_usage(self) -> Optional[float]:
        """Get current GPU usage percentage."""
        if HAS_GPUTIL:
            try:
                gpus = GPUtil.getGPUs()
                return gpus[0].load * 100 if gpus else None
            except Exception:
                pass
        return None
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get middleware statistics."""
        return {
            "total_requests": self._request_counter,
            "error_requests": self._error_counter,
            "slow_requests": self._slow_request_counter,
            "error_rate": self._error_counter / max(self._request_counter, 1),
            "slow_request_rate": self._slow_request_counter / max(self._request_counter, 1),
            "current_concurrent": self.profiler.concurrent_requests
        }
    
    def get_health_status(self) -> Dict[str, Any]:
        """Get system health status."""
        return self.profiler.get_system_health()
    
    def get_endpoint_statistics(self) -> Dict[str, Dict[str, float]]:
        """Get per-endpoint performance statistics."""
        stats = {}
        for endpoint in self.profiler.endpoint_stats.keys():
            stats[endpoint] = self.profiler.get_endpoint_statistics(endpoint)
        return stats