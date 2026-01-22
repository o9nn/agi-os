import asyncio
import json
import logging
import os
import time
from collections import defaultdict, deque
from dataclasses import asdict, dataclass, field
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional, Callable
from enum import Enum
import threading
import uuid
import psutil
import statistics
from fastapi import Request, Response
from fastapi.responses import HTMLResponse, JSONResponse
logger = logging.getLogger(__name__)
class MetricType(Enum):
    REQUEST_LATENCY = 'request_latency'
    THROUGHPUT = 'throughput'
    ERROR_RATE = 'error_rate'
    CPU_UTILIZATION = 'cpu_utilization'
    MEMORY_USAGE = 'memory_usage'
    DISK_IO = 'disk_io'
    NETWORK_IO = 'network_io'
    GPU_UTILIZATION = 'gpu_utilization'
    KV_CACHE_USAGE = 'kv_cache_usage'
    CONCURRENT_REQUESTS = 'concurrent_requests'
    QUEUE_DEPTH = 'queue_depth'
class AlertLevel(Enum):
    INFO = 'info'
    WARNING = 'warning'
    CRITICAL = 'critical'
    EMERGENCY = 'emergency'
@dataclass
class RequestTrace:
    trace_id: str
    start_time: float
    method: str
    path: str
    status_code: Optional[int] = None
    end_time: Optional[float] = None
    error: Optional[str] = None
    model: Optional[str] = None
    tokens_processed: int = 0
    cpu_time: float = 0.0
    memory_peak: int = 0
    gpu_time: float = 0.0
    cache_hits: int = 0
    cache_misses: int = 0
    @property
    def duration_ms(self) -> float:
        if self.end_time:
            return (self.end_time - self.start_time) * 1000
        return (time.time() - self.start_time) * 1000
    @property
    def tokens_per_second(self) -> float:
        duration = self.duration_ms / 1000.0
        return self.tokens_processed / duration if duration > 0 else 0
    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)
@dataclass
class SystemMetrics:
    timestamp: datetime
    cpu_percent: float
    cpu_count: int
    load_average: List[float]
    memory_total: int
    memory_available: int
    memory_used: int
    memory_percent: float
    disk_read_bytes: int
    disk_write_bytes: int
    disk_read_ops: int
    disk_write_ops: int
    network_bytes_sent: int
    network_bytes_recv: int
    network_packets_sent: int
    network_packets_recv: int
    process_cpu_percent: float
    process_memory_rss: int
    process_memory_vms: int
    process_num_threads: int
    process_num_fds: int
    def to_dict(self) -> Dict[str, Any]:
        data = asdict(self)
        data['timestamp'] = self.timestamp.isoformat()
        return data
@dataclass
class PerformanceMetrics:
    timestamp: datetime
    requests_per_second: float = 0.0
    avg_response_time_ms: float = 0.0
    p95_response_time_ms: float = 0.0
    p99_response_time_ms: float = 0.0
    error_rate: float = 0.0
    tokens_per_second: float = 0.0
    avg_tokens_per_request: float = 0.0
    cpu_efficiency: float = 0.0
    memory_efficiency: float = 0.0
    concurrent_requests: int = 0
    queue_depth: int = 0
    cache_hit_rate: float = 0.0
    cache_memory_usage: int = 0
    def to_dict(self) -> Dict[str, Any]:
        data = asdict(self)
        data['timestamp'] = self.timestamp.isoformat()
        return data
class RequestTracer:
    def __init__(self, max_traces: int=10000):
        self.active_traces: Dict[str, RequestTrace] = {}
        self.completed_traces: deque = deque(maxlen=max_traces)
        self.trace_lock = threading.Lock()
    def start_trace(self, request: Request) -> str:
        trace_id = str(uuid.uuid4())
        with self.trace_lock:
            trace = RequestTrace(trace_id=trace_id, start_time=time.time(), method=request.method, path=request.url.path)
            self.active_traces[trace_id] = trace
        return trace_id
    def update_trace(self, trace_id: str, **kwargs):
        with self.trace_lock:
            if trace_id in self.active_traces:
                trace = self.active_traces[trace_id]
                for key, value in kwargs.items():
                    if hasattr(trace, key):
                        setattr(trace, key, value)
    def end_trace(self, trace_id: str, status_code: int, error: Optional[str]=None):
        with self.trace_lock:
            if trace_id in self.active_traces:
                trace = self.active_traces.pop(trace_id)
                trace.end_time = time.time()
                trace.status_code = status_code
                trace.error = error
                self.completed_traces.append(trace)
    def get_recent_traces(self, limit: int=100) -> List[RequestTrace]:
        with self.trace_lock:
            return list(self.completed_traces)[-limit:]
    def get_active_traces(self) -> List[RequestTrace]:
        with self.trace_lock:
            return list(self.active_traces.values())
class ResourceMonitor:
    def __init__(self, collection_interval: float=1.0):
        self.collection_interval = collection_interval
        self.metrics_history: deque = deque(maxlen=3600)
        self.monitoring = False
        self.monitor_thread: Optional[threading.Thread] = None
        self._process = psutil.Process()
        self._baseline_disk_io = psutil.disk_io_counters()
        self._baseline_net_io = psutil.net_io_counters()
    def start_monitoring(self):
        if not self.monitoring:
            self.monitoring = True
            self.monitor_thread = threading.Thread(target=self._monitor_loop, daemon=True)
            self.monitor_thread.start()
            logger.info('Resource monitoring started')
    def stop_monitoring(self):
        self.monitoring = False
        if self.monitor_thread:
            self.monitor_thread.join(timeout=5.0)
        logger.info('Resource monitoring stopped')
    def _monitor_loop(self):
        while self.monitoring:
            try:
                metrics = self._collect_system_metrics()
                self.metrics_history.append(metrics)
                time.sleep(self.collection_interval)
            except Exception as e:
                logger.error(f'Resource monitoring error: {e}')
                time.sleep(self.collection_interval)
    def _collect_system_metrics(self) -> SystemMetrics:
        cpu_percent = psutil.cpu_percent(interval=None)
        cpu_count = psutil.cpu_count()
        load_avg = os.getloadavg() if hasattr(os, 'getloadavg') else [0.0, 0.0, 0.0]
        memory = psutil.virtual_memory()
        disk_io = psutil.disk_io_counters()
        disk_read_bytes = disk_io.read_bytes if disk_io else 0
        disk_write_bytes = disk_io.write_bytes if disk_io else 0
        disk_read_ops = disk_io.read_count if disk_io else 0
        disk_write_ops = disk_io.write_count if disk_io else 0
        net_io = psutil.net_io_counters()
        net_bytes_sent = net_io.bytes_sent if net_io else 0
        net_bytes_recv = net_io.bytes_recv if net_io else 0
        net_packets_sent = net_io.packets_sent if net_io else 0
        net_packets_recv = net_io.packets_recv if net_io else 0
        process_info = self._process.as_dict(['cpu_percent', 'memory_info', 'num_threads', 'num_fds'])
        return SystemMetrics(timestamp=datetime.now(), cpu_percent=cpu_percent, cpu_count=cpu_count, load_average=load_avg, memory_total=memory.total, memory_available=memory.available, memory_used=memory.used, memory_percent=memory.percent, disk_read_bytes=disk_read_bytes, disk_write_bytes=disk_write_bytes, disk_read_ops=disk_read_ops, disk_write_ops=disk_write_ops, network_bytes_sent=net_bytes_sent, network_bytes_recv=net_bytes_recv, network_packets_sent=net_packets_sent, network_packets_recv=net_packets_recv, process_cpu_percent=process_info.get('cpu_percent', 0.0), process_memory_rss=process_info.get('memory_info', {}).get('rss', 0), process_memory_vms=process_info.get('memory_info', {}).get('vms', 0), process_num_threads=process_info.get('num_threads', 0), process_num_fds=process_info.get('num_fds', 0))
    def get_current_metrics(self) -> Optional[SystemMetrics]:
        if self.metrics_history:
            return self.metrics_history[-1]
        return None
    def get_metrics_history(self, seconds: int=300) -> List[SystemMetrics]:
        cutoff_time = datetime.now() - timedelta(seconds=seconds)
        return [m for m in self.metrics_history if m.timestamp > cutoff_time]
class PerformanceAnalyzer:
    def __init__(self, tracer: RequestTracer, resource_monitor: ResourceMonitor):
        self.tracer = tracer
        self.resource_monitor = resource_monitor
    def analyze_performance(self, window_minutes: int=5) -> PerformanceMetrics:
        window_start = time.time() - window_minutes * 60
        recent_traces = [trace for trace in self.tracer.completed_traces if trace.start_time > window_start and trace.end_time]
        if not recent_traces:
            return PerformanceMetrics(timestamp=datetime.now())
        durations = [trace.duration_ms for trace in recent_traces]
        token_rates = [trace.tokens_per_second for trace in recent_traces if trace.tokens_per_second > 0]
        errors = [trace for trace in recent_traces if trace.status_code >= 400]
        requests_per_second = len(recent_traces) / (window_minutes * 60)
        avg_response_time = statistics.mean(durations) if durations else 0
        p95_response_time = statistics.quantiles(durations, n=20)[18] if len(durations) >= 20 else avg_response_time
        p99_response_time = statistics.quantiles(durations, n=100)[98] if len(durations) >= 100 else p95_response_time
        error_rate = len(errors) / len(recent_traces) if recent_traces else 0
        tokens_per_second = statistics.mean(token_rates) if token_rates else 0
        avg_tokens_per_request = statistics.mean([t.tokens_processed for t in recent_traces]) if recent_traces else 0
        system_metrics = self.resource_monitor.get_current_metrics()
        cpu_efficiency = 0.0
        memory_efficiency = 0.0
        if system_metrics and tokens_per_second > 0:
            cpu_efficiency = tokens_per_second / system_metrics.cpu_count
            memory_gb = system_metrics.memory_total / 1024 ** 3
            memory_efficiency = tokens_per_second / memory_gb
        active_traces = self.tracer.get_active_traces()
        concurrent_requests = len(active_traces)
        cache_hit_rate = 0.0
        cache_memory_usage = 0
        return PerformanceMetrics(timestamp=datetime.now(), requests_per_second=requests_per_second, avg_response_time_ms=avg_response_time, p95_response_time_ms=p95_response_time, p99_response_time_ms=p99_response_time, error_rate=error_rate, tokens_per_second=tokens_per_second, avg_tokens_per_request=avg_tokens_per_request, cpu_efficiency=cpu_efficiency, memory_efficiency=memory_efficiency, concurrent_requests=concurrent_requests, queue_depth=0, cache_hit_rate=cache_hit_rate, cache_memory_usage=cache_memory_usage)
    def predict_capacity_needs(self, target_rps: float, current_metrics: PerformanceMetrics) -> Dict[str, Any]:
        if current_metrics.requests_per_second == 0:
            return {'error': 'No current traffic to analyze'}
        scaling_factor = target_rps / current_metrics.requests_per_second
        system_metrics = self.resource_monitor.get_current_metrics()
        if not system_metrics:
            return {'error': 'No system metrics available'}
        predicted_cpu = system_metrics.cpu_percent * scaling_factor
        predicted_memory = system_metrics.memory_percent * scaling_factor
        return {'target_rps': target_rps, 'scaling_factor': scaling_factor, 'predicted_cpu_percent': predicted_cpu, 'predicted_memory_percent': predicted_memory, 'recommended_instances': max(1, int(predicted_cpu / 80)), 'bottleneck_prediction': self._identify_bottlenecks(predicted_cpu, predicted_memory), 'confidence': self._calculate_prediction_confidence(current_metrics)}
    def _identify_bottlenecks(self, predicted_cpu: float, predicted_memory: float) -> List[str]:
        bottlenecks = []
        if predicted_cpu > 90:
            bottlenecks.append('CPU will be saturated')
        if predicted_memory > 90:
            bottlenecks.append('Memory will be saturated')
        if predicted_cpu > 80 and predicted_memory > 80:
            bottlenecks.append('Both CPU and memory will be constrained')
        return bottlenecks
    def _calculate_prediction_confidence(self, metrics: PerformanceMetrics) -> float:
        base_confidence = min(0.9, metrics.requests_per_second / 100)
        if metrics.error_rate > 0.05:
            base_confidence *= 0.8
        return max(0.1, base_confidence)
class MonitoringDashboard:
    def __init__(self, analyzer: PerformanceAnalyzer, resource_monitor: ResourceMonitor, tracer: RequestTracer):
        self.analyzer = analyzer
        self.resource_monitor = resource_monitor
        self.tracer = tracer
    def get_dashboard_html(self) -> str:
        performance_metrics = self.analyzer.analyze_performance()
        system_metrics = self.resource_monitor.get_current_metrics()
        active_traces = self.tracer.get_active_traces()
        return f"""\n<!DOCTYPE html>\n<html>\n<head>\n    <title>Aphrodite Engine Monitoring Dashboard</title>\n    <meta charset="utf-8">\n    <meta name="viewport" content="width=device-width, initial-scale=1">\n    <style>\n        body {{ font-family: Arial, sans-serif; margin: 0; padding: 20px; background: #f5f5f5; }}\n        .container {{ max-width: 1200px; margin: 0 auto; }}\n        .header {{ background: #2c3e50; color: white; padding: 20px; border-radius: 8px; margin-bottom: 20px; }}\n        .metrics-grid {{ display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px; }}\n        .metric-card {{ background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }}\n        .metric-title {{ font-size: 18px; font-weight: bold; margin-bottom: 15px; color: #2c3e50; }}\n        .metric-value {{ font-size: 24px; font-weight: bold; margin: 10px 0; }}\n        .metric-label {{ color: #666; font-size: 14px; }}\n        .status-good {{ color: #27ae60; }}\n        .status-warning {{ color: #f39c12; }}\n        .status-critical {{ color: #e74c3c; }}\n        .trace-list {{ max-height: 300px; overflow-y: auto; }}\n        .trace-item {{ padding: 8px; border-bottom: 1px solid #eee; font-size: 12px; }}\n        .refresh-note {{ text-align: center; margin-top: 20px; color: #666; }}\n    </style>\n    <meta http-equiv="refresh" content="30">\n</head>\n<body>\n    <div class="container">\n        <div class="header">\n            <h1>🚀 Aphrodite Engine Monitoring Dashboard</h1>\n            <p>Real-time performance and resource monitoring - Updated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>\n        </div>\n        \n        <div class="metrics-grid">\n            <div class="metric-card">\n                <div class="metric-title">📈 Performance Metrics</div>\n                <div class="metric-value status-{self._get_status_class(performance_metrics.requests_per_second, 10, 50)}">\n                    {performance_metrics.requests_per_second:.1f} RPS\n                </div>\n                <div class="metric-label">Requests per Second</div>\n                \n                <div class="metric-value status-{self._get_status_class(performance_metrics.avg_response_time_ms, 500, 1000, reverse=True)}">\n                    {performance_metrics.avg_response_time_ms:.0f}ms\n                </div>\n                <div class="metric-label">Average Response Time</div>\n                \n                <div class="metric-value status-{self._get_status_class(performance_metrics.error_rate, 0.05, 0.1, reverse=True)}">\n                    {performance_metrics.error_rate:.1%}\n                </div>\n                <div class="metric-label">Error Rate</div>\n            </div>\n            \n            <div class="metric-card">\n                <div class="metric-title">🧠 Model Performance</div>\n                <div class="metric-value status-{self._get_status_class(performance_metrics.tokens_per_second, 100, 500)}">\n                    {performance_metrics.tokens_per_second:.0f} tokens/s\n                </div>\n                <div class="metric-label">Token Generation Rate</div>\n                \n                <div class="metric-value">\n                    {performance_metrics.avg_tokens_per_request:.0f}\n                </div>\n                <div class="metric-label">Avg Tokens per Request</div>\n                \n                <div class="metric-value">\n                    {performance_metrics.concurrent_requests}\n                </div>\n                <div class="metric-label">Active Requests</div>\n            </div>\n            \n            <div class="metric-card">\n                <div class="metric-title">💻 System Resources</div>\n                {self._render_system_metrics(system_metrics)}\n            </div>\n            \n            <div class="metric-card">\n                <div class="metric-title">⚡ Efficiency Metrics</div>\n                <div class="metric-value">\n                    {performance_metrics.cpu_efficiency:.1f}\n                </div>\n                <div class="metric-label">Tokens/sec per CPU Core</div>\n                \n                <div class="metric-value">\n                    {performance_metrics.memory_efficiency:.1f}\n                </div>\n                <div class="metric-label">Tokens/sec per GB RAM</div>\n            </div>\n            \n            <div class="metric-card">\n                <div class="metric-title">🔍 Active Request Traces</div>\n                <div class="trace-list">\n                    {self._render_active_traces(active_traces)}\n                </div>\n            </div>\n            \n            <div class="metric-card">\n                <div class="metric-title">📊 Response Time Distribution</div>\n                <div class="metric-value">P95: {performance_metrics.p95_response_time_ms:.0f}ms</div>\n                <div class="metric-value">P99: {performance_metrics.p99_response_time_ms:.0f}ms</div>\n                <div class="metric-label">Latency Percentiles</div>\n            </div>\n        </div>\n        \n        <div class="refresh-note">\n            Dashboard auto-refreshes every 30 seconds | \n            <a href="/monitoring/api/metrics">JSON API</a> | \n            <a href="/monitoring/api/traces">Request Traces</a>\n        </div>\n    </div>\n</body>\n</html>"""
    def _get_status_class(self, value: float, warning_threshold: float, critical_threshold: float, reverse: bool=False) -> str:
        if reverse:
            if value >= critical_threshold:
                return 'critical'
            elif value >= warning_threshold:
                return 'warning'
            else:
                return 'good'
        elif value >= critical_threshold:
            return 'good'
        elif value >= warning_threshold:
            return 'warning'
        else:
            return 'critical'
    def _render_system_metrics(self, metrics: Optional[SystemMetrics]) -> str:
        if not metrics:
            return '<div>No system metrics available</div>'
        return f'\n            <div class="metric-value status-{self._get_status_class(metrics.cpu_percent, 70, 90, reverse=True)}">\n                {metrics.cpu_percent:.1f}%\n            </div>\n            <div class="metric-label">CPU Usage</div>\n            \n            <div class="metric-value status-{self._get_status_class(metrics.memory_percent, 80, 95, reverse=True)}">\n                {metrics.memory_percent:.1f}%\n            </div>\n            <div class="metric-label">Memory Usage ({metrics.memory_used // 1024 ** 3}GB / {metrics.memory_total // 1024 ** 3}GB)</div>\n            \n            <div class="metric-value">\n                {metrics.process_num_threads}\n            </div>\n            <div class="metric-label">Process Threads</div>\n        '
    def _render_active_traces(self, traces: List[RequestTrace]) -> str:
        if not traces:
            return "<div class='trace-item'>No active requests</div>"
        trace_html = ''
        for trace in traces[:10]:
            duration = trace.duration_ms
            trace_html += f'\n                <div class="trace-item">\n                    <strong>{trace.method} {trace.path}</strong><br>\n                    Duration: {duration:.0f}ms | Tokens: {trace.tokens_processed} | \n                    Rate: {trace.tokens_per_second:.0f} tok/s\n                </div>\n            '
        if len(traces) > 10:
            trace_html += f"<div class='trace-item'><em>... and {len(traces) - 10} more active requests</em></div>"
        return trace_html
request_tracer = RequestTracer()
resource_monitor = ResourceMonitor()
performance_analyzer = PerformanceAnalyzer(request_tracer, resource_monitor)
monitoring_dashboard = MonitoringDashboard(performance_analyzer, resource_monitor, request_tracer)
def start_monitoring():
    resource_monitor.start_monitoring()
    logger.info('Comprehensive server-side monitoring started')
def stop_monitoring():
    resource_monitor.stop_monitoring()
    logger.info('Comprehensive server-side monitoring stopped')
async def monitoring_middleware(request: Request, call_next):
    trace_id = request_tracer.start_trace(request)
    try:
        response = await call_next(request)
        request_tracer.end_trace(trace_id, response.status_code)
        return response
    except Exception as e:
        request_tracer.end_trace(trace_id, 500, str(e))
        raise
def get_monitoring_routes():
    from fastapi import APIRouter
    router = APIRouter(prefix='/monitoring', tags=['monitoring'])
    @router.get('/', response_class=HTMLResponse)
    async def dashboard():
        return monitoring_dashboard.get_dashboard_html()
    @router.get('/api/metrics')
    async def get_metrics():
        performance_metrics = performance_analyzer.analyze_performance()
        system_metrics = resource_monitor.get_current_metrics()
        return {'timestamp': datetime.now().isoformat(), 'performance': performance_metrics.to_dict(), 'system': system_metrics.to_dict() if system_metrics else None, 'monitoring_status': 'active' if resource_monitor.monitoring else 'inactive'}
    @router.get('/api/traces')
    async def get_traces(limit: int=100):
        traces = request_tracer.get_recent_traces(limit)
        return {'traces': [trace.to_dict() for trace in traces], 'active_count': len(request_tracer.get_active_traces()), 'completed_count': len(traces)}
    @router.get('/api/capacity')
    async def get_capacity_analysis(target_rps: float=100):
        current_metrics = performance_analyzer.analyze_performance()
        capacity_prediction = performance_analyzer.predict_capacity_needs(target_rps, current_metrics)
        return {'current_metrics': current_metrics.to_dict(), 'capacity_prediction': capacity_prediction, 'timestamp': datetime.now().isoformat()}
    @router.get('/api/health')
    async def health_check():
        system_metrics = resource_monitor.get_current_metrics()
        performance_metrics = performance_analyzer.analyze_performance()
        if not system_metrics:
            status = 'unhealthy'
            details = 'No system metrics available'
        elif system_metrics.cpu_percent > 95 or system_metrics.memory_percent > 98:
            status = 'critical'
            details = 'System resources critically low'
        elif performance_metrics.error_rate > 0.1:
            status = 'degraded'
            details = f'High error rate: {performance_metrics.error_rate:.1%}'
        elif system_metrics.cpu_percent > 80 or system_metrics.memory_percent > 85:
            status = 'warning'
            details = 'High resource utilization'
        else:
            status = 'healthy'
            details = 'All systems operating normally'
        return {'status': status, 'details': details, 'timestamp': datetime.now().isoformat(), 'uptime': resource_monitor.monitoring, 'metrics_collected': len(resource_monitor.metrics_history)}
    return router