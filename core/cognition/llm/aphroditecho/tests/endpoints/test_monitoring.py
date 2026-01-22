import asyncio
import json
import pytest
import time
from datetime import datetime, timedelta
from unittest.mock import Mock, patch, MagicMock
import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../../'))
from aphrodite.endpoints.monitoring import RequestTracer, ResourceMonitor, PerformanceAnalyzer, MonitoringDashboard, RequestTrace, SystemMetrics, PerformanceMetrics, MetricType, AlertLevel, start_monitoring, stop_monitoring, get_monitoring_routes
class TestRequestTracer:
    def test_trace_lifecycle(self):
        tracer = RequestTracer()
        mock_request = Mock()
        mock_request.method = 'POST'
        mock_request.url.path = '/v1/chat/completions'
        trace_id = tracer.start_trace(mock_request)
        assert trace_id is not None
        assert trace_id in tracer.active_traces
        tracer.update_trace(trace_id, tokens_processed=150, model='gpt-3.5-turbo')
        trace = tracer.active_traces[trace_id]
        assert trace.tokens_processed == 150
        assert trace.model == 'gpt-3.5-turbo'
        tracer.end_trace(trace_id, 200)
        assert trace_id not in tracer.active_traces
        assert len(tracer.completed_traces) == 1
        completed_trace = list(tracer.completed_traces)[0]
        assert completed_trace.status_code == 200
        assert completed_trace.end_time is not None
    def test_trace_metrics_calculations(self):
        tracer = RequestTracer()
        mock_request = Mock()
        mock_request.method = 'POST'
        mock_request.url.path = '/v1/completions'
        trace_id = tracer.start_trace(mock_request)
        time.sleep(0.1)
        tracer.update_trace(trace_id, tokens_processed=100)
        tracer.end_trace(trace_id, 200)
        completed_trace = list(tracer.completed_traces)[0]
        assert completed_trace.duration_ms >= 100
        assert completed_trace.duration_ms < 200
        assert completed_trace.tokens_per_second > 0
        assert completed_trace.tokens_per_second < 10000
    def test_trace_history_limits(self):
        tracer = RequestTracer(max_traces=5)
        mock_request = Mock()
        mock_request.method = 'GET'
        mock_request.url.path = '/health'
        for i in range(10):
            trace_id = tracer.start_trace(mock_request)
            tracer.end_trace(trace_id, 200)
        assert len(tracer.completed_traces) == 5
class TestResourceMonitor:
    def test_metrics_collection(self):
        monitor = ResourceMonitor(collection_interval=0.1)
        metrics = monitor._collect_system_metrics()
        assert isinstance(metrics, SystemMetrics)
        assert metrics.cpu_percent >= 0
        assert metrics.memory_total > 0
        assert metrics.memory_used > 0
        assert metrics.cpu_count > 0
        assert isinstance(metrics.timestamp, datetime)
    @patch('time.sleep')
    def test_monitoring_loop(self, mock_sleep):
        monitor = ResourceMonitor(collection_interval=0.1)
        mock_sleep.return_value = None
        monitor.start_monitoring()
        assert monitor.monitoring is True
        assert monitor.monitor_thread is not None
        time.sleep(0.3)
        monitor.stop_monitoring()
        assert monitor.monitoring is False
        assert len(monitor.metrics_history) > 0
    def test_metrics_history_filtering(self):
        monitor = ResourceMonitor()
        now = datetime.now()
        old_metrics = SystemMetrics(timestamp=now - timedelta(minutes=10), cpu_percent=50.0, cpu_count=4, load_average=[1.0, 1.0, 1.0], memory_total=8000000000, memory_available=4000000000, memory_used=4000000000, memory_percent=50.0, disk_read_bytes=0, disk_write_bytes=0, disk_read_ops=0, disk_write_ops=0, network_bytes_sent=0, network_bytes_recv=0, network_packets_sent=0, network_packets_recv=0, process_cpu_percent=10.0, process_memory_rss=1000000, process_memory_vms=2000000, process_num_threads=5, process_num_fds=10)
        recent_metrics = SystemMetrics(timestamp=now - timedelta(seconds=30), cpu_percent=60.0, cpu_count=4, load_average=[1.2, 1.1, 1.0], memory_total=8000000000, memory_available=3500000000, memory_used=4500000000, memory_percent=56.25, disk_read_bytes=100, disk_write_bytes=200, disk_read_ops=5, disk_write_ops=10, network_bytes_sent=1000, network_bytes_recv=2000, network_packets_sent=10, network_packets_recv=20, process_cpu_percent=15.0, process_memory_rss=1200000, process_memory_vms=2200000, process_num_threads=6, process_num_fds=12)
        monitor.metrics_history.extend([old_metrics, recent_metrics])
        recent = monitor.get_metrics_history(300)
        assert len(recent) == 1
        assert recent[0].cpu_percent == 60.0
class TestPerformanceAnalyzer:
    def test_performance_analysis(self):
        tracer = RequestTracer()
        monitor = ResourceMonitor()
        analyzer = PerformanceAnalyzer(tracer, monitor)
        traces = []
        base_time = time.time() - 60
        for i in range(10):
            trace = RequestTrace(trace_id=f'trace_{i}', start_time=base_time + i, method='POST', path='/v1/chat/completions', status_code=200 if i < 9 else 500, end_time=base_time + i + 0.5, tokens_processed=100, model='test-model')
            traces.append(trace)
        tracer.completed_traces.extend(traces)
        system_metrics = SystemMetrics(timestamp=datetime.now(), cpu_percent=75.0, cpu_count=8, load_average=[2.0, 1.8, 1.5], memory_total=16000000000, memory_available=8000000000, memory_used=8000000000, memory_percent=50.0, disk_read_bytes=1000, disk_write_bytes=2000, disk_read_ops=10, disk_write_ops=20, network_bytes_sent=5000, network_bytes_recv=10000, network_packets_sent=50, network_packets_recv=100, process_cpu_percent=25.0, process_memory_rss=2000000, process_memory_vms=4000000, process_num_threads=12, process_num_fds=25)
        monitor.metrics_history.append(system_metrics)
        metrics = analyzer.analyze_performance(window_minutes=5)
        assert isinstance(metrics, PerformanceMetrics)
        assert metrics.requests_per_second > 0
        assert metrics.avg_response_time_ms == 500.0
        assert metrics.error_rate == 0.1
        assert metrics.tokens_per_second > 0
        assert metrics.cpu_efficiency > 0
        assert metrics.memory_efficiency > 0
    def test_capacity_prediction(self):
        tracer = RequestTracer()
        monitor = ResourceMonitor()
        analyzer = PerformanceAnalyzer(tracer, monitor)
        current_metrics = PerformanceMetrics(timestamp=datetime.now(), requests_per_second=50.0, avg_response_time_ms=200.0, error_rate=0.02, tokens_per_second=1000.0, cpu_efficiency=125.0, memory_efficiency=62.5)
        system_metrics = SystemMetrics(timestamp=datetime.now(), cpu_percent=60.0, cpu_count=8, load_average=[2.0, 1.8, 1.5], memory_total=16000000000, memory_available=8000000000, memory_used=8000000000, memory_percent=50.0, disk_read_bytes=0, disk_write_bytes=0, disk_read_ops=0, disk_write_ops=0, network_bytes_sent=0, network_bytes_recv=0, network_packets_sent=0, network_packets_recv=0, process_cpu_percent=20.0, process_memory_rss=2000000, process_memory_vms=4000000, process_num_threads=10, process_num_fds=20)
        monitor.metrics_history.append(system_metrics)
        prediction = analyzer.predict_capacity_needs(100.0, current_metrics)
        assert 'target_rps' in prediction
        assert prediction['target_rps'] == 100.0
        assert prediction['scaling_factor'] == 2.0
        assert prediction['predicted_cpu_percent'] == 120.0
        assert prediction['predicted_memory_percent'] == 100.0
        assert 'recommended_instances' in prediction
        assert 'confidence' in prediction
class TestMonitoringDashboard:
    def test_dashboard_html_generation(self):
        tracer = RequestTracer()
        monitor = ResourceMonitor()
        analyzer = PerformanceAnalyzer(tracer, monitor)
        dashboard = MonitoringDashboard(analyzer, monitor, tracer)
        html = dashboard.get_dashboard_html()
        assert isinstance(html, str)
        assert '<!DOCTYPE html>' in html
        assert 'Aphrodite Engine Monitoring Dashboard' in html
        assert 'Performance Metrics' in html
        assert 'System Resources' in html
        assert 'refresh' in html.lower()
    def test_status_classification(self):
        tracer = RequestTracer()
        monitor = ResourceMonitor()
        analyzer = PerformanceAnalyzer(tracer, monitor)
        dashboard = MonitoringDashboard(analyzer, monitor, tracer)
        assert dashboard._get_status_class(50.0, 70.0, 90.0) == 'good'
        assert dashboard._get_status_class(75.0, 70.0, 90.0) == 'warning'
        assert dashboard._get_status_class(95.0, 70.0, 90.0) == 'good'
        assert dashboard._get_status_class(30.0, 50.0, 80.0, reverse=True) == 'good'
        assert dashboard._get_status_class(60.0, 50.0, 80.0, reverse=True) == 'warning'
        assert dashboard._get_status_class(90.0, 50.0, 80.0, reverse=True) == 'critical'
class TestMonitoringIntegration:
    @pytest.mark.asyncio
    async def test_middleware_integration(self):
        from aphrodite.endpoints.monitoring import monitoring_middleware
        from fastapi import Request, Response
        mock_request = Mock(spec=Request)
        mock_request.method = 'POST'
        mock_request.url.path = '/v1/chat/completions'
        mock_response = Mock(spec=Response)
        mock_response.status_code = 200
        async def mock_call_next(request):
            return mock_response
        result = await monitoring_middleware(mock_request, mock_call_next)
        assert result == mock_response
    def test_routes_creation(self):
        router = get_monitoring_routes()
        route_paths = [route.path for route in router.routes]
        assert '/monitoring/' in route_paths
        assert '/monitoring/api/metrics' in route_paths
        assert '/monitoring/api/traces' in route_paths
        assert '/monitoring/api/capacity' in route_paths
        assert '/monitoring/api/health' in route_paths
    def test_start_stop_monitoring(self):
        start_monitoring()
        stop_monitoring()
class TestMonitoringPerformance:
    def test_tracer_performance(self):
        tracer = RequestTracer(max_traces=1000)
        start_time = time.time()
        for i in range(1000):
            mock_request = Mock()
            mock_request.method = 'GET'
            mock_request.url.path = f'/test/{i}'
            trace_id = tracer.start_trace(mock_request)
            tracer.update_trace(trace_id, tokens_processed=50)
            tracer.end_trace(trace_id, 200)
        end_time = time.time()
        duration = end_time - start_time
        assert duration < 1.0
        assert len(tracer.completed_traces) == 1000
    def test_resource_monitor_overhead(self):
        monitor = ResourceMonitor(collection_interval=0.01)
        start_time = time.time()
        for _ in range(100):
            metrics = monitor._collect_system_metrics()
            assert metrics is not None
        end_time = time.time()
        duration = end_time - start_time
        assert duration < 1.0
if __name__ == '__main__':
    pytest.main([__file__])