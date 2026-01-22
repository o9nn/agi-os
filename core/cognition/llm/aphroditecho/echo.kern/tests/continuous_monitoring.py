import sys
import os
import time
import threading
import json
import statistics
from typing import Dict, List, Any, Callable, Optional
from dataclasses import dataclass, asdict
from datetime import datetime, timezone
import requests
import queue
import signal
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from tests.real_time_test_framework import create_test_framework
@dataclass
class MonitoringData:
    timestamp: str
    metric_name: str
    value: float
    unit: str
    threshold: Optional[float]
    status: str
    metadata: Dict[str, Any]
class ContinuousMonitor:
    def __init__(self, interval_ms: int=100):
        self.interval_ms = interval_ms
        self.interval_s = interval_ms / 1000.0
        self.monitoring_active = False
        self.monitor_threads = {}
        self.data_queue = queue.Queue()
        self.historical_data: List[MonitoringData] = []
        self.alert_callbacks: List[Callable[[MonitoringData], None]] = []
        self.thresholds = {'web_response_time_ms': 100.0, 'javascript_load_time_ms': 50.0, 'server_cpu_percent': 80.0, 'memory_usage_percent': 90.0, 'error_rate_percent': 5.0, 'request_rate_per_second': 1000.0}
        self.metrics = {'web_response_times': [], 'error_counts': 0, 'request_counts': 0, 'last_error_time': None, 'uptime_start': time.time()}
        self.setup_signal_handlers()
    def setup_signal_handlers(self):
        def signal_handler(signum, frame):
            print(f'\nReceived signal {signum}, shutting down monitoring...')
            self.stop_all_monitoring()
            sys.exit(0)
        signal.signal(signal.SIGINT, signal_handler)
        signal.signal(signal.SIGTERM, signal_handler)
    def add_alert_callback(self, callback: Callable[[MonitoringData], None]):
        self.alert_callbacks.append(callback)
    def create_monitoring_data(self, metric_name: str, value: float, unit: str, threshold: Optional[float]=None, **metadata) -> MonitoringData:
        status = 'OK'
        if threshold is not None:
            if value > threshold:
                status = 'CRITICAL'
            elif value > threshold * 0.8:
                status = 'WARNING'
        return MonitoringData(timestamp=datetime.now(timezone.utc).isoformat(), metric_name=metric_name, value=value, unit=unit, threshold=threshold, status=status, metadata=metadata)
    def record_metric(self, data: MonitoringData):
        self.historical_data.append(data)
        self.data_queue.put(data)
        if data.status in ['WARNING', 'CRITICAL']:
            for callback in self.alert_callbacks:
                try:
                    callback(data)
                except Exception as e:
                    print(f'Alert callback error: {e}')
        if len(self.historical_data) > 1000:
            self.historical_data = self.historical_data[-1000:]
    def monitor_web_application(self, base_url: str='http://localhost:8000'):
        def web_monitor():
            while self.monitoring_active:
                try:
                    start_time = time.perf_counter()
                    response = requests.get(base_url, timeout=2.0)
                    end_time = time.perf_counter()
                    response_time_ms = (end_time - start_time) * 1000
                    data = self.create_monitoring_data('web_response_time_ms', response_time_ms, 'ms', self.thresholds['web_response_time_ms'], status_code=response.status_code, url=base_url)
                    self.record_metric(data)
                    self.metrics['web_response_times'].append(response_time_ms)
                    self.metrics['request_counts'] += 1
                    if len(self.metrics['web_response_times']) > 100:
                        self.metrics['web_response_times'] = self.metrics['web_response_times'][-100:]
                    if response.status_code != 200:
                        self.metrics['error_counts'] += 1
                        self.metrics['last_error_time'] = time.time()
                        error_data = self.create_monitoring_data('http_error', response.status_code, 'status_code', 200, url=base_url, error_type='http_status')
                        self.record_metric(error_data)
                except requests.exceptions.RequestException as e:
                    self.metrics['error_counts'] += 1
                    self.metrics['last_error_time'] = time.time()
                    error_data = self.create_monitoring_data('connection_error', 1, 'count', 0, url=base_url, error_type=type(e).__name__, error_message=str(e))
                    self.record_metric(error_data)
                except Exception as e:
                    print(f'Web monitor error: {e}')
                time.sleep(self.interval_s)
        if 'web_application' not in self.monitor_threads:
            self.monitor_threads['web_application'] = threading.Thread(target=web_monitor, daemon=True)
            self.monitor_threads['web_application'].start()
            print(f'Started web application monitoring (interval: {self.interval_ms}ms)')
    def monitor_system_resources(self):
        def resource_monitor():
            while self.monitoring_active:
                try:
                    import psutil
                    cpu_percent = psutil.cpu_percent(interval=None)
                    cpu_data = self.create_monitoring_data('cpu_usage_percent', cpu_percent, 'percent', self.thresholds['server_cpu_percent'])
                    self.record_metric(cpu_data)
                    memory = psutil.virtual_memory()
                    memory_data = self.create_monitoring_data('memory_usage_percent', memory.percent, 'percent', self.thresholds['memory_usage_percent'], total_gb=memory.total / 1024 ** 3, available_gb=memory.available / 1024 ** 3)
                    self.record_metric(memory_data)
                except ImportError:
                    pass
                except Exception as e:
                    print(f'Resource monitor error: {e}')
                time.sleep(self.interval_s * 10)
        try:
            import psutil
            if 'system_resources' not in self.monitor_threads:
                self.monitor_threads['system_resources'] = threading.Thread(target=resource_monitor, daemon=True)
                self.monitor_threads['system_resources'].start()
                print('Started system resource monitoring')
        except ImportError:
            print('psutil not available, skipping system resource monitoring')
    def monitor_dtesn_performance(self):
        def dtesn_monitor():
            from tests.performance_tests import DTESNPerformanceTests
            framework = create_test_framework()
            dtesn_tests = DTESNPerformanceTests(framework)
            while self.monitoring_active:
                try:
                    operations = [('membrane_evolution', dtesn_tests.test_membrane_evolution, 10.0), ('b_series_computation', lambda: dtesn_tests.test_b_series_computation(4), 100.0), ('esn_state_update', dtesn_tests.test_esn_state_update, 1000.0), ('oeis_validation', dtesn_tests.test_oeis_validation_performance, 1000.0)]
                    for op_name, op_func, threshold_us in operations:
                        start_time = time.perf_counter_ns()
                        try:
                            result = op_func()
                            success = True
                        except Exception:
                            result = False
                            success = False
                        end_time = time.perf_counter_ns()
                        execution_time_us = (end_time - start_time) / 1000.0
                        data = self.create_monitoring_data(f'dtesn_{op_name}_time_us', execution_time_us, 'μs', threshold_us, success=success, result=str(result))
                        self.record_metric(data)
                except Exception as e:
                    print(f'DTESN monitor error: {e}')
                time.sleep(self.interval_s * 50)
        if 'dtesn_performance' not in self.monitor_threads:
            self.monitor_threads['dtesn_performance'] = threading.Thread(target=dtesn_monitor, daemon=True)
            self.monitor_threads['dtesn_performance'].start()
            print('Started DTESN performance monitoring')
    def start_monitoring(self, base_url: str='http://localhost:8000', enable_system_monitoring: bool=True, enable_dtesn_monitoring: bool=True):
        if self.monitoring_active:
            print('Monitoring already active')
            return
        self.monitoring_active = True
        print('Starting Echo.Kern continuous monitoring...')
        self.monitor_web_application(base_url)
        if enable_system_monitoring:
            self.monitor_system_resources()
        if enable_dtesn_monitoring:
            self.monitor_dtesn_performance()
        print(f'Monitoring started with {self.interval_ms}ms interval')
    def stop_all_monitoring(self):
        self.monitoring_active = False
        print('Stopping all monitoring threads...')
        for name, thread in self.monitor_threads.items():
            if thread.is_alive():
                thread.join(timeout=2.0)
                print(f'Stopped {name} monitoring')
        self.monitor_threads.clear()
        print('All monitoring stopped')
    def get_real_time_status(self) -> Dict[str, Any]:
        current_time = time.time()
        uptime_seconds = current_time - self.metrics['uptime_start']
        recent_response_times = self.metrics['web_response_times'][-10:]
        avg_response_time = statistics.mean(recent_response_times) if recent_response_times else 0
        error_rate = 0
        if self.metrics['request_counts'] > 0:
            error_rate = self.metrics['error_counts'] / self.metrics['request_counts'] * 100
        return {'uptime_seconds': uptime_seconds, 'total_requests': self.metrics['request_counts'], 'total_errors': self.metrics['error_counts'], 'error_rate_percent': error_rate, 'avg_response_time_ms': avg_response_time, 'monitoring_active': self.monitoring_active, 'active_monitors': list(self.monitor_threads.keys()), 'historical_data_points': len(self.historical_data)}
    def generate_monitoring_report(self, output_file: Optional[str]=None) -> str:
        status = self.get_real_time_status()
        recent_data = self.historical_data[-100:] if self.historical_data else []
        critical_alerts = [d for d in recent_data if d.status == 'CRITICAL']
        warning_alerts = [d for d in recent_data if d.status == 'WARNING']
        report_lines = ['Echo.Kern Continuous Monitoring Report', '=' * 50, f'Generated: {datetime.now(timezone.utc).isoformat()}', f"Monitoring Active: {status['monitoring_active']}", f"Uptime: {status['uptime_seconds']:.1f} seconds", '', 'Performance Summary:', f"  Total Requests: {status['total_requests']}", f"  Total Errors: {status['total_errors']}", f"  Error Rate: {status['error_rate_percent']:.2f}%", f"  Avg Response Time: {status['avg_response_time_ms']:.2f}ms", '', 'Alert Summary:', f'  Critical Alerts: {len(critical_alerts)}', f'  Warning Alerts: {len(warning_alerts)}', '', 'Active Monitors:']
        for monitor in status['active_monitors']:
            report_lines.append(f'  ✅ {monitor}')
        if critical_alerts:
            report_lines.extend(['', 'Recent Critical Alerts:', '-' * 25])
            for alert in critical_alerts[-5:]:
                report_lines.append(f'❌ {alert.timestamp}: {alert.metric_name} = {alert.value} {alert.unit}')
        report = '\n'.join(report_lines)
        if output_file:
            with open(output_file, 'w') as f:
                f.write(report)
            print(f'Monitoring report saved to {output_file}')
        return report
    def export_monitoring_data(self, filename: str):
        data = {'export_time': datetime.now(timezone.utc).isoformat(), 'status': self.get_real_time_status(), 'thresholds': self.thresholds, 'historical_data': [asdict(d) for d in self.historical_data]}
        with open(filename, 'w') as f:
            json.dump(data, f, indent=2)
        print(f'Monitoring data exported to {filename}')
def default_alert_handler(data: MonitoringData):
    status_symbol = '⚠️' if data.status == 'WARNING' else '❌'
    print(f'{status_symbol} ALERT: {data.metric_name} = {data.value} {data.unit} (threshold: {data.threshold} {data.unit})')
def run_continuous_monitoring(interval_ms: int=100, duration_seconds: Optional[int]=None, output_file: Optional[str]=None) -> bool:
    print('Echo.Kern Continuous Monitoring System')
    print('=' * 50)
    monitor = ContinuousMonitor(interval_ms)
    monitor.add_alert_callback(default_alert_handler)
    try:
        monitor.start_monitoring()
        if duration_seconds:
            print(f'Running monitoring for {duration_seconds} seconds...')
            time.sleep(duration_seconds)
        else:
            print('Running continuous monitoring (Ctrl+C to stop)...')
            try:
                while True:
                    time.sleep(1)
                    if int(time.time()) % 10 == 0:
                        status = monitor.get_real_time_status()
                        print(f"Status: {status['total_requests']} requests, {status['error_rate_percent']:.1f}% error rate, {status['avg_response_time_ms']:.1f}ms avg response")
            except KeyboardInterrupt:
                print('\nShutdown requested...')
        print('\nGenerating final monitoring report...')
        report = monitor.generate_monitoring_report(output_file)
        print(report)
        return True
    except Exception as e:
        print(f'Monitoring error: {e}')
        return False
    finally:
        monitor.stop_all_monitoring()
if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Run continuous monitoring')
    parser.add_argument('--interval', type=int, default=100, help='Monitoring interval in milliseconds (default: 100)')
    parser.add_argument('--duration', type=int, help='Monitoring duration in seconds (default: continuous)')
    parser.add_argument('--output', type=str, help='Output file for monitoring report')
    args = parser.parse_args()
    success = run_continuous_monitoring(interval_ms=args.interval, duration_seconds=args.duration, output_file=args.output)
    exit(0 if success else 1)