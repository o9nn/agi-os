import time
import logging
import threading
import statistics
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Callable, Any, Tuple
from enum import Enum
from collections import deque, defaultdict
import numpy as np
logger = logging.getLogger(__name__)
class SLAViolationType(Enum):
    LATENCY_BREACH = 'latency_breach'
    THROUGHPUT_DEGRADATION = 'throughput_degradation'
    ERROR_RATE_SPIKE = 'error_rate_spike'
    AVAILABILITY_LOSS = 'availability_loss'
    RESOURCE_EXHAUSTION = 'resource_exhaustion'
    PERFORMANCE_REGRESSION = 'performance_regression'
class ViolationSeverity(Enum):
    MINOR = 'minor'
    MAJOR = 'major'
    CRITICAL = 'critical'
@dataclass
class SLAThreshold:
    metric_name: str
    target_value: float
    tolerance_percent: float = 5.0
    measurement_window_minutes: int = 5
    violation_threshold_percent: int = 80
    enabled: bool = True
    def get_violation_boundary(self, is_upper_bound: bool=True) -> float:
        if is_upper_bound:
            return self.target_value * (1 + self.tolerance_percent / 100)
        else:
            return self.target_value * (1 - self.tolerance_percent / 100)
@dataclass
class SLAViolation:
    violation_id: str
    timestamp: float
    violation_type: SLAViolationType
    severity: ViolationSeverity
    metric_name: str
    threshold: SLAThreshold
    actual_value: float
    expected_value: float
    breach_percentage: float
    measurement_window: List[float]
    context: Dict[str, Any] = field(default_factory=dict)
    resolved_timestamp: Optional[float] = None
    @property
    def duration_seconds(self) -> Optional[float]:
        if self.resolved_timestamp:
            return self.resolved_timestamp - self.timestamp
        return None
class StatisticalAnalyzer:
    def __init__(self, baseline_window_hours: int=24):
        self.baseline_window_hours = baseline_window_hours
        self.baseline_data: Dict[str, deque] = defaultdict(lambda: deque(maxlen=1440))
    def add_measurement(self, metric_name: str, value: float, timestamp: float):
        self.baseline_data[metric_name].append((timestamp, value))
    def get_baseline_stats(self, metric_name: str) -> Tuple[float, float, float]:
        if metric_name not in self.baseline_data or len(self.baseline_data[metric_name]) < 10:
            return (0.0, 0.0, 0.0)
        values = [v for t, v in self.baseline_data[metric_name]]
        mean_val = statistics.mean(values)
        std_val = statistics.stdev(values) if len(values) > 1 else 0.0
        if len(values) >= 5:
            x = list(range(len(values)))
            y = values
            n = len(x)
            sum_x = sum(x)
            sum_y = sum(y)
            sum_xy = sum((x[i] * y[i] for i in range(n)))
            sum_x2 = sum((x[i] ** 2 for i in range(n)))
            trend = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x ** 2) if n * sum_x2 - sum_x ** 2 != 0 else 0.0
        else:
            trend = 0.0
        return (mean_val, std_val, trend)
    def is_anomaly(self, metric_name: str, value: float, z_threshold: float=2.5) -> Tuple[bool, float]:
        mean_val, std_val, _ = self.get_baseline_stats(metric_name)
        if std_val == 0:
            return (False, 0.0)
        z_score = abs(value - mean_val) / std_val
        return (z_score > z_threshold, z_score)
    def detect_performance_regression(self, metric_name: str, recent_window_minutes: int=15) -> Tuple[bool, float]:
        if metric_name not in self.baseline_data:
            return (False, 0.0)
        data = list(self.baseline_data[metric_name])
        if len(data) < 30:
            return (False, 0.0)
        current_time = time.time()
        cutoff_time = current_time - recent_window_minutes * 60
        recent_values = [v for t, v in data if t >= cutoff_time]
        if len(recent_values) < 5:
            return (False, 0.0)
        baseline_values = [v for t, v in data if t < cutoff_time]
        if len(baseline_values) < 10:
            return (False, 0.0)
        recent_mean = statistics.mean(recent_values)
        baseline_mean = statistics.mean(baseline_values)
        if baseline_mean == 0:
            return (False, 0.0)
        change_percent = (recent_mean - baseline_mean) / baseline_mean * 100
        regression_threshold = 10.0
        is_regression = abs(change_percent) > regression_threshold
        return (is_regression, change_percent)
class SLAManager:
    def __init__(self):
        self.thresholds: Dict[str, SLAThreshold] = {}
        self.violations: List[SLAViolation] = []
        self.active_violations: Dict[str, SLAViolation] = {}
        self.measurement_windows: Dict[str, deque] = defaultdict(lambda: deque(maxlen=300))
        self.violation_callbacks: List[Callable[[SLAViolation], None]] = []
        self.statistical_analyzer = StatisticalAnalyzer()
        self.is_monitoring = False
        self._lock = threading.RLock()
        self._monitoring_thread = None
        self._initialize_default_thresholds()
        logger.info('SLA Manager initialized with statistical anomaly detection')
    def _initialize_default_thresholds(self):
        self.add_threshold(SLAThreshold(metric_name='request_latency_p95', target_value=200.0, tolerance_percent=25.0, measurement_window_minutes=5))
        self.add_threshold(SLAThreshold(metric_name='request_latency_p99', target_value=500.0, tolerance_percent=50.0, measurement_window_minutes=5))
        self.add_threshold(SLAThreshold(metric_name='tokens_per_second', target_value=100.0, tolerance_percent=20.0, measurement_window_minutes=3))
        self.add_threshold(SLAThreshold(metric_name='error_rate_percent', target_value=0.1, tolerance_percent=400.0, measurement_window_minutes=5))
        self.add_threshold(SLAThreshold(metric_name='gpu_utilization_percent', target_value=85.0, tolerance_percent=10.0, measurement_window_minutes=10))
        self.add_threshold(SLAThreshold(metric_name='kv_cache_usage_percent', target_value=80.0, tolerance_percent=20.0, measurement_window_minutes=5))
        self.add_threshold(SLAThreshold(metric_name='echo_evolution_convergence', target_value=0.85, tolerance_percent=15.0, measurement_window_minutes=15))
        logger.info(f'Initialized {len(self.thresholds)} default SLA thresholds')
    def add_threshold(self, threshold: SLAThreshold):
        with self._lock:
            self.thresholds[threshold.metric_name] = threshold
        logger.info(f'Added SLA threshold for {threshold.metric_name}: {threshold.target_value}')
    def remove_threshold(self, metric_name: str):
        with self._lock:
            if metric_name in self.thresholds:
                del self.thresholds[metric_name]
        logger.info(f'Removed SLA threshold for {metric_name}')
    def register_violation_callback(self, callback: Callable[[SLAViolation], None]):
        self.violation_callbacks.append(callback)
        logger.info('Registered SLA violation callback')
    def start_monitoring(self):
        if self.is_monitoring:
            logger.warning('SLA monitoring already running')
            return
        self.is_monitoring = True
        self._monitoring_thread = threading.Thread(target=self._monitoring_loop, daemon=True, name='SLAMonitoring')
        self._monitoring_thread.start()
        logger.info('SLA monitoring started')
    def stop_monitoring(self):
        self.is_monitoring = False
        if self._monitoring_thread:
            self._monitoring_thread.join(timeout=5.0)
        logger.info('SLA monitoring stopped')
    def record_measurement(self, metric_name: str, value: float, timestamp: Optional[float]=None):
        if timestamp is None:
            timestamp = time.time()
        with self._lock:
            self.measurement_windows[metric_name].append((timestamp, value))
            self.statistical_analyzer.add_measurement(metric_name, value, timestamp)
            if metric_name in self.thresholds:
                self._check_threshold_violation(metric_name, value, timestamp)
    def _monitoring_loop(self):
        logger.info('SLA monitoring loop started')
        while self.is_monitoring:
            try:
                current_time = time.time()
                self._check_performance_regressions(current_time)
                self._check_resolved_violations(current_time)
                time.sleep(30)
            except Exception as e:
                logger.error(f'Error in SLA monitoring loop: {e}')
                time.sleep(60)
        logger.info('SLA monitoring loop stopped')
    def _check_threshold_violation(self, metric_name: str, value: float, timestamp: float):
        threshold = self.thresholds[metric_name]
        if not threshold.enabled:
            return
        is_upper_bound = self._is_upper_bound_metric(metric_name)
        violation_boundary = threshold.get_violation_boundary(is_upper_bound)
        is_violation = False
        if is_upper_bound:
            is_violation = value > violation_boundary
        else:
            is_violation = value < violation_boundary
        if not is_violation:
            if metric_name in self.active_violations:
                self._check_violation_resolution(metric_name, value, timestamp)
            return
        window_data = list(self.measurement_windows[metric_name])
        window_minutes = threshold.measurement_window_minutes
        cutoff_time = timestamp - window_minutes * 60
        window_measurements = [(t, v) for t, v in window_data if t >= cutoff_time]
        if len(window_measurements) < 5:
            return
        violation_count = 0
        for _, window_value in window_measurements:
            if is_upper_bound and window_value > violation_boundary:
                violation_count += 1
            elif not is_upper_bound and window_value < violation_boundary:
                violation_count += 1
        violation_percentage = violation_count / len(window_measurements) * 100
        if violation_percentage >= threshold.violation_threshold_percent and metric_name not in self.active_violations:
            self._create_violation(metric_name, value, timestamp, threshold, window_measurements)
    def _check_performance_regressions(self, current_time: float):
        with self._lock:
            for metric_name in self.measurement_windows.keys():
                try:
                    is_regression, change_percent = self.statistical_analyzer.detect_performance_regression(metric_name, recent_window_minutes=15)
                    if is_regression and metric_name not in self.active_violations:
                        violation_id = f'regression_{metric_name}_{int(current_time)}'
                        violation = SLAViolation(violation_id=violation_id, timestamp=current_time, violation_type=SLAViolationType.PERFORMANCE_REGRESSION, severity=self._calculate_regression_severity(change_percent), metric_name=metric_name, threshold=self.thresholds.get(metric_name, SLAThreshold(metric_name, 0.0)), actual_value=0.0, expected_value=0.0, breach_percentage=abs(change_percent), measurement_window=[], context={'regression_type': 'statistical', 'change_percent': change_percent, 'detection_method': 'baseline_comparison'})
                        self._register_violation(violation)
                except Exception as e:
                    logger.error(f'Error checking regression for {metric_name}: {e}')
    def _check_resolved_violations(self, current_time: float):
        resolved_violations = []
        with self._lock:
            for metric_name, violation in self.active_violations.items():
                if self._is_violation_resolved(metric_name, violation, current_time):
                    violation.resolved_timestamp = current_time
                    resolved_violations.append(metric_name)
                    logger.info(f'SLA violation resolved: {violation.violation_id} after {violation.duration_seconds:.1f}s')
            for metric_name in resolved_violations:
                del self.active_violations[metric_name]
    def _create_violation(self, metric_name: str, value: float, timestamp: float, threshold: SLAThreshold, window_measurements: List[Tuple[float, float]]):
        violation_id = f'sla_{metric_name}_{int(timestamp)}'
        is_upper_bound = self._is_upper_bound_metric(metric_name)
        boundary = threshold.get_violation_boundary(is_upper_bound)
        if is_upper_bound:
            breach_percent = (value - boundary) / boundary * 100
        else:
            breach_percent = (boundary - value) / boundary * 100
        violation_type = self._determine_violation_type(metric_name, value, threshold)
        severity = self._calculate_severity(breach_percent)
        violation = SLAViolation(violation_id=violation_id, timestamp=timestamp, violation_type=violation_type, severity=severity, metric_name=metric_name, threshold=threshold, actual_value=value, expected_value=threshold.target_value, breach_percentage=breach_percent, measurement_window=[v for t, v in window_measurements], context={'detection_method': 'threshold_based', 'window_size_minutes': threshold.measurement_window_minutes})
        self._register_violation(violation)
    def _register_violation(self, violation: SLAViolation):
        self.violations.append(violation)
        self.active_violations[violation.metric_name] = violation
        logger.warning(f'SLA violation detected: {violation.violation_id} - {violation.metric_name} = {violation.actual_value} (expected {violation.expected_value}, breach: {violation.breach_percentage:.1f}%)')
        for callback in self.violation_callbacks:
            try:
                callback(violation)
            except Exception as e:
                logger.error(f'Error in violation callback: {e}')
    def _is_violation_resolved(self, metric_name: str, violation: SLAViolation, current_time: float) -> bool:
        threshold = violation.threshold
        window_minutes = max(threshold.measurement_window_minutes, 2)
        cutoff_time = current_time - window_minutes * 60
        window_data = list(self.measurement_windows[metric_name])
        recent_measurements = [(t, v) for t, v in window_data if t >= cutoff_time]
        if len(recent_measurements) < 3:
            return False
        is_upper_bound = self._is_upper_bound_metric(metric_name)
        boundary = threshold.get_violation_boundary(is_upper_bound)
        compliant_count = 0
        for _, value in recent_measurements:
            if is_upper_bound:
                if value <= boundary:
                    compliant_count += 1
            elif value >= boundary:
                compliant_count += 1
        compliance_percentage = compliant_count / len(recent_measurements) * 100
        return compliance_percentage >= 80.0
    def _is_upper_bound_metric(self, metric_name: str) -> bool:
        upper_bound_metrics = ['latency', 'response_time', 'error_rate', 'cpu_usage', 'memory_usage', 'gpu_utilization', 'kv_cache_usage']
        return any((keyword in metric_name.lower() for keyword in upper_bound_metrics))
    def _determine_violation_type(self, metric_name: str, value: float, threshold: SLAThreshold) -> SLAViolationType:
        metric_lower = metric_name.lower()
        if 'latency' in metric_lower or 'response_time' in metric_lower:
            return SLAViolationType.LATENCY_BREACH
        elif 'throughput' in metric_lower or 'tokens_per_second' in metric_lower:
            return SLAViolationType.THROUGHPUT_DEGRADATION
        elif 'error_rate' in metric_lower:
            return SLAViolationType.ERROR_RATE_SPIKE
        elif 'cpu' in metric_lower or 'memory' in metric_lower or 'gpu' in metric_lower:
            return SLAViolationType.RESOURCE_EXHAUSTION
        else:
            return SLAViolationType.PERFORMANCE_REGRESSION
    def _calculate_severity(self, breach_percentage: float) -> ViolationSeverity:
        if breach_percentage < 5.0:
            return ViolationSeverity.MINOR
        elif breach_percentage < 15.0:
            return ViolationSeverity.MAJOR
        else:
            return ViolationSeverity.CRITICAL
    def _calculate_regression_severity(self, change_percent: float) -> ViolationSeverity:
        abs_change = abs(change_percent)
        if abs_change < 15.0:
            return ViolationSeverity.MINOR
        elif abs_change < 30.0:
            return ViolationSeverity.MAJOR
        else:
            return ViolationSeverity.CRITICAL
    def get_active_violations(self) -> List[SLAViolation]:
        with self._lock:
            return list(self.active_violations.values())
    def get_violation_history(self, hours: int=24) -> List[SLAViolation]:
        cutoff_time = time.time() - hours * 3600
        return [v for v in self.violations if v.timestamp >= cutoff_time]
    def get_sla_summary(self) -> Dict[str, Any]:
        with self._lock:
            active_count = len(self.active_violations)
            recent_violations = self.get_violation_history(24)
            severity_counts = {severity.value: 0 for severity in ViolationSeverity}
            for violation in recent_violations:
                severity_counts[violation.severity.value] += 1
            type_counts = {vtype.value: 0 for vtype in SLAViolationType}
            for violation in recent_violations:
                type_counts[violation.violation_type.value] += 1
            total_measurements = sum((len(window) for window in self.measurement_windows.values()))
            violation_measurements = sum((len(v.measurement_window) for v in recent_violations))
            compliance_rate = (total_measurements - violation_measurements) / max(total_measurements, 1) * 100
            return {'monitoring_active': self.is_monitoring, 'active_violations': active_count, 'total_violations_24h': len(recent_violations), 'compliance_rate_percent': compliance_rate, 'thresholds_configured': len(self.thresholds), 'severity_breakdown': severity_counts, 'violation_type_breakdown': type_counts, 'metrics_monitored': list(self.thresholds.keys())}
def create_production_sla_manager() -> SLAManager:
    return SLAManager()
if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    print('🎯 Production SLA Manager Demo')
    print('=' * 50)
    sla_manager = create_production_sla_manager()
    def violation_handler(violation: SLAViolation):
        print(f'🚨 SLA Violation: {violation.metric_name} = {violation.actual_value} (expected {violation.expected_value}, severity: {violation.severity.value})')
    sla_manager.register_violation_callback(violation_handler)
    try:
        sla_manager.start_monitoring()
        print('✅ SLA monitoring started')
        print('\n📊 Simulating measurements...')
        for i in range(10):
            sla_manager.record_measurement('request_latency_p95', 150.0 + i * 5)
            time.sleep(0.1)
        print('🔥 Simulating SLA violation...')
        for i in range(5):
            sla_manager.record_measurement('request_latency_p95', 300.0)
            time.sleep(0.1)
        time.sleep(2)
        summary = sla_manager.get_sla_summary()
        print(f'\n📈 SLA Summary:')
        print(f"   Active violations: {summary['active_violations']}")
        print(f"   Compliance rate: {summary['compliance_rate_percent']:.1f}%")
        print(f"   Thresholds configured: {summary['thresholds_configured']}")
    finally:
        sla_manager.stop_monitoring()
        print('🛑 SLA monitoring stopped')