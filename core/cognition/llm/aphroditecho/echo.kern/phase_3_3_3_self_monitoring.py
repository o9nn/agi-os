import time
import logging
import statistics
from typing import Dict, List, Any, Optional, Tuple, Callable
from dataclasses import dataclass, field
from datetime import datetime, timezone
from enum import Enum
import json
import threading
from collections import defaultdict, deque
logger = logging.getLogger(__name__)
class MonitoringLevel(Enum):
    BASIC = 'basic'
    DETAILED = 'detailed'
    COMPREHENSIVE = 'comprehensive'
class ErrorSeverity(Enum):
    LOW = 'low'
    MEDIUM = 'medium'
    HIGH = 'high'
    CRITICAL = 'critical'
class CorrectionStrategy(Enum):
    AUTOMATIC = 'automatic'
    GUIDED = 'guided'
    MANUAL = 'manual'
    ESCALATE = 'escalate'
@dataclass
class PerformanceMetrics:
    timestamp: float = field(default_factory=time.time)
    response_time_ms: float = 0.0
    accuracy_score: float = 0.0
    efficiency_ratio: float = 0.0
    resource_utilization: float = 0.0
    error_rate: float = 0.0
    throughput: float = 0.0
    success_rate: float = 1.0
    metadata: Dict[str, Any] = field(default_factory=dict)
    def to_dict(self) -> Dict[str, Any]:
        return {'timestamp': self.timestamp, 'response_time_ms': self.response_time_ms, 'accuracy_score': self.accuracy_score, 'efficiency_ratio': self.efficiency_ratio, 'resource_utilization': self.resource_utilization, 'error_rate': self.error_rate, 'throughput': self.throughput, 'success_rate': self.success_rate, 'metadata': self.metadata}
@dataclass
class BodyStateMetrics:
    timestamp: float = field(default_factory=time.time)
    joint_positions: Dict[str, float] = field(default_factory=dict)
    joint_velocities: Dict[str, float] = field(default_factory=dict)
    body_orientation: Tuple[float, float, float] = (0.0, 0.0, 0.0)
    center_of_mass: Tuple[float, float, float] = (0.0, 0.0, 0.0)
    balance_state: float = 1.0
    movement_execution_error: float = 0.0
    proprioceptive_feedback: Dict[str, float] = field(default_factory=dict)
    def to_dict(self) -> Dict[str, Any]:
        return {'timestamp': self.timestamp, 'joint_positions': self.joint_positions, 'joint_velocities': self.joint_velocities, 'body_orientation': self.body_orientation, 'center_of_mass': self.center_of_mass, 'balance_state': self.balance_state, 'movement_execution_error': self.movement_execution_error, 'proprioceptive_feedback': self.proprioceptive_feedback}
@dataclass
class DetectedError:
    error_id: str
    timestamp: float
    error_type: str
    severity: ErrorSeverity
    description: str
    context: Dict[str, Any]
    correction_applied: Optional[str] = None
    correction_successful: bool = False
    learning_feedback: Optional[str] = None
    def to_dict(self) -> Dict[str, Any]:
        return {'error_id': self.error_id, 'timestamp': self.timestamp, 'error_type': self.error_type, 'severity': self.severity.value, 'description': self.description, 'context': self.context, 'correction_applied': self.correction_applied, 'correction_successful': self.correction_successful, 'learning_feedback': self.learning_feedback}
class SelfMonitoringSystem:
    def __init__(self, agent_id: str, monitoring_level: MonitoringLevel=MonitoringLevel.DETAILED, max_history_size: int=1000, performance_thresholds: Optional[Dict[str, float]]=None):
        self.agent_id = agent_id
        self.monitoring_level = monitoring_level
        self.max_history_size = max_history_size
        self.is_monitoring = False
        self.performance_history: deque = deque(maxlen=max_history_size)
        self.performance_baselines: Dict[str, float] = {}
        self.performance_thresholds = performance_thresholds or {'response_time_ms': 1000.0, 'accuracy_score': 0.8, 'efficiency_ratio': 0.7, 'error_rate': 0.05, 'success_rate': 0.95}
        self.detected_errors: deque = deque(maxlen=max_history_size)
        self.error_patterns: Dict[str, int] = defaultdict(int)
        self.correction_strategies: Dict[str, Callable] = {}
        self.auto_correction_enabled = True
        self.body_state_history: deque = deque(maxlen=max_history_size)
        self.body_state_baselines: Dict[str, float] = {}
        self.proprioceptive_thresholds = {'balance_threshold': 0.3, 'movement_error_threshold': 0.1, 'joint_velocity_limit': 10.0}
        self.monitoring_thread: Optional[threading.Thread] = None
        self.monitoring_lock = threading.Lock()
        self.learning_enabled = True
        self.adaptation_rate = 0.1
        logger.info(f'Self-monitoring system initialized for agent {agent_id}')
    def start_monitoring(self, monitoring_interval: float=0.1):
        if self.is_monitoring:
            logger.warning(f'Monitoring already active for agent {self.agent_id}')
            return
        self.is_monitoring = True
        def monitoring_loop():
            while self.is_monitoring:
                try:
                    self._perform_monitoring_cycle()
                except Exception as e:
                    logger.error(f'Monitoring cycle error for agent {self.agent_id}: {e}')
                time.sleep(monitoring_interval)
        self.monitoring_thread = threading.Thread(target=monitoring_loop, daemon=True)
        self.monitoring_thread.start()
        logger.info(f'Started self-monitoring for agent {self.agent_id} with {monitoring_interval}s interval')
    def stop_monitoring(self):
        if not self.is_monitoring:
            return
        self.is_monitoring = False
        if self.monitoring_thread:
            self.monitoring_thread.join(timeout=2.0)
        logger.info(f'Stopped self-monitoring for agent {self.agent_id}')
    def _perform_monitoring_cycle(self):
        with self.monitoring_lock:
            current_metrics = self._collect_performance_metrics()
            if current_metrics:
                self.performance_history.append(current_metrics)
                self._assess_performance(current_metrics)
            if self.monitoring_level in [MonitoringLevel.DETAILED, MonitoringLevel.COMPREHENSIVE]:
                body_state = self._collect_body_state_metrics()
                if body_state:
                    self.body_state_history.append(body_state)
                    self._assess_body_state(body_state)
            self._detect_errors()
            if self.learning_enabled:
                self._update_baselines()
                self._adapt_thresholds()
    def _collect_performance_metrics(self) -> Optional[PerformanceMetrics]:
        try:
            import random
            metrics = PerformanceMetrics(response_time_ms=random.uniform(50, 200), accuracy_score=random.uniform(0.85, 0.98), efficiency_ratio=random.uniform(0.7, 0.95), resource_utilization=random.uniform(0.2, 0.8), error_rate=random.uniform(0.0, 0.02), throughput=random.uniform(50, 100), success_rate=random.uniform(0.95, 1.0), metadata={'agent_id': self.agent_id, 'monitoring_level': self.monitoring_level.value})
            return metrics
        except Exception as e:
            logger.error(f'Error collecting performance metrics for agent {self.agent_id}: {e}')
            return None
    def _collect_body_state_metrics(self) -> Optional[BodyStateMetrics]:
        try:
            import random
            body_state = BodyStateMetrics(joint_positions={'shoulder_left': random.uniform(-1.5, 1.5), 'shoulder_right': random.uniform(-1.5, 1.5), 'elbow_left': random.uniform(-2.0, 2.0), 'elbow_right': random.uniform(-2.0, 2.0)}, joint_velocities={'shoulder_left': random.uniform(-2.0, 2.0), 'shoulder_right': random.uniform(-2.0, 2.0), 'elbow_left': random.uniform(-3.0, 3.0), 'elbow_right': random.uniform(-3.0, 3.0)}, body_orientation=(random.uniform(-0.1, 0.1), random.uniform(-0.1, 0.1), random.uniform(-0.1, 0.1)), balance_state=random.uniform(0.8, 1.0), movement_execution_error=random.uniform(0.0, 0.05), proprioceptive_feedback={'force_sensors': random.uniform(0.0, 1.0), 'pressure_sensors': random.uniform(0.0, 1.0)})
            return body_state
        except Exception as e:
            logger.error(f'Error collecting body state metrics for agent {self.agent_id}: {e}')
            return None
    def _assess_performance(self, metrics: PerformanceMetrics):
        assessment_results = {}
        for metric_name, threshold in self.performance_thresholds.items():
            current_value = getattr(metrics, metric_name, None)
            if current_value is not None:
                if metric_name in ['error_rate']:
                    assessment_results[metric_name] = current_value <= threshold
                else:
                    assessment_results[metric_name] = current_value >= threshold
        recommendations = self._generate_performance_recommendations(metrics, assessment_results)
        if recommendations:
            logger.info(f'Performance recommendations for agent {self.agent_id}: {recommendations}')
    def _assess_body_state(self, body_state: BodyStateMetrics):
        issues = []
        if body_state.balance_state < self.proprioceptive_thresholds['balance_threshold']:
            issues.append({'type': 'balance_issue', 'severity': ErrorSeverity.HIGH, 'description': f'Balance state {body_state.balance_state:.3f} below threshold'})
        if body_state.movement_execution_error > self.proprioceptive_thresholds['movement_error_threshold']:
            issues.append({'type': 'movement_execution_error', 'severity': ErrorSeverity.MEDIUM, 'description': f'Movement execution error {body_state.movement_execution_error:.3f} above threshold'})
        for joint, velocity in body_state.joint_velocities.items():
            if abs(velocity) > self.proprioceptive_thresholds['joint_velocity_limit']:
                issues.append({'type': 'excessive_joint_velocity', 'severity': ErrorSeverity.MEDIUM, 'description': f'Joint {joint} velocity {velocity:.3f} exceeds safe limit'})
        for issue in issues:
            self._handle_detected_issue(issue, body_state)
    def _detect_errors(self):
        errors_detected = []
        if len(self.performance_history) >= 5:
            recent_metrics = list(self.performance_history)[-5:]
            if self._detect_performance_degradation(recent_metrics):
                errors_detected.append({'type': 'performance_degradation', 'severity': ErrorSeverity.MEDIUM, 'description': 'Performance degradation detected in recent metrics'})
            recent_error_rates = [m.error_rate for m in recent_metrics]
            if statistics.mean(recent_error_rates) > self.performance_thresholds['error_rate']:
                errors_detected.append({'type': 'error_rate_spike', 'severity': ErrorSeverity.HIGH, 'description': f'Error rate spike: {statistics.mean(recent_error_rates):.4f}'})
        if len(self.body_state_history) >= 3:
            recent_states = list(self.body_state_history)[-3:]
            balance_trend = [s.balance_state for s in recent_states]
            if all((b < self.proprioceptive_thresholds['balance_threshold'] for b in balance_trend)):
                errors_detected.append({'type': 'persistent_instability', 'severity': ErrorSeverity.CRITICAL, 'description': 'Persistent balance instability detected'})
        for error_info in errors_detected:
            self._create_and_handle_error(error_info)
    def _detect_performance_degradation(self, recent_metrics: List[PerformanceMetrics]) -> bool:
        if len(recent_metrics) < 3:
            return False
        accuracy_scores = [m.accuracy_score for m in recent_metrics]
        response_times = [m.response_time_ms for m in recent_metrics]
        mid_point = len(accuracy_scores) // 2
        early_accuracy = statistics.mean(accuracy_scores[:mid_point])
        recent_accuracy = statistics.mean(accuracy_scores[mid_point:])
        early_response = statistics.mean(response_times[:mid_point])
        recent_response = statistics.mean(response_times[mid_point:])
        accuracy_degraded = recent_accuracy < early_accuracy * 0.95
        response_degraded = recent_response > early_response * 1.1
        return accuracy_degraded or response_degraded
    def _create_and_handle_error(self, error_info: Dict[str, Any]):
        error = DetectedError(error_id=f'{self.agent_id}_{int(time.time() * 1000)}', timestamp=time.time(), error_type=error_info['type'], severity=error_info['severity'], description=error_info['description'], context={'agent_id': self.agent_id})
        self.detected_errors.append(error)
        self.error_patterns[error.error_type] += 1
        if self.auto_correction_enabled:
            self._attempt_error_correction(error)
        logger.warning(f'Error detected for agent {self.agent_id}: {error.description}')
    def _handle_detected_issue(self, issue: Dict[str, Any], body_state: BodyStateMetrics):
        error_info = {'type': issue['type'], 'severity': issue['severity'], 'description': issue['description']}
        self._create_and_handle_error(error_info)
    def _attempt_error_correction(self, error: DetectedError):
        correction_strategy = self._determine_correction_strategy(error)
        if correction_strategy == CorrectionStrategy.AUTOMATIC:
            correction_applied = self._apply_automatic_correction(error)
            if correction_applied:
                error.correction_applied = correction_applied
                error.correction_successful = True
                logger.info(f'Automatic correction applied for agent {self.agent_id}: {correction_applied}')
        elif correction_strategy == CorrectionStrategy.ESCALATE:
            logger.critical(f'Error requires escalation for agent {self.agent_id}: {error.description}')
    def _determine_correction_strategy(self, error: DetectedError) -> CorrectionStrategy:
        if error.severity == ErrorSeverity.CRITICAL:
            return CorrectionStrategy.ESCALATE
        elif error.error_type in ['performance_degradation', 'error_rate_spike']:
            return CorrectionStrategy.AUTOMATIC
        else:
            return CorrectionStrategy.GUIDED
    def _apply_automatic_correction(self, error: DetectedError) -> Optional[str]:
        correction_applied = None
        if error.error_type == 'performance_degradation':
            correction_applied = 'reset_performance_optimizations'
        elif error.error_type == 'error_rate_spike':
            correction_applied = 'increase_error_checking'
        elif error.error_type == 'balance_issue':
            correction_applied = 'stabilize_posture'
        elif error.error_type == 'excessive_joint_velocity':
            correction_applied = 'reduce_joint_velocities'
        return correction_applied
    def _generate_performance_recommendations(self, metrics: PerformanceMetrics, assessment: Dict[str, bool]) -> List[str]:
        recommendations = []
        if not assessment.get('response_time_ms', True):
            recommendations.append('Optimize response time - consider caching or algorithm improvements')
        if not assessment.get('accuracy_score', True):
            recommendations.append('Improve accuracy - review training data or model parameters')
        if not assessment.get('efficiency_ratio', True):
            recommendations.append('Enhance efficiency - profile code for optimization opportunities')
        if metrics.resource_utilization > 0.9:
            recommendations.append('High resource utilization - consider load balancing or scaling')
        return recommendations
    def _update_baselines(self):
        if not self.learning_enabled:
            return
        if len(self.performance_history) >= 10:
            recent_metrics = list(self.performance_history)[-10:]
            for metric_name in ['response_time_ms', 'accuracy_score', 'efficiency_ratio']:
                values = [getattr(m, metric_name) for m in recent_metrics]
                new_baseline = statistics.mean(values)
                if metric_name in self.performance_baselines:
                    old_baseline = self.performance_baselines[metric_name]
                    self.performance_baselines[metric_name] = old_baseline * (1 - self.adaptation_rate) + new_baseline * self.adaptation_rate
                else:
                    self.performance_baselines[metric_name] = new_baseline
        if len(self.body_state_history) >= 10:
            recent_states = list(self.body_state_history)[-10:]
            balance_values = [s.balance_state for s in recent_states]
            new_balance_baseline = statistics.mean(balance_values)
            if 'balance_state' in self.body_state_baselines:
                old_baseline = self.body_state_baselines['balance_state']
                self.body_state_baselines['balance_state'] = old_baseline * (1 - self.adaptation_rate) + new_balance_baseline * self.adaptation_rate
            else:
                self.body_state_baselines['balance_state'] = new_balance_baseline
    def _adapt_thresholds(self):
        for error_type, count in self.error_patterns.items():
            if count > 5:
                if error_type == 'balance_issue':
                    current = self.proprioceptive_thresholds['balance_threshold']
                    self.proprioceptive_thresholds['balance_threshold'] = min(0.5, current * 1.1)
    def get_monitoring_status(self) -> Dict[str, Any]:
        with self.monitoring_lock:
            status = {'agent_id': self.agent_id, 'is_monitoring': self.is_monitoring, 'monitoring_level': self.monitoring_level.value, 'performance_metrics_count': len(self.performance_history), 'body_state_metrics_count': len(self.body_state_history), 'detected_errors_count': len(self.detected_errors), 'error_patterns': dict(self.error_patterns), 'performance_baselines': self.performance_baselines.copy(), 'body_state_baselines': self.body_state_baselines.copy(), 'auto_correction_enabled': self.auto_correction_enabled, 'learning_enabled': self.learning_enabled}
            if self.performance_history:
                recent_metrics = list(self.performance_history)[-5:]
                status['recent_performance'] = {'avg_response_time_ms': statistics.mean([m.response_time_ms for m in recent_metrics]), 'avg_accuracy_score': statistics.mean([m.accuracy_score for m in recent_metrics]), 'avg_success_rate': statistics.mean([m.success_rate for m in recent_metrics])}
            if self.body_state_history:
                recent_states = list(self.body_state_history)[-5:]
                status['recent_body_state'] = {'avg_balance_state': statistics.mean([s.balance_state for s in recent_states]), 'avg_movement_error': statistics.mean([s.movement_execution_error for s in recent_states])}
        return status
    def export_monitoring_data(self, output_path: str):
        export_data = {'export_timestamp': datetime.now(timezone.utc).isoformat(), 'agent_id': self.agent_id, 'monitoring_status': self.get_monitoring_status(), 'performance_history': [m.to_dict() for m in self.performance_history], 'body_state_history': [s.to_dict() for s in self.body_state_history], 'detected_errors': [e.to_dict() for e in self.detected_errors]}
        with open(output_path, 'w') as f:
            json.dump(export_data, f, indent=2)
        logger.info(f'Monitoring data exported to {output_path}')
    def reset_monitoring_data(self):
        with self.monitoring_lock:
            self.performance_history.clear()
            self.body_state_history.clear()
            self.detected_errors.clear()
            self.error_patterns.clear()
            self.performance_baselines.clear()
            self.body_state_baselines.clear()
        logger.info(f'Monitoring data reset for agent {self.agent_id}')
def create_self_monitoring_system(agent_id: str, monitoring_level: MonitoringLevel=MonitoringLevel.DETAILED) -> SelfMonitoringSystem:
    return SelfMonitoringSystem(agent_id=agent_id, monitoring_level=monitoring_level)
class DTESNSelfMonitoringIntegration:
    def __init__(self, dtesn_system=None):
        self.dtesn_system = dtesn_system
        self.monitoring_systems: Dict[str, SelfMonitoringSystem] = {}
    def register_agent_monitor(self, agent_id: str, monitoring_system: SelfMonitoringSystem):
        self.monitoring_systems[agent_id] = monitoring_system
    def get_system_wide_status(self) -> Dict[str, Any]:
        return {agent_id: system.get_monitoring_status() for agent_id, system in self.monitoring_systems.items()}
if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Run self-monitoring system demo')
    parser.add_argument('--agent-id', default='demo_agent', help='Agent ID for monitoring')
    parser.add_argument('--duration', type=int, default=30, help='Demo duration in seconds')
    parser.add_argument('--output', help='Output file for monitoring data export')
    args = parser.parse_args()
    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    monitor = create_self_monitoring_system(agent_id=args.agent_id, monitoring_level=MonitoringLevel.COMPREHENSIVE)
    try:
        monitor.start_monitoring(monitoring_interval=0.5)
        print(f'Running self-monitoring demo for {args.duration} seconds...')
        print(f'Agent ID: {args.agent_id}')
        print('Monitoring for performance, errors, and body state...')
        for i in range(args.duration):
            time.sleep(1)
            if i % 10 == 0:
                status = monitor.get_monitoring_status()
                print(f"Status update: {status['detected_errors_count']} errors, {status['performance_metrics_count']} performance samples")
        print('\nFinal monitoring status:')
        final_status = monitor.get_monitoring_status()
        print(json.dumps(final_status, indent=2))
        if args.output:
            monitor.export_monitoring_data(args.output)
            print(f'Monitoring data exported to {args.output}')
    finally:
        monitor.stop_monitoring()
        print('Monitoring demo completed.')