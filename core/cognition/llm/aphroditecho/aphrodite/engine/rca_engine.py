import time
import logging
import threading
import json
import statistics
from dataclasses import dataclass, field, asdict
from typing import Dict, List, Optional, Tuple, Any, Set
from enum import Enum
from collections import defaultdict, deque
import numpy as np
from pathlib import Path
from aphrodite.engine.sla_manager import SLAViolation, ViolationSeverity
from aphrodite.engine.recovery_engine import RecoveryExecution
logger = logging.getLogger(__name__)
class CorrelationType(Enum):
    TEMPORAL = 'temporal'
    CAUSAL = 'causal'
    STATISTICAL = 'statistical'
    PATTERN = 'pattern'
    DEPENDENCY = 'dependency'
class RootCauseCategory(Enum):
    INFRASTRUCTURE = 'infrastructure'
    APPLICATION = 'application'
    NETWORK = 'network'
    RESOURCE_CONTENTION = 'resource_contention'
    CONFIGURATION = 'configuration'
    EXTERNAL_DEPENDENCY = 'external_dependency'
    CAPACITY_LIMITS = 'capacity_limits'
    CODE_REGRESSION = 'code_regression'
class ConfidenceLevel(Enum):
    LOW = 'low'
    MEDIUM = 'medium'
    HIGH = 'high'
    VERY_HIGH = 'very_high'
@dataclass
class MetricCorrelation:
    metric1: str
    metric2: str
    correlation_type: CorrelationType
    correlation_strength: float
    time_lag_seconds: float
    confidence: ConfidenceLevel
    sample_size: int
    @property
    def is_strong_correlation(self) -> bool:
        return abs(self.correlation_strength) > 0.7
@dataclass
class DiagnosticData:
    timestamp: float
    incident_id: str
    system_metrics: Dict[str, float]
    process_metrics: Dict[str, Any]
    network_metrics: Dict[str, float]
    application_logs: List[str]
    error_traces: List[str]
    configuration_snapshot: Dict[str, Any]
    resource_usage: Dict[str, float]
    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)
@dataclass
class RootCauseHypothesis:
    hypothesis_id: str
    category: RootCauseCategory
    description: str
    evidence: List[str]
    supporting_correlations: List[MetricCorrelation]
    confidence_score: float
    confidence_level: ConfidenceLevel
    likelihood_percent: float
    actionable_insights: List[str]
    prevention_recommendations: List[str]
@dataclass
class RCAnalysis:
    analysis_id: str
    timestamp: float
    incident_violation: SLAViolation
    recovery_execution: Optional[RecoveryExecution]
    diagnostic_data: DiagnosticData
    correlations_found: List[MetricCorrelation]
    hypotheses: List[RootCauseHypothesis]
    primary_root_cause: Optional[RootCauseHypothesis]
    analysis_duration_seconds: float
    confidence_summary: Dict[str, int]
    recommendations: List[str]
    def to_dict(self) -> Dict[str, Any]:
        return {'analysis_id': self.analysis_id, 'timestamp': self.timestamp, 'incident_id': self.incident_violation.violation_id, 'metric_name': self.incident_violation.metric_name, 'violation_severity': self.incident_violation.severity.value, 'analysis_duration': self.analysis_duration_seconds, 'correlations_count': len(self.correlations_found), 'hypotheses_count': len(self.hypotheses), 'primary_cause': self.primary_root_cause.description if self.primary_root_cause else None, 'confidence_level': self.primary_root_cause.confidence_level.value if self.primary_root_cause else 'unknown', 'recommendations': self.recommendations}
class MetricsCollector:
    def __init__(self, retention_hours: int=48):
        self.retention_hours = retention_hours
        self.metrics_history: Dict[str, deque] = defaultdict(lambda: deque(maxlen=2880))
        self.metrics_metadata: Dict[str, Dict[str, Any]] = {}
        self._lock = threading.RLock()
    def add_metric(self, metric_name: str, value: float, timestamp: Optional[float]=None, metadata: Optional[Dict]=None):
        if timestamp is None:
            timestamp = time.time()
        with self._lock:
            self.metrics_history[metric_name].append((timestamp, value))
            if metadata:
                self.metrics_metadata[metric_name] = metadata
    def get_metric_window(self, metric_name: str, window_minutes: int, end_time: Optional[float]=None) -> List[Tuple[float, float]]:
        if end_time is None:
            end_time = time.time()
        start_time = end_time - window_minutes * 60
        with self._lock:
            if metric_name not in self.metrics_history:
                return []
            return [(t, v) for t, v in self.metrics_history[metric_name] if start_time <= t <= end_time]
    def get_all_metrics_at_time(self, timestamp: float, tolerance_seconds: int=60) -> Dict[str, float]:
        results = {}
        with self._lock:
            for metric_name, history in self.metrics_history.items():
                closest_value = None
                min_diff = float('inf')
                for t, v in history:
                    diff = abs(t - timestamp)
                    if diff <= tolerance_seconds and diff < min_diff:
                        min_diff = diff
                        closest_value = v
                if closest_value is not None:
                    results[metric_name] = closest_value
        return results
class CorrelationAnalyzer:
    def __init__(self, min_samples: int=10):
        self.min_samples = min_samples
    def find_temporal_correlations(self, metrics_collector: MetricsCollector, incident_time: float, window_minutes: int=60) -> List[MetricCorrelation]:
        correlations = []
        metric_names = list(metrics_collector.metrics_history.keys())
        metric_data = {}
        for metric_name in metric_names:
            data = metrics_collector.get_metric_window(metric_name, window_minutes, incident_time)
            if len(data) >= self.min_samples:
                metric_data[metric_name] = data
        for i, metric1 in enumerate(metric_names):
            for metric2 in metric_names[i + 1:]:
                if metric1 in metric_data and metric2 in metric_data:
                    correlation = self._calculate_cross_correlation(metric_data[metric1], metric_data[metric2], metric1, metric2)
                    if correlation and correlation.is_strong_correlation:
                        correlations.append(correlation)
        return correlations
    def find_causal_relationships(self, metrics_collector: MetricsCollector, violation: SLAViolation) -> List[MetricCorrelation]:
        causal_correlations = []
        violation_time = violation.timestamp
        lookback_window = 30
        violation_metric = violation.metric_name
        other_metrics = [name for name in metrics_collector.metrics_history.keys() if name != violation_metric]
        for metric_name in other_metrics:
            before_data = metrics_collector.get_metric_window(metric_name, lookback_window, violation_time - 300)
            during_data = metrics_collector.get_metric_window(metric_name, 15, violation_time)
            if len(before_data) >= 5 and len(during_data) >= 3:
                before_values = [v for t, v in before_data]
                during_values = [v for t, v in during_data]
                before_mean = statistics.mean(before_values)
                during_mean = statistics.mean(during_values)
                if before_mean != 0:
                    change_percent = abs((during_mean - before_mean) / before_mean) * 100
                    if change_percent > 20:
                        correlation = MetricCorrelation(metric1=metric_name, metric2=violation_metric, correlation_type=CorrelationType.CAUSAL, correlation_strength=min(change_percent / 100, 1.0), time_lag_seconds=300, confidence=self._calculate_confidence(change_percent, len(before_data)), sample_size=len(before_data) + len(during_data))
                        causal_correlations.append(correlation)
        return causal_correlations
    def _calculate_cross_correlation(self, data1: List[Tuple[float, float]], data2: List[Tuple[float, float]], metric1: str, metric2: str) -> Optional[MetricCorrelation]:
        try:
            values1 = [v for t, v in data1]
            values2 = [v for t, v in data2]
            if len(values1) < self.min_samples or len(values2) < self.min_samples:
                return None
            min_len = min(len(values1), len(values2))
            values1 = values1[:min_len]
            values2 = values2[:min_len]
            correlation_coeff = np.corrcoef(values1, values2)[0, 1]
            if np.isnan(correlation_coeff):
                return None
            confidence = self._calculate_correlation_confidence(abs(correlation_coeff), min_len)
            return MetricCorrelation(metric1=metric1, metric2=metric2, correlation_type=CorrelationType.STATISTICAL, correlation_strength=correlation_coeff, time_lag_seconds=0, confidence=confidence, sample_size=min_len)
        except Exception as e:
            logger.error(f'Error calculating correlation between {metric1} and {metric2}: {e}')
            return None
    def _calculate_confidence(self, change_percent: float, sample_size: int) -> ConfidenceLevel:
        if sample_size < 5:
            return ConfidenceLevel.LOW
        elif change_percent > 50 and sample_size >= 10:
            return ConfidenceLevel.VERY_HIGH
        elif change_percent > 30 and sample_size >= 8:
            return ConfidenceLevel.HIGH
        elif change_percent > 15 and sample_size >= 6:
            return ConfidenceLevel.MEDIUM
        else:
            return ConfidenceLevel.LOW
    def _calculate_correlation_confidence(self, correlation_strength: float, sample_size: int) -> ConfidenceLevel:
        if sample_size < 10:
            return ConfidenceLevel.LOW
        elif correlation_strength > 0.8 and sample_size >= 20:
            return ConfidenceLevel.VERY_HIGH
        elif correlation_strength > 0.6 and sample_size >= 15:
            return ConfidenceLevel.HIGH
        elif correlation_strength > 0.4 and sample_size >= 10:
            return ConfidenceLevel.MEDIUM
        else:
            return ConfidenceLevel.LOW
class DiagnosticCollector:
    def collect_diagnostic_data(self, incident_id: str, timestamp: Optional[float]=None) -> DiagnosticData:
        if timestamp is None:
            timestamp = time.time()
        try:
            import psutil
            system_metrics = {'cpu_percent': psutil.cpu_percent(interval=0.1), 'memory_percent': psutil.virtual_memory().percent, 'disk_percent': psutil.disk_usage('/').percent, 'load_avg_1m': psutil.getloadavg()[0] if hasattr(psutil, 'getloadavg') else 0.0, 'process_count': len(psutil.pids())}
            process_metrics = {'top_cpu_processes': self._get_top_cpu_processes(), 'top_memory_processes': self._get_top_memory_processes()}
            network_metrics = {'connections_count': len(psutil.net_connections()), 'network_io_sent': psutil.net_io_counters().bytes_sent, 'network_io_recv': psutil.net_io_counters().bytes_recv}
            resource_usage = {'cpu_cores': psutil.cpu_count(), 'memory_total': psutil.virtual_memory().total, 'memory_available': psutil.virtual_memory().available, 'disk_total': psutil.disk_usage('/').total, 'disk_free': psutil.disk_usage('/').free}
            return DiagnosticData(timestamp=timestamp, incident_id=incident_id, system_metrics=system_metrics, process_metrics=process_metrics, network_metrics=network_metrics, application_logs=self._collect_recent_logs(), error_traces=self._collect_error_traces(), configuration_snapshot=self._collect_configuration(), resource_usage=resource_usage)
        except Exception as e:
            logger.error(f'Error collecting diagnostic data: {e}')
            return DiagnosticData(timestamp=timestamp, incident_id=incident_id, system_metrics={}, process_metrics={}, network_metrics={}, application_logs=[], error_traces=[], configuration_snapshot={}, resource_usage={})
    def _get_top_cpu_processes(self, limit: int=5) -> List[Dict[str, Any]]:
        try:
            import psutil
            processes = []
            for proc in psutil.process_iter(['pid', 'name', 'cpu_percent', 'memory_percent']):
                try:
                    proc_info = proc.info
                    if proc_info['cpu_percent'] > 1.0:
                        processes.append(proc_info)
                except (psutil.NoSuchProcess, psutil.AccessDenied):
                    continue
            return sorted(processes, key=lambda x: x['cpu_percent'], reverse=True)[:limit]
        except Exception:
            return []
    def _get_top_memory_processes(self, limit: int=5) -> List[Dict[str, Any]]:
        try:
            import psutil
            processes = []
            for proc in psutil.process_iter(['pid', 'name', 'cpu_percent', 'memory_percent']):
                try:
                    proc_info = proc.info
                    if proc_info['memory_percent'] > 1.0:
                        processes.append(proc_info)
                except (psutil.NoSuchProcess, psutil.AccessDenied):
                    continue
            return sorted(processes, key=lambda x: x['memory_percent'], reverse=True)[:limit]
        except Exception:
            return []
    def _collect_recent_logs(self, lines: int=50) -> List[str]:
        return [f"[{time.strftime('%Y-%m-%d %H:%M:%S')}] INFO: Sample log entry {i}" for i in range(lines)]
    def _collect_error_traces(self) -> List[str]:
        return ['Sample error trace: Connection timeout in model inference', 'Sample error trace: GPU memory allocation failed']
    def _collect_configuration(self) -> Dict[str, Any]:
        return {'aphrodite_config': {'max_model_len': 4096, 'gpu_memory_utilization': 0.9, 'tensor_parallel_size': 1}, 'system_config': {'python_version': '3.12.3', 'platform': 'linux'}}
class HypothesisGenerator:
    def generate_hypotheses(self, violation: SLAViolation, correlations: List[MetricCorrelation], diagnostic_data: DiagnosticData) -> List[RootCauseHypothesis]:
        hypotheses = []
        infra_hypothesis = self._generate_infrastructure_hypothesis(violation, diagnostic_data)
        if infra_hypothesis:
            hypotheses.append(infra_hypothesis)
        resource_hypothesis = self._generate_resource_hypothesis(violation, diagnostic_data, correlations)
        if resource_hypothesis:
            hypotheses.append(resource_hypothesis)
        app_hypothesis = self._generate_application_hypothesis(violation, correlations)
        if app_hypothesis:
            hypotheses.append(app_hypothesis)
        network_hypothesis = self._generate_network_hypothesis(violation, diagnostic_data)
        if network_hypothesis:
            hypotheses.append(network_hypothesis)
        config_hypothesis = self._generate_configuration_hypothesis(violation, diagnostic_data)
        if config_hypothesis:
            hypotheses.append(config_hypothesis)
        return hypotheses
    def _generate_infrastructure_hypothesis(self, violation: SLAViolation, diagnostic_data: DiagnosticData) -> Optional[RootCauseHypothesis]:
        evidence = []
        confidence_score = 0.0
        system_metrics = diagnostic_data.system_metrics
        if system_metrics.get('cpu_percent', 0) > 85:
            evidence.append(f"High CPU usage: {system_metrics['cpu_percent']:.1f}%")
            confidence_score += 0.3
        if system_metrics.get('memory_percent', 0) > 90:
            evidence.append(f"High memory usage: {system_metrics['memory_percent']:.1f}%")
            confidence_score += 0.3
        if system_metrics.get('disk_percent', 0) > 95:
            evidence.append(f"High disk usage: {system_metrics['disk_percent']:.1f}%")
            confidence_score += 0.2
        if not evidence:
            return None
        return RootCauseHypothesis(hypothesis_id=f'infra_{violation.violation_id}', category=RootCauseCategory.INFRASTRUCTURE, description='Infrastructure resource exhaustion causing performance degradation', evidence=evidence, supporting_correlations=[], confidence_score=min(confidence_score, 1.0), confidence_level=self._score_to_confidence_level(confidence_score), likelihood_percent=confidence_score * 100, actionable_insights=['Scale up infrastructure resources', 'Optimize resource allocation', 'Investigate resource-intensive processes'], prevention_recommendations=['Implement auto-scaling policies', 'Set up proactive resource monitoring', 'Regular capacity planning reviews'])
    def _generate_resource_hypothesis(self, violation: SLAViolation, diagnostic_data: DiagnosticData, correlations: List[MetricCorrelation]) -> Optional[RootCauseHypothesis]:
        evidence = []
        confidence_score = 0.0
        supporting_correlations = []
        resource_metrics = ['cpu_usage', 'memory_usage', 'gpu_utilization', 'kv_cache_usage']
        for correlation in correlations:
            if any((metric in correlation.metric1.lower() or metric in correlation.metric2.lower() for metric in resource_metrics)):
                if correlation.is_strong_correlation:
                    evidence.append(f'Strong correlation between {correlation.metric1} and {correlation.metric2}')
                    supporting_correlations.append(correlation)
                    confidence_score += 0.2
        top_cpu_processes = diagnostic_data.process_metrics.get('top_cpu_processes', [])
        if top_cpu_processes:
            high_cpu_process = top_cpu_processes[0]
            if high_cpu_process.get('cpu_percent', 0) > 50:
                evidence.append(f"High CPU process: {high_cpu_process['name']} ({high_cpu_process['cpu_percent']:.1f}%)")
                confidence_score += 0.3
        if not evidence:
            return None
        return RootCauseHypothesis(hypothesis_id=f'resource_{violation.violation_id}', category=RootCauseCategory.RESOURCE_CONTENTION, description='Resource contention between competing processes', evidence=evidence, supporting_correlations=supporting_correlations, confidence_score=min(confidence_score, 1.0), confidence_level=self._score_to_confidence_level(confidence_score), likelihood_percent=confidence_score * 100, actionable_insights=['Identify and optimize resource-intensive processes', 'Implement resource isolation', 'Adjust process priorities'], prevention_recommendations=['Implement resource quotas', 'Regular process optimization', 'Capacity-aware scheduling'])
    def _generate_application_hypothesis(self, violation: SLAViolation, correlations: List[MetricCorrelation]) -> Optional[RootCauseHypothesis]:
        evidence = []
        confidence_score = 0.0
        supporting_correlations = []
        app_metrics = ['latency', 'throughput', 'error_rate', 'request_time']
        for correlation in correlations:
            if any((metric in correlation.metric1.lower() or metric in correlation.metric2.lower() for metric in app_metrics)):
                if correlation.correlation_type == CorrelationType.CAUSAL:
                    evidence.append(f'Causal relationship: {correlation.metric1} -> {correlation.metric2}')
                    supporting_correlations.append(correlation)
                    confidence_score += 0.4
                elif correlation.is_strong_correlation:
                    evidence.append(f'Strong correlation: {correlation.metric1} <-> {correlation.metric2}')
                    supporting_correlations.append(correlation)
                    confidence_score += 0.2
        if violation.violation_type.value in ['latency_breach', 'throughput_degradation']:
            evidence.append(f'Application performance violation: {violation.violation_type.value}')
            confidence_score += 0.3
        if not evidence:
            return None
        return RootCauseHypothesis(hypothesis_id=f'app_{violation.violation_id}', category=RootCauseCategory.APPLICATION, description='Application-level performance degradation or bug', evidence=evidence, supporting_correlations=supporting_correlations, confidence_score=min(confidence_score, 1.0), confidence_level=self._score_to_confidence_level(confidence_score), likelihood_percent=confidence_score * 100, actionable_insights=['Profile application performance', 'Review recent code changes', 'Analyze request patterns'], prevention_recommendations=['Implement comprehensive performance testing', 'Code review for performance impact', 'Continuous performance monitoring'])
    def _generate_network_hypothesis(self, violation: SLAViolation, diagnostic_data: DiagnosticData) -> Optional[RootCauseHypothesis]:
        evidence = []
        confidence_score = 0.0
        network_metrics = diagnostic_data.network_metrics
        connections_count = network_metrics.get('connections_count', 0)
        if connections_count > 1000:
            evidence.append(f'High network connection count: {connections_count}')
            confidence_score += 0.2
        if 'latency' in violation.metric_name.lower():
            evidence.append('Latency violation may indicate network issues')
            confidence_score += 0.3
        if not evidence:
            return None
        return RootCauseHypothesis(hypothesis_id=f'network_{violation.violation_id}', category=RootCauseCategory.NETWORK, description='Network latency or connectivity issues', evidence=evidence, supporting_correlations=[], confidence_score=min(confidence_score, 1.0), confidence_level=self._score_to_confidence_level(confidence_score), likelihood_percent=confidence_score * 100, actionable_insights=['Check network connectivity and latency', 'Analyze network traffic patterns', 'Review firewall and routing rules'], prevention_recommendations=['Implement network monitoring', 'Regular network performance testing', 'Redundant network paths'])
    def _generate_configuration_hypothesis(self, violation: SLAViolation, diagnostic_data: DiagnosticData) -> Optional[RootCauseHypothesis]:
        evidence = []
        confidence_score = 0.0
        config = diagnostic_data.configuration_snapshot.get('aphrodite_config', {})
        gpu_utilization = config.get('gpu_memory_utilization', 0.9)
        if gpu_utilization > 0.95:
            evidence.append(f'Very high GPU memory utilization configured: {gpu_utilization}')
            confidence_score += 0.2
        max_model_len = config.get('max_model_len', 0)
        if max_model_len > 8192:
            evidence.append(f'Large max model length configured: {max_model_len}')
            confidence_score += 0.1
        if not evidence:
            return None
        return RootCauseHypothesis(hypothesis_id=f'config_{violation.violation_id}', category=RootCauseCategory.CONFIGURATION, description='Sub-optimal configuration settings', evidence=evidence, supporting_correlations=[], confidence_score=min(confidence_score, 1.0), confidence_level=self._score_to_confidence_level(confidence_score), likelihood_percent=confidence_score * 100, actionable_insights=['Review and optimize configuration settings', 'Compare with recommended configurations', 'Test configuration changes in staging'], prevention_recommendations=['Configuration validation checks', 'Regular configuration reviews', 'Automated configuration optimization'])
    def _score_to_confidence_level(self, score: float) -> ConfidenceLevel:
        if score >= 0.9:
            return ConfidenceLevel.VERY_HIGH
        elif score >= 0.7:
            return ConfidenceLevel.HIGH
        elif score >= 0.3:
            return ConfidenceLevel.MEDIUM
        else:
            return ConfidenceLevel.LOW
class RCAEngine:
    def __init__(self, retention_hours: int=168):
        self.retention_hours = retention_hours
        self.metrics_collector = MetricsCollector(retention_hours)
        self.correlation_analyzer = CorrelationAnalyzer()
        self.diagnostic_collector = DiagnosticCollector()
        self.hypothesis_generator = HypothesisGenerator()
        self.analyses: List[RCAnalysis] = []
        self.rca_callbacks: List[Callable[[RCAnalysis], None]] = []
        self.stats_dir = Path('/tmp/rca_analyses')
        self.stats_dir.mkdir(exist_ok=True)
        self._lock = threading.RLock()
        logger.info('RCA Engine initialized with correlation analysis and hypothesis generation')
    def record_metric(self, metric_name: str, value: float, timestamp: Optional[float]=None):
        self.metrics_collector.add_metric(metric_name, value, timestamp)
    def register_rca_callback(self, callback: Callable[[RCAnalysis], None]):
        self.rca_callbacks.append(callback)
        logger.info('Registered RCA callback')
    async def analyze_incident(self, violation: SLAViolation, recovery_execution: Optional[RecoveryExecution]=None) -> RCAnalysis:
        start_time = time.time()
        analysis_id = f'rca_{violation.violation_id}_{int(start_time)}'
        logger.info(f'Starting root cause analysis: {analysis_id}')
        try:
            diagnostic_data = self.diagnostic_collector.collect_diagnostic_data(incident_id=violation.violation_id, timestamp=violation.timestamp)
            correlations = []
            temporal_correlations = self.correlation_analyzer.find_temporal_correlations(self.metrics_collector, violation.timestamp, window_minutes=60)
            correlations.extend(temporal_correlations)
            causal_correlations = self.correlation_analyzer.find_causal_relationships(self.metrics_collector, violation)
            correlations.extend(causal_correlations)
            hypotheses = self.hypothesis_generator.generate_hypotheses(violation, correlations, diagnostic_data)
            primary_cause = None
            if hypotheses:
                primary_cause = max(hypotheses, key=lambda h: h.confidence_score)
            recommendations = self._generate_recommendations(hypotheses, correlations, violation)
            confidence_summary = {level.value: 0 for level in ConfidenceLevel}
            for hypothesis in hypotheses:
                confidence_summary[hypothesis.confidence_level.value] += 1
            analysis = RCAnalysis(analysis_id=analysis_id, timestamp=start_time, incident_violation=violation, recovery_execution=recovery_execution, diagnostic_data=diagnostic_data, correlations_found=correlations, hypotheses=hypotheses, primary_root_cause=primary_cause, analysis_duration_seconds=time.time() - start_time, confidence_summary=confidence_summary, recommendations=recommendations)
            with self._lock:
                self.analyses.append(analysis)
            await self._save_analysis_report(analysis)
            logger.info(f'RCA completed: {analysis_id} - Found {len(correlations)} correlations, {len(hypotheses)} hypotheses')
            for callback in self.rca_callbacks:
                try:
                    callback(analysis)
                except Exception as e:
                    logger.error(f'Error in RCA callback: {e}')
            return analysis
        except Exception as e:
            logger.error(f'Error in root cause analysis {analysis_id}: {e}')
            return RCAnalysis(analysis_id=analysis_id, timestamp=start_time, incident_violation=violation, recovery_execution=recovery_execution, diagnostic_data=DiagnosticData(timestamp=start_time, incident_id=violation.violation_id, system_metrics={}, process_metrics={}, network_metrics={}, application_logs=[], error_traces=[], configuration_snapshot={}, resource_usage={}), correlations_found=[], hypotheses=[], primary_root_cause=None, analysis_duration_seconds=time.time() - start_time, confidence_summary={level.value: 0 for level in ConfidenceLevel}, recommendations=['RCA analysis failed - manual investigation required'])
    def _generate_recommendations(self, hypotheses: List[RootCauseHypothesis], correlations: List[MetricCorrelation], violation: SLAViolation) -> List[str]:
        recommendations = []
        for hypothesis in hypotheses:
            if hypothesis.confidence_level in [ConfidenceLevel.HIGH, ConfidenceLevel.VERY_HIGH]:
                recommendations.extend(hypothesis.actionable_insights)
        strong_correlations = [c for c in correlations if c.is_strong_correlation]
        if strong_correlations:
            recommendations.append(f"Investigate strong correlations found between metrics: {', '.join(set([c.metric1 for c in strong_correlations] + [c.metric2 for c in strong_correlations]))}")
        if violation.severity == ViolationSeverity.CRITICAL:
            recommendations.append('Implement immediate incident response procedures')
            recommendations.append('Consider emergency capacity scaling')
        seen = set()
        unique_recommendations = []
        for rec in recommendations:
            if rec not in seen:
                seen.add(rec)
                unique_recommendations.append(rec)
        return unique_recommendations[:10]
    async def _save_analysis_report(self, analysis: RCAnalysis):
        try:
            report_file = self.stats_dir / f'rca_{analysis.analysis_id}.json'
            report_data = {'analysis_summary': analysis.to_dict(), 'detailed_findings': {'correlations': [{'metric1': c.metric1, 'metric2': c.metric2, 'type': c.correlation_type.value, 'strength': c.correlation_strength, 'confidence': c.confidence.value} for c in analysis.correlations_found], 'hypotheses': [{'category': h.category.value, 'description': h.description, 'confidence': h.confidence_level.value, 'likelihood': h.likelihood_percent, 'evidence': h.evidence, 'insights': h.actionable_insights} for h in analysis.hypotheses]}, 'diagnostic_data': analysis.diagnostic_data.to_dict()}
            with open(report_file, 'w') as f:
                json.dump(report_data, f, indent=2)
            logger.info(f'RCA report saved: {report_file}')
        except Exception as e:
            logger.error(f'Error saving RCA report: {e}')
    def get_analysis_by_id(self, analysis_id: str) -> Optional[RCAnalysis]:
        with self._lock:
            for analysis in self.analyses:
                if analysis.analysis_id == analysis_id:
                    return analysis
        return None
    def get_recent_analyses(self, hours: int=24) -> List[RCAnalysis]:
        cutoff_time = time.time() - hours * 3600
        return [a for a in self.analyses if a.timestamp >= cutoff_time]
    def get_rca_summary(self) -> Dict[str, Any]:
        with self._lock:
            total_analyses = len(self.analyses)
            recent_analyses = self.get_recent_analyses(24)
            confidence_dist = {level.value: 0 for level in ConfidenceLevel}
            category_dist = {category.value: 0 for category in RootCauseCategory}
            for analysis in recent_analyses:
                if analysis.primary_root_cause:
                    confidence_dist[analysis.primary_root_cause.confidence_level.value] += 1
                    category_dist[analysis.primary_root_cause.category.value] += 1
            if recent_analyses:
                avg_analysis_time = statistics.mean([a.analysis_duration_seconds for a in recent_analyses])
            else:
                avg_analysis_time = 0.0
            return {'total_analyses': total_analyses, 'analyses_24h': len(recent_analyses), 'average_analysis_time_seconds': avg_analysis_time, 'confidence_distribution': confidence_dist, 'root_cause_categories': category_dist, 'metrics_tracked': len(self.metrics_collector.metrics_history), 'retention_hours': self.retention_hours}
def create_production_rca_engine() -> RCAEngine:
    return RCAEngine(retention_hours=168)
if __name__ == '__main__':
    import asyncio
    logging.basicConfig(level=logging.INFO)
    print('🔍 Root Cause Analysis Engine Demo')
    print('=' * 50)
    async def demo_rca():
        rca_engine = create_production_rca_engine()
        def rca_handler(analysis: RCAnalysis):
            print(f'🔍 RCA Completed: {analysis.analysis_id}')
            if analysis.primary_root_cause:
                print(f'   Primary cause: {analysis.primary_root_cause.description}')
                print(f'   Confidence: {analysis.primary_root_cause.confidence_level.value}')
                print(f'   Likelihood: {analysis.primary_root_cause.likelihood_percent:.1f}%')
        rca_engine.register_rca_callback(rca_handler)
        current_time = time.time()
        for i in range(100):
            timestamp = current_time - (100 - i) * 60
            if i < 80:
                latency = 150 + i * 2
                cpu = 60 + i * 0.3
            else:
                latency = 250 + i * 5
                cpu = 85 + i * 0.5
            rca_engine.record_metric('request_latency_p95', latency, timestamp)
            rca_engine.record_metric('cpu_usage', cpu, timestamp)
            rca_engine.record_metric('memory_usage', 70 + i * 0.2, timestamp)
        print('📊 Added metric history for correlation analysis')
        from aphrodite.engine.sla_manager import SLAViolation, SLAViolationType, ViolationSeverity, SLAThreshold
        mock_threshold = SLAThreshold(metric_name='request_latency_p95', target_value=200.0, tolerance_percent=25.0)
        mock_violation = SLAViolation(violation_id='test_rca_violation', timestamp=current_time, violation_type=SLAViolationType.LATENCY_BREACH, severity=ViolationSeverity.CRITICAL, metric_name='request_latency_p95', threshold=mock_threshold, actual_value=350.0, expected_value=200.0, breach_percentage=40.0, measurement_window=[300.0, 320.0, 350.0, 340.0, 360.0])
        print('🚨 Running RCA for simulated violation...')
        analysis = await rca_engine.analyze_incident(mock_violation)
        print(f'\n📈 RCA Results:')
        print(f'   Analysis ID: {analysis.analysis_id}')
        print(f'   Duration: {analysis.analysis_duration_seconds:.2f}s')
        print(f'   Correlations found: {len(analysis.correlations_found)}')
        print(f'   Hypotheses generated: {len(analysis.hypotheses)}')
        print(f'   Recommendations: {len(analysis.recommendations)}')
        if analysis.recommendations:
            print('\n💡 Top Recommendations:')
            for i, rec in enumerate(analysis.recommendations[:3], 1):
                print(f'   {i}. {rec}')
        summary = rca_engine.get_rca_summary()
        print(f'\n📊 RCA Engine Summary:')
        print(f"   Total analyses: {summary['total_analyses']}")
        print(f"   Metrics tracked: {summary['metrics_tracked']}")
        print(f"   Average analysis time: {summary['average_analysis_time_seconds']:.2f}s")
    try:
        asyncio.run(demo_rca())
    except KeyboardInterrupt:
        print('\n🛑 Demo interrupted')