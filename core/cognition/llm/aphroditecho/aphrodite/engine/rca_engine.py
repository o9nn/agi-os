#!/usr/bin/env python3
"""
Root Cause Analysis (RCA) Engine for Production Incident Analysis
Implements automated root cause analysis with correlation detection and diagnostic data collection.

This module provides intelligent RCA capabilities that analyze incident patterns,
correlate metrics, and generate actionable insights for performance issues.
"""

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
    """Types of correlations in RCA"""
    TEMPORAL = "temporal"           # Time-based correlation
    CAUSAL = "causal"              # Direct cause-effect relationship
    STATISTICAL = "statistical"    # Statistical correlation
    PATTERN = "pattern"            # Pattern-based correlation
    DEPENDENCY = "dependency"      # Service dependency correlation


class RootCauseCategory(Enum):
    """Categories of root causes"""
    INFRASTRUCTURE = "infrastructure"
    APPLICATION = "application" 
    NETWORK = "network"
    RESOURCE_CONTENTION = "resource_contention"
    CONFIGURATION = "configuration"
    EXTERNAL_DEPENDENCY = "external_dependency"
    CAPACITY_LIMITS = "capacity_limits"
    CODE_REGRESSION = "code_regression"


class ConfidenceLevel(Enum):
    """Confidence levels for RCA findings"""
    LOW = "low"          # < 30% confidence
    MEDIUM = "medium"    # 30-70% confidence  
    HIGH = "high"        # 70-90% confidence
    VERY_HIGH = "very_high"  # > 90% confidence


@dataclass
class MetricCorrelation:
    """Correlation between two metrics"""
    metric1: str
    metric2: str
    correlation_type: CorrelationType
    correlation_strength: float  # -1.0 to 1.0
    time_lag_seconds: float
    confidence: ConfidenceLevel
    sample_size: int
    
    @property
    def is_strong_correlation(self) -> bool:
        return abs(self.correlation_strength) > 0.7


@dataclass
class DiagnosticData:
    """Diagnostic data collected during incidents"""
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
    """A hypothesis about the root cause of an incident"""
    hypothesis_id: str
    category: RootCauseCategory
    description: str
    evidence: List[str]
    supporting_correlations: List[MetricCorrelation]
    confidence_score: float  # 0.0 to 1.0
    confidence_level: ConfidenceLevel
    likelihood_percent: float
    actionable_insights: List[str]
    prevention_recommendations: List[str]


@dataclass
class RCAnalysis:
    """Complete root cause analysis for an incident"""
    analysis_id: str
    timestamp: float
    incident_violation: SLAViolation
    recovery_execution: Optional[RecoveryExecution]
    diagnostic_data: DiagnosticData
    correlations_found: List[MetricCorrelation] 
    hypotheses: List[RootCauseHypothesis]
    primary_root_cause: Optional[RootCauseHypothesis]
    analysis_duration_seconds: float
    confidence_summary: Dict[str, int]  # Count by confidence level
    recommendations: List[str]
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            'analysis_id': self.analysis_id,
            'timestamp': self.timestamp,
            'incident_id': self.incident_violation.violation_id,
            'metric_name': self.incident_violation.metric_name,
            'violation_severity': self.incident_violation.severity.value,
            'analysis_duration': self.analysis_duration_seconds,
            'correlations_count': len(self.correlations_found),
            'hypotheses_count': len(self.hypotheses),
            'primary_cause': self.primary_root_cause.description if self.primary_root_cause else None,
            'confidence_level': self.primary_root_cause.confidence_level.value if self.primary_root_cause else 'unknown',
            'recommendations': self.recommendations
        }


class MetricsCollector:
    """Collects and stores metrics for correlation analysis"""
    
    def __init__(self, retention_hours: int = 48):
        self.retention_hours = retention_hours
        self.metrics_history: Dict[str, deque] = defaultdict(lambda: deque(maxlen=2880))  # 48h at 1min intervals
        self.metrics_metadata: Dict[str, Dict[str, Any]] = {}
        self._lock = threading.RLock()
    
    def add_metric(self, metric_name: str, value: float, timestamp: Optional[float] = None, metadata: Optional[Dict] = None):
        """Add a metric measurement"""
        if timestamp is None:
            timestamp = time.time()
            
        with self._lock:
            self.metrics_history[metric_name].append((timestamp, value))
            
            if metadata:
                self.metrics_metadata[metric_name] = metadata
    
    def get_metric_window(self, metric_name: str, window_minutes: int, end_time: Optional[float] = None) -> List[Tuple[float, float]]:
        """Get metric values for a specific time window"""
        if end_time is None:
            end_time = time.time()
            
        start_time = end_time - (window_minutes * 60)
        
        with self._lock:
            if metric_name not in self.metrics_history:
                return []
                
            return [(t, v) for t, v in self.metrics_history[metric_name] 
                   if start_time <= t <= end_time]
    
    def get_all_metrics_at_time(self, timestamp: float, tolerance_seconds: int = 60) -> Dict[str, float]:
        """Get all metric values near a specific timestamp"""
        results = {}
        
        with self._lock:
            for metric_name, history in self.metrics_history.items():
                # Find closest measurement to the timestamp
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
    """Analyzes correlations between metrics"""
    
    def __init__(self, min_samples: int = 10):
        self.min_samples = min_samples
    
    def find_temporal_correlations(self, metrics_collector: MetricsCollector, 
                                 incident_time: float, window_minutes: int = 60) -> List[MetricCorrelation]:
        """Find temporal correlations around incident time"""
        correlations = []
        
        # Get all metrics in the window
        metric_names = list(metrics_collector.metrics_history.keys())
        metric_data = {}
        
        for metric_name in metric_names:
            data = metrics_collector.get_metric_window(metric_name, window_minutes, incident_time)
            if len(data) >= self.min_samples:
                metric_data[metric_name] = data
        
        # Analyze pairwise correlations
        for i, metric1 in enumerate(metric_names):
            for metric2 in metric_names[i+1:]:
                if metric1 in metric_data and metric2 in metric_data:
                    correlation = self._calculate_cross_correlation(
                        metric_data[metric1], metric_data[metric2], metric1, metric2
                    )
                    if correlation and correlation.is_strong_correlation:
                        correlations.append(correlation)
        
        return correlations
    
    def find_causal_relationships(self, metrics_collector: MetricsCollector,
                                violation: SLAViolation) -> List[MetricCorrelation]:
        """Find potential causal relationships leading to violation"""
        causal_correlations = []
        
        # Look for metrics that changed before the violation
        violation_time = violation.timestamp
        lookback_window = 30  # 30 minutes before violation
        
        violation_metric = violation.metric_name
        other_metrics = [name for name in metrics_collector.metrics_history.keys() 
                        if name != violation_metric]
        
        for metric_name in other_metrics:
            # Get data before violation
            before_data = metrics_collector.get_metric_window(
                metric_name, lookback_window, violation_time - 300  # 5 min before violation start
            )
            
            # Get data during violation
            during_data = metrics_collector.get_metric_window(
                metric_name, 15, violation_time  # 15 min around violation
            )
            
            if len(before_data) >= 5 and len(during_data) >= 3:
                # Check for significant change
                before_values = [v for t, v in before_data]
                during_values = [v for t, v in during_data]
                
                before_mean = statistics.mean(before_values)
                during_mean = statistics.mean(during_values)
                
                if before_mean != 0:
                    change_percent = abs((during_mean - before_mean) / before_mean) * 100
                    
                    if change_percent > 20:  # Significant change
                        # Create causal correlation
                        correlation = MetricCorrelation(
                            metric1=metric_name,
                            metric2=violation_metric,
                            correlation_type=CorrelationType.CAUSAL,
                            correlation_strength=min(change_percent / 100, 1.0),
                            time_lag_seconds=300,  # Assume 5 min lag
                            confidence=self._calculate_confidence(change_percent, len(before_data)),
                            sample_size=len(before_data) + len(during_data)
                        )
                        causal_correlations.append(correlation)
        
        return causal_correlations
    
    def _calculate_cross_correlation(self, data1: List[Tuple[float, float]], 
                                   data2: List[Tuple[float, float]],
                                   metric1: str, metric2: str) -> Optional[MetricCorrelation]:
        """Calculate cross-correlation between two time series"""
        
        try:
            # Align time series (simplified - assumes similar timestamps)
            values1 = [v for t, v in data1]
            values2 = [v for t, v in data2]
            
            if len(values1) < self.min_samples or len(values2) < self.min_samples:
                return None
            
            # Take minimum length
            min_len = min(len(values1), len(values2))
            values1 = values1[:min_len]
            values2 = values2[:min_len]
            
            # Calculate Pearson correlation
            correlation_coeff = np.corrcoef(values1, values2)[0, 1]
            
            if np.isnan(correlation_coeff):
                return None
            
            # Determine confidence based on sample size and correlation strength
            confidence = self._calculate_correlation_confidence(
                abs(correlation_coeff), min_len
            )
            
            return MetricCorrelation(
                metric1=metric1,
                metric2=metric2,
                correlation_type=CorrelationType.STATISTICAL,
                correlation_strength=correlation_coeff,
                time_lag_seconds=0,  # Simplified - no lag calculation
                confidence=confidence,
                sample_size=min_len
            )
            
        except Exception as e:
            logger.error(f"Error calculating correlation between {metric1} and {metric2}: {e}")
            return None
    
    def _calculate_confidence(self, change_percent: float, sample_size: int) -> ConfidenceLevel:
        """Calculate confidence level based on change magnitude and sample size"""
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
    
    def _calculate_correlation_confidence(self, correlation_strength: float, 
                                        sample_size: int) -> ConfidenceLevel:
        """Calculate confidence for correlation strength"""
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
    """Collects diagnostic data during incidents"""
    
    def collect_diagnostic_data(self, incident_id: str, timestamp: Optional[float] = None) -> DiagnosticData:
        """Collect comprehensive diagnostic data"""
        if timestamp is None:
            timestamp = time.time()
        
        try:
            import psutil
            
            # System metrics
            system_metrics = {
                'cpu_percent': psutil.cpu_percent(interval=0.1),
                'memory_percent': psutil.virtual_memory().percent,
                'disk_percent': psutil.disk_usage('/').percent,
                'load_avg_1m': psutil.getloadavg()[0] if hasattr(psutil, 'getloadavg') else 0.0,
                'process_count': len(psutil.pids())
            }
            
            # Process metrics
            process_metrics = {
                'top_cpu_processes': self._get_top_cpu_processes(),
                'top_memory_processes': self._get_top_memory_processes()
            }
            
            # Network metrics (basic)
            network_metrics = {
                'connections_count': len(psutil.net_connections()),
                'network_io_sent': psutil.net_io_counters().bytes_sent,
                'network_io_recv': psutil.net_io_counters().bytes_recv
            }
            
            # Resource usage
            resource_usage = {
                'cpu_cores': psutil.cpu_count(),
                'memory_total': psutil.virtual_memory().total,
                'memory_available': psutil.virtual_memory().available,
                'disk_total': psutil.disk_usage('/').total,
                'disk_free': psutil.disk_usage('/').free
            }
            
            return DiagnosticData(
                timestamp=timestamp,
                incident_id=incident_id,
                system_metrics=system_metrics,
                process_metrics=process_metrics,
                network_metrics=network_metrics,
                application_logs=self._collect_recent_logs(),
                error_traces=self._collect_error_traces(),
                configuration_snapshot=self._collect_configuration(),
                resource_usage=resource_usage
            )
            
        except Exception as e:
            logger.error(f"Error collecting diagnostic data: {e}")
            return DiagnosticData(
                timestamp=timestamp,
                incident_id=incident_id,
                system_metrics={},
                process_metrics={},
                network_metrics={},
                application_logs=[],
                error_traces=[],
                configuration_snapshot={},
                resource_usage={}
            )
    
    def _get_top_cpu_processes(self, limit: int = 5) -> List[Dict[str, Any]]:
        """Get top CPU consuming processes"""
        try:
            import psutil
            processes = []
            for proc in psutil.process_iter(['pid', 'name', 'cpu_percent', 'memory_percent']):
                try:
                    proc_info = proc.info
                    if proc_info['cpu_percent'] > 1.0:  # Only processes using > 1% CPU
                        processes.append(proc_info)
                except (psutil.NoSuchProcess, psutil.AccessDenied):
                    continue
            
            return sorted(processes, key=lambda x: x['cpu_percent'], reverse=True)[:limit]
        except Exception:
            return []
    
    def _get_top_memory_processes(self, limit: int = 5) -> List[Dict[str, Any]]:
        """Get top memory consuming processes"""
        try:
            import psutil
            processes = []
            for proc in psutil.process_iter(['pid', 'name', 'cpu_percent', 'memory_percent']):
                try:
                    proc_info = proc.info
                    if proc_info['memory_percent'] > 1.0:  # Only processes using > 1% memory
                        processes.append(proc_info)
                except (psutil.NoSuchProcess, psutil.AccessDenied):
                    continue
            
            return sorted(processes, key=lambda x: x['memory_percent'], reverse=True)[:limit]
        except Exception:
            return []
    
    def _collect_recent_logs(self, lines: int = 50) -> List[str]:
        """Collect recent application logs"""
        # In production, this would read from actual log files
        # For now, return simulated logs
        return [
            f"[{time.strftime('%Y-%m-%d %H:%M:%S')}] INFO: Sample log entry {i}"
            for i in range(lines)
        ]
    
    def _collect_error_traces(self) -> List[str]:
        """Collect recent error traces"""
        # In production, this would collect from error tracking systems
        return [
            "Sample error trace: Connection timeout in model inference",
            "Sample error trace: GPU memory allocation failed"
        ]
    
    def _collect_configuration(self) -> Dict[str, Any]:
        """Collect current configuration snapshot"""
        return {
            'aphrodite_config': {
                'max_model_len': 4096,
                'gpu_memory_utilization': 0.9,
                'tensor_parallel_size': 1
            },
            'system_config': {
                'python_version': "3.12.3",
                'platform': "linux"
            }
        }


class HypothesisGenerator:
    """Generates root cause hypotheses based on analysis"""
    
    def generate_hypotheses(self, violation: SLAViolation, correlations: List[MetricCorrelation],
                          diagnostic_data: DiagnosticData) -> List[RootCauseHypothesis]:
        """Generate root cause hypotheses"""
        hypotheses = []
        
        # Generate infrastructure-related hypotheses
        infra_hypothesis = self._generate_infrastructure_hypothesis(violation, diagnostic_data)
        if infra_hypothesis:
            hypotheses.append(infra_hypothesis)
        
        # Generate resource contention hypotheses
        resource_hypothesis = self._generate_resource_hypothesis(violation, diagnostic_data, correlations)
        if resource_hypothesis:
            hypotheses.append(resource_hypothesis)
        
        # Generate application-related hypotheses
        app_hypothesis = self._generate_application_hypothesis(violation, correlations)
        if app_hypothesis:
            hypotheses.append(app_hypothesis)
        
        # Generate network-related hypotheses
        network_hypothesis = self._generate_network_hypothesis(violation, diagnostic_data)
        if network_hypothesis:
            hypotheses.append(network_hypothesis)
        
        # Generate configuration-related hypotheses
        config_hypothesis = self._generate_configuration_hypothesis(violation, diagnostic_data)
        if config_hypothesis:
            hypotheses.append(config_hypothesis)
        
        return hypotheses
    
    def _generate_infrastructure_hypothesis(self, violation: SLAViolation, 
                                         diagnostic_data: DiagnosticData) -> Optional[RootCauseHypothesis]:
        """Generate infrastructure-related hypothesis"""
        evidence = []
        confidence_score = 0.0
        
        system_metrics = diagnostic_data.system_metrics
        
        # Check CPU usage
        if system_metrics.get('cpu_percent', 0) > 85:
            evidence.append(f"High CPU usage: {system_metrics['cpu_percent']:.1f}%")
            confidence_score += 0.3
        
        # Check memory usage
        if system_metrics.get('memory_percent', 0) > 90:
            evidence.append(f"High memory usage: {system_metrics['memory_percent']:.1f}%")
            confidence_score += 0.3
        
        # Check disk usage
        if system_metrics.get('disk_percent', 0) > 95:
            evidence.append(f"High disk usage: {system_metrics['disk_percent']:.1f}%")
            confidence_score += 0.2
        
        if not evidence:
            return None
        
        return RootCauseHypothesis(
            hypothesis_id=f"infra_{violation.violation_id}",
            category=RootCauseCategory.INFRASTRUCTURE,
            description="Infrastructure resource exhaustion causing performance degradation",
            evidence=evidence,
            supporting_correlations=[],
            confidence_score=min(confidence_score, 1.0),
            confidence_level=self._score_to_confidence_level(confidence_score),
            likelihood_percent=confidence_score * 100,
            actionable_insights=[
                "Scale up infrastructure resources",
                "Optimize resource allocation",
                "Investigate resource-intensive processes"
            ],
            prevention_recommendations=[
                "Implement auto-scaling policies",
                "Set up proactive resource monitoring",
                "Regular capacity planning reviews"
            ]
        )
    
    def _generate_resource_hypothesis(self, violation: SLAViolation, diagnostic_data: DiagnosticData,
                                    correlations: List[MetricCorrelation]) -> Optional[RootCauseHypothesis]:
        """Generate resource contention hypothesis"""
        evidence = []
        confidence_score = 0.0
        supporting_correlations = []
        
        # Look for resource-related correlations
        resource_metrics = ['cpu_usage', 'memory_usage', 'gpu_utilization', 'kv_cache_usage']
        
        for correlation in correlations:
            if any(metric in correlation.metric1.lower() or metric in correlation.metric2.lower() 
                  for metric in resource_metrics):
                if correlation.is_strong_correlation:
                    evidence.append(f"Strong correlation between {correlation.metric1} and {correlation.metric2}")
                    supporting_correlations.append(correlation)
                    confidence_score += 0.2
        
        # Check process metrics
        top_cpu_processes = diagnostic_data.process_metrics.get('top_cpu_processes', [])
        if top_cpu_processes:
            high_cpu_process = top_cpu_processes[0]
            if high_cpu_process.get('cpu_percent', 0) > 50:
                evidence.append(f"High CPU process: {high_cpu_process['name']} ({high_cpu_process['cpu_percent']:.1f}%)")
                confidence_score += 0.3
        
        if not evidence:
            return None
        
        return RootCauseHypothesis(
            hypothesis_id=f"resource_{violation.violation_id}",
            category=RootCauseCategory.RESOURCE_CONTENTION,
            description="Resource contention between competing processes",
            evidence=evidence,
            supporting_correlations=supporting_correlations,
            confidence_score=min(confidence_score, 1.0),
            confidence_level=self._score_to_confidence_level(confidence_score),
            likelihood_percent=confidence_score * 100,
            actionable_insights=[
                "Identify and optimize resource-intensive processes",
                "Implement resource isolation",
                "Adjust process priorities"
            ],
            prevention_recommendations=[
                "Implement resource quotas",
                "Regular process optimization",
                "Capacity-aware scheduling"
            ]
        )
    
    def _generate_application_hypothesis(self, violation: SLAViolation,
                                       correlations: List[MetricCorrelation]) -> Optional[RootCauseHypothesis]:
        """Generate application-related hypothesis"""
        evidence = []
        confidence_score = 0.0
        supporting_correlations = []
        
        # Look for application performance correlations
        app_metrics = ['latency', 'throughput', 'error_rate', 'request_time']
        
        for correlation in correlations:
            if any(metric in correlation.metric1.lower() or metric in correlation.metric2.lower()
                  for metric in app_metrics):
                if correlation.correlation_type == CorrelationType.CAUSAL:
                    evidence.append(f"Causal relationship: {correlation.metric1} -> {correlation.metric2}")
                    supporting_correlations.append(correlation)
                    confidence_score += 0.4
                elif correlation.is_strong_correlation:
                    evidence.append(f"Strong correlation: {correlation.metric1} <-> {correlation.metric2}")
                    supporting_correlations.append(correlation)
                    confidence_score += 0.2
        
        # Check violation pattern
        if violation.violation_type.value in ['latency_breach', 'throughput_degradation']:
            evidence.append(f"Application performance violation: {violation.violation_type.value}")
            confidence_score += 0.3
        
        if not evidence:
            return None
        
        return RootCauseHypothesis(
            hypothesis_id=f"app_{violation.violation_id}",
            category=RootCauseCategory.APPLICATION,
            description="Application-level performance degradation or bug",
            evidence=evidence,
            supporting_correlations=supporting_correlations,
            confidence_score=min(confidence_score, 1.0),
            confidence_level=self._score_to_confidence_level(confidence_score),
            likelihood_percent=confidence_score * 100,
            actionable_insights=[
                "Profile application performance",
                "Review recent code changes",
                "Analyze request patterns"
            ],
            prevention_recommendations=[
                "Implement comprehensive performance testing",
                "Code review for performance impact",
                "Continuous performance monitoring"
            ]
        )
    
    def _generate_network_hypothesis(self, violation: SLAViolation,
                                   diagnostic_data: DiagnosticData) -> Optional[RootCauseHypothesis]:
        """Generate network-related hypothesis"""
        evidence = []
        confidence_score = 0.0
        
        # Check network metrics
        network_metrics = diagnostic_data.network_metrics
        connections_count = network_metrics.get('connections_count', 0)
        
        if connections_count > 1000:  # High connection count
            evidence.append(f"High network connection count: {connections_count}")
            confidence_score += 0.2
        
        # Check if it's a latency-related violation (often network related)
        if 'latency' in violation.metric_name.lower():
            evidence.append("Latency violation may indicate network issues")
            confidence_score += 0.3
        
        if not evidence:
            return None
        
        return RootCauseHypothesis(
            hypothesis_id=f"network_{violation.violation_id}",
            category=RootCauseCategory.NETWORK,
            description="Network latency or connectivity issues",
            evidence=evidence,
            supporting_correlations=[],
            confidence_score=min(confidence_score, 1.0),
            confidence_level=self._score_to_confidence_level(confidence_score),
            likelihood_percent=confidence_score * 100,
            actionable_insights=[
                "Check network connectivity and latency",
                "Analyze network traffic patterns",
                "Review firewall and routing rules"
            ],
            prevention_recommendations=[
                "Implement network monitoring",
                "Regular network performance testing",
                "Redundant network paths"
            ]
        )
    
    def _generate_configuration_hypothesis(self, violation: SLAViolation,
                                         diagnostic_data: DiagnosticData) -> Optional[RootCauseHypothesis]:
        """Generate configuration-related hypothesis"""
        evidence = []
        confidence_score = 0.0
        
        config = diagnostic_data.configuration_snapshot.get('aphrodite_config', {})
        
        # Check for potentially problematic configurations
        gpu_utilization = config.get('gpu_memory_utilization', 0.9)
        if gpu_utilization > 0.95:
            evidence.append(f"Very high GPU memory utilization configured: {gpu_utilization}")
            confidence_score += 0.2
        
        max_model_len = config.get('max_model_len', 0)
        if max_model_len > 8192:
            evidence.append(f"Large max model length configured: {max_model_len}")
            confidence_score += 0.1
        
        if not evidence:
            return None
        
        return RootCauseHypothesis(
            hypothesis_id=f"config_{violation.violation_id}",
            category=RootCauseCategory.CONFIGURATION,
            description="Sub-optimal configuration settings",
            evidence=evidence,
            supporting_correlations=[],
            confidence_score=min(confidence_score, 1.0),
            confidence_level=self._score_to_confidence_level(confidence_score),
            likelihood_percent=confidence_score * 100,
            actionable_insights=[
                "Review and optimize configuration settings",
                "Compare with recommended configurations",
                "Test configuration changes in staging"
            ],
            prevention_recommendations=[
                "Configuration validation checks",
                "Regular configuration reviews",
                "Automated configuration optimization"
            ]
        )
    
    def _score_to_confidence_level(self, score: float) -> ConfidenceLevel:
        """Convert confidence score to confidence level"""
        if score >= 0.9:
            return ConfidenceLevel.VERY_HIGH
        elif score >= 0.7:
            return ConfidenceLevel.HIGH
        elif score >= 0.3:
            return ConfidenceLevel.MEDIUM
        else:
            return ConfidenceLevel.LOW


class RCAEngine:
    """
    Root Cause Analysis Engine for automated incident analysis with
    correlation detection and actionable insights generation.
    """
    
    def __init__(self, retention_hours: int = 168):  # 1 week retention
        self.retention_hours = retention_hours
        self.metrics_collector = MetricsCollector(retention_hours)
        self.correlation_analyzer = CorrelationAnalyzer()
        self.diagnostic_collector = DiagnosticCollector()
        self.hypothesis_generator = HypothesisGenerator()
        
        self.analyses: List[RCAnalysis] = []
        self.rca_callbacks: List[Callable[[RCAnalysis], None]] = []
        
        self.stats_dir = Path("/tmp/rca_analyses")
        self.stats_dir.mkdir(exist_ok=True)
        
        self._lock = threading.RLock()
        
        logger.info("RCA Engine initialized with correlation analysis and hypothesis generation")
    
    def record_metric(self, metric_name: str, value: float, timestamp: Optional[float] = None):
        """Record a metric for correlation analysis"""
        self.metrics_collector.add_metric(metric_name, value, timestamp)
    
    def register_rca_callback(self, callback: Callable[[RCAnalysis], None]):
        """Register callback for RCA completion"""
        self.rca_callbacks.append(callback)
        logger.info("Registered RCA callback")
    
    async def analyze_incident(self, violation: SLAViolation, 
                             recovery_execution: Optional[RecoveryExecution] = None) -> RCAnalysis:
        """Perform comprehensive root cause analysis for an incident"""
        
        start_time = time.time()
        analysis_id = f"rca_{violation.violation_id}_{int(start_time)}"
        
        logger.info(f"Starting root cause analysis: {analysis_id}")
        
        try:
            # Collect diagnostic data
            diagnostic_data = self.diagnostic_collector.collect_diagnostic_data(
                incident_id=violation.violation_id,
                timestamp=violation.timestamp
            )
            
            # Find correlations
            correlations = []
            
            # Temporal correlations around incident time
            temporal_correlations = self.correlation_analyzer.find_temporal_correlations(
                self.metrics_collector, violation.timestamp, window_minutes=60
            )
            correlations.extend(temporal_correlations)
            
            # Causal relationships
            causal_correlations = self.correlation_analyzer.find_causal_relationships(
                self.metrics_collector, violation
            )
            correlations.extend(causal_correlations)
            
            # Generate hypotheses
            hypotheses = self.hypothesis_generator.generate_hypotheses(
                violation, correlations, diagnostic_data
            )
            
            # Select primary root cause (highest confidence)
            primary_cause = None
            if hypotheses:
                primary_cause = max(hypotheses, key=lambda h: h.confidence_score)
            
            # Generate recommendations
            recommendations = self._generate_recommendations(hypotheses, correlations, violation)
            
            # Calculate confidence summary
            confidence_summary = {level.value: 0 for level in ConfidenceLevel}
            for hypothesis in hypotheses:
                confidence_summary[hypothesis.confidence_level.value] += 1
            
            # Create analysis record
            analysis = RCAnalysis(
                analysis_id=analysis_id,
                timestamp=start_time,
                incident_violation=violation,
                recovery_execution=recovery_execution,
                diagnostic_data=diagnostic_data,
                correlations_found=correlations,
                hypotheses=hypotheses,
                primary_root_cause=primary_cause,
                analysis_duration_seconds=time.time() - start_time,
                confidence_summary=confidence_summary,
                recommendations=recommendations
            )
            
            # Store analysis
            with self._lock:
                self.analyses.append(analysis)
            
            # Save to file
            await self._save_analysis_report(analysis)
            
            logger.info(f"RCA completed: {analysis_id} - Found {len(correlations)} correlations, "
                       f"{len(hypotheses)} hypotheses")
            
            # Notify callbacks
            for callback in self.rca_callbacks:
                try:
                    callback(analysis)
                except Exception as e:
                    logger.error(f"Error in RCA callback: {e}")
            
            return analysis
            
        except Exception as e:
            logger.error(f"Error in root cause analysis {analysis_id}: {e}")
            # Create minimal analysis record for the error case
            return RCAnalysis(
                analysis_id=analysis_id,
                timestamp=start_time,
                incident_violation=violation,
                recovery_execution=recovery_execution,
                diagnostic_data=DiagnosticData(
                    timestamp=start_time,
                    incident_id=violation.violation_id,
                    system_metrics={},
                    process_metrics={},
                    network_metrics={},
                    application_logs=[],
                    error_traces=[],
                    configuration_snapshot={},
                    resource_usage={}
                ),
                correlations_found=[],
                hypotheses=[],
                primary_root_cause=None,
                analysis_duration_seconds=time.time() - start_time,
                confidence_summary={level.value: 0 for level in ConfidenceLevel},
                recommendations=["RCA analysis failed - manual investigation required"]
            )
    
    def _generate_recommendations(self, hypotheses: List[RootCauseHypothesis],
                                correlations: List[MetricCorrelation],
                                violation: SLAViolation) -> List[str]:
        """Generate actionable recommendations based on analysis"""
        
        recommendations = []
        
        # Add recommendations from high-confidence hypotheses
        for hypothesis in hypotheses:
            if hypothesis.confidence_level in [ConfidenceLevel.HIGH, ConfidenceLevel.VERY_HIGH]:
                recommendations.extend(hypothesis.actionable_insights)
        
        # Add correlation-based recommendations
        strong_correlations = [c for c in correlations if c.is_strong_correlation]
        if strong_correlations:
            recommendations.append(
                f"Investigate strong correlations found between metrics: "
                f"{', '.join(set([c.metric1 for c in strong_correlations] + [c.metric2 for c in strong_correlations]))}"
            )
        
        # Add violation-specific recommendations
        if violation.severity == ViolationSeverity.CRITICAL:
            recommendations.append("Implement immediate incident response procedures")
            recommendations.append("Consider emergency capacity scaling")
        
        # Remove duplicates while preserving order
        seen = set()
        unique_recommendations = []
        for rec in recommendations:
            if rec not in seen:
                seen.add(rec)
                unique_recommendations.append(rec)
        
        return unique_recommendations[:10]  # Limit to top 10 recommendations
    
    async def _save_analysis_report(self, analysis: RCAnalysis):
        """Save RCA analysis report to file"""
        try:
            report_file = self.stats_dir / f"rca_{analysis.analysis_id}.json"
            
            report_data = {
                'analysis_summary': analysis.to_dict(),
                'detailed_findings': {
                    'correlations': [
                        {
                            'metric1': c.metric1,
                            'metric2': c.metric2,
                            'type': c.correlation_type.value,
                            'strength': c.correlation_strength,
                            'confidence': c.confidence.value
                        }
                        for c in analysis.correlations_found
                    ],
                    'hypotheses': [
                        {
                            'category': h.category.value,
                            'description': h.description,
                            'confidence': h.confidence_level.value,
                            'likelihood': h.likelihood_percent,
                            'evidence': h.evidence,
                            'insights': h.actionable_insights
                        }
                        for h in analysis.hypotheses
                    ]
                },
                'diagnostic_data': analysis.diagnostic_data.to_dict()
            }
            
            with open(report_file, 'w') as f:
                json.dump(report_data, f, indent=2)
                
            logger.info(f"RCA report saved: {report_file}")
            
        except Exception as e:
            logger.error(f"Error saving RCA report: {e}")
    
    def get_analysis_by_id(self, analysis_id: str) -> Optional[RCAnalysis]:
        """Get analysis by ID"""
        with self._lock:
            for analysis in self.analyses:
                if analysis.analysis_id == analysis_id:
                    return analysis
        return None
    
    def get_recent_analyses(self, hours: int = 24) -> List[RCAnalysis]:
        """Get analyses from the last N hours"""
        cutoff_time = time.time() - (hours * 3600)
        return [a for a in self.analyses if a.timestamp >= cutoff_time]
    
    def get_rca_summary(self) -> Dict[str, Any]:
        """Get comprehensive RCA engine summary"""
        with self._lock:
            total_analyses = len(self.analyses)
            recent_analyses = self.get_recent_analyses(24)
            
            # Calculate confidence distribution
            confidence_dist = {level.value: 0 for level in ConfidenceLevel}
            category_dist = {category.value: 0 for category in RootCauseCategory}
            
            for analysis in recent_analyses:
                if analysis.primary_root_cause:
                    confidence_dist[analysis.primary_root_cause.confidence_level.value] += 1
                    category_dist[analysis.primary_root_cause.category.value] += 1
            
            # Calculate average analysis time
            if recent_analyses:
                avg_analysis_time = statistics.mean([a.analysis_duration_seconds for a in recent_analyses])
            else:
                avg_analysis_time = 0.0
            
            return {
                'total_analyses': total_analyses,
                'analyses_24h': len(recent_analyses),
                'average_analysis_time_seconds': avg_analysis_time,
                'confidence_distribution': confidence_dist,
                'root_cause_categories': category_dist,
                'metrics_tracked': len(self.metrics_collector.metrics_history),
                'retention_hours': self.retention_hours
            }


def create_production_rca_engine() -> RCAEngine:
    """Create a production-configured RCA engine"""
    return RCAEngine(retention_hours=168)  # 1 week retention


if __name__ == "__main__":
    # Demo usage
    import asyncio
    logging.basicConfig(level=logging.INFO)
    
    print("🔍 Root Cause Analysis Engine Demo")
    print("=" * 50)
    
    async def demo_rca():
        # Create RCA engine
        rca_engine = create_production_rca_engine()
        
        def rca_handler(analysis: RCAnalysis):
            print(f"🔍 RCA Completed: {analysis.analysis_id}")
            if analysis.primary_root_cause:
                print(f"   Primary cause: {analysis.primary_root_cause.description}")
                print(f"   Confidence: {analysis.primary_root_cause.confidence_level.value}")
                print(f"   Likelihood: {analysis.primary_root_cause.likelihood_percent:.1f}%")
        
        rca_engine.register_rca_callback(rca_handler)
        
        # Add some metric history
        current_time = time.time()
        for i in range(100):
            timestamp = current_time - (100 - i) * 60  # 100 minutes of data
            
            # Simulate normal then degraded performance
            if i < 80:
                latency = 150 + (i * 2)  # Gradual increase
                cpu = 60 + (i * 0.3)
            else:
                latency = 250 + (i * 5)  # Sharp increase
                cpu = 85 + (i * 0.5)
            
            rca_engine.record_metric("request_latency_p95", latency, timestamp)
            rca_engine.record_metric("cpu_usage", cpu, timestamp)
            rca_engine.record_metric("memory_usage", 70 + (i * 0.2), timestamp)
        
        print("📊 Added metric history for correlation analysis")
        
        # Create a mock violation
        from aphrodite.engine.sla_manager import SLAViolation, SLAViolationType, ViolationSeverity, SLAThreshold
        
        mock_threshold = SLAThreshold(
            metric_name="request_latency_p95",
            target_value=200.0,
            tolerance_percent=25.0
        )
        
        mock_violation = SLAViolation(
            violation_id="test_rca_violation",
            timestamp=current_time,
            violation_type=SLAViolationType.LATENCY_BREACH,
            severity=ViolationSeverity.CRITICAL,
            metric_name="request_latency_p95", 
            threshold=mock_threshold,
            actual_value=350.0,
            expected_value=200.0,
            breach_percentage=40.0,
            measurement_window=[300.0, 320.0, 350.0, 340.0, 360.0]
        )
        
        print("🚨 Running RCA for simulated violation...")
        
        # Perform RCA
        analysis = await rca_engine.analyze_incident(mock_violation)
        
        print(f"\n📈 RCA Results:")
        print(f"   Analysis ID: {analysis.analysis_id}")
        print(f"   Duration: {analysis.analysis_duration_seconds:.2f}s")
        print(f"   Correlations found: {len(analysis.correlations_found)}")
        print(f"   Hypotheses generated: {len(analysis.hypotheses)}")
        print(f"   Recommendations: {len(analysis.recommendations)}")
        
        if analysis.recommendations:
            print("\n💡 Top Recommendations:")
            for i, rec in enumerate(analysis.recommendations[:3], 1):
                print(f"   {i}. {rec}")
        
        # Get summary
        summary = rca_engine.get_rca_summary()
        print(f"\n📊 RCA Engine Summary:")
        print(f"   Total analyses: {summary['total_analyses']}")
        print(f"   Metrics tracked: {summary['metrics_tracked']}")
        print(f"   Average analysis time: {summary['average_analysis_time_seconds']:.2f}s")
    
    # Run async demo
    try:
        asyncio.run(demo_rca())
    except KeyboardInterrupt:
        print("\n🛑 Demo interrupted")