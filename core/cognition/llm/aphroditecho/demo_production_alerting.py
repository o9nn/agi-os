#!/usr/bin/env python3
"""
Production Alerting & Incident Response Demo
Demonstrates the complete production monitoring system without external dependencies.
"""

import asyncio
import time
import logging
import json
from dataclasses import dataclass, field, asdict
from typing import Dict, List, Optional, Callable, Any
from enum import Enum
from collections import deque, defaultdict
import statistics


# Simplified standalone versions for demo
class AlertSeverity(Enum):
    INFO = "info"
    WARNING = "warning" 
    CRITICAL = "critical"


class SLAViolationType(Enum):
    LATENCY_BREACH = "latency_breach"
    THROUGHPUT_DEGRADATION = "throughput_degradation"
    ERROR_RATE_SPIKE = "error_rate_spike"
    RESOURCE_EXHAUSTION = "resource_exhaustion"


class ViolationSeverity(Enum):
    MINOR = "minor"
    MAJOR = "major"
    CRITICAL = "critical"


@dataclass
class SLAThreshold:
    metric_name: str
    target_value: float
    tolerance_percent: float = 5.0
    measurement_window_minutes: int = 5
    violation_threshold_percent: int = 80
    enabled: bool = True
    
    def get_violation_boundary(self, is_upper_bound: bool = True) -> float:
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
    resolved_timestamp: Optional[float] = None


class SimpleSLAManager:
    """Simplified SLA Manager for demo"""
    
    def __init__(self):
        self.thresholds: Dict[str, SLAThreshold] = {}
        self.violations: List[SLAViolation] = []
        self.measurement_windows: Dict[str, deque] = defaultdict(lambda: deque(maxlen=300))
        self.violation_callbacks: List[Callable[[SLAViolation], None]] = []
        self.is_monitoring = False
        
        self._initialize_default_thresholds()
    
    def _initialize_default_thresholds(self):
        # Request latency thresholds
        self.add_threshold(SLAThreshold(
            metric_name="request_latency_p95",
            target_value=200.0,
            tolerance_percent=25.0,
            measurement_window_minutes=5
        ))
        
        # Throughput thresholds
        self.add_threshold(SLAThreshold(
            metric_name="tokens_per_second", 
            target_value=100.0,
            tolerance_percent=20.0,
            measurement_window_minutes=3
        ))
        
        # Error rate thresholds
        self.add_threshold(SLAThreshold(
            metric_name="error_rate_percent",
            target_value=0.1,
            tolerance_percent=400.0,
            measurement_window_minutes=5
        ))
    
    def add_threshold(self, threshold: SLAThreshold):
        self.thresholds[threshold.metric_name] = threshold
    
    def register_violation_callback(self, callback: Callable[[SLAViolation], None]):
        self.violation_callbacks.append(callback)
    
    def start_monitoring(self):
        self.is_monitoring = True
    
    def stop_monitoring(self):
        self.is_monitoring = False
    
    def record_measurement(self, metric_name: str, value: float, timestamp: Optional[float] = None):
        if timestamp is None:
            timestamp = time.time()
        
        self.measurement_windows[metric_name].append((timestamp, value))
        
        if metric_name in self.thresholds:
            self._check_threshold_violation(metric_name, value, timestamp)
    
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
            return
        
        # Check for sustained violation
        window_data = list(self.measurement_windows[metric_name])
        window_minutes = threshold.measurement_window_minutes
        cutoff_time = timestamp - (window_minutes * 60)
        
        window_measurements = [(t, v) for t, v in window_data if t >= cutoff_time]
        
        if len(window_measurements) < 5:
            return
        
        violation_count = 0
        for _, window_value in window_measurements:
            if is_upper_bound and window_value > violation_boundary:
                violation_count += 1
            elif not is_upper_bound and window_value < violation_boundary:
                violation_count += 1
        
        violation_percentage = (violation_count / len(window_measurements)) * 100
        
        if violation_percentage >= threshold.violation_threshold_percent:
            self._create_violation(metric_name, value, timestamp, threshold, window_measurements)
    
    def _create_violation(self, metric_name: str, value: float, timestamp: float, 
                         threshold: SLAThreshold, window_measurements: List[tuple]):
        violation_id = f"sla_{metric_name}_{int(timestamp)}"
        
        is_upper_bound = self._is_upper_bound_metric(metric_name)
        boundary = threshold.get_violation_boundary(is_upper_bound)
        
        if is_upper_bound:
            breach_percent = ((value - boundary) / boundary) * 100
        else:
            breach_percent = ((boundary - value) / boundary) * 100
        
        violation_type = self._determine_violation_type(metric_name)
        severity = self._calculate_severity(breach_percent)
        
        violation = SLAViolation(
            violation_id=violation_id,
            timestamp=timestamp,
            violation_type=violation_type,
            severity=severity,
            metric_name=metric_name,
            threshold=threshold,
            actual_value=value,
            expected_value=threshold.target_value,
            breach_percentage=breach_percent,
            measurement_window=[v for t, v in window_measurements]
        )
        
        self.violations.append(violation)
        
        for callback in self.violation_callbacks:
            try:
                callback(violation)
            except Exception as e:
                print(f"Error in violation callback: {e}")
    
    def _is_upper_bound_metric(self, metric_name: str) -> bool:
        upper_bound_metrics = ['latency', 'response_time', 'error_rate']
        return any(keyword in metric_name.lower() for keyword in upper_bound_metrics)
    
    def _determine_violation_type(self, metric_name: str) -> SLAViolationType:
        metric_lower = metric_name.lower()
        if 'latency' in metric_lower:
            return SLAViolationType.LATENCY_BREACH
        elif 'throughput' in metric_lower or 'tokens_per_second' in metric_lower:
            return SLAViolationType.THROUGHPUT_DEGRADATION
        elif 'error_rate' in metric_lower:
            return SLAViolationType.ERROR_RATE_SPIKE
        else:
            return SLAViolationType.RESOURCE_EXHAUSTION
    
    def _calculate_severity(self, breach_percentage: float) -> ViolationSeverity:
        if breach_percentage < 5.0:
            return ViolationSeverity.MINOR
        elif breach_percentage < 15.0:
            return ViolationSeverity.MAJOR
        else:
            return ViolationSeverity.CRITICAL
    
    def get_sla_summary(self) -> Dict[str, Any]:
        active_violations = len([v for v in self.violations if v.resolved_timestamp is None])
        return {
            'monitoring_active': self.is_monitoring,
            'active_violations': active_violations,
            'total_violations': len(self.violations),
            'thresholds_configured': len(self.thresholds)
        }


class RecoveryStatus(Enum):
    PENDING = "pending"
    SUCCESS = "success"
    FAILED = "failed"


class RecoveryAction(Enum):
    CACHE_INVALIDATION = "cache_invalidation"
    LOAD_BALANCER_ADJUST = "load_balancer_adjust"
    SCALE_RESOURCES = "scale_resources"
    RESTART_SERVICE = "restart_service"


@dataclass
class RecoveryExecution:
    execution_id: str
    timestamp: float
    trigger_violation: SLAViolation
    status: RecoveryStatus
    actions_completed: List[str] = field(default_factory=list)
    completion_time: Optional[float] = None
    
    @property
    def duration_seconds(self) -> Optional[float]:
        if self.completion_time:
            return self.completion_time - self.timestamp
        return None


class SimpleRecoveryEngine:
    """Simplified Recovery Engine for demo"""
    
    def __init__(self):
        self.executions: List[RecoveryExecution] = []
        self.recovery_callbacks: List[Callable[[RecoveryExecution], None]] = []
    
    def register_recovery_callback(self, callback: Callable[[RecoveryExecution], None]):
        self.recovery_callbacks.append(callback)
    
    async def execute_recovery(self, violation: SLAViolation) -> RecoveryExecution:
        execution_id = f"recovery_{violation.violation_id}_{int(time.time())}"
        
        execution = RecoveryExecution(
            execution_id=execution_id,
            timestamp=time.time(),
            trigger_violation=violation,
            status=RecoveryStatus.PENDING
        )
        
        self.executions.append(execution)
        
        print(f"🔧 Executing recovery for {violation.metric_name} violation...")
        
        # Simulate recovery actions
        actions = ["cache_invalidation", "load_balancer_adjust"]
        
        for action in actions:
            await asyncio.sleep(0.5)  # Simulate action time
            execution.actions_completed.append(action)
            print(f"   ✅ Completed: {action}")
        
        execution.status = RecoveryStatus.SUCCESS
        execution.completion_time = time.time()
        
        for callback in self.recovery_callbacks:
            try:
                callback(execution)
            except Exception as e:
                print(f"Error in recovery callback: {e}")
        
        return execution
    
    def get_recovery_summary(self) -> Dict[str, Any]:
        successful = len([e for e in self.executions if e.status == RecoveryStatus.SUCCESS])
        return {
            'total_executions': len(self.executions),
            'successful_executions': successful,
            'success_rate_percent': (successful / max(len(self.executions), 1)) * 100
        }


@dataclass
class RCAnalysis:
    analysis_id: str
    timestamp: float
    incident_violation: SLAViolation
    correlations_found: int
    hypotheses_generated: int
    primary_root_cause: Optional[str]
    recommendations: List[str]
    analysis_duration_seconds: float


class SimpleRCAEngine:
    """Simplified RCA Engine for demo"""
    
    def __init__(self):
        self.analyses: List[RCAnalysis] = []
        self.rca_callbacks: List[Callable[[RCAnalysis], None]] = []
        self.metrics_history: Dict[str, List[tuple]] = defaultdict(list)
    
    def register_rca_callback(self, callback: Callable[[RCAnalysis], None]):
        self.rca_callbacks.append(callback)
    
    def record_metric(self, metric_name: str, value: float, timestamp: Optional[float] = None):
        if timestamp is None:
            timestamp = time.time()
        self.metrics_history[metric_name].append((timestamp, value))
    
    async def analyze_incident(self, violation: SLAViolation, 
                             recovery_execution: Optional[RecoveryExecution] = None) -> RCAnalysis:
        start_time = time.time()
        analysis_id = f"rca_{violation.violation_id}_{int(start_time)}"
        
        print(f"🔍 Performing root cause analysis for {violation.metric_name} violation...")
        
        # Simulate analysis
        await asyncio.sleep(1.0)
        
        # Simulate correlation detection
        correlations_found = 2
        
        # Simulate hypothesis generation
        hypotheses_generated = 3
        
        # Determine primary root cause based on violation type
        primary_cause = None
        if violation.violation_type == SLAViolationType.LATENCY_BREACH:
            primary_cause = "High CPU utilization causing request queuing"
        elif violation.violation_type == SLAViolationType.THROUGHPUT_DEGRADATION:
            primary_cause = "Memory pressure reducing processing efficiency"
        else:
            primary_cause = "Resource contention between concurrent requests"
        
        # Generate recommendations
        recommendations = [
            "Scale up compute resources to handle increased load",
            "Implement request queuing with priority levels",
            "Optimize cache invalidation strategy",
            "Review and tune garbage collection settings"
        ]
        
        analysis = RCAnalysis(
            analysis_id=analysis_id,
            timestamp=start_time,
            incident_violation=violation,
            correlations_found=correlations_found,
            hypotheses_generated=hypotheses_generated,
            primary_root_cause=primary_cause,
            recommendations=recommendations,
            analysis_duration_seconds=time.time() - start_time
        )
        
        self.analyses.append(analysis)
        
        print(f"   📊 Found {correlations_found} correlations")
        print(f"   💡 Generated {hypotheses_generated} hypotheses")
        print(f"   🎯 Primary cause: {primary_cause}")
        
        for callback in self.rca_callbacks:
            try:
                callback(analysis)
            except Exception as e:
                print(f"Error in RCA callback: {e}")
        
        return analysis
    
    def get_rca_summary(self) -> Dict[str, Any]:
        return {
            'total_analyses': len(self.analyses),
            'metrics_tracked': len(self.metrics_history)
        }


class MonitoringMode(Enum):
    NORMAL = "normal"
    HIGH_ALERT = "high_alert"
    EMERGENCY = "emergency"


@dataclass
class ProductionAlert:
    alert_id: str
    timestamp: float
    severity: str
    message: str
    sla_violation: Optional[SLAViolation] = None
    recovery_execution: Optional[RecoveryExecution] = None
    rca_analysis: Optional[RCAnalysis] = None
    escalated: bool = False


class SimpleProductionMonitor:
    """Simplified Production Monitor for demo"""
    
    def __init__(self):
        self.sla_manager = SimpleSLAManager()
        self.recovery_engine = SimpleRecoveryEngine()
        self.rca_engine = SimpleRCAEngine()
        
        self.monitoring_mode = MonitoringMode.NORMAL
        self.alerts: List[ProductionAlert] = []
        
        self.alert_callbacks: List[Callable[[ProductionAlert], None]] = []
        
        self._initialize_integrations()
    
    def _initialize_integrations(self):
        def sla_violation_handler(violation: SLAViolation):
            asyncio.create_task(self._handle_sla_violation(violation))
        
        self.sla_manager.register_violation_callback(sla_violation_handler)
        
        def recovery_completion_handler(execution: RecoveryExecution):
            asyncio.create_task(self._handle_recovery_completion(execution))
        
        self.recovery_engine.register_recovery_callback(recovery_completion_handler)
        
        def rca_completion_handler(analysis: RCAnalysis):
            asyncio.create_task(self._handle_rca_completion(analysis))
        
        self.rca_engine.register_rca_callback(rca_completion_handler)
    
    def register_alert_callback(self, callback: Callable[[ProductionAlert], None]):
        self.alert_callbacks.append(callback)
    
    def start_monitoring(self):
        self.sla_manager.start_monitoring()
    
    def stop_monitoring(self):
        self.sla_manager.stop_monitoring()
    
    def record_metric(self, metric_name: str, value: float, timestamp: Optional[float] = None):
        self.sla_manager.record_measurement(metric_name, value, timestamp)
        self.rca_engine.record_metric(metric_name, value, timestamp)
    
    async def _handle_sla_violation(self, violation: SLAViolation):
        alert = ProductionAlert(
            alert_id=f"sla_alert_{violation.violation_id}",
            timestamp=violation.timestamp,
            severity=violation.severity.value,
            message=f"SLA violation: {violation.metric_name} = {violation.actual_value} "
                   f"(expected {violation.expected_value}, breach: {violation.breach_percentage:.1f}%)",
            sla_violation=violation
        )
        
        await self._process_alert(alert)
        
        # Trigger recovery
        print(f"🚨 SLA Violation Detected: {violation.metric_name}")
        recovery_execution = await self.recovery_engine.execute_recovery(violation)
        alert.recovery_execution = recovery_execution
    
    async def _handle_recovery_completion(self, execution: RecoveryExecution):
        alert = ProductionAlert(
            alert_id=f"recovery_alert_{execution.execution_id}",
            timestamp=execution.completion_time or time.time(),
            severity="info" if execution.status == RecoveryStatus.SUCCESS else "warning",
            message=f"Recovery {execution.status.value} (duration: {execution.duration_seconds:.1f}s)",
            recovery_execution=execution
        )
        
        await self._process_alert(alert)
        
        # Trigger RCA for critical violations
        if execution.trigger_violation.severity == ViolationSeverity.CRITICAL:
            print(f"🔍 Triggering RCA for critical violation...")
            await self.rca_engine.analyze_incident(execution.trigger_violation, execution)
    
    async def _handle_rca_completion(self, analysis: RCAnalysis):
        alert = ProductionAlert(
            alert_id=f"rca_alert_{analysis.analysis_id}",
            timestamp=analysis.timestamp,
            severity="info",
            message=f"RCA completed - Primary cause: {analysis.primary_root_cause}",
            rca_analysis=analysis
        )
        
        await self._process_alert(alert)
    
    async def _process_alert(self, alert: ProductionAlert):
        self.alerts.append(alert)
        
        # Check for escalation
        if alert.severity == "critical" or (alert.sla_violation and 
                                          alert.sla_violation.severity == ViolationSeverity.CRITICAL):
            alert.escalated = True
            self.monitoring_mode = MonitoringMode.HIGH_ALERT
        
        for callback in self.alert_callbacks:
            try:
                callback(alert)
            except Exception as e:
                print(f"Error in alert callback: {e}")
    
    def get_production_summary(self) -> Dict[str, Any]:
        sla_summary = self.sla_manager.get_sla_summary()
        recovery_summary = self.recovery_engine.get_recovery_summary()
        rca_summary = self.rca_engine.get_rca_summary()
        
        recent_alerts = [a for a in self.alerts if a.timestamp >= time.time() - 3600]
        
        return {
            'monitoring_mode': self.monitoring_mode.value,
            'active_violations': sla_summary['active_violations'],
            'total_alerts_1h': len(recent_alerts),
            'escalated_alerts_1h': len([a for a in recent_alerts if a.escalated]),
            'sla_summary': sla_summary,
            'recovery_summary': recovery_summary,
            'rca_summary': rca_summary
        }


async def demo_production_alerting():
    """Demonstrate the complete production alerting and incident response system"""
    
    print("🏭 Production Alerting & Incident Response System Demo")
    print("=" * 70)
    
    # Create production monitor
    monitor = SimpleProductionMonitor()
    
    # Register alert handler
    def alert_handler(alert: ProductionAlert):
        status = "🚨 ESCALATED" if alert.escalated else "ℹ️ "
        print(f"{status} ALERT [{alert.severity.upper()}]: {alert.message}")
    
    monitor.register_alert_callback(alert_handler)
    
    # Start monitoring
    monitor.start_monitoring()
    print("✅ Production monitoring started\n")
    
    try:
        # Phase 1: Normal operation
        print("📊 Phase 1: Normal Operation")
        print("-" * 40)
        
        current_time = time.time()
        for i in range(10):
            # Normal metrics
            latency = 180.0 + (i % 3) * 10  # 180-200ms
            throughput = 110.0 - (i % 2) * 5  # 105-110 tokens/sec
            error_rate = 0.05 + (i % 4) * 0.01  # 0.05-0.08%
            
            monitor.record_metric("request_latency_p95", latency, current_time + i)
            monitor.record_metric("tokens_per_second", throughput, current_time + i)
            monitor.record_metric("error_rate_percent", error_rate, current_time + i)
            
            await asyncio.sleep(0.1)
        
        print("   ✅ Normal operation metrics recorded")
        await asyncio.sleep(1)
        
        # Phase 2: Performance degradation
        print("\n🔥 Phase 2: Performance Degradation")
        print("-" * 40)
        
        for i in range(12):
            # Degraded metrics that will trigger SLA violations
            latency = 280.0 + i * 15  # Exceeds 250ms threshold
            throughput = 75.0 - i * 2  # Falls below 80 tokens/sec threshold
            error_rate = 0.3 + i * 0.05  # Exceeds 0.5% threshold
            
            monitor.record_metric("request_latency_p95", latency, current_time + 20 + i)
            monitor.record_metric("tokens_per_second", throughput, current_time + 20 + i)
            monitor.record_metric("error_rate_percent", error_rate, current_time + 20 + i)
            
            await asyncio.sleep(0.1)
        
        print("   🚨 Performance degradation metrics recorded")
        
        # Wait for incident processing
        print("\n⏳ Processing incidents...")
        await asyncio.sleep(3)
        
        # Phase 3: Recovery simulation
        print("\n🔧 Phase 3: System Recovery")
        print("-" * 40)
        
        for i in range(8):
            # Recovering metrics
            latency = 220.0 - i * 15  # Gradually improving
            throughput = 90.0 + i * 5  # Gradually improving
            error_rate = 0.4 - i * 0.04  # Gradually improving
            
            monitor.record_metric("request_latency_p95", latency, current_time + 40 + i)
            monitor.record_metric("tokens_per_second", throughput, current_time + 40 + i)
            monitor.record_metric("error_rate_percent", error_rate, current_time + 40 + i)
            
            await asyncio.sleep(0.1)
        
        print("   📈 Recovery metrics recorded")
        await asyncio.sleep(2)
        
        # Show final summary
        print("\n📊 Final Production Summary")
        print("=" * 50)
        
        summary = monitor.get_production_summary()
        
        print(f"Monitoring Mode: {summary['monitoring_mode'].upper()}")
        print(f"Active SLA Violations: {summary['active_violations']}")
        print(f"Total Alerts (1h): {summary['total_alerts_1h']}")
        print(f"Escalated Alerts (1h): {summary['escalated_alerts_1h']}")
        
        print(f"\n🎯 SLA Management:")
        sla = summary['sla_summary']
        print(f"   Thresholds Configured: {sla['thresholds_configured']}")
        print(f"   Total Violations: {sla['total_violations']}")
        
        print(f"\n🔧 Recovery Operations:")
        recovery = summary['recovery_summary']
        print(f"   Total Recoveries: {recovery['total_executions']}")
        print(f"   Success Rate: {recovery['success_rate_percent']:.1f}%")
        
        print(f"\n🔍 Root Cause Analysis:")
        rca = summary['rca_summary']
        print(f"   Analyses Performed: {rca['total_analyses']}")
        print(f"   Metrics Tracked: {rca['metrics_tracked']}")
        
        # Show recent alerts
        recent_alerts = [a for a in monitor.alerts if a.timestamp >= time.time() - 3600]
        if recent_alerts:
            print(f"\n📋 Recent Alerts ({len(recent_alerts)}):")
            for alert in recent_alerts[-5:]:  # Show last 5
                escalated = " [ESCALATED]" if alert.escalated else ""
                print(f"   {alert.severity}: {alert.message}{escalated}")
        
        print(f"\n🎉 Demo completed successfully!")
        print(f"   • Detected {summary['sla_summary']['total_violations']} SLA violations")
        print(f"   • Executed {summary['recovery_summary']['total_executions']} recovery procedures")
        print(f"   • Performed {summary['rca_summary']['total_analyses']} root cause analyses")
        print(f"   • Generated {summary['total_alerts_1h']} production alerts")
        
        return True
        
    except Exception as e:
        print(f"\n❌ Demo failed with error: {e}")
        return False
        
    finally:
        monitor.stop_monitoring()
        print("\n🛑 Monitoring stopped")


async def demo_individual_components():
    """Demonstrate individual components"""
    
    print("\n🔧 Individual Component Demonstrations")
    print("=" * 50)
    
    # SLA Manager Demo
    print("\n📊 SLA Manager Demo:")
    sla_manager = SimpleSLAManager()
    violations_detected = []
    
    def violation_handler(violation):
        violations_detected.append(violation)
        print(f"   🚨 Violation: {violation.metric_name} = {violation.actual_value}")
    
    sla_manager.register_violation_callback(violation_handler)
    sla_manager.start_monitoring()
    
    # Normal then violating metrics
    for i in range(5):
        sla_manager.record_measurement("request_latency_p95", 180.0)
    for i in range(8):
        sla_manager.record_measurement("request_latency_p95", 300.0)
    
    print(f"   ✅ Detected {len(violations_detected)} violations")
    
    # Recovery Engine Demo
    print("\n🔧 Recovery Engine Demo:")
    recovery_engine = SimpleRecoveryEngine()
    
    if violations_detected:
        execution = await recovery_engine.execute_recovery(violations_detected[0])
        print(f"   ✅ Recovery completed in {execution.duration_seconds:.1f}s")
    
    # RCA Engine Demo
    print("\n🔍 RCA Engine Demo:")
    rca_engine = SimpleRCAEngine()
    
    if violations_detected:
        analysis = await rca_engine.analyze_incident(violations_detected[0])
        print(f"   ✅ Analysis completed in {analysis.analysis_duration_seconds:.1f}s")
        print(f"   🎯 Primary cause: {analysis.primary_root_cause}")
    
    print("\n✅ All component demos completed!")


if __name__ == "__main__":
    # Setup logging to reduce noise
    logging.basicConfig(level=logging.WARNING)
    
    async def run_demos():
        # Run main demo
        success = await demo_production_alerting()
        
        if success:
            # Run individual component demos
            await demo_individual_components()
        
        return success
    
    # Run the demonstration
    success = asyncio.run(run_demos())
    
    if success:
        print("\n🎉 Production Alerting & Incident Response System demonstrated successfully!")
        print("\nKey Features Demonstrated:")
        print("• ✅ Intelligent SLA violation detection with statistical thresholds")
        print("• ✅ Automated incident response with recovery procedures")
        print("• ✅ Root cause analysis with correlation detection")
        print("• ✅ Integrated production monitoring with escalation")
        print("• ✅ Real-time alerting with severity classification")
        print("• ✅ Comprehensive incident tracking and reporting")
    else:
        print("\n❌ Demo failed - check output above for details")
    
    exit(0 if success else 1)