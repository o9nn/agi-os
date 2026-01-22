import time
import logging
import asyncio
import threading
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Callable, Any, Set
from enum import Enum
from pathlib import Path
import json
from aphrodite.engine.sla_manager import SLAManager, SLAViolation, create_production_sla_manager
from aphrodite.engine.recovery_engine import RecoveryEngine, RecoveryExecution, create_production_recovery_engine
from aphrodite.engine.rca_engine import RCAEngine, RCAnalysis, create_production_rca_engine
from aphrodite.engine.metrics import Metrics, Stats
from aphrodite.engine.metrics_types import StatLoggerBase
logger = logging.getLogger(__name__)
class MonitoringMode(Enum):
    NORMAL = 'normal'
    HIGH_ALERT = 'high_alert'
    MAINTENANCE = 'maintenance'
    EMERGENCY = 'emergency'
@dataclass
class ProductionAlert:
    alert_id: str
    timestamp: float
    severity: str
    component: str
    message: str
    sla_violation: Optional[SLAViolation] = None
    recovery_execution: Optional[RecoveryExecution] = None
    rca_analysis: Optional[RCAnalysis] = None
    escalated: bool = False
    resolved_timestamp: Optional[float] = None
    context: Dict[str, Any] = field(default_factory=dict)
@dataclass
class ProductionHealth:
    timestamp: float
    overall_status: str
    sla_compliance_percent: float
    active_violations: int
    active_recoveries: int
    system_metrics: Dict[str, float]
    service_health: Dict[str, bool]
    recent_alerts: List[ProductionAlert]
class ProductionMonitor:
    def __init__(self):
        self.sla_manager = create_production_sla_manager()
        self.recovery_engine = create_production_recovery_engine()
        self.rca_engine = create_production_rca_engine()
        self.monitoring_mode = MonitoringMode.NORMAL
        self.alerts: List[ProductionAlert] = []
        self.health_history: List[ProductionHealth] = []
        self.alert_callbacks: List[Callable[[ProductionAlert], None]] = []
        self.health_callbacks: List[Callable[[ProductionHealth], None]] = []
        self.is_monitoring = False
        self._lock = threading.RLock()
        self._monitoring_thread = None
        self.alert_cooldown_seconds = 300
        self.health_check_interval = 60
        self.auto_recovery_enabled = True
        self.auto_rca_enabled = True
        self.stats_dir = Path('/tmp/production_monitoring')
        self.stats_dir.mkdir(exist_ok=True)
        self._initialize_integrations()
        logger.info('Production Monitor initialized with integrated SLA, recovery, and RCA')
    def _initialize_integrations(self):
        def sla_violation_handler(violation: SLAViolation):
            asyncio.create_task(self._handle_sla_violation(violation))
        self.sla_manager.register_violation_callback(sla_violation_handler)
        def recovery_completion_handler(execution: RecoveryExecution):
            if execution.status.value in ['success', 'failed']:
                asyncio.create_task(self._handle_recovery_completion(execution))
        self.recovery_engine.register_recovery_callback(recovery_completion_handler)
        def rca_completion_handler(analysis: RCAnalysis):
            asyncio.create_task(self._handle_rca_completion(analysis))
        self.rca_engine.register_rca_callback(rca_completion_handler)
        logger.info('Initialized component integrations')
    def start_monitoring(self):
        if self.is_monitoring:
            logger.warning('Production monitoring already running')
            return
        self.is_monitoring = True
        self.sla_manager.start_monitoring()
        logger.info('SLA monitoring started')
        self._monitoring_thread = threading.Thread(target=self._monitoring_loop, daemon=True, name='ProductionMonitoring')
        self._monitoring_thread.start()
        logger.info('Production monitoring started in mode: %s', self.monitoring_mode.value)
    def stop_monitoring(self):
        self.is_monitoring = False
        self.sla_manager.stop_monitoring()
        if self._monitoring_thread:
            self._monitoring_thread.join(timeout=10.0)
        logger.info('Production monitoring stopped')
    def record_metric(self, metric_name: str, value: float, timestamp: Optional[float]=None):
        self.sla_manager.record_measurement(metric_name, value, timestamp)
        self.rca_engine.record_metric(metric_name, value, timestamp)
    def record_stats(self, stats: Stats):
        current_time = time.time()
        if hasattr(stats, 'time_e2e_requests') and stats.time_e2e_requests:
            latencies = sorted(stats.time_e2e_requests)
            if latencies:
                p95_index = int(0.95 * len(latencies))
                p95_latency = latencies[p95_index] * 1000
                self.record_metric('request_latency_p95', p95_latency, current_time)
        total_tokens = stats.num_prompt_tokens_iter + stats.num_generation_tokens_iter
        if total_tokens > 0:
            tokens_per_second = total_tokens
            self.record_metric('tokens_per_second', tokens_per_second, current_time)
        self.record_metric('gpu_cache_usage_percent', stats.gpu_cache_usage_sys * 100, current_time)
        self.record_metric('cpu_cache_usage_percent', stats.cpu_cache_usage_sys * 100, current_time)
        total_requests = stats.num_running_sys + stats.num_waiting_sys
        if total_requests > 0:
            queue_utilization = stats.num_waiting_sys / total_requests * 100
            self.record_metric('queue_utilization_percent', queue_utilization, current_time)
    def set_monitoring_mode(self, mode: MonitoringMode):
        old_mode = self.monitoring_mode
        self.monitoring_mode = mode
        if mode == MonitoringMode.HIGH_ALERT:
            self.alert_cooldown_seconds = 60
            self.health_check_interval = 30
        elif mode == MonitoringMode.EMERGENCY:
            self.alert_cooldown_seconds = 30
            self.health_check_interval = 15
        elif mode == MonitoringMode.MAINTENANCE:
            self.alert_cooldown_seconds = 900
            self.health_check_interval = 300
        else:
            self.alert_cooldown_seconds = 300
            self.health_check_interval = 60
        logger.info(f'Monitoring mode changed: {old_mode.value} -> {mode.value}')
    def register_alert_callback(self, callback: Callable[[ProductionAlert], None]):
        self.alert_callbacks.append(callback)
        logger.info('Registered production alert callback')
    def register_health_callback(self, callback: Callable[[ProductionHealth], None]):
        self.health_callbacks.append(callback)
        logger.info('Registered production health callback')
    async def _handle_sla_violation(self, violation: SLAViolation):
        logger.warning(f'Handling SLA violation: {violation.violation_id}')
        alert = ProductionAlert(alert_id=f'sla_alert_{violation.violation_id}', timestamp=violation.timestamp, severity=violation.severity.value, component='sla_manager', message=f'SLA violation detected: {violation.metric_name} = {violation.actual_value} (expected {violation.expected_value}, breach: {violation.breach_percentage:.1f}%)', sla_violation=violation, context={'violation_type': violation.violation_type.value, 'metric_name': violation.metric_name, 'breach_percentage': violation.breach_percentage})
        await self._process_alert(alert)
        if self.auto_recovery_enabled:
            try:
                logger.info(f'Initiating automated recovery for violation: {violation.violation_id}')
                recovery_execution = await self.recovery_engine.execute_recovery(violation)
                if recovery_execution:
                    alert.recovery_execution = recovery_execution
                    if violation.severity.value == 'critical':
                        self.set_monitoring_mode(MonitoringMode.HIGH_ALERT)
            except Exception as e:
                logger.error(f'Error in automated recovery for {violation.violation_id}: {e}')
    async def _handle_recovery_completion(self, execution: RecoveryExecution):
        logger.info(f'Recovery completed: {execution.execution_id} - Status: {execution.status.value}')
        alert = ProductionAlert(alert_id=f'recovery_alert_{execution.execution_id}', timestamp=execution.completion_time or time.time(), severity='info' if execution.status.value == 'success' else 'warning', component='recovery_engine', message=f'Recovery {execution.status.value}: {execution.procedure.name} (duration: {execution.duration_seconds:.1f}s)', recovery_execution=execution, context={'procedure_name': execution.procedure.name, 'actions_completed': len(execution.actions_completed), 'actions_failed': len(execution.actions_failed)})
        await self._process_alert(alert)
        if self.auto_rca_enabled and (execution.status.value == 'failed' or execution.trigger_violation.severity.value == 'critical'):
            try:
                logger.info(f'Initiating RCA for recovery: {execution.execution_id}')
                await self.rca_engine.analyze_incident(execution.trigger_violation, execution)
            except Exception as e:
                logger.error(f'Error in RCA for {execution.execution_id}: {e}')
    async def _handle_rca_completion(self, analysis: RCAnalysis):
        logger.info(f'RCA completed: {analysis.analysis_id}')
        primary_cause = analysis.primary_root_cause
        message = 'Root cause analysis completed'
        if primary_cause:
            message += f' - Primary cause: {primary_cause.description} '
            message += f'(confidence: {primary_cause.confidence_level.value})'
        alert = ProductionAlert(alert_id=f'rca_alert_{analysis.analysis_id}', timestamp=analysis.timestamp, severity='info', component='rca_engine', message=message, rca_analysis=analysis, context={'correlations_found': len(analysis.correlations_found), 'hypotheses_generated': len(analysis.hypotheses), 'analysis_duration': analysis.analysis_duration_seconds})
        await self._process_alert(alert)
    async def _process_alert(self, alert: ProductionAlert):
        with self._lock:
            self.alerts.append(alert)
        logger.info(f'Production alert: {alert.alert_id} - {alert.message}')
        if self._should_escalate_alert(alert):
            alert.escalated = True
            await self._escalate_alert(alert)
        for callback in self.alert_callbacks:
            try:
                callback(alert)
            except Exception as e:
                logger.error(f'Error in alert callback: {e}')
        await self._save_alert(alert)
    def _should_escalate_alert(self, alert: ProductionAlert) -> bool:
        if alert.sla_violation and alert.sla_violation.severity.value == 'critical':
            return True
        if alert.recovery_execution and alert.recovery_execution.status.value == 'failed' and (alert.recovery_execution.trigger_violation.severity.value in ['major', 'critical']):
            return True
        if self.monitoring_mode == MonitoringMode.EMERGENCY:
            return alert.severity in ['warning', 'critical']
        return False
    async def _escalate_alert(self, alert: ProductionAlert):
        logger.warning(f'ESCALATED ALERT: {alert.alert_id} - {alert.message}')
        escalation_context = {'alert_id': alert.alert_id, 'severity': alert.severity, 'component': alert.component, 'message': alert.message, 'escalation_time': time.time(), 'monitoring_mode': self.monitoring_mode.value}
        escalation_file = self.stats_dir / f'escalation_{alert.alert_id}.json'
        with open(escalation_file, 'w') as f:
            json.dump(escalation_context, f, indent=2)
    def _monitoring_loop(self):
        logger.info('Production monitoring loop started')
        while self.is_monitoring:
            try:
                current_time = time.time()
                health_status = self._generate_health_status(current_time)
                with self._lock:
                    self.health_history.append(health_status)
                    cutoff_time = current_time - 24 * 3600
                    self.health_history = [h for h in self.health_history if h.timestamp >= cutoff_time]
                for callback in self.health_callbacks:
                    try:
                        callback(health_status)
                    except Exception as e:
                        logger.error(f'Error in health callback: {e}')
                self._adjust_monitoring_mode(health_status)
                time.sleep(self.health_check_interval)
            except Exception as e:
                logger.error(f'Error in production monitoring loop: {e}')
                time.sleep(60)
        logger.info('Production monitoring loop stopped')
    def _generate_health_status(self, timestamp: float) -> ProductionHealth:
        sla_summary = self.sla_manager.get_sla_summary()
        recovery_summary = self.recovery_engine.get_recovery_summary()
        cutoff_time = timestamp - 3600
        recent_alerts = [a for a in self.alerts if a.timestamp >= cutoff_time]
        overall_status = 'healthy'
        if sla_summary['active_violations'] > 0:
            if any((a.severity == 'critical' for a in recent_alerts)):
                overall_status = 'critical'
            else:
                overall_status = 'degraded'
        service_health = recovery_summary['health_status']
        if any((not healthy for healthy in service_health.values())):
            if overall_status == 'healthy':
                overall_status = 'degraded'
        return ProductionHealth(timestamp=timestamp, overall_status=overall_status, sla_compliance_percent=sla_summary['compliance_rate_percent'], active_violations=sla_summary['active_violations'], active_recoveries=recovery_summary['active_executions'], system_metrics={'cpu_percent': 0.0, 'memory_percent': 0.0, 'gpu_utilization': 0.0}, service_health=service_health, recent_alerts=recent_alerts)
    def _adjust_monitoring_mode(self, health_status: ProductionHealth):
        current_mode = self.monitoring_mode
        new_mode = current_mode
        if health_status.overall_status == 'critical' or health_status.active_violations >= 3:
            new_mode = MonitoringMode.EMERGENCY
        elif health_status.overall_status == 'degraded' or health_status.active_violations >= 1 or health_status.sla_compliance_percent < 95:
            if current_mode not in [MonitoringMode.EMERGENCY]:
                new_mode = MonitoringMode.HIGH_ALERT
        elif health_status.overall_status == 'healthy' and health_status.active_violations == 0 and (health_status.sla_compliance_percent >= 99):
            if current_mode in [MonitoringMode.HIGH_ALERT, MonitoringMode.EMERGENCY]:
                new_mode = MonitoringMode.NORMAL
        if new_mode != current_mode:
            self.set_monitoring_mode(new_mode)
    async def _save_alert(self, alert: ProductionAlert):
        try:
            alert_file = self.stats_dir / f'alert_{alert.alert_id}.json'
            alert_data = {'alert_id': alert.alert_id, 'timestamp': alert.timestamp, 'severity': alert.severity, 'component': alert.component, 'message': alert.message, 'escalated': alert.escalated, 'resolved_timestamp': alert.resolved_timestamp, 'context': alert.context}
            if alert.sla_violation:
                alert_data['sla_violation'] = {'violation_id': alert.sla_violation.violation_id, 'metric_name': alert.sla_violation.metric_name, 'severity': alert.sla_violation.severity.value, 'breach_percentage': alert.sla_violation.breach_percentage}
            if alert.recovery_execution:
                alert_data['recovery_execution'] = {'execution_id': alert.recovery_execution.execution_id, 'status': alert.recovery_execution.status.value, 'procedure_name': alert.recovery_execution.procedure.name, 'duration': alert.recovery_execution.duration_seconds}
            if alert.rca_analysis:
                alert_data['rca_analysis'] = alert.rca_analysis.to_dict()
            with open(alert_file, 'w') as f:
                json.dump(alert_data, f, indent=2)
        except Exception as e:
            logger.error(f'Error saving alert {alert.alert_id}: {e}')
    def get_production_summary(self) -> Dict[str, Any]:
        current_time = time.time()
        sla_summary = self.sla_manager.get_sla_summary()
        recovery_summary = self.recovery_engine.get_recovery_summary()
        rca_summary = self.rca_engine.get_rca_summary()
        recent_alerts = [a for a in self.alerts if a.timestamp >= current_time - 86400]
        escalated_alerts = [a for a in recent_alerts if a.escalated]
        alert_stats = {'total_24h': len(recent_alerts), 'escalated_24h': len(escalated_alerts), 'by_severity': {'critical': len([a for a in recent_alerts if a.severity == 'critical']), 'warning': len([a for a in recent_alerts if a.severity == 'warning']), 'info': len([a for a in recent_alerts if a.severity == 'info'])}}
        current_health = None
        if self.health_history:
            current_health = self.health_history[-1]
        return {'monitoring_active': self.is_monitoring, 'monitoring_mode': self.monitoring_mode.value, 'current_health': {'status': current_health.overall_status if current_health else 'unknown', 'sla_compliance': current_health.sla_compliance_percent if current_health else 0, 'active_violations': current_health.active_violations if current_health else 0, 'active_recoveries': current_health.active_recoveries if current_health else 0}, 'component_status': {'sla_manager': sla_summary, 'recovery_engine': recovery_summary, 'rca_engine': rca_summary}, 'alert_statistics': alert_stats, 'configuration': {'auto_recovery_enabled': self.auto_recovery_enabled, 'auto_rca_enabled': self.auto_rca_enabled, 'alert_cooldown_seconds': self.alert_cooldown_seconds, 'health_check_interval': self.health_check_interval}}
    def get_recent_alerts(self, hours: int=24) -> List[ProductionAlert]:
        cutoff_time = time.time() - hours * 3600
        return [a for a in self.alerts if a.timestamp >= cutoff_time]
    def get_health_trend(self, hours: int=24) -> List[ProductionHealth]:
        cutoff_time = time.time() - hours * 3600
        return [h for h in self.health_history if h.timestamp >= cutoff_time]
class AphroditeProductionStatLogger(StatLoggerBase):
    def __init__(self, local_interval: float, production_monitor: ProductionMonitor, aphrodite_config):
        super().__init__(local_interval, aphrodite_config)
        self.production_monitor = production_monitor
    def log(self, stats: Stats) -> None:
        self.production_monitor.record_stats(stats)
        self.num_prompt_tokens.append(stats.num_prompt_tokens_iter)
        self.num_generation_tokens.append(stats.num_generation_tokens_iter)
        if self._should_log_locally(stats.now):
            self._reset(stats)
    def _should_log_locally(self, now: float) -> bool:
        return now - self.last_local_log >= self.local_interval
    def _reset(self, stats) -> None:
        self.num_prompt_tokens = []
        self.num_generation_tokens = []
        self.last_local_log = stats.now
    def info(self, type: str, obj) -> None:
        pass
def create_production_monitor() -> ProductionMonitor:
    return ProductionMonitor()
if __name__ == '__main__':
    import asyncio
    logging.basicConfig(level=logging.INFO)
    print('🏭 Production Monitoring System Demo')
    print('=' * 60)
    async def demo_production_monitoring():
        monitor = create_production_monitor()
        def alert_handler(alert: ProductionAlert):
            print(f'🚨 ALERT: {alert.severity.upper()} - {alert.message}')
            if alert.escalated:
                print(f'   ⚠️  ESCALATED!')
        def health_handler(health: ProductionHealth):
            print(f'💓 HEALTH: {health.overall_status} - SLA: {health.sla_compliance_percent:.1f}% - Violations: {health.active_violations}')
        monitor.register_alert_callback(alert_handler)
        monitor.register_health_callback(health_handler)
        try:
            monitor.start_monitoring()
            print('✅ Production monitoring started')
            print('\n📊 Simulating production metrics...')
            current_time = time.time()
            for i in range(5):
                monitor.record_metric('request_latency_p95', 150.0 + i * 10, current_time + i)
                monitor.record_metric('tokens_per_second', 120.0 - i * 2, current_time + i)
                await asyncio.sleep(0.5)
            print('🔥 Simulating performance degradation...')
            for i in range(3):
                monitor.record_metric('request_latency_p95', 280.0 + i * 20, current_time + 10 + i)
                monitor.record_metric('tokens_per_second', 80.0 - i * 10, current_time + 10 + i)
                await asyncio.sleep(0.5)
            print('\n⏳ Processing incidents...')
            await asyncio.sleep(10)
            summary = monitor.get_production_summary()
            print(f'\n📈 Production Summary:')
            print(f"   Status: {summary['current_health']['status']}")
            print(f"   Mode: {summary['monitoring_mode']}")
            print(f"   SLA Compliance: {summary['current_health']['sla_compliance']:.1f}%")
            print(f"   Active Violations: {summary['current_health']['active_violations']}")
            print(f"   Alerts (24h): {summary['alert_statistics']['total_24h']}")
            print(f"   Escalated (24h): {summary['alert_statistics']['escalated_24h']}")
            recent_alerts = monitor.get_recent_alerts(1)
            if recent_alerts:
                print(f'\n🚨 Recent Alerts ({len(recent_alerts)}):')
                for alert in recent_alerts[-3:]:
                    print(f'   {alert.severity}: {alert.message}')
        finally:
            monitor.stop_monitoring()
            print('\n🛑 Production monitoring stopped')
    try:
        asyncio.run(demo_production_monitoring())
    except KeyboardInterrupt:
        print('\n🛑 Demo interrupted')