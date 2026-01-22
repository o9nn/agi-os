from typing import Dict, List, Optional, Any
import time
from datetime import datetime, timedelta
from fastapi import APIRouter, HTTPException, Query, Depends
from fastapi.responses import JSONResponse
from pydantic import BaseModel, Field
from loguru import logger
from aphrodite.monitoring.backend_monitor import BackendPerformanceMonitor, create_backend_monitor
from aphrodite.monitoring.metrics_collector import AphroditeMetricsCollector, create_metrics_collector
from aphrodite.monitoring.regression_detector import PerformanceRegressionDetector, create_regression_detector
from aphrodite.monitoring.alerting_system import PerformanceAlertingSystem, create_alerting_system
class MetricsResponse(BaseModel):
    timestamp: float
    cpu_usage_percent: float
    memory_usage_percent: float
    memory_usage_gb: float
    token_throughput: float
    request_latency_p95: float
    active_requests: int
    queued_requests: int
    error_rate_percent: float
    aar_agents_active: Optional[int] = 0
    dtesn_processing_rate: Optional[float] = 0.0
    echo_self_evolution_score: Optional[float] = 0.0
class PerformanceSummaryResponse(BaseModel):
    timestamp: float
    status: str
    metrics: Dict[str, float]
    echo_metrics: Optional[Dict[str, Any]] = {}
    alerts: Dict[str, int]
    performance_status: Dict[str, Any]
class AlertResponse(BaseModel):
    timestamp: float
    severity: str
    metric_name: str
    current_value: float
    threshold: float
    message: str
    component: str
class RegressionResponse(BaseModel):
    timestamp: float
    metric_name: str
    severity: str
    current_value: float
    baseline_value: float
    regression_percent: float
    confidence: float
    message: str
class HealthCheckResponse(BaseModel):
    status: str
    timestamp: float
    monitoring_active: bool
    components: Dict[str, bool]
    uptime_seconds: float
_backend_monitor: Optional[BackendPerformanceMonitor] = None
_metrics_collector: Optional[AphroditeMetricsCollector] = None
_regression_detector: Optional[PerformanceRegressionDetector] = None
_alerting_system: Optional[PerformanceAlertingSystem] = None
_startup_time: float = time.time()
def get_backend_monitor() -> BackendPerformanceMonitor:
    global _backend_monitor
    if _backend_monitor is None:
        _backend_monitor = create_backend_monitor()
        _backend_monitor.start_monitoring()
        logger.info('Backend monitor initialized via API')
    return _backend_monitor
def get_metrics_collector() -> AphroditeMetricsCollector:
    global _metrics_collector
    if _metrics_collector is None:
        _metrics_collector = create_metrics_collector()
        _metrics_collector.start_collection()
        logger.info('Metrics collector initialized via API')
    return _metrics_collector
def get_regression_detector() -> PerformanceRegressionDetector:
    global _regression_detector
    if _regression_detector is None:
        _regression_detector = create_regression_detector()
        logger.info('Regression detector initialized via API')
    return _regression_detector
def get_alerting_system() -> PerformanceAlertingSystem:
    global _alerting_system
    if _alerting_system is None:
        _alerting_system = create_alerting_system()
        _alerting_system.start()
        backend_monitor = get_backend_monitor()
        regression_detector = get_regression_detector()
        backend_monitor.register_alert_handler(_alerting_system.process_performance_alert)
        regression_detector.register_alert_callback(_alerting_system.process_regression_alert)
        logger.info('Alerting system initialized via API')
    return _alerting_system
router = APIRouter(prefix='/monitoring', tags=['monitoring'])
@router.get('/health', response_model=HealthCheckResponse)
async def health_check():
    try:
        backend_monitor = get_backend_monitor()
        metrics_collector = get_metrics_collector()
        regression_detector = get_regression_detector()
        alerting_system = get_alerting_system()
        components = {'backend_monitor': backend_monitor.is_monitoring, 'metrics_collector': metrics_collector.is_collecting, 'regression_detector': True, 'alerting_system': alerting_system.is_active}
        overall_status = 'healthy' if all(components.values()) else 'degraded'
        uptime = time.time() - _startup_time
        return HealthCheckResponse(status=overall_status, timestamp=time.time(), monitoring_active=backend_monitor.is_monitoring, components=components, uptime_seconds=uptime)
    except Exception as e:
        logger.error(f'Health check failed: {e}')
        raise HTTPException(status_code=500, detail=f'Health check failed: {str(e)}')
@router.get('/metrics/current', response_model=MetricsResponse)
async def get_current_metrics():
    try:
        backend_monitor = get_backend_monitor()
        current_metrics = backend_monitor.get_current_metrics()
        if not current_metrics:
            raise HTTPException(status_code=503, detail='No metrics data available')
        return MetricsResponse(timestamp=current_metrics.timestamp, cpu_usage_percent=current_metrics.cpu_usage_percent, memory_usage_percent=current_metrics.memory_usage_percent, memory_usage_gb=current_metrics.memory_usage_gb, token_throughput=current_metrics.token_throughput, request_latency_p95=current_metrics.request_latency_p95, active_requests=current_metrics.active_requests, queued_requests=current_metrics.queued_requests, error_rate_percent=current_metrics.error_rate_percent, aar_agents_active=current_metrics.aar_agents_active, dtesn_processing_rate=current_metrics.dtesn_processing_rate, echo_self_evolution_score=current_metrics.echo_self_evolution_score)
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f'Failed to get current metrics: {e}')
        raise HTTPException(status_code=500, detail=f'Failed to get metrics: {str(e)}')
@router.get('/metrics/history')
async def get_metrics_history(minutes: int=Query(60, ge=1, le=1440, description='Minutes of history to retrieve')):
    try:
        backend_monitor = get_backend_monitor()
        history = backend_monitor.get_metrics_history(minutes=minutes)
        return {'timerange_minutes': minutes, 'data_points': len(history), 'metrics': [{'timestamp': m.timestamp, 'cpu_usage': m.cpu_usage_percent, 'memory_usage': m.memory_usage_percent, 'token_throughput': m.token_throughput, 'request_latency_p95': m.request_latency_p95, 'active_requests': m.active_requests, 'error_rate': m.error_rate_percent, 'aar_agents': m.aar_agents_active, 'dtesn_rate': m.dtesn_processing_rate} for m in history]}
    except Exception as e:
        logger.error(f'Failed to get metrics history: {e}')
        raise HTTPException(status_code=500, detail=f'Failed to get history: {str(e)}')
@router.get('/summary', response_model=PerformanceSummaryResponse)
async def get_performance_summary():
    try:
        backend_monitor = get_backend_monitor()
        summary = backend_monitor.get_performance_summary()
        return PerformanceSummaryResponse(**summary)
    except Exception as e:
        logger.error(f'Failed to get performance summary: {e}')
        raise HTTPException(status_code=500, detail=f'Failed to get summary: {str(e)}')
@router.get('/alerts/recent')
async def get_recent_alerts(hours: int=Query(24, ge=1, le=168, description='Hours of alert history to retrieve')):
    try:
        backend_monitor = get_backend_monitor()
        alerts = backend_monitor.get_recent_alerts(hours=hours)
        return {'timerange_hours': hours, 'alert_count': len(alerts), 'alerts': [AlertResponse(timestamp=a.timestamp, severity=a.severity, metric_name=a.metric_name, current_value=a.current_value, threshold=a.threshold, message=a.message, component=a.component) for a in alerts]}
    except Exception as e:
        logger.error(f'Failed to get recent alerts: {e}')
        raise HTTPException(status_code=500, detail=f'Failed to get alerts: {str(e)}')
@router.get('/alerts/active')
async def get_active_alerts():
    try:
        alerting_system = get_alerting_system()
        active_alerts = alerting_system.get_active_alerts()
        return {'active_count': len(active_alerts), 'alerts': active_alerts}
    except Exception as e:
        logger.error(f'Failed to get active alerts: {e}')
        raise HTTPException(status_code=500, detail=f'Failed to get active alerts: {str(e)}')
@router.get('/regression/recent')
async def get_recent_regressions(hours: int=Query(24, ge=1, le=168, description='Hours of regression history to retrieve')):
    try:
        regression_detector = get_regression_detector()
        regressions = regression_detector.get_recent_regressions(hours=hours)
        return {'timerange_hours': hours, 'regression_count': len(regressions), 'regressions': [RegressionResponse(timestamp=r.timestamp, metric_name=r.metric_name, severity=r.severity.value, current_value=r.current_value, baseline_value=r.baseline_value, regression_percent=r.regression_percent, confidence=r.confidence, message=r.message) for r in regressions]}
    except Exception as e:
        logger.error(f'Failed to get recent regressions: {e}')
        raise HTTPException(status_code=500, detail=f'Failed to get regressions: {str(e)}')
@router.get('/regression/baselines')
async def get_regression_baselines():
    try:
        regression_detector = get_regression_detector()
        baselines = regression_detector.get_baseline_status()
        return {'baseline_count': len(baselines), 'baselines': baselines}
    except Exception as e:
        logger.error(f'Failed to get regression baselines: {e}')
        raise HTTPException(status_code=500, detail=f'Failed to get baselines: {str(e)}')
@router.post('/regression/reset-baselines')
async def reset_regression_baselines(metric_names: Optional[List[str]]=None):
    try:
        regression_detector = get_regression_detector()
        regression_detector.reset_baselines(metric_names)
        reset_count = len(metric_names) if metric_names else len(regression_detector.baselines)
        return {'status': 'success', 'message': f'Reset baselines for {reset_count} metrics', 'reset_metrics': metric_names or list(regression_detector.baselines.keys())}
    except Exception as e:
        logger.error(f'Failed to reset baselines: {e}')
        raise HTTPException(status_code=500, detail=f'Failed to reset baselines: {str(e)}')
@router.get('/collectors/status')
async def get_collectors_status():
    try:
        metrics_collector = get_metrics_collector()
        summary = metrics_collector.get_metrics_summary()
        return summary
    except Exception as e:
        logger.error(f'Failed to get collectors status: {e}')
        raise HTTPException(status_code=500, detail=f'Failed to get status: {str(e)}')
@router.get('/system/status')
async def get_system_status():
    try:
        backend_monitor = get_backend_monitor()
        metrics_collector = get_metrics_collector()
        regression_detector = get_regression_detector()
        alerting_system = get_alerting_system()
        return {'system_uptime_seconds': time.time() - _startup_time, 'backend_monitor': {'is_active': backend_monitor.is_monitoring, 'metrics_collected': len(backend_monitor.metrics_history), 'collection_interval': backend_monitor.collection_interval, 'alerts_generated': len(backend_monitor.alerts_history)}, 'metrics_collector': metrics_collector.get_metrics_summary(), 'regression_detector': regression_detector.get_regression_summary(), 'alerting_system': alerting_system.get_alert_status()}
    except Exception as e:
        logger.error(f'Failed to get system status: {e}')
        raise HTTPException(status_code=500, detail=f'Failed to get system status: {str(e)}')
@router.post('/alerts/{alert_id}/resolve')
async def resolve_alert(alert_id: str):
    try:
        alerting_system = get_alerting_system()
        success = alerting_system.resolve_alert_manually(alert_id)
        if success:
            return {'status': 'success', 'message': f'Alert {alert_id} resolved'}
        else:
            raise HTTPException(status_code=404, detail=f'Alert {alert_id} not found')
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f'Failed to resolve alert {alert_id}: {e}')
        raise HTTPException(status_code=500, detail=f'Failed to resolve alert: {str(e)}')
@router.get('/export/metrics')
async def export_metrics_data():
    try:
        backend_monitor = get_backend_monitor()
        metrics_collector = get_metrics_collector()
        data = {'export_timestamp': time.time(), 'backend_metrics': backend_monitor.export_metrics_to_dict(), 'collected_metrics': metrics_collector.export_metrics_data()}
        return data
    except Exception as e:
        logger.error(f'Failed to export metrics data: {e}')
        raise HTTPException(status_code=500, detail=f'Failed to export data: {str(e)}')
@router.get('/dashboard/data')
async def get_dashboard_data():
    try:
        backend_monitor = get_backend_monitor()
        current_metrics = backend_monitor.get_current_metrics()
        recent_alerts = backend_monitor.get_recent_alerts(hours=1)
        regression_detector = get_regression_detector()
        recent_regressions = regression_detector.get_recent_regressions(hours=1)
        alerting_system = get_alerting_system()
        active_alerts = alerting_system.get_active_alerts()
        dashboard_data = {'timestamp': time.time(), 'current_metrics': {'cpu_usage': current_metrics.cpu_usage_percent if current_metrics else 0, 'memory_usage': current_metrics.memory_usage_percent if current_metrics else 0, 'token_throughput': current_metrics.token_throughput if current_metrics else 0, 'request_latency': current_metrics.request_latency_p95 if current_metrics else 0, 'error_rate': current_metrics.error_rate_percent if current_metrics else 0, 'active_requests': current_metrics.active_requests if current_metrics else 0}, 'echo_metrics': {'aar_agents': current_metrics.aar_agents_active if current_metrics else 0, 'dtesn_rate': current_metrics.dtesn_processing_rate if current_metrics else 0, 'evolution_score': current_metrics.echo_self_evolution_score if current_metrics else 0} if current_metrics else {}, 'alerts_summary': {'active_count': len(active_alerts), 'recent_1h': len(recent_alerts), 'regressions_1h': len(recent_regressions), 'severity_breakdown': {'critical': len([a for a in recent_alerts if a.severity == 'CRITICAL']), 'warning': len([a for a in recent_alerts if a.severity == 'WARNING'])}}, 'system_health': {'monitoring_active': backend_monitor.is_monitoring, 'uptime_hours': (time.time() - _startup_time) / 3600, 'data_points': len(backend_monitor.metrics_history)}}
        return dashboard_data
    except Exception as e:
        logger.error(f'Failed to get dashboard data: {e}')
        raise HTTPException(status_code=500, detail=f'Failed to get dashboard data: {str(e)}')
@router.get('/prometheus/metrics')
async def get_prometheus_metrics():
    try:
        from prometheus_client import generate_latest, CONTENT_TYPE_LATEST
        metrics_data = generate_latest()
        return JSONResponse(content=metrics_data.decode('utf-8'), media_type=CONTENT_TYPE_LATEST)
    except Exception as e:
        logger.error(f'Failed to get Prometheus metrics: {e}')
        raise HTTPException(status_code=500, detail=f'Failed to get Prometheus metrics: {str(e)}')
__all__ = ['router']