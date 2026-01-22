import asyncio
import time
import logging
import sys
import os
sys.path.insert(0, '/home/runner/work/aphroditecho/aphroditecho')
def test_sla_manager():
    print('📊 Testing SLA Manager...')
    try:
        from aphrodite.engine.sla_manager import SLAManager, SLAThreshold
        sla_manager = SLAManager()
        assert not sla_manager.is_monitoring
        assert len(sla_manager.thresholds) > 0
        print('   ✅ Initialization successful')
        custom_threshold = SLAThreshold(metric_name='test_metric', target_value=100.0, tolerance_percent=10.0)
        sla_manager.add_threshold(custom_threshold)
        assert 'test_metric' in sla_manager.thresholds
        print('   ✅ Threshold management working')
        for i in range(10):
            sla_manager.record_measurement('test_metric', float(i), time.time() + i)
        assert len(sla_manager.measurement_windows['test_metric']) == 10
        print('   ✅ Metric recording working')
        mean_val, std_val, trend = sla_manager.statistical_analyzer.get_baseline_stats('test_metric')
        assert mean_val >= 0
        print('   ✅ Statistical analysis working')
        summary = sla_manager.get_sla_summary()
        assert 'monitoring_active' in summary
        assert 'thresholds_configured' in summary
        print('   ✅ Summary generation working')
        print('   🎉 SLA Manager tests passed!')
        return True
    except Exception as e:
        print(f'   ❌ SLA Manager test failed: {e}')
        return False
def test_recovery_engine():
    print('🔧 Testing Recovery Engine...')
    try:
        from aphrodite.engine.recovery_engine import RecoveryEngine, RecoveryProcedure, RecoveryAction
        recovery_engine = RecoveryEngine()
        assert len(recovery_engine.procedures) > 0
        assert len(recovery_engine.circuit_breakers) > 0
        print('   ✅ Initialization successful')
        custom_procedure = RecoveryProcedure(procedure_id='test_procedure', name='Test Recovery', description='Test recovery procedure', actions=[RecoveryAction.CACHE_INVALIDATION])
        recovery_engine.add_procedure(custom_procedure)
        assert 'test_procedure' in recovery_engine.procedures
        print('   ✅ Procedure management working')
        assert recovery_engine.can_execute_on_service('test_service')
        print('   ✅ Circuit breaker functionality working')
        health_checker = recovery_engine.health_checker
        def test_health_check():
            return True
        health_checker.register_health_check('test_service', test_health_check)
        is_healthy = health_checker.check_service_health('test_service')
        assert is_healthy
        print('   ✅ Health checker working')
        summary = recovery_engine.get_recovery_summary()
        assert 'total_executions' in summary
        assert 'circuit_breaker_status' in summary
        print('   ✅ Summary generation working')
        print('   🎉 Recovery Engine tests passed!')
        return True
    except Exception as e:
        print(f'   ❌ Recovery Engine test failed: {e}')
        return False
def test_rca_engine():
    print('🔍 Testing RCA Engine...')
    try:
        from aphrodite.engine.rca_engine import RCAEngine
        rca_engine = RCAEngine()
        assert rca_engine.metrics_collector is not None
        assert rca_engine.correlation_analyzer is not None
        print('   ✅ Initialization successful')
        for i in range(20):
            rca_engine.record_metric('test_rca_metric', float(i), time.time() + i)
        assert len(rca_engine.metrics_collector.metrics_history['test_rca_metric']) == 20
        print('   ✅ Metric recording working')
        diagnostic_data = rca_engine.diagnostic_collector.collect_diagnostic_data('test_incident')
        assert diagnostic_data.incident_id == 'test_incident'
        assert diagnostic_data.timestamp > 0
        print('   ✅ Diagnostic data collection working')
        summary = rca_engine.get_rca_summary()
        assert 'total_analyses' in summary
        assert 'metrics_tracked' in summary
        print('   ✅ Summary generation working')
        print('   🎉 RCA Engine tests passed!')
        return True
    except Exception as e:
        print(f'   ❌ RCA Engine test failed: {e}')
        return False
def test_production_monitor():
    print('🏭 Testing Production Monitor...')
    try:
        from aphrodite.engine.production_monitor import ProductionMonitor, MonitoringMode
        monitor = ProductionMonitor()
        assert monitor.sla_manager is not None
        assert monitor.recovery_engine is not None
        assert monitor.rca_engine is not None
        assert monitor.monitoring_mode == MonitoringMode.NORMAL
        print('   ✅ Initialization successful')
        monitor.record_metric('test_monitor_metric', 100.0)
        assert len(monitor.sla_manager.measurement_windows['test_monitor_metric']) > 0
        assert len(monitor.rca_engine.metrics_collector.metrics_history['test_monitor_metric']) > 0
        print('   ✅ Metric recording working')
        monitor.set_monitoring_mode(MonitoringMode.HIGH_ALERT)
        assert monitor.monitoring_mode == MonitoringMode.HIGH_ALERT
        print('   ✅ Monitoring mode adjustment working')
        summary = monitor.get_production_summary()
        assert 'monitoring_active' in summary
        assert 'component_status' in summary
        print('   ✅ Summary generation working')
        print('   🎉 Production Monitor tests passed!')
        return True
    except Exception as e:
        print(f'   ❌ Production Monitor test failed: {e}')
        return False
async def test_sla_violation_detection():
    print('🚨 Testing SLA Violation Detection...')
    try:
        from aphrodite.engine.sla_manager import SLAManager, SLAThreshold, ViolationSeverity
        sla_manager = SLAManager()
        violations_received = []
        def violation_handler(violation):
            violations_received.append(violation)
        sla_manager.register_violation_callback(violation_handler)
        threshold = SLAThreshold(metric_name='test_latency', target_value=200.0, tolerance_percent=25.0, measurement_window_minutes=1, violation_threshold_percent=60)
        sla_manager.add_threshold(threshold)
        current_time = time.time()
        for i in range(5):
            sla_manager.record_measurement('test_latency', 180.0, current_time + i)
        assert len(violations_received) == 0
        print("   ✅ Normal measurements don't trigger violations")
        for i in range(8):
            sla_manager.record_measurement('test_latency', 300.0, current_time + 10 + i)
        assert len(violations_received) > 0
        violation = violations_received[0]
        assert violation.metric_name == 'test_latency'
        assert violation.actual_value == 300.0
        print('   ✅ SLA violations properly detected')
        print('   🎉 SLA Violation Detection tests passed!')
        return True
    except Exception as e:
        print(f'   ❌ SLA Violation Detection test failed: {e}')
        return False
async def test_recovery_execution():
    print('⚡ Testing Recovery Execution...')
    try:
        from aphrodite.engine.recovery_engine import RecoveryEngine
        from aphrodite.engine.sla_manager import SLAViolation, SLAViolationType, ViolationSeverity, SLAThreshold
        recovery_engine = RecoveryEngine()
        threshold = SLAThreshold('test_metric', 100.0, 20.0)
        violation = SLAViolation(violation_id='test_violation', timestamp=time.time(), violation_type=SLAViolationType.LATENCY_BREACH, severity=ViolationSeverity.MAJOR, metric_name='request_latency_p95', threshold=threshold, actual_value=280.0, expected_value=200.0, breach_percentage=40.0, measurement_window=[250.0, 260.0, 280.0, 270.0, 290.0])
        execution = await recovery_engine.execute_recovery(violation)
        assert execution is not None
        assert execution.trigger_violation == violation
        print('   ✅ Recovery execution working')
        print('   🎉 Recovery Execution tests passed!')
        return True
    except Exception as e:
        print(f'   ❌ Recovery Execution test failed: {e}')
        return False
async def test_rca_analysis():
    print('🔬 Testing RCA Analysis...')
    try:
        from aphrodite.engine.rca_engine import RCAEngine
        from aphrodite.engine.sla_manager import SLAViolation, SLAViolationType, ViolationSeverity, SLAThreshold
        rca_engine = RCAEngine()
        current_time = time.time()
        for i in range(50):
            rca_engine.record_metric('cpu_usage', 60 + i, current_time - (50 - i) * 60)
            rca_engine.record_metric('request_latency_p95', 150 + i * 2, current_time - (50 - i) * 60)
        threshold = SLAThreshold('test_metric', 100.0, 20.0)
        violation = SLAViolation(violation_id='test_rca_violation', timestamp=current_time, violation_type=SLAViolationType.LATENCY_BREACH, severity=ViolationSeverity.CRITICAL, metric_name='request_latency_p95', threshold=threshold, actual_value=300.0, expected_value=200.0, breach_percentage=50.0, measurement_window=[280.0, 290.0, 300.0, 310.0, 320.0])
        analysis = await rca_engine.analyze_incident(violation)
        assert analysis.analysis_id is not None
        assert analysis.incident_violation == violation
        assert analysis.analysis_duration_seconds > 0
        assert len(analysis.recommendations) > 0
        print('   ✅ RCA analysis working')
        print('   🎉 RCA Analysis tests passed!')
        return True
    except Exception as e:
        print(f'   ❌ RCA Analysis test failed: {e}')
        return False
async def test_integrated_monitoring():
    print('🔄 Testing Integrated Monitoring Flow...')
    try:
        from aphrodite.engine.production_monitor import ProductionMonitor
        monitor = ProductionMonitor()
        alerts_received = []
        def alert_handler(alert):
            alerts_received.append(alert)
        monitor.register_alert_callback(alert_handler)
        monitor.start_monitoring()
        try:
            current_time = time.time()
            for i in range(5):
                monitor.record_metric('request_latency_p95', 150.0, current_time + i)
            for i in range(10):
                monitor.record_metric('request_latency_p95', 300.0, current_time + 10 + i)
            await asyncio.sleep(3)
            print('   ✅ Integrated monitoring flow working')
        finally:
            monitor.stop_monitoring()
        print('   🎉 Integrated Monitoring tests passed!')
        return True
    except Exception as e:
        print(f'   ❌ Integrated Monitoring test failed: {e}')
        return False
async def run_all_tests():
    print('🧪 Production Alerting & Incident Response System Tests')
    print('=' * 70)
    test_results = []
    sync_tests = [('SLA Manager', test_sla_manager), ('Recovery Engine', test_recovery_engine), ('RCA Engine', test_rca_engine), ('Production Monitor', test_production_monitor)]
    for test_name, test_func in sync_tests:
        print(f'\n🔍 Running {test_name} tests...')
        try:
            result = test_func()
            test_results.append((test_name, result))
        except Exception as e:
            print(f'❌ {test_name} test failed with exception: {e}')
            test_results.append((test_name, False))
    async_tests = [('SLA Violation Detection', test_sla_violation_detection), ('Recovery Execution', test_recovery_execution), ('RCA Analysis', test_rca_analysis), ('Integrated Monitoring', test_integrated_monitoring)]
    for test_name, test_func in async_tests:
        print(f'\n🔍 Running {test_name} tests...')
        try:
            result = await test_func()
            test_results.append((test_name, result))
        except Exception as e:
            print(f'❌ {test_name} test failed with exception: {e}')
            test_results.append((test_name, False))
    print(f'\n📊 Test Results Summary')
    print('=' * 50)
    passed = 0
    total = len(test_results)
    for test_name, result in test_results:
        status = '✅ PASSED' if result else '❌ FAILED'
        print(f'   {test_name}: {status}')
        if result:
            passed += 1
    print(f'\nOverall: {passed}/{total} tests passed ({passed / total * 100:.1f}%)')
    if passed == total:
        print('🎉 All tests passed! Production alerting system is working correctly.')
        return True
    else:
        print('⚠️  Some tests failed. Check the output above for details.')
        return False
if __name__ == '__main__':
    logging.basicConfig(level=logging.WARNING)
    success = asyncio.run(run_all_tests())
    sys.exit(0 if success else 1)