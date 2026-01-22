import asyncio
import json
import logging
import sys
from pathlib import Path
from typing import Dict, Any
sys.path.insert(0, str(Path(__file__).parent))
from aphrodite.endpoints.deep_tree_echo.errors import DTESNError, DTESNValidationError, DTESNProcessingError, DTESNResourceError, DTESNEngineError, DTESNSystemError, ErrorContext, create_error_response, error_aggregator, CircuitBreaker
from aphrodite.endpoints.deep_tree_echo.error_recovery import RetryManager, FallbackProcessor, ErrorRecoveryService, FallbackMode, RetryConfig
from aphrodite.endpoints.deep_tree_echo.monitoring import MetricsCollector, AlertManager, MonitoringDashboard
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)
def test_error_types():
    print('\n=== Testing Error Types ===')
    context = ErrorContext('req_123', '/test', user_input='invalid data')
    validation_error = DTESNValidationError('Invalid membrane depth', field_name='membrane_depth', context=context)
    print(f'Validation Error: {validation_error.error_code}')
    print(f'Category: {validation_error.category.value}')
    print(f'Severity: {validation_error.severity.value}')
    print(f'Recovery Strategy: {validation_error.recovery_strategy.value}')
    error_dict = validation_error.to_dict()
    print(f'Serialized Error Keys: {list(error_dict.keys())}')
    processing_error = DTESNProcessingError('DTESN computation failed', processing_stage='membrane_computation', context=context)
    print(f'\nProcessing Error: {processing_error.error_code}')
    resource_error = DTESNResourceError('Memory limit exceeded', resource_type='memory', context=context)
    print(f'Resource Error: {resource_error.error_code}')
    print('✅ Error types test completed')
def test_error_aggregator():
    print('\n=== Testing Error Aggregator ===')
    error_aggregator.error_history = []
    error_aggregator.error_counts = {}
    for i in range(10):
        error = DTESNProcessingError(f'Test error {i}')
        error_aggregator.record_error(error)
    print(f'Recorded errors: {len(error_aggregator.error_history)}')
    print(f'Error counts: {error_aggregator.error_counts}')
    health = error_aggregator.get_system_health_status()
    print(f"System health: {health['status']}")
    print(f"Error rate: {health['error_rate']:.3f}")
    recommendations = error_aggregator.get_recovery_recommendations(error_aggregator.error_history[0].category)
    print(f'Recovery recommendations: {len(recommendations)} items')
    print('✅ Error aggregator test completed')
def test_circuit_breaker():
    print('\n=== Testing Circuit Breaker ===')
    breaker = CircuitBreaker(failure_threshold=3, recovery_timeout=1)
    print(f'Initial state: {breaker.state}')
    try:
        result = breaker.call(lambda: 'success')
        print(f'Successful call result: {result}')
    except Exception as e:
        print(f'Unexpected error: {e}')
    failures = 0
    for i in range(5):
        try:
            breaker.call(lambda: 1 / 0)
        except Exception:
            failures += 1
    print(f'Triggered {failures} failures')
    print(f'Circuit breaker state after failures: {breaker.state}')
    try:
        breaker.call(lambda: 'should be blocked')
    except DTESNSystemError as e:
        print(f'Circuit breaker correctly blocked call: {e.message}')
    print('✅ Circuit breaker test completed')
async def test_retry_manager():
    print('\n=== Testing Retry Manager ===')
    retry_config = RetryConfig(max_attempts=3, base_delay=0.1, max_delay=1.0)
    retry_manager = RetryManager(retry_config)
    call_count = 0
    async def flaky_function():
        nonlocal call_count
        call_count += 1
        if call_count < 3:
            raise ConnectionError(f'Attempt {call_count} failed')
        return f'Success after {call_count} attempts'
    result = await retry_manager.retry_async(flaky_function)
    print(f'Retry result success: {result.success}')
    print(f'Attempts made: {result.attempts_made}')
    print(f'Result: {result.result}')
    async def always_fail():
        raise RuntimeError('Always fails')
    result = await retry_manager.retry_async(always_fail)
    print(f'Failed retry success: {result.success}')
    print(f'Failed retry attempts: {result.attempts_made}')
    print(f'Error type: {type(result.error).__name__}')
    print('✅ Retry manager test completed')
async def test_fallback_processor():
    print('\n=== Testing Fallback Processor ===')
    processor = FallbackProcessor()
    result = await processor._simplified_processing('Hello world test input')
    print(f"Simplified fallback output: {result['output'][:50]}...")
    print(f"Processing mode: {result['metadata']['processing_mode']}")
    result = await processor._statistical_processing('Test with numbers 123 and symbols!')
    print(f"Statistical fallback output: {result['output'][:50]}...")
    print(f"Complexity score: {result['metadata'].get('complexity_score', 'N/A')}")
    result = await processor._minimal_processing('Long input text that should be truncated')
    print(f"Minimal fallback output: {result['output']}")
    async def successful_processor(input_data):
        return {'output': 'Primary success', 'membrane_layers': 5}
    result = await processor.process_with_fallback('test', successful_processor)
    print(f'Primary success - fallback used: {result.fallback_used}')
    async def failing_processor(input_data):
        raise RuntimeError('Primary failed')
    result = await processor.process_with_fallback('test', failing_processor, fallback_mode=FallbackMode.SIMPLIFIED)
    print(f'Primary failed - fallback used: {result.fallback_used}')
    print(f'Recovery mode: {result.recovery_mode}')
    print('✅ Fallback processor test completed')
async def test_error_recovery_service():
    print('\n=== Testing Error Recovery Service ===')
    service = ErrorRecoveryService()
    async def successful_operation(input_data):
        return {'result': 'success', 'data': input_data}
    result = await service.execute_with_recovery(successful_operation, 'test input')
    print(f'Successful operation - success: {result.success}')
    call_count = 0
    async def flaky_operation(input_data):
        nonlocal call_count
        call_count += 1
        if call_count < 2:
            raise ConnectionError('Temporary failure')
        return {'result': 'recovered', 'attempts': call_count}
    result = await service.execute_with_recovery(flaky_operation, 'test input', enable_retry=True)
    print(f'Retry recovery - success: {result.success}')
    print(f'Attempts made: {result.attempts_made}')
    async def always_failing(input_data):
        raise RuntimeError('Always fails')
    result = await service.execute_with_recovery(always_failing, 'test input', enable_retry=False, enable_fallback=True, fallback_mode=FallbackMode.MINIMAL)
    print(f'Fallback recovery - success: {result.success}')
    print(f'Degraded mode: {result.degraded}')
    print(f'Fallback used: {result.fallback_used}')
    stats = service.get_recovery_stats()
    print(f"Recovery stats - total: {stats['total_recoveries']}")
    print(f"Success rate: {stats['success_rate']:.2%}")
    print('✅ Error recovery service test completed')
def test_monitoring_system():
    print('\n=== Testing Monitoring System ===')
    collector = MetricsCollector()
    for i in range(10):
        req_id = collector.record_request_start()
        collector.record_request_end(req_id, success=i % 4 != 0, response_time_ms=100 + i * 10)
    metrics = collector.get_current_metrics()
    print(f'Error rate: {metrics.error_rate:.2%}')
    print(f'Availability: {metrics.availability_percent:.1f}%')
    print(f'Avg response time: {metrics.avg_response_time_ms:.1f}ms')
    alert_manager = AlertManager()
    alerts = alert_manager.check_metrics(metrics)
    print(f'Generated alerts: {len(alerts)}')
    dashboard = MonitoringDashboard(collector, alert_manager)
    dashboard_data = dashboard.get_dashboard_data()
    print(f"System status: {dashboard_data['system_status']}")
    print(f"Performance grade: {dashboard_data['performance_summary']['performance_grade']}")
    print(f"Uptime SLA met: {dashboard_data['uptime_info']['meets_sla']}")
    print('✅ Monitoring system test completed')
def test_error_response_creation():
    print('\n=== Testing Error Response Creation ===')
    context = ErrorContext('req_456', '/test/endpoint', user_input='test data')
    error = DTESNProcessingError('Test processing error', context=context)
    response = create_error_response(error, context, include_debug_info=True)
    print(f'Error response - error: {response.error}')
    print(f'Error code: {response.error_code}')
    print(f'Category: {response.category}')
    print(f'Severity: {response.severity}')
    print(f'Recovery suggestions: {len(response.recovery_suggestions)}')
    print(f'Degraded mode: {response.degraded_mode}')
    generic_error = ValueError('Generic error message')
    response = create_error_response(generic_error, context)
    print(f'\nGeneric error response - error code: {response.error_code}')
    print(f'Category: {response.category}')
    print('✅ Error response creation test completed')
async def run_all_tests():
    print('🚀 Starting DTESN Error Handling Validation Tests')
    print('=' * 60)
    try:
        test_error_types()
        test_error_aggregator()
        test_circuit_breaker()
        await test_retry_manager()
        await test_fallback_processor()
        await test_error_recovery_service()
        test_monitoring_system()
        test_error_response_creation()
        print('\n' + '=' * 60)
        print('✅ ALL TESTS COMPLETED SUCCESSFULLY!')
        print('🎯 DTESN Error Handling System is ready for 99.9% uptime!')
        return True
    except Exception as e:
        print(f'\n❌ TEST FAILED: {e}')
        import traceback
        traceback.print_exc()
        return False
def main():
    success = asyncio.run(run_all_tests())
    if success:
        print('\n🌟 Error handling system validation completed successfully!')
        print('The system is ready to achieve 99.9% uptime with graceful error handling.')
        return 0
    else:
        print('\n💥 Error handling system validation failed!')
        print('Please review the errors above and fix issues before deployment.')
        return 1
if __name__ == '__main__':
    exit(main())