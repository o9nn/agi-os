import pytest
import asyncio
import time
from unittest.mock import AsyncMock, MagicMock, patch
import sys
sys.path.append('/home/runner/work/aphroditecho/aphroditecho')
from backend_services.infrastructure.circuit_breaker import CircuitBreaker, CircuitBreakerConfig, CircuitBreakerException, CircuitState, CircuitBreakerRegistry, get_circuit_breaker, circuit_breaker
class TestCircuitBreaker:
    @pytest.fixture
    def circuit_config(self):
        return CircuitBreakerConfig(failure_threshold=3, timeout=1.0, half_open_max_calls=2, success_threshold=2, request_timeout=0.5, slow_call_threshold=0.3, slow_call_rate_threshold=0.5, minimum_throughput=2)
    @pytest.fixture
    def circuit_breaker_instance(self, circuit_config):
        return CircuitBreaker('test-circuit', config=circuit_config)
    @pytest.fixture
    async def failing_function(self):
        async def fail(*args, **kwargs):
            raise Exception('Service unavailable')
        return fail
    @pytest.fixture
    async def succeeding_function(self):
        async def succeed(*args, **kwargs):
            await asyncio.sleep(0.1)
            return 'success'
        return succeed
    @pytest.fixture
    async def slow_function(self):
        async def slow(*args, **kwargs):
            await asyncio.sleep(0.4)
            return 'slow_success'
        return slow
    @pytest.mark.asyncio
    async def test_circuit_breaker_initialization(self, circuit_breaker_instance):
        await circuit_breaker_instance.initialize()
        assert circuit_breaker_instance.state == CircuitState.CLOSED
        assert circuit_breaker_instance.metrics.total_calls == 0
        assert circuit_breaker_instance.metrics.successful_calls == 0
        assert circuit_breaker_instance.metrics.failed_calls == 0
    @pytest.mark.asyncio
    async def test_successful_call(self, circuit_breaker_instance, succeeding_function):
        await circuit_breaker_instance.initialize()
        result = await circuit_breaker_instance.call(succeeding_function)
        assert result == 'success'
        assert circuit_breaker_instance.state == CircuitState.CLOSED
        assert circuit_breaker_instance.metrics.successful_calls == 1
        assert circuit_breaker_instance.metrics.failed_calls == 0
        assert circuit_breaker_instance.metrics.consecutive_successes == 1
        assert circuit_breaker_instance.metrics.consecutive_failures == 0
    @pytest.mark.asyncio
    async def test_failed_call(self, circuit_breaker_instance, failing_function):
        await circuit_breaker_instance.initialize()
        with pytest.raises(Exception, match='Service unavailable'):
            await circuit_breaker_instance.call(failing_function)
        assert circuit_breaker_instance.state == CircuitState.CLOSED
        assert circuit_breaker_instance.metrics.successful_calls == 0
        assert circuit_breaker_instance.metrics.failed_calls == 1
        assert circuit_breaker_instance.metrics.consecutive_failures == 1
    @pytest.mark.asyncio
    async def test_circuit_opens_on_failures(self, circuit_breaker_instance, failing_function):
        await circuit_breaker_instance.initialize()
        for i in range(circuit_breaker_instance.config.failure_threshold):
            with pytest.raises(Exception):
                await circuit_breaker_instance.call(failing_function)
        assert circuit_breaker_instance.state == CircuitState.OPEN
        assert circuit_breaker_instance.metrics.consecutive_failures >= circuit_breaker_instance.config.failure_threshold
    @pytest.mark.asyncio
    async def test_circuit_rejects_calls_when_open(self, circuit_breaker_instance, succeeding_function, failing_function):
        await circuit_breaker_instance.initialize()
        for _ in range(circuit_breaker_instance.config.failure_threshold):
            with pytest.raises(Exception):
                await circuit_breaker_instance.call(failing_function)
        assert circuit_breaker_instance.state == CircuitState.OPEN
        with pytest.raises(CircuitBreakerException):
            await circuit_breaker_instance.call(succeeding_function)
    @pytest.mark.asyncio
    async def test_circuit_transitions_to_half_open(self, circuit_breaker_instance, failing_function):
        await circuit_breaker_instance.initialize()
        for _ in range(circuit_breaker_instance.config.failure_threshold):
            with pytest.raises(Exception):
                await circuit_breaker_instance.call(failing_function)
        assert circuit_breaker_instance.state == CircuitState.OPEN
        await asyncio.sleep(circuit_breaker_instance.config.timeout + 0.1)
        await circuit_breaker_instance._check_and_update_state()
        assert circuit_breaker_instance.state == CircuitState.HALF_OPEN
    @pytest.mark.asyncio
    async def test_circuit_closes_on_recovery(self, circuit_breaker_instance, failing_function, succeeding_function):
        await circuit_breaker_instance.initialize()
        for _ in range(circuit_breaker_instance.config.failure_threshold):
            with pytest.raises(Exception):
                await circuit_breaker_instance.call(failing_function)
        await asyncio.sleep(circuit_breaker_instance.config.timeout + 0.1)
        await circuit_breaker_instance._check_and_update_state()
        assert circuit_breaker_instance.state == CircuitState.HALF_OPEN
        for _ in range(circuit_breaker_instance.config.success_threshold):
            result = await circuit_breaker_instance.call(succeeding_function)
            assert result == 'success'
        assert circuit_breaker_instance.state == CircuitState.CLOSED
    @pytest.mark.asyncio
    async def test_circuit_reopens_on_half_open_failure(self, circuit_breaker_instance, failing_function, succeeding_function):
        await circuit_breaker_instance.initialize()
        for _ in range(circuit_breaker_instance.config.failure_threshold):
            with pytest.raises(Exception):
                await circuit_breaker_instance.call(failing_function)
        await asyncio.sleep(circuit_breaker_instance.config.timeout + 0.1)
        await circuit_breaker_instance._check_and_update_state()
        assert circuit_breaker_instance.state == CircuitState.HALF_OPEN
        await circuit_breaker_instance.call(succeeding_function)
        with pytest.raises(Exception):
            await circuit_breaker_instance.call(failing_function)
        assert circuit_breaker_instance.state == CircuitState.OPEN
    @pytest.mark.asyncio
    async def test_slow_call_detection(self, circuit_breaker_instance, slow_function):
        await circuit_breaker_instance.initialize()
        result = await circuit_breaker_instance.call(slow_function)
        assert result == 'slow_success'
        assert circuit_breaker_instance.metrics.slow_calls == 1
        assert circuit_breaker_instance.metrics.slow_call_rate == 1.0
    @pytest.mark.asyncio
    async def test_fallback_function(self, circuit_config):
        def fallback_func(*args, **kwargs):
            return 'fallback_result'
        circuit = CircuitBreaker('test-fallback', config=circuit_config, fallback_function=fallback_func)
        await circuit.initialize()
        async def failing_func():
            raise Exception('Service down')
        for _ in range(circuit_config.failure_threshold):
            with pytest.raises(Exception):
                await circuit.call(failing_func)
        assert circuit.state == CircuitState.OPEN
        result = await circuit.call(failing_func)
        assert result == 'fallback_result'
    @pytest.mark.asyncio
    async def test_context_manager_success(self, circuit_breaker_instance):
        await circuit_breaker_instance.initialize()
        async with circuit_breaker_instance:
            pass
        assert circuit_breaker_instance.metrics.successful_calls == 1
        assert circuit_breaker_instance.metrics.failed_calls == 0
    @pytest.mark.asyncio
    async def test_context_manager_failure(self, circuit_breaker_instance):
        await circuit_breaker_instance.initialize()
        with pytest.raises(Exception):
            async with circuit_breaker_instance:
                raise Exception('Context manager test failure')
        assert circuit_breaker_instance.metrics.successful_calls == 0
        assert circuit_breaker_instance.metrics.failed_calls == 1
    @pytest.mark.asyncio
    async def test_context_manager_open_circuit(self, circuit_breaker_instance):
        await circuit_breaker_instance.initialize()
        circuit_breaker_instance.state = CircuitState.OPEN
        with pytest.raises(CircuitBreakerException):
            async with circuit_breaker_instance:
                pass
    @pytest.mark.asyncio
    async def test_metrics_collection(self, circuit_breaker_instance, succeeding_function, failing_function):
        await circuit_breaker_instance.initialize()
        await circuit_breaker_instance.call(succeeding_function)
        await circuit_breaker_instance.call(succeeding_function)
        with pytest.raises(Exception):
            await circuit_breaker_instance.call(failing_function)
        metrics = circuit_breaker_instance.get_metrics()
        assert metrics['name'] == 'test-circuit'
        assert metrics['state'] == CircuitState.CLOSED.value
        assert metrics['total_calls'] == 3
        assert metrics['successful_calls'] == 2
        assert metrics['failed_calls'] == 1
        assert metrics['failure_rate'] == pytest.approx(1 / 3, rel=0.01)
    @pytest.mark.asyncio
    async def test_circuit_breaker_reset(self, circuit_breaker_instance, failing_function):
        await circuit_breaker_instance.initialize()
        for _ in range(circuit_breaker_instance.config.failure_threshold):
            with pytest.raises(Exception):
                await circuit_breaker_instance.call(failing_function)
        assert circuit_breaker_instance.state == CircuitState.OPEN
        assert circuit_breaker_instance.metrics.failed_calls > 0
        await circuit_breaker_instance.reset()
        assert circuit_breaker_instance.state == CircuitState.CLOSED
        assert circuit_breaker_instance.metrics.total_calls == 0
        assert circuit_breaker_instance.metrics.failed_calls == 0
class TestCircuitBreakerRegistry:
    @pytest.mark.asyncio
    async def test_registry_get_or_create(self):
        registry = CircuitBreakerRegistry()
        cb1 = await registry.get_or_create_circuit_breaker('test-circuit-1')
        assert cb1.name == 'test-circuit-1'
        cb2 = await registry.get_or_create_circuit_breaker('test-circuit-1')
        assert cb1 is cb2
        cb3 = await registry.get_or_create_circuit_breaker('test-circuit-2')
        assert cb3.name == 'test-circuit-2'
        assert cb3 is not cb1
    @pytest.mark.asyncio
    async def test_registry_get_all_metrics(self):
        registry = CircuitBreakerRegistry()
        await registry.get_or_create_circuit_breaker('circuit-1')
        await registry.get_or_create_circuit_breaker('circuit-2')
        all_metrics = registry.get_all_metrics()
        assert len(all_metrics) == 2
        assert 'circuit-1' in all_metrics
        assert 'circuit-2' in all_metrics
        assert all_metrics['circuit-1']['name'] == 'circuit-1'
        assert all_metrics['circuit-2']['name'] == 'circuit-2'
    @pytest.mark.asyncio
    async def test_registry_reset_all(self):
        registry = CircuitBreakerRegistry()
        cb1 = await registry.get_or_create_circuit_breaker('circuit-1')
        cb2 = await registry.get_or_create_circuit_breaker('circuit-2')
        cb1.state = CircuitState.OPEN
        cb2.metrics.total_calls = 10
        await registry.reset_all()
        assert cb1.state == CircuitState.CLOSED
        assert cb1.metrics.total_calls == 0
        assert cb2.state == CircuitState.CLOSED
        assert cb2.metrics.total_calls == 0
class TestCircuitBreakerDecorator:
    @pytest.mark.asyncio
    async def test_decorator_successful_call(self):
        @circuit_breaker('decorator-test')
        async def test_function():
            return 'decorator_success'
        result = await test_function()
        assert result == 'decorator_success'
    @pytest.mark.asyncio
    async def test_decorator_failed_call(self):
        @circuit_breaker('decorator-fail-test')
        async def failing_function():
            raise Exception('Decorator test failure')
        with pytest.raises(Exception, match='Decorator test failure'):
            await failing_function()
    @pytest.mark.asyncio
    async def test_decorator_with_config(self):
        config = CircuitBreakerConfig(failure_threshold=2)
        @circuit_breaker('decorator-config-test', config=config)
        async def test_function():
            raise Exception('Config test failure')
        with pytest.raises(Exception):
            await test_function()
        with pytest.raises(Exception):
            await test_function()
        with pytest.raises(CircuitBreakerException):
            await test_function()
    @pytest.mark.asyncio
    async def test_decorator_with_fallback(self):
        async def fallback():
            return 'fallback_from_decorator'
        @circuit_breaker('decorator-fallback-test', fallback_function=fallback)
        async def failing_function():
            raise Exception('Always fails')
        config = CircuitBreakerConfig(failure_threshold=2)
        cb = await get_circuit_breaker('decorator-fallback-test', config=config)
        for _ in range(config.failure_threshold):
            with pytest.raises(Exception):
                await failing_function()
        result = await failing_function()
        assert result == 'fallback_from_decorator'
class TestCircuitBreakerIntegration:
    @pytest.mark.asyncio
    async def test_dtesn_service_integration(self):
        config = CircuitBreakerConfig(failure_threshold=3, timeout=0.5, request_timeout=1.0)
        circuit = CircuitBreaker('dtesn-membrane-service', config=config)
        await circuit.initialize()
        async def dtesn_membrane_call(membrane_type, input_data):
            if membrane_type == 'failing_membrane':
                raise Exception('Membrane processing failed')
            return {'membrane_type': membrane_type, 'processed': True, 'result': input_data}
        result = await circuit.call(dtesn_membrane_call, 'memory', 'test_data')
        assert result['processed'] is True
        for _ in range(config.failure_threshold):
            with pytest.raises(Exception):
                await circuit.call(dtesn_membrane_call, 'failing_membrane', 'test_data')
        assert circuit.state == CircuitState.OPEN
        with pytest.raises(CircuitBreakerException):
            await circuit.call(dtesn_membrane_call, 'memory', 'test_data')
    @pytest.mark.asyncio
    async def test_external_api_circuit_breaker(self):
        async def fallback_response():
            return {'status': 'degraded', 'message': 'Using cached data'}
        circuit = CircuitBreaker('external-api', config=CircuitBreakerConfig(failure_threshold=2), fallback_function=fallback_response)
        await circuit.initialize()
        async def external_api_call():
            await asyncio.sleep(1.0)
            raise Exception('API timeout')
        with pytest.raises(Exception):
            await circuit.call(external_api_call)
        with pytest.raises(Exception):
            await circuit.call(external_api_call)
        result = await circuit.call(external_api_call)
        assert result['status'] == 'degraded'
    @pytest.mark.asyncio
    async def test_database_circuit_breaker_recovery(self):
        circuit = CircuitBreaker('database-service', config=CircuitBreakerConfig(failure_threshold=2, timeout=0.5, half_open_max_calls=2, success_threshold=2))
        await circuit.initialize()
        db_available = False
        async def database_query():
            if not db_available:
                raise Exception('Database connection failed')
            return {'query': 'success', 'data': [1, 2, 3]}
        for _ in range(2):
            with pytest.raises(Exception):
                await circuit.call(database_query)
        assert circuit.state == CircuitState.OPEN
        await asyncio.sleep(0.6)
        await circuit._check_and_update_state()
        assert circuit.state == CircuitState.HALF_OPEN
        db_available = True
        result1 = await circuit.call(database_query)
        result2 = await circuit.call(database_query)
        assert result1['query'] == 'success'
        assert result2['query'] == 'success'
        assert circuit.state == CircuitState.CLOSED
if __name__ == '__main__':
    pytest.main([__file__, '-v'])