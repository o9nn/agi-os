import pytest
import asyncio
import time
from unittest.mock import AsyncMock, MagicMock, patch
import sys
sys.path.append('/home/runner/work/aphroditecho/aphroditecho')
from backend_services.infrastructure.service_discovery import ServiceDiscovery, ServiceEndpoint, ServiceType, ServiceStatus
from backend_services.infrastructure.circuit_breaker import CircuitBreaker, CircuitBreakerConfig, CircuitState, CircuitBreakerException
from backend_services.infrastructure.service_degradation import ServiceDegradationManager, DegradationLevel, FeaturePriority, ResourceType, Feature
class TestBackendServiceIntegration:
    @pytest.fixture
    async def service_discovery_system(self):
        discovery = ServiceDiscovery(redis_url='redis://localhost:6379', health_check_interval=0.5, health_check_timeout=1.0, max_consecutive_failures=2)
        await discovery.initialize()
        return discovery
    @pytest.fixture
    def dtesn_services(self):
        return [ServiceEndpoint(service_id='dtesn-memory-1', service_type=ServiceType.DTESN_MEMBRANE, host='localhost', port=8081, metadata={'membrane_type': 'memory', 'capacity': 1000}), ServiceEndpoint(service_id='dtesn-reasoning-1', service_type=ServiceType.DTESN_MEMBRANE, host='localhost', port=8082, metadata={'membrane_type': 'reasoning', 'complexity': 'high'}), ServiceEndpoint(service_id='cognitive-service-1', service_type=ServiceType.COGNITIVE_SERVICE, host='localhost', port=8083, metadata={'version': '2.0', 'capabilities': ['nlp', 'vision']})]
    @pytest.fixture
    async def degradation_manager(self):
        manager = ServiceDegradationManager('integrated-dtesn-system', check_interval=0.2, recovery_delay=0.5)
        await manager.initialize()
        features = [Feature('membrane_processing', FeaturePriority.CRITICAL), Feature('advanced_reasoning', FeaturePriority.HIGH), Feature('real_time_adaptation', FeaturePriority.MEDIUM), Feature('performance_monitoring', FeaturePriority.LOW), Feature('debug_visualization', FeaturePriority.OPTIONAL)]
        for feature in features:
            manager.register_feature(feature)
        return manager
    @pytest.mark.asyncio
    async def test_complete_dtesn_service_lifecycle(self, service_discovery_system, dtesn_services):
        discovery = service_discovery_system
        for service in dtesn_services:
            result = await discovery.register_service(service)
            assert result is True
        for service in dtesn_services:
            discovery.service_health[service.service_id].status = ServiceStatus.HEALTHY
        all_services = await discovery.discover_services()
        assert len(all_services) == 3
        dtesn_services_discovered = await discovery.discover_services(ServiceType.DTESN_MEMBRANE)
        assert len(dtesn_services_discovered) == 2
        memory_service = next((s for s in dtesn_services_discovered if s.service_id == 'dtesn-memory-1'), None)
        assert memory_service is not None
        assert memory_service.metadata['membrane_type'] == 'memory'
        discovery.service_health['dtesn-memory-1'].status = ServiceStatus.UNHEALTHY
        healthy_dtesn = await discovery.discover_services(ServiceType.DTESN_MEMBRANE)
        assert len(healthy_dtesn) == 1
        assert healthy_dtesn[0].service_id == 'dtesn-reasoning-1'
        await discovery.shutdown()
    @pytest.mark.asyncio
    async def test_circuit_breaker_with_service_discovery(self, service_discovery_system, dtesn_services):
        discovery = service_discovery_system
        for service in dtesn_services:
            await discovery.register_service(service)
            discovery.service_health[service.service_id].status = ServiceStatus.HEALTHY
        circuit_breakers = {}
        for service in dtesn_services:
            cb_config = CircuitBreakerConfig(failure_threshold=2, timeout=0.5, request_timeout=1.0)
            circuit_breakers[service.service_id] = CircuitBreaker(f'cb-{service.service_id}', config=cb_config)
            await circuit_breakers[service.service_id].initialize()
        async def simulate_membrane_call(service_id, input_data):
            if service_id == 'dtesn-memory-1':
                raise Exception(f'Service {service_id} unavailable')
            return {'service': service_id, 'processed': input_data, 'status': 'success'}
        cb_reasoning = circuit_breakers['dtesn-reasoning-1']
        result = await cb_reasoning.call(simulate_membrane_call, 'dtesn-reasoning-1', 'test_input')
        assert result['status'] == 'success'
        cb_memory = circuit_breakers['dtesn-memory-1']
        for _ in range(2):
            with pytest.raises(Exception):
                await cb_memory.call(simulate_membrane_call, 'dtesn-memory-1', 'test_input')
        assert cb_memory.state == CircuitState.OPEN
        discovery.service_health['dtesn-memory-1'].status = ServiceStatus.UNHEALTHY
        available_services = await discovery.discover_services(ServiceType.DTESN_MEMBRANE)
        available_ids = {s.service_id for s in available_services}
        assert 'dtesn-reasoning-1' in available_ids
        assert 'dtesn-memory-1' not in available_ids
        await discovery.shutdown()
    @pytest.mark.asyncio
    async def test_service_degradation_with_discovery_integration(self, service_discovery_system, degradation_manager, dtesn_services):
        discovery = service_discovery_system
        manager = degradation_manager
        for service in dtesn_services:
            await discovery.register_service(service)
            discovery.service_health[service.service_id].status = ServiceStatus.HEALTHY
        service_availability = len(dtesn_services)
        async def on_service_down(service_id):
            nonlocal service_availability
            service_availability -= 1
            if service_availability <= 1:
                await manager.force_degradation(DegradationLevel.EMERGENCY, f'Critical service unavailable: {service_id}')
            elif service_availability <= 2:
                await manager.force_degradation(DegradationLevel.MINIMAL, f'Service degraded due to failure: {service_id}')
        async def on_service_up(service_id):
            nonlocal service_availability
            service_availability += 1
            if service_availability >= 3:
                await manager.force_degradation(DegradationLevel.NORMAL, f'All services available, recovering from: {service_id}')
        discovery.add_service_down_callback(on_service_down)
        discovery.add_service_up_callback(on_service_up)
        assert manager.current_level == DegradationLevel.NORMAL
        discovery.service_health['dtesn-memory-1'].status = ServiceStatus.UNHEALTHY
        await on_service_down('dtesn-memory-1')
        assert manager.current_level == DegradationLevel.MINIMAL
        assert await manager.is_feature_enabled('membrane_processing')
        assert not await manager.is_feature_enabled('real_time_adaptation')
        discovery.service_health['cognitive-service-1'].status = ServiceStatus.UNHEALTHY
        await on_service_down('cognitive-service-1')
        assert manager.current_level == DegradationLevel.EMERGENCY
        assert await manager.is_feature_enabled('membrane_processing')
        assert not await manager.is_feature_enabled('advanced_reasoning')
        discovery.service_health['dtesn-memory-1'].status = ServiceStatus.HEALTHY
        await on_service_up('dtesn-memory-1')
        discovery.service_health['cognitive-service-1'].status = ServiceStatus.HEALTHY
        await on_service_up('cognitive-service-1')
        assert manager.current_level == DegradationLevel.NORMAL
        assert await manager.is_feature_enabled('debug_visualization')
        await discovery.shutdown()
        await manager.shutdown()
    @pytest.mark.asyncio
    async def test_load_balanced_circuit_breaker_scenario(self, service_discovery_system):
        discovery = service_discovery_system
        service_instances = [ServiceEndpoint(f'cognitive-{i}', ServiceType.COGNITIVE_SERVICE, 'localhost', 8090 + i, weight=1.0 if i != 1 else 2.0) for i in range(3)]
        for service in service_instances:
            await discovery.register_service(service)
            discovery.service_health[service.service_id].status = ServiceStatus.HEALTHY
        circuit_breakers = {}
        for service in service_instances:
            cb = CircuitBreaker(f'cb-{service.service_id}', config=CircuitBreakerConfig(failure_threshold=2, timeout=0.5))
            await cb.initialize()
            circuit_breakers[service.service_id] = cb
        async def make_service_call(service_id, should_fail=False):
            cb = circuit_breakers[service_id]
            async def service_operation():
                if should_fail:
                    raise Exception(f'Service {service_id} failed')
                return {'service': service_id, 'result': 'success'}
            try:
                return await cb.call(service_operation)
            except CircuitBreakerException:
                return {'service': service_id, 'result': 'circuit_open'}
            except Exception:
                return {'service': service_id, 'result': 'failed'}
        available_services = await discovery.discover_services(ServiceType.COGNITIVE_SERVICE)
        assert len(available_services) == 3
        for service in available_services:
            result = await make_service_call(service.service_id)
            assert result['result'] == 'success'
        failing_service_id = 'cognitive-1'
        for _ in range(2):
            result = await make_service_call(failing_service_id, should_fail=True)
            assert result['result'] == 'failed'
        assert circuit_breakers[failing_service_id].state == CircuitState.OPEN
        result = await make_service_call(failing_service_id)
        assert result['result'] == 'circuit_open'
        discovery.service_health[failing_service_id].status = ServiceStatus.UNHEALTHY
        healthy_services = await discovery.discover_services(ServiceType.COGNITIVE_SERVICE)
        healthy_ids = {s.service_id for s in healthy_services}
        assert len(healthy_services) == 2
        assert failing_service_id not in healthy_ids
        for service in healthy_services:
            result = await make_service_call(service.service_id)
            assert result['result'] == 'success'
        await discovery.shutdown()
    @pytest.mark.asyncio
    async def test_dtesn_membrane_processing_resilience(self, service_discovery_system, degradation_manager):
        discovery = service_discovery_system
        manager = degradation_manager
        membrane_services = [ServiceEndpoint('memory-membrane-1', ServiceType.DTESN_MEMBRANE, 'localhost', 8100, metadata={'type': 'memory', 'priority': 'high'}), ServiceEndpoint('reasoning-membrane-1', ServiceType.DTESN_MEMBRANE, 'localhost', 8101, metadata={'type': 'reasoning', 'priority': 'high'}), ServiceEndpoint('grammar-membrane-1', ServiceType.DTESN_MEMBRANE, 'localhost', 8102, metadata={'type': 'grammar', 'priority': 'medium'}), ServiceEndpoint('cache-service-1', ServiceType.CACHE_SERVICE, 'localhost', 8103, metadata={'type': 'redis', 'priority': 'low'})]
        for service in membrane_services:
            await discovery.register_service(service)
            discovery.service_health[service.service_id].status = ServiceStatus.HEALTHY
        membrane_circuits = {}
        for service in membrane_services:
            cb = CircuitBreaker(f'membrane-{service.service_id}', config=CircuitBreakerConfig(failure_threshold=3, timeout=1.0))
            await cb.initialize()
            membrane_circuits[service.service_id] = cb
        system_resources = {ResourceType.CPU: 0.3, ResourceType.MEMORY: 0.4}
        def create_resource_monitor(resource_type):
            def monitor():
                return system_resources[resource_type]
            return monitor
        for resource_type in system_resources:
            manager.register_resource_monitor(resource_type, create_resource_monitor(resource_type))
        async def dtesn_membrane_workflow():
            results = {}
            available_membranes = await discovery.discover_services(ServiceType.DTESN_MEMBRANE)
            for service in available_membranes:
                membrane_type = service.metadata.get('type', 'unknown')
                circuit = membrane_circuits[service.service_id]
                try:
                    async def membrane_operation():
                        processing_time = 0.1 * (1 + system_resources[ResourceType.CPU])
                        await asyncio.sleep(processing_time)
                        if system_resources[ResourceType.CPU] > 0.8 and membrane_type == 'grammar':
                            raise Exception(f'High load failure in {membrane_type} membrane')
                        return {'membrane': service.service_id, 'type': membrane_type, 'processed': True, 'load': system_resources[ResourceType.CPU]}
                    result = await circuit.call(membrane_operation)
                    results[membrane_type] = result
                except CircuitBreakerException:
                    if await manager.is_feature_enabled('membrane_processing'):
                        results[membrane_type] = {'membrane': service.service_id, 'type': membrane_type, 'processed': False, 'fallback': True}
                except Exception:
                    continue
            return results
        result = await dtesn_membrane_workflow()
        assert len(result) == 3
        assert all((r['processed'] for r in result.values()))
        system_resources[ResourceType.CPU] = 0.75
        await manager._check_resources_and_adjust()
        assert manager.current_level == DegradationLevel.PARTIAL
        result = await dtesn_membrane_workflow()
        assert len(result) >= 2
        system_resources[ResourceType.CPU] = 0.9
        await manager._check_resources_and_adjust()
        assert manager.current_level == DegradationLevel.EMERGENCY
        for _ in range(5):
            try:
                result = await dtesn_membrane_workflow()
            except Exception:
                continue
        grammar_circuit = membrane_circuits['grammar-membrane-1']
        assert await manager.is_feature_enabled('membrane_processing')
        assert not await manager.is_feature_enabled('performance_monitoring')
        await discovery.shutdown()
        await manager.shutdown()
    @pytest.mark.asyncio
    async def test_graceful_system_recovery(self, service_discovery_system, degradation_manager):
        discovery = service_discovery_system
        manager = degradation_manager
        all_services = [ServiceEndpoint('api-gateway', ServiceType.API_GATEWAY, 'localhost', 8200), ServiceEndpoint('load-balancer', ServiceType.LOAD_BALANCER, 'localhost', 8201), ServiceEndpoint('dtesn-core', ServiceType.DTESN_MEMBRANE, 'localhost', 8202), ServiceEndpoint('cognitive-main', ServiceType.COGNITIVE_SERVICE, 'localhost', 8203), ServiceEndpoint('cache-primary', ServiceType.CACHE_SERVICE, 'localhost', 8204), ServiceEndpoint('monitoring', ServiceType.MONITORING, 'localhost', 8205)]
        for service in all_services:
            await discovery.register_service(service)
            discovery.service_health[service.service_id].status = ServiceStatus.HEALTHY
        system_state = {'cpu': 0.3, 'memory': 0.4, 'network': 0.2, 'services_failing': 0}
        def cpu_monitor():
            return system_state['cpu']
        def memory_monitor():
            return system_state['memory']
        def network_monitor():
            return system_state['network']
        manager.register_resource_monitor(ResourceType.CPU, cpu_monitor)
        manager.register_resource_monitor(ResourceType.MEMORY, memory_monitor)
        manager.register_resource_monitor(ResourceType.NETWORK, network_monitor)
        health_timeline = []
        async def record_system_health():
            available_services = await discovery.discover_services()
            degradation_status = manager.get_status()
            health_timeline.append({'timestamp': time.time(), 'available_services': len(available_services), 'degradation_level': degradation_status['current_level'], 'enabled_features': len(degradation_status['enabled_features']), 'system_resources': dict(system_state)})
        await record_system_health()
        failure_timeline = [{'cpu': 0.7, 'memory': 0.6, 'services_failing': 0}, {'cpu': 0.8, 'memory': 0.7, 'services_failing': 1}, {'cpu': 0.9, 'memory': 0.9, 'services_failing': 3}, {'cpu': 0.7, 'memory': 0.8, 'services_failing': 2}, {'cpu': 0.4, 'memory': 0.5, 'services_failing': 0}]
        for i, state in enumerate(failure_timeline):
            system_state.update(state)
            failed_services = ['cache-primary', 'monitoring', 'cognitive-main'][:state['services_failing']]
            for service in all_services:
                if service.service_id in failed_services:
                    discovery.service_health[service.service_id].status = ServiceStatus.UNHEALTHY
                else:
                    discovery.service_health[service.service_id].status = ServiceStatus.HEALTHY
            await manager._check_resources_and_adjust()
            await record_system_health()
            await asyncio.sleep(0.1)
        assert len(health_timeline) == 6
        initial_state = health_timeline[0]
        worst_state = health_timeline[3]
        final_state = health_timeline[-1]
        assert initial_state['available_services'] == 6
        assert initial_state['degradation_level'] == DegradationLevel.NORMAL.value
        assert worst_state['available_services'] < initial_state['available_services']
        assert worst_state['degradation_level'] in [DegradationLevel.EMERGENCY.value, DegradationLevel.MINIMAL.value]
        assert final_state['available_services'] == initial_state['available_services']
        assert final_state['degradation_level'] == DegradationLevel.NORMAL.value
        assert final_state['enabled_features'] >= worst_state['enabled_features']
        service_counts = [h['available_services'] for h in health_timeline]
        degradation_severity = [list(DegradationLevel).index(DegradationLevel(h['degradation_level'])) for h in health_timeline]
        max_degradation_point = degradation_severity.index(max(degradation_severity))
        assert max_degradation_point > 0
        assert max_degradation_point < len(degradation_severity) - 1
        await discovery.shutdown()
        await manager.shutdown()
if __name__ == '__main__':
    pytest.main([__file__, '-v'])