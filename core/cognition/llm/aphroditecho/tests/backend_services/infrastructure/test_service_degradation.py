import pytest
import asyncio
import time
from unittest.mock import AsyncMock, MagicMock, patch
import sys
sys.path.append('/home/runner/work/aphroditecho/aphroditecho')
from backend_services.infrastructure.service_degradation import ServiceDegradationManager, DegradationLevel, FeaturePriority, ResourceType, ResourceThreshold, Feature, DegradationEvent, GlobalDegradationManager, get_global_degradation_manager
class TestServiceDegradationManager:
    @pytest.fixture
    def resource_thresholds(self):
        return [ResourceThreshold(ResourceType.CPU, 0.6, 0.7, 0.8, 0.9), ResourceThreshold(ResourceType.MEMORY, 0.7, 0.8, 0.9, 0.95), ResourceThreshold(ResourceType.NETWORK, 0.5, 0.6, 0.7, 0.8)]
    @pytest.fixture
    def degradation_manager(self, resource_thresholds):
        return ServiceDegradationManager(service_name='test-service', resource_thresholds=resource_thresholds, check_interval=0.1, recovery_delay=0.5)
    @pytest.fixture
    def sample_features(self):
        return [Feature('core_processing', FeaturePriority.CRITICAL), Feature('advanced_analytics', FeaturePriority.HIGH), Feature('real_time_monitoring', FeaturePriority.MEDIUM), Feature('detailed_logging', FeaturePriority.LOW), Feature('debug_interface', FeaturePriority.OPTIONAL)]
    @pytest.mark.asyncio
    async def test_initialization(self, degradation_manager):
        await degradation_manager.initialize()
        assert degradation_manager._initialized
        assert degradation_manager.current_level == DegradationLevel.NORMAL
        assert degradation_manager.monitoring_task is not None
        await degradation_manager.shutdown()
    def test_feature_registration(self, degradation_manager, sample_features):
        for feature in sample_features:
            degradation_manager.register_feature(feature)
        assert len(degradation_manager.features) == len(sample_features)
        assert 'core_processing' in degradation_manager.features
        assert degradation_manager.features['core_processing'].priority == FeaturePriority.CRITICAL
    def test_resource_monitor_registration(self, degradation_manager):
        def cpu_monitor():
            return 0.5
        async def memory_monitor():
            return 0.7
        degradation_manager.register_resource_monitor(ResourceType.CPU, cpu_monitor)
        degradation_manager.register_resource_monitor(ResourceType.MEMORY, memory_monitor)
        assert ResourceType.CPU in degradation_manager.resource_monitors
        assert ResourceType.MEMORY in degradation_manager.resource_monitors
    @pytest.mark.asyncio
    async def test_feature_enabled_check(self, degradation_manager, sample_features):
        for feature in sample_features:
            degradation_manager.register_feature(feature)
        assert await degradation_manager.is_feature_enabled('core_processing')
        assert await degradation_manager.is_feature_enabled('debug_interface')
        degradation_manager.features['debug_interface'].enabled = False
        assert not await degradation_manager.is_feature_enabled('debug_interface')
    @pytest.mark.asyncio
    async def test_execute_with_fallback_enabled(self, degradation_manager):
        feature = Feature('test_feature', FeaturePriority.HIGH)
        degradation_manager.register_feature(feature)
        async def test_function(value):
            return value * 2
        result = await degradation_manager.execute_with_fallback('test_feature', test_function, 5)
        assert result == 10
    @pytest.mark.asyncio
    async def test_execute_with_fallback_disabled_with_fallback(self, degradation_manager):
        def fallback_function(value):
            return value
        feature = Feature('test_feature', FeaturePriority.LOW, enabled=False, fallback_function=fallback_function)
        degradation_manager.register_feature(feature)
        async def test_function(value):
            return value * 2
        result = await degradation_manager.execute_with_fallback('test_feature', test_function, 5)
        assert result == 5
    @pytest.mark.asyncio
    async def test_execute_with_fallback_disabled_no_fallback(self, degradation_manager):
        feature = Feature('test_feature', FeaturePriority.LOW, enabled=False)
        degradation_manager.register_feature(feature)
        async def test_function(value):
            return value * 2
        with pytest.raises(RuntimeError, match="Feature 'test_feature' is disabled"):
            await degradation_manager.execute_with_fallback('test_feature', test_function, 5)
    @pytest.mark.asyncio
    async def test_force_degradation(self, degradation_manager, sample_features):
        await degradation_manager.initialize()
        for feature in sample_features:
            degradation_manager.register_feature(feature)
        await degradation_manager.force_degradation(DegradationLevel.MINIMAL, 'Test degradation')
        assert degradation_manager.current_level == DegradationLevel.MINIMAL
        assert len(degradation_manager.degradation_history) == 1
        assert degradation_manager.features['core_processing'].enabled
        assert degradation_manager.features['advanced_analytics'].enabled
        assert not degradation_manager.features['real_time_monitoring'].enabled
        assert not degradation_manager.features['detailed_logging'].enabled
        assert not degradation_manager.features['debug_interface'].enabled
        await degradation_manager.shutdown()
    @pytest.mark.asyncio
    async def test_resource_based_degradation(self, degradation_manager):
        await degradation_manager.initialize()
        cpu_usage = 0.5
        memory_usage = 0.6
        def cpu_monitor():
            return cpu_usage
        def memory_monitor():
            return memory_usage
        degradation_manager.register_resource_monitor(ResourceType.CPU, cpu_monitor)
        degradation_manager.register_resource_monitor(ResourceType.MEMORY, memory_monitor)
        await degradation_manager._check_resources_and_adjust()
        assert degradation_manager.current_level == DegradationLevel.NORMAL
        cpu_usage = 0.65
        await degradation_manager._check_resources_and_adjust()
        assert degradation_manager.current_level == DegradationLevel.PARTIAL
        memory_usage = 0.85
        await degradation_manager._check_resources_and_adjust()
        assert degradation_manager.current_level == DegradationLevel.MINIMAL
        await degradation_manager.shutdown()
    @pytest.mark.asyncio
    async def test_degradation_callbacks(self, degradation_manager):
        await degradation_manager.initialize()
        degradation_events = []
        recovery_events = []
        def on_degradation(level):
            degradation_events.append(level)
        def on_recovery(level):
            recovery_events.append(level)
        degradation_manager.add_degradation_callback(on_degradation)
        degradation_manager.add_recovery_callback(on_recovery)
        await degradation_manager.force_degradation(DegradationLevel.MINIMAL)
        assert len(degradation_events) == 1
        assert degradation_events[0] == DegradationLevel.MINIMAL
        await degradation_manager._apply_degradation_level(DegradationLevel.NORMAL, is_recovery=True)
        assert len(recovery_events) == 1
        assert recovery_events[0] == DegradationLevel.NORMAL
        await degradation_manager.shutdown()
    def test_feature_priority_levels(self, degradation_manager):
        features = [Feature('critical_feat', FeaturePriority.CRITICAL), Feature('high_feat', FeaturePriority.HIGH), Feature('medium_feat', FeaturePriority.MEDIUM), Feature('low_feat', FeaturePriority.LOW), Feature('optional_feat', FeaturePriority.OPTIONAL)]
        for feature in features:
            degradation_manager.register_feature(feature)
        test_cases = [(DegradationLevel.NORMAL, [True, True, True, True, True]), (DegradationLevel.PARTIAL, [True, True, True, True, False]), (DegradationLevel.MINIMAL, [True, True, False, False, False]), (DegradationLevel.EMERGENCY, [True, False, False, False, False]), (DegradationLevel.OFFLINE, [False, False, False, False, False])]
        for level, expected_states in test_cases:
            for feature in degradation_manager.features.values():
                feature.enabled = degradation_manager._should_feature_be_enabled(feature, level)
            actual_states = [degradation_manager.features['critical_feat'].enabled, degradation_manager.features['high_feat'].enabled, degradation_manager.features['medium_feat'].enabled, degradation_manager.features['low_feat'].enabled, degradation_manager.features['optional_feat'].enabled]
            assert actual_states == expected_states, f'Failed for level {level.value}'
    @pytest.mark.asyncio
    async def test_recovery_stability_check(self, degradation_manager):
        await degradation_manager.initialize()
        cpu_usage = 0.8
        def cpu_monitor():
            return cpu_usage
        degradation_manager.register_resource_monitor(ResourceType.CPU, cpu_monitor)
        await degradation_manager.force_degradation(DegradationLevel.MINIMAL)
        assert degradation_manager.current_level == DegradationLevel.MINIMAL
        cpu_usage = 0.5
        await degradation_manager._check_recovery_stability(DegradationLevel.NORMAL)
        assert degradation_manager.current_level == DegradationLevel.NORMAL
        await degradation_manager.shutdown()
    def test_status_reporting(self, degradation_manager, sample_features):
        for feature in sample_features:
            degradation_manager.register_feature(feature)
        degradation_manager.current_resources[ResourceType.CPU] = 0.7
        degradation_manager.current_resources[ResourceType.MEMORY] = 0.8
        asyncio.run(degradation_manager.force_degradation(DegradationLevel.PARTIAL))
        status = degradation_manager.get_status()
        assert status['service_name'] == 'test-service'
        assert status['current_level'] == DegradationLevel.PARTIAL.value
        assert 'enabled_features' in status
        assert 'disabled_features' in status
        assert 'current_resources' in status
        assert status['degradation_events_count'] == 1
        assert status['current_resources'][ResourceType.CPU] == 0.7
        assert status['current_resources'][ResourceType.MEMORY] == 0.8
    def test_degradation_history(self, degradation_manager):
        event1 = DegradationEvent(timestamp=time.time(), previous_level=DegradationLevel.NORMAL, new_level=DegradationLevel.PARTIAL, trigger_resource=ResourceType.CPU, trigger_value=0.75, disabled_features=['debug_interface'], reason='CPU threshold exceeded')
        event2 = DegradationEvent(timestamp=time.time() + 1, previous_level=DegradationLevel.PARTIAL, new_level=DegradationLevel.NORMAL, trigger_resource=None, trigger_value=None, disabled_features=[], reason='Resource conditions improved')
        degradation_manager.degradation_history.extend([event1, event2])
        history = degradation_manager.get_degradation_history()
        assert len(history) == 2
        assert history[0]['previous_level'] == DegradationLevel.NORMAL.value
        assert history[0]['new_level'] == DegradationLevel.PARTIAL.value
        assert history[0]['trigger_resource'] == ResourceType.CPU.value
        limited_history = degradation_manager.get_degradation_history(limit=1)
        assert len(limited_history) == 1
        assert limited_history[0]['new_level'] == DegradationLevel.NORMAL.value
class TestGlobalDegradationManager:
    @pytest.fixture
    def global_manager(self):
        return GlobalDegradationManager()
    @pytest.fixture
    def sample_service_managers(self):
        managers = []
        for i in range(3):
            manager = ServiceDegradationManager(service_name=f'service-{i}', check_interval=10.0)
            managers.append(manager)
        return managers
    @pytest.mark.asyncio
    async def test_service_registration(self, global_manager, sample_service_managers):
        for manager in sample_service_managers:
            await global_manager.register_service(manager)
        assert len(global_manager.service_managers) == 3
        assert 'service-0' in global_manager.service_managers
        assert 'service-2' in global_manager.service_managers
    @pytest.mark.asyncio
    async def test_global_status(self, global_manager, sample_service_managers):
        for manager in sample_service_managers:
            await global_manager.register_service(manager)
        sample_service_managers[0].current_level = DegradationLevel.NORMAL
        sample_service_managers[1].current_level = DegradationLevel.PARTIAL
        sample_service_managers[2].current_level = DegradationLevel.MINIMAL
        global_status = await global_manager.get_global_status()
        assert global_status['overall_level'] == DegradationLevel.MINIMAL.value
        assert global_status['total_services'] == 3
        assert global_status['degraded_services'] == 2
        assert len(global_status['services']) == 3
    @pytest.mark.asyncio
    async def test_emergency_shutdown(self, global_manager, sample_service_managers):
        for manager in sample_service_managers:
            await global_manager.register_service(manager)
        await global_manager.emergency_shutdown_all('Test emergency')
        for manager in sample_service_managers:
            assert manager.current_level == DegradationLevel.OFFLINE
class TestServiceDegradationIntegration:
    @pytest.mark.asyncio
    async def test_dtesn_membrane_degradation_scenario(self):
        manager = ServiceDegradationManager('dtesn-membrane-processor', check_interval=0.1, recovery_delay=0.2)
        await manager.initialize()
        features = [Feature('membrane_computation', FeaturePriority.CRITICAL, metadata={'complexity': 'high'}), Feature('parallel_processing', FeaturePriority.HIGH, metadata={'threads': 8}), Feature('real_time_visualization', FeaturePriority.MEDIUM), Feature('detailed_metrics', FeaturePriority.LOW), Feature('debug_tracing', FeaturePriority.OPTIONAL)]
        for feature in features:
            manager.register_feature(feature)
        system_load = 0.3
        def cpu_monitor():
            return system_load
        manager.register_resource_monitor(ResourceType.CPU, cpu_monitor)
        assert await manager.is_feature_enabled('membrane_computation')
        assert await manager.is_feature_enabled('debug_tracing')
        system_load = 0.75
        await manager._check_resources_and_adjust()
        assert manager.current_level == DegradationLevel.PARTIAL
        assert await manager.is_feature_enabled('membrane_computation')
        assert not await manager.is_feature_enabled('debug_tracing')
        system_load = 0.85
        await manager._check_resources_and_adjust()
        assert manager.current_level == DegradationLevel.MINIMAL
        assert await manager.is_feature_enabled('membrane_computation')
        assert await manager.is_feature_enabled('parallel_processing')
        assert not await manager.is_feature_enabled('real_time_visualization')
        await manager.shutdown()
    @pytest.mark.asyncio
    async def test_cognitive_service_degradation_with_fallbacks(self):
        manager = ServiceDegradationManager('cognitive-service', check_interval=0.1)
        await manager.initialize()
        def simple_reasoning_fallback(*args, **kwargs):
            return {'reasoning': 'basic', 'confidence': 0.5}
        def cached_memory_fallback(*args, **kwargs):
            return {'memory': 'cached', 'items': []}
        features = [Feature('advanced_reasoning', FeaturePriority.HIGH, fallback_function=simple_reasoning_fallback), Feature('dynamic_memory', FeaturePriority.MEDIUM, fallback_function=cached_memory_fallback), Feature('learning_adaptation', FeaturePriority.LOW)]
        for feature in features:
            manager.register_feature(feature)
        async def advanced_reasoning(query):
            return {'reasoning': 'advanced', 'confidence': 0.9, 'query': query}
        result = await manager.execute_with_fallback('advanced_reasoning', advanced_reasoning, 'test_query')
        assert result['reasoning'] == 'advanced'
        await manager.force_degradation(DegradationLevel.MINIMAL)
        result = await manager.execute_with_fallback('advanced_reasoning', advanced_reasoning, 'test_query')
        assert result['reasoning'] == 'basic'
        await manager.shutdown()
    @pytest.mark.asyncio
    async def test_multi_service_coordination_scenario(self):
        global_manager = get_global_degradation_manager()
        services = []
        for service_name in ['api-gateway', 'cognitive-processor', 'memory-store']:
            manager = ServiceDegradationManager(service_name, check_interval=0.1)
            await manager.initialize()
            await global_manager.register_service(manager)
            services.append(manager)
        await services[0].force_degradation(DegradationLevel.PARTIAL, 'High network load')
        await services[1].force_degradation(DegradationLevel.MINIMAL, 'High CPU load')
        global_status = await global_manager.get_global_status()
        assert global_status['overall_level'] == DegradationLevel.MINIMAL.value
        assert global_status['total_services'] == 3
        assert global_status['degraded_services'] == 2
        await services[0].force_degradation(DegradationLevel.NORMAL, 'Network load normalized')
        await services[1].force_degradation(DegradationLevel.PARTIAL, 'CPU load reduced')
        global_status = await global_manager.get_global_status()
        assert global_status['overall_level'] == DegradationLevel.PARTIAL.value
        assert global_status['degraded_services'] == 1
        for service in services:
            await service.shutdown()
    @pytest.mark.asyncio
    async def test_cascading_failure_scenario(self):
        manager = ServiceDegradationManager('distributed-system', check_interval=0.05, recovery_delay=0.1)
        await manager.initialize()
        degradation_levels = []
        def track_degradation(level):
            degradation_levels.append(level)
        manager.add_degradation_callback(track_degradation)
        resources = {ResourceType.CPU: 0.3, ResourceType.MEMORY: 0.4, ResourceType.NETWORK: 0.2}
        def create_monitor(resource_type):
            def monitor():
                return resources[resource_type]
            return monitor
        for resource_type in resources:
            manager.register_resource_monitor(resource_type, create_monitor(resource_type))
        test_scenarios = [({'cpu': 0.7}, DegradationLevel.PARTIAL), ({'memory': 0.85}, DegradationLevel.MINIMAL), ({'network': 0.75}, DegradationLevel.MINIMAL), ({'cpu': 0.9, 'memory': 0.95}, DegradationLevel.EMERGENCY)]
        for resource_updates, expected_level in test_scenarios:
            for resource_name, value in resource_updates.items():
                if resource_name == 'cpu':
                    resources[ResourceType.CPU] = value
                elif resource_name == 'memory':
                    resources[ResourceType.MEMORY] = value
                elif resource_name == 'network':
                    resources[ResourceType.NETWORK] = value
            await manager._check_resources_and_adjust()
            assert manager.current_level == expected_level
        assert len(degradation_levels) >= 3
        await manager.shutdown()
if __name__ == '__main__':
    pytest.main([__file__, '-v'])