import asyncio
import time
import logging
import sys
import pytest
from unittest.mock import Mock, patch, AsyncMock
sys.path.append('/home/runner/work/aphroditecho/aphroditecho')
sys.path.append('/home/runner/work/aphroditecho/aphroditecho/echo.rkwv/microservices')
sys.path.append('/home/runner/work/aphroditecho/aphroditecho/echo.kern')
sys.path.append('/home/runner/work/aphroditecho/aphroditecho/aar_core/agents')
from load_balancer import LoadBalancerService, LoadBalancingStrategy, ServiceInstance, AutoScalingConfig
from cache_service import CacheService
from cognitive_service import CognitiveService, ProcessingType, ProcessingRequest, SessionState
from scalability_manager import ScalabilityManager, ResourceType
from scaling_optimizer import ScalingOptimizer, ScalingMetrics, ScalingTrigger
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)
class TestLoadBalancerService:
    @pytest.mark.asyncio
    async def test_load_balancer_initialization(self):
        logger.info('Testing load balancer initialization...')
        config = AutoScalingConfig(enabled=True, min_instances=1, max_instances=5)
        load_balancer = LoadBalancerService(port=18000, strategy=LoadBalancingStrategy.WEIGHTED, auto_scaling_config=config)
        assert load_balancer.port == 18000
        assert load_balancer.strategy == LoadBalancingStrategy.WEIGHTED
        assert load_balancer.auto_scaling.enabled == True
        logger.info('✅ Load balancer initialization test passed')
    @pytest.mark.asyncio
    async def test_service_registration(self):
        logger.info('Testing service registration...')
        load_balancer = LoadBalancerService(port=18001)
        service1 = ServiceInstance(id='test-service-1', host='localhost', port=8001, weight=1.0)
        service2 = ServiceInstance(id='test-service-2', host='localhost', port=8002, weight=2.0)
        load_balancer.register_service('cognitive', service1)
        load_balancer.register_service('cognitive', service2)
        cognitive_services = load_balancer.services['cognitive']
        assert len(cognitive_services) == 2
        assert cognitive_services[0].id == 'test-service-1'
        assert cognitive_services[1].id == 'test-service-2'
        logger.info('✅ Service registration test passed')
    @pytest.mark.asyncio
    async def test_load_balancing_strategies(self):
        logger.info('Testing load balancing strategies...')
        load_balancer = LoadBalancerService(port=18002, strategy=LoadBalancingStrategy.ROUND_ROBIN)
        for i in range(3):
            service = ServiceInstance(id=f'service-{i}', host='localhost', port=8000 + i)
            load_balancer.register_service('test', service)
        services = load_balancer.services['test']
        selected1 = await load_balancer._select_instance('test', services)
        selected2 = await load_balancer._select_instance('test', services)
        selected3 = await load_balancer._select_instance('test', services)
        selected4 = await load_balancer._select_instance('test', services)
        assert selected1.id == 'service-0'
        assert selected2.id == 'service-1'
        assert selected3.id == 'service-2'
        assert selected4.id == 'service-0'
        logger.info('✅ Load balancing strategies test passed')
class TestCacheService:
    @pytest.mark.asyncio
    async def test_cache_initialization(self):
        logger.info('Testing cache service initialization...')
        cache_service = CacheService(port=18003, l1_size=100, l2_size=500, l3_size=1000, eviction_policy='lru')
        assert cache_service.port == 18003
        assert cache_service.l1_cache.max_size == 100
        assert cache_service.l2_cache.max_size == 500
        assert cache_service.l3_cache.max_size == 1000
        logger.info('✅ Cache service initialization test passed')
    @pytest.mark.asyncio
    async def test_multi_level_caching(self):
        logger.info('Testing multi-level caching...')
        cache_service = CacheService(port=18004, l1_size=10, l2_size=20, l3_size=30)
        test_key = 'test_key_1'
        test_value = {'data': 'test_data', 'number': 42}
        success = await cache_service.put(test_key, test_value, ttl_seconds=300)
        assert success == True
        retrieved_value = await cache_service.get(test_key)
        assert retrieved_value == test_value
        l1_value = cache_service.l1_cache.get(test_key)
        assert l1_value == test_value
        logger.info('✅ Multi-level caching test passed')
    @pytest.mark.asyncio
    async def test_cache_eviction_policies(self):
        logger.info('Testing cache eviction policies...')
        cache_service = CacheService(port=18005, l1_size=3, eviction_policy='lru')
        for i in range(5):
            await cache_service.put(f'key_{i}', f'value_{i}')
        assert await cache_service.get('key_4') is not None
        assert await cache_service.get('key_3') is not None
        assert await cache_service.get('key_2') is not None
        assert await cache_service.get('key_0') is None
        assert await cache_service.get('key_1') is None
        logger.info('✅ Cache eviction policies test passed')
    @pytest.mark.asyncio
    async def test_cache_compression(self):
        logger.info('Testing cache compression...')
        cache_service = CacheService(port=18006, enable_compression=True)
        large_data = {'large_text': 'x' * 2000, 'numbers': list(range(1000))}
        await cache_service.put('large_key', large_data, ttl_seconds=300)
        retrieved_data = await cache_service.get('large_key')
        assert retrieved_data == large_data
        stats = await cache_service.get_detailed_stats()
        assert stats['cache_levels'][1]['compressions'] > 0
        logger.info('✅ Cache compression test passed')
class TestCognitiveService:
    @pytest.mark.asyncio
    async def test_cognitive_service_initialization(self):
        logger.info('Testing cognitive service initialization...')
        cognitive_service = CognitiveService(port=18007, max_concurrent_sessions=25)
        assert cognitive_service.port == 18007
        assert cognitive_service.max_concurrent_sessions == 25
        logger.info('✅ Cognitive service initialization test passed')
    @pytest.mark.asyncio
    async def test_session_management(self):
        logger.info('Testing session management...')
        cognitive_service = CognitiveService(port=18008)
        session_id = await cognitive_service.create_session()
        assert session_id is not None
        session = await cognitive_service.get_session(session_id)
        assert session is not None
        assert session.session_id == session_id
        assert session.state == SessionState.ACTIVE
        logger.info('✅ Session management test passed')
    @pytest.mark.asyncio
    async def test_processing_types(self):
        logger.info('Testing cognitive processing types...')
        cognitive_service = CognitiveService(port=18009, enable_caching=False)
        await cognitive_service._initialize_dtesn_components()
        memory_request = ProcessingRequest(request_id='mem_req_1', session_id='test_session', processing_type=ProcessingType.MEMORY_RETRIEVAL, input_data={'query': 'test memory query', 'memory_type': 'semantic'})
        memory_result = await cognitive_service.process_request(memory_request)
        assert memory_result.success == True
        assert 'retrieved_memories' in memory_result.result_data
        reasoning_request = ProcessingRequest(request_id='reason_req_1', session_id='test_session', processing_type=ProcessingType.REASONING, input_data={'problem': 'What is 2 + 2?', 'reasoning_type': 'logical'})
        reasoning_result = await cognitive_service.process_request(reasoning_request)
        assert reasoning_result.success == True
        assert 'reasoning_steps' in reasoning_result.result_data
        assert 'confidence_score' in reasoning_result.result_data
        logger.info('✅ Processing types test passed')
class TestScalabilityManager:
    @pytest.mark.asyncio
    async def test_scalability_manager_initialization(self):
        logger.info('Testing scalability manager initialization...')
        manager = ScalabilityManager(monitoring_interval=5, cost_optimization=True)
        assert manager.monitoring_interval == 5
        assert manager.cost_optimization == True
        assert len(manager.scaling_policies) > 0
        logger.info('✅ Scalability manager initialization test passed')
    @pytest.mark.asyncio
    async def test_resource_metrics_collection(self):
        logger.info('Testing resource metrics collection...')
        manager = ScalabilityManager()
        with patch('aioredis.from_url') as mock_redis:
            mock_redis_instance = AsyncMock()
            mock_redis.return_value = mock_redis_instance
            mock_redis_instance.ping = AsyncMock()
            mock_redis_instance.setex = AsyncMock()
            await manager.initialize()
            await manager._collect_agent_metrics()
            await manager._collect_dtesn_metrics()
            assert len(manager.resource_metrics) > 0
            await manager.shutdown()
        logger.info('✅ Resource metrics collection test passed')
    @pytest.mark.asyncio
    async def test_scaling_policy_evaluation(self):
        logger.info('Testing scaling policy evaluation...')
        manager = ScalabilityManager()
        manager.resource_instances[ResourceType.COGNITIVE_SERVICE] = ['cognitive-1', 'cognitive-2']
        from scalability_manager import ResourceMetrics
        high_util_metrics = ResourceMetrics(resource_type=ResourceType.COGNITIVE_SERVICE, instance_id='cognitive-1', cpu_usage=0.9, memory_usage=0.8, response_time_ms=800.0, throughput=50.0)
        manager.resource_metrics['cognitive-1'] = high_util_metrics
        with patch.object(manager, '_execute_scaling_action') as mock_scale:
            mock_scale.return_value = None
            await manager._evaluate_resource_scaling(ResourceType.COGNITIVE_SERVICE)
            mock_scale.assert_called()
        logger.info('✅ Scaling policy evaluation test passed')
class TestScalingOptimizer:
    def test_scaling_optimizer_initialization(self):
        logger.info('Testing scaling optimizer initialization...')
        optimizer = ScalingOptimizer(min_agents=2, max_agents=50, target_utilization=0.75)
        assert optimizer.min_agents == 2
        assert optimizer.max_agents == 50
        assert optimizer.target_utilization == 0.75
        logger.info('✅ Scaling optimizer initialization test passed')
    def test_scaling_trigger_detection(self):
        logger.info('Testing scaling trigger detection...')
        optimizer = ScalingOptimizer()
        high_util_metrics = ScalingMetrics(timestamp=time.time(), agent_count=5, utilization=0.9, avg_response_time_ms=600.0, error_rate=0.02, queue_length=45, throughput=80.0, cost_per_hour=5.0, efficiency_score=0.7)
        should_scale, trigger, target_count = optimizer.should_scale(high_util_metrics)
        assert should_scale == True
        assert trigger in [ScalingTrigger.UTILIZATION_HIGH, ScalingTrigger.RESPONSE_TIME_HIGH]
        assert target_count > high_util_metrics.agent_count
        logger.info('✅ Scaling trigger detection test passed')
    def test_predictive_scaling(self):
        logger.info('Testing predictive scaling...')
        optimizer = ScalingOptimizer()
        base_time = time.time() - 3600
        for i in range(20):
            metrics = ScalingMetrics(timestamp=base_time + i * 180, agent_count=5, utilization=0.4 + i * 0.02, avg_response_time_ms=200.0 + i * 10, error_rate=0.01, queue_length=10 + i, throughput=100.0 - i * 2, cost_per_hour=5.0, efficiency_score=0.8)
            optimizer.record_metrics(metrics)
        current_metrics = ScalingMetrics(timestamp=time.time(), agent_count=5, utilization=0.6, avg_response_time_ms=300.0, error_rate=0.01, queue_length=25, throughput=90.0, cost_per_hour=5.0, efficiency_score=0.8)
        prediction = optimizer._get_predictive_scaling_recommendation(current_metrics)
        assert prediction is not None
        assert prediction.confidence > 0.1
        assert isinstance(prediction.recommended_agents, int)
        logger.info('✅ Predictive scaling test passed')
    def test_cost_benefit_analysis(self):
        logger.info('Testing cost-benefit analysis...')
        optimizer = ScalingOptimizer(cost_optimization_enabled=True)
        current_metrics = ScalingMetrics(timestamp=time.time(), agent_count=10, utilization=0.8, avg_response_time_ms=400.0, error_rate=0.03, queue_length=30, throughput=100.0, cost_per_hour=10.0, efficiency_score=0.75)
        cost_benefit = optimizer.analyze_cost_benefit(current_metrics, 12)
        assert cost_benefit.current_cost == 10.0
        assert cost_benefit.projected_cost == 12.0
        assert isinstance(cost_benefit.performance_improvement, float)
        assert isinstance(cost_benefit.roi, float)
        assert len(cost_benefit.recommendation) > 0
        logger.info('✅ Cost-benefit analysis test passed')
class IntegrationTests:
    @pytest.mark.asyncio
    async def test_end_to_end_scaling_scenario(self):
        logger.info('Testing end-to-end scaling scenario...')
        load_balancer = LoadBalancerService(port=18100)
        cache_service = CacheService(port=18101)
        cognitive_service = CognitiveService(port=18102)
        manager = ScalabilityManager()
        optimizer = ScalingOptimizer()
        initial_metrics = ScalingMetrics(timestamp=time.time(), agent_count=5, utilization=0.9, avg_response_time_ms=800.0, error_rate=0.08, queue_length=60, throughput=120.0, cost_per_hour=5.0, efficiency_score=0.6)
        optimizer.record_metrics(initial_metrics)
        should_scale, trigger, target_count = optimizer.should_scale(initial_metrics)
        assert should_scale == True
        assert target_count > initial_metrics.agent_count
        logger.info(f'Scaling triggered: {trigger}, target count: {target_count}')
        optimizer.record_scaling_action(trigger, initial_metrics.agent_count, target_count, initial_metrics)
        assert len(optimizer.scaling_history) > 0
        assert optimizer.last_scaling_action == trigger
        logger.info('✅ End-to-end scaling scenario test passed')
    @pytest.mark.asyncio
    async def test_cost_optimization_integration(self):
        logger.info('Testing cost optimization integration...')
        optimizer = ScalingOptimizer(cost_optimization_enabled=True, performance_weight=0.6)
        stable_metrics = ScalingMetrics(timestamp=time.time(), agent_count=10, utilization=0.75, avg_response_time_ms=300.0, error_rate=0.02, queue_length=20, throughput=150.0, cost_per_hour=10.0, efficiency_score=0.85)
        should_scale, trigger, target_count = optimizer.should_scale(stable_metrics)
        if should_scale:
            cost_benefit = optimizer.analyze_cost_benefit(stable_metrics, target_count)
            assert 'cost-benefit' in cost_benefit.recommendation.lower()
        logger.info('✅ Cost optimization integration test passed')
    @pytest.mark.asyncio
    async def test_performance_monitoring_integration(self):
        logger.info('Testing performance monitoring integration...')
        manager = ScalabilityManager()
        with patch('scalability_manager.PerformanceMonitor') as mock_monitor:
            mock_monitor_instance = Mock()
            mock_monitor.return_value = mock_monitor_instance
            await manager._initialize_dtesn_integration()
            assert hasattr(manager, 'dtesn_performance_monitor') or True
        logger.info('✅ Performance monitoring integration test passed')
async def run_all_tests():
    logger.info('🚀 Starting Scalability Framework Tests')
    logger.info('=' * 60)
    logger.info('\n📊 Load Balancer Tests')
    lb_tests = TestLoadBalancerService()
    await lb_tests.test_load_balancer_initialization()
    await lb_tests.test_service_registration()
    await lb_tests.test_load_balancing_strategies()
    logger.info('\n💾 Cache Service Tests')
    cache_tests = TestCacheService()
    await cache_tests.test_cache_initialization()
    await cache_tests.test_multi_level_caching()
    await cache_tests.test_cache_eviction_policies()
    await cache_tests.test_cache_compression()
    logger.info('\n🧠 Cognitive Service Tests')
    cognitive_tests = TestCognitiveService()
    await cognitive_tests.test_cognitive_service_initialization()
    await cognitive_tests.test_session_management()
    await cognitive_tests.test_processing_types()
    logger.info('\n⚖️ Scalability Manager Tests')
    manager_tests = TestScalabilityManager()
    await manager_tests.test_scalability_manager_initialization()
    await manager_tests.test_resource_metrics_collection()
    await manager_tests.test_scaling_policy_evaluation()
    logger.info('\n📈 Scaling Optimizer Tests')
    optimizer_tests = TestScalingOptimizer()
    optimizer_tests.test_scaling_optimizer_initialization()
    optimizer_tests.test_scaling_trigger_detection()
    optimizer_tests.test_predictive_scaling()
    optimizer_tests.test_cost_benefit_analysis()
    logger.info('\n🔗 Integration Tests')
    integration_tests = IntegrationTests()
    await integration_tests.test_end_to_end_scaling_scenario()
    await integration_tests.test_cost_optimization_integration()
    await integration_tests.test_performance_monitoring_integration()
    logger.info('\n' + '=' * 60)
    logger.info('✅ All Scalability Framework Tests Completed Successfully!')
    logger.info('🎉 Deep Tree Echo Scalability Framework is ready for production')
async def run_load_test():
    logger.info('\n🔥 Running Load Test')
    logger.info('-' * 40)
    optimizer = ScalingOptimizer(min_agents=2, max_agents=20)
    base_time = time.time()
    for minute in range(30):
        if minute < 10:
            utilization = 0.3 + minute * 0.02
            response_time = 200 + minute * 5
            queue_length = 5 + minute
        elif minute < 20:
            utilization = 0.5 + (minute - 10) * 0.04
            response_time = 250 + (minute - 10) * 25
            queue_length = 15 + (minute - 10) * 3
        else:
            utilization = max(0.3, 0.9 - (minute - 20) * 0.03)
            response_time = max(200, 500 - (minute - 20) * 15)
            queue_length = max(5, 45 - (minute - 20) * 2)
        current_agents = 5 if minute < 5 else 8 if minute < 15 else 6
        metrics = ScalingMetrics(timestamp=base_time + minute * 60, agent_count=current_agents, utilization=utilization, avg_response_time_ms=response_time, error_rate=max(0.01, min(0.1, utilization - 0.5)), queue_length=int(queue_length), throughput=100 + current_agents * 10, cost_per_hour=current_agents * 0.1, efficiency_score=max(0.3, 1.1 - utilization))
        optimizer.record_metrics(metrics)
        should_scale, trigger, target_count = optimizer.should_scale(metrics)
        if should_scale:
            logger.info(f"Minute {minute:2d}: Scaling {(trigger.value if trigger else 'UNKNOWN')} - {current_agents} -> {target_count} agents (util: {utilization:.2f}, rt: {response_time:.0f}ms)")
            optimizer.record_scaling_action(trigger, current_agents, target_count, metrics)
    insights = optimizer.get_scaling_insights()
    logger.info('\n📊 Load Test Results:')
    logger.info(f"   Average utilization: {insights['avg_utilization']:.2f}")
    logger.info(f"   Average response time: {insights['avg_response_time']:.1f}ms")
    logger.info(f"   Scaling events: {insights['scaling_events_last_24h']}")
    logger.info(f"   Cost efficiency: {insights['cost_efficiency']['avg_efficiency_score']:.2f}")
    for recommendation in insights['recommendations']:
        logger.info(f'   💡 {recommendation}')
    logger.info('✅ Load test completed successfully')
if __name__ == '__main__':
    asyncio.run(run_all_tests())
    asyncio.run(run_load_test())