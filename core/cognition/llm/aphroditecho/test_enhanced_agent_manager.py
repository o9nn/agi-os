import asyncio
import time
import logging
from aar_core.agents.agent_manager import AgentManager, AgentCapabilities
from aar_core.agents.agent_performance_optimizer import OptimizationStrategy
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)
async def test_enhanced_agent_manager():
    logger.info('=== Enhanced Agent Manager Tests ===')
    strategies = [OptimizationStrategy.PERFORMANCE_BASED, OptimizationStrategy.ROUND_ROBIN, OptimizationStrategy.LEAST_LOADED]
    for strategy in strategies:
        logger.info(f'\nTesting optimization strategy: {strategy.value}')
        manager = AgentManager(max_concurrent_agents=50, optimization_strategy=strategy)
        try:
            agent_ids = []
            for i in range(20):
                capabilities = AgentCapabilities(reasoning=True, multimodal=i % 2 == 0, processing_power=1.0 + i % 4 * 0.25, specialized_domains=['math'] if i < 10 else ['language'])
                agent_id = await manager.spawn_agent(capabilities)
                agent_ids.append(agent_id)
            logger.info(f'✅ Spawned {len(agent_ids)} agents')
            test_cases = [{'name': 'Math specialists', 'request': {'required_capabilities': {'reasoning': True, 'domains': ['math'], 'min_processing_power': 1.25}}, 'count': 5}, {'name': 'High performance agents', 'request': {'required_capabilities': {'reasoning': True, 'min_processing_power': 1.5}}, 'count': 3}, {'name': 'Multimodal agents', 'request': {'required_capabilities': {'multimodal': True}}, 'count': 4}]
            allocation_results = {}
            for test_case in test_cases:
                allocated = await manager.allocate_agents(test_case['request'], test_case['count'])
                allocation_results[test_case['name']] = allocated
                logger.info(f"✅ {test_case['name']}: allocated {len(allocated)} agents")
            logger.info('Processing requests to generate performance metrics...')
            process_tasks = []
            for i, agent_id in enumerate(agent_ids[:15]):
                request = {'task_id': f'perf_test_{i}', 'complexity': 0.1 + i % 3 * 0.1, 'features': ['reasoning']}
                task = manager.process_agent_request(agent_id, request)
                process_tasks.append(task)
            start_time = time.time()
            results = await asyncio.gather(*process_tasks, return_exceptions=True)
            process_time = time.time() - start_time
            successful = [r for r in results if not isinstance(r, Exception)]
            logger.info(f'✅ Processed {len(successful)} requests in {process_time:.2f}s')
            await asyncio.sleep(0.5)
            report = manager.get_performance_report()
            logger.info('✅ Performance report generated:')
            logger.info(f"   - Total agents monitored: {report['optimization']['total_agents_monitored']}")
            logger.info(f"   - System health score: {report['health_status']['overall_score']:.2f}")
            if 'system_performance' in report['optimization']:
                sys_perf = report['optimization']['system_performance']
                logger.info(f"   - Average performance score: {sys_perf['average_score']:.2f}")
                logger.info(f"   - Performance range: {sys_perf['min_score']:.2f} - {sys_perf['max_score']:.2f}")
            suggestions = manager.get_optimization_suggestions()
            logger.info(f'✅ Generated {len(suggestions)} optimization suggestions')
            optimization_result = await manager.trigger_system_optimization()
            logger.info('✅ System optimization completed:')
            logger.info(f"   - Suggestions: {len(optimization_result['suggestions'])}")
            logger.info(f"   - Applied optimizations: {len(optimization_result['applied_optimizations'])}")
            logger.info(f"   - Optimization time: {optimization_result['optimization_time']:.3f}s")
        finally:
            await manager.shutdown()
    logger.info('\n=== Enhanced Agent Manager Tests Completed Successfully ===')
async def test_load_balancing_comparison():
    logger.info('\n=== Load Balancing Strategy Comparison ===')
    strategies = [OptimizationStrategy.ROUND_ROBIN, OptimizationStrategy.PERFORMANCE_BASED, OptimizationStrategy.LEAST_LOADED]
    results = {}
    for strategy in strategies:
        logger.info(f'\nTesting load balancing with {strategy.value}...')
        manager = AgentManager(max_concurrent_agents=30, optimization_strategy=strategy)
        try:
            agent_ids = []
            for i in range(15):
                capabilities = AgentCapabilities(reasoning=True, processing_power=0.8 + i % 5 * 0.3)
                agent_id = await manager.spawn_agent(capabilities)
                agent_ids.append(agent_id)
            allocation_counts = {agent_id: 0 for agent_id in agent_ids}
            for round_num in range(10):
                request = {'required_capabilities': {'reasoning': True}, 'context': {'round': round_num}}
                allocated = await manager.allocate_agents(request, count=5)
                for agent_id in allocated:
                    allocation_counts[agent_id] += 1
            counts = list(allocation_counts.values())
            min_allocations = min(counts)
            max_allocations = max(counts)
            avg_allocations = sum(counts) / len(counts)
            variance = sum(((x - avg_allocations) ** 2 for x in counts)) / len(counts)
            results[strategy.value] = {'min_allocations': min_allocations, 'max_allocations': max_allocations, 'average_allocations': avg_allocations, 'variance': variance, 'distribution_evenness': 1.0 - (variance / avg_allocations if avg_allocations > 0 else 0)}
            logger.info(f'✅ {strategy.value} results:')
            logger.info(f'   - Allocation range: {min_allocations} - {max_allocations}')
            logger.info(f'   - Average allocations: {avg_allocations:.1f}')
            logger.info(f"   - Distribution evenness: {results[strategy.value]['distribution_evenness']:.2f}")
        finally:
            await manager.shutdown()
    logger.info('\n=== Load Balancing Comparison Summary ===')
    for strategy, result in results.items():
        logger.info(f'{strategy}:')
        logger.info(f"  Distribution evenness: {result['distribution_evenness']:.3f}")
        logger.info(f"  Allocation variance: {result['variance']:.2f}")
async def test_performance_optimization():
    logger.info('\n=== Performance Optimization Test ===')
    manager = AgentManager(max_concurrent_agents=25, optimization_strategy=OptimizationStrategy.PERFORMANCE_BASED)
    try:
        fast_agents = []
        slow_agents = []
        for i in range(10):
            fast_capabilities = AgentCapabilities(reasoning=True, processing_power=1.8 + i % 3 * 0.1)
            fast_id = await manager.spawn_agent(fast_capabilities)
            fast_agents.append(fast_id)
            slow_capabilities = AgentCapabilities(reasoning=True, processing_power=0.7 + i % 3 * 0.1)
            slow_id = await manager.spawn_agent(slow_capabilities)
            slow_agents.append(slow_id)
        logger.info(f'Created {len(fast_agents)} fast agents and {len(slow_agents)} slow agents')
        logger.info('Simulating performance differences...')
        for i, agent_id in enumerate(fast_agents[:5]):
            request = {'task_id': f'fast_test_{i}', 'complexity': 0.05, 'features': ['reasoning']}
            await manager.process_agent_request(agent_id, request)
        for i, agent_id in enumerate(slow_agents[:5]):
            request = {'task_id': f'slow_test_{i}', 'complexity': 0.2, 'features': ['reasoning']}
            await manager.process_agent_request(agent_id, request)
        await asyncio.sleep(0.5)
        logger.info('Testing performance-based allocation...')
        allocation_request = {'required_capabilities': {'reasoning': True}, 'context': {'prefer_performance': True}}
        allocated_agents = await manager.allocate_agents(allocation_request, count=8)
        fast_allocated = len([aid for aid in allocated_agents if aid in fast_agents])
        slow_allocated = len([aid for aid in allocated_agents if aid in slow_agents])
        logger.info('✅ Performance-based allocation results:')
        logger.info(f'   - Fast agents allocated: {fast_allocated}')
        logger.info(f'   - Slow agents allocated: {slow_allocated}')
        logger.info(f'   - Fast agent preference: {fast_allocated / len(allocated_agents):.1%}')
        suggestions = manager.get_optimization_suggestions()
        if suggestions:
            logger.info(f'✅ System generated {len(suggestions)} optimization suggestions:')
            for suggestion in suggestions:
                logger.info(f"   - {suggestion['action']}: {suggestion['reason']}")
    finally:
        await manager.shutdown()
if __name__ == '__main__':
    asyncio.run(test_enhanced_agent_manager())
    asyncio.run(test_load_balancing_comparison())
    asyncio.run(test_performance_optimization())