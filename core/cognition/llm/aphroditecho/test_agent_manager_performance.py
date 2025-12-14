#!/usr/bin/env python3
"""
Performance tests for Agent Manager Component - Task 1.2.2
Tests the acceptance criteria: Supports 100+ concurrent agents

This test validates:
- Agent lifecycle management (spawn, evolve, terminate)
- Resource allocation and scheduling
- Performance monitoring and optimization
- Concurrent agent handling up to 100+ agents
"""

import asyncio
import time
import pytest
import logging

from aar_core.agents.agent_manager import AgentManager, AgentCapabilities, AgentStatus

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class TestAgentManagerPerformance:
    """Performance and functionality tests for Agent Manager Component."""
    
    @pytest.fixture
    @pytest.mark.asyncio
    async def agent_manager(self):
        """Create Agent Manager for testing."""
        manager = AgentManager(max_concurrent_agents=200)  # Allow more than 100 for testing
        try:
            yield manager
        finally:
            await manager.shutdown()
    
    @pytest.mark.asyncio
    async def test_agent_lifecycle_management(self, agent_manager):
        """Test basic agent lifecycle operations."""
        # Test agent spawning
        agent_id = await agent_manager.spawn_agent()
        assert agent_id is not None
        assert len(agent_manager.agents) == 1
        
        # Test agent status retrieval
        status = agent_manager.get_agent_status(agent_id)
        assert status is not None
        assert status['status'] == AgentStatus.ACTIVE.value
        
        # Test agent termination
        await agent_manager.terminate_agent(agent_id)
        assert len(agent_manager.agents) == 0
        
        logger.info("✅ Agent lifecycle management test passed")
    
    @pytest.mark.asyncio
    async def test_concurrent_agents_100_plus(self):
        """Test handling of 100+ concurrent agents (acceptance criteria)."""
        agent_manager = AgentManager(max_concurrent_agents=200)
        
        try:
            target_agents = 120  # Test above the 100+ requirement
            
            logger.info(f"Spawning {target_agents} concurrent agents...")
            start_time = time.time()
            
            # Spawn agents concurrently
            spawn_tasks = []
            for i in range(target_agents):
                capabilities = AgentCapabilities(
                    reasoning=True,
                    multimodal=i % 3 == 0,  # Vary capabilities
                    processing_power=1.0 + (i % 5) * 0.2
                )
                task = agent_manager.spawn_agent(capabilities)
                spawn_tasks.append(task)
            
            # Wait for all agents to be spawned
            agent_ids = await asyncio.gather(*spawn_tasks)
            spawn_time = time.time() - start_time
            
            # Validate all agents were created successfully
            assert len(agent_ids) == target_agents
            assert len(agent_manager.agents) == target_agents
            
            # Check system stats
            stats = agent_manager.get_system_stats()
            assert stats['agent_counts']['total'] == target_agents
            assert stats['resource_usage']['utilization_percentage'] <= 100
            
            logger.info(f"✅ Successfully spawned {target_agents} agents in {spawn_time:.2f}s")
            
            # Test resource allocation with multiple agents
            allocation_request = {
                'required_capabilities': {
                    'reasoning': True,
                    'min_processing_power': 1.0
                },
                'context': {'test': True}
            }
            
            allocated_agents = await agent_manager.allocate_agents(allocation_request, count=10)
            assert len(allocated_agents) == 10
            
            logger.info(f"✅ Successfully allocated 10 agents from pool of {target_agents}")
            
            # Test concurrent request processing
            process_start = time.time()
            process_tasks = []
            
            for i in range(min(50, len(agent_ids))):  # Process requests on first 50 agents
                agent_id = agent_ids[i]
                request = {
                    'task_id': f'test_task_{i}',
                    'complexity': 0.1,  # Low complexity for speed
                    'features': ['reasoning']
                }
                task = agent_manager.process_agent_request(agent_id, request)
                process_tasks.append(task)
            
            # Wait for all requests to complete
            results = await asyncio.gather(*process_tasks, return_exceptions=True)
            process_time = time.time() - process_start
            
            # Validate results
            successful_results = [r for r in results if not isinstance(r, Exception)]
            assert len(successful_results) >= 40  # Allow some failures for robustness
            
            logger.info(f"✅ Processed {len(successful_results)} concurrent requests in {process_time:.2f}s")
            
            # Test performance monitoring
            final_stats = agent_manager.get_system_stats()
            assert final_stats['performance_stats']['total_spawned'] == target_agents
            assert final_stats['resource_usage']['total_requests'] >= 40
            
            health_status = final_stats['health_status']
            assert health_status['overall_score'] > 0.0  # System should be functional
            
            logger.info(f"✅ System health score: {health_status['overall_score']:.2f}")
            logger.info(f"✅ Final system utilization: {final_stats['resource_usage']['utilization_percentage']:.1f}%")
            
        finally:
            await agent_manager.shutdown()
    
    @pytest.mark.asyncio
    async def test_agent_evolution_capabilities(self, agent_manager):
        """Test agent evolution functionality."""
        # Spawn agent with learning capabilities
        capabilities = AgentCapabilities(learning_enabled=True, processing_power=1.0)
        agent_id = await agent_manager.spawn_agent(capabilities)
        
        # Get initial stats
        initial_status = agent_manager.get_agent_status(agent_id)
        initial_generation = initial_status['metrics']['evolution_generation']
        initial_processing_power = initial_status['capabilities']['processing_power']
        
        # Evolve the agent
        evolution_data = {'performance_score': 0.9}  # High performance score
        await agent_manager.evolve_agent(agent_id, evolution_data)
        
        # Check evolution results
        evolved_status = agent_manager.get_agent_status(agent_id)
        assert evolved_status['metrics']['evolution_generation'] > initial_generation
        assert evolved_status['capabilities']['processing_power'] >= initial_processing_power
        
        # Check system evolution stats
        stats = agent_manager.get_system_stats()
        assert stats['performance_stats']['evolution_cycles'] > 0
        
        logger.info("✅ Agent evolution capabilities test passed")
    
    @pytest.mark.asyncio
    async def test_resource_allocation_and_scheduling(self, agent_manager):
        """Test resource allocation and intelligent scheduling."""
        # Create agents with different capabilities
        agents = []
        for i in range(20):
            capabilities = AgentCapabilities(
                reasoning=True,
                multimodal=i % 2 == 0,
                specialized_domains=['math'] if i < 10 else ['language'],
                processing_power=1.0 + i * 0.1
            )
            agent_id = await agent_manager.spawn_agent(capabilities)
            agents.append(agent_id)
        
        # Test capability-based allocation
        math_request = {
            'required_capabilities': {
                'reasoning': True,
                'domains': ['math'],
                'min_processing_power': 1.5
            }
        }
        
        allocated = await agent_manager.allocate_agents(math_request, count=5)
        assert len(allocated) == 5
        
        # Verify allocated agents have correct capabilities
        for agent_id in allocated:
            status = agent_manager.get_agent_status(agent_id)
            assert status['capabilities']['reasoning'] is True
            assert 'math' in status['capabilities']['specialized_domains']
            assert status['capabilities']['processing_power'] >= 1.5
        
        logger.info("✅ Resource allocation and scheduling test passed")
    
    @pytest.mark.asyncio
    async def test_performance_monitoring_and_optimization(self, agent_manager):
        """Test performance monitoring and system optimization."""
        # Spawn multiple agents
        agent_count = 30
        agent_ids = []
        for i in range(agent_count):
            agent_id = await agent_manager.spawn_agent()
            agent_ids.append(agent_id)
        
        # Process multiple requests to generate metrics
        for i, agent_id in enumerate(agent_ids[:10]):  # Use first 10 agents
            request = {
                'task_id': f'perf_test_{i}',
                'complexity': 0.2,
                'features': ['reasoning']
            }
            await agent_manager.process_agent_request(agent_id, request)
        
        # Get detailed system statistics
        stats = agent_manager.get_system_stats()
        
        # Validate performance monitoring
        assert 'agent_counts' in stats
        assert 'resource_usage' in stats
        assert 'performance_stats' in stats
        assert 'health_status' in stats
        
        assert stats['agent_counts']['total'] == agent_count
        assert stats['resource_usage']['total_requests'] >= 10
        assert stats['performance_stats']['total_spawned'] == agent_count
        
        # Test health monitoring
        health = stats['health_status']
        assert 0.0 <= health['overall_score'] <= 1.0
        assert health['status'] in ['healthy', 'degraded', 'critical']
        
        logger.info("✅ Performance monitoring validated")
        logger.info(f"   - Total agents: {stats['agent_counts']['total']}")
        logger.info(f"   - Health score: {health['overall_score']:.2f}")
        logger.info(f"   - System status: {health['status']}")
    
    @pytest.mark.asyncio
    async def test_error_handling_and_recovery(self, agent_manager):
        """Test error handling and system recovery capabilities."""
        # Test invalid agent operations
        invalid_agent_id = "nonexistent_agent"
        
        # Should handle gracefully without crashing
        status = agent_manager.get_agent_status(invalid_agent_id)
        assert status is None
        
        await agent_manager.terminate_agent(invalid_agent_id)  # Should not crash
        await agent_manager.evolve_agent(invalid_agent_id, {})  # Should not crash
        
        # Test capacity limits
        original_capacity = agent_manager.max_concurrent_agents
        agent_manager.max_concurrent_agents = 5  # Temporarily reduce capacity
        
        # Spawn up to capacity
        spawned_agents = []
        for i in range(5):
            agent_id = await agent_manager.spawn_agent()
            spawned_agents.append(agent_id)
        
        # Test capacity exceeded
        with pytest.raises(RuntimeError, match="Maximum agent capacity"):
            await agent_manager.spawn_agent()
        
        # Restore capacity
        agent_manager.max_concurrent_agents = original_capacity
        
        logger.info("✅ Error handling and recovery test passed")


# Standalone test runner
async def run_performance_tests():
    """Run performance tests standalone."""
    logger.info("=== Agent Manager Performance Tests ===")
    
    # Create agent manager
    manager = AgentManager(max_concurrent_agents=150)
    
    try:
        # Basic functionality test
        logger.info("Testing basic functionality...")
        agent_id = await manager.spawn_agent()
        status = manager.get_agent_status(agent_id)
        assert status['status'] == AgentStatus.ACTIVE.value
        await manager.terminate_agent(agent_id)
        logger.info("✅ Basic functionality test passed")
        
        # Concurrent agents test
        logger.info("Testing 100+ concurrent agents...")
        start_time = time.time()
        
        # Spawn 120 agents
        spawn_tasks = [manager.spawn_agent() for _ in range(120)]
        agent_ids = await asyncio.gather(*spawn_tasks)
        
        spawn_time = time.time() - start_time
        logger.info(f"✅ Spawned {len(agent_ids)} agents in {spawn_time:.2f}s")
        
        # Test system stats
        stats = manager.get_system_stats()
        logger.info(f"✅ System utilization: {stats['resource_usage']['utilization_percentage']:.1f}%")
        logger.info(f"✅ Health score: {stats['health_status']['overall_score']:.2f}")
        
        # Process concurrent requests
        process_tasks = []
        for i, agent_id in enumerate(agent_ids[:50]):
            request = {
                'task_id': f'test_{i}',
                'complexity': 0.1,
                'features': ['reasoning']
            }
            task = manager.process_agent_request(agent_id, request)
            process_tasks.append(task)
        
        process_start = time.time()
        results = await asyncio.gather(*process_tasks, return_exceptions=True)
        process_time = time.time() - process_start
        
        successful = [r for r in results if not isinstance(r, Exception)]
        logger.info(f"✅ Processed {len(successful)} requests in {process_time:.2f}s")
        
        logger.info("=== All Performance Tests PASSED ===")
        
    except Exception as e:
        logger.error(f"❌ Test failed: {e}")
        raise
    finally:
        await manager.shutdown()


if __name__ == "__main__":
    # Run tests if called directly
    asyncio.run(run_performance_tests())