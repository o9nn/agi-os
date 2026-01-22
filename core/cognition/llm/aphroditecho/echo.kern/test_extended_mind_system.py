import pytest
import asyncio
import time
import numpy as np
from unittest.mock import Mock
from typing import List
from extended_mind_system import ExtendedMindSystem, CognitiveTask, CognitiveTaskType, ToolType, ResourceType, CognitiveTool, EnvironmentalResource, ToolIntegrationManager, ResourceCouplingEngine, SocialCoordinationSystem, ScaffoldingResult
from cognitive_tools import MemoryStoreTool, ComputationTool, KnowledgeBaseTool, create_default_cognitive_tools
import contextlib
class TestExtendedMindSystem:
    @pytest.fixture
    def extended_mind_system(self):
        return ExtendedMindSystem()
    @pytest.fixture
    def sample_cognitive_task(self):
        return CognitiveTask(task_id='test_task_001', task_type=CognitiveTaskType.PROBLEM_SOLVING, description='Solve optimization problem with constraints', parameters={'problem_type': 'optimization', 'constraints': {'max_iterations': 1000}, 'objective': 'minimize_cost'}, priority=0.7, required_capabilities=['optimization', 'mathematical_calculation'])
    @pytest.mark.asyncio
    async def test_basic_cognitive_scaffolding(self, extended_mind_system, sample_cognitive_task):
        default_tools = create_default_cognitive_tools()
        for tool_spec, tool_interface in default_tools:
            extended_mind_system.tool_integration.register_tool(tool_spec, tool_interface)
        resources = ['computational', 'memory']
        extended_mind_system.resource_coupling.register_resource(EnvironmentalResource(resource_id='computational', resource_type=ResourceType.COMPUTATIONAL, name='CPU Computational Resource', capacity=100.0, available_capacity=80.0))
        result = await extended_mind_system.enhance_cognition(sample_cognitive_task, resources)
        assert isinstance(result, ScaffoldingResult)
        assert result.task_id == sample_cognitive_task.task_id
        assert isinstance(result.tools_used, list)
        assert isinstance(result.resources_utilized, list)
        assert 'response_time' in result.performance_metrics
    @pytest.mark.asyncio
    async def test_performance_constraints(self, extended_mind_system, sample_cognitive_task):
        start_time = time.time()
        result = await extended_mind_system.enhance_cognition(sample_cognitive_task, [])
        response_time = time.time() - start_time
        assert response_time < 1.0, f'Response time {response_time:.3f}s exceeds 1.0s limit'
        assert result.performance_metrics['response_time'] < 1.0
    def test_performance_metrics_tracking(self, extended_mind_system):
        metrics = extended_mind_system.get_performance_summary()
        assert metrics['response_time_count'] == 0
        extended_mind_system.performance_metrics['response_time'] = [0.1, 0.2, 0.15]
        extended_mind_system.performance_metrics['success_rate'] = [1.0, 1.0, 0.0]
        metrics = extended_mind_system.get_performance_summary()
        assert metrics['response_time_avg'] == pytest.approx(0.15, abs=0.001)
        assert metrics['success_rate_avg'] == pytest.approx(0.667, abs=0.01)
        assert metrics['response_time_count'] == 3
class TestToolIntegrationManager:
    @pytest.fixture
    def tool_manager(self):
        return ToolIntegrationManager()
    @pytest.fixture
    def mock_tool(self):
        return CognitiveTool(tool_id='mock_tool_01', tool_type=ToolType.COMPUTATION, name='Mock Computation Tool', description='Mock tool for testing', capabilities=['calculation', 'analysis'], interface={'operations': ['compute']}, availability=1.0, cost=0.1, reliability=0.95)
    @pytest.fixture
    def mock_tool_interface(self):
        interface = Mock()
        interface.execute.return_value = {'result': 'mock_result'}
        interface.get_capabilities.return_value = ['calculation', 'analysis']
        interface.estimate_cost.return_value = 0.1
        return interface
    def test_tool_registration(self, tool_manager, mock_tool, mock_tool_interface):
        tool_manager.register_tool(mock_tool, mock_tool_interface)
        assert mock_tool.tool_id in tool_manager.tools
        assert mock_tool.tool_id in tool_manager.tool_interfaces
        assert tool_manager.tools[mock_tool.tool_id] == mock_tool
    def test_tool_selection_oeis_compliance(self, tool_manager):
        for i in range(15):
            tool = CognitiveTool(tool_id=f'tool_{i:02d}', tool_type=ToolType.COMPUTATION, name=f'Tool {i}', description=f'Test tool {i}', capabilities=['test'], interface={}, availability=1.0 - i * 0.05, reliability=1.0)
            interface = Mock()
            tool_manager.register_tool(tool, interface)
        task = CognitiveTask(task_id='test_selection', task_type=CognitiveTaskType.COMPUTATION, description='Test task for tool selection', parameters={}, required_capabilities=['test'])
        selected_tools = tool_manager.identify_tools(task)
        assert len(selected_tools) <= 9, f'Selected {len(selected_tools)} tools, exceeds A000081[4] = 9'
        availabilities = [tool_manager.tools[tool_id].availability for tool_id in selected_tools]
        assert availabilities == sorted(availabilities, reverse=True), 'Tools not sorted by availability'
    @pytest.mark.asyncio
    async def test_concurrent_tool_execution_limits(self, tool_manager, mock_tool, mock_tool_interface):
        tool_manager.register_tool(mock_tool, mock_tool_interface)
        async def mock_execute(task, params):
            await asyncio.sleep(0.1)
            return {'result': 'success'}
        mock_tool_interface.execute = mock_execute
        task = CognitiveTask(task_id='concurrent_test', task_type=CognitiveTaskType.COMPUTATION, description='Test concurrent execution', parameters={})
        operations = []
        for i in range(tool_manager.max_concurrent_tools + 5):
            try:
                op = tool_manager.execute_tool_operation(mock_tool.tool_id, task, {})
                operations.append(op)
            except RuntimeError as e:
                assert 'Maximum concurrent tool operations reached' in str(e)
                break
        for op in operations:
            with contextlib.suppress(Exception):
                await op
class TestResourceCouplingEngine:
    @pytest.fixture
    def resource_engine(self):
        return ResourceCouplingEngine()
    @pytest.fixture
    def sample_resources(self):
        return [EnvironmentalResource(resource_id='cpu_01', resource_type=ResourceType.COMPUTATIONAL, name='CPU Resource 1', capacity=100.0, available_capacity=80.0, access_time=0.01, quality=0.9), EnvironmentalResource(resource_id='memory_01', resource_type=ResourceType.MEMORY, name='Memory Resource 1', capacity=1000.0, available_capacity=750.0, access_time=0.005, quality=0.95)]
    def test_resource_registration(self, resource_engine, sample_resources):
        for resource in sample_resources:
            resource_engine.register_resource(resource)
        assert len(resource_engine.resources) == 2
        assert 'cpu_01' in resource_engine.resources
        assert 'memory_01' in resource_engine.resources
    def test_resource_allocation(self, resource_engine, sample_resources):
        for resource in sample_resources:
            resource_engine.register_resource(resource)
        task = CognitiveTask(task_id='resource_test', task_type=CognitiveTaskType.COMPUTATION, description='Test resource allocation', parameters={})
        available_resource_ids = [r.resource_id for r in sample_resources]
        allocation = resource_engine.couple_resources(task, available_resource_ids)
        assert isinstance(allocation, dict)
        assert len(allocation) <= len(sample_resources)
        for resource_id, allocated_amount in allocation.items():
            resource = resource_engine.resources[resource_id]
            assert allocated_amount <= resource.available_capacity
class TestSocialCoordinationSystem:
    @pytest.fixture
    def social_system(self):
        return SocialCoordinationSystem()
    def test_agent_registration(self, social_system):
        social_system.register_agent('agent_01', capabilities=['problem_solving', 'analysis'], availability=0.8)
        assert 'agent_01' in social_system.agents
        assert social_system.agents['agent_01']['availability'] == 0.8
        assert 'problem_solving' in social_system.agents['agent_01']['capabilities']
    def test_coordination_strategies(self, social_system):
        agents = [('agent_01', ['problem_solving'], 1.0), ('agent_02', ['analysis'], 0.9), ('agent_03', ['computation'], 0.8), ('agent_04', ['verification'], 0.7)]
        for agent_id, capabilities, availability in agents:
            social_system.register_agent(agent_id, capabilities, availability)
        test_cases = [(CognitiveTaskType.PROBLEM_SOLVING, 'hierarchical_decomposition'), (CognitiveTaskType.PLANNING, 'hierarchical_decomposition'), (CognitiveTaskType.MEMORY_RETRIEVAL, 'distributed_processing')]
        for task_type, expected_strategy in test_cases:
            task = CognitiveTask(task_id='coordination_test', task_type=task_type, description='Test coordination', parameters={}, required_capabilities=['problem_solving'])
            result = social_system.coordinate(task, [], {})
            if len(result['participants']) > 1:
                assert result['coordination_type'] == expected_strategy
class TestCognitiveTool:
    @pytest.mark.asyncio
    async def test_memory_store_tool(self):
        tool = MemoryStoreTool(storage_capacity=100)
        task = CognitiveTask('test', CognitiveTaskType.MEMORY_RETRIEVAL, 'Test storage', {})
        store_result = await tool.execute(task, {'operation': 'store', 'key': 'test_key', 'value': 'test_value', 'metadata': {'category': 'test'}})
        assert store_result['status'] == 'stored'
        assert store_result['key'] == 'test_key'
        retrieve_result = await tool.execute(task, {'operation': 'retrieve', 'key': 'test_key'})
        assert retrieve_result['status'] == 'found'
        assert retrieve_result['value'] == 'test_value'
        search_result = await tool.execute(task, {'operation': 'search', 'query': {'terms': ['test']}})
        assert len(search_result) > 0
        assert any((result['key'] == 'test_key' for result in search_result))
    @pytest.mark.asyncio
    async def test_computation_tool(self):
        tool = ComputationTool()
        task = CognitiveTask('test', CognitiveTaskType.PROBLEM_SOLVING, 'Test computation', {})
        calc_result = await tool.execute(task, {'type': 'calculate', 'expression': '2 + 2 * 3'})
        assert calc_result['status'] == 'success'
        assert calc_result['result'] == 8
        analysis_result = await tool.execute(task, {'type': 'analyze', 'data': [1, 2, 3, 4, 5]})
        assert analysis_result['status'] == 'success'
        assert 'statistics' in analysis_result
        assert analysis_result['statistics']['mean'] == 3.0
        sim_result = await tool.execute(task, {'type': 'simulate', 'model': 'linear_growth', 'parameters': {'steps': 10, 'growth_rate': 0.1, 'initial_value': 1.0}})
        assert sim_result['status'] == 'success'
        assert 'results' in sim_result
        assert len(sim_result['results']['values']) == 10
    @pytest.mark.asyncio
    async def test_knowledge_base_tool(self):
        tool = KnowledgeBaseTool()
        task = CognitiveTask('test', CognitiveTaskType.REASONING, 'Test knowledge', {})
        lookup_result = await tool.execute(task, {'type': 'lookup', 'concept': 'cognition'})
        assert lookup_result['status'] == 'found'
        assert 'information' in lookup_result
        search_result = await tool.execute(task, {'type': 'search', 'query': 'memory'})
        assert search_result['status'] == 'success'
        assert len(search_result['results']) > 0
        relation_result = await tool.execute(task, {'type': 'relate', 'concept1': 'cognition', 'concept2': 'memory'})
        assert relation_result['status'] == 'success'
        assert 'direct_relations' in relation_result
class TestOEISA000081Compliance:
    def test_tool_selection_enumeration(self):
        manager = ToolIntegrationManager()
        for i in range(20):
            tool = CognitiveTool(tool_id=f'tool_{i:02d}', tool_type=ToolType.COMPUTATION, name=f'Tool {i}', description=f'Test tool {i}', capabilities=['test'], interface={}, availability=1.0, reliability=1.0)
            interface = Mock()
            manager.register_tool(tool, interface)
        task = CognitiveTask(task_id='oeis_test', task_type=CognitiveTaskType.PROBLEM_SOLVING, description='Test OEIS compliance', parameters={}, required_capabilities=['test'])
        selected_tools = manager.identify_tools(task)
        assert len(selected_tools) <= 9, f'Tool selection violates A000081[4] = 9 limit: {len(selected_tools)}'
    def test_resource_membrane_hierarchy(self):
        engine = ResourceCouplingEngine()
        if hasattr(engine, 'resource_psystem'):
            assert engine.resource_psystem.max_membranes == 4
    def test_neural_network_sizing(self):
        manager = ToolIntegrationManager()
        if hasattr(manager, 'tool_selection_esn'):
            assert manager.tool_selection_esn.reservoir_size == 48
            assert manager.tool_selection_esn.output_size == 9
class TestRealTimePerformance:
    @pytest.mark.asyncio
    async def test_tool_execution_latency(self):
        tools = create_default_cognitive_tools()
        for tool_spec, tool_interface in tools:
            task = CognitiveTask(task_id='latency_test', task_type=CognitiveTaskType.COMPUTATION, description='Test latency', parameters={'operation': 'test'})
            start_time = time.time()
            try:
                await tool_interface.execute(task, {'operation': 'test'})
                latency = time.time() - start_time
                assert latency <= tool_spec.latency * 2, f'Tool {tool_spec.name} exceeded latency: {latency:.3f}s > {tool_spec.latency * 2:.3f}s'
            except (ValueError, KeyError):
                pass
    @pytest.mark.asyncio
    async def test_memory_consolidation_timing(self):
        tool = MemoryStoreTool()
        task = CognitiveTask('test', CognitiveTaskType.MEMORY_RETRIEVAL, 'Test', {})
        times = []
        for i in range(10):
            start_time = time.time()
            await tool.execute(task, {'operation': 'store', 'key': f'test_key_{i}', 'value': f'test_value_{i}'})
            consolidation_time = time.time() - start_time
            times.append(consolidation_time)
        avg_time = np.mean(times)
        max_time = np.max(times)
        assert avg_time <= 0.1, f'Average consolidation time {avg_time:.3f}s exceeds 100ms limit'
        assert max_time <= 0.15, f'Maximum consolidation time {max_time:.3f}s exceeds reasonable bounds'
def create_test_task(task_type: CognitiveTaskType=CognitiveTaskType.PROBLEM_SOLVING, capabilities: List[str]=None) -> CognitiveTask:
    if capabilities is None:
        capabilities = ['general']
    return CognitiveTask(task_id=f'test_task_{int(time.time())}', task_type=task_type, description=f'Test task for {task_type.value}', parameters={}, required_capabilities=capabilities)
def create_test_resource(resource_type: ResourceType=ResourceType.COMPUTATIONAL) -> EnvironmentalResource:
    return EnvironmentalResource(resource_id=f'test_resource_{int(time.time())}', resource_type=resource_type, name=f'Test {resource_type.value} Resource', capacity=100.0, available_capacity=80.0, access_time=0.01, quality=0.9)
if __name__ == '__main__':
    pytest.main([__file__, '-v'])