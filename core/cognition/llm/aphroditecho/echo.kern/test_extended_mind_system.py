#!/usr/bin/env python3
"""
Test Suite for Extended Mind System - Cognitive Scaffolding

This module provides comprehensive tests for the Extended Mind System
implementation, ensuring OEIS A000081 compliance and real-time performance.
"""

import pytest
import asyncio
import time
import numpy as np
from unittest.mock import Mock
from typing import List

# Import the modules to test
from extended_mind_system import (
    ExtendedMindSystem, CognitiveTask, CognitiveTaskType, ToolType, 
    ResourceType, CognitiveTool, EnvironmentalResource, 
    ToolIntegrationManager, ResourceCouplingEngine,
    SocialCoordinationSystem, ScaffoldingResult
)

from cognitive_tools import (
    MemoryStoreTool, ComputationTool, KnowledgeBaseTool,
    create_default_cognitive_tools
)
import contextlib

class TestExtendedMindSystem:
    """Test cases for the main Extended Mind System."""
    
    @pytest.fixture
    def extended_mind_system(self):
        """Create Extended Mind System for testing."""
        return ExtendedMindSystem()
    
    @pytest.fixture
    def sample_cognitive_task(self):
        """Create sample cognitive task for testing."""
        return CognitiveTask(
            task_id="test_task_001",
            task_type=CognitiveTaskType.PROBLEM_SOLVING,
            description="Solve optimization problem with constraints",
            parameters={
                'problem_type': 'optimization',
                'constraints': {'max_iterations': 1000},
                'objective': 'minimize_cost'
            },
            priority=0.7,
            required_capabilities=['optimization', 'mathematical_calculation']
        )
    
    @pytest.mark.asyncio
    async def test_basic_cognitive_scaffolding(self, extended_mind_system, sample_cognitive_task):
        """Test basic cognitive scaffolding functionality."""
        # Setup default tools
        default_tools = create_default_cognitive_tools()
        for tool_spec, tool_interface in default_tools:
            extended_mind_system.tool_integration.register_tool(tool_spec, tool_interface)
        
        # Setup resources
        resources = ['computational', 'memory']
        extended_mind_system.resource_coupling.register_resource(
            EnvironmentalResource(
                resource_id='computational',
                resource_type=ResourceType.COMPUTATIONAL,
                name='CPU Computational Resource',
                capacity=100.0,
                available_capacity=80.0
            )
        )
        
        # Execute cognitive scaffolding
        result = await extended_mind_system.enhance_cognition(
            sample_cognitive_task, resources
        )
        
        # Verify result structure
        assert isinstance(result, ScaffoldingResult)
        assert result.task_id == sample_cognitive_task.task_id
        assert isinstance(result.tools_used, list)
        assert isinstance(result.resources_utilized, list)
        assert 'response_time' in result.performance_metrics
    
    @pytest.mark.asyncio
    async def test_performance_constraints(self, extended_mind_system, sample_cognitive_task):
        """Test that system meets real-time performance constraints."""
        # Setup minimal configuration for performance test
        start_time = time.time()
        
        result = await extended_mind_system.enhance_cognition(
            sample_cognitive_task, []
        )
        
        response_time = time.time() - start_time
        
        # Verify performance constraints
        # Extended mind scaffolding should complete within 1 second for basic tasks
        assert response_time < 1.0, f"Response time {response_time:.3f}s exceeds 1.0s limit"
        assert result.performance_metrics['response_time'] < 1.0
    
    def test_performance_metrics_tracking(self, extended_mind_system):
        """Test performance metrics tracking."""
        # Initially empty metrics
        metrics = extended_mind_system.get_performance_summary()
        assert metrics['response_time_count'] == 0
        
        # Add some mock performance data
        extended_mind_system.performance_metrics['response_time'] = [0.1, 0.2, 0.15]
        extended_mind_system.performance_metrics['success_rate'] = [1.0, 1.0, 0.0]
        
        metrics = extended_mind_system.get_performance_summary()
        
        assert metrics['response_time_avg'] == pytest.approx(0.15, abs=1e-3)
        assert metrics['success_rate_avg'] == pytest.approx(0.667, abs=1e-2)
        assert metrics['response_time_count'] == 3

class TestToolIntegrationManager:
    """Test cases for Tool Integration Manager."""
    
    @pytest.fixture
    def tool_manager(self):
        """Create Tool Integration Manager for testing."""
        return ToolIntegrationManager()
    
    @pytest.fixture
    def mock_tool(self):
        """Create mock cognitive tool."""
        return CognitiveTool(
            tool_id="mock_tool_01",
            tool_type=ToolType.COMPUTATION,
            name="Mock Computation Tool",
            description="Mock tool for testing",
            capabilities=['calculation', 'analysis'],
            interface={'operations': ['compute']},
            availability=1.0,
            cost=0.1,
            reliability=0.95
        )
    
    @pytest.fixture
    def mock_tool_interface(self):
        """Create mock tool interface."""
        interface = Mock()
        interface.execute.return_value = {'result': 'mock_result'}
        interface.get_capabilities.return_value = ['calculation', 'analysis']
        interface.estimate_cost.return_value = 0.1
        return interface
    
    def test_tool_registration(self, tool_manager, mock_tool, mock_tool_interface):
        """Test tool registration functionality."""
        tool_manager.register_tool(mock_tool, mock_tool_interface)
        
        assert mock_tool.tool_id in tool_manager.tools
        assert mock_tool.tool_id in tool_manager.tool_interfaces
        assert tool_manager.tools[mock_tool.tool_id] == mock_tool
    
    def test_tool_selection_oeis_compliance(self, tool_manager):
        """Test that tool selection follows OEIS A000081 constraints."""
        # Register multiple tools
        for i in range(15):  # More than A000081[4] = 9 limit
            tool = CognitiveTool(
                tool_id=f"tool_{i:02d}",
                tool_type=ToolType.COMPUTATION,
                name=f"Tool {i}",
                description=f"Test tool {i}",
                capabilities=['test'],
                interface={},
                availability=1.0 - (i * 0.05),  # Decreasing availability
                reliability=1.0
            )
            interface = Mock()
            tool_manager.register_tool(tool, interface)
        
        # Create test task
        task = CognitiveTask(
            task_id="test_selection",
            task_type=CognitiveTaskType.COMPUTATION,
            description="Test task for tool selection",
            parameters={},
            required_capabilities=['test']
        )
        
        selected_tools = tool_manager.identify_tools(task)
        
        # Verify OEIS A000081 compliance (max 9 tools)
        assert len(selected_tools) <= 9, f"Selected {len(selected_tools)} tools, exceeds A000081[4] = 9"
        
        # Verify tools are selected by availability (higher availability first)
        availabilities = [tool_manager.tools[tool_id].availability for tool_id in selected_tools]
        assert availabilities == sorted(availabilities, reverse=True), "Tools not sorted by availability"
    
    @pytest.mark.asyncio
    async def test_concurrent_tool_execution_limits(self, tool_manager, mock_tool, mock_tool_interface):
        """Test concurrent tool execution limits."""
        tool_manager.register_tool(mock_tool, mock_tool_interface)
        
        # Configure mock to simulate async execution
        async def mock_execute(task, params):
            await asyncio.sleep(0.1)
            return {'result': 'success'}
        
        mock_tool_interface.execute = mock_execute
        
        # Create test task
        task = CognitiveTask(
            task_id="concurrent_test",
            task_type=CognitiveTaskType.COMPUTATION,
            description="Test concurrent execution",
            parameters={}
        )
        
        # Start multiple operations
        operations = []
        for i in range(tool_manager.max_concurrent_tools + 5):  # Exceed limit
            try:
                op = tool_manager.execute_tool_operation(mock_tool.tool_id, task, {})
                operations.append(op)
            except RuntimeError as e:
                # Should hit concurrency limit
                assert "Maximum concurrent tool operations reached" in str(e)
                break
        
        # Clean up operations
        for op in operations:
            with contextlib.suppress(Exception):
                await op

class TestResourceCouplingEngine:
    """Test cases for Resource Coupling Engine."""
    
    @pytest.fixture
    def resource_engine(self):
        """Create Resource Coupling Engine for testing."""
        return ResourceCouplingEngine()
    
    @pytest.fixture
    def sample_resources(self):
        """Create sample environmental resources."""
        return [
            EnvironmentalResource(
                resource_id='cpu_01',
                resource_type=ResourceType.COMPUTATIONAL,
                name='CPU Resource 1',
                capacity=100.0,
                available_capacity=80.0,
                access_time=0.01,
                quality=0.9
            ),
            EnvironmentalResource(
                resource_id='memory_01',
                resource_type=ResourceType.MEMORY,
                name='Memory Resource 1',
                capacity=1000.0,
                available_capacity=750.0,
                access_time=0.005,
                quality=0.95
            )
        ]
    
    def test_resource_registration(self, resource_engine, sample_resources):
        """Test resource registration."""
        for resource in sample_resources:
            resource_engine.register_resource(resource)
        
        assert len(resource_engine.resources) == 2
        assert 'cpu_01' in resource_engine.resources
        assert 'memory_01' in resource_engine.resources
    
    def test_resource_allocation(self, resource_engine, sample_resources):
        """Test resource allocation algorithm."""
        # Register resources
        for resource in sample_resources:
            resource_engine.register_resource(resource)
        
        # Create test task
        task = CognitiveTask(
            task_id="resource_test",
            task_type=CognitiveTaskType.COMPUTATION,
            description="Test resource allocation",
            parameters={}
        )
        
        # Allocate resources
        available_resource_ids = [r.resource_id for r in sample_resources]
        allocation = resource_engine.couple_resources(task, available_resource_ids)
        
        # Verify allocation
        assert isinstance(allocation, dict)
        assert len(allocation) <= len(sample_resources)
        
        # Verify allocation doesn't exceed available capacity
        for resource_id, allocated_amount in allocation.items():
            resource = resource_engine.resources[resource_id]
            assert allocated_amount <= resource.available_capacity

class TestSocialCoordinationSystem:
    """Test cases for Social Coordination System."""
    
    @pytest.fixture
    def social_system(self):
        """Create Social Coordination System for testing."""
        return SocialCoordinationSystem()
    
    def test_agent_registration(self, social_system):
        """Test collaborative agent registration."""
        social_system.register_agent(
            'agent_01', 
            capabilities=['problem_solving', 'analysis'],
            availability=0.8
        )
        
        assert 'agent_01' in social_system.agents
        assert social_system.agents['agent_01']['availability'] == 0.8
        assert 'problem_solving' in social_system.agents['agent_01']['capabilities']
    
    def test_coordination_strategies(self, social_system):
        """Test different coordination strategies."""
        # Register multiple agents
        agents = [
            ('agent_01', ['problem_solving'], 1.0),
            ('agent_02', ['analysis'], 0.9),
            ('agent_03', ['computation'], 0.8),
            ('agent_04', ['verification'], 0.7)
        ]
        
        for agent_id, capabilities, availability in agents:
            social_system.register_agent(agent_id, capabilities, availability)
        
        # Test different task types and expected coordination strategies
        test_cases = [
            (CognitiveTaskType.PROBLEM_SOLVING, 'hierarchical_decomposition'),
            (CognitiveTaskType.PLANNING, 'hierarchical_decomposition'),
            (CognitiveTaskType.MEMORY_RETRIEVAL, 'distributed_processing')
        ]
        
        for task_type, expected_strategy in test_cases:
            task = CognitiveTask(
                task_id="coordination_test",
                task_type=task_type,
                description="Test coordination",
                parameters={},
                required_capabilities=['problem_solving']
            )
            
            result = social_system.coordinate(task, [], {})
            
            if len(result['participants']) > 1:
                assert result['coordination_type'] == expected_strategy

class TestCognitiveTool:
    """Test cases for cognitive tools."""
    
    @pytest.mark.asyncio
    async def test_memory_store_tool(self):
        """Test Memory Store Tool functionality."""
        tool = MemoryStoreTool(storage_capacity=100)
        
        # Test storage
        task = CognitiveTask("test", CognitiveTaskType.MEMORY_RETRIEVAL, "Test storage", {})
        
        store_result = await tool.execute(task, {
            'operation': 'store',
            'key': 'test_key',
            'value': 'test_value',
            'metadata': {'category': 'test'}
        })
        
        assert store_result['status'] == 'stored'
        assert store_result['key'] == 'test_key'
        
        # Test retrieval
        retrieve_result = await tool.execute(task, {
            'operation': 'retrieve',
            'key': 'test_key'
        })
        
        assert retrieve_result['status'] == 'found'
        assert retrieve_result['value'] == 'test_value'
        
        # Test search
        search_result = await tool.execute(task, {
            'operation': 'search',
            'query': {'terms': ['test']}
        })
        
        assert len(search_result) > 0
        assert any(result['key'] == 'test_key' for result in search_result)
    
    @pytest.mark.asyncio
    async def test_computation_tool(self):
        """Test Computation Tool functionality."""
        tool = ComputationTool()
        
        task = CognitiveTask("test", CognitiveTaskType.PROBLEM_SOLVING, "Test computation", {})
        
        # Test calculation
        calc_result = await tool.execute(task, {
            'type': 'calculate',
            'expression': '2 + 2 * 3'
        })
        
        assert calc_result['status'] == 'success'
        assert calc_result['result'] == 8
        
        # Test data analysis
        analysis_result = await tool.execute(task, {
            'type': 'analyze',
            'data': [1, 2, 3, 4, 5]
        })
        
        assert analysis_result['status'] == 'success'
        assert 'statistics' in analysis_result
        assert analysis_result['statistics']['mean'] == 3.0
        
        # Test simulation
        sim_result = await tool.execute(task, {
            'type': 'simulate',
            'model': 'linear_growth',
            'parameters': {'steps': 10, 'growth_rate': 0.1, 'initial_value': 1.0}
        })
        
        assert sim_result['status'] == 'success'
        assert 'results' in sim_result
        assert len(sim_result['results']['values']) == 10
    
    @pytest.mark.asyncio
    async def test_knowledge_base_tool(self):
        """Test Knowledge Base Tool functionality."""
        tool = KnowledgeBaseTool()
        
        task = CognitiveTask("test", CognitiveTaskType.REASONING, "Test knowledge", {})
        
        # Test concept lookup
        lookup_result = await tool.execute(task, {
            'type': 'lookup',
            'concept': 'cognition'
        })
        
        assert lookup_result['status'] == 'found'
        assert 'information' in lookup_result
        
        # Test search
        search_result = await tool.execute(task, {
            'type': 'search',
            'query': 'memory'
        })
        
        assert search_result['status'] == 'success'
        assert len(search_result['results']) > 0
        
        # Test relation finding
        relation_result = await tool.execute(task, {
            'type': 'relate',
            'concept1': 'cognition',
            'concept2': 'memory'
        })
        
        assert relation_result['status'] == 'success'
        assert 'direct_relations' in relation_result

class TestOEISA000081Compliance:
    """Test cases for OEIS A000081 mathematical compliance."""
    
    def test_tool_selection_enumeration(self):
        """Test that tool selection follows A000081 enumeration limits."""
        # A000081 sequence: 1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842, 4766, 12486, ...
        # We use A000081[4] = 9 as the maximum tool selection limit
        
        manager = ToolIntegrationManager()
        
        # Create many tools
        for i in range(20):
            tool = CognitiveTool(
                tool_id=f"tool_{i:02d}",
                tool_type=ToolType.COMPUTATION,
                name=f"Tool {i}",
                description=f"Test tool {i}",
                capabilities=['test'],
                interface={},
                availability=1.0,
                reliability=1.0
            )
            interface = Mock()
            manager.register_tool(tool, interface)
        
        task = CognitiveTask(
            task_id="oeis_test",
            task_type=CognitiveTaskType.PROBLEM_SOLVING,  # Use existing task type
            description="Test OEIS compliance",
            parameters={},
            required_capabilities=['test']
        )
        
        selected_tools = manager.identify_tools(task)
        
        # Verify A000081 compliance
        assert len(selected_tools) <= 9, f"Tool selection violates A000081[4] = 9 limit: {len(selected_tools)}"
    
    def test_resource_membrane_hierarchy(self):
        """Test that resource allocation follows A000081 membrane hierarchy."""
        engine = ResourceCouplingEngine()
        
        # Verify P-System membrane count follows A000081
        if hasattr(engine, 'resource_psystem'):
            # A000081[3] = 4 membranes for resource categories
            assert engine.resource_psystem.max_membranes == 4
    
    def test_neural_network_sizing(self):
        """Test that neural network components follow A000081 sizing."""
        manager = ToolIntegrationManager()
        
        if hasattr(manager, 'tool_selection_esn'):
            # ESN reservoir should follow A000081[6] = 48
            assert manager.tool_selection_esn.reservoir_size == 48
            
            # Output size should follow A000081[4] = 9
            assert manager.tool_selection_esn.output_size == 9

class TestRealTimePerformance:
    """Test cases for real-time performance constraints."""
    
    @pytest.mark.asyncio
    async def test_tool_execution_latency(self):
        """Test that tool execution meets latency requirements."""
        tools = create_default_cognitive_tools()
        
        for tool_spec, tool_interface in tools:
            task = CognitiveTask(
                task_id="latency_test",
                task_type=CognitiveTaskType.COMPUTATION,
                description="Test latency",
                parameters={'operation': 'test'}
            )
            
            start_time = time.time()
            
            try:
                await tool_interface.execute(task, {'operation': 'test'})
                latency = time.time() - start_time
                
                # Verify latency is within specified bounds
                assert latency <= tool_spec.latency * 2, f"Tool {tool_spec.name} exceeded latency: {latency:.3f}s > {tool_spec.latency * 2:.3f}s"
            except (ValueError, KeyError):
                # Some tools may not support 'test' operation
                pass
    
    @pytest.mark.asyncio
    async def test_memory_consolidation_timing(self):
        """Test memory consolidation meets timing requirements."""
        # From architecture docs: Memory Consolidation ≤ 100ms
        tool = MemoryStoreTool()
        
        task = CognitiveTask("test", CognitiveTaskType.MEMORY_RETRIEVAL, "Test", {})
        
        # Test multiple operations for statistical significance
        times = []
        for i in range(10):
            start_time = time.time()
            
            await tool.execute(task, {
                'operation': 'store',
                'key': f'test_key_{i}',
                'value': f'test_value_{i}'
            })
            
            consolidation_time = time.time() - start_time
            times.append(consolidation_time)
        
        avg_time = np.mean(times)
        max_time = np.max(times)
        
        # Verify timing constraints
        assert avg_time <= 0.1, f"Average consolidation time {avg_time:.3f}s exceeds 100ms limit"
        assert max_time <= 0.15, f"Maximum consolidation time {max_time:.3f}s exceeds reasonable bounds"

# Utility functions for testing
def create_test_task(task_type: CognitiveTaskType = CognitiveTaskType.PROBLEM_SOLVING,
                    capabilities: List[str] = None) -> CognitiveTask:
    """Create a test cognitive task."""
    if capabilities is None:
        capabilities = ['general']
    
    return CognitiveTask(
        task_id=f"test_task_{int(time.time())}",
        task_type=task_type,
        description=f"Test task for {task_type.value}",
        parameters={},
        required_capabilities=capabilities
    )

def create_test_resource(resource_type: ResourceType = ResourceType.COMPUTATIONAL) -> EnvironmentalResource:
    """Create a test environmental resource."""
    return EnvironmentalResource(
        resource_id=f"test_resource_{int(time.time())}",
        resource_type=resource_type,
        name=f"Test {resource_type.value} Resource",
        capacity=100.0,
        available_capacity=80.0,
        access_time=0.01,
        quality=0.9
    )

if __name__ == "__main__":
    # Run tests if executed directly
    pytest.main([__file__, "-v"])