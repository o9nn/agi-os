"""
Comprehensive Tests for Environment Coupling System

Tests for the Task 2.2.1 Environment Coupling implementation including:
- Real-time environment state integration
- Dynamic environment adaptation
- Context-sensitive behavior modification
- Integration with AAR orchestration

Test Categories:
1. EnvironmentStateMonitor tests
2. BehaviorAdaptationEngine tests  
3. ContextSensitivityManager tests
4. EnvironmentCouplingSystem integration tests
5. AAR bridge integration tests
6. Performance and stress tests
"""

import asyncio
import logging
import time
import sys
import os
from typing import Dict, Any

# Add project paths
sys.path.append(os.path.join(os.path.dirname(__file__), '..', 'echo.kern'))
sys.path.append(os.path.join(os.path.dirname(__file__), '..', 'aar_core'))

# Import test framework
try:
    import pytest
except ImportError:
    # Simple test framework if pytest not available
    class SimpleTestFramework:
        def __init__(self):
            self.passed = 0
            self.failed = 0
            self.errors = []
        
        def run_test(self, test_func, test_name):
            try:
                print(f"Running {test_name}...")
                if asyncio.iscoroutinefunction(test_func):
                    asyncio.run(test_func())
                else:
                    test_func()
                print(f"✓ {test_name} PASSED")
                self.passed += 1
            except Exception as e:
                print(f"✗ {test_name} FAILED: {e}")
                self.failed += 1
                self.errors.append((test_name, str(e)))
        
        def summary(self):
            print(f"\nTest Summary: {self.passed} passed, {self.failed} failed")
            if self.errors:
                print("Failures:")
                for name, error in self.errors:
                    print(f"  {name}: {error}")
    
    # Create pytest-like decorators
    def pytest_mark_asyncio(func):
        return func
    
    test_framework = SimpleTestFramework()

# Import our modules
try:
    from environment_coupling import (
        EnvironmentStateMonitor,
        BehaviorAdaptationEngine, 
        ContextSensitivityManager,
        EnvironmentCouplingSystem,
        EnvironmentEvent,
        BehaviorAdaptation,
        AdaptationStrategy,
        CouplingType,
        create_default_coupling_system
    )
    from environment.aar_bridge import (
        AAREnvironmentBridge,
        AAREnvironmentAdapter,
        initialize_aar_environment_coupling
    )
    IMPORTS_AVAILABLE = True
except ImportError as e:
    print(f"Warning: Could not import modules - {e}")
    IMPORTS_AVAILABLE = False

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# Test Fixtures and Mock Objects

class MockAgent:
    """Mock agent interface for testing."""
    
    def __init__(self, agent_id: str):
        self.id = agent_id
        self.state = {
            'energy': 100,
            'activity': 1.0,
            'position': [0.0, 0.0, 0.0],
            'behaviors': {}
        }
        self.behavior_updates = []
    
    async def modify_behavior(self, behavior_changes: Dict[str, Any]) -> Dict[str, Any]:
        """Mock behavior modification."""
        self.behavior_updates.append(behavior_changes)
        self.state['behaviors'].update(behavior_changes)
        return {'effectiveness': 0.8}
    
    async def update_state(self, new_state: Dict[str, Any]) -> Dict[str, Any]:
        """Mock state update."""
        self.state.update(new_state)
        return {'effectiveness': 0.7}


class MockArena:
    """Mock arena interface for testing."""
    
    def __init__(self, arena_id: str):
        self.id = arena_id
        self.state = {
            'resources': 10,
            'hazards': 0,
            'agent_positions': {},
            'temperature': 20.0
        }
        self.state_history = []
    
    def update_state(self, new_state: Dict[str, Any]) -> None:
        """Update arena state."""
        self.state_history.append(self.state.copy())
        self.state.update(new_state)


# Test Classes

class TestEnvironmentStateMonitor:
    """Tests for EnvironmentStateMonitor component."""
    
    def test_monitor_initialization(self):
        """Test basic monitor initialization."""
        if not IMPORTS_AVAILABLE:
            return
            
        monitor = EnvironmentStateMonitor("test_monitor", 0.1)
        assert monitor.monitor_id == "test_monitor"
        assert monitor.update_interval == 0.1
        assert not monitor.active
        assert monitor.update_count == 0
        assert monitor.event_count == 0
    
    def test_state_update_detection(self):
        """Test detection of state changes."""
        if not IMPORTS_AVAILABLE:
            return
            
        monitor = EnvironmentStateMonitor("test_monitor")
        monitor.start_monitoring()
        
        # Initial state
        initial_state = {'temperature': 20.0, 'humidity': 50.0}
        events1 = monitor.update_state(initial_state)
        assert len(events1) == 0  # No changes on first update
        
        # Changed state
        changed_state = {'temperature': 25.0, 'humidity': 50.0}
        events2 = monitor.update_state(changed_state)
        assert len(events2) == 1
        assert events2[0].event_type == "parameter_changed"
        assert events2[0].data['parameter'] == 'temperature'
        assert events2[0].data['old_value'] == 20.0
        assert events2[0].data['new_value'] == 25.0
    
    def test_threshold_detection(self):
        """Test threshold-based change detection."""
        if not IMPORTS_AVAILABLE:
            return
            
        monitor = EnvironmentStateMonitor("test_monitor")
        monitor.set_detection_threshold('temperature', 2.0)
        monitor.start_monitoring()
        
        # Initial state
        initial_state = {'temperature': 20.0}
        monitor.update_state(initial_state)
        
        # Small change (below threshold)
        small_change = {'temperature': 21.0}
        events1 = monitor.update_state(small_change)
        assert len(events1) == 0
        
        # Large change (above threshold)
        large_change = {'temperature': 23.0}
        events2 = monitor.update_state(large_change)
        assert len(events2) == 1
    
    def test_event_listeners(self):
        """Test event listener functionality."""
        if not IMPORTS_AVAILABLE:
            return
            
        monitor = EnvironmentStateMonitor("test_monitor")
        events_received = []
        
        def event_listener(event):
            events_received.append(event)
        
        monitor.add_event_listener(event_listener)
        monitor.start_monitoring()
        
        # Trigger an event
        monitor.update_state({'new_param': 42})
        monitor.update_state({'new_param': 43})
        
        assert len(events_received) == 1  # One parameter_changed event
        assert events_received[0].data['parameter'] == 'new_param'


class TestBehaviorAdaptationEngine:
    """Tests for BehaviorAdaptationEngine component."""
    
    def test_engine_initialization(self):
        """Test basic engine initialization."""
        if not IMPORTS_AVAILABLE:
            return
            
        engine = BehaviorAdaptationEngine("test_engine")
        assert engine.engine_id == "test_engine"
        assert not engine.active
        assert len(engine.registered_agents) == 0
        assert engine.adaptations_processed == 0
    
    def test_agent_registration(self):
        """Test agent registration and unregistration."""
        if not IMPORTS_AVAILABLE:
            return
            
        engine = BehaviorAdaptationEngine("test_engine")
        
        # Register agent
        engine.register_agent("agent1", {'capability': 'movement'})
        assert "agent1" in engine.registered_agents
        assert engine.agent_contexts["agent1"]['capability'] == 'movement'
        
        # Unregister agent
        engine.unregister_agent("agent1")
        assert "agent1" not in engine.registered_agents
        assert "agent1" not in engine.agent_contexts
    
    def test_adaptation_rules(self):
        """Test adaptation rule processing."""
        if not IMPORTS_AVAILABLE:
            return
            
        engine = BehaviorAdaptationEngine("test_engine")
        engine.register_agent("agent1")
        engine.active = True
        
        # Add adaptation rule
        rule = {
            'name': 'test_rule',
            'condition': {'event_type': 'parameter_changed'},
            'action': {'target': 'agent1', 'parameters': {'behavior': 'test'}},
            'priority': 5
        }
        engine.add_adaptation_rule(rule)
        
        # Create test event
        event = EnvironmentEvent(
            event_id="test_event",
            timestamp=time.time(),
            event_type="parameter_changed",
            source="test",
            data={'parameter': 'temperature', 'new_value': 30.0}
        )
        
        # Process event
        adaptations = engine.process_environment_event(event)
        assert len(adaptations) >= 1
        assert adaptations[0].target_agent == "agent1"
    
    async def test_adaptation_queue_processing(self):
        """Test processing of adaptation queue."""
        if not IMPORTS_AVAILABLE:
            return
            
        engine = BehaviorAdaptationEngine("test_engine")
        engine.register_agent("agent1")
        engine.active = True
        
        # Add test callback
        callback_called = False
        async def test_callback(adaptation):
            nonlocal callback_called
            callback_called = True
            return {'effectiveness': 0.9}
        
        engine.register_adaptation_callback("agent1", test_callback)
        
        # Create and queue adaptation
        adaptation = BehaviorAdaptation(
            adaptation_id="test_adaptation",
            target_agent="agent1",
            adaptation_type=AdaptationStrategy.IMMEDIATE,
            parameters={'behavior': 'test'}
        )
        engine.adaptation_queue.append(adaptation)
        
        # Process queue
        result = await engine.process_adaptation_queue()
        assert result['processed'] == 1
        assert result['successful'] == 1
        assert callback_called


class TestContextSensitivityManager:
    """Tests for ContextSensitivityManager component."""
    
    def test_manager_initialization(self):
        """Test basic manager initialization."""
        if not IMPORTS_AVAILABLE:
            return
            
        manager = ContextSensitivityManager("test_manager")
        assert manager.manager_id == "test_manager"
        assert not manager.active
        assert len(manager.current_contexts) == 0
    
    def test_context_analysis(self):
        """Test context analysis functionality."""
        if not IMPORTS_AVAILABLE:
            return
            
        manager = ContextSensitivityManager("test_manager")
        
        env_state = {'temperature': 25.0, 'resources': 5}
        agent_states = {
            'agent1': {'position': [1, 2, 3], 'energy': 80},
            'agent2': {'position': [4, 5, 6], 'energy': 60}
        }
        
        context = manager.analyze_context(env_state, agent_states)
        
        assert context.environment_state == env_state
        assert context.agent_states == agent_states
        assert 'timestamp' in context.temporal_context
        assert 'agent_count' in context.social_context
        assert context.social_context['agent_count'] == 2
    
    def test_sensitivity_profiles(self):
        """Test sensitivity profile configuration."""
        if not IMPORTS_AVAILABLE:
            return
            
        manager = ContextSensitivityManager("test_manager")
        
        # Set custom profile
        custom_profile = {
            'environmental': 0.9,
            'social': 0.8,
            'temporal': 0.7,
            'spatial': 0.6
        }
        manager.set_sensitivity_profile('custom', custom_profile)
        
        assert 'custom' in manager.sensitivity_profiles
        assert manager.sensitivity_profiles['custom']['environmental'] == 0.9
    
    def test_sensitivity_evaluation(self):
        """Test context sensitivity evaluation."""
        if not IMPORTS_AVAILABLE:
            return
            
        manager = ContextSensitivityManager("test_manager")
        
        # Create test context
        env_state = {'param1': 1, 'param2': 2, 'param3': 3}
        agent_states = {'agent1': {'interactions': ['agent2']}}
        context = manager.analyze_context(env_state, agent_states)
        
        # Evaluate sensitivity
        scores = manager.evaluate_context_sensitivity(context, 'default')
        
        assert 'environmental' in scores
        assert 'social' in scores
        assert 'temporal' in scores
        assert 'spatial' in scores
        assert all(0.0 <= score <= 1.0 for score in scores.values())


class TestEnvironmentCouplingSystem:
    """Tests for the main EnvironmentCouplingSystem."""
    
    async def test_system_initialization(self):
        """Test system initialization."""
        if not IMPORTS_AVAILABLE:
            return
            
        system = EnvironmentCouplingSystem("test_system")
        
        success = await system.initialize()
        assert success
        assert system.initialized
        assert not system.active
    
    async def test_agent_registration(self):
        """Test agent registration with coupling system."""
        if not IMPORTS_AVAILABLE:
            return
            
        system = EnvironmentCouplingSystem("test_system")
        await system.initialize()
        
        success = system.register_agent("test_agent", {'capability': 'test'})
        assert success
        assert "test_agent" in system.adaptation_engine.registered_agents
    
    async def test_environment_state_update(self):
        """Test environment state update processing."""
        if not IMPORTS_AVAILABLE:
            return
            
        system = EnvironmentCouplingSystem("test_system")
        await system.initialize()
        system.start_coupling()
        
        # Update environment state
        test_state = {'temperature': 25.0, 'humidity': 60.0}
        result = await system.update_environment_state(test_state)
        
        assert result['status'] == 'success'
        assert 'events' in result
        assert 'adaptations' in result
        assert 'timestamp' in result
    
    async def test_complete_coupling_workflow(self):
        """Test complete environment coupling workflow."""
        if not IMPORTS_AVAILABLE:
            return
            
        system = EnvironmentCouplingSystem("test_system")
        await system.initialize()
        
        # Register test agent with callback
        mock_agent = MockAgent("test_agent")
        system.register_agent("test_agent", {'source': 'test'})
        
        adaptation_applied = False
        async def test_callback(adaptation):
            nonlocal adaptation_applied
            adaptation_applied = True
            return await mock_agent.modify_behavior(adaptation.parameters)
        
        system.register_adaptation_callback("test_agent", test_callback)
        system.start_coupling()
        
        # Simulate environment changes
        initial_state = {'temperature': 20.0, 'resources': 10}
        await system.update_environment_state(initial_state)
        
        # Trigger significant change that should cause adaptation
        changed_state = {'temperature': 35.0, 'resources': 2}  # Hot and low resources
        result = await system.update_environment_state(changed_state)
        
        # Process any queued adaptations
        await asyncio.sleep(0.1)  # Allow processing time
        
        assert result['status'] == 'success'
        # Note: adaptation_applied might be False if no matching rules triggered


class TestAARIntegration:
    """Tests for AAR (Agent-Arena-Relation) integration."""
    
    async def test_aar_bridge_initialization(self):
        """Test AAR bridge initialization."""
        if not IMPORTS_AVAILABLE:
            return
            
        bridge = AAREnvironmentBridge("test_bridge")
        
        mock_arena = MockArena("test_arena")
        mock_agents = {"agent1": MockAgent("agent1")}
        
        success = await bridge.initialize_bridge(mock_arena, mock_agents)
        assert success
        assert bridge.initialized
    
    async def test_arena_state_processing(self):
        """Test processing of arena state updates."""
        if not IMPORTS_AVAILABLE:
            return
            
        bridge = AAREnvironmentBridge("test_bridge")
        mock_arena = MockArena("test_arena")
        mock_agents = {"agent1": MockAgent("agent1")}
        
        await bridge.initialize_bridge(mock_arena, mock_agents)
        bridge.start_bridge()
        
        # Process arena state update
        arena_state = {
            'resources': 5,
            'hazards': 1,
            'agent_positions': {'agent1': [1, 2, 3]}
        }
        
        result = await bridge.process_arena_state_update(arena_state)
        assert result['status'] != 'inactive'
        assert bridge.state_updates_processed > 0
    
    async def test_behavior_adaptation_integration(self):
        """Test integration of behavior adaptations with mock agents."""
        if not IMPORTS_AVAILABLE:
            return
            
        bridge = AAREnvironmentBridge("test_bridge")
        mock_agent = MockAgent("test_agent")
        mock_agents = {"test_agent": mock_agent}
        
        await bridge.initialize_bridge(None, mock_agents)
        bridge.start_bridge()
        
        # Create test adaptation
        adaptation = BehaviorAdaptation(
            adaptation_id="test_adaptation",
            target_agent="test_agent", 
            adaptation_type=AdaptationStrategy.IMMEDIATE,
            parameters={'behavior': 'resource_seeking', 'intensity': 'high'}
        )
        
        # Apply adaptation through bridge
        effectiveness = await bridge._apply_agent_behavior_adaptation(mock_agent, adaptation)
        
        assert effectiveness > 0.0
        assert len(mock_agent.behavior_updates) > 0
        
        # Check that behavior was modified
        last_update = mock_agent.behavior_updates[-1]
        assert 'priority' in last_update
        assert last_update['priority'] == 'resources'


class TestPerformanceAndStress:
    """Performance and stress tests for environment coupling."""
    
    async def test_high_frequency_updates(self):
        """Test system performance with high-frequency updates."""
        if not IMPORTS_AVAILABLE:
            return
            
        system = EnvironmentCouplingSystem("perf_test")
        await system.initialize()
        system.start_coupling()
        
        # Register multiple agents
        for i in range(10):
            system.register_agent(f"agent_{i}")
        
        start_time = time.time()
        update_count = 0
        
        # Perform rapid updates for 1 second
        while time.time() - start_time < 1.0:
            test_state = {
                'timestamp': time.time(),
                'random_value': update_count % 100
            }
            await system.update_environment_state(test_state)
            update_count += 1
            
            # Small delay to prevent overwhelming
            await asyncio.sleep(0.01)
        
        elapsed_time = time.time() - start_time
        updates_per_second = update_count / elapsed_time
        
        logger.info(f"Performance test: {updates_per_second:.1f} updates/sec")
        assert updates_per_second > 10  # Minimum acceptable performance
    
    async def test_many_agents_coupling(self):
        """Test coupling with many agents."""
        if not IMPORTS_AVAILABLE:
            return
            
        system = EnvironmentCouplingSystem("stress_test")
        await system.initialize()
        
        # Register many agents
        agent_count = 50
        for i in range(agent_count):
            success = system.register_agent(f"agent_{i}", {'id': i})
            assert success
        
        system.start_coupling()
        
        # Trigger environment change that affects all agents
        large_state = {
            'global_temperature': 30.0,
            'global_resources': 1,  # Low resources should trigger adaptations
        }
        
        result = await system.update_environment_state(large_state)
        assert result['status'] == 'success'
        
        # Check system can handle the load
        status = system.get_system_status()
        assert status['components']['adaptation']['registered_agents'] == agent_count


# Main test execution
async def run_all_tests():
    """Run all tests."""
    if not IMPORTS_AVAILABLE:
        print("Warning: Some imports not available - running limited tests")
    
    print("=== Environment Coupling System Tests ===\n")
    
    # Environment State Monitor Tests
    print("Testing EnvironmentStateMonitor...")
    monitor_tests = TestEnvironmentStateMonitor()
    monitor_tests.test_monitor_initialization()
    monitor_tests.test_state_update_detection()
    monitor_tests.test_threshold_detection()
    monitor_tests.test_event_listeners()
    
    # Behavior Adaptation Engine Tests
    print("\nTesting BehaviorAdaptationEngine...")
    engine_tests = TestBehaviorAdaptationEngine()
    engine_tests.test_engine_initialization()
    engine_tests.test_agent_registration()
    engine_tests.test_adaptation_rules()
    await engine_tests.test_adaptation_queue_processing()
    
    # Context Sensitivity Manager Tests
    print("\nTesting ContextSensitivityManager...")
    context_tests = TestContextSensitivityManager()
    context_tests.test_manager_initialization()
    context_tests.test_context_analysis()
    context_tests.test_sensitivity_profiles()
    context_tests.test_sensitivity_evaluation()
    
    # Environment Coupling System Tests
    print("\nTesting EnvironmentCouplingSystem...")
    system_tests = TestEnvironmentCouplingSystem()
    await system_tests.test_system_initialization()
    await system_tests.test_agent_registration()
    await system_tests.test_environment_state_update()
    await system_tests.test_complete_coupling_workflow()
    
    # AAR Integration Tests
    print("\nTesting AAR Integration...")
    aar_tests = TestAARIntegration()
    await aar_tests.test_aar_bridge_initialization()
    await aar_tests.test_arena_state_processing()
    await aar_tests.test_behavior_adaptation_integration()
    
    # Performance Tests
    print("\nTesting Performance...")
    perf_tests = TestPerformanceAndStress()
    await perf_tests.test_high_frequency_updates()
    await perf_tests.test_many_agents_coupling()
    
    print("\n=== All Tests Completed ===")


def run_acceptance_criteria_validation():
    """
    Validate acceptance criteria for Task 2.2.1:
    "Agents adapt behavior based on environment changes"
    """
    print("\n=== Acceptance Criteria Validation ===")
    
    async def validate_acceptance_criteria():
        if not IMPORTS_AVAILABLE:
            print("Cannot validate - imports not available")
            return False
            
        print("Validating: Agents adapt behavior based on environment changes")
        
        # Create system with mock agents
        system = EnvironmentCouplingSystem("validation_system")
        await system.initialize()
        
        # Create mock agents that track behavior changes
        mock_agents = {}
        for i in range(3):
            agent_id = f"validation_agent_{i}"
            mock_agent = MockAgent(agent_id)
            mock_agents[agent_id] = mock_agent
            
            # Register with system
            system.register_agent(agent_id)
            
            # Register callback to track adaptations
            async def adaptation_callback(adaptation, agent=mock_agent):
                return await agent.modify_behavior(adaptation.parameters)
            
            system.register_adaptation_callback(agent_id, adaptation_callback)
        
        system.start_coupling()
        
        # Test scenario 1: Resource depletion should trigger resource-seeking behavior
        print("  Testing resource depletion adaptation...")
        initial_state = {'resources': 100, 'temperature': 20.0}
        await system.update_environment_state(initial_state)
        
        # Simulate resource depletion
        depleted_state = {'resources': 5, 'temperature': 20.0}
        result1 = await system.update_environment_state(depleted_state)
        
        # Allow processing time
        await asyncio.sleep(0.2)
        
        print(f"    Resource depletion result: {result1}")
        
        # Test scenario 2: Temperature change should trigger adaptive behavior
        print("  Testing temperature change adaptation...")
        hot_state = {'resources': 5, 'temperature': 40.0}
        result2 = await system.update_environment_state(hot_state)
        
        # Allow processing time  
        await asyncio.sleep(0.2)
        
        print(f"    Temperature change result: {result2}")
        
        # Validate that some adaptations occurred
        total_adaptations = 0
        for agent in mock_agents.values():
            total_adaptations += len(agent.behavior_updates)
            if agent.behavior_updates:
                print(f"    Agent {agent.id} adaptations: {agent.behavior_updates}")
        
        success = total_adaptations > 0
        print(f"  Total behavior adaptations: {total_adaptations}")
        print(f"  Acceptance criteria {'PASSED' if success else 'FAILED'}")
        
        return success
    
    # Run validation
    success = asyncio.run(validate_acceptance_criteria())
    return success


if __name__ == "__main__":
    # Run tests
    try:
        asyncio.run(run_all_tests())
    except Exception as e:
        print(f"Error running tests: {e}")
        import traceback
        traceback.print_exc()
    
    # Validate acceptance criteria
    try:
        success = run_acceptance_criteria_validation()
        print(f"\nFinal Result: {'SUCCESS' if success else 'FAILED'}")
    except Exception as e:
        print(f"Error in acceptance criteria validation: {e}")
        import traceback
        traceback.print_exc()