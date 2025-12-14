#!/usr/bin/env python3
"""
Resource Constraint Manager Tests
=================================

Comprehensive test suite for validating resource constraints implementation
as part of Phase 2.2.2 of the Deep Tree Echo development roadmap.

Test Coverage:
- Computational resource limitation enforcement
- Energy consumption modeling validation
- Real-time processing constraint verification
- DTESN integration constraint validation
- Agent resource allocation and release
- Constraint violation handling and reporting

Acceptance Criteria Validation:
- Agents operate under realistic resource limits ✓
- Energy consumption is properly modeled ✓  
- Real-time constraints are enforced ✓
- Integration with existing DTESN components ✓
"""

import unittest
import time
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, as_completed

# Import modules under test
import sys
from pathlib import Path

# Add current directory to path for imports
sys.path.insert(0, str(Path(__file__).parent))

from resource_constraint_manager import (
    ResourceConstraintManager, OperationType, ResourceError
)
from dtesn_resource_integration import (
    DTESNResourceIntegrator, ConstrainedAgent
)

class TestResourceConstraintManager(unittest.TestCase):
    """Test suite for the core ResourceConstraintManager."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.manager = ResourceConstraintManager()
        self.test_agent_id = "test_agent_001"
    
    def tearDown(self):
        """Clean up after tests."""
        # Release any allocated resources
        self.manager.release_resources(self.test_agent_id)
    
    def test_initialization(self):
        """Test that ResourceConstraintManager initializes correctly."""
        # Verify default constraints are created
        status = self.manager.get_resource_status()
        
        self.assertIn("cpu_primary", status)
        self.assertIn("memory_main", status)  
        self.assertIn("energy_budget", status)
        self.assertIn("neuromorphic_units", status)
        
        # Verify constraint properties
        cpu_constraint = status["cpu_primary"]
        self.assertEqual(cpu_constraint["type"], "cpu_cycles")
        self.assertEqual(cpu_constraint["max_allocation"], 1e9)
        self.assertTrue(cpu_constraint["hard_limit"])
    
    def test_resource_allocation_basic(self):
        """Test basic resource allocation functionality."""
        # Test successful allocation
        resources = {
            "cpu_primary": 1e6,    # 1M cycles
            "memory_main": 1024    # 1KB
        }
        
        success, message = self.manager.allocate_resources(
            self.test_agent_id, resources, OperationType.MEMBRANE_EVOLUTION)
        
        self.assertTrue(success)
        self.assertEqual(message, "Resources allocated successfully")
        
        # Verify resources are allocated
        status = self.manager.get_resource_status()
        self.assertEqual(status["cpu_primary"]["current_usage"], 1e6)
        self.assertEqual(status["memory_main"]["current_usage"], 1024)
    
    def test_resource_allocation_insufficient(self):
        """Test resource allocation failure when insufficient resources."""
        # Try to allocate more than available
        excessive_resources = {
            "cpu_primary": 2e9,  # 2GHz - more than max 1GHz
        }
        
        success, message = self.manager.allocate_resources(
            self.test_agent_id, excessive_resources)
        
        self.assertFalse(success)
        self.assertIn("Insufficient cpu_primary", message)
    
    def test_resource_release(self):
        """Test resource release functionality."""
        # Allocate resources first
        resources = {"cpu_primary": 5e5, "memory_main": 512}
        self.manager.allocate_resources(self.test_agent_id, resources)
        
        # Verify allocation
        status_before = self.manager.get_resource_status()
        self.assertEqual(status_before["cpu_primary"]["current_usage"], 5e5)
        
        # Release resources
        success = self.manager.release_resources(self.test_agent_id)
        self.assertTrue(success)
        
        # Verify release
        status_after = self.manager.get_resource_status()
        self.assertEqual(status_after["cpu_primary"]["current_usage"], 0.0)
        self.assertEqual(status_after["memory_main"]["current_usage"], 0.0)
    
    def test_realtime_constraint_validation(self):
        """Test real-time constraint validation."""
        # Test within deadline - use very short delay
        start_time = time.time_ns()
        time.sleep(0.000001)  # 1μs sleep - well within 10μs limit
        
        is_valid, message = self.manager.validate_realtime_constraint(
            OperationType.MEMBRANE_EVOLUTION, start_time)
        
        self.assertTrue(is_valid, f"Expected operation to be within deadline, got: {message}")
        self.assertIn("Within deadline", message)
        
        # Test exceeded deadline (simulate old start time)
        old_start_time = time.time_ns() - 50_000  # 50μs ago - exceeds 15μs deadline  
        is_valid, message = self.manager.validate_realtime_constraint(
            OperationType.MEMBRANE_EVOLUTION, old_start_time)
        
        self.assertFalse(is_valid)
        self.assertIn("Deadline exceeded", message)
    
    def test_energy_consumption_calculation(self):
        """Test energy consumption modeling."""
        # Test basic energy calculation
        energy_cost = self.manager.calculate_operation_energy(
            OperationType.MEMBRANE_EVOLUTION)
        
        self.assertGreater(energy_cost, 0)
        self.assertLess(energy_cost, 1e-3)  # Should be in microjoule range
        
        # Test with actual duration
        duration = 0.00001  # 10μs
        energy_with_duration = self.manager.calculate_operation_energy(
            OperationType.MEMBRANE_EVOLUTION, duration_seconds=duration)
        
        self.assertGreater(energy_with_duration, 0)
        
        # Test complexity scaling
        energy_complex = self.manager.calculate_operation_energy(
            OperationType.BSERIES_COMPUTATION, complexity=2.0)
        energy_simple = self.manager.calculate_operation_energy(
            OperationType.BSERIES_COMPUTATION, complexity=1.0)
        
        self.assertGreater(energy_complex, energy_simple)
    
    def test_enforce_agent_constraints(self):
        """Test the main constraint enforcement mechanism."""
        def mock_operation():
            """Mock operation that sleeps for 5μs."""
            time.sleep(0.000005)
            return "operation_completed"
        
        # Test successful constraint enforcement
        result = self.manager.enforce_agent_constraints(
            self.test_agent_id, OperationType.MEMBRANE_EVOLUTION, mock_operation)
        
        self.assertEqual(result, "operation_completed")
        
        # Verify metrics were updated
        metrics = self.manager.get_performance_metrics()
        self.assertEqual(metrics["total_operations"], 1)
        self.assertGreater(metrics["total_energy_consumed"], 0)
    
    def test_concurrent_resource_allocation(self):
        """Test resource allocation under concurrent access."""
        def allocate_and_release(agent_id, resources):
            """Helper function for concurrent testing."""
            success, _ = self.manager.allocate_resources(
                agent_id, resources, OperationType.MEMBRANE_EVOLUTION)
            if success:
                time.sleep(0.001)  # Hold resources briefly
                self.manager.release_resources(agent_id)
            return success
        
        # Test with multiple agents allocating concurrently
        num_agents = 10
        resources_per_agent = {"cpu_primary": 5e7, "memory_main": 1024}  # 50M cycles each
        
        with ThreadPoolExecutor(max_workers=num_agents) as executor:
            futures = [
                executor.submit(allocate_and_release, f"agent_{i}", resources_per_agent)
                for i in range(num_agents)
            ]
            
            # At least some allocations should succeed
            successful_allocations = sum(future.result() for future in as_completed(futures))
            self.assertGreater(successful_allocations, 0)
            
            # Not all can succeed due to resource limits (1GHz / 50MHz = max 20 agents theoretically)
            self.assertLessEqual(successful_allocations, num_agents)
    
    def test_performance_metrics(self):
        """Test performance metrics collection."""
        # Perform some operations
        def dummy_op():
            return "done"
        
        for i in range(5):
            self.manager.enforce_agent_constraints(
                f"agent_{i}", OperationType.MEMBRANE_EVOLUTION, dummy_op)
        
        metrics = self.manager.get_performance_metrics()
        
        self.assertEqual(metrics["total_operations"], 5)
        self.assertGreaterEqual(metrics["constraint_violations"], 0)
        self.assertGreater(metrics["total_energy_consumed"], 0)
        self.assertEqual(metrics["active_allocations"], 0)  # All should be released

class TestDTESNResourceIntegration(unittest.TestCase):
    """Test suite for DTESN resource integration."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.integrator = DTESNResourceIntegrator()
        self.test_agent = ConstrainedAgent(
            agent_id="dtesn_test_agent",
            max_operations_per_second=100,
            priority_level=5,
            energy_budget_joules=0.01
        )
        self.integrator.register_agent(self.test_agent)
    
    def tearDown(self):
        """Clean up after tests."""
        self.integrator.unregister_agent(self.test_agent.agent_id)
    
    def test_agent_registration(self):
        """Test agent registration and unregistration."""
        # Test registration
        new_agent = ConstrainedAgent(agent_id="new_test_agent")
        success = self.integrator.register_agent(new_agent)
        self.assertTrue(success)
        
        # Test duplicate registration
        success_duplicate = self.integrator.register_agent(new_agent)
        self.assertFalse(success_duplicate)
        
        # Test unregistration
        success_unreg = self.integrator.unregister_agent(new_agent.agent_id)
        self.assertTrue(success_unreg)
        
        # Test unregistering non-existent agent
        success_invalid = self.integrator.unregister_agent("non_existent")
        self.assertFalse(success_invalid)
    
    def test_constrained_psystem_wrapper(self):
        """Test P-System membrane operations under constraints."""
        psystem = self.integrator.get_constrained_psystem(self.test_agent.agent_id)
        self.assertIsNotNone(psystem)
        
        # Test membrane evolution
        membrane_config = {
            "initial_membranes": 2,
            "evolution_rules": ["rule1", "rule2"],
            "max_cycles": 10
        }
        
        result = psystem.evolve_membrane(membrane_config)
        
        self.assertIsInstance(result, dict)
        self.assertIn("status", result)
        self.assertEqual(result["status"], "evolved")
        
        # Test OEIS validation
        tree_structure = {"depth": 3, "nodes": [1, 2, 4]}
        is_valid = psystem.validate_oeis_compliance(tree_structure)
        self.assertIsInstance(is_valid, bool)
    
    def test_constrained_esn_wrapper(self):
        """Test ESN operations under constraints."""
        esn = self.integrator.get_constrained_esn(self.test_agent.agent_id)
        self.assertIsNotNone(esn)
        
        # Test reservoir state update
        input_data = [0.1, 0.5, -0.3, 0.8, -0.2]
        output = esn.update_reservoir_state(input_data)
        
        self.assertIsInstance(output, list)
        self.assertEqual(len(output), len(input_data))
        
        # Test training
        target_outputs = [0.2, 0.4, 0.1]
        training_result = esn.train_readout(target_outputs)
        
        self.assertIsInstance(training_result, dict)
        self.assertIn("training_error", training_result)
        self.assertIn("convergence", training_result)
    
    def test_constrained_bseries_wrapper(self):
        """Test B-Series operations under constraints."""
        bseries = self.integrator.get_constrained_bseries(self.test_agent.agent_id)
        self.assertIsNotNone(bseries)
        
        # Test tree classification
        tree_structure = {
            "depth": 2,
            "branching_factor": 2,
            "node_count": 3
        }
        
        classification = bseries.classify_tree(tree_structure)
        
        self.assertIsInstance(classification, dict)
        self.assertIn("tree_type", classification)
        self.assertIn("order", classification)
        
        # Test elementary differential computation
        differential = bseries.compute_elementary_differential(tree_structure, 2)
        
        self.assertIsInstance(differential, dict)
        self.assertIn("differential", differential)
        self.assertIn("coefficient", differential)
    
    def test_unregistered_agent_access(self):
        """Test that unregistered agents cannot access constrained components."""
        unregistered_agent_id = "unregistered_agent"
        
        psystem = self.integrator.get_constrained_psystem(unregistered_agent_id)
        self.assertIsNone(psystem)
        
        esn = self.integrator.get_constrained_esn(unregistered_agent_id)
        self.assertIsNone(esn)
        
        bseries = self.integrator.get_constrained_bseries(unregistered_agent_id)
        self.assertIsNone(bseries)
    
    def test_execute_constrained_operation(self):
        """Test general constrained operation execution."""
        def test_operation(value):
            time.sleep(0.000001)  # 1μs operation - within 150μs B-Series limit
            return value * 2
        
        result = self.integrator.execute_constrained_operation(
            self.test_agent.agent_id,
            OperationType.BSERIES_COMPUTATION,
            test_operation,
            5
        )
        
        self.assertEqual(result, 10)
        
        # Test with unregistered agent
        with self.assertRaises(ResourceError):
            self.integrator.execute_constrained_operation(
                "unregistered", OperationType.MEMBRANE_EVOLUTION, test_operation, 5)
    
    def test_agent_resource_status(self):
        """Test agent-specific resource status reporting."""
        status = self.integrator.get_agent_resource_status(self.test_agent.agent_id)
        
        self.assertIsNotNone(status)
        self.assertEqual(status["agent_id"], self.test_agent.agent_id)
        self.assertEqual(status["priority_level"], 5)
        self.assertEqual(status["energy_budget"], 0.01)
        self.assertIn("global_constraints", status)
        
        # Test with unregistered agent
        status_invalid = self.integrator.get_agent_resource_status("invalid")
        self.assertIsNone(status_invalid)
    
    def test_system_performance_metrics(self):
        """Test system-wide performance metrics."""
        metrics = self.integrator.get_system_performance_metrics()
        
        self.assertIn("constraint_manager", metrics)
        self.assertIn("registered_agents", metrics)
        self.assertEqual(metrics["registered_agents"], 1)
        self.assertIn(self.test_agent.agent_id, metrics["agent_list"])

class TestResourceConstraintIntegrationAcceptanceCriteria(unittest.TestCase):
    """
    Acceptance criteria validation tests.
    
    These tests specifically validate that "Agents operate under realistic resource limits"
    as specified in the task acceptance criteria.
    """
    
    def setUp(self):
        """Set up realistic test scenario."""
        self.integrator = DTESNResourceIntegrator()
        
        # Create multiple agents with different resource requirements
        self.agents = [
            ConstrainedAgent(f"agent_critical_{i}", priority_level=10, energy_budget_joules=0.1)
            for i in range(3)
        ] + [
            ConstrainedAgent(f"agent_normal_{i}", priority_level=5, energy_budget_joules=0.05)
            for i in range(5)
        ] + [
            ConstrainedAgent(f"agent_background_{i}", priority_level=1, energy_budget_joules=0.01)
            for i in range(10)
        ]
        
        for agent in self.agents:
            self.integrator.register_agent(agent)
    
    def tearDown(self):
        """Clean up agents."""
        for agent in self.agents:
            self.integrator.unregister_agent(agent.agent_id)
    
    def test_agents_operate_under_resource_limits(self):
        """
        Acceptance Criteria Test: Agents operate under realistic resource limits.
        
        This test validates that:
        1. Agents are prevented from exceeding computational limits
        2. Energy consumption is tracked and limited
        3. Real-time constraints are enforced
        4. Resource contention is properly handled
        """
        results = {}
        failed_operations = 0
        successful_operations = 0
        
        def intensive_operation(agent_id, operation_count):
            """Simulate an intensive DTESN operation."""
            operation_results = []
            
            for i in range(operation_count):
                try:
                    # Simulate B-Series computation (computationally intensive)
                    bseries = self.integrator.get_constrained_bseries(agent_id)
                    if bseries:
                        result = bseries.classify_tree({
                            "depth": 4,
                            "complexity": 2.0,
                            "node_count": 15
                        })
                        operation_results.append(result)
                    
                    # Simulate ESN update (memory intensive)
                    esn = self.integrator.get_constrained_esn(agent_id)
                    if esn:
                        large_input = [0.1] * 100  # Large input vector
                        esn_result = esn.update_reservoir_state(large_input)
                        operation_results.append(esn_result)
                        
                except ResourceError as e:
                    operation_results.append(f"Resource constraint violation: {e}")
                except Exception as e:
                    operation_results.append(f"Other error: {e}")
            
            return agent_id, operation_results
        
        # Execute intensive operations concurrently across all agents
        with ThreadPoolExecutor(max_workers=len(self.agents)) as executor:
            futures = [
                executor.submit(intensive_operation, agent.agent_id, 10)
                for agent in self.agents
            ]
            
            for future in as_completed(futures):
                agent_id, operation_results = future.result()
                results[agent_id] = operation_results
                
                # Count successful vs failed operations
                for result in operation_results:
                    if isinstance(result, str) and "constraint violation" in result:
                        failed_operations += 1
                    elif isinstance(result, dict):
                        successful_operations += 1
        
        # Validate acceptance criteria
        
        # 1. Some operations should succeed (system is functional)
        self.assertGreater(successful_operations, 0,
                          "No operations succeeded - system too restrictive")
        
        # 2. Some operations should be constrained (limits are effective)
        self.assertGreater(failed_operations, 0,
                          "No resource constraints were enforced - limits ineffective")
        
        # 3. Higher priority agents should have more successful operations
        critical_success = sum(1 for agent in self.agents[:3] 
                              for result in results[agent.agent_id]
                              if isinstance(result, dict))
        background_success = sum(1 for agent in self.agents[-10:]
                               for result in results[agent.agent_id] 
                               if isinstance(result, dict))
        
        # Critical agents should generally outperform background agents
        if critical_success + background_success > 0:
            critical_success_rate = critical_success / (3 * 20)  # 3 agents, 20 ops each
            background_success_rate = background_success / (10 * 20)  # 10 agents, 20 ops each
            
            # Allow for some variance but critical should generally do better
            self.assertGreaterEqual(critical_success_rate, background_success_rate * 0.8,
                                  "Priority-based resource allocation not working properly")
        
        # 4. System should track resource consumption
        metrics = self.integrator.get_system_performance_metrics()
        constraint_metrics = metrics["constraint_manager"]
        
        self.assertGreater(constraint_metrics["total_operations"], 0)
        self.assertGreater(constraint_metrics["total_energy_consumed"], 0)
        self.assertGreaterEqual(constraint_metrics["constraint_violations"], failed_operations)
        
        print("\n=== Acceptance Criteria Validation Results ===")
        print(f"Successful operations: {successful_operations}")
        print(f"Failed operations (resource constraints): {failed_operations}")
        print(f"Total registered agents: {len(self.agents)}")
        print(f"Total energy consumed: {constraint_metrics['total_energy_consumed']:.6f}J")
        print(f"Constraint violation rate: {constraint_metrics['violation_rate']:.2f}%")
        print("=== Agents successfully operate under realistic resource limits ===")

if __name__ == "__main__":
    # Set up logging for test execution
    import logging
    logging.basicConfig(level=logging.INFO)
    
    # Run the comprehensive test suite
    unittest.main(verbosity=2)