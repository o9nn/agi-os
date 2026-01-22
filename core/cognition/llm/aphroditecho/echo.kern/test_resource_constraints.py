import unittest
import time
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, as_completed
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))
from resource_constraint_manager import ResourceConstraintManager, OperationType, ResourceError
from dtesn_resource_integration import DTESNResourceIntegrator, ConstrainedAgent
class TestResourceConstraintManager(unittest.TestCase):
    def setUp(self):
        self.manager = ResourceConstraintManager()
        self.test_agent_id = 'test_agent_001'
    def tearDown(self):
        self.manager.release_resources(self.test_agent_id)
    def test_initialization(self):
        status = self.manager.get_resource_status()
        self.assertIn('cpu_primary', status)
        self.assertIn('memory_main', status)
        self.assertIn('energy_budget', status)
        self.assertIn('neuromorphic_units', status)
        cpu_constraint = status['cpu_primary']
        self.assertEqual(cpu_constraint['type'], 'cpu_cycles')
        self.assertEqual(cpu_constraint['max_allocation'], 1000000000.0)
        self.assertTrue(cpu_constraint['hard_limit'])
    def test_resource_allocation_basic(self):
        resources = {'cpu_primary': 1000000.0, 'memory_main': 1024}
        success, message = self.manager.allocate_resources(self.test_agent_id, resources, OperationType.MEMBRANE_EVOLUTION)
        self.assertTrue(success)
        self.assertEqual(message, 'Resources allocated successfully')
        status = self.manager.get_resource_status()
        self.assertEqual(status['cpu_primary']['current_usage'], 1000000.0)
        self.assertEqual(status['memory_main']['current_usage'], 1024)
    def test_resource_allocation_insufficient(self):
        excessive_resources = {'cpu_primary': 2000000000.0}
        success, message = self.manager.allocate_resources(self.test_agent_id, excessive_resources)
        self.assertFalse(success)
        self.assertIn('Insufficient cpu_primary', message)
    def test_resource_release(self):
        resources = {'cpu_primary': 500000.0, 'memory_main': 512}
        self.manager.allocate_resources(self.test_agent_id, resources)
        status_before = self.manager.get_resource_status()
        self.assertEqual(status_before['cpu_primary']['current_usage'], 500000.0)
        success = self.manager.release_resources(self.test_agent_id)
        self.assertTrue(success)
        status_after = self.manager.get_resource_status()
        self.assertEqual(status_after['cpu_primary']['current_usage'], 0.0)
        self.assertEqual(status_after['memory_main']['current_usage'], 0.0)
    def test_realtime_constraint_validation(self):
        start_time = time.time_ns()
        time.sleep(1e-06)
        is_valid, message = self.manager.validate_realtime_constraint(OperationType.MEMBRANE_EVOLUTION, start_time)
        self.assertTrue(is_valid, f'Expected operation to be within deadline, got: {message}')
        self.assertIn('Within deadline', message)
        old_start_time = time.time_ns() - 50000
        is_valid, message = self.manager.validate_realtime_constraint(OperationType.MEMBRANE_EVOLUTION, old_start_time)
        self.assertFalse(is_valid)
        self.assertIn('Deadline exceeded', message)
    def test_energy_consumption_calculation(self):
        energy_cost = self.manager.calculate_operation_energy(OperationType.MEMBRANE_EVOLUTION)
        self.assertGreater(energy_cost, 0)
        self.assertLess(energy_cost, 0.001)
        duration = 1e-05
        energy_with_duration = self.manager.calculate_operation_energy(OperationType.MEMBRANE_EVOLUTION, duration_seconds=duration)
        self.assertGreater(energy_with_duration, 0)
        energy_complex = self.manager.calculate_operation_energy(OperationType.BSERIES_COMPUTATION, complexity=2.0)
        energy_simple = self.manager.calculate_operation_energy(OperationType.BSERIES_COMPUTATION, complexity=1.0)
        self.assertGreater(energy_complex, energy_simple)
    def test_enforce_agent_constraints(self):
        def mock_operation():
            time.sleep(5e-06)
            return 'operation_completed'
        result = self.manager.enforce_agent_constraints(self.test_agent_id, OperationType.MEMBRANE_EVOLUTION, mock_operation)
        self.assertEqual(result, 'operation_completed')
        metrics = self.manager.get_performance_metrics()
        self.assertEqual(metrics['total_operations'], 1)
        self.assertGreater(metrics['total_energy_consumed'], 0)
    def test_concurrent_resource_allocation(self):
        def allocate_and_release(agent_id, resources):
            success, _ = self.manager.allocate_resources(agent_id, resources, OperationType.MEMBRANE_EVOLUTION)
            if success:
                time.sleep(0.001)
                self.manager.release_resources(agent_id)
            return success
        num_agents = 10
        resources_per_agent = {'cpu_primary': 50000000.0, 'memory_main': 1024}
        with ThreadPoolExecutor(max_workers=num_agents) as executor:
            futures = [executor.submit(allocate_and_release, f'agent_{i}', resources_per_agent) for i in range(num_agents)]
            successful_allocations = sum((future.result() for future in as_completed(futures)))
            self.assertGreater(successful_allocations, 0)
            self.assertLessEqual(successful_allocations, num_agents)
    def test_performance_metrics(self):
        def dummy_op():
            return 'done'
        for i in range(5):
            self.manager.enforce_agent_constraints(f'agent_{i}', OperationType.MEMBRANE_EVOLUTION, dummy_op)
        metrics = self.manager.get_performance_metrics()
        self.assertEqual(metrics['total_operations'], 5)
        self.assertGreaterEqual(metrics['constraint_violations'], 0)
        self.assertGreater(metrics['total_energy_consumed'], 0)
        self.assertEqual(metrics['active_allocations'], 0)
class TestDTESNResourceIntegration(unittest.TestCase):
    def setUp(self):
        self.integrator = DTESNResourceIntegrator()
        self.test_agent = ConstrainedAgent(agent_id='dtesn_test_agent', max_operations_per_second=100, priority_level=5, energy_budget_joules=0.01)
        self.integrator.register_agent(self.test_agent)
    def tearDown(self):
        self.integrator.unregister_agent(self.test_agent.agent_id)
    def test_agent_registration(self):
        new_agent = ConstrainedAgent(agent_id='new_test_agent')
        success = self.integrator.register_agent(new_agent)
        self.assertTrue(success)
        success_duplicate = self.integrator.register_agent(new_agent)
        self.assertFalse(success_duplicate)
        success_unreg = self.integrator.unregister_agent(new_agent.agent_id)
        self.assertTrue(success_unreg)
        success_invalid = self.integrator.unregister_agent('non_existent')
        self.assertFalse(success_invalid)
    def test_constrained_psystem_wrapper(self):
        psystem = self.integrator.get_constrained_psystem(self.test_agent.agent_id)
        self.assertIsNotNone(psystem)
        membrane_config = {'initial_membranes': 2, 'evolution_rules': ['rule1', 'rule2'], 'max_cycles': 10}
        result = psystem.evolve_membrane(membrane_config)
        self.assertIsInstance(result, dict)
        self.assertIn('status', result)
        self.assertEqual(result['status'], 'evolved')
        tree_structure = {'depth': 3, 'nodes': [1, 2, 4]}
        is_valid = psystem.validate_oeis_compliance(tree_structure)
        self.assertIsInstance(is_valid, bool)
    def test_constrained_esn_wrapper(self):
        esn = self.integrator.get_constrained_esn(self.test_agent.agent_id)
        self.assertIsNotNone(esn)
        input_data = [0.1, 0.5, -0.3, 0.8, -0.2]
        output = esn.update_reservoir_state(input_data)
        self.assertIsInstance(output, list)
        self.assertEqual(len(output), len(input_data))
        target_outputs = [0.2, 0.4, 0.1]
        training_result = esn.train_readout(target_outputs)
        self.assertIsInstance(training_result, dict)
        self.assertIn('training_error', training_result)
        self.assertIn('convergence', training_result)
    def test_constrained_bseries_wrapper(self):
        bseries = self.integrator.get_constrained_bseries(self.test_agent.agent_id)
        self.assertIsNotNone(bseries)
        tree_structure = {'depth': 2, 'branching_factor': 2, 'node_count': 3}
        classification = bseries.classify_tree(tree_structure)
        self.assertIsInstance(classification, dict)
        self.assertIn('tree_type', classification)
        self.assertIn('order', classification)
        differential = bseries.compute_elementary_differential(tree_structure, 2)
        self.assertIsInstance(differential, dict)
        self.assertIn('differential', differential)
        self.assertIn('coefficient', differential)
    def test_unregistered_agent_access(self):
        unregistered_agent_id = 'unregistered_agent'
        psystem = self.integrator.get_constrained_psystem(unregistered_agent_id)
        self.assertIsNone(psystem)
        esn = self.integrator.get_constrained_esn(unregistered_agent_id)
        self.assertIsNone(esn)
        bseries = self.integrator.get_constrained_bseries(unregistered_agent_id)
        self.assertIsNone(bseries)
    def test_execute_constrained_operation(self):
        def test_operation(value):
            time.sleep(1e-06)
            return value * 2
        result = self.integrator.execute_constrained_operation(self.test_agent.agent_id, OperationType.BSERIES_COMPUTATION, test_operation, 5)
        self.assertEqual(result, 10)
        with self.assertRaises(ResourceError):
            self.integrator.execute_constrained_operation('unregistered', OperationType.MEMBRANE_EVOLUTION, test_operation, 5)
    def test_agent_resource_status(self):
        status = self.integrator.get_agent_resource_status(self.test_agent.agent_id)
        self.assertIsNotNone(status)
        self.assertEqual(status['agent_id'], self.test_agent.agent_id)
        self.assertEqual(status['priority_level'], 5)
        self.assertEqual(status['energy_budget'], 0.01)
        self.assertIn('global_constraints', status)
        status_invalid = self.integrator.get_agent_resource_status('invalid')
        self.assertIsNone(status_invalid)
    def test_system_performance_metrics(self):
        metrics = self.integrator.get_system_performance_metrics()
        self.assertIn('constraint_manager', metrics)
        self.assertIn('registered_agents', metrics)
        self.assertEqual(metrics['registered_agents'], 1)
        self.assertIn(self.test_agent.agent_id, metrics['agent_list'])
class TestResourceConstraintIntegrationAcceptanceCriteria(unittest.TestCase):
    def setUp(self):
        self.integrator = DTESNResourceIntegrator()
        self.agents = [ConstrainedAgent(f'agent_critical_{i}', priority_level=10, energy_budget_joules=0.1) for i in range(3)] + [ConstrainedAgent(f'agent_normal_{i}', priority_level=5, energy_budget_joules=0.05) for i in range(5)] + [ConstrainedAgent(f'agent_background_{i}', priority_level=1, energy_budget_joules=0.01) for i in range(10)]
        for agent in self.agents:
            self.integrator.register_agent(agent)
    def tearDown(self):
        for agent in self.agents:
            self.integrator.unregister_agent(agent.agent_id)
    def test_agents_operate_under_resource_limits(self):
        results = {}
        failed_operations = 0
        successful_operations = 0
        def intensive_operation(agent_id, operation_count):
            operation_results = []
            for i in range(operation_count):
                try:
                    bseries = self.integrator.get_constrained_bseries(agent_id)
                    if bseries:
                        result = bseries.classify_tree({'depth': 4, 'complexity': 2.0, 'node_count': 15})
                        operation_results.append(result)
                    esn = self.integrator.get_constrained_esn(agent_id)
                    if esn:
                        large_input = [0.1] * 100
                        esn_result = esn.update_reservoir_state(large_input)
                        operation_results.append(esn_result)
                except ResourceError as e:
                    operation_results.append(f'Resource constraint violation: {e}')
                except Exception as e:
                    operation_results.append(f'Other error: {e}')
            return (agent_id, operation_results)
        with ThreadPoolExecutor(max_workers=len(self.agents)) as executor:
            futures = [executor.submit(intensive_operation, agent.agent_id, 10) for agent in self.agents]
            for future in as_completed(futures):
                agent_id, operation_results = future.result()
                results[agent_id] = operation_results
                for result in operation_results:
                    if isinstance(result, str) and 'constraint violation' in result:
                        failed_operations += 1
                    elif isinstance(result, dict):
                        successful_operations += 1
        self.assertGreater(successful_operations, 0, 'No operations succeeded - system too restrictive')
        self.assertGreater(failed_operations, 0, 'No resource constraints were enforced - limits ineffective')
        critical_success = sum((1 for agent in self.agents[:3] for result in results[agent.agent_id] if isinstance(result, dict)))
        background_success = sum((1 for agent in self.agents[-10:] for result in results[agent.agent_id] if isinstance(result, dict)))
        if critical_success + background_success > 0:
            critical_success_rate = critical_success / (3 * 20)
            background_success_rate = background_success / (10 * 20)
            self.assertGreaterEqual(critical_success_rate, background_success_rate * 0.8, 'Priority-based resource allocation not working properly')
        metrics = self.integrator.get_system_performance_metrics()
        constraint_metrics = metrics['constraint_manager']
        self.assertGreater(constraint_metrics['total_operations'], 0)
        self.assertGreater(constraint_metrics['total_energy_consumed'], 0)
        self.assertGreaterEqual(constraint_metrics['constraint_violations'], failed_operations)
        print('\n=== Acceptance Criteria Validation Results ===')
        print(f'Successful operations: {successful_operations}')
        print(f'Failed operations (resource constraints): {failed_operations}')
        print(f'Total registered agents: {len(self.agents)}')
        print(f"Total energy consumed: {constraint_metrics['total_energy_consumed']:.6f}J")
        print(f"Constraint violation rate: {constraint_metrics['violation_rate']:.2f}%")
        print('=== Agents successfully operate under realistic resource limits ===')
if __name__ == '__main__':
    import logging
    logging.basicConfig(level=logging.INFO)
    unittest.main(verbosity=2)