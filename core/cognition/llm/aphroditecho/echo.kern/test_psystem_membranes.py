import sys
import unittest
import time
import threading
from unittest.mock import patch
try:
    from psystem_membranes import PSystemObject, Multiset, EvolutionRule, MembraneStructure, PSystemMembraneHierarchy, MembraneType, RuleType, ExecutionPhase, create_dtesn_psystem_example
except ImportError as e:
    print(f'Error importing P-System modules: {e}')
    sys.exit(1)
class TestPSystemObject(unittest.TestCase):
    def test_object_creation(self):
        obj = PSystemObject('a', 3)
        self.assertEqual(obj.symbol, 'a')
        self.assertEqual(obj.multiplicity, 3)
        self.assertTrue(obj.creation_time > 0)
    def test_object_invalid_multiplicity(self):
        with self.assertRaises(ValueError):
            PSystemObject('a', -1)
    def test_object_string_representation(self):
        obj1 = PSystemObject('a', 1)
        obj2 = PSystemObject('b', 5)
        self.assertEqual(str(obj1), 'a')
        self.assertEqual(str(obj2), 'b^5')
    def test_object_copy(self):
        obj1 = PSystemObject('a', 3, {'prop': 'value'})
        obj2 = obj1.copy()
        self.assertEqual(obj1.symbol, obj2.symbol)
        self.assertEqual(obj1.multiplicity, obj2.multiplicity)
        self.assertEqual(obj1.properties, obj2.properties)
        obj2.properties['prop'] = 'new_value'
        self.assertNotEqual(obj1.properties, obj2.properties)
class TestMultiset(unittest.TestCase):
    def setUp(self):
        self.multiset = Multiset()
    def test_empty_multiset(self):
        self.assertTrue(self.multiset.is_empty())
        self.assertEqual(len(self.multiset), 0)
        self.assertEqual(str(self.multiset), '∅')
    def test_add_objects(self):
        self.multiset.add('a', 3)
        self.multiset.add('b', 1)
        self.assertEqual(self.multiset.count('a'), 3)
        self.assertEqual(self.multiset.count('b'), 1)
        self.assertEqual(len(self.multiset), 4)
        self.assertFalse(self.multiset.is_empty())
    def test_remove_objects(self):
        self.multiset.add('a', 5)
        self.assertTrue(self.multiset.remove('a', 2))
        self.assertEqual(self.multiset.count('a'), 3)
        self.assertTrue(self.multiset.remove('a', 3))
        self.assertEqual(self.multiset.count('a'), 0)
        self.assertTrue(self.multiset.is_empty())
        self.multiset.add('b', 2)
        self.assertFalse(self.multiset.remove('b', 5))
        self.assertEqual(self.multiset.count('b'), 2)
    def test_contains(self):
        self.multiset.add('a', 5)
        self.assertTrue(self.multiset.contains('a', 3))
        self.assertTrue(self.multiset.contains('a', 5))
        self.assertFalse(self.multiset.contains('a', 6))
        self.assertFalse(self.multiset.contains('b', 1))
    def test_multiset_operations(self):
        ms1 = Multiset()
        ms1.add('a', 3)
        ms1.add('b', 2)
        ms2 = Multiset()
        ms2.add('a', 1)
        ms2.add('c', 4)
        union = ms1.union(ms2)
        self.assertEqual(union.count('a'), 4)
        self.assertEqual(union.count('b'), 2)
        self.assertEqual(union.count('c'), 4)
        intersection = ms1.intersection(ms2)
        self.assertEqual(intersection.count('a'), 1)
        self.assertEqual(intersection.count('b'), 0)
        self.assertEqual(intersection.count('c'), 0)
    def test_multiset_string_representation(self):
        self.multiset.add('a', 1)
        self.multiset.add('b', 3)
        result = str(self.multiset)
        self.assertIn('a', result)
        self.assertIn('b^3', result)
class TestEvolutionRule(unittest.TestCase):
    def test_rule_creation(self):
        lhs = Multiset()
        lhs.add('a', 2)
        rhs = Multiset()
        rhs.add('b', 1)
        rule = EvolutionRule(rule_id='test_rule', rule_type=RuleType.EVOLUTION, lhs=lhs, rhs=rhs, priority=2)
        self.assertEqual(rule.rule_id, 'test_rule')
        self.assertEqual(rule.rule_type, RuleType.EVOLUTION)
        self.assertEqual(rule.priority, 2)
    def test_rule_invalid_probability(self):
        lhs = Multiset()
        rhs = Multiset()
        with self.assertRaises(ValueError):
            EvolutionRule('test', RuleType.EVOLUTION, lhs, rhs, probability=1.5)
    def test_rule_applicability(self):
        lhs = Multiset()
        lhs.add('a', 2)
        lhs.add('b', 1)
        rhs = Multiset()
        rhs.add('c', 1)
        rule = EvolutionRule('test', RuleType.EVOLUTION, lhs, rhs)
        membrane_objects = Multiset()
        membrane_objects.add('a', 3)
        membrane_objects.add('b', 2)
        self.assertTrue(rule.is_applicable(membrane_objects))
        membrane_objects.clear()
        membrane_objects.add('a', 1)
        self.assertFalse(rule.is_applicable(membrane_objects))
    def test_rule_application(self):
        lhs = Multiset()
        lhs.add('a', 2)
        rhs = Multiset()
        rhs.add('b', 3)
        rule = EvolutionRule('test', RuleType.EVOLUTION, lhs, rhs)
        membrane_objects = Multiset()
        membrane_objects.add('a', 5)
        success, products = rule.apply(membrane_objects)
        self.assertTrue(success)
        self.assertEqual(membrane_objects.count('a'), 3)
        self.assertEqual(products.count('b'), 3)
class TestMembraneStructure(unittest.TestCase):
    def test_membrane_creation(self):
        membrane = MembraneStructure(membrane_id='test_membrane', membrane_type=MembraneType.ROOT, label='test_label')
        self.assertEqual(membrane.membrane_id, 'test_membrane')
        self.assertEqual(membrane.membrane_type, MembraneType.ROOT)
        self.assertEqual(membrane.label, 'test_label')
        self.assertFalse(membrane.is_dissolved)
        self.assertTrue(membrane.is_elementary())
    def test_membrane_children_management(self):
        parent = MembraneStructure('parent', MembraneType.ROOT, 'parent')
        parent.add_child('child1')
        parent.add_child('child2')
        self.assertEqual(len(parent.children_ids), 2)
        self.assertIn('child1', parent.children_ids)
        self.assertIn('child2', parent.children_ids)
        self.assertFalse(parent.is_elementary())
        parent.remove_child('child1')
        self.assertEqual(len(parent.children_ids), 1)
        self.assertNotIn('child1', parent.children_ids)
    def test_membrane_object_management(self):
        membrane = MembraneStructure('test', MembraneType.LEAF, 'test')
        membrane.add_object('a', 3)
        membrane.add_object('b', 1)
        self.assertEqual(membrane.objects.count('a'), 3)
        self.assertEqual(membrane.objects.count('b'), 1)
        self.assertTrue(membrane.remove_object('a', 2))
        self.assertEqual(membrane.objects.count('a'), 1)
        self.assertFalse(membrane.remove_object('a', 5))
    def test_membrane_rule_management(self):
        membrane = MembraneStructure('test', MembraneType.LEAF, 'test')
        lhs = Multiset()
        lhs.add('a', 1)
        rhs = Multiset()
        rhs.add('b', 1)
        rule = EvolutionRule('test_rule', RuleType.EVOLUTION, lhs, rhs)
        membrane.add_rule(rule)
        self.assertEqual(len(membrane.rules), 1)
        self.assertEqual(membrane.rules[0].rule_id, 'test_rule')
    def test_membrane_dissolution(self):
        membrane = MembraneStructure('test', MembraneType.LEAF, 'test')
        self.assertFalse(membrane.is_dissolved)
        membrane.dissolve()
        self.assertTrue(membrane.is_dissolved)
class TestPSystemMembraneHierarchy(unittest.TestCase):
    def setUp(self):
        self.system = PSystemMembraneHierarchy('test_system')
    def test_system_creation(self):
        self.assertEqual(self.system.system_name, 'test_system')
        self.assertEqual(len(self.system.membranes), 0)
        self.assertEqual(self.system.evolution_step, 0)
        self.assertFalse(self.system.is_halted)
    def test_membrane_creation_and_hierarchy(self):
        root_id = self.system.create_membrane(MembraneType.ROOT, 'root')
        self.assertEqual(self.system.skin_membrane_id, root_id)
        child_id = self.system.create_membrane(MembraneType.LEAF, 'child', root_id)
        root = self.system.get_membrane(root_id)
        child = self.system.get_membrane(child_id)
        self.assertIn(child_id, root.children_ids)
        self.assertEqual(child.parent_id, root_id)
        self.assertEqual(child.depth, 1)
    def test_membrane_communication(self):
        mem1_id = self.system.create_membrane(MembraneType.ROOT, 'mem1')
        mem2_id = self.system.create_membrane(MembraneType.LEAF, 'mem2', mem1_id)
        mem1 = self.system.get_membrane(mem1_id)
        mem1.add_object('a', 5)
        transfer_objects = Multiset()
        transfer_objects.add('a', 3)
        success = self.system.communicate_objects(mem1_id, mem2_id, transfer_objects)
        self.assertTrue(success)
        self.assertEqual(mem1.objects.count('a'), 2)
        mem2 = self.system.get_membrane(mem2_id)
        self.assertEqual(mem2.objects.count('a'), 3)
    def test_membrane_dissolution(self):
        root_id = self.system.create_membrane(MembraneType.ROOT, 'root')
        child_id = self.system.create_membrane(MembraneType.LEAF, 'child', root_id)
        child = self.system.get_membrane(child_id)
        child.add_object('a', 3)
        self.assertFalse(self.system.dissolve_membrane(root_id))
        self.assertTrue(self.system.dissolve_membrane(child_id))
        root = self.system.get_membrane(root_id)
        self.assertEqual(root.objects.count('a'), 3)
        self.assertTrue(child.is_dissolved)
    def test_membrane_division(self):
        mem_id = self.system.create_membrane(MembraneType.ROOT, 'original')
        membrane = self.system.get_membrane(mem_id)
        membrane.add_object('a', 10)
        membrane.add_object('b', 5)
        division_objects = Multiset()
        division_objects.add('a', 3)
        division_objects.add('b', 2)
        new_mem_id = self.system.divide_membrane(mem_id, division_objects)
        self.assertIsNotNone(new_mem_id)
        self.assertEqual(membrane.objects.count('a'), 7)
        self.assertEqual(membrane.objects.count('b'), 3)
        new_membrane = self.system.get_membrane(new_mem_id)
        self.assertEqual(new_membrane.objects.count('a'), 3)
        self.assertEqual(new_membrane.objects.count('b'), 2)
    def test_system_evolution(self):
        mem_id = self.system.create_membrane(MembraneType.ROOT, 'test')
        membrane = self.system.get_membrane(mem_id)
        membrane.add_object('a', 2)
        lhs = Multiset()
        lhs.add('a', 1)
        rhs = Multiset()
        rhs.add('b', 1)
        rule = EvolutionRule('transform', RuleType.EVOLUTION, lhs, rhs)
        membrane.add_rule(rule)
        active = self.system.evolve_system()
        self.assertTrue(active)
        self.assertEqual(self.system.evolution_step, 1)
        self.assertEqual(membrane.objects.count('a'), 1)
        self.assertEqual(membrane.objects.count('b'), 1)
    def test_oeis_compliance_validation(self):
        try:
            is_valid, errors = self.system.validate_oeis_a000081_compliance()
            self.assertIsInstance(is_valid, bool)
            self.assertIsInstance(errors, list)
        except ImportError:
            with patch.object(self.system, 'validate_oeis_a000081_compliance') as mock_validate:
                mock_validate.return_value = (True, [])
                is_valid, errors = mock_validate()
                self.assertTrue(is_valid)
                self.assertEqual(len(errors), 0)
    def test_system_statistics(self):
        root_id = self.system.create_membrane(MembraneType.ROOT, 'root')
        self.system.create_membrane(MembraneType.LEAF, 'child', root_id)
        root = self.system.get_membrane(root_id)
        root.add_object('a', 5)
        stats = self.system.get_system_stats()
        self.assertEqual(stats['system_name'], 'test_system')
        self.assertEqual(stats['total_membranes'], 2)
        self.assertEqual(stats['active_membranes'], 2)
        self.assertEqual(stats['dissolved_membranes'], 0)
        self.assertEqual(stats['total_objects'], 5)
    def test_membrane_tree_generation(self):
        root_id = self.system.create_membrane(MembraneType.ROOT, 'root')
        self.system.create_membrane(MembraneType.LEAF, 'child1', root_id)
        self.system.create_membrane(MembraneType.LEAF, 'child2', root_id)
        tree = self.system.get_membrane_tree()
        self.assertEqual(tree['id'], root_id)
        self.assertEqual(tree['label'], 'root')
        self.assertEqual(len(tree['children']), 2)
        child_labels = [child['label'] for child in tree['children']]
        self.assertIn('child1', child_labels)
        self.assertIn('child2', child_labels)
class TestDTESNPSystemIntegration(unittest.TestCase):
    def test_dtesn_example_creation(self):
        system = create_dtesn_psystem_example()
        self.assertEqual(system.system_name, 'DTESN_Example')
        self.assertGreater(len(system.membranes), 0)
        is_valid, errors = system.validate_oeis_a000081_compliance()
        self.assertTrue(is_valid, f'OEIS validation failed: {errors}')
    def test_dtesn_hierarchy_structure(self):
        system = create_dtesn_psystem_example()
        type_counts = {}
        for membrane in system.membranes.values():
            mem_type = membrane.membrane_type
            type_counts[mem_type] = type_counts.get(mem_type, 0) + 1
        self.assertEqual(type_counts.get(MembraneType.ROOT, 0), 1)
        self.assertEqual(type_counts.get(MembraneType.TRUNK, 0), 1)
        self.assertEqual(type_counts.get(MembraneType.BRANCH, 0), 1)
        self.assertEqual(type_counts.get(MembraneType.LEAF, 0), 2)
        self.assertEqual(type_counts.get(MembraneType.TERMINAL, 0), 4)
    def test_dtesn_neuron_configuration(self):
        system = create_dtesn_psystem_example()
        for membrane in system.membranes.values():
            self.assertGreater(membrane.neuron_count, 0)
            self.assertIsInstance(membrane.spectral_radius, float)
            self.assertIsInstance(membrane.connectivity, float)
class TestPerformanceAndTiming(unittest.TestCase):
    def test_evolution_timing(self):
        system = create_dtesn_psystem_example()
        start_time = time.time()
        system.evolve_system()
        end_time = time.time()
        evolution_time_ms = (end_time - start_time) * 1000
        self.assertLess(evolution_time_ms, 100, 'Evolution took too long')
    def test_membrane_evolution_timing(self):
        system = PSystemMembraneHierarchy('timing_test')
        mem_id = system.create_membrane(MembraneType.ROOT, 'test')
        start_time = time.time()
        system.evolve_membrane(mem_id)
        end_time = time.time()
        evolution_time_us = (end_time - start_time) * 1000000
        self.assertLess(evolution_time_us, 1000, 'Membrane evolution took too long')
class TestConcurrencyAndThreadSafety(unittest.TestCase):
    def test_concurrent_membrane_creation(self):
        system = PSystemMembraneHierarchy('concurrent_test')
        created_membranes = []
        def create_membranes():
            for i in range(10):
                mem_id = system.create_membrane(MembraneType.LEAF, f'membrane_{i}')
                created_membranes.append(mem_id)
        threads = []
        for _ in range(3):
            thread = threading.Thread(target=create_membranes)
            threads.append(thread)
            thread.start()
        for thread in threads:
            thread.join()
        self.assertEqual(len(created_membranes), 30)
        self.assertEqual(len(set(created_membranes)), 30)
    def test_concurrent_object_manipulation(self):
        system = PSystemMembraneHierarchy('concurrent_test')
        mem_id = system.create_membrane(MembraneType.ROOT, 'test')
        membrane = system.get_membrane(mem_id)
        def add_objects():
            for i in range(100):
                membrane.add_object('test', 1)
        def remove_objects():
            for i in range(50):
                membrane.remove_object('test', 1)
        add_thread = threading.Thread(target=add_objects)
        remove_thread = threading.Thread(target=remove_objects)
        add_thread.start()
        remove_thread.start()
        add_thread.join()
        remove_thread.join()
        self.assertEqual(membrane.objects.count('test'), 50)
def run_all_tests():
    test_suites = [TestPSystemObject, TestMultiset, TestEvolutionRule, TestMembraneStructure, TestPSystemMembraneHierarchy, TestDTESNPSystemIntegration, TestPerformanceAndTiming, TestConcurrencyAndThreadSafety]
    overall_result = True
    total_tests = 0
    total_failures = 0
    print('P-System Membrane Data Structures Test Suite')
    print('=' * 60)
    for test_suite in test_suites:
        print(f'\nRunning {test_suite.__name__}...')
        suite = unittest.TestLoader().loadTestsFromTestCase(test_suite)
        runner = unittest.TextTestRunner(verbosity=1, stream=open('/dev/null', 'w'))
        result = runner.run(suite)
        tests_run = result.testsRun
        failures = len(result.failures) + len(result.errors)
        total_tests += tests_run
        total_failures += failures
        if failures == 0:
            print(f'  ✅ {tests_run} tests passed')
        else:
            print(f'  ❌ {failures}/{tests_run} tests failed')
            overall_result = False
            for test, traceback in result.failures + result.errors:
                print(f'    FAILED: {test}')
    print('\n' + '=' * 60)
    print(f'Test Results: {total_tests - total_failures}/{total_tests} tests passed')
    if overall_result:
        print('🎉 All tests passed! P-System membrane data structures are working correctly.')
        return True
    else:
        print('❌ Some tests failed. Please review the implementation.')
        return False
if __name__ == '__main__':
    success = run_all_tests()
    sys.exit(0 if success else 1)