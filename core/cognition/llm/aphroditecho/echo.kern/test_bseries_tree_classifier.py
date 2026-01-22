import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from bseries_tree_classifier import TreeStructureType, create_bseries_classifier
def test_basic_classification():
    print('Testing basic classification...')
    classifier = create_bseries_classifier()
    expected_counts = {1: 1, 2: 1, 3: 2, 4: 4, 5: 9}
    for order, expected_count in expected_counts.items():
        trees = classifier.get_trees_by_order(order)
        actual_count = len(trees)
        assert actual_count == expected_count, f'Order {order}: expected {expected_count} trees, got {actual_count}'
    print('✅ Basic classification test passed')
def test_tree_structure_types():
    print('Testing tree structure types...')
    classifier = create_bseries_classifier()
    tree_1 = classifier.get_tree_by_id(1)
    assert tree_1.structure_type == TreeStructureType.SINGLE_NODE
    tree_2 = classifier.get_tree_by_id(2)
    assert tree_2.structure_type == TreeStructureType.LINEAR_CHAIN
    stats = classifier.get_classification_statistics()
    assert stats['single_node_count'] == 1, 'Should have exactly 1 single node'
    assert stats['linear_chain_count'] >= 1, 'Should have at least 1 linear chain'
    assert stats['star_graph_count'] >= 1, 'Should have at least 1 star graph'
    print('✅ Tree structure type test passed')
def test_bseries_coefficients():
    print('Testing B-Series coefficients...')
    classifier = create_bseries_classifier()
    tree_1 = classifier.get_tree_by_id(1)
    assert tree_1.coefficient.coefficient_value == 1.0
    tree_2 = classifier.get_tree_by_id(2)
    assert abs(tree_2.coefficient.coefficient_value - 0.5) < 1e-10
    for tree_id, tree in classifier.classified_trees.items():
        assert tree.coefficient.coefficient_value > 0, f'Tree {tree_id} has non-positive coefficient'
    for tree_id, tree in classifier.classified_trees.items():
        assert tree.coefficient.denominator > 0, f'Tree {tree_id} has non-positive denominator'
        assert isinstance(tree.coefficient.denominator, int), f'Tree {tree_id} has non-integer denominator'
    print('✅ B-Series coefficient test passed')
def test_elementary_differentials():
    print('Testing elementary differentials...')
    classifier = create_bseries_classifier()
    for tree_id, tree in classifier.classified_trees.items():
        ed = tree.elementary_diff
        assert ed is not None, f'Tree {tree_id} missing elementary differential'
        assert ed.order > 0, f'Tree {tree_id} has invalid order'
        assert ed.expression, f'Tree {tree_id} has empty expression'
        assert ed.computational_cost > 0, f'Tree {tree_id} has non-positive cost'
    tree_1 = classifier.get_tree_by_id(1)
    assert tree_1.elementary_diff.expression == 'f'
    tree_2 = classifier.get_tree_by_id(2)
    assert tree_2.elementary_diff.expression == "f'(f)"
    print('✅ Elementary differential test passed')
def test_oeis_a000081_validation():
    print('Testing OEIS A000081 validation...')
    classifier = create_bseries_classifier()
    is_valid, errors = classifier.validate_against_oeis_a000081()
    if not is_valid:
        print('Validation errors:')
        for error in errors:
            print(f'  {error}')
        raise AssertionError('OEIS A000081 validation failed')
    print('✅ OEIS A000081 validation test passed')
def test_computational_costs():
    print('Testing computational costs...')
    classifier = create_bseries_classifier()
    costs = classifier.get_computational_cost_summary()
    for order in range(1, 5):
        assert order in costs, f'Missing cost for order {order}'
        assert costs[order] > 0, f'Non-positive cost for order {order}'
    assert costs[1] <= costs[2] * 2, 'Order 1 cost unexpectedly high'
    assert costs[2] <= costs[3] * 2, 'Order 2 cost unexpectedly high'
    print('✅ Computational cost test passed')
def test_tree_access_methods():
    print('Testing tree access methods...')
    classifier = create_bseries_classifier()
    tree_1 = classifier.get_tree_by_id(1)
    assert tree_1 is not None, 'Could not retrieve tree by ID'
    assert tree_1.tree_id == 1, 'Tree ID mismatch'
    order_1_trees = classifier.get_trees_by_order(1)
    assert len(order_1_trees) == 1, 'Wrong number of order-1 trees'
    assert order_1_trees[0].tree_id == 1, 'Wrong tree in order-1 list'
    nonexistent = classifier.get_tree_by_id(9999)
    assert nonexistent is None, 'Should return None for nonexistent tree'
    empty_order = classifier.get_trees_by_order(999)
    assert empty_order == [], 'Should return empty list for nonexistent order'
    print('✅ Tree access method test passed')
def test_symmetry_factors():
    print('Testing symmetry factors...')
    classifier = create_bseries_classifier()
    for tree_id, tree in classifier.classified_trees.items():
        assert tree.symmetry_factor > 0, f'Tree {tree_id} has non-positive symmetry factor'
        assert isinstance(tree.symmetry_factor, int), f'Tree {tree_id} has non-integer symmetry factor'
    tree_1 = classifier.get_tree_by_id(1)
    assert tree_1.symmetry_factor == 1
    star_trees = [tree for tree in classifier.classified_trees.values() if tree.structure_type == TreeStructureType.STAR_GRAPH]
    for tree in star_trees:
        assert tree.symmetry_factor >= 1, 'Star graph should have symmetry >= 1'
    print('✅ Symmetry factor test passed')
def test_classification_statistics():
    print('Testing classification statistics...')
    classifier = create_bseries_classifier()
    stats = classifier.get_classification_statistics()
    required_fields = ['total_trees', 'max_order', 'single_node_count', 'linear_chain_count', 'star_graph_count', 'binary_tree_count', 'general_tree_count']
    for field in required_fields:
        assert field in stats, f'Missing required field: {field}'
        assert isinstance(stats[field], int), f'Field {field} should be integer'
        assert stats[field] >= 0, f'Field {field} should be non-negative'
    type_count_sum = stats['single_node_count'] + stats['linear_chain_count'] + stats['star_graph_count'] + stats['binary_tree_count'] + stats['general_tree_count']
    assert type_count_sum == stats['total_trees'], "Structure type counts don't sum to total"
    print('✅ Classification statistics test passed')
def test_integration_with_dtesn_system():
    print('Testing DTESN system integration...')
    try:
        import dtesn_compiler
        classifier = create_bseries_classifier()
        assert classifier is not None, 'Could not create classifier'
        order_4_trees = classifier.get_trees_by_order(4)
        assert len(order_4_trees) == 4, 'Order 4 should have 4 trees per OEIS A000081'
    except ImportError:
        print('⚠️  DTESN compiler not available, skipping integration test')
    print('✅ DTESN system integration test passed')
def run_all_tests():
    print('B-Series Tree Classification Test Suite')
    print('=' * 50)
    tests = [test_basic_classification, test_tree_structure_types, test_bseries_coefficients, test_elementary_differentials, test_oeis_a000081_validation, test_computational_costs, test_tree_access_methods, test_symmetry_factors, test_classification_statistics, test_integration_with_dtesn_system]
    passed = 0
    total = len(tests)
    for test in tests:
        try:
            test()
            passed += 1
        except Exception as e:
            print(f'❌ {test.__name__} failed: {e}')
    print(f'\nTest Results: {passed}/{total} tests passed')
    if passed == total:
        print('🎉 All tests passed! B-Series tree classification is working correctly.')
        return True
    else:
        print('💥 Some tests failed. Please check the implementation.')
        return False
if __name__ == '__main__':
    success = run_all_tests()
    sys.exit(0 if success else 1)