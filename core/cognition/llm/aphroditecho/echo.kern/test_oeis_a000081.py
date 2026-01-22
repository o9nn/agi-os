import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from oeis_a000081_enumerator import create_enhanced_validator, validate_membrane_hierarchy_enhanced, KNOWN_A000081_VALUES
def test_basic_enumeration():
    print('Testing basic enumeration...')
    enumerator = create_enhanced_validator()
    sequence = enumerator.get_sequence(10)
    expected = KNOWN_A000081_VALUES[:10]
    assert sequence == expected, f'Sequence mismatch: {sequence} != {expected}'
    print('✅ Basic enumeration test passed')
def test_individual_terms():
    print('Testing individual term access...')
    enumerator = create_enhanced_validator()
    test_cases = [(0, 0), (1, 1), (2, 1), (3, 2), (4, 4), (5, 9), (6, 20), (7, 48), (8, 115), (9, 286), (10, 719)]
    for n, expected in test_cases:
        actual = enumerator.get_term(n)
        assert actual == expected, f'Term {n}: expected {expected}, got {actual}'
    print('✅ Individual term access test passed')
def test_validation_functions():
    print('Testing validation functions...')
    enumerator = create_enhanced_validator()
    assert enumerator.is_valid_tree_count(5, 9), 'Should validate 9 trees for 5 nodes'
    assert enumerator.is_valid_tree_count(0, 0), 'Should validate 0 trees for 0 nodes'
    assert enumerator.is_valid_tree_count(1, 1), 'Should validate 1 tree for 1 node'
    assert not enumerator.is_valid_tree_count(5, 10), 'Should not validate 10 trees for 5 nodes'
    assert not enumerator.is_valid_tree_count(1, 0), 'Should not validate 0 trees for 1 node'
    print('✅ Validation functions test passed')
def test_membrane_hierarchy_validation():
    print('Testing membrane hierarchy validation...')
    valid_hierarchy = [1, 1, 1, 2, 4]
    is_valid, errors = validate_membrane_hierarchy_enhanced(valid_hierarchy, 4)
    assert is_valid, f'Valid hierarchy should pass validation. Errors: {errors}'
    invalid_hierarchy = [1, 1, 2, 2, 4]
    is_valid, errors = validate_membrane_hierarchy_enhanced(invalid_hierarchy, 4)
    assert not is_valid, 'Invalid hierarchy should fail validation'
    assert len(errors) > 0, 'Should have validation errors'
    assert any(('Level 2' in error for error in errors)), 'Should detect level 2 error'
    invalid_root = [2, 1, 1, 2, 4]
    is_valid, errors = validate_membrane_hierarchy_enhanced(invalid_root, 4)
    assert not is_valid, 'Invalid root count should fail validation'
    assert any(('Level 0' in error and 'root' in error for error in errors)), 'Should detect root error'
    print('✅ Membrane hierarchy validation test passed')
def test_utility_functions():
    print('Testing utility functions...')
    enumerator = create_enhanced_validator()
    max_nodes_100 = enumerator.get_max_nodes_for_count(100)
    assert max_nodes_100 >= 6, 'Should find at least 6 nodes for ≤100 trees'
    assert enumerator.get_term(max_nodes_100) <= 100, 'Max nodes should have ≤100 trees'
    assert enumerator.get_term(max_nodes_100 + 1) > 100, 'Next level should exceed 100 trees'
    known_range = enumerator.get_known_range()
    assert known_range >= 20, 'Should have at least 20 known terms'
    print('✅ Utility functions test passed')
def test_error_handling():
    print('Testing error handling...')
    enumerator = create_enhanced_validator()
    try:
        enumerator.get_term(-1)
        raise AssertionError('Should raise ValueError for negative index')
    except ValueError as e:
        assert 'non-negative' in str(e), 'Should mention non-negative requirement'
    empty_sequence = enumerator.get_sequence(0)
    assert empty_sequence == [], 'Empty sequence should return empty list'
    print('✅ Error handling test passed')
def test_integration_with_dtesn_compiler():
    print('Testing integration with DTESN compiler...')
    try:
        import dtesn_compiler
        assert dtesn_compiler._USE_ENHANCED_ENUMERATOR, 'DTESN compiler should use enhanced enumerator'
        assert len(dtesn_compiler.OEIS_A000081) >= 20, 'Should have extended OEIS sequence'
        validator = dtesn_compiler.OEIS_A000081_Validator()
        assert hasattr(validator, 'get_expected_count'), 'Should have enhanced methods'
        assert hasattr(validator, 'get_max_reliable_depth'), 'Should have enhanced methods'
        assert validator.get_expected_count(0) == 1, 'Level 0 should have count 1'
        assert validator.get_expected_count(5) == 9, 'Level 5 should have count 9'
        print('✅ DTESN compiler integration test passed')
    except ImportError:
        print('⚠️  DTESN compiler not available, skipping integration test')
def run_all_tests():
    print('OEIS A000081 Enumeration Validator Test Suite')
    print('=' * 50)
    tests = [test_basic_enumeration, test_individual_terms, test_validation_functions, test_membrane_hierarchy_validation, test_utility_functions, test_error_handling, test_integration_with_dtesn_compiler]
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
        print('🎉 All tests passed! OEIS A000081 enumeration validator is working correctly.')
        return True
    else:
        print('💥 Some tests failed. Please check the implementation.')
        return False
if __name__ == '__main__':
    success = run_all_tests()
    sys.exit(0 if success else 1)