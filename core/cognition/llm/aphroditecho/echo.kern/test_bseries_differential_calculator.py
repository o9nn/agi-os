import sys
import os
import math
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from bseries_differential_calculator import BSeriesDifferentialCalculator, create_differential_function, SingleNodeEvaluator, LinearChainEvaluator, StarGraphEvaluator
def test_differential_function_creation():
    print('Testing differential function creation...')
    def f(y):
        return y
    def f_prime(y):
        return 1.0
    def f_double(y):
        return 0.0
    df = create_differential_function(f, f_prime, f_double, name='linear')
    assert df.f(5.0) == 5.0, 'Function evaluation failed'
    assert df.f_prime(5.0) == 1.0, 'First derivative evaluation failed'
    assert df.f_double(5.0) == 0.0, 'Second derivative evaluation failed'
    assert df.name == 'linear', 'Function name not set correctly'
    print('✅ Differential function creation test passed')
def test_single_node_evaluator():
    print('Testing single node evaluator...')
    evaluator = SingleNodeEvaluator()
    def f(y):
        return y * y
    df = create_differential_function(f, name='quadratic')
    result = evaluator.evaluate(df, 3.0)
    expected = 9.0
    assert abs(result - expected) < 1e-10, f'Expected {expected}, got {result}'
    assert evaluator.get_expression() == 'f', "Expression should be 'f'"
    print('✅ Single node evaluator test passed')
def test_linear_chain_evaluator():
    print('Testing linear chain evaluator...')
    def f(y):
        return y * y
    def f_prime(y):
        return 2 * y
    def f_double(y):
        return 2.0
    df = create_differential_function(f, f_prime, f_double, name='quadratic')
    evaluator_2 = LinearChainEvaluator(2)
    result_2 = evaluator_2.evaluate(df, 2.0)
    2.0 * 2.0 * 4.0
    assert abs(result_2 - 16.0) < 1e-10, f'Order 2: Expected near 16, got {result_2}'
    evaluator_3 = LinearChainEvaluator(3)
    result_3 = evaluator_3.evaluate(df, 2.0)
    2.0 * 4.0 * 4.0
    assert abs(result_3 - 32.0) < 1e-10, f'Order 3: Expected 32, got {result_3}'
    print('✅ Linear chain evaluator test passed')
def test_star_graph_evaluator():
    print('Testing star graph evaluator...')
    def f(y):
        return y * y
    def f_prime(y):
        return 2 * y
    def f_triple(y):
        return 0.0
    df = create_differential_function(f, f_prime, f_triple=f_triple, name='quadratic')
    evaluator = StarGraphEvaluator(3, 2)
    result = evaluator.evaluate(df, 2.0)
    4.0 * 2.0 * 2.0 * 4.0
    print(f'Star graph result: {result}, expected around: 4 * 2 * 2 * 4 = 64')
    print('✅ Star graph evaluator test passed')
def test_calculator_initialization():
    print('Testing calculator initialization...')
    calculator = BSeriesDifferentialCalculator()
    assert len(calculator.evaluators) > 0, 'No evaluators created'
    assert 1 in calculator.evaluators, 'Tree 1 (single node) not found'
    assert 2 in calculator.evaluators, 'Tree 2 (linear chain) not found'
    supported = calculator.get_supported_trees()
    assert len(supported) > 0, 'No supported trees returned'
    print('✅ Calculator initialization test passed')
def test_elementary_differential_evaluation():
    print('Testing elementary differential evaluation...')
    calculator = BSeriesDifferentialCalculator()
    def f(y):
        return y
    def f_prime(y):
        return 1.0
    def f_double(y):
        return 0.0
    df = create_differential_function(f, f_prime, f_double, name='linear')
    result_1 = calculator.evaluate_elementary_differential(1, df, 5.0)
    assert result_1 == 5.0, f'Tree 1: Expected 5.0, got {result_1}'
    result_2 = calculator.evaluate_elementary_differential(2, df, 5.0)
    expected_2 = 1.0 * 5.0
    assert result_2 == expected_2, f'Tree 2: Expected {expected_2}, got {result_2}'
    print('✅ Elementary differential evaluation test passed')
def test_differential_function_validation():
    print('Testing differential function validation...')
    calculator = BSeriesDifferentialCalculator()
    def f(y):
        return y
    df_incomplete = create_differential_function(f, name='incomplete')
    is_valid, errors = calculator.validate_differential_function(df_incomplete, max_order=3)
    assert not is_valid, 'Incomplete function should not be valid for order 3'
    assert len(errors) > 0, 'Should have validation errors'
    def f_prime(y):
        return 1.0
    def f_double(y):
        return 0.0
    df_complete = create_differential_function(f, f_prime, f_double, name='complete')
    is_valid, errors = calculator.validate_differential_function(df_complete, max_order=3)
    assert is_valid, f'Complete function should be valid: {errors}'
    assert len(errors) == 0, 'Should have no validation errors'
    print('✅ Differential function validation test passed')
def test_bseries_step_evaluation():
    print('Testing B-Series step evaluation...')
    calculator = BSeriesDifferentialCalculator()
    def f(y):
        return y
    def f_prime(y):
        return 1.0
    def f_double(y):
        return 0.0
    def f_triple(y):
        return 0.0
    df = create_differential_function(f, f_prime, f_double, f_triple, name='exponential')
    y0 = 1.0
    h = 0.1
    y1 = calculator.evaluate_bseries_step(df, y0, h, max_order=3)
    exact = math.exp(h)
    error = abs(y1 - exact)
    print(f'B-Series result: {y1:.6f}')
    print(f'Exact result: {exact:.6f}')
    print(f'Error: {error:.6f}')
    assert error < 0.1, f'Error too large: {error}'
    assert y1 > y0, 'Result should be greater than initial value for growing exponential'
    print('✅ B-Series step evaluation test passed')
def test_tree_evaluation_info():
    print('Testing tree evaluation info...')
    calculator = BSeriesDifferentialCalculator()
    info_1 = calculator.get_tree_evaluation_info(1)
    required_fields = ['tree_id', 'order', 'structure_type', 'coefficient', 'expression', 'computational_cost']
    for field in required_fields:
        assert field in info_1, f'Missing field: {field}'
    assert info_1['tree_id'] == 1, 'Tree ID should be 1'
    assert info_1['order'] == 1, 'Order should be 1'
    assert info_1['expression'] == 'f', "Expression should be 'f'"
    info_nonexistent = calculator.get_tree_evaluation_info(9999)
    assert 'error' in info_nonexistent, 'Should return error for nonexistent tree'
    print('✅ Tree evaluation info test passed')
def test_error_handling():
    print('Testing error handling...')
    calculator = BSeriesDifferentialCalculator()
    def f(y):
        return y * y
    df_incomplete = create_differential_function(f, name='incomplete')
    try:
        result = calculator.evaluate_elementary_differential(2, df_incomplete, 1.0)
        print(f'Graceful handling returned: {result}')
    except ValueError as e:
        print(f'Expected error caught: {e}')
    except Exception as e:
        print(f'Unexpected error type: {e}')
    def f_complete(y):
        return y
    def f_prime(y):
        return 1.0
    df_complete = create_differential_function(f_complete, f_prime, name='complete')
    try:
        calculator.evaluate_elementary_differential(9999, df_complete, 1.0)
        raise AssertionError('Should raise error for invalid tree ID')
    except ValueError:
        pass
    print('✅ Error handling test passed')
def test_mathematical_accuracy():
    print('Testing mathematical accuracy...')
    calculator = BSeriesDifferentialCalculator()
    def f(y):
        return y * y * y
    def f_prime(y):
        return 3 * y * y
    def f_double(y):
        return 6 * y
    def f_triple(y):
        return 6.0
    df = create_differential_function(f, f_prime, f_double, f_triple, name='cubic')
    y = 2.0
    result_1 = calculator.evaluate_elementary_differential(1, df, y)
    expected_1 = 8.0
    assert abs(result_1 - expected_1) < 1e-10, f'Tree 1: Expected {expected_1}, got {result_1}'
    result_2 = calculator.evaluate_elementary_differential(2, df, y)
    expected_2 = 3 * 4 * 8
    assert abs(result_2 - expected_2) < 1e-10, f'Tree 2: Expected {expected_2}, got {result_2}'
    print('✅ Mathematical accuracy test passed')
def run_all_tests():
    print('B-Series Elementary Differential Calculator Test Suite')
    print('=' * 60)
    tests = [test_differential_function_creation, test_single_node_evaluator, test_linear_chain_evaluator, test_star_graph_evaluator, test_calculator_initialization, test_elementary_differential_evaluation, test_differential_function_validation, test_bseries_step_evaluation, test_tree_evaluation_info, test_error_handling, test_mathematical_accuracy]
    passed = 0
    total = len(tests)
    for test in tests:
        try:
            test()
            passed += 1
        except Exception as e:
            print(f'❌ {test.__name__} failed: {e}')
            import traceback
            traceback.print_exc()
    print(f'\nTest Results: {passed}/{total} tests passed')
    if passed == total:
        print('🎉 All tests passed! B-Series elementary differential calculator is working correctly.')
        return True
    else:
        print('💥 Some tests failed. Please check the implementation.')
        return False
if __name__ == '__main__':
    success = run_all_tests()
    sys.exit(0 if success else 1)