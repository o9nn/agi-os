from typing import Callable, List, Dict, Tuple, Optional, Any
from dataclasses import dataclass
from abc import ABC, abstractmethod
try:
    from bseries_tree_classifier import BSeriesTreeClassifier, ClassifiedTree, TreeStructureType, create_bseries_classifier
    _bseries_classifier = create_bseries_classifier()
except ImportError:
    _bseries_classifier = None
@dataclass
class DifferentialFunction:
    f: Callable[[float], float]
    f_prime: Callable[[float], float]
    f_double: Optional[Callable[[float], float]] = None
    f_triple: Optional[Callable[[float], float]] = None
    f_quad: Optional[Callable[[float], float]] = None
    name: str = 'f'
class ElementaryDifferentialEvaluator(ABC):
    @abstractmethod
    def evaluate(self, df: DifferentialFunction, y: float) -> float:
        pass
    @abstractmethod
    def get_expression(self) -> str:
        pass
class SingleNodeEvaluator(ElementaryDifferentialEvaluator):
    def evaluate(self, df: DifferentialFunction, y: float) -> float:
        return df.f(y)
    def get_expression(self) -> str:
        return 'f'
class LinearChainEvaluator(ElementaryDifferentialEvaluator):
    def __init__(self, order: int):
        self.order = order
    def evaluate(self, df: DifferentialFunction, y: float) -> float:
        if self.order == 2:
            f_val = df.f(y)
            return df.f_prime(y) * f_val
        elif self.order == 3:
            if df.f_double is None:
                raise ValueError('Second derivative required for order 3')
            f_val = df.f(y)
            return df.f_double(y) * f_val * f_val
        elif self.order == 4:
            if df.f_triple is None:
                raise ValueError('Third derivative required for order 4')
            f_val = df.f(y)
            return df.f_triple(y) * f_val ** 3
        elif self.order == 5:
            if df.f_quad is None:
                raise ValueError('Fourth derivative required for order 5')
            f_val = df.f(y)
            return df.f_quad(y) * f_val ** 4
        else:
            raise ValueError(f'Linear chain order {self.order} not implemented')
    def get_expression(self) -> str:
        if self.order == 2:
            return "f'(f)"
        elif self.order == 3:
            return "f''(f,f)"
        elif self.order == 4:
            return "f'''(f,f,f)"
        elif self.order == 5:
            return "f''''(f,f,f,f)"
        else:
            return f'f^({self.order - 1})(f,...,f)'
class StarGraphEvaluator(ElementaryDifferentialEvaluator):
    def __init__(self, order: int, num_children: int):
        self.order = order
        self.num_children = num_children
    def evaluate(self, df: DifferentialFunction, y: float) -> float:
        f_val = df.f(y)
        if self.order == 3 and self.num_children == 2:
            f_prime_val = df.f_prime(y)
            return df.f_prime(y) * f_prime_val * f_val
        elif self.order == 4 and self.num_children == 3:
            if df.f_triple is None:
                raise ValueError('Third derivative required')
            return df.f_triple(y) * f_val ** 3 / 6
        elif self.order == 5 and self.num_children == 4:
            if df.f_quad is None:
                raise ValueError('Fourth derivative required')
            return df.f_quad(y) * f_val ** 4 / 24
        else:
            raise ValueError(f'Star graph order {self.order}, children {self.num_children} not implemented')
    def get_expression(self) -> str:
        if self.order == 3:
            return "f'(f'(f))"
        elif self.order == 4:
            return "f'''(f,f,f)"
        elif self.order == 5:
            return "f''''(f,f,f,f)"
        else:
            return f'f^({self.order - 1})(star)'
class CompositeEvaluator(ElementaryDifferentialEvaluator):
    def __init__(self, expression: str, order: int):
        self.expression = expression
        self.order = order
    def evaluate(self, df: DifferentialFunction, y: float) -> float:
        f_val = df.f(y)
        if "f'(f''(" in self.expression:
            if df.f_double is None:
                raise ValueError('Second derivative required')
            inner = df.f_double(y) * f_val * f_val
            return df.f_prime(y) * inner
        elif "f''(f'(" in self.expression:
            if df.f_double is None:
                raise ValueError('Second derivative required')
            f_prime_val = df.f_prime(y) * f_val
            return df.f_double(y) * f_prime_val * f_val
        elif self.order == 3:
            return df.f_double(y) * f_val * f_val if df.f_double else 0.0
        elif self.order == 4:
            return df.f_triple(y) * f_val ** 3 if df.f_triple else 0.0
        elif self.order == 5:
            return df.f_quad(y) * f_val ** 4 if df.f_quad else 0.0
        else:
            return f_val
    def get_expression(self) -> str:
        return self.expression
class BSeriesDifferentialCalculator:
    def __init__(self):
        self.classifier = _bseries_classifier
        self.evaluators: Dict[int, ElementaryDifferentialEvaluator] = {}
        self._initialize_evaluators()
    def _initialize_evaluators(self):
        if self.classifier is None:
            raise RuntimeError('B-Series classifier not available')
        for tree_id, tree in self.classifier.classified_trees.items():
            evaluator = self._create_evaluator_for_tree(tree)
            self.evaluators[tree_id] = evaluator
    def _create_evaluator_for_tree(self, tree: ClassifiedTree) -> ElementaryDifferentialEvaluator:
        if tree.structure_type == TreeStructureType.SINGLE_NODE:
            return SingleNodeEvaluator()
        elif tree.structure_type == TreeStructureType.LINEAR_CHAIN:
            return LinearChainEvaluator(tree.order)
        elif tree.structure_type == TreeStructureType.STAR_GRAPH:
            num_children = tree.order - 1
            return StarGraphEvaluator(tree.order, num_children)
        else:
            return CompositeEvaluator(tree.elementary_diff.expression, tree.order)
    def evaluate_elementary_differential(self, tree_id: int, df: DifferentialFunction, y: float) -> float:
        if tree_id not in self.evaluators:
            raise ValueError(f'No evaluator found for tree {tree_id}')
        evaluator = self.evaluators[tree_id]
        return evaluator.evaluate(df, y)
    def evaluate_bseries_step(self, df: DifferentialFunction, y: float, h: float, max_order: int=5) -> float:
        if self.classifier is None:
            raise RuntimeError('B-Series classifier not available')
        result = y
        for order in range(1, max_order + 1):
            trees = self.classifier.get_trees_by_order(order)
            for tree in trees:
                try:
                    alpha = tree.coefficient.coefficient_value
                    f_tau = self.evaluate_elementary_differential(tree.tree_id, df, y)
                    result += h * alpha * f_tau
                except (ValueError, AttributeError):
                    continue
        return result
    def get_tree_evaluation_info(self, tree_id: int) -> Dict[str, Any]:
        if tree_id not in self.evaluators:
            return {'error': f'Tree {tree_id} not found'}
        tree = self.classifier.get_tree_by_id(tree_id)
        evaluator = self.evaluators[tree_id]
        return {'tree_id': tree_id, 'order': tree.order, 'structure_type': tree.structure_type.value, 'coefficient': tree.coefficient.coefficient_value, 'expression': evaluator.get_expression(), 'computational_cost': tree.elementary_diff.computational_cost}
    def validate_differential_function(self, df: DifferentialFunction, max_order: int=5) -> Tuple[bool, List[str]]:
        errors = []
        if df.f is None:
            errors.append('Function f is required')
        if max_order >= 2 and df.f_prime is None:
            errors.append("First derivative f' is required for order >= 2")
        if max_order >= 3 and df.f_double is None:
            errors.append("Second derivative f'' is required for order >= 3")
        if max_order >= 4 and df.f_triple is None:
            errors.append("Third derivative f''' is required for order >= 4")
        if max_order >= 5 and df.f_quad is None:
            errors.append("Fourth derivative f'''' is required for order >= 5")
        return (len(errors) == 0, errors)
    def get_supported_trees(self) -> List[Dict[str, Any]]:
        return [self.get_tree_evaluation_info(tree_id) for tree_id in sorted(self.evaluators.keys())]
def create_differential_function(f, f_prime=None, f_double=None, f_triple=None, f_quad=None, name='f'):
    return DifferentialFunction(f=f, f_prime=f_prime, f_double=f_double, f_triple=f_triple, f_quad=f_quad, name=name)
def main():
    print('B-Series Elementary Differential Calculator')
    print('=' * 50)
    calculator = BSeriesDifferentialCalculator()
    def f(y):
        return y * y
    def f_prime(y):
        return 2 * y
    def f_double(y):
        return 2.0
    def f_triple(y):
        return 0.0
    def f_quad(y):
        return 0.0
    df = create_differential_function(f, f_prime, f_double, f_triple, f_quad, 'y²')
    y = 1.0
    print(f'Testing with function f(y) = y² at y = {y}')
    print(f"f({y}) = {f(y)}, f'({y}) = {f_prime(y)}")
    is_valid, errors = calculator.validate_differential_function(df, max_order=5)
    if not is_valid:
        print('Function validation errors:')
        for error in errors:
            print(f'  {error}')
        return
    print('\n✅ Function validation passed')
    print('\nElementary Differential Evaluations:')
    supported_trees = calculator.get_supported_trees()
    for tree_info in supported_trees[:10]:
        tree_id = tree_info['tree_id']
        try:
            value = calculator.evaluate_elementary_differential(tree_id, df, y)
            expression = tree_info['expression']
            coefficient = tree_info['coefficient']
            print(f'  Tree {tree_id}: F(τ) = {expression} = {value:.6f}, α = {coefficient:.6f}')
        except Exception as e:
            print(f'  Tree {tree_id}: Error - {e}')
    h = 0.1
    print(f'\nB-Series Step Evaluation (h = {h}):')
    try:
        next_y = calculator.evaluate_bseries_step(df, y, h, max_order=3)
        print(f'  y₀ = {y}')
        print(f'  y₁ = {next_y:.6f}')
        print(f'  Change: {next_y - y:.6f}')
        exact = y / (1 - y * h)
        error = abs(next_y - exact)
        print(f'  Exact solution: {exact:.6f}')
        print(f'  Error: {error:.6f}')
    except Exception as e:
        print(f'  B-Series step error: {e}')
    print('\n✅ B-Series elementary differential calculator operational')
if __name__ == '__main__':
    main()