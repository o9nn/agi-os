from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass
from enum import Enum
try:
    from oeis_a000081_enumerator import create_enhanced_validator
    _oeis_enumerator = create_enhanced_validator()
except ImportError:
    _oeis_enumerator = None
class TreeStructureType(Enum):
    SINGLE_NODE = 'single_node'
    LINEAR_CHAIN = 'linear_chain'
    STAR_GRAPH = 'star_graph'
    BINARY_TREE = 'binary_tree'
    GENERAL_TREE = 'general_tree'
@dataclass
class ElementaryDifferential:
    order: int
    expression: str
    computational_cost: float
    description: str
@dataclass
class BSeriesCoefficient:
    tree_order: int
    tree_id: int
    coefficient_value: float
    denominator: int
    computational_formula: str
@dataclass
class ClassifiedTree:
    tree_id: int
    order: int
    structure_type: TreeStructureType
    coefficient: BSeriesCoefficient
    elementary_diff: ElementaryDifferential
    symmetry_factor: int
    parent_tree_id: Optional[int] = None
class BSeriesTreeClassifier:
    def __init__(self):
        self.classified_trees: Dict[int, ClassifiedTree] = {}
        self.trees_by_order: Dict[int, List[ClassifiedTree]] = {}
        self.max_classified_order = 0
        self._initialize_basic_trees()
    def _initialize_basic_trees(self):
        tree_1 = ClassifiedTree(tree_id=1, order=1, structure_type=TreeStructureType.SINGLE_NODE, coefficient=BSeriesCoefficient(tree_order=1, tree_id=1, coefficient_value=1.0, denominator=1, computational_formula='1'), elementary_diff=ElementaryDifferential(order=1, expression='f', computational_cost=1.0, description='Function evaluation f(y)'), symmetry_factor=1)
        self._add_classified_tree(tree_1)
        tree_2 = ClassifiedTree(tree_id=2, order=2, structure_type=TreeStructureType.LINEAR_CHAIN, coefficient=BSeriesCoefficient(tree_order=2, tree_id=2, coefficient_value=0.5, denominator=2, computational_formula='1/2'), elementary_diff=ElementaryDifferential(order=2, expression="f'(f)", computational_cost=2.0, description='Derivative of f applied to f'), symmetry_factor=1)
        self._add_classified_tree(tree_2)
        tree_3a = ClassifiedTree(tree_id=3, order=3, structure_type=TreeStructureType.LINEAR_CHAIN, coefficient=BSeriesCoefficient(tree_order=3, tree_id=3, coefficient_value=1.0 / 3.0, denominator=3, computational_formula='1/3'), elementary_diff=ElementaryDifferential(order=3, expression="f''(f,f)", computational_cost=3.0, description='Second derivative of f with two f arguments'), symmetry_factor=1)
        self._add_classified_tree(tree_3a)
        tree_3b = ClassifiedTree(tree_id=4, order=3, structure_type=TreeStructureType.STAR_GRAPH, coefficient=BSeriesCoefficient(tree_order=3, tree_id=4, coefficient_value=1.0 / 6.0, denominator=6, computational_formula='1/6'), elementary_diff=ElementaryDifferential(order=3, expression="f'(f'(f))", computational_cost=3.5, description="Derivative of f applied to f'(f)"), symmetry_factor=2)
        self._add_classified_tree(tree_3b)
        self._initialize_order_4_trees()
        self._initialize_order_5_trees()
        self.max_classified_order = 5
    def _initialize_order_4_trees(self):
        tree_4a = ClassifiedTree(tree_id=5, order=4, structure_type=TreeStructureType.LINEAR_CHAIN, coefficient=BSeriesCoefficient(tree_order=4, tree_id=5, coefficient_value=1.0 / 4.0, denominator=4, computational_formula='1/4'), elementary_diff=ElementaryDifferential(order=4, expression="f'''(f,f,f)", computational_cost=4.0, description='Third derivative with three f arguments'), symmetry_factor=1)
        self._add_classified_tree(tree_4a)
        tree_4b = ClassifiedTree(tree_id=6, order=4, structure_type=TreeStructureType.GENERAL_TREE, coefficient=BSeriesCoefficient(tree_order=4, tree_id=6, coefficient_value=1.0 / 8.0, denominator=8, computational_formula='1/8'), elementary_diff=ElementaryDifferential(order=4, expression="f''(f'(f),f)", computational_cost=4.5, description='Mixed second/first derivative composition'), symmetry_factor=1)
        self._add_classified_tree(tree_4b)
        tree_4c = ClassifiedTree(tree_id=7, order=4, structure_type=TreeStructureType.STAR_GRAPH, coefficient=BSeriesCoefficient(tree_order=4, tree_id=7, coefficient_value=1.0 / 24.0, denominator=24, computational_formula='1/24'), elementary_diff=ElementaryDifferential(order=4, expression="f'''(f,f,f)", computational_cost=5.0, description='Third derivative with symmetric arguments'), symmetry_factor=6)
        self._add_classified_tree(tree_4c)
        tree_4d = ClassifiedTree(tree_id=8, order=4, structure_type=TreeStructureType.GENERAL_TREE, coefficient=BSeriesCoefficient(tree_order=4, tree_id=8, coefficient_value=1.0 / 12.0, denominator=12, computational_formula='1/12'), elementary_diff=ElementaryDifferential(order=4, expression="f'(f''(f,f))", computational_cost=4.5, description='First derivative of second derivative composition'), symmetry_factor=1)
        self._add_classified_tree(tree_4d)
    def _initialize_order_5_trees(self):
        tree_5a = ClassifiedTree(tree_id=9, order=5, structure_type=TreeStructureType.LINEAR_CHAIN, coefficient=BSeriesCoefficient(tree_order=5, tree_id=9, coefficient_value=1.0 / 5.0, denominator=5, computational_formula='1/5'), elementary_diff=ElementaryDifferential(order=5, expression="f''''(f,f,f,f)", computational_cost=5.0, description='Fourth derivative with four f arguments'), symmetry_factor=1)
        self._add_classified_tree(tree_5a)
        tree_5b = ClassifiedTree(tree_id=10, order=5, structure_type=TreeStructureType.STAR_GRAPH, coefficient=BSeriesCoefficient(tree_order=5, tree_id=10, coefficient_value=1.0 / 120.0, denominator=120, computational_formula='1/120'), elementary_diff=ElementaryDifferential(order=5, expression="f''''(f,f,f,f)", computational_cost=6.0, description='Fourth derivative star configuration'), symmetry_factor=24)
        self._add_classified_tree(tree_5b)
        tree_5c = ClassifiedTree(tree_id=11, order=5, structure_type=TreeStructureType.GENERAL_TREE, coefficient=BSeriesCoefficient(tree_order=5, tree_id=11, coefficient_value=1.0 / 20.0, denominator=20, computational_formula='1/20'), elementary_diff=ElementaryDifferential(order=5, expression="f'''(f''(f,f),f)", computational_cost=5.5, description='Third derivative with mixed arguments'), symmetry_factor=1)
        self._add_classified_tree(tree_5c)
        tree_5d = ClassifiedTree(tree_id=12, order=5, structure_type=TreeStructureType.GENERAL_TREE, coefficient=BSeriesCoefficient(tree_order=5, tree_id=12, coefficient_value=1.0 / 30.0, denominator=30, computational_formula='1/30'), elementary_diff=ElementaryDifferential(order=5, expression="f''(f'''(f,f,f),f)", computational_cost=6.0, description='Second derivative with third derivative input'), symmetry_factor=1)
        self._add_classified_tree(tree_5d)
        tree_5e = ClassifiedTree(tree_id=13, order=5, structure_type=TreeStructureType.BINARY_TREE, coefficient=BSeriesCoefficient(tree_order=5, tree_id=13, coefficient_value=1.0 / 40.0, denominator=40, computational_formula='1/40'), elementary_diff=ElementaryDifferential(order=5, expression="f'(f''(f'(f),f))", computational_cost=5.5, description='Nested derivative composition'), symmetry_factor=1)
        self._add_classified_tree(tree_5e)
        tree_5f = ClassifiedTree(tree_id=14, order=5, structure_type=TreeStructureType.STAR_GRAPH, coefficient=BSeriesCoefficient(tree_order=5, tree_id=14, coefficient_value=1.0 / 60.0, denominator=60, computational_formula='1/60'), elementary_diff=ElementaryDifferential(order=5, expression="f'''(f,f,f'(f))", computational_cost=6.5, description='Third derivative with nested first derivative'), symmetry_factor=2)
        self._add_classified_tree(tree_5f)
        tree_5g = ClassifiedTree(tree_id=15, order=5, structure_type=TreeStructureType.GENERAL_TREE, coefficient=BSeriesCoefficient(tree_order=5, tree_id=15, coefficient_value=1.0 / 24.0, denominator=24, computational_formula='1/24'), elementary_diff=ElementaryDifferential(order=5, expression="f'(f''''(f,f,f,f))", computational_cost=6.0, description='First derivative of fourth-order composition'), symmetry_factor=1)
        self._add_classified_tree(tree_5g)
        tree_5h = ClassifiedTree(tree_id=16, order=5, structure_type=TreeStructureType.BINARY_TREE, coefficient=BSeriesCoefficient(tree_order=5, tree_id=16, coefficient_value=1.0 / 80.0, denominator=80, computational_formula='1/80'), elementary_diff=ElementaryDifferential(order=5, expression="f'(f''(f,f,f'(f)))", computational_cost=7.0, description='Complex nested derivative structure'), symmetry_factor=2)
        self._add_classified_tree(tree_5h)
        tree_5i = ClassifiedTree(tree_id=17, order=5, structure_type=TreeStructureType.GENERAL_TREE, coefficient=BSeriesCoefficient(tree_order=5, tree_id=17, coefficient_value=1.0 / 48.0, denominator=48, computational_formula='1/48'), elementary_diff=ElementaryDifferential(order=5, expression="f'(f''(f'(f),f))", computational_cost=6.5, description='Mixed composition with multiple nesting levels'), symmetry_factor=1)
        self._add_classified_tree(tree_5i)
    def _add_classified_tree(self, tree: ClassifiedTree):
        self.classified_trees[tree.tree_id] = tree
        if tree.order not in self.trees_by_order:
            self.trees_by_order[tree.order] = []
        self.trees_by_order[tree.order].append(tree)
    def get_trees_by_order(self, order: int) -> List[ClassifiedTree]:
        return self.trees_by_order.get(order, [])
    def get_tree_by_id(self, tree_id: int) -> Optional[ClassifiedTree]:
        return self.classified_trees.get(tree_id)
    def classify_tree_structure(self, order: int, structure_hint: str=None) -> TreeStructureType:
        if order == 1:
            return TreeStructureType.SINGLE_NODE
        elif order == 2:
            return TreeStructureType.LINEAR_CHAIN
        elif order == 3:
            if structure_hint == 'star':
                return TreeStructureType.STAR_GRAPH
            else:
                return TreeStructureType.LINEAR_CHAIN
        elif order == 4:
            if structure_hint == 'star':
                return TreeStructureType.STAR_GRAPH
            else:
                return TreeStructureType.GENERAL_TREE
        else:
            return TreeStructureType.GENERAL_TREE
    def calculate_bseries_coefficient(self, tree_id: int) -> Optional[BSeriesCoefficient]:
        tree = self.get_tree_by_id(tree_id)
        if tree:
            return tree.coefficient
        return None
    def get_elementary_differential(self, tree_id: int) -> Optional[ElementaryDifferential]:
        tree = self.get_tree_by_id(tree_id)
        if tree:
            return tree.elementary_diff
        return None
    def validate_against_oeis_a000081(self) -> Tuple[bool, List[str]]:
        errors = []
        if _oeis_enumerator is None:
            errors.append('OEIS A000081 enumerator not available for validation')
            return (False, errors)
        for order in range(1, self.max_classified_order + 1):
            classified_count = len(self.get_trees_by_order(order))
            expected_count = _oeis_enumerator.get_term(order) if order > 0 else 1
            if order == 0:
                continue
            if classified_count != expected_count:
                errors.append(f'Order {order}: classified {classified_count} trees, OEIS A000081 expects {expected_count}')
        return (len(errors) == 0, errors)
    def get_computational_cost_summary(self) -> Dict[int, float]:
        costs = {}
        for order in range(1, self.max_classified_order + 1):
            trees = self.get_trees_by_order(order)
            if trees:
                total_cost = sum((tree.elementary_diff.computational_cost for tree in trees))
                costs[order] = total_cost
        return costs
    def get_classification_statistics(self) -> Dict[str, int]:
        stats = {'total_trees': len(self.classified_trees), 'max_order': self.max_classified_order, 'single_node_count': 0, 'linear_chain_count': 0, 'star_graph_count': 0, 'binary_tree_count': 0, 'general_tree_count': 0}
        for tree in self.classified_trees.values():
            if tree.structure_type == TreeStructureType.SINGLE_NODE:
                stats['single_node_count'] += 1
            elif tree.structure_type == TreeStructureType.LINEAR_CHAIN:
                stats['linear_chain_count'] += 1
            elif tree.structure_type == TreeStructureType.STAR_GRAPH:
                stats['star_graph_count'] += 1
            elif tree.structure_type == TreeStructureType.BINARY_TREE:
                stats['binary_tree_count'] += 1
            elif tree.structure_type == TreeStructureType.GENERAL_TREE:
                stats['general_tree_count'] += 1
        return stats
def create_bseries_classifier() -> BSeriesTreeClassifier:
    return BSeriesTreeClassifier()
def main():
    print('B-Series Tree Classification System')
    print('=' * 50)
    classifier = create_bseries_classifier()
    stats = classifier.get_classification_statistics()
    print('Classification Statistics:')
    print(f"  Total trees classified: {stats['total_trees']}")
    print(f"  Maximum order: {stats['max_order']}")
    print('  Structure types:')
    print(f"    Single nodes: {stats['single_node_count']}")
    print(f"    Linear chains: {stats['linear_chain_count']}")
    print(f"    Star graphs: {stats['star_graph_count']}")
    print(f"    General trees: {stats['general_tree_count']}")
    print('\nTrees by Order:')
    for order in range(1, stats['max_order'] + 1):
        trees = classifier.get_trees_by_order(order)
        print(f'  Order {order}: {len(trees)} trees')
        for tree in trees:
            coeff = tree.coefficient.coefficient_value
            expr = tree.elementary_diff.expression
            print(f'    Tree {tree.tree_id}: α={coeff:.4f}, F(τ)={expr}')
    print('\nOEIS A000081 Validation:')
    is_valid, errors = classifier.validate_against_oeis_a000081()
    if is_valid:
        print('  ✅ All tree counts match OEIS A000081')
    else:
        print('  ❌ Validation errors:')
        for error in errors:
            print(f'    {error}')
    print('\nComputational Cost Summary:')
    costs = classifier.get_computational_cost_summary()
    for order, cost in costs.items():
        print(f'  Order {order}: Total cost {cost:.2f}')
    print('\n✅ B-Series tree classification system operational')
if __name__ == '__main__':
    main()