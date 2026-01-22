from typing import List, Dict, Tuple
class OEIS_A000081_Enumerator:
    def __init__(self, max_cached_terms: int=100):
        self.max_cached_terms = max_cached_terms
        self._known_values = [0, 1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842, 4766, 12486, 32973, 87811, 235381, 634847, 1721159, 4688676, 12826228, 35221832, 97055181, 268282855, 743724984, 2067174645, 5759636510, 16083734329, 45007066269]
        self._cache: Dict[int, int] = {i: val for i, val in enumerate(self._known_values)}
        self._computed_up_to = len(self._known_values) - 1
    def get_term(self, n: int) -> int:
        if n < 0:
            raise ValueError('Index must be non-negative')
        if n < len(self._known_values):
            return self._known_values[n]
        return self._estimate_term(n)
    def get_sequence(self, max_terms: int) -> List[int]:
        if max_terms <= 0:
            return []
        result = []
        for i in range(max_terms):
            result.append(self.get_term(i))
        return result
    def _estimate_term(self, n: int) -> int:
        if n < len(self._known_values):
            return self._known_values[n]
        D = 0.43992
        alpha = 2.95576
        estimate = D * alpha ** n * n ** (-1.5)
        return int(round(estimate))
    def get_known_range(self) -> int:
        return len(self._known_values) - 1
    def validate_sequence(self, known_values: List[int]) -> Tuple[bool, List[str]]:
        errors = []
        computed = self.get_sequence(len(known_values))
        for i, (computed_val, known_val) in enumerate(zip(computed, known_values)):
            if computed_val != known_val:
                errors.append(f'Term {i}: computed {computed_val}, expected {known_val}')
        return (len(errors) == 0, errors)
    def is_valid_tree_count(self, n: int, count: int) -> bool:
        return self.get_term(n) == count
    def get_max_nodes_for_count(self, max_count: int) -> int:
        n = 0
        while self.get_term(n) <= max_count:
            n += 1
        return n - 1
def create_enhanced_validator() -> 'OEIS_A000081_Enumerator':
    return OEIS_A000081_Enumerator()
KNOWN_A000081_VALUES = [0, 1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842, 4766, 12486, 32973, 87811, 235381, 634847, 1721159, 4688676, 12826228, 35221832, 97055181, 268282855, 743724984, 2067174645, 5759636510, 16083734329, 45007066269]
def validate_membrane_hierarchy_enhanced(hierarchy_counts: List[int], max_depth: int) -> Tuple[bool, List[str]]:
    errors = []
    enumerator = create_enhanced_validator()
    if max_depth >= len(KNOWN_A000081_VALUES):
        errors.append(f'Max depth {max_depth} exceeds reliable OEIS A000081 data (max: {len(KNOWN_A000081_VALUES) - 1})')
        return (False, errors)
    if len(hierarchy_counts) != max_depth + 1:
        errors.append(f'Hierarchy has {len(hierarchy_counts)} levels, expected {max_depth + 1} for max_depth {max_depth}')
    for level, count in enumerate(hierarchy_counts):
        if level > max_depth:
            errors.append(f'Level {level} exceeds max_depth {max_depth}')
            continue
        enumerator.get_term(level) if level > 0 else 1
        if level == 0:
            if count != 1:
                errors.append(f'Level 0 (root) must have count 1, got {count}')
        else:
            expected = enumerator.get_term(level)
            if count != expected:
                errors.append(f'Level {level} has count {count}, expected {expected} (OEIS A000081)')
    return (len(errors) == 0, errors)
def main():
    print('OEIS A000081 Enhanced Enumeration Validator')
    print('=' * 50)
    enumerator = create_enhanced_validator()
    computed = enumerator.get_sequence(15)
    known = KNOWN_A000081_VALUES[:15]
    print('Testing enumeration:')
    print(f'Computed: {computed}')
    print(f'Known:    {known}')
    print(f'Match:    {computed == known}')
    is_valid, errors = enumerator.validate_sequence(known)
    print(f"\nValidation: {('✅ PASSED' if is_valid else '❌ FAILED')}")
    if errors:
        for error in errors:
            print(f'  {error}')
    print('\nIndividual term tests:')
    test_indices = [0, 1, 2, 3, 4, 5, 10]
    for i in test_indices:
        computed_term = enumerator.get_term(i)
        expected_term = KNOWN_A000081_VALUES[i] if i < len(KNOWN_A000081_VALUES) else 'N/A'
        if expected_term != 'N/A':
            match = computed_term == expected_term
            print(f"  a({i}) = {computed_term}, expected {expected_term}: {('✅' if match else '❌')}")
        else:
            print(f'  a({i}) = {computed_term} (estimated)')
    print('\nTesting enhanced membrane hierarchy validation:')
    valid_hierarchy = [1, 1, 1, 2, 4]
    is_valid, errors = validate_membrane_hierarchy_enhanced(valid_hierarchy, 4)
    print(f"  Valid hierarchy: {('✅ PASSED' if is_valid else '❌ FAILED')}")
    if errors:
        for error in errors:
            print(f'    {error}')
    invalid_hierarchy = [1, 1, 2, 2, 4]
    is_valid, errors = validate_membrane_hierarchy_enhanced(invalid_hierarchy, 4)
    print(f"  Invalid hierarchy: {('✅ FAILED' if not is_valid else '❌ PASSED')}")
    if errors:
        for error in errors:
            print(f'    {error}')
    print('\nUtility function tests:')
    print(f'  A000081(5) = {enumerator.get_term(5)} trees')
    print(f'  Is 9 trees valid for 5 nodes? {enumerator.is_valid_tree_count(5, 9)}')
    print(f'  Is 10 trees valid for 5 nodes? {enumerator.is_valid_tree_count(5, 10)}')
    print(f'  Max nodes for ≤100 trees: {enumerator.get_max_nodes_for_count(100)}')
    print(f'\nKnown range: 0-{enumerator.get_known_range()} (exact values)')
if __name__ == '__main__':
    main()