import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from memory_layout_validator import MemoryRegionType, create_memory_validator, validate_custom_layout
def test_memory_validator_creation():
    print('Testing memory validator creation...')
    validator = create_memory_validator()
    assert validator is not None, 'Should create validator instance'
    assert len(validator.regions) > 0, 'Should have memory regions defined'
    assert len(validator.membrane_levels) > 0, 'Should have membrane levels defined'
    print('✅ Memory validator creation test passed')
def test_memory_regions_definition():
    print('Testing memory regions definition...')
    validator = create_memory_validator()
    region_types = {region.region_type for region in validator.regions}
    required_types = {MemoryRegionType.USER_SPACE, MemoryRegionType.KERNEL_DIRECT, MemoryRegionType.DTESN_MEMBRANES, MemoryRegionType.ESN_RESERVOIRS, MemoryRegionType.B_SERIES_CACHE}
    assert required_types.issubset(region_types), f'Missing required region types: {required_types - region_types}'
    dtesn_regions = [r for r in validator.regions if r.region_type == MemoryRegionType.DTESN_MEMBRANES]
    assert len(dtesn_regions) == 1, 'Should have exactly one DTESN membranes region'
    dtesn_region = dtesn_regions[0]
    assert dtesn_region.start_addr == 18446612132314218496, 'DTESN membranes should start at correct address'
    assert dtesn_region.end_addr == 18446673704965373952, 'DTESN membranes should end at correct address'
    print('✅ Memory regions definition test passed')
def test_membrane_hierarchy():
    print('Testing membrane hierarchy...')
    validator = create_memory_validator()
    assert len(validator.membrane_levels) == 5, 'Should have 5 membrane levels'
    expected_counts = [1, 1, 1, 2, 4]
    actual_counts = [level.expected_count for level in validator.membrane_levels]
    assert actual_counts == expected_counts, f"Membrane counts {actual_counts} don't match OEIS A000081 {expected_counts}"
    for i in range(len(validator.membrane_levels) - 1):
        current_level = validator.membrane_levels[i]
        next_level = validator.membrane_levels[i + 1]
        assert current_level.base_addr < next_level.base_addr, f'Level {i} address should be less than level {i + 1}'
    print('✅ Membrane hierarchy test passed')
def test_address_space_partitioning_validation():
    print('Testing address space partitioning validation...')
    validator = create_memory_validator()
    is_valid, errors = validator.validate_address_space_partitioning()
    assert is_valid, f'Default address space partitioning should be valid. Errors: {errors}'
    print('✅ Address space partitioning validation test passed')
def test_membrane_hierarchy_validation():
    print('Testing membrane hierarchy validation...')
    validator = create_memory_validator()
    is_valid, errors = validator.validate_membrane_hierarchy()
    assert is_valid, f'Default membrane hierarchy should be valid. Errors: {errors}'
    print('✅ Membrane hierarchy validation test passed')
def test_alignment_constraints_validation():
    print('Testing alignment constraints validation...')
    validator = create_memory_validator()
    is_valid, errors = validator.validate_alignment_constraints()
    assert is_valid, f'Default layout should meet alignment constraints. Errors: {errors}'
    print('✅ Alignment constraints validation test passed')
def test_size_constraints_validation():
    print('Testing size constraints validation...')
    validator = create_memory_validator()
    is_valid, errors = validator.validate_size_constraints()
    assert is_valid, f'Default layout should meet size constraints. Errors: {errors}'
    print('✅ Size constraints validation test passed')
def test_address_ranges_validation():
    print('Testing address ranges validation...')
    validator = create_memory_validator()
    is_valid, errors = validator.validate_address_ranges()
    assert is_valid, f'Default layout should have valid address ranges. Errors: {errors}'
    print('✅ Address ranges validation test passed')
def test_full_layout_validation():
    print('Testing full layout validation...')
    validator = create_memory_validator()
    is_valid, errors = validator.validate_full_layout()
    assert is_valid, f'Default layout should be fully valid. Errors: {errors}'
    print('✅ Full layout validation test passed')
def test_memory_layout_summary():
    print('Testing memory layout summary...')
    validator = create_memory_validator()
    summary = validator.get_memory_layout_summary()
    required_keys = ['total_address_space', 'regions', 'membrane_levels', 'validation_status']
    for key in required_keys:
        assert key in summary, f"Summary should contain '{key}'"
    assert len(summary['regions']) == len(validator.regions), 'Summary should include all regions'
    assert len(summary['membrane_levels']) == len(validator.membrane_levels), 'Summary should include all membrane levels'
    assert summary['validation_status'] in ['valid', 'invalid'], 'Should have validation status'
    print('✅ Memory layout summary test passed')
def test_custom_layout_validation():
    print('Testing custom layout validation...')
    valid_regions = [('Region A', 4096, 8192), ('Region B', 8192, 12288), ('Region C', 16384, 20480)]
    is_valid, errors = validate_custom_layout(valid_regions)
    assert is_valid, f'Valid custom layout should pass validation. Errors: {errors}'
    invalid_regions = [('Region A', 4096, 9472), ('Region B', 8192, 12288), ('Region C', 16384, 20480)]
    is_valid, errors = validate_custom_layout(invalid_regions)
    assert not is_valid, 'Invalid custom layout should fail validation'
    assert len(errors) > 0, 'Should have validation errors for overlapping regions'
    assert 'Overlap' in errors[0], 'Should detect overlap error'
    print('✅ Custom layout validation test passed')
def test_error_detection():
    print('Testing error detection...')
    validator = create_memory_validator()
    validator.membrane_levels[2].expected_count = 999
    is_valid, errors = validator.validate_membrane_hierarchy()
    assert not is_valid, 'Should detect invalid membrane count'
    assert len(errors) > 0, 'Should have validation errors'
    error_found = any(('count' in error and '999' in error for error in errors))
    assert error_found, f'Should detect count error in: {errors}'
    print('✅ Error detection test passed')
def test_performance_constraints():
    print('Testing performance constraints...')
    validator = create_memory_validator()
    assert validator.MAX_MEMORY_ACCESS_NS == 100, 'Should have correct memory access time constraint'
    for level in validator.membrane_levels:
        if level.expected_count > 0:
            assert level.individual_size > 0, f'Level {level.level} should have positive individual size'
            assert level.individual_size >= 16777216, f'Level {level.level} individual size too small'
    print('✅ Performance constraints test passed')
def test_oeis_integration():
    print('Testing OEIS integration...')
    validator = create_memory_validator()
    try:
        from oeis_a000081_enumerator import create_enhanced_validator
        oeis_validator = create_enhanced_validator()
        for level in validator.membrane_levels:
            expected_oeis = oeis_validator.get_term(level.level) if level.level > 0 else 1
            assert level.expected_count == expected_oeis, f'Level {level.level} count {level.expected_count} should match OEIS A000081 {expected_oeis}'
        print('✅ OEIS integration test passed (enhanced validator available)')
    except ImportError:
        expected_counts = [1, 1, 1, 2, 4]
        actual_counts = [level.expected_count for level in validator.membrane_levels]
        assert actual_counts == expected_counts, 'Should match hardcoded OEIS A000081 values'
        print('✅ OEIS integration test passed (using fallback values)')
def test_memory_constants():
    print('Testing memory constants...')
    validator = create_memory_validator()
    assert validator.DTESN_MEMBRANES_START == 18446612132314218496, 'DTESN membranes start address'
    assert validator.DTESN_MEMBRANES_END == 18446673704965373952, 'DTESN membranes end address'
    assert validator.ESN_RESERVOIRS_START == 18446673704965373952, 'ESN reservoirs start address'
    assert validator.ESN_RESERVOIRS_END == 18446708889337462784, 'ESN reservoirs end address'
    assert validator.B_SERIES_CACHE_START == 18446708889337462784, 'B-Series cache start address'
    assert validator.B_SERIES_CACHE_END == 18446744073709551615, 'B-Series cache end address'
    expected_addrs = {0: 18446612132314218496, 1: 18446612201033695232, 2: 18446612269753171968, 3: 18446612407192125440, 4: 18446613231825846272}
    for level, expected_addr in expected_addrs.items():
        assert validator.MEMBRANE_LEVEL_ADDRS[level] == expected_addr, f'Membrane level {level} address should be 0x{expected_addr:016X}'
    print('✅ Memory constants test passed')
def run_all_tests():
    print('DTESN Memory Layout Validation Test Suite')
    print('=' * 50)
    tests = [test_memory_validator_creation, test_memory_regions_definition, test_membrane_hierarchy, test_address_space_partitioning_validation, test_membrane_hierarchy_validation, test_alignment_constraints_validation, test_size_constraints_validation, test_address_ranges_validation, test_full_layout_validation, test_memory_layout_summary, test_custom_layout_validation, test_error_detection, test_performance_constraints, test_oeis_integration, test_memory_constants]
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
        print('🎉 All tests passed! Memory layout validation tools are working correctly.')
        return True
    else:
        print('💥 Some tests failed. Please check the implementation.')
        return False
if __name__ == '__main__':
    success = run_all_tests()
    sys.exit(0 if success else 1)