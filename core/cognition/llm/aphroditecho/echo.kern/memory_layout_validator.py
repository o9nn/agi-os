import sys
from typing import Dict, List, Tuple
from dataclasses import dataclass
from enum import Enum
try:
    from oeis_a000081_enumerator import create_enhanced_validator
    _oeis_validator = create_enhanced_validator()
    _HAS_OEIS_VALIDATOR = True
except ImportError:
    _oeis_validator = None
    _HAS_OEIS_VALIDATOR = False
class MemoryRegionType(Enum):
    DTESN_MEMBRANES = 'dtesn_membranes'
    ESN_RESERVOIRS = 'esn_reservoirs'
    B_SERIES_CACHE = 'b_series_cache'
    USER_SPACE = 'user_space'
    KERNEL_DIRECT = 'kernel_direct'
@dataclass
class MemoryRegion:
    name: str
    start_addr: int
    end_addr: int
    region_type: MemoryRegionType
    size_bytes: int = 0
    def __post_init__(self):
        if self.size_bytes == 0:
            self.size_bytes = self.end_addr - self.start_addr
@dataclass
class MembraneLevel:
    level: int
    expected_count: int
    base_addr: int
    level_size: int
    individual_size: int = 0
    def __post_init__(self):
        if self.individual_size == 0 and self.expected_count > 0:
            self.individual_size = self.level_size // self.expected_count
class DTESNMemoryValidator:
    DTESN_MEMBRANES_START = 18446612132314218496
    DTESN_MEMBRANES_END = 18446673704965373952
    ESN_RESERVOIRS_START = 18446673704965373952
    ESN_RESERVOIRS_END = 18446708889337462784
    B_SERIES_CACHE_START = 18446708889337462784
    B_SERIES_CACHE_END = 18446744073709551615
    USER_SPACE_START = 0
    USER_SPACE_END = 140737488355327
    KERNEL_DIRECT_START = 18446603336221196288
    KERNEL_DIRECT_END = 18446612132314218496
    MEMBRANE_LEVEL_ADDRS = {0: 18446612132314218496, 1: 18446612201033695232, 2: 18446612269753171968, 3: 18446612407192125440, 4: 18446613231825846272}
    MAX_MEMORY_ACCESS_NS = 100
    def __init__(self):
        self.regions = self._define_memory_regions()
        self.membrane_levels = self._define_membrane_levels()
    def _define_memory_regions(self) -> List[MemoryRegion]:
        return [MemoryRegion('User Space', self.USER_SPACE_START, self.USER_SPACE_END, MemoryRegionType.USER_SPACE), MemoryRegion('Kernel Direct Mapping', self.KERNEL_DIRECT_START, self.KERNEL_DIRECT_END, MemoryRegionType.KERNEL_DIRECT), MemoryRegion('DTESN Membranes', self.DTESN_MEMBRANES_START, self.DTESN_MEMBRANES_END, MemoryRegionType.DTESN_MEMBRANES), MemoryRegion('ESN Reservoirs', self.ESN_RESERVOIRS_START, self.ESN_RESERVOIRS_END, MemoryRegionType.ESN_RESERVOIRS), MemoryRegion('B-Series Cache', self.B_SERIES_CACHE_START, self.B_SERIES_CACHE_END, MemoryRegionType.B_SERIES_CACHE)]
    def _define_membrane_levels(self) -> List[MembraneLevel]:
        levels = []
        if _HAS_OEIS_VALIDATOR:
            for level in range(5):
                expected_count = _oeis_validator.get_term(level) if level > 0 else 1
                base_addr = self.MEMBRANE_LEVEL_ADDRS.get(level, 0)
                level_size = 17179869184
                levels.append(MembraneLevel(level=level, expected_count=expected_count, base_addr=base_addr, level_size=level_size))
        else:
            oeis_values = [1, 1, 1, 2, 4]
            for level, expected_count in enumerate(oeis_values):
                base_addr = self.MEMBRANE_LEVEL_ADDRS.get(level, 0)
                level_size = 17179869184
                levels.append(MembraneLevel(level=level, expected_count=expected_count, base_addr=base_addr, level_size=level_size))
        return levels
    def validate_address_space_partitioning(self) -> Tuple[bool, List[str]]:
        errors = []
        sorted_regions = sorted(self.regions, key=lambda r: r.start_addr)
        for i in range(len(sorted_regions) - 1):
            current = sorted_regions[i]
            next_region = sorted_regions[i + 1]
            if current.end_addr > next_region.start_addr:
                errors.append(f'Memory overlap: {current.name} (ends 0x{current.end_addr:016X}) overlaps with {next_region.name} (starts 0x{next_region.start_addr:016X})')
            elif current.end_addr < next_region.start_addr and (not (current.region_type == MemoryRegionType.USER_SPACE and next_region.region_type == MemoryRegionType.KERNEL_DIRECT)):
                gap_size = next_region.start_addr - current.end_addr
                errors.append(f'Memory gap: 0x{gap_size:016X} bytes between {current.name} and {next_region.name}')
        return (len(errors) == 0, errors)
    def validate_membrane_hierarchy(self) -> Tuple[bool, List[str]]:
        errors = []
        for membrane_level in self.membrane_levels:
            level = membrane_level.level
            expected_count = membrane_level.expected_count
            base_addr = membrane_level.base_addr
            if not self.DTESN_MEMBRANES_START <= base_addr < self.DTESN_MEMBRANES_END:
                errors.append(f'Membrane level {level} base address 0x{base_addr:016X} is outside DTESN membranes region')
            if _HAS_OEIS_VALIDATOR:
                oeis_expected = _oeis_validator.get_term(level) if level > 0 else 1
                if expected_count != oeis_expected:
                    errors.append(f"Membrane level {level} count {expected_count} doesn't match OEIS A000081 expected value {oeis_expected}")
            if membrane_level.individual_size == 0:
                errors.append(f'Membrane level {level} has zero individual membrane size')
            elif membrane_level.individual_size < 16777216:
                errors.append(f'Membrane level {level} individual size 0x{membrane_level.individual_size:X} is too small (< 16MB)')
        return (len(errors) == 0, errors)
    def validate_alignment_constraints(self) -> Tuple[bool, List[str]]:
        errors = []
        page_size = 4096
        for region in self.regions:
            if region.start_addr % page_size != 0:
                errors.append(f'Region {region.name} start address 0x{region.start_addr:016X} is not page-aligned (4KB)')
            if region.region_type in [MemoryRegionType.DTESN_MEMBRANES, MemoryRegionType.ESN_RESERVOIRS] and region.end_addr % page_size != 0:
                errors.append(f'Region {region.name} end address 0x{region.end_addr:016X} is not page-aligned (4KB)')
        membrane_alignment = 16777216
        for membrane_level in self.membrane_levels:
            if membrane_level.base_addr % membrane_alignment != 0:
                errors.append(f'Membrane level {membrane_level.level} base address 0x{membrane_level.base_addr:016X} is not aligned to 16MB boundary')
        return (len(errors) == 0, errors)
    def validate_size_constraints(self) -> Tuple[bool, List[str]]:
        errors = []
        min_sizes = {MemoryRegionType.DTESN_MEMBRANES: 3848290697216, MemoryRegionType.ESN_RESERVOIRS: 2199023255552, MemoryRegionType.B_SERIES_CACHE: 2199023255552}
        for region in self.regions:
            if region.region_type in min_sizes:
                min_size = min_sizes[region.region_type]
                if region.size_bytes < min_size:
                    errors.append(f'Region {region.name} size 0x{region.size_bytes:X} is below minimum 0x{min_size:X}')
        return (len(errors) == 0, errors)
    def validate_address_ranges(self) -> Tuple[bool, List[str]]:
        errors = []
        max_64bit_addr = 18446744073709551615
        for region in self.regions:
            if region.start_addr > max_64bit_addr:
                errors.append(f'Region {region.name} start address 0x{region.start_addr:016X} exceeds 64-bit address space')
            if region.end_addr > max_64bit_addr:
                errors.append(f'Region {region.name} end address 0x{region.end_addr:016X} exceeds 64-bit address space')
            if region.start_addr >= region.end_addr:
                errors.append(f'Region {region.name} has invalid address range: start 0x{region.start_addr:016X} >= end 0x{region.end_addr:016X}')
        return (len(errors) == 0, errors)
    def validate_full_layout(self) -> Tuple[bool, List[str]]:
        all_errors = []
        validation_checks = [('Address Space Partitioning', self.validate_address_space_partitioning), ('Membrane Hierarchy', self.validate_membrane_hierarchy), ('Alignment Constraints', self.validate_alignment_constraints), ('Size Constraints', self.validate_size_constraints), ('Address Ranges', self.validate_address_ranges)]
        for check_name, check_func in validation_checks:
            is_valid, errors = check_func()
            if not is_valid:
                all_errors.extend([f'[{check_name}] {error}' for error in errors])
        return (len(all_errors) == 0, all_errors)
    def get_memory_layout_summary(self) -> Dict:
        summary = {'total_address_space': '64-bit (0x0000000000000000 - 0xFFFFFFFFFFFFFFFF)', 'regions': [], 'membrane_levels': [], 'validation_status': 'unknown'}
        for region in self.regions:
            summary['regions'].append({'name': region.name, 'start': f'0x{region.start_addr:016X}', 'end': f'0x{region.end_addr:016X}', 'size_gb': region.size_bytes / 1024 ** 3, 'type': region.region_type.value})
        for level in self.membrane_levels:
            summary['membrane_levels'].append({'level': level.level, 'count': level.expected_count, 'base_addr': f'0x{level.base_addr:016X}', 'level_size_gb': level.level_size / 1024 ** 3, 'individual_size_mb': level.individual_size / 1024 ** 2 if level.individual_size else 0})
        is_valid, errors = self.validate_full_layout()
        summary['validation_status'] = 'valid' if is_valid else 'invalid'
        if not is_valid:
            summary['validation_errors'] = errors
        return summary
def create_memory_validator() -> DTESNMemoryValidator:
    return DTESNMemoryValidator()
def validate_custom_layout(regions: List[Tuple[str, int, int]]) -> Tuple[bool, List[str]]:
    errors = []
    memory_regions = []
    for name, start, end in regions:
        memory_regions.append(MemoryRegion(name, start, end, MemoryRegionType.USER_SPACE))
    sorted_regions = sorted(memory_regions, key=lambda r: r.start_addr)
    for i in range(len(sorted_regions) - 1):
        current = sorted_regions[i]
        next_region = sorted_regions[i + 1]
        if current.end_addr > next_region.start_addr:
            errors.append(f'Overlap: {current.name} and {next_region.name}')
    return (len(errors) == 0, errors)
if __name__ == '__main__':
    '\n    Command-line interface for memory layout validation\n    '
    import json
    print('DTESN Memory Layout Validation Tool')
    print('=' * 40)
    validator = create_memory_validator()
    summary = validator.get_memory_layout_summary()
    print(f"\nValidation Status: {summary['validation_status'].upper()}")
    print(f"\nMemory Regions ({len(summary['regions'])}):")
    for region in summary['regions']:
        print(f"  {region['name']:20} {region['start']} - {region['end']} ({region['size_gb']:.1f} GB)")
    print(f"\nMembrane Levels ({len(summary['membrane_levels'])}):")
    for level in summary['membrane_levels']:
        print(f"  Level {level['level']}: {level['count']:2d} membranes at {level['base_addr']} ({level['individual_size_mb']:.1f} MB each)")
    if 'validation_errors' in summary:
        print(f"\nValidation Errors ({len(summary['validation_errors'])}):")
        for error in summary['validation_errors']:
            print(f'  ❌ {error}')
    else:
        print('\n✅ All validation checks passed!')
    if len(sys.argv) > 1 and sys.argv[1] == '--json':
        with open('dtesn_memory_layout.json', 'w') as f:
            json.dump(summary, f, indent=2)
        print('\nMemory layout summary exported to dtesn_memory_layout.json')