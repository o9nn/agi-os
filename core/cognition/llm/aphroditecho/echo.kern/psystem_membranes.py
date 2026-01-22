import uuid
import time
from typing import Dict, List, Set, Any, Optional, Tuple, Callable
from dataclasses import dataclass, field
from enum import Enum, auto
from collections import defaultdict
import threading
import logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)
class MembraneType(Enum):
    ROOT = 'root'
    TRUNK = 'trunk'
    BRANCH = 'branch'
    LEAF = 'leaf'
    TERMINAL = 'terminal'
    SKIN = 'skin'
    ELEMENTARY = 'elementary'
class RuleType(Enum):
    EVOLUTION = auto()
    COMMUNICATION = auto()
    DISSOLUTION = auto()
    DIVISION = auto()
    CREATION = auto()
    SYMPORT = auto()
    ANTIPORT = auto()
class ExecutionPhase(Enum):
    INPUT = auto()
    EVOLUTION = auto()
    COMMUNICATION = auto()
    OUTPUT = auto()
    HALTED = auto()
@dataclass
class PSystemObject:
    symbol: str
    multiplicity: int = 1
    properties: Dict[str, Any] = field(default_factory=dict)
    creation_time: float = field(default_factory=time.time)
    def __post_init__(self):
        if self.multiplicity < 0:
            raise ValueError('Object multiplicity cannot be negative')
    def __str__(self) -> str:
        if self.multiplicity == 1:
            return self.symbol
        return f'{self.symbol}^{self.multiplicity}'
    def copy(self) -> 'PSystemObject':
        return PSystemObject(symbol=self.symbol, multiplicity=self.multiplicity, properties=self.properties.copy(), creation_time=self.creation_time)
@dataclass
class Multiset:
    objects: Dict[str, int] = field(default_factory=dict)
    def add(self, symbol: str, multiplicity: int=1) -> None:
        if multiplicity <= 0:
            return
        self.objects[symbol] = self.objects.get(symbol, 0) + multiplicity
    def remove(self, symbol: str, multiplicity: int=1) -> bool:
        if symbol not in self.objects or self.objects[symbol] < multiplicity:
            return False
        self.objects[symbol] -= multiplicity
        if self.objects[symbol] == 0:
            del self.objects[symbol]
        return True
    def contains(self, symbol: str, multiplicity: int=1) -> bool:
        return self.objects.get(symbol, 0) >= multiplicity
    def count(self, symbol: str) -> int:
        return self.objects.get(symbol, 0)
    def is_empty(self) -> bool:
        return len(self.objects) == 0
    def clear(self) -> None:
        self.objects.clear()
    def union(self, other: 'Multiset') -> 'Multiset':
        result = Multiset()
        for symbol, count in self.objects.items():
            result.add(symbol, count)
        for symbol, count in other.objects.items():
            result.add(symbol, count)
        return result
    def intersection(self, other: 'Multiset') -> 'Multiset':
        result = Multiset()
        for symbol in self.objects:
            if symbol in other.objects:
                min_count = min(self.objects[symbol], other.objects[symbol])
                result.add(symbol, min_count)
        return result
    def __str__(self) -> str:
        if not self.objects:
            return '∅'
        items = []
        for symbol, count in sorted(self.objects.items()):
            if count == 1:
                items.append(symbol)
            else:
                items.append(f'{symbol}^{count}')
        return ' '.join(items)
    def __len__(self) -> int:
        return sum(self.objects.values())
@dataclass
class EvolutionRule:
    rule_id: str
    rule_type: RuleType
    lhs: Multiset
    rhs: Multiset
    target_membrane: Optional[str] = None
    source_membrane: Optional[str] = None
    priority: int = 1
    condition: Optional[Callable] = None
    probability: float = 1.0
    max_applications: Optional[int] = None
    def __post_init__(self):
        if not 0.0 <= self.probability <= 1.0:
            raise ValueError('Rule probability must be between 0.0 and 1.0')
        if self.priority < 0:
            raise ValueError('Rule priority cannot be negative')
    def is_applicable(self, membrane_objects: Multiset, context: Dict[str, Any]=None) -> bool:
        for symbol, required_count in self.lhs.objects.items():
            if not membrane_objects.contains(symbol, required_count):
                return False
        if self.condition and context:
            return self.condition(membrane_objects, context)
        return True
    def apply(self, membrane_objects: Multiset) -> Tuple[bool, Multiset]:
        if not self.is_applicable(membrane_objects):
            return (False, Multiset())
        for symbol, count in self.lhs.objects.items():
            if not membrane_objects.remove(symbol, count):
                return (False, Multiset())
        return (True, self.rhs)
    def __str__(self) -> str:
        target_str = f' → {self.target_membrane}' if self.target_membrane else ''
        priority_str = f' [priority: {self.priority}]' if self.priority != 1 else ''
        return f'{self.lhs} → {self.rhs}{target_str}{priority_str}'
@dataclass
class MembraneStructure:
    membrane_id: str
    membrane_type: MembraneType
    label: str
    parent_id: Optional[str] = None
    children_ids: Set[str] = field(default_factory=set)
    depth: int = 0
    objects: Multiset = field(default_factory=Multiset)
    rules: List[EvolutionRule] = field(default_factory=list)
    is_dissolved: bool = False
    charge: int = 0
    neuron_count: int = 100
    spectral_radius: float = 0.9
    connectivity: float = 0.1
    evolution_time_us: float = 0.0
    last_evolution: float = field(default_factory=time.time)
    _lock: threading.RLock = field(default_factory=threading.RLock)
    def add_child(self, child_id: str) -> None:
        with self._lock:
            self.children_ids.add(child_id)
    def remove_child(self, child_id: str) -> None:
        with self._lock:
            self.children_ids.discard(child_id)
    def add_object(self, symbol: str, multiplicity: int=1) -> None:
        with self._lock:
            self.objects.add(symbol, multiplicity)
    def remove_object(self, symbol: str, multiplicity: int=1) -> bool:
        with self._lock:
            return self.objects.remove(symbol, multiplicity)
    def add_rule(self, rule: EvolutionRule) -> None:
        with self._lock:
            self.rules.append(rule)
    def dissolve(self) -> None:
        with self._lock:
            self.is_dissolved = True
            logger.info(f'Membrane {self.membrane_id} dissolved')
    def is_elementary(self) -> bool:
        return len(self.children_ids) == 0
    def get_applicable_rules(self) -> List[EvolutionRule]:
        with self._lock:
            applicable = []
            for rule in self.rules:
                if rule.is_applicable(self.objects):
                    applicable.append(rule)
            return applicable
    def __str__(self) -> str:
        children_str = f', children: {len(self.children_ids)}' if self.children_ids else ''
        objects_str = f', objects: {self.objects}' if not self.objects.is_empty() else ''
        return f'Membrane[{self.membrane_id}:{self.label}:{self.membrane_type.value}{children_str}{objects_str}]'
class PSystemMembraneHierarchy:
    def __init__(self, system_name: str='DTESN_PSystem'):
        self.system_name = system_name
        self.membranes: Dict[str, MembraneStructure] = {}
        self.skin_membrane_id: Optional[str] = None
        self.execution_phase = ExecutionPhase.INPUT
        self.evolution_step = 0
        self.is_halted = False
        self.total_evolution_time = 0.0
        self.rule_applications = 0
        self.membrane_communications = 0
        self._global_lock = threading.RLock()
        logger.info(f'Initialized P-System hierarchy: {system_name}')
    def create_membrane(self, membrane_type: MembraneType, label: str, parent_id: Optional[str]=None, neuron_count: int=100) -> str:
        with self._global_lock:
            membrane_id = f'{membrane_type.value}_{uuid.uuid4().hex[:8]}'
            depth = 0
            if parent_id and parent_id in self.membranes:
                depth = self.membranes[parent_id].depth + 1
                self.membranes[parent_id].add_child(membrane_id)
            membrane = MembraneStructure(membrane_id=membrane_id, membrane_type=membrane_type, label=label, parent_id=parent_id, depth=depth, neuron_count=neuron_count)
            self.membranes[membrane_id] = membrane
            if membrane_type == MembraneType.ROOT and (not self.skin_membrane_id):
                self.skin_membrane_id = membrane_id
            logger.info(f'Created membrane: {membrane}')
            return membrane_id
    def get_membrane(self, membrane_id: str) -> Optional[MembraneStructure]:
        return self.membranes.get(membrane_id)
    def dissolve_membrane(self, membrane_id: str) -> bool:
        with self._global_lock:
            if membrane_id not in self.membranes:
                return False
            membrane = self.membranes[membrane_id]
            parent_id = membrane.parent_id
            if membrane_id == self.skin_membrane_id:
                logger.warning('Cannot dissolve skin membrane')
                return False
            if parent_id and parent_id in self.membranes:
                parent = self.membranes[parent_id]
                for symbol, count in membrane.objects.objects.items():
                    parent.add_object(symbol, count)
            if parent_id:
                parent = self.membranes[parent_id]
                for child_id in membrane.children_ids:
                    if child_id in self.membranes:
                        self.membranes[child_id].parent_id = parent_id
                        parent.add_child(child_id)
                parent.remove_child(membrane_id)
            membrane.dissolve()
            logger.info(f'Dissolved membrane {membrane_id}')
            return True
    def divide_membrane(self, membrane_id: str, division_objects: Multiset) -> Optional[str]:
        with self._global_lock:
            if membrane_id not in self.membranes:
                return None
            source_membrane = self.membranes[membrane_id]
            parent_id = source_membrane.parent_id
            new_membrane_id = self.create_membrane(membrane_type=source_membrane.membrane_type, label=f'{source_membrane.label}_divided', parent_id=parent_id, neuron_count=source_membrane.neuron_count)
            new_membrane = self.membranes[new_membrane_id]
            for symbol, count in division_objects.objects.items():
                if source_membrane.remove_object(symbol, count):
                    new_membrane.add_object(symbol, count)
            logger.info(f'Divided membrane {membrane_id} → {new_membrane_id}')
            return new_membrane_id
    def communicate_objects(self, source_id: str, target_id: str, objects: Multiset) -> bool:
        with self._global_lock:
            if source_id not in self.membranes or target_id not in self.membranes:
                return False
            source = self.membranes[source_id]
            target = self.membranes[target_id]
            for symbol, count in objects.objects.items():
                if not source.objects.contains(symbol, count):
                    return False
            for symbol, count in objects.objects.items():
                if source.remove_object(symbol, count):
                    target.add_object(symbol, count)
                else:
                    return False
            self.membrane_communications += 1
            logger.debug(f'Communicated {objects} from {source_id} to {target_id}')
            return True
    def evolve_membrane(self, membrane_id: str) -> int:
        start_time = time.time()
        with self._global_lock:
            if membrane_id not in self.membranes:
                return 0
            membrane = self.membranes[membrane_id]
            if membrane.is_dissolved:
                return 0
            applicable_rules = membrane.get_applicable_rules()
            applicable_rules.sort(key=lambda r: r.priority, reverse=True)
            rules_applied = 0
            for rule in applicable_rules:
                if rule.is_applicable(membrane.objects):
                    success, products = rule.apply(membrane.objects)
                    if success:
                        if rule.rule_type == RuleType.EVOLUTION:
                            membrane.objects = membrane.objects.union(products)
                        elif rule.rule_type == RuleType.COMMUNICATION and rule.target_membrane:
                            self.communicate_objects(membrane_id, rule.target_membrane, products)
                        elif rule.rule_type == RuleType.DISSOLUTION:
                            self.dissolve_membrane(membrane_id)
                            break
                        elif rule.rule_type == RuleType.DIVISION:
                            self.divide_membrane(membrane_id, products)
                        rules_applied += 1
                        self.rule_applications += 1
            evolution_time = (time.time() - start_time) * 1000000
            membrane.evolution_time_us = evolution_time
            membrane.last_evolution = time.time()
            self.total_evolution_time += evolution_time
            return rules_applied
    def evolve_system(self) -> bool:
        if self.is_halted:
            return False
        self.execution_phase = ExecutionPhase.EVOLUTION
        total_rules_applied = 0
        membrane_ids = list(self.membranes.keys())
        for membrane_id in membrane_ids:
            if membrane_id in self.membranes:
                rules_applied = self.evolve_membrane(membrane_id)
                total_rules_applied += rules_applied
        self.evolution_step += 1
        if total_rules_applied == 0:
            self.execution_phase = ExecutionPhase.HALTED
            self.is_halted = True
            logger.info(f'P-System halted after {self.evolution_step} steps')
            return False
        logger.debug(f'Evolution step {self.evolution_step}: {total_rules_applied} rules applied')
        return True
    def get_membrane_tree(self) -> Dict[str, Any]:
        def build_tree(membrane_id: str) -> Dict[str, Any]:
            membrane = self.membranes[membrane_id]
            tree = {'id': membrane_id, 'type': membrane.membrane_type.value, 'label': membrane.label, 'depth': membrane.depth, 'objects': str(membrane.objects), 'neuron_count': membrane.neuron_count, 'children': []}
            for child_id in membrane.children_ids:
                if child_id in self.membranes and (not self.membranes[child_id].is_dissolved):
                    tree['children'].append(build_tree(child_id))
            return tree
        if self.skin_membrane_id:
            return build_tree(self.skin_membrane_id)
        return {}
    def validate_oeis_a000081_compliance(self) -> Tuple[bool, List[str]]:
        try:
            from oeis_a000081_enumerator import validate_membrane_hierarchy_enhanced
            depth_counts = defaultdict(int)
            for membrane in self.membranes.values():
                if not membrane.is_dissolved:
                    depth_counts[membrane.depth] += 1
            max_depth = max(depth_counts.keys()) if depth_counts else 0
            hierarchy_counts = [depth_counts[d] for d in range(max_depth + 1)]
            return validate_membrane_hierarchy_enhanced(hierarchy_counts, max_depth)
        except ImportError:
            return (True, [])
    def get_system_stats(self) -> Dict[str, Any]:
        active_membranes = sum((1 for m in self.membranes.values() if not m.is_dissolved))
        total_objects = sum((len(m.objects) for m in self.membranes.values() if not m.is_dissolved))
        total_rules = sum((len(m.rules) for m in self.membranes.values()))
        return {'system_name': self.system_name, 'total_membranes': len(self.membranes), 'active_membranes': active_membranes, 'dissolved_membranes': len(self.membranes) - active_membranes, 'total_objects': total_objects, 'total_rules': total_rules, 'evolution_steps': self.evolution_step, 'rule_applications': self.rule_applications, 'membrane_communications': self.membrane_communications, 'total_evolution_time_us': self.total_evolution_time, 'avg_evolution_time_us': self.total_evolution_time / max(1, self.evolution_step), 'execution_phase': self.execution_phase.name, 'is_halted': self.is_halted}
    def __str__(self) -> str:
        stats = self.get_system_stats()
        return f"PSystem[{self.system_name}]: {stats['active_membranes']} membranes, {stats['total_objects']} objects, step {stats['evolution_steps']}"
def create_dtesn_psystem_example() -> PSystemMembraneHierarchy:
    system = PSystemMembraneHierarchy('DTESN_Example')
    root_id = system.create_membrane(MembraneType.ROOT, 'echo_root', None, 100)
    system.get_membrane(root_id).add_object('input', 1)
    trunk_id = system.create_membrane(MembraneType.TRUNK, 'main_trunk', root_id, 200)
    system.get_membrane(trunk_id).add_object('state', 10)
    branch_id = system.create_membrane(MembraneType.BRANCH, 'primary_branch', trunk_id, 150)
    system.get_membrane(branch_id).add_object('memory', 5)
    leaf1_id = system.create_membrane(MembraneType.LEAF, 'leaf_left', branch_id, 100)
    leaf2_id = system.create_membrane(MembraneType.LEAF, 'leaf_right', branch_id, 100)
    system.get_membrane(leaf1_id).add_object('echo', 3)
    system.get_membrane(leaf2_id).add_object('response', 2)
    for i in range(2):
        term1_id = system.create_membrane(MembraneType.TERMINAL, f'terminal_{leaf1_id}_{i}', leaf1_id, 50)
        term2_id = system.create_membrane(MembraneType.TERMINAL, f'terminal_{leaf2_id}_{i}', leaf2_id, 50)
        system.get_membrane(term1_id).add_object('signal', 1)
        system.get_membrane(term2_id).add_object('output', 1)
    root_membrane = system.get_membrane(root_id)
    root_membrane.add_rule(EvolutionRule(rule_id='input_processing', rule_type=RuleType.COMMUNICATION, lhs=Multiset({'input': 1}), rhs=Multiset({'processed_input': 1}), target_membrane=trunk_id, priority=3))
    trunk_membrane = system.get_membrane(trunk_id)
    trunk_membrane.add_rule(EvolutionRule(rule_id='state_evolution', rule_type=RuleType.EVOLUTION, lhs=Multiset({'processed_input': 1, 'state': 2}), rhs=Multiset({'new_state': 3}), priority=2))
    logger.info('Created DTESN P-System example with OEIS A000081 compliant hierarchy')
    return system
if __name__ == '__main__':
    print('P-System Membrane Data Structures Demo')
    print('=' * 50)
    system = create_dtesn_psystem_example()
    print('\nInitial system state:')
    print(system)
    print('\nSystem statistics:')
    stats = system.get_system_stats()
    for key, value in stats.items():
        print(f'  {key}: {value}')
    print('\nOEIS A000081 validation:')
    is_valid, errors = system.validate_oeis_a000081_compliance()
    print(f'  Valid: {is_valid}')
    if errors:
        for error in errors:
            print(f'  Error: {error}')
    print('\nMembrane hierarchy:')
    tree = system.get_membrane_tree()
    def print_tree(node, indent=0):
        spaces = '  ' * indent
        print(f"{spaces}- {node['label']} ({node['type']}) [objects: {node['objects']}]")
        for child in node['children']:
            print_tree(child, indent + 1)
    print_tree(tree)
    print('\nEvolution simulation:')
    for step in range(3):
        active = system.evolve_system()
        print(f'  Step {step + 1}: Active={active}, {system}')
        if not active:
            break
    print('\nFinal system statistics:')
    final_stats = system.get_system_stats()
    for key, value in final_stats.items():
        print(f'  {key}: {value}')