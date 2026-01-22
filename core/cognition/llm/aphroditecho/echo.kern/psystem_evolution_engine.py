import sys
import time
import threading
import multiprocessing
from abc import ABC, abstractmethod
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Dict, List, Any, Optional, Tuple
import logging
from collections import deque
import statistics
try:
    from psystem_membranes import PSystemMembraneHierarchy, MembraneStructure, EvolutionRule, Multiset, MembraneType, RuleType, ExecutionPhase
except ImportError as e:
    print(f'Error importing P-System modules: {e}')
    sys.exit(1)
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)
class EvolutionStrategy(Enum):
    SYNCHRONOUS = 'synchronous'
    ASYNCHRONOUS = 'asynchronous'
    PROBABILISTIC = 'probabilistic'
    PRIORITY_BASED = 'priority_based'
    ADAPTIVE = 'adaptive'
    REAL_TIME = 'real_time'
class EvolutionPhase(Enum):
    PREPARATION = auto()
    RULE_SELECTION = auto()
    RULE_APPLICATION = auto()
    COMMUNICATION = auto()
    SYNCHRONIZATION = auto()
    CLEANUP = auto()
@dataclass
class EvolutionConfig:
    strategy: EvolutionStrategy = EvolutionStrategy.SYNCHRONOUS
    max_parallel_workers: int = 4
    target_evolution_time_us: float = 50.0
    max_evolution_time_us: float = 1000.0
    enable_analytics: bool = True
    enable_profiling: bool = False
    rule_selection_algorithm: str = 'priority_first'
    communication_batching: bool = True
    adaptive_threshold: float = 0.8
    def __post_init__(self):
        if self.max_parallel_workers <= 0:
            self.max_parallel_workers = multiprocessing.cpu_count()
@dataclass
class EvolutionMetrics:
    total_evolution_time_us: float = 0.0
    membrane_evolution_times: Dict[str, float] = field(default_factory=dict)
    rules_applied: int = 0
    membranes_processed: int = 0
    communications_executed: int = 0
    strategy_switches: int = 0
    performance_score: float = 1.0
    timestamp: float = field(default_factory=time.time)
    def get_average_membrane_time(self) -> float:
        if not self.membrane_evolution_times:
            return 0.0
        return statistics.mean(self.membrane_evolution_times.values())
    def meets_timing_constraints(self, config: EvolutionConfig) -> bool:
        return self.total_evolution_time_us <= config.max_evolution_time_us
class EvolutionAnalytics:
    def __init__(self, history_size: int=100):
        self.history_size = history_size
        self.metrics_history: deque = deque(maxlen=history_size)
        self.performance_trends: Dict[str, deque] = {'evolution_time': deque(maxlen=history_size), 'rules_applied': deque(maxlen=history_size), 'performance_score': deque(maxlen=history_size)}
        self._lock = threading.Lock()
    def record_metrics(self, metrics: EvolutionMetrics) -> None:
        with self._lock:
            self.metrics_history.append(metrics)
            self.performance_trends['evolution_time'].append(metrics.total_evolution_time_us)
            self.performance_trends['rules_applied'].append(metrics.rules_applied)
            self.performance_trends['performance_score'].append(metrics.performance_score)
    def get_performance_trend(self, metric: str, window: int=10) -> float:
        try:
            with self._lock:
                if metric not in self.performance_trends:
                    return 0.0
                values = list(self.performance_trends[metric])
                if len(values) < window:
                    return 0.0
                if len(values) < 4:
                    return 0.0
                half_window = max(2, window // 2)
                if len(values) < half_window * 2:
                    return 0.0
                recent_values = values[-half_window:]
                earlier_values = values[-half_window * 2:-half_window]
                if not recent_values or not earlier_values:
                    return 0.0
                recent = statistics.mean(recent_values)
                earlier = statistics.mean(earlier_values)
                if earlier == 0:
                    return 0.0
                return (recent - earlier) / earlier
        except Exception:
            return 0.0
    def get_current_performance(self) -> Dict[str, Any]:
        with self._lock:
            if not self.metrics_history:
                return {'avg_evolution_time_us': 0.0, 'avg_rules_applied': 0.0, 'avg_performance_score': 1.0, 'evolution_trend': 0.0, 'rules_trend': 0.0, 'total_evolution_cycles': 0}
            recent_metrics = list(self.metrics_history)[-10:]
            try:
                evolution_trend = self.get_performance_trend('evolution_time')
                rules_trend = self.get_performance_trend('rules_applied')
            except Exception:
                evolution_trend = 0.0
                rules_trend = 0.0
            return {'avg_evolution_time_us': statistics.mean([m.total_evolution_time_us for m in recent_metrics]), 'avg_rules_applied': statistics.mean([m.rules_applied for m in recent_metrics]), 'avg_performance_score': statistics.mean([m.performance_score for m in recent_metrics]), 'evolution_trend': evolution_trend, 'rules_trend': rules_trend, 'total_evolution_cycles': len(self.metrics_history)}
class EvolutionStrategyBase(ABC):
    def __init__(self, config: EvolutionConfig, analytics: EvolutionAnalytics):
        self.config = config
        self.analytics = analytics
        self.logger = logging.getLogger(f'{__name__}.{self.__class__.__name__}')
    @abstractmethod
    def evolve_system(self, system: PSystemMembraneHierarchy) -> EvolutionMetrics:
        pass
    @abstractmethod
    def evolve_membrane(self, membrane: MembraneStructure, system: PSystemMembraneHierarchy) -> Tuple[int, float]:
        pass
    def select_rules(self, membrane: MembraneStructure) -> List[EvolutionRule]:
        applicable_rules = membrane.get_applicable_rules()
        if self.config.rule_selection_algorithm == 'priority_first':
            return sorted(applicable_rules, key=lambda r: r.priority, reverse=True)
        elif self.config.rule_selection_algorithm == 'random':
            import random
            rules = applicable_rules.copy()
            random.shuffle(rules)
            return rules
        else:
            return applicable_rules
class SynchronousEvolutionStrategy(EvolutionStrategyBase):
    def evolve_system(self, system: PSystemMembraneHierarchy) -> EvolutionMetrics:
        start_time = time.time()
        metrics = EvolutionMetrics()
        membrane_ids = list(system.membranes.keys())
        for membrane_id in membrane_ids:
            if membrane_id in system.membranes and (not system.membranes[membrane_id].is_dissolved):
                rules_applied, evolution_time = self.evolve_membrane(system.membranes[membrane_id], system)
                metrics.rules_applied += rules_applied
                metrics.membrane_evolution_times[membrane_id] = evolution_time
                metrics.membranes_processed += 1
        metrics.total_evolution_time_us = (time.time() - start_time) * 1000000
        metrics.performance_score = self._calculate_performance_score(metrics)
        return metrics
    def evolve_membrane(self, membrane: MembraneStructure, system: PSystemMembraneHierarchy) -> Tuple[int, float]:
        start_time = time.time()
        rules_applied = system.evolve_membrane(membrane.membrane_id)
        evolution_time = (time.time() - start_time) * 1000000
        return (rules_applied, evolution_time)
    def _calculate_performance_score(self, metrics: EvolutionMetrics) -> float:
        if metrics.total_evolution_time_us == 0:
            return 1.0
        timing_score = min(1.0, self.config.target_evolution_time_us / metrics.total_evolution_time_us)
        efficiency_score = min(1.0, metrics.rules_applied / max(1, metrics.membranes_processed))
        return (timing_score + efficiency_score) / 2.0
class ParallelEvolutionStrategy(EvolutionStrategyBase):
    def __init__(self, config: EvolutionConfig, analytics: EvolutionAnalytics):
        super().__init__(config, analytics)
        self.executor = ThreadPoolExecutor(max_workers=config.max_parallel_workers)
    def evolve_system(self, system: PSystemMembraneHierarchy) -> EvolutionMetrics:
        start_time = time.time()
        metrics = EvolutionMetrics()
        membrane_ids = [mid for mid, m in system.membranes.items() if not m.is_dissolved]
        future_to_membrane = {}
        for membrane_id in membrane_ids:
            future = self.executor.submit(self._evolve_membrane_safe, system.membranes[membrane_id], system)
            future_to_membrane[future] = membrane_id
        for future in as_completed(future_to_membrane):
            membrane_id = future_to_membrane[future]
            try:
                rules_applied, evolution_time = future.result()
                metrics.rules_applied += rules_applied
                metrics.membrane_evolution_times[membrane_id] = evolution_time
                metrics.membranes_processed += 1
            except Exception as e:
                self.logger.error(f'Error evolving membrane {membrane_id}: {e}')
        metrics.total_evolution_time_us = (time.time() - start_time) * 1000000
        metrics.performance_score = self._calculate_performance_score(metrics)
        return metrics
    def evolve_membrane(self, membrane: MembraneStructure, system: PSystemMembraneHierarchy) -> Tuple[int, float]:
        return self._evolve_membrane_safe(membrane, system)
    def _evolve_membrane_safe(self, membrane: MembraneStructure, system: PSystemMembraneHierarchy) -> Tuple[int, float]:
        start_time = time.time()
        try:
            with membrane._lock:
                rules_applied = 0
                applicable_rules = self.select_rules(membrane)
                for rule in applicable_rules:
                    if rule.is_applicable(membrane.objects):
                        success, products = rule.apply(membrane.objects)
                        if success:
                            if rule.rule_type == RuleType.EVOLUTION:
                                membrane.objects = membrane.objects.union(products)
                            rules_applied += 1
                evolution_time = (time.time() - start_time) * 1000000
                return (rules_applied, evolution_time)
        except Exception as e:
            self.logger.error(f'Error in membrane evolution: {e}')
            return (0, 0.0)
    def _calculate_performance_score(self, metrics: EvolutionMetrics) -> float:
        if metrics.total_evolution_time_us == 0:
            return 1.0
        timing_score = min(1.0, self.config.target_evolution_time_us / metrics.total_evolution_time_us)
        parallel_efficiency = min(1.0, metrics.membranes_processed / self.config.max_parallel_workers)
        return (timing_score + parallel_efficiency) / 2.0
    def __del__(self):
        if hasattr(self, 'executor'):
            self.executor.shutdown(wait=True)
class AdaptiveEvolutionStrategy(EvolutionStrategyBase):
    def __init__(self, config: EvolutionConfig, analytics: EvolutionAnalytics):
        super().__init__(config, analytics)
        self.strategies = {EvolutionStrategy.SYNCHRONOUS: SynchronousEvolutionStrategy(config, analytics), EvolutionStrategy.ASYNCHRONOUS: ParallelEvolutionStrategy(config, analytics)}
        self.current_strategy = EvolutionStrategy.SYNCHRONOUS
        self.switch_threshold = config.adaptive_threshold
        self.evaluation_window = 10
        self.last_switch_time = time.time()
        self.min_switch_interval = 5.0
    def evolve_system(self, system: PSystemMembraneHierarchy) -> EvolutionMetrics:
        current_time = time.time()
        if len(self.analytics.metrics_history) >= self.evaluation_window and current_time - self.last_switch_time > self.min_switch_interval:
            try:
                recent_metrics = list(self.analytics.metrics_history)[-5:]
                if recent_metrics:
                    avg_score = sum((m.performance_score for m in recent_metrics)) / len(recent_metrics)
                    if avg_score < self.switch_threshold:
                        self._switch_strategy()
                        self.last_switch_time = current_time
            except Exception:
                pass
        strategy = self.strategies[self.current_strategy]
        metrics = strategy.evolve_system(system)
        return metrics
    def evolve_membrane(self, membrane: MembraneStructure, system: PSystemMembraneHierarchy) -> Tuple[int, float]:
        strategy = self.strategies[self.current_strategy]
        return strategy.evolve_membrane(membrane, system)
    def _switch_strategy(self):
        if self.current_strategy == EvolutionStrategy.SYNCHRONOUS:
            self.current_strategy = EvolutionStrategy.ASYNCHRONOUS
        else:
            self.current_strategy = EvolutionStrategy.SYNCHRONOUS
        self.logger.info(f'Adaptive strategy switched to: {self.current_strategy.value}')
class PSystemEvolutionEngine:
    def __init__(self, config: Optional[EvolutionConfig]=None):
        self.config = config or EvolutionConfig()
        self.analytics = EvolutionAnalytics()
        self.strategies = {EvolutionStrategy.SYNCHRONOUS: SynchronousEvolutionStrategy(self.config, self.analytics), EvolutionStrategy.ASYNCHRONOUS: ParallelEvolutionStrategy(self.config, self.analytics), EvolutionStrategy.ADAPTIVE: AdaptiveEvolutionStrategy(self.config, self.analytics)}
        self.current_strategy = self.config.strategy
        self.logger = logging.getLogger(__name__)
        self.total_evolution_cycles = 0
        self.constraint_violations = 0
        self.logger.info(f'P-System Evolution Engine initialized with strategy: {self.current_strategy.value}')
    def evolve_system(self, system: PSystemMembraneHierarchy) -> EvolutionMetrics:
        if system.is_halted:
            return EvolutionMetrics()
        strategy = self.strategies[self.current_strategy]
        metrics = strategy.evolve_system(system)
        self.analytics.record_metrics(metrics)
        self.total_evolution_cycles += 1
        if not metrics.meets_timing_constraints(self.config):
            self.constraint_violations += 1
            self.logger.warning(f'Evolution cycle exceeded timing constraint: {metrics.total_evolution_time_us:.2f}μs > {self.config.max_evolution_time_us}μs')
        system.evolution_step += 1
        if metrics.rules_applied == 0:
            system.execution_phase = ExecutionPhase.HALTED
            system.is_halted = True
            self.logger.info(f'P-System halted after {system.evolution_step} steps')
        if self.config.enable_profiling:
            self._log_performance_profile(metrics)
        return metrics
    def evolve_membrane(self, membrane_id: str, system: PSystemMembraneHierarchy) -> Tuple[int, float]:
        if membrane_id not in system.membranes:
            return (0, 0.0)
        membrane = system.membranes[membrane_id]
        if membrane.is_dissolved:
            return (0, 0.0)
        strategy = self.strategies[self.current_strategy]
        return strategy.evolve_membrane(membrane, system)
    def set_strategy(self, strategy: EvolutionStrategy) -> None:
        if strategy in self.strategies:
            self.current_strategy = strategy
            self.logger.info(f'Evolution strategy changed to: {strategy.value}')
        else:
            raise ValueError(f'Unsupported evolution strategy: {strategy}')
    def get_performance_statistics(self) -> Dict[str, Any]:
        current_perf = self.analytics.get_current_performance()
        return {'total_evolution_cycles': self.total_evolution_cycles, 'constraint_violations': self.constraint_violations, 'violation_rate': self.constraint_violations / max(1, self.total_evolution_cycles), 'current_strategy': self.current_strategy.value, 'config': {'target_time_us': self.config.target_evolution_time_us, 'max_time_us': self.config.max_evolution_time_us, 'parallel_workers': self.config.max_parallel_workers}, **current_perf}
    def optimize_configuration(self) -> Dict[str, Any]:
        try:
            performance = self.analytics.get_current_performance()
            recommendations = {}
            avg_time = performance.get('avg_evolution_time_us', 0)
            if avg_time > self.config.max_evolution_time_us * 0.8:
                if self.current_strategy == EvolutionStrategy.SYNCHRONOUS:
                    recommendations['strategy'] = EvolutionStrategy.ASYNCHRONOUS.value
                elif self.config.max_parallel_workers < multiprocessing.cpu_count():
                    recommendations['max_parallel_workers'] = min(multiprocessing.cpu_count(), self.config.max_parallel_workers + 2)
            if avg_time > self.config.target_evolution_time_us * 1.5:
                recommendations['target_evolution_time_us'] = self.config.target_evolution_time_us * 1.2
            return recommendations
        except Exception:
            return {}
    def _log_performance_profile(self, metrics: EvolutionMetrics) -> None:
        avg_membrane_time = metrics.get_average_membrane_time()
        self.logger.debug(f'Evolution Profile - Total: {metrics.total_evolution_time_us:.2f}μs, Avg Membrane: {avg_membrane_time:.2f}μs, Rules: {metrics.rules_applied}, Membranes: {metrics.membranes_processed}, Score: {metrics.performance_score:.3f}')
    def __str__(self) -> str:
        return f'PSystemEvolutionEngine[{self.current_strategy.value}]: {self.total_evolution_cycles} cycles, {self.constraint_violations} violations'
def create_evolution_engine_demo() -> PSystemEvolutionEngine:
    config = EvolutionConfig(strategy=EvolutionStrategy.ADAPTIVE, max_parallel_workers=4, target_evolution_time_us=30.0, max_evolution_time_us=100.0, enable_analytics=True, enable_profiling=True, rule_selection_algorithm='priority_first', communication_batching=True)
    engine = PSystemEvolutionEngine(config)
    logger.info('Created demonstration evolution engine with adaptive strategy')
    return engine
if __name__ == '__main__':
    print('P-System Membrane Evolution Engine Demo')
    print('=' * 50)
    engine = create_evolution_engine_demo()
    from psystem_membranes import create_dtesn_psystem_example
    system = create_dtesn_psystem_example()
    print(f'\nInitial system: {system}')
    print(f'Evolution engine: {engine}')
    print('\nRunning evolution cycles...')
    for cycle in range(5):
        metrics = engine.evolve_system(system)
        print(f'Cycle {cycle + 1}:')
        print(f'  Time: {metrics.total_evolution_time_us:.2f}μs')
        print(f'  Rules Applied: {metrics.rules_applied}')
        print(f'  Membranes Processed: {metrics.membranes_processed}')
        print(f'  Performance Score: {metrics.performance_score:.3f}')
        print(f'  Constraints Met: {metrics.meets_timing_constraints(engine.config)}')
        if system.is_halted:
            break
    print('\nFinal Performance Statistics:')
    stats = engine.get_performance_statistics()
    for key, value in stats.items():
        if isinstance(value, dict):
            print(f'  {key}:')
            for subkey, subvalue in value.items():
                print(f'    {subkey}: {subvalue}')
        else:
            print(f'  {key}: {value}')
    print('\nOptimization Recommendations:')
    recommendations = engine.optimize_configuration()
    if recommendations:
        for key, value in recommendations.items():
            print(f'  {key}: {value}')
    else:
        print('  No optimizations recommended - performance is optimal')