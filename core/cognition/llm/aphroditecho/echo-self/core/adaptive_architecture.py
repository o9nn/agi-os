import asyncio
import logging
import time
from typing import Dict, Any, List, Optional, Tuple
from dataclasses import dataclass, field
import threading
from collections import deque
import statistics
import contextlib
try:
    from core.evolution_engine import EchoSelfEvolutionEngine
except ImportError:
    from .evolution_engine import EchoSelfEvolutionEngine
logger = logging.getLogger(__name__)
@dataclass
class PerformanceMetrics:
    latency_ms: float = 0.0
    throughput_tokens_per_sec: float = 0.0
    memory_usage_mb: float = 0.0
    accuracy_score: float = 0.0
    inference_time_ms: float = 0.0
    gpu_utilization: float = 0.0
    timestamp: float = field(default_factory=time.time)
    def overall_score(self) -> float:
        latency_score = max(0.0, 1.0 - self.latency_ms / 1000.0)
        throughput_score = min(1.0, self.throughput_tokens_per_sec / 100.0)
        memory_score = max(0.0, 1.0 - self.memory_usage_mb / 8192.0)
        accuracy_score = self.accuracy_score
        return 0.3 * latency_score + 0.3 * throughput_score + 0.2 * memory_score + 0.2 * accuracy_score
@dataclass
class ArchitectureMutation:
    mutation_type: str
    target_layer: Optional[int] = None
    parameters: Dict[str, Any] = field(default_factory=dict)
    expected_impact: float = 0.0
    confidence: float = 0.0
    def apply_to_genome(self, genome: Dict[str, Any]) -> Dict[str, Any]:
        mutated_genome = genome.copy()
        if self.mutation_type == 'add_layer':
            self._add_layer(mutated_genome)
        elif self.mutation_type == 'remove_layer':
            self._remove_layer(mutated_genome)
        elif self.mutation_type == 'modify_layer':
            self._modify_layer(mutated_genome)
        elif self.mutation_type == 'adjust_connections':
            self._adjust_connections(mutated_genome)
        return mutated_genome
    def _add_layer(self, genome: Dict[str, Any]) -> None:
        layers = genome.setdefault('layers', [])
        insert_pos = self.parameters.get('position', len(layers))
        layer_config = self.parameters.get('layer_config', {'type': 'dense', 'size': 128, 'activation': 'relu'})
        layers.insert(insert_pos, layer_config)
        self._update_connections_after_layer_addition(genome, insert_pos)
    def _remove_layer(self, genome: Dict[str, Any]) -> None:
        layers = genome.get('layers', [])
        if not layers or self.target_layer is None:
            return
        if 0 <= self.target_layer < len(layers):
            layers.pop(self.target_layer)
            self._update_connections_after_layer_removal(genome, self.target_layer)
    def _modify_layer(self, genome: Dict[str, Any]) -> None:
        layers = genome.get('layers', [])
        if not layers or self.target_layer is None:
            return
        if 0 <= self.target_layer < len(layers):
            for key, value in self.parameters.items():
                layers[self.target_layer][key] = value
    def _adjust_connections(self, genome: Dict[str, Any]) -> None:
        connections = genome.setdefault('connections', [])
        adjustment_factor = self.parameters.get('weight_adjustment', 0.1)
        for connection in connections:
            if 'weight' in connection:
                current_weight = connection['weight']
                connection['weight'] = current_weight * (1.0 + adjustment_factor)
    def _update_connections_after_layer_addition(self, genome: Dict[str, Any], insert_pos: int) -> None:
        connections = genome.setdefault('connections', [])
        for connection in connections:
            if connection.get('from', 0) >= insert_pos:
                connection['from'] += 1
            if connection.get('to', 0) >= insert_pos:
                connection['to'] += 1
        if insert_pos > 0:
            connections.append({'from': insert_pos - 1, 'to': insert_pos, 'weight': 1.0, 'type': 'direct'})
        layers = genome.get('layers', [])
        if insert_pos < len(layers) - 1:
            connections.append({'from': insert_pos, 'to': insert_pos + 1, 'weight': 1.0, 'type': 'direct'})
    def _update_connections_after_layer_removal(self, genome: Dict[str, Any], removed_pos: int) -> None:
        connections = genome.get('connections', [])
        connections[:] = [conn for conn in connections if conn.get('from', 0) != removed_pos and conn.get('to', 0) != removed_pos]
        for connection in connections:
            if connection.get('from', 0) > removed_pos:
                connection['from'] -= 1
            if connection.get('to', 0) > removed_pos:
                connection['to'] -= 1
class PerformanceMonitor:
    def __init__(self, window_size: int=100):
        self.window_size = window_size
        self.metrics_history: deque = deque(maxlen=window_size)
        self._lock = threading.RLock()
        self.baseline_metrics: Optional[PerformanceMetrics] = None
    def add_metrics(self, metrics: PerformanceMetrics) -> None:
        with self._lock:
            self.metrics_history.append(metrics)
            if self.baseline_metrics is None:
                self.baseline_metrics = metrics
    def get_current_performance(self) -> Dict[str, float]:
        with self._lock:
            if not self.metrics_history:
                return {}
            recent_metrics = list(self.metrics_history)[-10:]
            return {'avg_latency_ms': statistics.mean([m.latency_ms for m in recent_metrics]), 'avg_throughput': statistics.mean([m.throughput_tokens_per_sec for m in recent_metrics]), 'avg_memory_mb': statistics.mean([m.memory_usage_mb for m in recent_metrics]), 'avg_accuracy': statistics.mean([m.accuracy_score for m in recent_metrics]), 'avg_overall_score': statistics.mean([m.overall_score() for m in recent_metrics])}
    def detect_performance_degradation(self, threshold: float=0.1) -> bool:
        with self._lock:
            if len(self.metrics_history) < 20 or self.baseline_metrics is None:
                return False
            recent_performance = self.get_current_performance()
            baseline_score = self.baseline_metrics.overall_score()
            current_score = recent_performance.get('avg_overall_score', baseline_score)
            return baseline_score - current_score > threshold
    def get_performance_trend(self) -> float:
        with self._lock:
            if len(self.metrics_history) < 10:
                return 0.0
            metrics_list = list(self.metrics_history)
            half_point = len(metrics_list) // 2
            early_scores = [m.overall_score() for m in metrics_list[:half_point]]
            recent_scores = [m.overall_score() for m in metrics_list[half_point:]]
            if not early_scores or not recent_scores:
                return 0.0
            early_avg = statistics.mean(early_scores)
            recent_avg = statistics.mean(recent_scores)
            return max(-1.0, min(1.0, (recent_avg - early_avg) * 2.0))
class ArchitectureOptimizer:
    def __init__(self):
        self.mutation_strategies = {'layer_scaling': self._suggest_layer_scaling, 'depth_adjustment': self._suggest_depth_adjustment, 'connection_optimization': self._suggest_connection_optimization, 'parameter_tuning': self._suggest_parameter_tuning}
        self.mutation_history: List[Tuple[ArchitectureMutation, float]] = []
    def suggest_mutations(self, current_genome: Dict[str, Any], performance_metrics: PerformanceMetrics, target_improvement: float=0.1) -> List[ArchitectureMutation]:
        mutations = []
        if performance_metrics.latency_ms > 500:
            mutations.extend(self._suggest_latency_reduction(current_genome))
        if performance_metrics.memory_usage_mb > 4096:
            mutations.extend(self._suggest_memory_optimization(current_genome))
        if performance_metrics.accuracy_score < 0.8:
            mutations.extend(self._suggest_accuracy_improvement(current_genome))
        for strategy_name, strategy_func in self.mutation_strategies.items():
            strategy_mutations = strategy_func(current_genome, performance_metrics)
            mutations.extend(strategy_mutations)
        mutations.sort(key=lambda m: m.expected_impact, reverse=True)
        return mutations[:5]
    def _suggest_latency_reduction(self, genome: Dict[str, Any]) -> List[ArchitectureMutation]:
        mutations = []
        layers = genome.get('layers', [])
        for i, layer in enumerate(layers):
            if layer.get('size', 0) > 512:
                mutations.append(ArchitectureMutation(mutation_type='modify_layer', target_layer=i, parameters={'size': max(256, layer['size'] // 2)}, expected_impact=0.3, confidence=0.8))
        if len(layers) > 8:
            mutations.append(ArchitectureMutation(mutation_type='remove_layer', target_layer=len(layers) - 2, expected_impact=0.4, confidence=0.7))
        return mutations
    def _suggest_memory_optimization(self, genome: Dict[str, Any]) -> List[ArchitectureMutation]:
        mutations = []
        layers = genome.get('layers', [])
        for i, layer in enumerate(layers):
            current_size = layer.get('size', 64)
            if current_size > 128:
                mutations.append(ArchitectureMutation(mutation_type='modify_layer', target_layer=i, parameters={'size': max(64, int(current_size * 0.8))}, expected_impact=0.25, confidence=0.9))
        return mutations
    def _suggest_accuracy_improvement(self, genome: Dict[str, Any]) -> List[ArchitectureMutation]:
        mutations = []
        layers = genome.get('layers', [])
        has_attention = any((layer.get('type') == 'attention' for layer in layers))
        if not has_attention and len(layers) > 2:
            mutations.append(ArchitectureMutation(mutation_type='add_layer', parameters={'position': len(layers) - 1, 'layer_config': {'type': 'attention', 'size': 128, 'heads': 8}}, expected_impact=0.4, confidence=0.6))
        for i, layer in enumerate(layers):
            if layer.get('size', 0) < 64:
                mutations.append(ArchitectureMutation(mutation_type='modify_layer', target_layer=i, parameters={'size': min(256, layer['size'] * 2)}, expected_impact=0.3, confidence=0.7))
        return mutations
    def _suggest_layer_scaling(self, genome: Dict[str, Any], metrics: PerformanceMetrics) -> List[ArchitectureMutation]:
        mutations = []
        if metrics.overall_score() < 0.6:
            layers = genome.get('layers', [])
            for i, layer in enumerate(layers):
                current_size = layer.get('size', 64)
                if current_size < 512:
                    mutations.append(ArchitectureMutation(mutation_type='modify_layer', target_layer=i, parameters={'size': min(512, int(current_size * 1.5))}, expected_impact=0.2, confidence=0.5))
        return mutations
    def _suggest_depth_adjustment(self, genome: Dict[str, Any], metrics: PerformanceMetrics) -> List[ArchitectureMutation]:
        mutations = []
        layers = genome.get('layers', [])
        if len(layers) < 4 and metrics.accuracy_score < 0.7:
            mutations.append(ArchitectureMutation(mutation_type='add_layer', parameters={'position': len(layers) // 2, 'layer_config': {'type': 'dense', 'size': 128, 'activation': 'relu'}}, expected_impact=0.3, confidence=0.6))
        elif len(layers) > 8 and metrics.latency_ms > 1000:
            mutations.append(ArchitectureMutation(mutation_type='remove_layer', target_layer=len(layers) - 2, expected_impact=0.4, confidence=0.7))
        return mutations
    def _suggest_connection_optimization(self, genome: Dict[str, Any], metrics: PerformanceMetrics) -> List[ArchitectureMutation]:
        mutations = []
        adjustment = 0.1 if metrics.overall_score() < 0.5 else -0.05
        mutations.append(ArchitectureMutation(mutation_type='adjust_connections', parameters={'weight_adjustment': adjustment}, expected_impact=0.15, confidence=0.5))
        return mutations
    def _suggest_parameter_tuning(self, genome: Dict[str, Any], metrics: PerformanceMetrics) -> List[ArchitectureMutation]:
        mutations = []
        current_params = genome.get('parameters', {})
        current_lr = current_params.get('learning_rate', 0.001)
        if metrics.overall_score() < 0.5:
            new_lr = current_lr * 0.5 if current_lr > 0.0001 else current_lr * 2.0
            mutations.append(ArchitectureMutation(mutation_type='modify_layer', parameters={'learning_rate': new_lr}, expected_impact=0.2, confidence=0.6))
        return mutations
class AdaptiveArchitectureFramework:
    def __init__(self, evolution_engine: EchoSelfEvolutionEngine, performance_monitor: Optional[PerformanceMonitor]=None, optimizer: Optional[ArchitectureOptimizer]=None):
        self.evolution_engine = evolution_engine
        self.performance_monitor = performance_monitor or PerformanceMonitor()
        self.optimizer = optimizer or ArchitectureOptimizer()
        self.is_adapting = False
        self.adaptation_interval = 10
        self.min_adaptation_threshold = 0.05
        self.max_adaptations_per_hour = 6
        self.adaptation_history: List[Dict[str, Any]] = []
        self.last_adaptation_time = 0.0
        self.adaptation_count = 0
        self._lock = threading.RLock()
        self._adaptation_task: Optional[asyncio.Task] = None
        logger.info('AdaptiveArchitectureFramework initialized')
    async def start_adaptive_monitoring(self) -> None:
        if self.is_adapting:
            logger.warning('Adaptive monitoring already running')
            return
        self.is_adapting = True
        self._adaptation_task = asyncio.create_task(self._adaptation_loop())
        logger.info('Adaptive monitoring started')
    async def stop_adaptive_monitoring(self) -> None:
        self.is_adapting = False
        if self._adaptation_task and (not self._adaptation_task.done()):
            self._adaptation_task.cancel()
            with contextlib.suppress(asyncio.CancelledError):
                await self._adaptation_task
        logger.info('Adaptive monitoring stopped')
    async def _adaptation_loop(self) -> None:
        logger.info('Starting adaptive monitoring loop')
        try:
            while self.is_adapting:
                await asyncio.sleep(self.adaptation_interval)
                if not self.is_adapting:
                    break
                if await self._should_adapt():
                    await self._perform_adaptation()
        except asyncio.CancelledError:
            logger.info('Adaptation loop cancelled')
            raise
        except Exception as e:
            logger.error(f'Error in adaptation loop: {e}')
            raise
        finally:
            logger.info('Adaptation loop ended')
    async def _should_adapt(self) -> bool:
        with self._lock:
            current_time = time.time()
            if current_time - self.last_adaptation_time < 600:
                return False
            hour_ago = current_time - 3600
            recent_adaptations = sum((1 for record in self.adaptation_history if record.get('timestamp', 0) > hour_ago))
            if recent_adaptations >= self.max_adaptations_per_hour:
                logger.debug('Rate limit reached for adaptations')
                return False
            if self.performance_monitor.detect_performance_degradation(self.min_adaptation_threshold):
                logger.info('Performance degradation detected, adaptation needed')
                return True
            trend = self.performance_monitor.get_performance_trend()
            if trend < -0.3:
                logger.info(f'Negative performance trend detected: {trend:.3f}')
                return True
            return False
    async def _perform_adaptation(self) -> None:
        logger.info('Performing architecture adaptation')
        try:
            with self._lock:
                current_time = time.time()
                best_individual = self.evolution_engine.get_best_individual()
                if best_individual is None:
                    logger.warning('No best individual available for adaptation')
                    return
                current_performance = self.performance_monitor.get_current_performance()
                if not current_performance:
                    logger.warning('No performance metrics available for adaptation')
                    return
                metrics = PerformanceMetrics(latency_ms=current_performance.get('avg_latency_ms', 100.0), throughput_tokens_per_sec=current_performance.get('avg_throughput', 50.0), memory_usage_mb=current_performance.get('avg_memory_mb', 1024.0), accuracy_score=current_performance.get('avg_accuracy', 0.8))
                mutations = self.optimizer.suggest_mutations(best_individual.genome, metrics, target_improvement=0.1)
                if not mutations:
                    logger.info('No beneficial mutations suggested')
                    return
                best_mutation = mutations[0]
                logger.info(f'Applying mutation: {best_mutation.mutation_type} with expected impact: {best_mutation.expected_impact:.3f}')
                mutated_genome = best_mutation.apply_to_genome(best_individual.genome)
                mutated_individual = best_individual.__class__(mutated_genome)
                mutated_individual.parent_ids = [best_individual.id]
                if self.evolution_engine.current_population is not None:
                    self.evolution_engine.current_population.add_individual(mutated_individual)
                    if self.evolution_engine.current_population.size() > self.evolution_engine.config.population_size:
                        worst_individual = self.evolution_engine.current_population.get_worst_individual()
                        if worst_individual:
                            self.evolution_engine.current_population.remove_individual(worst_individual)
                adaptation_record = {'timestamp': current_time, 'mutation_type': best_mutation.mutation_type, 'expected_impact': best_mutation.expected_impact, 'confidence': best_mutation.confidence, 'pre_adaptation_performance': current_performance, 'genome_hash': hash(str(best_individual.genome))}
                self.adaptation_history.append(adaptation_record)
                self.last_adaptation_time = current_time
                self.adaptation_count += 1
                logger.info(f'Architecture adaptation completed (#{self.adaptation_count})')
        except Exception as e:
            logger.error(f'Error during architecture adaptation: {e}')
            raise
    def add_performance_metrics(self, metrics: PerformanceMetrics) -> None:
        self.performance_monitor.add_metrics(metrics)
    def get_adaptation_status(self) -> Dict[str, Any]:
        with self._lock:
            current_performance = self.performance_monitor.get_current_performance()
            return {'is_adapting': self.is_adapting, 'adaptation_count': self.adaptation_count, 'last_adaptation_time': self.last_adaptation_time, 'current_performance': current_performance, 'performance_trend': self.performance_monitor.get_performance_trend(), 'adaptation_interval': self.adaptation_interval, 'next_adaptation_check': self.last_adaptation_time + self.adaptation_interval}
    def get_adaptation_history(self) -> List[Dict[str, Any]]:
        with self._lock:
            return self.adaptation_history.copy()
    def configure_adaptation(self, interval: Optional[int]=None, threshold: Optional[float]=None, max_per_hour: Optional[int]=None) -> None:
        with self._lock:
            if interval is not None:
                self.adaptation_interval = max(5, interval)
            if threshold is not None:
                self.min_adaptation_threshold = max(0.01, min(0.5, threshold))
            if max_per_hour is not None:
                self.max_adaptations_per_hour = max(1, min(20, max_per_hour))
        logger.info(f'Adaptation configured: interval={self.adaptation_interval}s, threshold={self.min_adaptation_threshold}, max_per_hour={self.max_adaptations_per_hour}')