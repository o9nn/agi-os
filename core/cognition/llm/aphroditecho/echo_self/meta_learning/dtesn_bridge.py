from typing import Dict, List, Any
import logging
from dataclasses import dataclass
import random
logger = logging.getLogger(__name__)
def _mean(values: List[float]) -> float:
    return sum(values) / len(values) if values else 0.0
def _std(values: List[float]) -> float:
    if len(values) < 2:
        return 0.0
    mean_val = _mean(values)
    variance = sum(((x - mean_val) ** 2 for x in values)) / len(values)
    return variance ** 0.5
@dataclass
class DTESNPerformanceMetrics:
    membrane_efficiency: float
    reservoir_stability: float
    b_series_convergence: float
    memory_usage: Dict[str, float]
    computation_time: Dict[str, float]
class DTESNMetaLearningBridge:
    def __init__(self, meta_optimizer=None):
        self.meta_optimizer = meta_optimizer
        self.dtesn_kernel = None
        self.performance_cache = {}
        self.dtesn_meta_params = {'membrane_hierarchy_depth': 8, 'reservoir_size_factor': 1.0, 'b_series_order': 16, 'plasticity_threshold': 0.1, 'homeostasis_target': 0.5}
        logger.info('DTESN meta-learning bridge initialized')
    def set_dtesn_kernel(self, dtesn_kernel) -> None:
        self.dtesn_kernel = dtesn_kernel
        logger.info('DTESN kernel integration enabled in meta-learning bridge')
    async def extract_dtesn_metrics(self) -> DTESNPerformanceMetrics:
        if not self.dtesn_kernel:
            return DTESNPerformanceMetrics(membrane_efficiency=0.8, reservoir_stability=0.7, b_series_convergence=0.9, memory_usage={'reservoir': 0.6, 'membranes': 0.3}, computation_time={'forward': 0.001, 'learning': 0.01})
        try:
            metrics = DTESNPerformanceMetrics(membrane_efficiency=await self._get_membrane_efficiency(), reservoir_stability=await self._get_reservoir_stability(), b_series_convergence=await self._get_b_series_convergence(), memory_usage=await self._get_memory_usage(), computation_time=await self._get_computation_times())
            return metrics
        except Exception as e:
            logger.warning(f'Failed to extract DTESN metrics: {e}')
            return DTESNPerformanceMetrics(membrane_efficiency=0.5, reservoir_stability=0.5, b_series_convergence=0.5, memory_usage={'unknown': 0.5}, computation_time={'unknown': 0.001})
    async def _get_membrane_efficiency(self) -> float:
        return random.uniform(0.6, 0.9)
    async def _get_reservoir_stability(self) -> float:
        return random.uniform(0.5, 0.8)
    async def _get_b_series_convergence(self) -> float:
        return random.uniform(0.7, 0.95)
    async def _get_memory_usage(self) -> Dict[str, float]:
        return {'reservoir': random.uniform(0.4, 0.8), 'membranes': random.uniform(0.2, 0.6), 'b_series': random.uniform(0.1, 0.3)}
    async def _get_computation_times(self) -> Dict[str, float]:
        return {'membrane_evolution': random.uniform(0.001, 0.01), 'reservoir_update': random.uniform(0.0001, 0.001), 'b_series_computation': random.uniform(1e-05, 0.0001)}
    async def optimize_dtesn_parameters(self, current_config: Dict[str, Any], performance_history: List[DTESNPerformanceMetrics]) -> Dict[str, Any]:
        if len(performance_history) < 5:
            return current_config
        optimized_config = current_config.copy()
        recent_metrics = performance_history[-10:]
        avg_membrane_efficiency = _mean([m.membrane_efficiency for m in recent_metrics])
        if avg_membrane_efficiency < 0.6:
            optimized_config['membrane_hierarchy_depth'] = min(12, current_config.get('membrane_hierarchy_depth', 8) + 1)
            logger.debug('Increased membrane hierarchy depth for better efficiency')
        avg_stability = _mean([m.reservoir_stability for m in recent_metrics])
        if avg_stability < 0.5:
            optimized_config['reservoir_size_factor'] = min(2.0, current_config.get('reservoir_size_factor', 1.0) * 1.1)
            logger.debug('Increased reservoir size factor for better stability')
        avg_convergence = _mean([m.b_series_convergence for m in recent_metrics])
        if avg_convergence < 0.7:
            optimized_config['b_series_order'] = min(32, current_config.get('b_series_order', 16) + 2)
            logger.debug('Increased B-Series order for better convergence')
        avg_memory = _mean([sum(m.memory_usage.values()) for m in recent_metrics])
        if avg_memory > 0.8:
            optimized_config['reservoir_size_factor'] *= 0.9
            logger.debug('Reduced reservoir size due to high memory usage')
        return optimized_config
    async def apply_meta_learning_to_dtesn(self, architecture_params: Dict[str, Any]) -> Dict[str, Any]:
        if not self.meta_optimizer:
            return architecture_params
        recommendations = await self.meta_optimizer.get_architecture_recommendations(3)
        if not recommendations:
            return architecture_params
        top_recommendation = recommendations[0]
        optimized_params = architecture_params.copy()
        rec_params = top_recommendation['architecture_params']
        if 'membrane_depth' in rec_params:
            optimized_params['membrane_hierarchy_depth'] = int(rec_params['membrane_depth'])
        if 'reservoir_scaling' in rec_params:
            optimized_params['reservoir_size_factor'] = float(rec_params['reservoir_scaling'])
        if 'b_series_order' in rec_params:
            optimized_params['b_series_order'] = int(rec_params['b_series_order'])
        if 'plasticity_factor' in rec_params:
            optimized_params['plasticity_threshold'] = float(rec_params['plasticity_factor'])
        logger.info(f'Applied meta-learning recommendations to DTESN config: {optimized_params}')
        return optimized_params
    async def record_dtesn_performance(self, config: Dict[str, Any], performance_metrics: DTESNPerformanceMetrics, generation: int) -> None:
        if not self.meta_optimizer:
            return
        architecture_params = {'membrane_hierarchy_depth': config.get('membrane_hierarchy_depth', 8), 'reservoir_size_factor': config.get('reservoir_size_factor', 1.0), 'b_series_order': config.get('b_series_order', 16), 'plasticity_threshold': config.get('plasticity_threshold', 0.1)}
        fitness_score = self._calculate_composite_fitness(performance_metrics)
        convergence_rate = performance_metrics.b_series_convergence
        diversity_metric = self._calculate_dtesn_diversity(performance_metrics)
        await self.meta_optimizer.record_architecture_performance(architecture_params=architecture_params, fitness_score=fitness_score, generation=generation, convergence_rate=convergence_rate, diversity_metric=diversity_metric, resource_usage={'memory_total': sum(performance_metrics.memory_usage.values()), 'compute_time': sum(performance_metrics.computation_time.values())})
    def _calculate_composite_fitness(self, metrics: DTESNPerformanceMetrics) -> float:
        weights = {'membrane_efficiency': 0.3, 'reservoir_stability': 0.3, 'b_series_convergence': 0.2, 'memory_efficiency': 0.1, 'compute_efficiency': 0.1}
        total_memory = sum(metrics.memory_usage.values())
        memory_efficiency = max(0.0, 1.0 - total_memory)
        total_compute_time = sum(metrics.computation_time.values())
        compute_efficiency = max(0.0, 1.0 - min(1.0, total_compute_time * 1000))
        fitness = weights['membrane_efficiency'] * metrics.membrane_efficiency + weights['reservoir_stability'] * metrics.reservoir_stability + weights['b_series_convergence'] * metrics.b_series_convergence + weights['memory_efficiency'] * memory_efficiency + weights['compute_efficiency'] * compute_efficiency
        return max(0.0, min(1.0, fitness))
    def _calculate_dtesn_diversity(self, metrics: DTESNPerformanceMetrics) -> float:
        performance_values = [metrics.membrane_efficiency, metrics.reservoir_stability, metrics.b_series_convergence]
        return _std(performance_values) if len(performance_values) > 1 else 0.5
    def get_dtesn_integration_stats(self) -> Dict[str, Any]:
        return {'dtesn_kernel_connected': self.dtesn_kernel is not None, 'meta_optimizer_connected': self.meta_optimizer is not None, 'dtesn_meta_params': self.dtesn_meta_params.copy(), 'performance_cache_size': len(self.performance_cache)}