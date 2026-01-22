import logging
import time
from dataclasses import dataclass, field
from typing import Dict, Any, List, Optional
from datetime import datetime
import numpy as np
import torch
from aphrodite.dtesn_integration import DTESNDynamicIntegration
from aphrodite.dynamic_model_manager import DynamicModelManager, IncrementalUpdateRequest
from echo_self.meta_learning.meta_optimizer import ExperienceReplay, ArchitecturePerformance
logger = logging.getLogger(__name__)
@dataclass
class ServerSideConfig:
    enable_request_monitoring: bool = True
    enable_response_feedback: bool = True
    enable_user_interactions: bool = True
    background_learning_interval: float = 30.0
    max_concurrent_learning_tasks: int = 2
    enable_hot_swapping: bool = True
    max_learning_rate_production: float = 0.0001
    learning_rate_decay_production: float = 0.99
    enable_rollback_on_failure: bool = True
    min_interactions_for_learning: int = 10
    interaction_quality_threshold: float = 0.5
    enable_performance_feedback: bool = True
@dataclass
class InteractionFeedback:
    request_id: str
    endpoint: str
    method: str
    response_time_ms: float
    status_code: int
    user_satisfaction: Optional[float] = None
    error_occurred: bool = False
    model_confidence: Optional[float] = None
    timestamp: datetime = field(default_factory=datetime.now)
    metadata: Dict[str, Any] = field(default_factory=dict)
@dataclass
class InteractionData:
    interaction_id: str
    interaction_type: str
    input_data: Dict[str, Any]
    output_data: Dict[str, Any]
    performance_feedback: float
    timestamp: datetime
    context_metadata: Dict[str, Any] = field(default_factory=dict)
    success: bool = True
@dataclass
class ContinuousLearningConfig:
    max_experiences: int = 10000
    replay_batch_size: int = 32
    replay_frequency: int = 10
    learning_rate_base: float = 0.001
    learning_rate_decay: float = 0.99
    min_learning_rate: float = 1e-06
    adaptation_threshold: float = 0.1
    enable_ewc: bool = True
    ewc_lambda: float = 1000.0
    importance_decay: float = 0.9
    consolidation_frequency: int = 100
    consolidation_strength: float = 0.5
    performance_window: int = 50
    performance_threshold: float = 0.7
class ContinuousLearningSystem:
    def __init__(self, dynamic_manager: DynamicModelManager, dtesn_integration: DTESNDynamicIntegration, config: Optional[ContinuousLearningConfig]=None):
        self.dynamic_manager = dynamic_manager
        self.dtesn_integration = dtesn_integration
        self.config = config or ContinuousLearningConfig()
        self.experience_replay = ExperienceReplay(max_size=self.config.max_experiences)
        self.interaction_count = 0
        self.current_learning_rate = self.config.learning_rate_base
        self.parameter_importance = {}
        self.consolidated_parameters = {}
        self.performance_history = []
        self.learning_metrics = {'total_interactions': 0, 'successful_adaptations': 0, 'forgetting_events': 0, 'consolidations': 0}
        logger.info(f'Continuous Learning System initialized with config: {config}')
    async def learn_from_interaction(self, interaction_data: InteractionData) -> Dict[str, Any]:
        try:
            start_time = time.time()
            self.interaction_count += 1
            self.learning_metrics['total_interactions'] += 1
            learning_signal = self._extract_learning_signal(interaction_data)
            update_result = await self._apply_online_update(interaction_data, learning_signal)
            experience = self._create_experience_record(interaction_data, learning_signal, update_result)
            self.experience_replay.add_experience(experience)
            if self.config.enable_ewc:
                self._update_parameter_importance(interaction_data, learning_signal)
            replay_result = None
            if self.interaction_count % self.config.replay_frequency == 0:
                replay_result = await self._perform_experience_replay()
            consolidation_result = None
            if self.interaction_count % self.config.consolidation_frequency == 0:
                consolidation_result = await self._perform_memory_consolidation()
            self._update_performance_tracking(interaction_data, update_result)
            self._adapt_learning_rate()
            learning_time = time.time() - start_time
            result = {'success': update_result.get('success', False), 'learning_signal': learning_signal, 'online_update': update_result, 'replay_result': replay_result, 'consolidation_result': consolidation_result, 'learning_time': learning_time, 'interaction_count': self.interaction_count, 'current_learning_rate': self.current_learning_rate, 'metrics': self.learning_metrics.copy()}
            if result['success']:
                self.learning_metrics['successful_adaptations'] += 1
            logger.debug(f"Learned from interaction {interaction_data.interaction_id}: success={result['success']}, time={learning_time:.4f}s")
            return result
        except Exception as e:
            logger.error(f'Failed to learn from interaction: {e}')
            return {'success': False, 'error': str(e), 'interaction_count': self.interaction_count, 'metrics': self.learning_metrics.copy()}
    def _extract_learning_signal(self, interaction_data: InteractionData) -> Dict[str, Any]:
        signal_strength = abs(interaction_data.performance_feedback)
        signal_direction = np.sign(interaction_data.performance_feedback)
        context_weight = 1.0
        if 'importance' in interaction_data.context_metadata:
            context_weight = float(interaction_data.context_metadata['importance'])
        time_diff = (datetime.now() - interaction_data.timestamp).total_seconds()
        temporal_weight = np.exp(-time_diff / 3600)
        learning_signal = {'strength': signal_strength * context_weight * temporal_weight, 'direction': signal_direction, 'context_weight': context_weight, 'temporal_weight': temporal_weight, 'raw_feedback': interaction_data.performance_feedback}
        return learning_signal
    async def _apply_online_update(self, interaction_data: InteractionData, learning_signal: Dict[str, Any]) -> Dict[str, Any]:
        try:
            target_parameters = self._identify_target_parameters(interaction_data)
            update_results = {}
            for param_name in target_parameters:
                current_params = self._get_current_parameters(param_name)
                update_gradient = self._compute_update_gradient(interaction_data, learning_signal, param_name)
                updated_params, dtesn_metrics = await self.dtesn_integration.adaptive_parameter_update(parameter_name=param_name, current_params=current_params, target_gradient=update_gradient, performance_feedback=learning_signal['raw_feedback'])
                if self.config.enable_ewc and param_name in self.parameter_importance:
                    updated_params = self._apply_ewc_regularization(param_name, current_params, updated_params)
                update_request = IncrementalUpdateRequest(parameter_name=param_name, update_data=updated_params, learning_rate=self.current_learning_rate, update_type='replace', metadata={'interaction_id': interaction_data.interaction_id, 'learning_signal': learning_signal, 'dtesn_metrics': dtesn_metrics})
                dm_result = await self.dynamic_manager.apply_incremental_update(update_request)
                update_results[param_name] = {'dtesn_metrics': dtesn_metrics, 'dynamic_manager_result': dm_result, 'parameter_shape': updated_params.shape if hasattr(updated_params, 'shape') else None}
            return {'success': all((r.get('dynamic_manager_result', {}).get('success', False) for r in update_results.values())), 'updated_parameters': list(target_parameters), 'update_results': update_results}
        except Exception as e:
            logger.error(f'Failed to apply online update: {e}')
            return {'success': False, 'error': str(e)}
    def _identify_target_parameters(self, interaction_data: InteractionData) -> List[str]:
        parameter_map = {'text_generation': ['transformer.h.*.mlp.c_proj.weight', 'transformer.h.*.attn.c_proj.weight'], 'reasoning': ['transformer.h.*.attn.c_attn.weight', 'transformer.h.*.mlp.c_fc.weight'], 'memory_recall': ['transformer.wte.weight', 'transformer.h.*.attn.c_attn.weight'], 'default': ['transformer.h.10.mlp.c_proj.weight']}
        interaction_type = interaction_data.interaction_type
        return parameter_map.get(interaction_type, parameter_map['default'])
    def _get_current_parameters(self, param_name: str) -> torch.Tensor:
        if 'mlp' in param_name:
            return torch.randn(768, 3072)
        elif 'attn' in param_name:
            return torch.randn(768, 768)
        else:
            return torch.randn(768, 768)
    def _compute_update_gradient(self, interaction_data: InteractionData, learning_signal: Dict[str, Any], param_name: str) -> torch.Tensor:
        current_params = self._get_current_parameters(param_name)
        signal_strength = learning_signal['strength']
        signal_direction = learning_signal['direction']
        gradient_scale = signal_strength * self.current_learning_rate * 0.1
        gradient = torch.randn_like(current_params) * gradient_scale * signal_direction
        return gradient
    def _apply_ewc_regularization(self, param_name: str, current_params: torch.Tensor, updated_params: torch.Tensor) -> torch.Tensor:
        if param_name not in self.parameter_importance:
            return updated_params
        importance = self.parameter_importance[param_name]
        consolidated = self.consolidated_parameters.get(param_name, current_params)
        ewc_loss = self.config.ewc_lambda * importance * (updated_params - consolidated) ** 2
        regularization_strength = torch.clamp(ewc_loss / (1.0 + ewc_loss), 0.0, 0.9)
        regularized_params = (1 - regularization_strength) * updated_params + regularization_strength * consolidated
        return regularized_params
    def _update_parameter_importance(self, interaction_data: InteractionData, learning_signal: Dict[str, Any]):
        signal_strength = learning_signal['strength']
        target_parameters = self._identify_target_parameters(interaction_data)
        for param_name in target_parameters:
            current_params = self._get_current_parameters(param_name)
            importance_update = torch.ones_like(current_params) * signal_strength ** 2
            if param_name in self.parameter_importance:
                self.parameter_importance[param_name] = self.config.importance_decay * self.parameter_importance[param_name] + (1 - self.config.importance_decay) * importance_update
            else:
                self.parameter_importance[param_name] = importance_update
    async def _perform_experience_replay(self) -> Dict[str, Any]:
        try:
            batch = self.experience_replay.sample_batch(self.config.replay_batch_size)
            if not batch:
                return {'success': True, 'replayed_count': 0}
            replay_results = []
            for experience in batch:
                if 'interaction_data' not in experience.architecture_params:
                    continue
                interaction_data = experience.architecture_params['interaction_data']
                learning_signal = experience.architecture_params['learning_signal']
                replay_learning_rate = self.current_learning_rate * 0.1
                original_lr = self.current_learning_rate
                self.current_learning_rate = replay_learning_rate
                try:
                    replay_result = await self._apply_online_update(interaction_data, learning_signal)
                    replay_results.append(replay_result)
                finally:
                    self.current_learning_rate = original_lr
            success_count = sum((1 for r in replay_results if r.get('success', False)))
            logger.debug(f'Experience replay: {success_count}/{len(replay_results)} successful')
            return {'success': True, 'replayed_count': len(replay_results), 'successful_count': success_count, 'batch_size': len(batch)}
        except Exception as e:
            logger.error(f'Experience replay failed: {e}')
            return {'success': False, 'error': str(e)}
    async def _perform_memory_consolidation(self) -> Dict[str, Any]:
        try:
            consolidation_count = 0
            for param_name, importance in self.parameter_importance.items():
                mean_importance = torch.mean(importance).item()
                if mean_importance > self.config.consolidation_strength:
                    current_params = self._get_current_parameters(param_name)
                    if param_name in self.consolidated_parameters:
                        weight = self.config.consolidation_strength
                        self.consolidated_parameters[param_name] = (1 - weight) * self.consolidated_parameters[param_name] + weight * current_params
                    else:
                        self.consolidated_parameters[param_name] = current_params.clone()
                    consolidation_count += 1
            self.learning_metrics['consolidations'] += 1
            logger.debug(f'Memory consolidation: consolidated {consolidation_count} parameters')
            return {'success': True, 'consolidated_parameters': consolidation_count, 'total_consolidated': len(self.consolidated_parameters)}
        except Exception as e:
            logger.error(f'Memory consolidation failed: {e}')
            return {'success': False, 'error': str(e)}
    def _create_experience_record(self, interaction_data: InteractionData, learning_signal: Dict[str, Any], update_result: Dict[str, Any]) -> ArchitecturePerformance:
        return ArchitecturePerformance(architecture_params={'interaction_data': interaction_data, 'learning_signal': learning_signal, 'update_result': update_result}, fitness_score=interaction_data.performance_feedback, generation=self.interaction_count, timestamp=interaction_data.timestamp, convergence_rate=learning_signal['strength'], diversity_metric=learning_signal['context_weight'])
    def _update_performance_tracking(self, interaction_data: InteractionData, update_result: Dict[str, Any]):
        performance_score = interaction_data.performance_feedback
        self.performance_history.append({'timestamp': interaction_data.timestamp, 'performance': performance_score, 'success': update_result.get('success', False), 'interaction_type': interaction_data.interaction_type})
        if len(self.performance_history) > self.config.performance_window:
            self.performance_history = self.performance_history[-self.config.performance_window:]
    def _adapt_learning_rate(self):
        if len(self.performance_history) < 10:
            return
        recent_performances = [p['performance'] for p in self.performance_history[-10:]]
        avg_performance = np.mean(recent_performances)
        if avg_performance < self.config.performance_threshold:
            self.current_learning_rate = min(self.config.learning_rate_base, self.current_learning_rate * 1.01)
        else:
            self.current_learning_rate = max(self.config.min_learning_rate, self.current_learning_rate * self.config.learning_rate_decay)
    def get_learning_stats(self) -> Dict[str, Any]:
        stats = {'metrics': self.learning_metrics.copy(), 'current_learning_rate': self.current_learning_rate, 'interaction_count': self.interaction_count, 'experience_count': len(self.experience_replay.experiences), 'consolidated_parameters': len(self.consolidated_parameters), 'parameter_importance_count': len(self.parameter_importance)}
        if self.performance_history:
            recent_performances = [p['performance'] for p in self.performance_history[-20:]]
            stats['performance_stats'] = {'mean': np.mean(recent_performances), 'std': np.std(recent_performances), 'min': np.min(recent_performances), 'max': np.max(recent_performances), 'recent_trend': np.mean(recent_performances[-5:]) - np.mean(recent_performances[-10:-5]) if len(recent_performances) >= 10 else 0.0}
        return stats
    async def reset_learning_state(self):
        """This allows for structured forgetting while retaining important knowledge."""
        self.interaction_count = 0
        self.current_learning_rate = self.config.learning_rate_base
        self.performance_history = []
        self.experience_replay = ExperienceReplay(max_size=self.config.max_experiences)
        consolidations = self.learning_metrics['consolidations']
        self.learning_metrics = {'total_interactions': 0, 'successful_adaptations': 0, 'forgetting_events': 0, 'consolidations': consolidations}
        logger.info('Learning state reset while preserving consolidated memory')