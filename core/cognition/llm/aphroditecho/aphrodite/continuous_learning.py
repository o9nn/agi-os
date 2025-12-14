"""
Continuous Learning System for Aphrodite Engine.

Implements online training from interaction data, experience replay, and 
catastrophic forgetting prevention using DTESN adaptive learning.

This system orchestrates existing components:
- ExperienceReplay from meta_optimizer.py
- DTESNDynamicIntegration for adaptive learning
- DynamicModelManager for incremental updates
- MLSystem interaction learning capabilities
"""

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


# Server-side learning extensions
@dataclass
class ServerSideConfig:
    """Configuration for server-side continuous learning."""
    # Server-side data collection
    enable_request_monitoring: bool = True
    enable_response_feedback: bool = True
    enable_user_interactions: bool = True
    
    # Background learning
    background_learning_interval: float = 30.0  # seconds
    max_concurrent_learning_tasks: int = 2
    enable_hot_swapping: bool = True
    
    # Production safety
    max_learning_rate_production: float = 0.0001
    learning_rate_decay_production: float = 0.99
    enable_rollback_on_failure: bool = True
    
    # Data aggregation
    min_interactions_for_learning: int = 10
    interaction_quality_threshold: float = 0.5
    enable_performance_feedback: bool = True


@dataclass
class InteractionFeedback:
    """Server-side interaction feedback data."""
    request_id: str
    endpoint: str
    method: str
    response_time_ms: float
    status_code: int
    user_satisfaction: Optional[float] = None  # -1 to 1
    error_occurred: bool = False
    model_confidence: Optional[float] = None
    timestamp: datetime = field(default_factory=datetime.now)
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class InteractionData:
    """Represents interaction data for continuous learning."""
    interaction_id: str
    interaction_type: str
    input_data: Dict[str, Any]
    output_data: Dict[str, Any] 
    performance_feedback: float  # -1 to 1 scale
    timestamp: datetime
    context_metadata: Dict[str, Any] = field(default_factory=dict)
    success: bool = True


@dataclass
class ContinuousLearningConfig:
    """Configuration for continuous learning system."""
    # Experience replay settings
    max_experiences: int = 10000
    replay_batch_size: int = 32
    replay_frequency: int = 10  # Every N interactions
    
    # Online learning settings
    learning_rate_base: float = 0.001
    learning_rate_decay: float = 0.99
    min_learning_rate: float = 1e-6
    adaptation_threshold: float = 0.1
    
    # Catastrophic forgetting prevention
    enable_ewc: bool = True  # Elastic Weight Consolidation
    ewc_lambda: float = 1000.0  # EWC regularization strength
    importance_decay: float = 0.9
    
    # Memory consolidation
    consolidation_frequency: int = 100  # Every N experiences
    consolidation_strength: float = 0.5
    
    # Performance monitoring
    performance_window: int = 50
    performance_threshold: float = 0.7


class ContinuousLearningSystem:
    """
    Unified continuous learning system for Aphrodite Engine.
    
    Orchestrates online learning, experience replay, and catastrophic 
    forgetting prevention across DTESN and dynamic model components.
    """
    
    def __init__(
        self,
        dynamic_manager: DynamicModelManager,
        dtesn_integration: DTESNDynamicIntegration,
        config: Optional[ContinuousLearningConfig] = None
    ):
        self.dynamic_manager = dynamic_manager
        self.dtesn_integration = dtesn_integration
        self.config = config or ContinuousLearningConfig()
        
        # Core components
        self.experience_replay = ExperienceReplay(max_size=self.config.max_experiences)
        self.interaction_count = 0
        self.current_learning_rate = self.config.learning_rate_base
        
        # Catastrophic forgetting prevention
        self.parameter_importance = {}  # Fisher Information Matrix approximation
        self.consolidated_parameters = {}  # Important parameter snapshots
        
        # Performance tracking
        self.performance_history = []
        self.learning_metrics = {
            'total_interactions': 0,
            'successful_adaptations': 0,
            'forgetting_events': 0,
            'consolidations': 0
        }
        
        logger.info(f"Continuous Learning System initialized with config: {config}")
    
    async def learn_from_interaction(
        self, 
        interaction_data: InteractionData
    ) -> Dict[str, Any]:
        """
        Learn from a single interaction with online adaptation.
        
        Args:
            interaction_data: The interaction to learn from
            
        Returns:
            Dictionary with learning results and metrics
        """
        try:
            start_time = time.time()
            self.interaction_count += 1
            self.learning_metrics['total_interactions'] += 1
            
            # Extract learning signal from interaction
            learning_signal = self._extract_learning_signal(interaction_data)
            
            # Apply online learning update
            update_result = await self._apply_online_update(
                interaction_data, learning_signal
            )
            
            # Store experience for replay
            experience = self._create_experience_record(
                interaction_data, learning_signal, update_result
            )
            self.experience_replay.add_experience(experience)
            
            # Update parameter importance for catastrophic forgetting prevention
            if self.config.enable_ewc:
                self._update_parameter_importance(
                    interaction_data, learning_signal
                )
            
            # Trigger experience replay if needed
            replay_result = None
            if self.interaction_count % self.config.replay_frequency == 0:
                replay_result = await self._perform_experience_replay()
            
            # Trigger memory consolidation if needed
            consolidation_result = None
            if self.interaction_count % self.config.consolidation_frequency == 0:
                consolidation_result = await self._perform_memory_consolidation()
            
            # Update performance tracking
            self._update_performance_tracking(interaction_data, update_result)
            
            # Adapt learning rate
            self._adapt_learning_rate()
            
            learning_time = time.time() - start_time
            
            result = {
                'success': update_result.get('success', False),
                'learning_signal': learning_signal,
                'online_update': update_result,
                'replay_result': replay_result,
                'consolidation_result': consolidation_result,
                'learning_time': learning_time,
                'interaction_count': self.interaction_count,
                'current_learning_rate': self.current_learning_rate,
                'metrics': self.learning_metrics.copy()
            }
            
            if result['success']:
                self.learning_metrics['successful_adaptations'] += 1
            
            logger.debug(
                f"Learned from interaction {interaction_data.interaction_id}: "
                f"success={result['success']}, time={learning_time:.4f}s"
            )
            
            return result
            
        except Exception as e:
            logger.error(f"Failed to learn from interaction: {e}")
            return {
                'success': False,
                'error': str(e),
                'interaction_count': self.interaction_count,
                'metrics': self.learning_metrics.copy()
            }
    
    def _extract_learning_signal(self, interaction_data: InteractionData) -> Dict[str, Any]:
        """Extract learning signal from interaction data."""
        # Base learning signal from performance feedback
        signal_strength = abs(interaction_data.performance_feedback)
        signal_direction = np.sign(interaction_data.performance_feedback)
        
        # Context-aware signal modification
        context_weight = 1.0
        if 'importance' in interaction_data.context_metadata:
            context_weight = float(interaction_data.context_metadata['importance'])
        
        # Temporal recency weighting
        time_diff = (datetime.now() - interaction_data.timestamp).total_seconds()
        temporal_weight = np.exp(-time_diff / 3600)  # Decay over hours
        
        learning_signal = {
            'strength': signal_strength * context_weight * temporal_weight,
            'direction': signal_direction,
            'context_weight': context_weight,
            'temporal_weight': temporal_weight,
            'raw_feedback': interaction_data.performance_feedback
        }
        
        return learning_signal
    
    async def _apply_online_update(
        self, 
        interaction_data: InteractionData, 
        learning_signal: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Apply online learning update using DTESN integration."""
        try:
            # Determine which parameters to update based on interaction type
            target_parameters = self._identify_target_parameters(interaction_data)
            
            update_results = {}
            
            for param_name in target_parameters:
                # Get current parameters (placeholder - would interface with model)
                current_params = self._get_current_parameters(param_name)
                
                # Compute update direction from learning signal
                update_gradient = self._compute_update_gradient(
                    interaction_data, learning_signal, param_name
                )
                
                # Apply DTESN adaptive update with catastrophic forgetting prevention
                updated_params, dtesn_metrics = await self.dtesn_integration.adaptive_parameter_update(
                    parameter_name=param_name,
                    current_params=current_params,
                    target_gradient=update_gradient,
                    performance_feedback=learning_signal['raw_feedback']
                )
                
                # Apply EWC regularization if enabled
                if self.config.enable_ewc and param_name in self.parameter_importance:
                    updated_params = self._apply_ewc_regularization(
                        param_name, current_params, updated_params
                    )
                
                # Update parameters via dynamic model manager
                update_request = IncrementalUpdateRequest(
                    parameter_name=param_name,
                    update_data=updated_params,
                    learning_rate=self.current_learning_rate,
                    update_type="replace",
                    metadata={
                        'interaction_id': interaction_data.interaction_id,
                        'learning_signal': learning_signal,
                        'dtesn_metrics': dtesn_metrics
                    }
                )
                
                dm_result = await self.dynamic_manager.apply_incremental_update(update_request)
                
                update_results[param_name] = {
                    'dtesn_metrics': dtesn_metrics,
                    'dynamic_manager_result': dm_result,
                    'parameter_shape': updated_params.shape if hasattr(updated_params, 'shape') else None
                }
            
            return {
                'success': all(r.get('dynamic_manager_result', {}).get('success', False) 
                              for r in update_results.values()),
                'updated_parameters': list(target_parameters),
                'update_results': update_results
            }
            
        except Exception as e:
            logger.error(f"Failed to apply online update: {e}")
            return {'success': False, 'error': str(e)}
    
    def _identify_target_parameters(self, interaction_data: InteractionData) -> List[str]:
        """Identify which model parameters should be updated for this interaction."""
        # Default parameter mapping based on interaction type
        parameter_map = {
            'text_generation': ['transformer.h.*.mlp.c_proj.weight', 'transformer.h.*.attn.c_proj.weight'],
            'reasoning': ['transformer.h.*.attn.c_attn.weight', 'transformer.h.*.mlp.c_fc.weight'], 
            'memory_recall': ['transformer.wte.weight', 'transformer.h.*.attn.c_attn.weight'],
            'default': ['transformer.h.10.mlp.c_proj.weight']  # Middle layer as default
        }
        
        interaction_type = interaction_data.interaction_type
        return parameter_map.get(interaction_type, parameter_map['default'])
    
    def _get_current_parameters(self, param_name: str) -> torch.Tensor:
        """Get current parameters for the named parameter."""
        # Placeholder implementation - in real system would interface with model
        # For now, return a small tensor for demonstration
        if 'mlp' in param_name:
            return torch.randn(768, 3072)  # Typical MLP dimension
        elif 'attn' in param_name:
            return torch.randn(768, 768)   # Typical attention dimension
        else:
            return torch.randn(768, 768)   # Default dimension
    
    def _compute_update_gradient(
        self,
        interaction_data: InteractionData,
        learning_signal: Dict[str, Any], 
        param_name: str
    ) -> torch.Tensor:
        """Compute gradient for parameter update."""
        # Get parameter shape to generate appropriate gradient
        current_params = self._get_current_parameters(param_name)
        
        # Generate gradient based on learning signal
        signal_strength = learning_signal['strength']
        signal_direction = learning_signal['direction']
        
        # Create gradient with appropriate scale and direction
        gradient_scale = signal_strength * self.current_learning_rate * 0.1
        gradient = torch.randn_like(current_params) * gradient_scale * signal_direction
        
        return gradient
    
    def _apply_ewc_regularization(
        self,
        param_name: str,
        current_params: torch.Tensor,
        updated_params: torch.Tensor
    ) -> torch.Tensor:
        """Apply Elastic Weight Consolidation to prevent catastrophic forgetting."""
        if param_name not in self.parameter_importance:
            return updated_params
        
        importance = self.parameter_importance[param_name]
        consolidated = self.consolidated_parameters.get(param_name, current_params)
        
        # EWC penalty term
        ewc_loss = self.config.ewc_lambda * importance * (updated_params - consolidated)**2
        
        # Apply regularization by moving updated parameters towards consolidated ones
        regularization_strength = torch.clamp(ewc_loss / (1.0 + ewc_loss), 0.0, 0.9)
        
        regularized_params = (
            (1 - regularization_strength) * updated_params + 
            regularization_strength * consolidated
        )
        
        return regularized_params
    
    def _update_parameter_importance(
        self,
        interaction_data: InteractionData,
        learning_signal: Dict[str, Any]
    ):
        """Update parameter importance estimates for EWC."""
        # Simplified Fisher Information approximation
        signal_strength = learning_signal['strength']
        
        target_parameters = self._identify_target_parameters(interaction_data)
        
        for param_name in target_parameters:
            current_params = self._get_current_parameters(param_name)
            
            # Estimate importance as squared gradient magnitude
            importance_update = torch.ones_like(current_params) * signal_strength**2
            
            if param_name in self.parameter_importance:
                # Exponential moving average of importance
                self.parameter_importance[param_name] = (
                    self.config.importance_decay * self.parameter_importance[param_name] +
                    (1 - self.config.importance_decay) * importance_update
                )
            else:
                self.parameter_importance[param_name] = importance_update
    
    async def _perform_experience_replay(self) -> Dict[str, Any]:
        """Perform experience replay to reinforce important learning."""
        try:
            # Sample batch of experiences
            batch = self.experience_replay.sample_batch(self.config.replay_batch_size)
            
            if not batch:
                return {'success': True, 'replayed_count': 0}
            
            replay_results = []
            
            for experience in batch:
                # Extract interaction data from experience (stored in architecture_params)
                if 'interaction_data' not in experience.architecture_params:
                    continue
                
                interaction_data = experience.architecture_params['interaction_data']
                learning_signal = experience.architecture_params['learning_signal']
                
                # Apply reduced-strength replay update
                replay_learning_rate = self.current_learning_rate * 0.1  # Reduced for replay
                original_lr = self.current_learning_rate
                self.current_learning_rate = replay_learning_rate
                
                try:
                    replay_result = await self._apply_online_update(
                        interaction_data, learning_signal
                    )
                    replay_results.append(replay_result)
                finally:
                    self.current_learning_rate = original_lr
            
            success_count = sum(1 for r in replay_results if r.get('success', False))
            
            logger.debug(f"Experience replay: {success_count}/{len(replay_results)} successful")
            
            return {
                'success': True,
                'replayed_count': len(replay_results),
                'successful_count': success_count,
                'batch_size': len(batch)
            }
            
        except Exception as e:
            logger.error(f"Experience replay failed: {e}")
            return {'success': False, 'error': str(e)}
    
    async def _perform_memory_consolidation(self) -> Dict[str, Any]:
        """Consolidate important parameters to prevent forgetting."""
        try:
            consolidation_count = 0
            
            # Consolidate parameters with high importance
            for param_name, importance in self.parameter_importance.items():
                mean_importance = torch.mean(importance).item()
                
                if mean_importance > self.config.consolidation_strength:
                    current_params = self._get_current_parameters(param_name)
                    
                    # Update consolidated parameters
                    if param_name in self.consolidated_parameters:
                        # Weighted average with existing consolidated parameters
                        weight = self.config.consolidation_strength
                        self.consolidated_parameters[param_name] = (
                            (1 - weight) * self.consolidated_parameters[param_name] +
                            weight * current_params
                        )
                    else:
                        self.consolidated_parameters[param_name] = current_params.clone()
                    
                    consolidation_count += 1
            
            self.learning_metrics['consolidations'] += 1
            
            logger.debug(f"Memory consolidation: consolidated {consolidation_count} parameters")
            
            return {
                'success': True,
                'consolidated_parameters': consolidation_count,
                'total_consolidated': len(self.consolidated_parameters)
            }
            
        except Exception as e:
            logger.error(f"Memory consolidation failed: {e}")
            return {'success': False, 'error': str(e)}
    
    def _create_experience_record(
        self,
        interaction_data: InteractionData,
        learning_signal: Dict[str, Any],
        update_result: Dict[str, Any]
    ) -> ArchitecturePerformance:
        """Create experience record for replay system."""
        return ArchitecturePerformance(
            architecture_params={
                'interaction_data': interaction_data,
                'learning_signal': learning_signal,
                'update_result': update_result
            },
            fitness_score=interaction_data.performance_feedback,
            generation=self.interaction_count,
            timestamp=interaction_data.timestamp,
            convergence_rate=learning_signal['strength'],
            diversity_metric=learning_signal['context_weight']
        )
    
    def _update_performance_tracking(
        self,
        interaction_data: InteractionData,
        update_result: Dict[str, Any]
    ):
        """Update performance tracking metrics."""
        performance_score = interaction_data.performance_feedback
        self.performance_history.append({
            'timestamp': interaction_data.timestamp,
            'performance': performance_score,
            'success': update_result.get('success', False),
            'interaction_type': interaction_data.interaction_type
        })
        
        # Keep only recent performance history
        if len(self.performance_history) > self.config.performance_window:
            self.performance_history = self.performance_history[-self.config.performance_window:]
    
    def _adapt_learning_rate(self):
        """Adapt learning rate based on recent performance."""
        if len(self.performance_history) < 10:
            return
        
        # Calculate recent performance trend
        recent_performances = [p['performance'] for p in self.performance_history[-10:]]
        avg_performance = np.mean(recent_performances)
        
        # Adapt learning rate based on performance
        if avg_performance < self.config.performance_threshold:
            # Performance is low, increase learning rate slightly
            self.current_learning_rate = min(
                self.config.learning_rate_base,
                self.current_learning_rate * 1.01
            )
        else:
            # Performance is good, apply decay
            self.current_learning_rate = max(
                self.config.min_learning_rate,
                self.current_learning_rate * self.config.learning_rate_decay
            )
    
    def get_learning_stats(self) -> Dict[str, Any]:
        """Get comprehensive learning statistics."""
        stats = {
            'metrics': self.learning_metrics.copy(),
            'current_learning_rate': self.current_learning_rate,
            'interaction_count': self.interaction_count,
            'experience_count': len(self.experience_replay.experiences),
            'consolidated_parameters': len(self.consolidated_parameters),
            'parameter_importance_count': len(self.parameter_importance)
        }
        
        # Performance statistics
        if self.performance_history:
            recent_performances = [p['performance'] for p in self.performance_history[-20:]]
            stats['performance_stats'] = {
                'mean': np.mean(recent_performances),
                'std': np.std(recent_performances),
                'min': np.min(recent_performances),
                'max': np.max(recent_performances),
                'recent_trend': np.mean(recent_performances[-5:]) - np.mean(recent_performances[-10:-5])
                    if len(recent_performances) >= 10 else 0.0
            }
        
        return stats
    
    async def reset_learning_state(self):
        """Reset learning state while preserving consolidated memory."""
        """This allows for structured forgetting while retaining important knowledge."""
        self.interaction_count = 0
        self.current_learning_rate = self.config.learning_rate_base
        self.performance_history = []
        
        # Preserve consolidated parameters but reset working memory
        self.experience_replay = ExperienceReplay(max_size=self.config.max_experiences)
        
        # Reset metrics but preserve consolidation count
        consolidations = self.learning_metrics['consolidations']
        self.learning_metrics = {
            'total_interactions': 0,
            'successful_adaptations': 0,
            'forgetting_events': 0,
            'consolidations': consolidations
        }
        
        logger.info("Learning state reset while preserving consolidated memory")