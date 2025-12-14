"""
Integration bridge between Dynamic Model Updates and DTESN Adaptive Learning.

Connects the Aphrodite dynamic model manager with the Echo.Kern DTESN
cognitive learning system for enhanced online learning capabilities.
"""

import asyncio
import logging
import numpy as np
import torch
from pathlib import Path
from typing import Dict, Any, Optional, List, Tuple
from dataclasses import dataclass

from aphrodite.dynamic_model_manager import (
    DynamicModelManager,
    IncrementalUpdateRequest
)

logger = logging.getLogger(__name__)


@dataclass
class DTESNLearningConfig:
    """Configuration for DTESN learning integration."""
    learning_rate: float = 0.01
    adaptation_rate: float = 0.001
    max_iterations: int = 100
    convergence_threshold: float = 1e-4
    enable_plasticity: bool = True
    enable_homeostasis: bool = True
    batch_size: int = 32
    reservoir_size: int = 1000


class DTESNDynamicIntegration:
    """
    Integration bridge between Aphrodite dynamic updates and DTESN learning.
    
    Provides enhanced online learning by leveraging DTESN's adaptive
    cognitive computing capabilities for model parameter updates.
    """
    
    def __init__(
        self,
        dynamic_manager: DynamicModelManager,
        dtesn_config: Optional[DTESNLearningConfig] = None
    ):
        self.dynamic_manager = dynamic_manager
        self.dtesn_config = dtesn_config or DTESNLearningConfig()
        
        # DTESN integration state
        self.dtesn_available = False
        self.learning_history: List[Dict[str, Any]] = []
        
        # Initialize DTESN integration
        self._initialize_dtesn()
    
    def _initialize_dtesn(self):
        """Initialize DTESN cognitive learning integration."""
        try:
            # Try to load DTESN library if available
            echo_kern_path = Path(__file__).parent.parent / "echo.kern"
            if (echo_kern_path / "lib").exists():
                # DTESN library is available
                self.dtesn_available = True
                logger.info("DTESN cognitive learning integration enabled")
            else:
                logger.info("DTESN library not found, using standard learning")
                
        except Exception as e:
            logger.warning(f"Failed to initialize DTESN integration: {e}")
            self.dtesn_available = False
    
    async def adaptive_parameter_update(
        self,
        parameter_name: str,
        current_params: torch.Tensor,
        target_gradient: torch.Tensor,
        performance_feedback: float
    ) -> Tuple[torch.Tensor, Dict[str, Any]]:
        """
        Apply adaptive parameter update using DTESN learning algorithms.
        
        Args:
            parameter_name: Name of parameter to update
            current_params: Current parameter values
            target_gradient: Target gradient for update
            performance_feedback: Performance feedback signal (-1 to 1)
            
        Returns:
            Tuple of (updated_parameters, learning_metrics)
        """
        if self.dtesn_available:
            return await self._dtesn_adaptive_update(
                parameter_name, current_params, target_gradient, performance_feedback
            )
        else:
            return await self._standard_adaptive_update(
                parameter_name, current_params, target_gradient, performance_feedback
            )
    
    async def _dtesn_adaptive_update(
        self,
        parameter_name: str,
        current_params: torch.Tensor,
        target_gradient: torch.Tensor,
        performance_feedback: float
    ) -> Tuple[torch.Tensor, Dict[str, Any]]:
        """Apply DTESN-based adaptive learning update."""
        try:
            # Convert tensors to numpy for DTESN processing
            params_np = current_params.detach().cpu().numpy()
            gradient_np = target_gradient.detach().cpu().numpy()
            
            # Apply DTESN learning algorithms
            updated_params, metrics = await self._apply_dtesn_learning(
                params_np, gradient_np, performance_feedback
            )
            
            # Convert back to tensor
            updated_tensor = torch.from_numpy(updated_params).to(current_params.device)
            
            # Record learning history
            self.learning_history.append({
                "parameter_name": parameter_name,
                "timestamp": asyncio.get_event_loop().time(),
                "performance_feedback": performance_feedback,
                "learning_type": "dtesn_adaptive",
                "metrics": metrics
            })
            
            return updated_tensor, metrics
            
        except Exception as e:
            logger.error(f"DTESN adaptive update failed: {e}")
            # Fall back to standard update
            return await self._standard_adaptive_update(
                parameter_name, current_params, target_gradient, performance_feedback
            )
    
    async def _apply_dtesn_learning(
        self,
        params: np.ndarray,
        gradient: np.ndarray,
        feedback: float
    ) -> Tuple[np.ndarray, Dict[str, Any]]:
        """
        Apply DTESN learning algorithms to parameter update.
        
        Simulates the DTESN adaptive learning process with different
        learning rules based on performance feedback.
        """
        # Choose learning algorithm based on feedback
        if feedback > 0.5:
            # High performance - use Hebbian learning
            return await self._apply_hebbian_learning(params, gradient, feedback)
        elif feedback > 0.0:
            # Moderate performance - use STDP
            return await self._apply_stdp_learning(params, gradient, feedback)
        elif feedback > -0.5:
            # Poor performance - use BCM rule
            return await self._apply_bcm_learning(params, gradient, feedback)
        else:
            # Very poor performance - use reinforcement learning
            return await self._apply_reinforcement_learning(params, gradient, feedback)
    
    async def _apply_hebbian_learning(
        self, 
        params: np.ndarray, 
        gradient: np.ndarray, 
        feedback: float
    ) -> Tuple[np.ndarray, Dict[str, Any]]:
        """Apply Hebbian learning rule."""
        # Hebbian learning: strengthen connections that fire together
        learning_rate = self.dtesn_config.learning_rate * feedback
        
        # Simple Hebbian update: dw = lr * x * y
        param_activity = np.tanh(params)  # Activation function
        gradient_activity = np.tanh(gradient)  # Target activity
        
        weight_delta = learning_rate * param_activity * gradient_activity
        updated_params = params + weight_delta
        
        metrics = {
            "learning_type": "hebbian",
            "learning_rate": learning_rate,
            "weight_delta_mean": float(np.mean(weight_delta)),
            "weight_delta_std": float(np.std(weight_delta)),
            "convergence": float(np.linalg.norm(weight_delta))
        }
        
        return updated_params, metrics
    
    async def _apply_stdp_learning(
        self, 
        params: np.ndarray, 
        gradient: np.ndarray, 
        feedback: float
    ) -> Tuple[np.ndarray, Dict[str, Any]]:
        """Apply Spike-Timing Dependent Plasticity (STDP)."""
        # STDP: timing-dependent synaptic plasticity
        learning_rate = self.dtesn_config.learning_rate * (0.5 + feedback)
        
        # Simulate timing-dependent updates
        timing_window = np.exp(-0.5 * np.abs(params - gradient))
        weight_delta = learning_rate * gradient * timing_window
        updated_params = params + weight_delta
        
        metrics = {
            "learning_type": "stdp",
            "learning_rate": learning_rate,
            "timing_correlation": float(np.mean(timing_window)),
            "weight_delta_mean": float(np.mean(weight_delta)),
            "synaptic_efficacy": float(np.mean(np.abs(updated_params)))
        }
        
        return updated_params, metrics
    
    async def _apply_bcm_learning(
        self, 
        params: np.ndarray, 
        gradient: np.ndarray, 
        feedback: float
    ) -> Tuple[np.ndarray, Dict[str, Any]]:
        """Apply BCM (Bienenstock-Cooper-Munro) learning rule."""
        # BCM: sliding threshold for synaptic modification
        learning_rate = self.dtesn_config.learning_rate * abs(feedback)
        
        # Compute sliding threshold
        activity = np.tanh(params)
        theta = np.mean(activity**2) + 0.01  # Sliding threshold
        
        # BCM update rule
        post_activity = np.tanh(gradient)
        weight_delta = learning_rate * activity * post_activity * (post_activity - theta)
        updated_params = params + weight_delta
        
        metrics = {
            "learning_type": "bcm",
            "learning_rate": learning_rate,
            "sliding_threshold": float(theta),
            "weight_delta_mean": float(np.mean(weight_delta)),
            "homeostatic_regulation": float(np.mean(activity**2))
        }
        
        return updated_params, metrics
    
    async def _apply_reinforcement_learning(
        self, 
        params: np.ndarray, 
        gradient: np.ndarray, 
        feedback: float
    ) -> Tuple[np.ndarray, Dict[str, Any]]:
        """Apply reward-modulated reinforcement learning."""
        # Reinforcement learning: reward-modulated updates
        learning_rate = self.dtesn_config.learning_rate
        
        # Use feedback as reward signal
        reward = feedback
        
        # Reward-modulated weight update
        activity = np.tanh(params)
        weight_delta = learning_rate * reward * activity * gradient
        updated_params = params + weight_delta
        
        metrics = {
            "learning_type": "reinforcement",
            "learning_rate": learning_rate,
            "reward_signal": float(reward),
            "weight_delta_mean": float(np.mean(weight_delta)),
            "policy_gradient": float(np.mean(gradient * activity))
        }
        
        return updated_params, metrics
    
    async def _standard_adaptive_update(
        self,
        parameter_name: str,
        current_params: torch.Tensor,
        target_gradient: torch.Tensor,
        performance_feedback: float
    ) -> Tuple[torch.Tensor, Dict[str, Any]]:
        """Apply standard adaptive learning update when DTESN is not available."""
        # Adaptive learning rate based on performance feedback
        base_lr = self.dtesn_config.learning_rate
        adaptive_lr = base_lr * (1.0 + performance_feedback)
        
        # Momentum-based update with performance modulation
        momentum = 0.9 * max(0.0, performance_feedback)
        
        # Apply update
        weight_delta = adaptive_lr * target_gradient
        if hasattr(self, '_momentum_buffer'):
            if parameter_name in self._momentum_buffer:
                momentum_term = momentum * self._momentum_buffer[parameter_name]
                weight_delta += momentum_term
        else:
            self._momentum_buffer = {}
        
        # Store momentum for next update
        self._momentum_buffer[parameter_name] = weight_delta.clone()
        
        updated_params = current_params + weight_delta
        
        metrics = {
            "learning_type": "standard_adaptive",
            "learning_rate": adaptive_lr,
            "momentum": momentum,
            "weight_delta_mean": float(weight_delta.mean()),
            "weight_delta_std": float(weight_delta.std())
        }
        
        return updated_params, metrics
    
    async def apply_ewc_constraint(
        self,
        parameter_name: str,
        current_params: torch.Tensor,
        proposed_update: torch.Tensor,
        fisher_information: torch.Tensor,
        consolidated_params: torch.Tensor,
        ewc_lambda: float = 1000.0
    ) -> Tuple[torch.Tensor, Dict[str, Any]]:
        """
        Apply Elastic Weight Consolidation constraint to prevent catastrophic forgetting.
        
        Args:
            parameter_name: Name of the parameter being updated
            current_params: Current parameter values
            proposed_update: Proposed parameter update
            fisher_information: Fisher Information Matrix diagonal approximation
            consolidated_params: Previously consolidated important parameters
            ewc_lambda: EWC regularization strength
            
        Returns:
            Tuple of (constrained_update, ewc_metrics)
        """
        try:
            # Compute EWC penalty for proposed update
            param_diff = proposed_update - consolidated_params
            ewc_penalty = fisher_information * (param_diff ** 2)
            total_ewc_loss = torch.sum(ewc_penalty)
            
            # Adaptive constraint strength based on parameter importance
            importance_weights = torch.clamp(fisher_information / torch.max(fisher_information), 0.1, 1.0)
            
            # Apply constraint: blend between proposed update and consolidated parameters
            constraint_strength = torch.sigmoid(ewc_lambda * importance_weights * param_diff.abs())
            constrained_update = (
                (1 - constraint_strength) * proposed_update + 
                constraint_strength * consolidated_params
            )
            
            # Calculate how much the constraint modified the update
            modification_magnitude = torch.norm(constrained_update - proposed_update).item()
            preservation_score = torch.norm(constrained_update - consolidated_params).item() / (
                torch.norm(proposed_update - consolidated_params).item() + 1e-8
            )
            
            ewc_metrics = {
                "ewc_loss": float(total_ewc_loss),
                "ewc_lambda": ewc_lambda,
                "constraint_strength_mean": float(torch.mean(constraint_strength)),
                "modification_magnitude": modification_magnitude,
                "preservation_score": preservation_score,
                "parameter_importance_mean": float(torch.mean(fisher_information)),
                "forgetting_prevention_active": modification_magnitude > 1e-6
            }
            
            logger.debug(
                f"EWC constraint applied to {parameter_name}: "
                f"loss={total_ewc_loss:.6f}, modification={modification_magnitude:.6f}"
            )
            
            return constrained_update, ewc_metrics
            
        except Exception as e:
            logger.error(f"EWC constraint application failed: {e}")
            # Fallback to original update if EWC fails
            return proposed_update, {
                "ewc_error": str(e),
                "fallback_used": True,
                "forgetting_prevention_active": False
            }
    
    async def update_fisher_information(
        self,
        parameter_name: str,
        current_params: torch.Tensor,
        gradient: torch.Tensor,
        learning_signal_strength: float
    ) -> torch.Tensor:
        """
        Update Fisher Information Matrix approximation for EWC.
        
        The Fisher Information Matrix represents the importance of each parameter
        for the current task performance.
        
        Args:
            parameter_name: Name of the parameter
            current_params: Current parameter values
            gradient: Gradient from current learning
            learning_signal_strength: Strength of the learning signal (0-1)
            
        Returns:
            Updated Fisher Information Matrix (diagonal approximation)
        """
        try:
            # Simple diagonal Fisher Information approximation using squared gradients
            # This approximates the second derivative of the loss w.r.t. parameters
            fisher_update = (gradient ** 2) * learning_signal_strength
            
            # Get existing Fisher information or initialize
            if not hasattr(self, '_fisher_information'):
                self._fisher_information = {}
            
            if parameter_name in self._fisher_information:
                # Exponential moving average update
                decay_rate = 0.9
                existing_fisher = self._fisher_information[parameter_name]
                updated_fisher = decay_rate * existing_fisher + (1 - decay_rate) * fisher_update
            else:
                updated_fisher = fisher_update
            
            self._fisher_information[parameter_name] = updated_fisher
            
            logger.debug(
                f"Updated Fisher information for {parameter_name}: "
                f"mean={torch.mean(updated_fisher):.6f}, max={torch.max(updated_fisher):.6f}"
            )
            
            return updated_fisher
            
        except Exception as e:
            logger.error(f"Fisher information update failed: {e}")
            # Return uniform importance if computation fails
            return torch.ones_like(current_params) * 0.1
    
    async def consolidate_important_parameters(
        self,
        parameter_name: str,
        current_params: torch.Tensor,
        fisher_information: torch.Tensor,
        consolidation_threshold: float = 0.5
    ) -> Tuple[bool, Dict[str, Any]]:
        """
        Consolidate parameters that are deemed important for retention.
        
        Args:
            parameter_name: Name of the parameter to potentially consolidate
            current_params: Current parameter values
            fisher_information: Fisher Information Matrix for importance
            consolidation_threshold: Threshold for consolidation decision
            
        Returns:
            Tuple of (was_consolidated, consolidation_metrics)
        """
        try:
            # Determine if parameters should be consolidated based on importance
            mean_importance = torch.mean(fisher_information).item()
            max_importance = torch.max(fisher_information).item()
            
            should_consolidate = mean_importance > consolidation_threshold
            
            if should_consolidate:
                # Store consolidated parameters
                if not hasattr(self, '_consolidated_parameters'):
                    self._consolidated_parameters = {}
                
                if parameter_name in self._consolidated_parameters:
                    # Weighted average with existing consolidated parameters
                    existing_consolidated = self._consolidated_parameters[parameter_name]
                    importance_weight = min(mean_importance, 0.8)  # Cap at 0.8
                    
                    consolidated = (
                        (1 - importance_weight) * existing_consolidated +
                        importance_weight * current_params
                    )
                else:
                    consolidated = current_params.clone()
                
                self._consolidated_parameters[parameter_name] = consolidated
                
                consolidation_metrics = {
                    "parameter_name": parameter_name,
                    "consolidated": True,
                    "mean_importance": mean_importance,
                    "max_importance": max_importance,
                    "consolidation_threshold": consolidation_threshold,
                    "parameter_norm": float(torch.norm(consolidated)),
                    "importance_distribution": {
                        "q25": float(torch.quantile(fisher_information, 0.25)),
                        "q50": float(torch.quantile(fisher_information, 0.50)),
                        "q75": float(torch.quantile(fisher_information, 0.75))
                    }
                }
                
                logger.info(
                    f"Consolidated parameters for {parameter_name}: "
                    f"importance={mean_importance:.4f}, norm={consolidation_metrics['parameter_norm']:.4f}"
                )
                
                return True, consolidation_metrics
            
            else:
                return False, {
                    "consolidated": False,
                    "mean_importance": mean_importance,
                    "consolidation_threshold": consolidation_threshold,
                    "reason": "importance_below_threshold"
                }
                
        except Exception as e:
            logger.error(f"Parameter consolidation failed: {e}")
            return False, {"consolidated": False, "error": str(e)}
    
    def get_consolidated_parameters(self) -> Dict[str, torch.Tensor]:
        """Get all consolidated parameters."""
        if hasattr(self, '_consolidated_parameters'):
            return self._consolidated_parameters.copy()
        return {}
    
    def get_fisher_information(self) -> Dict[str, torch.Tensor]:
        """Get all Fisher Information matrices."""
        if hasattr(self, '_fisher_information'):
            return self._fisher_information.copy()
        return {}
    
    async def enhanced_incremental_update(
        self,
        parameter_name: str,
        update_data: torch.Tensor,
        learning_rate: float = None,
        performance_context: Optional[Dict[str, float]] = None
    ) -> Dict[str, Any]:
        """
        Apply enhanced incremental update with DTESN cognitive learning.
        
        Args:
            parameter_name: Name of parameter to update
            update_data: Update data/gradient
            learning_rate: Optional override learning rate
            performance_context: Performance metrics for adaptation
            
        Returns:
            Update result with enhanced metrics
        """
        try:
            # Get performance feedback
            if performance_context:
                accuracy_change = performance_context.get("accuracy_change", 0.0)
                latency_change = performance_context.get("latency_change", 0.0)
                
                # Combine metrics into feedback signal
                feedback = accuracy_change - 0.1 * max(0, latency_change)
                feedback = np.clip(feedback, -1.0, 1.0)
            else:
                feedback = 0.0
            
            # Get current parameter values (mock for now)
            current_params = torch.randn_like(update_data)  # Would get from actual model
            
            # Apply adaptive update
            updated_params, learning_metrics = await self.adaptive_parameter_update(
                parameter_name, current_params, update_data, feedback
            )
            
            # Create incremental update request
            request = IncrementalUpdateRequest(
                parameter_name=parameter_name,
                update_data=updated_params - current_params,  # Delta update
                learning_rate=learning_rate or self.dtesn_config.learning_rate,
                update_type="additive",
                metadata={
                    "dtesn_enhanced": self.dtesn_available,
                    "learning_metrics": learning_metrics,
                    "performance_feedback": feedback
                }
            )
            
            # Apply through dynamic manager
            result = await self.dynamic_manager.apply_incremental_update(request)
            
            # Enhance result with DTESN metrics
            if result["success"]:
                result["data"]["dtesn_metrics"] = learning_metrics
                result["data"]["learning_algorithm"] = learning_metrics.get("learning_type", "standard")
            
            return result
            
        except Exception as e:
            logger.error(f"Enhanced incremental update failed: {e}")
            return {
                "success": False,
                "reason": f"Enhanced update failed: {str(e)}"
            }
    
    def get_learning_history(self) -> List[Dict[str, Any]]:
        """Get history of DTESN learning applications."""
        return self.learning_history.copy()
    
    def get_integration_status(self) -> Dict[str, Any]:
        """Get status of DTESN integration."""
        return {
            "dtesn_available": self.dtesn_available,
            "total_learning_updates": len(self.learning_history),
            "config": {
                "learning_rate": self.dtesn_config.learning_rate,
                "adaptation_rate": self.dtesn_config.adaptation_rate,
                "reservoir_size": self.dtesn_config.reservoir_size,
                "plasticity_enabled": self.dtesn_config.enable_plasticity,
                "homeostasis_enabled": self.dtesn_config.enable_homeostasis
            },
            "recent_algorithms": [
                entry["learning_type"] 
                for entry in self.learning_history[-10:]
                if "learning_type" in entry.get("metrics", {})
            ]
        }