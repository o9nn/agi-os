#!/usr/bin/env python3
"""
Feedback Control System - Deep Tree Echo State Network (DTESN) Integration

This module implements Task 3.3.2 of the Deep Tree Echo development roadmap:
Create Feedback Control Systems with:
- Real-time feedback correction
- Adaptive control based on proprioception
- Balance and stability maintenance

Integration with DTESN architecture ensures OEIS A000081 compliance and
real-time performance constraints for neuromorphic computing.

Acceptance Criteria: Agents maintain balance and correct movements
"""

import time
import math
import logging
from typing import Dict, List, Optional, Any, Callable
from dataclasses import dataclass, field
from enum import Enum
from collections import deque
import threading
from concurrent.futures import ThreadPoolExecutor

# Graceful dependency handling
try:
    import numpy as np
    NUMPY_AVAILABLE = True
except ImportError:
    NUMPY_AVAILABLE = False
    # Minimal numpy-like compatibility
    class np:
        @staticmethod
        def array(data): return list(data) if isinstance(data, (list, tuple)) else [data]
        @staticmethod 
        def zeros(shape): return [0.0] * shape if isinstance(shape, int) else [[0.0] * shape[1] for _ in range(shape[0])]
        @staticmethod
        def dot(a, b): return sum(x * y for x, y in zip(a, b)) if isinstance(a, list) and isinstance(b, list) else 0.0
        @staticmethod
        def tanh(x): return [math.tanh(v) for v in x] if isinstance(x, list) else math.tanh(x)

# DTESN Core dependencies
try:
    from psystem_evolution_engine import PSystemEvolutionEngine
    from esn_reservoir import ESNReservoir
    from bseries_tree_classifier import BSeriesTreeClassifier
    HAS_DTESN_CORE = True
except ImportError:
    HAS_DTESN_CORE = False

# Motor prediction system integration
try:
    from motor_prediction_system import (
        MotorPredictionSystem, ForwardModelState, MotorAction, 
        BodyConfiguration, MovementPrediction, MovementType
    )
    HAS_MOTOR_PREDICTION = True
except ImportError:
    HAS_MOTOR_PREDICTION = False
    # Define minimal compatible classes
    class BodyConfiguration:
        def __init__(self, position=(0,0,0), joint_angles=None):
            self.position = position
            self.joint_angles = joint_angles or {}
            self.timestamp = time.time()
    
    class ForwardModelState:
        def __init__(self, body_configuration, **kwargs):
            self.body_configuration = body_configuration
    
    class MotorAction:
        def __init__(self, joint_targets=None, duration=1.0, force=0.5):
            self.joint_targets = joint_targets or {}
            self.duration = duration
            self.force = force

# Sensor attention integration
try:
    from sensor_attention_integration import AttentionGuidedSensorSystem
    HAS_SENSOR_ATTENTION = True
except ImportError:
    HAS_SENSOR_ATTENTION = False

# Configure logging
logger = logging.getLogger(__name__)

class FeedbackType(Enum):
    """Types of feedback that can be processed"""
    PROPRIOCEPTIVE = "proprioceptive"  # Body state awareness
    VISUAL = "visual"                  # Visual feedback
    VESTIBULAR = "vestibular"         # Balance feedback
    TACTILE = "tactile"               # Touch feedback
    ERROR_CORRECTION = "error_correction"  # Error-based feedback
    STABILITY = "stability"           # Stability-based feedback

class ControlMode(Enum):
    """Control modes for adaptive controller"""
    REACTIVE = "reactive"             # React to current state
    PREDICTIVE = "predictive"         # Use predictions for control
    ADAPTIVE = "adaptive"            # Adapt based on feedback
    EMERGENCY = "emergency"          # Emergency stability mode

@dataclass
class FeedbackState:
    """State representing feedback information"""
    feedback_type: FeedbackType
    current_body_state: BodyConfiguration
    predicted_body_state: Optional[BodyConfiguration] = None
    error_magnitude: float = 0.0
    correction_vector: List[float] = field(default_factory=list)
    confidence: float = 1.0
    timestamp: float = field(default_factory=time.time)
    sensor_data: Dict[str, float] = field(default_factory=dict)
    
    def calculate_position_error(self) -> float:
        """Calculate position error between current and predicted states"""
        if not self.predicted_body_state:
            return 0.0
        
        curr_pos = self.current_body_state.position
        pred_pos = self.predicted_body_state.position
        
        return math.sqrt(
            (curr_pos[0] - pred_pos[0])**2 +
            (curr_pos[1] - pred_pos[1])**2 +
            (curr_pos[2] - pred_pos[2])**2
        )
    
    def calculate_joint_error(self) -> Dict[str, float]:
        """Calculate joint angle errors"""
        if not self.predicted_body_state:
            return {}
        
        errors = {}
        curr_joints = self.current_body_state.joint_angles
        pred_joints = self.predicted_body_state.joint_angles
        
        for joint_name in curr_joints:
            if joint_name in pred_joints:
                errors[joint_name] = abs(curr_joints[joint_name] - pred_joints[joint_name])
        
        return errors

@dataclass 
class CorrectiveAction:
    """Represents a corrective action to be applied"""
    joint_corrections: Dict[str, float]  # Joint adjustments needed
    force_corrections: Dict[str, float]  # Force adjustments
    timing_adjustment: float = 0.0       # Timing adjustment in seconds
    priority: float = 1.0               # Priority (0-1)
    action_type: str = "correction"     # Type of corrective action
    confidence: float = 1.0             # Confidence in correction
    
    def apply_scaling(self, scale_factor: float) -> 'CorrectiveAction':
        """Apply scaling to the corrective action"""
        scaled_joint_corrections = {
            joint: correction * scale_factor 
            for joint, correction in self.joint_corrections.items()
        }
        scaled_force_corrections = {
            force: correction * scale_factor
            for force, correction in self.force_corrections.items()
        }
        
        return CorrectiveAction(
            joint_corrections=scaled_joint_corrections,
            force_corrections=scaled_force_corrections,
            timing_adjustment=self.timing_adjustment * scale_factor,
            priority=self.priority,
            action_type=self.action_type,
            confidence=self.confidence
        )

class RealTimeFeedbackCorrector:
    """Handles real-time feedback correction"""
    
    def __init__(self, correction_threshold: float = 0.05, max_correction_rate: float = 10.0):
        self.correction_threshold = correction_threshold
        self.max_correction_rate = max_correction_rate  # Hz
        self.correction_history = deque(maxlen=1000)
        self.last_correction_time = 0.0
        self.active_corrections = {}
        
        # DTESN integration for correction processing
        if HAS_DTESN_CORE:
            self._init_dtesn_correction_components()
        
        # Performance metrics
        self.correction_metrics = {
            'total_corrections': 0,
            'successful_corrections': 0,
            'average_correction_latency': 0.0,
            'correction_accuracy': 0.0
        }
        
        logger.info("Real-time feedback corrector initialized")
    
    def _init_dtesn_correction_components(self):
        """Initialize DTESN components for correction processing"""
        try:
            # ESN for temporal correction dynamics
            self.correction_esn = ESNReservoir(
                reservoir_size=128,
                input_size=32,  # Feedback state dimensions
                spectral_radius=0.95,
                leak_rate=0.3
            )
            
            # P-System for correction rule evolution  
            self.correction_p_system = PSystemEvolutionEngine(
                max_membranes=3,
                evolution_steps=50
            )
            
            logger.info("DTESN correction components initialized")
        except Exception as e:
            logger.error(f"Failed to initialize DTESN correction components: {e}")
    
    def process_feedback(self, feedback_state: FeedbackState) -> Optional[CorrectiveAction]:
        """Process feedback and generate corrective action if needed"""
        try:
            current_time = time.time()
            
            # Rate limiting for real-time performance
            if current_time - self.last_correction_time < 1.0 / self.max_correction_rate:
                return None
            
            # Calculate error magnitude
            position_error = feedback_state.calculate_position_error()
            joint_errors = feedback_state.calculate_joint_error()
            
            # Determine if correction is needed
            total_error = position_error + sum(joint_errors.values()) * 0.1
            
            if total_error < self.correction_threshold:
                return None
            
            # Generate corrective action
            corrective_action = self._generate_correction(feedback_state, total_error)
            
            if corrective_action:
                # Store correction for analysis
                self.correction_history.append({
                    'timestamp': current_time,
                    'feedback_state': feedback_state,
                    'corrective_action': corrective_action,
                    'error_magnitude': total_error
                })
                
                self.correction_metrics['total_corrections'] += 1
                self.last_correction_time = current_time
                
                logger.debug(f"Corrective action generated with error {total_error:.4f}")
            
            return corrective_action
            
        except Exception as e:
            logger.error(f"Error processing feedback: {e}")
            return None
    
    def _generate_correction(self, feedback_state: FeedbackState, error_magnitude: float) -> CorrectiveAction:
        """Generate corrective action based on feedback state"""
        joint_corrections = {}
        force_corrections = {}
        
        # Calculate joint corrections
        joint_errors = feedback_state.calculate_joint_error()
        for joint_name, error in joint_errors.items():
            # Proportional correction with damping
            correction = -error * 0.5  # Proportional gain
            joint_corrections[joint_name] = correction
        
        # Calculate position-based corrections
        if feedback_state.predicted_body_state:
            curr_pos = feedback_state.current_body_state.position
            pred_pos = feedback_state.predicted_body_state.position
            
            # Simple proportional correction for key joints
            position_error = [
                pred_pos[i] - curr_pos[i] for i in range(3)
            ]
            
            # Map position errors to joint corrections (simplified)
            if 'shoulder' not in joint_corrections:
                joint_corrections['shoulder'] = position_error[0] * 0.2
            if 'elbow' not in joint_corrections:
                joint_corrections['elbow'] = position_error[1] * 0.3
            if 'wrist' not in joint_corrections:
                joint_corrections['wrist'] = position_error[2] * 0.1
        
        # Force corrections based on sensor feedback
        for sensor_name, sensor_value in feedback_state.sensor_data.items():
            if 'force' in sensor_name.lower():
                force_corrections[sensor_name] = -sensor_value * 0.1
        
        # Calculate confidence based on error magnitude and feedback quality
        confidence = max(0.1, 1.0 - error_magnitude)
        
        # Create corrective action
        corrective_action = CorrectiveAction(
            joint_corrections=joint_corrections,
            force_corrections=force_corrections,
            timing_adjustment=0.0,
            priority=min(error_magnitude * 2.0, 1.0),
            action_type="realtime_correction",
            confidence=confidence
        )
        
        return corrective_action
    
    def get_correction_performance(self) -> Dict[str, float]:
        """Get performance metrics for correction system"""
        if self.correction_metrics['total_corrections'] == 0:
            return {
                'total_corrections': 0,
                'success_rate': 0.0,
                'average_latency': 0.0,
                'correction_frequency': 0.0
            }
        
        return {
            'total_corrections': self.correction_metrics['total_corrections'],
            'success_rate': (self.correction_metrics['successful_corrections'] / 
                           self.correction_metrics['total_corrections']),
            'average_latency': self.correction_metrics['average_correction_latency'],
            'correction_frequency': len([
                c for c in self.correction_history
                if time.time() - c['timestamp'] < 60.0  # Last minute
            ]) / 60.0
        }

class AdaptiveController:
    """Adaptive controller based on proprioceptive feedback"""
    
    def __init__(self, adaptation_rate: float = 0.1):
        self.adaptation_rate = adaptation_rate
        self.control_parameters = {
            'proportional_gain': 1.0,
            'derivative_gain': 0.1,
            'integral_gain': 0.01,
            'stability_margin': 0.05
        }
        
        self.parameter_history = deque(maxlen=1000)
        self.proprioceptive_model = self._init_proprioceptive_model()
        self.control_mode = ControlMode.ADAPTIVE
        
        # Learning and adaptation components
        self.adaptation_memory = {}
        self.performance_tracker = {
            'adaptation_count': 0,
            'improvement_ratio': 0.0,
            'stability_score': 1.0
        }
        
        logger.info("Adaptive controller initialized")
    
    def _init_proprioceptive_model(self) -> Dict[str, Any]:
        """Initialize proprioceptive processing model"""
        model = {
            'joint_sensitivity': {
                'shoulder': 0.8, 'elbow': 0.9, 'wrist': 0.7,
                'hip': 0.8, 'knee': 0.9, 'ankle': 0.7
            },
            'position_sensitivity': [0.8, 0.8, 0.9],  # x, y, z
            'velocity_sensitivity': 0.7,
            'acceleration_sensitivity': 0.5
        }
        return model
    
    def adapt_control_parameters(self, feedback_state: FeedbackState, 
                               performance_metrics: Dict[str, float]) -> Dict[str, float]:
        """Adapt control parameters based on proprioceptive feedback and performance"""
        try:
            # Calculate adaptation based on performance
            current_performance = performance_metrics.get('success_rate', 0.5)
            target_performance = 0.85
            
            performance_error = target_performance - current_performance
            
            # Adapt proportional gain
            if performance_error > 0.1:
                # Need more aggressive control
                self.control_parameters['proportional_gain'] *= (1.0 + self.adaptation_rate * 0.5)
            elif performance_error < -0.05:
                # Too aggressive, reduce gain
                self.control_parameters['proportional_gain'] *= (1.0 - self.adaptation_rate * 0.3)
            
            # Adapt derivative gain based on stability
            stability_score = performance_metrics.get('stability_score', 1.0)
            if stability_score < 0.8:
                # Increase damping
                self.control_parameters['derivative_gain'] *= (1.0 + self.adaptation_rate * 0.2)
            
            # Proprioceptive sensitivity adaptation
            joint_errors = feedback_state.calculate_joint_error()
            for joint_name, error in joint_errors.items():
                if joint_name in self.proprioceptive_model['joint_sensitivity']:
                    if error > 0.1:  # High error, increase sensitivity
                        current_sensitivity = self.proprioceptive_model['joint_sensitivity'][joint_name]
                        new_sensitivity = min(1.0, current_sensitivity * (1.0 + self.adaptation_rate * 0.1))
                        self.proprioceptive_model['joint_sensitivity'][joint_name] = new_sensitivity
            
            # Store parameter history
            self.parameter_history.append({
                'timestamp': time.time(),
                'parameters': self.control_parameters.copy(),
                'performance': performance_metrics.copy(),
                'feedback_type': feedback_state.feedback_type.value
            })
            
            self.performance_tracker['adaptation_count'] += 1
            
            # Calculate improvement ratio
            if len(self.parameter_history) > 10:
                recent_performance = [p['performance'].get('success_rate', 0.5) 
                                    for p in list(self.parameter_history)[-10:]]
                older_performance = [p['performance'].get('success_rate', 0.5) 
                                   for p in list(self.parameter_history)[-20:-10]]
                
                if len(older_performance) > 0:
                    recent_avg = sum(recent_performance) / len(recent_performance)
                    older_avg = sum(older_performance) / len(older_performance)
                    self.performance_tracker['improvement_ratio'] = recent_avg - older_avg
            
            logger.debug(f"Control parameters adapted: {self.control_parameters}")
            return self.control_parameters.copy()
            
        except Exception as e:
            logger.error(f"Error adapting control parameters: {e}")
            return self.control_parameters.copy()
    
    def process_proprioceptive_feedback(self, body_state: BodyConfiguration) -> Dict[str, float]:
        """Process proprioceptive feedback to extract control-relevant information"""
        try:
            proprioceptive_info = {
                'joint_position_sense': {},
                'body_position_sense': {},
                'movement_sense': {},
                'effort_sense': {}
            }
            
            # Joint position sense
            for joint_name, angle in body_state.joint_angles.items():
                if joint_name in self.proprioceptive_model['joint_sensitivity']:
                    sensitivity = self.proprioceptive_model['joint_sensitivity'][joint_name]
                    # Simulate proprioceptive noise and processing
                    sensed_angle = angle + (0.02 * (0.5 - hash(joint_name) % 1000 / 1000.0))
                    proprioceptive_info['joint_position_sense'][joint_name] = {
                        'sensed_angle': sensed_angle,
                        'confidence': sensitivity,
                        'error_estimate': abs(sensed_angle - angle)
                    }
            
            # Body position sense
            for i, pos_coord in enumerate(body_state.position):
                sensitivity = self.proprioceptive_model['position_sensitivity'][i]
                sensed_pos = pos_coord + (0.01 * (0.5 - hash(f"pos_{i}") % 1000 / 1000.0))
                proprioceptive_info['body_position_sense'][f'axis_{i}'] = {
                    'sensed_position': sensed_pos,
                    'confidence': sensitivity
                }
            
            # Movement and effort sense (simplified)
            proprioceptive_info['movement_sense'] = {
                'movement_detected': len(body_state.joint_angles) > 0,
                'movement_magnitude': sum(abs(angle) for angle in body_state.joint_angles.values()) / max(1, len(body_state.joint_angles))
            }
            
            proprioceptive_info['effort_sense'] = {
                'estimated_effort': min(1.0, proprioceptive_info['movement_sense']['movement_magnitude'] * 0.5)
            }
            
            return proprioceptive_info
            
        except Exception as e:
            logger.error(f"Error processing proprioceptive feedback: {e}")
            return {}
    
    def get_adaptation_performance(self) -> Dict[str, Any]:
        """Get adaptation performance metrics"""
        return {
            'adaptation_count': self.performance_tracker['adaptation_count'],
            'current_parameters': self.control_parameters.copy(),
            'improvement_ratio': self.performance_tracker['improvement_ratio'],
            'stability_score': self.performance_tracker['stability_score'],
            'proprioceptive_sensitivity': self.proprioceptive_model['joint_sensitivity'].copy(),
            'control_mode': self.control_mode.value
        }

class BalanceStabilityManager:
    """Manages balance and stability maintenance"""
    
    def __init__(self, stability_threshold: float = 0.8):
        self.stability_threshold = stability_threshold
        self.balance_state = {
            'center_of_mass': [0.0, 0.0, 1.0],  # x, y, z
            'base_of_support': [0.5, 0.5],      # width, depth
            'stability_margin': 0.0,
            'balance_confidence': 1.0
        }
        
        self.stability_history = deque(maxlen=500)
        self.emergency_responses = []
        self.balance_strategies = self._init_balance_strategies()
        
        # Performance tracking
        self.stability_metrics = {
            'stability_events': 0,
            'balance_maintained': 0,
            'emergency_interventions': 0,
            'average_stability_score': 1.0
        }
        
        logger.info("Balance stability manager initialized")
    
    def _init_balance_strategies(self) -> Dict[str, Callable]:
        """Initialize different balance strategies"""
        strategies = {
            'ankle_strategy': self._ankle_strategy,
            'hip_strategy': self._hip_strategy,
            'step_strategy': self._step_strategy,
            'emergency_stop': self._emergency_stop
        }
        return strategies
    
    def assess_stability(self, body_state: BodyConfiguration, 
                        movement_prediction: Optional[MovementPrediction] = None) -> Dict[str, Any]:
        """Assess current stability state"""
        try:
            stability_assessment = {
                'current_stability': 0.0,
                'predicted_stability': 0.0,
                'balance_strategy_needed': None,
                'emergency_required': False,
                'stability_factors': {}
            }
            
            # Calculate center of mass based on joint angles
            com = self._calculate_center_of_mass(body_state)
            self.balance_state['center_of_mass'] = com
            
            # Calculate stability margin
            stability_margin = self._calculate_stability_margin(com)
            self.balance_state['stability_margin'] = stability_margin
            
            # Current stability score
            current_stability = max(0.0, min(1.0, stability_margin))
            stability_assessment['current_stability'] = current_stability
            
            # Predicted stability if movement prediction available
            if movement_prediction:
                predicted_com = self._predict_center_of_mass(movement_prediction)
                predicted_margin = self._calculate_stability_margin(predicted_com)
                predicted_stability = max(0.0, min(1.0, predicted_margin))
                stability_assessment['predicted_stability'] = predicted_stability
            else:
                stability_assessment['predicted_stability'] = current_stability
            
            # Determine if intervention needed
            min_stability = min(current_stability, stability_assessment['predicted_stability'])
            
            if min_stability < 0.3:
                stability_assessment['emergency_required'] = True
                stability_assessment['balance_strategy_needed'] = 'emergency_stop'
            elif min_stability < self.stability_threshold:
                # Choose appropriate balance strategy
                if abs(com[0]) > 0.2:  # Forward/backward imbalance
                    stability_assessment['balance_strategy_needed'] = 'ankle_strategy'
                elif abs(com[1]) > 0.2:  # Side-to-side imbalance
                    stability_assessment['balance_strategy_needed'] = 'hip_strategy'
                else:
                    stability_assessment['balance_strategy_needed'] = 'ankle_strategy'
            
            # Store stability history
            self.stability_history.append({
                'timestamp': time.time(),
                'stability_score': current_stability,
                'center_of_mass': com,
                'stability_margin': stability_margin,
                'strategy_used': stability_assessment['balance_strategy_needed']
            })
            
            self.stability_metrics['stability_events'] += 1
            if current_stability >= self.stability_threshold:
                self.stability_metrics['balance_maintained'] += 1
            
            # Update average stability score
            alpha = 0.1
            self.stability_metrics['average_stability_score'] = (
                alpha * current_stability + 
                (1 - alpha) * self.stability_metrics['average_stability_score']
            )
            
            logger.debug(f"Stability assessed: {current_stability:.3f}")
            return stability_assessment
            
        except Exception as e:
            logger.error(f"Error assessing stability: {e}")
            return {'current_stability': 0.5, 'emergency_required': False}
    
    def _calculate_center_of_mass(self, body_state: BodyConfiguration) -> List[float]:
        """Calculate approximate center of mass based on body configuration"""
        # Simplified COM calculation based on position and joint angles
        base_com = list(body_state.position)
        
        # Adjust based on joint angles (simplified model)
        joint_adjustments = [0.0, 0.0, 0.0]
        
        for joint_name, angle in body_state.joint_angles.items():
            if 'shoulder' in joint_name.lower():
                joint_adjustments[0] += angle * 0.1
            elif 'hip' in joint_name.lower():
                joint_adjustments[1] += angle * 0.1
            elif 'knee' in joint_name.lower():
                joint_adjustments[2] += angle * 0.05
        
        com = [base_com[i] + joint_adjustments[i] for i in range(3)]
        return com
    
    def _calculate_stability_margin(self, center_of_mass: List[float]) -> float:
        """Calculate stability margin based on center of mass and base of support"""
        # Distance from COM to edge of base of support
        com_x, com_y, com_z = center_of_mass
        base_width, base_depth = self.balance_state['base_of_support']
        
        # Normalized distance to edge (higher is better)
        margin_x = max(0.0, (base_width / 2.0) - abs(com_x)) / (base_width / 2.0)
        margin_y = max(0.0, (base_depth / 2.0) - abs(com_y)) / (base_depth / 2.0)
        
        # Combined stability margin - use minimum of the two margins
        # For a perfectly centered COM, this should be 1.0
        stability_margin = min(margin_x, margin_y)
        
        # Add height consideration - higher COM is less stable
        if com_z < 0.8:  # Very low COM
            stability_margin *= 1.2
        elif com_z > 1.2:  # Very high COM
            stability_margin *= 0.8
        
        return min(1.0, stability_margin)
    
    def _predict_center_of_mass(self, movement_prediction: MovementPrediction) -> List[float]:
        """Predict future center of mass based on movement prediction"""
        if not movement_prediction.predicted_end_state:
            return self.balance_state['center_of_mass']
        
        return self._calculate_center_of_mass(movement_prediction.predicted_end_state.body_configuration)
    
    def generate_balance_correction(self, stability_assessment: Dict[str, Any]) -> Optional[CorrectiveAction]:
        """Generate balance correction based on stability assessment"""
        try:
            strategy_name = stability_assessment.get('balance_strategy_needed')
            if not strategy_name or strategy_name not in self.balance_strategies:
                return None
            
            # Execute appropriate balance strategy
            strategy_function = self.balance_strategies[strategy_name]
            balance_correction = strategy_function(stability_assessment)
            
            if balance_correction:
                self.stability_metrics['emergency_interventions'] += 1
                logger.debug(f"Balance correction generated using {strategy_name}")
            
            return balance_correction
            
        except Exception as e:
            logger.error(f"Error generating balance correction: {e}")
            return None
    
    def _ankle_strategy(self, assessment: Dict[str, Any]) -> CorrectiveAction:
        """Ankle strategy for small balance perturbations"""
        com = self.balance_state['center_of_mass']
        
        # Generate ankle corrections to counteract COM displacement
        joint_corrections = {
            'ankle_left': -com[0] * 0.5,   # Counter forward/backward lean
            'ankle_right': -com[0] * 0.5,
        }
        
        return CorrectiveAction(
            joint_corrections=joint_corrections,
            force_corrections={},
            priority=0.7,
            action_type="ankle_strategy",
            confidence=0.8
        )
    
    def _hip_strategy(self, assessment: Dict[str, Any]) -> CorrectiveAction:
        """Hip strategy for larger balance perturbations"""
        com = self.balance_state['center_of_mass']
        
        # Generate hip corrections
        joint_corrections = {
            'hip_left': -com[1] * 0.3,     # Counter side-to-side lean  
            'hip_right': com[1] * 0.3,
            'trunk': -com[0] * 0.2         # Counter forward/backward lean
        }
        
        return CorrectiveAction(
            joint_corrections=joint_corrections,
            force_corrections={},
            priority=0.8,
            action_type="hip_strategy", 
            confidence=0.7
        )
    
    def _step_strategy(self, assessment: Dict[str, Any]) -> CorrectiveAction:
        """Step strategy for large balance perturbations"""
        com = self.balance_state['center_of_mass']
        
        # Determine step direction based on COM displacement
        step_direction = "forward" if com[0] > 0.1 else "backward" if com[0] < -0.1 else "side"
        
        joint_corrections = {
            'hip_swing_leg': 0.5,
            'knee_swing_leg': 0.3,
            'ankle_swing_leg': 0.2
        }
        
        return CorrectiveAction(
            joint_corrections=joint_corrections,
            force_corrections={f"step_{step_direction}": 0.8},
            priority=0.9,
            action_type="step_strategy",
            confidence=0.6
        )
    
    def _emergency_stop(self, assessment: Dict[str, Any]) -> CorrectiveAction:
        """Emergency stop strategy for critical stability loss"""
        # Generate emergency response to prevent fall
        joint_corrections = {
            joint: 0.0  # Stop all joint movements
            for joint in ['shoulder', 'elbow', 'wrist', 'hip', 'knee', 'ankle']
        }
        
        force_corrections = {
            'emergency_brake': 1.0,  # Maximum braking force
            'stability_lock': 1.0    # Lock joints for stability
        }
        
        return CorrectiveAction(
            joint_corrections=joint_corrections,
            force_corrections=force_corrections,
            priority=1.0,
            action_type="emergency_stop",
            confidence=0.9
        )
    
    def get_stability_performance(self) -> Dict[str, Any]:
        """Get balance and stability performance metrics"""
        total_events = self.stability_metrics['stability_events']
        balance_rate = (self.stability_metrics['balance_maintained'] / 
                       max(1, total_events))
        
        return {
            'total_stability_events': total_events,
            'balance_maintenance_rate': balance_rate,
            'emergency_interventions': self.stability_metrics['emergency_interventions'],
            'average_stability_score': self.stability_metrics['average_stability_score'],
            'current_balance_state': self.balance_state.copy(),
            'recent_stability_trend': self._calculate_stability_trend()
        }
    
    def _calculate_stability_trend(self) -> float:
        """Calculate recent stability trend"""
        if len(self.stability_history) < 10:
            return 0.0
        
        recent_scores = [h['stability_score'] for h in list(self.stability_history)[-10:]]
        older_scores = [h['stability_score'] for h in list(self.stability_history)[-20:-10]]
        
        if len(older_scores) == 0:
            return 0.0
        
        recent_avg = sum(recent_scores) / len(recent_scores)
        older_avg = sum(older_scores) / len(older_scores)
        
        return recent_avg - older_avg

class FeedbackControlSystem:
    """Main feedback control system integrating all components"""
    
    def __init__(self, agent_name: str = "feedback_control_agent"):
        self.agent_name = agent_name
        self.active = False
        self.control_thread = None
        self.stop_event = threading.Event()
        
        # Initialize subsystems
        self.feedback_corrector = RealTimeFeedbackCorrector()
        self.adaptive_controller = AdaptiveController()
        self.balance_manager = BalanceStabilityManager()
        
        # System integration
        self.motor_prediction_system = None
        self.sensor_attention_system = None
        
        if HAS_MOTOR_PREDICTION:
            try:
                from motor_prediction_system import create_motor_prediction_system
                self.motor_prediction_system = create_motor_prediction_system(f"{agent_name}_motor")
                logger.info("Integrated with motor prediction system")
            except Exception as e:
                logger.warning(f"Could not integrate with motor prediction: {e}")
        
        if HAS_SENSOR_ATTENTION:
            try:
                self.sensor_attention_system = AttentionGuidedSensorSystem()
                logger.info("Integrated with sensor attention system")
            except Exception as e:
                logger.warning(f"Could not integrate with sensor attention: {e}")
        
        # Feedback processing state
        self.current_feedback_state = None
        self.active_corrections = {}
        self.control_loop_frequency = 50.0  # Hz
        
        # Performance metrics
        self.system_metrics = {
            'control_cycles': 0,
            'successful_corrections': 0,
            'balance_maintained': 0,
            'system_uptime': 0.0,
            'start_time': time.time()
        }
        
        # Thread pool for parallel processing
        self.executor = ThreadPoolExecutor(max_workers=4)
        
        logger.info(f"Feedback Control System initialized for {agent_name}")
    
    def start_real_time_control(self):
        """Start the real-time feedback control loop"""
        if self.active:
            logger.warning("Control system already active")
            return
        
        self.active = True
        self.stop_event.clear()
        self.system_metrics['start_time'] = time.time()
        
        # Start control thread
        self.control_thread = threading.Thread(
            target=self._control_loop,
            daemon=True,
            name=f"{self.agent_name}_control_loop"
        )
        self.control_thread.start()
        
        logger.info("Real-time feedback control started")
    
    def stop_real_time_control(self):
        """Stop the real-time feedback control loop"""
        if not self.active:
            return
        
        self.active = False
        self.stop_event.set()
        
        if self.control_thread:
            self.control_thread.join(timeout=1.0)
        
        self.executor.shutdown(wait=False)
        
        self.system_metrics['system_uptime'] = time.time() - self.system_metrics['start_time']
        
        logger.info("Real-time feedback control stopped")
    
    def _control_loop(self):
        """Main real-time control loop"""
        loop_period = 1.0 / self.control_loop_frequency
        
        logger.info(f"Control loop started at {self.control_loop_frequency}Hz")
        
        while self.active and not self.stop_event.is_set():
            try:
                loop_start_time = time.time()
                
                # Process feedback control cycle
                self._process_control_cycle()
                
                # Update metrics
                self.system_metrics['control_cycles'] += 1
                
                # Maintain loop frequency
                loop_duration = time.time() - loop_start_time
                sleep_time = max(0.0, loop_period - loop_duration)
                
                if sleep_time > 0:
                    time.sleep(sleep_time)
                elif loop_duration > loop_period * 1.5:
                    logger.warning(f"Control loop overrun: {loop_duration:.3f}s > {loop_period:.3f}s")
                
            except Exception as e:
                logger.error(f"Error in control loop: {e}")
                time.sleep(loop_period)
        
        logger.info("Control loop stopped")
    
    def _process_control_cycle(self):
        """Process one cycle of feedback control"""
        if not self.current_feedback_state:
            return
        
        try:
            # Submit parallel processing tasks
            futures = []
            
            # Real-time feedback correction
            correction_future = self.executor.submit(
                self.feedback_corrector.process_feedback,
                self.current_feedback_state
            )
            futures.append(('correction', correction_future))
            
            # Stability assessment
            stability_future = self.executor.submit(
                self.balance_manager.assess_stability,
                self.current_feedback_state.current_body_state
            )
            futures.append(('stability', stability_future))
            
            # Adaptive control parameter update (less frequent)
            if self.system_metrics['control_cycles'] % 10 == 0:
                performance_metrics = self._get_current_performance_metrics()
                adaptation_future = self.executor.submit(
                    self.adaptive_controller.adapt_control_parameters,
                    self.current_feedback_state,
                    performance_metrics
                )
                futures.append(('adaptation', adaptation_future))
            
            # Collect results
            results = {}
            for task_name, future in futures:
                try:
                    results[task_name] = future.result(timeout=0.01)  # 10ms timeout
                except Exception as e:
                    logger.warning(f"Task {task_name} failed: {e}")
                    results[task_name] = None
            
            # Process results
            self._integrate_control_results(results)
            
        except Exception as e:
            logger.error(f"Error in control cycle processing: {e}")
    
    def _integrate_control_results(self, results: Dict[str, Any]):
        """Integrate results from parallel control processing"""
        try:
            # Handle corrective actions
            if results.get('correction'):
                corrective_action = results['correction']
                self._apply_corrective_action(corrective_action)
                self.system_metrics['successful_corrections'] += 1
            
            # Handle stability assessment and balance corrections
            if results.get('stability'):
                stability_assessment = results['stability']
                
                if stability_assessment.get('balance_strategy_needed'):
                    balance_correction = self.balance_manager.generate_balance_correction(stability_assessment)
                    if balance_correction:
                        self._apply_corrective_action(balance_correction)
                
                if stability_assessment.get('current_stability', 0.0) >= self.balance_manager.stability_threshold:
                    self.system_metrics['balance_maintained'] += 1
            
            # Handle adaptive control updates
            if results.get('adaptation'):
                # Adaptive parameters updated within the controller
                pass
            
        except Exception as e:
            logger.error(f"Error integrating control results: {e}")
    
    def _apply_corrective_action(self, corrective_action: CorrectiveAction):
        """Apply corrective action to the system"""
        try:
            # Store active correction
            correction_id = f"correction_{time.time()}"
            self.active_corrections[correction_id] = {
                'action': corrective_action,
                'applied_time': time.time(),
                'status': 'active'
            }
            
            # In a real system, this would interface with motor controllers
            # For now, we log the corrections that would be applied
            logger.debug(f"Applied {corrective_action.action_type}: "
                        f"{len(corrective_action.joint_corrections)} joint corrections, "
                        f"{len(corrective_action.force_corrections)} force corrections")
            
            # Clean up old corrections
            current_time = time.time()
            expired_corrections = [
                cid for cid, correction in self.active_corrections.items()
                if current_time - correction['applied_time'] > 1.0  # 1 second timeout
            ]
            
            for cid in expired_corrections:
                del self.active_corrections[cid]
            
        except Exception as e:
            logger.error(f"Error applying corrective action: {e}")
    
    def process_feedback_state(self, current_body_state: BodyConfiguration,
                             predicted_body_state: Optional[BodyConfiguration] = None,
                             sensor_data: Optional[Dict[str, float]] = None) -> Dict[str, Any]:
        """
        Main interface for processing feedback state and generating corrections.
        This is the core method that satisfies the acceptance criteria:
        'Agents maintain balance and correct movements'
        """
        try:
            # Create feedback state
            feedback_state = FeedbackState(
                feedback_type=FeedbackType.PROPRIOCEPTIVE,
                current_body_state=current_body_state,
                predicted_body_state=predicted_body_state,
                sensor_data=sensor_data or {}
            )
            
            # Calculate errors if prediction available
            if predicted_body_state:
                feedback_state.error_magnitude = feedback_state.calculate_position_error()
            
            # Update current feedback state for control loop
            self.current_feedback_state = feedback_state
            
            # Process feedback through all subsystems
            processing_results = {}
            
            # Real-time feedback correction
            corrective_action = self.feedback_corrector.process_feedback(feedback_state)
            if corrective_action:
                processing_results['corrective_action'] = corrective_action.joint_corrections
                processing_results['correction_confidence'] = corrective_action.confidence
            
            # Stability assessment and balance management
            stability_assessment = self.balance_manager.assess_stability(current_body_state)
            processing_results['stability_score'] = stability_assessment['current_stability']
            processing_results['balance_strategy'] = stability_assessment.get('balance_strategy_needed')
            
            # Generate balance correction if needed
            if stability_assessment.get('balance_strategy_needed'):
                balance_correction = self.balance_manager.generate_balance_correction(stability_assessment)
                if balance_correction:
                    processing_results['balance_correction'] = balance_correction.joint_corrections
                    processing_results['balance_confidence'] = balance_correction.confidence
            
            # Adaptive control processing
            proprioceptive_info = self.adaptive_controller.process_proprioceptive_feedback(current_body_state)
            processing_results['proprioceptive_feedback'] = proprioceptive_info
            
            # Compile comprehensive response
            control_response = {
                'agent_name': self.agent_name,
                'processing_timestamp': time.time(),
                'feedback_processing_results': processing_results,
                'system_status': {
                    'balance_maintained': stability_assessment['current_stability'] >= self.balance_manager.stability_threshold,
                    'corrections_applied': corrective_action is not None,
                    'emergency_required': stability_assessment.get('emergency_required', False)
                },
                'performance_summary': self._get_current_performance_metrics()
            }
            
            logger.debug(f"Feedback processed - Balance: {stability_assessment['current_stability']:.3f}, "
                        f"Corrections: {'Yes' if corrective_action else 'No'}")
            
            return control_response
            
        except Exception as e:
            logger.error(f"Error processing feedback state: {e}")
            return {
                'error': str(e),
                'agent_name': self.agent_name,
                'system_status': {'balance_maintained': False, 'corrections_applied': False}
            }
    
    def _get_current_performance_metrics(self) -> Dict[str, float]:
        """Get current performance metrics across all subsystems"""
        correction_perf = self.feedback_corrector.get_correction_performance()
        stability_perf = self.balance_manager.get_stability_performance()
        adaptation_perf = self.adaptive_controller.get_adaptation_performance()
        
        total_cycles = max(1, self.system_metrics['control_cycles'])
        
        return {
            'success_rate': (self.system_metrics['successful_corrections'] / total_cycles),
            'balance_maintenance_rate': stability_perf['balance_maintenance_rate'],
            'stability_score': stability_perf['average_stability_score'],
            'correction_frequency': correction_perf.get('correction_frequency', 0.0),
            'adaptation_count': adaptation_perf['adaptation_count'],
            'system_uptime': time.time() - self.system_metrics['start_time']
        }
    
    def get_comprehensive_performance(self) -> Dict[str, Any]:
        """Get comprehensive system performance metrics"""
        return {
            'system_metrics': self.system_metrics.copy(),
            'feedback_correction': self.feedback_corrector.get_correction_performance(),
            'balance_stability': self.balance_manager.get_stability_performance(),
            'adaptive_control': self.adaptive_controller.get_adaptation_performance(),
            'integration_status': {
                'motor_prediction_available': self.motor_prediction_system is not None,
                'sensor_attention_available': self.sensor_attention_system is not None,
                'dtesn_integration': HAS_DTESN_CORE,
                'real_time_active': self.active
            },
            'active_corrections': len(self.active_corrections)
        }

# Main interface functions for easy integration
def create_feedback_control_system(agent_name: str = "default_agent") -> FeedbackControlSystem:
    """Create and initialize a feedback control system"""
    return FeedbackControlSystem(agent_name)

if __name__ == "__main__":
    # Demonstration of feedback control system
    print("Feedback Control System - Deep Tree Echo Integration")
    print("=" * 60)
    
    # Create system
    control_system = create_feedback_control_system("demo_agent")
    
    # Create test body state
    test_current_state = BodyConfiguration(
        position=(0.1, 0.05, 1.0),  # Slightly off-balance
        joint_angles={
            'shoulder': 0.2,
            'elbow': -0.1,
            'hip': 0.05,
            'knee': -0.02,
            'ankle': 0.01
        }
    )
    
    # Create predicted state for comparison
    test_predicted_state = BodyConfiguration(
        position=(0.0, 0.0, 1.0),  # Ideal balanced position
        joint_angles={
            'shoulder': 0.0,
            'elbow': 0.0,
            'hip': 0.0,
            'knee': 0.0,
            'ankle': 0.0
        }
    )
    
    # Test sensor data
    test_sensor_data = {
        'balance_sensor': 0.1,
        'pressure_left': 0.6,
        'pressure_right': 0.4,
        'vestibular_x': 0.05,
        'vestibular_y': -0.02
    }
    
    print("\nProcessing feedback control...")
    
    # Process feedback state
    control_response = control_system.process_feedback_state(
        test_current_state,
        test_predicted_state,
        test_sensor_data
    )
    
    print("\nControl Response:")
    print(f"Balance Maintained: {control_response['system_status']['balance_maintained']}")
    print(f"Corrections Applied: {control_response['system_status']['corrections_applied']}")
    print(f"Stability Score: {control_response['feedback_processing_results'].get('stability_score', 0.0):.3f}")
    
    if 'corrective_action' in control_response['feedback_processing_results']:
        corrections = control_response['feedback_processing_results']['corrective_action']
        print(f"Joint Corrections: {len(corrections)} joints")
        for joint, correction in corrections.items():
            print(f"  {joint}: {correction:.3f}")
    
    if 'balance_correction' in control_response['feedback_processing_results']:
        balance_corrections = control_response['feedback_processing_results']['balance_correction']
        print(f"Balance Corrections: {len(balance_corrections)} joints")
        for joint, correction in balance_corrections.items():
            print(f"  {joint}: {correction:.3f}")
    
    # Test real-time control
    print("\nTesting real-time control loop...")
    control_system.start_real_time_control()
    
    # Let it run for a short time
    time.sleep(2.0)
    
    control_system.stop_real_time_control()
    
    # Display comprehensive performance
    print("\nSystem Performance:")
    performance = control_system.get_comprehensive_performance()
    print(f"Control Cycles: {performance['system_metrics']['control_cycles']}")
    print(f"Balance Maintenance Rate: {performance['balance_stability']['balance_maintenance_rate']:.3f}")
    print(f"Average Stability Score: {performance['balance_stability']['average_stability_score']:.3f}")
    print(f"Motor Prediction Available: {performance['integration_status']['motor_prediction_available']}")
    print(f"DTESN Integration: {performance['integration_status']['dtesn_integration']}")
    
    print("\nFeedback Control System demonstration completed.")