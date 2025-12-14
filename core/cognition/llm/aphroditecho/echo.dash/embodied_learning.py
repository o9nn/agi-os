#!/usr/bin/env python3
"""
Embodied Learning Algorithms for Deep Tree Echo

Implements sensorimotor learning for body awareness, spatial reasoning based on
body constraints, and motor skill acquisition through embodied practice.

This module integrates with the DTESN cognitive architecture and provides
the foundation for 4E Embodied AI Framework implementation.
"""

import numpy as np
import logging
import time
from dataclasses import dataclass, field
from typing import Dict, List, Tuple, Optional, Any

# Import existing cognitive architecture if available
try:
    from cognitive_architecture import Memory, MemoryType, CognitiveArchitecture
    COGNITIVE_ARCH_AVAILABLE = True
except ImportError:
    COGNITIVE_ARCH_AVAILABLE = False
    
# Import sensory motor system for integration
try:
    from sensory_motor import SensoryMotor
    SENSORY_MOTOR_AVAILABLE = True
except ImportError:
    SENSORY_MOTOR_AVAILABLE = False

logger = logging.getLogger(__name__)


@dataclass
class BodyState:
    """Represents the current state of the embodied agent's body"""
    position: Tuple[float, float, float] = (0.0, 0.0, 0.0)  # x, y, z
    orientation: Tuple[float, float, float] = (0.0, 0.0, 0.0)  # roll, pitch
    joint_angles: Dict[str, float] = field(default_factory=dict)
    joint_velocities: Dict[str, float] = field(default_factory=dict)
    muscle_activations: Dict[str, float] = field(default_factory=dict)
    sensory_state: Dict[str, Any] = field(default_factory=dict)
    timestamp: float = field(default_factory=time.time)
    
    def to_vector(self) -> np.ndarray:
        """Convert body state to numerical vector for learning algorithms"""
        vector = list(self.position) + list(self.orientation)
        vector.extend(self.joint_angles.values())
        vector.extend(self.joint_velocities.values())
        vector.extend(self.muscle_activations.values())
        return np.array(vector)
    
    def update_from_sensory_input(self, sensory_data: Dict[str, Any]):
        """Update body state from sensory input"""
        self.sensory_state.update(sensory_data)
        self.timestamp = time.time()


@dataclass 
class MotorAction:
    """Represents a motor action to be executed"""
    joint_targets: Dict[str, float] = field(default_factory=dict)
    muscle_commands: Dict[str, float] = field(default_factory=dict)
    duration: float = 1.0
    force: float = 1.0
    precision: float = 1.0
    timestamp: float = field(default_factory=time.time)
    
    def to_vector(self) -> np.ndarray:
        """Convert motor action to numerical vector"""
        vector = list(self.joint_targets.values()) + list(self.muscle_commands.values())
        vector.extend([self.duration, self.force, self.precision])
        return np.array(vector)


@dataclass
class SensorimotorExperience:
    """Represents a complete sensorimotor experience for learning"""
    initial_body_state: BodyState
    motor_action: MotorAction
    resulting_body_state: BodyState
    sensory_feedback: Dict[str, Any]
    reward: float = 0.0
    success: bool = False
    timestamp: float = field(default_factory=time.time)


class BodySchemaLearner:
    """Learns and maintains body schema through embodied experience"""
    
    def __init__(self, body_dimensions: Dict[str, float]):
        self.body_dimensions = body_dimensions
        self.joint_limits = {}
        self.muscle_strength = {}
        self.proprioceptive_model = {}
        self.body_schema_weights = np.random.randn(100, 100) * 0.1  # Neural network weights
        self.learning_rate = 0.01
        self.experiences = []
        
    def update_body_schema(self, experience: SensorimotorExperience):
        """Update body schema based on new sensorimotor experience"""
        try:
            # Extract features from experience
            initial_state = experience.initial_body_state.to_vector()
            motor_action = experience.motor_action.to_vector()
            resulting_state = experience.resulting_body_state.to_vector()
            
            # Pad vectors to consistent size
            max_size = max(len(initial_state), len(motor_action), len(resulting_state))
            initial_state = np.pad(initial_state, (0, max_size - len(initial_state)))
            motor_action = np.pad(motor_action, (0, max_size - len(motor_action)))
            resulting_state = np.pad(resulting_state, (0, max_size - len(resulting_state)))
            
            # Compute prediction error
            predicted_state = self._predict_outcome(initial_state, motor_action)
            error = resulting_state - predicted_state
            
            # Update proprioceptive model using Hebbian learning
            self._update_proprioceptive_model(initial_state, motor_action, error)
            
            # Store experience for future learning
            self.experiences.append(experience)
            if len(self.experiences) > 1000:  # Limit memory usage
                self.experiences = self.experiences[-1000:]
                
            logger.debug(f"Updated body schema with prediction error: {np.mean(np.abs(error))}")
            
        except Exception as e:
            logger.error(f"Error updating body schema: {e}")
    
    def _predict_outcome(self, initial_state: np.ndarray, motor_action: np.ndarray) -> np.ndarray:
        """Predict the outcome of a motor action given initial body state"""
        # Simple forward model - can be enhanced with neural networks
        combined_input = np.concatenate([initial_state[:50], motor_action[:50]])
        if len(combined_input) < 100:
            combined_input = np.pad(combined_input, (0, 100 - len(combined_input)))
        
        # Forward pass through simple neural network
        hidden = np.tanh(np.dot(self.body_schema_weights, combined_input[:100]))
        prediction = np.dot(self.body_schema_weights.T[:len(initial_state)], hidden)[:len(initial_state)]
        
        return prediction
    
    def _update_proprioceptive_model(self, initial_state: np.ndarray, 
                                   motor_action: np.ndarray, error: np.ndarray):
        """Update proprioceptive model based on prediction error"""
        try:
            # Gradient descent update with proper shape handling
            combined_input = np.concatenate([initial_state[:50], motor_action[:50]])
            if len(combined_input) < 100:
                combined_input = np.pad(combined_input, (0, 100 - len(combined_input)))
            
            # Ensure error vector has correct shape
            if len(error) < 100:
                error = np.pad(error, (0, 100 - len(error)))
            error = error[:100]  # Truncate to fit
            
            # Update weights based on error with proper broadcasting
            gradient = np.outer(error, combined_input[:100]) * self.learning_rate
            self.body_schema_weights += gradient[:100, :100]
            
        except Exception:
            # Fallback to simple weight adjustment
            adjustment = self.learning_rate * 0.01 * np.random.randn(*self.body_schema_weights.shape)
            self.body_schema_weights += adjustment
        
    def get_body_awareness(self) -> Dict[str, float]:
        """Get current body awareness metrics"""
        return {
            'schema_confidence': self._compute_schema_confidence(),
            'proprioceptive_accuracy': self._compute_proprioceptive_accuracy(),
            'joint_flexibility': self._compute_joint_flexibility(),
            'muscle_coordination': self._compute_muscle_coordination()
        }
    
    def _compute_schema_confidence(self) -> float:
        """Compute confidence in current body schema"""
        if len(self.experiences) < 10:
            return 0.0
            
        recent_errors = []
        for exp in self.experiences[-10:]:
            try:
                initial = exp.initial_body_state.to_vector()
                action = exp.motor_action.to_vector()
                result = exp.resulting_body_state.to_vector()
                
                max_size = max(len(initial), len(action), len(result))
                initial = np.pad(initial, (0, max_size - len(initial)))
                action = np.pad(action, (0, max_size - len(action)))
                result = np.pad(result, (0, max_size - len(result)))
                
                predicted = self._predict_outcome(initial, action)
                error = np.mean(np.abs(result - predicted))
                recent_errors.append(error)
            except Exception:
                continue
                
        if not recent_errors:
            return 0.0
            
        avg_error = np.mean(recent_errors)
        confidence = 1.0 / (1.0 + avg_error)  # Higher confidence with lower error
        return float(confidence)
    
    def _compute_proprioceptive_accuracy(self) -> float:
        """Compute proprioceptive sensing accuracy"""
        # Simplified metric based on recent prediction accuracy
        return min(1.0, self._compute_schema_confidence() * 1.2)
    
    def _compute_joint_flexibility(self) -> float:
        """Compute joint flexibility metric"""
        if not self.experiences:
            return 0.5
            
        # Compute range of motion from recent experiences
        joint_ranges = {}
        for exp in self.experiences[-50:]:
            for joint, angle in exp.initial_body_state.joint_angles.items():
                if joint not in joint_ranges:
                    joint_ranges[joint] = [angle, angle]
                else:
                    joint_ranges[joint][0] = min(joint_ranges[joint][0], angle)
                    joint_ranges[joint][1] = max(joint_ranges[joint][1], angle)
        
        if not joint_ranges:
            return 0.5
            
        avg_range = np.mean([r[1] - r[0] for r in joint_ranges.values()])
        return min(1.0, avg_range / np.pi)  # Normalize to [0,1]
    
    def _compute_muscle_coordination(self) -> float:
        """Compute muscle coordination metric"""
        if len(self.experiences) < 5:
            return 0.5
            
        # Measure consistency of muscle activations for similar actions
        coordination_scores = []
        for i, exp in enumerate(self.experiences[-10:]):
            if exp.success:
                coordination_scores.append(1.0 - np.var(list(exp.motor_action.muscle_commands.values())))
        
        if not coordination_scores:
            return 0.5
            
        return min(1.0, max(0.0, np.mean(coordination_scores)))


class SpatialReasoningEngine:
    """Spatial reasoning engine based on body constraints"""
    
    def __init__(self, body_schema_learner: BodySchemaLearner):
        self.body_schema = body_schema_learner
        self.spatial_memory = {}
        self.reachability_map = {}
        self.obstacle_map = {}
        
    def update_spatial_understanding(self, experience: SensorimotorExperience):
        """Update spatial understanding from embodied experience"""
        try:
            # Extract spatial information
            position = experience.resulting_body_state.position
            reachable = experience.success
            
            # Update reachability map
            pos_key = self._discretize_position(position)
            if pos_key not in self.reachability_map:
                self.reachability_map[pos_key] = {'attempts': 0, 'successes': 0}
            
            self.reachability_map[pos_key]['attempts'] += 1
            if reachable:
                self.reachability_map[pos_key]['successes'] += 1
            
            # Update spatial memory with contextual information
            self.spatial_memory[pos_key] = {
                'last_body_state': experience.resulting_body_state,
                'last_action': experience.motor_action,
                'timestamp': time.time(),
                'sensory_context': experience.sensory_feedback
            }
            
        except Exception as e:
            logger.error(f"Error updating spatial understanding: {e}")
    
    def _discretize_position(self, position: Tuple[float, float, float], 
                           resolution: float = 0.1) -> Tuple[int, int, int]:
        """Discretize continuous position for spatial mapping"""
        return (
            int(position[0] / resolution),
            int(position[1] / resolution), 
            int(position[2] / resolution)
        )
    
    def plan_spatial_action(self, target_position: Tuple[float, float, float],
                          current_body_state: BodyState) -> Optional[MotorAction]:
        """Plan motor action to reach target position using spatial reasoning"""
        try:
            # Check if target is reachable based on body constraints
            if not self._is_position_reachable(target_position, current_body_state):
                logger.debug(f"Target position {target_position} not reachable")
                return None
            
            # Plan trajectory based on spatial memory
            trajectory = self._plan_trajectory(current_body_state.position, target_position)
            
            # Convert trajectory to motor commands
            motor_action = self._trajectory_to_motor_action(trajectory, current_body_state)
            
            return motor_action
            
        except Exception as e:
            logger.error(f"Error planning spatial action: {e}")
            return None
    
    def _is_position_reachable(self, target: Tuple[float, float, float],
                             current_state: BodyState) -> bool:
        """Check if target position is reachable given body constraints"""
        # Simple distance-based reachability check
        current_pos = current_state.position
        distance = np.sqrt(sum((t - c)**2 for t, c in zip(target, current_pos)))
        
        # Get body dimensions for reach calculation
        max_reach = sum(self.body_schema.body_dimensions.get(limb, 1.0) 
                       for limb in ['arm_length', 'torso_height']) * 1.5
        
        return distance <= max_reach
    
    def _plan_trajectory(self, start: Tuple[float, float, float],
                        end: Tuple[float, float, float]) -> List[Tuple[float, float, float]]:
        """Plan trajectory from start to end position"""
        # Simple linear interpolation trajectory
        steps = 10
        trajectory = []
        for i in range(steps + 1):
            alpha = i / steps
            point = tuple(s + alpha * (e - s) for s, e in zip(start, end))
            trajectory.append(point)
        
        return trajectory
    
    def _trajectory_to_motor_action(self, trajectory: List[Tuple[float, float, float]],
                                  current_state: BodyState) -> MotorAction:
        """Convert trajectory to motor action commands"""
        if len(trajectory) < 2:
            return MotorAction()
        
        # Calculate required joint movements for trajectory
        target_pos = trajectory[1]  # Next position in trajectory
        current_pos = current_state.position
        
        # Simple inverse kinematics approximation
        joint_targets = {}
        direction = np.array([t - c for t, c in zip(target_pos, current_pos)])
        
        # Map to joint space (simplified)
        joint_targets['shoulder'] = float(np.arctan2(direction[1], direction[0]))
        joint_targets['elbow'] = float(np.linalg.norm(direction) * 0.1)
        
        return MotorAction(
            joint_targets=joint_targets,
            duration=0.5,
            force=0.8,
            precision=0.9
        )
    
    def get_spatial_metrics(self) -> Dict[str, Any]:
        """Get current spatial reasoning metrics"""
        total_positions = len(self.reachability_map)
        if total_positions == 0:
            return {
                'spatial_coverage': 0.0,
                'reachability_accuracy': 0.0,
                'spatial_memory_size': 0
            }
        
        reachable_positions = sum(1 for pos_data in self.reachability_map.values()
                                if pos_data['successes'] > 0)
        
        total_attempts = sum(pos_data['attempts'] for pos_data in self.reachability_map.values())
        total_successes = sum(pos_data['successes'] for pos_data in self.reachability_map.values())
        
        return {
            'spatial_coverage': reachable_positions / total_positions if total_positions > 0 else 0.0,
            'reachability_accuracy': total_successes / total_attempts if total_attempts > 0 else 0.0,
            'spatial_memory_size': len(self.spatial_memory)
        }


class InverseDynamicsLearner:
    """Learns inverse dynamics mapping from desired outcomes to motor commands"""
    
    def __init__(self, state_dim: int = 20, action_dim: int = 10):
        self.state_dim = state_dim
        self.action_dim = action_dim
        
        # Forward model: predicts outcome from state and action
        self.forward_model_weights = np.random.randn(state_dim + action_dim, state_dim) * 0.01
        
        # Inverse model: predicts action from state and desired outcome
        self.inverse_model_weights = np.random.randn(state_dim + state_dim, action_dim) * 0.01
        
        self.learning_rate = 0.01
        self.model_confidence = 0.1  # Confidence in learned models
        
        # Experience buffer for training
        self.experiences = []
        self.max_buffer_size = 1000
        
    def update_forward_model(self, state: np.ndarray, action: np.ndarray, 
                           next_state: np.ndarray) -> float:
        """Update forward dynamics model with new experience"""
        try:
            # Combine state and action as input
            input_vector = np.concatenate([state, action])
            
            # Predict next state
            predicted_state = input_vector @ self.forward_model_weights
            
            # Compute prediction error
            prediction_error = np.mean((predicted_state - next_state) ** 2)
            
            # Update weights using gradient descent
            error_gradient = 2 * (predicted_state - next_state) / len(predicted_state)
            weight_gradient = np.outer(input_vector, error_gradient)
            
            self.forward_model_weights -= self.learning_rate * weight_gradient
            
            # Update model confidence based on accuracy
            accuracy = np.exp(-prediction_error)
            self.model_confidence = 0.95 * self.model_confidence + 0.05 * accuracy
            
            return float(prediction_error)
            
        except Exception as e:
            logger.error(f"Error updating forward model: {e}")
            return 1.0
    
    def predict_action(self, current_state: np.ndarray, desired_state: np.ndarray) -> np.ndarray:
        """Predict motor action needed to achieve desired state"""
        try:
            # Combine current and desired state as input
            input_vector = np.concatenate([current_state, desired_state])
            
            # Predict required action using inverse model
            predicted_action = input_vector @ self.inverse_model_weights
            
            # Add exploration noise inversely proportional to confidence
            exploration_noise = np.random.normal(0, (1 - self.model_confidence) * 0.1, 
                                               predicted_action.shape)
            predicted_action += exploration_noise
            
            return predicted_action
            
        except Exception as e:
            logger.error(f"Error predicting action: {e}")
            return np.zeros(self.action_dim)
    
    def update_inverse_model(self, state: np.ndarray, action: np.ndarray, 
                           achieved_state: np.ndarray) -> float:
        """Update inverse dynamics model with actual action-outcome pairs"""
        try:
            # Input: current state + achieved state change
            state_change = achieved_state - state
            input_vector = np.concatenate([state, state_change])
            
            # Predict what action should have been taken
            predicted_action = input_vector @ self.inverse_model_weights
            
            # Compute prediction error
            prediction_error = np.mean((predicted_action - action) ** 2)
            
            # Update weights
            error_gradient = 2 * (predicted_action - action) / len(predicted_action)
            weight_gradient = np.outer(input_vector, error_gradient)
            
            self.inverse_model_weights -= self.learning_rate * weight_gradient
            
            return float(prediction_error)
            
        except Exception as e:
            logger.error(f"Error updating inverse model: {e}")
            return 1.0
    
    def add_experience(self, state: np.ndarray, action: np.ndarray, next_state: np.ndarray):
        """Add experience to buffer for batch training"""
        experience = {
            'state': state.copy(),
            'action': action.copy(),
            'next_state': next_state.copy(),
            'timestamp': time.time()
        }
        
        self.experiences.append(experience)
        
        # Remove old experiences if buffer is full
        if len(self.experiences) > self.max_buffer_size:
            self.experiences.pop(0)
    
    def batch_update(self, batch_size: int = 32) -> Dict[str, float]:
        """Perform batch update on models using buffered experiences"""
        if len(self.experiences) < batch_size:
            return {'forward_error': 0.0, 'inverse_error': 0.0, 'samples': 0}
        
        # Sample random batch
        batch_indices = np.random.choice(len(self.experiences), batch_size, replace=False)
        batch_experiences = [self.experiences[i] for i in batch_indices]
        
        forward_errors = []
        inverse_errors = []
        
        for exp in batch_experiences:
            forward_error = self.update_forward_model(exp['state'], exp['action'], exp['next_state'])
            inverse_error = self.update_inverse_model(exp['state'], exp['action'], exp['next_state'])
            
            forward_errors.append(forward_error)
            inverse_errors.append(inverse_error)
        
        return {
            'forward_error': np.mean(forward_errors),
            'inverse_error': np.mean(inverse_errors),
            'samples': len(batch_experiences),
            'model_confidence': self.model_confidence
        }


class EnvironmentalAdaptationEngine:
    """Detects environmental changes and adapts motor behaviors accordingly"""
    
    def __init__(self):
        self.environment_model = {}  # Current model of environment
        self.adaptation_history = []  # History of adaptations made
        self.change_detection_threshold = 0.1  # Threshold for detecting significant changes
        self.adaptation_strategies = {}  # Learned adaptation strategies
        
        # Environmental state tracking
        self.recent_outcomes = []  # Recent sensorimotor outcomes
        self.outcome_buffer_size = 50
        
        # Change detection parameters
        self.baseline_performance = {}  # Baseline performance for each skill
        self.performance_window = 20  # Number of recent attempts to consider
        
    def detect_environmental_change(self, skill_name: str, recent_outcomes: List[SensorimotorExperience]) -> bool:
        """Detect if environment has changed based on performance degradation"""
        if len(recent_outcomes) < self.performance_window:
            return False
        
        # Calculate recent success rate
        recent_successes = sum(1 for outcome in recent_outcomes[-self.performance_window:] if outcome.success)
        recent_success_rate = recent_successes / self.performance_window
        
        # Compare with baseline
        if skill_name not in self.baseline_performance:
            self.baseline_performance[skill_name] = recent_success_rate
            return False
        
        baseline_rate = self.baseline_performance[skill_name]
        performance_drop = baseline_rate - recent_success_rate
        
        # Detect significant performance drop
        change_detected = performance_drop > self.change_detection_threshold
        
        if change_detected:
            logger.info(f"Environmental change detected for skill {skill_name}: "
                      f"performance dropped from {baseline_rate:.3f} to {recent_success_rate:.3f}")
        
        return change_detected
    
    def adapt_to_environment(self, skill_name: str, environmental_context: Dict[str, Any]) -> Dict[str, Any]:
        """Generate adaptation strategy for detected environmental change"""
        try:
            adaptation_strategy = {
                'skill_name': skill_name,
                'context': environmental_context.copy(),
                'timestamp': time.time(),
                'adaptations': {}
            }
            
            # Analyze environmental context to determine adaptations
            if 'temperature' in environmental_context:
                temp = environmental_context['temperature']
                if temp > 30:  # Hot environment
                    adaptation_strategy['adaptations']['force_reduction'] = 0.1
                    adaptation_strategy['adaptations']['precision_increase'] = 0.05
                elif temp < 10:  # Cold environment
                    adaptation_strategy['adaptations']['force_increase'] = 0.1
                    adaptation_strategy['adaptations']['duration_increase'] = 0.2
            
            if 'friction' in environmental_context:
                friction = environmental_context['friction']
                if friction > 0.8:  # High friction
                    adaptation_strategy['adaptations']['force_increase'] = 0.2
                elif friction < 0.2:  # Low friction
                    adaptation_strategy['adaptations']['precision_increase'] = 0.1
                    adaptation_strategy['adaptations']['force_reduction'] = 0.15
            
            if 'gravity' in environmental_context:
                gravity = environmental_context['gravity']
                gravity_ratio = gravity / 9.81  # Ratio to Earth gravity
                adaptation_strategy['adaptations']['force_scaling'] = gravity_ratio
            
            # Store adaptation for future reference
            self.adaptation_history.append(adaptation_strategy)
            
            # Learn adaptation strategy
            self._update_adaptation_strategies(skill_name, environmental_context, adaptation_strategy)
            
            return adaptation_strategy
            
        except Exception as e:
            logger.error(f"Error adapting to environment: {e}")
            return {'adaptations': {}}
    
    def apply_adaptations(self, motor_action: MotorAction, adaptations: Dict[str, Any]) -> MotorAction:
        """Apply environmental adaptations to a motor action"""
        try:
            adapted_action = MotorAction(
                joint_targets=motor_action.joint_targets.copy(),
                muscle_commands=motor_action.muscle_commands.copy(),
                duration=motor_action.duration,
                force=motor_action.force,
                precision=motor_action.precision,
                timestamp=time.time()
            )
            
            # Apply force adaptations
            if 'force_increase' in adaptations:
                adapted_action.force = min(1.0, motor_action.force + adaptations['force_increase'])
            elif 'force_reduction' in adaptations:
                adapted_action.force = max(0.1, motor_action.force - adaptations['force_reduction'])
            elif 'force_scaling' in adaptations:
                adapted_action.force = max(0.1, min(1.0, motor_action.force * adaptations['force_scaling']))
            
            # Apply precision adaptations
            if 'precision_increase' in adaptations:
                adapted_action.precision = min(1.0, motor_action.precision + adaptations['precision_increase'])
            elif 'precision_reduction' in adaptations:
                adapted_action.precision = max(0.1, motor_action.precision - adaptations['precision_reduction'])
            
            # Apply duration adaptations
            if 'duration_increase' in adaptations:
                adapted_action.duration = min(5.0, motor_action.duration + adaptations['duration_increase'])
            elif 'duration_reduction' in adaptations:
                adapted_action.duration = max(0.1, motor_action.duration - adaptations['duration_reduction'])
            
            return adapted_action
            
        except Exception as e:
            logger.error(f"Error applying adaptations: {e}")
            return motor_action
    
    def _update_adaptation_strategies(self, skill_name: str, context: Dict[str, Any], 
                                    strategy: Dict[str, Any]):
        """Learn and update adaptation strategies based on success"""
        context_key = self._hash_context(context)
        
        if skill_name not in self.adaptation_strategies:
            self.adaptation_strategies[skill_name] = {}
        
        if context_key not in self.adaptation_strategies[skill_name]:
            self.adaptation_strategies[skill_name][context_key] = {
                'context': context.copy(),
                'strategy': strategy.copy(),
                'success_count': 0,
                'attempt_count': 0
            }
    
    def _hash_context(self, context: Dict[str, Any]) -> str:
        """Create a hash key for environmental context"""
        # Simple context hashing - could be improved
        key_parts = []
        for key in sorted(context.keys()):
            if isinstance(context[key], (int, float)):
                # Discretize continuous values
                discretized = int(context[key] * 10) / 10
                key_parts.append(f"{key}:{discretized}")
            else:
                key_parts.append(f"{key}:{context[key]}")
        return "_".join(key_parts)
    
    def update_baseline_performance(self, skill_name: str, success_rate: float):
        """Update baseline performance for a skill"""
        if skill_name not in self.baseline_performance:
            self.baseline_performance[skill_name] = success_rate
        else:
            # Exponential moving average
            alpha = 0.1
            self.baseline_performance[skill_name] = (1 - alpha) * self.baseline_performance[skill_name] + alpha * success_rate


class SkillAcquisitionTracker:
    """Tracks skill acquisition progress and implements practice-based improvement"""
    
    def __init__(self):
        self.skill_progression = {}  # Tracks skill development over time
        self.practice_schedules = {}  # Optimal practice schedules for skills
        self.difficulty_levels = {}  # Current difficulty level for each skill
        self.mastery_thresholds = {
            'novice': 0.3,
            'intermediate': 0.6,
            'advanced': 0.8,
            'expert': 0.95
        }
        
    def track_practice_session(self, skill_name: str, outcome: SensorimotorExperience) -> Dict[str, Any]:
        """Track a practice session and update skill progression"""
        try:
            if skill_name not in self.skill_progression:
                self.skill_progression[skill_name] = {
                    'sessions': [],
                    'total_practice_time': 0.0,
                    'current_level': 'novice',
                    'improvement_rate': 0.0,
                    'plateau_detection': {
                        'last_improvement': time.time(),
                        'stagnation_count': 0
                    }
                }
            
            progression = self.skill_progression[skill_name]
            
            # Record session details
            session_data = {
                'timestamp': outcome.timestamp,
                'success': outcome.success,
                'reward': outcome.reward,
                'duration': outcome.motor_action.duration,
                'session_id': len(progression['sessions'])
            }
            
            progression['sessions'].append(session_data)
            progression['total_practice_time'] += outcome.motor_action.duration
            
            # Calculate improvement metrics
            improvement_metrics = self._calculate_improvement_metrics(skill_name)
            progression.update(improvement_metrics)
            
            # Update skill level
            self._update_skill_level(skill_name)
            
            # Check for plateau and adjust difficulty
            self._detect_and_handle_plateau(skill_name)
            
            return {
                'skill_level': progression['current_level'],
                'improvement_rate': progression['improvement_rate'],
                'practice_time': progression['total_practice_time'],
                'session_count': len(progression['sessions'])
            }
            
        except Exception as e:
            logger.error(f"Error tracking practice session for {skill_name}: {e}")
            return {}
    
    def _calculate_improvement_metrics(self, skill_name: str) -> Dict[str, Any]:
        """Calculate improvement rate and learning curve metrics"""
        progression = self.skill_progression[skill_name]
        sessions = progression['sessions']
        
        if len(sessions) < 10:
            return {'improvement_rate': 0.0}
        
        # Calculate success rate over sliding windows
        window_size = min(10, len(sessions) // 4)
        recent_window = sessions[-window_size:]
        earlier_window = sessions[-2*window_size:-window_size] if len(sessions) >= 2*window_size else sessions[:-window_size]
        
        recent_success_rate = sum(s['success'] for s in recent_window) / len(recent_window)
        earlier_success_rate = sum(s['success'] for s in earlier_window) / len(earlier_window)
        
        # Calculate improvement rate (success rate improvement per session)
        improvement_rate = (recent_success_rate - earlier_success_rate) / window_size
        
        return {
            'improvement_rate': improvement_rate,
            'recent_success_rate': recent_success_rate,
            'learning_curve_slope': improvement_rate
        }
    
    def _update_skill_level(self, skill_name: str):
        """Update skill mastery level based on performance"""
        progression = self.skill_progression[skill_name]
        
        if 'recent_success_rate' not in progression:
            return
        
        success_rate = progression['recent_success_rate']
        current_level = progression['current_level']
        
        # Determine new skill level
        new_level = 'novice'
        for level, threshold in self.mastery_thresholds.items():
            if success_rate >= threshold:
                new_level = level
        
        # Only update if there's actual progression
        level_order = list(self.mastery_thresholds.keys())
        if level_order.index(new_level) > level_order.index(current_level):
            progression['current_level'] = new_level
            progression['plateau_detection']['last_improvement'] = time.time()
            progression['plateau_detection']['stagnation_count'] = 0
            
            logger.info(f"Skill {skill_name} advanced to {new_level} level")
    
    def _detect_and_handle_plateau(self, skill_name: str):
        """Detect learning plateaus and adjust practice accordingly"""
        progression = self.skill_progression[skill_name]
        plateau_info = progression['plateau_detection']
        
        # Check for plateau (no improvement for extended period)
        time_since_improvement = time.time() - plateau_info['last_improvement']
        improvement_rate = progression.get('improvement_rate', 0.0)
        
        if time_since_improvement > 300 and improvement_rate < 0.01:  # 5 minutes without improvement
            plateau_info['stagnation_count'] += 1
            
            if plateau_info['stagnation_count'] > 3:  # Multiple plateau detections
                self._adjust_practice_for_plateau(skill_name)
                plateau_info['stagnation_count'] = 0  # Reset counter
    
    def _adjust_practice_for_plateau(self, skill_name: str):
        """Adjust practice strategy when plateau is detected"""
        if skill_name not in self.practice_schedules:
            self.practice_schedules[skill_name] = {
                'difficulty_increase': 0.1,
                'exploration_boost': 0.2,
                'practice_variation': True
            }
        
        schedule = self.practice_schedules[skill_name]
        
        # Increase difficulty to push beyond plateau
        schedule['difficulty_increase'] = min(0.3, schedule['difficulty_increase'] + 0.05)
        
        # Boost exploration to find new solutions
        schedule['exploration_boost'] = min(0.5, schedule['exploration_boost'] + 0.1)
        
        # Enable practice variation
        schedule['practice_variation'] = True
        
        logger.info(f"Adjusted practice for {skill_name} to overcome plateau: "
                   f"difficulty +{schedule['difficulty_increase']}, "
                   f"exploration +{schedule['exploration_boost']}")
    
    def get_practice_recommendations(self, skill_name: str) -> Dict[str, Any]:
        """Get recommendations for optimizing practice sessions"""
        if skill_name not in self.skill_progression:
            return {
                'focus_areas': ['basic_control'],
                'difficulty_adjustment': 0.0,
                'exploration_level': 0.2,
                'practice_duration': 60.0
            }
        
        progression = self.skill_progression[skill_name]
        level = progression['current_level']
        
        recommendations = {
            'focus_areas': self._get_focus_areas_for_level(level),
            'difficulty_adjustment': self._get_difficulty_adjustment(skill_name),
            'exploration_level': self._get_exploration_level(skill_name),
            'practice_duration': self._get_optimal_practice_duration(skill_name)
        }
        
        return recommendations
    
    def _get_focus_areas_for_level(self, level: str) -> List[str]:
        """Get practice focus areas based on skill level"""
        focus_mapping = {
            'novice': ['basic_control', 'stability'],
            'intermediate': ['precision', 'timing', 'consistency'],
            'advanced': ['efficiency', 'adaptability', 'complex_sequences'],
            'expert': ['optimization', 'innovation', 'teaching']
        }
        return focus_mapping.get(level, ['basic_control'])
    
    def _get_difficulty_adjustment(self, skill_name: str) -> float:
        """Calculate difficulty adjustment based on recent performance"""
        if skill_name in self.practice_schedules:
            return self.practice_schedules[skill_name].get('difficulty_increase', 0.0)
        return 0.0
    
    def _get_exploration_level(self, skill_name: str) -> float:
        """Calculate exploration level based on learning progress"""
        if skill_name in self.practice_schedules:
            return self.practice_schedules[skill_name].get('exploration_boost', 0.2)
        return 0.2
    
    def _get_optimal_practice_duration(self, skill_name: str) -> float:
        """Calculate optimal practice session duration"""
        if skill_name not in self.skill_progression:
            return 60.0
        
        progression = self.skill_progression[skill_name]
        level = progression['current_level']
        
        # Adjust duration based on skill level
        duration_mapping = {
            'novice': 60.0,
            'intermediate': 90.0,
            'advanced': 120.0,
            'expert': 150.0
        }
        
        return duration_mapping.get(level, 60.0)


class MotorSkillLearner:
    """Motor skill acquisition through embodied practice"""
    
    def __init__(self, body_schema_learner: BodySchemaLearner):
        self.body_schema = body_schema_learner
        self.skill_library = {}
        self.practice_history = {}
        self.skill_performance = {}
        
        # Motor learning components
        self.inverse_dynamics_learner = InverseDynamicsLearner()
        self.environmental_adapter = EnvironmentalAdaptationEngine()
        self.skill_acquisition_tracker = SkillAcquisitionTracker()
        
    def practice_skill(self, skill_name: str, target_outcome: Dict[str, Any],
                      current_state: BodyState) -> MotorAction:
        """Practice a motor skill and learn from experience"""
        try:
            # Get or initialize skill
            if skill_name not in self.skill_library:
                self.skill_library[skill_name] = self._initialize_skill(skill_name)
            
            skill = self.skill_library[skill_name]
            
            # Generate motor action based on current skill level
            motor_action = self._generate_skill_action(skill, current_state, target_outcome)
            
            # Record practice attempt
            if skill_name not in self.practice_history:
                self.practice_history[skill_name] = []
            
            practice_record = {
                'timestamp': time.time(),
                'initial_state': current_state,
                'action': motor_action,
                'target': target_outcome
            }
            
            self.practice_history[skill_name].append(practice_record)
            
            return motor_action
            
        except Exception as e:
            logger.error(f"Error practicing skill {skill_name}: {e}")
            return MotorAction()
    
    def learn_from_outcome(self, skill_name: str, outcome: SensorimotorExperience):
        """Learn from the outcome of skill practice using enhanced motor learning algorithms"""
        try:
            if skill_name not in self.skill_library:
                return
            
            skill = self.skill_library[skill_name]
            
            # 1. Update inverse dynamics models
            self._update_inverse_dynamics(outcome)
            
            # 2. Track skill acquisition progress
            progression_metrics = self.skill_acquisition_tracker.track_practice_session(skill_name, outcome)
            
            # 3. Detect and adapt to environmental changes
            self._check_environmental_adaptation(skill_name, outcome)
            
            # 4. Update skill parameters based on outcome
            if outcome.success:
                # Reinforce successful actions
                self._reinforce_skill_action(skill, outcome)
            else:
                # Adjust skill to avoid unsuccessful actions
                self._adjust_skill_action(skill, outcome)
            
            # 5. Update performance metrics
            if skill_name not in self.skill_performance:
                self.skill_performance[skill_name] = {
                    'attempts': 0,
                    'successes': 0,
                    'avg_reward': 0.0,
                    'environmental_adaptations': 0,
                    'inverse_dynamics_updates': 0
                }
            
            perf = self.skill_performance[skill_name]
            perf['attempts'] += 1
            if outcome.success:
                perf['successes'] += 1
            
            # Update average reward using exponential moving average
            alpha = 0.1
            perf['avg_reward'] = (1 - alpha) * perf['avg_reward'] + alpha * outcome.reward
            
            # Update skill progression metrics
            perf.update(progression_metrics)
            
        except Exception as e:
            logger.error(f"Error learning from outcome for skill {skill_name}: {e}")
    
    def _initialize_skill(self, skill_name: str) -> Dict[str, Any]:
        """Initialize a new motor skill"""
        return {
            'name': skill_name,
            'parameters': np.random.randn(20) * 0.1,  # Skill parameters
            'variance': np.ones(20) * 0.2,  # Exploration variance
            'created_time': time.time(),
            'updates': 0
        }
    
    def _generate_skill_action(self, skill: Dict[str, Any], current_state: BodyState,
                             target_outcome: Dict[str, Any]) -> MotorAction:
        """Generate motor action for skill execution"""
        # Simple policy: use skill parameters with exploration
        params = skill['parameters']
        variance = skill['variance']
        
        # Add exploration noise
        exploration = np.random.normal(0, variance)
        action_params = params + exploration
        
        # Convert to motor action
        joint_targets = {
            'shoulder': float(action_params[0]),
            'elbow': float(action_params[1]),
            'wrist': float(action_params[2]) if len(action_params) > 2 else 0.0
        }
        
        muscle_commands = {
            'bicep': max(0, min(1, action_params[3])) if len(action_params) > 3 else 0.5,
            'tricep': max(0, min(1, action_params[4])) if len(action_params) > 4 else 0.5
        }
        
        return MotorAction(
            joint_targets=joint_targets,
            muscle_commands=muscle_commands,
            duration=max(0.1, min(2.0, action_params[5] if len(action_params) > 5 else 1.0)),
            force=max(0.1, min(1.0, action_params[6] if len(action_params) > 6 else 0.8)),
            precision=max(0.1, min(1.0, action_params[7] if len(action_params) > 7 else 0.7))
        )
    
    def _reinforce_skill_action(self, skill: Dict[str, Any], outcome: SensorimotorExperience):
        """Reinforce successful skill actions"""
        # Simple policy gradient update
        learning_rate = 0.01
        
        # Get action parameters that led to success
        action_vector = outcome.motor_action.to_vector()
        
        # Update skill parameters toward successful action
        if len(action_vector) <= len(skill['parameters']):
            skill['parameters'][:len(action_vector)] += learning_rate * action_vector * outcome.reward
        
        # Reduce exploration variance for successful actions
        skill['variance'] *= 0.99
        skill['updates'] += 1
    
    def _adjust_skill_action(self, skill: Dict[str, Any], outcome: SensorimotorExperience):
        """Adjust skill to avoid unsuccessful actions"""
        # Increase exploration variance after failures
        skill['variance'] *= 1.01
        skill['variance'] = np.minimum(skill['variance'], 1.0)  # Cap variance
        skill['updates'] += 1
    
    def get_skill_metrics(self) -> Dict[str, Any]:
        """Get motor skill learning metrics"""
        if not self.skill_performance:
            return {
                'total_skills': 0,
                'avg_success_rate': 0.0,
                'total_practice_time': 0
            }
        
        total_skills = len(self.skill_performance)
        success_rates = []
        total_attempts = 0
        
        for skill_name, perf in self.skill_performance.items():
            if perf['attempts'] > 0:
                success_rate = perf['successes'] / perf['attempts']
                success_rates.append(success_rate)
                total_attempts += perf['attempts']
        
        avg_success_rate = np.mean(success_rates) if success_rates else 0.0
        
        # Add enhanced metrics
        environmental_adaptations = sum(perf.get('environmental_adaptations', 0) for perf in self.skill_performance.values())
        inverse_dynamics_updates = sum(perf.get('inverse_dynamics_updates', 0) for perf in self.skill_performance.values())
        
        return {
            'total_skills': total_skills,
            'avg_success_rate': float(avg_success_rate),
            'total_practice_attempts': total_attempts,
            'skill_names': list(self.skill_performance.keys()),
            'environmental_adaptations': environmental_adaptations,
            'inverse_dynamics_updates': inverse_dynamics_updates,
            'model_confidence': self.inverse_dynamics_learner.model_confidence,
            'skill_levels': {name: self.skill_acquisition_tracker.skill_progression.get(name, {}).get('current_level', 'novice') 
                           for name in self.skill_performance}
        }
    
    # New Motor Learning Algorithm Methods
    
    def learn_inverse_dynamics(self, skill_name: str, target_outcome: Dict[str, Any], 
                              current_state: BodyState) -> MotorAction:
        """Use inverse dynamics learning to predict motor action for desired outcome"""
        try:
            # Convert states to numpy arrays for inverse dynamics model
            current_state_vector = current_state.to_vector()
            
            # Create desired state vector from target outcome
            desired_state_vector = self._target_to_state_vector(target_outcome, current_state_vector)
            
            # Use inverse dynamics learner to predict action
            predicted_action_vector = self.inverse_dynamics_learner.predict_action(
                current_state_vector, desired_state_vector
            )
            
            # Convert predicted action vector to MotorAction
            motor_action = self._action_vector_to_motor_action(predicted_action_vector)
            
            logger.debug(f"Inverse dynamics prediction for {skill_name}: confidence={self.inverse_dynamics_learner.model_confidence:.3f}")
            
            return motor_action
            
        except Exception as e:
            logger.error(f"Error in inverse dynamics learning for {skill_name}: {e}")
            return MotorAction()
    
    def adapt_to_environmental_changes(self, skill_name: str, environmental_context: Dict[str, Any]) -> Dict[str, Any]:
        """Adapt motor behavior to detected environmental changes"""
        try:
            # Detect if environment has changed
            recent_outcomes = self._get_recent_outcomes(skill_name)
            change_detected = self.environmental_adapter.detect_environmental_change(skill_name, recent_outcomes)
            
            if change_detected:
                # Generate adaptation strategy
                adaptation_strategy = self.environmental_adapter.adapt_to_environment(skill_name, environmental_context)
                
                # Update skill performance tracking
                if skill_name in self.skill_performance:
                    self.skill_performance[skill_name]['environmental_adaptations'] = \
                        self.skill_performance[skill_name].get('environmental_adaptations', 0) + 1
                
                logger.info(f"Adapted {skill_name} to environmental changes: {adaptation_strategy['adaptations']}")
                
                return adaptation_strategy
            else:
                return {'adaptations': {}}
                
        except Exception as e:
            logger.error(f"Error adapting to environmental changes for {skill_name}: {e}")
            return {'adaptations': {}}
    
    def practice_skill_with_improvement(self, skill_name: str, target_outcome: Dict[str, Any],
                                      current_state: BodyState, environmental_context: Optional[Dict[str, Any]] = None) -> Tuple[MotorAction, Dict[str, Any]]:
        """Enhanced skill practice with all three motor learning algorithms"""
        try:
            # 1. Get practice recommendations from skill acquisition tracker
            recommendations = self.skill_acquisition_tracker.get_practice_recommendations(skill_name)
            
            # 2. Use inverse dynamics learning if model confidence is high enough
            if self.inverse_dynamics_learner.model_confidence > 0.5:
                base_action = self.learn_inverse_dynamics(skill_name, target_outcome, current_state)
            else:
                # Fallback to traditional skill practice
                base_action = self.practice_skill(skill_name, target_outcome, current_state)
            
            # 3. Apply environmental adaptations if context is provided
            if environmental_context:
                adaptation_strategy = self.adapt_to_environmental_changes(skill_name, environmental_context)
                if adaptation_strategy.get('adaptations'):
                    adapted_action = self.environmental_adapter.apply_adaptations(base_action, adaptation_strategy['adaptations'])
                else:
                    adapted_action = base_action
            else:
                adapted_action = base_action
                adaptation_strategy = {'adaptations': {}}
            
            # 4. Apply practice recommendations (adjust exploration, difficulty)
            final_action = self._apply_practice_recommendations(adapted_action, recommendations)
            
            # 5. Prepare learning metrics
            learning_metrics = {
                'recommendations': recommendations,
                'adaptation_strategy': adaptation_strategy,
                'inverse_dynamics_used': self.inverse_dynamics_learner.model_confidence > 0.5,
                'model_confidence': self.inverse_dynamics_learner.model_confidence
            }
            
            return final_action, learning_metrics
            
        except Exception as e:
            logger.error(f"Error in enhanced skill practice for {skill_name}: {e}")
            return MotorAction(), {}
    
    # Helper methods for new motor learning algorithms
    
    def _update_inverse_dynamics(self, outcome: SensorimotorExperience):
        """Update inverse dynamics models with new experience"""
        try:
            initial_state_vector = outcome.initial_body_state.to_vector()
            final_state_vector = outcome.resulting_body_state.to_vector()
            action_vector = outcome.motor_action.to_vector()
            
            # Ensure consistent dimensions by using fixed sizes
            state_dim = 20  # Fixed state dimension
            action_dim = 10  # Fixed action dimension
            
            # Pad or truncate vectors to consistent size
            initial_state_vector = self._normalize_vector(initial_state_vector, state_dim)
            final_state_vector = self._normalize_vector(final_state_vector, state_dim)  
            action_vector = self._normalize_vector(action_vector, action_dim)
            
            # Update forward model
            self.inverse_dynamics_learner.update_forward_model(
                initial_state_vector, action_vector, final_state_vector
            )
            
            # Update inverse model
            self.inverse_dynamics_learner.update_inverse_model(
                initial_state_vector, action_vector, final_state_vector
            )
            
            # Add experience to buffer
            self.inverse_dynamics_learner.add_experience(initial_state_vector, action_vector, final_state_vector)
            
            # Track successful update
            if hasattr(self, '_recent_outcomes'):
                for skill_name in self.skill_performance:
                    if 'inverse_dynamics_updates' in self.skill_performance[skill_name]:
                        self.skill_performance[skill_name]['inverse_dynamics_updates'] += 1
            
            # Perform batch update periodically
            if len(self.inverse_dynamics_learner.experiences) % 32 == 0:
                batch_metrics = self.inverse_dynamics_learner.batch_update()
                logger.debug(f"Batch update completed: {batch_metrics}")
            
        except Exception as e:
            logger.error(f"Error updating inverse dynamics: {e}")
    
    def _normalize_vector(self, vector: np.ndarray, target_size: int) -> np.ndarray:
        """Normalize vector to target size by padding or truncating"""
        if len(vector) == target_size:
            return vector
        elif len(vector) < target_size:
            # Pad with zeros
            return np.pad(vector, (0, target_size - len(vector)))
        else:
            # Truncate
            return vector[:target_size]
    
    def _check_environmental_adaptation(self, skill_name: str, outcome: SensorimotorExperience):
        """Check if environmental adaptation is needed based on outcome"""
        try:
            # Update recent outcomes
            if not hasattr(self, '_recent_outcomes'):
                self._recent_outcomes = {}
            
            if skill_name not in self._recent_outcomes:
                self._recent_outcomes[skill_name] = []
            
            self._recent_outcomes[skill_name].append(outcome)
            
            # Keep only recent outcomes
            max_outcomes = 50
            if len(self._recent_outcomes[skill_name]) > max_outcomes:
                self._recent_outcomes[skill_name] = self._recent_outcomes[skill_name][-max_outcomes:]
            
            # Check for environmental change every 10 outcomes
            recent_outcomes = self._recent_outcomes[skill_name]
            if len(recent_outcomes) >= 20 and len(recent_outcomes) % 10 == 0:
                change_detected = self.environmental_adapter.detect_environmental_change(skill_name, recent_outcomes)
                
                if change_detected:
                    # Increment adaptation counter
                    if skill_name in self.skill_performance:
                        self.skill_performance[skill_name]['environmental_adaptations'] = \
                            self.skill_performance[skill_name].get('environmental_adaptations', 0) + 1
            
            # Update baseline performance
            if len(recent_outcomes) >= 20:
                success_rate = sum(1 for o in recent_outcomes[-20:] if o.success) / 20
                self.environmental_adapter.update_baseline_performance(skill_name, success_rate)
            
        except Exception as e:
            logger.error(f"Error checking environmental adaptation: {e}")
    
    def _target_to_state_vector(self, target_outcome: Dict[str, Any], current_state_vector: np.ndarray) -> np.ndarray:
        """Convert target outcome to desired state vector"""
        desired_state_vector = current_state_vector.copy()
        
        # Update position if specified in target
        if 'position' in target_outcome and isinstance(target_outcome['position'], (list, tuple)):
            position = target_outcome['position']
            desired_state_vector[:3] = position[:3]  # x, y, z
        
        # Update joint angles if specified
        if 'joint_angles' in target_outcome:
            # This would be more sophisticated in a real implementation
            pass
            
        return desired_state_vector
    
    def _action_vector_to_motor_action(self, action_vector: np.ndarray) -> MotorAction:
        """Convert action vector from inverse dynamics to MotorAction"""
        # Ensure we have enough elements
        padded_vector = np.pad(action_vector, (0, max(0, 10 - len(action_vector))))
        
        joint_targets = {
            'shoulder': float(padded_vector[0]),
            'elbow': float(padded_vector[1]),
            'wrist': float(padded_vector[2])
        }
        
        muscle_commands = {
            'bicep': max(0, min(1, float(padded_vector[3]))),
            'tricep': max(0, min(1, float(padded_vector[4])))
        }
        
        return MotorAction(
            joint_targets=joint_targets,
            muscle_commands=muscle_commands,
            duration=max(0.1, min(2.0, float(padded_vector[5]) if len(padded_vector) > 5 else 1.0)),
            force=max(0.1, min(1.0, float(padded_vector[6]) if len(padded_vector) > 6 else 0.8)),
            precision=max(0.1, min(1.0, float(padded_vector[7]) if len(padded_vector) > 7 else 0.7))
        )
    
    def _get_recent_outcomes(self, skill_name: str) -> List[SensorimotorExperience]:
        """Get recent outcomes for a skill"""
        if not hasattr(self, '_recent_outcomes') or skill_name not in self._recent_outcomes:
            return []
        return self._recent_outcomes[skill_name]
    
    def _apply_practice_recommendations(self, motor_action: MotorAction, recommendations: Dict[str, Any]) -> MotorAction:
        """Apply practice recommendations to motor action"""
        try:
            adjusted_action = MotorAction(
                joint_targets=motor_action.joint_targets.copy(),
                muscle_commands=motor_action.muscle_commands.copy(),
                duration=motor_action.duration,
                force=motor_action.force,
                precision=motor_action.precision,
                timestamp=time.time()
            )
            
            # Apply difficulty adjustment
            difficulty_adj = recommendations.get('difficulty_adjustment', 0.0)
            if difficulty_adj > 0:
                # Increase precision requirements
                adjusted_action.precision = min(1.0, motor_action.precision + difficulty_adj)
            
            # Apply exploration level
            exploration_level = recommendations.get('exploration_level', 0.2)
            if exploration_level > 0.3:  # High exploration
                # Add noise to joint targets
                for joint in adjusted_action.joint_targets:
                    noise = np.random.normal(0, exploration_level * 0.1)
                    adjusted_action.joint_targets[joint] += noise
            
            return adjusted_action
            
        except Exception as e:
            logger.error(f"Error applying practice recommendations: {e}")
            return motor_action


class EmbodiedLearningSystem:
    """Main embodied learning system integrating all components"""
    
    def __init__(self, body_dimensions: Optional[Dict[str, float]] = None):
        # Default body dimensions
        if body_dimensions is None:
            body_dimensions = {
                'arm_length': 0.7,
                'leg_length': 0.9,
                'torso_height': 0.6,
                'head_size': 0.2
            }
        
        self.body_schema_learner = BodySchemaLearner(body_dimensions)
        self.spatial_reasoning = SpatialReasoningEngine(self.body_schema_learner)
        self.motor_skill_learner = MotorSkillLearner(self.body_schema_learner)
        
        # Integration with existing systems
        self.sensory_motor = None
        if SENSORY_MOTOR_AVAILABLE:
            try:
                self.sensory_motor = SensoryMotor()
            except Exception as e:
                logger.warning(f"Could not initialize SensoryMotor: {e}")
        
        self.cognitive_architecture = None
        if COGNITIVE_ARCH_AVAILABLE:
            try:
                self.cognitive_architecture = CognitiveArchitecture()
            except Exception as e:
                logger.warning(f"Could not initialize CognitiveArchitecture: {e}")
        
        self.learning_active = True
        self.experience_count = 0
    
    def process_embodied_experience(self, experience: SensorimotorExperience) -> Dict[str, Any]:
        """Process a complete embodied experience for learning"""
        try:
            results = {}
            
            if not self.learning_active:
                return results
            
            # Update body schema
            self.body_schema_learner.update_body_schema(experience)
            results['body_schema_updated'] = True
            
            # Update spatial understanding
            self.spatial_reasoning.update_spatial_understanding(experience)
            results['spatial_understanding_updated'] = True
            
            # Create embodied memory if cognitive architecture available
            if self.cognitive_architecture and COGNITIVE_ARCH_AVAILABLE:
                memory = Memory(
                    content=f"Embodied experience at position {experience.resulting_body_state.position}",
                    memory_type=MemoryType.EPISODIC,
                    timestamp=experience.timestamp,
                    emotional_valence=1.0 if experience.success else -0.5,
                    importance=0.8 if experience.success else 0.3
                )
                memory.context.update({
                    'body_position': experience.resulting_body_state.position,
                    'motor_action': experience.motor_action.joint_targets,
                    'success': experience.success,
                    'reward': experience.reward
                })
                results['embodied_memory_created'] = True
            
            self.experience_count += 1
            results['total_experiences'] = self.experience_count
            
            return results
            
        except Exception as e:
            logger.error(f"Error processing embodied experience: {e}")
            return {'error': str(e)}
    
    def learn_motor_skill(self, skill_name: str, target_outcome: Dict[str, Any],
                         current_body_state: BodyState) -> Tuple[MotorAction, Dict[str, Any]]:
        """Learn a motor skill through embodied practice"""
        try:
            # Practice the skill
            motor_action = self.motor_skill_learner.practice_skill(
                skill_name, target_outcome, current_body_state
            )
            
            # Get current skill metrics
            skill_metrics = self.motor_skill_learner.get_skill_metrics()
            
            return motor_action, skill_metrics
            
        except Exception as e:
            logger.error(f"Error learning motor skill {skill_name}: {e}")
            return MotorAction(), {'error': str(e)}
    
    def update_from_skill_outcome(self, skill_name: str, outcome: SensorimotorExperience):
        """Update learning from motor skill practice outcome"""
        try:
            # Update motor skill learning
            self.motor_skill_learner.learn_from_outcome(skill_name, outcome)
            
            # Process as general embodied experience
            self.process_embodied_experience(outcome)
            
        except Exception as e:
            logger.error(f"Error updating from skill outcome: {e}")
    
    def plan_spatial_movement(self, target_position: Tuple[float, float, float],
                            current_body_state: BodyState) -> Optional[MotorAction]:
        """Plan movement to target position using spatial reasoning"""
        return self.spatial_reasoning.plan_spatial_action(target_position, current_body_state)
    
    def get_embodiment_metrics(self) -> Dict[str, Any]:
        """Get comprehensive embodied learning metrics"""
        try:
            metrics = {
                'body_awareness': self.body_schema_learner.get_body_awareness(),
                'spatial_metrics': self.spatial_reasoning.get_spatial_metrics(),
                'motor_skill_metrics': self.motor_skill_learner.get_skill_metrics(),
                'system_metrics': {
                    'total_experiences': self.experience_count,
                    'learning_active': self.learning_active,
                    'sensory_motor_available': self.sensory_motor is not None,
                    'cognitive_arch_available': self.cognitive_architecture is not None
                }
            }
            
            return metrics
            
        except Exception as e:
            logger.error(f"Error getting embodiment metrics: {e}")
            return {'error': str(e)}
    
    def set_learning_active(self, active: bool):
        """Enable or disable active learning"""
        self.learning_active = active
        logger.info(f"Embodied learning {'activated' if active else 'deactivated'}")


# Factory function for easy instantiation
def create_embodied_learning_system(body_dimensions: Optional[Dict[str, float]] = None) -> EmbodiedLearningSystem:
    """Create and initialize an embodied learning system"""
    return EmbodiedLearningSystem(body_dimensions)


# Example usage and testing
if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    
    # Create embodied learning system
    embodied_system = create_embodied_learning_system()
    
    # Example body state
    body_state = BodyState(
        position=(0.0, 0.0, 1.0),
        joint_angles={'shoulder': 0.5, 'elbow': 1.0},
        joint_velocities={'shoulder': 0.0, 'elbow': 0.0}
    )
    
    # Example motor action
    motor_action = MotorAction(
        joint_targets={'shoulder': 1.0, 'elbow': 0.5},
        duration=1.0
    )
    
    # Create and process embodied experience
    experience = SensorimotorExperience(
        initial_body_state=body_state,
        motor_action=motor_action,
        resulting_body_state=body_state,
        sensory_feedback={'touch': 'object_contact'},
        reward=1.0,
        success=True
    )
    
    result = embodied_system.process_embodied_experience(experience)
    print(f"Processing result: {result}")
    
    # Get embodiment metrics
    metrics = embodied_system.get_embodiment_metrics()
    print(f"Embodiment metrics: {metrics}")
