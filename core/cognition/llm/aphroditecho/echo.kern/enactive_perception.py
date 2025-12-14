#!/usr/bin/env python3
"""
Enactive Perception System for Deep Tree Echo

Implements Task 2.3.3 from the Deep Tree Echo roadmap:
- Action-based perception mechanisms
- Sensorimotor contingency learning
- Perceptual prediction through action

This module provides the core enactive perception capabilities where perception
emerges through agent-environment interaction, following the 4E Embodied AI Framework.
"""

try:
    import numpy as np
    NUMPY_AVAILABLE = True
except ImportError:
    NUMPY_AVAILABLE = False
    # Define minimal numpy-like functions for compatibility
    class np:
        @staticmethod
        def array(data):
            return list(data) if isinstance(data, (list, tuple)) else [data]
        
        @staticmethod
        def random():
            import random as python_random
            class _random:
                @staticmethod  
                def uniform(low, high, size=None):
                    if size:
                        return [python_random.uniform(low, high) for _ in range(size)]
                    return python_random.uniform(low, high)
                
                @staticmethod
                def normal(mean, std, size=None):
                    if size:
                        return [python_random.gauss(mean, std) for _ in range(size)]
                    return python_random.gauss(mean, std)
                
                @staticmethod  
                def choice(choices):
                    return python_random.choice(choices)
                
                @staticmethod
                def randn(*shape):
                    if len(shape) == 0:
                        return python_random.gauss(0, 1)
                    elif len(shape) == 1:
                        return [python_random.gauss(0, 1) for _ in range(shape[0])]
                    else:
                        # Simple 2D case
                        return [[python_random.gauss(0, 1) for _ in range(shape[1])] for _ in range(shape[0])]
            return _random()
        
        @staticmethod
        def mean(data):
            if not data:
                return 0.0
            return sum(data) / len(data)
        
        @staticmethod
        def clip(value, min_val, max_val):
            return max(min_val, min(max_val, value))
        
        @staticmethod
        def minimum(a, b):
            if hasattr(a, '__iter__') and hasattr(b, '__iter__'):
                return [min(x, y) for x, y in zip(a, b)]
            return min(a, b)
        
        pi = 3.14159265359

import logging
import time
from dataclasses import dataclass, field
from typing import Dict, List, Tuple, Optional, Any
from collections import deque

# Import existing components for integration
try:
    import sys
    from pathlib import Path
    sys.path.append(str(Path(__file__).parent.parent / "echo.dash"))
    from embodied_learning import BodyState, MotorAction, SensorimotorExperience
    EMBODIED_LEARNING_AVAILABLE = True
except ImportError:
    EMBODIED_LEARNING_AVAILABLE = False
    # Define minimal fallback classes
    @dataclass
    class BodyState:
        position: Tuple[float, float, float] = (0.0, 0.0, 0.0)
        orientation: Tuple[float, float, float] = (0.0, 0.0, 0.0)
        joint_angles: Dict[str, float] = field(default_factory=dict)
        sensory_state: Dict[str, Any] = field(default_factory=dict)
        timestamp: float = field(default_factory=time.time)
    
    @dataclass
    class MotorAction:
        joint_targets: Dict[str, float] = field(default_factory=dict)
        muscle_commands: Dict[str, float] = field(default_factory=dict)
        duration: float = 1.0
        force: float = 1.0
        precision: float = 1.0
        timestamp: float = field(default_factory=time.time)
    
    @dataclass
    class SensorimotorExperience:
        initial_body_state: BodyState
        motor_action: MotorAction
        resulting_body_state: BodyState
        sensory_feedback: Dict[str, Any]
        reward: float = 0.0
        success: bool = False
        timestamp: float = field(default_factory=time.time)

try:
    sys.path.append(str(Path(__file__).parent.parent / "echo.dream"))
    from aar_system import Agent, Arena, AARComponent
    AAR_SYSTEM_AVAILABLE = True
except ImportError:
    AAR_SYSTEM_AVAILABLE = False

logger = logging.getLogger(__name__)


@dataclass
class SensorimotorContingency:
    """Represents a learned sensorimotor contingency mapping"""
    action_pattern: Dict[str, Any]  # The action that was taken
    sensory_context: Dict[str, Any]  # The sensory context when action occurred
    expected_outcome: Dict[str, Any]  # Expected sensory changes
    actual_outcome: Dict[str, Any]  # Actual sensory changes observed
    confidence: float = 0.5  # Confidence in this contingency
    frequency: int = 1  # How often this pattern has been observed
    last_updated: float = field(default_factory=time.time)


@dataclass
class PerceptualPrediction:
    """Represents a prediction about upcoming perceptual experience"""
    action_plan: MotorAction  # The action being considered
    predicted_sensory_outcome: Dict[str, Any]  # What we expect to perceive
    confidence: float = 0.5  # Confidence in prediction
    exploration_value: float = 0.0  # How much this action would explore unknown space
    prediction_timestamp: float = field(default_factory=time.time)


class SensorimotorContingencyLearner:
    """Learns mappings between actions and their sensory consequences"""
    
    def __init__(self, max_contingencies: int = 1000, learning_rate: float = 0.1):
        self.max_contingencies = max_contingencies
        self.learning_rate = learning_rate
        self.contingencies: List[SensorimotorContingency] = []
        self.action_history = deque(maxlen=100)
        self.sensory_history = deque(maxlen=100)
    
    def learn_contingency(self, experience: SensorimotorExperience) -> bool:
        """Learn a new sensorimotor contingency from experience"""
        try:
            # Extract action pattern
            action_pattern = {
                'joint_targets': experience.motor_action.joint_targets.copy(),
                'muscle_commands': experience.motor_action.muscle_commands.copy(),
                'force': experience.motor_action.force,
                'duration': experience.motor_action.duration
            }
            
            # Extract sensory context and outcome
            sensory_context = experience.initial_body_state.sensory_state.copy()
            actual_outcome = experience.sensory_feedback.copy()
            
            # Look for existing similar contingency
            similar_contingency = self._find_similar_contingency(action_pattern, sensory_context)
            
            if similar_contingency:
                # Update existing contingency
                self._update_contingency(similar_contingency, actual_outcome)
                logger.debug(f"Updated existing contingency, confidence: {similar_contingency.confidence:.3f}")
            else:
                # Create new contingency
                new_contingency = SensorimotorContingency(
                    action_pattern=action_pattern,
                    sensory_context=sensory_context,
                    expected_outcome=actual_outcome.copy(),  # Start with what we observed
                    actual_outcome=actual_outcome,
                    confidence=0.1,  # Start with low confidence
                    frequency=1
                )
                self.contingencies.append(new_contingency)
                logger.debug(f"Learned new contingency: {len(self.contingencies)} total")
            
            # Maintain maximum contingencies limit
            if len(self.contingencies) > self.max_contingencies:
                # Remove least confident contingencies
                self.contingencies.sort(key=lambda c: c.confidence)
                self.contingencies = self.contingencies[-self.max_contingencies:]
            
            return True
            
        except Exception as e:
            logger.error(f"Error learning contingency: {e}")
            return False
    
    def predict_sensory_outcome(self, action: MotorAction, current_sensory_state: Dict[str, Any]) -> Dict[str, Any]:
        """Predict sensory outcome of an action given current sensory state"""
        try:
            action_pattern = {
                'joint_targets': action.joint_targets.copy(),
                'muscle_commands': action.muscle_commands.copy(),
                'force': action.force,
                'duration': action.duration
            }
            
            # Find best matching contingency
            best_match = None
            best_similarity = 0.0
            
            for contingency in self.contingencies:
                similarity = self._calculate_similarity(
                    action_pattern, contingency.action_pattern,
                    current_sensory_state, contingency.sensory_context
                )
                
                if similarity > best_similarity and contingency.confidence > 0.3:
                    best_similarity = similarity
                    best_match = contingency
            
            if best_match and best_similarity > 0.5:
                # Scale prediction by confidence and similarity
                confidence_factor = best_match.confidence * best_similarity
                predicted_outcome = {}
                
                for key, value in best_match.expected_outcome.items():
                    if isinstance(value, (int, float)):
                        predicted_outcome[key] = value * confidence_factor
                    else:
                        predicted_outcome[key] = value
                
                return predicted_outcome
            
            return {}  # No good prediction available
            
        except Exception as e:
            logger.error(f"Error predicting sensory outcome: {e}")
            return {}
    
    def _find_similar_contingency(self, action_pattern: Dict[str, Any], 
                                sensory_context: Dict[str, Any]) -> Optional[SensorimotorContingency]:
        """Find existing contingency similar to the given pattern"""
        for contingency in self.contingencies:
            similarity = self._calculate_similarity(
                action_pattern, contingency.action_pattern,
                sensory_context, contingency.sensory_context
            )
            if similarity > 0.8:  # High similarity threshold for matching
                return contingency
        return None
    
    def _update_contingency(self, contingency: SensorimotorContingency, actual_outcome: Dict[str, Any]):
        """Update an existing contingency with new outcome data"""
        # Update expected outcome with running average
        for key, value in actual_outcome.items():
            if key in contingency.expected_outcome:
                if isinstance(value, (int, float)) and isinstance(contingency.expected_outcome[key], (int, float)):
                    # Running average update
                    old_value = contingency.expected_outcome[key]
                    contingency.expected_outcome[key] = (old_value * contingency.frequency + value) / (contingency.frequency + 1)
                else:
                    contingency.expected_outcome[key] = value
            else:
                contingency.expected_outcome[key] = value
        
        # Update confidence based on prediction accuracy
        accuracy = self._calculate_prediction_accuracy(contingency.expected_outcome, actual_outcome)
        contingency.confidence = min(1.0, contingency.confidence + self.learning_rate * (accuracy - 0.5))
        contingency.frequency += 1
        contingency.last_updated = time.time()
    
    def _calculate_similarity(self, pattern1: Dict[str, Any], pattern2: Dict[str, Any],
                            context1: Dict[str, Any], context2: Dict[str, Any]) -> float:
        """Calculate similarity between two action patterns and sensory contexts"""
        try:
            action_similarity = self._dict_similarity(pattern1, pattern2)
            context_similarity = self._dict_similarity(context1, context2)
            return (action_similarity + context_similarity) / 2.0
        except:
            return 0.0
    
    def _dict_similarity(self, dict1: Dict[str, Any], dict2: Dict[str, Any]) -> float:
        """Calculate similarity between two dictionaries"""
        if not dict1 or not dict2:
            return 0.0
        
        common_keys = set(dict1.keys()) & set(dict2.keys())
        if not common_keys:
            return 0.0
        
        similarities = []
        for key in common_keys:
            val1, val2 = dict1[key], dict2[key]
            if isinstance(val1, (int, float)) and isinstance(val2, (int, float)):
                if val1 == 0 and val2 == 0:
                    similarities.append(1.0)
                else:
                    diff = abs(val1 - val2) / (abs(val1) + abs(val2) + 1e-6)
                    similarities.append(max(0.0, 1.0 - diff))
            elif val1 == val2:
                similarities.append(1.0)
            else:
                similarities.append(0.0)
        
        return sum(similarities) / len(similarities) if similarities else 0.0
    
    def _calculate_prediction_accuracy(self, predicted: Dict[str, Any], actual: Dict[str, Any]) -> float:
        """Calculate accuracy of a prediction"""
        return self._dict_similarity(predicted, actual)


class ActionBasedPerceptionModule:
    """Implements active perception through exploratory actions"""
    
    def __init__(self, exploration_rate: float = 0.2):
        self.exploration_rate = exploration_rate
        self.attention_weights = {}  # What aspects of perception to focus on
        self.perceptual_expectations = {}  # What we expect to perceive
        self.exploration_actions = deque(maxlen=50)  # Recent exploratory actions
    
    def generate_exploratory_action(self, current_body_state: BodyState, 
                                  goal_context: Optional[Dict[str, Any]] = None) -> MotorAction:
        """Generate an action designed to gather perceptual information"""
        try:
            # Start with current joint positions
            current_joints = current_body_state.joint_angles.copy()
            
            # Add exploratory perturbations
            exploratory_targets = {}
            for joint_name in ['shoulder', 'elbow', 'wrist']:  # Standard joints
                current_angle = current_joints.get(joint_name, 0.0)
                
                # Add small random exploration
                import random
                exploration_magnitude = random.uniform(0.1, 0.3) * self.exploration_rate
                exploration_direction = random.choice([-1, 1])
                new_target = current_angle + exploration_magnitude * exploration_direction
                
                # Keep within reasonable bounds
                new_target = max(-3.14159, min(3.14159, new_target))
                exploratory_targets[joint_name] = new_target
            
            # Create exploratory action
            import random
            action = MotorAction(
                joint_targets=exploratory_targets,
                muscle_commands={'primary': 0.6, 'secondary': 0.3},
                duration=random.uniform(0.5, 1.5),
                force=random.uniform(0.3, 0.8),
                precision=random.uniform(0.5, 0.9)
            )
            
            self.exploration_actions.append(action)
            logger.debug(f"Generated exploratory action with {len(exploratory_targets)} joint targets")
            return action
            
        except Exception as e:
            logger.error(f"Error generating exploratory action: {e}")
            # Return safe default action
            return MotorAction(
                joint_targets={'shoulder': 0.1},
                muscle_commands={'primary': 0.5},
                duration=1.0
            )
    
    def update_attention_weights(self, sensory_surprise: Dict[str, float]):
        """Update attention weights based on sensory surprise"""
        for modality, surprise in sensory_surprise.items():
            current_weight = self.attention_weights.get(modality, 0.5)
            # Increase attention to surprising modalities
            new_weight = min(1.0, current_weight + 0.1 * surprise)
            self.attention_weights[modality] = new_weight
    
    def focus_perception(self, sensory_input: Dict[str, Any]) -> Dict[str, Any]:
        """Apply attention weights to focus perception"""
        focused_input = {}
        for modality, data in sensory_input.items():
            weight = self.attention_weights.get(modality, 0.5)
            if isinstance(data, (int, float)):
                focused_input[modality] = data * weight
            else:
                focused_input[modality] = data  # Keep non-numeric data as is
        return focused_input


class EnactivePerceptionSystem:
    """
    Main enactive perception system that integrates action-based perception,
    sensorimotor contingency learning, and perceptual prediction.
    """
    
    def __init__(self, agent_name: str = "enactive_agent"):
        self.agent_name = agent_name
        self.contingency_learner = SensorimotorContingencyLearner()
        self.action_perception_module = ActionBasedPerceptionModule()
        
        # Perceptual state tracking
        self.current_perceptual_state = {}
        self.perceptual_history = deque(maxlen=100)
        self.prediction_accuracy_history = deque(maxlen=50)
        
        # Integration with existing systems
        self.aar_agent = None
        if AAR_SYSTEM_AVAILABLE:
            try:
                self.aar_agent = Agent(f"{agent_name}_enactive", capabilities=['perceive', 'predict', 'explore'])
                logger.info("Integrated with AAR system")
            except Exception as e:
                logger.warning(f"Could not integrate with AAR system: {e}")
        
        logger.info(f"Enactive Perception System initialized for {agent_name}")
    
    def process_embodied_experience(self, experience: SensorimotorExperience) -> Dict[str, Any]:
        """Process an embodied experience to update enactive perception"""
        try:
            # Learn sensorimotor contingency
            contingency_learned = self.contingency_learner.learn_contingency(experience)
            
            # Update perceptual state
            self._update_perceptual_state(experience.sensory_feedback)
            
            # Calculate sensory surprise
            surprise = self._calculate_sensory_surprise(experience.sensory_feedback)
            
            # Update attention based on surprise
            self.action_perception_module.update_attention_weights(surprise)
            
            # Update AAR agent if available
            if self.aar_agent:
                self.aar_agent.process({
                    'sensory_feedback': experience.sensory_feedback,
                    'motor_action': experience.motor_action,
                    'success': experience.success,
                    'surprise': surprise
                })
            
            result = {
                'contingency_learned': contingency_learned,
                'sensory_surprise': surprise,
                'attention_updated': True,
                'perceptual_state_updated': True,
                'total_contingencies': len(self.contingency_learner.contingencies)
            }
            
            logger.debug(f"Processed embodied experience: {result}")
            return result
            
        except Exception as e:
            logger.error(f"Error processing embodied experience: {e}")
            return {'error': str(e)}
    
    def predict_perceptual_outcome(self, planned_action: MotorAction, 
                                 current_body_state: BodyState) -> PerceptualPrediction:
        """Predict perceptual outcome of a planned action"""
        try:
            # Get sensory prediction from contingency learner
            predicted_outcome = self.contingency_learner.predict_sensory_outcome(
                planned_action, 
                current_body_state.sensory_state
            )
            
            # Calculate exploration value
            exploration_value = self._calculate_exploration_value(planned_action)
            
            # Estimate confidence based on contingency history
            confidence = self._estimate_prediction_confidence(planned_action, current_body_state)
            
            prediction = PerceptualPrediction(
                action_plan=planned_action,
                predicted_sensory_outcome=predicted_outcome,
                confidence=confidence,
                exploration_value=exploration_value
            )
            
            logger.debug(f"Generated perceptual prediction with confidence {confidence:.3f}")
            return prediction
            
        except Exception as e:
            logger.error(f"Error predicting perceptual outcome: {e}")
            return PerceptualPrediction(
                action_plan=planned_action,
                predicted_sensory_outcome={},
                confidence=0.0
            )
    
    def generate_perceptual_action(self, current_body_state: BodyState, 
                                 perceptual_goal: Optional[Dict[str, Any]] = None) -> MotorAction:
        """Generate an action specifically designed to gather perceptual information"""
        return self.action_perception_module.generate_exploratory_action(current_body_state, perceptual_goal)
    
    def get_enactive_metrics(self) -> Dict[str, Any]:
        """Get metrics about enactive perception performance"""
        return {
            'total_contingencies_learned': len(self.contingency_learner.contingencies),
            'average_contingency_confidence': np.mean([c.confidence for c in self.contingency_learner.contingencies]) if self.contingency_learner.contingencies else 0.0,
            'exploration_actions_taken': len(self.action_perception_module.exploration_actions),
            'attention_weights': self.action_perception_module.attention_weights.copy(),
            'recent_prediction_accuracy': np.mean(list(self.prediction_accuracy_history)) if self.prediction_accuracy_history else 0.0,
            'perceptual_history_length': len(self.perceptual_history),
            'system_active': True
        }
    
    def _update_perceptual_state(self, sensory_feedback: Dict[str, Any]):
        """Update internal perceptual state"""
        # Apply attention weighting
        focused_perception = self.action_perception_module.focus_perception(sensory_feedback)
        
        # Update current state
        self.current_perceptual_state.update(focused_perception)
        self.current_perceptual_state['timestamp'] = time.time()
        
        # Add to history
        self.perceptual_history.append(focused_perception.copy())
    
    def _calculate_sensory_surprise(self, sensory_feedback: Dict[str, Any]) -> Dict[str, float]:
        """Calculate surprise in sensory feedback"""
        surprise = {}
        
        for modality, value in sensory_feedback.items():
            if isinstance(value, (int, float)):
                # Compare with recent history
                recent_values = [h.get(modality, 0) for h in list(self.perceptual_history)[-10:] if isinstance(h.get(modality), (int, float))]
                if recent_values:
                    expected = np.mean(recent_values)
                    surprise[modality] = min(1.0, abs(value - expected) / (abs(expected) + 1e-6))
                else:
                    surprise[modality] = 0.5  # Moderate surprise for new modalities
            else:
                # For non-numeric data, check if it's different from recent
                recent_values = [h.get(modality) for h in list(self.perceptual_history)[-5:]]
                if recent_values and all(v == recent_values[0] for v in recent_values):
                    surprise[modality] = 1.0 if value != recent_values[0] else 0.0
                else:
                    surprise[modality] = 0.3  # Moderate surprise for varied data
        
        return surprise
    
    def _calculate_exploration_value(self, action: MotorAction) -> float:
        """Calculate how much an action would explore unknown perceptual space"""
        # Simple heuristic: actions that are different from recent ones have higher exploration value
        if not self.action_perception_module.exploration_actions:
            return 1.0
        
        similarities = []
        for past_action in list(self.action_perception_module.exploration_actions)[-10:]:
            similarity = self.contingency_learner._dict_similarity(
                action.joint_targets, past_action.joint_targets
            )
            similarities.append(similarity)
        
        avg_similarity = np.mean(similarities) if similarities else 0.5
        return max(0.0, 1.0 - avg_similarity)  # Lower similarity = higher exploration value
    
    def _estimate_prediction_confidence(self, action: MotorAction, body_state: BodyState) -> float:
        """Estimate confidence in prediction based on past experience"""
        # Find similar past experiences
        similar_contingencies = [
            c for c in self.contingency_learner.contingencies
            if self.contingency_learner._dict_similarity(
                action.joint_targets, c.action_pattern.get('joint_targets', {})
            ) > 0.7
        ]
        
        if similar_contingencies:
            return np.mean([c.confidence for c in similar_contingencies])
        else:
            return 0.1  # Low confidence for novel actions


def create_enactive_perception_system(agent_name: str = "default_agent") -> EnactivePerceptionSystem:
    """Factory function to create an enactive perception system"""
    return EnactivePerceptionSystem(agent_name)


# Integration function for existing systems
def integrate_with_embodied_learning(enactive_system: EnactivePerceptionSystem, 
                                   embodied_learning_system) -> bool:
    """Integrate enactive perception with existing embodied learning system"""
    try:
        # This would be called by the main embodied learning system
        # to add enactive perception capabilities
        if hasattr(embodied_learning_system, 'sensory_motor'):
            # Connect to sensory-motor system
            embodied_learning_system.enactive_perception = enactive_system
            logger.info("Successfully integrated enactive perception with embodied learning")
            return True
        else:
            logger.warning("Embodied learning system missing sensory_motor component")
            return False
    except Exception as e:
        logger.error(f"Failed to integrate enactive perception: {e}")
        return False


if __name__ == "__main__":
    # Demo/test the enactive perception system
    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
    
    print("=== Enactive Perception System Demo ===")
    
    # Create system
    system = create_enactive_perception_system("demo_agent")
    
    # Create test experience
    initial_state = BodyState(
        position=(0.0, 0.0, 1.0),
        joint_angles={'shoulder': 0.5, 'elbow': 0.3},
        sensory_state={'vision': 0.8, 'touch': 0.2, 'proprioception': 0.6}
    )
    
    action = MotorAction(
        joint_targets={'shoulder': 0.7, 'elbow': 0.4},
        muscle_commands={'primary': 0.8, 'secondary': 0.5},
        duration=1.0
    )
    
    resulting_state = BodyState(
        position=(0.1, 0.0, 1.0),
        joint_angles={'shoulder': 0.7, 'elbow': 0.4},
        sensory_state={'vision': 0.9, 'touch': 0.3, 'proprioception': 0.8}
    )
    
    experience = SensorimotorExperience(
        initial_body_state=initial_state,
        motor_action=action,
        resulting_body_state=resulting_state,
        sensory_feedback={'vision': 0.9, 'touch': 0.3, 'proprioception': 0.8, 'reward': 1.0},
        success=True,
        reward=1.0
    )
    
    # Process experience
    print("Processing embodied experience...")
    result = system.process_embodied_experience(experience)
    print(f"Processing result: {result}")
    
    # Generate prediction
    print("\nGenerating perceptual prediction...")
    prediction = system.predict_perceptual_outcome(action, initial_state)
    print(f"Prediction confidence: {prediction.confidence:.3f}")
    print(f"Predicted outcome: {prediction.predicted_sensory_outcome}")
    
    # Generate exploratory action
    print("\nGenerating exploratory action...")
    exploratory_action = system.generate_perceptual_action(initial_state)
    print(f"Exploratory action: {exploratory_action.joint_targets}")
    
    # Get metrics
    print("\nSystem metrics:")
    metrics = system.get_enactive_metrics()
    for key, value in metrics.items():
        print(f"  {key}: {value}")
    
    print("\n=== Demo completed successfully ===")