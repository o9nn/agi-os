try:
    import numpy as np
    NUMPY_AVAILABLE = True
except ImportError:
    NUMPY_AVAILABLE = False
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
try:
    import sys
    from pathlib import Path
    sys.path.append(str(Path(__file__).parent.parent / 'echo.dash'))
    from embodied_learning import BodyState, MotorAction, SensorimotorExperience
    EMBODIED_LEARNING_AVAILABLE = True
except ImportError:
    EMBODIED_LEARNING_AVAILABLE = False
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
    sys.path.append(str(Path(__file__).parent.parent / 'echo.dream'))
    from aar_system import Agent, Arena, AARComponent
    AAR_SYSTEM_AVAILABLE = True
except ImportError:
    AAR_SYSTEM_AVAILABLE = False
logger = logging.getLogger(__name__)
@dataclass
class SensorimotorContingency:
    action_pattern: Dict[str, Any]
    sensory_context: Dict[str, Any]
    expected_outcome: Dict[str, Any]
    actual_outcome: Dict[str, Any]
    confidence: float = 0.5
    frequency: int = 1
    last_updated: float = field(default_factory=time.time)
@dataclass
class PerceptualPrediction:
    action_plan: MotorAction
    predicted_sensory_outcome: Dict[str, Any]
    confidence: float = 0.5
    exploration_value: float = 0.0
    prediction_timestamp: float = field(default_factory=time.time)
class SensorimotorContingencyLearner:
    def __init__(self, max_contingencies: int=1000, learning_rate: float=0.1):
        self.max_contingencies = max_contingencies
        self.learning_rate = learning_rate
        self.contingencies: List[SensorimotorContingency] = []
        self.action_history = deque(maxlen=100)
        self.sensory_history = deque(maxlen=100)
    def learn_contingency(self, experience: SensorimotorExperience) -> bool:
        try:
            action_pattern = {'joint_targets': experience.motor_action.joint_targets.copy(), 'muscle_commands': experience.motor_action.muscle_commands.copy(), 'force': experience.motor_action.force, 'duration': experience.motor_action.duration}
            sensory_context = experience.initial_body_state.sensory_state.copy()
            actual_outcome = experience.sensory_feedback.copy()
            similar_contingency = self._find_similar_contingency(action_pattern, sensory_context)
            if similar_contingency:
                self._update_contingency(similar_contingency, actual_outcome)
                logger.debug(f'Updated existing contingency, confidence: {similar_contingency.confidence:.3f}')
            else:
                new_contingency = SensorimotorContingency(action_pattern=action_pattern, sensory_context=sensory_context, expected_outcome=actual_outcome.copy(), actual_outcome=actual_outcome, confidence=0.1, frequency=1)
                self.contingencies.append(new_contingency)
                logger.debug(f'Learned new contingency: {len(self.contingencies)} total')
            if len(self.contingencies) > self.max_contingencies:
                self.contingencies.sort(key=lambda c: c.confidence)
                self.contingencies = self.contingencies[-self.max_contingencies:]
            return True
        except Exception as e:
            logger.error(f'Error learning contingency: {e}')
            return False
    def predict_sensory_outcome(self, action: MotorAction, current_sensory_state: Dict[str, Any]) -> Dict[str, Any]:
        try:
            action_pattern = {'joint_targets': action.joint_targets.copy(), 'muscle_commands': action.muscle_commands.copy(), 'force': action.force, 'duration': action.duration}
            best_match = None
            best_similarity = 0.0
            for contingency in self.contingencies:
                similarity = self._calculate_similarity(action_pattern, contingency.action_pattern, current_sensory_state, contingency.sensory_context)
                if similarity > best_similarity and contingency.confidence > 0.3:
                    best_similarity = similarity
                    best_match = contingency
            if best_match and best_similarity > 0.5:
                confidence_factor = best_match.confidence * best_similarity
                predicted_outcome = {}
                for key, value in best_match.expected_outcome.items():
                    if isinstance(value, (int, float)):
                        predicted_outcome[key] = value * confidence_factor
                    else:
                        predicted_outcome[key] = value
                return predicted_outcome
            return {}
        except Exception as e:
            logger.error(f'Error predicting sensory outcome: {e}')
            return {}
    def _find_similar_contingency(self, action_pattern: Dict[str, Any], sensory_context: Dict[str, Any]) -> Optional[SensorimotorContingency]:
        for contingency in self.contingencies:
            similarity = self._calculate_similarity(action_pattern, contingency.action_pattern, sensory_context, contingency.sensory_context)
            if similarity > 0.8:
                return contingency
        return None
    def _update_contingency(self, contingency: SensorimotorContingency, actual_outcome: Dict[str, Any]):
        for key, value in actual_outcome.items():
            if key in contingency.expected_outcome:
                if isinstance(value, (int, float)) and isinstance(contingency.expected_outcome[key], (int, float)):
                    old_value = contingency.expected_outcome[key]
                    contingency.expected_outcome[key] = (old_value * contingency.frequency + value) / (contingency.frequency + 1)
                else:
                    contingency.expected_outcome[key] = value
            else:
                contingency.expected_outcome[key] = value
        accuracy = self._calculate_prediction_accuracy(contingency.expected_outcome, actual_outcome)
        contingency.confidence = min(1.0, contingency.confidence + self.learning_rate * (accuracy - 0.5))
        contingency.frequency += 1
        contingency.last_updated = time.time()
    def _calculate_similarity(self, pattern1: Dict[str, Any], pattern2: Dict[str, Any], context1: Dict[str, Any], context2: Dict[str, Any]) -> float:
        try:
            action_similarity = self._dict_similarity(pattern1, pattern2)
            context_similarity = self._dict_similarity(context1, context2)
            return (action_similarity + context_similarity) / 2.0
        except:
            return 0.0
    def _dict_similarity(self, dict1: Dict[str, Any], dict2: Dict[str, Any]) -> float:
        if not dict1 or not dict2:
            return 0.0
        common_keys = set(dict1.keys()) & set(dict2.keys())
        if not common_keys:
            return 0.0
        similarities = []
        for key in common_keys:
            val1, val2 = (dict1[key], dict2[key])
            if isinstance(val1, (int, float)) and isinstance(val2, (int, float)):
                if val1 == 0 and val2 == 0:
                    similarities.append(1.0)
                else:
                    diff = abs(val1 - val2) / (abs(val1) + abs(val2) + 1e-06)
                    similarities.append(max(0.0, 1.0 - diff))
            elif val1 == val2:
                similarities.append(1.0)
            else:
                similarities.append(0.0)
        return sum(similarities) / len(similarities) if similarities else 0.0
    def _calculate_prediction_accuracy(self, predicted: Dict[str, Any], actual: Dict[str, Any]) -> float:
        return self._dict_similarity(predicted, actual)
class ActionBasedPerceptionModule:
    def __init__(self, exploration_rate: float=0.2):
        self.exploration_rate = exploration_rate
        self.attention_weights = {}
        self.perceptual_expectations = {}
        self.exploration_actions = deque(maxlen=50)
    def generate_exploratory_action(self, current_body_state: BodyState, goal_context: Optional[Dict[str, Any]]=None) -> MotorAction:
        try:
            current_joints = current_body_state.joint_angles.copy()
            exploratory_targets = {}
            for joint_name in ['shoulder', 'elbow', 'wrist']:
                current_angle = current_joints.get(joint_name, 0.0)
                import random
                exploration_magnitude = random.uniform(0.1, 0.3) * self.exploration_rate
                exploration_direction = random.choice([-1, 1])
                new_target = current_angle + exploration_magnitude * exploration_direction
                new_target = max(-3.14159, min(3.14159, new_target))
                exploratory_targets[joint_name] = new_target
            import random
            action = MotorAction(joint_targets=exploratory_targets, muscle_commands={'primary': 0.6, 'secondary': 0.3}, duration=random.uniform(0.5, 1.5), force=random.uniform(0.3, 0.8), precision=random.uniform(0.5, 0.9))
            self.exploration_actions.append(action)
            logger.debug(f'Generated exploratory action with {len(exploratory_targets)} joint targets')
            return action
        except Exception as e:
            logger.error(f'Error generating exploratory action: {e}')
            return MotorAction(joint_targets={'shoulder': 0.1}, muscle_commands={'primary': 0.5}, duration=1.0)
    def update_attention_weights(self, sensory_surprise: Dict[str, float]):
        for modality, surprise in sensory_surprise.items():
            current_weight = self.attention_weights.get(modality, 0.5)
            new_weight = min(1.0, current_weight + 0.1 * surprise)
            self.attention_weights[modality] = new_weight
    def focus_perception(self, sensory_input: Dict[str, Any]) -> Dict[str, Any]:
        focused_input = {}
        for modality, data in sensory_input.items():
            weight = self.attention_weights.get(modality, 0.5)
            if isinstance(data, (int, float)):
                focused_input[modality] = data * weight
            else:
                focused_input[modality] = data
        return focused_input
class EnactivePerceptionSystem:
    def __init__(self, agent_name: str='enactive_agent'):
        self.agent_name = agent_name
        self.contingency_learner = SensorimotorContingencyLearner()
        self.action_perception_module = ActionBasedPerceptionModule()
        self.current_perceptual_state = {}
        self.perceptual_history = deque(maxlen=100)
        self.prediction_accuracy_history = deque(maxlen=50)
        self.aar_agent = None
        if AAR_SYSTEM_AVAILABLE:
            try:
                self.aar_agent = Agent(f'{agent_name}_enactive', capabilities=['perceive', 'predict', 'explore'])
                logger.info('Integrated with AAR system')
            except Exception as e:
                logger.warning(f'Could not integrate with AAR system: {e}')
        logger.info(f'Enactive Perception System initialized for {agent_name}')
    def process_embodied_experience(self, experience: SensorimotorExperience) -> Dict[str, Any]:
        try:
            contingency_learned = self.contingency_learner.learn_contingency(experience)
            self._update_perceptual_state(experience.sensory_feedback)
            surprise = self._calculate_sensory_surprise(experience.sensory_feedback)
            self.action_perception_module.update_attention_weights(surprise)
            if self.aar_agent:
                self.aar_agent.process({'sensory_feedback': experience.sensory_feedback, 'motor_action': experience.motor_action, 'success': experience.success, 'surprise': surprise})
            result = {'contingency_learned': contingency_learned, 'sensory_surprise': surprise, 'attention_updated': True, 'perceptual_state_updated': True, 'total_contingencies': len(self.contingency_learner.contingencies)}
            logger.debug(f'Processed embodied experience: {result}')
            return result
        except Exception as e:
            logger.error(f'Error processing embodied experience: {e}')
            return {'error': str(e)}
    def predict_perceptual_outcome(self, planned_action: MotorAction, current_body_state: BodyState) -> PerceptualPrediction:
        try:
            predicted_outcome = self.contingency_learner.predict_sensory_outcome(planned_action, current_body_state.sensory_state)
            exploration_value = self._calculate_exploration_value(planned_action)
            confidence = self._estimate_prediction_confidence(planned_action, current_body_state)
            prediction = PerceptualPrediction(action_plan=planned_action, predicted_sensory_outcome=predicted_outcome, confidence=confidence, exploration_value=exploration_value)
            logger.debug(f'Generated perceptual prediction with confidence {confidence:.3f}')
            return prediction
        except Exception as e:
            logger.error(f'Error predicting perceptual outcome: {e}')
            return PerceptualPrediction(action_plan=planned_action, predicted_sensory_outcome={}, confidence=0.0)
    def generate_perceptual_action(self, current_body_state: BodyState, perceptual_goal: Optional[Dict[str, Any]]=None) -> MotorAction:
        return self.action_perception_module.generate_exploratory_action(current_body_state, perceptual_goal)
    def get_enactive_metrics(self) -> Dict[str, Any]:
        return {'total_contingencies_learned': len(self.contingency_learner.contingencies), 'average_contingency_confidence': np.mean([c.confidence for c in self.contingency_learner.contingencies]) if self.contingency_learner.contingencies else 0.0, 'exploration_actions_taken': len(self.action_perception_module.exploration_actions), 'attention_weights': self.action_perception_module.attention_weights.copy(), 'recent_prediction_accuracy': np.mean(list(self.prediction_accuracy_history)) if self.prediction_accuracy_history else 0.0, 'perceptual_history_length': len(self.perceptual_history), 'system_active': True}
    def _update_perceptual_state(self, sensory_feedback: Dict[str, Any]):
        focused_perception = self.action_perception_module.focus_perception(sensory_feedback)
        self.current_perceptual_state.update(focused_perception)
        self.current_perceptual_state['timestamp'] = time.time()
        self.perceptual_history.append(focused_perception.copy())
    def _calculate_sensory_surprise(self, sensory_feedback: Dict[str, Any]) -> Dict[str, float]:
        surprise = {}
        for modality, value in sensory_feedback.items():
            if isinstance(value, (int, float)):
                recent_values = [h.get(modality, 0) for h in list(self.perceptual_history)[-10:] if isinstance(h.get(modality), (int, float))]
                if recent_values:
                    expected = np.mean(recent_values)
                    surprise[modality] = min(1.0, abs(value - expected) / (abs(expected) + 1e-06))
                else:
                    surprise[modality] = 0.5
            else:
                recent_values = [h.get(modality) for h in list(self.perceptual_history)[-5:]]
                if recent_values and all((v == recent_values[0] for v in recent_values)):
                    surprise[modality] = 1.0 if value != recent_values[0] else 0.0
                else:
                    surprise[modality] = 0.3
        return surprise
    def _calculate_exploration_value(self, action: MotorAction) -> float:
        if not self.action_perception_module.exploration_actions:
            return 1.0
        similarities = []
        for past_action in list(self.action_perception_module.exploration_actions)[-10:]:
            similarity = self.contingency_learner._dict_similarity(action.joint_targets, past_action.joint_targets)
            similarities.append(similarity)
        avg_similarity = np.mean(similarities) if similarities else 0.5
        return max(0.0, 1.0 - avg_similarity)
    def _estimate_prediction_confidence(self, action: MotorAction, body_state: BodyState) -> float:
        similar_contingencies = [c for c in self.contingency_learner.contingencies if self.contingency_learner._dict_similarity(action.joint_targets, c.action_pattern.get('joint_targets', {})) > 0.7]
        if similar_contingencies:
            return np.mean([c.confidence for c in similar_contingencies])
        else:
            return 0.1
def create_enactive_perception_system(agent_name: str='default_agent') -> EnactivePerceptionSystem:
    return EnactivePerceptionSystem(agent_name)
def integrate_with_embodied_learning(enactive_system: EnactivePerceptionSystem, embodied_learning_system) -> bool:
    try:
        if hasattr(embodied_learning_system, 'sensory_motor'):
            embodied_learning_system.enactive_perception = enactive_system
            logger.info('Successfully integrated enactive perception with embodied learning')
            return True
        else:
            logger.warning('Embodied learning system missing sensory_motor component')
            return False
    except Exception as e:
        logger.error(f'Failed to integrate enactive perception: {e}')
        return False
if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
    print('=== Enactive Perception System Demo ===')
    system = create_enactive_perception_system('demo_agent')
    initial_state = BodyState(position=(0.0, 0.0, 1.0), joint_angles={'shoulder': 0.5, 'elbow': 0.3}, sensory_state={'vision': 0.8, 'touch': 0.2, 'proprioception': 0.6})
    action = MotorAction(joint_targets={'shoulder': 0.7, 'elbow': 0.4}, muscle_commands={'primary': 0.8, 'secondary': 0.5}, duration=1.0)
    resulting_state = BodyState(position=(0.1, 0.0, 1.0), joint_angles={'shoulder': 0.7, 'elbow': 0.4}, sensory_state={'vision': 0.9, 'touch': 0.3, 'proprioception': 0.8})
    experience = SensorimotorExperience(initial_body_state=initial_state, motor_action=action, resulting_body_state=resulting_state, sensory_feedback={'vision': 0.9, 'touch': 0.3, 'proprioception': 0.8, 'reward': 1.0}, success=True, reward=1.0)
    print('Processing embodied experience...')
    result = system.process_embodied_experience(experience)
    print(f'Processing result: {result}')
    print('\nGenerating perceptual prediction...')
    prediction = system.predict_perceptual_outcome(action, initial_state)
    print(f'Prediction confidence: {prediction.confidence:.3f}')
    print(f'Predicted outcome: {prediction.predicted_sensory_outcome}')
    print('\nGenerating exploratory action...')
    exploratory_action = system.generate_perceptual_action(initial_state)
    print(f'Exploratory action: {exploratory_action.joint_targets}')
    print('\nSystem metrics:')
    metrics = system.get_enactive_metrics()
    for key, value in metrics.items():
        print(f'  {key}: {value}')
    print('\n=== Demo completed successfully ===')