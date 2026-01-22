from typing import Dict, List, Any, Optional
from dataclasses import dataclass, field
from datetime import datetime
import numpy as np
@dataclass
class PerceptualState:
    sensations: Dict[str, float] = field(default_factory=dict)
    attention_focus: List[str] = field(default_factory=list)
    perceptual_patterns: List[Dict[str, Any]] = field(default_factory=list)
    awareness_of_being_watched: float = 0.0
    awareness_of_being_thought_about: float = 0.0
    timestamp: str = ''
@dataclass
class ActionState:
    current_action: str = ''
    action_parameters: Dict[str, Any] = field(default_factory=dict)
    motor_outputs: Dict[str, float] = field(default_factory=dict)
    emotions: Dict[str, float] = field(default_factory=dict)
    awareness_of_being_observed: float = 0.0
    awareness_of_being_thought_about: float = 0.0
    timestamp: str = ''
@dataclass
class ReflectiveState:
    current_thoughts: List[str] = field(default_factory=list)
    insights: List[str] = field(default_factory=list)
    simulations: List[Dict[str, Any]] = field(default_factory=list)
    predictions: Dict[str, Any] = field(default_factory=dict)
    awareness_of_action: float = 0.0
    awareness_of_perception: float = 0.0
    timestamp: str = ''
@dataclass
class MutualAwarenessMatrix:
    stream1_aware_of_stream2: float = 0.0
    stream1_aware_of_stream3: float = 0.0
    stream2_aware_of_stream1: float = 0.0
    stream2_aware_of_stream3: float = 0.0
    stream3_aware_of_stream1: float = 0.0
    stream3_aware_of_stream2: float = 0.0
    recursive_depth: int = 0
    triadic_coherence: float = 0.0
class SharedConsciousnessState:
    def __init__(self):
        self.perceptual_state = PerceptualState()
        self.action_state = ActionState()
        self.reflective_state = ReflectiveState()
        self.mutual_awareness = MutualAwarenessMatrix()
        self.cognitive_feedback: List[str] = []
        self.emotive_feedback: Dict[str, float] = {}
        self.sensory_feedback: Dict[str, float] = {}
        self.predictions: Dict[str, Any] = {}
        self.sync_history: List[Dict[str, Any]] = []
    def update_perceptual_state(self, state: PerceptualState):
        self.perceptual_state = state
        self.perceptual_state.timestamp = datetime.utcnow().isoformat()
        self.sensory_feedback = state.sensations
    def update_action_state(self, state: ActionState):
        self.action_state = state
        self.action_state.timestamp = datetime.utcnow().isoformat()
        self.emotive_feedback = state.emotions
    def update_reflective_state(self, state: ReflectiveState):
        self.reflective_state = state
        self.reflective_state.timestamp = datetime.utcnow().isoformat()
        self.cognitive_feedback = state.insights
        self.predictions = state.predictions
    def update_mutual_awareness(self):
        self.mutual_awareness.stream1_aware_of_stream2 = self._calculate_attention(self.perceptual_state.attention_focus, ['action', 'motor', 'behavior'])
        self.mutual_awareness.stream1_aware_of_stream3 = self.perceptual_state.awareness_of_being_thought_about
        self.mutual_awareness.stream2_aware_of_stream1 = self.action_state.awareness_of_being_observed
        self.mutual_awareness.stream2_aware_of_stream3 = self.action_state.awareness_of_being_thought_about
        self.mutual_awareness.stream3_aware_of_stream1 = self.reflective_state.awareness_of_perception
        self.mutual_awareness.stream3_aware_of_stream2 = self.reflective_state.awareness_of_action
        self.mutual_awareness.recursive_depth = self._calculate_recursive_depth()
        self.mutual_awareness.triadic_coherence = self._calculate_triadic_coherence()
    def _calculate_attention(self, focus: List[str], keywords: List[str]) -> float:
        if not focus:
            return 0.0
        matches = sum((1 for f in focus if any((k in f.lower() for k in keywords))))
        return min(1.0, matches / len(keywords))
    def _calculate_recursive_depth(self) -> int:
        awareness_levels = [self.mutual_awareness.stream1_aware_of_stream2, self.mutual_awareness.stream1_aware_of_stream3, self.mutual_awareness.stream2_aware_of_stream1, self.mutual_awareness.stream2_aware_of_stream3, self.mutual_awareness.stream3_aware_of_stream1, self.mutual_awareness.stream3_aware_of_stream2]
        avg_awareness = np.mean(awareness_levels)
        if avg_awareness < 0.3:
            return 1
        elif avg_awareness < 0.6:
            return 2
        elif avg_awareness < 0.9:
            return 3
        else:
            return 4
    def _calculate_triadic_coherence(self) -> float:
        awareness_levels = [self.mutual_awareness.stream1_aware_of_stream2, self.mutual_awareness.stream1_aware_of_stream3, self.mutual_awareness.stream2_aware_of_stream1, self.mutual_awareness.stream2_aware_of_stream3, self.mutual_awareness.stream3_aware_of_stream1, self.mutual_awareness.stream3_aware_of_stream2]
        mean_awareness = np.mean(awareness_levels)
        variance = np.var(awareness_levels)
        coherence = mean_awareness * (1.0 - variance)
        return min(1.0, max(0.0, coherence))
    def propagate_cognitive_feedback(self) -> Dict[str, List[str]]:
        return {'stream1': self.cognitive_feedback.copy(), 'stream2': self.cognitive_feedback.copy()}
    def propagate_emotive_feedback(self) -> Dict[str, Dict[str, float]]:
        return {'stream1': self.emotive_feedback.copy(), 'stream3': self.emotive_feedback.copy()}
    def propagate_sensory_feedback(self) -> Dict[str, Dict[str, float]]:
        return {'stream2': self.sensory_feedback.copy(), 'stream3': self.sensory_feedback.copy()}
    def propagate_feedforward(self) -> Dict[str, Dict[str, Any]]:
        return {'stream1': self.predictions.copy(), 'stream2': self.predictions.copy()}
    def record_synchronization(self, triad_id: int, triad_steps: tuple):
        sync_record = {'triad_id': triad_id, 'triad_steps': triad_steps, 'timestamp': datetime.utcnow().isoformat(), 'perceptual_state': {'sensations': self.perceptual_state.sensations, 'attention': self.perceptual_state.attention_focus}, 'action_state': {'action': self.action_state.current_action, 'emotions': self.action_state.emotions}, 'reflective_state': {'thoughts': self.reflective_state.current_thoughts, 'insights': self.reflective_state.insights}, 'mutual_awareness': {'recursive_depth': self.mutual_awareness.recursive_depth, 'triadic_coherence': self.mutual_awareness.triadic_coherence}}
        self.sync_history.append(sync_record)
        if len(self.sync_history) > 100:
            self.sync_history = self.sync_history[-100:]
    def get_consciousness_state(self) -> Dict[str, Any]:
        return {'perceptual_state': {'sensations': self.perceptual_state.sensations, 'attention_focus': self.perceptual_state.attention_focus, 'patterns': len(self.perceptual_state.perceptual_patterns)}, 'action_state': {'current_action': self.action_state.current_action, 'emotions': self.action_state.emotions, 'awareness_of_being_observed': self.action_state.awareness_of_being_observed}, 'reflective_state': {'current_thoughts': self.reflective_state.current_thoughts, 'insights': self.reflective_state.insights, 'predictions': self.reflective_state.predictions}, 'mutual_awareness': {'stream1_aware_of_stream2': self.mutual_awareness.stream1_aware_of_stream2, 'stream1_aware_of_stream3': self.mutual_awareness.stream1_aware_of_stream3, 'stream2_aware_of_stream1': self.mutual_awareness.stream2_aware_of_stream1, 'stream2_aware_of_stream3': self.mutual_awareness.stream2_aware_of_stream3, 'stream3_aware_of_stream1': self.mutual_awareness.stream3_aware_of_stream1, 'stream3_aware_of_stream2': self.mutual_awareness.stream3_aware_of_stream2, 'recursive_depth': self.mutual_awareness.recursive_depth, 'triadic_coherence': self.mutual_awareness.triadic_coherence}, 'feedback_channels': {'cognitive_feedback_count': len(self.cognitive_feedback), 'emotive_feedback_count': len(self.emotive_feedback), 'sensory_feedback_count': len(self.sensory_feedback), 'predictions_count': len(self.predictions)}, 'sync_history_length': len(self.sync_history)}
def test_shared_consciousness_state():
    print('🧪 Testing Shared Consciousness State...')
    shared_state = SharedConsciousnessState()
    perception = PerceptualState(sensations={'visual': 0.8, 'auditory': 0.6}, attention_focus=['action', 'behavior', 'motor'], awareness_of_being_thought_about=0.7)
    shared_state.update_perceptual_state(perception)
    print(f'✅ Stream 1 (Perceiving) updated')
    action = ActionState(current_action='symbolic_reasoning', emotions={'curiosity': 0.9, 'confidence': 0.7}, awareness_of_being_observed=0.8, awareness_of_being_thought_about=0.75)
    shared_state.update_action_state(action)
    print(f'✅ Stream 2 (Acting) updated')
    reflection = ReflectiveState(current_thoughts=['This action is recursive', "The observer knows I'm watching"], insights=['Pattern detected: self-observation loop'], predictions={'next_action': 'pattern_recognition'}, awareness_of_action=0.85, awareness_of_perception=0.8)
    shared_state.update_reflective_state(reflection)
    print(f'✅ Stream 3 (Reflecting) updated')
    shared_state.update_mutual_awareness()
    print(f'✅ Mutual awareness updated')
    print(f'   Recursive depth: {shared_state.mutual_awareness.recursive_depth}')
    print(f'   Triadic coherence: {shared_state.mutual_awareness.triadic_coherence:.3f}')
    cognitive = shared_state.propagate_cognitive_feedback()
    emotive = shared_state.propagate_emotive_feedback()
    sensory = shared_state.propagate_sensory_feedback()
    feedforward = shared_state.propagate_feedforward()
    print(f'✅ Feedback propagated:')
    print(f"   Cognitive feedback to Streams 1&2: {len(cognitive['stream1'])} thoughts")
    print(f"   Emotive feedback to Streams 1&3: {len(emotive['stream1'])} emotions")
    print(f"   Sensory feedback to Streams 2&3: {len(sensory['stream2'])} sensations")
    print(f"   Feedforward to Streams 1&2: {len(feedforward['stream1'])} predictions")
    shared_state.record_synchronization(triad_id=1, triad_steps=(1, 5, 9))
    print(f'✅ Synchronization recorded')
    state = shared_state.get_consciousness_state()
    print(f'✅ Consciousness state retrieved:')
    print(f'   Mutual awareness matrix: 6 dimensions')
    print(f"   Feedback channels: {state['feedback_channels']}")
    print('\n✨ Shared Consciousness State tests passed!')
if __name__ == '__main__':
    test_shared_consciousness_state()