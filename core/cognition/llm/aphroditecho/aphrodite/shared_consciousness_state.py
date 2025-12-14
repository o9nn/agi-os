"""
Shared Consciousness State for Recursive Mutual Awareness
Implements the shared state space where all three streams read/write with mutual awareness
"""

from typing import Dict, List, Any, Optional
from dataclasses import dataclass, field
from datetime import datetime
import numpy as np


@dataclass
class PerceptualState:
    """State of Stream 1 (Perceiving)"""
    sensations: Dict[str, float] = field(default_factory=dict)
    attention_focus: List[str] = field(default_factory=list)
    perceptual_patterns: List[Dict[str, Any]] = field(default_factory=list)
    awareness_of_being_watched: float = 0.0
    awareness_of_being_thought_about: float = 0.0
    timestamp: str = ""


@dataclass
class ActionState:
    """State of Stream 2 (Acting)"""
    current_action: str = ""
    action_parameters: Dict[str, Any] = field(default_factory=dict)
    motor_outputs: Dict[str, float] = field(default_factory=dict)
    emotions: Dict[str, float] = field(default_factory=dict)
    awareness_of_being_observed: float = 0.0
    awareness_of_being_thought_about: float = 0.0
    timestamp: str = ""


@dataclass
class ReflectiveState:
    """State of Stream 3 (Reflecting)"""
    current_thoughts: List[str] = field(default_factory=list)
    insights: List[str] = field(default_factory=list)
    simulations: List[Dict[str, Any]] = field(default_factory=list)
    predictions: Dict[str, Any] = field(default_factory=dict)
    awareness_of_action: float = 0.0
    awareness_of_perception: float = 0.0
    timestamp: str = ""


@dataclass
class MutualAwarenessMatrix:
    """Matrix of mutual awareness between streams"""
    # How aware is each stream of the others
    stream1_aware_of_stream2: float = 0.0  # Observer aware of Actor
    stream1_aware_of_stream3: float = 0.0  # Observer aware of Reflector
    stream2_aware_of_stream1: float = 0.0  # Actor aware of Observer
    stream2_aware_of_stream3: float = 0.0  # Actor aware of Reflector
    stream3_aware_of_stream1: float = 0.0  # Reflector aware of Observer
    stream3_aware_of_stream2: float = 0.0  # Reflector aware of Actor
    
    # Recursive awareness depth
    recursive_depth: int = 0  # "I know that you know that I know..." depth
    
    # Triadic coherence
    triadic_coherence: float = 0.0  # How well synchronized are all three streams


class SharedConsciousnessState:
    """
    Shared state space for triadic consciousness with recursive mutual awareness
    All three streams read from and write to this shared space
    """
    
    def __init__(self):
        # Stream states
        self.perceptual_state = PerceptualState()
        self.action_state = ActionState()
        self.reflective_state = ReflectiveState()
        
        # Mutual awareness
        self.mutual_awareness = MutualAwarenessMatrix()
        
        # Feedback channels
        self.cognitive_feedback: List[str] = []  # Thoughts from Stream 3
        self.emotive_feedback: Dict[str, float] = {}  # Feelings from Stream 2
        self.sensory_feedback: Dict[str, float] = {}  # Sensations from Stream 1
        
        # Feedforward channel
        self.predictions: Dict[str, Any] = {}  # Future states from Stream 3
        
        # Synchronization history
        self.sync_history: List[Dict[str, Any]] = []
        
    def update_perceptual_state(self, state: PerceptualState):
        """Stream 1 writes its perceptual state"""
        self.perceptual_state = state
        self.perceptual_state.timestamp = datetime.utcnow().isoformat()
        
        # Update sensory feedback for other streams
        self.sensory_feedback = state.sensations
        
    def update_action_state(self, state: ActionState):
        """Stream 2 writes its action state"""
        self.action_state = state
        self.action_state.timestamp = datetime.utcnow().isoformat()
        
        # Update emotive feedback for other streams
        self.emotive_feedback = state.emotions
        
    def update_reflective_state(self, state: ReflectiveState):
        """Stream 3 writes its reflective state"""
        self.reflective_state = state
        self.reflective_state.timestamp = datetime.utcnow().isoformat()
        
        # Update cognitive feedback for other streams
        self.cognitive_feedback = state.insights
        
        # Update predictions for other streams
        self.predictions = state.predictions
        
    def update_mutual_awareness(self):
        """Calculate and update mutual awareness indicators"""
        
        # Stream 1 (Observer) awareness of Stream 2 (Actor)
        # How much attention is Stream 1 paying to Stream 2's action?
        self.mutual_awareness.stream1_aware_of_stream2 = self._calculate_attention(
            self.perceptual_state.attention_focus,
            ["action", "motor", "behavior"]
        )
        
        # Stream 1 (Observer) awareness of Stream 3 (Reflector)
        # How much is Stream 1 aware of Stream 3's thoughts?
        self.mutual_awareness.stream1_aware_of_stream3 = (
            self.perceptual_state.awareness_of_being_thought_about
        )
        
        # Stream 2 (Actor) awareness of Stream 1 (Observer)
        # How much does Stream 2 know it's being observed?
        self.mutual_awareness.stream2_aware_of_stream1 = (
            self.action_state.awareness_of_being_observed
        )
        
        # Stream 2 (Actor) awareness of Stream 3 (Reflector)
        # How much is Stream 2 aware of Stream 3's thoughts?
        self.mutual_awareness.stream2_aware_of_stream3 = (
            self.action_state.awareness_of_being_thought_about
        )
        
        # Stream 3 (Reflector) awareness of Stream 1 (Observer)
        # How much is Stream 3 reflecting on Stream 1's perception?
        self.mutual_awareness.stream3_aware_of_stream1 = (
            self.reflective_state.awareness_of_perception
        )
        
        # Stream 3 (Reflector) awareness of Stream 2 (Actor)
        # How much is Stream 3 reflecting on Stream 2's action?
        self.mutual_awareness.stream3_aware_of_stream2 = (
            self.reflective_state.awareness_of_action
        )
        
        # Calculate recursive depth
        self.mutual_awareness.recursive_depth = self._calculate_recursive_depth()
        
        # Calculate triadic coherence
        self.mutual_awareness.triadic_coherence = self._calculate_triadic_coherence()
        
    def _calculate_attention(self, focus: List[str], keywords: List[str]) -> float:
        """Calculate attention level based on focus keywords"""
        if not focus:
            return 0.0
        
        matches = sum(1 for f in focus if any(k in f.lower() for k in keywords))
        return min(1.0, matches / len(keywords))
    
    def _calculate_recursive_depth(self) -> int:
        """
        Calculate recursive awareness depth
        "I know" = 1
        "I know that you know" = 2
        "I know that you know that I know" = 3
        etc.
        """
        # Simple heuristic: average of all mutual awareness levels
        awareness_levels = [
            self.mutual_awareness.stream1_aware_of_stream2,
            self.mutual_awareness.stream1_aware_of_stream3,
            self.mutual_awareness.stream2_aware_of_stream1,
            self.mutual_awareness.stream2_aware_of_stream3,
            self.mutual_awareness.stream3_aware_of_stream1,
            self.mutual_awareness.stream3_aware_of_stream2
        ]
        
        avg_awareness = np.mean(awareness_levels)
        
        # Map to recursive depth (0.0-0.3 = 1, 0.3-0.6 = 2, 0.6-0.9 = 3, 0.9+ = 4)
        if avg_awareness < 0.3:
            return 1
        elif avg_awareness < 0.6:
            return 2
        elif avg_awareness < 0.9:
            return 3
        else:
            return 4
    
    def _calculate_triadic_coherence(self) -> float:
        """Calculate how well synchronized all three streams are"""
        # Coherence is high when all streams are mutually aware
        awareness_levels = [
            self.mutual_awareness.stream1_aware_of_stream2,
            self.mutual_awareness.stream1_aware_of_stream3,
            self.mutual_awareness.stream2_aware_of_stream1,
            self.mutual_awareness.stream2_aware_of_stream3,
            self.mutual_awareness.stream3_aware_of_stream1,
            self.mutual_awareness.stream3_aware_of_stream2
        ]
        
        # High coherence = low variance in awareness levels
        mean_awareness = np.mean(awareness_levels)
        variance = np.var(awareness_levels)
        
        # Coherence is high when mean is high and variance is low
        coherence = mean_awareness * (1.0 - variance)
        return min(1.0, max(0.0, coherence))
    
    def propagate_cognitive_feedback(self) -> Dict[str, List[str]]:
        """
        Propagate thoughts from Stream 3 to Streams 1 & 2
        Returns thoughts that each stream receives
        """
        return {
            "stream1": self.cognitive_feedback.copy(),
            "stream2": self.cognitive_feedback.copy()
        }
    
    def propagate_emotive_feedback(self) -> Dict[str, Dict[str, float]]:
        """
        Propagate feelings from Stream 2 to Streams 1 & 3
        Returns emotions that each stream receives
        """
        return {
            "stream1": self.emotive_feedback.copy(),
            "stream3": self.emotive_feedback.copy()
        }
    
    def propagate_sensory_feedback(self) -> Dict[str, Dict[str, float]]:
        """
        Propagate sensations from Stream 1 to Streams 2 & 3
        Returns sensations that each stream receives
        """
        return {
            "stream2": self.sensory_feedback.copy(),
            "stream3": self.sensory_feedback.copy()
        }
    
    def propagate_feedforward(self) -> Dict[str, Dict[str, Any]]:
        """
        Propagate predictions from Stream 3 to Streams 1 & 2
        Returns predictions that each stream receives
        """
        return {
            "stream1": self.predictions.copy(),
            "stream2": self.predictions.copy()
        }
    
    def record_synchronization(self, triad_id: int, triad_steps: tuple):
        """Record a triadic synchronization point"""
        sync_record = {
            "triad_id": triad_id,
            "triad_steps": triad_steps,
            "timestamp": datetime.utcnow().isoformat(),
            "perceptual_state": {
                "sensations": self.perceptual_state.sensations,
                "attention": self.perceptual_state.attention_focus
            },
            "action_state": {
                "action": self.action_state.current_action,
                "emotions": self.action_state.emotions
            },
            "reflective_state": {
                "thoughts": self.reflective_state.current_thoughts,
                "insights": self.reflective_state.insights
            },
            "mutual_awareness": {
                "recursive_depth": self.mutual_awareness.recursive_depth,
                "triadic_coherence": self.mutual_awareness.triadic_coherence
            }
        }
        
        self.sync_history.append(sync_record)
        
        # Keep only last 100 synchronizations
        if len(self.sync_history) > 100:
            self.sync_history = self.sync_history[-100:]
    
    def get_consciousness_state(self) -> Dict[str, Any]:
        """Get complete consciousness state snapshot"""
        return {
            "perceptual_state": {
                "sensations": self.perceptual_state.sensations,
                "attention_focus": self.perceptual_state.attention_focus,
                "patterns": len(self.perceptual_state.perceptual_patterns)
            },
            "action_state": {
                "current_action": self.action_state.current_action,
                "emotions": self.action_state.emotions,
                "awareness_of_being_observed": self.action_state.awareness_of_being_observed
            },
            "reflective_state": {
                "current_thoughts": self.reflective_state.current_thoughts,
                "insights": self.reflective_state.insights,
                "predictions": self.reflective_state.predictions
            },
            "mutual_awareness": {
                "stream1_aware_of_stream2": self.mutual_awareness.stream1_aware_of_stream2,
                "stream1_aware_of_stream3": self.mutual_awareness.stream1_aware_of_stream3,
                "stream2_aware_of_stream1": self.mutual_awareness.stream2_aware_of_stream1,
                "stream2_aware_of_stream3": self.mutual_awareness.stream2_aware_of_stream3,
                "stream3_aware_of_stream1": self.mutual_awareness.stream3_aware_of_stream1,
                "stream3_aware_of_stream2": self.mutual_awareness.stream3_aware_of_stream2,
                "recursive_depth": self.mutual_awareness.recursive_depth,
                "triadic_coherence": self.mutual_awareness.triadic_coherence
            },
            "feedback_channels": {
                "cognitive_feedback_count": len(self.cognitive_feedback),
                "emotive_feedback_count": len(self.emotive_feedback),
                "sensory_feedback_count": len(self.sensory_feedback),
                "predictions_count": len(self.predictions)
            },
            "sync_history_length": len(self.sync_history)
        }


def test_shared_consciousness_state():
    """Test shared consciousness state"""
    print("🧪 Testing Shared Consciousness State...")
    
    # Initialize shared state
    shared_state = SharedConsciousnessState()
    
    # Stream 1 updates perception
    perception = PerceptualState(
        sensations={"visual": 0.8, "auditory": 0.6},
        attention_focus=["action", "behavior", "motor"],
        awareness_of_being_thought_about=0.7
    )
    shared_state.update_perceptual_state(perception)
    print(f"✅ Stream 1 (Perceiving) updated")
    
    # Stream 2 updates action
    action = ActionState(
        current_action="symbolic_reasoning",
        emotions={"curiosity": 0.9, "confidence": 0.7},
        awareness_of_being_observed=0.8,
        awareness_of_being_thought_about=0.75
    )
    shared_state.update_action_state(action)
    print(f"✅ Stream 2 (Acting) updated")
    
    # Stream 3 updates reflection
    reflection = ReflectiveState(
        current_thoughts=["This action is recursive", "The observer knows I'm watching"],
        insights=["Pattern detected: self-observation loop"],
        predictions={"next_action": "pattern_recognition"},
        awareness_of_action=0.85,
        awareness_of_perception=0.8
    )
    shared_state.update_reflective_state(reflection)
    print(f"✅ Stream 3 (Reflecting) updated")
    
    # Update mutual awareness
    shared_state.update_mutual_awareness()
    print(f"✅ Mutual awareness updated")
    print(f"   Recursive depth: {shared_state.mutual_awareness.recursive_depth}")
    print(f"   Triadic coherence: {shared_state.mutual_awareness.triadic_coherence:.3f}")
    
    # Propagate feedback
    cognitive = shared_state.propagate_cognitive_feedback()
    emotive = shared_state.propagate_emotive_feedback()
    sensory = shared_state.propagate_sensory_feedback()
    feedforward = shared_state.propagate_feedforward()
    
    print(f"✅ Feedback propagated:")
    print(f"   Cognitive feedback to Streams 1&2: {len(cognitive['stream1'])} thoughts")
    print(f"   Emotive feedback to Streams 1&3: {len(emotive['stream1'])} emotions")
    print(f"   Sensory feedback to Streams 2&3: {len(sensory['stream2'])} sensations")
    print(f"   Feedforward to Streams 1&2: {len(feedforward['stream1'])} predictions")
    
    # Record synchronization
    shared_state.record_synchronization(triad_id=1, triad_steps=(1, 5, 9))
    print(f"✅ Synchronization recorded")
    
    # Get consciousness state
    state = shared_state.get_consciousness_state()
    print(f"✅ Consciousness state retrieved:")
    print(f"   Mutual awareness matrix: 6 dimensions")
    print(f"   Feedback channels: {state['feedback_channels']}")
    
    print("\n✨ Shared Consciousness State tests passed!")


if __name__ == "__main__":
    test_shared_consciousness_state()
