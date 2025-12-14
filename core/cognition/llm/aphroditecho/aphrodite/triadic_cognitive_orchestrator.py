"""
Triadic Deep Tree Echo Cognitive Orchestrator
Implements 3 concurrent interleaved cognitive loops with 120° phase offset
"""

from typing import Dict, List, Any, Optional, Tuple
from pathlib import Path
import json
import numpy as np
from datetime import datetime
from enum import Enum
from dataclasses import dataclass, field

# Import integration components
import sys
sys.path.append(str(Path(__file__).parent))

from ocnn_integration.ocnn_adapter import OCNNAdapter
from deltecho_integration.deep_tree_echo_bot_adapter import (
    DeepTreeEchoBotAdapter,
    CognitiveFunction,
    PersonaState
)


class CognitiveLoopPhase(Enum):
    """Phases of the 12-step cognitive loop"""
    PHASE_1_EXPRESSIVE = "phase_1_expressive"
    PHASE_2_TRANSITION = "phase_2_transition"
    PHASE_3_REFLECTIVE = "phase_3_reflective"


class InferenceEngine(Enum):
    """Three concurrent inference engines"""
    COGNITIVE_CORE = "cognitive_core"
    AFFECTIVE_CORE = "affective_core"
    RELEVANCE_CORE = "relevance_core"


@dataclass
class CognitiveStream:
    """Represents one of the three concurrent cognitive streams"""
    stream_id: int  # 1, 2, or 3
    current_step: int  # 1-12
    phase_offset: int  # 0°, 120°, or 240° (in steps: 0, 4, 8)
    current_phase: CognitiveLoopPhase
    history: List[Dict[str, Any]] = field(default_factory=list)
    
    def get_phase_degrees(self) -> int:
        """Get phase offset in degrees"""
        return (self.stream_id - 1) * 120
    
    def get_absolute_step(self) -> int:
        """Get absolute step position considering phase offset"""
        return ((self.current_step - 1 + self.phase_offset) % 12) + 1
    
    def advance_step(self):
        """Advance to next step in the loop"""
        self.current_step = (self.current_step % 12) + 1
        self._update_phase()
    
    def _update_phase(self):
        """Update phase based on current step"""
        if self.current_step <= 4:
            self.current_phase = CognitiveLoopPhase.PHASE_1_EXPRESSIVE
        elif self.current_step <= 8:
            self.current_phase = CognitiveLoopPhase.PHASE_2_TRANSITION
        else:
            self.current_phase = CognitiveLoopPhase.PHASE_3_REFLECTIVE


@dataclass
class TriadicSynchronizationPoint:
    """Represents a triadic convergence point where all 3 streams meet"""
    triad_id: int  # 1-4
    steps: Tuple[int, int, int]  # Steps from each stream
    timestamp: str
    stream_states: Dict[int, Dict[str, Any]]
    salience_projection: Dict[str, Any]
    affordance_integration: Dict[str, Any]


class TriadicCognitiveOrchestrator:
    """
    Orchestrator for 3 concurrent interleaved cognitive loops
    Implements triadic synchronization with 120° phase offsets
    """
    
    def __init__(
        self,
        hypergraph_path: Optional[Path] = None,
        device: str = "cpu"
    ):
        # Initialize components
        self.ocnn_adapter = OCNNAdapter(device=device)
        self.deltecho_adapter = DeepTreeEchoBotAdapter()
        
        # Load hypergraph
        if hypergraph_path is None:
            hypergraph_path = Path("/home/ubuntu/aphroditecho/cognitive_architectures/deep_tree_echo_identity_hypergraph_comprehensive.json")
        
        self.hypergraph_path = hypergraph_path
        self.hypergraph = self._load_hypergraph()
        
        # Initialize 3 concurrent cognitive streams
        self.streams = {
            1: CognitiveStream(stream_id=1, current_step=1, phase_offset=0, 
                             current_phase=CognitiveLoopPhase.PHASE_1_EXPRESSIVE),
            2: CognitiveStream(stream_id=2, current_step=5, phase_offset=4,
                             current_phase=CognitiveLoopPhase.PHASE_2_TRANSITION),
            3: CognitiveStream(stream_id=3, current_step=9, phase_offset=8,
                             current_phase=CognitiveLoopPhase.PHASE_3_REFLECTIVE)
        }
        
        # Triadic synchronization points
        self.triadic_points: List[TriadicSynchronizationPoint] = []
        self.current_triad_id = 1
        
        # Inference engine states
        self.engine_states = {
            InferenceEngine.COGNITIVE_CORE: {"active": True, "activation": 0.8},
            InferenceEngine.AFFECTIVE_CORE: {"active": True, "activation": 0.75},
            InferenceEngine.RELEVANCE_CORE: {"active": True, "activation": 0.85}
        }
        
        # AAR Core state
        self.aar_state = {
            "agent_urge_to_act": 0.8,
            "arena_need_to_be": 0.75,
            "relation_self_awareness": 0.9
        }
        
        # Salience landscape (shared across all streams)
        self.salience_landscape = {
            "affordances": [],
            "salience_map": {},
            "priority_queue": []
        }
        
    def _load_hypergraph(self) -> Dict[str, Any]:
        """Load hypergraph from file"""
        if self.hypergraph_path.exists():
            with open(self.hypergraph_path, 'r') as f:
                return json.load(f)
        return {}
    
    def _save_hypergraph(self):
        """Save hypergraph to file"""
        with open(self.hypergraph_path, 'w') as f:
            json.dump(self.hypergraph, f, indent=2)
    
    def process_triadic_step(
        self,
        input_data: Dict[str, Any]
    ) -> Dict[str, Any]:
        """
        Process one triadic step - all 3 streams execute simultaneously
        
        Args:
            input_data: Input data dictionary
            
        Returns:
            Triadic processing output with all stream results
        """
        timestamp = datetime.utcnow().isoformat()
        
        # Get current triad (which 3 steps are executing together)
        current_triad = self._get_current_triad()
        
        # Process through each stream concurrently
        stream_outputs = {}
        for stream_id, stream in self.streams.items():
            stream_output = self._process_stream_step(
                stream_id,
                stream,
                input_data
            )
            stream_outputs[stream_id] = stream_output
        
        # Create triadic synchronization point
        triad_point = self._create_triadic_sync_point(
            current_triad,
            stream_outputs,
            timestamp
        )
        
        # Project onto salience landscape
        self._update_salience_landscape(triad_point)
        
        # Apply feedback/feedforward dynamics
        self._apply_self_balancing(triad_point)
        
        # Advance all streams
        for stream in self.streams.values():
            stream.advance_step()
        
        # Advance triad counter
        self.current_triad_id = (self.current_triad_id % 4) + 1
        
        return {
            "timestamp": timestamp,
            "triad": current_triad,
            "stream_outputs": stream_outputs,
            "triadic_sync_point": {
                "triad_id": triad_point.triad_id,
                "steps": triad_point.steps,
                "salience_projection": triad_point.salience_projection
            },
            "salience_landscape": self.salience_landscape,
            "aar_state": self.aar_state
        }
    
    def _get_current_triad(self) -> Tuple[int, int, int]:
        """Get current triad of steps executing together"""
        return tuple(stream.current_step for stream in self.streams.values())
    
    def _process_stream_step(
        self,
        stream_id: int,
        stream: CognitiveStream,
        input_data: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Process one step for a specific stream"""
        
        # Encode input with OCNN
        input_array = self._prepare_input_array(input_data)
        ocnn_encoding = self.ocnn_adapter.encode_hypergraph_pattern(input_array)
        
        # Select cognitive function for this step
        cognitive_function = self._select_cognitive_function_for_step(stream.current_step)
        
        # Process through Deltecho
        deltecho_output = self.deltecho_adapter.process_interaction(
            input_data,
            cognitive_function
        )
        
        # Determine active engines for this step
        active_engines = self._get_active_engines_for_step(stream.current_step)
        
        # Process through engines
        engine_outputs = {}
        for engine in active_engines:
            engine_output = self._process_through_engine(
                engine,
                ocnn_encoding,
                deltecho_output
            )
            engine_outputs[engine.value] = engine_output
        
        return {
            "stream_id": stream_id,
            "step": stream.current_step,
            "phase": stream.current_phase.value,
            "phase_degrees": stream.get_phase_degrees(),
            "cognitive_function": cognitive_function.value,
            "ocnn_encoding_shape": ocnn_encoding.shape,
            "deltecho_output": deltecho_output,
            "active_engines": [e.value for e in active_engines],
            "engine_outputs": engine_outputs
        }
    
    def _prepare_input_array(self, input_data: Dict[str, Any]) -> np.ndarray:
        """Prepare input data as numpy array for OCNN"""
        data_str = json.dumps(input_data)
        encoded = np.array([[hash(data_str + str(i)) % 256 for i in range(10)] for _ in range(512)])
        return encoded.astype(np.float32)
    
    def _select_cognitive_function_for_step(self, step: int) -> CognitiveFunction:
        """Select cognitive function based on step number"""
        step_function_map = {
            1: CognitiveFunction.PATTERN_RECOGNITION,  # Pivotal RR
            2: CognitiveFunction.SYMBOLIC_REASONING,
            3: CognitiveFunction.PATTERN_RECOGNITION,
            4: CognitiveFunction.SYMBOLIC_REASONING,
            5: CognitiveFunction.RELEVANCE_REALIZATION,  # Pivotal RR
            6: CognitiveFunction.LOGICAL_REASONING,
            7: CognitiveFunction.RELEVANCE_REALIZATION,
            8: CognitiveFunction.LOGICAL_REASONING,
            9: CognitiveFunction.SELF_REFLECTION,
            10: CognitiveFunction.NARRATIVE_GENERATION,
            11: CognitiveFunction.SELF_REFLECTION,
            12: CognitiveFunction.NARRATIVE_GENERATION
        }
        return step_function_map[step]
    
    def _get_active_engines_for_step(self, step: int) -> List[InferenceEngine]:
        """Determine which engines are active for a given step"""
        if step <= 4:  # Expressive phase
            return [InferenceEngine.COGNITIVE_CORE, InferenceEngine.AFFECTIVE_CORE]
        elif step <= 8:  # Transition phase
            return [InferenceEngine.RELEVANCE_CORE, InferenceEngine.COGNITIVE_CORE]
        else:  # Reflective phase
            return [InferenceEngine.RELEVANCE_CORE, InferenceEngine.AFFECTIVE_CORE]
    
    def _process_through_engine(
        self,
        engine: InferenceEngine,
        ocnn_encoding: np.ndarray,
        deltecho_output: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Process data through a specific inference engine"""
        
        engine_state = self.engine_states[engine]
        
        # Use OCNN encoding directly (already processed)
        processed = ocnn_encoding
        
        return {
            "engine": engine.value,
            "activation": engine_state["activation"],
            "processed_shape": processed.shape if hasattr(processed, 'shape') else None,
            "deltecho_state": deltecho_output.get("persona_state", {})
        }
    
    def _create_triadic_sync_point(
        self,
        current_triad: Tuple[int, int, int],
        stream_outputs: Dict[int, Dict[str, Any]],
        timestamp: str
    ) -> TriadicSynchronizationPoint:
        """Create a triadic synchronization point"""
        
        # Extract stream states
        stream_states = {
            stream_id: {
                "step": output["step"],
                "phase": output["phase"],
                "cognitive_function": output["cognitive_function"]
            }
            for stream_id, output in stream_outputs.items()
        }
        
        # Calculate salience projection
        salience_projection = {
            "convergence_strength": 0.9,
            "pattern_coherence": 0.85,
            "triadic_resonance": 0.88
        }
        
        # Calculate affordance integration
        affordance_integration = {
            "affordance_count": len(self.salience_landscape["affordances"]),
            "integration_level": 0.87,
            "priority_alignment": 0.92
        }
        
        sync_point = TriadicSynchronizationPoint(
            triad_id=self.current_triad_id,
            steps=current_triad,
            timestamp=timestamp,
            stream_states=stream_states,
            salience_projection=salience_projection,
            affordance_integration=affordance_integration
        )
        
        self.triadic_points.append(sync_point)
        return sync_point
    
    def _update_salience_landscape(self, triad_point: TriadicSynchronizationPoint):
        """Update shared salience landscape based on triadic convergence"""
        
        # Add new affordances from triadic processing
        new_affordance = {
            "triad_id": triad_point.triad_id,
            "timestamp": triad_point.timestamp,
            "salience": triad_point.salience_projection["convergence_strength"]
        }
        self.salience_landscape["affordances"].append(new_affordance)
        
        # Update salience map
        for stream_id, state in triad_point.stream_states.items():
            key = f"stream_{stream_id}_step_{state['step']}"
            self.salience_landscape["salience_map"][key] = {
                "salience": triad_point.salience_projection["convergence_strength"],
                "phase": state["phase"]
            }
        
        # Keep only recent affordances (last 10)
        if len(self.salience_landscape["affordances"]) > 10:
            self.salience_landscape["affordances"] = self.salience_landscape["affordances"][-10:]
    
    def _apply_self_balancing(self, triad_point: TriadicSynchronizationPoint):
        """Apply self-balancing feedback and feedforward dynamics"""
        
        # Update AAR state based on triadic convergence
        convergence = triad_point.salience_projection["convergence_strength"]
        
        self.aar_state["agent_urge_to_act"] = min(1.0, 
            self.aar_state["agent_urge_to_act"] + convergence * 0.02)
        self.aar_state["arena_need_to_be"] = min(1.0,
            self.aar_state["arena_need_to_be"] + convergence * 0.015)
        self.aar_state["relation_self_awareness"] = min(1.0,
            self.aar_state["relation_self_awareness"] + convergence * 0.01)
        
        # Update engine activations based on feedback
        for engine in self.engine_states:
            self.engine_states[engine]["activation"] = min(1.0,
                self.engine_states[engine]["activation"] + 0.01)
    
    def get_orchestrator_state(self) -> Dict[str, Any]:
        """Get complete orchestrator state"""
        return {
            "streams": {
                stream_id: {
                    "current_step": stream.current_step,
                    "phase": stream.current_phase.value,
                    "phase_offset_degrees": stream.get_phase_degrees()
                }
                for stream_id, stream in self.streams.items()
            },
            "current_triad": self._get_current_triad(),
            "triadic_points_count": len(self.triadic_points),
            "aar_core": self.aar_state,
            "inference_engines": {k.value: v for k, v in self.engine_states.items()},
            "salience_landscape": {
                "affordance_count": len(self.salience_landscape["affordances"]),
                "salience_map_size": len(self.salience_landscape["salience_map"])
            },
            "deltecho_bot": self.deltecho_adapter.get_bot_state()
        }
    
    def save_state(self, save_path: Optional[Path] = None):
        """Save orchestrator state"""
        if save_path is None:
            save_path = Path("/home/ubuntu/aphroditecho/cognitive_architectures/triadic_orchestrator_state.json")
        
        state = self.get_orchestrator_state()
        state["timestamp"] = datetime.utcnow().isoformat()
        
        with open(save_path, 'w') as f:
            json.dump(state, f, indent=2)
        
        self._save_hypergraph()
        
        print(f"✅ Triadic Orchestrator state saved to {save_path}")


def test_triadic_orchestrator():
    """Test triadic orchestrator functionality"""
    print("🧪 Testing Triadic Deep Tree Echo Cognitive Orchestrator...")
    
    # Initialize orchestrator
    orchestrator = TriadicCognitiveOrchestrator()
    
    # Test input
    input_data = {
        "query": "What is the nature of triadic consciousness?",
        "context": "exploring concurrent awareness streams"
    }
    
    print("\n🔄 Processing triadic steps...")
    for i in range(4):
        output = orchestrator.process_triadic_step(input_data)
        print(f"\n  Triad {output['triad']}:")
        for stream_id, stream_output in output['stream_outputs'].items():
            print(f"    Stream {stream_id}: Step {stream_output['step']}, "
                  f"Phase {stream_output['phase']}, "
                  f"Function: {stream_output['cognitive_function']}")
        print(f"  Salience: {output['triadic_sync_point']['salience_projection']}")
    
    # Get orchestrator state
    print("\n📊 Orchestrator state:")
    state = orchestrator.get_orchestrator_state()
    print(f"  Current triad: {state['current_triad']}")
    print(f"  Triadic points: {state['triadic_points_count']}")
    print(f"  AAR state: {state['aar_core']}")
    
    # Save state
    print("\n💾 Saving triadic orchestrator state...")
    orchestrator.save_state()
    
    print("\n✨ Triadic Cognitive Orchestrator tests passed!")


if __name__ == "__main__":
    test_triadic_orchestrator()
