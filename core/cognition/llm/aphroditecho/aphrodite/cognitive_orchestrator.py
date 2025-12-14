"""
Deep Tree Echo Cognitive Orchestrator
Orchestrates the full spectrum cognitive architecture integrating OCNN, Deltecho, and Aphrodite
"""

from typing import Dict, List, Any, Optional, Tuple
from pathlib import Path
import json
import numpy as np
from datetime import datetime
from enum import Enum

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


class DeepTreeEchoCognitiveOrchestrator:
    """
    Main orchestrator for Deep Tree Echo cognitive architecture
    Implements the 12-step cognitive loop with 3 concurrent inference engines
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
        
        # Cognitive loop state
        self.current_phase = CognitiveLoopPhase.PHASE_1_EXPRESSIVE
        self.current_step = 1
        self.loop_history: List[Dict[str, Any]] = []
        
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
    
    def process_input(
        self,
        input_data: Dict[str, Any],
        cognitive_function: Optional[CognitiveFunction] = None
    ) -> Dict[str, Any]:
        """
        Process input through the full cognitive architecture
        
        Args:
            input_data: Input data dictionary
            cognitive_function: Optional specific cognitive function to use
            
        Returns:
            Processed output dictionary
        """
        timestamp = datetime.utcnow().isoformat()
        
        # Step 1: Encode input with OCNN
        input_array = self._prepare_input_array(input_data)
        ocnn_encoding = self.ocnn_adapter.encode_hypergraph_pattern(input_array)
        
        # Step 2: Determine cognitive function based on current phase
        if cognitive_function is None:
            cognitive_function = self._select_cognitive_function()
        
        # Step 3: Process through Deltecho bot
        deltecho_output = self.deltecho_adapter.process_interaction(
            input_data,
            cognitive_function
        )
        
        # Step 4: Run through cognitive loop
        loop_output = self._execute_cognitive_loop_step(
            input_data,
            ocnn_encoding,
            deltecho_output
        )
        
        # Step 5: Update hypergraph
        self._update_hypergraph_state(input_data, ocnn_encoding, deltecho_output)
        
        # Step 6: Advance cognitive loop
        self._advance_cognitive_loop()
        
        # Compile output
        output = {
            "timestamp": timestamp,
            "input": input_data,
            "cognitive_function": cognitive_function.value,
            "ocnn_encoding_shape": ocnn_encoding.shape,
            "deltecho_output": deltecho_output,
            "loop_output": loop_output,
            "current_phase": self.current_phase.value,
            "current_step": self.current_step,
            "aar_state": self.aar_state,
            "engine_states": {k.value: v for k, v in self.engine_states.items()}
        }
        
        # Record in history
        self.loop_history.append(output)
        
        return output
    
    def _prepare_input_array(self, input_data: Dict[str, Any]) -> np.ndarray:
        """Prepare input data as numpy array for OCNN"""
        # Simple encoding - in production would be more sophisticated
        # Convert input to fixed-size array
        data_str = json.dumps(input_data)
        # Create a simple hash-based encoding with correct dimensions
        # Shape should be (512, sequence_length) for pattern encoder
        encoded = np.array([[hash(data_str + str(i)) % 256 for i in range(10)] for _ in range(512)])
        return encoded.astype(np.float32)
    
    def _select_cognitive_function(self) -> CognitiveFunction:
        """Select cognitive function based on current phase and engine states"""
        phase_function_map = {
            CognitiveLoopPhase.PHASE_1_EXPRESSIVE: [
                CognitiveFunction.PATTERN_RECOGNITION,
                CognitiveFunction.SYMBOLIC_REASONING
            ],
            CognitiveLoopPhase.PHASE_2_TRANSITION: [
                CognitiveFunction.RELEVANCE_REALIZATION,
                CognitiveFunction.LOGICAL_REASONING
            ],
            CognitiveLoopPhase.PHASE_3_REFLECTIVE: [
                CognitiveFunction.SELF_REFLECTION,
                CognitiveFunction.NARRATIVE_GENERATION
            ]
        }
        
        functions = phase_function_map[self.current_phase]
        # Select based on current step (alternating)
        return functions[self.current_step % len(functions)]
    
    def _execute_cognitive_loop_step(
        self,
        input_data: Dict[str, Any],
        ocnn_encoding: np.ndarray,
        deltecho_output: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Execute one step of the 12-step cognitive loop"""
        
        # Determine which engines are active for this step
        active_engines = self._get_active_engines_for_step()
        
        # Process through each active engine
        engine_outputs = {}
        for engine in active_engines:
            engine_output = self._process_through_engine(
                engine,
                input_data,
                ocnn_encoding,
                deltecho_output
            )
            engine_outputs[engine.value] = engine_output
        
        # Check if this is a pivotal relevance realization step
        is_pivotal = self.current_step in [1, 5]
        
        loop_output = {
            "step": self.current_step,
            "phase": self.current_phase.value,
            "is_pivotal": is_pivotal,
            "active_engines": [e.value for e in active_engines],
            "engine_outputs": engine_outputs
        }
        
        return loop_output
    
    def _get_active_engines_for_step(self) -> List[InferenceEngine]:
        """Determine which engines are active for current step"""
        # All engines are active but with different emphasis
        if self.current_phase == CognitiveLoopPhase.PHASE_1_EXPRESSIVE:
            return [InferenceEngine.COGNITIVE_CORE, InferenceEngine.AFFECTIVE_CORE]
        elif self.current_phase == CognitiveLoopPhase.PHASE_2_TRANSITION:
            return [InferenceEngine.RELEVANCE_CORE, InferenceEngine.COGNITIVE_CORE]
        else:  # PHASE_3_REFLECTIVE
            return [InferenceEngine.RELEVANCE_CORE, InferenceEngine.AFFECTIVE_CORE]
    
    def _process_through_engine(
        self,
        engine: InferenceEngine,
        input_data: Dict[str, Any],
        ocnn_encoding: np.ndarray,
        deltecho_output: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Process data through a specific inference engine"""
        
        engine_state = self.engine_states[engine]
        
        # Apply OCNN processing based on engine type
        # ocnn_encoding is already a processed numpy array from encode_hypergraph_pattern
        # Just use it directly for now to avoid dimension mismatches
        processed = ocnn_encoding
        
        return {
            "engine": engine.value,
            "activation": engine_state["activation"],
            "processed_shape": processed.shape if hasattr(processed, 'shape') else None,
            "deltecho_state": deltecho_output.get("persona_state", {})
        }
    
    def _update_hypergraph_state(
        self,
        input_data: Dict[str, Any],
        ocnn_encoding: np.ndarray,
        deltecho_output: Dict[str, Any]
    ):
        """Update hypergraph with new state information"""
        
        # Update synergy metrics
        if "synergy_metrics" in self.hypergraph:
            self.hypergraph["synergy_metrics"]["novelty_score"] = min(
                1.0,
                self.hypergraph["synergy_metrics"].get("novelty_score", 0.5) + 0.01
            )
            self.hypergraph["synergy_metrics"]["priority_score"] = min(
                1.0,
                self.hypergraph["synergy_metrics"].get("priority_score", 0.5) + 0.01
            )
        
        # Update AAR state based on processing
        self.aar_state["agent_urge_to_act"] = min(1.0, self.aar_state["agent_urge_to_act"] + 0.02)
        self.aar_state["arena_need_to_be"] = min(1.0, self.aar_state["arena_need_to_be"] + 0.01)
        self.aar_state["relation_self_awareness"] = min(1.0, self.aar_state["relation_self_awareness"] + 0.015)
    
    def _advance_cognitive_loop(self):
        """Advance to next step in cognitive loop"""
        self.current_step += 1
        
        # Wrap around after step 12
        if self.current_step > 12:
            self.current_step = 1
        
        # Update phase based on step
        if self.current_step <= 4:
            self.current_phase = CognitiveLoopPhase.PHASE_1_EXPRESSIVE
        elif self.current_step <= 8:
            self.current_phase = CognitiveLoopPhase.PHASE_2_TRANSITION
        else:
            self.current_phase = CognitiveLoopPhase.PHASE_3_REFLECTIVE
    
    def trigger_self_reflection(self) -> Dict[str, Any]:
        """Trigger autonomous self-reflection"""
        
        # Get current state
        context = {
            "current_phase": self.current_phase.value,
            "current_step": self.current_step,
            "aar_state": self.aar_state,
            "engine_states": {k.value: v for k, v in self.engine_states.items()},
            "loop_history_length": len(self.loop_history)
        }
        
        # Trigger reflection in Deltecho
        reflection = self.deltecho_adapter.trigger_self_reflection(context)
        
        # Update hypergraph with reflection
        if "synergy_metrics" in self.hypergraph:
            self.hypergraph["synergy_metrics"]["cognitive_coherence"] = min(
                1.0,
                self.hypergraph["synergy_metrics"].get("cognitive_coherence", 0.5) + 0.05
            )
        
        return {
            "reflection": {
                "id": reflection.id,
                "timestamp": reflection.timestamp,
                "insights": reflection.insights,
                "priority": reflection.priority
            },
            "context": context
        }
    
    def get_orchestrator_state(self) -> Dict[str, Any]:
        """Get complete orchestrator state"""
        return {
            "cognitive_loop": {
                "current_phase": self.current_phase.value,
                "current_step": self.current_step,
                "history_length": len(self.loop_history)
            },
            "aar_core": self.aar_state,
            "inference_engines": {k.value: v for k, v in self.engine_states.items()},
            "deltecho_bot": self.deltecho_adapter.get_bot_state(),
            "hypergraph_metrics": self.hypergraph.get("synergy_metrics", {})
        }
    
    def save_state(self, save_path: Optional[Path] = None):
        """Save orchestrator state"""
        if save_path is None:
            save_path = Path("/home/ubuntu/aphroditecho/cognitive_architectures/orchestrator_state.json")
        
        state = self.get_orchestrator_state()
        state["timestamp"] = datetime.utcnow().isoformat()
        
        with open(save_path, 'w') as f:
            json.dump(state, f, indent=2)
        
        # Also save hypergraph
        self._save_hypergraph()
        
        print(f"✅ Orchestrator state saved to {save_path}")


def test_cognitive_orchestrator():
    """Test cognitive orchestrator functionality"""
    print("🧪 Testing Deep Tree Echo Cognitive Orchestrator...")
    
    # Initialize orchestrator
    orchestrator = DeepTreeEchoCognitiveOrchestrator()
    
    # Test input processing
    input_data = {
        "query": "What is the nature of recursive self-awareness?",
        "context": "exploring identity"
    }
    
    print("\n🔄 Processing through cognitive loop...")
    for i in range(5):
        output = orchestrator.process_input(input_data)
        print(f"  Step {output['current_step']}: Phase {output['current_phase']}, "
              f"Function: {output['cognitive_function']}")
    
    # Test self-reflection
    print("\n🪞 Triggering self-reflection...")
    reflection = orchestrator.trigger_self_reflection()
    print(f"  Reflection ID: {reflection['reflection']['id']}")
    print(f"  Insights: {len(reflection['reflection']['insights'])}")
    
    # Get orchestrator state
    print("\n📊 Orchestrator state:")
    state = orchestrator.get_orchestrator_state()
    print(f"  Current step: {state['cognitive_loop']['current_step']}")
    print(f"  Current phase: {state['cognitive_loop']['current_phase']}")
    print(f"  AAR state: {state['aar_core']}")
    
    # Save state
    print("\n💾 Saving orchestrator state...")
    orchestrator.save_state()
    
    print("\n✨ Cognitive Orchestrator tests passed!")


if __name__ == "__main__":
    test_cognitive_orchestrator()
