#!/usr/bin/env python3
"""
DTESN Sensor Attention Mechanism
================================

Implementation of Phase 3.1.3: Create Attention Mechanisms for Sensors
- Selective attention for sensory input
- Dynamic sensor prioritization
- Attention-guided perception

Integrates with existing DTESN attention systems and sensory-motor infrastructure.
"""

import time
import json
import logging
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
import threading
import queue


class SensorModalityType(Enum):
    """Types of sensor modalities supported"""
    VISUAL = "visual"
    AUDITORY = "auditory"
    TACTILE = "tactile"
    PROPRIOCEPTIVE = "proprioceptive"
    ENVIRONMENTAL = "environmental"
    MOTION = "motion"


@dataclass
class SensorInput:
    """Represents input from a sensor"""
    modality: SensorModalityType
    data: Any
    timestamp: float
    confidence: float = 1.0
    priority: float = 0.5
    spatial_location: Optional[Tuple[float, float, float]] = None
    metadata: Dict = field(default_factory=dict)


@dataclass
class AttentionFocus:
    """Represents an attention focus configuration"""
    modality_weights: Dict[SensorModalityType, float]
    saliency_threshold: float
    temporal_window: float  # seconds
    spatial_radius: Optional[float] = None
    task_context: Optional[str] = None


@dataclass
class SensorAttentionConfig:
    """Configuration for sensor attention mechanism"""
    max_concurrent_foci: int = 3
    attention_switch_threshold: float = 0.7
    decay_rate: float = 0.95
    competition_threshold: float = 0.1
    cooperative_weight: float = 0.8
    min_focus_duration: float = 0.1  # seconds
    max_focus_duration: float = 5.0  # seconds


class SensorAttentionMechanism:
    """
    Selective attention mechanism for multi-modal sensor input.
    
    Implements:
    - Selective attention for sensory input
    - Dynamic sensor prioritization 
    - Attention-guided perception
    """
    
    def __init__(self, config: Optional[SensorAttentionConfig] = None):
        """Initialize the sensor attention mechanism"""
        self.config = config or SensorAttentionConfig()
        
        # Attention state
        self.current_foci: List[AttentionFocus] = []
        self.sensor_weights: Dict[SensorModalityType, float] = {
            modality: 1.0 for modality in SensorModalityType
        }
        self.attention_history: List[Tuple[float, AttentionFocus]] = []
        
        # Input processing
        self.input_queue: queue.Queue = queue.Queue()
        self.filtered_outputs: Dict[SensorModalityType, List] = {}
        
        # Performance tracking
        self.attention_switches: int = 0
        self.total_switch_time: float = 0.0
        self.last_switch_time: float = 0.0
        
        # Locks for thread safety
        self.state_lock = threading.Lock()
        
        # Logger
        self.logger = logging.getLogger(__name__)
        self.logger.setLevel(logging.INFO)
        
        # Initialize modality outputs
        for modality in SensorModalityType:
            self.filtered_outputs[modality] = []
    
    def compute_saliency_score(self, sensor_input: SensorInput) -> float:
        """
        Compute saliency score for a sensor input.
        
        Args:
            sensor_input: The sensor input to evaluate
            
        Returns:
            Saliency score between 0.0 and 1.0
        """
        base_score = sensor_input.priority * sensor_input.confidence
        
        # Modality-specific saliency adjustments
        if sensor_input.modality == SensorModalityType.VISUAL:
            # Higher saliency for visual input with motion or high contrast
            if sensor_input.metadata.get('motion_detected', False):
                base_score *= 1.3
            if sensor_input.metadata.get('high_contrast', False):
                base_score *= 1.2
                
        elif sensor_input.modality == SensorModalityType.MOTION:
            # Motion always has high saliency
            base_score *= 1.5
            
        elif sensor_input.modality == SensorModalityType.AUDITORY:
            # Sudden sounds have high saliency
            if sensor_input.metadata.get('sudden_change', False):
                base_score *= 1.4
        
        # Temporal recency boost
        time_since = time.time() - sensor_input.timestamp
        recency_factor = max(0.1, 1.0 - (time_since / 2.0))  # Decay over 2 seconds
        base_score *= recency_factor
        
        # Apply current modality weight
        base_score *= self.sensor_weights.get(sensor_input.modality, 1.0)
        
        return min(1.0, base_score)
    
    def update_modality_weights(self, context: Optional[str] = None) -> None:
        """
        Dynamically update sensor modality weights based on context.
        
        Args:
            context: Optional context information for weight adjustment
        """
        with self.state_lock:
            if context == "navigation":
                # Prioritize visual and proprioceptive for navigation
                self.sensor_weights[SensorModalityType.VISUAL] = 1.2
                self.sensor_weights[SensorModalityType.PROPRIOCEPTIVE] = 1.1
                self.sensor_weights[SensorModalityType.AUDITORY] = 0.8
                
            elif context == "interaction":
                # Prioritize auditory and tactile for interaction
                self.sensor_weights[SensorModalityType.AUDITORY] = 1.3
                self.sensor_weights[SensorModalityType.TACTILE] = 1.2
                self.sensor_weights[SensorModalityType.VISUAL] = 0.9
                
            elif context == "exploration":
                # Balance all modalities for exploration
                for modality in SensorModalityType:
                    self.sensor_weights[modality] = 1.0
                    
            else:
                # Default balanced weighting with slight visual preference
                self.sensor_weights[SensorModalityType.VISUAL] = 1.1
                self.sensor_weights[SensorModalityType.MOTION] = 1.2
                for modality in [SensorModalityType.AUDITORY, SensorModalityType.TACTILE,
                               SensorModalityType.PROPRIOCEPTIVE, SensorModalityType.ENVIRONMENTAL]:
                    self.sensor_weights[modality] = 1.0
    
    def create_attention_focus(self, 
                              dominant_modality: SensorModalityType,
                              saliency_threshold: float = 0.6,
                              temporal_window: float = 1.0) -> AttentionFocus:
        """
        Create a new attention focus based on sensor input.
        
        Args:
            dominant_modality: The primary modality to focus on
            saliency_threshold: Minimum saliency for inclusion
            temporal_window: Time window for focus in seconds
            
        Returns:
            New AttentionFocus configuration
        """
        # Create modality weights favoring the dominant modality
        modality_weights = {}
        for modality in SensorModalityType:
            if modality == dominant_modality:
                modality_weights[modality] = 1.0
            elif modality == SensorModalityType.MOTION:
                # Motion is always somewhat relevant
                modality_weights[modality] = 0.7
            else:
                modality_weights[modality] = 0.4
        
        return AttentionFocus(
            modality_weights=modality_weights,
            saliency_threshold=saliency_threshold,
            temporal_window=temporal_window
        )
    
    def apply_selective_attention(self, sensor_inputs: List[SensorInput]) -> List[SensorInput]:
        """
        Apply selective attention to filter sensor inputs.
        
        Args:
            sensor_inputs: List of sensor inputs to filter
            
        Returns:
            Filtered list of sensor inputs that pass attention
        """
        start_time = time.time()
        
        # Compute saliency scores for all inputs
        scored_inputs = [
            (self.compute_saliency_score(inp), inp) 
            for inp in sensor_inputs
        ]
        
        # Sort by saliency score (highest first)
        scored_inputs.sort(key=lambda x: x[0], reverse=True)
        
        # Apply focus-based filtering
        filtered_inputs = []
        
        with self.state_lock:
            for focus in self.current_foci:
                for saliency, inp in scored_inputs:
                    # Check if input passes saliency threshold
                    if saliency < focus.saliency_threshold:
                        continue
                    
                    # Check if input matches focus modality weights
                    modality_weight = focus.modality_weights.get(inp.modality, 0.0)
                    if modality_weight * saliency > focus.saliency_threshold:
                        # Check temporal window
                        time_diff = abs(inp.timestamp - start_time)
                        if time_diff <= focus.temporal_window:
                            if inp not in [fi[1] for fi in filtered_inputs]:
                                filtered_inputs.append((saliency, inp))
        
        # If no foci are active, use default filtering
        if not self.current_foci:
            # Take top N inputs based on saliency
            max_inputs = min(5, len(scored_inputs))
            filtered_inputs = scored_inputs[:max_inputs]
        
        # Sort filtered inputs by saliency and extract sensor inputs
        filtered_inputs.sort(key=lambda x: x[0], reverse=True)
        result = [inp for _, inp in filtered_inputs]
        
        # Update performance metrics
        switch_time = time.time() - start_time
        if switch_time > 0.010:  # 10ms threshold from DTESN spec
            self.logger.warning(f"Attention filtering took {switch_time*1000:.2f}ms (target: ≤10ms)")
        
        return result
    
    def update_attention_focus(self, sensor_inputs: List[SensorInput]) -> None:
        """
        Update attention focus based on current sensor inputs.
        
        Args:
            sensor_inputs: Current sensor inputs to analyze
        """
        if not sensor_inputs:
            return
        
        start_time = time.time()
        
        # Analyze input patterns to determine if focus should change
        modality_saliency = {}
        for inp in sensor_inputs:
            saliency = self.compute_saliency_score(inp)
            if inp.modality not in modality_saliency:
                modality_saliency[inp.modality] = []
            modality_saliency[inp.modality].append(saliency)
        
        # Find highest average saliency modality
        avg_saliency = {}
        for modality, saliencies in modality_saliency.items():
            avg_saliency[modality] = sum(saliencies) / len(saliencies)
        
        if not avg_saliency:
            return
        
        dominant_modality = max(avg_saliency, key=avg_saliency.get)
        max_saliency = avg_saliency[dominant_modality]
        
        with self.state_lock:
            # Check if we need to switch focus
            should_switch = False
            
            if not self.current_foci:
                should_switch = True
            elif max_saliency > self.config.attention_switch_threshold:
                # Check if current foci don't adequately cover the dominant modality
                current_weight = 0.0
                for focus in self.current_foci:
                    current_weight += focus.modality_weights.get(dominant_modality, 0.0)
                
                if current_weight < 0.5:  # Current focus insufficient
                    should_switch = True
            
            # Apply attention switching
            if should_switch:
                # Remove old foci if at capacity
                if len(self.current_foci) >= self.config.max_concurrent_foci:
                    self.current_foci.pop(0)  # Remove oldest focus
                
                # Create new focus
                new_focus = self.create_attention_focus(
                    dominant_modality, 
                    saliency_threshold=max(0.5, max_saliency * 0.8),
                    temporal_window=min(2.0, max_saliency * 3.0)
                )
                
                self.current_foci.append(new_focus)
                
                # Update performance metrics
                self.attention_switches += 1
                switch_time = time.time() - start_time
                self.total_switch_time += switch_time
                self.last_switch_time = start_time
                
                # Log switch
                self.logger.info(f"Attention switched to {dominant_modality.value} "
                               f"(saliency: {max_saliency:.3f}, {switch_time*1000:.2f}ms)")
    
    def process_sensor_inputs(self, sensor_inputs: List[SensorInput]) -> Dict[SensorModalityType, List[SensorInput]]:
        """
        Process sensor inputs through the attention mechanism.
        
        Args:
            sensor_inputs: List of sensor inputs to process
            
        Returns:
            Dictionary mapping modalities to filtered inputs
        """
        # Update attention focus based on current inputs
        self.update_attention_focus(sensor_inputs)
        
        # Apply selective attention
        filtered_inputs = self.apply_selective_attention(sensor_inputs)
        
        # Organize by modality
        result = {modality: [] for modality in SensorModalityType}
        for inp in filtered_inputs:
            result[inp.modality].append(inp)
        
        # Store results
        with self.state_lock:
            for modality, inputs in result.items():
                self.filtered_outputs[modality] = inputs
        
        return result
    
    def get_attention_state(self) -> Dict:
        """
        Get current attention state for monitoring/debugging.
        
        Returns:
            Dictionary containing attention state information
        """
        with self.state_lock:
            return {
                'current_foci': len(self.current_foci),
                'sensor_weights': {
                    modality.value: weight 
                    for modality, weight in self.sensor_weights.items()
                },
                'attention_switches': self.attention_switches,
                'avg_switch_time_ms': (self.total_switch_time / max(1, self.attention_switches)) * 1000,
                'last_switch_time': self.last_switch_time,
                'filtered_outputs': {
                    modality.value: len(outputs) 
                    for modality, outputs in self.filtered_outputs.items()
                }
            }
    
    def apply_attention_decay(self, system) -> int:
        """
        Apply attention decay to reduce attention weights over time.
        
        Args:
            system: The attention system (for compatibility with C implementation)
            
        Returns:
            0 for success, negative for error
        """
        try:
            with self.state_lock:
                # Decay attention weights
                for modality in SensorModalityType:
                    current_weight = self.sensor_weights[modality]
                    decayed_weight = current_weight * self.config.decay_rate
                    
                    # Don't let weights go below minimum
                    self.sensor_weights[modality] = max(
                        0.001,  # DTESN_ATTENTION_MIN_WEIGHT equivalent
                        decayed_weight
                    )
                
                # Remove old foci that have expired
                current_time = time.time()
                self.current_foci = [
                    focus for focus in self.current_foci
                    if (current_time - self.last_switch_time) < focus.temporal_window
                ]
            
            return 0
            
        except Exception as e:
            self.logger.error(f"Error applying attention decay: {e}")
            return -1
        """Reset attention mechanism to initial state"""
        with self.state_lock:
            self.current_foci.clear()
            self.attention_history.clear()
            self.attention_switches = 0
            self.total_switch_time = 0.0
            self.last_switch_time = 0.0
            
            for modality in SensorModalityType:
                self.filtered_outputs[modality] = []
                self.sensor_weights[modality] = 1.0
    
    def save_attention_log(self, filepath: Path) -> None:
        """Save attention mechanism performance log"""
        state = self.get_attention_state()
        state['timestamp'] = time.time()
        
        try:
            with open(filepath, 'w') as f:
                json.dump(state, f, indent=2)
            self.logger.info(f"Attention log saved to {filepath}")
        except Exception as e:
            self.logger.error(f"Failed to save attention log: {e}")


# Integration utilities for existing systems
def integrate_with_sensory_motor(sensor_attention: SensorAttentionMechanism, sensory_motor_data: Dict) -> Dict:
    """
    Integrate sensor attention mechanism with existing sensory-motor system.
    
    Args:
        sensor_attention: The sensor attention mechanism
        sensory_motor_data: Data from sensory-motor system
        
    Returns:
        Processed data with attention filtering applied
    """
    sensor_inputs = []
    
    # Convert sensory-motor data to SensorInput objects
    if 'motion' in sensory_motor_data:
        sensor_inputs.append(SensorInput(
            modality=SensorModalityType.MOTION,
            data=sensory_motor_data['motion'],
            timestamp=time.time(),
            confidence=0.9 if sensory_motor_data['motion'].get('motion_detected', False) else 0.5,
            priority=0.8,
            metadata=sensory_motor_data['motion']
        ))
    
    if 'objects' in sensory_motor_data:
        sensor_inputs.append(SensorInput(
            modality=SensorModalityType.VISUAL,
            data=sensory_motor_data['objects'],
            timestamp=time.time(),
            confidence=0.8,
            priority=0.7,
            metadata={'objects': sensory_motor_data['objects']}
        ))
    
    if 'mouse_moved' in sensory_motor_data:
        sensor_inputs.append(SensorInput(
            modality=SensorModalityType.PROPRIOCEPTIVE,
            data=sensory_motor_data.get('mouse_moved'),
            timestamp=time.time(),
            confidence=0.6,
            priority=0.4,
            metadata={'mouse_moved': sensory_motor_data['mouse_moved']}
        ))
    
    # Process through attention mechanism
    filtered_data = sensor_attention.process_sensor_inputs(sensor_inputs)
    
    # Convert back to sensory-motor format
    result = {'status': sensory_motor_data.get('status', 'processed')}
    
    for modality, inputs in filtered_data.items():
        if inputs:  # Only include modalities with filtered data
            modality_data = [inp.data for inp in inputs]
            result[f'{modality.value}_filtered'] = modality_data
            result[f'{modality.value}_attention_active'] = True
    
    return result


def create_sensor_attention_for_dtesn(dtesn_config: Optional[Dict] = None) -> SensorAttentionMechanism:
    """
    Create a sensor attention mechanism configured for DTESN integration.
    
    Args:
        dtesn_config: Optional DTESN configuration parameters
        
    Returns:
        Configured SensorAttentionMechanism
    """
    config = SensorAttentionConfig()
    
    if dtesn_config:
        # Apply DTESN-specific configuration
        config.max_concurrent_foci = dtesn_config.get('max_attention_channels', 3)
        config.attention_switch_threshold = dtesn_config.get('attention_threshold', 0.7)
        config.cooperative_weight = dtesn_config.get('cooperative_weight', 0.8)
    
    return SensorAttentionMechanism(config)


# Export main classes and functions
__all__ = [
    'SensorModalityType',
    'SensorInput', 
    'AttentionFocus',
    'SensorAttentionConfig',
    'SensorAttentionMechanism',
    'integrate_with_sensory_motor',
    'create_sensor_attention_for_dtesn'
]


if __name__ == "__main__":
    # Example usage and basic testing
    logging.basicConfig(level=logging.INFO)
    
    # Create sensor attention mechanism
    attention = SensorAttentionMechanism()
    
    # Example sensor inputs
    test_inputs = [
        SensorInput(
            modality=SensorModalityType.VISUAL,
            data={'frame': 'test_frame'},
            timestamp=time.time(),
            confidence=0.8,
            priority=0.6,
            metadata={'high_contrast': True}
        ),
        SensorInput(
            modality=SensorModalityType.MOTION,
            data={'velocity': [1.0, 2.0]},
            timestamp=time.time(),
            confidence=0.9,
            priority=0.8,
            metadata={'motion_detected': True}
        ),
        SensorInput(
            modality=SensorModalityType.AUDITORY,
            data={'audio_level': 0.5},
            timestamp=time.time(),
            confidence=0.7,
            priority=0.4,
            metadata={'sudden_change': False}
        )
    ]
    
    # Process inputs
    print("Processing sensor inputs...")
    filtered_outputs = attention.process_sensor_inputs(test_inputs)
    
    # Print results
    print("\nFiltered outputs by modality:")
    for modality, inputs in filtered_outputs.items():
        if inputs:
            print(f"  {modality.value}: {len(inputs)} inputs")
    
    # Print attention state
    print(f"\nAttention state: {attention.get_attention_state()}")