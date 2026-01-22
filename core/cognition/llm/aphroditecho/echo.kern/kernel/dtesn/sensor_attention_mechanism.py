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
    VISUAL = 'visual'
    AUDITORY = 'auditory'
    TACTILE = 'tactile'
    PROPRIOCEPTIVE = 'proprioceptive'
    ENVIRONMENTAL = 'environmental'
    MOTION = 'motion'
@dataclass
class SensorInput:
    modality: SensorModalityType
    data: Any
    timestamp: float
    confidence: float = 1.0
    priority: float = 0.5
    spatial_location: Optional[Tuple[float, float, float]] = None
    metadata: Dict = field(default_factory=dict)
@dataclass
class AttentionFocus:
    modality_weights: Dict[SensorModalityType, float]
    saliency_threshold: float
    temporal_window: float
    spatial_radius: Optional[float] = None
    task_context: Optional[str] = None
@dataclass
class SensorAttentionConfig:
    max_concurrent_foci: int = 3
    attention_switch_threshold: float = 0.7
    decay_rate: float = 0.95
    competition_threshold: float = 0.1
    cooperative_weight: float = 0.8
    min_focus_duration: float = 0.1
    max_focus_duration: float = 5.0
class SensorAttentionMechanism:
    def __init__(self, config: Optional[SensorAttentionConfig]=None):
        self.config = config or SensorAttentionConfig()
        self.current_foci: List[AttentionFocus] = []
        self.sensor_weights: Dict[SensorModalityType, float] = {modality: 1.0 for modality in SensorModalityType}
        self.attention_history: List[Tuple[float, AttentionFocus]] = []
        self.input_queue: queue.Queue = queue.Queue()
        self.filtered_outputs: Dict[SensorModalityType, List] = {}
        self.attention_switches: int = 0
        self.total_switch_time: float = 0.0
        self.last_switch_time: float = 0.0
        self.state_lock = threading.Lock()
        self.logger = logging.getLogger(__name__)
        self.logger.setLevel(logging.INFO)
        for modality in SensorModalityType:
            self.filtered_outputs[modality] = []
    def compute_saliency_score(self, sensor_input: SensorInput) -> float:
        base_score = sensor_input.priority * sensor_input.confidence
        if sensor_input.modality == SensorModalityType.VISUAL:
            if sensor_input.metadata.get('motion_detected', False):
                base_score *= 1.3
            if sensor_input.metadata.get('high_contrast', False):
                base_score *= 1.2
        elif sensor_input.modality == SensorModalityType.MOTION:
            base_score *= 1.5
        elif sensor_input.modality == SensorModalityType.AUDITORY:
            if sensor_input.metadata.get('sudden_change', False):
                base_score *= 1.4
        time_since = time.time() - sensor_input.timestamp
        recency_factor = max(0.1, 1.0 - time_since / 2.0)
        base_score *= recency_factor
        base_score *= self.sensor_weights.get(sensor_input.modality, 1.0)
        return min(1.0, base_score)
    def update_modality_weights(self, context: Optional[str]=None) -> None:
        with self.state_lock:
            if context == 'navigation':
                self.sensor_weights[SensorModalityType.VISUAL] = 1.2
                self.sensor_weights[SensorModalityType.PROPRIOCEPTIVE] = 1.1
                self.sensor_weights[SensorModalityType.AUDITORY] = 0.8
            elif context == 'interaction':
                self.sensor_weights[SensorModalityType.AUDITORY] = 1.3
                self.sensor_weights[SensorModalityType.TACTILE] = 1.2
                self.sensor_weights[SensorModalityType.VISUAL] = 0.9
            elif context == 'exploration':
                for modality in SensorModalityType:
                    self.sensor_weights[modality] = 1.0
            else:
                self.sensor_weights[SensorModalityType.VISUAL] = 1.1
                self.sensor_weights[SensorModalityType.MOTION] = 1.2
                for modality in [SensorModalityType.AUDITORY, SensorModalityType.TACTILE, SensorModalityType.PROPRIOCEPTIVE, SensorModalityType.ENVIRONMENTAL]:
                    self.sensor_weights[modality] = 1.0
    def create_attention_focus(self, dominant_modality: SensorModalityType, saliency_threshold: float=0.6, temporal_window: float=1.0) -> AttentionFocus:
        modality_weights = {}
        for modality in SensorModalityType:
            if modality == dominant_modality:
                modality_weights[modality] = 1.0
            elif modality == SensorModalityType.MOTION:
                modality_weights[modality] = 0.7
            else:
                modality_weights[modality] = 0.4
        return AttentionFocus(modality_weights=modality_weights, saliency_threshold=saliency_threshold, temporal_window=temporal_window)
    def apply_selective_attention(self, sensor_inputs: List[SensorInput]) -> List[SensorInput]:
        start_time = time.time()
        scored_inputs = [(self.compute_saliency_score(inp), inp) for inp in sensor_inputs]
        scored_inputs.sort(key=lambda x: x[0], reverse=True)
        filtered_inputs = []
        with self.state_lock:
            for focus in self.current_foci:
                for saliency, inp in scored_inputs:
                    if saliency < focus.saliency_threshold:
                        continue
                    modality_weight = focus.modality_weights.get(inp.modality, 0.0)
                    if modality_weight * saliency > focus.saliency_threshold:
                        time_diff = abs(inp.timestamp - start_time)
                        if time_diff <= focus.temporal_window:
                            if inp not in [fi[1] for fi in filtered_inputs]:
                                filtered_inputs.append((saliency, inp))
        if not self.current_foci:
            max_inputs = min(5, len(scored_inputs))
            filtered_inputs = scored_inputs[:max_inputs]
        filtered_inputs.sort(key=lambda x: x[0], reverse=True)
        result = [inp for _, inp in filtered_inputs]
        switch_time = time.time() - start_time
        if switch_time > 0.01:
            self.logger.warning(f'Attention filtering took {switch_time * 1000:.2f}ms (target: ≤10ms)')
        return result
    def update_attention_focus(self, sensor_inputs: List[SensorInput]) -> None:
        if not sensor_inputs:
            return
        start_time = time.time()
        modality_saliency = {}
        for inp in sensor_inputs:
            saliency = self.compute_saliency_score(inp)
            if inp.modality not in modality_saliency:
                modality_saliency[inp.modality] = []
            modality_saliency[inp.modality].append(saliency)
        avg_saliency = {}
        for modality, saliencies in modality_saliency.items():
            avg_saliency[modality] = sum(saliencies) / len(saliencies)
        if not avg_saliency:
            return
        dominant_modality = max(avg_saliency, key=avg_saliency.get)
        max_saliency = avg_saliency[dominant_modality]
        with self.state_lock:
            should_switch = False
            if not self.current_foci:
                should_switch = True
            elif max_saliency > self.config.attention_switch_threshold:
                current_weight = 0.0
                for focus in self.current_foci:
                    current_weight += focus.modality_weights.get(dominant_modality, 0.0)
                if current_weight < 0.5:
                    should_switch = True
            if should_switch:
                if len(self.current_foci) >= self.config.max_concurrent_foci:
                    self.current_foci.pop(0)
                new_focus = self.create_attention_focus(dominant_modality, saliency_threshold=max(0.5, max_saliency * 0.8), temporal_window=min(2.0, max_saliency * 3.0))
                self.current_foci.append(new_focus)
                self.attention_switches += 1
                switch_time = time.time() - start_time
                self.total_switch_time += switch_time
                self.last_switch_time = start_time
                self.logger.info(f'Attention switched to {dominant_modality.value} (saliency: {max_saliency:.3f}, {switch_time * 1000:.2f}ms)')
    def process_sensor_inputs(self, sensor_inputs: List[SensorInput]) -> Dict[SensorModalityType, List[SensorInput]]:
        self.update_attention_focus(sensor_inputs)
        filtered_inputs = self.apply_selective_attention(sensor_inputs)
        result = {modality: [] for modality in SensorModalityType}
        for inp in filtered_inputs:
            result[inp.modality].append(inp)
        with self.state_lock:
            for modality, inputs in result.items():
                self.filtered_outputs[modality] = inputs
        return result
    def get_attention_state(self) -> Dict:
        with self.state_lock:
            return {'current_foci': len(self.current_foci), 'sensor_weights': {modality.value: weight for modality, weight in self.sensor_weights.items()}, 'attention_switches': self.attention_switches, 'avg_switch_time_ms': self.total_switch_time / max(1, self.attention_switches) * 1000, 'last_switch_time': self.last_switch_time, 'filtered_outputs': {modality.value: len(outputs) for modality, outputs in self.filtered_outputs.items()}}
    def apply_attention_decay(self, system) -> int:
        try:
            with self.state_lock:
                for modality in SensorModalityType:
                    current_weight = self.sensor_weights[modality]
                    decayed_weight = current_weight * self.config.decay_rate
                    self.sensor_weights[modality] = max(0.001, decayed_weight)
                current_time = time.time()
                self.current_foci = [focus for focus in self.current_foci if current_time - self.last_switch_time < focus.temporal_window]
            return 0
        except Exception as e:
            self.logger.error(f'Error applying attention decay: {e}')
            return -1
        'Reset attention mechanism to initial state'
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
        state = self.get_attention_state()
        state['timestamp'] = time.time()
        try:
            with open(filepath, 'w') as f:
                json.dump(state, f, indent=2)
            self.logger.info(f'Attention log saved to {filepath}')
        except Exception as e:
            self.logger.error(f'Failed to save attention log: {e}')
def integrate_with_sensory_motor(sensor_attention: SensorAttentionMechanism, sensory_motor_data: Dict) -> Dict:
    sensor_inputs = []
    if 'motion' in sensory_motor_data:
        sensor_inputs.append(SensorInput(modality=SensorModalityType.MOTION, data=sensory_motor_data['motion'], timestamp=time.time(), confidence=0.9 if sensory_motor_data['motion'].get('motion_detected', False) else 0.5, priority=0.8, metadata=sensory_motor_data['motion']))
    if 'objects' in sensory_motor_data:
        sensor_inputs.append(SensorInput(modality=SensorModalityType.VISUAL, data=sensory_motor_data['objects'], timestamp=time.time(), confidence=0.8, priority=0.7, metadata={'objects': sensory_motor_data['objects']}))
    if 'mouse_moved' in sensory_motor_data:
        sensor_inputs.append(SensorInput(modality=SensorModalityType.PROPRIOCEPTIVE, data=sensory_motor_data.get('mouse_moved'), timestamp=time.time(), confidence=0.6, priority=0.4, metadata={'mouse_moved': sensory_motor_data['mouse_moved']}))
    filtered_data = sensor_attention.process_sensor_inputs(sensor_inputs)
    result = {'status': sensory_motor_data.get('status', 'processed')}
    for modality, inputs in filtered_data.items():
        if inputs:
            modality_data = [inp.data for inp in inputs]
            result[f'{modality.value}_filtered'] = modality_data
            result[f'{modality.value}_attention_active'] = True
    return result
def create_sensor_attention_for_dtesn(dtesn_config: Optional[Dict]=None) -> SensorAttentionMechanism:
    config = SensorAttentionConfig()
    if dtesn_config:
        config.max_concurrent_foci = dtesn_config.get('max_attention_channels', 3)
        config.attention_switch_threshold = dtesn_config.get('attention_threshold', 0.7)
        config.cooperative_weight = dtesn_config.get('cooperative_weight', 0.8)
    return SensorAttentionMechanism(config)
__all__ = ['SensorModalityType', 'SensorInput', 'AttentionFocus', 'SensorAttentionConfig', 'SensorAttentionMechanism', 'integrate_with_sensory_motor', 'create_sensor_attention_for_dtesn']
if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)
    attention = SensorAttentionMechanism()
    test_inputs = [SensorInput(modality=SensorModalityType.VISUAL, data={'frame': 'test_frame'}, timestamp=time.time(), confidence=0.8, priority=0.6, metadata={'high_contrast': True}), SensorInput(modality=SensorModalityType.MOTION, data={'velocity': [1.0, 2.0]}, timestamp=time.time(), confidence=0.9, priority=0.8, metadata={'motion_detected': True}), SensorInput(modality=SensorModalityType.AUDITORY, data={'audio_level': 0.5}, timestamp=time.time(), confidence=0.7, priority=0.4, metadata={'sudden_change': False})]
    print('Processing sensor inputs...')
    filtered_outputs = attention.process_sensor_inputs(test_inputs)
    print('\nFiltered outputs by modality:')
    for modality, inputs in filtered_outputs.items():
        if inputs:
            print(f'  {modality.value}: {len(inputs)} inputs')
    print(f'\nAttention state: {attention.get_attention_state()}')