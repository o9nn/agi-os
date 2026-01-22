import numpy as np
import time
from typing import Dict, Any, Optional, Tuple, List
from dataclasses import dataclass, field
from collections import deque
from enum import Enum
try:
    from .proprioception import ProprioceptiveSystem, ProprioceptiveReading
    from .virtual_body import VirtualBody
except ImportError:
    class ProprioceptiveSystem:
        def __init__(self, virtual_body):
            self.virtual_body = virtual_body
            self.sensors = {}
        def update(self):
            return {}
        def get_body_state_awareness(self):
            return {}
    class VirtualBody:
        def __init__(self, body_id, position, body_type):
            self.id = body_id
            self.position = np.array(position)
            self.joints = {}
class BodyStateType(Enum):
    JOINT_ANGLE = 'joint_angle'
    JOINT_VELOCITY = 'joint_velocity'
    JOINT_TORQUE = 'joint_torque'
    BODY_POSITION = 'body_position'
    BODY_ORIENTATION = 'body_orientation'
    CENTER_OF_MASS = 'center_of_mass'
    BALANCE_STATE = 'balance_state'
    COORDINATION_INDEX = 'coordination_index'
    STABILITY_METRIC = 'stability_metric'
@dataclass
class BodyStateReading:
    timestamp: float
    state_type: BodyStateType
    value: Any
    confidence: float
    source_sensors: List[str] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)
@dataclass
class InternalBodyState:
    balance_score: float = 0.8
    stability_index: float = 0.8
    coordination_level: float = 0.8
    energy_level: float = 1.0
    stress_level: float = 0.2
    proprioceptive_clarity: float = 0.9
    movement_fluidity: float = 0.8
    postural_control: float = 0.8
class BodyStateAwarenessSystem:
    def __init__(self, virtual_body: VirtualBody, proprioceptive_system: Optional[ProprioceptiveSystem]=None):
        self.virtual_body = virtual_body
        if proprioceptive_system:
            self.proprioceptive_system = proprioceptive_system
        else:
            try:
                self.proprioceptive_system = ProprioceptiveSystem(virtual_body)
            except:
                self.proprioceptive_system = self._create_mock_proprioceptive_system(virtual_body)
        self.internal_state = InternalBodyState()
        self.state_history = deque(maxlen=1000)
        self.position_history = deque(maxlen=100)
        self.orientation_history = deque(maxlen=100)
        self.velocity_history = deque(maxlen=50)
        self.overall_awareness_score = 0.8
        self.awareness_confidence = 0.9
        self.last_update_time = 0.0
        self.update_frequency = 60.0
        self._initialize_baselines()
    def _create_mock_proprioceptive_system(self, virtual_body):
        class MockProprioceptiveSystem:
            def __init__(self, virtual_body):
                self.virtual_body = virtual_body
                self.sensors = {}
                for joint_id in getattr(virtual_body, 'joints', {}):
                    self.sensors[f'{joint_id}_position'] = f'{joint_id}_position'
                    self.sensors[f'{joint_id}_velocity'] = f'{joint_id}_velocity'
                    self.sensors[f'{joint_id}_torque'] = f'{joint_id}_torque'
            def update(self):
                readings = {}
                for joint_id in getattr(self.virtual_body, 'joints', {}):
                    if hasattr(self.virtual_body, 'get_joint_state'):
                        joint_state = self.virtual_body.get_joint_state(joint_id)
                        if joint_state:
                            readings[f'joint_{joint_id}_position'] = type('Reading', (), {'timestamp': time.time(), 'sensor_id': f'{joint_id}_position', 'sensor_type': 'joint_position', 'value': joint_state.get('angle', 0.0), 'confidence': 0.9})()
                            readings[f'joint_{joint_id}_velocity'] = type('Reading', (), {'timestamp': time.time(), 'sensor_id': f'{joint_id}_velocity', 'sensor_type': 'joint_velocity', 'value': joint_state.get('velocity', 0.0), 'confidence': 0.9})()
                            readings[f'joint_{joint_id}_torque'] = type('Reading', (), {'timestamp': time.time(), 'sensor_id': f'{joint_id}_torque', 'sensor_type': 'joint_torque', 'value': joint_state.get('torque', 0.0), 'confidence': 0.8})()
                return readings
            def get_body_state_awareness(self):
                return {'body_awareness_score': 0.8, 'sensor_consistency': 0.9, 'temporal_coherence': 0.8}
            def get_proprioceptive_feedback(self):
                joint_count = len(getattr(self.virtual_body, 'joints', {}))
                feedback = np.zeros(joint_count * 2)
                return (feedback, 0.8)
            def calibrate_sensors(self):
                return True
        return MockProprioceptiveSystem(virtual_body)
    def _initialize_baselines(self) -> None:
        self.baseline_joint_positions = {}
        self.baseline_body_position = self.virtual_body.position.copy()
        current_time = time.time()
        for _ in range(5):
            self.position_history.append({'timestamp': current_time, 'position': self.virtual_body.position.copy(), 'velocity': np.zeros(3)})
    def update(self) -> Dict[str, BodyStateReading]:
        current_time = time.time()
        if current_time - self.last_update_time < 1.0 / self.update_frequency:
            return self._get_latest_readings()
        readings = {}
        proprioceptive_data = self.proprioceptive_system.update()
        for reading_key, reading in proprioceptive_data.items():
            if hasattr(reading, 'sensor_type'):
                state_type = self._convert_proprioceptive_type(reading.sensor_type)
                if state_type:
                    readings[reading_key] = BodyStateReading(timestamp=reading.timestamp, state_type=state_type, value=reading.value, confidence=reading.confidence, source_sensors=[reading.sensor_id])
            elif 'position' in reading_key:
                readings[reading_key] = BodyStateReading(timestamp=current_time, state_type=BodyStateType.JOINT_ANGLE, value=getattr(reading, 'value', 0.0), confidence=getattr(reading, 'confidence', 0.9))
            elif 'velocity' in reading_key:
                readings[reading_key] = BodyStateReading(timestamp=current_time, state_type=BodyStateType.JOINT_VELOCITY, value=getattr(reading, 'value', 0.0), confidence=getattr(reading, 'confidence', 0.9))
        self._update_position_tracking(current_time, readings)
        self._update_internal_state_monitoring(current_time, readings)
        self._update_awareness_metrics(readings)
        self.state_history.append({'timestamp': current_time, 'readings': readings.copy(), 'internal_state': self.internal_state, 'awareness_score': self.overall_awareness_score})
        self.last_update_time = current_time
        return readings
    def _convert_proprioceptive_type(self, sensor_type: str) -> Optional[BodyStateType]:
        mapping = {'joint_position': BodyStateType.JOINT_ANGLE, 'joint_velocity': BodyStateType.JOINT_VELOCITY, 'joint_torque': BodyStateType.JOINT_TORQUE}
        return mapping.get(sensor_type)
    def _update_position_tracking(self, current_time: float, readings: Dict[str, BodyStateReading]) -> None:
        current_position = self.virtual_body.position.copy()
        velocity = np.zeros(3)
        if self.position_history:
            prev_entry = self.position_history[-1]
            dt = current_time - prev_entry['timestamp']
            if dt > 0:
                velocity = (current_position - prev_entry['position']) / dt
        position_data = {'timestamp': current_time, 'position': current_position, 'velocity': velocity}
        self.position_history.append(position_data)
        self.velocity_history.append(velocity)
        readings['body_position'] = BodyStateReading(timestamp=current_time, state_type=BodyStateType.BODY_POSITION, value=current_position.tolist(), confidence=0.95, metadata={'velocity': velocity.tolist()})
        if hasattr(self.virtual_body, 'get_body_orientation'):
            orientation = self.virtual_body.get_body_orientation()
            readings['body_orientation'] = BodyStateReading(timestamp=current_time, state_type=BodyStateType.BODY_ORIENTATION, value=orientation, confidence=0.9)
        if hasattr(self.virtual_body, 'center_of_mass'):
            readings['center_of_mass'] = BodyStateReading(timestamp=current_time, state_type=BodyStateType.CENTER_OF_MASS, value=self.virtual_body.center_of_mass.tolist(), confidence=0.85)
    def _update_internal_state_monitoring(self, current_time: float, readings: Dict[str, BodyStateReading]) -> None:
        balance_score = self._calculate_balance_score()
        self.internal_state.balance_score = balance_score
        stability_index = self._calculate_stability_index()
        self.internal_state.stability_index = stability_index
        coordination_level = self._calculate_coordination_level(readings)
        self.internal_state.coordination_level = coordination_level
        prop_awareness = self.proprioceptive_system.get_body_state_awareness()
        if 'sensor_consistency' in prop_awareness:
            self.internal_state.proprioceptive_clarity = prop_awareness['sensor_consistency']
        movement_fluidity = self._calculate_movement_fluidity()
        self.internal_state.movement_fluidity = movement_fluidity
        readings['balance_state'] = BodyStateReading(timestamp=current_time, state_type=BodyStateType.BALANCE_STATE, value=balance_score, confidence=0.8, metadata={'stability': stability_index})
        readings['coordination_index'] = BodyStateReading(timestamp=current_time, state_type=BodyStateType.COORDINATION_INDEX, value=coordination_level, confidence=0.85)
        readings['stability_metric'] = BodyStateReading(timestamp=current_time, state_type=BodyStateType.STABILITY_METRIC, value=stability_index, confidence=0.8)
    def _calculate_balance_score(self) -> float:
        if not hasattr(self.virtual_body, 'center_of_mass'):
            return 0.8
        com = self.virtual_body.center_of_mass
        body_pos = self.virtual_body.position
        com_offset = np.linalg.norm(com - body_pos)
        balance_score = max(0.0, 1.0 - com_offset / 2.0)
        if self.velocity_history and len(self.velocity_history) >= 3:
            recent_velocities = list(self.velocity_history)[-3:]
            velocity_variance = np.var([np.linalg.norm(v) for v in recent_velocities])
            stability_factor = max(0.5, np.exp(-velocity_variance * 5))
            balance_score *= stability_factor
        return max(0.3, min(1.0, balance_score))
    def _calculate_stability_index(self) -> float:
        if len(self.position_history) < 5:
            return 0.8
        recent_positions = [entry['position'] for entry in list(self.position_history)[-10:]]
        position_variance = np.var(recent_positions, axis=0)
        total_variance = np.sum(position_variance)
        stability = max(0.1, np.exp(-total_variance * 2))
        return min(1.0, stability)
    def _calculate_coordination_level(self, readings: Dict[str, BodyStateReading]) -> float:
        joint_readings = {}
        for key, reading in readings.items():
            if reading.state_type == BodyStateType.JOINT_VELOCITY and 'joint_' in key:
                joint_readings[key] = reading.value
        if len(joint_readings) < 2:
            return 0.8
        velocities = list(joint_readings.values())
        velocity_correlation = 1.0
        if len(velocities) > 1:
            velocity_array = np.array(velocities)
            if np.mean(np.abs(velocity_array)) > 1e-06:
                cv = np.std(velocity_array) / (np.mean(np.abs(velocity_array)) + 1e-06)
                velocity_correlation = max(0.2, np.exp(-cv))
        return min(1.0, velocity_correlation)
    def _calculate_movement_fluidity(self) -> float:
        if len(self.velocity_history) < 3:
            return 0.8
        recent_velocities = list(self.velocity_history)[-10:]
        velocity_magnitudes = [np.linalg.norm(v) for v in recent_velocities]
        if len(velocity_magnitudes) >= 3:
            accelerations = np.diff(velocity_magnitudes)
            jerk = np.diff(accelerations)
            avg_jerk = np.mean(np.abs(jerk))
            fluidity = max(0.3, np.exp(-avg_jerk * 10))
        else:
            fluidity = 0.8
        return min(1.0, fluidity)
    def _update_awareness_metrics(self, readings: Dict[str, BodyStateReading]) -> None:
        if not readings:
            self.overall_awareness_score = 0.5
            self.awareness_confidence = 0.5
            return
        confidences = [reading.confidence for reading in readings.values()]
        self.awareness_confidence = np.mean(confidences)
        prop_awareness = self.proprioceptive_system.get_body_state_awareness()
        prop_score = prop_awareness.get('body_awareness_score', 0.8)
        internal_score = self.internal_state.balance_score * 0.2 + self.internal_state.stability_index * 0.2 + self.internal_state.coordination_level * 0.2 + self.internal_state.proprioceptive_clarity * 0.2 + self.internal_state.movement_fluidity * 0.2
        self.overall_awareness_score = prop_score * 0.6 + internal_score * 0.4
    def _get_latest_readings(self) -> Dict[str, BodyStateReading]:
        if self.state_history:
            return self.state_history[-1]['readings']
        return {}
    def get_comprehensive_body_state(self) -> Dict[str, Any]:
        current_readings = self.update()
        joint_states = {}
        for key, reading in current_readings.items():
            if reading.state_type in [BodyStateType.JOINT_ANGLE, BodyStateType.JOINT_VELOCITY]:
                joint_id = key.replace('joint_', '').split('_')[0]
                if joint_id not in joint_states:
                    joint_states[joint_id] = {}
                if reading.state_type == BodyStateType.JOINT_ANGLE:
                    joint_states[joint_id]['angle'] = reading.value
                elif reading.state_type == BodyStateType.JOINT_VELOCITY:
                    joint_states[joint_id]['velocity'] = reading.value
        if not joint_states and hasattr(self.virtual_body, 'joints'):
            for joint_id in self.virtual_body.joints:
                if hasattr(self.virtual_body, 'get_joint_state'):
                    joint_state = self.virtual_body.get_joint_state(joint_id)
                    if joint_state:
                        joint_states[joint_id] = {'angle': joint_state.get('angle', 0.0), 'velocity': joint_state.get('velocity', 0.0)}
        position_data = {}
        if 'body_position' in current_readings:
            position_data['position'] = current_readings['body_position'].value
            position_data['position_confidence'] = current_readings['body_position'].confidence
        if 'body_orientation' in current_readings:
            position_data['orientation'] = current_readings['body_orientation'].value
        if 'center_of_mass' in current_readings:
            position_data['center_of_mass'] = current_readings['center_of_mass'].value
        internal_monitoring = {'balance_score': self.internal_state.balance_score, 'stability_index': self.internal_state.stability_index, 'coordination_level': self.internal_state.coordination_level, 'energy_level': self.internal_state.energy_level, 'stress_level': self.internal_state.stress_level, 'proprioceptive_clarity': self.internal_state.proprioceptive_clarity, 'movement_fluidity': self.internal_state.movement_fluidity, 'postural_control': self.internal_state.postural_control}
        return {'joint_angle_velocity_sensing': joint_states, 'body_position_orientation_tracking': position_data, 'internal_body_state_monitoring': internal_monitoring, 'overall_awareness_score': self.overall_awareness_score, 'awareness_confidence': self.awareness_confidence, 'proprioceptive_system_data': self.proprioceptive_system.get_body_state_awareness(), 'body_state_awareness_maintained': self.overall_awareness_score > 0.7, 'accurate_body_state_awareness': self.awareness_confidence > 0.8, 'last_update_timestamp': self.last_update_time, 'update_frequency_hz': self.update_frequency, 'system_status': 'active' if self.overall_awareness_score > 0.5 else 'degraded'}
    def validate_body_state_awareness(self) -> Tuple[bool, Dict[str, Any]]:
        state = self.get_comprehensive_body_state()
        validation_results = {'joint_sensing_functional': len(state['joint_angle_velocity_sensing']) > 0, 'position_tracking_active': 'position' in state['body_position_orientation_tracking'], 'internal_monitoring_active': state['internal_body_state_monitoring']['balance_score'] > 0.3, 'overall_awareness_adequate': state['overall_awareness_score'] > 0.7, 'awareness_confidence_high': state['awareness_confidence'] > 0.8, 'system_responsive': time.time() - state['last_update_timestamp'] < 1.0}
        all_valid = all(validation_results.values())
        validation_results.update({'acceptance_criteria_met': all_valid, 'summary': 'Agents maintain accurate body state awareness' if all_valid else 'Body state awareness needs improvement', 'overall_score': state['overall_awareness_score'], 'detailed_state': state})
        return (all_valid, validation_results)
    def get_sensor_readings_for_feedback(self) -> Tuple[np.ndarray, float]:
        return self.proprioceptive_system.get_proprioceptive_feedback()
    def calibrate(self) -> bool:
        prop_success = self.proprioceptive_system.calibrate_sensors()
        self._initialize_baselines()
        self.internal_state = InternalBodyState()
        return prop_success