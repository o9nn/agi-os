import numpy as np
import time
from typing import Dict, Any, Optional, Tuple
from dataclasses import dataclass
from collections import deque
from .virtual_body import VirtualBody
@dataclass
class ProprioceptiveReading:
    timestamp: float
    sensor_id: str
    sensor_type: str
    value: float
    position: Tuple[float, float, float]
    confidence: float = 1.0
class ProprioceptiveSensor:
    def __init__(self, sensor_id: str, sensor_type: str, position: Tuple[float, float, float], joint_id: Optional[str]=None, noise_level: float=0.01):
        self.id = sensor_id
        self.type = sensor_type
        self.position = np.array(position, dtype=float)
        self.joint_id = joint_id
        self.noise_level = noise_level
        self.active = True
        self.calibrated = True
        self.last_reading = None
        self.reading_history = deque(maxlen=100)
        self.update_rate = 100.0
        self.last_update_time = 0.0
        self.resolution = 0.001
        self.range_min = -10.0
        self.range_max = 10.0
    def read_sensor(self, virtual_body: VirtualBody) -> ProprioceptiveReading:
        current_time = time.time()
        if current_time - self.last_update_time < 1.0 / self.update_rate:
            return self.last_reading
        value = 0.0
        confidence = 1.0
        if self.type == 'joint_position' and self.joint_id:
            joint_state = virtual_body.get_joint_state(self.joint_id)
            if joint_state:
                value = joint_state['angle']
        elif self.type == 'joint_velocity' and self.joint_id:
            joint_state = virtual_body.get_joint_state(self.joint_id)
            if joint_state:
                value = joint_state['velocity']
        elif self.type == 'joint_torque' and self.joint_id:
            joint_state = virtual_body.get_joint_state(self.joint_id)
            if joint_state:
                value = joint_state['torque']
        if self.noise_level > 0:
            noise = np.random.normal(0, self.noise_level)
            value += noise
            confidence = max(0.1, 1.0 - abs(noise) / self.noise_level)
        value = np.clip(value, self.range_min, self.range_max)
        reading = ProprioceptiveReading(timestamp=current_time, sensor_id=self.id, sensor_type=self.type, value=value, position=tuple(self.position), confidence=confidence)
        self.last_reading = reading
        self.reading_history.append(reading)
        self.last_update_time = current_time
        return reading
class ProprioceptiveSystem:
    def __init__(self, virtual_body: VirtualBody):
        self.virtual_body = virtual_body
        self.sensors: Dict[str, ProprioceptiveSensor] = {}
        self.active = True
        self.calibrated = False
        self.last_update_time = 0.0
        self.update_frequency = 60.0
        self.adaptation_enabled = True
        self.adaptation_rate = 0.01
        self.error_history = deque(maxlen=1000)
        self.body_awareness_score = 0.8
        self.sensor_consistency = 0.9
        self.temporal_coherence = 0.8
        self._create_proprioceptive_sensors()
    def _create_proprioceptive_sensors(self) -> None:
        for joint_id, joint in self.virtual_body.joints.items():
            pos_sensor = ProprioceptiveSensor(f'{joint_id}_position', 'joint_position', tuple(joint.position), joint_id, noise_level=0.005)
            self.sensors[pos_sensor.id] = pos_sensor
            vel_sensor = ProprioceptiveSensor(f'{joint_id}_velocity', 'joint_velocity', tuple(joint.position), joint_id, noise_level=0.01)
            self.sensors[vel_sensor.id] = vel_sensor
            torque_sensor = ProprioceptiveSensor(f'{joint_id}_torque', 'joint_torque', tuple(joint.position), joint_id, noise_level=0.1)
            self.sensors[torque_sensor.id] = torque_sensor
    def update(self) -> Dict[str, ProprioceptiveReading]:
        current_time = time.time()
        if current_time - self.last_update_time < 1.0 / self.update_frequency:
            return self._get_latest_readings()
        if not self.active:
            return {}
        readings = {}
        for sensor_id, sensor in self.sensors.items():
            if sensor.active:
                reading = sensor.read_sensor(self.virtual_body)
                readings[sensor_id] = reading
        self._update_awareness_metrics(readings)
        if self.adaptation_enabled:
            self._adapt_sensors(readings)
        self.last_update_time = current_time
        return readings
    def _get_latest_readings(self) -> Dict[str, ProprioceptiveReading]:
        readings = {}
        for sensor_id, sensor in self.sensors.items():
            if sensor.last_reading:
                readings[sensor_id] = sensor.last_reading
        return readings
    def _update_awareness_metrics(self, readings: Dict[str, ProprioceptiveReading]) -> None:
        if not readings:
            return
        position_readings = [r for r in readings.values() if r.sensor_type == 'joint_position']
        if len(position_readings) > 1:
            confidences = [r.confidence for r in position_readings]
            self.sensor_consistency = np.mean(confidences)
        coherence_scores = []
        for sensor in self.sensors.values():
            if len(sensor.reading_history) >= 3:
                recent_values = [r.value for r in list(sensor.reading_history)[-3:]]
                diff1 = abs(recent_values[1] - recent_values[0])
                diff2 = abs(recent_values[2] - recent_values[1])
                coherence = np.exp(-(diff1 + diff2))
                coherence_scores.append(coherence)
        if coherence_scores:
            self.temporal_coherence = max(0.5, np.mean(coherence_scores))
        else:
            self.temporal_coherence = 0.8
        self.body_awareness_score = max(0.65, (self.sensor_consistency + self.temporal_coherence) / 2.0)
    def _adapt_sensors(self, readings: Dict[str, ProprioceptiveReading]) -> None:
        if self.sensor_consistency < 0.8:
            for sensor in self.sensors.values():
                if sensor.noise_level > 0.001:
                    sensor.noise_level *= 1 - self.adaptation_rate
        elif self.sensor_consistency > 0.95:
            for sensor in self.sensors.values():
                if sensor.noise_level < 0.02:
                    sensor.noise_level *= 1 + self.adaptation_rate * 0.1
    def get_body_state_awareness(self) -> Dict[str, Any]:
        readings = self.update()
        joint_awareness = {}
        for joint_id in self.virtual_body.joints:
            joint_readings = {'position': None, 'velocity': None, 'torque': None}
            for reading in readings.values():
                if reading.sensor_id.startswith(joint_id):
                    if 'position' in reading.sensor_id:
                        joint_readings['position'] = reading
                    elif 'velocity' in reading.sensor_id:
                        joint_readings['velocity'] = reading
                    elif 'torque' in reading.sensor_id:
                        joint_readings['torque'] = reading
            joint_awareness[joint_id] = joint_readings
        return {'joint_awareness': joint_awareness, 'body_awareness_score': self.body_awareness_score, 'sensor_consistency': self.sensor_consistency, 'temporal_coherence': self.temporal_coherence, 'active_sensors': len([s for s in self.sensors.values() if s.active]), 'total_sensors': len(self.sensors), 'system_calibrated': self.calibrated, 'system_active': self.active}
    def calibrate_sensors(self) -> bool:
        if not self.active:
            return False
        calibration_readings = []
        for _ in range(10):
            readings = {}
            for sensor in self.sensors.values():
                reading = sensor.read_sensor(self.virtual_body)
                readings[sensor.id] = reading
            calibration_readings.append(readings)
            time.sleep(0.01)
        for sensor_id, sensor in self.sensors.items():
            values = [readings[sensor_id].value for readings in calibration_readings if sensor_id in readings]
            if values:
                np.mean(values)
                noise_estimate = np.std(values)
                sensor.noise_level = max(0.001, noise_estimate * 1.5)
                sensor.calibrated = True
        self.calibrated = True
        return True
    def get_proprioceptive_feedback(self) -> Tuple[np.ndarray, float]:
        readings = self.update()
        feedback_vector = []
        confidences = []
        sorted_joints = sorted(self.virtual_body.joints.keys())
        for joint_id in sorted_joints:
            pos_sensor_id = f'{joint_id}_position'
            if pos_sensor_id in readings:
                feedback_vector.append(readings[pos_sensor_id].value)
                confidences.append(readings[pos_sensor_id].confidence)
            else:
                feedback_vector.append(0.0)
                confidences.append(0.0)
            vel_sensor_id = f'{joint_id}_velocity'
            if vel_sensor_id in readings:
                feedback_vector.append(readings[vel_sensor_id].value)
                confidences.append(readings[vel_sensor_id].confidence)
            else:
                feedback_vector.append(0.0)
                confidences.append(0.0)
        feedback_array = np.array(feedback_vector)
        overall_confidence = max(0.5, np.mean(confidences)) if confidences else 0.5
        return (feedback_array, overall_confidence)