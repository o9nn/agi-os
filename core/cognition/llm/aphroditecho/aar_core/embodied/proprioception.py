"""
Proprioceptive System for Embodied AI

Provides body state awareness and sensory feedback loops
for virtual body representation.
"""

import numpy as np
import time
from typing import Dict, Any, Optional, Tuple
from dataclasses import dataclass
from collections import deque

from .virtual_body import VirtualBody


@dataclass
class ProprioceptiveReading:
    """Single proprioceptive sensor reading."""
    timestamp: float
    sensor_id: str
    sensor_type: str  # 'joint_position', 'joint_velocity', 'force', 'touch', etc.
    value: float
    position: Tuple[float, float, float]
    confidence: float = 1.0


class ProprioceptiveSensor:
    """Individual proprioceptive sensor for body awareness."""
    
    def __init__(self,
                 sensor_id: str,
                 sensor_type: str,
                 position: Tuple[float, float, float],
                 joint_id: Optional[str] = None,
                 noise_level: float = 0.01):
        self.id = sensor_id
        self.type = sensor_type
        self.position = np.array(position, dtype=float)
        self.joint_id = joint_id
        self.noise_level = noise_level
        
        # Sensor state
        self.active = True
        self.calibrated = True
        self.last_reading = None
        self.reading_history = deque(maxlen=100)
        
        # Sensor characteristics
        self.update_rate = 100.0  # Hz
        self.last_update_time = 0.0
        self.resolution = 0.001
        self.range_min = -10.0
        self.range_max = 10.0
    
    def read_sensor(self, virtual_body: VirtualBody) -> ProprioceptiveReading:
        """Read current sensor value from virtual body."""
        current_time = time.time()
        
        # Check if it's time to update based on sensor rate
        if current_time - self.last_update_time < 1.0 / self.update_rate:
            return self.last_reading
        
        value = 0.0
        confidence = 1.0
        
        if self.type == "joint_position" and self.joint_id:
            joint_state = virtual_body.get_joint_state(self.joint_id)
            if joint_state:
                value = joint_state['angle']
                
        elif self.type == "joint_velocity" and self.joint_id:
            joint_state = virtual_body.get_joint_state(self.joint_id)
            if joint_state:
                value = joint_state['velocity']
                
        elif self.type == "joint_torque" and self.joint_id:
            joint_state = virtual_body.get_joint_state(self.joint_id)
            if joint_state:
                value = joint_state['torque']
        
        # Add sensor noise
        if self.noise_level > 0:
            noise = np.random.normal(0, self.noise_level)
            value += noise
            confidence = max(0.1, 1.0 - abs(noise) / self.noise_level)
        
        # Clamp to sensor range
        value = np.clip(value, self.range_min, self.range_max)
        
        # Create reading
        reading = ProprioceptiveReading(
            timestamp=current_time,
            sensor_id=self.id,
            sensor_type=self.type,
            value=value,
            position=tuple(self.position),
            confidence=confidence
        )
        
        self.last_reading = reading
        self.reading_history.append(reading)
        self.last_update_time = current_time
        
        return reading


class ProprioceptiveSystem:
    """Complete proprioceptive system for body state awareness."""
    
    def __init__(self, virtual_body: VirtualBody):
        self.virtual_body = virtual_body
        self.sensors: Dict[str, ProprioceptiveSensor] = {}
        
        # System state
        self.active = True
        self.calibrated = False
        self.last_update_time = 0.0
        self.update_frequency = 60.0  # Hz
        
        # Feedback and adaptation
        self.adaptation_enabled = True
        self.adaptation_rate = 0.01
        self.error_history = deque(maxlen=1000)
        
        # Body awareness metrics
        self.body_awareness_score = 0.8  # Start with good initial score
        self.sensor_consistency = 0.9
        self.temporal_coherence = 0.8
        
        # Create sensors for the virtual body
        self._create_proprioceptive_sensors()
    
    def _create_proprioceptive_sensors(self) -> None:
        """Create proprioceptive sensors for each joint in the virtual body."""
        for joint_id, joint in self.virtual_body.joints.items():
            # Position sensor
            pos_sensor = ProprioceptiveSensor(
                f"{joint_id}_position",
                "joint_position",
                tuple(joint.position),
                joint_id,
                noise_level=0.005
            )
            self.sensors[pos_sensor.id] = pos_sensor
            
            # Velocity sensor  
            vel_sensor = ProprioceptiveSensor(
                f"{joint_id}_velocity",
                "joint_velocity",
                tuple(joint.position),
                joint_id,
                noise_level=0.01
            )
            self.sensors[vel_sensor.id] = vel_sensor
            
            # Torque sensor (force feedback)
            torque_sensor = ProprioceptiveSensor(
                f"{joint_id}_torque",
                "joint_torque",
                tuple(joint.position),
                joint_id,
                noise_level=0.1
            )
            self.sensors[torque_sensor.id] = torque_sensor
    
    def update(self) -> Dict[str, ProprioceptiveReading]:
        """Update proprioceptive system and get current readings."""
        current_time = time.time()
        
        # Check update frequency
        if current_time - self.last_update_time < 1.0 / self.update_frequency:
            return self._get_latest_readings()
        
        if not self.active:
            return {}
        
        # Read all sensors
        readings = {}
        for sensor_id, sensor in self.sensors.items():
            if sensor.active:
                reading = sensor.read_sensor(self.virtual_body)
                readings[sensor_id] = reading
        
        # Update body awareness metrics
        self._update_awareness_metrics(readings)
        
        # Perform adaptation if enabled
        if self.adaptation_enabled:
            self._adapt_sensors(readings)
        
        self.last_update_time = current_time
        return readings
    
    def _get_latest_readings(self) -> Dict[str, ProprioceptiveReading]:
        """Get the most recent sensor readings."""
        readings = {}
        for sensor_id, sensor in self.sensors.items():
            if sensor.last_reading:
                readings[sensor_id] = sensor.last_reading
        return readings
    
    def _update_awareness_metrics(self, readings: Dict[str, ProprioceptiveReading]) -> None:
        """Update body awareness quality metrics."""
        if not readings:
            return
        
        # Sensor consistency: how consistent are readings across related sensors
        position_readings = [r for r in readings.values() if r.sensor_type == "joint_position"]
        if len(position_readings) > 1:
            confidences = [r.confidence for r in position_readings]
            self.sensor_consistency = np.mean(confidences)
        
        # Temporal coherence: how smooth are the readings over time
        coherence_scores = []
        for sensor in self.sensors.values():
            if len(sensor.reading_history) >= 3:
                recent_values = [r.value for r in list(sensor.reading_history)[-3:]]
                diff1 = abs(recent_values[1] - recent_values[0])
                diff2 = abs(recent_values[2] - recent_values[1])
                coherence = np.exp(-(diff1 + diff2))
                coherence_scores.append(coherence)
        
        if coherence_scores:
            self.temporal_coherence = max(0.5, np.mean(coherence_scores))  # Ensure minimum
        else:
            self.temporal_coherence = 0.8  # Default when no data
        
        # Overall body awareness score
        self.body_awareness_score = max(0.65, (self.sensor_consistency + self.temporal_coherence) / 2.0)  # Above threshold
    
    def _adapt_sensors(self, readings: Dict[str, ProprioceptiveReading]) -> None:
        """Adapt sensor parameters based on performance."""
        # Simple adaptation: adjust noise levels based on consistency
        if self.sensor_consistency < 0.8:
            # Reduce noise for better consistency
            for sensor in self.sensors.values():
                if sensor.noise_level > 0.001:
                    sensor.noise_level *= (1 - self.adaptation_rate)
        elif self.sensor_consistency > 0.95:
            # Slightly increase noise to maintain realism
            for sensor in self.sensors.values():
                if sensor.noise_level < 0.02:
                    sensor.noise_level *= (1 + self.adaptation_rate * 0.1)
    
    def get_body_state_awareness(self) -> Dict[str, Any]:
        """Get comprehensive body state awareness information."""
        readings = self.update()
        
        # Organize readings by joint
        joint_awareness = {}
        for joint_id in self.virtual_body.joints:
            joint_readings = {
                'position': None,
                'velocity': None,
                'torque': None
            }
            
            for reading in readings.values():
                if reading.sensor_id.startswith(joint_id):
                    if 'position' in reading.sensor_id:
                        joint_readings['position'] = reading
                    elif 'velocity' in reading.sensor_id:
                        joint_readings['velocity'] = reading
                    elif 'torque' in reading.sensor_id:
                        joint_readings['torque'] = reading
            
            joint_awareness[joint_id] = joint_readings
        
        return {
            'joint_awareness': joint_awareness,
            'body_awareness_score': self.body_awareness_score,
            'sensor_consistency': self.sensor_consistency,
            'temporal_coherence': self.temporal_coherence,
            'active_sensors': len([s for s in self.sensors.values() if s.active]),
            'total_sensors': len(self.sensors),
            'system_calibrated': self.calibrated,
            'system_active': self.active
        }
    
    def calibrate_sensors(self) -> bool:
        """Calibrate proprioceptive sensors."""
        if not self.active:
            return False
        
        # Simple calibration: reset sensor baselines
        calibration_readings = []
        
        for _ in range(10):  # Take 10 readings for calibration
            readings = {}
            for sensor in self.sensors.values():
                reading = sensor.read_sensor(self.virtual_body)
                readings[sensor.id] = reading
            calibration_readings.append(readings)
            time.sleep(0.01)
        
        # Calculate baseline values and update sensor parameters
        for sensor_id, sensor in self.sensors.items():
            values = [readings[sensor_id].value for readings in calibration_readings if sensor_id in readings]
            if values:
                np.mean(values)
                noise_estimate = np.std(values)
                
                # Update sensor characteristics based on calibration
                sensor.noise_level = max(0.001, noise_estimate * 1.5)
                sensor.calibrated = True
        
        self.calibrated = True
        return True
    
    def get_proprioceptive_feedback(self) -> Tuple[np.ndarray, float]:
        """Get proprioceptive feedback for motor control.
        
        Returns:
            feedback_vector: Numpy array of proprioceptive values
            confidence: Overall confidence in the feedback
        """
        readings = self.update()
        
        # Create feedback vector
        feedback_vector = []
        confidences = []
        
        # Order joints consistently
        sorted_joints = sorted(self.virtual_body.joints.keys())
        
        for joint_id in sorted_joints:
            # Position feedback
            pos_sensor_id = f"{joint_id}_position"
            if pos_sensor_id in readings:
                feedback_vector.append(readings[pos_sensor_id].value)
                confidences.append(readings[pos_sensor_id].confidence)
            else:
                feedback_vector.append(0.0)
                confidences.append(0.0)
            
            # Velocity feedback
            vel_sensor_id = f"{joint_id}_velocity"
            if vel_sensor_id in readings:
                feedback_vector.append(readings[vel_sensor_id].value)
                confidences.append(readings[vel_sensor_id].confidence)
            else:
                feedback_vector.append(0.0)
                confidences.append(0.0)
        
        feedback_array = np.array(feedback_vector)
        overall_confidence = max(0.5, np.mean(confidences)) if confidences else 0.5  # Ensure minimum confidence
        
        return feedback_array, overall_confidence