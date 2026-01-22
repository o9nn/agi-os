import numpy as np
import time
from typing import Dict, List, Any, Optional
from dataclasses import dataclass
from .hardware_abstraction import EmbeddedHardwareSimulator, VirtualSensor, VirtualActuator, SensorType, ActuatorType, SensorReading, ActuatorCommand
from .proprioception import ProprioceptiveSystem, ProprioceptiveReading
from .virtual_body import VirtualBody
@dataclass
class HardwareMapping:
    joint_id: str
    hardware_device_id: str
    sensor_type: Optional[SensorType] = None
    actuator_type: Optional[ActuatorType] = None
    scale_factor: float = 1.0
    offset: float = 0.0
class ProprioceptiveHardwareBridge:
    def __init__(self, proprioceptive_system: ProprioceptiveSystem, hardware_simulator: EmbeddedHardwareSimulator):
        self.proprioceptive_system = proprioceptive_system
        self.hardware_simulator = hardware_simulator
        self.virtual_body = proprioceptive_system.virtual_body
        self.hardware_mappings: List[HardwareMapping] = []
        self.sensor_mappings: Dict[str, str] = {}
        self.actuator_mappings: Dict[str, str] = {}
        self.active = False
        self.last_update_time = 0.0
        self.update_frequency = 100.0
        self.bridge_update_count = 0
        self.bridge_errors = 0
        self._setup_default_mappings()
    def _setup_default_mappings(self):
        for joint_id, joint in self.virtual_body.joints.items():
            sensor_id = f'{joint_id}_hw_pos_sensor'
            sensor = VirtualSensor(sensor_id=sensor_id, sensor_type=SensorType.POSITION, position=tuple(joint.position), update_rate=self.update_frequency, range_min=-np.pi, range_max=np.pi)
            if self.hardware_simulator.add_sensor(sensor):
                mapping = HardwareMapping(joint_id=joint_id, hardware_device_id=sensor_id, sensor_type=SensorType.POSITION, scale_factor=1.0)
                self.hardware_mappings.append(mapping)
                self.sensor_mappings[joint_id] = sensor_id
            actuator_id = f'{joint_id}_hw_actuator'
            actuator = VirtualActuator(actuator_id=actuator_id, actuator_type=ActuatorType.SERVO, position=tuple(joint.position), max_force=joint.limits.max_torque if joint.limits else 100.0, max_speed=joint.limits.max_velocity if joint.limits else 10.0)
            if self.hardware_simulator.add_actuator(actuator):
                mapping = HardwareMapping(joint_id=joint_id, hardware_device_id=actuator_id, actuator_type=ActuatorType.SERVO, scale_factor=1.0)
                self.hardware_mappings.append(mapping)
                self.actuator_mappings[joint_id] = actuator_id
    def add_hardware_mapping(self, mapping: HardwareMapping) -> bool:
        if mapping.joint_id not in self.virtual_body.joints:
            return False
        device = self.hardware_simulator.registry.get_device(mapping.hardware_device_id)
        if device is None:
            return False
        self.hardware_mappings.append(mapping)
        if mapping.sensor_type:
            self.sensor_mappings[mapping.joint_id] = mapping.hardware_device_id
        if mapping.actuator_type:
            self.actuator_mappings[mapping.joint_id] = mapping.hardware_device_id
        return True
    def start_bridge(self):
        if self.active:
            return
        self.active = True
        self.hardware_simulator.start_simulation()
    def stop_bridge(self):
        if not self.active:
            return
        self.active = False
        self.hardware_simulator.stop_simulation()
    def update_from_hardware(self) -> Dict[str, Any]:
        if not self.active:
            return {}
        current_time = time.time()
        if current_time - self.last_update_time < 1.0 / self.update_frequency:
            return {}
        hardware_readings = {}
        try:
            for joint_id, sensor_id in self.sensor_mappings.items():
                reading = self.hardware_simulator.get_sensor_reading(sensor_id)
                if reading:
                    proprio_reading = self._convert_sensor_reading(joint_id, reading)
                    if proprio_reading:
                        hardware_readings[joint_id] = proprio_reading
                        self._update_proprioceptive_sensor(joint_id, proprio_reading)
            self.bridge_update_count += 1
            self.last_update_time = current_time
        except Exception:
            self.bridge_errors += 1
            pass
        return hardware_readings
    def send_motor_commands(self, motor_commands: Dict[str, float]) -> bool:
        if not self.active:
            return False
        success = True
        try:
            for joint_id, target_angle in motor_commands.items():
                if joint_id in self.actuator_mappings:
                    actuator_id = self.actuator_mappings[joint_id]
                    command = ActuatorCommand(timestamp=time.time(), actuator_id=actuator_id, actuator_type=ActuatorType.SERVO, command='set_position', value=target_angle, feedback_required=True)
                    if not self.hardware_simulator.send_actuator_command(actuator_id, command):
                        success = False
        except Exception:
            self.bridge_errors += 1
            success = False
        return success
    def _convert_sensor_reading(self, joint_id: str, hardware_reading: SensorReading) -> Optional[ProprioceptiveReading]:
        mapping = None
        for m in self.hardware_mappings:
            if m.joint_id == joint_id and m.hardware_device_id == hardware_reading.sensor_id:
                mapping = m
                break
        if mapping is None:
            return None
        scaled_value = hardware_reading.value * mapping.scale_factor + mapping.offset
        if hardware_reading.sensor_type == SensorType.POSITION:
            sensor_type = 'joint_position'
        elif hardware_reading.sensor_type == SensorType.VELOCITY:
            sensor_type = 'joint_velocity'
        elif hardware_reading.sensor_type == SensorType.FORCE:
            sensor_type = 'joint_torque'
        else:
            sensor_type = 'unknown'
        return ProprioceptiveReading(timestamp=hardware_reading.timestamp, sensor_id=f'{joint_id}_{sensor_type}_hw', sensor_type=sensor_type, value=scaled_value, position=hardware_reading.position or (0.0, 0.0, 0.0), confidence=hardware_reading.confidence)
    def _update_proprioceptive_sensor(self, joint_id: str, reading: ProprioceptiveReading):
        sensor_id = f'{joint_id}_{reading.sensor_type}'
        if sensor_id in self.proprioceptive_system.sensors:
            sensor = self.proprioceptive_system.sensors[sensor_id]
            sensor.last_reading = reading
            sensor.reading_history.append(reading)
    def get_hardware_bridge_status(self) -> Dict[str, Any]:
        return {'active': self.active, 'update_frequency': self.update_frequency, 'bridge_updates': self.bridge_update_count, 'bridge_errors': self.bridge_errors, 'hardware_mappings': len(self.hardware_mappings), 'sensor_mappings': len(self.sensor_mappings), 'actuator_mappings': len(self.actuator_mappings), 'hardware_simulator_status': self.hardware_simulator.get_system_status()}
    def validate_real_time_performance(self) -> bool:
        if not self.active or self.bridge_update_count < 10:
            return False
        hw_constraints_met = self.hardware_simulator.validate_real_time_constraints()
        error_rate = self.bridge_errors / max(self.bridge_update_count, 1)
        MAX_ERROR_RATE = 0.05
        bridge_constraints_met = self.active and len(self.hardware_mappings) > 0 and (error_rate <= MAX_ERROR_RATE)
        return hw_constraints_met and bridge_constraints_met
class EmbodiedHardwareManager:
    def __init__(self, virtual_body: VirtualBody):
        self.virtual_body = virtual_body
        self.hardware_simulator = EmbeddedHardwareSimulator(update_rate=1000.0)
        self.proprioceptive_system = ProprioceptiveSystem(virtual_body)
        self.hardware_bridge = ProprioceptiveHardwareBridge(self.proprioceptive_system, self.hardware_simulator)
        self.initialized = False
        self.running = False
        self.total_updates = 0
        self.last_performance_check = time.time()
    def initialize(self) -> bool:
        try:
            if hasattr(self.proprioceptive_system, 'initialize'):
                self.proprioceptive_system.initialize()
            self.hardware_simulator.start_simulation()
            self.hardware_bridge.start_bridge()
            self.initialized = True
            return True
        except Exception:
            self.initialized = False
            return False
    def shutdown(self):
        if not self.initialized:
            return
        self.running = False
        self.hardware_bridge.stop_bridge()
        self.hardware_simulator.stop_simulation()
        self.initialized = False
    def start(self) -> bool:
        if not self.initialized and (not self.initialize()):
            return False
        self.running = True
        return True
    def stop(self):
        self.running = False
    def update(self, dt: float, environment_data: Optional[Dict[str, Any]]=None) -> Dict[str, Any]:
        if not self.running:
            return {}
        self.total_updates += 1
        if environment_data:
            self.hardware_simulator.update_environment(environment_data)
        proprioceptive_readings = self.proprioceptive_system.update()
        hardware_readings = self.hardware_bridge.update_from_hardware()
        return {'proprioceptive_readings': proprioceptive_readings, 'hardware_readings': hardware_readings, 'system_status': self.get_system_status()}
    def send_motor_command(self, joint_id: str, target_angle: float) -> bool:
        return self.hardware_bridge.send_motor_commands({joint_id: target_angle})
    def send_motor_commands(self, commands: Dict[str, float]) -> bool:
        return self.hardware_bridge.send_motor_commands(commands)
    def get_sensor_reading(self, sensor_id: str) -> Optional[SensorReading]:
        return self.hardware_simulator.get_sensor_reading(sensor_id)
    def add_custom_sensor(self, sensor: VirtualSensor, joint_id: Optional[str]=None) -> bool:
        if not self.hardware_simulator.add_sensor(sensor):
            return False
        if joint_id and joint_id in self.virtual_body.joints:
            mapping = HardwareMapping(joint_id=joint_id, hardware_device_id=sensor.device_id, sensor_type=sensor.sensor_type)
            self.hardware_bridge.add_hardware_mapping(mapping)
        return True
    def add_custom_actuator(self, actuator: VirtualActuator, joint_id: Optional[str]=None) -> bool:
        if not self.hardware_simulator.add_actuator(actuator):
            return False
        if joint_id and joint_id in self.virtual_body.joints:
            mapping = HardwareMapping(joint_id=joint_id, hardware_device_id=actuator.device_id, actuator_type=actuator.actuator_type)
            self.hardware_bridge.add_hardware_mapping(mapping)
        return True
    def get_system_status(self) -> Dict[str, Any]:
        return {'embodied_hardware_manager': {'initialized': self.initialized, 'running': self.running, 'total_updates': self.total_updates, 'virtual_body_id': self.virtual_body.id}, 'hardware_bridge': self.hardware_bridge.get_hardware_bridge_status(), 'proprioceptive_system': {'active': self.proprioceptive_system.active, 'calibrated': self.proprioceptive_system.calibrated, 'num_sensors': len(self.proprioceptive_system.sensors)}}
    def validate_system_integration(self) -> Dict[str, Any]:
        validation_results = {'acceptance_criteria_met': False, 'tests': {}}
        if not self.initialized or not self.running:
            validation_results['tests']['initialization'] = {'passed': False, 'message': 'System not properly initialized or running'}
            return validation_results
        hw_status = self.hardware_simulator.get_system_status()
        validation_results['tests']['hardware_simulation'] = {'passed': hw_status['simulator']['running'], 'device_count': hw_status['simulator']['device_count'], 'message': f"Hardware simulation running with {hw_status['simulator']['device_count']} devices"}
        sensors = self.hardware_simulator.registry.get_sensors()
        sensor_readings_valid = True
        for sensor in sensors[:3]:
            reading = sensor.read_sensor()
            if reading is None or reading.confidence < 0.5:
                sensor_readings_valid = False
                break
        validation_results['tests']['sensor_interface'] = {'passed': len(sensors) > 0 and sensor_readings_valid, 'sensor_count': len(sensors), 'message': f'Virtual sensor interface working with {len(sensors)} sensors'}
        actuators = self.hardware_simulator.registry.get_actuators()
        test_command = ActuatorCommand(timestamp=time.time(), actuator_id=actuators[0].device_id if actuators else 'none', actuator_type=ActuatorType.SERVO, command='set_position', value=0.1)
        actuator_command_success = False
        if actuators:
            actuator_command_success = actuators[0].send_command(test_command)
        validation_results['tests']['actuator_interface'] = {'passed': len(actuators) > 0 and actuator_command_success, 'actuator_count': len(actuators), 'message': f'Virtual actuator interface working with {len(actuators)} actuators'}
        rt_performance = self.hardware_bridge.validate_real_time_performance()
        validation_results['tests']['real_time_performance'] = {'passed': rt_performance, 'message': f"Real-time constraints {('met' if rt_performance else 'not met')}"}
        bridge_status = self.hardware_bridge.get_hardware_bridge_status()
        bridge_working = bridge_status['active'] and bridge_status['hardware_mappings'] > 0 and (bridge_status.get('bridge_updates', 0) > 0)
        validation_results['tests']['bridge_integration'] = {'passed': bridge_working, 'mappings': bridge_status['hardware_mappings'], 'updates': bridge_status.get('bridge_updates', 0), 'errors': bridge_status.get('bridge_errors', 0), 'message': f"Hardware bridge integration {('working' if bridge_working else 'failing')}"}
        all_tests_passed = all((test['passed'] for test in validation_results['tests'].values()))
        validation_results['acceptance_criteria_met'] = all_tests_passed
        validation_results['summary'] = 'System can interface with simulated hardware' if all_tests_passed else 'System cannot reliably interface with simulated hardware'
        return validation_results