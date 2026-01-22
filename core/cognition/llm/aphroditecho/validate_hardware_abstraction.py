import sys
import time
import json
import numpy as np
from typing import Dict, Any
sys.path.insert(0, '/home/runner/work/aphroditecho/aphroditecho')
try:
    from aar_core.embodied import EmbodiedHardwareManager, VirtualBody, VirtualSensor, VirtualActuator, SensorType, ActuatorType, ActuatorCommand
    print('✓ Successfully imported embedded hardware components')
except ImportError as e:
    print(f'✗ Import error: {e}')
    sys.exit(1)
class HardwareAbstractionValidator:
    def __init__(self):
        self.test_results = {}
        self.overall_passed = True
    def run_all_tests(self) -> Dict[str, Any]:
        print('=' * 60)
        print('EMBEDDED HARDWARE ABSTRACTION VALIDATION')
        print('Task 2.2.3: Build Embedded Hardware Abstractions')
        print('=' * 60)
        self.test_virtual_sensor_interfaces()
        self.test_virtual_actuator_interfaces()
        self.test_hardware_simulation()
        self.test_real_time_integration()
        self.test_acceptance_criteria()
        self.print_summary()
        return {'overall_passed': self.overall_passed, 'test_results': self.test_results, 'acceptance_criteria_met': self.test_results.get('acceptance_criteria', {}).get('passed', False)}
    def test_virtual_sensor_interfaces(self):
        print('\n1. Testing Virtual Sensor Interfaces...')
        try:
            position_sensor = VirtualSensor(sensor_id='test_pos_sensor', sensor_type=SensorType.POSITION, position=(0.0, 0.0, 0.0), update_rate=100.0)
            imu_sensor = VirtualSensor(sensor_id='test_imu_sensor', sensor_type=SensorType.IMU, position=(0.1, 0.0, 0.0), update_rate=1000.0)
            pos_reading = position_sensor.read_sensor({'position': 1.5})
            imu_reading = imu_sensor.read_sensor()
            pos_valid = pos_reading is not None and pos_reading.sensor_type == SensorType.POSITION and (pos_reading.confidence > 0.5)
            imu_valid = imu_reading is not None and imu_reading.sensor_type == SensorType.IMU and hasattr(imu_reading.value, '__len__')
            pos_status = position_sensor.get_status()
            status_valid = 'device_id' in pos_status and 'sensor_type' in pos_status and pos_status['active']
            test_passed = pos_valid and imu_valid and status_valid
            self.test_results['virtual_sensors'] = {'passed': test_passed, 'position_sensor_valid': pos_valid, 'imu_sensor_valid': imu_valid, 'status_valid': status_valid, 'details': {'position_reading_value': float(pos_reading.value) if pos_reading else None, 'imu_reading_shape': np.array(imu_reading.value).shape if imu_reading else None, 'status_keys': list(pos_status.keys()) if pos_status else []}}
            if test_passed:
                print('   ✓ Virtual sensor interfaces working correctly')
            else:
                print('   ✗ Virtual sensor interfaces failed validation')
                self.overall_passed = False
        except Exception as e:
            print(f'   ✗ Virtual sensor test failed with error: {e}')
            self.test_results['virtual_sensors'] = {'passed': False, 'error': str(e)}
            self.overall_passed = False
    def test_virtual_actuator_interfaces(self):
        print('\n2. Testing Virtual Actuator Interfaces...')
        try:
            servo_actuator = VirtualActuator(actuator_id='test_servo', actuator_type=ActuatorType.SERVO, position=(0.0, 0.0, 0.0), max_force=50.0, max_speed=5.0)
            test_command = ActuatorCommand(timestamp=time.time(), actuator_id='test_servo', actuator_type=ActuatorType.SERVO, command='set_position', value=1.0)
            command_sent = servo_actuator.send_command(test_command)
            for _ in range(10):
                servo_actuator.update(0.01)
            status = servo_actuator.get_status()
            command_valid = command_sent
            status_valid = 'current_position' in status and 'target_position' in status and status['active']
            movement_valid = abs(status['current_position']) > 0.01
            test_passed = command_valid and status_valid and movement_valid
            self.test_results['virtual_actuators'] = {'passed': test_passed, 'command_sent': command_valid, 'status_valid': status_valid, 'movement_detected': movement_valid, 'details': {'current_position': status.get('current_position'), 'target_position': status.get('target_position'), 'current_velocity': status.get('current_velocity')}}
            if test_passed:
                print('   ✓ Virtual actuator interfaces working correctly')
            else:
                print('   ✗ Virtual actuator interfaces failed validation')
                self.overall_passed = False
        except Exception as e:
            print(f'   ✗ Virtual actuator test failed with error: {e}')
            self.test_results['virtual_actuators'] = {'passed': False, 'error': str(e)}
            self.overall_passed = False
    def test_hardware_simulation(self):
        print('\n3. Testing Hardware Simulation...')
        try:
            virtual_body = VirtualBody(body_id='test_body', position=(0.0, 0.0, 0.0), body_type='humanoid')
            hw_manager = EmbodiedHardwareManager(virtual_body)
            init_success = hw_manager.initialize()
            start_success = hw_manager.start()
            if not (init_success and start_success):
                raise Exception('Failed to initialize or start hardware manager')
            time.sleep(0.1)
            update_result = hw_manager.update(0.01, {'test_env': 'data'})
            motor_success = hw_manager.send_motor_command('torso', 0.5)
            system_status = hw_manager.get_system_status()
            update_valid = update_result is not None and 'system_status' in update_result
            motor_valid = motor_success
            status_valid = 'embodied_hardware_manager' in system_status and 'hardware_bridge' in system_status and system_status['embodied_hardware_manager']['running']
            hw_running = system_status['hardware_bridge']['hardware_simulator_status']['simulator']['running']
            test_passed = init_success and start_success and update_valid and status_valid and hw_running
            self.test_results['hardware_simulation'] = {'passed': test_passed, 'initialization': init_success, 'system_start': start_success, 'update_valid': update_valid, 'motor_commands': motor_valid, 'status_valid': status_valid, 'hardware_running': hw_running, 'details': {'device_count': system_status['hardware_bridge']['hardware_simulator_status']['simulator']['device_count'], 'sensor_count': system_status['hardware_bridge']['hardware_simulator_status']['simulator']['sensor_count'], 'actuator_count': system_status['hardware_bridge']['hardware_simulator_status']['simulator']['actuator_count']}}
            hw_manager.shutdown()
            if test_passed:
                print('   ✓ Hardware simulation functioning correctly')
            else:
                print('   ✗ Hardware simulation failed validation')
                self.overall_passed = False
        except Exception as e:
            print(f'   ✗ Hardware simulation test failed with error: {e}')
            self.test_results['hardware_simulation'] = {'passed': False, 'error': str(e)}
            self.overall_passed = False
    def test_real_time_integration(self):
        print('\n4. Testing Real-time System Integration...')
        try:
            virtual_body = VirtualBody('rt_test_body', (0, 0, 0))
            hw_manager = EmbodiedHardwareManager(virtual_body)
            hw_manager.initialize()
            hw_manager.start()
            start_time = time.time()
            update_count = 0
            max_update_time = 0.0
            total_update_time = 0.0
            while time.time() - start_time < 0.1:
                update_start = time.time()
                hw_manager.update(0.001)
                update_time = time.time() - update_start
                total_update_time += update_time
                max_update_time = max(max_update_time, update_time)
                update_count += 1
            system_status = hw_manager.get_system_status()
            hw_perf = system_status['hardware_bridge']['hardware_simulator_status']['performance']
            avg_update_time_ms = total_update_time / update_count * 1000 if update_count > 0 else 0
            max_update_time_ms = max_update_time * 1000
            avg_latency_ok = avg_update_time_ms <= 2.0
            max_latency_ok = max_update_time_ms <= 10.0
            hw_constraints_met = hw_manager.hardware_bridge.validate_real_time_performance()
            test_passed = avg_latency_ok and max_latency_ok and hw_constraints_met
            self.test_results['real_time_integration'] = {'passed': test_passed, 'avg_latency_ok': avg_latency_ok, 'max_latency_ok': max_latency_ok, 'hw_constraints_met': hw_constraints_met, 'details': {'avg_update_time_ms': avg_update_time_ms, 'max_update_time_ms': max_update_time_ms, 'update_count': update_count, 'hw_performance': hw_perf}}
            hw_manager.shutdown()
            if test_passed:
                print('   ✓ Real-time system integration validated')
            else:
                print('   ✗ Real-time constraints not met')
                self.overall_passed = False
        except Exception as e:
            print(f'   ✗ Real-time integration test failed with error: {e}')
            self.test_results['real_time_integration'] = {'passed': False, 'error': str(e)}
            self.overall_passed = False
    def test_acceptance_criteria(self):
        print('\n5. Testing Acceptance Criteria...')
        print("   Criteria: 'System can interface with simulated hardware'")
        try:
            virtual_body = VirtualBody('acceptance_test_body', (0, 0, 0))
            hw_manager = EmbodiedHardwareManager(virtual_body)
            init_success = hw_manager.initialize()
            start_success = hw_manager.start()
            if not (init_success and start_success):
                raise Exception('System initialization failed')
            time.sleep(0.2)
            for _ in range(10):
                hw_manager.update(0.01, {'test': 'data'})
                time.sleep(0.01)
            validation_results = hw_manager.validate_system_integration()
            tests_passed = validation_results.get('tests', {})
            all_tests_passed = validation_results.get('acceptance_criteria_met', False)
            self.test_results['acceptance_criteria'] = {'passed': all_tests_passed, 'validation_results': validation_results, 'individual_tests': {'hardware_simulation': tests_passed.get('hardware_simulation', {}).get('passed', False), 'sensor_interface': tests_passed.get('sensor_interface', {}).get('passed', False), 'actuator_interface': tests_passed.get('actuator_interface', {}).get('passed', False), 'real_time_performance': tests_passed.get('real_time_performance', {}).get('passed', False), 'bridge_integration': tests_passed.get('bridge_integration', {}).get('passed', False)}}
            hw_manager.shutdown()
            if all_tests_passed:
                print('   ✓ ACCEPTANCE CRITERIA MET')
                print('   ✓ System can interface with simulated hardware')
            else:
                print('   ✗ ACCEPTANCE CRITERIA NOT MET')
                print('   ✗ System cannot reliably interface with simulated hardware')
                self.overall_passed = False
            for test_name, test_result in tests_passed.items():
                status = '✓' if test_result.get('passed', False) else '✗'
                message = test_result.get('message', '')
                print(f'     {status} {test_name}: {message}')
        except Exception as e:
            print(f'   ✗ Acceptance criteria test failed with error: {e}')
            self.test_results['acceptance_criteria'] = {'passed': False, 'error': str(e)}
            self.overall_passed = False
    def print_summary(self):
        print('\n' + '=' * 60)
        print('VALIDATION SUMMARY')
        print('=' * 60)
        for test_name, result in self.test_results.items():
            status = 'PASSED' if result.get('passed', False) else 'FAILED'
            print(f"{test_name.replace('_', ' ').title()}: {status}")
        print('\n' + '-' * 60)
        overall_status = 'PASSED' if self.overall_passed else 'FAILED'
        print(f'OVERALL VALIDATION: {overall_status}')
        if self.overall_passed:
            print('\n✓ Task 2.2.3: Build Embedded Hardware Abstractions - COMPLETE')
            print('✓ All acceptance criteria met')
            print('✓ System can interface with simulated hardware')
        else:
            print('\n✗ Task 2.2.3: Build Embedded Hardware Abstractions - INCOMPLETE')
            print('✗ Some acceptance criteria not met')
        print('=' * 60)
def main():
    validator = HardwareAbstractionValidator()
    results = validator.run_all_tests()
    results_file = '/tmp/hardware_abstraction_validation.json'
    try:
        with open(results_file, 'w') as f:
            json.dump(results, f, indent=2, default=str)
        print(f'\nDetailed results saved to: {results_file}')
    except Exception as e:
        print(f'Failed to save results: {e}')
    return 0 if results['overall_passed'] else 1
if __name__ == '__main__':
    sys.exit(main())