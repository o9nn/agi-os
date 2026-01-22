import numpy as np
import time
import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
from aar_core.embodied.hardware_abstraction import SensorType, VisionSensor, AuditorySensor, TactileSensor, MultiModalSensorManager, SensorReading
class TestVisionSensor:
    def test_vision_sensor_creation(self):
        sensor = VisionSensor(sensor_id='camera_01', position=(0.0, 0.0, 1.5), resolution=(1920, 1080), field_of_view=90.0)
        assert sensor.sensor_type == SensorType.VISION
        assert sensor.resolution == (1920, 1080)
        assert sensor.field_of_view == 90.0
        assert sensor.depth_range == (0.1, 100.0)
    def test_vision_sensor_focal_length_calculation(self):
        sensor = VisionSensor(sensor_id='camera_02', resolution=(640, 480), field_of_view=60.0)
        expected_focal_length = 320.0 / np.tan(np.radians(30.0))
        assert abs(sensor.focal_length - expected_focal_length) < 1.0
    def test_vision_sensor_intrinsic_matrix(self):
        sensor = VisionSensor(sensor_id='camera_03', resolution=(640, 480))
        intrinsic = sensor.intrinsic_matrix
        assert intrinsic.shape == (3, 3)
        assert abs(intrinsic[0, 2] - 320.0) < 0.1
        assert abs(intrinsic[1, 2] - 240.0) < 0.1
    def test_vision_sensor_reading(self):
        sensor = VisionSensor('camera_04')
        environment_data = {'objects': [{'type': 'cube'}, {'type': 'sphere'}], 'ambient_light': 0.8, 'motion_detected': True, 'dominant_color': [0.7, 0.3, 0.2]}
        reading = sensor.read_sensor(environment_data)
        assert isinstance(reading, SensorReading)
        assert reading.sensor_type == SensorType.VISION
        assert isinstance(reading.value, np.ndarray)
        assert len(reading.value) == 7
        assert abs(reading.value[0] - 2) < 0.1
    def test_vision_sensor_camera_parameters(self):
        sensor = VisionSensor(sensor_id='camera_05', resolution=(800, 600), field_of_view=75.0, depth_range=(0.5, 50.0))
        params = sensor.get_camera_parameters()
        assert params['resolution'] == (800, 600)
        assert params['field_of_view'] == 75.0
        assert params['depth_range'] == (0.5, 50.0)
        assert 'intrinsic_matrix' in params
        assert 'position' in params
class TestAuditorySensor:
    def test_auditory_sensor_creation(self):
        sensor = AuditorySensor(sensor_id='mic_01', position=(0.0, 0.0, 1.7), frequency_range=(50.0, 16000.0), spatial_resolution=180)
        assert sensor.sensor_type == SensorType.AUDITORY
        assert sensor.frequency_range == (50.0, 16000.0)
        assert sensor.spatial_resolution == 180
        assert sensor.num_frequency_bins == 256
    def test_auditory_sensor_reading(self):
        sensor = AuditorySensor('mic_02')
        environment_data = {'sound_sources': [{'position': [5.0, 0.0, 0.0], 'volume': 0.8, 'frequency': 440.0}, {'position': [0.0, -3.0, 0.0], 'volume': 0.6, 'frequency': 880.0}], 'ambient_noise': 0.05}
        reading = sensor.read_sensor(environment_data)
        assert isinstance(reading, SensorReading)
        assert reading.sensor_type == SensorType.AUDITORY
        assert isinstance(reading.value, np.ndarray)
        assert len(reading.value) == 16
    def test_auditory_spatial_localization(self):
        sensor = AuditorySensor('mic_03')
        audio_data = np.zeros(16)
        audio_data[2] = 0.8
        audio_data[8:] = np.random.uniform(0, 0.2, 8)
        localization = sensor.get_spatial_localization(audio_data)
        assert 'dominant_direction_degrees' in localization
        assert 'confidence' in localization
        assert 'energy_distribution' in localization
        assert localization['dominant_direction_degrees'] == 90.0
    def test_auditory_frequency_processing(self):
        sensor = AuditorySensor(sensor_id='mic_04', frequency_range=(100.0, 8000.0))
        assert len(sensor.frequency_bins) == sensor.num_frequency_bins
        assert sensor.frequency_bins[0] == 100.0
        assert sensor.frequency_bins[-1] == 8000.0
class TestTactileSensor:
    def test_tactile_sensor_creation(self):
        sensor = TactileSensor(sensor_id='touch_01', position=(0.5, 0.0, 0.0), sensing_area=(0.02, 0.02), pressure_range=(0.0, 15.0))
        assert sensor.sensor_type == SensorType.TOUCH
        assert sensor.sensing_area == (0.02, 0.02)
        assert sensor.pressure_range == (0.0, 15.0)
        assert sensor.spatial_resolution == (8, 8)
    def test_tactile_sensor_no_contact(self):
        sensor = TactileSensor('touch_02')
        environment_data = {'contact_info': {'in_contact': False}}
        reading = sensor.read_sensor(environment_data)
        assert isinstance(reading, SensorReading)
        assert reading.sensor_type == SensorType.TOUCH
        assert not sensor.contact_detected
        assert sensor.contact_force == 0.0
        assert sensor.contact_area == 0.0
    def test_tactile_sensor_with_contact(self):
        sensor = TactileSensor('touch_03')
        environment_data = {'contact_info': {'in_contact': True, 'pressure': 5.0, 'contact_position': (0.3, 0.7), 'texture_roughness': 0.2, 'surface_temperature': 30.0}}
        reading = sensor.read_sensor(environment_data)
        assert isinstance(reading, SensorReading)
        assert sensor.contact_detected
        assert sensor.contact_force == 5.0
        assert sensor.contact_area > 0.0
        assert len(reading.value) == 68
    def test_tactile_contact_info(self):
        sensor = TactileSensor('touch_04', sensing_area=(0.03, 0.03))
        environment_data = {'contact_info': {'in_contact': True, 'pressure': 2.5, 'contact_position': (0.5, 0.5)}}
        sensor.read_sensor(environment_data)
        contact_info = sensor.get_contact_info()
        assert contact_info['contact_detected']
        assert contact_info['contact_force'] == 2.5
        assert contact_info['sensing_area'] == (0.03, 0.03)
        assert contact_info['spatial_resolution'] == (8, 8)
        assert len(contact_info['position']) == 3
    def test_tactile_texture_detection(self):
        sensor = TactileSensor('touch_05')
        sensor.texture_detection = True
        smooth_env = {'contact_info': {'in_contact': True, 'pressure': 3.0, 'texture_roughness': 0.01}}
        rough_env = {'contact_info': {'in_contact': True, 'pressure': 3.0, 'texture_roughness': 0.5}}
        smooth_reading = sensor.read_sensor(smooth_env)
        rough_reading = sensor.read_sensor(rough_env)
        smooth_std = np.std(smooth_reading.value[:64])
        rough_std = np.std(rough_reading.value[:64])
        assert rough_std > smooth_std
class TestMultiModalSensorManager:
    def test_sensor_manager_creation(self):
        manager = MultiModalSensorManager()
        assert manager.sensor_fusion_enabled
        assert len(manager.sensors) == 0
        assert manager.sync_tolerance == 0.01
    def test_sensor_registration(self):
        manager = MultiModalSensorManager()
        vision_sensor = VisionSensor('camera_01')
        audio_sensor = AuditorySensor('mic_01')
        tactile_sensor = TactileSensor('touch_01')
        assert manager.register_sensor(vision_sensor)
        assert manager.register_sensor(audio_sensor)
        assert manager.register_sensor(tactile_sensor)
        assert len(manager.sensors) == 3
        assert not manager.register_sensor(vision_sensor)
        assert manager.unregister_sensor('mic_01')
        assert len(manager.sensors) == 2
        assert not manager.unregister_sensor('nonexistent_sensor')
    def test_synchronized_readings(self):
        manager = MultiModalSensorManager()
        vision_sensor = VisionSensor('camera_01')
        audio_sensor = AuditorySensor('mic_01')
        tactile_sensor = TactileSensor('touch_01')
        manager.register_sensor(vision_sensor)
        manager.register_sensor(audio_sensor)
        manager.register_sensor(tactile_sensor)
        environment_data = {'objects': [{'type': 'cube'}], 'ambient_light': 0.6, 'motion_detected': False, 'sound_sources': [{'position': [1, 0, 0], 'volume': 0.7, 'frequency': 500}], 'contact_info': {'in_contact': True, 'pressure': 2.0}}
        readings = manager.get_synchronized_readings(environment_data)
        assert len(readings) == 3
        assert 'camera_01' in readings
        assert 'mic_01' in readings
        assert 'touch_01' in readings
        current_time = time.time()
        for reading in readings.values():
            assert current_time - reading.timestamp < manager.sync_tolerance
    def test_sensor_data_fusion(self):
        manager = MultiModalSensorManager(sensor_fusion_enabled=True)
        vision_sensor = VisionSensor('camera_01')
        audio_sensor = AuditorySensor('mic_01')
        tactile_sensor = TactileSensor('touch_01')
        manager.register_sensor(vision_sensor)
        manager.register_sensor(audio_sensor)
        manager.register_sensor(tactile_sensor)
        environment_data = {'objects': [{'type': 'sphere'}], 'ambient_light': 0.8, 'sound_sources': [{'position': [2, 1, 0], 'volume': 0.5, 'frequency': 1000}], 'contact_info': {'in_contact': False}}
        readings = manager.get_synchronized_readings(environment_data)
        fused_data = manager.fuse_sensor_data(readings)
        assert 'timestamp' in fused_data
        assert 'modalities' in fused_data
        assert 'confidence' in fused_data
        assert 'fused_features' in fused_data
        modalities = fused_data['modalities']
        assert 'vision' in modalities
        assert 'auditory' in modalities
        assert 'touch' in modalities
        fused_features = fused_data['fused_features']
        assert 'audiovisual_correlation' in fused_features
        assert 'visuotactile_correlation' in fused_features
    def test_fusion_confidence_weighting(self):
        manager = MultiModalSensorManager()
        manager.modality_weights = {SensorType.VISION: 0.5, SensorType.AUDITORY: 0.3, SensorType.TOUCH: 0.2}
        vision_sensor = VisionSensor('camera_01')
        manager.register_sensor(vision_sensor)
        vision_reading = vision_sensor.read_sensor({})
        vision_reading.confidence = 0.9
        readings = {'camera_01': vision_reading}
        fused_data = manager.fuse_sensor_data(readings)
        expected_confidence = 0.9 * 0.5
        assert abs(fused_data['confidence'] - expected_confidence) < 0.01
    def test_system_status(self):
        manager = MultiModalSensorManager()
        vision_sensor = VisionSensor('camera_01', position=(1.0, 0.0, 1.5))
        audio_sensor = AuditorySensor('mic_01', position=(0.0, 0.0, 1.7))
        manager.register_sensor(vision_sensor)
        manager.register_sensor(audio_sensor)
        status = manager.get_system_status()
        assert status['sensor_count'] == 2
        assert status['fusion_enabled']
        assert 'registered_sensors' in status
        assert 'modality_weights' in status
        assert 'sync_tolerance' in status
        sensors = status['registered_sensors']
        assert 'camera_01' in sensors
        assert 'mic_01' in sensors
        assert sensors['camera_01']['type'] == 'vision'
        assert sensors['mic_01']['type'] == 'auditory'
class TestMultiModalIntegration:
    def test_agents_receive_multimodal_input(self):
        manager = MultiModalSensorManager()
        vision_sensor = VisionSensor(sensor_id='agent_camera', position=(0.0, 0.0, 1.6), resolution=(640, 480), field_of_view=70.0)
        auditory_sensor = AuditorySensor(sensor_id='agent_microphone', position=(0.0, 0.0, 1.65), frequency_range=(50.0, 15000.0))
        tactile_sensor = TactileSensor(sensor_id='agent_hand', position=(0.5, 0.0, 1.2), sensing_area=(0.025, 0.025))
        assert manager.register_sensor(vision_sensor)
        assert manager.register_sensor(auditory_sensor)
        assert manager.register_sensor(tactile_sensor)
        environment_data = {'objects': [{'type': 'red_ball', 'position': [2, 0, 1]}, {'type': 'blue_cube', 'position': [1, 1, 0.5]}], 'ambient_light': 0.7, 'motion_detected': True, 'dominant_color': [0.8, 0.2, 0.1], 'sound_sources': [{'position': [2, 0, 1], 'volume': 0.6, 'frequency': 800.0}, {'position': [-1, 2, 0], 'volume': 0.4, 'frequency': 200.0}], 'ambient_noise': 0.1, 'contact_info': {'in_contact': True, 'pressure': 3.0, 'contact_position': (0.4, 0.6), 'texture_roughness': 0.3, 'surface_temperature': 28.0}}
        readings = manager.get_synchronized_readings(environment_data)
        assert len(readings) == 3, 'Agent must receive input from all sensor modalities'
        vision_reading = readings['agent_camera']
        assert vision_reading.sensor_type == SensorType.VISION
        assert isinstance(vision_reading.value, np.ndarray)
        assert len(vision_reading.value) > 0
        assert vision_reading.confidence > 0
        audio_reading = readings['agent_microphone']
        assert audio_reading.sensor_type == SensorType.AUDITORY
        assert isinstance(audio_reading.value, np.ndarray)
        assert len(audio_reading.value) > 0
        assert audio_reading.confidence > 0
        tactile_reading = readings['agent_hand']
        assert tactile_reading.sensor_type == SensorType.TOUCH
        assert isinstance(tactile_reading.value, np.ndarray)
        assert len(tactile_reading.value) > 0
        assert tactile_reading.confidence > 0
        fused_data = manager.fuse_sensor_data(readings)
        assert 'modalities' in fused_data
        assert len(fused_data['modalities']) == 3
        assert 'vision' in fused_data['modalities']
        assert 'auditory' in fused_data['modalities']
        assert 'touch' in fused_data['modalities']
        assert 'fused_features' in fused_data
        fused_features = fused_data['fused_features']
        assert 'audiovisual_correlation' in fused_features
        assert 'visuotactile_correlation' in fused_features
        assert 0.0 <= fused_data['confidence'] <= 1.0
        print('✓ ACCEPTANCE CRITERIA MET: Agents receive multi-modal sensory input')
        print(f'  - Vision sensor active: {vision_reading.confidence:.2f} confidence')
        print(f'  - Auditory sensor active: {audio_reading.confidence:.2f} confidence')
        print(f'  - Tactile sensor active: {tactile_reading.confidence:.2f} confidence')
        print(f"  - Overall fusion confidence: {fused_data['confidence']:.2f}")
        return True
if __name__ == '__main__':
    test_integration = TestMultiModalIntegration()
    test_integration.test_agents_receive_multimodal_input()
    print('Multi-Modal Virtual Sensors implementation successful!')