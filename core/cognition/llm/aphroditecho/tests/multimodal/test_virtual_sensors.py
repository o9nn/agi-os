"""
Test suite for Multi-Modal Virtual Sensors
Testing Task 3.1.1: Implement Multi-Modal Virtual Sensors
- Vision system with configurable cameras
- Auditory system with spatial sound processing  
- Tactile sensors for surface interaction
- Acceptance Criteria: Agents receive multi-modal sensory input
"""

import numpy as np
import time
import sys
import os

# Add project root to path for imports
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))

from aar_core.embodied.hardware_abstraction import (
    SensorType, VisionSensor, AuditorySensor, TactileSensor,
    MultiModalSensorManager, SensorReading
)


class TestVisionSensor:
    """Test Vision Sensor with configurable camera parameters."""
    
    def test_vision_sensor_creation(self):
        """Test VisionSensor can be created with proper parameters."""
        sensor = VisionSensor(
            sensor_id="camera_01",
            position=(0.0, 0.0, 1.5),
            resolution=(1920, 1080),
            field_of_view=90.0
        )
        
        assert sensor.sensor_type == SensorType.VISION
        assert sensor.resolution == (1920, 1080)
        assert sensor.field_of_view == 90.0
        assert sensor.depth_range == (0.1, 100.0)
        
    def test_vision_sensor_focal_length_calculation(self):
        """Test focal length calculation from field of view."""
        sensor = VisionSensor(
            sensor_id="camera_02",
            resolution=(640, 480),
            field_of_view=60.0
        )
        
        # For 60 degree FOV and 640 width: focal_length = 320 / tan(30°) ≈ 554
        expected_focal_length = 320.0 / np.tan(np.radians(30.0))
        assert abs(sensor.focal_length - expected_focal_length) < 1.0
        
    def test_vision_sensor_intrinsic_matrix(self):
        """Test camera intrinsic matrix calculation."""
        sensor = VisionSensor(
            sensor_id="camera_03",
            resolution=(640, 480)
        )
        
        intrinsic = sensor.intrinsic_matrix
        assert intrinsic.shape == (3, 3)
        # Check principal point is at image center
        assert abs(intrinsic[0, 2] - 320.0) < 0.1  # cx
        assert abs(intrinsic[1, 2] - 240.0) < 0.1  # cy
        
    def test_vision_sensor_reading(self):
        """Test vision sensor reading with environment data."""
        sensor = VisionSensor("camera_04")
        
        # Test with environment data
        environment_data = {
            'objects': [{'type': 'cube'}, {'type': 'sphere'}],
            'ambient_light': 0.8,
            'motion_detected': True,
            'dominant_color': [0.7, 0.3, 0.2]
        }
        
        reading = sensor.read_sensor(environment_data)
        
        assert isinstance(reading, SensorReading)
        assert reading.sensor_type == SensorType.VISION
        assert isinstance(reading.value, np.ndarray)
        assert len(reading.value) == 7  # object_count + brightness + depth + motion + RGB
        assert abs(reading.value[0] - 2) < 0.1  # Two objects (with noise)
        
    def test_vision_sensor_camera_parameters(self):
        """Test camera parameter retrieval."""
        sensor = VisionSensor(
            sensor_id="camera_05",
            resolution=(800, 600),
            field_of_view=75.0,
            depth_range=(0.5, 50.0)
        )
        
        params = sensor.get_camera_parameters()
        
        assert params['resolution'] == (800, 600)
        assert params['field_of_view'] == 75.0
        assert params['depth_range'] == (0.5, 50.0)
        assert 'intrinsic_matrix' in params
        assert 'position' in params


class TestAuditorySensor:
    """Test Auditory Sensor with spatial sound processing."""
    
    def test_auditory_sensor_creation(self):
        """Test AuditorySensor can be created with proper parameters."""
        sensor = AuditorySensor(
            sensor_id="mic_01",
            position=(0.0, 0.0, 1.7),
            frequency_range=(50.0, 16000.0),
            spatial_resolution=180
        )
        
        assert sensor.sensor_type == SensorType.AUDITORY
        assert sensor.frequency_range == (50.0, 16000.0)
        assert sensor.spatial_resolution == 180
        assert sensor.num_frequency_bins == 256
        
    def test_auditory_sensor_reading(self):
        """Test auditory sensor reading with sound sources."""
        sensor = AuditorySensor("mic_02")
        
        # Test with sound sources
        environment_data = {
            'sound_sources': [
                {
                    'position': [5.0, 0.0, 0.0],
                    'volume': 0.8,
                    'frequency': 440.0  # A4 note
                },
                {
                    'position': [0.0, -3.0, 0.0],
                    'volume': 0.6,
                    'frequency': 880.0  # A5 note
                }
            ],
            'ambient_noise': 0.05
        }
        
        reading = sensor.read_sensor(environment_data)
        
        assert isinstance(reading, SensorReading)
        assert reading.sensor_type == SensorType.AUDITORY
        assert isinstance(reading.value, np.ndarray)
        # Should have spatial features (8) + frequency features (8)
        assert len(reading.value) == 16
        
    def test_auditory_spatial_localization(self):
        """Test spatial localization processing."""
        sensor = AuditorySensor("mic_03")
        
        # Create test audio data with dominant signal in first sector
        audio_data = np.zeros(16)
        audio_data[2] = 0.8  # High energy in sector 2 (90-135 degrees)
        audio_data[8:] = np.random.uniform(0, 0.2, 8)  # Random frequency data
        
        localization = sensor.get_spatial_localization(audio_data)
        
        assert 'dominant_direction_degrees' in localization
        assert 'confidence' in localization
        assert 'energy_distribution' in localization
        assert localization['dominant_direction_degrees'] == 90.0  # Sector 2 * 45 degrees
        
    def test_auditory_frequency_processing(self):
        """Test frequency range processing."""
        sensor = AuditorySensor(
            sensor_id="mic_04",
            frequency_range=(100.0, 8000.0)
        )
        
        # Test frequency bin calculation
        assert len(sensor.frequency_bins) == sensor.num_frequency_bins
        assert sensor.frequency_bins[0] == 100.0
        assert sensor.frequency_bins[-1] == 8000.0


class TestTactileSensor:
    """Test Tactile Sensor for surface interaction."""
    
    def test_tactile_sensor_creation(self):
        """Test TactileSensor can be created with proper parameters."""
        sensor = TactileSensor(
            sensor_id="touch_01",
            position=(0.5, 0.0, 0.0),
            sensing_area=(0.02, 0.02),
            pressure_range=(0.0, 15.0)
        )
        
        assert sensor.sensor_type == SensorType.TOUCH
        assert sensor.sensing_area == (0.02, 0.02)
        assert sensor.pressure_range == (0.0, 15.0)
        assert sensor.spatial_resolution == (8, 8)
        
    def test_tactile_sensor_no_contact(self):
        """Test tactile sensor with no contact."""
        sensor = TactileSensor("touch_02")
        
        # No contact environment
        environment_data = {
            'contact_info': {
                'in_contact': False
            }
        }
        
        reading = sensor.read_sensor(environment_data)
        
        assert isinstance(reading, SensorReading)
        assert reading.sensor_type == SensorType.TOUCH
        assert not sensor.contact_detected
        assert sensor.contact_force == 0.0
        assert sensor.contact_area == 0.0
        
    def test_tactile_sensor_with_contact(self):
        """Test tactile sensor with surface contact."""
        sensor = TactileSensor("touch_03")
        
        # Contact environment
        environment_data = {
            'contact_info': {
                'in_contact': True,
                'pressure': 5.0,
                'contact_position': (0.3, 0.7),
                'texture_roughness': 0.2,
                'surface_temperature': 30.0
            }
        }
        
        reading = sensor.read_sensor(environment_data)
        
        assert isinstance(reading, SensorReading)
        assert sensor.contact_detected
        assert sensor.contact_force == 5.0
        assert sensor.contact_area > 0.0
        
        # Check tactile array features (64 spatial + 4 summary = 68 total)
        assert len(reading.value) == 68
        
    def test_tactile_contact_info(self):
        """Test detailed contact information retrieval."""
        sensor = TactileSensor("touch_04", sensing_area=(0.03, 0.03))
        
        # Simulate contact
        environment_data = {
            'contact_info': {
                'in_contact': True,
                'pressure': 2.5,
                'contact_position': (0.5, 0.5)
            }
        }
        
        sensor.read_sensor(environment_data)
        contact_info = sensor.get_contact_info()
        
        assert contact_info['contact_detected']
        assert contact_info['contact_force'] == 2.5
        assert contact_info['sensing_area'] == (0.03, 0.03)
        assert contact_info['spatial_resolution'] == (8, 8)
        assert len(contact_info['position']) == 3
        
    def test_tactile_texture_detection(self):
        """Test texture detection capabilities."""
        sensor = TactileSensor("touch_05")
        sensor.texture_detection = True
        
        # Different texture roughness
        smooth_env = {
            'contact_info': {
                'in_contact': True,
                'pressure': 3.0,
                'texture_roughness': 0.01  # Very smooth
            }
        }
        
        rough_env = {
            'contact_info': {
                'in_contact': True,
                'pressure': 3.0,
                'texture_roughness': 0.5  # Very rough
            }
        }
        
        smooth_reading = sensor.read_sensor(smooth_env)
        rough_reading = sensor.read_sensor(rough_env)
        
        # Rough surface should have higher variance in tactile array
        smooth_std = np.std(smooth_reading.value[:64])  # First 64 are spatial data
        rough_std = np.std(rough_reading.value[:64])
        
        assert rough_std > smooth_std


class TestMultiModalSensorManager:
    """Test Multi-Modal Sensor coordination and fusion."""
    
    def test_sensor_manager_creation(self):
        """Test MultiModalSensorManager can be created."""
        manager = MultiModalSensorManager()
        
        assert manager.sensor_fusion_enabled
        assert len(manager.sensors) == 0
        assert manager.sync_tolerance == 0.01
        
    def test_sensor_registration(self):
        """Test sensor registration and unregistration."""
        manager = MultiModalSensorManager()
        
        vision_sensor = VisionSensor("camera_01")
        audio_sensor = AuditorySensor("mic_01")
        tactile_sensor = TactileSensor("touch_01")
        
        # Register sensors
        assert manager.register_sensor(vision_sensor)
        assert manager.register_sensor(audio_sensor)
        assert manager.register_sensor(tactile_sensor)
        
        assert len(manager.sensors) == 3
        
        # Try to register same sensor again
        assert not manager.register_sensor(vision_sensor)
        
        # Unregister sensor
        assert manager.unregister_sensor("mic_01")
        assert len(manager.sensors) == 2
        assert not manager.unregister_sensor("nonexistent_sensor")
        
    def test_synchronized_readings(self):
        """Test synchronized multi-modal readings."""
        manager = MultiModalSensorManager()
        
        # Register multi-modal sensors
        vision_sensor = VisionSensor("camera_01")
        audio_sensor = AuditorySensor("mic_01") 
        tactile_sensor = TactileSensor("touch_01")
        
        manager.register_sensor(vision_sensor)
        manager.register_sensor(audio_sensor)
        manager.register_sensor(tactile_sensor)
        
        # Environment with multi-modal data
        environment_data = {
            'objects': [{'type': 'cube'}],
            'ambient_light': 0.6,
            'motion_detected': False,
            'sound_sources': [{'position': [1, 0, 0], 'volume': 0.7, 'frequency': 500}],
            'contact_info': {'in_contact': True, 'pressure': 2.0}
        }
        
        readings = manager.get_synchronized_readings(environment_data)
        
        assert len(readings) == 3
        assert "camera_01" in readings
        assert "mic_01" in readings
        assert "touch_01" in readings
        
        # Check all readings are recent (within sync tolerance)
        current_time = time.time()
        for reading in readings.values():
            assert current_time - reading.timestamp < manager.sync_tolerance
            
    def test_sensor_data_fusion(self):
        """Test multi-modal sensor data fusion."""
        manager = MultiModalSensorManager(sensor_fusion_enabled=True)
        
        # Create and register sensors
        vision_sensor = VisionSensor("camera_01")
        audio_sensor = AuditorySensor("mic_01")
        tactile_sensor = TactileSensor("touch_01")
        
        manager.register_sensor(vision_sensor)
        manager.register_sensor(audio_sensor)
        manager.register_sensor(tactile_sensor)
        
        # Get readings
        environment_data = {
            'objects': [{'type': 'sphere'}],
            'ambient_light': 0.8,
            'sound_sources': [{'position': [2, 1, 0], 'volume': 0.5, 'frequency': 1000}],
            'contact_info': {'in_contact': False}
        }
        
        readings = manager.get_synchronized_readings(environment_data)
        fused_data = manager.fuse_sensor_data(readings)
        
        assert 'timestamp' in fused_data
        assert 'modalities' in fused_data
        assert 'confidence' in fused_data
        assert 'fused_features' in fused_data
        
        # Check all modalities are present
        modalities = fused_data['modalities']
        assert 'vision' in modalities
        assert 'auditory' in modalities
        assert 'touch' in modalities
        
        # Check cross-modal features
        fused_features = fused_data['fused_features']
        assert 'audiovisual_correlation' in fused_features
        assert 'visuotactile_correlation' in fused_features
        
    def test_fusion_confidence_weighting(self):
        """Test confidence weighting in sensor fusion."""
        manager = MultiModalSensorManager()
        
        # Set custom modality weights
        manager.modality_weights = {
            SensorType.VISION: 0.5,
            SensorType.AUDITORY: 0.3,
            SensorType.TOUCH: 0.2
        }
        
        vision_sensor = VisionSensor("camera_01")
        manager.register_sensor(vision_sensor)
        
        # Create mock reading with known confidence
        vision_reading = vision_sensor.read_sensor({})
        vision_reading.confidence = 0.9
        
        readings = {"camera_01": vision_reading}
        fused_data = manager.fuse_sensor_data(readings)
        
        # Confidence should be weighted by modality weight
        expected_confidence = 0.9 * 0.5  # confidence * weight
        assert abs(fused_data['confidence'] - expected_confidence) < 0.01
        
    def test_system_status(self):
        """Test multi-modal system status reporting."""
        manager = MultiModalSensorManager()
        
        vision_sensor = VisionSensor("camera_01", position=(1.0, 0.0, 1.5))
        audio_sensor = AuditorySensor("mic_01", position=(0.0, 0.0, 1.7))
        
        manager.register_sensor(vision_sensor)
        manager.register_sensor(audio_sensor)
        
        status = manager.get_system_status()
        
        assert status['sensor_count'] == 2
        assert status['fusion_enabled']
        assert 'registered_sensors' in status
        assert 'modality_weights' in status
        assert 'sync_tolerance' in status
        
        # Check sensor details
        sensors = status['registered_sensors']
        assert 'camera_01' in sensors
        assert 'mic_01' in sensors
        assert sensors['camera_01']['type'] == 'vision'
        assert sensors['mic_01']['type'] == 'auditory'


class TestMultiModalIntegration:
    """Test integration of multi-modal sensors with acceptance criteria."""
    
    def test_agents_receive_multimodal_input(self):
        """
        Test acceptance criteria: Agents receive multi-modal sensory input
        
        This test verifies that an agent can successfully receive and process
        input from vision, auditory, and tactile sensors simultaneously.
        """
        # Create multi-modal sensor manager (representing agent's sensory system)
        manager = MultiModalSensorManager()
        
        # Create all three required sensor modalities
        vision_sensor = VisionSensor(
            sensor_id="agent_camera",
            position=(0.0, 0.0, 1.6),  # Eye level
            resolution=(640, 480),
            field_of_view=70.0
        )
        
        auditory_sensor = AuditorySensor(
            sensor_id="agent_microphone", 
            position=(0.0, 0.0, 1.65),  # Ear level
            frequency_range=(50.0, 15000.0)
        )
        
        tactile_sensor = TactileSensor(
            sensor_id="agent_hand",
            position=(0.5, 0.0, 1.2),  # Hand position
            sensing_area=(0.025, 0.025)
        )
        
        # Register all sensors with the agent
        assert manager.register_sensor(vision_sensor)
        assert manager.register_sensor(auditory_sensor)
        assert manager.register_sensor(tactile_sensor)
        
        # Simulate rich multi-modal environment
        environment_data = {
            # Visual scene
            'objects': [
                {'type': 'red_ball', 'position': [2, 0, 1]},
                {'type': 'blue_cube', 'position': [1, 1, 0.5]}
            ],
            'ambient_light': 0.7,
            'motion_detected': True,
            'dominant_color': [0.8, 0.2, 0.1],  # Reddish scene
            
            # Audio environment
            'sound_sources': [
                {
                    'position': [2, 0, 1],  # Same as red ball
                    'volume': 0.6,
                    'frequency': 800.0
                },
                {
                    'position': [-1, 2, 0],  # Off to the side
                    'volume': 0.4,
                    'frequency': 200.0
                }
            ],
            'ambient_noise': 0.1,
            
            # Tactile interaction
            'contact_info': {
                'in_contact': True,
                'pressure': 3.0,
                'contact_position': (0.4, 0.6),
                'texture_roughness': 0.3,
                'surface_temperature': 28.0
            }
        }
        
        # Agent receives multi-modal sensory input
        readings = manager.get_synchronized_readings(environment_data)
        
        # Verify agent receives input from all modalities
        assert len(readings) == 3, "Agent must receive input from all sensor modalities"
        
        # Verify vision input
        vision_reading = readings["agent_camera"]
        assert vision_reading.sensor_type == SensorType.VISION
        assert isinstance(vision_reading.value, np.ndarray)
        assert len(vision_reading.value) > 0
        assert vision_reading.confidence > 0
        
        # Verify auditory input
        audio_reading = readings["agent_microphone"]
        assert audio_reading.sensor_type == SensorType.AUDITORY
        assert isinstance(audio_reading.value, np.ndarray)
        assert len(audio_reading.value) > 0
        assert audio_reading.confidence > 0
        
        # Verify tactile input
        tactile_reading = readings["agent_hand"]
        assert tactile_reading.sensor_type == SensorType.TOUCH
        assert isinstance(tactile_reading.value, np.ndarray)
        assert len(tactile_reading.value) > 0
        assert tactile_reading.confidence > 0
        
        # Test sensor fusion for enhanced perception
        fused_data = manager.fuse_sensor_data(readings)
        
        assert 'modalities' in fused_data
        assert len(fused_data['modalities']) == 3
        assert 'vision' in fused_data['modalities']
        assert 'auditory' in fused_data['modalities'] 
        assert 'touch' in fused_data['modalities']
        
        # Verify cross-modal correlations are computed
        assert 'fused_features' in fused_data
        fused_features = fused_data['fused_features']
        assert 'audiovisual_correlation' in fused_features
        assert 'visuotactile_correlation' in fused_features
        
        # Verify overall confidence is reasonable
        assert 0.0 <= fused_data['confidence'] <= 1.0
        
        print("✓ ACCEPTANCE CRITERIA MET: Agents receive multi-modal sensory input")
        print(f"  - Vision sensor active: {vision_reading.confidence:.2f} confidence")
        print(f"  - Auditory sensor active: {audio_reading.confidence:.2f} confidence") 
        print(f"  - Tactile sensor active: {tactile_reading.confidence:.2f} confidence")
        print(f"  - Overall fusion confidence: {fused_data['confidence']:.2f}")
        
        return True


if __name__ == "__main__":
    # Run basic functionality test
    test_integration = TestMultiModalIntegration()
    test_integration.test_agents_receive_multimodal_input()
    print("Multi-Modal Virtual Sensors implementation successful!")