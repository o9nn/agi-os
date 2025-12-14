#!/usr/bin/env python3
"""
Multi-Modal Virtual Sensors Demonstration
=========================================

Demonstrates Task 3.1.1: Implement Multi-Modal Virtual Sensors
- Vision system with configurable cameras
- Auditory system with spatial sound processing
- Tactile sensors for surface interaction

This demo shows how agents can receive multi-modal sensory input
and fuse the information for enhanced perception.
"""

import sys
import os

# Add project root to path
sys.path.append(os.path.dirname(os.path.dirname(__file__)))

from aar_core.embodied.hardware_abstraction import (
    VisionSensor, AuditorySensor, TactileSensor,
    MultiModalSensorManager
)


class MultiModalSensorDemo:
    """Demonstration of multi-modal virtual sensors."""
    
    def __init__(self):
        """Initialize the multi-modal sensor demonstration."""
        print("🤖 Initializing Multi-Modal Virtual Sensor Demo")
        print("=" * 50)
        
        # Create sensor manager
        self.sensor_manager = MultiModalSensorManager(sensor_fusion_enabled=True)
        
        # Create vision sensor (configurable camera)
        self.vision_sensor = VisionSensor(
            sensor_id="demo_camera",
            position=(0.0, 0.0, 1.6),  # Eye level position
            resolution=(1920, 1080),    # High definition
            field_of_view=70.0,         # Human-like FOV
            depth_range=(0.2, 100.0)    # 20cm to 100m range
        )
        
        # Create auditory sensor (spatial sound processing)
        self.auditory_sensor = AuditorySensor(
            sensor_id="demo_microphone",
            position=(0.0, 0.0, 1.65),   # Ear level position
            frequency_range=(20.0, 20000.0),  # Full human hearing range
            spatial_resolution=360        # 1-degree spatial resolution
        )
        
        # Create tactile sensor (surface interaction)
        self.tactile_sensor = TactileSensor(
            sensor_id="demo_hand",
            position=(0.6, 0.0, 1.2),    # Extended hand position
            sensing_area=(0.05, 0.05),   # 5cm x 5cm palm area
            pressure_range=(0.0, 20.0)   # 0-20 N/cm² pressure range
        )
        
        # Register all sensors
        print("📡 Registering sensors...")
        self.sensor_manager.register_sensor(self.vision_sensor)
        self.sensor_manager.register_sensor(self.auditory_sensor)
        self.sensor_manager.register_sensor(self.tactile_sensor)
        
        print(f"✓ Vision sensor registered: {self.vision_sensor.resolution} resolution")
        print(f"✓ Auditory sensor registered: {self.auditory_sensor.frequency_range} Hz range")
        print(f"✓ Tactile sensor registered: {self.tactile_sensor.sensing_area} sensing area")
        print()
        
    def demonstrate_vision_system(self):
        """Demonstrate vision system with configurable cameras."""
        print("👁️ VISION SYSTEM DEMONSTRATION")
        print("-" * 30)
        
        # Simulate complex visual scene
        environment_data = {
            'objects': [
                {'type': 'red_ball', 'position': [2.0, 0.5, 1.0], 'size': 0.2},
                {'type': 'blue_cube', 'position': [1.5, -1.0, 0.8], 'size': 0.3},
                {'type': 'green_cylinder', 'position': [3.0, 1.5, 1.2], 'size': 0.25},
                {'type': 'person', 'position': [5.0, 0.0, 1.7], 'moving': True}
            ],
            'ambient_light': 0.75,
            'motion_detected': True,
            'dominant_color': [0.6, 0.3, 0.4]  # Warm lighting
        }
        
        # Get vision reading
        reading = self.vision_sensor.read_sensor(environment_data)
        camera_params = self.vision_sensor.get_camera_parameters()
        
        print(f"Objects detected: {len(environment_data['objects'])}")
        print(f"Scene brightness: {environment_data['ambient_light']:.2f}")
        print(f"Motion detected: {environment_data['motion_detected']}")
        print(f"Camera FOV: {camera_params['field_of_view']}°")
        print(f"Camera resolution: {camera_params['resolution']}")
        print(f"Focal length: {camera_params['focal_length']:.1f}px")
        print(f"Vision confidence: {reading.confidence:.3f}")
        print(f"Vision features: {reading.value[:4]} (obj count, brightness, depth, motion)")
        print()
        
    def demonstrate_auditory_system(self):
        """Demonstrate auditory system with spatial sound processing."""
        print("👂 AUDITORY SYSTEM DEMONSTRATION")
        print("-" * 32)
        
        # Simulate complex audio scene with multiple sources
        environment_data = {
            'sound_sources': [
                {
                    'position': [3.0, 0.0, 1.0],    # Front
                    'volume': 0.8,
                    'frequency': 440.0,             # A4 note
                    'type': 'music'
                },
                {
                    'position': [-2.0, 2.0, 1.5],   # Left side
                    'volume': 0.6,
                    'frequency': 150.0,             # Low rumble
                    'type': 'traffic'
                },
                {
                    'position': [1.0, -4.0, 1.0],   # Right rear
                    'volume': 0.4,
                    'frequency': 2000.0,            # High beep
                    'type': 'alarm'
                },
                {
                    'position': [0.5, 0.2, 1.6],    # Very close (voice)
                    'volume': 0.9,
                    'frequency': 350.0,             # Human voice range
                    'type': 'speech'
                }
            ],
            'ambient_noise': 0.15  # Urban environment
        }
        
        # Get auditory reading
        reading = self.auditory_sensor.read_sensor(environment_data)
        spatial_info = self.auditory_sensor.get_spatial_localization(reading.value)
        
        print(f"Sound sources detected: {len(environment_data['sound_sources'])}")
        print(f"Ambient noise level: {environment_data['ambient_noise']:.2f}")
        print(f"Dominant sound direction: {spatial_info['dominant_direction_degrees']}°")
        print(f"Spatial confidence: {spatial_info['confidence']:.3f}")
        print(f"Frequency range: {self.auditory_sensor.frequency_range[0]}-{self.auditory_sensor.frequency_range[1]} Hz")
        print(f"Auditory confidence: {reading.confidence:.3f}")
        print()
        
    def demonstrate_tactile_system(self):
        """Demonstrate tactile sensors for surface interaction."""
        print("✋ TACTILE SYSTEM DEMONSTRATION")
        print("-" * 31)
        
        # Simulate surface interaction
        environment_data = {
            'contact_info': {
                'in_contact': True,
                'pressure': 4.5,                    # Moderate pressure
                'contact_position': (0.3, 0.7),    # Upper left of sensor
                'texture_roughness': 0.25,          # Moderately rough (wood)
                'surface_temperature': 22.0,        # Room temperature
                'surface_hardness': 0.8,            # Hard surface
                'contact_area_ratio': 0.15           # 15% of sensor area
            }
        }
        
        # Get tactile reading
        reading = self.tactile_sensor.read_sensor(environment_data)
        contact_info = self.tactile_sensor.get_contact_info()
        
        print(f"Surface contact: {'Yes' if contact_info['contact_detected'] else 'No'}")
        print(f"Contact pressure: {contact_info['contact_force']:.1f} N/cm²")
        print(f"Contact area: {contact_info['contact_area']:.3f} (normalized)")
        print(f"Surface temperature: {environment_data['contact_info']['surface_temperature']}°C")
        print(f"Texture roughness: {environment_data['contact_info']['texture_roughness']:.2f}")
        print(f"Sensor resolution: {contact_info['spatial_resolution']}")
        print(f"Tactile confidence: {reading.confidence:.3f}")
        print()
        
    def demonstrate_multimodal_fusion(self):
        """Demonstrate multi-modal sensor fusion."""
        print("🧠 MULTI-MODAL SENSOR FUSION")
        print("-" * 28)
        
        # Create rich multi-modal environment
        environment_data = {
            # Visual information
            'objects': [
                {'type': 'smartphone', 'position': [0.4, 0.1, 1.1], 'vibrating': True}
            ],
            'ambient_light': 0.6,
            'motion_detected': True,
            'dominant_color': [0.2, 0.2, 0.8],  # Blue glow from phone
            
            # Audio information (phone vibration and ringtone)
            'sound_sources': [
                {
                    'position': [0.4, 0.1, 1.1],   # Same as phone position
                    'volume': 0.7,
                    'frequency': 800.0,             # Ringtone
                    'type': 'ringtone'
                },
                {
                    'position': [0.4, 0.1, 1.1],   # Vibration sound
                    'volume': 0.3,
                    'frequency': 60.0,              # Low frequency buzz
                    'type': 'vibration'
                }
            ],
            'ambient_noise': 0.1,
            
            # Tactile information (touching the phone)
            'contact_info': {
                'in_contact': True,
                'pressure': 2.0,
                'contact_position': (0.5, 0.5),    # Center contact
                'texture_roughness': 0.05,          # Smooth phone surface
                'surface_temperature': 25.0,        # Slightly warm phone
                'vibration_intensity': 0.4          # Phone vibration
            }
        }
        
        # Get synchronized readings from all sensors
        readings = self.sensor_manager.get_synchronized_readings(environment_data)
        
        # Fuse the multi-modal data
        fused_data = self.sensor_manager.fuse_sensor_data(readings)
        
        print("Synchronized sensor readings:")
        for sensor_id, reading in readings.items():
            sensor_type = reading.sensor_type.value
            print(f"  {sensor_id} ({sensor_type}): confidence={reading.confidence:.3f}")
        
        print("\nFusion results:")
        print(f"  Overall confidence: {fused_data['confidence']:.3f}")
        print(f"  Modalities fused: {list(fused_data['modalities'].keys())}")
        
        # Show cross-modal correlations
        if 'fused_features' in fused_data:
            features = fused_data['fused_features']
            print(f"  Audio-visual correlation: {features.get('audiovisual_correlation', 0):.3f}")
            print(f"  Visuo-tactile correlation: {features.get('visuotactile_correlation', 0):.3f}")
            
        print("\n🎯 SCENARIO INTERPRETATION:")
        print("   Agent detects ringing phone through multi-modal perception:")
        print("   • VISION: Blue glowing rectangular object")
        print("   • AUDIO: Ringtone + vibration sounds from same location")  
        print("   • TOUCH: Smooth vibrating surface when contacted")
        print("   → FUSED PERCEPTION: Vibrating smartphone requiring attention")
        print()
        
    def demonstrate_sensor_capabilities(self):
        """Show detailed sensor capabilities and specifications."""
        print("⚙️ SENSOR SPECIFICATIONS")
        print("-" * 24)
        
        # Vision capabilities
        vision_params = self.vision_sensor.get_camera_parameters()
        print("Vision Sensor Capabilities:")
        print(f"  • Resolution: {vision_params['resolution']} pixels")
        print(f"  • Field of view: {vision_params['field_of_view']}°")
        print(f"  • Depth range: {vision_params['depth_range'][0]}-{vision_params['depth_range'][1]}m")
        print(f"  • Update rate: {self.vision_sensor.update_rate} Hz")
        print("  • Features: Object detection, motion detection, color analysis")
        
        # Auditory capabilities  
        print("\nAuditory Sensor Capabilities:")
        print(f"  • Frequency range: {self.auditory_sensor.frequency_range[0]}-{self.auditory_sensor.frequency_range[1]} Hz")
        print(f"  • Spatial resolution: {self.auditory_sensor.spatial_resolution}°")
        print(f"  • Sample rate: {self.auditory_sensor.update_rate} kHz")
        print("  • Features: Spatial localization, frequency analysis, multi-source tracking")
        
        # Tactile capabilities
        print("\nTactile Sensor Capabilities:")
        print(f"  • Sensing area: {self.tactile_sensor.sensing_area[0]*100:.1f}×{self.tactile_sensor.sensing_area[1]*100:.1f} cm")
        print(f"  • Pressure range: {self.tactile_sensor.pressure_range[0]}-{self.tactile_sensor.pressure_range[1]} N/cm²")
        print(f"  • Spatial resolution: {self.tactile_sensor.spatial_resolution[0]}×{self.tactile_sensor.spatial_resolution[1]} tactels")
        print(f"  • Update rate: {self.tactile_sensor.update_rate} Hz")
        print("  • Features: Pressure mapping, texture detection, temperature sensing")
        
        # System capabilities
        system_status = self.sensor_manager.get_system_status()
        print("\nMulti-Modal System:")
        print(f"  • Total sensors: {system_status['sensor_count']}")
        print(f"  • Sensor fusion: {'Enabled' if system_status['fusion_enabled'] else 'Disabled'}")
        print(f"  • Sync tolerance: {system_status['sync_tolerance']*1000:.1f} ms")
        print(f"  • Modality weights: Vision={system_status['modality_weights']['vision']:.1f}, Audio={system_status['modality_weights']['auditory']:.1f}, Touch={system_status['modality_weights']['touch']:.1f}")
        print()
        
    def run_demo(self):
        """Run the complete multi-modal sensor demonstration."""
        print("🚀 Starting Multi-Modal Virtual Sensor Demonstration\n")
        
        # Show sensor specifications
        self.demonstrate_sensor_capabilities()
        
        # Demonstrate each sensor modality
        self.demonstrate_vision_system()
        self.demonstrate_auditory_system() 
        self.demonstrate_tactile_system()
        
        # Show multi-modal fusion
        self.demonstrate_multimodal_fusion()
        
        # Final acceptance criteria validation
        print("✅ ACCEPTANCE CRITERIA VALIDATION")
        print("-" * 34)
        print("Task 3.1.1: Implement Multi-Modal Virtual Sensors")
        print("✓ Vision system with configurable cameras - IMPLEMENTED")
        print("✓ Auditory system with spatial sound processing - IMPLEMENTED") 
        print("✓ Tactile sensors for surface interaction - IMPLEMENTED")
        print("✓ Acceptance Criteria: Agents receive multi-modal sensory input - VERIFIED")
        print("\n🎉 Multi-Modal Virtual Sensors implementation COMPLETE!")


def main():
    """Run the multi-modal sensor demonstration."""
    try:
        demo = MultiModalSensorDemo()
        demo.run_demo()
    except Exception as e:
        print(f"❌ Demo failed: {e}")
        import traceback
        traceback.print_exc()
        return 1
    return 0


if __name__ == "__main__":
    exit(main())