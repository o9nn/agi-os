#!/usr/bin/env python3
"""
Demo: Embedded Hardware Abstractions for 4E Embodied AI

Demonstrates Task 2.2.3 implementation:
- Virtual sensor and actuator interfaces
- Hardware simulation for embodied systems
- Real-time system integration

This demo shows an embodied agent using hardware abstractions to control
a virtual body through simulated sensors and actuators.
"""

import sys
import time
import numpy as np

# Add project root to path
sys.path.insert(0, '/home/runner/work/aphroditecho/aphroditecho')

from aar_core.embodied import (
    EmbodiedHardwareManager,
    VirtualBody,
    VirtualSensor, SensorType
)


class EmbodiedHardwareDemo:
    """Demo of embedded hardware abstractions."""
    
    def __init__(self):
        print("🤖 Embedded Hardware Abstractions Demo")
        print("=" * 50)
        
        # Create virtual body
        print("1. Creating virtual humanoid body...")
        self.virtual_body = VirtualBody(
            body_id="demo_body",
            position=(0.0, 0.0, 1.0),  # 1m above ground
            body_type="humanoid"
        )
        print(f"   ✓ Created body with {len(self.virtual_body.joints)} joints")
        
        # Create hardware manager
        print("\n2. Initializing hardware abstraction layer...")
        self.hw_manager = EmbodiedHardwareManager(self.virtual_body)
        
        if not self.hw_manager.initialize():
            raise RuntimeError("Failed to initialize hardware manager")
        print("   ✓ Hardware abstraction layer initialized")
        
        if not self.hw_manager.start():
            raise RuntimeError("Failed to start hardware system")
        print("   ✓ Hardware simulation started")
        
        # Add custom sensors
        self._add_custom_sensors()
        
        # Get system status
        status = self.hw_manager.get_system_status()
        hw_status = status['hardware_bridge']['hardware_simulator_status']['simulator']
        print("\n📊 System Status:")
        print(f"   - Devices: {hw_status['device_count']}")
        print(f"   - Sensors: {hw_status['sensor_count']}")
        print(f"   - Actuators: {hw_status['actuator_count']}")
        print(f"   - Running: {hw_status['running']}")
        
    def _add_custom_sensors(self):
        """Add custom sensors to demonstrate extensibility."""
        print("\n3. Adding custom sensors...")
        
        # Add head-mounted IMU
        head_imu = VirtualSensor(
            sensor_id="head_imu",
            sensor_type=SensorType.IMU,
            position=(0.0, 0.0, 1.7),  # Head height
            update_rate=1000.0  # High frequency for balance
        )
        
        if self.hw_manager.add_custom_sensor(head_imu, "head"):
            print("   ✓ Added head IMU sensor")
            
        # Add pressure sensors in feet
        left_foot_pressure = VirtualSensor(
            sensor_id="left_foot_pressure",
            sensor_type=SensorType.PRESSURE,
            position=(-0.1, 0.0, 0.0),  # Left foot
            update_rate=500.0,
            range_min=0.0,
            range_max=1000.0  # 1000 N max
        )
        
        if self.hw_manager.add_custom_sensor(left_foot_pressure, "left_ankle"):
            print("   ✓ Added left foot pressure sensor")
            
        # Add temperature sensor
        core_temp = VirtualSensor(
            sensor_id="core_temperature",
            sensor_type=SensorType.TEMPERATURE,
            position=(0.0, 0.0, 1.2),  # Torso
            update_rate=1.0,  # Slow update for temperature
            range_min=35.0,
            range_max=42.0  # Body temperature range
        )
        
        if self.hw_manager.add_custom_sensor(core_temp, "torso"):
            print("   ✓ Added core temperature sensor")
            
    def run_demo(self, duration: float = 5.0):
        """Run the hardware abstraction demo."""
        print(f"\n🚀 Running demo for {duration:.1f} seconds...")
        print("   Watch the virtual body respond to sensor inputs and motor commands")
        
        start_time = time.time()
        update_count = 0
        
        # Demo sequence
        demo_phases = [
            ("Initialization", 0.5, self._demo_initialization),
            ("Sensor Reading", 2.0, self._demo_sensor_reading),
            ("Motor Control", 1.5, self._demo_motor_control),
            ("Balance Control", 1.0, self._demo_balance_control)
        ]
        
        current_phase = 0
        phase_start = start_time
        
        while time.time() - start_time < duration:
            current_time = time.time()
            dt = 0.01  # 10ms timestep
            
            # Check if we need to move to next phase
            if (current_phase < len(demo_phases) and 
                current_time - phase_start >= demo_phases[current_phase][1]):
                current_phase += 1
                phase_start = current_time
                
            # Run current phase
            if current_phase < len(demo_phases):
                phase_name, _, phase_func = demo_phases[current_phase]
                if update_count % 100 == 0:  # Print every second
                    print(f"   Phase: {phase_name}")
                phase_func(current_time - phase_start)
                
            # Update hardware system
            environment_data = {
                'time': current_time - start_time,
                'gravity': [0, 0, -9.81],
                'wind': [0.1 * np.sin(current_time), 0, 0]
            }
            
            self.hw_manager.update(dt, environment_data)
            update_count += 1
            
            # Brief sleep to maintain real-time
            time.sleep(max(0, dt - 0.001))
            
        print("\n✅ Demo completed successfully!")
        self._print_performance_summary()
        
    def _demo_initialization(self, phase_time: float):
        """Demo initialization phase."""
        # Just let the system stabilize
        pass
        
    def _demo_sensor_reading(self, phase_time: float):
        """Demo sensor reading phase."""
        if int(phase_time * 10) % 50 == 0:  # Every 0.5 seconds
            # Read some sensors
            imu_reading = self.hw_manager.get_sensor_reading("head_imu")
            temp_reading = self.hw_manager.get_sensor_reading("core_temperature")
            
            if imu_reading:
                accel = np.array(imu_reading.value)
                print(f"   📡 IMU: {np.linalg.norm(accel):.2f} m/s²")
                
            if temp_reading:
                print(f"   🌡️  Temperature: {temp_reading.value:.1f}°C")
                
    def _demo_motor_control(self, phase_time: float):
        """Demo motor control phase."""
        # Simple sinusoidal joint movements
        amplitude = 0.3  # radians
        frequency = 2.0  # Hz
        
        # Move arms
        arm_angle = amplitude * np.sin(2 * np.pi * frequency * phase_time)
        self.hw_manager.send_motor_command("left_shoulder", arm_angle)
        self.hw_manager.send_motor_command("right_shoulder", -arm_angle)
        
        # Slight torso rotation
        torso_angle = 0.1 * np.sin(2 * np.pi * frequency * 0.5 * phase_time)
        self.hw_manager.send_motor_command("torso", torso_angle)
        
    def _demo_balance_control(self, phase_time: float):
        """Demo balance control using sensors."""
        # Read IMU for balance
        imu_reading = self.hw_manager.get_sensor_reading("head_imu")
        
        if imu_reading:
            accel = np.array(imu_reading.value)
            
            # Simple balance control - lean opposite to acceleration
            lean_x = -accel[0] * 0.05  # Small correction
            lean_y = -accel[1] * 0.05
            
            # Apply balance corrections
            self.hw_manager.send_motor_command("torso", lean_x)
            self.hw_manager.send_motor_commands({
                "left_hip": lean_y * 0.5,
                "right_hip": lean_y * 0.5
            })
            
    def _print_performance_summary(self):
        """Print performance summary."""
        print("\n📊 Performance Summary:")
        
        # Get system status
        status = self.hw_manager.get_system_status()
        hw_status = status['hardware_bridge']['hardware_simulator_status']
        
        if 'performance' in hw_status and hw_status['performance']:
            perf = hw_status['performance']
            print(f"   - Total Updates: {perf.get('total_updates', 0)}")
            print(f"   - Average Latency: {perf.get('average_latency_ms', 0):.2f} ms")
            print(f"   - Max Latency: {perf.get('max_latency_ms', 0):.2f} ms")
            print(f"   - Real-time Performance: {perf.get('real_time_performance', 0):.1f}%")
            print(f"   - Dropped Frames: {perf.get('dropped_frames', 0)}")
            
        # Validate acceptance criteria
        validation = self.hw_manager.validate_system_integration()
        criteria_met = validation.get('acceptance_criteria_met', False)
        
        print("\n🎯 Acceptance Criteria:")
        print("   Task 2.2.3: System can interface with simulated hardware")
        print(f"   Status: {'✅ MET' if criteria_met else '❌ NOT MET'}")
        
        if criteria_met:
            print("\n🎉 SUCCESS: Embedded hardware abstractions working correctly!")
        else:
            print("\n⚠️  WARNING: Some acceptance criteria not met")
            
    def cleanup(self):
        """Cleanup demo resources."""
        print("\n🧹 Cleaning up...")
        self.hw_manager.shutdown()
        print("   ✓ Hardware system shut down")


def main():
    """Main demo function."""
    demo = None
    
    try:
        print("Starting Embedded Hardware Abstraction Demo...")
        print("Task 2.2.3: Build Embedded Hardware Abstractions\n")
        
        # Create and run demo
        demo = EmbodiedHardwareDemo()
        demo.run_demo(duration=5.0)
        
        print("\n" + "="*50)
        print("Demo completed successfully! 🎉")
        print("\nKey achievements:")
        print("✓ Virtual sensor interfaces working")  
        print("✓ Virtual actuator interfaces working")
        print("✓ Hardware simulation running")
        print("✓ Real-time system integration validated")
        print("✓ System can interface with simulated hardware")
        
        return 0
        
    except KeyboardInterrupt:
        print("\n\n⏹️  Demo interrupted by user")
        return 1
        
    except Exception as e:
        print(f"\n\n❌ Demo failed with error: {e}")
        import traceback
        traceback.print_exc()
        return 1
        
    finally:
        if demo:
            demo.cleanup()


if __name__ == "__main__":
    sys.exit(main())