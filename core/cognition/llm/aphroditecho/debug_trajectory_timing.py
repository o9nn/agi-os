#!/usr/bin/env python3
"""
Detailed trajectory timing diagnosis
"""

import time
from aar_core.embodied import (
    EmbodiedAgent, HierarchicalMotorController
)

def debug_trajectory_timing():
    """Debug the detailed timing of trajectory execution"""
    print("=== TRAJECTORY TIMING DIAGNOSIS ===")
    
    agent = EmbodiedAgent("timing_agent", (0, 0, 1))
    controller = HierarchicalMotorController(agent)
    
    # Create trajectory with the same parameters as the test
    objective = {
        'type': 'reach',
        'joint_targets': {'right_shoulder': 0.3, 'right_elbow': -0.5},
        'duration': 1.0,  # Match the test duration
        'priority': 0.8,
        'coordination_groups': [['right_shoulder', 'right_elbow']]
    }
    
    trajectory = controller.trajectory_generator.generate_trajectory("timing_debug", objective)
    
    print(f"Generated trajectory with duration: {trajectory.total_duration}")
    
    # Look at trajectory points in detail
    shoulder_points = trajectory.joint_trajectories['right_shoulder']
    print(f"Shoulder trajectory has {len(shoulder_points)} points")
    
    # Show first few and last few points
    print("First 10 trajectory points:")
    for i in range(min(10, len(shoulder_points))):
        t, angle = shoulder_points[i]
        print(f"  Point {i}: t={t:.3f}, angle={angle:.4f}")
    
    print("Last 10 trajectory points:")
    for i in range(max(0, len(shoulder_points)-10), len(shoulder_points)):
        t, angle = shoulder_points[i]
        print(f"  Point {i}: t={t:.3f}, angle={angle:.4f}")
    
    # Now test the motor executor timing
    executor = controller.motor_executor
    success = executor.execute_trajectory(trajectory)
    
    if success:
        print(f"\n✓ Trajectory execution started at {time.time():.3f}")
        
        # Simulate the timing that happens in the real test
        print("Simulating executor timing:")
        for step in [0, 25, 50, 75, 99]:  # Key points in the trajectory including completion
            dt = 0.01
            trajectory_time = step * dt
            
            print(f"\nStep {step}: trajectory_time={trajectory_time:.3f}")
            
            # Call the interpolation method directly
            shoulder_target = executor._interpolate_trajectory_point(
                shoulder_points, trajectory_time
            )
            
            print(f"  Interpolated shoulder target: {shoulder_target:.4f}")
            
            # Also check what the executor would calculate
            targets = executor._interpolate_trajectory_targets(trajectory_time)
            print(f"  Executor targets: {targets}")
    
    return True

if __name__ == "__main__":
    debug_trajectory_timing()