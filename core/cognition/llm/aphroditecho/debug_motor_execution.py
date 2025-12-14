#!/usr/bin/env python3
"""
Diagnostic test for trajectory execution
"""

from aar_core.embodied import (
    EmbodiedAgent, HierarchicalMotorController, 
    MidLevelTrajectoryGenerator
)

def debug_trajectory_generation():
    """Debug trajectory generation step by step"""
    print("=== TRAJECTORY GENERATION DIAGNOSIS ===")
    
    agent = EmbodiedAgent("debug_agent", (0, 0, 1))
    
    # Check initial joint states
    initial_states = agent.get_all_joint_states()
    print("Initial joint states:")
    for joint_id, state in initial_states.items():
        if 'shoulder' in joint_id or 'elbow' in joint_id:
            print(f"  {joint_id}: {state['angle']:.3f}")
    
    # Create trajectory generator
    traj_gen = MidLevelTrajectoryGenerator(agent.virtual_body)
    
    # Test objective
    objective = {
        'type': 'reach',
        'joint_targets': {'right_shoulder': 0.3, 'right_elbow': -0.5},
        'duration': 1.5,
        'priority': 0.8,
        'coordination_groups': [['right_shoulder', 'right_elbow']]
    }
    
    print(f"\nObjective targets: {objective['joint_targets']}")
    
    # Generate trajectory
    trajectory = traj_gen.generate_trajectory("debug", objective)
    
    if trajectory:
        print(f"✓ Trajectory generated with duration: {trajectory.total_duration}")
        print(f"✓ Joint trajectories: {list(trajectory.joint_trajectories.keys())}")
        
        # Check first and last trajectory points
        for joint_id, traj_points in trajectory.joint_trajectories.items():
            if traj_points:
                first_point = traj_points[0]
                last_point = traj_points[-1]
                print(f"  {joint_id}: start={first_point[1]:.3f}, end={last_point[1]:.3f}")
    else:
        print("❌ No trajectory generated")
        return False
    
    return True

def debug_motor_executor():
    """Debug motor executor step by step"""
    print("\n=== MOTOR EXECUTOR DIAGNOSIS ===")
    
    agent = EmbodiedAgent("debug_agent", (0, 0, 1))
    controller = HierarchicalMotorController(agent)
    
    # Simple manual trajectory
    from aar_core.embodied.hierarchical_motor_control import Trajectory
    
    # Create a simple trajectory from current position to target
    current_shoulder = agent.get_joint_state('right_shoulder')['angle']
    current_elbow = agent.get_joint_state('right_elbow')['angle']
    
    target_shoulder = 0.2
    target_elbow = -0.3
    
    print(f"Current: shoulder={current_shoulder:.3f}, elbow={current_elbow:.3f}")
    print(f"Target: shoulder={target_shoulder:.3f}, elbow={target_elbow:.3f}")
    
    # Create manual trajectory points
    duration = 1.0
    dt = 0.01
    steps = int(duration / dt)
    
    shoulder_traj = []
    elbow_traj = []
    
    for i in range(steps):
        t = i * dt
        progress = t / duration
        
        # Linear interpolation
        shoulder_angle = current_shoulder + (target_shoulder - current_shoulder) * progress
        elbow_angle = current_elbow + (target_elbow - current_elbow) * progress
        
        shoulder_traj.append((t, shoulder_angle))
        elbow_traj.append((t, elbow_angle))
    
    manual_trajectory = Trajectory(
        trajectory_id="manual_test",
        joint_trajectories={
            'right_shoulder': shoulder_traj,
            'right_elbow': elbow_traj
        },
        total_duration=duration
    )
    
    # Execute the manual trajectory
    executor = controller.motor_executor
    success = executor.execute_trajectory(manual_trajectory)
    
    print(f"✓ Trajectory execution started: {success}")
    
    # Run a few execution steps
    from aar_core.arena.simulation_engine import ArenaPhysics, ArenaEnvironment
    physics = ArenaPhysics()
    environment = ArenaEnvironment()
    
    print("Execution progress:")
    for step in range(0, 101, 20):  # Every 20 steps
        dt = 0.01
        
        # Update executor
        exec_status = executor.update_execution(dt)
        
        # Update agent
        agent.update(dt, physics, environment)
        
        # Get current states
        current_states = agent.get_all_joint_states()
        shoulder_actual = current_states['right_shoulder']['angle']
        elbow_actual = current_states['right_elbow']['angle']
        
        # Get current targets
        targets = executor.current_trajectory_targets
        shoulder_target = targets.get('right_shoulder', 0.0)
        elbow_target = targets.get('right_elbow', 0.0)
        
        print(f"  Step {step:3d}: S_target={shoulder_target:.3f}, S_actual={shoulder_actual:.3f}")
        print(f"           E_target={elbow_target:.3f}, E_actual={elbow_actual:.3f}")
        
        if exec_status['status'] == 'completed':
            print(f"  ✓ Completed at step {step}")
            break
    
    return True

if __name__ == "__main__":
    debug_trajectory_generation()
    debug_motor_executor()