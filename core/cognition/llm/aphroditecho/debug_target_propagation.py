#!/usr/bin/env python3
"""
Debug exactly what targets are being sent to PID controller
"""

from aar_core.embodied import (
    EmbodiedAgent, HierarchicalMotorController, 
    MotorGoal, MotorGoalType
)

def debug_target_propagation():
    """Debug what targets are actually sent to PID controller"""
    print("=== TARGET PROPAGATION DIAGNOSIS ===")
    
    agent = EmbodiedAgent("target_debug", (0, 0, 1))
    controller = HierarchicalMotorController(agent)
    
    # Simple reach goal with explicit targets
    goal = MotorGoal(
        goal_id="debug_reach",
        goal_type=MotorGoalType.REACH_POSITION,
        target_data={
            'target_position': (0.3, 0.1, 1.0),
            'end_effector': 'right_elbow',
            'joint_targets': {'right_shoulder': 0.3, 'right_elbow': -0.5},  # Direct targets
            'duration': 0.5  # Very short for quick test
        },
        priority=1.0
    )
    
    controller.add_motor_goal(goal)
    controller.start_control_loop()
    
    print("Checking what goals produce...")
    
    # Step 1: Check goal planning output
    objectives = controller.goal_planner.plan_motor_objectives()
    print(f"Goal planner objectives: {objectives}")
    
    # Step 2: Check trajectory generation
    if objectives:
        goal_id, objective = next(iter(objectives.items()))
        print(f"\nObjective joint targets: {objective.get('joint_targets', {})}")
        
        # Generate trajectory
        trajectory = controller.trajectory_generator.generate_trajectory(goal_id, objective)
        if trajectory:
            print(f"Generated trajectory duration: {trajectory.total_duration}")
            for joint_id, points in trajectory.joint_trajectories.items():
                if points:
                    start_val = points[0][1]
                    end_val = points[-1][1]
                    print(f"  {joint_id}: {start_val:.4f} -> {end_val:.4f}")
            
            # Step 3: Check motor executor interpolation
            executor = controller.motor_executor
            executor.execute_trajectory(trajectory)
            
            # Test interpolation at different times
            print("\nTesting trajectory interpolation:")
            for t_frac in [0.0, 0.25, 0.5, 0.75, 1.0]:
                trajectory_time = t_frac * trajectory.total_duration
                targets = executor._interpolate_trajectory_targets(trajectory_time)
                print(f"  t={trajectory_time:.3f}: {targets}")
            
            # Step 4: Check what gets sent to embodied agent
            print("\nTesting what gets sent to embodied agent:")
            from aar_core.arena.simulation_engine import ArenaPhysics, ArenaEnvironment
            physics = ArenaPhysics()
            environment = ArenaEnvironment()
            
            # Run a few control steps
            for step in range(5):
                dt = 0.01
                
                # Before control update
                agent.motor_commands.copy()
                before_states = {
                    'right_shoulder': agent.get_joint_state('right_shoulder')['angle'],
                    'right_elbow': agent.get_joint_state('right_elbow')['angle']
                }
                
                # Control update
                status = controller.update(dt)
                
                # After control update
                after_commands = agent.motor_commands.copy()
                {
                    'right_shoulder': agent.get_joint_state('right_shoulder')['angle'],
                    'right_elbow': agent.get_joint_state('right_elbow')['angle']
                }
                
                # Physics update
                agent.update(dt, physics, environment)
                
                # Final states after physics
                final_states = {
                    'right_shoulder': agent.get_joint_state('right_shoulder')['angle'],
                    'right_elbow': agent.get_joint_state('right_elbow')['angle']
                }
                
                print(f"\n  Step {step}:")
                print(f"    Commands: {after_commands}")
                print(f"    Exec targets: {status.get('execution_status', {}).get('targets', {})}")
                print(f"    States: before={before_states}, after_physics={final_states}")
                
                if status['execution_status']['status'] == 'completed':
                    print("    ✓ Trajectory completed")
                    break
    
    controller.stop_control_loop()
    return True

if __name__ == "__main__":
    debug_target_propagation()