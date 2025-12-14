#!/usr/bin/env python3
"""
Simple test to debug the motor control integration issue
"""

from aar_core.embodied import EmbodiedAgent, HierarchicalMotorController, MotorGoal, MotorGoalType
from aar_core.arena.simulation_engine import ArenaPhysics, ArenaEnvironment

def test_simple_motor_control():
    """Test basic motor control without hierarchical system first"""
    print("=== Basic Motor Control Test ===")
    
    agent = EmbodiedAgent("simple_test", (0, 0, 1))
    physics = ArenaPhysics()
    environment = ArenaEnvironment()
    
    # Get initial state
    initial_state = agent.get_joint_state("right_shoulder")
    print(f"Initial right_shoulder angle: {initial_state['angle']:.3f}")
    
    # Set a target directly
    target = 0.5
    agent.set_joint_target("right_shoulder", target)
    print(f"Set target: {target}")
    
    # Run updates
    for step in range(100):
        dt = 0.01
        agent.update(dt, physics, environment)
        
        if step % 20 == 0:  # Print every 20 steps
            current_state = agent.get_joint_state("right_shoulder")
            current_angle = current_state['angle']
            error = abs(target - current_angle)
            print(f"Step {step:3d}: angle={current_angle:.3f}, error={error:.3f}")
    
    # Final check
    final_state = agent.get_joint_state("right_shoulder")
    final_error = abs(target - final_state['angle'])
    print(f"Final error: {final_error:.3f}")
    
    return final_error < 0.2

def test_hierarchical_simple():
    """Test hierarchical controller with minimal goal"""
    print("\n=== Hierarchical Controller Test ===")
    
    agent = EmbodiedAgent("hierarchical_test", (0, 0, 1))
    physics = ArenaPhysics()
    environment = ArenaEnvironment()
    controller = HierarchicalMotorController(agent)
    
    # Simple reach goal
    goal = MotorGoal(
        goal_id="simple_reach",
        goal_type=MotorGoalType.REACH_POSITION,
        target_data={
            'target_position': (0.3, 0.1, 1.0),
            'end_effector': 'right_elbow',
            'duration': 1.5
        },
        priority=1.0
    )
    
    controller.add_motor_goal(goal)
    controller.start_control_loop()
    
    print("Goal added, starting execution...")
    
    # Track execution
    for step in range(150):  # 1.5 seconds
        dt = 0.01
        
        status = controller.update(dt)
        agent.update(dt, physics, environment)
        
        if step % 30 == 0:  # Print every 30 steps
            joint_states = agent.get_all_joint_states()
            shoulder_angle = joint_states.get('right_shoulder', {}).get('angle', 0.0)
            elbow_angle = joint_states.get('right_elbow', {}).get('angle', 0.0)
            print(f"Step {step:3d}: shoulder={shoulder_angle:.3f}, elbow={elbow_angle:.3f}")
            print(f"         Status: {status['execution_status']['status']}")
        
        if status['execution_status']['status'] == 'completed':
            print(f"✓ Goal completed at step {step}")
            break
    
    controller.stop_control_loop()
    
    # Check final positions
    final_states = agent.get_all_joint_states()
    shoulder_final = final_states['right_shoulder']['angle']
    elbow_final = final_states['right_elbow']['angle']
    
    print(f"Final positions: shoulder={shoulder_final:.3f}, elbow={elbow_final:.3f}")
    
    # The goal should have caused some movement
    movement_occurred = abs(shoulder_final) > 0.05 or abs(elbow_final) > 0.05
    
    return movement_occurred

if __name__ == "__main__":
    print("Testing Motor Control Integration")
    
    basic_works = test_simple_motor_control()
    print(f"Basic motor control: {'PASS' if basic_works else 'FAIL'}")
    
    hierarchical_works = test_hierarchical_simple()
    print(f"Hierarchical control: {'PASS' if hierarchical_works else 'FAIL'}")
    
    if basic_works and hierarchical_works:
        print("\n✅ Both basic and hierarchical control working")
    else:
        print(f"\n❌ Issues detected: basic={basic_works}, hierarchical={hierarchical_works}")