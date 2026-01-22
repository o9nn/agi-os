import numpy as np
import matplotlib
matplotlib.use('Agg')
from aar_core.embodied import EmbodiedAgent, HierarchicalMotorController, MotorGoal, MotorGoalType
from aar_core.arena.simulation_engine import ArenaPhysics, ArenaEnvironment
def test_high_level_goal_planning():
    print('=' * 60)
    print('TEST: High-Level Goal Planning')
    print('=' * 60)
    agent = EmbodiedAgent('goal_planning_test', (0, 0, 1))
    motor_controller = HierarchicalMotorController(agent)
    reach_goal = MotorGoal(goal_id='reach_test', goal_type=MotorGoalType.REACH_POSITION, target_data={'target_position': (0.5, 0.2, 1.2), 'end_effector': 'right_elbow', 'joint_targets': {'right_shoulder': 0.3, 'right_elbow': -0.5}, 'duration': 2.0}, priority=0.8)
    success = motor_controller.add_motor_goal(reach_goal)
    assert success, 'Should successfully add motor goal'
    print('✓ Goal creation and addition successful')
    motor_objectives = motor_controller.goal_planner.plan_motor_objectives()
    assert len(motor_objectives) == 1, 'Should have one motor objective'
    assert 'reach_test' in motor_objectives, 'Should contain reach goal'
    objective = motor_objectives['reach_test']
    assert objective['type'] == 'reach', 'Should be reach objective'
    assert 'joint_targets' in objective, 'Should have joint targets'
    assert len(objective['joint_targets']) > 0, 'Should have target joint angles'
    print(f"✓ Goal decomposition: {len(objective['joint_targets'])} joint targets generated")
    print(f"✓ Target joints: {list(objective['joint_targets'].keys())}")
    balance_goal = MotorGoal(goal_id='balance_test', goal_type=MotorGoalType.BALANCE, target_data={'duration': 3.0}, priority=0.6)
    motor_controller.add_motor_goal(balance_goal)
    objectives = motor_controller.goal_planner.plan_motor_objectives()
    assert len(objectives) == 2, 'Should manage multiple goals'
    print(f'✓ Multiple goal management: {len(objectives)} active objectives')
    print('High-Level Goal Planning Test: PASSED\n')
    return True
def test_mid_level_trajectory_generation():
    print('=' * 60)
    print('TEST: Mid-Level Trajectory Generation')
    print('=' * 60)
    agent = EmbodiedAgent('trajectory_test', (0, 0, 1))
    motor_controller = HierarchicalMotorController(agent)
    reach_objective = {'type': 'reach', 'joint_targets': {'right_shoulder': 0.5, 'right_elbow': -0.8}, 'duration': 2.0, 'priority': 0.8, 'coordination_groups': [['right_shoulder', 'right_elbow']]}
    trajectory = motor_controller.trajectory_generator.generate_trajectory('test_reach', reach_objective)
    assert trajectory is not None, 'Should generate trajectory'
    assert trajectory.total_duration == 2.0, 'Should have correct duration'
    assert len(trajectory.joint_trajectories) == 2, 'Should have trajectories for both joints'
    shoulder_traj = trajectory.joint_trajectories['right_shoulder']
    assert len(shoulder_traj) > 100, 'Should have sufficient trajectory points for smoothness'
    angles = [point[1] for point in shoulder_traj]
    velocities = np.diff(angles)
    max_velocity_change = np.max(np.abs(np.diff(velocities)))
    assert max_velocity_change < 0.1, f'Velocity changes should be smooth, got {max_velocity_change}'
    print('✓ Smooth trajectory generation successful')
    print(f'✓ Trajectory points: {len(shoulder_traj)}')
    print(f'✓ Maximum velocity change: {max_velocity_change:.4f}')
    trajectory.joint_trajectories['right_elbow']
    assert len(trajectory.coordination_weights) > 0, 'Should have coordination weights'
    print(f'✓ Coordination weights: {trajectory.coordination_weights}')
    balance_objective = {'type': 'balance', 'joint_targets': {'left_hip': 0.0, 'right_hip': 0.0}, 'duration': 3.0, 'priority': 0.6, 'balance_constraints': {'target_com': (0, 0, 1)}}
    balance_trajectory = motor_controller.trajectory_generator.generate_trajectory('test_balance', balance_objective)
    assert balance_trajectory is not None, 'Should generate balance trajectory'
    assert balance_trajectory.total_duration == 3.0, 'Should have correct balance duration'
    print('✓ Multiple trajectory types supported')
    print('Mid-Level Trajectory Generation Test: PASSED\n')
    return True
def test_low_level_motor_execution():
    print('=' * 60)
    print('TEST: Low-Level Motor Execution')
    print('=' * 60)
    agent = EmbodiedAgent('execution_test', (0, 0, 1))
    physics = ArenaPhysics()
    environment = ArenaEnvironment()
    motor_controller = HierarchicalMotorController(agent)
    reach_objective = {'type': 'reach', 'joint_targets': {'right_shoulder': 0.3, 'right_elbow': -0.5}, 'duration': 1.0, 'priority': 0.8, 'coordination_groups': [['right_shoulder', 'right_elbow']]}
    trajectory = motor_controller.trajectory_generator.generate_trajectory('exec_test', reach_objective)
    executor = motor_controller.motor_executor
    success = executor.execute_trajectory(trajectory)
    assert success, 'Should successfully start trajectory execution'
    assert executor.active_trajectory is not None, 'Should have active trajectory'
    print('✓ Trajectory execution setup successful')
    execution_history = []
    motor_controller.start_control_loop()
    for step in range(120):
        dt = 0.01
        status = motor_controller.update(dt)
        agent.update(dt, physics, environment)
        execution_history.append({'time': step * dt, 'joint_states': agent.get_all_joint_states(), 'execution_status': status['execution_status'], 'coordination_quality': status['performance']['coordination_score']})
        if status['execution_status']['status'] == 'completed' or step >= 119:
            print(f"✓ Trajectory execution stopped at step {step}, status: {status['execution_status']['status']}")
            break
    motor_controller.stop_control_loop()
    assert len(execution_history) > 50, 'Should have substantial execution history'
    final_state = execution_history[-1]['joint_states']
    target_shoulder = reach_objective['joint_targets']['right_shoulder']
    target_elbow = reach_objective['joint_targets']['right_elbow']
    actual_shoulder = final_state['right_shoulder']['angle']
    actual_elbow = final_state['right_elbow']['angle']
    shoulder_error = abs(target_shoulder - actual_shoulder)
    elbow_error = abs(target_elbow - actual_elbow)
    print(f'   Target vs Actual - Shoulder: {target_shoulder:.3f} vs {actual_shoulder:.3f}')
    print(f'   Target vs Actual - Elbow: {target_elbow:.3f} vs {actual_elbow:.3f}')
    max_acceptable_error = 0.2
    assert shoulder_error < max_acceptable_error, f'Shoulder should reach target within {max_acceptable_error} rad, error: {shoulder_error}'
    assert elbow_error < max_acceptable_error, f'Elbow should reach target within {max_acceptable_error} rad, error: {elbow_error}'
    print(f'✓ Trajectory following accuracy: shoulder={shoulder_error:.4f}, elbow={elbow_error:.4f}')
    coordination_scores = [h['coordination_quality'] for h in execution_history if h['coordination_quality'] is not None]
    avg_coordination = np.mean(coordination_scores) if coordination_scores else 0.0
    assert avg_coordination > 0.6, f'Coordination quality should be good, got {avg_coordination}'
    print(f'✓ Average coordination quality: {avg_coordination:.3f}')
    print('Low-Level Motor Execution Test: PASSED\n')
    return True
def test_integrated_hierarchical_control():
    print('=' * 60)
    print('TEST: Integrated Hierarchical Control')
    print('=' * 60)
    agent = EmbodiedAgent('integrated_test', (0, 0, 1))
    physics = ArenaPhysics()
    environment = ArenaEnvironment()
    motor_controller = HierarchicalMotorController(agent)
    motor_controller.start_control_loop()
    goals = [MotorGoal('reach_1', MotorGoalType.REACH_POSITION, {'target_position': (0.4, 0.1, 1.1), 'end_effector': 'right_elbow', 'duration': 2.0}, priority=0.8), MotorGoal('gesture_1', MotorGoalType.GESTURE, {'gesture_sequence': ['nod'], 'duration': 1.5}, priority=0.7), MotorGoal('balance_1', MotorGoalType.BALANCE, {'duration': 2.0}, priority=0.6)]
    for goal in goals:
        success = motor_controller.add_motor_goal(goal)
        assert success, f'Should add goal {goal.goal_id}'
    print(f'✓ Added {len(goals)} goals to integrated system')
    integration_history = []
    max_steps = 500
    for step in range(max_steps):
        dt = 0.01
        status = motor_controller.update(dt)
        agent.update(dt, physics, environment)
        integration_history.append({'time': step * dt, 'control_status': status, 'system_status': motor_controller.get_system_status(), 'agent_status': agent.get_embodiment_status()})
        if step > 100 and status['active_goals'] == 0:
            print(f'✓ All goals completed at step {step}')
            break
    motor_controller.stop_control_loop()
    assert len(integration_history) > 100, 'Should have run for substantial time'
    goal_processing_found = False
    for h in integration_history:
        if h['control_status']['active_goals'] > 0:
            goal_processing_found = True
            break
    assert goal_processing_found, 'Should have processed goals during execution'
    print('✓ Goal processing confirmed during integration')
    final_performance = integration_history[-1]['control_status']['performance']
    smoothness = final_performance['smoothness_score']
    coordination = final_performance['coordination_score']
    assert smoothness > 0.5, f'Smoothness should be reasonable, got {smoothness}'
    assert coordination > 0.5, f'Coordination should be reasonable, got {coordination}'
    print(f'✓ Integration performance: smoothness={smoothness:.3f}, coordination={coordination:.3f}')
    print('Integrated Hierarchical Control Test: PASSED\n')
    return True
def test_smooth_coordinated_movement_acceptance_criteria():
    print('=' * 60)
    print('TEST: Smooth and Coordinated Movement (ACCEPTANCE CRITERIA)')
    print('=' * 60)
    agent = EmbodiedAgent('acceptance_test', (0, 0, 1))
    physics = ArenaPhysics()
    environment = ArenaEnvironment()
    motor_controller = HierarchicalMotorController(agent)
    motor_controller.start_control_loop()
    complex_reach_goal = MotorGoal(goal_id='complex_reach', goal_type=MotorGoalType.REACH_POSITION, target_data={'target_position': (0.6, 0.3, 1.0), 'end_effector': 'right_elbow', 'joint_targets': {'right_shoulder': 0.4, 'right_elbow': -0.6}, 'duration': 3.0}, priority=1.0)
    motor_controller.add_motor_goal(complex_reach_goal)
    movement_data = {'positions': [], 'velocities': [], 'accelerations': [], 'coordination_scores': [], 'smoothness_scores': [], 'joint_trajectories': {joint: [] for joint in ['right_shoulder', 'right_elbow', 'neck']}}
    previous_joint_states = None
    previous_velocities = None
    for step in range(300):
        dt = 0.01
        status = motor_controller.update(dt)
        agent.update(dt, physics, environment)
        joint_states = agent.get_all_joint_states()
        for joint_id, joint_data in movement_data['joint_trajectories'].items():
            if joint_id in joint_states:
                joint_data.append(joint_states[joint_id]['angle'])
        if previous_joint_states:
            velocities = {}
            for joint_id in joint_states:
                if joint_id in previous_joint_states:
                    vel = (joint_states[joint_id]['angle'] - previous_joint_states[joint_id]['angle']) / dt
                    velocities[joint_id] = vel
            movement_data['velocities'].append(velocities)
            if previous_velocities and len(previous_velocities) > 0:
                accelerations = {}
                for joint_id in velocities:
                    if joint_id in previous_velocities:
                        acc = (velocities[joint_id] - previous_velocities[joint_id]) / dt
                        accelerations[joint_id] = acc
                movement_data['accelerations'].append(accelerations)
            previous_velocities = velocities
        previous_joint_states = joint_states
        if 'performance' in status:
            movement_data['coordination_scores'].append(status['performance']['coordination_score'])
            movement_data['smoothness_scores'].append(status['performance']['smoothness_score'])
        if status['execution_status']['status'] == 'completed':
            print(f'✓ Complex movement completed at {step * dt:.2f}s')
            break
    motor_controller.stop_control_loop()
    print('\n--- SMOOTHNESS ANALYSIS ---')
    max_velocity_changes = {}
    for joint_id in movement_data['joint_trajectories']:
        trajectory = movement_data['joint_trajectories'][joint_id]
        if len(trajectory) < 10:
            continue
        velocities = np.diff(trajectory)
        velocity_changes = np.diff(velocities)
        max_change = np.max(np.abs(velocity_changes)) if len(velocity_changes) > 0 else 0
        max_velocity_changes[joint_id] = max_change
    avg_max_velocity_change = np.mean(list(max_velocity_changes.values()))
    smoothness_threshold = 0.05
    smoothness_passed = avg_max_velocity_change < smoothness_threshold
    print(f'✓ Velocity smoothness: avg_max_change={avg_max_velocity_change:.4f}, threshold={smoothness_threshold}')
    print(f'✓ Per-joint velocity changes: {max_velocity_changes}')
    print('\n--- COORDINATION ANALYSIS ---')
    if len(movement_data['coordination_scores']) > 0:
        avg_coordination = np.mean(movement_data['coordination_scores'])
        min_coordination = np.min(movement_data['coordination_scores'])
        coordination_threshold = 0.7
        coordination_passed = avg_coordination > coordination_threshold and min_coordination > 0.5
        print(f'✓ Coordination quality: avg={avg_coordination:.3f}, min={min_coordination:.3f}')
        print(f'✓ Coordination threshold: {coordination_threshold}')
    else:
        coordination_passed = False
        print('✗ No coordination scores collected')
    print('\n--- MOVEMENT COMPLETION ---')
    final_joint_states = agent.get_all_joint_states()
    target_joints = ['right_shoulder', 'right_elbow']
    completion_errors = {}
    for joint_id in target_joints:
        if joint_id in final_joint_states:
            final_angle = final_joint_states[joint_id]['angle']
            completion_errors[joint_id] = abs(final_angle)
    movement_completion = len(completion_errors) > 0 and np.mean(list(completion_errors.values())) > 0.1
    print(f'✓ Movement completion: joint_movements={completion_errors}')
    print('\n--- SYSTEM VALIDATION ---')
    validation_passed, validation_results = motor_controller.validate_smooth_coordinated_movement()
    print(f"✓ System validation: {validation_results['smoothness_score']:.3f} smoothness, {validation_results['coordination_score']:.3f} coordination")
    print(f"✓ Execution quality: {validation_results['execution_quality_score']:.3f}")
    acceptance_passed = smoothness_passed and coordination_passed and movement_completion and validation_passed
    if acceptance_passed:
        print('\n🎉 ACCEPTANCE CRITERIA MET: Smooth and coordinated movement execution achieved!')
        print('✅ HIGH-LEVEL GOAL PLANNING: Working')
        print('✅ MID-LEVEL TRAJECTORY GENERATION: Working')
        print('✅ LOW-LEVEL MOTOR EXECUTION: Working')
        print('✅ MOVEMENT SMOOTHNESS: Achieved')
        print('✅ MOVEMENT COORDINATION: Achieved')
    else:
        print('\n❌ ACCEPTANCE CRITERIA NOT MET')
        print(f"   Smoothness: {('✅' if smoothness_passed else '❌')}")
        print(f"   Coordination: {('✅' if coordination_passed else '❌')}")
        print(f"   Completion: {('✅' if movement_completion else '❌')}")
        print(f"   System validation: {('✅' if validation_passed else '❌')}")
    print('Smooth and Coordinated Movement Test:', 'PASSED' if acceptance_passed else 'FAILED')
    print()
    return acceptance_passed
def main():
    print('🤖 TASK 3.2.1 VALIDATION: Design Hierarchical Motor Control')
    print('📋 Acceptance Criteria: Smooth and coordinated movement execution')
    print('🧪 Testing: High-level goal planning, Mid-level trajectory generation, Low-level motor execution')
    print()
    test_results = []
    try:
        test_results.append(('High-Level Goal Planning', test_high_level_goal_planning()))
        test_results.append(('Mid-Level Trajectory Generation', test_mid_level_trajectory_generation()))
        test_results.append(('Low-Level Motor Execution', test_low_level_motor_execution()))
        test_results.append(('Integrated Hierarchical Control', test_integrated_hierarchical_control()))
        test_results.append(('Smooth Coordinated Movement (MAIN)', test_smooth_coordinated_movement_acceptance_criteria()))
    except Exception as e:
        print(f'❌ Test execution failed: {e}')
        test_results.append(('Test Execution', False))
    print('=' * 80)
    print('VALIDATION SUMMARY')
    print('=' * 80)
    passed_tests = 0
    for test_name, passed in test_results:
        status = '✓ PASSED' if passed else '✗ FAILED'
        print(f'{test_name:<40} {status}')
        if passed:
            passed_tests += 1
    print()
    print(f'Tests passed: {passed_tests}/{len(test_results)}')
    print(f'Success rate: {100 * passed_tests / len(test_results):.1f}%')
    all_passed = all((result[1] for result in test_results))
    if all_passed:
        print('\n🎉 TASK 3.2.1 VALIDATION: ALL TESTS PASSED')
        print('✅ ACCEPTANCE CRITERIA MET: Smooth and coordinated movement execution')
        print('\n📊 HIERARCHICAL MOTOR CONTROL SYSTEM STATUS:')
        print('   ✅ High-level goal planning layer: Functional')
        print('   ✅ Mid-level trajectory generation layer: Functional')
        print('   ✅ Low-level motor execution layer: Functional')
        print('   ✅ Integration with existing virtual body system: Complete')
        print('   ✅ Smooth movement execution: Validated')
        print('   ✅ Coordinated movement execution: Validated')
    else:
        print('\n❌ TASK 3.2.1 VALIDATION: SOME TESTS FAILED')
        print('   Review failed tests above for details')
    return all_passed
if __name__ == '__main__':
    success = main()
    exit(0 if success else 1)