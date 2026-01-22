import time
from aar_core.embodied import EmbodiedAgent, HierarchicalMotorController
def debug_trajectory_timing():
    print('=== TRAJECTORY TIMING DIAGNOSIS ===')
    agent = EmbodiedAgent('timing_agent', (0, 0, 1))
    controller = HierarchicalMotorController(agent)
    objective = {'type': 'reach', 'joint_targets': {'right_shoulder': 0.3, 'right_elbow': -0.5}, 'duration': 1.0, 'priority': 0.8, 'coordination_groups': [['right_shoulder', 'right_elbow']]}
    trajectory = controller.trajectory_generator.generate_trajectory('timing_debug', objective)
    print(f'Generated trajectory with duration: {trajectory.total_duration}')
    shoulder_points = trajectory.joint_trajectories['right_shoulder']
    print(f'Shoulder trajectory has {len(shoulder_points)} points')
    print('First 10 trajectory points:')
    for i in range(min(10, len(shoulder_points))):
        t, angle = shoulder_points[i]
        print(f'  Point {i}: t={t:.3f}, angle={angle:.4f}')
    print('Last 10 trajectory points:')
    for i in range(max(0, len(shoulder_points) - 10), len(shoulder_points)):
        t, angle = shoulder_points[i]
        print(f'  Point {i}: t={t:.3f}, angle={angle:.4f}')
    executor = controller.motor_executor
    success = executor.execute_trajectory(trajectory)
    if success:
        print(f'\n✓ Trajectory execution started at {time.time():.3f}')
        print('Simulating executor timing:')
        for step in [0, 25, 50, 75, 99]:
            dt = 0.01
            trajectory_time = step * dt
            print(f'\nStep {step}: trajectory_time={trajectory_time:.3f}')
            shoulder_target = executor._interpolate_trajectory_point(shoulder_points, trajectory_time)
            print(f'  Interpolated shoulder target: {shoulder_target:.4f}')
            targets = executor._interpolate_trajectory_targets(trajectory_time)
            print(f'  Executor targets: {targets}')
    return True
if __name__ == '__main__':
    debug_trajectory_timing()