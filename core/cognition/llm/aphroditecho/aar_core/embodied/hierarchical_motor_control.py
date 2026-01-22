import numpy as np
import time
from typing import Dict, List, Any, Optional, Tuple, Callable
from dataclasses import dataclass, field
from enum import Enum
import math
from .virtual_body import VirtualBody
from .embodied_agent import EmbodiedAgent
class MotorGoalType(Enum):
    REACH_POSITION = 'reach_position'
    MAINTAIN_POSE = 'maintain_pose'
    FOLLOW_TRAJECTORY = 'follow_trajectory'
    BALANCE = 'balance'
    LOCOMOTE = 'locomote'
    GESTURE = 'gesture'
@dataclass
class MotorGoal:
    goal_id: str
    goal_type: MotorGoalType
    target_data: Dict[str, Any]
    priority: float = 1.0
    duration: Optional[float] = None
    completion_tolerance: float = 0.1
    created_time: float = field(default_factory=time.time)
    @property
    def is_expired(self) -> bool:
        if self.duration is None:
            return False
        return time.time() - self.created_time > self.duration
@dataclass
class Trajectory:
    trajectory_id: str
    joint_trajectories: Dict[str, List[Tuple[float, float]]]
    total_duration: float
    smoothness_factor: float = 0.8
    coordination_weights: Dict[str, float] = field(default_factory=dict)
    created_time: float = field(default_factory=time.time)
class HighLevelGoalPlanner:
    def __init__(self, virtual_body: VirtualBody):
        self.virtual_body = virtual_body
        self.active_goals: Dict[str, MotorGoal] = {}
        self.goal_history: List[MotorGoal] = []
        self.goal_completion_callbacks: Dict[str, Callable] = {}
        self.max_concurrent_goals = 3
        self.goal_conflict_resolution = 'priority'
        self.cognitive_interface = None
    def add_goal(self, goal: MotorGoal) -> bool:
        if len(self.active_goals) >= self.max_concurrent_goals:
            lowest_priority_id = min(self.active_goals.keys(), key=lambda x: self.active_goals[x].priority)
            if self.active_goals[lowest_priority_id].priority < goal.priority:
                self.remove_goal(lowest_priority_id)
            else:
                return False
        self.active_goals[goal.goal_id] = goal
        return True
    def remove_goal(self, goal_id: str) -> bool:
        if goal_id in self.active_goals:
            completed_goal = self.active_goals.pop(goal_id)
            self.goal_history.append(completed_goal)
            if goal_id in self.goal_completion_callbacks:
                self.goal_completion_callbacks[goal_id](completed_goal)
                del self.goal_completion_callbacks[goal_id]
            return True
        return False
    def plan_motor_objectives(self) -> Dict[str, Dict[str, Any]]:
        motor_objectives = {}
        expired_goals = [gid for gid, goal in self.active_goals.items() if goal.is_expired]
        for gid in expired_goals:
            self.remove_goal(gid)
        for goal_id, goal in self.active_goals.items():
            objective = self._decompose_goal_to_objectives(goal)
            if objective:
                motor_objectives[goal_id] = objective
        return motor_objectives
    def _decompose_goal_to_objectives(self, goal: MotorGoal) -> Optional[Dict[str, Any]]:
        if goal.goal_type == MotorGoalType.REACH_POSITION:
            return self._plan_reach_objective(goal)
        elif goal.goal_type == MotorGoalType.MAINTAIN_POSE:
            return self._plan_pose_objective(goal)
        elif goal.goal_type == MotorGoalType.BALANCE:
            return self._plan_balance_objective(goal)
        elif goal.goal_type == MotorGoalType.GESTURE:
            return self._plan_gesture_objective(goal)
        else:
            return None
    def _plan_reach_objective(self, goal: MotorGoal) -> Dict[str, Any]:
        target_pos = goal.target_data.get('target_position', (0, 0, 0))
        end_effector = goal.target_data.get('end_effector', 'right_elbow')
        direct_targets = goal.target_data.get('joint_targets')
        if direct_targets:
            joint_targets = direct_targets
        else:
            joint_targets = self._inverse_kinematics_reach(target_pos, end_effector)
        return {'type': 'reach', 'joint_targets': joint_targets, 'duration': goal.target_data.get('duration', 2.0), 'priority': goal.priority, 'coordination_groups': goal.target_data.get('coordination_groups', [['right_shoulder', 'right_elbow'], ['left_shoulder', 'left_elbow']])}
    def _plan_pose_objective(self, goal: MotorGoal) -> Dict[str, Any]:
        target_pose = goal.target_data.get('target_pose', {})
        hold_duration = goal.target_data.get('hold_duration', 1.0)
        return {'type': 'pose', 'joint_targets': target_pose, 'duration': hold_duration, 'priority': goal.priority, 'coordination_groups': [list(target_pose.keys())]}
    def _plan_balance_objective(self, goal: MotorGoal) -> Dict[str, Any]:
        stability_joints = ['base', 'left_hip', 'right_hip', 'left_knee', 'right_knee']
        com = self.virtual_body.center_of_mass
        target_com = goal.target_data.get('target_center_of_mass', com)
        return {'type': 'balance', 'joint_targets': {joint: 0.0 for joint in stability_joints}, 'duration': goal.target_data.get('duration', 5.0), 'priority': goal.priority, 'coordination_groups': [['left_hip', 'left_knee'], ['right_hip', 'right_knee']], 'balance_constraints': {'target_com': target_com}}
    def _plan_gesture_objective(self, goal: MotorGoal) -> Dict[str, Any]:
        gesture_sequence = goal.target_data.get('gesture_sequence', [])
        return {'type': 'gesture', 'gesture_sequence': gesture_sequence, 'duration': goal.target_data.get('duration', 3.0), 'priority': goal.priority, 'coordination_groups': [['neck', 'left_shoulder', 'right_shoulder']]}
    def _inverse_kinematics_reach(self, target_pos: Tuple[float, float, float], end_effector: str) -> Dict[str, float]:
        x, y, z = target_pos
        if 'right' in end_effector:
            shoulder_joint = 'right_shoulder'
            elbow_joint = 'right_elbow'
        else:
            shoulder_joint = 'left_shoulder'
            elbow_joint = 'left_elbow'
        L1, L2 = (0.3, 0.25)
        dist = np.sqrt(x ** 2 + y ** 2)
        if dist > L1 + L2:
            dist = L1 + L2 - 0.01
        elif dist < abs(L1 - L2):
            dist = abs(L1 - L2) + 0.01
        cos_elbow = (L1 ** 2 + L2 ** 2 - dist ** 2) / (2 * L1 * L2)
        cos_elbow = np.clip(cos_elbow, -1, 1)
        elbow_angle = math.acos(cos_elbow) - math.pi
        alpha = math.atan2(y, x)
        beta = math.acos((L1 ** 2 + dist ** 2 - L2 ** 2) / (2 * L1 * dist))
        shoulder_angle = alpha - beta
        return {shoulder_joint: shoulder_angle, elbow_joint: elbow_angle}
class MidLevelTrajectoryGenerator:
    def __init__(self, virtual_body: VirtualBody):
        self.virtual_body = virtual_body
        self.active_trajectories: Dict[str, Trajectory] = {}
        self.trajectory_cache: Dict[str, Trajectory] = {}
        self.default_smoothness = 0.8
        self.coordination_strength = 0.7
        self.temporal_resolution = 0.01
        self.spline_order = 3
        self.velocity_continuity = True
        self.acceleration_limits = True
    def generate_trajectory(self, goal_id: str, objective: Dict[str, Any]) -> Optional[Trajectory]:
        obj_type = objective.get('type')
        if obj_type == 'reach':
            return self._generate_reach_trajectory(goal_id, objective)
        elif obj_type == 'pose':
            return self._generate_pose_trajectory(goal_id, objective)
        elif obj_type == 'balance':
            return self._generate_balance_trajectory(goal_id, objective)
        elif obj_type == 'gesture':
            return self._generate_gesture_trajectory(goal_id, objective)
        else:
            return None
    def _generate_reach_trajectory(self, goal_id: str, objective: Dict[str, Any]) -> Trajectory:
        joint_targets = objective['joint_targets']
        duration = objective['duration']
        coordination_groups = objective.get('coordination_groups', [])
        joint_trajectories = {}
        current_positions = {}
        for joint_id in joint_targets:
            joint_state = self.virtual_body.get_joint_state(joint_id)
            if joint_state:
                current_positions[joint_id] = joint_state['angle']
            else:
                current_positions[joint_id] = 0.0
        for joint_id, target_angle in joint_targets.items():
            current_angle = current_positions[joint_id]
            trajectory_points = self._generate_minimum_jerk_trajectory(current_angle, target_angle, duration)
            joint_trajectories[joint_id] = trajectory_points
        joint_trajectories = self._apply_coordination_constraints(joint_trajectories, coordination_groups, duration)
        return Trajectory(trajectory_id=f'reach_{goal_id}', joint_trajectories=joint_trajectories, total_duration=duration, smoothness_factor=self.default_smoothness, coordination_weights=self._calculate_coordination_weights(coordination_groups))
    def _generate_pose_trajectory(self, goal_id: str, objective: Dict[str, Any]) -> Trajectory:
        joint_targets = objective['joint_targets']
        duration = objective['duration']
        joint_trajectories = {}
        current_positions = {}
        for joint_id in joint_targets:
            joint_state = self.virtual_body.get_joint_state(joint_id)
            if joint_state:
                current_positions[joint_id] = joint_state['angle']
            else:
                current_positions[joint_id] = 0.0
        for joint_id, target_angle in joint_targets.items():
            current_angle = current_positions[joint_id]
            transition_duration = duration * 0.2
            trajectory_points = []
            transition_steps = int(transition_duration / self.temporal_resolution)
            for i in range(transition_steps):
                t = i * self.temporal_resolution
                progress = self._sigmoid_transition(t / transition_duration)
                angle = current_angle + (target_angle - current_angle) * progress
                trajectory_points.append((t, angle))
            hold_steps = int((duration - transition_duration) / self.temporal_resolution)
            for i in range(hold_steps):
                t = transition_duration + i * self.temporal_resolution
                trajectory_points.append((t, target_angle))
            joint_trajectories[joint_id] = trajectory_points
        return Trajectory(trajectory_id=f'pose_{goal_id}', joint_trajectories=joint_trajectories, total_duration=duration)
    def _generate_balance_trajectory(self, goal_id: str, objective: Dict[str, Any]) -> Trajectory:
        joint_targets = objective['joint_targets']
        duration = objective['duration']
        objective.get('balance_constraints', {})
        joint_trajectories = {}
        for joint_id, base_angle in joint_targets.items():
            trajectory_points = []
            amplitude = 0.05
            frequency = 0.5
            steps = int(duration / self.temporal_resolution)
            for i in range(steps):
                t = i * self.temporal_resolution
                balance_variation = amplitude * math.sin(2 * math.pi * frequency * t)
                angle = base_angle + balance_variation
                trajectory_points.append((t, angle))
            joint_trajectories[joint_id] = trajectory_points
        return Trajectory(trajectory_id=f'balance_{goal_id}', joint_trajectories=joint_trajectories, total_duration=duration)
    def _generate_gesture_trajectory(self, goal_id: str, objective: Dict[str, Any]) -> Trajectory:
        objective.get('gesture_sequence', [])
        duration = objective['duration']
        joint_trajectories = {}
        if 'neck' in objective.get('coordination_groups', [[]])[0]:
            neck_trajectory = []
            steps = int(duration / self.temporal_resolution)
            for i in range(steps):
                t = i * self.temporal_resolution
                progress = t / duration
                nod_angle = 0.2 * math.sin(4 * math.pi * progress)
                neck_trajectory.append((t, nod_angle))
            joint_trajectories['neck'] = neck_trajectory
        for arm_joint in ['left_shoulder', 'right_shoulder']:
            if arm_joint in joint_trajectories:
                continue
            arm_trajectory = []
            steps = int(duration / self.temporal_resolution)
            for i in range(steps):
                t = i * self.temporal_resolution
                progress = t / duration
                wave_angle = 0.5 * math.sin(2 * math.pi * progress)
                arm_trajectory.append((t, wave_angle))
            joint_trajectories[arm_joint] = arm_trajectory
        return Trajectory(trajectory_id=f'gesture_{goal_id}', joint_trajectories=joint_trajectories, total_duration=duration)
    def _generate_minimum_jerk_trajectory(self, start: float, end: float, duration: float) -> List[Tuple[float, float]]:
        trajectory_points = []
        steps = int(duration / self.temporal_resolution)
        for i in range(steps):
            t = i * self.temporal_resolution
            tau = t / duration
            s = 10 * tau ** 3 - 15 * tau ** 4 + 6 * tau ** 5
            angle = start + (end - start) * s
            trajectory_points.append((t, angle))
        return trajectory_points
    def _sigmoid_transition(self, x: float) -> float:
        return 1 / (1 + math.exp(-10 * (x - 0.5)))
    def _apply_coordination_constraints(self, joint_trajectories: Dict[str, List[Tuple[float, float]]], coordination_groups: List[List[str]], duration: float) -> Dict[str, List[Tuple[float, float]]]:
        for group in coordination_groups:
            if len(group) < 2:
                continue
            for i, joint_a in enumerate(group):
                for joint_b in group[i + 1:]:
                    if joint_a in joint_trajectories and joint_b in joint_trajectories:
                        self._apply_joint_coupling(joint_trajectories[joint_a], joint_trajectories[joint_b], coupling_strength=0.1)
        return joint_trajectories
    def _apply_joint_coupling(self, traj_a: List[Tuple[float, float]], traj_b: List[Tuple[float, float]], coupling_strength: float):
        if len(traj_a) != len(traj_b):
            return
        for i in range(len(traj_a)):
            t_a, angle_a = traj_a[i]
            t_b, angle_b = traj_b[i]
            avg_influence = coupling_strength * 0.5 * (angle_a + angle_b)
            traj_a[i] = (t_a, angle_a + coupling_strength * (avg_influence - angle_a))
            traj_b[i] = (t_b, angle_b + coupling_strength * (avg_influence - angle_b))
    def _calculate_coordination_weights(self, coordination_groups: List[List[str]]) -> Dict[str, float]:
        weights = {}
        for group in coordination_groups:
            group_weight = 1.0 / len(group) if group else 1.0
            for joint_id in group:
                weights[joint_id] = group_weight
        return weights
class LowLevelMotorExecutor:
    def __init__(self, embodied_agent: EmbodiedAgent):
        self.embodied_agent = embodied_agent
        self.active_trajectory: Optional[Trajectory] = None
        self.trajectory_simulation_time: float = 0.0
        self.current_trajectory_targets: Dict[str, float] = {}
        self.coordination_gains = {'kp': 15.0, 'kd': 2.0, 'ki': 0.05}
        self.coordination_active = True
        self.coordination_errors: Dict[str, Dict[str, float]] = {}
        self.coordination_groups: Dict[str, List[str]] = {}
        self.execution_quality_score = 1.0
        self.trajectory_following_error = 0.0
        self.coordination_quality_score = 1.0
    def execute_trajectory(self, trajectory: Trajectory) -> bool:
        if trajectory is None:
            return False
        self.active_trajectory = trajectory
        self.trajectory_simulation_time = 0.0
        self.coordination_groups = self._extract_coordination_groups(trajectory)
        for group_name, joint_list in self.coordination_groups.items():
            self.coordination_errors[group_name] = {joint: 0.0 for joint in joint_list}
        return True
    def update_execution(self, dt: float) -> Dict[str, Any]:
        if self.active_trajectory is None:
            return {'status': 'idle'}
        self.trajectory_simulation_time += dt
        trajectory_time = min(self.trajectory_simulation_time, self.active_trajectory.total_duration)
        if self.trajectory_simulation_time >= self.active_trajectory.total_duration:
            completed_trajectory = self.active_trajectory
            self.active_trajectory = None
            self.trajectory_simulation_time = 0.0
            return {'status': 'completed', 'trajectory_id': completed_trajectory.trajectory_id, 'execution_quality': self.execution_quality_score}
        self.current_trajectory_targets = self._interpolate_trajectory_targets(trajectory_time)
        self._execute_coordinated_control(dt)
        self._update_execution_metrics()
        return {'status': 'executing', 'trajectory_id': self.active_trajectory.trajectory_id, 'progress': trajectory_time / self.active_trajectory.total_duration, 'targets': self.current_trajectory_targets, 'execution_quality': self.execution_quality_score, 'coordination_quality': self.coordination_quality_score, 'following_error': self.trajectory_following_error}
    def _interpolate_trajectory_targets(self, trajectory_time: float) -> Dict[str, float]:
        targets = {}
        if not self.active_trajectory:
            return targets
        for joint_id, trajectory_points in self.active_trajectory.joint_trajectories.items():
            if not trajectory_points:
                continue
            target_angle = self._interpolate_trajectory_point(trajectory_points, trajectory_time)
            targets[joint_id] = target_angle
        return targets
    def _interpolate_trajectory_point(self, trajectory_points: List[Tuple[float, float]], current_time: float) -> float:
        if not trajectory_points:
            return 0.0
        if current_time <= trajectory_points[0][0]:
            return trajectory_points[0][1]
        if current_time >= trajectory_points[-1][0]:
            return trajectory_points[-1][1]
        for i in range(len(trajectory_points) - 1):
            t1, angle1 = trajectory_points[i]
            t2, angle2 = trajectory_points[i + 1]
            if t1 <= current_time <= t2:
                if t2 == t1:
                    return angle1
                alpha = (current_time - t1) / (t2 - t1)
                return angle1 + alpha * (angle2 - angle1)
        return trajectory_points[-1][1]
    def _execute_coordinated_control(self, dt: float):
        if not self.current_trajectory_targets:
            return
        for joint_id, target_angle in self.current_trajectory_targets.items():
            self.embodied_agent.set_joint_target(joint_id, target_angle)
        if self.coordination_active:
            self._apply_coordination_corrections(dt)
    def _apply_coordination_corrections(self, dt: float):
        for group_name, joint_list in self.coordination_groups.items():
            if len(joint_list) < 2:
                continue
            group_targets = []
            group_actuals = []
            group_errors = []
            for joint_id in joint_list:
                if joint_id not in self.current_trajectory_targets:
                    continue
                target = self.current_trajectory_targets[joint_id]
                actual_state = self.embodied_agent.get_joint_state(joint_id)
                if actual_state:
                    actual = actual_state['angle']
                    error = target - actual
                    group_targets.append(target)
                    group_actuals.append(actual)
                    group_errors.append(error)
                    self.coordination_errors[group_name][joint_id] = error
            if len(group_errors) < 2:
                continue
            avg_error = np.mean(group_errors)
            for i, joint_id in enumerate(joint_list[:len(group_errors)]):
                if joint_id not in self.current_trajectory_targets:
                    continue
                individual_error = group_errors[i]
                coordination_correction = self.coordination_gains['kp'] * (avg_error - individual_error) * 0.1
                corrected_target = self.current_trajectory_targets[joint_id] + coordination_correction
                self.embodied_agent.set_joint_target(joint_id, corrected_target)
    def _extract_coordination_groups(self, trajectory: Trajectory) -> Dict[str, List[str]]:
        groups = {}
        for joint_id, weight in trajectory.coordination_weights.items():
            if weight > 0:
                group_name = f'group_{int(weight * 10)}'
                if group_name not in groups:
                    groups[group_name] = []
                groups[group_name].append(joint_id)
        if not groups and trajectory.joint_trajectories:
            groups['default'] = list(trajectory.joint_trajectories.keys())
        return groups
    def _update_execution_metrics(self):
        if not self.current_trajectory_targets:
            return
        following_errors = []
        for joint_id, target in self.current_trajectory_targets.items():
            actual_state = self.embodied_agent.get_joint_state(joint_id)
            if actual_state:
                error = abs(target - actual_state['angle'])
                following_errors.append(error)
        if following_errors:
            self.trajectory_following_error = np.mean(following_errors)
            self.execution_quality_score = np.exp(-self.trajectory_following_error * 5)
        coordination_errors = []
        for group_errors in self.coordination_errors.values():
            if len(group_errors) > 1:
                group_error_vals = list(group_errors.values())
                coordination_variance = np.var(group_error_vals)
                coordination_errors.append(coordination_variance)
        if coordination_errors:
            avg_coordination_error = np.mean(coordination_errors)
            self.coordination_quality_score = np.exp(-avg_coordination_error * 10)
        else:
            self.coordination_quality_score = 1.0
class HierarchicalMotorController:
    def __init__(self, embodied_agent: EmbodiedAgent):
        self.embodied_agent = embodied_agent
        self.goal_planner = HighLevelGoalPlanner(embodied_agent.virtual_body)
        self.trajectory_generator = MidLevelTrajectoryGenerator(embodied_agent.virtual_body)
        self.motor_executor = LowLevelMotorExecutor(embodied_agent)
        self.active = False
        self.update_frequency = 100.0
        self.last_update_time = 0.0
        self.control_loop_performance = {'planning_time': 0.0, 'generation_time': 0.0, 'execution_time': 0.0, 'total_cycle_time': 0.0, 'smoothness_score': 1.0, 'coordination_score': 1.0}
        self.dtesn_interface = None
        self.cogprime_interface = None
    def start_control_loop(self):
        self.active = True
        self.last_update_time = time.time()
    def stop_control_loop(self):
        self.active = False
    def add_motor_goal(self, goal: MotorGoal) -> bool:
        return self.goal_planner.add_goal(goal)
    def update(self, dt: float) -> Dict[str, Any]:
        if not self.active:
            return {'status': 'inactive'}
        start_time = time.perf_counter()
        planning_start = time.perf_counter()
        motor_objectives = self.goal_planner.plan_motor_objectives()
        planning_time = time.perf_counter() - planning_start
        generation_start = time.perf_counter()
        new_trajectory = None
        for goal_id, objective in motor_objectives.items():
            if self.motor_executor.active_trajectory is None:
                trajectory = self.trajectory_generator.generate_trajectory(goal_id, objective)
                if trajectory:
                    new_trajectory = trajectory
                    break
        if new_trajectory:
            self.motor_executor.execute_trajectory(new_trajectory)
        generation_time = time.perf_counter() - generation_start
        execution_start = time.perf_counter()
        execution_status = self.motor_executor.update_execution(dt)
        execution_time = time.perf_counter() - execution_start
        total_time = time.perf_counter() - start_time
        self.control_loop_performance.update({'planning_time': planning_time, 'generation_time': generation_time, 'execution_time': execution_time, 'total_cycle_time': total_time, 'smoothness_score': execution_status.get('execution_quality', 1.0), 'coordination_score': execution_status.get('coordination_quality', 1.0)})
        return {'status': 'active', 'active_goals': len(self.goal_planner.active_goals), 'execution_status': execution_status, 'performance': self.control_loop_performance, 'layer_status': {'planning': 'active' if motor_objectives else 'idle', 'generation': 'active' if new_trajectory else 'idle', 'execution': execution_status.get('status', 'idle')}}
    def get_system_status(self) -> Dict[str, Any]:
        return {'controller_active': self.active, 'goal_planner': {'active_goals': len(self.goal_planner.active_goals), 'goal_history_count': len(self.goal_planner.goal_history)}, 'trajectory_generator': {'active_trajectories': len(self.trajectory_generator.active_trajectories), 'cached_trajectories': len(self.trajectory_generator.trajectory_cache)}, 'motor_executor': {'has_active_trajectory': self.motor_executor.active_trajectory is not None, 'execution_quality': self.motor_executor.execution_quality_score, 'coordination_quality': self.motor_executor.coordination_quality_score, 'trajectory_error': self.motor_executor.trajectory_following_error}, 'performance': self.control_loop_performance, 'embodied_agent': self.embodied_agent.get_embodiment_status()}
    def validate_smooth_coordinated_movement(self) -> Tuple[bool, Dict[str, Any]]:
        validation_results = {'smooth_movement': False, 'coordinated_movement': False, 'overall_success': False, 'smoothness_score': 0.0, 'coordination_score': 0.0, 'execution_quality_score': 0.0, 'details': {}}
        try:
            smoothness_score = self.control_loop_performance['smoothness_score']
            validation_results['smoothness_score'] = smoothness_score
            validation_results['smooth_movement'] = smoothness_score > 0.7
            coordination_score = self.motor_executor.coordination_quality_score
            validation_results['coordination_score'] = coordination_score
            validation_results['coordinated_movement'] = coordination_score > 0.7
            agent_status = self.embodied_agent.get_embodiment_status()
            execution_quality = agent_status.get('motor_performance_score', 0.0)
            validation_results['execution_quality_score'] = execution_quality
            all_scores = [smoothness_score, coordination_score, execution_quality]
            overall_score = np.mean(all_scores)
            validation_results['overall_success'] = overall_score > 0.7 and validation_results['smooth_movement'] and validation_results['coordinated_movement']
            validation_results['details'] = {'control_loop_performance': self.control_loop_performance, 'trajectory_following_error': self.motor_executor.trajectory_following_error, 'active_coordination_groups': len(self.motor_executor.coordination_groups), 'agent_motor_performance': execution_quality, 'system_status': self.get_system_status()}
        except Exception as e:
            validation_results['details']['error'] = str(e)
        return (validation_results['overall_success'], validation_results)