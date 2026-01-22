import numpy as np
import time
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass
from enum import Enum
from ..arena.simulation_engine import ArenaObject, ArenaPhysics
class JointType(Enum):
    REVOLUTE = 'revolute'
    PRISMATIC = 'prismatic'
    SPHERICAL = 'spherical'
    FIXED = 'fixed'
    UNIVERSAL = 'universal'
@dataclass
class JointLimits:
    min_angle: float = -np.pi
    max_angle: float = np.pi
    max_velocity: float = 10.0
    max_torque: float = 100.0
    damping: float = 0.1
    stiffness: float = 1.0
class BodyJoint:
    def __init__(self, joint_id: str, joint_type: JointType, parent_link: Optional[str]=None, child_link: Optional[str]=None, position: Tuple[float, float, float]=(0.0, 0.0, 0.0), axis: Tuple[float, float, float]=(0.0, 0.0, 1.0), limits: Optional[JointLimits]=None):
        self.id = joint_id
        self.type = joint_type
        self.parent_link = parent_link
        self.child_link = child_link
        self.position = np.array(position, dtype=float)
        self.axis = np.array(axis, dtype=float)
        self.limits = limits or JointLimits()
        self.angle = 0.0
        self.velocity = 0.0
        self.acceleration = 0.0
        self.torque = 0.0
        self.local_transform = np.eye(4)
        self.world_transform = np.eye(4)
        self.history = []
        self.max_history = 100
    def update_kinematics(self, dt: float) -> None:
        mass_moment = self.limits.stiffness if self.limits.stiffness > 0 else 1.0
        self.acceleration = (self.torque - self.limits.damping * self.velocity) / mass_moment
        self.velocity += self.acceleration * dt
        self.angle += self.velocity * dt
        if self.type in [JointType.REVOLUTE, JointType.UNIVERSAL]:
            self.angle = np.clip(self.angle, self.limits.min_angle, self.limits.max_angle)
        self.velocity = np.clip(self.velocity, -self.limits.max_velocity, self.limits.max_velocity)
        self._update_transform()
        self.history.append({'timestamp': time.time(), 'angle': self.angle, 'velocity': self.velocity, 'acceleration': self.acceleration, 'torque': self.torque})
        if len(self.history) > self.max_history:
            self.history.pop(0)
    def _update_transform(self) -> None:
        if self.type == JointType.REVOLUTE:
            cos_a, sin_a = (np.cos(self.angle), np.sin(self.angle))
            axis = self.axis / np.linalg.norm(self.axis)
            K = np.array([[0, -axis[2], axis[1]], [axis[2], 0, -axis[0]], [-axis[1], axis[0], 0]])
            R = np.eye(3) + sin_a * K + (1 - cos_a) * np.dot(K, K)
            self.local_transform[:3, :3] = R
            self.local_transform[:3, 3] = self.position
        elif self.type == JointType.PRISMATIC:
            displacement = self.angle * self.axis
            self.local_transform[:3, 3] = self.position + displacement
        elif self.type == JointType.FIXED:
            self.local_transform[:3, 3] = self.position
    def get_proprioceptive_state(self) -> Dict[str, Any]:
        return {'joint_id': self.id, 'angle': self.angle, 'velocity': self.velocity, 'acceleration': self.acceleration, 'torque': self.torque, 'position': self.position.tolist(), 'transform': self.world_transform.tolist()}
class BodySchema:
    def __init__(self, num_joints: int, schema_dim: int=64):
        self.num_joints = num_joints
        self.schema_dim = schema_dim
        self.joint_encodings = np.zeros((num_joints, schema_dim))
        self.spatial_map = np.zeros((schema_dim, schema_dim))
        self.temporal_buffer = []
        self.max_temporal_buffer = 20
        self.learning_rate = 0.001
        self.adaptation_rate = 0.01
        self.coherence_score = 1.0
        self.spatial_accuracy = 1.0
        self.temporal_consistency = 1.0
    def update_schema(self, joint_states: List[Dict[str, Any]]) -> None:
        current_encoding = np.zeros((self.num_joints, self.schema_dim))
        for i, joint_state in enumerate(joint_states[:self.num_joints]):
            angle = joint_state.get('angle', 0.0)
            velocity = joint_state.get('velocity', 0.0)
            position = joint_state.get('position', [0, 0, 0])
            encoding = np.zeros(self.schema_dim)
            encoding[0] = np.sin(angle)
            encoding[1] = np.cos(angle)
            encoding[2] = np.tanh(velocity)
            encoding[3:6] = np.array(position[:3]) / 10.0
            if i < len(self.temporal_buffer):
                prev_states = self.temporal_buffer[-3:]
                for j, prev_state in enumerate(prev_states):
                    if i < len(prev_state):
                        prev_angle = prev_state[i].get('angle', 0.0)
                        encoding[6 + j] = np.sin(prev_angle - angle)
            current_encoding[i] = encoding
        self.joint_encodings = (1 - self.adaptation_rate) * self.joint_encodings + self.adaptation_rate * current_encoding
        self._update_spatial_map(joint_states)
        self.temporal_buffer.append(joint_states.copy())
        if len(self.temporal_buffer) > self.max_temporal_buffer:
            self.temporal_buffer.pop(0)
        self._update_awareness_metrics()
    def _update_spatial_map(self, joint_states: List[Dict[str, Any]]) -> None:
        self.spatial_map.fill(0)
        for i, joint_state in enumerate(joint_states):
            position = joint_state.get('position', [0, 0, 0])
            x = int(np.clip((position[0] + 5.0) / 10.0 * self.schema_dim, 0, self.schema_dim - 1))
            y = int(np.clip((position[1] + 5.0) / 10.0 * self.schema_dim, 0, self.schema_dim - 1))
            sigma = 2.0
            for dx in range(-3, 4):
                for dy in range(-3, 4):
                    nx, ny = (x + dx, y + dy)
                    if 0 <= nx < self.schema_dim and 0 <= ny < self.schema_dim:
                        distance_sq = dx * dx + dy * dy
                        value = np.exp(-distance_sq / (2 * sigma ** 2))
                        self.spatial_map[ny, nx] = max(self.spatial_map[ny, nx], value)
    def _update_awareness_metrics(self) -> None:
        if len(self.temporal_buffer) >= 2:
            current = np.array([s.get('angle', 0) for s in self.temporal_buffer[-1]])
            previous = np.array([s.get('angle', 0) for s in self.temporal_buffer[-2]])
            angle_diff = np.mean(np.abs(current - previous))
            self.temporal_consistency = max(0.5, np.exp(-angle_diff * 2))
        self.spatial_accuracy = max(0.5, np.mean(self.spatial_map))
        self.coherence_score = (self.temporal_consistency + self.spatial_accuracy) / 2.0
    def get_body_representation(self) -> Dict[str, Any]:
        return {'joint_encodings': self.joint_encodings.tolist(), 'spatial_map': self.spatial_map.tolist(), 'coherence_score': self.coherence_score, 'spatial_accuracy': self.spatial_accuracy, 'temporal_consistency': self.temporal_consistency, 'schema_dim': self.schema_dim}
class VirtualBody(ArenaObject):
    def __init__(self, body_id: str, position: Tuple[float, float, float]=(0.0, 0.0, 0.0), body_type: str='humanoid', properties: Optional[Dict[str, Any]]=None):
        super().__init__(body_id, f'virtual_body_{body_type}', position, properties)
        self.body_type = body_type
        self.joints: Dict[str, BodyJoint] = {}
        self.joint_hierarchy: Dict[str, List[str]] = {}
        self.body_schema = BodySchema(num_joints=20)
        self.center_of_mass = np.array(position, dtype=float)
        self.total_mass = 70.0
        self.inertia_tensor = np.eye(3) * 10.0
        if body_type == 'humanoid':
            self._create_humanoid_body()
    def _create_humanoid_body(self) -> None:
        self.add_joint('base', JointType.FIXED, position=(0, 0, 0))
        self.add_joint('neck', JointType.REVOLUTE, parent_link='base', position=(0, 0, 0.6), axis=(0, 0, 1), limits=JointLimits(-np.pi / 3, np.pi / 3, 5.0))
        self.add_joint('left_shoulder', JointType.SPHERICAL, parent_link='base', position=(-0.2, 0, 0.5), limits=JointLimits(-np.pi, np.pi, 8.0))
        self.add_joint('left_elbow', JointType.REVOLUTE, parent_link='left_shoulder', position=(-0.4, 0, 0.5), axis=(0, 1, 0), limits=JointLimits(0, np.pi, 8.0))
        self.add_joint('right_shoulder', JointType.SPHERICAL, parent_link='base', position=(0.2, 0, 0.5), limits=JointLimits(-np.pi, np.pi, 8.0))
        self.add_joint('right_elbow', JointType.REVOLUTE, parent_link='right_shoulder', position=(0.4, 0, 0.5), axis=(0, 1, 0), limits=JointLimits(0, np.pi, 8.0))
        self.add_joint('left_hip', JointType.SPHERICAL, parent_link='base', position=(-0.1, 0, -0.1), limits=JointLimits(-np.pi / 2, np.pi / 2, 6.0))
        self.add_joint('left_knee', JointType.REVOLUTE, parent_link='left_hip', position=(-0.1, 0, -0.5), axis=(0, 1, 0), limits=JointLimits(-np.pi, 0, 6.0))
        self.add_joint('right_hip', JointType.SPHERICAL, parent_link='base', position=(0.1, 0, -0.1), limits=JointLimits(-np.pi / 2, np.pi / 2, 6.0))
        self.add_joint('right_knee', JointType.REVOLUTE, parent_link='right_hip', position=(0.1, 0, -0.5), axis=(0, 1, 0), limits=JointLimits(-np.pi, 0, 6.0))
    def add_joint(self, joint_id: str, joint_type: JointType, parent_link: Optional[str]=None, position: Tuple[float, float, float]=(0.0, 0.0, 0.0), axis: Tuple[float, float, float]=(0.0, 0.0, 1.0), limits: Optional[JointLimits]=None) -> None:
        joint = BodyJoint(joint_id, joint_type, parent_link, joint_id, position, axis, limits)
        self.joints[joint_id] = joint
        if parent_link:
            if parent_link not in self.joint_hierarchy:
                self.joint_hierarchy[parent_link] = []
            self.joint_hierarchy[parent_link].append(joint_id)
        self.body_schema = BodySchema(len(self.joints))
    def update_physics(self, dt: float, physics: ArenaPhysics) -> None:
        super().update_physics(dt, physics)
        self._update_kinematics(dt)
        self._forward_kinematics()
        joint_states = [joint.get_proprioceptive_state() for joint in self.joints.values()]
        self.body_schema.update_schema(joint_states)
        self._update_center_of_mass()
    def _update_kinematics(self, dt: float) -> None:
        for joint in self.joints.values():
            joint.update_kinematics(dt)
    def _forward_kinematics(self) -> None:
        base_transform = np.eye(4)
        base_transform[:3, 3] = self.position
        def compute_transform(joint_id: str, parent_transform: np.ndarray):
            joint = self.joints[joint_id]
            joint.world_transform = np.dot(parent_transform, joint.local_transform)
            if joint_id in self.joint_hierarchy:
                for child_id in self.joint_hierarchy[joint_id]:
                    compute_transform(child_id, joint.world_transform)
        if 'base' in self.joints:
            compute_transform('base', base_transform)
    def _update_center_of_mass(self) -> None:
        if not self.joints:
            return
        total_weighted_pos = np.zeros(3)
        total_weight = 0.0
        joint_weight = self.total_mass / len(self.joints)
        for joint in self.joints.values():
            world_pos = joint.world_transform[:3, 3]
            total_weighted_pos += world_pos * joint_weight
            total_weight += joint_weight
        if total_weight > 0:
            self.center_of_mass = total_weighted_pos / total_weight
    def set_joint_torque(self, joint_id: str, torque: float) -> None:
        if joint_id in self.joints:
            self.joints[joint_id].torque = torque
    def get_joint_state(self, joint_id: str) -> Optional[Dict[str, Any]]:
        if joint_id in self.joints:
            return self.joints[joint_id].get_proprioceptive_state()
        return None
    def get_all_joint_states(self) -> Dict[str, Dict[str, Any]]:
        return {jid: joint.get_proprioceptive_state() for jid, joint in self.joints.items()}
    def get_body_schema_representation(self) -> Dict[str, Any]:
        return self.body_schema.get_body_representation()
    def get_comprehensive_state(self) -> Dict[str, Any]:
        base_state = super().get_state()
        body_state = {'body_type': self.body_type, 'center_of_mass': self.center_of_mass.tolist(), 'total_mass': self.total_mass, 'joint_count': len(self.joints), 'joint_states': self.get_all_joint_states(), 'body_schema': self.get_body_schema_representation(), 'kinematics_valid': True}
        base_state.update(body_state)
        return base_state