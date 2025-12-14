"""
Virtual Body Representation with Articulated Joints

Implements 3D body model with articulated joints for embodied AI agents.
Integrates with existing arena physics simulation.
"""

import numpy as np
import time
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass
from enum import Enum

from ..arena.simulation_engine import ArenaObject, ArenaPhysics


class JointType(Enum):
    """Types of body joints."""
    REVOLUTE = "revolute"  # Hinge joint (1 DOF rotation)
    PRISMATIC = "prismatic"  # Sliding joint (1 DOF translation)
    SPHERICAL = "spherical"  # Ball joint (3 DOF rotation)
    FIXED = "fixed"  # No degrees of freedom
    UNIVERSAL = "universal"  # 2 DOF rotation


@dataclass
class JointLimits:
    """Joint movement limits."""
    min_angle: float = -np.pi
    max_angle: float = np.pi
    max_velocity: float = 10.0  # rad/s or m/s
    max_torque: float = 100.0  # N·m or N
    damping: float = 0.1
    stiffness: float = 1.0


class BodyJoint:
    """Represents an articulated joint in a virtual body."""
    
    def __init__(self,
                 joint_id: str,
                 joint_type: JointType,
                 parent_link: Optional[str] = None,
                 child_link: Optional[str] = None,
                 position: Tuple[float, float, float] = (0.0, 0.0, 0.0),
                 axis: Tuple[float, float, float] = (0.0, 0.0, 1.0),
                 limits: Optional[JointLimits] = None):
        
        self.id = joint_id
        self.type = joint_type
        self.parent_link = parent_link
        self.child_link = child_link
        self.position = np.array(position, dtype=float)
        self.axis = np.array(axis, dtype=float)
        self.limits = limits or JointLimits()
        
        # Joint state
        self.angle = 0.0  # Current joint angle/position
        self.velocity = 0.0  # Current joint velocity
        self.acceleration = 0.0  # Current joint acceleration
        self.torque = 0.0  # Applied torque/force
        
        # Transform matrices
        self.local_transform = np.eye(4)
        self.world_transform = np.eye(4)
        
        # Joint history for proprioception
        self.history = []
        self.max_history = 100
    
    def update_kinematics(self, dt: float) -> None:
        """Update joint kinematics."""
        # Apply torque to acceleration (simplified dynamics)
        mass_moment = self.limits.stiffness if self.limits.stiffness > 0 else 1.0
        self.acceleration = (self.torque - self.limits.damping * self.velocity) / mass_moment
        
        # Integrate to get velocity and position
        self.velocity += self.acceleration * dt
        self.angle += self.velocity * dt
        
        # Apply joint limits
        if self.type in [JointType.REVOLUTE, JointType.UNIVERSAL]:
            self.angle = np.clip(self.angle, self.limits.min_angle, self.limits.max_angle)
        
        # Apply velocity limits
        self.velocity = np.clip(self.velocity, -self.limits.max_velocity, self.limits.max_velocity)
        
        # Update transform based on joint type and angle
        self._update_transform()
        
        # Record history for proprioception
        self.history.append({
            'timestamp': time.time(),
            'angle': self.angle,
            'velocity': self.velocity,
            'acceleration': self.acceleration,
            'torque': self.torque
        })
        
        if len(self.history) > self.max_history:
            self.history.pop(0)
    
    def _update_transform(self) -> None:
        """Update local transformation matrix based on joint angle."""
        if self.type == JointType.REVOLUTE:
            # Rotation around axis
            cos_a, sin_a = np.cos(self.angle), np.sin(self.angle)
            axis = self.axis / np.linalg.norm(self.axis)
            
            # Rodrigues rotation formula
            K = np.array([[0, -axis[2], axis[1]],
                         [axis[2], 0, -axis[0]],
                         [-axis[1], axis[0], 0]])
            
            R = np.eye(3) + sin_a * K + (1 - cos_a) * np.dot(K, K)
            
            self.local_transform[:3, :3] = R
            self.local_transform[:3, 3] = self.position
            
        elif self.type == JointType.PRISMATIC:
            # Translation along axis
            displacement = self.angle * self.axis
            self.local_transform[:3, 3] = self.position + displacement
            
        elif self.type == JointType.FIXED:
            # No movement
            self.local_transform[:3, 3] = self.position
    
    def get_proprioceptive_state(self) -> Dict[str, Any]:
        """Get proprioceptive information about joint state."""
        return {
            'joint_id': self.id,
            'angle': self.angle,
            'velocity': self.velocity,
            'acceleration': self.acceleration,
            'torque': self.torque,
            'position': self.position.tolist(),
            'transform': self.world_transform.tolist()
        }


class BodySchema:
    """Neural network representation of body schema.
    
    Maintains spatial representation of body configuration and dynamics
    for embodied cognition.
    """
    
    def __init__(self, num_joints: int, schema_dim: int = 64):
        self.num_joints = num_joints
        self.schema_dim = schema_dim
        
        # Body schema neural representation
        # This is a simplified neural network representation
        self.joint_encodings = np.zeros((num_joints, schema_dim))
        self.spatial_map = np.zeros((schema_dim, schema_dim))  # 2D spatial representation
        self.temporal_buffer = []  # For temporal dynamics
        self.max_temporal_buffer = 20
        
        # Learning parameters
        self.learning_rate = 0.001
        self.adaptation_rate = 0.01
        
        # Body awareness metrics
        self.coherence_score = 1.0
        self.spatial_accuracy = 1.0
        self.temporal_consistency = 1.0
    
    def update_schema(self, joint_states: List[Dict[str, Any]]) -> None:
        """Update body schema based on current joint states."""
        current_encoding = np.zeros((self.num_joints, self.schema_dim))
        
        for i, joint_state in enumerate(joint_states[:self.num_joints]):
            # Encode joint state into neural representation
            angle = joint_state.get('angle', 0.0)
            velocity = joint_state.get('velocity', 0.0)
            position = joint_state.get('position', [0, 0, 0])
            
            # Simple encoding: angle, velocity, and position features
            encoding = np.zeros(self.schema_dim)
            encoding[0] = np.sin(angle)  # Angle encoding
            encoding[1] = np.cos(angle)
            encoding[2] = np.tanh(velocity)  # Velocity encoding
            encoding[3:6] = np.array(position[:3]) / 10.0  # Position encoding (normalized)
            
            # Add temporal context if available
            if i < len(self.temporal_buffer):
                prev_states = self.temporal_buffer[-3:]  # Last 3 states
                for j, prev_state in enumerate(prev_states):
                    if i < len(prev_state):
                        prev_angle = prev_state[i].get('angle', 0.0)
                        encoding[6 + j] = np.sin(prev_angle - angle)  # Angular difference
            
            current_encoding[i] = encoding
        
        # Update joint encodings with adaptation
        self.joint_encodings = (1 - self.adaptation_rate) * self.joint_encodings + \
                              self.adaptation_rate * current_encoding
        
        # Update spatial map (simplified 2D projection)
        self._update_spatial_map(joint_states)
        
        # Add to temporal buffer
        self.temporal_buffer.append(joint_states.copy())
        if len(self.temporal_buffer) > self.max_temporal_buffer:
            self.temporal_buffer.pop(0)
        
        # Update body awareness metrics
        self._update_awareness_metrics()
    
    def _update_spatial_map(self, joint_states: List[Dict[str, Any]]) -> None:
        """Update 2D spatial representation of body configuration."""
        self.spatial_map.fill(0)
        
        for i, joint_state in enumerate(joint_states):
            position = joint_state.get('position', [0, 0, 0])
            
            # Project 3D position to 2D map coordinates
            x = int(np.clip((position[0] + 5.0) / 10.0 * self.schema_dim, 0, self.schema_dim - 1))
            y = int(np.clip((position[1] + 5.0) / 10.0 * self.schema_dim, 0, self.schema_dim - 1))
            
            # Add Gaussian blob around joint position
            sigma = 2.0
            for dx in range(-3, 4):
                for dy in range(-3, 4):
                    nx, ny = x + dx, y + dy
                    if 0 <= nx < self.schema_dim and 0 <= ny < self.schema_dim:
                        distance_sq = dx*dx + dy*dy
                        value = np.exp(-distance_sq / (2 * sigma**2))
                        self.spatial_map[ny, nx] = max(self.spatial_map[ny, nx], value)
    
    def _update_awareness_metrics(self) -> None:
        """Update body awareness quality metrics."""
        # Coherence: how consistent is the body representation
        if len(self.temporal_buffer) >= 2:
            current = np.array([s.get('angle', 0) for s in self.temporal_buffer[-1]])
            previous = np.array([s.get('angle', 0) for s in self.temporal_buffer[-2]])
            angle_diff = np.mean(np.abs(current - previous))
            self.temporal_consistency = max(0.5, np.exp(-angle_diff * 2))  # More stable
        
        # Spatial accuracy: how well defined is the spatial map
        self.spatial_accuracy = max(0.5, np.mean(self.spatial_map))  # Ensure minimum
        
        # Overall coherence
        self.coherence_score = (self.temporal_consistency + self.spatial_accuracy) / 2.0
    
    def get_body_representation(self) -> Dict[str, Any]:
        """Get current body schema representation."""
        return {
            'joint_encodings': self.joint_encodings.tolist(),
            'spatial_map': self.spatial_map.tolist(),
            'coherence_score': self.coherence_score,
            'spatial_accuracy': self.spatial_accuracy,
            'temporal_consistency': self.temporal_consistency,
            'schema_dim': self.schema_dim
        }


class VirtualBody(ArenaObject):
    """Virtual body with articulated joints extending ArenaObject.
    
    Provides embodied agents with consistent 3D body representation
    integrated with arena physics simulation.
    """
    
    def __init__(self,
                 body_id: str,
                 position: Tuple[float, float, float] = (0.0, 0.0, 0.0),
                 body_type: str = "humanoid",
                 properties: Optional[Dict[str, Any]] = None):
        
        super().__init__(body_id, f"virtual_body_{body_type}", position, properties)
        
        self.body_type = body_type
        self.joints: Dict[str, BodyJoint] = {}
        self.joint_hierarchy: Dict[str, List[str]] = {}  # parent -> children
        
        # Initialize body schema
        self.body_schema = BodySchema(num_joints=20)  # Start with capacity for 20 joints
        
        # Body dynamics
        self.center_of_mass = np.array(position, dtype=float)
        self.total_mass = 70.0  # kg (default humanoid mass)
        self.inertia_tensor = np.eye(3) * 10.0  # Simplified inertia
        
        # Create default humanoid body structure
        if body_type == "humanoid":
            self._create_humanoid_body()
    
    def _create_humanoid_body(self) -> None:
        """Create a basic humanoid body structure."""
        # Torso (base)
        self.add_joint("base", JointType.FIXED, position=(0, 0, 0))
        
        # Head/neck
        self.add_joint("neck", JointType.REVOLUTE, 
                      parent_link="base", 
                      position=(0, 0, 0.6),
                      axis=(0, 0, 1),
                      limits=JointLimits(-np.pi/3, np.pi/3, 5.0))
        
        # Arms
        # Left shoulder
        self.add_joint("left_shoulder", JointType.SPHERICAL, 
                      parent_link="base", 
                      position=(-0.2, 0, 0.5),
                      limits=JointLimits(-np.pi, np.pi, 8.0))
        
        # Left elbow
        self.add_joint("left_elbow", JointType.REVOLUTE, 
                      parent_link="left_shoulder", 
                      position=(-0.4, 0, 0.5),
                      axis=(0, 1, 0),
                      limits=JointLimits(0, np.pi, 8.0))
        
        # Right shoulder
        self.add_joint("right_shoulder", JointType.SPHERICAL, 
                      parent_link="base", 
                      position=(0.2, 0, 0.5),
                      limits=JointLimits(-np.pi, np.pi, 8.0))
        
        # Right elbow
        self.add_joint("right_elbow", JointType.REVOLUTE, 
                      parent_link="right_shoulder", 
                      position=(0.4, 0, 0.5),
                      axis=(0, 1, 0),
                      limits=JointLimits(0, np.pi, 8.0))
        
        # Legs
        # Left hip
        self.add_joint("left_hip", JointType.SPHERICAL, 
                      parent_link="base", 
                      position=(-0.1, 0, -0.1),
                      limits=JointLimits(-np.pi/2, np.pi/2, 6.0))
        
        # Left knee
        self.add_joint("left_knee", JointType.REVOLUTE, 
                      parent_link="left_hip", 
                      position=(-0.1, 0, -0.5),
                      axis=(0, 1, 0),
                      limits=JointLimits(-np.pi, 0, 6.0))
        
        # Right hip
        self.add_joint("right_hip", JointType.SPHERICAL, 
                      parent_link="base", 
                      position=(0.1, 0, -0.1),
                      limits=JointLimits(-np.pi/2, np.pi/2, 6.0))
        
        # Right knee
        self.add_joint("right_knee", JointType.REVOLUTE, 
                      parent_link="right_hip", 
                      position=(0.1, 0, -0.5),
                      axis=(0, 1, 0),
                      limits=JointLimits(-np.pi, 0, 6.0))
    
    def add_joint(self, 
                  joint_id: str, 
                  joint_type: JointType,
                  parent_link: Optional[str] = None,
                  position: Tuple[float, float, float] = (0.0, 0.0, 0.0),
                  axis: Tuple[float, float, float] = (0.0, 0.0, 1.0),
                  limits: Optional[JointLimits] = None) -> None:
        """Add a new joint to the body."""
        
        joint = BodyJoint(joint_id, joint_type, parent_link, joint_id, position, axis, limits)
        self.joints[joint_id] = joint
        
        # Update hierarchy
        if parent_link:
            if parent_link not in self.joint_hierarchy:
                self.joint_hierarchy[parent_link] = []
            self.joint_hierarchy[parent_link].append(joint_id)
        
        # Update body schema with new joint count
        self.body_schema = BodySchema(len(self.joints))
    
    def update_physics(self, dt: float, physics: ArenaPhysics) -> None:
        """Update virtual body physics including joint dynamics."""
        # Update parent physics (position, velocity, gravity effects)
        super().update_physics(dt, physics)
        
        # Update all joint kinematics
        self._update_kinematics(dt)
        
        # Update forward kinematics to get world transforms
        self._forward_kinematics()
        
        # Update body schema with current joint states
        joint_states = [joint.get_proprioceptive_state() for joint in self.joints.values()]
        self.body_schema.update_schema(joint_states)
        
        # Update center of mass based on joint positions
        self._update_center_of_mass()
    
    def _update_kinematics(self, dt: float) -> None:
        """Update kinematics for all joints."""
        for joint in self.joints.values():
            joint.update_kinematics(dt)
    
    def _forward_kinematics(self) -> None:
        """Compute world transforms for all joints using forward kinematics."""
        # Start with base transform at body position
        base_transform = np.eye(4)
        base_transform[:3, 3] = self.position
        
        # Recursively compute transforms
        def compute_transform(joint_id: str, parent_transform: np.ndarray):
            joint = self.joints[joint_id]
            joint.world_transform = np.dot(parent_transform, joint.local_transform)
            
            # Process children
            if joint_id in self.joint_hierarchy:
                for child_id in self.joint_hierarchy[joint_id]:
                    compute_transform(child_id, joint.world_transform)
        
        # Start from base joint
        if "base" in self.joints:
            compute_transform("base", base_transform)
    
    def _update_center_of_mass(self) -> None:
        """Update center of mass based on current joint configuration."""
        if not self.joints:
            return
            
        total_weighted_pos = np.zeros(3)
        total_weight = 0.0
        
        # Simple approximation: each joint has equal weight
        joint_weight = self.total_mass / len(self.joints)
        
        for joint in self.joints.values():
            world_pos = joint.world_transform[:3, 3]
            total_weighted_pos += world_pos * joint_weight
            total_weight += joint_weight
        
        if total_weight > 0:
            self.center_of_mass = total_weighted_pos / total_weight
    
    def set_joint_torque(self, joint_id: str, torque: float) -> None:
        """Apply torque to a specific joint."""
        if joint_id in self.joints:
            self.joints[joint_id].torque = torque
    
    def get_joint_state(self, joint_id: str) -> Optional[Dict[str, Any]]:
        """Get proprioceptive state of a specific joint."""
        if joint_id in self.joints:
            return self.joints[joint_id].get_proprioceptive_state()
        return None
    
    def get_all_joint_states(self) -> Dict[str, Dict[str, Any]]:
        """Get proprioceptive states of all joints."""
        return {jid: joint.get_proprioceptive_state() for jid, joint in self.joints.items()}
    
    def get_body_schema_representation(self) -> Dict[str, Any]:
        """Get current body schema neural representation."""
        return self.body_schema.get_body_representation()
    
    def get_comprehensive_state(self) -> Dict[str, Any]:
        """Get comprehensive body state including physics and schema."""
        base_state = super().get_state()
        
        body_state = {
            'body_type': self.body_type,
            'center_of_mass': self.center_of_mass.tolist(),
            'total_mass': self.total_mass,
            'joint_count': len(self.joints),
            'joint_states': self.get_all_joint_states(),
            'body_schema': self.get_body_schema_representation(),
            'kinematics_valid': True
        }
        
        # Merge with base state
        base_state.update(body_state)
        return base_state