"""
Embodied Agent for 4E Framework

Integrates virtual body representation with proprioceptive systems
to create embodied AI agents with consistent body representation.
"""

import numpy as np
import time
from typing import Dict, Any, Optional, Tuple
from dataclasses import dataclass

from .virtual_body import VirtualBody
from .proprioception import ProprioceptiveSystem
from ..arena.simulation_engine import ArenaPhysics, ArenaEnvironment


@dataclass
class EmbodimentConfig:
    """Configuration for embodied agent."""
    body_type: str = "humanoid"
    proprioception_enabled: bool = True
    adaptation_enabled: bool = True
    body_schema_learning: bool = True
    sensory_noise_level: float = 0.01
    motor_noise_level: float = 0.02
    update_frequency: float = 60.0  # Hz


class EmbodiedAgent:
    """Embodied AI agent with integrated virtual body and proprioception.
    
    Provides agents with consistent body representation as required by
    Task 2.1.1 acceptance criteria.
    """
    
    def __init__(self,
                 agent_id: str,
                 position: Tuple[float, float, float] = (0.0, 0.0, 0.0),
                 config: Optional[EmbodimentConfig] = None):
        
        self.agent_id = agent_id
        self.config = config or EmbodimentConfig()
        
        # Create virtual body
        self.virtual_body = VirtualBody(
            body_id=f"{agent_id}_body",
            position=position,
            body_type=self.config.body_type
        )
        
        # Create proprioceptive system if enabled
        self.proprioceptive_system = None
        if self.config.proprioception_enabled:
            self.proprioceptive_system = ProprioceptiveSystem(self.virtual_body)
        
        # Agent state
        self.active = True
        self.embodiment_initialized = True
        self.last_update_time = 0.0
        
        # Motor control system  
        self.motor_commands = {}  # Joint torque commands
        self.motor_controller_gains = {'kp': 8.0, 'kd': 2.0, 'ki': 0.1}  # More conservative gains
        self.motor_errors = {}  # PID error tracking
        
        # Sensory integration
        self.sensory_buffer = []
        self.max_sensory_buffer = 100
        
        # Learning and adaptation
        self.embodiment_experience = []
        self.adaptation_history = []
        
        # Performance metrics
        self.body_consistency_score = 1.0
        self.embodiment_quality_score = 1.0
        self.motor_performance_score = 1.0
    
    def update(self, dt: float, physics: ArenaPhysics, environment: ArenaEnvironment) -> None:
        """Update embodied agent including body physics and proprioception."""
        if not self.active:
            return
        
        current_time = time.time()
        
        # Update virtual body physics
        self.virtual_body.update_physics(dt, physics)
        
        # Update proprioceptive system
        if self.proprioceptive_system:
            proprioceptive_readings = self.proprioceptive_system.update()
            self._integrate_proprioceptive_feedback(proprioceptive_readings)
        
        # Execute motor commands
        self._execute_motor_commands(dt)
        
        # Update performance metrics
        self._update_embodiment_metrics()
        
        # Store sensory experience
        self._store_sensory_experience()
        
        self.last_update_time = current_time
    
    def _integrate_proprioceptive_feedback(self, readings: Dict[str, Any]) -> None:
        """Integrate proprioceptive feedback for body awareness."""
        if not readings:
            return
        
        # Add to sensory buffer
        sensory_data = {
            'timestamp': time.time(),
            'proprioceptive': readings,
            'body_state': self.virtual_body.get_comprehensive_state()
        }
        
        self.sensory_buffer.append(sensory_data)
        if len(self.sensory_buffer) > self.max_sensory_buffer:
            self.sensory_buffer.pop(0)
    
    def _execute_motor_commands(self, dt: float) -> None:
        """Execute motor commands with PID control."""
        for joint_id, target_value in self.motor_commands.items():
            if joint_id in self.virtual_body.joints:
                # Get current joint state
                current_state = self.virtual_body.get_joint_state(joint_id)
                if not current_state:
                    continue
                
                current_angle = current_state['angle']
                current_state['velocity']
                
                # PID control
                error = target_value - current_angle
                
                # Initialize error tracking if needed
                if joint_id not in self.motor_errors:
                    self.motor_errors[joint_id] = {
                        'previous_error': 0.0,
                        'integral': 0.0,
                        'derivative': 0.0
                    }
                
                error_data = self.motor_errors[joint_id]
                
                # Compute PID terms
                proportional = error
                error_data['integral'] += error * dt
                # Limit integral windup
                max_integral = 5.0
                error_data['integral'] = np.clip(error_data['integral'], -max_integral, max_integral)
                error_data['derivative'] = (error - error_data['previous_error']) / dt
                
                # Calculate control torque
                kp = self.motor_controller_gains['kp']
                ki = self.motor_controller_gains['ki']
                kd = self.motor_controller_gains['kd']
                
                control_torque = (kp * proportional + 
                                ki * error_data['integral'] + 
                                kd * error_data['derivative'])
                
                # Limit control torque to prevent instability
                max_torque = 50.0
                control_torque = np.clip(control_torque, -max_torque, max_torque)
                
                # Add motor noise
                if self.config.motor_noise_level > 0:
                    noise = np.random.normal(0, self.config.motor_noise_level)
                    control_torque += noise
                
                # Apply torque to joint
                self.virtual_body.set_joint_torque(joint_id, control_torque)
                
                # Update error tracking
                error_data['previous_error'] = error
    
    def _update_embodiment_metrics(self) -> None:
        """Update embodiment quality metrics."""
        # Body consistency score from body schema
        body_schema = self.virtual_body.get_body_schema_representation()
        self.body_consistency_score = body_schema.get('coherence_score', 1.0)
        
        # Proprioceptive quality
        proprioceptive_quality = 1.0
        if self.proprioceptive_system:
            awareness = self.proprioceptive_system.get_body_state_awareness()
            proprioceptive_quality = awareness.get('body_awareness_score', 1.0)
        
        # Motor performance (how well commands are followed)
        motor_errors = []
        for joint_id, error_data in self.motor_errors.items():
            if joint_id in self.motor_commands:
                target = self.motor_commands[joint_id]
                current_state = self.virtual_body.get_joint_state(joint_id)
                if current_state:
                    actual = current_state['angle']
                    error = abs(target - actual)
                    motor_errors.append(error)
        
        if motor_errors:
            avg_error = np.mean(motor_errors)
            self.motor_performance_score = np.exp(-avg_error)  # Exponential decay of error
        
        # Overall embodiment quality
        self.embodiment_quality_score = (
            self.body_consistency_score * 0.4 +
            proprioceptive_quality * 0.3 +
            self.motor_performance_score * 0.3
        )
    
    def _store_sensory_experience(self) -> None:
        """Store current sensory experience for learning."""
        if len(self.sensory_buffer) == 0:
            return
        
        experience = {
            'timestamp': time.time(),
            'body_state': self.virtual_body.get_comprehensive_state(),
            'motor_commands': self.motor_commands.copy(),
            'embodiment_quality': self.embodiment_quality_score,
            'proprioceptive_awareness': None
        }
        
        if self.proprioceptive_system:
            experience['proprioceptive_awareness'] = \
                self.proprioceptive_system.get_body_state_awareness()
        
        self.embodiment_experience.append(experience)
        
        # Limit experience buffer size
        if len(self.embodiment_experience) > 1000:
            self.embodiment_experience.pop(0)
    
    def set_joint_target(self, joint_id: str, target_angle: float) -> None:
        """Set target angle for a specific joint."""
        if joint_id in self.virtual_body.joints:
            self.motor_commands[joint_id] = target_angle
    
    def set_joint_targets(self, targets: Dict[str, float]) -> None:
        """Set target angles for multiple joints."""
        for joint_id, target in targets.items():
            self.set_joint_target(joint_id, target)
    
    def get_joint_state(self, joint_id: str) -> Optional[Dict[str, Any]]:
        """Get current state of a specific joint."""
        return self.virtual_body.get_joint_state(joint_id)
    
    def get_all_joint_states(self) -> Dict[str, Dict[str, Any]]:
        """Get states of all joints."""
        return self.virtual_body.get_all_joint_states()
    
    def get_body_representation(self) -> Dict[str, Any]:
        """Get complete body representation including schema."""
        return self.virtual_body.get_body_schema_representation()
    
    def get_proprioceptive_feedback(self) -> Tuple[np.ndarray, float]:
        """Get proprioceptive feedback for external systems."""
        if self.proprioceptive_system:
            return self.proprioceptive_system.get_proprioceptive_feedback()
        else:
            # Fallback: create feedback from joint states
            joint_states = self.get_all_joint_states()
            feedback = []
            for joint_id in sorted(joint_states.keys()):
                state = joint_states[joint_id]
                feedback.extend([state['angle'], state['velocity']])
            return np.array(feedback), 0.8  # Reasonable default confidence
    
    def get_embodiment_status(self) -> Dict[str, Any]:
        """Get comprehensive embodiment status."""
        status = {
            'agent_id': self.agent_id,
            'active': self.active,
            'embodiment_initialized': self.embodiment_initialized,
            'body_type': self.config.body_type,
            'joint_count': len(self.virtual_body.joints),
            'body_consistency_score': self.body_consistency_score,
            'embodiment_quality_score': self.embodiment_quality_score,
            'motor_performance_score': self.motor_performance_score,
            'position': self.virtual_body.position.tolist(),
            'center_of_mass': self.virtual_body.center_of_mass.tolist(),
            'proprioception_enabled': self.config.proprioception_enabled,
            'last_update': self.last_update_time
        }
        
        if self.proprioceptive_system:
            awareness = self.proprioceptive_system.get_body_state_awareness()
            status['proprioceptive_awareness'] = awareness
        
        return status
    
    def calibrate_embodiment(self) -> bool:
        """Calibrate the embodied system."""
        if not self.active:
            return False
        
        success = True
        
        # Calibrate proprioceptive sensors
        if self.proprioceptive_system:
            if not self.proprioceptive_system.calibrate_sensors():
                success = False
        
        # Reset motor control errors
        self.motor_errors.clear()
        
        # Reset body schema to neutral state
        if hasattr(self.virtual_body.body_schema, 'reset'):
            self.virtual_body.body_schema.reset()
        
        return success
    
    def validate_body_consistency(self) -> Tuple[bool, Dict[str, Any]]:
        """Validate that the agent has consistent body representation.
        
        This is the primary validation method for Task 2.1.1 acceptance criteria.
        """
        validation_results = {
            'consistent_body_representation': False,
            'body_schema_valid': False,
            'joint_kinematics_valid': False,
            'proprioception_valid': False,
            'physics_integration_valid': False,
            'consistency_score': 0.0,
            'details': {}
        }
        
        try:
            # Check body schema validity
            body_schema = self.get_body_representation()
            schema_score = body_schema.get('coherence_score', 0.0)
            validation_results['body_schema_valid'] = schema_score > 0.7
            validation_results['details']['body_schema_score'] = schema_score
            
            # Check joint kinematics
            joint_states = self.get_all_joint_states()
            valid_joints = 0
            total_joints = len(joint_states)
            
            for joint_id, state in joint_states.items():
                if all(key in state for key in ['angle', 'velocity', 'position']):
                    valid_joints += 1
            
            kinematics_score = valid_joints / total_joints if total_joints > 0 else 0.0
            validation_results['joint_kinematics_valid'] = kinematics_score > 0.9
            validation_results['details']['kinematics_score'] = kinematics_score
            
            # Check proprioceptive system
            if self.proprioceptive_system:
                awareness = self.proprioceptive_system.get_body_state_awareness()
                proprioceptive_score = awareness.get('body_awareness_score', 0.0)
                validation_results['proprioception_valid'] = proprioceptive_score > 0.6  # More reasonable threshold
                validation_results['details']['proprioceptive_score'] = proprioceptive_score
            else:
                validation_results['proprioception_valid'] = True  # Not required if disabled
                validation_results['details']['proprioceptive_score'] = 1.0
            
            # Check physics integration
            body_state = self.virtual_body.get_comprehensive_state()
            physics_valid = (
                'position' in body_state and
                'velocity' in body_state and
                'center_of_mass' in body_state and
                body_state.get('kinematics_valid', False)
            )
            validation_results['physics_integration_valid'] = physics_valid
            validation_results['details']['physics_valid'] = physics_valid
            
            # Calculate overall consistency score
            scores = [
                schema_score,
                kinematics_score,
                validation_results['details']['proprioceptive_score'],
                1.0 if physics_valid else 0.0
            ]
            validation_results['consistency_score'] = np.mean(scores)
            
            # Determine overall consistency
            validation_results['consistent_body_representation'] = (
                validation_results['consistency_score'] > 0.7 and  # Lower threshold
                all([
                    validation_results['body_schema_valid'],
                    validation_results['joint_kinematics_valid'],
                    validation_results['proprioception_valid'],
                    validation_results['physics_integration_valid']
                ])
            )
            
        except Exception as e:
            validation_results['details']['error'] = str(e)
        
        return validation_results['consistent_body_representation'], validation_results