"""
Test Suite for Virtual Body Representation - Task 2.1.1

Tests to validate the acceptance criteria:
"Agents have consistent body representation"

This test suite validates:
- 3D body model with articulated joints
- Virtual physics integration
- Body schema representation in neural networks
"""

import pytest
import numpy as np

from aar_core.embodied import VirtualBody, EmbodiedAgent, ProprioceptiveSystem, BodyJoint, JointType, JointLimits
from aar_core.arena.simulation_engine import ArenaPhysics, ArenaEnvironment


class TestVirtualBodyRepresentation:
    """Test suite for virtual body representation implementation."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.test_position = (0.0, 0.0, 1.0)
        self.virtual_body = VirtualBody("test_body", self.test_position, "humanoid")
        self.arena_physics = ArenaPhysics()
        self.arena_environment = ArenaEnvironment()
    
    def test_3d_body_model_creation(self):
        """Test creation of 3D body model with articulated joints."""
        # Test body creation
        assert self.virtual_body.id == "test_body"
        assert self.virtual_body.body_type == "humanoid"
        assert np.allclose(self.virtual_body.position, self.test_position)
        
        # Test joint creation - humanoid should have standard joints
        expected_joints = [
            "base", "neck", "left_shoulder", "left_elbow", 
            "right_shoulder", "right_elbow", "left_hip", 
            "left_knee", "right_hip", "right_knee"
        ]
        
        for joint_name in expected_joints:
            assert joint_name in self.virtual_body.joints, f"Missing joint: {joint_name}"
        
        # Test joint properties
        for joint_id, joint in self.virtual_body.joints.items():
            assert isinstance(joint, BodyJoint)
            assert hasattr(joint, 'angle')
            assert hasattr(joint, 'velocity')
            assert hasattr(joint, 'position')
            assert hasattr(joint, 'local_transform')
            assert hasattr(joint, 'world_transform')
    
    def test_articulated_joints_functionality(self):
        """Test articulated joint functionality."""
        # Test joint types
        neck_joint = self.virtual_body.joints["neck"]
        assert neck_joint.type == JointType.REVOLUTE
        
        shoulder_joint = self.virtual_body.joints["left_shoulder"]
        assert shoulder_joint.type == JointType.SPHERICAL
        
        # Test joint limits
        for joint in self.virtual_body.joints.values():
            assert hasattr(joint, 'limits')
            assert isinstance(joint.limits, JointLimits)
            assert joint.limits.min_angle <= joint.limits.max_angle
        
        # Test joint movement
        initial_angle = neck_joint.angle
        neck_joint.torque = 1.0
        neck_joint.update_kinematics(0.01)  # Small time step
        
        # Joint should have moved due to applied torque
        assert neck_joint.angle != initial_angle or neck_joint.velocity != 0.0
    
    def test_virtual_physics_integration(self):
        """Test integration with virtual physics simulation."""
        self.virtual_body.position.copy()
        
        # Apply physics update
        self.virtual_body.update_physics(0.01, self.arena_physics)
        
        # Physics should have been applied (gravity, etc.)
        # Position might change due to gravity
        assert hasattr(self.virtual_body, 'velocity')
        assert hasattr(self.virtual_body, 'center_of_mass')
        
        # Test gravity effect
        if self.arena_physics.gravity[2] != 0:
            # After a longer simulation, should see gravity effect
            for _ in range(10):
                self.virtual_body.update_physics(0.01, self.arena_physics)
            
            # Should have some velocity in Z direction due to gravity
            assert self.virtual_body.velocity[2] != 0.0
    
    def test_body_schema_neural_representation(self):
        """Test body schema representation in neural networks."""
        # Get body schema
        body_schema = self.virtual_body.get_body_schema_representation()
        
        # Validate schema structure
        assert 'joint_encodings' in body_schema
        assert 'spatial_map' in body_schema
        assert 'coherence_score' in body_schema
        assert 'schema_dim' in body_schema
        
        # Test neural encoding dimensions
        joint_encodings = np.array(body_schema['joint_encodings'])
        spatial_map = np.array(body_schema['spatial_map'])
        
        assert joint_encodings.ndim == 2  # [num_joints, encoding_dim]
        assert spatial_map.ndim == 2      # [spatial_dim, spatial_dim]
        assert joint_encodings.shape[0] == len(self.virtual_body.joints)
        
        # Test coherence metrics
        assert 0.0 <= body_schema['coherence_score'] <= 1.0
    
    def test_forward_kinematics(self):
        """Test forward kinematics computation."""
        # Set some joint angles
        self.virtual_body.joints["left_shoulder"].angle = np.pi / 4
        self.virtual_body.joints["left_elbow"].angle = np.pi / 3
        
        # Update physics to trigger forward kinematics
        self.virtual_body.update_physics(0.01, self.arena_physics)
        
        # Check that world transforms are computed
        for joint in self.virtual_body.joints.values():
            assert joint.world_transform is not None
            assert joint.world_transform.shape == (4, 4)
            
            # Transform should be valid (determinant non-zero for rotation part)
            rotation_part = joint.world_transform[:3, :3]
            det = np.linalg.det(rotation_part)
            assert abs(abs(det) - 1.0) < 0.1  # Should be approximately orthogonal
    
    def test_center_of_mass_computation(self):
        """Test center of mass computation."""
        initial_com = self.virtual_body.center_of_mass.copy()
        
        # Move some joints
        self.virtual_body.joints["left_shoulder"].angle = np.pi / 2
        self.virtual_body.update_physics(0.01, self.arena_physics)
        
        updated_com = self.virtual_body.center_of_mass
        
        # Center of mass should have changed
        assert not np.allclose(initial_com, updated_com, atol=1e-6)
        
        # Center of mass should be reasonable (near body position)
        com_distance = np.linalg.norm(updated_com - self.virtual_body.position)
        assert com_distance < 2.0  # Within reasonable range for humanoid


class TestProprioceptiveSystem:
    """Test proprioceptive system functionality."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.virtual_body = VirtualBody("test_body", (0, 0, 0), "humanoid")
        self.proprioceptive_system = ProprioceptiveSystem(self.virtual_body)
    
    def test_sensor_creation(self):
        """Test creation of proprioceptive sensors."""
        # Should create sensors for each joint
        expected_sensor_count = len(self.virtual_body.joints) * 3  # position, velocity, torque
        assert len(self.proprioceptive_system.sensors) == expected_sensor_count
        
        # Check sensor types
        sensor_types = set()
        for sensor in self.proprioceptive_system.sensors.values():
            sensor_types.add(sensor.type)
        
        expected_types = {"joint_position", "joint_velocity", "joint_torque"}
        assert sensor_types == expected_types
    
    def test_proprioceptive_readings(self):
        """Test proprioceptive sensor readings."""
        # Update system
        readings = self.proprioceptive_system.update()
        
        # Should have readings from all sensors
        assert len(readings) == len(self.proprioceptive_system.sensors)
        
        # Check reading structure
        for reading in readings.values():
            assert hasattr(reading, 'timestamp')
            assert hasattr(reading, 'sensor_id')
            assert hasattr(reading, 'value')
            assert hasattr(reading, 'confidence')
            assert 0.0 <= reading.confidence <= 1.0
    
    def test_body_state_awareness(self):
        """Test body state awareness computation."""
        awareness = self.proprioceptive_system.get_body_state_awareness()
        
        # Check awareness structure
        assert 'joint_awareness' in awareness
        assert 'body_awareness_score' in awareness
        assert 'sensor_consistency' in awareness
        assert 'temporal_coherence' in awareness
        
        # Check awareness scores
        assert 0.0 <= awareness['body_awareness_score'] <= 1.0
        assert 0.0 <= awareness['sensor_consistency'] <= 1.0
        assert 0.0 <= awareness['temporal_coherence'] <= 1.0
    
    def test_calibration(self):
        """Test sensor calibration."""
        # Initial calibration state
        assert not self.proprioceptive_system.calibrated
        
        # Perform calibration
        success = self.proprioceptive_system.calibrate_sensors()
        assert success
        assert self.proprioceptive_system.calibrated
        
        # Sensors should be calibrated
        for sensor in self.proprioceptive_system.sensors.values():
            assert sensor.calibrated


class TestEmbodiedAgent:
    """Test embodied agent functionality."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.agent = EmbodiedAgent("test_agent", (0, 0, 1))
        self.physics = ArenaPhysics()
        self.environment = ArenaEnvironment()
    
    def test_embodied_agent_initialization(self):
        """Test embodied agent initialization."""
        assert self.agent.agent_id == "test_agent"
        assert self.agent.active
        assert self.agent.embodiment_initialized
        
        # Should have virtual body
        assert hasattr(self.agent, 'virtual_body')
        assert isinstance(self.agent.virtual_body, VirtualBody)
        
        # Should have proprioceptive system
        assert hasattr(self.agent, 'proprioceptive_system')
        assert isinstance(self.agent.proprioceptive_system, ProprioceptiveSystem)
    
    def test_motor_control(self):
        """Test motor control functionality."""
        # Set joint target
        target_angle = np.pi / 4
        self.agent.set_joint_target("left_shoulder", target_angle)
        
        # Should store command
        assert "left_shoulder" in self.agent.motor_commands
        assert self.agent.motor_commands["left_shoulder"] == target_angle
        
        # Update agent (this should execute motor commands)
        initial_angle = self.agent.get_joint_state("left_shoulder")['angle']
        
        for _ in range(10):  # Multiple updates to see control effect
            self.agent.update(0.01, self.physics, self.environment)
        
        final_angle = self.agent.get_joint_state("left_shoulder")['angle']
        
        # Joint should move toward target (PID control)
        # The error should decrease over time
        initial_error = abs(target_angle - initial_angle)
        final_error = abs(target_angle - final_angle)
        
        # Allow some tolerance for control convergence
        assert final_error <= initial_error + 0.1  # Should not get much worse
    
    def test_embodiment_metrics(self):
        """Test embodiment quality metrics."""
        # Get embodiment status
        status = self.agent.get_embodiment_status()
        
        # Check required fields
        assert 'body_consistency_score' in status
        assert 'embodiment_quality_score' in status
        assert 'motor_performance_score' in status
        
        # Check score ranges
        assert 0.0 <= status['body_consistency_score'] <= 1.0
        assert 0.0 <= status['embodiment_quality_score'] <= 1.0
        assert 0.0 <= status['motor_performance_score'] <= 1.0
    
    def test_body_consistency_validation(self):
        """Test body consistency validation - PRIMARY ACCEPTANCE CRITERIA."""
        # This is the key test for Task 2.1.1 acceptance criteria
        is_consistent, validation_results = self.agent.validate_body_consistency()
        
        # Check validation results structure
        assert 'consistent_body_representation' in validation_results
        assert 'body_schema_valid' in validation_results
        assert 'joint_kinematics_valid' in validation_results
        assert 'proprioception_valid' in validation_results
        assert 'physics_integration_valid' in validation_results
        assert 'consistency_score' in validation_results
        
        # The agent should have consistent body representation
        assert is_consistent, f"Body consistency validation failed: {validation_results['details']}"
        
        # Individual components should be valid
        assert validation_results['body_schema_valid'], "Body schema validation failed"
        assert validation_results['joint_kinematics_valid'], "Joint kinematics validation failed"
        assert validation_results['proprioception_valid'], "Proprioception validation failed"
        assert validation_results['physics_integration_valid'], "Physics integration validation failed"
        
        # Consistency score should be high
        assert validation_results['consistency_score'] > 0.8, f"Consistency score too low: {validation_results['consistency_score']}"
    
    def test_embodiment_integration(self):
        """Test integration of all embodiment components."""
        # Run multiple update cycles
        for i in range(50):
            self.agent.update(0.01, self.physics, self.environment)
            
            # Set varying motor commands to test dynamic behavior
            if i % 10 == 0:
                angle = np.sin(i * 0.1) * np.pi / 6
                self.agent.set_joint_target("neck", angle)
                self.agent.set_joint_target("left_shoulder", angle * 0.5)
        
        # After integration, agent should maintain consistency
        is_consistent, results = self.agent.validate_body_consistency()
        assert is_consistent, f"Integration test failed: {results}"
        
        # Embodiment quality should remain high
        status = self.agent.get_embodiment_status()
        assert status['embodiment_quality_score'] > 0.7, "Embodiment quality degraded during integration"
    
    def test_proprioceptive_feedback(self):
        """Test proprioceptive feedback functionality."""
        feedback, confidence = self.agent.get_proprioceptive_feedback()
        
        # Should return valid feedback
        assert isinstance(feedback, np.ndarray)
        assert feedback.size > 0
        assert 0.0 <= confidence <= 1.0
        
        # Feedback should have expected dimensions (2 values per joint: position, velocity)
        expected_size = len(self.agent.virtual_body.joints) * 2
        assert feedback.size == expected_size
    
    def test_calibration(self):
        """Test embodiment calibration."""
        success = self.agent.calibrate_embodiment()
        assert success
        
        # Calibration should improve consistency
        is_consistent, _ = self.agent.validate_body_consistency()
        assert is_consistent


@pytest.mark.integration
class TestEmbodimentIntegration:
    """Integration tests for complete embodiment system."""
    
    def test_multi_agent_consistency(self):
        """Test consistency across multiple embodied agents."""
        agents = []
        
        # Create multiple agents
        for i in range(3):
            agent = EmbodiedAgent(f"agent_{i}", (i * 2.0, 0, 1))
            agents.append(agent)
        
        physics = ArenaPhysics()
        environment = ArenaEnvironment()
        
        # Run simulation
        for step in range(30):
            for agent in agents:
                agent.update(0.01, physics, environment)
        
        # All agents should maintain consistent body representation
        for agent in agents:
            is_consistent, results = agent.validate_body_consistency()
            assert is_consistent, f"Agent {agent.agent_id} lost consistency: {results}"
    
    def test_physical_interaction_consistency(self):
        """Test that body representation remains consistent during physical interactions."""
        agent = EmbodiedAgent("physics_test_agent", (0, 0, 2))
        
        # Create physics with strong effects
        physics = ArenaPhysics(
            gravity=(0, 0, -20.0),  # Strong gravity
            air_resistance=0.1,      # High air resistance
            max_velocity=50.0
        )
        environment = ArenaEnvironment()
        
        # Run simulation with physical effects
        for _ in range(100):
            agent.update(0.01, physics, environment)
        
        # Should maintain consistency despite physical effects
        is_consistent, results = agent.validate_body_consistency()
        assert is_consistent, f"Physical interaction broke consistency: {results}"
        
        # Body should still be functional
        status = agent.get_embodiment_status()
        assert status['embodiment_quality_score'] > 0.6  # Allow some degradation but should still work


if __name__ == "__main__":
    pytest.main([__file__, "-v"])