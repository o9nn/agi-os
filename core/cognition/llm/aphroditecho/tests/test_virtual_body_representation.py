import pytest
import numpy as np
from aar_core.embodied import VirtualBody, EmbodiedAgent, ProprioceptiveSystem, BodyJoint, JointType, JointLimits
from aar_core.arena.simulation_engine import ArenaPhysics, ArenaEnvironment
class TestVirtualBodyRepresentation:
    def setup_method(self):
        self.test_position = (0.0, 0.0, 1.0)
        self.virtual_body = VirtualBody('test_body', self.test_position, 'humanoid')
        self.arena_physics = ArenaPhysics()
        self.arena_environment = ArenaEnvironment()
    def test_3d_body_model_creation(self):
        assert self.virtual_body.id == 'test_body'
        assert self.virtual_body.body_type == 'humanoid'
        assert np.allclose(self.virtual_body.position, self.test_position)
        expected_joints = ['base', 'neck', 'left_shoulder', 'left_elbow', 'right_shoulder', 'right_elbow', 'left_hip', 'left_knee', 'right_hip', 'right_knee']
        for joint_name in expected_joints:
            assert joint_name in self.virtual_body.joints, f'Missing joint: {joint_name}'
        for joint_id, joint in self.virtual_body.joints.items():
            assert isinstance(joint, BodyJoint)
            assert hasattr(joint, 'angle')
            assert hasattr(joint, 'velocity')
            assert hasattr(joint, 'position')
            assert hasattr(joint, 'local_transform')
            assert hasattr(joint, 'world_transform')
    def test_articulated_joints_functionality(self):
        neck_joint = self.virtual_body.joints['neck']
        assert neck_joint.type == JointType.REVOLUTE
        shoulder_joint = self.virtual_body.joints['left_shoulder']
        assert shoulder_joint.type == JointType.SPHERICAL
        for joint in self.virtual_body.joints.values():
            assert hasattr(joint, 'limits')
            assert isinstance(joint.limits, JointLimits)
            assert joint.limits.min_angle <= joint.limits.max_angle
        initial_angle = neck_joint.angle
        neck_joint.torque = 1.0
        neck_joint.update_kinematics(0.01)
        assert neck_joint.angle != initial_angle or neck_joint.velocity != 0.0
    def test_virtual_physics_integration(self):
        self.virtual_body.position.copy()
        self.virtual_body.update_physics(0.01, self.arena_physics)
        assert hasattr(self.virtual_body, 'velocity')
        assert hasattr(self.virtual_body, 'center_of_mass')
        if self.arena_physics.gravity[2] != 0:
            for _ in range(10):
                self.virtual_body.update_physics(0.01, self.arena_physics)
            assert self.virtual_body.velocity[2] != 0.0
    def test_body_schema_neural_representation(self):
        body_schema = self.virtual_body.get_body_schema_representation()
        assert 'joint_encodings' in body_schema
        assert 'spatial_map' in body_schema
        assert 'coherence_score' in body_schema
        assert 'schema_dim' in body_schema
        joint_encodings = np.array(body_schema['joint_encodings'])
        spatial_map = np.array(body_schema['spatial_map'])
        assert joint_encodings.ndim == 2
        assert spatial_map.ndim == 2
        assert joint_encodings.shape[0] == len(self.virtual_body.joints)
        assert 0.0 <= body_schema['coherence_score'] <= 1.0
    def test_forward_kinematics(self):
        self.virtual_body.joints['left_shoulder'].angle = np.pi / 4
        self.virtual_body.joints['left_elbow'].angle = np.pi / 3
        self.virtual_body.update_physics(0.01, self.arena_physics)
        for joint in self.virtual_body.joints.values():
            assert joint.world_transform is not None
            assert joint.world_transform.shape == (4, 4)
            rotation_part = joint.world_transform[:3, :3]
            det = np.linalg.det(rotation_part)
            assert abs(abs(det) - 1.0) < 0.1
    def test_center_of_mass_computation(self):
        initial_com = self.virtual_body.center_of_mass.copy()
        self.virtual_body.joints['left_shoulder'].angle = np.pi / 2
        self.virtual_body.update_physics(0.01, self.arena_physics)
        updated_com = self.virtual_body.center_of_mass
        assert not np.allclose(initial_com, updated_com, atol=1e-06)
        com_distance = np.linalg.norm(updated_com - self.virtual_body.position)
        assert com_distance < 2.0
class TestProprioceptiveSystem:
    def setup_method(self):
        self.virtual_body = VirtualBody('test_body', (0, 0, 0), 'humanoid')
        self.proprioceptive_system = ProprioceptiveSystem(self.virtual_body)
    def test_sensor_creation(self):
        expected_sensor_count = len(self.virtual_body.joints) * 3
        assert len(self.proprioceptive_system.sensors) == expected_sensor_count
        sensor_types = set()
        for sensor in self.proprioceptive_system.sensors.values():
            sensor_types.add(sensor.type)
        expected_types = {'joint_position', 'joint_velocity', 'joint_torque'}
        assert sensor_types == expected_types
    def test_proprioceptive_readings(self):
        readings = self.proprioceptive_system.update()
        assert len(readings) == len(self.proprioceptive_system.sensors)
        for reading in readings.values():
            assert hasattr(reading, 'timestamp')
            assert hasattr(reading, 'sensor_id')
            assert hasattr(reading, 'value')
            assert hasattr(reading, 'confidence')
            assert 0.0 <= reading.confidence <= 1.0
    def test_body_state_awareness(self):
        awareness = self.proprioceptive_system.get_body_state_awareness()
        assert 'joint_awareness' in awareness
        assert 'body_awareness_score' in awareness
        assert 'sensor_consistency' in awareness
        assert 'temporal_coherence' in awareness
        assert 0.0 <= awareness['body_awareness_score'] <= 1.0
        assert 0.0 <= awareness['sensor_consistency'] <= 1.0
        assert 0.0 <= awareness['temporal_coherence'] <= 1.0
    def test_calibration(self):
        assert not self.proprioceptive_system.calibrated
        success = self.proprioceptive_system.calibrate_sensors()
        assert success
        assert self.proprioceptive_system.calibrated
        for sensor in self.proprioceptive_system.sensors.values():
            assert sensor.calibrated
class TestEmbodiedAgent:
    def setup_method(self):
        self.agent = EmbodiedAgent('test_agent', (0, 0, 1))
        self.physics = ArenaPhysics()
        self.environment = ArenaEnvironment()
    def test_embodied_agent_initialization(self):
        assert self.agent.agent_id == 'test_agent'
        assert self.agent.active
        assert self.agent.embodiment_initialized
        assert hasattr(self.agent, 'virtual_body')
        assert isinstance(self.agent.virtual_body, VirtualBody)
        assert hasattr(self.agent, 'proprioceptive_system')
        assert isinstance(self.agent.proprioceptive_system, ProprioceptiveSystem)
    def test_motor_control(self):
        target_angle = np.pi / 4
        self.agent.set_joint_target('left_shoulder', target_angle)
        assert 'left_shoulder' in self.agent.motor_commands
        assert self.agent.motor_commands['left_shoulder'] == target_angle
        initial_angle = self.agent.get_joint_state('left_shoulder')['angle']
        for _ in range(10):
            self.agent.update(0.01, self.physics, self.environment)
        final_angle = self.agent.get_joint_state('left_shoulder')['angle']
        initial_error = abs(target_angle - initial_angle)
        final_error = abs(target_angle - final_angle)
        assert final_error <= initial_error + 0.1
    def test_embodiment_metrics(self):
        status = self.agent.get_embodiment_status()
        assert 'body_consistency_score' in status
        assert 'embodiment_quality_score' in status
        assert 'motor_performance_score' in status
        assert 0.0 <= status['body_consistency_score'] <= 1.0
        assert 0.0 <= status['embodiment_quality_score'] <= 1.0
        assert 0.0 <= status['motor_performance_score'] <= 1.0
    def test_body_consistency_validation(self):
        is_consistent, validation_results = self.agent.validate_body_consistency()
        assert 'consistent_body_representation' in validation_results
        assert 'body_schema_valid' in validation_results
        assert 'joint_kinematics_valid' in validation_results
        assert 'proprioception_valid' in validation_results
        assert 'physics_integration_valid' in validation_results
        assert 'consistency_score' in validation_results
        assert is_consistent, f"Body consistency validation failed: {validation_results['details']}"
        assert validation_results['body_schema_valid'], 'Body schema validation failed'
        assert validation_results['joint_kinematics_valid'], 'Joint kinematics validation failed'
        assert validation_results['proprioception_valid'], 'Proprioception validation failed'
        assert validation_results['physics_integration_valid'], 'Physics integration validation failed'
        assert validation_results['consistency_score'] > 0.8, f"Consistency score too low: {validation_results['consistency_score']}"
    def test_embodiment_integration(self):
        for i in range(50):
            self.agent.update(0.01, self.physics, self.environment)
            if i % 10 == 0:
                angle = np.sin(i * 0.1) * np.pi / 6
                self.agent.set_joint_target('neck', angle)
                self.agent.set_joint_target('left_shoulder', angle * 0.5)
        is_consistent, results = self.agent.validate_body_consistency()
        assert is_consistent, f'Integration test failed: {results}'
        status = self.agent.get_embodiment_status()
        assert status['embodiment_quality_score'] > 0.7, 'Embodiment quality degraded during integration'
    def test_proprioceptive_feedback(self):
        feedback, confidence = self.agent.get_proprioceptive_feedback()
        assert isinstance(feedback, np.ndarray)
        assert feedback.size > 0
        assert 0.0 <= confidence <= 1.0
        expected_size = len(self.agent.virtual_body.joints) * 2
        assert feedback.size == expected_size
    def test_calibration(self):
        success = self.agent.calibrate_embodiment()
        assert success
        is_consistent, _ = self.agent.validate_body_consistency()
        assert is_consistent
@pytest.mark.integration
class TestEmbodimentIntegration:
    def test_multi_agent_consistency(self):
        agents = []
        for i in range(3):
            agent = EmbodiedAgent(f'agent_{i}', (i * 2.0, 0, 1))
            agents.append(agent)
        physics = ArenaPhysics()
        environment = ArenaEnvironment()
        for step in range(30):
            for agent in agents:
                agent.update(0.01, physics, environment)
        for agent in agents:
            is_consistent, results = agent.validate_body_consistency()
            assert is_consistent, f'Agent {agent.agent_id} lost consistency: {results}'
    def test_physical_interaction_consistency(self):
        agent = EmbodiedAgent('physics_test_agent', (0, 0, 2))
        physics = ArenaPhysics(gravity=(0, 0, -20.0), air_resistance=0.1, max_velocity=50.0)
        environment = ArenaEnvironment()
        for _ in range(100):
            agent.update(0.01, physics, environment)
        is_consistent, results = agent.validate_body_consistency()
        assert is_consistent, f'Physical interaction broke consistency: {results}'
        status = agent.get_embodiment_status()
        assert status['embodiment_quality_score'] > 0.6
if __name__ == '__main__':
    pytest.main([__file__, '-v'])