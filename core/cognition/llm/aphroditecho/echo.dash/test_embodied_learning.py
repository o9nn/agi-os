#!/usr/bin/env python3
"""
Test script for Embodied Learning Algorithms

Comprehensive tests for sensorimotor learning, body awareness, spatial reasoning,
and motor skill acquisition through embodied practice.
"""

import unittest
import numpy as np
import time
import logging
import sys
from pathlib import Path

# Add the current directory to the path for imports
sys.path.insert(0, str(Path(__file__).parent))

# Import the module under test
try:
    from embodied_learning import (
        BodyState, MotorAction, SensorimotorExperience,
        BodySchemaLearner, SpatialReasoningEngine, MotorSkillLearner,
        EmbodiedLearningSystem, create_embodied_learning_system
    )
    EMBODIED_LEARNING_AVAILABLE = True
except ImportError as e:
    EMBODIED_LEARNING_AVAILABLE = False
    print(f"Warning: Could not import embodied_learning: {e}")


class TestBodyState(unittest.TestCase):
    """Test cases for BodyState data structure"""
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_body_state_creation(self):
        """Test BodyState creation and basic attributes"""
        body_state = BodyState(
            position=(1.0, 2.0, 3.0),
            orientation=(0.1, 0.2, 0.3),
            joint_angles={'shoulder': 0.5, 'elbow': 1.0},
            joint_velocities={'shoulder': 0.1, 'elbow': 0.2}
        )
        
        self.assertEqual(body_state.position, (1.0, 2.0, 3.0))
        self.assertEqual(body_state.orientation, (0.1, 0.2, 0.3))
        self.assertEqual(body_state.joint_angles['shoulder'], 0.5)
        self.assertEqual(body_state.joint_velocities['elbow'], 0.2)
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_body_state_to_vector(self):
        """Test conversion of body state to numerical vector"""
        body_state = BodyState(
            position=(1.0, 2.0, 3.0),
            orientation=(0.1, 0.2, 0.3),
            joint_angles={'shoulder': 0.5},
            joint_velocities={'shoulder': 0.1},
            muscle_activations={'bicep': 0.8}
        )
        
        vector = body_state.to_vector()
        
        self.assertIsInstance(vector, np.ndarray)
        self.assertEqual(len(vector), 9)  # 3 pos + 3 orient + 1 joint + 1 vel + 1 muscle
        self.assertEqual(vector[0], 1.0)  # x position
        self.assertEqual(vector[6], 0.5)  # joint angle
        self.assertEqual(vector[8], 0.8)  # muscle activation
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_body_state_update_sensory(self):
        """Test updating body state from sensory input"""
        body_state = BodyState()
        initial_timestamp = body_state.timestamp
        
        time.sleep(0.01)  # Small delay to ensure timestamp changes
        
        sensory_data = {
            'temperature': 25.0,
            'pressure': 1.0,
            'touch': True
        }
        
        body_state.update_from_sensory_input(sensory_data)
        
        self.assertGreater(body_state.timestamp, initial_timestamp)
        self.assertEqual(body_state.sensory_state['temperature'], 25.0)
        self.assertEqual(body_state.sensory_state['touch'], True)


class TestMotorAction(unittest.TestCase):
    """Test cases for MotorAction data structure"""
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_motor_action_creation(self):
        """Test MotorAction creation and attributes"""
        motor_action = MotorAction(
            joint_targets={'shoulder': 1.0, 'elbow': 0.5},
            muscle_commands={'bicep': 0.8, 'tricep': 0.3},
            duration=2.0,
            force=0.9,
            precision=0.7
        )
        
        self.assertEqual(motor_action.joint_targets['shoulder'], 1.0)
        self.assertEqual(motor_action.muscle_commands['bicep'], 0.8)
        self.assertEqual(motor_action.duration, 2.0)
        self.assertEqual(motor_action.force, 0.9)
        self.assertEqual(motor_action.precision, 0.7)
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_motor_action_to_vector(self):
        """Test conversion of motor action to numerical vector"""
        motor_action = MotorAction(
            joint_targets={'shoulder': 1.0, 'elbow': 0.5},
            muscle_commands={'bicep': 0.8},
            duration=2.0,
            force=0.9,
            precision=0.7
        )
        
        vector = motor_action.to_vector()
        
        self.assertIsInstance(vector, np.ndarray)
        self.assertEqual(len(vector), 6)  # 2 joints + 1 muscle + 3 params
        self.assertIn(1.0, vector)  # shoulder target
        self.assertIn(0.8, vector)  # bicep command
        self.assertIn(2.0, vector)  # duration


class TestBodySchemaLearner(unittest.TestCase):
    """Test cases for BodySchemaLearner"""
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_body_schema_learner_creation(self):
        """Test BodySchemaLearner initialization"""
        body_dimensions = {
            'arm_length': 0.7,
            'leg_length': 0.9,
            'torso_height': 0.6
        }
        
        learner = BodySchemaLearner(body_dimensions)
        
        self.assertEqual(learner.body_dimensions, body_dimensions)
        self.assertIsInstance(learner.body_schema_weights, np.ndarray)
        self.assertEqual(learner.body_schema_weights.shape, (100, 100))
        self.assertEqual(learner.learning_rate, 0.01)
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_body_schema_update(self):
        """Test body schema update from experience"""
        learner = BodySchemaLearner({'arm_length': 0.7})
        
        # Create test experience
        initial_state = BodyState(
            position=(0.0, 0.0, 1.0),
            joint_angles={'shoulder': 0.0}
        )
        
        motor_action = MotorAction(
            joint_targets={'shoulder': 0.5}
        )
        
        resulting_state = BodyState(
            position=(0.1, 0.0, 1.0),
            joint_angles={'shoulder': 0.5}
        )
        
        experience = SensorimotorExperience(
            initial_body_state=initial_state,
            motor_action=motor_action,
            resulting_body_state=resulting_state,
            sensory_feedback={'touch': 'contact'},
            success=True
        )
        
        initial_experiences = len(learner.experiences)
        learner.update_body_schema(experience)
        
        self.assertEqual(len(learner.experiences), initial_experiences + 1)
        self.assertEqual(learner.experiences[-1], experience)
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_body_awareness_metrics(self):
        """Test body awareness metrics computation"""
        learner = BodySchemaLearner({'arm_length': 0.7})
        
        # Add some test experiences
        for i in range(5):
            experience = SensorimotorExperience(
                initial_body_state=BodyState(joint_angles={'joint': float(i * 0.1)}),
                motor_action=MotorAction(joint_targets={'joint': float(i * 0.1 + 0.05)}),
                resulting_body_state=BodyState(joint_angles={'joint': float(i * 0.1 + 0.05)}),
                success=True,
                reward=1.0
            )
            learner.update_body_schema(experience)
        
        awareness = learner.get_body_awareness()
        
        self.assertIsInstance(awareness, dict)
        self.assertIn('schema_confidence', awareness)
        self.assertIn('proprioceptive_accuracy', awareness)
        self.assertIn('joint_flexibility', awareness)
        self.assertIn('muscle_coordination', awareness)
        
        # All metrics should be between 0 and 1
        for metric_name, value in awareness.items():
            self.assertGreaterEqual(value, 0.0, f"{metric_name} should be >= 0")
            self.assertLessEqual(value, 1.0, f"{metric_name} should be <= 1")


class TestSpatialReasoningEngine(unittest.TestCase):
    """Test cases for SpatialReasoningEngine"""
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_spatial_reasoning_creation(self):
        """Test SpatialReasoningEngine initialization"""
        body_schema = BodySchemaLearner({'arm_length': 0.7})
        spatial_engine = SpatialReasoningEngine(body_schema)
        
        self.assertEqual(spatial_engine.body_schema, body_schema)
        self.assertIsInstance(spatial_engine.spatial_memory, dict)
        self.assertIsInstance(spatial_engine.reachability_map, dict)
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_spatial_understanding_update(self):
        """Test spatial understanding update from experience"""
        body_schema = BodySchemaLearner({'arm_length': 0.7})
        spatial_engine = SpatialReasoningEngine(body_schema)
        
        experience = SensorimotorExperience(
            initial_body_state=BodyState(position=(0.0, 0.0, 1.0)),
            motor_action=MotorAction(joint_targets={'shoulder': 0.5}),
            resulting_body_state=BodyState(position=(0.2, 0.1, 1.0)),
            success=True
        )
        
        initial_memory_size = len(spatial_engine.spatial_memory)
        spatial_engine.update_spatial_understanding(experience)
        
        self.assertGreater(len(spatial_engine.spatial_memory), initial_memory_size)
        self.assertGreater(len(spatial_engine.reachability_map), 0)
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_spatial_action_planning(self):
        """Test spatial action planning"""
        body_schema = BodySchemaLearner({'arm_length': 0.7})
        spatial_engine = SpatialReasoningEngine(body_schema)
        
        current_state = BodyState(position=(0.0, 0.0, 1.0))
        target_position = (0.3, 0.2, 1.0)
        
        motor_action = spatial_engine.plan_spatial_action(target_position, current_state)
        
        # Should return a motor action for reachable targets
        self.assertIsInstance(motor_action, MotorAction)
        self.assertIsInstance(motor_action.joint_targets, dict)
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_spatial_metrics(self):
        """Test spatial metrics computation"""
        body_schema = BodySchemaLearner({'arm_length': 0.7})
        spatial_engine = SpatialReasoningEngine(body_schema)
        
        # Add some spatial experiences
        for i in range(3):
            experience = SensorimotorExperience(
                initial_body_state=BodyState(position=(0.0, 0.0, 1.0)),
                motor_action=MotorAction(),
                resulting_body_state=BodyState(position=(i * 0.1, 0.0, 1.0)),
                success=i % 2 == 0  # Alternate success/failure
            )
            spatial_engine.update_spatial_understanding(experience)
        
        metrics = spatial_engine.get_spatial_metrics()
        
        self.assertIsInstance(metrics, dict)
        self.assertIn('spatial_coverage', metrics)
        self.assertIn('reachability_accuracy', metrics)
        self.assertIn('spatial_memory_size', metrics)


class TestMotorSkillLearner(unittest.TestCase):
    """Test cases for MotorSkillLearner"""
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_motor_skill_learner_creation(self):
        """Test MotorSkillLearner initialization"""
        body_schema = BodySchemaLearner({'arm_length': 0.7})
        skill_learner = MotorSkillLearner(body_schema)
        
        self.assertEqual(skill_learner.body_schema, body_schema)
        self.assertIsInstance(skill_learner.skill_library, dict)
        self.assertIsInstance(skill_learner.practice_history, dict)
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_skill_practice(self):
        """Test motor skill practice"""
        body_schema = BodySchemaLearner({'arm_length': 0.7})
        skill_learner = MotorSkillLearner(body_schema)
        
        skill_name = "reach_target"
        target_outcome = {'position': (0.5, 0.3, 1.0)}
        current_state = BodyState(position=(0.0, 0.0, 1.0))
        
        motor_action = skill_learner.practice_skill(skill_name, target_outcome, current_state)
        
        self.assertIsInstance(motor_action, MotorAction)
        self.assertIn(skill_name, skill_learner.skill_library)
        self.assertIn(skill_name, skill_learner.practice_history)
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_learning_from_outcome(self):
        """Test learning from skill practice outcome"""
        body_schema = BodySchemaLearner({'arm_length': 0.7})
        skill_learner = MotorSkillLearner(body_schema)
        
        skill_name = "reach_target"
        
        # Practice the skill first
        skill_learner.practice_skill(
            skill_name, 
            {'position': (0.5, 0.3, 1.0)}, 
            BodyState()
        )
        
        # Create outcome
        outcome = SensorimotorExperience(
            initial_body_state=BodyState(),
            motor_action=MotorAction(joint_targets={'shoulder': 0.5}),
            resulting_body_state=BodyState(),
            success=True,
            reward=1.0
        )
        
        initial_updates = skill_learner.skill_library[skill_name]['updates']
        skill_learner.learn_from_outcome(skill_name, outcome)
        
        self.assertGreater(skill_learner.skill_library[skill_name]['updates'], initial_updates)
        self.assertIn(skill_name, skill_learner.skill_performance)
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_skill_metrics(self):
        """Test skill learning metrics"""
        body_schema = BodySchemaLearner({'arm_length': 0.7})
        skill_learner = MotorSkillLearner(body_schema)
        
        # Practice and learn from multiple skills
        for i, skill_name in enumerate(['reach', 'grasp', 'lift']):
            skill_learner.practice_skill(skill_name, {}, BodyState())
            
            outcome = SensorimotorExperience(
                initial_body_state=BodyState(),
                motor_action=MotorAction(),
                resulting_body_state=BodyState(),
                success=i % 2 == 0,  # Alternate success
                reward=float(i % 2)
            )
            skill_learner.learn_from_outcome(skill_name, outcome)
        
        metrics = skill_learner.get_skill_metrics()
        
        self.assertIsInstance(metrics, dict)
        self.assertIn('total_skills', metrics)
        self.assertIn('avg_success_rate', metrics)
        self.assertIn('total_practice_attempts', metrics)
        self.assertEqual(metrics['total_skills'], 3)


class TestEmbodiedLearningSystem(unittest.TestCase):
    """Test cases for complete EmbodiedLearningSystem"""
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_embodied_learning_system_creation(self):
        """Test EmbodiedLearningSystem initialization"""
        system = EmbodiedLearningSystem()
        
        self.assertIsInstance(system.body_schema_learner, BodySchemaLearner)
        self.assertIsInstance(system.spatial_reasoning, SpatialReasoningEngine)
        self.assertIsInstance(system.motor_skill_learner, MotorSkillLearner)
        self.assertTrue(system.learning_active)
        self.assertEqual(system.experience_count, 0)
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_embodied_learning_system_custom_dimensions(self):
        """Test system creation with custom body dimensions"""
        custom_dimensions = {
            'arm_length': 0.8,
            'leg_length': 1.0,
            'torso_height': 0.7
        }
        
        system = EmbodiedLearningSystem(custom_dimensions)
        
        self.assertEqual(system.body_schema_learner.body_dimensions, custom_dimensions)
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_process_embodied_experience(self):
        """Test processing of embodied experiences"""
        system = EmbodiedLearningSystem()
        
        experience = SensorimotorExperience(
            initial_body_state=BodyState(position=(0.0, 0.0, 1.0)),
            motor_action=MotorAction(joint_targets={'shoulder': 0.5}),
            resulting_body_state=BodyState(position=(0.1, 0.0, 1.0)),
            success=True,
            reward=1.0
        )
        
        initial_count = system.experience_count
        result = system.process_embodied_experience(experience)
        
        self.assertIsInstance(result, dict)
        self.assertEqual(system.experience_count, initial_count + 1)
        self.assertIn('body_schema_updated', result)
        self.assertIn('spatial_understanding_updated', result)
        self.assertTrue(result['body_schema_updated'])
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_learn_motor_skill(self):
        """Test motor skill learning through the system"""
        system = EmbodiedLearningSystem()
        
        skill_name = "precise_movement"
        target_outcome = {'accuracy': 0.9}
        current_state = BodyState(position=(0.0, 0.0, 1.0))
        
        motor_action, skill_metrics = system.learn_motor_skill(
            skill_name, target_outcome, current_state
        )
        
        self.assertIsInstance(motor_action, MotorAction)
        self.assertIsInstance(skill_metrics, dict)
        self.assertIn('total_skills', skill_metrics)
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_spatial_movement_planning(self):
        """Test spatial movement planning"""
        system = EmbodiedLearningSystem()
        
        target_position = (0.3, 0.2, 1.0)
        current_state = BodyState(position=(0.0, 0.0, 1.0))
        
        motor_action = system.plan_spatial_movement(target_position, current_state)
        
        self.assertIsInstance(motor_action, MotorAction)
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_embodiment_metrics(self):
        """Test comprehensive embodiment metrics"""
        system = EmbodiedLearningSystem()
        
        # Add some experiences
        for i in range(3):
            experience = SensorimotorExperience(
                initial_body_state=BodyState(),
                motor_action=MotorAction(),
                resulting_body_state=BodyState(),
                success=True
            )
            system.process_embodied_experience(experience)
        
        metrics = system.get_embodiment_metrics()
        
        self.assertIsInstance(metrics, dict)
        self.assertIn('body_awareness', metrics)
        self.assertIn('spatial_metrics', metrics)
        self.assertIn('motor_skill_metrics', metrics)
        self.assertIn('system_metrics', metrics)
        
        # Check system metrics
        system_metrics = metrics['system_metrics']
        self.assertEqual(system_metrics['total_experiences'], 3)
        self.assertTrue(system_metrics['learning_active'])
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_learning_activation_control(self):
        """Test learning activation/deactivation"""
        system = EmbodiedLearningSystem()
        
        self.assertTrue(system.learning_active)
        
        system.set_learning_active(False)
        self.assertFalse(system.learning_active)
        
        system.set_learning_active(True)
        self.assertTrue(system.learning_active)
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_factory_function(self):
        """Test factory function for system creation"""
        system = create_embodied_learning_system()
        
        self.assertIsInstance(system, EmbodiedLearningSystem)
        self.assertTrue(system.learning_active)
        
        # Test with custom dimensions
        custom_dims = {'arm_length': 0.9}
        system2 = create_embodied_learning_system(custom_dims)
        
        self.assertEqual(system2.body_schema_learner.body_dimensions['arm_length'], 0.9)


class TestIntegrationAndPerformance(unittest.TestCase):
    """Integration tests and performance validation"""
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_complete_learning_cycle(self):
        """Test complete learning cycle from practice to improvement"""
        system = EmbodiedLearningSystem()
        
        # Practice a skill multiple times
        skill_name = "target_reaching"
        target_outcome = {'position': (0.5, 0.3, 1.0)}
        current_state = BodyState(position=(0.0, 0.0, 1.0))
        
        success_rates = []
        
        for trial in range(10):
            # Practice skill
            motor_action, _ = system.learn_motor_skill(skill_name, target_outcome, current_state)
            
            # Simulate outcome (gradually improving performance)
            success = trial > 5 or (trial > 2 and trial % 2 == 0)
            reward = 1.0 if success else 0.0
            
            outcome = SensorimotorExperience(
                initial_body_state=current_state,
                motor_action=motor_action,
                resulting_body_state=BodyState(position=(0.4, 0.25, 1.0)),
                success=success,
                reward=reward
            )
            
            # Update from outcome
            system.update_from_skill_outcome(skill_name, outcome)
            
            # Track success rate
            metrics = system.motor_skill_learner.get_skill_metrics()
            if 'avg_success_rate' in metrics:
                success_rates.append(metrics['avg_success_rate'])
        
        # Should show improvement over time
        if len(success_rates) >= 2:
            self.assertGreaterEqual(success_rates[-1], success_rates[0], 
                                  "Success rate should improve with practice")
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_spatial_learning_progression(self):
        """Test spatial learning progression over multiple experiences"""
        system = EmbodiedLearningSystem()
        
        # Create experiences in different spatial locations
        positions = [(0.1, 0.0, 1.0), (0.2, 0.1, 1.0), (0.0, 0.2, 1.0)]
        
        for i, pos in enumerate(positions):
            experience = SensorimotorExperience(
                initial_body_state=BodyState(position=(0.0, 0.0, 1.0)),
                motor_action=MotorAction(),
                resulting_body_state=BodyState(position=pos),
                success=True
            )
            system.process_embodied_experience(experience)
        
        # Check spatial metrics improve
        metrics = system.get_embodiment_metrics()
        spatial_metrics = metrics['spatial_metrics']
        
        self.assertGreater(spatial_metrics['spatial_memory_size'], 0)
        self.assertGreater(spatial_metrics['spatial_coverage'], 0.0)
    
    @unittest.skipIf(not EMBODIED_LEARNING_AVAILABLE, "embodied_learning not available")
    def test_body_awareness_development(self):
        """Test body awareness development over time"""
        system = EmbodiedLearningSystem()
        
        initial_metrics = system.get_embodiment_metrics()
        initial_confidence = initial_metrics['body_awareness']['schema_confidence']
        
        # Add multiple successful experiences
        for i in range(15):
            experience = SensorimotorExperience(
                initial_body_state=BodyState(joint_angles={'joint': float(i * 0.1)}),
                motor_action=MotorAction(joint_targets={'joint': float(i * 0.1 + 0.05)}),
                resulting_body_state=BodyState(joint_angles={'joint': float(i * 0.1 + 0.05)}),
                success=True,
                reward=1.0
            )
            system.process_embodied_experience(experience)
        
        final_metrics = system.get_embodiment_metrics()
        final_confidence = final_metrics['body_awareness']['schema_confidence']
        
        # Confidence should improve with more experiences
        self.assertGreaterEqual(final_confidence, initial_confidence,
                              "Body schema confidence should improve with experience")


def run_performance_benchmarks():
    """Run performance benchmarks for embodied learning system"""
    if not EMBODIED_LEARNING_AVAILABLE:
        print("Embodied learning not available for benchmarks")
        return
    
    print("\n=== Embodied Learning Performance Benchmarks ===")
    
    system = EmbodiedLearningSystem()
    
    # Benchmark experience processing
    start_time = time.time()
    for i in range(100):
        experience = SensorimotorExperience(
            initial_body_state=BodyState(),
            motor_action=MotorAction(),
            resulting_body_state=BodyState(),
            success=True
        )
        system.process_embodied_experience(experience)
    
    processing_time = time.time() - start_time
    print(f"Experience Processing: {processing_time:.3f}s for 100 experiences")
    print(f"Average per experience: {processing_time/100*1000:.1f}ms")
    
    # Benchmark skill learning
    start_time = time.time()
    for i in range(50):
        system.learn_motor_skill(f"skill_{i%5}", {}, BodyState())
    
    skill_time = time.time() - start_time
    print(f"Skill Learning: {skill_time:.3f}s for 50 skill practices")
    print(f"Average per skill: {skill_time/50*1000:.1f}ms")
    
    # Benchmark metrics computation
    start_time = time.time()
    for i in range(20):
        system.get_embodiment_metrics()
    
    metrics_time = time.time() - start_time
    print(f"Metrics Computation: {metrics_time:.3f}s for 20 calls")
    print(f"Average per call: {metrics_time/20*1000:.1f}ms")


def main():
    """Run the test suite"""
    # Set up logging
    logging.basicConfig(
        level=logging.WARNING,  # Reduce log noise during tests
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    
    print("Testing Embodied Learning Algorithms...")
    print(f"Embodied Learning Available: {EMBODIED_LEARNING_AVAILABLE}")
    
    if EMBODIED_LEARNING_AVAILABLE:
        # Run main test suite
        unittest.main(verbosity=2, exit=False)
        
        # Run performance benchmarks
        run_performance_benchmarks()
    else:
        print("Skipping tests - embodied_learning module not available")


if __name__ == '__main__':
    main()
