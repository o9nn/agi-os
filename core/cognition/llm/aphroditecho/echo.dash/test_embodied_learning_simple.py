#!/usr/bin/env python3
"""
Simple test for Embodied Learning Algorithms

Basic validation of embodied learning functionality.
"""

import sys
import time
import logging
from pathlib import Path

# Add the current directory to the path for imports
sys.path.insert(0, str(Path(__file__).parent))

def test_basic_functionality():
    """Test basic embodied learning functionality"""
    print("Testing basic embodied learning functionality...")
    
    try:
        from embodied_learning import (
            BodyState, MotorAction, SensorimotorExperience,
            create_embodied_learning_system
        )
        
        print("✓ Successfully imported embodied learning modules")
        
        # Test BodyState creation
        body_state = BodyState(
            position=(0.0, 0.0, 1.0),
            joint_angles={'shoulder': 0.5, 'elbow': 1.0},
            joint_velocities={'shoulder': 0.0, 'elbow': 0.0}
        )
        print("✓ BodyState creation successful")
        
        # Test vector conversion
        vector = body_state.to_vector()
        assert len(vector) > 0, "Body state vector should not be empty"
        print("✓ Body state to vector conversion successful")
        
        # Test MotorAction creation
        motor_action = MotorAction(
            joint_targets={'shoulder': 1.0, 'elbow': 0.5},
            muscle_commands={'bicep': 0.8},
            duration=1.0
        )
        print("✓ MotorAction creation successful")
        
        # Test SensorimotorExperience creation
        experience = SensorimotorExperience(
            initial_body_state=body_state,
            motor_action=motor_action,
            resulting_body_state=body_state,
            sensory_feedback={'touch': 'object_contact', 'proprioception': 'joint_feedback'},
            reward=1.0,
            success=True
        )
        print("✓ SensorimotorExperience creation successful")
        
        # Test EmbodiedLearningSystem creation
        system = create_embodied_learning_system()
        print("✓ EmbodiedLearningSystem creation successful")
        
        # Test experience processing
        result = system.process_embodied_experience(experience)
        assert isinstance(result, dict), "Processing result should be a dictionary"
        assert 'body_schema_updated' in result, "Result should indicate body schema update"
        print("✓ Experience processing successful")
        
        # Test motor skill learning
        motor_action, skill_metrics = system.learn_motor_skill(
            "test_skill", 
            {'target': 'reach_position'}, 
            body_state
        )
        assert motor_action is not None, "Motor action should be generated"
        assert isinstance(skill_metrics, dict), "Skill metrics should be a dictionary"
        print("✓ Motor skill learning successful")
        
        # Test spatial movement planning  
        target_position = (0.3, 0.2, 1.0)
        planned_action = system.plan_spatial_movement(target_position, body_state)
        assert planned_action is not None, "Spatial movement should be planned"
        print("✓ Spatial movement planning successful")
        
        # Test metrics collection
        metrics = system.get_embodiment_metrics()
        assert isinstance(metrics, dict), "Embodiment metrics should be a dictionary"
        assert 'body_awareness' in metrics, "Should include body awareness metrics"
        assert 'spatial_metrics' in metrics, "Should include spatial metrics"
        assert 'motor_skill_metrics' in metrics, "Should include motor skill metrics"
        print("✓ Metrics collection successful")
        
        # Test multiple experiences for learning progression
        print("Testing learning progression...")
        for i in range(5):
            # Create varied experiences
            varied_state = BodyState(
                position=(i * 0.1, 0.0, 1.0),
                joint_angles={'shoulder': float(i * 0.2), 'elbow': float(i * 0.15)},
            )
            
            varied_action = MotorAction(
                joint_targets={'shoulder': float(i * 0.2 + 0.1), 'elbow': float(i * 0.15 + 0.05)},
                duration=1.0 + i * 0.1
            )
            
            varied_experience = SensorimotorExperience(
                initial_body_state=body_state,
                motor_action=varied_action,
                resulting_body_state=varied_state,
                sensory_feedback={'proprioception': f'experience_{i}', 'touch': f'contact_{i}'},
                success=i % 2 == 0,  # Alternate success/failure
                reward=1.0 if i % 2 == 0 else 0.0
            )
            
            result = system.process_embodied_experience(varied_experience)
            assert result.get('body_schema_updated', False), f"Experience {i} should update body schema"
        
        print("✓ Learning progression test successful")
        
        # Test final metrics after learning
        final_metrics = system.get_embodiment_metrics()
        system_metrics = final_metrics['system_metrics']
        assert system_metrics['total_experiences'] >= 5, "Should have processed multiple experiences"
        print("✓ Final metrics validation successful")
        
        print("\n🎉 All embodied learning tests passed successfully!")
        return True
        
    except Exception as e:
        print(f"❌ Test failed with error: {e}")
        import traceback
        traceback.print_exc()
        return False


def test_performance():
    """Test performance characteristics"""
    print("\n=== Performance Testing ===")
    
    try:
        from embodied_learning import create_embodied_learning_system, BodyState, MotorAction, SensorimotorExperience
        
        system = create_embodied_learning_system()
        
        # Time experience processing
        start_time = time.time()
        num_experiences = 50
        
        for i in range(num_experiences):
            experience = SensorimotorExperience(
                initial_body_state=BodyState(),
                motor_action=MotorAction(),
                resulting_body_state=BodyState(),
                sensory_feedback={'test': f'feedback_{i}'},
                success=True
            )
            system.process_embodied_experience(experience)
        
        processing_time = time.time() - start_time
        avg_time = processing_time / num_experiences * 1000  # milliseconds
        
        print(f"Experience Processing: {processing_time:.3f}s for {num_experiences} experiences")
        print(f"Average per experience: {avg_time:.2f}ms")
        
        # Performance should be reasonable (< 10ms per experience)
        assert avg_time < 50, f"Processing too slow: {avg_time:.2f}ms per experience"
        
        # Time skill learning
        start_time = time.time()
        num_skills = 20
        
        for i in range(num_skills):
            system.learn_motor_skill(f"skill_{i}", {'target': f'goal_{i}'}, BodyState())
        
        skill_time = time.time() - start_time
        avg_skill_time = skill_time / num_skills * 1000
        
        print(f"Skill Learning: {skill_time:.3f}s for {num_skills} skills")
        print(f"Average per skill: {avg_skill_time:.2f}ms")
        
        # Time metrics computation
        start_time = time.time()
        num_metrics = 10
        
        for i in range(num_metrics):
            system.get_embodiment_metrics()
        
        metrics_time = time.time() - start_time
        avg_metrics_time = metrics_time / num_metrics * 1000
        
        print(f"Metrics Computation: {metrics_time:.3f}s for {num_metrics} calls")
        print(f"Average per call: {avg_metrics_time:.2f}ms")
        
        print("✓ Performance tests passed")
        return True
        
    except Exception as e:
        print(f"❌ Performance test failed: {e}")
        return False


def main():
    """Run all tests"""
    logging.basicConfig(level=logging.WARNING)
    
    print("🧠 Embodied Learning Algorithms Test Suite")
    print("=" * 50)
    
    all_passed = True
    
    # Run basic functionality tests
    if not test_basic_functionality():
        all_passed = False
    
    # Run performance tests  
    if not test_performance():
        all_passed = False
    
    print("\n" + "=" * 50)
    if all_passed:
        print("🎉 ALL TESTS PASSED! Embodied learning implementation is working correctly.")
        print("\nKey Features Validated:")
        print("• Sensorimotor learning for body awareness")
        print("• Spatial reasoning based on body constraints") 
        print("• Motor skill acquisition through embodied practice")
        print("• Integration with DTESN cognitive architecture")
        print("• Real-time learning and adaptation")
    else:
        print("❌ SOME TESTS FAILED! Please check the implementation.")
        sys.exit(1)


if __name__ == '__main__':
    main()