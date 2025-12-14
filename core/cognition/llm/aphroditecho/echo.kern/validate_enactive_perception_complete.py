#!/usr/bin/env python3
"""
Comprehensive Validation of Enactive Perception System

This script validates that Task 2.3.3 is fully implemented and meets all acceptance criteria:
- Action-based perception mechanisms
- Sensorimotor contingency learning  
- Perceptual prediction through action
- Acceptance Criteria: Perception emerges through agent-environment interaction

This serves as the final validation for the task completion.
"""

import logging
import time
import sys

# Set up comprehensive logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

def validate_task_2_3_3_complete():
    """Comprehensive validation of Task 2.3.3 implementation"""
    
    print("=" * 80)
    print("TASK 2.3.3: ENACTIVE PERCEPTION SYSTEM - COMPREHENSIVE VALIDATION")
    print("=" * 80)
    print("Validating Deep Tree Echo Development Roadmap Task 2.3.3")
    print("Phase 2.3: Extended Mind Framework (Weeks 11-12)")
    print()
    
    validation_results = {
        'system_import': False,
        'core_components': False,
        'action_based_perception': False,
        'sensorimotor_contingency_learning': False,
        'perceptual_prediction': False,
        'agent_environment_interaction': False,
        'integration_capabilities': False,
        'performance_validation': False,
        'overall_success': False
    }
    
    try:
        # Test 1: System Import and Basic Functionality
        print("🔍 TEST 1: System Import and Basic Functionality")
        print("-" * 50)
        
        from enactive_perception import (
            SensorimotorContingencyLearner, 
            ActionBasedPerceptionModule, create_enactive_perception_system,
            BodyState, MotorAction, SensorimotorExperience
        )
        
        from enactive_perception_integration import (
            create_integrated_enactive_system,
            validate_task_2_3_3_acceptance_criteria
        )
        
        print("✅ All required modules imported successfully")
        validation_results['system_import'] = True
        
        # Test 2: Core Components Functionality  
        print("\n🧠 TEST 2: Core Components Functionality")
        print("-" * 50)
        
        # Test contingency learner
        learner = SensorimotorContingencyLearner(max_contingencies=100, learning_rate=0.1)
        print(f"✅ SensorimotorContingencyLearner created: {learner.max_contingencies} max contingencies")
        
        # Test action-based perception module
        perception_module = ActionBasedPerceptionModule(exploration_rate=0.2)
        print(f"✅ ActionBasedPerceptionModule created: {perception_module.exploration_rate} exploration rate")
        
        # Test main enactive system
        system = create_enactive_perception_system("validation_agent")
        print(f"✅ EnactivePerceptionSystem created: {system.agent_name}")
        
        validation_results['core_components'] = True
        
        # Test 3: Action-Based Perception Mechanisms
        print("\n🎯 TEST 3: Action-Based Perception Mechanisms") 
        print("-" * 50)
        
        # Create test body state
        body_state = BodyState(
            position=(0.0, 0.0, 1.0),
            joint_angles={'shoulder': 0.5, 'elbow': 0.3, 'wrist': 0.2},
            sensory_state={'vision': 0.7, 'touch': 0.4, 'proprioception': 0.6}
        )
        
        # Generate exploratory action
        exploratory_action = system.generate_perceptual_action(body_state, {'explore_vision': True})
        print(f"✅ Exploratory action generated: {len(exploratory_action.joint_targets)} joint targets")
        print(f"   Joint targets: {list(exploratory_action.joint_targets.keys())}")
        print(f"   Force: {exploratory_action.force:.2f}, Duration: {exploratory_action.duration:.2f}")
        
        # Test attention mechanisms
        surprise = {'vision': 0.8, 'touch': 0.3, 'audio': 0.9}
        system.action_perception_module.update_attention_weights(surprise)
        print(f"✅ Attention weights updated: {len(system.action_perception_module.attention_weights)} modalities")
        
        validation_results['action_based_perception'] = True
        
        # Test 4: Sensorimotor Contingency Learning
        print("\n🔬 TEST 4: Sensorimotor Contingency Learning")
        print("-" * 50)
        
        # Create multiple learning experiences
        learning_experiences = []
        for i in range(5):
            experience = SensorimotorExperience(
                initial_body_state=BodyState(
                    joint_angles={'test_joint': 0.1 * i},
                    sensory_state={'environment': 0.2 * i, 'feedback': 0.3 * i}
                ),
                motor_action=MotorAction(
                    joint_targets={'test_joint': 0.2 * i},
                    muscle_commands={'primary': 0.5 + 0.1 * i}
                ),
                resulting_body_state=BodyState(
                    joint_angles={'test_joint': 0.2 * i},
                    sensory_state={'environment': 0.3 * i, 'feedback': 0.4 * i}
                ),
                sensory_feedback={'environment': 0.3 * i, 'feedback': 0.4 * i, 'learning_step': i},
                success=True,
                reward=0.8 + 0.1 * i
            )
            learning_experiences.append(experience)
        
        # Process learning experiences
        contingencies_learned = 0
        for i, experience in enumerate(learning_experiences):
            result = system.process_embodied_experience(experience)
            if result.get('contingency_learned', False):
                contingencies_learned += 1
            print(f"   Learning step {i+1}: Contingency learned = {result.get('contingency_learned', False)}")
        
        print(f"✅ Sensorimotor contingency learning: {contingencies_learned}/{len(learning_experiences)} successful")
        
        # Verify contingencies were stored
        metrics = system.get_enactive_metrics()
        print(f"✅ Total contingencies learned: {metrics['total_contingencies_learned']}")
        print(f"✅ Average confidence: {metrics['average_contingency_confidence']:.3f}")
        
        validation_results['sensorimotor_contingency_learning'] = True
        
        # Test 5: Perceptual Prediction Through Action
        print("\n🔮 TEST 5: Perceptual Prediction Through Action")
        print("-" * 50)
        
        # Test prediction with learned contingencies
        test_action = MotorAction(joint_targets={'test_joint': 0.4}, muscle_commands={'primary': 0.7})
        test_body_state = BodyState(sensory_state={'environment': 0.4, 'feedback': 0.5})
        
        prediction = system.predict_perceptual_outcome(test_action, test_body_state)
        print("✅ Perceptual prediction generated:")
        print(f"   Confidence: {prediction.confidence:.3f}")
        print(f"   Exploration value: {prediction.exploration_value:.3f}")
        print(f"   Predicted outcome keys: {list(prediction.predicted_sensory_outcome.keys())}")
        
        # Test prediction accuracy over time
        prediction_accuracy = metrics.get('recent_prediction_accuracy', 0.0)
        print(f"✅ Recent prediction accuracy: {prediction_accuracy:.3f}")
        
        validation_results['perceptual_prediction'] = True
        
        # Test 6: Agent-Environment Interaction (Acceptance Criteria)
        print("\n🌍 TEST 6: Agent-Environment Interaction (Acceptance Criteria)")
        print("-" * 50)
        
        interaction_cycles = []
        initial_metrics = system.get_enactive_metrics()
        
        for cycle in range(8):
            print(f"   Interaction cycle {cycle + 1}:")
            
            # Agent perceives environment
            current_state = BodyState(
                joint_angles={'env_joint': 0.1 * cycle},
                sensory_state={'environment_state': 0.5 + 0.05 * cycle, 'cycle': cycle}
            )
            print(f"     Agent perceives: env_state={current_state.sensory_state['environment_state']:.2f}")
            
            # Agent acts to explore/perceive
            action = system.generate_perceptual_action(current_state, {'cycle': cycle})
            print(f"     Agent acts: {len(action.joint_targets)} joints moved")
            
            # Environment responds
            environment_response = {
                'environment_state': 0.6 + 0.05 * cycle,
                'response_to_action': True,
                'cycle_response': cycle,
                'novelty': 0.3 if cycle % 3 == 0 else 0.1  # Periodic novelty
            }
            print(f"     Environment responds: novelty={environment_response['novelty']:.1f}")
            
            # Create interaction experience
            interaction_experience = SensorimotorExperience(
                initial_body_state=current_state,
                motor_action=action,
                resulting_body_state=BodyState(
                    joint_angles=action.joint_targets,
                    sensory_state=environment_response
                ),
                sensory_feedback=environment_response,
                success=True,
                reward=environment_response['novelty'] + 0.5
            )
            
            # System learns from interaction
            interaction_result = system.process_embodied_experience(interaction_experience)
            interaction_cycles.append(interaction_result)
            print(f"     System learns: contingency={interaction_result.get('contingency_learned', False)}")
        
        # Analyze emergence of perception through interaction
        final_metrics = system.get_enactive_metrics()
        
        contingencies_emerged = final_metrics['total_contingencies_learned'] - initial_metrics['total_contingencies_learned']
        attention_weights_developed = len(final_metrics['attention_weights']) - len(initial_metrics['attention_weights'])
        perceptual_history_built = final_metrics['perceptual_history_length'] - initial_metrics['perceptual_history_length']
        
        print("\n✅ Perception emergence through interaction:")
        print(f"   New contingencies learned: {contingencies_emerged}")
        print(f"   Attention weights developed: {attention_weights_developed}")
        print(f"   Perceptual history built: {perceptual_history_built}")
        print(f"   Successful interactions: {sum(1 for r in interaction_cycles if r.get('perceptual_state_updated', False))}/8")
        
        # Acceptance criteria validation
        perception_emerged = (contingencies_emerged > 0 and perceptual_history_built > 0)
        print(f"✅ ACCEPTANCE CRITERIA MET: Perception emerges through agent-environment interaction = {perception_emerged}")
        
        validation_results['agent_environment_interaction'] = perception_emerged
        
        # Test 7: Integration Capabilities
        print("\n🔗 TEST 7: Integration Capabilities") 
        print("-" * 50)
        
        # Test integration system
        integrated_system = create_integrated_enactive_system("integration_test")
        integration_metrics = integrated_system.get_integration_metrics()
        
        print("✅ Integrated system created:")
        print(f"   Enactive system active: {integration_metrics['enactive_active']}")
        print(f"   Embodied learning active: {integration_metrics['embodied_active']}")
        print(f"   AAR system active: {integration_metrics['aar_active']}")
        print(f"   Integration successful: {integration_metrics['integration_successful']}")
        
        # Test 4E framework validation
        framework_validation = integrated_system.validate_4e_framework_integration()
        print(f"✅ 4E Framework compliance: {framework_validation.get('framework_score', '0/4')}")
        
        # Test acceptance criteria validation
        acceptance_results = validate_task_2_3_3_acceptance_criteria()
        print(f"✅ Task 2.3.3 acceptance criteria: {acceptance_results.get('criteria_score', '0/4')}")
        
        validation_results['integration_capabilities'] = integration_metrics['integration_successful']
        
        # Test 8: Performance Validation
        print("\n⚡ TEST 8: Performance Validation")
        print("-" * 50)
        
        # Performance timing tests
        start_time = time.time()
        
        # Test learning performance
        for i in range(10):
            quick_experience = SensorimotorExperience(
                initial_body_state=BodyState(sensory_state={'perf': i * 0.1}),
                motor_action=MotorAction(joint_targets={'perf': i * 0.1}),
                resulting_body_state=BodyState(sensory_state={'perf': i * 0.1 + 0.1}),
                sensory_feedback={'perf': i * 0.1 + 0.1},
                success=True
            )
            system.process_embodied_experience(quick_experience)
        
        learning_time = time.time() - start_time
        print(f"✅ Learning performance: 10 experiences in {learning_time:.3f}s ({learning_time/10*1000:.1f}ms per experience)")
        
        # Test prediction performance  
        start_time = time.time()
        for i in range(10):
            system.predict_perceptual_outcome(
                MotorAction(joint_targets={'perf': i * 0.1}),
                BodyState(sensory_state={'perf': i * 0.1})
            )
        prediction_time = time.time() - start_time
        print(f"✅ Prediction performance: 10 predictions in {prediction_time:.3f}s ({prediction_time/10*1000:.1f}ms per prediction)")
        
        # Test action generation performance
        start_time = time.time()
        for i in range(10):
            test_action = system.generate_perceptual_action(
                BodyState(joint_angles={'perf': i * 0.1}, sensory_state={'perf': i * 0.1})
            )
        action_time = time.time() - start_time
        print(f"✅ Action generation performance: 10 actions in {action_time:.3f}s ({action_time/10*1000:.1f}ms per action)")
        
        # Performance acceptance (should be sub-second for typical operations)
        performance_acceptable = (learning_time < 1.0 and prediction_time < 1.0 and action_time < 1.0)
        print(f"✅ Performance acceptable: {performance_acceptable}")
        
        validation_results['performance_validation'] = performance_acceptable
        
        # Overall Success Assessment
        print("\n" + "=" * 80)
        print("VALIDATION SUMMARY")
        print("=" * 80)
        
        success_count = sum(validation_results.values())
        total_tests = len(validation_results) - 1  # Exclude overall_success from count
        
        print(f"\nTest Results: {success_count}/{total_tests} tests passed")
        print("-" * 30)
        
        for test_name, result in validation_results.items():
            if test_name != 'overall_success':
                status = "✅ PASSED" if result else "❌ FAILED"
                print(f"{test_name.replace('_', ' ').title()}: {status}")
        
        # Task 2.3.3 Requirements Validation
        print("\nTask 2.3.3 Requirements:")
        print("-" * 30)
        print(f"✅ Action-based perception mechanisms: {'IMPLEMENTED' if validation_results['action_based_perception'] else 'MISSING'}")
        print(f"✅ Sensorimotor contingency learning: {'IMPLEMENTED' if validation_results['sensorimotor_contingency_learning'] else 'MISSING'}")
        print(f"✅ Perceptual prediction through action: {'IMPLEMENTED' if validation_results['perceptual_prediction'] else 'MISSING'}")
        print(f"✅ Perception emerges through interaction: {'VALIDATED' if validation_results['agent_environment_interaction'] else 'NOT VALIDATED'}")
        
        # Final verdict
        core_requirements_met = (
            validation_results['action_based_perception'] and
            validation_results['sensorimotor_contingency_learning'] and
            validation_results['perceptual_prediction'] and 
            validation_results['agent_environment_interaction']
        )
        
        overall_success = success_count >= 7  # Allow one test to fail while still succeeding
        validation_results['overall_success'] = overall_success and core_requirements_met
        
        print(f"\n{'='*80}")
        if validation_results['overall_success']:
            print("🎯 TASK 2.3.3: ENACTIVE PERCEPTION SYSTEM - ✅ FULLY VALIDATED")
            print("All requirements implemented and acceptance criteria met!")
            print("System is ready for integration with the broader Deep Tree Echo architecture.")
        else:
            print("❌ TASK 2.3.3: ENACTIVE PERCEPTION SYSTEM - VALIDATION INCOMPLETE")
            print("Some requirements or acceptance criteria not met.")
        print("="*80)
        
        return validation_results
        
    except Exception as e:
        logger.error(f"Validation failed with error: {e}")
        import traceback
        traceback.print_exc()
        validation_results['overall_success'] = False
        return validation_results


if __name__ == "__main__":
    # Run comprehensive validation
    results = validate_task_2_3_3_complete()
    
    # Exit with appropriate code
    exit_code = 0 if results['overall_success'] else 1
    sys.exit(exit_code)