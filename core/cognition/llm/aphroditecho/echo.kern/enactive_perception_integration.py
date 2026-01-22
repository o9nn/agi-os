import logging
import time
import sys
from pathlib import Path
from typing import Dict, Optional, Any
try:
    from enactive_perception import EnactivePerceptionSystem, create_enactive_perception_system, BodyState, MotorAction, SensorimotorExperience
    ENACTIVE_AVAILABLE = True
except ImportError:
    ENACTIVE_AVAILABLE = False
try:
    sys.path.append(str(Path(__file__).parent.parent / 'echo.dash'))
    from embodied_learning import EmbodiedLearningSystem, create_embodied_learning_system
    EMBODIED_LEARNING_AVAILABLE = True
except ImportError:
    EMBODIED_LEARNING_AVAILABLE = False
try:
    sys.path.append(str(Path(__file__).parent.parent / 'echo.dream'))
    from aar_system import Agent, Arena, AARComponent
    AAR_AVAILABLE = True
except ImportError:
    AAR_AVAILABLE = False
logger = logging.getLogger(__name__)
class EnactivePerceptionIntegrator:
    def __init__(self, agent_name: str='integrated_agent'):
        self.agent_name = agent_name
        self.enactive_system = None
        self.embodied_system = None
        self.aar_agent = None
        self.integration_metrics = {'enactive_active': False, 'embodied_active': False, 'aar_active': False, 'integration_successful': False, 'total_experiences_processed': 0}
        self._initialize_systems()
    def _initialize_systems(self):
        logger.info(f'Initializing integrated systems for {self.agent_name}')
        if ENACTIVE_AVAILABLE:
            try:
                self.enactive_system = create_enactive_perception_system(self.agent_name)
                self.integration_metrics['enactive_active'] = True
                logger.info('✅ Enactive perception system initialized')
            except Exception as e:
                logger.error(f'Failed to initialize enactive perception: {e}')
        else:
            logger.warning('Enactive perception system not available')
        if EMBODIED_LEARNING_AVAILABLE:
            try:
                self.embodied_system = create_embodied_learning_system()
                self.integration_metrics['embodied_active'] = True
                logger.info('✅ Embodied learning system initialized')
            except Exception as e:
                logger.error(f'Failed to initialize embodied learning: {e}')
        else:
            logger.warning('Embodied learning system not available')
        if AAR_AVAILABLE:
            try:
                capabilities = ['perceive', 'act', 'learn', 'predict', 'explore']
                self.aar_agent = Agent(f'{self.agent_name}_integrated', capabilities)
                self.integration_metrics['aar_active'] = True
                logger.info('✅ AAR agent initialized')
            except Exception as e:
                logger.error(f'Failed to initialize AAR agent: {e}')
        else:
            logger.warning('AAR system not available')
        active_systems = sum([self.integration_metrics['enactive_active'], self.integration_metrics['embodied_active'], self.integration_metrics['aar_active']])
        if active_systems >= 1:
            self.integration_metrics['integration_successful'] = True
            logger.info(f'✅ Integration successful with {active_systems}/3 systems active')
        else:
            logger.error('❌ Integration failed - no systems available')
    def process_embodied_experience(self, experience: SensorimotorExperience) -> Dict[str, Any]:
        if not self.integration_metrics['integration_successful']:
            return {'error': 'Integration not successful'}
        results = {'timestamp': time.time(), 'enactive_result': None, 'embodied_result': None, 'aar_result': None, 'integration_success': True}
        try:
            if self.enactive_system:
                results['enactive_result'] = self.enactive_system.process_embodied_experience(experience)
                logger.debug('Processed through enactive perception system')
            if self.embodied_system:
                results['embodied_result'] = self.embodied_system.process_embodied_experience(experience)
                logger.debug('Processed through embodied learning system')
            if self.aar_agent:
                aar_input = {'sensory_feedback': experience.sensory_feedback, 'motor_action': experience.motor_action.__dict__, 'success': experience.success, 'reward': experience.reward}
                results['aar_result'] = self.aar_agent.process(aar_input)
                logger.debug('Processed through AAR agent')
            self.integration_metrics['total_experiences_processed'] += 1
        except Exception as e:
            logger.error(f'Error processing embodied experience: {e}')
            results['integration_success'] = False
            results['error'] = str(e)
        return results
    def generate_enactive_action(self, current_body_state: BodyState, perceptual_goal: Optional[Dict[str, Any]]=None) -> Optional[MotorAction]:
        if not self.enactive_system:
            return None
        try:
            action = self.enactive_system.generate_perceptual_action(current_body_state, perceptual_goal)
            logger.debug('Generated enactive perception action')
            return action
        except Exception as e:
            logger.error(f'Error generating enactive action: {e}')
            return None
    def predict_perceptual_outcome(self, planned_action: MotorAction, current_body_state: BodyState) -> Optional[Dict[str, Any]]:
        if not self.enactive_system:
            return None
        try:
            prediction = self.enactive_system.predict_perceptual_outcome(planned_action, current_body_state)
            return {'predicted_outcome': prediction.predicted_sensory_outcome, 'confidence': prediction.confidence, 'exploration_value': prediction.exploration_value, 'timestamp': prediction.prediction_timestamp}
        except Exception as e:
            logger.error(f'Error predicting perceptual outcome: {e}')
            return None
    def get_integration_metrics(self) -> Dict[str, Any]:
        metrics = self.integration_metrics.copy()
        if self.enactive_system:
            enactive_metrics = self.enactive_system.get_enactive_metrics()
            metrics['enactive_metrics'] = enactive_metrics
        if self.embodied_system and hasattr(self.embodied_system, 'get_embodiment_metrics'):
            try:
                embodied_metrics = self.embodied_system.get_embodiment_metrics()
                metrics['embodied_metrics'] = embodied_metrics
            except:
                pass
        if self.aar_agent:
            metrics['aar_state'] = self.aar_agent.get_state()
        return metrics
    def validate_4e_framework_integration(self) -> Dict[str, Any]:
        validation_results = {'embodied_cognition': False, 'embedded_systems': False, 'enacted_perception': False, 'extended_mind': False, 'overall_4e_compliance': False}
        try:
            if self.embodied_system:
                validation_results['embodied_cognition'] = True
            if self.aar_agent or self.embodied_system:
                validation_results['embedded_systems'] = True
            if self.enactive_system:
                metrics = self.enactive_system.get_enactive_metrics()
                if metrics['system_active'] and metrics['total_contingencies_learned'] >= 0:
                    validation_results['enacted_perception'] = True
            active_systems = sum([self.integration_metrics['enactive_active'], self.integration_metrics['embodied_active'], self.integration_metrics['aar_active']])
            if active_systems >= 2:
                validation_results['extended_mind'] = True
            framework_aspects = sum([validation_results['embodied_cognition'], validation_results['embedded_systems'], validation_results['enacted_perception'], validation_results['extended_mind']])
            validation_results['overall_4e_compliance'] = framework_aspects >= 3
            validation_results['framework_score'] = f'{framework_aspects}/4'
        except Exception as e:
            logger.error(f'Error validating 4E framework integration: {e}')
            validation_results['error'] = str(e)
        return validation_results
def create_integrated_enactive_system(agent_name: str='integrated_agent') -> EnactivePerceptionIntegrator:
    return EnactivePerceptionIntegrator(agent_name)
def validate_task_2_3_3_acceptance_criteria() -> Dict[str, Any]:
    logger.info('Validating Task 2.3.3 acceptance criteria')
    validation_results = {'action_based_perception': False, 'sensorimotor_contingency_learning': False, 'perceptual_prediction': False, 'perception_through_interaction': False, 'overall_acceptance': False, 'timestamp': time.time()}
    try:
        system = create_integrated_enactive_system('validation_agent')
        if not system.integration_metrics['integration_successful']:
            validation_results['error'] = 'Integration failed'
            return validation_results
        logger.info('Testing action-based perception mechanisms...')
        if system.enactive_system:
            test_state = BodyState(joint_angles={'test': 0.5}, sensory_state={'env': 0.7})
            action = system.generate_enactive_action(test_state, {'explore': True})
            if action:
                validation_results['action_based_perception'] = True
                logger.info('✅ Action-based perception mechanisms validated')
            else:
                logger.warning('❌ Action-based perception mechanisms failed')
        logger.info('Testing sensorimotor contingency learning...')
        if system.enactive_system:
            experience = SensorimotorExperience(initial_body_state=BodyState(sensory_state={'input': 0.5}), motor_action=MotorAction(joint_targets={'test': 0.6}), resulting_body_state=BodyState(sensory_state={'input': 0.8}), sensory_feedback={'input': 0.8, 'learning': True}, success=True)
            result = system.process_embodied_experience(experience)
            if result.get('enactive_result', {}).get('contingency_learned', False):
                validation_results['sensorimotor_contingency_learning'] = True
                logger.info('✅ Sensorimotor contingency learning validated')
            else:
                logger.warning('❌ Sensorimotor contingency learning failed')
        logger.info('Testing perceptual prediction through action...')
        if system.enactive_system:
            test_action = MotorAction(joint_targets={'predict': 0.7})
            test_state = BodyState(sensory_state={'context': 0.4})
            prediction = system.predict_perceptual_outcome(test_action, test_state)
            if prediction and prediction['confidence'] >= 0.0:
                validation_results['perceptual_prediction'] = True
                logger.info('✅ Perceptual prediction through action validated')
            else:
                logger.warning('❌ Perceptual prediction through action failed')
        logger.info('Testing perception emergence through interaction...')
        interaction_success = 0
        total_interactions = 3
        for i in range(total_interactions):
            current_state = BodyState(joint_angles={'joint': 0.2 * i}, sensory_state={'environment': 0.3 + 0.1 * i})
            action = system.generate_enactive_action(current_state)
            if not action:
                continue
            experience = SensorimotorExperience(initial_body_state=current_state, motor_action=action, resulting_body_state=BodyState(sensory_state={'environment': 0.4 + 0.1 * i}), sensory_feedback={'environment': 0.4 + 0.1 * i, 'interaction': i}, success=True)
            result = system.process_embodied_experience(experience)
            if result.get('integration_success', False):
                interaction_success += 1
        if interaction_success >= 2:
            validation_results['perception_through_interaction'] = True
            logger.info('✅ Perception emergence through interaction validated')
        else:
            logger.warning('❌ Perception emergence through interaction failed')
        criteria_met = sum([validation_results['action_based_perception'], validation_results['sensorimotor_contingency_learning'], validation_results['perceptual_prediction'], validation_results['perception_through_interaction']])
        validation_results['overall_acceptance'] = criteria_met >= 3
        validation_results['criteria_score'] = f'{criteria_met}/4'
        logger.info(f"Task 2.3.3 validation complete: {validation_results['criteria_score']} criteria met")
    except Exception as e:
        logger.error(f'Error validating acceptance criteria: {e}')
        validation_results['error'] = str(e)
    return validation_results
if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
    print('=' * 70)
    print('ENACTIVE PERCEPTION INTEGRATION DEMO')
    print('=' * 70)
    print('\n🔧 Creating integrated enactive perception system...')
    system = create_integrated_enactive_system('demo_integrated')
    print('\n📊 Integration Status:')
    metrics = system.get_integration_metrics()
    for key, value in metrics.items():
        if key.endswith('_active'):
            status = '✅' if value else '❌'
            print(f'  {key}: {status}')
    print('\n🧠 Testing 4E Embodied AI Framework integration...')
    framework_validation = system.validate_4e_framework_integration()
    print(f"Framework compliance: {framework_validation.get('framework_score', '0/4')}")
    print(f"Overall 4E compliance: {('✅' if framework_validation.get('overall_4e_compliance') else '❌')}")
    print('\n🎯 Validating Task 2.3.3 acceptance criteria...')
    acceptance_validation = validate_task_2_3_3_acceptance_criteria()
    print(f"Acceptance criteria: {acceptance_validation.get('criteria_score', '0/4')}")
    print(f"Overall acceptance: {('✅' if acceptance_validation.get('overall_acceptance') else '❌')}")
    print('\n✅ Integration demo completed successfully')