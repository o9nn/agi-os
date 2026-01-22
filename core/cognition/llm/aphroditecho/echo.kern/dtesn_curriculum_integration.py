import time
import logging
import numpy as np
from typing import Dict, List, Optional, Any
from dataclasses import dataclass
try:
    from .curriculum_learning import CurriculumLearningSystem, SkillObjective, DifficultyLevel, LearningStage, CurriculumConfig
except ImportError:
    from curriculum_learning import CurriculumLearningSystem
try:
    from .dtesn_integration import DTESNIntegratedSystem, DTESNConfiguration
    DTESN_AVAILABLE = True
except ImportError:
    try:
        from dtesn_integration import DTESNIntegratedSystem, DTESNConfiguration
        DTESN_AVAILABLE = True
    except ImportError:
        DTESN_AVAILABLE = False
try:
    from .performance_monitor import UnifiedPerformanceMonitor
    PERFORMANCE_MONITOR_AVAILABLE = True
except ImportError:
    try:
        from performance_monitor import UnifiedPerformanceMonitor
        PERFORMANCE_MONITOR_AVAILABLE = True
    except ImportError:
        PERFORMANCE_MONITOR_AVAILABLE = False
logger = logging.getLogger(__name__)
@dataclass
class DTESNCurriculumConfig:
    enable_dtesn_feedback: bool = True
    enable_membrane_adaptation: bool = True
    cognitive_learning_weight: float = 0.3
    adaptation_threshold: float = 0.1
    dtesn_update_frequency: int = 5
    reservoir_curriculum_coupling: bool = True
class DTESNCurriculumIntegration:
    def __init__(self, curriculum_system: CurriculumLearningSystem, dtesn_system: Optional[Any]=None, performance_monitor: Optional[Any]=None, config: Optional[DTESNCurriculumConfig]=None):
        self.curriculum_system = curriculum_system
        self.dtesn_system = dtesn_system
        self.performance_monitor = performance_monitor
        self.config = config or DTESNCurriculumConfig()
        self.dtesn_curriculum_history: List[Dict[str, Any]] = []
        self.cognitive_skill_mapping: Dict[str, str] = {}
        self.reservoir_skill_states: Dict[str, np.ndarray] = {}
        self.integration_metrics = {'total_dtesn_updates': 0, 'successful_adaptations': 0, 'cognitive_improvements': 0, 'last_update_timestamp': time.time()}
        self._initialize_integration()
        logger.info('DTESN-Curriculum integration initialized')
    def _initialize_integration(self):
        try:
            if self.dtesn_system and DTESN_AVAILABLE:
                self._establish_cognitive_skill_mapping()
                self._initialize_reservoir_skill_states()
                logger.info('DTESN integration enabled')
            else:
                logger.info('DTESN system not available, using standard curriculum learning')
        except Exception as e:
            logger.warning(f'DTESN integration initialization failed: {e}')
    def _establish_cognitive_skill_mapping(self):
        cognitive_mappings = {'attention': ['basic_attention', 'selective_attention', 'divided_attention'], 'memory': ['working_memory', 'episodic_memory', 'procedural_memory'], 'pattern_recognition': ['pattern_recognition', 'feature_detection', 'classification'], 'motor_control': ['motor_coordination', 'adaptive_control', 'fine_motor_skills'], 'reasoning': ['logical_reasoning', 'spatial_reasoning', 'causal_reasoning'], 'learning': ['associative_learning', 'reinforcement_learning', 'transfer_learning']}
        for cognitive_process, skills in cognitive_mappings.items():
            for skill in skills:
                if skill in self.curriculum_system.skills_catalog:
                    self.cognitive_skill_mapping[skill] = cognitive_process
        logger.info(f'Established cognitive skill mapping for {len(self.cognitive_skill_mapping)} skills')
    def _initialize_reservoir_skill_states(self):
        if not self.dtesn_system:
            return
        try:
            for skill_id in self.curriculum_system.skills_catalog.keys():
                reservoir_size = getattr(self.dtesn_system.config, 'reservoir_size', 100)
                self.reservoir_skill_states[skill_id] = np.zeros(reservoir_size)
            logger.info(f'Initialized reservoir states for {len(self.reservoir_skill_states)} skills')
        except Exception as e:
            logger.warning(f'Failed to initialize reservoir skill states: {e}')
    def update_skill_with_dtesn_feedback(self, skill_id: str, performance_score: float, session_duration: float, sensory_input: Optional[np.ndarray]=None, motor_output: Optional[np.ndarray]=None) -> Dict[str, Any]:
        standard_result = self.curriculum_system.update_skill_progress(skill_id, performance_score, session_duration)
        if not standard_result.get('success', False):
            return standard_result
        dtesn_enhancement = {}
        if self.dtesn_system and self.config.enable_dtesn_feedback:
            try:
                dtesn_enhancement = self._apply_dtesn_cognitive_learning(skill_id, performance_score, sensory_input, motor_output)
            except Exception as e:
                logger.warning(f'DTESN enhancement failed for {skill_id}: {e}')
                dtesn_enhancement = {'error': str(e)}
        enhanced_result = standard_result.copy()
        enhanced_result['dtesn_enhancement'] = dtesn_enhancement
        enhanced_result['integration_metrics'] = self.integration_metrics.copy()
        self.integration_metrics['total_dtesn_updates'] += 1
        self.integration_metrics['last_update_timestamp'] = time.time()
        if dtesn_enhancement.get('cognitive_improvement', False):
            self.integration_metrics['cognitive_improvements'] += 1
        integration_event = {'timestamp': time.time(), 'skill_id': skill_id, 'performance_score': performance_score, 'dtesn_enabled': bool(dtesn_enhancement), 'cognitive_process': self.cognitive_skill_mapping.get(skill_id, 'general'), 'enhancement_metrics': dtesn_enhancement}
        self.dtesn_curriculum_history.append(integration_event)
        return enhanced_result
    def _apply_dtesn_cognitive_learning(self, skill_id: str, performance_score: float, sensory_input: Optional[np.ndarray]=None, motor_output: Optional[np.ndarray]=None) -> Dict[str, Any]:
        if not self.dtesn_system:
            return {'error': 'dtesn_system_not_available'}
        try:
            current_reservoir_state = self.reservoir_skill_states.get(skill_id)
            if current_reservoir_state is None:
                return {'error': 'reservoir_state_not_found'}
            input_pattern = self._create_dtesn_input_pattern(skill_id, performance_score, sensory_input, motor_output)
            dtesn_result = self._update_dtesn_with_skill_context(input_pattern, skill_id)
            cognitive_analysis = self._analyze_cognitive_learning_outcome(skill_id, dtesn_result, performance_score)
            if 'new_reservoir_state' in dtesn_result:
                self.reservoir_skill_states[skill_id] = dtesn_result['new_reservoir_state']
            return {'dtesn_update_successful': True, 'cognitive_process': self.cognitive_skill_mapping.get(skill_id, 'general'), 'reservoir_dynamics': dtesn_result.get('dynamics_metrics', {}), 'cognitive_improvement': cognitive_analysis.get('improvement_detected', False), 'learning_efficiency': cognitive_analysis.get('learning_efficiency', 0.0), 'adaptation_suggestion': cognitive_analysis.get('adaptation_suggestion', 'maintain')}
        except Exception as e:
            logger.error(f'DTESN cognitive learning failed for {skill_id}: {e}')
            return {'error': str(e)}
    def _create_dtesn_input_pattern(self, skill_id: str, performance_score: float, sensory_input: Optional[np.ndarray]=None, motor_output: Optional[np.ndarray]=None) -> np.ndarray:
        base_pattern = [performance_score, hash(skill_id) % 100 / 100.0, time.time() % 1000 / 1000.0]
        if sensory_input is not None:
            sensory_normalized = sensory_input.flatten()[:10]
            sensory_normalized = sensory_normalized / (np.linalg.norm(sensory_normalized) + 1e-08)
            base_pattern.extend(sensory_normalized)
        if motor_output is not None:
            motor_normalized = motor_output.flatten()[:5]
            motor_normalized = motor_normalized / (np.linalg.norm(motor_normalized) + 1e-08)
            base_pattern.extend(motor_normalized)
        target_size = 20
        while len(base_pattern) < target_size:
            base_pattern.append(0.0)
        return np.array(base_pattern[:target_size])
    def _update_dtesn_with_skill_context(self, input_pattern: np.ndarray, skill_id: str) -> Dict[str, Any]:
        try:
            if hasattr(self.dtesn_system, 'update'):
                result = self.dtesn_system.update(input_pattern)
            else:
                result = self._simulate_dtesn_update(input_pattern, skill_id)
            return result
        except Exception as e:
            logger.warning(f'DTESN update failed, using fallback: {e}')
            return self._simulate_dtesn_update(input_pattern, skill_id)
    def _simulate_dtesn_update(self, input_pattern: np.ndarray, skill_id: str) -> Dict[str, Any]:
        current_state = self.reservoir_skill_states.get(skill_id, np.zeros(100))
        weight_matrix = np.random.normal(0, 0.1, (len(current_state), len(input_pattern)))
        new_state = np.tanh(current_state * 0.9 + weight_matrix @ input_pattern * 0.1)
        state_change = np.linalg.norm(new_state - current_state)
        activation_level = np.mean(np.abs(new_state))
        complexity_measure = np.std(new_state)
        return {'new_reservoir_state': new_state, 'dynamics_metrics': {'state_change': state_change, 'activation_level': activation_level, 'complexity_measure': complexity_measure}, 'simulation': True}
    def _analyze_cognitive_learning_outcome(self, skill_id: str, dtesn_result: Dict[str, Any], performance_score: float) -> Dict[str, Any]:
        dynamics = dtesn_result.get('dynamics_metrics', {})
        state_change = dynamics.get('state_change', 0.0)
        activation_level = dynamics.get('activation_level', 0.0)
        improvement_detected = state_change > 0.1 and activation_level > 0.3 and (performance_score > 0.6)
        learning_efficiency = min(1.0, state_change * performance_score)
        if learning_efficiency > 0.8:
            adaptation_suggestion = 'increase_difficulty'
        elif learning_efficiency < 0.3:
            adaptation_suggestion = 'decrease_difficulty'
        else:
            adaptation_suggestion = 'maintain'
        return {'improvement_detected': improvement_detected, 'learning_efficiency': learning_efficiency, 'adaptation_suggestion': adaptation_suggestion, 'cognitive_complexity': dynamics.get('complexity_measure', 0.0)}
    def get_curriculum_skills_for_cognitive_process(self, cognitive_process: str) -> List[str]:
        return [skill_id for skill_id, process in self.cognitive_skill_mapping.items() if process == cognitive_process]
    def adapt_curriculum_based_on_dtesn_feedback(self) -> Dict[str, Any]:
        if not self.dtesn_curriculum_history:
            return {'adapted': False, 'reason': 'no_history_available'}
        adaptations = []
        try:
            recent_events = self.dtesn_curriculum_history[-10:]
            process_performance = {}
            for event in recent_events:
                process = event.get('cognitive_process', 'general')
                performance = event.get('performance_score', 0.0)
                if process not in process_performance:
                    process_performance[process] = []
                process_performance[process].append(performance)
            for process, performances in process_performance.items():
                avg_performance = np.mean(performances)
                performance_variance = np.var(performances)
                if avg_performance > 0.85 and performance_variance < 0.02:
                    skills = self.get_curriculum_skills_for_cognitive_process(process)
                    for skill_id in skills:
                        if not self.curriculum_system.learning_progress[skill_id].mastery_achieved:
                            adaptations.append({'skill_id': skill_id, 'adaptation': 'increase_difficulty', 'reason': f'high_performance_in_{process}'})
                elif avg_performance < 0.4:
                    skills = self.get_curriculum_skills_for_cognitive_process(process)
                    for skill_id in skills:
                        adaptations.append({'skill_id': skill_id, 'adaptation': 'decrease_difficulty', 'reason': f'low_performance_in_{process}'})
            successful_adaptations = 0
            for adaptation in adaptations:
                try:
                    logger.info(f'DTESN-suggested adaptation: {adaptation}')
                    successful_adaptations += 1
                except Exception as e:
                    logger.warning(f'Failed to apply adaptation {adaptation}: {e}')
            self.integration_metrics['successful_adaptations'] += successful_adaptations
            return {'adapted': len(adaptations) > 0, 'total_adaptations': len(adaptations), 'successful_adaptations': successful_adaptations, 'adaptations': adaptations}
        except Exception as e:
            logger.error(f'Curriculum adaptation based on DTESN feedback failed: {e}')
            return {'adapted': False, 'error': str(e)}
    def get_integration_status(self) -> Dict[str, Any]:
        curriculum_status = self.curriculum_system.get_curriculum_status()
        integration_status = {'dtesn_available': DTESN_AVAILABLE and self.dtesn_system is not None, 'performance_monitor_available': PERFORMANCE_MONITOR_AVAILABLE and self.performance_monitor is not None, 'cognitive_skill_mappings': len(self.cognitive_skill_mapping), 'reservoir_skill_states': len(self.reservoir_skill_states), 'integration_metrics': self.integration_metrics.copy(), 'recent_dtesn_events': len(self.dtesn_curriculum_history[-10:]) if self.dtesn_curriculum_history else 0}
        return {'curriculum_status': curriculum_status, 'integration_status': integration_status, 'config': {'enable_dtesn_feedback': self.config.enable_dtesn_feedback, 'enable_membrane_adaptation': self.config.enable_membrane_adaptation, 'cognitive_learning_weight': self.config.cognitive_learning_weight, 'dtesn_update_frequency': self.config.dtesn_update_frequency}}
    def export_integration_data(self) -> Dict[str, Any]:
        curriculum_data = self.curriculum_system.export_curriculum_data()
        integration_data = {'cognitive_skill_mapping': self.cognitive_skill_mapping, 'dtesn_curriculum_history': self.dtesn_curriculum_history[-100:], 'integration_metrics': self.integration_metrics, 'reservoir_skill_states': {skill_id: state.tolist() for skill_id, state in self.reservoir_skill_states.items()}, 'export_timestamp': time.time()}
        return {'curriculum_data': curriculum_data, 'integration_data': integration_data}
def create_dtesn_curriculum_system(dtesn_system: Optional[Any]=None, performance_monitor: Optional[Any]=None) -> DTESNCurriculumIntegration:
    try:
        from .curriculum_learning import create_default_curriculum
    except ImportError:
        from curriculum_learning import create_default_curriculum
    curriculum = create_default_curriculum()
    integration = DTESNCurriculumIntegration(curriculum_system=curriculum, dtesn_system=dtesn_system, performance_monitor=performance_monitor)
    logger.info('DTESN-Curriculum integrated system created')
    return integration