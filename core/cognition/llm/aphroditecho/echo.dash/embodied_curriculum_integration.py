#!/usr/bin/env python3
"""
Embodied Learning Curriculum Integration
Connects curriculum learning with the existing embodied learning system
to provide comprehensive skill development for embodied AI agents.
"""

import time
import logging
import numpy as np
from typing import Dict, List, Optional, Any
from dataclasses import dataclass

# Import curriculum learning components
try:
    from ..echo.kern.curriculum_learning import (
        CurriculumLearningSystem, 
        SkillObjective, 
        DifficultyLevel, 
        LearningStage
    )
    CURRICULUM_AVAILABLE = True
except ImportError:
    try:
        from echo.kern.curriculum_learning import (
            CurriculumLearningSystem, 
            SkillObjective, 
            DifficultyLevel, 
            LearningStage
        )
        CURRICULUM_AVAILABLE = True
    except ImportError:
        try:
            import sys
            import os
            sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'echo.kern'))
            from curriculum_learning import (
                CurriculumLearningSystem, 
                SkillObjective, 
                DifficultyLevel, 
                LearningStage
            )
            CURRICULUM_AVAILABLE = True
        except ImportError:
            CURRICULUM_AVAILABLE = False

# Import existing embodied learning components
try:
    from .embodied_learning import (
        SkillAcquisitionTracker,
        MotorSkillLearner,
        SensorimotorExperience,
        BodyState
    )
    EMBODIED_LEARNING_AVAILABLE = True
except ImportError:
    EMBODIED_LEARNING_AVAILABLE = False

logger = logging.getLogger(__name__)


@dataclass
class EmbodiedCurriculumConfig:
    """Configuration for embodied learning curriculum integration"""
    enable_motor_skill_progression: bool = True
    enable_sensorimotor_adaptation: bool = True
    motor_learning_weight: float = 0.4
    sensorimotor_weight: float = 0.3
    curriculum_update_frequency: int = 5
    embodied_skill_threshold: float = 0.75


class EmbodiedCurriculumIntegration:
    """
    Integration between curriculum learning and embodied learning systems.
    
    This class provides enhanced embodied skill development by combining
    curriculum learning progression with sensorimotor learning capabilities.
    """
    
    def __init__(
        self,
        curriculum_system: Optional[Any] = None,
        skill_tracker: Optional[Any] = None,
        motor_learner: Optional[Any] = None,
        config: Optional[EmbodiedCurriculumConfig] = None
    ):
        self.curriculum_system = curriculum_system
        self.skill_tracker = skill_tracker
        self.motor_learner = motor_learner
        self.config = config or EmbodiedCurriculumConfig()
        
        # Integration state
        self.embodied_skill_mapping: Dict[str, str] = {}
        self.motor_progression_history: List[Dict[str, Any]] = []
        self.sensorimotor_curriculum_events: List[Dict[str, Any]] = []
        
        # Performance tracking
        self.integration_metrics = {
            'total_embodied_updates': 0,
            'motor_skill_progressions': 0,
            'sensorimotor_adaptations': 0,
            'curriculum_motor_alignments': 0,
            'last_update_timestamp': time.time()
        }
        
        self._initialize_embodied_integration()
        logger.info("Embodied-Curriculum integration initialized")
    
    def _initialize_embodied_integration(self):
        """Initialize the embodied learning curriculum integration"""
        try:
            if CURRICULUM_AVAILABLE and EMBODIED_LEARNING_AVAILABLE:
                self._establish_embodied_skill_mapping()
                self._register_embodied_curriculum_skills()
                logger.info("Embodied learning curriculum integration enabled")
            else:
                logger.warning("Required components not available for embodied curriculum integration")
                
        except Exception as e:
            logger.warning(f"Embodied curriculum integration initialization failed: {e}")
    
    def _establish_embodied_skill_mapping(self):
        """Establish mapping between curriculum skills and embodied learning capabilities"""
        
        # Map curriculum skills to embodied learning categories
        embodied_mappings = {
            'motor_coordination': ['basic_movement', 'joint_control', 'balance'],
            'adaptive_control': ['environmental_adaptation', 'object_manipulation', 'force_control'],
            'spatial_awareness': ['proprioception', 'spatial_memory', 'navigation'],
            'fine_motor_skills': ['precision_grasping', 'tool_use', 'dexterous_manipulation'],
            'sensorimotor_integration': ['eye_hand_coordination', 'tactile_feedback', 'multisensory_fusion'],
            'locomotion': ['walking', 'running', 'climbing', 'balance_recovery']
        }
        
        # Create bidirectional mapping
        for embodied_category, skills in embodied_mappings.items():
            for skill in skills:
                self.embodied_skill_mapping[skill] = embodied_category
        
        logger.info(f"Established embodied skill mapping for {len(self.embodied_skill_mapping)} skills")
    
    def _register_embodied_curriculum_skills(self):
        """Register embodied learning skills in the curriculum system"""
        
        if not self.curriculum_system:
            return
        
        # Define embodied skills for the curriculum
        embodied_skills = [
            SkillObjective(
                skill_id="basic_movement",
                name="Basic Movement Control",
                description="Fundamental movement control and coordination",
                difficulty_level=DifficultyLevel.BEGINNER,
                stage=LearningStage.FOUNDATION,
                performance_threshold=0.7,
                practice_sessions_required=8,
                estimated_duration=45.0
            ),
            SkillObjective(
                skill_id="joint_control",
                name="Joint Control",
                description="Individual joint control and coordination",
                difficulty_level=DifficultyLevel.NOVICE,
                stage=LearningStage.SKILL_BUILDING,
                prerequisites=["basic_movement"],
                performance_threshold=0.75,
                practice_sessions_required=10,
                estimated_duration=60.0
            ),
            SkillObjective(
                skill_id="environmental_adaptation",
                name="Environmental Adaptation",
                description="Adapt motor responses to environmental changes",
                difficulty_level=DifficultyLevel.INTERMEDIATE,
                stage=LearningStage.INTEGRATION,
                prerequisites=["joint_control", "basic_movement"],
                performance_threshold=0.8,
                practice_sessions_required=12,
                estimated_duration=75.0
            ),
            SkillObjective(
                skill_id="object_manipulation",
                name="Object Manipulation",
                description="Manipulate objects in 3D space",
                difficulty_level=DifficultyLevel.INTERMEDIATE,
                stage=LearningStage.INTEGRATION,
                prerequisites=["joint_control"],
                performance_threshold=0.78,
                practice_sessions_required=15,
                estimated_duration=90.0
            ),
            SkillObjective(
                skill_id="precision_grasping",
                name="Precision Grasping",
                description="Precise grasping and fine motor control",
                difficulty_level=DifficultyLevel.ADVANCED,
                stage=LearningStage.MASTERY,
                prerequisites=["object_manipulation"],
                performance_threshold=0.85,
                practice_sessions_required=20,
                estimated_duration=120.0
            ),
            SkillObjective(
                skill_id="eye_hand_coordination",
                name="Eye-Hand Coordination",
                description="Coordinate visual input with motor actions",
                difficulty_level=DifficultyLevel.ADVANCED,
                stage=LearningStage.MASTERY,
                prerequisites=["object_manipulation", "precision_grasping"],
                performance_threshold=0.88,
                practice_sessions_required=18,
                estimated_duration=100.0
            )
        ]
        
        # Register skills in curriculum
        registered_count = 0
        for skill in embodied_skills:
            if self.curriculum_system.register_skill(skill):
                registered_count += 1
        
        logger.info(f"Registered {registered_count} embodied skills in curriculum")
    
    def update_embodied_skill_progress(
        self,
        skill_id: str,
        sensorimotor_experience: Any,
        additional_metrics: Optional[Dict[str, float]] = None
    ) -> Dict[str, Any]:
        """
        Update embodied skill progress using sensorimotor experience data
        """
        
        try:
            # Extract performance metrics from sensorimotor experience
            performance_score = getattr(sensorimotor_experience, 'reward', 0.0)
            session_duration = getattr(sensorimotor_experience, 'duration', 0.0)
            
            if hasattr(sensorimotor_experience, 'motor_action'):
                session_duration = getattr(sensorimotor_experience.motor_action, 'duration', 0.0)
            
            # Create embodied-specific metrics
            embodied_metrics = self._extract_embodied_metrics(sensorimotor_experience)
            
            # Combine with any additional metrics
            all_metrics = embodied_metrics.copy()
            if additional_metrics:
                all_metrics.update(additional_metrics)
            
            # Update curriculum system if available
            curriculum_result = {}
            if self.curriculum_system:
                curriculum_result = self.curriculum_system.update_skill_progress(
                    skill_id, performance_score, session_duration, all_metrics
                )
            
            # Update embodied learning trackers if available
            embodied_result = {}
            if self.skill_tracker:
                embodied_result = self._update_embodied_trackers(
                    skill_id, sensorimotor_experience, all_metrics
                )
            
            # Analyze embodied learning progression
            progression_analysis = self._analyze_embodied_progression(
                skill_id, sensorimotor_experience, embodied_metrics
            )
            
            # Update integration metrics
            self.integration_metrics['total_embodied_updates'] += 1
            self.integration_metrics['last_update_timestamp'] = time.time()
            
            if progression_analysis.get('motor_skill_improved', False):
                self.integration_metrics['motor_skill_progressions'] += 1
            
            if progression_analysis.get('sensorimotor_adapted', False):
                self.integration_metrics['sensorimotor_adaptations'] += 1
            
            # Log integration event
            integration_event = {
                'timestamp': time.time(),
                'skill_id': skill_id,
                'performance_score': performance_score,
                'embodied_category': self.embodied_skill_mapping.get(skill_id, 'general'),
                'embodied_metrics': embodied_metrics,
                'progression_analysis': progression_analysis
            }
            self.sensorimotor_curriculum_events.append(integration_event)
            
            return {
                'success': True,
                'curriculum_result': curriculum_result,
                'embodied_result': embodied_result,
                'progression_analysis': progression_analysis,
                'embodied_metrics': embodied_metrics,
                'integration_metrics': self.integration_metrics.copy()
            }
            
        except Exception as e:
            logger.error(f"Failed to update embodied skill progress for {skill_id}: {e}")
            return {'success': False, 'error': str(e)}
    
    def _extract_embodied_metrics(self, sensorimotor_experience: Any) -> Dict[str, float]:
        """Extract embodied learning metrics from sensorimotor experience"""
        
        metrics = {}
        
        try:
            # Extract basic performance metrics
            if hasattr(sensorimotor_experience, 'success'):
                metrics['task_success'] = float(sensorimotor_experience.success)
            
            if hasattr(sensorimotor_experience, 'reward'):
                metrics['reward_signal'] = float(sensorimotor_experience.reward)
            
            # Extract body state metrics
            if hasattr(sensorimotor_experience, 'initial_body_state'):
                body_state = sensorimotor_experience.initial_body_state
                
                if hasattr(body_state, 'joint_angles'):
                    joint_angles = body_state.joint_angles
                    if joint_angles:
                        metrics['joint_range_utilization'] = np.std(list(joint_angles.values()))
                        metrics['joint_coordination'] = 1.0 / (1.0 + np.var(list(joint_angles.values())))
                
                if hasattr(body_state, 'muscle_activations'):
                    muscle_acts = body_state.muscle_activations
                    if muscle_acts:
                        metrics['muscle_efficiency'] = np.mean(list(muscle_acts.values()))
                        metrics['muscle_coordination'] = 1.0 / (1.0 + np.var(list(muscle_acts.values())))
            
            # Extract motor action metrics
            if hasattr(sensorimotor_experience, 'motor_action'):
                motor_action = sensorimotor_experience.motor_action
                
                if hasattr(motor_action, 'force') and motor_action.force:
                    metrics['force_control'] = min(1.0, motor_action.force)
                
                if hasattr(motor_action, 'precision') and motor_action.precision:
                    metrics['movement_precision'] = motor_action.precision
            
            # Calculate composite embodied metrics
            if len(metrics) > 0:
                metrics['embodied_performance'] = np.mean(list(metrics.values()))
                metrics['embodied_consistency'] = 1.0 / (1.0 + np.var(list(metrics.values())))
            
        except Exception as e:
            logger.warning(f"Failed to extract some embodied metrics: {e}")
        
        return metrics
    
    def _update_embodied_trackers(
        self,
        skill_id: str,
        sensorimotor_experience: Any,
        embodied_metrics: Dict[str, float]
    ) -> Dict[str, Any]:
        """Update embodied learning trackers with skill progress"""
        
        results = {}
        
        try:
            # Update skill acquisition tracker
            if self.skill_tracker and hasattr(self.skill_tracker, 'track_practice_session'):
                tracker_result = self.skill_tracker.track_practice_session(
                    skill_id, sensorimotor_experience
                )
                results['skill_tracker'] = tracker_result
            
            # Update motor skill learner
            if self.motor_learner and hasattr(self.motor_learner, 'practice_skill'):
                # Extract motor-specific data from sensorimotor experience
                motor_data = self._extract_motor_learning_data(sensorimotor_experience)
                
                motor_result = self.motor_learner.practice_skill(
                    skill_id,
                    motor_data.get('target', [0.0, 0.0, 0.0]),
                    motor_data.get('outcome', [0.0, 0.0, 0.0]),
                    motor_data.get('context', {})
                )
                results['motor_learner'] = motor_result
            
        except Exception as e:
            logger.warning(f"Failed to update embodied trackers: {e}")
            results['error'] = str(e)
        
        return results
    
    def _extract_motor_learning_data(self, sensorimotor_experience: Any) -> Dict[str, Any]:
        """Extract motor learning data from sensorimotor experience"""
        
        motor_data = {
            'target': [0.0, 0.0, 0.0],
            'outcome': [0.0, 0.0, 0.0],
            'context': {}
        }
        
        try:
            # Extract target and outcome positions
            if hasattr(sensorimotor_experience, 'initial_body_state'):
                initial_state = sensorimotor_experience.initial_body_state
                if hasattr(initial_state, 'position'):
                    motor_data['target'] = list(initial_state.position)
            
            if hasattr(sensorimotor_experience, 'resulting_body_state'):
                result_state = sensorimotor_experience.resulting_body_state
                if hasattr(result_state, 'position'):
                    motor_data['outcome'] = list(result_state.position)
            
            # Extract context information
            if hasattr(sensorimotor_experience, 'context'):
                motor_data['context'] = sensorimotor_experience.context
            
            if hasattr(sensorimotor_experience, 'timestamp'):
                motor_data['context']['timestamp'] = sensorimotor_experience.timestamp
                
        except Exception as e:
            logger.warning(f"Failed to extract motor learning data: {e}")
        
        return motor_data
    
    def _analyze_embodied_progression(
        self,
        skill_id: str,
        sensorimotor_experience: Any,
        embodied_metrics: Dict[str, float]
    ) -> Dict[str, Any]:
        """Analyze embodied learning progression and adaptation"""
        
        analysis = {
            'motor_skill_improved': False,
            'sensorimotor_adapted': False,
            'embodied_learning_rate': 0.0,
            'adaptation_recommendation': 'maintain'
        }
        
        try:
            # Analyze motor skill improvement
            embodied_performance = embodied_metrics.get('embodied_performance', 0.0)
            
            if embodied_performance > self.config.embodied_skill_threshold:
                analysis['motor_skill_improved'] = True
            
            # Analyze sensorimotor adaptation
            joint_coordination = embodied_metrics.get('joint_coordination', 0.0)
            muscle_coordination = embodied_metrics.get('muscle_coordination', 0.0)
            
            if joint_coordination > 0.7 and muscle_coordination > 0.7:
                analysis['sensorimotor_adapted'] = True
            
            # Calculate learning rate
            if len(self.sensorimotor_curriculum_events) > 1:
                recent_events = [e for e in self.sensorimotor_curriculum_events[-5:] if e['skill_id'] == skill_id]
                if len(recent_events) >= 2:
                    performances = [e.get('embodied_metrics', {}).get('embodied_performance', 0.0) 
                                 for e in recent_events]
                    if len(performances) > 1:
                        analysis['embodied_learning_rate'] = np.polyfit(
                            range(len(performances)), performances, 1
                        )[0]
            
            # Recommend adaptation
            if analysis['embodied_learning_rate'] > 0.1:
                analysis['adaptation_recommendation'] = 'increase_difficulty'
            elif analysis['embodied_learning_rate'] < -0.05:
                analysis['adaptation_recommendation'] = 'decrease_difficulty'
            elif embodied_performance < 0.4:
                analysis['adaptation_recommendation'] = 'provide_guidance'
            
        except Exception as e:
            logger.warning(f"Failed to analyze embodied progression: {e}")
            analysis['error'] = str(e)
        
        return analysis
    
    def get_recommended_embodied_skills(self, current_capabilities: Optional[Dict[str, float]] = None) -> List[str]:
        """Get recommended embodied skills based on current capabilities"""
        
        if not self.curriculum_system:
            return []
        
        # Get general curriculum recommendations
        recommended_skills = self.curriculum_system.get_recommended_skills()
        
        # Filter for embodied skills
        embodied_recommendations = [
            skill_id for skill_id in recommended_skills
            if skill_id in self.embodied_skill_mapping
        ]
        
        # Prioritize based on embodied learning considerations
        if current_capabilities:
            embodied_recommendations.sort(
                key=lambda x: self._calculate_embodied_skill_priority(x, current_capabilities),
                reverse=True
            )
        
        return embodied_recommendations
    
    def _calculate_embodied_skill_priority(
        self, 
        skill_id: str, 
        capabilities: Dict[str, float]
    ) -> float:
        """Calculate priority for embodied skill based on current capabilities"""
        
        base_priority = 0.5
        
        try:
            embodied_category = self.embodied_skill_mapping.get(skill_id, 'general')
            
            # Higher priority for skills in areas where capability is moderate
            # (not too low to be impossible, not too high to be redundant)
            if embodied_category in capabilities:
                capability_level = capabilities[embodied_category]
                # Optimal range is 0.3-0.7 for next skill development
                if 0.3 <= capability_level <= 0.7:
                    base_priority += 0.4
                elif capability_level < 0.3:
                    base_priority += 0.2  # Still valuable but challenging
                else:
                    base_priority -= 0.1  # Already strong in this area
            
            # Consider prerequisites satisfaction
            if self.curriculum_system and skill_id in self.curriculum_system.skills_catalog:
                skill = self.curriculum_system.skills_catalog[skill_id]
                prereq_satisfaction = sum(
                    1 for prereq in skill.prerequisites
                    if self.curriculum_system.learning_progress[prereq].mastery_achieved
                ) / max(1, len(skill.prerequisites))
                
                base_priority += 0.3 * prereq_satisfaction
            
        except Exception as e:
            logger.warning(f"Failed to calculate embodied skill priority: {e}")
        
        return base_priority
    
    def get_integration_status(self) -> Dict[str, Any]:
        """Get comprehensive status of embodied-curriculum integration"""
        
        status = {
            'integration_available': CURRICULUM_AVAILABLE and EMBODIED_LEARNING_AVAILABLE,
            'curriculum_system_connected': self.curriculum_system is not None,
            'skill_tracker_connected': self.skill_tracker is not None,
            'motor_learner_connected': self.motor_learner is not None,
            'embodied_skill_mappings': len(self.embodied_skill_mapping),
            'integration_metrics': self.integration_metrics.copy(),
            'recent_events': len(self.sensorimotor_curriculum_events[-10:]) if self.sensorimotor_curriculum_events else 0
        }
        
        # Add curriculum status if available
        if self.curriculum_system and hasattr(self.curriculum_system, 'get_curriculum_status'):
            status['curriculum_status'] = self.curriculum_system.get_curriculum_status()
        
        return status
    
    def export_embodied_curriculum_data(self) -> Dict[str, Any]:
        """Export embodied curriculum integration data"""
        
        export_data = {
            'embodied_skill_mapping': self.embodied_skill_mapping,
            'motor_progression_history': self.motor_progression_history[-50:],  # Last 50 events
            'sensorimotor_curriculum_events': self.sensorimotor_curriculum_events[-100:],  # Last 100 events
            'integration_metrics': self.integration_metrics,
            'config': {
                'enable_motor_skill_progression': self.config.enable_motor_skill_progression,
                'enable_sensorimotor_adaptation': self.config.enable_sensorimotor_adaptation,
                'motor_learning_weight': self.config.motor_learning_weight,
                'embodied_skill_threshold': self.config.embodied_skill_threshold
            },
            'export_timestamp': time.time()
        }
        
        # Add curriculum data if available
        if self.curriculum_system and hasattr(self.curriculum_system, 'export_curriculum_data'):
            export_data['curriculum_data'] = self.curriculum_system.export_curriculum_data()
        
        return export_data


def create_embodied_curriculum_integration(
    curriculum_system: Optional[Any] = None,
    skill_tracker: Optional[Any] = None,
    motor_learner: Optional[Any] = None
) -> EmbodiedCurriculumIntegration:
    """Create an embodied curriculum learning integration system"""
    
    integration = EmbodiedCurriculumIntegration(
        curriculum_system=curriculum_system,
        skill_tracker=skill_tracker,
        motor_learner=motor_learner
    )
    
    logger.info("Embodied curriculum integration system created")
    return integration