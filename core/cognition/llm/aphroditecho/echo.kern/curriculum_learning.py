#!/usr/bin/env python3
"""
Curriculum Learning Implementation for Deep Tree Echo Architecture
Implements Task 4.2.2: Implement Curriculum Learning

This module provides adaptive difficulty progression, skill-based learning stages,
and performance-driven curriculum advancement that integrates with the existing
DTESN cognitive architecture and embodied learning systems.
"""

import time
import logging
import numpy as np
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Any, Callable
from enum import Enum
from collections import deque, defaultdict

logger = logging.getLogger(__name__)


class DifficultyLevel(Enum):
    """Curriculum difficulty levels"""
    BEGINNER = "beginner"
    NOVICE = "novice"
    INTERMEDIATE = "intermediate"
    ADVANCED = "advanced"
    EXPERT = "expert"


class LearningStage(Enum):
    """Learning stage categories"""
    FOUNDATION = "foundation"
    SKILL_BUILDING = "skill_building"
    INTEGRATION = "integration"
    MASTERY = "mastery"
    ADAPTATION = "adaptation"


@dataclass
class SkillObjective:
    """Defines a skill objective within the curriculum"""
    skill_id: str
    name: str
    description: str
    difficulty_level: DifficultyLevel
    stage: LearningStage
    prerequisites: List[str] = field(default_factory=list)
    performance_threshold: float = 0.8
    practice_sessions_required: int = 10
    mastery_metrics: Dict[str, float] = field(default_factory=dict)
    estimated_duration: float = 60.0  # seconds
    
    def __post_init__(self):
        if not self.mastery_metrics:
            self.mastery_metrics = {
                'accuracy': self.performance_threshold,
                'consistency': 0.75,
                'efficiency': 0.6
            }


@dataclass
class LearningProgress:
    """Tracks learning progress for a specific skill"""
    skill_id: str
    current_difficulty: DifficultyLevel
    sessions_completed: int = 0
    total_practice_time: float = 0.0
    success_rate: float = 0.0
    performance_history: List[float] = field(default_factory=list)
    last_session_time: float = field(default_factory=time.time)
    mastery_achieved: bool = False
    plateau_detection: Dict[str, Any] = field(default_factory=dict)
    
    def __post_init__(self):
        if not self.plateau_detection:
            self.plateau_detection = {
                'stagnation_threshold': 5,
                'current_stagnation': 0,
                'last_improvement': time.time()
            }


@dataclass
class CurriculumConfig:
    """Configuration for curriculum learning system"""
    adaptation_rate: float = 0.1
    performance_window: int = 10
    difficulty_adjustment_threshold: float = 0.05
    plateau_threshold: int = 5
    mastery_threshold: float = 0.85
    enable_dynamic_scheduling: bool = True
    enable_prerequisite_enforcement: bool = True
    max_concurrent_skills: int = 3
    session_timeout: float = 300.0  # seconds


class CurriculumLearningSystem:
    """
    Main curriculum learning system that provides adaptive difficulty progression,
    skill-based learning stages, and performance-driven advancement.
    
    Integrates with DTESN cognitive architecture and embodied learning systems.
    """
    
    def __init__(self, config: Optional[CurriculumConfig] = None):
        self.config = config or CurriculumConfig()
        
        # Core curriculum state
        self.skills_catalog: Dict[str, SkillObjective] = {}
        self.learning_progress: Dict[str, LearningProgress] = {}
        self.curriculum_graph: Dict[str, List[str]] = {}  # skill dependencies
        
        # Performance tracking
        self.performance_buffer = deque(maxlen=self.config.performance_window)
        self.adaptation_history: List[Dict[str, Any]] = []
        
        # Dynamic scheduling
        self.active_skills: List[str] = []
        self.recommended_skills: List[str] = []
        self.blocked_skills: List[str] = []
        
        # Integration points
        self.performance_callbacks: List[Callable] = []
        self.dtesn_integration = None
        self.embodied_learning_integration = None
        
        logger.info("Curriculum Learning System initialized")
    
    def register_skill(self, skill: SkillObjective) -> bool:
        """Register a new skill objective in the curriculum"""
        try:
            self.skills_catalog[skill.skill_id] = skill
            self.learning_progress[skill.skill_id] = LearningProgress(
                skill_id=skill.skill_id,
                current_difficulty=skill.difficulty_level
            )
            
            # Update curriculum graph
            self.curriculum_graph[skill.skill_id] = skill.prerequisites.copy()
            
            logger.info(f"Registered skill: {skill.name} ({skill.skill_id})")
            return True
            
        except Exception as e:
            logger.error(f"Failed to register skill {skill.skill_id}: {e}")
            return False
    
    def get_recommended_skills(self, agent_id: Optional[str] = None) -> List[str]:
        """Get list of recommended skills based on current progress and prerequisites"""
        available_skills = []
        
        for skill_id, skill in self.skills_catalog.items():
            progress = self.learning_progress[skill_id]
            
            # Skip if already mastered
            if progress.mastery_achieved:
                continue
                
            # Check prerequisites
            if self.config.enable_prerequisite_enforcement:
                prerequisites_met = all(
                    self.learning_progress[prereq].mastery_achieved 
                    for prereq in skill.prerequisites
                    if prereq in self.learning_progress
                )
                if not prerequisites_met:
                    continue
            
            available_skills.append(skill_id)
        
        # Sort by difficulty and priority
        available_skills.sort(key=lambda x: (
            self.skills_catalog[x].difficulty_level.value,
            -self.learning_progress[x].success_rate
        ))
        
        # Limit concurrent skills
        max_skills = min(self.config.max_concurrent_skills, len(available_skills))
        self.recommended_skills = available_skills[:max_skills]
        
        return self.recommended_skills
    
    def update_skill_progress(
        self, 
        skill_id: str, 
        performance_score: float,
        session_duration: float,
        additional_metrics: Optional[Dict[str, float]] = None
    ) -> Dict[str, Any]:
        """Update progress for a specific skill based on practice session results"""
        
        if skill_id not in self.learning_progress:
            logger.warning(f"Skill {skill_id} not found in learning progress")
            return {'success': False, 'reason': 'skill_not_found'}
        
        progress = self.learning_progress[skill_id]
        skill = self.skills_catalog[skill_id]
        
        try:
            # Update basic metrics
            progress.sessions_completed += 1
            progress.total_practice_time += session_duration
            progress.performance_history.append(performance_score)
            progress.last_session_time = time.time()
            
            # Calculate rolling success rate
            recent_window = min(self.config.performance_window, len(progress.performance_history))
            recent_scores = progress.performance_history[-recent_window:]
            progress.success_rate = np.mean(recent_scores)
            
            # Check for mastery
            mastery_check = self._check_mastery(skill_id, additional_metrics)
            if mastery_check['achieved']:
                progress.mastery_achieved = True
                logger.info(f"Mastery achieved for skill: {skill.name}")
            
            # Detect plateaus and adjust difficulty
            plateau_info = self._detect_plateau(skill_id)
            adaptation_result = self._adapt_difficulty(skill_id, plateau_info)
            
            # Update performance buffer for global adaptation
            self.performance_buffer.append({
                'skill_id': skill_id,
                'performance': performance_score,
                'timestamp': time.time(),
                'difficulty': progress.current_difficulty.value
            })
            
            # Log adaptation event
            adaptation_event = {
                'timestamp': time.time(),
                'skill_id': skill_id,
                'session_count': progress.sessions_completed,
                'performance_score': performance_score,
                'success_rate': progress.success_rate,
                'difficulty_adjusted': adaptation_result['adjusted'],
                'mastery_achieved': mastery_check['achieved'],
                'plateau_detected': plateau_info['detected']
            }
            self.adaptation_history.append(adaptation_event)
            
            return {
                'success': True,
                'progress': {
                    'sessions_completed': progress.sessions_completed,
                    'success_rate': progress.success_rate,
                    'current_difficulty': progress.current_difficulty.value,
                    'mastery_achieved': progress.mastery_achieved
                },
                'adaptation': adaptation_result,
                'mastery_check': mastery_check,
                'plateau_info': plateau_info
            }
            
        except Exception as e:
            logger.error(f"Failed to update skill progress for {skill_id}: {e}")
            return {'success': False, 'reason': str(e)}
    
    def _check_mastery(
        self, 
        skill_id: str, 
        additional_metrics: Optional[Dict[str, float]] = None
    ) -> Dict[str, Any]:
        """Check if mastery has been achieved for a skill"""
        
        progress = self.learning_progress[skill_id]
        skill = self.skills_catalog[skill_id]
        
        # Basic requirements
        session_requirement = progress.sessions_completed >= skill.practice_sessions_required
        performance_requirement = progress.success_rate >= skill.performance_threshold
        
        mastery_met = session_requirement and performance_requirement
        
        # Check additional mastery metrics if provided
        additional_checks = {}
        if additional_metrics:
            for metric, target_value in skill.mastery_metrics.items():
                if metric in additional_metrics:
                    additional_checks[metric] = additional_metrics[metric] >= target_value
                    mastery_met = mastery_met and additional_checks[metric]
        
        return {
            'achieved': mastery_met,
            'session_requirement_met': session_requirement,
            'performance_requirement_met': performance_requirement,
            'additional_checks': additional_checks,
            'required_sessions': skill.practice_sessions_required,
            'current_sessions': progress.sessions_completed,
            'required_performance': skill.performance_threshold,
            'current_performance': progress.success_rate
        }
    
    def _detect_plateau(self, skill_id: str) -> Dict[str, Any]:
        """Detect learning plateaus for adaptive difficulty adjustment"""
        
        progress = self.learning_progress[skill_id]
        
        if len(progress.performance_history) < 5:
            return {'detected': False, 'reason': 'insufficient_data'}
        
        # Analyze recent performance trend
        recent_scores = progress.performance_history[-5:]
        performance_variance = np.var(recent_scores)
        performance_trend = np.polyfit(range(len(recent_scores)), recent_scores, 1)[0]
        
        # Check for stagnation
        stagnation_detected = (
            performance_variance < 0.01 and  # Low variance
            abs(performance_trend) < 0.02    # Minimal trend
        )
        
        if stagnation_detected:
            progress.plateau_detection['current_stagnation'] += 1
        else:
            progress.plateau_detection['current_stagnation'] = 0
            progress.plateau_detection['last_improvement'] = time.time()
        
        plateau_detected = (
            progress.plateau_detection['current_stagnation'] >= 
            progress.plateau_detection['stagnation_threshold']
        )
        
        return {
            'detected': plateau_detected,
            'stagnation_count': progress.plateau_detection['current_stagnation'],
            'performance_variance': performance_variance,
            'performance_trend': performance_trend,
            'time_since_improvement': time.time() - progress.plateau_detection['last_improvement']
        }
    
    def _adapt_difficulty(self, skill_id: str, plateau_info: Dict[str, Any]) -> Dict[str, Any]:
        """Adapt difficulty level based on performance and plateau detection"""
        
        progress = self.learning_progress[skill_id]
        current_difficulty = progress.current_difficulty
        
        # Determine if adjustment is needed
        adjustment_needed = False
        new_difficulty = current_difficulty
        adjustment_reason = "no_adjustment"
        
        # Performance-based adjustment
        if progress.success_rate > self.config.mastery_threshold:
            # Increase difficulty if performance is too high
            difficulty_levels = list(DifficultyLevel)
            current_index = difficulty_levels.index(current_difficulty)
            if current_index < len(difficulty_levels) - 1:
                new_difficulty = difficulty_levels[current_index + 1]
                adjustment_needed = True
                adjustment_reason = "performance_too_high"
        
        elif progress.success_rate < 0.5:
            # Decrease difficulty if performance is too low
            difficulty_levels = list(DifficultyLevel)
            current_index = difficulty_levels.index(current_difficulty)
            if current_index > 0:
                new_difficulty = difficulty_levels[current_index - 1]
                adjustment_needed = True
                adjustment_reason = "performance_too_low"
        
        # Plateau-based adjustment
        if plateau_info['detected'] and not adjustment_needed:
            # Shake things up with a difficulty change
            difficulty_levels = list(DifficultyLevel)
            current_index = difficulty_levels.index(current_difficulty)
            
            # Randomly increase or decrease difficulty to break plateau
            if np.random.random() < 0.6 and current_index < len(difficulty_levels) - 1:
                new_difficulty = difficulty_levels[current_index + 1]
                adjustment_reason = "plateau_increase"
            elif current_index > 0:
                new_difficulty = difficulty_levels[current_index - 1]
                adjustment_reason = "plateau_decrease"
            
            adjustment_needed = (new_difficulty != current_difficulty)
        
        # Apply adjustment
        if adjustment_needed:
            progress.current_difficulty = new_difficulty
            progress.plateau_detection['current_stagnation'] = 0  # Reset plateau counter
            logger.info(f"Difficulty adjusted for {skill_id}: {current_difficulty.value} -> {new_difficulty.value}")
        
        return {
            'adjusted': adjustment_needed,
            'previous_difficulty': current_difficulty.value,
            'new_difficulty': new_difficulty.value,
            'reason': adjustment_reason
        }
    
    def get_curriculum_status(self) -> Dict[str, Any]:
        """Get comprehensive status of the curriculum learning system"""
        
        total_skills = len(self.skills_catalog)
        active_skills = len(self.active_skills)
        mastered_skills = sum(1 for p in self.learning_progress.values() if p.mastery_achieved)
        
        # Calculate overall progress
        if total_skills > 0:
            overall_progress = mastered_skills / total_skills
            average_success_rate = np.mean([p.success_rate for p in self.learning_progress.values()])
        else:
            overall_progress = 0.0
            average_success_rate = 0.0
        
        # Recent adaptation activity
        recent_adaptations = len([
            event for event in self.adaptation_history[-10:]
            if event.get('difficulty_adjusted', False)
        ])
        
        return {
            'system_status': {
                'total_skills': total_skills,
                'active_skills': active_skills,
                'mastered_skills': mastered_skills,
                'overall_progress': overall_progress,
                'average_success_rate': average_success_rate
            },
            'adaptation_metrics': {
                'total_adaptations': len(self.adaptation_history),
                'recent_adaptations': recent_adaptations,
                'adaptation_rate': self.config.adaptation_rate
            },
            'skill_distribution': self._get_skill_distribution(),
            'recommended_skills': self.recommended_skills,
            'configuration': {
                'adaptation_rate': self.config.adaptation_rate,
                'performance_window': self.config.performance_window,
                'mastery_threshold': self.config.mastery_threshold,
                'max_concurrent_skills': self.config.max_concurrent_skills
            }
        }
    
    def _get_skill_distribution(self) -> Dict[str, int]:
        """Get distribution of skills by difficulty level and stage"""
        
        difficulty_distribution = defaultdict(int)
        stage_distribution = defaultdict(int)
        
        for skill in self.skills_catalog.values():
            difficulty_distribution[skill.difficulty_level.value] += 1
            stage_distribution[skill.stage.value] += 1
        
        return {
            'by_difficulty': dict(difficulty_distribution),
            'by_stage': dict(stage_distribution)
        }
    
    def export_curriculum_data(self) -> Dict[str, Any]:
        """Export curriculum data for analysis and persistence"""
        
        return {
            'curriculum_config': {
                'adaptation_rate': self.config.adaptation_rate,
                'performance_window': self.config.performance_window,
                'mastery_threshold': self.config.mastery_threshold,
                'max_concurrent_skills': self.config.max_concurrent_skills
            },
            'skills_catalog': {
                skill_id: {
                    'name': skill.name,
                    'description': skill.description,
                    'difficulty_level': skill.difficulty_level.value,
                    'stage': skill.stage.value,
                    'prerequisites': skill.prerequisites,
                    'performance_threshold': skill.performance_threshold,
                    'practice_sessions_required': skill.practice_sessions_required
                }
                for skill_id, skill in self.skills_catalog.items()
            },
            'learning_progress': {
                skill_id: {
                    'current_difficulty': progress.current_difficulty.value,
                    'sessions_completed': progress.sessions_completed,
                    'total_practice_time': progress.total_practice_time,
                    'success_rate': progress.success_rate,
                    'mastery_achieved': progress.mastery_achieved,
                    'performance_history': progress.performance_history[-20:]  # Last 20 sessions
                }
                for skill_id, progress in self.learning_progress.items()
            },
            'adaptation_history': self.adaptation_history[-50:],  # Last 50 adaptations
            'export_timestamp': time.time()
        }
    
    # Integration methods for DTESN and embodied learning
    def integrate_dtesn_feedback(self, dtesn_metrics: Dict[str, Any]) -> None:
        """Integrate DTESN cognitive learning feedback"""
        # This method can be extended to incorporate DTESN-specific metrics
        # into curriculum adaptation decisions
        pass
    
    def integrate_embodied_learning(self, embodied_metrics: Dict[str, Any]) -> None:
        """Integrate embodied learning system feedback"""
        # This method can be extended to incorporate embodied learning metrics
        # into curriculum progression decisions
        pass


def create_default_curriculum() -> CurriculumLearningSystem:
    """Create a curriculum learning system with default configuration"""
    
    curriculum = CurriculumLearningSystem()
    
    # Register some default skills for basic cognitive and motor functions
    basic_skills = [
        SkillObjective(
            skill_id="basic_attention",
            name="Basic Attention Control",
            description="Learn to maintain focus on relevant stimuli",
            difficulty_level=DifficultyLevel.BEGINNER,
            stage=LearningStage.FOUNDATION,
            performance_threshold=0.7,
            practice_sessions_required=5
        ),
        SkillObjective(
            skill_id="pattern_recognition",
            name="Pattern Recognition",
            description="Identify patterns in sensory input",
            difficulty_level=DifficultyLevel.NOVICE,
            stage=LearningStage.SKILL_BUILDING,
            prerequisites=["basic_attention"],
            performance_threshold=0.75,
            practice_sessions_required=8
        ),
        SkillObjective(
            skill_id="motor_coordination",
            name="Motor Coordination",
            description="Basic motor control and coordination",
            difficulty_level=DifficultyLevel.BEGINNER,
            stage=LearningStage.FOUNDATION,
            performance_threshold=0.7,
            practice_sessions_required=6
        ),
        SkillObjective(
            skill_id="adaptive_control",
            name="Adaptive Motor Control",
            description="Adapt motor responses to changing conditions",
            difficulty_level=DifficultyLevel.INTERMEDIATE,
            stage=LearningStage.INTEGRATION,
            prerequisites=["motor_coordination", "pattern_recognition"],
            performance_threshold=0.8,
            practice_sessions_required=12
        )
    ]
    
    for skill in basic_skills:
        curriculum.register_skill(skill)
    
    logger.info("Default curriculum created with 4 basic skills")
    return curriculum