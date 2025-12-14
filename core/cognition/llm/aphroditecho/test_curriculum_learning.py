#!/usr/bin/env python3
"""
Test Suite for Curriculum Learning Implementation
Tests Task 4.2.2: Implement Curriculum Learning

This module provides comprehensive tests for the curriculum learning system,
including adaptive difficulty progression, skill-based learning stages,
and performance-driven curriculum advancement.
"""

import pytest
import time
import numpy as np
from unittest.mock import Mock

# Import curriculum learning components
import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

try:
    from echo.kern.curriculum_learning import (
        CurriculumLearningSystem,
        SkillObjective,
        DifficultyLevel,
        LearningStage,
        CurriculumConfig,
        create_default_curriculum
    )
    CURRICULUM_AVAILABLE = True
except ImportError:
    CURRICULUM_AVAILABLE = False

try:
    from echo.kern.dtesn_curriculum_integration import (
        DTESNCurriculumIntegration,
        DTESNCurriculumConfig,
        create_dtesn_curriculum_system
    )
    DTESN_CURRICULUM_AVAILABLE = True
except ImportError:
    DTESN_CURRICULUM_AVAILABLE = False

try:
    from echo.dash.embodied_curriculum_integration import (
        EmbodiedCurriculumIntegration,
        EmbodiedCurriculumConfig,
        create_embodied_curriculum_integration
    )
    EMBODIED_CURRICULUM_AVAILABLE = True
except ImportError:
    EMBODIED_CURRICULUM_AVAILABLE = False


class TestCurriculumLearningSystem:
    """Test suite for the core curriculum learning system"""
    
    @pytest.fixture
    def curriculum_config(self):
        """Create test curriculum configuration"""
        return CurriculumConfig(
            adaptation_rate=0.2,
            performance_window=5,
            difficulty_adjustment_threshold=0.1,
            mastery_threshold=0.8,
            max_concurrent_skills=2
        )
    
    @pytest.fixture
    def sample_skill(self):
        """Create a sample skill objective for testing"""
        return SkillObjective(
            skill_id="test_skill",
            name="Test Skill",
            description="A skill for testing curriculum learning",
            difficulty_level=DifficultyLevel.BEGINNER,
            stage=LearningStage.FOUNDATION,
            performance_threshold=0.75,
            practice_sessions_required=5
        )
    
    @pytest.fixture
    def curriculum_system(self, curriculum_config):
        """Create curriculum learning system for testing"""
        if not CURRICULUM_AVAILABLE:
            pytest.skip("Curriculum learning module not available")
        return CurriculumLearningSystem(curriculum_config)
    
    def test_curriculum_system_initialization(self, curriculum_system):
        """Test curriculum learning system initialization"""
        assert curriculum_system is not None
        assert curriculum_system.config.adaptation_rate == 0.2
        assert curriculum_system.config.max_concurrent_skills == 2
        assert len(curriculum_system.skills_catalog) == 0
        assert len(curriculum_system.learning_progress) == 0
    
    def test_skill_registration(self, curriculum_system, sample_skill):
        """Test skill registration in curriculum"""
        result = curriculum_system.register_skill(sample_skill)
        
        assert result is True
        assert sample_skill.skill_id in curriculum_system.skills_catalog
        assert sample_skill.skill_id in curriculum_system.learning_progress
        
        progress = curriculum_system.learning_progress[sample_skill.skill_id]
        assert progress.skill_id == sample_skill.skill_id
        assert progress.current_difficulty == DifficultyLevel.BEGINNER
        assert progress.sessions_completed == 0
        assert progress.mastery_achieved is False
    
    def test_skill_progress_update(self, curriculum_system, sample_skill):
        """Test skill progress updates"""
        curriculum_system.register_skill(sample_skill)
        
        # Simulate a successful practice session
        result = curriculum_system.update_skill_progress(
            skill_id=sample_skill.skill_id,
            performance_score=0.8,
            session_duration=30.0,
            additional_metrics={'accuracy': 0.85}
        )
        
        assert result['success'] is True
        assert result['progress']['sessions_completed'] == 1
        assert result['progress']['success_rate'] == 0.8
        
        progress = curriculum_system.learning_progress[sample_skill.skill_id]
        assert progress.sessions_completed == 1
        assert len(progress.performance_history) == 1
        assert progress.performance_history[0] == 0.8
    
    def test_mastery_detection(self, curriculum_system, sample_skill):
        """Test mastery achievement detection"""
        curriculum_system.register_skill(sample_skill)
        
        # Simulate multiple successful sessions to achieve mastery
        for i in range(sample_skill.practice_sessions_required):
            result = curriculum_system.update_skill_progress(
                skill_id=sample_skill.skill_id,
                performance_score=0.85,  # Above threshold
                session_duration=30.0
            )
        
        # Check final result for mastery
        final_result = result
        assert final_result['mastery_check']['achieved'] is True
        
        progress = curriculum_system.learning_progress[sample_skill.skill_id]
        assert progress.mastery_achieved is True
    
    def test_difficulty_adaptation(self, curriculum_system, sample_skill):
        """Test adaptive difficulty progression"""
        curriculum_system.register_skill(sample_skill)
        
        # Simulate high performance to trigger difficulty increase
        for i in range(3):
            curriculum_system.update_skill_progress(
                skill_id=sample_skill.skill_id,
                performance_score=0.95,  # Very high performance
                session_duration=30.0
            )
        
        progress = curriculum_system.learning_progress[sample_skill.skill_id]
        # Difficulty should have been increased due to high performance
        assert progress.current_difficulty != DifficultyLevel.BEGINNER
    
    def test_plateau_detection(self, curriculum_system, sample_skill):
        """Test learning plateau detection"""
        curriculum_system.register_skill(sample_skill)
        
        # Simulate plateau with consistent moderate performance
        for i in range(8):
            curriculum_system.update_skill_progress(
                skill_id=sample_skill.skill_id,
                performance_score=0.6 + 0.01 * (i % 2),  # Very small variation
                session_duration=30.0
            )
        
        progress = curriculum_system.learning_progress[sample_skill.skill_id]
        # Should detect stagnation
        assert progress.plateau_detection['current_stagnation'] > 0
    
    def test_prerequisite_enforcement(self, curriculum_system):
        """Test prerequisite enforcement in skill recommendations"""
        # Create skills with prerequisites
        basic_skill = SkillObjective(
            skill_id="basic",
            name="Basic Skill",
            description="Basic foundational skill",
            difficulty_level=DifficultyLevel.BEGINNER,
            stage=LearningStage.FOUNDATION,
            performance_threshold=0.7,
            practice_sessions_required=3
        )
        
        advanced_skill = SkillObjective(
            skill_id="advanced",
            name="Advanced Skill",
            description="Advanced skill requiring basic skill",
            difficulty_level=DifficultyLevel.INTERMEDIATE,
            stage=LearningStage.INTEGRATION,
            prerequisites=["basic"],
            performance_threshold=0.8,
            practice_sessions_required=5
        )
        
        curriculum_system.register_skill(basic_skill)
        curriculum_system.register_skill(advanced_skill)
        
        # Initially, only basic skill should be recommended
        recommendations = curriculum_system.get_recommended_skills()
        assert "basic" in recommendations
        assert "advanced" not in recommendations
        
        # After mastering basic skill, advanced should be recommended
        for i in range(basic_skill.practice_sessions_required):
            curriculum_system.update_skill_progress("basic", 0.8, 30.0)
        
        recommendations = curriculum_system.get_recommended_skills()
        assert "advanced" in recommendations
    
    def test_curriculum_status(self, curriculum_system, sample_skill):
        """Test curriculum status reporting"""
        curriculum_system.register_skill(sample_skill)
        
        status = curriculum_system.get_curriculum_status()
        
        assert 'system_status' in status
        assert 'adaptation_metrics' in status
        assert 'skill_distribution' in status
        assert 'configuration' in status
        
        system_status = status['system_status']
        assert system_status['total_skills'] == 1
        assert system_status['mastered_skills'] == 0
        assert system_status['overall_progress'] == 0.0
    
    def test_curriculum_export(self, curriculum_system, sample_skill):
        """Test curriculum data export"""
        curriculum_system.register_skill(sample_skill)
        curriculum_system.update_skill_progress(sample_skill.skill_id, 0.8, 30.0)
        
        export_data = curriculum_system.export_curriculum_data()
        
        assert 'curriculum_config' in export_data
        assert 'skills_catalog' in export_data
        assert 'learning_progress' in export_data
        assert 'adaptation_history' in export_data
        assert 'export_timestamp' in export_data
        
        # Check skill data is properly exported
        assert sample_skill.skill_id in export_data['skills_catalog']
        assert sample_skill.skill_id in export_data['learning_progress']
    
    def test_default_curriculum_creation(self):
        """Test creation of default curriculum"""
        if not CURRICULUM_AVAILABLE:
            pytest.skip("Curriculum learning module not available")
            
        curriculum = create_default_curriculum()
        
        assert curriculum is not None
        assert len(curriculum.skills_catalog) > 0
        assert len(curriculum.learning_progress) > 0
        
        # Check that default skills include basic cognitive functions
        skill_names = [skill.name for skill in curriculum.skills_catalog.values()]
        assert any("attention" in name.lower() for name in skill_names)
        assert any("pattern" in name.lower() for name in skill_names)


class TestDTESNCurriculumIntegration:
    """Test suite for DTESN-Curriculum integration"""
    
    @pytest.fixture
    def mock_dtesn_system(self):
        """Create mock DTESN system for testing"""
        mock_system = Mock()
        mock_system.config = Mock()
        mock_system.config.reservoir_size = 100
        mock_system.update = Mock(return_value={
            'new_reservoir_state': np.random.normal(0, 0.1, 100),
            'dynamics_metrics': {
                'state_change': 0.2,
                'activation_level': 0.5,
                'complexity_measure': 0.3
            }
        })
        return mock_system
    
    @pytest.fixture
    def curriculum_with_dtesn(self, mock_dtesn_system):
        """Create DTESN-curriculum integration for testing"""
        if not DTESN_CURRICULUM_AVAILABLE:
            pytest.skip("DTESN curriculum integration not available")
        
        curriculum = create_default_curriculum()
        integration = DTESNCurriculumIntegration(
            curriculum_system=curriculum,
            dtesn_system=mock_dtesn_system
        )
        return integration
    
    def test_dtesn_integration_initialization(self, curriculum_with_dtesn):
        """Test DTESN integration initialization"""
        assert curriculum_with_dtesn is not None
        assert curriculum_with_dtesn.curriculum_system is not None
        assert curriculum_with_dtesn.dtesn_system is not None
        assert len(curriculum_with_dtesn.cognitive_skill_mapping) > 0
        assert len(curriculum_with_dtesn.reservoir_skill_states) > 0
    
    def test_dtesn_enhanced_skill_update(self, curriculum_with_dtesn):
        """Test skill updates with DTESN enhancement"""
        skill_id = "basic_attention"  # From default curriculum
        
        result = curriculum_with_dtesn.update_skill_with_dtesn_feedback(
            skill_id=skill_id,
            performance_score=0.8,
            session_duration=45.0,
            sensory_input=np.random.normal(0, 0.1, 10),
            motor_output=np.random.normal(0, 0.1, 5)
        )
        
        assert result['success'] is True
        assert 'dtesn_enhancement' in result
        assert 'integration_metrics' in result
        
        dtesn_enhancement = result['dtesn_enhancement']
        assert 'dtesn_update_successful' in dtesn_enhancement
        assert 'cognitive_process' in dtesn_enhancement
        assert 'reservoir_dynamics' in dtesn_enhancement
    
    def test_cognitive_skill_mapping(self, curriculum_with_dtesn):
        """Test cognitive skill mapping functionality"""
        attention_skills = curriculum_with_dtesn.get_curriculum_skills_for_cognitive_process('attention')
        assert len(attention_skills) > 0
        
        # Check that mapped skills exist in curriculum
        for skill_id in attention_skills:
            assert skill_id in curriculum_with_dtesn.curriculum_system.skills_catalog
    
    def test_dtesn_curriculum_adaptation(self, curriculum_with_dtesn):
        """Test curriculum adaptation based on DTESN feedback"""
        # Simulate several skill updates to build history
        skill_id = "basic_attention"
        
        for i in range(5):
            curriculum_with_dtesn.update_skill_with_dtesn_feedback(
                skill_id=skill_id,
                performance_score=0.9,  # High performance
                session_duration=30.0
            )
        
        # Test adaptation based on accumulated feedback
        adaptation_result = curriculum_with_dtesn.adapt_curriculum_based_on_dtesn_feedback()
        
        assert 'adapted' in adaptation_result
        assert 'total_adaptations' in adaptation_result
        
        if adaptation_result['adapted']:
            assert 'adaptations' in adaptation_result
            assert len(adaptation_result['adaptations']) > 0
    
    def test_integration_status(self, curriculum_with_dtesn):
        """Test integration status reporting"""
        status = curriculum_with_dtesn.get_integration_status()
        
        assert 'curriculum_status' in status
        assert 'integration_status' in status
        assert 'config' in status
        
        integration_status = status['integration_status']
        assert 'dtesn_available' in integration_status
        assert 'cognitive_skill_mappings' in integration_status
        assert 'integration_metrics' in integration_status


class TestEmbodiedCurriculumIntegration:
    """Test suite for embodied curriculum integration"""
    
    @pytest.fixture
    def mock_sensorimotor_experience(self):
        """Create mock sensorimotor experience for testing"""
        mock_experience = Mock()
        mock_experience.success = True
        mock_experience.reward = 0.8
        mock_experience.duration = 45.0
        mock_experience.timestamp = time.time()
        
        # Mock body states
        mock_body_state = Mock()
        mock_body_state.position = (1.0, 2.0, 3.0)
        mock_body_state.joint_angles = {'joint1': 0.5, 'joint2': -0.3, 'joint3': 0.8}
        mock_body_state.muscle_activations = {'muscle1': 0.7, 'muscle2': 0.6}
        
        mock_experience.initial_body_state = mock_body_state
        mock_experience.resulting_body_state = mock_body_state
        
        # Mock motor action
        mock_motor_action = Mock()
        mock_motor_action.duration = 45.0
        mock_motor_action.force = 0.8
        mock_motor_action.precision = 0.9
        
        mock_experience.motor_action = mock_motor_action
        mock_experience.context = {'environment': 'test'}
        
        return mock_experience
    
    @pytest.fixture
    def embodied_integration(self):
        """Create embodied curriculum integration for testing"""
        if not EMBODIED_CURRICULUM_AVAILABLE:
            pytest.skip("Embodied curriculum integration not available")
        
        curriculum = create_default_curriculum()
        integration = create_embodied_curriculum_integration(curriculum_system=curriculum)
        return integration
    
    def test_embodied_integration_initialization(self, embodied_integration):
        """Test embodied integration initialization"""
        assert embodied_integration is not None
        assert embodied_integration.curriculum_system is not None
        assert len(embodied_integration.embodied_skill_mapping) > 0
    
    def test_embodied_skill_update(self, embodied_integration, mock_sensorimotor_experience):
        """Test embodied skill progress updates"""
        skill_id = "basic_movement"  # Should be registered during initialization
        
        result = embodied_integration.update_embodied_skill_progress(
            skill_id=skill_id,
            sensorimotor_experience=mock_sensorimotor_experience,
            additional_metrics={'spatial_accuracy': 0.85}
        )
        
        assert result['success'] is True
        assert 'embodied_metrics' in result
        assert 'progression_analysis' in result
        assert 'integration_metrics' in result
        
        embodied_metrics = result['embodied_metrics']
        assert 'task_success' in embodied_metrics
        assert 'reward_signal' in embodied_metrics
    
    def test_embodied_metrics_extraction(self, embodied_integration, mock_sensorimotor_experience):
        """Test extraction of embodied learning metrics"""
        metrics = embodied_integration._extract_embodied_metrics(mock_sensorimotor_experience)
        
        assert len(metrics) > 0
        assert 'task_success' in metrics
        assert 'reward_signal' in metrics
        assert 'embodied_performance' in metrics
        assert 'embodied_consistency' in metrics
        
        # Check that joint and muscle metrics are extracted
        if 'joint_coordination' in metrics:
            assert 0.0 <= metrics['joint_coordination'] <= 1.0
        if 'muscle_efficiency' in metrics:
            assert 0.0 <= metrics['muscle_efficiency'] <= 1.0
    
    def test_embodied_skill_recommendations(self, embodied_integration):
        """Test embodied skill recommendations"""
        recommendations = embodied_integration.get_recommended_embodied_skills()
        
        assert isinstance(recommendations, list)
        
        # Check that recommended skills are in embodied mapping
        for skill_id in recommendations:
            assert skill_id in embodied_integration.embodied_skill_mapping
    
    def test_embodied_progression_analysis(self, embodied_integration, mock_sensorimotor_experience):
        """Test embodied learning progression analysis"""
        skill_id = "basic_movement"
        
        # Create sample embodied metrics
        embodied_metrics = {
            'embodied_performance': 0.8,
            'joint_coordination': 0.75,
            'muscle_coordination': 0.8,
            'movement_precision': 0.9
        }
        
        analysis = embodied_integration._analyze_embodied_progression(
            skill_id, mock_sensorimotor_experience, embodied_metrics
        )
        
        assert 'motor_skill_improved' in analysis
        assert 'sensorimotor_adapted' in analysis
        assert 'embodied_learning_rate' in analysis
        assert 'adaptation_recommendation' in analysis
        
        # Check that high coordination scores lead to sensorimotor adaptation
        assert analysis['sensorimotor_adapted'] is True
    
    def test_integration_export(self, embodied_integration):
        """Test embodied integration data export"""
        export_data = embodied_integration.export_embodied_curriculum_data()
        
        assert 'embodied_skill_mapping' in export_data
        assert 'integration_metrics' in export_data
        assert 'config' in export_data
        assert 'export_timestamp' in export_data
        
        # Check configuration data
        config = export_data['config']
        assert 'enable_motor_skill_progression' in config
        assert 'embodied_skill_threshold' in config


class TestIntegratedCurriculumSystem:
    """Test suite for integrated curriculum learning system"""
    
    def test_full_system_integration(self):
        """Test full integrated curriculum learning system"""
        if not (CURRICULUM_AVAILABLE and DTESN_CURRICULUM_AVAILABLE):
            pytest.skip("Required modules not available for integration test")
        
        # Create integrated system
        curriculum = create_default_curriculum()
        mock_dtesn = Mock()
        mock_dtesn.config = Mock()
        mock_dtesn.config.reservoir_size = 100
        
        dtesn_integration = DTESNCurriculumIntegration(
            curriculum_system=curriculum,
            dtesn_system=mock_dtesn
        )
        
        # Test skill learning progression
        skill_id = "basic_attention"
        
        # Simulate learning progression
        for session in range(10):
            performance = 0.5 + 0.03 * session  # Gradual improvement
            result = dtesn_integration.update_skill_with_dtesn_feedback(
                skill_id=skill_id,
                performance_score=performance,
                session_duration=30.0
            )
            
            assert result['success'] is True
        
        # Check final system status
        status = dtesn_integration.get_integration_status()
        curriculum_status = status['curriculum_status']['system_status']
        
        assert curriculum_status['total_skills'] > 0
        assert dtesn_integration.integration_metrics['total_dtesn_updates'] == 10
    
    def test_performance_driven_advancement(self):
        """Test performance-driven curriculum advancement"""
        if not CURRICULUM_AVAILABLE:
            pytest.skip("Curriculum learning module not available")
        
        curriculum = create_default_curriculum()
        
        # Find a skill with prerequisites
        advanced_skill_id = None
        for skill_id, skill in curriculum.skills_catalog.items():
            if len(skill.prerequisites) > 0:
                advanced_skill_id = skill_id
                break
        
        if advanced_skill_id is None:
            pytest.skip("No skills with prerequisites found in default curriculum")
        
        skill = curriculum.skills_catalog[advanced_skill_id]
        
        # Master all prerequisites
        for prereq in skill.prerequisites:
            if prereq in curriculum.skills_catalog:
                prereq_skill = curriculum.skills_catalog[prereq]
                for _ in range(prereq_skill.practice_sessions_required):
                    curriculum.update_skill_progress(prereq, 0.9, 30.0)
        
        # Now the advanced skill should be recommended
        recommendations = curriculum.get_recommended_skills()
        assert advanced_skill_id in recommendations
    
    def test_adaptive_difficulty_progression(self):
        """Test adaptive difficulty progression across multiple skills"""
        if not CURRICULUM_AVAILABLE:
            pytest.skip("Curriculum learning module not available")
        
        curriculum = create_default_curriculum()
        
        # Test difficulty adaptation for each skill
        for skill_id in list(curriculum.skills_catalog.keys())[:3]:  # Test first 3 skills
            initial_difficulty = curriculum.learning_progress[skill_id].current_difficulty
            
            # Simulate consistently high performance
            for _ in range(5):
                curriculum.update_skill_progress(skill_id, 0.95, 30.0)
            
            final_difficulty = curriculum.learning_progress[skill_id].current_difficulty
            
            # Difficulty should have increased or remained the same (if already at max)
            difficulty_levels = list(DifficultyLevel)
            initial_index = difficulty_levels.index(initial_difficulty)
            final_index = difficulty_levels.index(final_difficulty)
            
            assert final_index >= initial_index


# Integration test helpers
def create_test_curriculum_with_skills() -> 'CurriculumLearningSystem':
    """Create a test curriculum with predefined skills for testing"""
    if not CURRICULUM_AVAILABLE:
        return None
    
    curriculum = CurriculumLearningSystem()
    
    # Create a set of interconnected skills for testing
    test_skills = [
        SkillObjective(
            skill_id="foundation_1",
            name="Foundation Skill 1",
            description="Basic foundational skill",
            difficulty_level=DifficultyLevel.BEGINNER,
            stage=LearningStage.FOUNDATION,
            performance_threshold=0.7,
            practice_sessions_required=3
        ),
        SkillObjective(
            skill_id="foundation_2",
            name="Foundation Skill 2",
            description="Another basic foundational skill",
            difficulty_level=DifficultyLevel.BEGINNER,
            stage=LearningStage.FOUNDATION,
            performance_threshold=0.7,
            practice_sessions_required=4
        ),
        SkillObjective(
            skill_id="intermediate_1",
            name="Intermediate Skill 1",
            description="Intermediate skill building on foundations",
            difficulty_level=DifficultyLevel.INTERMEDIATE,
            stage=LearningStage.SKILL_BUILDING,
            prerequisites=["foundation_1", "foundation_2"],
            performance_threshold=0.8,
            practice_sessions_required=6
        ),
        SkillObjective(
            skill_id="advanced_1",
            name="Advanced Skill 1",
            description="Advanced skill requiring intermediate skills",
            difficulty_level=DifficultyLevel.ADVANCED,
            stage=LearningStage.MASTERY,
            prerequisites=["intermediate_1"],
            performance_threshold=0.85,
            practice_sessions_required=8
        )
    ]
    
    for skill in test_skills:
        curriculum.register_skill(skill)
    
    return curriculum


if __name__ == "__main__":
    # Run tests when script is executed directly
    pytest.main([__file__, "-v"])