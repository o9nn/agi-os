"""Tests for autognosis core components."""

import pytest
import asyncio
from core import (
    SelfOrganizingCore,
    SelfMonitor,
    HierarchicalSelfModeler,
    MetaCognitiveProcessor,
    AutognosisOrchestrator,
)


@pytest.mark.asyncio
async def test_self_organizing_core_initialization():
    """Test SelfOrganizingCore initialization."""
    soc = SelfOrganizingCore()
    assert not soc._initialized
    
    await soc.initialize()
    assert soc._initialized
    assert soc.autognosis is not None
    
    await soc.shutdown()
    assert not soc._initialized


@pytest.mark.asyncio
async def test_self_monitor_observation():
    """Test SelfMonitor can observe system."""
    soc = SelfOrganizingCore()
    await soc.initialize()
    
    monitor = SelfMonitor()
    observation = await monitor.observe_system(soc)
    
    assert observation is not None
    assert len(observation.component_states) > 0
    assert len(monitor.observation_history) == 1
    
    await soc.shutdown()


@pytest.mark.asyncio
async def test_self_monitor_pattern_detection():
    """Test SelfMonitor pattern detection."""
    soc = SelfOrganizingCore()
    await soc.initialize()
    
    monitor = SelfMonitor()
    
    # Create multiple observations
    for _ in range(5):
        await monitor.observe_system(soc)
    
    patterns = monitor.detect_patterns()
    assert isinstance(patterns, list)
    # Should detect some patterns with 5 observations
    
    await soc.shutdown()


@pytest.mark.asyncio
async def test_hierarchical_self_modeler():
    """Test HierarchicalSelfModeler builds self-images."""
    soc = SelfOrganizingCore()
    await soc.initialize()
    
    monitor = SelfMonitor()
    modeler = HierarchicalSelfModeler(max_levels=3)
    
    # Build level 0
    image_0 = await modeler.build_self_image(0, monitor, soc)
    assert image_0.level == 0
    assert image_0.confidence > 0
    assert image_0.image_id is not None
    
    # Build level 1
    image_1 = await modeler.build_self_image(1, monitor, soc)
    assert image_1.level == 1
    assert image_1.confidence > 0
    assert len(image_1.meta_reflections) > 0
    
    await soc.shutdown()


@pytest.mark.asyncio
async def test_meta_cognitive_processor():
    """Test MetaCognitiveProcessor generates insights."""
    soc = SelfOrganizingCore()
    await soc.initialize()
    
    monitor = SelfMonitor()
    modeler = HierarchicalSelfModeler()
    processor = MetaCognitiveProcessor()
    
    # Build self-image
    self_image = await modeler.build_self_image(0, monitor, soc)
    
    # Process for insights
    insights = await processor.process_self_image(self_image)
    assert isinstance(insights, list)
    
    # Get self-awareness assessment
    assessment = processor.get_self_awareness_assessment(self_image)
    assert "overall_score" in assessment
    assert 0 <= assessment["overall_score"] <= 1
    
    await soc.shutdown()


@pytest.mark.asyncio
async def test_autognosis_orchestrator():
    """Test AutognosisOrchestrator coordinates system."""
    soc = SelfOrganizingCore()
    await soc.initialize()
    
    orchestrator = AutognosisOrchestrator(max_levels=3)
    
    # Run cycle
    result = await orchestrator.run_autognosis_cycle(soc)
    
    assert result["cycle_number"] == 1
    assert "self_images" in result
    assert "insights" in result
    assert "optimization_opportunities" in result
    assert len(result["self_images"]) == 3
    
    # Check status
    status = orchestrator.get_status()
    assert status["cycle_count"] == 1
    assert status["max_levels"] == 3
    
    await soc.shutdown()


@pytest.mark.asyncio
async def test_autognosis_full_cycle():
    """Test complete autognosis cycle through SelfOrganizingCore."""
    soc = SelfOrganizingCore(autognosis_levels=3)
    await soc.initialize()
    
    # Run cycle
    result = await soc.run_autognosis_cycle()
    
    assert result is not None
    assert len(result["self_images"]) == 3
    
    # Check status
    status = soc.get_autognosis_status()
    assert status["running"] == True
    assert status["max_levels"] == 3
    assert status["cycle_count"] >= 1
    
    await soc.shutdown()


@pytest.mark.asyncio
async def test_self_image_properties():
    """Test SelfImage properties and ID generation."""
    soc = SelfOrganizingCore()
    await soc.initialize()
    
    monitor = SelfMonitor()
    modeler = HierarchicalSelfModeler()
    
    image = await modeler.build_self_image(0, monitor, soc)
    
    # Test image_id generation
    image_id = image.image_id
    assert isinstance(image_id, str)
    assert len(image_id) == 16  # SHA256 truncated to 16 chars
    
    # Test that different images have different IDs
    await asyncio.sleep(0.01)  # Small delay to ensure different timestamp
    image2 = await modeler.build_self_image(0, monitor, soc)
    assert image.image_id != image2.image_id
    
    await soc.shutdown()
