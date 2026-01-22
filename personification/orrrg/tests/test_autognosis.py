import pytest
import asyncio
from core import SelfOrganizingCore, SelfMonitor, HierarchicalSelfModeler, MetaCognitiveProcessor, AutognosisOrchestrator
@pytest.mark.asyncio
async def test_self_organizing_core_initialization():
    soc = SelfOrganizingCore()
    assert not soc._initialized
    await soc.initialize()
    assert soc._initialized
    assert soc.autognosis is not None
    await soc.shutdown()
    assert not soc._initialized
@pytest.mark.asyncio
async def test_self_monitor_observation():
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
    soc = SelfOrganizingCore()
    await soc.initialize()
    monitor = SelfMonitor()
    for _ in range(5):
        await monitor.observe_system(soc)
    patterns = monitor.detect_patterns()
    assert isinstance(patterns, list)
    await soc.shutdown()
@pytest.mark.asyncio
async def test_hierarchical_self_modeler():
    soc = SelfOrganizingCore()
    await soc.initialize()
    monitor = SelfMonitor()
    modeler = HierarchicalSelfModeler(max_levels=3)
    image_0 = await modeler.build_self_image(0, monitor, soc)
    assert image_0.level == 0
    assert image_0.confidence > 0
    assert image_0.image_id is not None
    image_1 = await modeler.build_self_image(1, monitor, soc)
    assert image_1.level == 1
    assert image_1.confidence > 0
    assert len(image_1.meta_reflections) > 0
    await soc.shutdown()
@pytest.mark.asyncio
async def test_meta_cognitive_processor():
    soc = SelfOrganizingCore()
    await soc.initialize()
    monitor = SelfMonitor()
    modeler = HierarchicalSelfModeler()
    processor = MetaCognitiveProcessor()
    self_image = await modeler.build_self_image(0, monitor, soc)
    insights = await processor.process_self_image(self_image)
    assert isinstance(insights, list)
    assessment = processor.get_self_awareness_assessment(self_image)
    assert 'overall_score' in assessment
    assert 0 <= assessment['overall_score'] <= 1
    await soc.shutdown()
@pytest.mark.asyncio
async def test_autognosis_orchestrator():
    soc = SelfOrganizingCore()
    await soc.initialize()
    orchestrator = AutognosisOrchestrator(max_levels=3)
    result = await orchestrator.run_autognosis_cycle(soc)
    assert result['cycle_number'] == 1
    assert 'self_images' in result
    assert 'insights' in result
    assert 'optimization_opportunities' in result
    assert len(result['self_images']) == 3
    status = orchestrator.get_status()
    assert status['cycle_count'] == 1
    assert status['max_levels'] == 3
    await soc.shutdown()
@pytest.mark.asyncio
async def test_autognosis_full_cycle():
    soc = SelfOrganizingCore(autognosis_levels=3)
    await soc.initialize()
    result = await soc.run_autognosis_cycle()
    assert result is not None
    assert len(result['self_images']) == 3
    status = soc.get_autognosis_status()
    assert status['running'] == True
    assert status['max_levels'] == 3
    assert status['cycle_count'] >= 1
    await soc.shutdown()
@pytest.mark.asyncio
async def test_self_image_properties():
    soc = SelfOrganizingCore()
    await soc.initialize()
    monitor = SelfMonitor()
    modeler = HierarchicalSelfModeler()
    image = await modeler.build_self_image(0, monitor, soc)
    image_id = image.image_id
    assert isinstance(image_id, str)
    assert len(image_id) == 16
    await asyncio.sleep(0.01)
    image2 = await modeler.build_self_image(0, monitor, soc)
    assert image.image_id != image2.image_id
    await soc.shutdown()