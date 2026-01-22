import sys
import os
import asyncio
import tempfile
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))
def test_imports():
    print('🧪 Testing module imports...')
    try:
        from core.autonomous_core_v10 import AutonomousCoreV10, CognitiveState, EngineType
        print('  ✅ AutonomousCoreV10 imported successfully')
    except Exception as e:
        print(f'  ❌ Failed to import AutonomousCoreV10: {e}')
        return False
    try:
        from core.consciousness.stream_of_consciousness import StreamOfConsciousness
        print('  ✅ StreamOfConsciousness imported successfully')
    except Exception as e:
        print(f'  ❌ Failed to import StreamOfConsciousness: {e}')
        return False
    try:
        from core.memory.hypergraph_memory import HypergraphMemory
        print('  ✅ HypergraphMemory imported successfully')
    except Exception as e:
        print(f'  ❌ Failed to import HypergraphMemory: {e}')
        return False
    try:
        from core.echodream.dream_consolidation_enhanced import DreamConsolidationEngine
        print('  ✅ DreamConsolidationEngine imported successfully')
    except Exception as e:
        print(f'  ❌ Failed to import DreamConsolidationEngine: {e}')
        return False
    return True
def test_orchestrator():
    print('🧪 Testing ThreeEngineOrchestrator...')
    from core.autonomous_core_v10 import ThreeEngineOrchestrator, EngineType
    orchestrator = ThreeEngineOrchestrator()
    print('  ✅ Orchestrator initialized')
    step_engines = []
    for i in range(12):
        engine = orchestrator.get_active_engine()
        step_engines.append((i, engine))
        orchestrator.advance_step()
    expected = [(0, EngineType.COHERENCE_ENGINE), (1, EngineType.COHERENCE_ENGINE), (2, EngineType.MEMORY_ENGINE), (3, EngineType.MEMORY_ENGINE), (4, EngineType.MEMORY_ENGINE), (5, EngineType.MEMORY_ENGINE), (6, EngineType.MEMORY_ENGINE), (7, EngineType.COHERENCE_ENGINE), (8, EngineType.COHERENCE_ENGINE), (9, EngineType.IMAGINATION_ENGINE), (10, EngineType.IMAGINATION_ENGINE), (11, EngineType.IMAGINATION_ENGINE)]
    for (step, engine), (exp_step, exp_engine) in zip(step_engines, expected):
        if step != exp_step or engine != exp_engine:
            print(f'  ❌ Step {step} expected {exp_engine.name}, got {engine.name}')
            return False
    print('  ✅ All 12 steps correctly assigned to engines')
    print(f'    - Coherence: steps 0,1,7,8 (4 steps)')
    print(f'    - Memory: steps 2,3,4,5,6 (5 steps)')
    print(f'    - Imagination: steps 9,10,11 (3 steps)')
    if orchestrator.cycle_count != 1:
        print(f'  ❌ Expected cycle_count=1, got {orchestrator.cycle_count}')
        return False
    print('  ✅ Cycle counting works')
    return True
def test_energy_management():
    print('🧪 Testing EnergyState management...')
    from core.autonomous_core_v10 import EnergyState
    energy = EnergyState()
    print('  ✅ EnergyState initialized')
    initial_energy = energy.energy
    for i in range(10):
        energy.consume_energy(0.05)
    if energy.energy >= initial_energy:
        print(f'  ❌ Energy should decrease, got {energy.energy}')
        return False
    print(f'  ✅ Energy consumption works: {initial_energy:.2f} -> {energy.energy:.2f}')
    for i in range(30):
        energy.consume_energy(0.05)
    if not energy.needs_rest():
        print('  ❌ Should need rest after extended activity')
        return False
    print('  ✅ Rest detection works')
    for i in range(5):
        energy.restore_energy(0.2)
    if not energy.can_wake():
        print('  ❌ Should be able to wake after rest')
        return False
    print('  ✅ Energy restoration works')
    return True
async def test_autonomous_core():
    print('🧪 Testing AutonomousCoreV10...')
    from core.autonomous_core_v10 import AutonomousCoreV10, CognitiveState
    with tempfile.TemporaryDirectory() as tmpdir:
        os.environ['DATA_DIR'] = tmpdir
        core = AutonomousCoreV10()
        print('  ✅ AutonomousCoreV10 initialized')
        if core.state != CognitiveState.INITIALIZING:
            print(f'  ❌ Expected INITIALIZING state, got {core.state}')
            return False
        print('  ✅ Initial state correct')
        if not core.orchestrator:
            print('  ❌ Orchestrator not initialized')
            return False
        print('  ✅ Orchestrator initialized')
        if not core.goal_orchestrator:
            print('  ❌ Goal orchestrator not initialized')
            return False
        print('  ✅ Goal orchestrator initialized')
        goals = core.goal_orchestrator.get_active_goals()
        if not goals:
            print('  ❌ No default goal created')
            return False
        print(f"  ✅ Default goal created: {goals[0]['name']}")
        print('  🔄 Running 5 cognitive cycles...')
        core_task = asyncio.create_task(core.start())
        await asyncio.sleep(10)
        core.stop()
        try:
            await asyncio.wait_for(core_task, timeout=5)
        except asyncio.TimeoutError:
            print('  ⚠️  Core shutdown timeout')
        if core.thought_count < 3:
            print(f'  ⚠️  Expected at least 3 thoughts, got {core.thought_count}')
        else:
            print(f'  ✅ Generated {core.thought_count} thoughts')
        if core.cycle_count < 1:
            print(f'  ❌ Expected at least 1 cycle, got {core.cycle_count}')
            return False
        print(f'  ✅ Completed {core.cycle_count} cognitive cycles')
    return True
def test_goal_orchestrator():
    print('🧪 Testing GoalOrchestrator...')
    from core.autonomous_core_v10 import GoalOrchestrator
    with tempfile.NamedTemporaryFile(suffix='.db', delete=False) as tmp:
        db_path = tmp.name
    try:
        orchestrator = GoalOrchestrator(db_path=db_path)
        print('  ✅ GoalOrchestrator initialized')
        goal_id = orchestrator.add_goal(name='Test Goal', description='A test goal for validation', priority=5)
        print(f'  ✅ Goal created: {goal_id}')
        goals = orchestrator.get_active_goals()
        if not goals:
            print('  ❌ No goals found')
            return False
        print(f'  ✅ Retrieved {len(goals)} goals')
        orchestrator.update_goal_progress(goal_id, 0.5, 'Test progress')
        print('  ✅ Goal progress updated')
        goals = orchestrator.get_active_goals()
        if goals[0]['progress'] != 0.5:
            print(f"  ❌ Expected progress 0.5, got {goals[0]['progress']}")
            return False
        print('  ✅ Progress update verified')
    finally:
        Path(db_path).unlink(missing_ok=True)
    return True
def main():
    print('=' * 60)
    print('Echo9llama Iteration N+10 Test Suite')
    print('=' * 60)
    results = {}
    results['imports'] = test_imports()
    print()
    results['orchestrator'] = test_orchestrator()
    print()
    results['energy'] = test_energy_management()
    print()
    results['goal_orchestrator'] = test_goal_orchestrator()
    print()
    print('🧪 Running async tests...')
    results['autonomous_core'] = asyncio.run(test_autonomous_core())
    print()
    print('=' * 60)
    print('Test Summary')
    print('=' * 60)
    passed = sum((1 for v in results.values() if v))
    total = len(results)
    for test_name, result in results.items():
        status = '✅ PASS' if result else '❌ FAIL'
        print(f'{status} - {test_name}')
    print(f'\nTotal: {passed}/{total} tests passed')
    if passed == total:
        print('\n🎉 All tests passed! Iteration N+10 is successful!')
        return 0
    else:
        print(f'\n⚠️  {total - passed} test(s) failed')
        return 1
if __name__ == '__main__':
    sys.exit(main())