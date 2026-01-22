import sys
import os
import asyncio
import tempfile
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))
def test_imports():
    print('🧪 Testing module imports...')
    try:
        from core.memory.hypergraph_memory import HypergraphMemory, Concept, Relation
        print('  ✅ HypergraphMemory imported successfully')
    except Exception as e:
        print(f'  ❌ Failed to import HypergraphMemory: {e}')
        return False
    try:
        from core.consciousness.stream_of_consciousness import StreamOfConsciousness, ThoughtSource
        print('  ✅ StreamOfConsciousness imported successfully')
    except Exception as e:
        print(f'  ❌ Failed to import StreamOfConsciousness: {e}')
        return False
    try:
        from core.echodream.dream_consolidation_enhanced import DreamConsolidationEngine, Experience
        print('  ✅ DreamConsolidationEngine imported successfully')
    except Exception as e:
        print(f'  ❌ Failed to import DreamConsolidationEngine: {e}')
        return False
    return True
def test_hypergraph_memory():
    print('\n🧪 Testing HypergraphMemory...')
    try:
        from core.memory.hypergraph_memory import HypergraphMemory, Concept, Relation
        with tempfile.NamedTemporaryFile(suffix='.db', delete=False) as tmp:
            db_path = tmp.name
        memory = HypergraphMemory(db_path=db_path)
        print('  ✅ HypergraphMemory initialized')
        concept1 = Concept(id='test_concept_1', name='Test concept about wisdom', concept_type='declarative', properties={'domain': 'philosophy'})
        concept2 = Concept(id='test_concept_2', name='Critical thinking skill', concept_type='procedural', properties={'category': 'cognitive'})
        assert memory.add_concept(concept1), 'Failed to add concept 1'
        assert memory.add_concept(concept2), 'Failed to add concept 2'
        print('  ✅ Concepts added successfully')
        relation = Relation(source='test_concept_1', target='test_concept_2', relation_type='requires', strength=0.8)
        assert memory.add_relation(relation), 'Failed to add relation'
        print('  ✅ Relation added successfully')
        retrieved = memory.get_concept('test_concept_1')
        assert retrieved is not None, 'Failed to retrieve concept'
        assert retrieved.name == 'Test concept about wisdom', 'Retrieved concept has wrong name'
        print('  ✅ Concept retrieval works')
        stats = memory.get_memory_stats()
        assert stats['total_concepts'] >= 2, 'Stats show wrong concept count'
        assert stats['total_relations'] >= 1, 'Stats show wrong relation count'
        print('  ✅ Memory stats: {total_concepts} concepts, {total_relations} relations'.format(**stats))
        os.unlink(db_path)
        return True
    except Exception as e:
        print(f'  ❌ HypergraphMemory test failed: {e}')
        import traceback
        traceback.print_exc()
        return False
async def test_stream_of_consciousness():
    print('\n🧪 Testing StreamOfConsciousness...')
    try:
        from core.consciousness.stream_of_consciousness import StreamOfConsciousness, ThoughtSource
        stream = StreamOfConsciousness(llm_provider='none')
        print('  ✅ StreamOfConsciousness initialized')
        stream.wake()
        assert stream.is_awake, 'Failed to wake'
        print('  ✅ Wake function works')
        thought_count = 0
        async for thought in stream.thought_stream():
            print(f'    💭 [{thought.source.value}] {thought.content[:60]}...')
            thought_count += 1
            if thought_count >= 3:
                break
        assert thought_count == 3, f'Expected 3 thoughts, got {thought_count}'
        print(f'  ✅ Generated {thought_count} thoughts')
        stream.sleep()
        assert not stream.is_awake, 'Failed to sleep'
        print('  ✅ Sleep function works')
        stream.update_state(energy=0.5, curiosity=0.8)
        assert stream.energy == 0.5, 'Energy update failed'
        assert stream.curiosity == 0.8, 'Curiosity update failed'
        print('  ✅ State updates work')
        return True
    except Exception as e:
        print(f'  ❌ StreamOfConsciousness test failed: {e}')
        import traceback
        traceback.print_exc()
        return False
async def test_dream_consolidation():
    print('\n🧪 Testing DreamConsolidationEngine...')
    try:
        from core.echodream.dream_consolidation_enhanced import DreamConsolidationEngine, Experience
        from datetime import datetime
        with tempfile.NamedTemporaryFile(suffix='.db', delete=False) as tmp:
            db_path = tmp.name
        engine = DreamConsolidationEngine(db_path=db_path)
        print('  ✅ DreamConsolidationEngine initialized')
        now = int(datetime.now().timestamp() * 1000)
        for i in range(5):
            exp = Experience(timestamp=now + i * 1000, content=f'Test experience {i}: learning about patterns and wisdom', experience_type='thought', emotional_valence=0.5, importance=0.6 + i * 0.05)
            engine.accumulate_experience(exp)
        print('  ✅ Accumulated 5 experiences')
        insights = await engine.consolidate_experiences()
        assert len(insights) > 0, 'No insights generated'
        print(f'  ✅ Generated {len(insights)} insights')
        for insight in insights:
            print(f'    ✨ [{insight.insight_type}] {insight.insight[:60]}...')
        stats = engine.get_stats()
        assert stats['consolidated_experiences'] >= 5, 'Not all experiences consolidated'
        assert stats['total_insights'] >= len(insights), 'Insights not stored'
        print(f"  ✅ Stats: {stats['consolidated_experiences']} consolidated, {stats['total_insights']} total insights")
        os.unlink(db_path)
        return True
    except Exception as e:
        print(f'  ❌ DreamConsolidationEngine test failed: {e}')
        import traceback
        traceback.print_exc()
        return False
def test_echobridge_server():
    print('\n🧪 Testing EchoBridge server build...')
    try:
        server_path = Path(__file__).parent / 'bin' / 'echobridge_server'
        if server_path.exists():
            print(f'  ✅ EchoBridge server binary exists at {server_path}')
            if os.access(server_path, os.X_OK):
                print('  ✅ Server binary is executable')
            else:
                print('  ⚠️  Server binary is not executable')
            size_mb = server_path.stat().st_size / (1024 * 1024)
            print(f'  ✅ Server binary size: {size_mb:.1f} MB')
            return True
        else:
            print(f'  ❌ EchoBridge server binary not found at {server_path}')
            return False
    except Exception as e:
        print(f'  ❌ EchoBridge server test failed: {e}')
        return False
def test_requirements_file():
    print('\n🧪 Testing requirements.txt...')
    try:
        req_path = Path(__file__).parent / 'requirements.txt'
        if not req_path.exists():
            print('  ❌ requirements.txt not found')
            return False
        with open(req_path, 'r') as f:
            lines = f.readlines()
        requirements = [l.strip() for l in lines if l.strip() and (not l.strip().startswith('#'))]
        print(f'  ✅ requirements.txt exists with {len(requirements)} dependencies')
        key_deps = ['anthropic', 'grpcio', 'networkx', 'sentence-transformers']
        found_deps = []
        for dep in key_deps:
            if any((dep in req.lower() for req in requirements)):
                found_deps.append(dep)
        print(f"  ✅ Found {len(found_deps)}/{len(key_deps)} key dependencies: {', '.join(found_deps)}")
        return len(found_deps) >= 3
    except Exception as e:
        print(f'  ❌ requirements.txt test failed: {e}')
        return False
async def run_all_tests():
    print('=' * 60)
    print('Echo9llama Iteration N+9 Test Suite')
    print('=' * 60)
    results = {}
    results['imports'] = test_imports()
    results['hypergraph_memory'] = test_hypergraph_memory()
    results['stream_of_consciousness'] = await test_stream_of_consciousness()
    results['dream_consolidation'] = await test_dream_consolidation()
    results['echobridge_server'] = test_echobridge_server()
    results['requirements'] = test_requirements_file()
    print('\n' + '=' * 60)
    print('Test Summary')
    print('=' * 60)
    passed = sum((1 for v in results.values() if v))
    total = len(results)
    for test_name, result in results.items():
        status = '✅ PASS' if result else '❌ FAIL'
        print(f'{status} - {test_name}')
    print(f'\nTotal: {passed}/{total} tests passed')
    if passed == total:
        print('\n🎉 All tests passed!')
        return 0
    else:
        print(f'\n⚠️  {total - passed} test(s) failed')
        return 1
if __name__ == '__main__':
    exit_code = asyncio.run(run_all_tests())
    sys.exit(exit_code)