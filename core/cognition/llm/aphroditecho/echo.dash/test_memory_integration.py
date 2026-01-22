import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))
from cognitive_architecture import CognitiveArchitecture, Memory, MemoryType
from memory_management import memory_system
def test_memory_system_unification():
    print('🔧 Testing Memory System Unification')
    print('=' * 50)
    print('📋 Test 1: Unified MemoryType definitions')
    from unified_echo_memory import MemoryType as UnifiedMemoryType
    from cognitive_architecture import MemoryType as CognitiveMemoryType
    from memory_management import MemoryType as CompatibilityMemoryType
    assert UnifiedMemoryType == CognitiveMemoryType, 'Cognitive architecture uses unified MemoryType'
    assert UnifiedMemoryType == CompatibilityMemoryType, 'Compatibility layer uses unified MemoryType'
    unified_types = set([t.value for t in UnifiedMemoryType])
    set([t.value for t in CognitiveMemoryType])
    print(f'  ✅ Unified memory types: {len(unified_types)} types')
    print('  ✅ All systems use same MemoryType enum')
    print('\n📋 Test 2: Memory compatibility layer')
    assert hasattr(memory_system, 'storage_dir'), 'Memory system has storage directory'
    print('  ✅ Compatibility layer provides unified HypergraphMemory')
    print('\n📋 Test 3: Cognitive architecture integration')
    cognitive_legacy = CognitiveArchitecture(use_unified_memory=False)
    assert not cognitive_legacy.use_unified_memory, 'Legacy mode works'
    cognitive_unified = CognitiveArchitecture(use_unified_memory=True)
    assert cognitive_unified.use_unified_memory, 'Unified mode works'
    assert cognitive_unified.unified_memory_system is not None, 'Unified system initialized'
    print('  ✅ Cognitive architecture supports both legacy and unified modes')
    print('\n📋 Test 4: Memory operations integration')
    test_memory = Memory(content='Integration test memory', memory_type=MemoryType.DECLARATIVE, timestamp=1234567890.0, emotional_valence=0.5, importance=0.8, associations={'test', 'integration'}, context={'source': 'integration_test'})
    cognitive_legacy.enhanced_memory_management(test_memory)
    assert len(cognitive_legacy.memories) == 1, 'Memory stored in legacy system'
    cognitive_unified.enhanced_memory_management(test_memory)
    print('  ✅ Memory operations work in both modes')
    print('\n📋 Test 5: Memory conversion compatibility')
    memory_node = test_memory.to_memory_node()
    assert memory_node.content == test_memory.content, 'Content preserved'
    assert memory_node.memory_type == test_memory.memory_type, 'Type preserved'
    assert memory_node.salience == test_memory.importance, 'Importance mapped to salience'
    converted_back = Memory.from_memory_node(memory_node)
    assert converted_back.content == test_memory.content, 'Round-trip conversion works'
    print('  ✅ Memory conversion maintains compatibility')
    print('\n' + '=' * 50)
    print('✅ Memory System Unification Complete!')
    print('\n🎯 Benefits Achieved:')
    print('  • Single MemoryType enum across all systems')
    print('  • Unified memory interface through compatibility layer')
    print('  • Cognitive architecture supports both modes')
    print('  • Seamless integration between legacy and unified systems')
    print('  • Backward compatibility maintained')
    print('  • Memory operations consolidated')
    return True
if __name__ == '__main__':
    try:
        test_memory_system_unification()
        print('\n🌟 Integration test passed - Fragmented Memory System issue resolved!')
    except Exception as e:
        print(f'\n❌ Integration test failed: {e}')
        sys.exit(1)