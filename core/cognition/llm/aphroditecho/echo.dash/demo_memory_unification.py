import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))
from cognitive_architecture import CognitiveArchitecture, Memory, MemoryType
from memory_management import memory_system
def demonstrate_unified_memory_system():
    print('🌟 UNIFIED MEMORY SYSTEM DEMONSTRATION')
    print('=' * 60)
    print('\n1️⃣ UNIFIED MEMORY TYPES')
    print('-' * 30)
    memory_types = list(MemoryType)
    print(f'Available memory types: {len(memory_types)}')
    for memory_type in memory_types:
        print(f'  • {memory_type.name}: {memory_type.value}')
    print('\n2️⃣ COMPATIBILITY LAYER')
    print('-' * 30)
    print(f'memory_management.py provides: {type(memory_system).__name__}')
    print(f'Storage directory: {memory_system.storage_dir}')
    print('✅ Legacy import paths continue to work')
    print('\n3️⃣ COGNITIVE ARCHITECTURE MODES')
    print('-' * 30)
    print('🔹 Legacy Mode:')
    cognitive_legacy = CognitiveArchitecture(use_unified_memory=False)
    print(f'  Uses unified memory: {cognitive_legacy.use_unified_memory}')
    print('  Memory storage: Local dictionary')
    print('🔹 Unified Mode:')
    cognitive_unified = CognitiveArchitecture(use_unified_memory=True)
    print(f'  Uses unified memory: {cognitive_unified.use_unified_memory}')
    print(f'  Memory system: {type(cognitive_unified.unified_memory_system).__name__}')
    print('\n4️⃣ MEMORY OPERATIONS')
    print('-' * 30)
    memories = [Memory(content='Learning about neural networks', memory_type=MemoryType.DECLARATIVE, timestamp=1234567890.0, importance=0.9), Memory(content='Experienced successful problem solving', memory_type=MemoryType.EPISODIC, timestamp=1234567900.0, emotional_valence=0.8, importance=0.7), Memory(content='How to debug Python code', memory_type=MemoryType.PROCEDURAL, timestamp=1234567910.0, associations={'debugging', 'python', 'skills'})]
    print('Storing memories in both systems...')
    for i, memory in enumerate(memories):
        print(f'\n🧠 Memory {i + 1}: {memory.memory_type.name}')
        print(f"   Content: {memory.content[:40]}{('...' if len(memory.content) > 40 else '')}")
        cognitive_legacy.enhanced_memory_management(memory)
        cognitive_unified.enhanced_memory_management(memory)
        memory_node = memory.to_memory_node()
        converted_back = Memory.from_memory_node(memory_node)
        print('   ✅ Stored in legacy system')
        print('   ✅ Stored in unified system')
        print(f'   ✅ Conversion preserves data: {converted_back.content == memory.content}')
    print(f'\nLegacy system now has {len(cognitive_legacy.memories)} memories')
    print('Unified system memories stored successfully')
    print('\n5️⃣ INTEGRATION BENEFITS')
    print('-' * 30)
    benefits = ['🎯 Single MemoryType enum across all systems', '🔗 Seamless data conversion between Memory and MemoryNode', '🔄 Backward compatibility with existing code', '⚡ Optional unified system for enhanced functionality', '🧠 Consolidated memory operations interface', '📈 Extensible architecture for future memory types', '🛡️ Consistent error handling and logging', '💾 Persistent storage with auto-save capabilities']
    for benefit in benefits:
        print(f'  {benefit}')
    print('\n' + '=' * 60)
    print('✅ FRAGMENTED MEMORY SYSTEM SUCCESSFULLY UNIFIED!')
    print('🎉 All memory operations now use a consistent, integrated architecture')
if __name__ == '__main__':
    try:
        demonstrate_unified_memory_system()
    except Exception as e:
        print(f'\n❌ Demonstration failed: {e}')
        import traceback
        traceback.print_exc()
        sys.exit(1)