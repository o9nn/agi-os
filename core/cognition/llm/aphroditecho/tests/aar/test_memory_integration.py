from datetime import timedelta
from aphrodite.aar_core.memory import MemoryManager, MemoryType
class TestMemoryIntegration:
    def setup_method(self):
        self.memory_manager = MemoryManager()
    def test_memory_lifecycle(self):
        memory_id = self.memory_manager.add_memory(content='Test memory content', memory_type=MemoryType.WORKING, tags=['test', 'integration'], source_agent='test_agent', arena_id='test_arena')
        assert memory_id is not None
        memory = self.memory_manager.get_memory(memory_id)
        assert memory is not None
        assert memory.content == 'Test memory content'
        assert memory.type == MemoryType.WORKING
        assert 'test' in memory.tags
        assert memory.source_agent == 'test_agent'
        assert memory.arena_id == 'test_arena'
    def test_memory_querying(self):
        for i in range(5):
            self.memory_manager.add_memory(content=f'Memory {i} with tag_{i % 2}', memory_type=MemoryType.EPISODIC, tags=[f'tag_{i % 2}'], arena_id='test_arena')
        results = self.memory_manager.query_memory('Memory', limit=3)
        assert len(results) == 3
        results = self.memory_manager.query_memory('', tags=['tag_0'], limit=10)
        assert len(results) == 3
        results = self.memory_manager.query_memory('', arena_id='test_arena', limit=10)
        assert len(results) == 5
    def test_working_memory_buffer(self):
        for i in range(1100):
            self.memory_manager.add_memory(content=f'Working memory {i}', memory_type=MemoryType.WORKING)
        working_memories = self.memory_manager.get_working_memory()
        assert len(working_memories) == 1000
        assert working_memories[0].content == 'Working memory 100'
    def test_memory_expiration(self):
        memory_id = self.memory_manager.add_memory(content='Expiring memory', memory_type=MemoryType.EPISODIC, ttl=timedelta(milliseconds=1))
        import time
        time.sleep(0.01)
        memory = self.memory_manager.get_memory(memory_id)
        assert memory is None
        cleanup_count = self.memory_manager.cleanup_expired()
        assert cleanup_count >= 0
    def test_memory_stats(self):
        self.memory_manager.add_memory('Content 1', MemoryType.WORKING)
        self.memory_manager.add_memory('Content 2', MemoryType.EPISODIC)
        self.memory_manager.add_memory('Content 3', MemoryType.SEMANTIC)
        stats = self.memory_manager.get_memory_stats()
        assert stats['total_memories'] == 3
        assert stats['type_counts']['working'] == 1
        assert stats['type_counts']['episodic'] == 1
        assert stats['type_counts']['semantic'] == 1
        assert stats['working_buffer_size'] == 1