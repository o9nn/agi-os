from aphrodite.aar_core.arena import ArenaManager, ArenaState
class TestArenaIntegration:
    def setup_method(self):
        self.arena_manager = ArenaManager()
    def test_arena_lifecycle(self):
        arena = self.arena_manager.create_arena(name='Test Arena', description='Test arena for integration testing', metadata={'test': True})
        assert arena.id is not None
        assert arena.name == 'Test Arena'
        assert arena.state == ArenaState.CREATING
        success = self.arena_manager.activate_arena(arena.id)
        assert success
        assert arena.state == ArenaState.ACTIVE
        success = self.arena_manager.pause_arena(arena.id)
        assert success
        assert arena.state == ArenaState.PAUSED
        success = self.arena_manager.close_arena(arena.id)
        assert success
        assert arena.state == ArenaState.CLOSED
    def test_agent_management(self):
        arena = self.arena_manager.create_arena('Agent Test Arena')
        success = self.arena_manager.add_agent_to_arena(arena.id, 'agent_1')
        assert success
        assert 'agent_1' in arena.agents
        success = self.arena_manager.add_agent_to_arena(arena.id, 'agent_2')
        assert success
        assert 'agent_2' in arena.agents
        success = self.arena_manager.remove_agent_from_arena(arena.id, 'agent_1')
        assert success
        assert 'agent_1' not in arena.agents
        assert 'agent_2' in arena.agents
    def test_arena_events(self):
        arena = self.arena_manager.create_arena('Event Test Arena')
        self.arena_manager.add_agent_to_arena(arena.id, 'test_agent')
        events = arena.events
        assert len(events) >= 2
        join_events = [e for e in events if e.event_type == 'agent_joined']
        assert len(join_events) == 1
        assert join_events[0].agent_id == 'test_agent'
    def test_memory_scope_management(self):
        arena = self.arena_manager.create_arena('Memory Test Arena')
        arena.add_memory_scope('memory_1')
        arena.add_memory_scope('memory_2')
        assert 'memory_1' in arena.memory_scope_ids
        assert 'memory_2' in arena.memory_scope_ids
        memory_events = [e for e in arena.events if e.event_type == 'memory_scope_added']
        assert len(memory_events) == 2
    def test_arena_listing(self):
        arena1 = self.arena_manager.create_arena('Arena 1')
        self.arena_manager.create_arena('Arena 2')
        all_arenas = self.arena_manager.list_arenas()
        assert len(all_arenas) >= 2
        creating_arenas = self.arena_manager.list_arenas(ArenaState.CREATING)
        assert len(creating_arenas) >= 2
        self.arena_manager.activate_arena(arena1.id)
        active_arenas = self.arena_manager.list_arenas(ArenaState.ACTIVE)
        assert len(active_arenas) == 1
        assert active_arenas[0].id == arena1.id
    def test_arena_stats(self):
        arena1 = self.arena_manager.create_arena('Stats Arena 1')
        self.arena_manager.create_arena('Stats Arena 2')
        self.arena_manager.activate_arena(arena1.id)
        stats = self.arena_manager.get_arena_stats()
        assert stats['total_arenas'] >= 2
        assert 'creating' in stats['state_counts']
        assert 'active' in stats['state_counts']
        assert stats['state_counts']['creating'] >= 1
        assert stats['state_counts']['active'] >= 1