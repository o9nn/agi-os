import pytest
import numpy as np
from aar_core.arena.simulation_engine import SimulationEngine, ArenaType, ArenaConfig, ArenaPhysics, ArenaEnvironment
class TestArenaSimulationFramework:
    def setup_method(self):
        self.engine = SimulationEngine()
    @pytest.mark.asyncio
    async def test_virtual_environments_creation(self):
        arena_types = [ArenaType.GENERAL, ArenaType.COLLABORATIVE, ArenaType.COMPETITIVE, ArenaType.PHYSICS_3D, ArenaType.LEARNING]
        created_arenas = []
        for arena_type in arena_types:
            arena_id = await self.engine.create_arena(arena_type)
            created_arenas.append(arena_id)
            arena = self.engine.get_arena(arena_id)
            assert arena is not None
            assert arena.config.arena_type == arena_type
            assert hasattr(arena.config.environment, 'dimensions')
            assert len(arena.config.environment.dimensions) == 3
        assert len(created_arenas) == len(arena_types)
        assert len(set(created_arenas)) == len(arena_types)
    @pytest.mark.asyncio
    async def test_physics_simulation_integration(self):
        arena_id = await self.engine.create_arena(ArenaType.PHYSICS_3D)
        arena = self.engine.get_arena(arena_id)
        assert arena.physics_enabled is True
        assert arena.config.physics is not None
        assert len(arena.config.physics.gravity) == 3
        assert arena.config.physics.collision_detection is True
        assert arena.config.physics.boundary_enforcement is True
        assert arena.config.physics.max_velocity > 0
        assert arena.config.physics.time_step > 0
        agent_data = {'position': np.array([0.0, 0.0, 10.0]), 'velocity': np.array([0.0, 0.0, 0.0]), 'energy': 100.0, 'resources_collected': 0}
        success = await arena.add_agent('physics_test_agent', agent_data)
        assert success is True
        agent = arena.agents['physics_test_agent']
        initial_z = agent['position'][2]
        assert initial_z == 10.0
        for _ in range(5):
            await arena._update_simulation_step()
        final_z = arena.agents['physics_test_agent']['position'][2]
        assert final_z < initial_z, 'Agent should fall due to gravity'
        velocity_z = arena.agents['physics_test_agent']['velocity'][2]
        assert velocity_z < 0, 'Agent should have downward velocity due to gravity'
    @pytest.mark.asyncio
    async def test_configurable_environment_parameters(self):
        custom_physics = ArenaPhysics(gravity=(0.0, 0.0, -5.0), air_resistance=0.02, max_velocity=50.0)
        custom_environment = ArenaEnvironment(dimensions=(200.0, 100.0, 80.0), boundary_type='rigid', resources=[{'position': (10.0, 10.0, 0.0), 'value': 5.0}, {'position': (-10.0, -10.0, 0.0), 'value': 3.0}], obstacles=[{'position': (0.0, 0.0, 5.0), 'collision_radius': 2.0}])
        custom_config = ArenaConfig(arena_type=ArenaType.PHYSICS_3D, max_agents=25, physics=custom_physics, environment=custom_environment, simulation_speed=2.0)
        arena_id = await self.engine.create_arena(ArenaType.PHYSICS_3D, custom_config)
        arena = self.engine.get_arena(arena_id)
        assert arena.config.max_agents == 25
        assert arena.config.simulation_speed == 2.0
        assert arena.config.physics.gravity == (0.0, 0.0, -5.0)
        assert arena.config.physics.air_resistance == 0.02
        assert arena.config.physics.max_velocity == 50.0
        assert arena.config.environment.dimensions == (200.0, 100.0, 80.0)
        assert len(arena.config.environment.resources) == 2
        assert len(arena.config.environment.obstacles) == 1
        resource_objects = [obj for obj in arena.objects.values() if obj.type == 'resource']
        obstacle_objects = [obj for obj in arena.objects.values() if obj.type == 'obstacle']
        assert len(resource_objects) == 2
        assert len(obstacle_objects) == 1
    @pytest.mark.asyncio
    async def test_agent_3d_navigation(self):
        arena_id = await self.engine.create_arena(ArenaType.PHYSICS_3D)
        arena = self.engine.get_arena(arena_id)
        agents_data = {'agent_1': {'position': np.array([0.0, 0.0, 0.0]), 'velocity': np.array([5.0, 0.0, 0.0]), 'energy': 100.0, 'resources_collected': 0}, 'agent_2': {'position': np.array([0.0, 0.0, 0.0]), 'velocity': np.array([0.0, 5.0, 0.0]), 'energy': 100.0, 'resources_collected': 0}, 'agent_3': {'position': np.array([0.0, 0.0, 5.0]), 'velocity': np.array([0.0, 0.0, 5.0]), 'energy': 100.0, 'resources_collected': 0}}
        for agent_id, agent_data in agents_data.items():
            success = await arena.add_agent(agent_id, agent_data)
            assert success is True
        initial_positions = {}
        for agent_id in agents_data:
            initial_positions[agent_id] = arena.agents[agent_id]['position'].copy()
        for _ in range(10):
            await arena._update_simulation_step()
        for agent_id in agents_data:
            current_pos = arena.agents[agent_id]['position']
            initial_pos = initial_positions[agent_id]
            movement = current_pos - initial_pos
            movement_magnitude = np.linalg.norm(movement)
            assert movement_magnitude > 0.01, f'Agent {agent_id} should have moved significantly'
        agent_1_movement = arena.agents['agent_1']['position'] - initial_positions['agent_1']
        assert abs(agent_1_movement[0]) > abs(agent_1_movement[1])
        agent_2_movement = arena.agents['agent_2']['position'] - initial_positions['agent_2']
        assert abs(agent_2_movement[1]) > abs(agent_2_movement[0])
    @pytest.mark.asyncio
    async def test_agent_object_interaction(self):
        custom_environment = ArenaEnvironment(dimensions=(50.0, 50.0, 25.0), resources=[{'position': (5.0, 0.0, 0.0), 'value': 10.0}, {'position': (-5.0, 0.0, 0.0), 'value': 15.0}])
        custom_config = ArenaConfig(arena_type=ArenaType.PHYSICS_3D, environment=custom_environment)
        arena_id = await self.engine.create_arena(ArenaType.PHYSICS_3D, custom_config)
        arena = self.engine.get_arena(arena_id)
        resource_objects = [obj for obj in arena.objects.values() if obj.type == 'resource']
        assert len(resource_objects) == 2
        agent_data = {'position': np.array([4.0, 0.0, 0.0]), 'velocity': np.array([0.0, 0.0, 0.0]), 'energy': 50.0, 'resources_collected': 0, 'interaction_range': 2.0}
        success = await arena.add_agent('collector_agent', agent_data)
        assert success is True
        nearby_resource = None
        for obj_id, obj in arena.objects.items():
            if obj.type == 'resource' and np.linalg.norm(obj.position - agent_data['position']) < 2.0:
                nearby_resource = obj_id
                break
        assert nearby_resource is not None, 'Should find a nearby resource'
        interaction_action = {'type': 'interact', 'target_id': nearby_resource, 'interaction_type': 'collect'}
        result = await arena._process_interact_action(arena.agents['collector_agent'], interaction_action)
        assert result['success'] is True
        assert result['type'] == 'interact'
        assert result['interaction_type'] == 'collect'
        agent = arena.agents['collector_agent']
        assert agent['energy'] > 50.0, 'Agent energy should increase after collecting resource'
        assert agent['resources_collected'] == 1, 'Resource collection counter should increment'
        resource_obj = arena.objects[nearby_resource]
        assert resource_obj.active is False, 'Resource should be deactivated after collection'
    @pytest.mark.asyncio
    async def test_boundary_enforcement(self):
        custom_environment = ArenaEnvironment(dimensions=(10.0, 10.0, 10.0), boundary_type='rigid')
        custom_config = ArenaConfig(arena_type=ArenaType.PHYSICS_3D, environment=custom_environment)
        arena_id = await self.engine.create_arena(ArenaType.PHYSICS_3D, custom_config)
        arena = self.engine.get_arena(arena_id)
        agent_data = {'position': np.array([0.0, 0.0, 0.0]), 'velocity': np.array([10.0, 10.0, 10.0]), 'energy': 100.0, 'resources_collected': 0}
        success = await arena.add_agent('boundary_agent', agent_data)
        assert success is True
        for _ in range(20):
            await arena._update_simulation_step()
        agent_pos = arena.agents['boundary_agent']['position']
        dimensions = np.array(arena.config.environment.dimensions)
        max_bounds = dimensions / 2
        min_bounds = -dimensions / 2
        assert np.all(agent_pos >= min_bounds), f'Agent position {agent_pos} exceeds minimum bounds {min_bounds}'
        assert np.all(agent_pos <= max_bounds), f'Agent position {agent_pos} exceeds maximum bounds {max_bounds}'
        assert arena.performance_stats['boundary_hits'] > 0, 'Should have recorded boundary hits'
    @pytest.mark.asyncio
    async def test_multi_agent_environment(self):
        arena_id = await self.engine.create_arena(ArenaType.COLLABORATIVE)
        arena = self.engine.get_arena(arena_id)
        num_agents = 5
        agent_positions = [[10.0, 10.0, 0.0], [-10.0, 10.0, 0.0], [10.0, -10.0, 0.0], [-10.0, -10.0, 0.0], [0.0, 0.0, 5.0]]
        for i in range(num_agents):
            agent_data = {'position': np.array(agent_positions[i]), 'velocity': np.array([1.0, 1.0, 0.0]), 'energy': 100.0, 'resources_collected': 0}
            success = await arena.add_agent(f'agent_{i}', agent_data)
            assert success is True
        assert len(arena.agents) == num_agents
        for _ in range(5):
            await arena._update_simulation_step()
        assert len(arena.agents) == num_agents
        for agent_id, agent in arena.agents.items():
            velocity_magnitude = np.linalg.norm(agent['velocity'])
            assert velocity_magnitude > 0, f'Agent {agent_id} should have velocity'
    def test_arena_type_configurations(self):
        expected_types = [ArenaType.GENERAL, ArenaType.COLLABORATIVE, ArenaType.COMPETITIVE, ArenaType.PHYSICS_3D]
        for arena_type in expected_types:
            assert arena_type in self.engine.default_configs, f'Missing default config for {arena_type}'
            config = self.engine.default_configs[arena_type]
            assert isinstance(config, ArenaConfig)
            assert config.arena_type == arena_type
            assert config.max_agents > 0
            assert len(config.environment.dimensions) == 3
            if arena_type == ArenaType.PHYSICS_3D:
                assert config.physics is not None
                assert len(config.physics.gravity) == 3
            if arena_type == ArenaType.COLLABORATIVE:
                assert len(config.environment.resources) > 0
            if arena_type == ArenaType.COMPETITIVE:
                assert len(config.environment.obstacles) > 0
    @pytest.mark.asyncio
    async def test_performance_tracking(self):
        arena_id = await self.engine.create_arena(ArenaType.PHYSICS_3D)
        arena = self.engine.get_arena(arena_id)
        agent_data = {'position': np.array([0.0, 0.0, 0.0]), 'velocity': np.array([1.0, 1.0, 0.0]), 'energy': 100.0, 'resources_collected': 0}
        await arena.add_agent('perf_test_agent', agent_data)
        initial_step_count = arena.step_count
        num_steps = 10
        for _ in range(num_steps):
            await arena._update_simulation_step()
        assert arena.step_count == initial_step_count + num_steps
        assert arena.simulation_time > 0
        assert arena.performance_stats['avg_frame_time'] > 0
        assert arena.last_update > arena.created_at