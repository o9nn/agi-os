"""
Test Arena Simulation Framework - Task 1.2.3

Tests to validate the acceptance criteria:
"Agents can navigate and interact in 3D environments"

This test suite validates:
- Virtual environments for agent interaction
- Physics simulation integration  
- Configurable environment parameters
"""

import pytest
import numpy as np
from aar_core.arena.simulation_engine import (
    SimulationEngine, ArenaType, ArenaConfig, 
    ArenaPhysics, ArenaEnvironment
)


class TestArenaSimulationFramework:
    """Test suite for Arena Simulation Framework implementation."""
    
    def setup_method(self):
        """Set up test fixtures."""
        self.engine = SimulationEngine()
    
    @pytest.mark.asyncio
    async def test_virtual_environments_creation(self):
        """Test creation of virtual environments for agent interaction."""
        # Test different arena types
        arena_types = [
            ArenaType.GENERAL,
            ArenaType.COLLABORATIVE, 
            ArenaType.COMPETITIVE,
            ArenaType.PHYSICS_3D,
            ArenaType.LEARNING
        ]
        
        created_arenas = []
        for arena_type in arena_types:
            arena_id = await self.engine.create_arena(arena_type)
            created_arenas.append(arena_id)
            
            arena = self.engine.get_arena(arena_id)
            assert arena is not None
            assert arena.config.arena_type == arena_type
            assert hasattr(arena.config.environment, 'dimensions')
            assert len(arena.config.environment.dimensions) == 3  # 3D environment
        
        # Verify multiple environments can coexist
        assert len(created_arenas) == len(arena_types)
        assert len(set(created_arenas)) == len(arena_types)  # All unique IDs
    
    @pytest.mark.asyncio
    async def test_physics_simulation_integration(self):
        """Test physics simulation integration in 3D environments."""
        # Create physics-enabled arena
        arena_id = await self.engine.create_arena(ArenaType.PHYSICS_3D)
        arena = self.engine.get_arena(arena_id)
        
        # Verify physics configuration
        assert arena.physics_enabled is True
        assert arena.config.physics is not None
        assert len(arena.config.physics.gravity) == 3
        assert arena.config.physics.collision_detection is True
        assert arena.config.physics.boundary_enforcement is True
        assert arena.config.physics.max_velocity > 0
        assert arena.config.physics.time_step > 0
        
        # Add agent and test physics effects
        agent_data = {
            'position': np.array([0.0, 0.0, 10.0]),  # Start elevated
            'velocity': np.array([0.0, 0.0, 0.0]),   # Initially at rest
            'energy': 100.0,
            'resources_collected': 0
        }
        
        success = await arena.add_agent('physics_test_agent', agent_data)
        assert success is True
        
        # Initial state
        agent = arena.agents['physics_test_agent']
        initial_z = agent['position'][2]
        assert initial_z == 10.0
        
        # Run simulation steps and verify gravity effect
        for _ in range(5):
            await arena._update_simulation_step()
        
        # Agent should fall due to gravity (z position should decrease)
        final_z = arena.agents['physics_test_agent']['position'][2]
        assert final_z < initial_z, "Agent should fall due to gravity"
        
        # Velocity should increase downward
        velocity_z = arena.agents['physics_test_agent']['velocity'][2]
        assert velocity_z < 0, "Agent should have downward velocity due to gravity"
    
    @pytest.mark.asyncio
    async def test_configurable_environment_parameters(self):
        """Test configurable environment parameters."""
        # Create custom arena configuration
        custom_physics = ArenaPhysics(
            gravity=(0.0, 0.0, -5.0),  # Reduced gravity
            air_resistance=0.02,       # Increased air resistance
            max_velocity=50.0          # Reduced max velocity
        )
        
        custom_environment = ArenaEnvironment(
            dimensions=(200.0, 100.0, 80.0),  # Custom dimensions
            boundary_type="rigid",
            resources=[
                {'position': (10.0, 10.0, 0.0), 'value': 5.0},
                {'position': (-10.0, -10.0, 0.0), 'value': 3.0}
            ],
            obstacles=[
                {'position': (0.0, 0.0, 5.0), 'collision_radius': 2.0}
            ]
        )
        
        custom_config = ArenaConfig(
            arena_type=ArenaType.PHYSICS_3D,
            max_agents=25,
            physics=custom_physics,
            environment=custom_environment,
            simulation_speed=2.0
        )
        
        # Create arena with custom configuration
        arena_id = await self.engine.create_arena(ArenaType.PHYSICS_3D, custom_config)
        arena = self.engine.get_arena(arena_id)
        
        # Verify custom parameters are applied
        assert arena.config.max_agents == 25
        assert arena.config.simulation_speed == 2.0
        assert arena.config.physics.gravity == (0.0, 0.0, -5.0)
        assert arena.config.physics.air_resistance == 0.02
        assert arena.config.physics.max_velocity == 50.0
        assert arena.config.environment.dimensions == (200.0, 100.0, 80.0)
        assert len(arena.config.environment.resources) == 2
        assert len(arena.config.environment.obstacles) == 1
        
        # Verify objects are created from configuration
        resource_objects = [obj for obj in arena.objects.values() if obj.type == 'resource']
        obstacle_objects = [obj for obj in arena.objects.values() if obj.type == 'obstacle']
        assert len(resource_objects) == 2
        assert len(obstacle_objects) == 1
    
    @pytest.mark.asyncio
    async def test_agent_3d_navigation(self):
        """Test that agents can navigate in 3D environments."""
        # Create 3D physics arena
        arena_id = await self.engine.create_arena(ArenaType.PHYSICS_3D)
        arena = self.engine.get_arena(arena_id)
        
        # Add multiple agents at different positions
        agents_data = {
            'agent_1': {
                'position': np.array([0.0, 0.0, 0.0]),
                'velocity': np.array([5.0, 0.0, 0.0]),  # Move in +X
                'energy': 100.0,
                'resources_collected': 0
            },
            'agent_2': {
                'position': np.array([0.0, 0.0, 0.0]),
                'velocity': np.array([0.0, 5.0, 0.0]),  # Move in +Y
                'energy': 100.0,
                'resources_collected': 0
            },
            'agent_3': {
                'position': np.array([0.0, 0.0, 5.0]),
                'velocity': np.array([0.0, 0.0, 5.0]),  # Move in +Z
                'energy': 100.0,
                'resources_collected': 0
            }
        }
        
        # Add all agents
        for agent_id, agent_data in agents_data.items():
            success = await arena.add_agent(agent_id, agent_data)
            assert success is True
        
        # Record initial positions
        initial_positions = {}
        for agent_id in agents_data:
            initial_positions[agent_id] = arena.agents[agent_id]['position'].copy()
        
        # Run simulation for several steps
        for _ in range(10):
            await arena._update_simulation_step()
        
        # Verify agents moved in their respective directions
        for agent_id in agents_data:
            current_pos = arena.agents[agent_id]['position']
            initial_pos = initial_positions[agent_id]
            
            # Calculate movement vector
            movement = current_pos - initial_pos
            
            # Verify significant movement occurred
            movement_magnitude = np.linalg.norm(movement)
            assert movement_magnitude > 0.01, f"Agent {agent_id} should have moved significantly"
        
        # Verify agent_1 moved primarily in X direction
        agent_1_movement = arena.agents['agent_1']['position'] - initial_positions['agent_1']
        assert abs(agent_1_movement[0]) > abs(agent_1_movement[1])  # More X than Y movement
        
        # Verify agent_2 moved primarily in Y direction  
        agent_2_movement = arena.agents['agent_2']['position'] - initial_positions['agent_2']
        assert abs(agent_2_movement[1]) > abs(agent_2_movement[0])  # More Y than X movement
    
    @pytest.mark.asyncio
    async def test_agent_object_interaction(self):
        """Test that agents can interact with objects in 3D environments."""
        # Create arena with resources
        custom_environment = ArenaEnvironment(
            dimensions=(50.0, 50.0, 25.0),
            resources=[
                {'position': (5.0, 0.0, 0.0), 'value': 10.0},
                {'position': (-5.0, 0.0, 0.0), 'value': 15.0}
            ]
        )
        
        custom_config = ArenaConfig(
            arena_type=ArenaType.PHYSICS_3D,
            environment=custom_environment
        )
        
        arena_id = await self.engine.create_arena(ArenaType.PHYSICS_3D, custom_config)
        arena = self.engine.get_arena(arena_id)
        
        # Verify resources were created
        resource_objects = [obj for obj in arena.objects.values() if obj.type == 'resource']
        assert len(resource_objects) == 2
        
        # Add agent near a resource
        agent_data = {
            'position': np.array([4.0, 0.0, 0.0]),  # Near first resource
            'velocity': np.array([0.0, 0.0, 0.0]),
            'energy': 50.0,
            'resources_collected': 0,
            'interaction_range': 2.0
        }
        
        success = await arena.add_agent('collector_agent', agent_data)
        assert success is True
        
        # Find the nearby resource
        nearby_resource = None
        for obj_id, obj in arena.objects.items():
            if obj.type == 'resource' and np.linalg.norm(obj.position - agent_data['position']) < 2.0:
                nearby_resource = obj_id
                break
        
        assert nearby_resource is not None, "Should find a nearby resource"
        
        # Test agent interaction with resource
        interaction_action = {
            'type': 'interact',
            'target_id': nearby_resource,
            'interaction_type': 'collect'
        }
        
        result = await arena._process_interact_action(
            arena.agents['collector_agent'], 
            interaction_action
        )
        
        # Verify successful interaction
        assert result['success'] is True
        assert result['type'] == 'interact'
        assert result['interaction_type'] == 'collect'
        
        # Verify agent energy increased
        agent = arena.agents['collector_agent']
        assert agent['energy'] > 50.0, "Agent energy should increase after collecting resource"
        assert agent['resources_collected'] == 1, "Resource collection counter should increment"
        
        # Verify resource was consumed
        resource_obj = arena.objects[nearby_resource]
        assert resource_obj.active is False, "Resource should be deactivated after collection"
    
    @pytest.mark.asyncio
    async def test_boundary_enforcement(self):
        """Test boundary enforcement in 3D environments."""
        # Create small arena for easy boundary testing
        custom_environment = ArenaEnvironment(
            dimensions=(10.0, 10.0, 10.0),
            boundary_type="rigid"
        )
        
        custom_config = ArenaConfig(
            arena_type=ArenaType.PHYSICS_3D,
            environment=custom_environment
        )
        
        arena_id = await self.engine.create_arena(ArenaType.PHYSICS_3D, custom_config)
        arena = self.engine.get_arena(arena_id)
        
        # Add agent moving toward boundary
        agent_data = {
            'position': np.array([0.0, 0.0, 0.0]),
            'velocity': np.array([10.0, 10.0, 10.0]),  # High velocity toward boundaries
            'energy': 100.0,
            'resources_collected': 0
        }
        
        success = await arena.add_agent('boundary_agent', agent_data)
        assert success is True
        
        # Run simulation until boundary hit
        for _ in range(20):
            await arena._update_simulation_step()
        
        # Verify agent stayed within boundaries
        agent_pos = arena.agents['boundary_agent']['position']
        dimensions = np.array(arena.config.environment.dimensions)
        max_bounds = dimensions / 2
        min_bounds = -dimensions / 2
        
        assert np.all(agent_pos >= min_bounds), f"Agent position {agent_pos} exceeds minimum bounds {min_bounds}"
        assert np.all(agent_pos <= max_bounds), f"Agent position {agent_pos} exceeds maximum bounds {max_bounds}"
        
        # Verify boundary hits were recorded
        assert arena.performance_stats['boundary_hits'] > 0, "Should have recorded boundary hits"
    
    @pytest.mark.asyncio
    async def test_multi_agent_environment(self):
        """Test multiple agents interacting in the same 3D environment."""
        # Create collaborative arena  
        arena_id = await self.engine.create_arena(ArenaType.COLLABORATIVE)
        arena = self.engine.get_arena(arena_id)
        
        # Add multiple agents
        num_agents = 5
        agent_positions = [
            [10.0, 10.0, 0.0],
            [-10.0, 10.0, 0.0], 
            [10.0, -10.0, 0.0],
            [-10.0, -10.0, 0.0],
            [0.0, 0.0, 5.0]
        ]
        
        for i in range(num_agents):
            agent_data = {
                'position': np.array(agent_positions[i]),
                'velocity': np.array([1.0, 1.0, 0.0]),
                'energy': 100.0,
                'resources_collected': 0
            }
            
            success = await arena.add_agent(f'agent_{i}', agent_data)
            assert success is True
        
        # Verify all agents were added
        assert len(arena.agents) == num_agents
        
        # Run simulation with multiple agents
        for _ in range(5):
            await arena._update_simulation_step()
        
        # Verify all agents are still active and have moved
        assert len(arena.agents) == num_agents
        
        for agent_id, agent in arena.agents.items():
            # Each agent should have some velocity (affected by physics)
            velocity_magnitude = np.linalg.norm(agent['velocity'])
            assert velocity_magnitude > 0, f"Agent {agent_id} should have velocity"
    
    def test_arena_type_configurations(self):
        """Test that different arena types have appropriate default configurations."""
        # Test that default configurations exist for key arena types
        expected_types = [ArenaType.GENERAL, ArenaType.COLLABORATIVE, ArenaType.COMPETITIVE, ArenaType.PHYSICS_3D]
        
        for arena_type in expected_types:
            assert arena_type in self.engine.default_configs, f"Missing default config for {arena_type}"
            config = self.engine.default_configs[arena_type]
            
            assert isinstance(config, ArenaConfig)
            assert config.arena_type == arena_type
            assert config.max_agents > 0
            assert len(config.environment.dimensions) == 3
            
            # Physics_3D should have physics enabled
            if arena_type == ArenaType.PHYSICS_3D:
                assert config.physics is not None
                assert len(config.physics.gravity) == 3
            
            # Collaborative should have resources
            if arena_type == ArenaType.COLLABORATIVE:
                assert len(config.environment.resources) > 0
            
            # Competitive should have obstacles
            if arena_type == ArenaType.COMPETITIVE:
                assert len(config.environment.obstacles) > 0
    
    @pytest.mark.asyncio
    async def test_performance_tracking(self):
        """Test that arena performance is tracked correctly."""
        arena_id = await self.engine.create_arena(ArenaType.PHYSICS_3D)
        arena = self.engine.get_arena(arena_id)
        
        # Add agent
        agent_data = {
            'position': np.array([0.0, 0.0, 0.0]),
            'velocity': np.array([1.0, 1.0, 0.0]),
            'energy': 100.0,
            'resources_collected': 0
        }
        
        await arena.add_agent('perf_test_agent', agent_data)
        
        # Run multiple simulation steps
        initial_step_count = arena.step_count
        num_steps = 10
        
        for _ in range(num_steps):
            await arena._update_simulation_step()
        
        # Verify performance tracking
        assert arena.step_count == initial_step_count + num_steps
        assert arena.simulation_time > 0
        assert arena.performance_stats['avg_frame_time'] > 0
        assert arena.last_update > arena.created_at