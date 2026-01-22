import logging
import time
import uuid
import numpy as np
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass, field
from enum import Enum
logger = logging.getLogger(__name__)
class ArenaType(Enum):
    GENERAL = 'general'
    COLLABORATIVE = 'collaborative'
    COMPETITIVE = 'competitive'
    LEARNING = 'learning'
    PHYSICS_3D = 'physics_3d'
    ABSTRACT = 'abstract'
    PROCEDURAL = 'procedural'
@dataclass
class ArenaPhysics:
    gravity: Tuple[float, float, float] = (0.0, 0.0, -9.81)
    air_resistance: float = 0.01
    collision_detection: bool = True
    boundary_enforcement: bool = True
    time_step: float = 0.016
    max_velocity: float = 100.0
@dataclass
class ArenaEnvironment:
    dimensions: Tuple[float, float, float] = (100.0, 100.0, 100.0)
    boundary_type: str = 'rigid'
    lighting: Dict[str, Any] = field(default_factory=dict)
    resources: List[Dict[str, Any]] = field(default_factory=list)
    obstacles: List[Dict[str, Any]] = field(default_factory=list)
    dynamic_elements: List[Dict[str, Any]] = field(default_factory=list)
@dataclass
class ArenaConfig:
    arena_type: ArenaType = ArenaType.GENERAL
    max_agents: int = 100
    physics: ArenaPhysics = field(default_factory=ArenaPhysics)
    environment: ArenaEnvironment = field(default_factory=ArenaEnvironment)
    simulation_speed: float = 1.0
    auto_cleanup: bool = True
    recording_enabled: bool = False
class ArenaObject:
    def __init__(self, obj_id: str, obj_type: str, position: Tuple[float, float, float], properties: Optional[Dict[str, Any]]=None):
        self.id = obj_id
        self.type = obj_type
        self.position = np.array(position, dtype=float)
        self.velocity = np.array([0.0, 0.0, 0.0])
        self.rotation = np.array([0.0, 0.0, 0.0])
        self.properties = properties or {}
        self.created_at = time.time()
        self.last_update = time.time()
        self.active = True
    def update_physics(self, dt: float, physics: ArenaPhysics) -> None:
        if not self.active:
            return
        if self.properties.get('affected_by_gravity', True):
            gravity_acceleration = np.array(physics.gravity)
            self.velocity += gravity_acceleration * dt
        if physics.air_resistance > 0:
            self.velocity *= 1.0 - physics.air_resistance * dt
        velocity_magnitude = np.linalg.norm(self.velocity)
        if velocity_magnitude > physics.max_velocity:
            self.velocity = self.velocity * (physics.max_velocity / velocity_magnitude)
        self.position += self.velocity * dt
        self.last_update = time.time()
    def check_boundaries(self, environment: ArenaEnvironment) -> bool:
        dimensions = np.array(environment.dimensions)
        min_bounds = -dimensions / 2
        max_bounds = dimensions / 2
        boundary_hit = False
        for i in range(3):
            if self.position[i] < min_bounds[i] or self.position[i] > max_bounds[i]:
                boundary_hit = True
                if environment.boundary_type == 'rigid':
                    self.position[i] = np.clip(self.position[i], min_bounds[i], max_bounds[i])
                    self.velocity[i] *= -0.8
                elif environment.boundary_type == 'absorbing':
                    self.position[i] = np.clip(self.position[i], min_bounds[i], max_bounds[i])
                    self.velocity[i] = 0.0
                elif environment.boundary_type == 'periodic':
                    if self.position[i] < min_bounds[i]:
                        self.position[i] = max_bounds[i]
                    elif self.position[i] > max_bounds[i]:
                        self.position[i] = min_bounds[i]
        return boundary_hit
    def get_state(self) -> Dict[str, Any]:
        return {'id': self.id, 'type': self.type, 'position': self.position.tolist(), 'velocity': self.velocity.tolist(), 'rotation': self.rotation.tolist(), 'properties': self.properties, 'active': self.active, 'created_at': self.created_at, 'last_update': self.last_update}
class Arena:
    def __init__(self, arena_id: str, config: ArenaConfig):
        self.id = arena_id
        self.config = config
        self.created_at = time.time()
        self.last_update = time.time()
        self.agents: Dict[str, Dict[str, Any]] = {}
        self.objects: Dict[str, ArenaObject] = {}
        self.simulation_time = 0.0
        self.step_count = 0
        self.active_sessions = 0
        self.physics_enabled = config.physics is not None
        self._last_physics_update = time.time()
        self.events: List[Dict[str, Any]] = []
        self.max_event_history = 1000
        self.performance_stats = {'total_interactions': 0, 'avg_frame_time': 0.0, 'agent_collisions': 0, 'boundary_hits': 0, 'resource_consumptions': 0}
        self._initialize_environment()
        logger.info(f'Arena {self.id} created with type {config.arena_type.value}')
    def _initialize_environment(self) -> None:
        for i, resource_config in enumerate(self.config.environment.resources):
            resource_id = f'resource_{i}'
            position = resource_config.get('position', (0.0, 0.0, 0.0))
            resource = ArenaObject(resource_id, 'resource', position, resource_config)
            self.objects[resource_id] = resource
        for i, obstacle_config in enumerate(self.config.environment.obstacles):
            obstacle_id = f'obstacle_{i}'
            position = obstacle_config.get('position', (0.0, 0.0, 0.0))
            obstacle = ArenaObject(obstacle_id, 'obstacle', position, obstacle_config)
            self.objects[obstacle_id] = obstacle
        for i, element_config in enumerate(self.config.environment.dynamic_elements):
            element_id = f'dynamic_{i}'
            position = element_config.get('position', (0.0, 0.0, 0.0))
            element = ArenaObject(element_id, 'dynamic', position, element_config)
            self.objects[element_id] = element
    async def add_agent(self, agent_id: str, agent_data: Dict[str, Any]) -> bool:
        if len(self.agents) >= self.config.max_agents:
            logger.warning(f'Arena {self.id} at maximum capacity ({self.config.max_agents})')
            return False
        if agent_id in self.agents:
            logger.warning(f'Agent {agent_id} already in arena {self.id}')
            return False
        if 'position' not in agent_data:
            agent_data['position'] = self._generate_spawn_position()
        position = agent_data['position']
        if hasattr(position, 'tolist'):
            position = position.tolist()
        elif not isinstance(position, (list, tuple)):
            position = [0.0, 0.0, 0.0]
        agent_state = {'id': agent_id, 'position': np.array(position, dtype=float), 'velocity': np.array([0.0, 0.0, 0.0]), 'orientation': np.array(agent_data.get('orientation', [0.0, 0.0, 1.0])), 'energy': agent_data.get('energy', 100.0), 'perception_range': agent_data.get('perception_range', 10.0), 'actions_taken': 0, 'last_action_time': time.time(), 'collision_count': 0, 'resources_collected': 0, **{k: v for k, v in agent_data.items() if k not in ['position', 'orientation']}}
        self.agents[agent_id] = agent_state
        self.active_sessions += 1
        self._record_event('agent_added', {'agent_id': agent_id, 'position': agent_state['position'].tolist()})
        logger.debug(f'Agent {agent_id} added to arena {self.id}')
        return True
    async def remove_agent(self, agent_id: str) -> bool:
        if agent_id not in self.agents:
            return False
        del self.agents[agent_id]
        self.active_sessions = max(0, self.active_sessions - 1)
        self._record_event('agent_removed', {'agent_id': agent_id})
        logger.debug(f'Agent {agent_id} removed from arena {self.id}')
        return True
    def _generate_spawn_position(self) -> Tuple[float, float, float]:
        dimensions = self.config.environment.dimensions
        x = np.random.uniform(-dimensions[0] / 3, dimensions[0] / 3)
        y = np.random.uniform(-dimensions[1] / 3, dimensions[1] / 3)
        z = np.random.uniform(0, dimensions[2] / 4)
        return (x, y, z)
    async def execute_agent(self, agent_data: Dict[str, Any], request: Dict[str, Any]) -> Dict[str, Any]:
        agent_id = agent_data.get('id', 'unknown_agent')
        if agent_id not in self.agents:
            await self.add_agent(agent_id, agent_data)
        action = request.get('action', {})
        result = await self._process_agent_action(agent_id, action)
        await self._update_simulation_step()
        perception = self._get_agent_perception(agent_id)
        return {'agent_id': agent_id, 'action_result': result, 'perception': perception, 'arena_state': self._get_arena_summary(), 'simulation_time': self.simulation_time, 'step_count': self.step_count}
    async def _process_agent_action(self, agent_id: str, action: Dict[str, Any]) -> Dict[str, Any]:
        if agent_id not in self.agents:
            return {'error': 'Agent not in arena'}
        agent = self.agents[agent_id]
        action_type = action.get('type', 'idle')
        result = {'type': action_type, 'success': False}
        try:
            if action_type == 'move':
                result = await self._process_move_action(agent, action)
            elif action_type == 'interact':
                result = await self._process_interact_action(agent, action)
            elif action_type == 'observe':
                result = await self._process_observe_action(agent, action)
            elif action_type == 'communicate':
                result = await self._process_communicate_action(agent, action)
            elif action_type == 'create':
                result = await self._process_create_action(agent, action)
            elif action_type == 'modify':
                result = await self._process_modify_action(agent, action)
            else:
                result = {'type': action_type, 'success': True, 'message': 'No action taken'}
            agent['actions_taken'] += 1
            agent['last_action_time'] = time.time()
            self._record_event('agent_action', {'agent_id': agent_id, 'action': action, 'result': result})
            self.performance_stats['total_interactions'] += 1
        except Exception as e:
            logger.error(f'Error processing action for agent {agent_id}: {e}')
            result = {'type': action_type, 'success': False, 'error': str(e)}
        return result
    async def _process_move_action(self, agent: Dict[str, Any], action: Dict[str, Any]) -> Dict[str, Any]:
        direction = action.get('direction', [0.0, 0.0, 0.0])
        speed = action.get('speed', 1.0)
        direction_array = np.array(direction, dtype=float)
        if np.linalg.norm(direction_array) > 0:
            direction_array = direction_array / np.linalg.norm(direction_array)
            agent['velocity'] = direction_array * speed
        return {'type': 'move', 'success': True, 'direction': direction, 'speed': speed}
    async def _process_interact_action(self, agent: Dict[str, Any], action: Dict[str, Any]) -> Dict[str, Any]:
        target_id = action.get('target_id')
        interaction_type = action.get('interaction_type', 'collect')
        if not target_id or target_id not in self.objects:
            return {'type': 'interact', 'success': False, 'message': 'Target not found'}
        target_object = self.objects[target_id]
        agent_pos = agent['position']
        target_pos = target_object.position
        distance = np.linalg.norm(agent_pos - target_pos)
        interaction_range = agent.get('interaction_range', 2.0)
        if distance > interaction_range:
            return {'type': 'interact', 'success': False, 'message': 'Target out of range'}
        if interaction_type == 'collect' and target_object.type == 'resource':
            resource_value = target_object.properties.get('value', 1.0)
            agent['energy'] = min(100.0, agent['energy'] + resource_value)
            agent['resources_collected'] += 1
            target_object.active = False
            self.performance_stats['resource_consumptions'] += 1
            return {'type': 'interact', 'success': True, 'interaction_type': 'collect', 'value': resource_value}
        return {'type': 'interact', 'success': True, 'interaction_type': interaction_type}
    async def _process_observe_action(self, agent: Dict[str, Any], action: Dict[str, Any]) -> Dict[str, Any]:
        observation_range = action.get('range', agent.get('perception_range', 10.0))
        observation_data = self._get_agent_perception(agent['id'], observation_range)
        return {'type': 'observe', 'success': True, 'data': observation_data}
    async def _process_communicate_action(self, agent: Dict[str, Any], action: Dict[str, Any]) -> Dict[str, Any]:
        message = action.get('message', '')
        target_agents = action.get('targets', [])
        broadcast_range = action.get('range', agent.get('communication_range', 20.0))
        recipients = []
        agent_pos = agent['position']
        for other_id, other_agent in self.agents.items():
            if other_id == agent['id']:
                continue
            if target_agents and other_id not in target_agents:
                continue
            distance = np.linalg.norm(agent_pos - other_agent['position'])
            if distance <= broadcast_range:
                recipients.append(other_id)
        return {'type': 'communicate', 'success': True, 'message': message, 'recipients': recipients}
    async def _process_create_action(self, agent: Dict[str, Any], action: Dict[str, Any]) -> Dict[str, Any]:
        object_type = action.get('object_type', 'generic')
        position = action.get('position', agent['position'].tolist())
        properties = action.get('properties', {})
        object_id = f'created_{uuid.uuid4().hex[:8]}'
        new_object = ArenaObject(object_id, object_type, position, properties)
        self.objects[object_id] = new_object
        return {'type': 'create', 'success': True, 'object_id': object_id}
    async def _process_modify_action(self, agent: Dict[str, Any], action: Dict[str, Any]) -> Dict[str, Any]:
        target_position = action.get('position', agent['position'].tolist())
        action.get('modification_type', 'elevation')
        intensity = action.get('intensity', 1.0)
        return {'type': 'modify', 'success': True, 'position': target_position, 'intensity': intensity}
    def _get_agent_perception(self, agent_id: str, perception_range: Optional[float]=None) -> Dict[str, Any]:
        if agent_id not in self.agents:
            return {}
        agent = self.agents[agent_id]
        agent_pos = agent['position']
        range_limit = perception_range or agent.get('perception_range', 10.0)
        perception = {'agents': [], 'objects': [], 'environment': {'boundaries': self.config.environment.dimensions, 'arena_type': self.config.arena_type.value}}
        for other_id, other_agent in self.agents.items():
            if other_id == agent_id:
                continue
            distance = np.linalg.norm(agent_pos - other_agent['position'])
            if distance <= range_limit:
                perception['agents'].append({'id': other_id, 'position': other_agent['position'].tolist(), 'velocity': other_agent['velocity'].tolist(), 'distance': distance})
        for obj_id, obj in self.objects.items():
            if not obj.active:
                continue
            distance = np.linalg.norm(agent_pos - obj.position)
            if distance <= range_limit:
                perception['objects'].append({'id': obj_id, 'type': obj.type, 'position': obj.position.tolist(), 'distance': distance, 'properties': obj.properties})
        return perception
    async def _update_simulation_step(self) -> None:
        start_time = time.time()
        current_time = time.time()
        dt = current_time - self._last_physics_update
        if self.physics_enabled and dt > 0:
            for agent in self.agents.values():
                agent['position'] += agent['velocity'] * dt
                self._apply_agent_physics(agent, dt)
                self._check_agent_boundaries(agent)
            for obj in self.objects.values():
                obj.update_physics(dt, self.config.physics)
                obj.check_boundaries(self.config.environment)
            await self._check_collisions()
            self._last_physics_update = current_time
        self.simulation_time += dt
        self.step_count += 1
        self.last_update = time.time()
        frame_time = time.time() - start_time
        total_steps = self.step_count
        current_avg = self.performance_stats['avg_frame_time']
        new_avg = (current_avg * (total_steps - 1) + frame_time) / total_steps
        self.performance_stats['avg_frame_time'] = new_avg
    def _apply_agent_physics(self, agent: Dict[str, Any], dt: float) -> None:
        if self.config.physics and self.config.physics.gravity:
            gravity_acceleration = np.array(self.config.physics.gravity)
            agent['velocity'] += gravity_acceleration * dt
        if self.config.physics.air_resistance > 0:
            agent['velocity'] *= 1.0 - self.config.physics.air_resistance * dt
        velocity_magnitude = np.linalg.norm(agent['velocity'])
        if velocity_magnitude > self.config.physics.max_velocity:
            agent['velocity'] = agent['velocity'] * (self.config.physics.max_velocity / velocity_magnitude)
    def _check_agent_boundaries(self, agent: Dict[str, Any]) -> None:
        dimensions = np.array(self.config.environment.dimensions)
        min_bounds = -dimensions / 2
        max_bounds = dimensions / 2
        for i in range(3):
            if agent['position'][i] < min_bounds[i] or agent['position'][i] > max_bounds[i]:
                self.performance_stats['boundary_hits'] += 1
                if self.config.environment.boundary_type == 'rigid':
                    agent['position'][i] = np.clip(agent['position'][i], min_bounds[i], max_bounds[i])
                    agent['velocity'][i] *= -0.5
                elif self.config.environment.boundary_type == 'absorbing':
                    agent['position'][i] = np.clip(agent['position'][i], min_bounds[i], max_bounds[i])
                    agent['velocity'][i] = 0.0
                elif self.config.environment.boundary_type == 'periodic':
                    if agent['position'][i] < min_bounds[i]:
                        agent['position'][i] = max_bounds[i]
                    elif agent['position'][i] > max_bounds[i]:
                        agent['position'][i] = min_bounds[i]
    async def _check_collisions(self) -> None:
        agent_list = list(self.agents.items())
        for i, (agent1_id, agent1) in enumerate(agent_list):
            for j, (agent2_id, agent2) in enumerate(agent_list[i + 1:], i + 1):
                distance = np.linalg.norm(agent1['position'] - agent2['position'])
                collision_distance = agent1.get('collision_radius', 1.0) + agent2.get('collision_radius', 1.0)
                if distance < collision_distance:
                    await self._handle_agent_collision(agent1_id, agent2_id)
        for agent_id, agent in self.agents.items():
            for obj_id, obj in self.objects.items():
                if not obj.active:
                    continue
                distance = np.linalg.norm(agent['position'] - obj.position)
                collision_distance = agent.get('collision_radius', 1.0) + obj.properties.get('collision_radius', 1.0)
                if distance < collision_distance:
                    await self._handle_agent_object_collision(agent_id, obj_id)
    async def _handle_agent_collision(self, agent1_id: str, agent2_id: str) -> None:
        agent1 = self.agents[agent1_id]
        agent2 = self.agents[agent2_id]
        direction = agent1['position'] - agent2['position']
        if np.linalg.norm(direction) > 0:
            direction = direction / np.linalg.norm(direction)
            temp_velocity = agent1['velocity'].copy()
            agent1['velocity'] = agent2['velocity'] * 0.8
            agent2['velocity'] = temp_velocity * 0.8
            separation_distance = 2.0
            agent1['position'] += direction * separation_distance / 2
            agent2['position'] -= direction * separation_distance / 2
        agent1['collision_count'] += 1
        agent2['collision_count'] += 1
        self.performance_stats['agent_collisions'] += 1
        self._record_event('agent_collision', {'agent1_id': agent1_id, 'agent2_id': agent2_id})
    async def _handle_agent_object_collision(self, agent_id: str, object_id: str) -> None:
        agent = self.agents[agent_id]
        obj = self.objects[object_id]
        if obj.type == 'obstacle':
            agent['velocity'] = np.array([0.0, 0.0, 0.0])
            direction = agent['position'] - obj.position
            if np.linalg.norm(direction) > 0:
                direction = direction / np.linalg.norm(direction)
                agent['position'] += direction * 2.0
        agent['collision_count'] += 1
        self._record_event('agent_object_collision', {'agent_id': agent_id, 'object_id': object_id, 'object_type': obj.type})
    def _record_event(self, event_type: str, event_data: Dict[str, Any]) -> None:
        if not self.config.recording_enabled:
            return
        event = {'type': event_type, 'timestamp': time.time(), 'simulation_time': self.simulation_time, 'step': self.step_count, 'data': event_data}
        self.events.append(event)
        if len(self.events) > self.max_event_history:
            self.events = self.events[-self.max_event_history:]
    def _get_arena_summary(self) -> Dict[str, Any]:
        return {'id': self.id, 'type': self.config.arena_type.value, 'agent_count': len(self.agents), 'object_count': len([obj for obj in self.objects.values() if obj.active]), 'simulation_time': self.simulation_time, 'step_count': self.step_count, 'active_sessions': self.active_sessions, 'dimensions': self.config.environment.dimensions}
    def get_full_state(self) -> Dict[str, Any]:
        return {'arena_info': {'id': self.id, 'type': self.config.arena_type.value, 'created_at': self.created_at, 'simulation_time': self.simulation_time, 'step_count': self.step_count, 'last_update': self.last_update}, 'config': {'max_agents': self.config.max_agents, 'physics_enabled': self.physics_enabled, 'dimensions': self.config.environment.dimensions, 'boundary_type': self.config.environment.boundary_type}, 'agents': {agent_id: {'position': agent['position'].tolist(), 'velocity': agent['velocity'].tolist(), 'energy': agent['energy'], 'actions_taken': agent['actions_taken'], 'collision_count': agent['collision_count'], 'resources_collected': agent['resources_collected']} for agent_id, agent in self.agents.items()}, 'objects': {obj_id: obj.get_state() for obj_id, obj in self.objects.items() if obj.active}, 'performance_stats': self.performance_stats, 'recent_events': self.events[-10:] if self.config.recording_enabled else []}
class SimulationEngine:
    def __init__(self):
        self.arenas: Dict[str, Arena] = {}
        self.default_configs: Dict[ArenaType, ArenaConfig] = {}
        self.total_arenas_created = 0
        self.total_agent_interactions = 0
        self.system_start_time = time.time()
        self._initialize_default_configs()
        logger.info('Simulation Engine initialized')
    def _initialize_default_configs(self) -> None:
        self.default_configs[ArenaType.GENERAL] = ArenaConfig(arena_type=ArenaType.GENERAL, max_agents=50, environment=ArenaEnvironment(dimensions=(50.0, 50.0, 25.0)))
        self.default_configs[ArenaType.COLLABORATIVE] = ArenaConfig(arena_type=ArenaType.COLLABORATIVE, max_agents=100, environment=ArenaEnvironment(dimensions=(100.0, 100.0, 50.0), resources=[{'position': (10.0, 10.0, 0.0), 'value': 5.0}, {'position': (-10.0, -10.0, 0.0), 'value': 5.0}]))
        self.default_configs[ArenaType.COMPETITIVE] = ArenaConfig(arena_type=ArenaType.COMPETITIVE, max_agents=20, environment=ArenaEnvironment(dimensions=(30.0, 30.0, 15.0), obstacles=[{'position': (0.0, 0.0, 0.0), 'collision_radius': 3.0}]))
        self.default_configs[ArenaType.PHYSICS_3D] = ArenaConfig(arena_type=ArenaType.PHYSICS_3D, max_agents=30, physics=ArenaPhysics(gravity=(0.0, 0.0, -9.81), collision_detection=True, boundary_enforcement=True), environment=ArenaEnvironment(dimensions=(60.0, 60.0, 30.0), boundary_type='rigid'), recording_enabled=True)
    async def create_arena(self, arena_type: ArenaType=ArenaType.GENERAL, custom_config: Optional[ArenaConfig]=None, arena_id: Optional[str]=None) -> str:
        if arena_id is None:
            arena_id = f'arena_{uuid.uuid4().hex[:8]}'
        if arena_id in self.arenas:
            raise ValueError(f'Arena {arena_id} already exists')
        config = custom_config or self.default_configs.get(arena_type, ArenaConfig(arena_type=arena_type))
        arena = Arena(arena_id, config)
        self.arenas[arena_id] = arena
        self.total_arenas_created += 1
        logger.info(f'Created arena {arena_id} of type {arena_type.value}')
        return arena_id
    async def destroy_arena(self, arena_id: str) -> bool:
        if arena_id not in self.arenas:
            logger.warning(f'Arena {arena_id} not found for destruction')
            return False
        arena = self.arenas[arena_id]
        agent_ids = list(arena.agents.keys())
        for agent_id in agent_ids:
            await arena.remove_agent(agent_id)
        del self.arenas[arena_id]
        logger.info(f'Arena {arena_id} destroyed')
        return True
    async def get_or_create_arena(self, context: Dict[str, Any], arena_type: ArenaType=ArenaType.GENERAL) -> str:
        arena_id = context.get('arena_id')
        if arena_id and arena_id in self.arenas:
            return arena_id
        custom_config = context.get('arena_config')
        config = ArenaConfig(**custom_config) if custom_config else None
        return await self.create_arena(arena_type, config, arena_id)
    def get_arena(self, arena_id: str) -> Optional[Arena]:
        return self.arenas.get(arena_id)
    def list_arenas(self) -> List[Dict[str, Any]]:
        return [{'id': arena.id, 'type': arena.config.arena_type.value, 'agent_count': len(arena.agents), 'active_sessions': arena.active_sessions, 'created_at': arena.created_at, 'simulation_time': arena.simulation_time} for arena in self.arenas.values()]
    async def execute_agent_in_arena(self, arena_id: str, agent_data: Dict[str, Any], request: Dict[str, Any]) -> Dict[str, Any]:
        arena = self.get_arena(arena_id)
        if not arena:
            raise ValueError(f'Arena {arena_id} not found')
        result = await arena.execute_agent(agent_data, request)
        self.total_agent_interactions += 1
        return result
    def get_system_stats(self) -> Dict[str, Any]:
        total_agents = sum((len(arena.agents) for arena in self.arenas.values()))
        total_objects = sum((len([obj for obj in arena.objects.values() if obj.active]) for arena in self.arenas.values()))
        avg_agents_per_arena = total_agents / max(len(self.arenas), 1)
        return {'system_info': {'uptime': time.time() - self.system_start_time, 'total_arenas_created': self.total_arenas_created, 'active_arenas': len(self.arenas), 'total_agent_interactions': self.total_agent_interactions}, 'current_state': {'total_agents': total_agents, 'total_objects': total_objects, 'avg_agents_per_arena': avg_agents_per_arena}, 'arena_breakdown': {arena_type.value: len([arena for arena in self.arenas.values() if arena.config.arena_type == arena_type]) for arena_type in ArenaType}}
    async def shutdown(self) -> None:
        logger.info('Shutting down Simulation Engine...')
        arena_ids = list(self.arenas.keys())
        for arena_id in arena_ids:
            await self.destroy_arena(arena_id)
        logger.info(f'Simulation Engine shutdown complete. Destroyed {len(arena_ids)} arenas')