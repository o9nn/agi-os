import asyncio
import logging
from typing import Dict, Any, Optional, Callable
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..', 'echo.kern'))
try:
    from environment_coupling import EnvironmentCouplingSystem, EnvironmentEvent, BehaviorAdaptation, AdaptationStrategy, create_default_coupling_system
except ImportError as e:
    logger = logging.getLogger(__name__)
    logger.error(f'Failed to import environment coupling system: {e}')
    class EnvironmentCouplingSystem:
        def __init__(self, *args, **kwargs):
            self.active = False
    class EnvironmentEvent:
        def __init__(self, *args, **kwargs):
            pass
    class BehaviorAdaptation:
        def __init__(self, *args, **kwargs):
            pass
logger = logging.getLogger(__name__)
class AAREnvironmentBridge:
    def __init__(self, bridge_id: str, coupling_system: Optional[EnvironmentCouplingSystem]=None):
        self.bridge_id = bridge_id
        self.coupling_system = coupling_system or create_default_coupling_system(f'{bridge_id}_coupling')
        self.active = False
        self.initialized = False
        self.arena_interface = None
        self.agent_interfaces: Dict[str, Any] = {}
        self.relation_interfaces: Dict[str, Any] = {}
        self.last_arena_state: Dict[str, Any] = {}
        self.agent_behavior_mappings: Dict[str, Dict[str, Any]] = {}
        self.state_updates_processed = 0
        self.adaptations_applied = 0
        self.integration_errors = 0
        logger.info(f'AAREnvironmentBridge {bridge_id} initialized')
    async def initialize_bridge(self, arena_interface=None, agent_interfaces: Optional[Dict[str, Any]]=None) -> bool:
        try:
            if arena_interface:
                self.arena_interface = arena_interface
            if agent_interfaces:
                self.agent_interfaces.update(agent_interfaces)
            if not await self.coupling_system.initialize():
                logger.error('Failed to initialize coupling system')
                return False
            for agent_id in self.agent_interfaces:
                success = self.coupling_system.register_agent(agent_id, {'source': 'aar_bridge'})
                if success:
                    callback = self._create_agent_adaptation_callback(agent_id)
                    self.coupling_system.register_adaptation_callback(agent_id, callback)
            if not self.coupling_system.start_coupling():
                logger.error('Failed to start coupling system')
                return False
            self.initialized = True
            logger.info(f'AAR Environment Bridge {self.bridge_id} initialized successfully')
            return True
        except Exception as e:
            logger.error(f'Failed to initialize AAR bridge: {e}')
            return False
    def _create_agent_adaptation_callback(self, agent_id: str) -> Callable:
        async def adaptation_callback(adaptation: BehaviorAdaptation) -> Dict[str, Any]:
            try:
                agent_interface = self.agent_interfaces.get(agent_id)
                if not agent_interface:
                    logger.warning(f'No interface found for agent {agent_id}')
                    return {'status': 'no_interface', 'effectiveness': 0.0}
                effectiveness = await self._apply_agent_behavior_adaptation(agent_interface, adaptation)
                self.adaptations_applied += 1
                return {'status': 'applied', 'effectiveness': effectiveness}
            except Exception as e:
                self.integration_errors += 1
                logger.error(f'Error in adaptation callback for {agent_id}: {e}')
                return {'status': 'error', 'effectiveness': 0.0}
        return adaptation_callback
    async def _apply_agent_behavior_adaptation(self, agent_interface: Any, adaptation: BehaviorAdaptation) -> float:
        try:
            adaptation_params = adaptation.parameters
            adaptation_type = adaptation.adaptation_type
            behavior_changes = self._map_adaptation_to_behavior(adaptation_params)
            if adaptation_type == AdaptationStrategy.IMMEDIATE:
                effectiveness = await self._apply_immediate_behavior_change(agent_interface, behavior_changes)
            elif adaptation_type == AdaptationStrategy.GRADUAL:
                effectiveness = await self._apply_gradual_behavior_change(agent_interface, behavior_changes)
            elif adaptation_type == AdaptationStrategy.THRESHOLD:
                effectiveness = await self._apply_threshold_behavior_change(agent_interface, behavior_changes)
            else:
                effectiveness = await self._apply_immediate_behavior_change(agent_interface, behavior_changes)
            logger.info(f'Applied adaptation {adaptation.adaptation_id} with effectiveness {effectiveness}')
            return effectiveness
        except Exception as e:
            logger.error(f'Failed to apply behavior adaptation: {e}')
            return 0.0
    def _map_adaptation_to_behavior(self, adaptation_params: Dict[str, Any]) -> Dict[str, Any]:
        behavior_changes = {}
        behavior_type = adaptation_params.get('behavior', 'default')
        intensity = adaptation_params.get('intensity', 'medium')
        if behavior_type == 'resource_seeking':
            behavior_changes = {'priority': 'resources', 'search_radius': 10.0 if intensity == 'high' else 5.0, 'movement_speed': 1.5 if intensity == 'high' else 1.0}
        elif behavior_type == 'collision_avoidance':
            behavior_changes = {'avoidance_distance': 5.0 if intensity == 'high' else 3.0, 'evasion_speed': 2.0 if intensity == 'high' else 1.2, 'caution_level': 0.8 if intensity == 'high' else 0.5}
        elif behavior_type == 'hazard_avoidance':
            behavior_changes = {'danger_sensitivity': 0.9 if intensity == 'high' else 0.6, 'flee_distance': 15.0 if intensity == 'high' else 10.0, 'alert_level': 'maximum' if intensity == 'high' else 'elevated'}
        else:
            behavior_changes = {'adaptation_type': behavior_type, 'intensity_modifier': 1.5 if intensity == 'high' else 1.0}
        return behavior_changes
    async def _apply_immediate_behavior_change(self, agent_interface: Any, behavior_changes: Dict[str, Any]) -> float:
        try:
            if hasattr(agent_interface, 'modify_behavior'):
                result = await agent_interface.modify_behavior(behavior_changes)
                return result.get('effectiveness', 0.8)
            if hasattr(agent_interface, 'update_state'):
                current_state = getattr(agent_interface, 'state', {})
                for key, value in behavior_changes.items():
                    current_state[key] = value
                result = await agent_interface.update_state(current_state)
                return result.get('effectiveness', 0.7) if isinstance(result, dict) else 0.7
            logger.info(f'Applied behavior changes: {behavior_changes}')
            return 0.6
        except Exception as e:
            logger.error(f'Error applying immediate behavior change: {e}')
            return 0.0
    async def _apply_gradual_behavior_change(self, agent_interface: Any, behavior_changes: Dict[str, Any]) -> float:
        try:
            agent_id = getattr(agent_interface, 'id', 'unknown')
            if agent_id not in self.agent_behavior_mappings:
                self.agent_behavior_mappings[agent_id] = {}
            self.agent_behavior_mappings[agent_id]['target_changes'] = behavior_changes
            self.agent_behavior_mappings[agent_id]['application_rate'] = 0.2
            self.agent_behavior_mappings[agent_id]['current_progress'] = 0.0
            effectiveness = await self._apply_behavior_increment(agent_interface, behavior_changes, 0.2)
            self.agent_behavior_mappings[agent_id]['current_progress'] = 0.2
            return effectiveness
        except Exception as e:
            logger.error(f'Error applying gradual behavior change: {e}')
            return 0.0
    async def _apply_threshold_behavior_change(self, agent_interface: Any, behavior_changes: Dict[str, Any]) -> float:
        try:
            current_state = getattr(agent_interface, 'state', {})
            energy_level = current_state.get('energy', 50)
            activity_level = current_state.get('activity', 0.5)
            threshold_met = energy_level > 30 and activity_level > 0.3
            if threshold_met:
                effectiveness = await self._apply_immediate_behavior_change(agent_interface, behavior_changes)
                logger.info(f'Threshold met - applied behavior changes with effectiveness {effectiveness}')
                return effectiveness
            else:
                logger.debug('Threshold not met - behavior changes deferred')
                return 0.1
        except Exception as e:
            logger.error(f'Error applying threshold behavior change: {e}')
            return 0.0
    async def _apply_behavior_increment(self, agent_interface: Any, behavior_changes: Dict[str, Any], increment_factor: float) -> float:
        try:
            incremental_changes = {}
            for key, value in behavior_changes.items():
                if isinstance(value, (int, float)):
                    incremental_changes[key] = value * increment_factor
                else:
                    incremental_changes[key] = value
            effectiveness = await self._apply_immediate_behavior_change(agent_interface, incremental_changes)
            return effectiveness * increment_factor
        except Exception as e:
            logger.error(f'Error applying behavior increment: {e}')
            return 0.0
    async def process_arena_state_update(self, arena_state: Dict[str, Any]) -> Dict[str, Any]:
        if not self.active or not self.coupling_system.active:
            return {'status': 'inactive'}
        try:
            coupling_result = await self.coupling_system.update_environment_state(arena_state)
            self.last_arena_state = arena_state.copy()
            self.state_updates_processed += 1
            await self._process_gradual_adaptations()
            logger.debug(f'Processed arena state update: {coupling_result}')
            return coupling_result
        except Exception as e:
            self.integration_errors += 1
            logger.error(f'Error processing arena state update: {e}')
            return {'status': 'error', 'error': str(e)}
    async def _process_gradual_adaptations(self) -> None:
        for agent_id, behavior_mapping in self.agent_behavior_mappings.items():
            if 'target_changes' not in behavior_mapping:
                continue
            current_progress = behavior_mapping.get('current_progress', 0.0)
            application_rate = behavior_mapping.get('application_rate', 0.2)
            if current_progress < 1.0:
                agent_interface = self.agent_interfaces.get(agent_id)
                if agent_interface:
                    target_changes = behavior_mapping['target_changes']
                    await self._apply_behavior_increment(agent_interface, target_changes, application_rate)
                    behavior_mapping['current_progress'] = min(1.0, current_progress + application_rate)
                    if behavior_mapping['current_progress'] >= 1.0:
                        logger.info(f'Completed gradual adaptation for agent {agent_id}')
    def start_bridge(self) -> bool:
        if not self.initialized:
            logger.error('Bridge not initialized - call initialize_bridge() first')
            return False
        self.active = True
        logger.info(f'AAR Environment Bridge {self.bridge_id} started')
        return True
    def stop_bridge(self) -> bool:
        self.active = False
        if self.coupling_system:
            self.coupling_system.stop_coupling()
        logger.info(f'AAR Environment Bridge {self.bridge_id} stopped')
        return True
    def get_bridge_status(self) -> Dict[str, Any]:
        coupling_status = {}
        if self.coupling_system:
            coupling_status = self.coupling_system.get_system_status()
        return {'bridge_id': self.bridge_id, 'active': self.active, 'initialized': self.initialized, 'arena_interface': self.arena_interface is not None, 'registered_agents': len(self.agent_interfaces), 'state_updates_processed': self.state_updates_processed, 'adaptations_applied': self.adaptations_applied, 'integration_errors': self.integration_errors, 'coupling_system': coupling_status}
class AAREnvironmentAdapter:
    def __init__(self):
        self.bridges: Dict[str, AAREnvironmentBridge] = {}
        self.default_bridge_id = None
    def create_bridge(self, bridge_id: str, arena_interface=None, agent_interfaces: Optional[Dict[str, Any]]=None) -> AAREnvironmentBridge:
        bridge = AAREnvironmentBridge(bridge_id)
        self.bridges[bridge_id] = bridge
        if not self.default_bridge_id:
            self.default_bridge_id = bridge_id
        if arena_interface or agent_interfaces:
            asyncio.create_task(bridge.initialize_bridge(arena_interface, agent_interfaces))
        return bridge
    def get_bridge(self, bridge_id: Optional[str]=None) -> Optional[AAREnvironmentBridge]:
        if bridge_id:
            return self.bridges.get(bridge_id)
        elif self.default_bridge_id:
            return self.bridges.get(self.default_bridge_id)
        return None
    async def connect_arena(self, arena_interface, bridge_id: Optional[str]=None) -> bool:
        bridge = self.get_bridge(bridge_id)
        if not bridge:
            bridge = self.create_bridge(bridge_id or 'default_arena_bridge')
        return await bridge.initialize_bridge(arena_interface=arena_interface)
    async def connect_agents(self, agent_interfaces: Dict[str, Any], bridge_id: Optional[str]=None) -> bool:
        bridge = self.get_bridge(bridge_id)
        if not bridge:
            bridge = self.create_bridge(bridge_id or 'default_agent_bridge')
        return await bridge.initialize_bridge(agent_interfaces=agent_interfaces)
    async def process_arena_update(self, arena_state: Dict[str, Any], bridge_id: Optional[str]=None) -> Dict[str, Any]:
        bridge = self.get_bridge(bridge_id)
        if not bridge:
            logger.warning('No bridge found for arena update')
            return {'status': 'no_bridge'}
        return await bridge.process_arena_state_update(arena_state)
    def get_adapter_status(self) -> Dict[str, Any]:
        bridge_statuses = {}
        for bridge_id, bridge in self.bridges.items():
            bridge_statuses[bridge_id] = bridge.get_bridge_status()
        return {'total_bridges': len(self.bridges), 'default_bridge': self.default_bridge_id, 'bridges': bridge_statuses}
aar_environment_adapter = AAREnvironmentAdapter()
async def initialize_aar_environment_coupling(arena_interface=None, agent_interfaces: Optional[Dict[str, Any]]=None, bridge_id: str='main_aar_bridge') -> bool:
    try:
        bridge = aar_environment_adapter.create_bridge(bridge_id)
        success = await bridge.initialize_bridge(arena_interface, agent_interfaces)
        if success:
            bridge.start_bridge()
            logger.info('AAR environment coupling initialized successfully')
            return True
        else:
            logger.error('Failed to initialize AAR environment coupling')
            return False
    except Exception as e:
        logger.error(f'Error initializing AAR environment coupling: {e}')
        return False
async def update_aar_environment_state(arena_state: Dict[str, Any], bridge_id: Optional[str]=None) -> Dict[str, Any]:
    return await aar_environment_adapter.process_arena_update(arena_state, bridge_id)
def get_aar_coupling_status() -> Dict[str, Any]:
    return aar_environment_adapter.get_adapter_status()