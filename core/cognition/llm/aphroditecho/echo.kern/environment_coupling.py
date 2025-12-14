"""
Environment Coupling System for Deep Tree Echo 4E Embodied AI

Implements real-time environment state integration, dynamic environment adaptation,
and context-sensitive behavior modification for agents in virtual environments.

This module provides the core coupling mechanisms between environmental state
changes and agent behavioral adaptations, fulfilling Task 2.2.1 of the
Phase 2 4E Embodied AI Framework.

Features:
- Real-time environment state monitoring and integration
- Dynamic environment adaptation mechanisms  
- Context-sensitive behavior modification
- Event-driven coupling between environments and agents
- Integration with existing AAR orchestration and DTESN components

Mathematical Foundation:
Based on OEIS A000081 tree enumeration principles for state space organization
and membrane computing paradigms for dynamic adaptation.
"""

import asyncio
import logging
import time
from dataclasses import dataclass, field
from enum import Enum
from typing import Dict, List, Any, Optional, Callable, Set


logger = logging.getLogger(__name__)


class CouplingType(Enum):
    """Types of environment-agent coupling mechanisms."""
    DIRECT = "direct"           # Immediate state propagation
    BUFFERED = "buffered"       # Buffered state updates
    REACTIVE = "reactive"       # Response to specific events
    PREDICTIVE = "predictive"   # Anticipatory adaptation
    CONTEXTUAL = "contextual"   # Context-sensitive coupling


class AdaptationStrategy(Enum):
    """Strategies for agent behavior adaptation."""
    IMMEDIATE = "immediate"     # Instant behavior change
    GRADUAL = "gradual"        # Smooth transition adaptation
    THRESHOLD = "threshold"     # Threshold-based adaptation
    LEARNING = "learning"       # Learning-based adaptation
    HYBRID = "hybrid"          # Multi-strategy adaptation


@dataclass
class EnvironmentEvent:
    """Represents an environmental state change event."""
    event_id: str
    timestamp: float
    event_type: str
    source: str
    data: Dict[str, Any]
    priority: int = 1
    processed: bool = False
    
    def __post_init__(self):
        if not self.timestamp:
            self.timestamp = time.time()


@dataclass  
class BehaviorAdaptation:
    """Represents a behavior adaptation directive."""
    adaptation_id: str
    target_agent: str
    adaptation_type: AdaptationStrategy
    parameters: Dict[str, Any]
    timestamp: float = field(default_factory=time.time)
    applied: bool = False
    effectiveness: float = 0.0


@dataclass
class ContextualState:
    """Represents contextual state information for coupling decisions."""
    context_id: str
    environment_state: Dict[str, Any]
    agent_states: Dict[str, Any]
    temporal_context: Dict[str, Any]
    spatial_context: Dict[str, Any]
    social_context: Dict[str, Any]
    timestamp: float = field(default_factory=time.time)


class EnvironmentStateMonitor:
    """
    Monitors and tracks environmental state changes in real-time.
    
    This component continuously observes environmental parameters and
    detects significant state changes that require agent adaptation.
    """
    
    def __init__(self, monitor_id: str, update_interval: float = 0.1):
        self.monitor_id = monitor_id
        self.update_interval = update_interval
        self.active = False
        self.last_update = 0.0
        
        # State tracking
        self.current_state: Dict[str, Any] = {}
        self.previous_state: Dict[str, Any] = {}
        self.state_history: List[Dict[str, Any]] = []
        self.max_history_size = 100
        
        # Event detection
        self.event_listeners: List[Callable] = []
        self.event_queue: List[EnvironmentEvent] = []
        self.detection_thresholds: Dict[str, float] = {}
        
        # Performance tracking
        self.update_count = 0
        self.event_count = 0
        
        logger.info(f"EnvironmentStateMonitor {monitor_id} initialized")
    
    def add_event_listener(self, listener: Callable[[EnvironmentEvent], None]) -> None:
        """Add a listener for environment events."""
        self.event_listeners.append(listener)
        logger.debug(f"Added event listener to monitor {self.monitor_id}")
    
    def set_detection_threshold(self, parameter: str, threshold: float) -> None:
        """Set detection threshold for a specific parameter."""
        self.detection_thresholds[parameter] = threshold
        logger.debug(f"Set threshold for {parameter}: {threshold}")
    
    def update_state(self, new_state: Dict[str, Any]) -> List[EnvironmentEvent]:
        """Update environmental state and detect changes."""
        if not self.active:
            return []
            
        current_time = time.time()
        self.previous_state = self.current_state.copy()
        self.current_state = new_state.copy()
        self.last_update = current_time
        self.update_count += 1
        
        # Store in history
        self.state_history.append({
            'timestamp': current_time,
            'state': new_state.copy()
        })
        
        # Trim history if needed
        if len(self.state_history) > self.max_history_size:
            self.state_history.pop(0)
        
        # Detect significant changes
        events = self._detect_changes(self.previous_state, new_state, current_time)
        
        # Notify listeners
        for event in events:
            self.event_queue.append(event)
            self.event_count += 1
            for listener in self.event_listeners:
                try:
                    listener(event)
                except Exception as e:
                    logger.error(f"Error in event listener: {e}")
        
        return events
    
    def _detect_changes(self, old_state: Dict[str, Any], new_state: Dict[str, Any], 
                       timestamp: float) -> List[EnvironmentEvent]:
        """Detect significant changes between states."""
        events = []
        
        # Check for new parameters
        for key in new_state:
            if key not in old_state:
                event = EnvironmentEvent(
                    event_id=f"{self.monitor_id}_new_{key}_{timestamp}",
                    timestamp=timestamp,
                    event_type="parameter_added",
                    source=self.monitor_id,
                    data={'parameter': key, 'value': new_state[key]}
                )
                events.append(event)
        
        # Check for removed parameters
        for key in old_state:
            if key not in new_state:
                event = EnvironmentEvent(
                    event_id=f"{self.monitor_id}_removed_{key}_{timestamp}",
                    timestamp=timestamp,
                    event_type="parameter_removed", 
                    source=self.monitor_id,
                    data={'parameter': key, 'old_value': old_state[key]}
                )
                events.append(event)
        
        # Check for significant value changes
        for key in old_state:
            if key in new_state:
                old_val = old_state[key]
                new_val = new_state[key]
                
                # Handle numeric comparisons
                if isinstance(old_val, (int, float)) and isinstance(new_val, (int, float)):
                    threshold = self.detection_thresholds.get(key, 0.1)
                    if abs(old_val - new_val) > threshold:
                        event = EnvironmentEvent(
                            event_id=f"{self.monitor_id}_change_{key}_{timestamp}",
                            timestamp=timestamp,
                            event_type="parameter_changed",
                            source=self.monitor_id,
                            data={
                                'parameter': key,
                                'old_value': old_val,
                                'new_value': new_val,
                                'change': new_val - old_val
                            }
                        )
                        events.append(event)
                
                # Handle other data types
                elif old_val != new_val:
                    event = EnvironmentEvent(
                        event_id=f"{self.monitor_id}_change_{key}_{timestamp}",
                        timestamp=timestamp,
                        event_type="parameter_changed",
                        source=self.monitor_id,
                        data={
                            'parameter': key,
                            'old_value': old_val,
                            'new_value': new_val
                        }
                    )
                    events.append(event)
        
        return events
    
    def start_monitoring(self) -> None:
        """Start the monitoring process."""
        self.active = True
        logger.info(f"Started monitoring for {self.monitor_id}")
    
    def stop_monitoring(self) -> None:
        """Stop the monitoring process."""
        self.active = False
        logger.info(f"Stopped monitoring for {self.monitor_id}")
    
    def get_monitoring_stats(self) -> Dict[str, Any]:
        """Get monitoring performance statistics."""
        return {
            'monitor_id': self.monitor_id,
            'active': self.active,
            'update_count': self.update_count,
            'event_count': self.event_count,
            'last_update': self.last_update,
            'listeners': len(self.event_listeners),
            'queue_size': len(self.event_queue),
            'history_size': len(self.state_history)
        }


class BehaviorAdaptationEngine:
    """
    Manages dynamic adaptation of agent behaviors based on environmental changes.
    
    This component processes environmental events and determines appropriate
    behavioral adaptations for agents, implementing various adaptation strategies.
    """
    
    def __init__(self, engine_id: str):
        self.engine_id = engine_id
        self.active = False
        
        # Adaptation management
        self.adaptation_strategies: Dict[str, AdaptationStrategy] = {}
        self.adaptation_queue: List[BehaviorAdaptation] = []
        self.adaptation_history: List[BehaviorAdaptation] = []
        self.max_history_size = 500
        
        # Agent management  
        self.registered_agents: Set[str] = set()
        self.agent_contexts: Dict[str, Dict[str, Any]] = {}
        
        # Adaptation rules and policies
        self.adaptation_rules: List[Dict[str, Any]] = []
        self.adaptation_callbacks: Dict[str, Callable] = {}
        
        # Performance tracking
        self.adaptations_processed = 0
        self.adaptations_successful = 0
        
        logger.info(f"BehaviorAdaptationEngine {engine_id} initialized")
    
    def register_agent(self, agent_id: str, initial_context: Optional[Dict[str, Any]] = None) -> None:
        """Register an agent for behavior adaptation."""
        self.registered_agents.add(agent_id)
        self.agent_contexts[agent_id] = initial_context or {}
        self.adaptation_strategies[agent_id] = AdaptationStrategy.HYBRID
        logger.info(f"Registered agent {agent_id} with adaptation engine")
    
    def unregister_agent(self, agent_id: str) -> None:
        """Unregister an agent from behavior adaptation."""
        self.registered_agents.discard(agent_id)
        self.agent_contexts.pop(agent_id, None)
        self.adaptation_strategies.pop(agent_id, None)
        logger.info(f"Unregistered agent {agent_id} from adaptation engine")
    
    def set_adaptation_strategy(self, agent_id: str, strategy: AdaptationStrategy) -> None:
        """Set the adaptation strategy for a specific agent."""
        if agent_id in self.registered_agents:
            self.adaptation_strategies[agent_id] = strategy
            logger.info(f"Set adaptation strategy for {agent_id}: {strategy.value}")
    
    def add_adaptation_rule(self, rule: Dict[str, Any]) -> None:
        """Add an adaptation rule for automatic behavior changes."""
        required_keys = ['name', 'condition', 'action', 'priority']
        if all(key in rule for key in required_keys):
            self.adaptation_rules.append(rule)
            # Sort by priority
            self.adaptation_rules.sort(key=lambda r: r.get('priority', 0), reverse=True)
            logger.info(f"Added adaptation rule: {rule['name']}")
    
    def process_environment_event(self, event: EnvironmentEvent) -> List[BehaviorAdaptation]:
        """Process an environmental event and generate adaptations."""
        if not self.active:
            return []
            
        adaptations = []
        
        # Apply adaptation rules
        for rule in self.adaptation_rules:
            if self._evaluate_rule_condition(rule, event):
                adaptation = self._execute_rule_action(rule, event)
                if adaptation:
                    adaptations.append(adaptation)
        
        # Queue adaptations for processing
        for adaptation in adaptations:
            self.adaptation_queue.append(adaptation)
        
        return adaptations
    
    def _evaluate_rule_condition(self, rule: Dict[str, Any], event: EnvironmentEvent) -> bool:
        """Evaluate whether a rule condition matches an event."""
        condition = rule.get('condition', {})
        
        # Check event type
        if 'event_type' in condition:
            if event.event_type != condition['event_type']:
                return False
        
        # Check source
        if 'source' in condition:
            if event.source != condition['source']:
                return False
        
        # Check data conditions
        if 'data' in condition:
            for key, expected_val in condition['data'].items():
                if key not in event.data or event.data[key] != expected_val:
                    return False
        
        # Check priority
        if 'min_priority' in condition:
            if event.priority < condition['min_priority']:
                return False
        
        return True
    
    def _execute_rule_action(self, rule: Dict[str, Any], event: EnvironmentEvent) -> Optional[BehaviorAdaptation]:
        """Execute the action specified in an adaptation rule."""
        action = rule.get('action', {})
        
        # Determine target agents
        target_agents = []
        target_spec = action.get('target', 'all')
        
        if target_spec == 'all':
            target_agents = list(self.registered_agents)
        elif isinstance(target_spec, str) and target_spec in self.registered_agents:
            target_agents = [target_spec]
        elif isinstance(target_spec, list):
            target_agents = [a for a in target_spec if a in self.registered_agents]
        
        # Create adaptations for each target agent
        adaptations = []
        for agent_id in target_agents:
            strategy = self.adaptation_strategies.get(agent_id, AdaptationStrategy.IMMEDIATE)
            
            adaptation = BehaviorAdaptation(
                adaptation_id=f"{rule['name']}_{agent_id}_{event.timestamp}",
                target_agent=agent_id,
                adaptation_type=strategy,
                parameters={
                    'rule_name': rule['name'],
                    'event_data': event.data,
                    'adaptation_params': action.get('parameters', {})
                }
            )
            adaptations.append(adaptation)
        
        return adaptations[0] if adaptations else None
    
    async def process_adaptation_queue(self) -> Dict[str, Any]:
        """Process queued behavior adaptations."""
        if not self.adaptation_queue:
            return {'processed': 0, 'successful': 0}
        
        processed = 0
        successful = 0
        
        while self.adaptation_queue and processed < 10:  # Process up to 10 per batch
            adaptation = self.adaptation_queue.pop(0)
            processed += 1
            self.adaptations_processed += 1
            
            try:
                success = await self._apply_adaptation(adaptation)
                if success:
                    successful += 1
                    self.adaptations_successful += 1
                    adaptation.applied = True
                
                # Add to history
                self.adaptation_history.append(adaptation)
                
                # Trim history if needed
                if len(self.adaptation_history) > self.max_history_size:
                    self.adaptation_history.pop(0)
                    
            except Exception as e:
                logger.error(f"Error processing adaptation {adaptation.adaptation_id}: {e}")
        
        return {'processed': processed, 'successful': successful}
    
    async def _apply_adaptation(self, adaptation: BehaviorAdaptation) -> bool:
        """Apply a specific behavior adaptation."""
        agent_id = adaptation.target_agent
        
        # Check if we have a callback for this agent
        if agent_id in self.adaptation_callbacks:
            try:
                callback = self.adaptation_callbacks[agent_id]
                result = await callback(adaptation) if asyncio.iscoroutinefunction(callback) else callback(adaptation)
                adaptation.effectiveness = result.get('effectiveness', 1.0) if isinstance(result, dict) else 1.0
                return True
            except Exception as e:
                logger.error(f"Adaptation callback failed for {agent_id}: {e}")
                return False
        
        # Default adaptation logic
        logger.info(f"Applied adaptation {adaptation.adaptation_id} to agent {agent_id}")
        adaptation.effectiveness = 0.8  # Default effectiveness
        return True
    
    def register_adaptation_callback(self, agent_id: str, callback: Callable) -> None:
        """Register a callback function for agent adaptations."""
        self.adaptation_callbacks[agent_id] = callback
        logger.info(f"Registered adaptation callback for agent {agent_id}")
    
    def get_adaptation_stats(self) -> Dict[str, Any]:
        """Get adaptation engine performance statistics."""
        return {
            'engine_id': self.engine_id,
            'active': self.active,
            'registered_agents': len(self.registered_agents),
            'adaptations_processed': self.adaptations_processed,
            'adaptations_successful': self.adaptations_successful,
            'success_rate': self.adaptations_successful / max(1, self.adaptations_processed),
            'queue_size': len(self.adaptation_queue),
            'rules_count': len(self.adaptation_rules)
        }


class ContextSensitivityManager:
    """
    Manages context-sensitive behavior modification based on environmental state.
    
    This component analyzes contextual information to determine appropriate
    behavior modifications that are sensitive to current environmental conditions.
    """
    
    def __init__(self, manager_id: str):
        self.manager_id = manager_id
        self.active = False
        
        # Context management
        self.current_contexts: Dict[str, ContextualState] = {}
        self.context_history: List[ContextualState] = []
        self.max_context_history = 100
        
        # Context analysis
        self.context_analyzers: Dict[str, Callable] = {}
        self.context_thresholds: Dict[str, Dict[str, float]] = {}
        
        # Sensitivity settings
        self.sensitivity_profiles: Dict[str, Dict[str, float]] = {}
        self.default_sensitivity = {
            'environmental': 0.5,
            'social': 0.3,
            'temporal': 0.7,
            'spatial': 0.4
        }
        
        logger.info(f"ContextSensitivityManager {manager_id} initialized")
    
    def add_context_analyzer(self, context_type: str, analyzer: Callable) -> None:
        """Add a context analyzer for a specific type of context."""
        self.context_analyzers[context_type] = analyzer
        logger.info(f"Added context analyzer for {context_type}")
    
    def set_sensitivity_profile(self, profile_name: str, sensitivities: Dict[str, float]) -> None:
        """Set sensitivity profile for context analysis."""
        self.sensitivity_profiles[profile_name] = sensitivities
        logger.info(f"Set sensitivity profile {profile_name}")
    
    def analyze_context(self, environment_state: Dict[str, Any], 
                       agent_states: Dict[str, Any]) -> ContextualState:
        """Analyze current context from environment and agent states."""
        current_time = time.time()
        
        # Generate context ID
        context_id = f"context_{self.manager_id}_{current_time}"
        
        # Analyze different context dimensions
        temporal_context = self._analyze_temporal_context(current_time)
        spatial_context = self._analyze_spatial_context(environment_state, agent_states)
        social_context = self._analyze_social_context(agent_states)
        
        # Create contextual state
        context = ContextualState(
            context_id=context_id,
            environment_state=environment_state.copy(),
            agent_states=agent_states.copy(),
            temporal_context=temporal_context,
            spatial_context=spatial_context,
            social_context=social_context,
            timestamp=current_time
        )
        
        # Store context
        self.current_contexts[context_id] = context
        self.context_history.append(context)
        
        # Trim history
        if len(self.context_history) > self.max_context_history:
            self.context_history.pop(0)
        
        return context
    
    def _analyze_temporal_context(self, current_time: float) -> Dict[str, Any]:
        """Analyze temporal context factors."""
        temporal_context = {
            'timestamp': current_time,
            'time_since_start': current_time - (self.context_history[0].timestamp if self.context_history else current_time),
            'context_count': len(self.context_history)
        }
        
        # Calculate temporal trends if we have history
        if len(self.context_history) >= 2:
            recent_contexts = self.context_history[-5:]  # Last 5 contexts
            time_intervals = []
            for i in range(1, len(recent_contexts)):
                interval = recent_contexts[i].timestamp - recent_contexts[i-1].timestamp
                time_intervals.append(interval)
            
            if time_intervals:
                temporal_context['avg_interval'] = sum(time_intervals) / len(time_intervals)
                temporal_context['temporal_stability'] = 1.0 - (max(time_intervals) - min(time_intervals)) / max(time_intervals)
        
        return temporal_context
    
    def _analyze_spatial_context(self, environment_state: Dict[str, Any], 
                                agent_states: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze spatial context factors."""
        spatial_context = {
            'environment_size': len(environment_state),
            'agent_count': len(agent_states),
            'density': len(agent_states) / max(1, len(environment_state))
        }
        
        # Analyze spatial distribution if position information is available
        agent_positions = []
        for agent_id, agent_data in agent_states.items():
            if 'position' in agent_data:
                agent_positions.append(agent_data['position'])
        
        if agent_positions and len(agent_positions) > 1:
            # Calculate spatial clustering (simplified)
            distances = []
            for i in range(len(agent_positions)):
                for j in range(i + 1, len(agent_positions)):
                    pos1, pos2 = agent_positions[i], agent_positions[j]
                    if isinstance(pos1, (list, tuple)) and isinstance(pos2, (list, tuple)):
                        # Simple Euclidean distance calculation
                        dist = sum((a - b) ** 2 for a, b in zip(pos1, pos2)) ** 0.5
                        distances.append(dist)
            
            if distances:
                spatial_context['avg_distance'] = sum(distances) / len(distances)
                spatial_context['spatial_clustering'] = 1.0 / (1.0 + spatial_context['avg_distance'])
        
        return spatial_context
    
    def _analyze_social_context(self, agent_states: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze social context factors."""
        social_context = {
            'agent_count': len(agent_states),
            'active_agents': sum(1 for state in agent_states.values() if state.get('active', True))
        }
        
        # Analyze agent interactions if interaction data is available
        interactions = 0
        for agent_data in agent_states.values():
            if 'interactions' in agent_data:
                interactions += len(agent_data['interactions'])
        
        social_context['total_interactions'] = interactions
        social_context['interaction_density'] = interactions / max(1, len(agent_states))
        
        return social_context
    
    def evaluate_context_sensitivity(self, context: ContextualState, 
                                   sensitivity_profile: str = 'default') -> Dict[str, float]:
        """Evaluate context sensitivity scores for different dimensions."""
        # Get sensitivity profile
        if sensitivity_profile in self.sensitivity_profiles:
            sensitivities = self.sensitivity_profiles[sensitivity_profile]
        else:
            sensitivities = self.default_sensitivity
        
        # Calculate context sensitivity scores
        sensitivity_scores = {}
        
        # Environmental sensitivity
        env_complexity = len(context.environment_state) / 10.0  # Normalized complexity
        sensitivity_scores['environmental'] = min(1.0, env_complexity * sensitivities['environmental'])
        
        # Social sensitivity  
        social_factor = context.social_context.get('interaction_density', 0.0)
        sensitivity_scores['social'] = min(1.0, social_factor * sensitivities['social'])
        
        # Temporal sensitivity
        temporal_factor = context.temporal_context.get('temporal_stability', 1.0)
        sensitivity_scores['temporal'] = (1.0 - temporal_factor) * sensitivities['temporal']
        
        # Spatial sensitivity
        spatial_factor = context.spatial_context.get('spatial_clustering', 0.5)
        sensitivity_scores['spatial'] = spatial_factor * sensitivities['spatial']
        
        return sensitivity_scores
    
    def get_context_stats(self) -> Dict[str, Any]:
        """Get context sensitivity manager statistics."""
        return {
            'manager_id': self.manager_id,
            'active': self.active,
            'current_contexts': len(self.current_contexts),
            'context_history_size': len(self.context_history),
            'analyzer_count': len(self.context_analyzers),
            'sensitivity_profiles': len(self.sensitivity_profiles)
        }


class EnvironmentCouplingSystem:
    """
    Main orchestrator for environment coupling in the 4E Embodied AI Framework.
    
    This system integrates environment monitoring, behavior adaptation, and
    context sensitivity management to provide real-time environment coupling
    capabilities for agents in virtual environments.
    """
    
    def __init__(self, system_id: str, config: Optional[Dict[str, Any]] = None):
        self.system_id = system_id
        self.config = config or {}
        self.active = False
        self.initialized = False
        
        # Core components
        self.state_monitor = EnvironmentStateMonitor(
            f"{system_id}_monitor",
            self.config.get('monitor_interval', 0.1)
        )
        self.adaptation_engine = BehaviorAdaptationEngine(f"{system_id}_adaptation")
        self.context_manager = ContextSensitivityManager(f"{system_id}_context")
        
        # System state
        self.last_coupling_update = 0.0
        self.coupling_events: List[Dict[str, Any]] = []
        self.max_event_history = 1000
        
        # Performance tracking
        self.coupling_cycles = 0
        self.successful_adaptations = 0
        self.total_events_processed = 0
        
        # Integration points
        self.external_systems: Dict[str, Any] = {}
        
        logger.info(f"EnvironmentCouplingSystem {system_id} created")
    
    async def initialize(self) -> bool:
        """Initialize the environment coupling system."""
        try:
            # Set up event coupling between components
            self.state_monitor.add_event_listener(self._handle_environment_event)
            
            # Configure default adaptation rules
            self._setup_default_adaptation_rules()
            
            # Configure context sensitivity
            self._setup_default_sensitivity_profiles()
            
            self.initialized = True
            logger.info(f"EnvironmentCouplingSystem {self.system_id} initialized successfully")
            return True
            
        except Exception as e:
            logger.error(f"Failed to initialize EnvironmentCouplingSystem: {e}")
            return False
    
    def _setup_default_adaptation_rules(self) -> None:
        """Set up default adaptation rules for common scenarios."""
        # Rule for resource depletion
        self.adaptation_engine.add_adaptation_rule({
            'name': 'resource_depletion_response',
            'condition': {
                'event_type': 'parameter_changed',
                'data': {'parameter': 'resources'}
            },
            'action': {
                'target': 'all',
                'parameters': {
                    'behavior': 'resource_seeking',
                    'intensity': 'high'
                }
            },
            'priority': 8
        })
        
        # Rule for agent collision
        self.adaptation_engine.add_adaptation_rule({
            'name': 'collision_avoidance',
            'condition': {
                'event_type': 'parameter_changed',
                'data': {'parameter': 'agent_positions'}
            },
            'action': {
                'target': 'all', 
                'parameters': {
                    'behavior': 'collision_avoidance',
                    'intensity': 'medium'
                }
            },
            'priority': 9
        })
        
        # Rule for environmental hazard
        self.adaptation_engine.add_adaptation_rule({
            'name': 'hazard_response',
            'condition': {
                'event_type': 'parameter_added',
                'data': {'parameter': 'hazard'}
            },
            'action': {
                'target': 'all',
                'parameters': {
                    'behavior': 'hazard_avoidance',
                    'intensity': 'high'
                }
            },
            'priority': 10
        })
    
    def _setup_default_sensitivity_profiles(self) -> None:
        """Set up default context sensitivity profiles."""
        # High sensitivity profile
        self.context_manager.set_sensitivity_profile('high_sensitivity', {
            'environmental': 0.8,
            'social': 0.7,
            'temporal': 0.9,
            'spatial': 0.8
        })
        
        # Low sensitivity profile
        self.context_manager.set_sensitivity_profile('low_sensitivity', {
            'environmental': 0.3,
            'social': 0.2,
            'temporal': 0.4,
            'spatial': 0.3
        })
        
        # Balanced profile
        self.context_manager.set_sensitivity_profile('balanced', {
            'environmental': 0.5,
            'social': 0.5,
            'temporal': 0.6,
            'spatial': 0.5
        })
    
    async def _handle_environment_event(self, event: EnvironmentEvent) -> None:
        """Handle environment events from the state monitor."""
        if not self.active:
            return
            
        try:
            # Log the event
            self.coupling_events.append({
                'timestamp': event.timestamp,
                'event_id': event.event_id,
                'event_type': event.event_type,
                'source': event.source
            })
            
            # Trim event history
            if len(self.coupling_events) > self.max_event_history:
                self.coupling_events.pop(0)
            
            # Process event through adaptation engine
            adaptations = self.adaptation_engine.process_environment_event(event)
            
            # Update counters
            self.total_events_processed += 1
            
            logger.debug(f"Processed environment event {event.event_id}, generated {len(adaptations)} adaptations")
            
        except Exception as e:
            logger.error(f"Error handling environment event {event.event_id}: {e}")
    
    def register_agent(self, agent_id: str, agent_context: Optional[Dict[str, Any]] = None) -> bool:
        """Register an agent with the coupling system."""
        try:
            self.adaptation_engine.register_agent(agent_id, agent_context)
            logger.info(f"Agent {agent_id} registered with environment coupling system")
            return True
        except Exception as e:
            logger.error(f"Failed to register agent {agent_id}: {e}")
            return False
    
    def unregister_agent(self, agent_id: str) -> bool:
        """Unregister an agent from the coupling system."""
        try:
            self.adaptation_engine.unregister_agent(agent_id)
            logger.info(f"Agent {agent_id} unregistered from environment coupling system")
            return True
        except Exception as e:
            logger.error(f"Failed to unregister agent {agent_id}: {e}")
            return False
    
    def register_adaptation_callback(self, agent_id: str, callback: Callable) -> bool:
        """Register a callback for agent behavior adaptations."""
        try:
            self.adaptation_engine.register_adaptation_callback(agent_id, callback)
            return True
        except Exception as e:
            logger.error(f"Failed to register adaptation callback for {agent_id}: {e}")
            return False
    
    async def update_environment_state(self, new_state: Dict[str, Any]) -> Dict[str, Any]:
        """Update environment state and trigger coupling processes."""
        if not self.active:
            return {'status': 'inactive', 'events': 0, 'adaptations': 0}
        
        try:
            # Update state monitor
            events = self.state_monitor.update_state(new_state)
            
            # Analyze context
            agent_states = self.config.get('current_agent_states', {})
            context = self.context_manager.analyze_context(new_state, agent_states)
            
            # Process adaptation queue
            adaptation_result = await self.adaptation_engine.process_adaptation_queue()
            
            # Update performance tracking
            self.coupling_cycles += 1
            self.successful_adaptations += adaptation_result.get('successful', 0)
            self.last_coupling_update = time.time()
            
            return {
                'status': 'success',
                'events': len(events),
                'adaptations': adaptation_result.get('processed', 0),
                'context_id': context.context_id,
                'timestamp': self.last_coupling_update
            }
            
        except Exception as e:
            logger.error(f"Error updating environment state: {e}")
            return {'status': 'error', 'error': str(e)}
    
    def start_coupling(self) -> bool:
        """Start the environment coupling system."""
        if not self.initialized:
            logger.error("Cannot start coupling system - not initialized")
            return False
        
        try:
            self.state_monitor.start_monitoring()
            self.adaptation_engine.active = True
            self.context_manager.active = True
            self.active = True
            
            logger.info(f"Environment coupling system {self.system_id} started")
            return True
        except Exception as e:
            logger.error(f"Failed to start coupling system: {e}")
            return False
    
    def stop_coupling(self) -> bool:
        """Stop the environment coupling system."""
        try:
            self.state_monitor.stop_monitoring()
            self.adaptation_engine.active = False
            self.context_manager.active = False
            self.active = False
            
            logger.info(f"Environment coupling system {self.system_id} stopped")
            return True
        except Exception as e:
            logger.error(f"Failed to stop coupling system: {e}")
            return False
    
    def get_system_status(self) -> Dict[str, Any]:
        """Get comprehensive system status information."""
        monitor_stats = self.state_monitor.get_monitoring_stats()
        adaptation_stats = self.adaptation_engine.get_adaptation_stats()
        context_stats = self.context_manager.get_context_stats()
        
        return {
            'system_id': self.system_id,
            'active': self.active,
            'initialized': self.initialized,
            'last_update': self.last_coupling_update,
            'coupling_cycles': self.coupling_cycles,
            'total_events_processed': self.total_events_processed,
            'successful_adaptations': self.successful_adaptations,
            'success_rate': self.successful_adaptations / max(1, self.coupling_cycles),
            'components': {
                'monitor': monitor_stats,
                'adaptation': adaptation_stats,
                'context': context_stats
            }
        }
    
    def integrate_external_system(self, system_name: str, system_interface: Any) -> bool:
        """Integrate with external systems (AAR, Arena, etc.)."""
        try:
            self.external_systems[system_name] = system_interface
            logger.info(f"Integrated external system: {system_name}")
            return True
        except Exception as e:
            logger.error(f"Failed to integrate external system {system_name}: {e}")
            return False


# Utility functions for integration with existing systems

def create_default_coupling_system(system_id: str) -> EnvironmentCouplingSystem:
    """Create a default environment coupling system with standard configuration."""
    config = {
        'monitor_interval': 0.1,
        'max_adaptations_per_cycle': 10,
        'adaptation_timeout': 5.0,
        'context_sensitivity': 'balanced'
    }
    
    system = EnvironmentCouplingSystem(system_id, config)
    return system


async def initialize_coupling_for_aar(coupling_system: EnvironmentCouplingSystem, 
                                     aar_arena, aar_agents: List[str]) -> bool:
    """Initialize environment coupling for AAR (Agent-Arena-Relation) system."""
    try:
        # Initialize the coupling system
        if not await coupling_system.initialize():
            return False
        
        # Register AAR agents
        for agent_id in aar_agents:
            coupling_system.register_agent(agent_id, {'source': 'aar'})
        
        # Integrate arena system
        coupling_system.integrate_external_system('aar_arena', aar_arena)
        
        # Start coupling
        if not coupling_system.start_coupling():
            return False
        
        logger.info("Environment coupling initialized for AAR system")
        return True
        
    except Exception as e:
        logger.error(f"Failed to initialize AAR coupling: {e}")
        return False


def create_coupling_adapter(coupling_system: EnvironmentCouplingSystem) -> Dict[str, Callable]:
    """Create adapter functions for integrating with existing arena/agent systems."""
    
    async def arena_state_adapter(arena_state: Dict[str, Any]) -> Dict[str, Any]:
        """Adapter function to connect arena state updates to coupling system."""
        return await coupling_system.update_environment_state(arena_state)
    
    def agent_behavior_adapter(agent_id: str, adaptation: BehaviorAdaptation) -> Dict[str, Any]:
        """Adapter function to handle behavior adaptations for agents."""
        # This would integrate with existing agent behavior systems
        logger.info(f"Applying adaptation {adaptation.adaptation_id} to agent {agent_id}")
        return {'status': 'applied', 'effectiveness': 0.8}
    
    return {
        'arena_state_adapter': arena_state_adapter,
        'agent_behavior_adapter': agent_behavior_adapter
    }