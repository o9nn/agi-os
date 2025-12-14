"""
Agent Manager Component

Manages the lifecycle of AI agents in the AAR orchestration system.
Provides agent spawning, evolution, termination, and resource management.

Enhanced with advanced performance optimization, load balancing, and monitoring.
"""

import asyncio
import logging
import time
import uuid
from typing import Dict, List, Any, Optional, Set
from dataclasses import dataclass, field
from enum import Enum

from .agent_performance_optimizer import AgentPerformanceOptimizer, OptimizationStrategy
import contextlib

logger = logging.getLogger(__name__)


class AgentStatus(Enum):
    """Agent lifecycle status."""
    INITIALIZING = "initializing"
    ACTIVE = "active"
    IDLE = "idle"
    BUSY = "busy"
    EVOLVING = "evolving"
    TERMINATING = "terminating"
    TERMINATED = "terminated"
    ERROR = "error"


@dataclass
class AgentCapabilities:
    """Agent capability specification."""
    reasoning: bool = True
    multimodal: bool = False
    memory_enabled: bool = True
    learning_enabled: bool = True
    collaboration: bool = True
    specialized_domains: List[str] = field(default_factory=list)
    max_context_length: int = 4096
    processing_power: float = 1.0  # Relative processing capability


@dataclass
class AgentMetrics:
    """Agent performance metrics."""
    requests_processed: int = 0
    avg_response_time: float = 0.0
    success_rate: float = 1.0
    collaboration_score: float = 0.5
    evolution_generation: int = 1
    energy_level: float = 100.0
    last_activity: float = field(default_factory=time.time)


class Agent:
    """Represents an AI agent in the AAR system."""
    
    def __init__(self, 
                 agent_id: str,
                 capabilities: AgentCapabilities,
                 initial_context: Optional[Dict[str, Any]] = None):
        self.id = agent_id
        self.capabilities = capabilities
        self.status = AgentStatus.INITIALIZING
        self.metrics = AgentMetrics()
        self.context = initial_context or {}
        self.memory = []
        self.relations = set()
        self.current_task = None
        self.created_at = time.time()
        
        # Agent state for simulation
        self.position = (0.0, 0.0, 0.0)  # 3D position in arena
        self.orientation = (0.0, 0.0, 1.0)  # 3D orientation vector
        self.perception_range = 10.0
        self.action_queue = asyncio.Queue()
        
        # Social cognition capabilities
        self.cognitive_profile = {
            'capabilities': {
                'reasoning': capabilities.reasoning,
                'multimodal': capabilities.multimodal,
                'memory_enabled': capabilities.memory_enabled,
                'learning_enabled': capabilities.learning_enabled,
                'collaboration': capabilities.collaboration
            },
            'specializations': capabilities.specialized_domains,
            'memory_capacity': getattr(capabilities, 'max_context_length', 4096),
            'processing_bandwidth': capabilities.processing_power,
            'sharing_preferences': {
                'default_sharing_mode': 'broadcast',
                'trust_threshold': 0.5,
                'collaboration_willingness': 0.8 if capabilities.collaboration else 0.3
            }
        }
        
        # Social cognition state
        self.shared_resources = set()  # Resource IDs this agent has shared
        self.accessed_resources = set()  # Resource IDs this agent has accessed
        self.active_collaborations = set()  # Collaboration IDs this agent participates in
        self.communication_history = []  # Recent communication events
        self.trust_network = {}  # agent_id -> trust_score mapping
        
    async def initialize(self) -> None:
        """Initialize the agent."""
        logger.debug(f"Initializing agent {self.id}")
        # Perform initialization tasks
        await asyncio.sleep(0.1)  # Simulate initialization time
        self.status = AgentStatus.ACTIVE
        self.metrics.last_activity = time.time()
        logger.info(f"Agent {self.id} initialized successfully")
    
    async def process_request(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """Process a request and return result."""
        start_time = time.time()
        self.status = AgentStatus.BUSY
        self.current_task = request.get('task_id')
        
        try:
            # Simulate request processing
            processing_time = request.get('complexity', 1.0) * 0.1
            await asyncio.sleep(processing_time)
            
            # Generate response based on capabilities
            result = await self._generate_response(request)
            
            # Update metrics
            self._update_metrics(start_time, True)
            
            self.status = AgentStatus.ACTIVE
            self.current_task = None
            
            return result
            
        except Exception as e:
            logger.error(f"Agent {self.id} failed to process request: {e}")
            self._update_metrics(start_time, False)
            self.status = AgentStatus.ERROR
            self.current_task = None
            raise
    
    async def _generate_response(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """Generate response based on agent capabilities."""
        response = {
            'agent_id': self.id,
            'task_id': request.get('task_id'),
            'result': f"Processed by agent {self.id}",
            'confidence': 0.8 + (self.metrics.success_rate * 0.2),
            'processing_time': time.time() - self.metrics.last_activity,
            'capabilities_used': []
        }
        
        # Add capability-specific processing
        if self.capabilities.reasoning and 'reasoning' in request.get('features', []):
            response['capabilities_used'].append('reasoning')
            response['confidence'] += 0.1
        
        if self.capabilities.multimodal and 'multimodal' in request.get('features', []):
            response['capabilities_used'].append('multimodal')
            response['result'] += " (multimodal)"
        
        return response
    
    def _update_metrics(self, start_time: float, success: bool) -> None:
        """Update agent performance metrics."""
        response_time = time.time() - start_time
        
        # Update average response time
        total_requests = self.metrics.requests_processed
        current_avg = self.metrics.avg_response_time
        
        new_avg = ((current_avg * total_requests) + response_time) / (total_requests + 1)
        self.metrics.avg_response_time = new_avg
        
        # Update success rate
        if success:
            self.metrics.success_rate = ((self.metrics.success_rate * total_requests) + 1.0) / (total_requests + 1)
        else:
            self.metrics.success_rate = (self.metrics.success_rate * total_requests) / (total_requests + 1)
        
        self.metrics.requests_processed += 1
        self.metrics.last_activity = time.time()
        
        # Energy level management
        energy_cost = response_time * 10  # Energy cost based on processing time
        self.metrics.energy_level = max(0, self.metrics.energy_level - energy_cost)
    
    async def evolve(self, evolution_data: Dict[str, Any]) -> None:
        """Evolve agent based on performance and feedback."""
        if not self.capabilities.learning_enabled:
            return
        
        self.status = AgentStatus.EVOLVING
        logger.info(f"Agent {self.id} beginning evolution process")
        
        # Simulate evolution process
        await asyncio.sleep(0.5)
        
        # Update capabilities based on evolution data
        performance_score = evolution_data.get('performance_score', 0.5)
        
        if performance_score > 0.8:
            # Improve processing power
            self.capabilities.processing_power = min(2.0, self.capabilities.processing_power * 1.1)
            
        # Increment generation
        self.metrics.evolution_generation += 1
        
        self.status = AgentStatus.ACTIVE
        logger.info(f"Agent {self.id} evolution complete - Generation {self.metrics.evolution_generation}")
    
    async def terminate(self) -> None:
        """Gracefully terminate the agent."""
        logger.info(f"Terminating agent {self.id}")
        self.status = AgentStatus.TERMINATING
        
        # Complete current task if any
        if self.current_task:
            await asyncio.sleep(0.1)  # Allow task completion
        
        # Cleanup resources
        self.relations.clear()
        self.memory.clear()
        
        self.status = AgentStatus.TERMINATED
        logger.info(f"Agent {self.id} terminated successfully")
    
    def add_relation(self, other_agent_id: str) -> None:
        """Add relation to another agent."""
        self.relations.add(other_agent_id)
    
    def remove_relation(self, other_agent_id: str) -> None:
        """Remove relation to another agent."""
        self.relations.discard(other_agent_id)
    
    # Social Cognition Methods
    
    async def share_cognitive_resource(self, 
                                     resource_type: str, 
                                     data: Dict[str, Any], 
                                     sharing_mode: str = "broadcast") -> str:
        """Share a cognitive resource with other agents."""
        # This would integrate with the SocialCognitionManager
        # For now, we simulate the sharing
        resource_id = f"resource_{uuid.uuid4().hex[:8]}"
        self.shared_resources.add(resource_id)
        
        # Update social cognition metrics
        self.metrics.collaboration_score += 0.1
        
        logger.info(f"Agent {self.id} shared {resource_type} resource {resource_id}")
        return resource_id
    
    async def access_shared_resource(self, resource_id: str) -> Optional[Dict[str, Any]]:
        """Access a shared cognitive resource."""
        # This would integrate with the SocialCognitionManager
        # For now, we simulate the access
        self.accessed_resources.add(resource_id)
        
        # Update metrics
        self.metrics.collaboration_score += 0.05
        
        logger.debug(f"Agent {self.id} accessed shared resource {resource_id}")
        
        return {
            'resource_id': resource_id,
            'access_time': time.time(),
            'data': {'placeholder': 'simulated_data'}
        }
    
    async def participate_in_collaboration(self, 
                                         collaboration_id: str, 
                                         contribution: Dict[str, Any]) -> bool:
        """Participate in a collaborative problem-solving session."""
        self.active_collaborations.add(collaboration_id)
        
        # Update collaboration metrics
        self.metrics.collaboration_score += 0.2
        
        logger.info(f"Agent {self.id} participating in collaboration {collaboration_id}")
        return True
    
    async def communicate_with_agent(self, 
                                   target_agent_id: str, 
                                   message_type: str, 
                                   content: Dict[str, Any]) -> bool:
        """Send structured message to another agent."""
        # This would integrate with CommunicationProtocols
        # For now, we simulate the communication
        communication_event = {
            'timestamp': time.time(),
            'target_agent': target_agent_id,
            'message_type': message_type,
            'content_summary': str(content)[:100]  # First 100 chars
        }
        
        self.communication_history.append(communication_event)
        
        # Keep only recent communication history
        if len(self.communication_history) > 50:
            self.communication_history = self.communication_history[-50:]
        
        # Update trust network
        if target_agent_id not in self.trust_network:
            self.trust_network[target_agent_id] = 0.5  # Neutral trust
        
        # Successful communication slightly increases trust
        self.trust_network[target_agent_id] = min(1.0, self.trust_network[target_agent_id] + 0.01)
        
        logger.debug(f"Agent {self.id} sent {message_type} message to {target_agent_id}")
        return True
    
    def update_trust_score(self, agent_id: str, interaction_success: bool, impact: float = 0.1) -> None:
        """Update trust score for another agent based on interaction outcome."""
        if agent_id not in self.trust_network:
            self.trust_network[agent_id] = 0.5  # Start with neutral trust
        
        if interaction_success:
            self.trust_network[agent_id] = min(1.0, self.trust_network[agent_id] + impact)
        else:
            self.trust_network[agent_id] = max(0.0, self.trust_network[agent_id] - impact * 2)  # Penalize failures more
    
    def get_social_cognition_status(self) -> Dict[str, Any]:
        """Get social cognition status for this agent."""
        return {
            'cognitive_profile': self.cognitive_profile,
            'shared_resources_count': len(self.shared_resources),
            'accessed_resources_count': len(self.accessed_resources),
            'active_collaborations_count': len(self.active_collaborations),
            'communication_events': len(self.communication_history),
            'trust_network_size': len(self.trust_network),
            'avg_trust_score': sum(self.trust_network.values()) / max(len(self.trust_network), 1),
            'collaboration_score': self.metrics.collaboration_score
        }
    
    def get_status_info(self) -> Dict[str, Any]:
        """Get comprehensive agent status information."""
        return {
            'id': self.id,
            'status': self.status.value,
            'capabilities': {
                'reasoning': self.capabilities.reasoning,
                'multimodal': self.capabilities.multimodal,
                'memory_enabled': self.capabilities.memory_enabled,
                'learning_enabled': self.capabilities.learning_enabled,
                'collaboration': self.capabilities.collaboration,
                'specialized_domains': self.capabilities.specialized_domains,
                'processing_power': self.capabilities.processing_power
            },
            'metrics': {
                'requests_processed': self.metrics.requests_processed,
                'avg_response_time': self.metrics.avg_response_time,
                'success_rate': self.metrics.success_rate,
                'collaboration_score': self.metrics.collaboration_score,
                'evolution_generation': self.metrics.evolution_generation,
                'energy_level': self.metrics.energy_level
            },
            'position': self.position,
            'relations_count': len(self.relations),
            'current_task': self.current_task,
            'created_at': self.created_at,
            'uptime': time.time() - self.created_at
        }


class AgentManager:
    """Manages the lifecycle of AI agents in the AAR system."""
    
    def __init__(self, max_concurrent_agents: int = 1000, 
                 optimization_strategy: OptimizationStrategy = OptimizationStrategy.PERFORMANCE_BASED):
        self.max_concurrent_agents = max_concurrent_agents
        self.agents: Dict[str, Agent] = {}
        self.agent_pool: Dict[AgentStatus, Set[str]] = {
            status: set() for status in AgentStatus
        }
        
        # Resource management
        self.resource_usage = {
            'cpu_usage': 0.0,
            'memory_usage': 0.0,
            'active_agents': 0,
            'total_requests': 0
        }
        
        # Performance monitoring
        self.performance_stats = {
            'total_spawned': 0,
            'total_terminated': 0,
            'avg_agent_lifetime': 0.0,
            'peak_concurrent_agents': 0,
            'evolution_cycles': 0
        }
        
        # Advanced performance optimization
        self.performance_optimizer = AgentPerformanceOptimizer(optimization_strategy)
        self._optimization_task: Optional[asyncio.Task] = None
        self._running = True
        
        logger.info(f"Agent Manager initialized with capacity: {max_concurrent_agents}, optimization: {optimization_strategy.value}")
        
        # Optimization loop will be started when first agent is spawned
    
    async def spawn_agent(self, 
                         capabilities: Optional[AgentCapabilities] = None,
                         context: Optional[Dict[str, Any]] = None) -> str:
        """Spawn a new agent and return its ID."""
        if len(self.agents) >= self.max_concurrent_agents:
            raise RuntimeError(f"Maximum agent capacity ({self.max_concurrent_agents}) reached")
        
        # Generate unique agent ID
        agent_id = f"agent_{uuid.uuid4().hex[:8]}"
        
        # Create agent with default capabilities if none provided
        if capabilities is None:
            capabilities = AgentCapabilities()
        
        # Create and initialize agent
        agent = Agent(agent_id, capabilities, context)
        await agent.initialize()
        
        # Register agent
        self.agents[agent_id] = agent
        self._update_agent_pool(agent_id, agent.status)
        
        # Register with performance optimizer
        self.performance_optimizer.register_agent(agent_id)
        
        # Start optimization loop if not already running
        if not self._optimization_task:
            self._start_optimization_loop()
        
        # Update statistics
        self.performance_stats['total_spawned'] += 1
        self.resource_usage['active_agents'] = len([a for a in self.agents.values() 
                                                   if a.status == AgentStatus.ACTIVE])
        
        peak_agents = max(self.performance_stats['peak_concurrent_agents'], len(self.agents))
        self.performance_stats['peak_concurrent_agents'] = peak_agents
        
        logger.info(f"Agent {agent_id} spawned successfully")
        return agent_id
    
    async def terminate_agent(self, agent_id: str) -> None:
        """Terminate a specific agent."""
        if agent_id not in self.agents:
            logger.warning(f"Agent {agent_id} not found for termination")
            return
        
        agent = self.agents[agent_id]
        lifetime = time.time() - agent.created_at
        
        # Terminate agent
        await agent.terminate()
        
        # Remove from pools and registry
        self._remove_from_pools(agent_id)
        del self.agents[agent_id]
        
        # Unregister from performance optimizer
        self.performance_optimizer.unregister_agent(agent_id)
        
        # Update statistics
        self.performance_stats['total_terminated'] += 1
        
        # Update average lifetime
        total_terminated = self.performance_stats['total_terminated']
        current_avg = self.performance_stats['avg_agent_lifetime']
        new_avg = ((current_avg * (total_terminated - 1)) + lifetime) / total_terminated
        self.performance_stats['avg_agent_lifetime'] = new_avg
        
        self.resource_usage['active_agents'] = len([a for a in self.agents.values() 
                                                   if a.status == AgentStatus.ACTIVE])
        
        logger.info(f"Agent {agent_id} terminated (lifetime: {lifetime:.2f}s)")
    
    async def allocate_agents(self, 
                             request: Dict[str, Any],
                             count: int = 1) -> List[str]:
        """Allocate agents for a specific request using intelligent optimization."""
        allocated = []
        required_capabilities = request.get('required_capabilities', {})
        
        # Get available agents
        available_agents = [agent_id for agent_id, agent in self.agents.items()
                          if agent.status == AgentStatus.ACTIVE and 
                          self._agent_matches_requirements(agent, required_capabilities)]
        
        # Use performance optimizer for intelligent selection
        if available_agents:
            optimal_agents = self.performance_optimizer.get_optimal_agents(
                available_agents, 
                count=min(count, len(available_agents)),
                capability_requirements=required_capabilities
            )
            allocated.extend(optimal_agents)
        
        # Spawn additional agents if needed
        while len(allocated) < count and len(self.agents) < self.max_concurrent_agents:
            capabilities = self._create_capabilities_from_requirements(required_capabilities)
            new_agent_id = await self.spawn_agent(capabilities, request.get('context'))
            allocated.append(new_agent_id)
        
        logger.debug(f"Allocated {len(allocated)} agents for request (optimized)")
        return allocated
    
    def _agent_matches_requirements(self, 
                                  agent: Agent, 
                                  requirements: Dict[str, Any]) -> bool:
        """Check if agent matches capability requirements."""
        if requirements.get('reasoning', False) and not agent.capabilities.reasoning:
            return False
        if requirements.get('multimodal', False) and not agent.capabilities.multimodal:
            return False
        if requirements.get('min_processing_power', 0) > agent.capabilities.processing_power:
            return False
        
        required_domains = requirements.get('domains', [])
        if required_domains:
            agent_domains = set(agent.capabilities.specialized_domains)
            if not any(domain in agent_domains for domain in required_domains):
                return False
        
        return True
    
    def _create_capabilities_from_requirements(self, 
                                             requirements: Dict[str, Any]) -> AgentCapabilities:
        """Create agent capabilities based on requirements."""
        return AgentCapabilities(
            reasoning=requirements.get('reasoning', True),
            multimodal=requirements.get('multimodal', False),
            memory_enabled=requirements.get('memory', True),
            learning_enabled=requirements.get('learning', True),
            collaboration=requirements.get('collaboration', True),
            specialized_domains=requirements.get('domains', []),
            processing_power=requirements.get('min_processing_power', 1.0)
        )
    
    def _update_agent_pool(self, agent_id: str, status: AgentStatus) -> None:
        """Update agent pool tracking."""
        # Remove from all pools
        self._remove_from_pools(agent_id)
        
        # Add to appropriate pool
        self.agent_pool[status].add(agent_id)
    
    def _remove_from_pools(self, agent_id: str) -> None:
        """Remove agent from all status pools."""
        for pool in self.agent_pool.values():
            pool.discard(agent_id)
    
    async def evolve_agent(self, agent_id: str, evolution_data: Dict[str, Any]) -> None:
        """Evolve a specific agent."""
        if agent_id not in self.agents:
            logger.warning(f"Agent {agent_id} not found for evolution")
            return
        
        agent = self.agents[agent_id]
        await agent.evolve(evolution_data)
        
        self.performance_stats['evolution_cycles'] += 1
        logger.info(f"Agent {agent_id} evolution completed")
    
    async def process_agent_request(self, 
                                  agent_id: str, 
                                  request: Dict[str, Any]) -> Dict[str, Any]:
        """Process request through specific agent with performance tracking."""
        if agent_id not in self.agents:
            raise ValueError(f"Agent {agent_id} not found")
        
        agent = self.agents[agent_id]
        
        # Track performance
        start_time = time.time()
        success = True
        
        try:
            result = await agent.process_request(request)
            self.resource_usage['total_requests'] += 1
            return result
        except Exception as e:
            success = False
            logger.error(f"Request failed for agent {agent_id}: {e}")
            raise
        finally:
            # Record performance metrics
            processing_time = time.time() - start_time
            resource_usage = self._estimate_agent_resource_usage(agent)
            self.performance_optimizer.record_performance(
                agent_id, processing_time, success, resource_usage
            )
    
    def get_agent_status(self, agent_id: str) -> Optional[Dict[str, Any]]:
        """Get status information for specific agent."""
        if agent_id not in self.agents:
            return None
        
        return self.agents[agent_id].get_status_info()
    
    def get_system_stats(self) -> Dict[str, Any]:
        """Get comprehensive system statistics."""
        # Calculate current resource usage
        active_agents = len(self.agent_pool[AgentStatus.ACTIVE])
        busy_agents = len(self.agent_pool[AgentStatus.BUSY])
        total_agents = len(self.agents)
        
        utilization = total_agents / self.max_concurrent_agents if self.max_concurrent_agents > 0 else 0
        
        return {
            'agent_counts': {
                'total': total_agents,
                'active': active_agents,
                'busy': busy_agents,
                'idle': len(self.agent_pool[AgentStatus.IDLE]),
                'evolving': len(self.agent_pool[AgentStatus.EVOLVING]),
                'error': len(self.agent_pool[AgentStatus.ERROR])
            },
            'resource_usage': {
                **self.resource_usage,
                'utilization_percentage': utilization * 100,
                'available_capacity': self.max_concurrent_agents - total_agents
            },
            'performance_stats': self.performance_stats,
            'health_status': self._calculate_health_status()
        }
    
    def _calculate_health_status(self) -> Dict[str, Any]:
        """Calculate system health metrics."""
        total_agents = len(self.agents)
        error_agents = len(self.agent_pool[AgentStatus.ERROR])
        
        error_rate = error_agents / max(total_agents, 1)
        utilization = total_agents / self.max_concurrent_agents
        
        health_score = 1.0
        health_score -= error_rate * 0.5  # Penalize errors
        health_score -= max(0, utilization - 0.8) * 2  # Penalize high utilization
        
        status = "healthy"
        if health_score < 0.7:
            status = "degraded"
        if health_score < 0.4:
            status = "critical"
        
        return {
            'overall_score': max(0.0, min(1.0, health_score)),
            'error_rate': error_rate,
            'utilization': utilization,
            'status': status
        }
    
    async def shutdown(self) -> None:
        """Gracefully shutdown all agents."""
        logger.info("Shutting down Agent Manager...")
        
        # Stop optimization loop
        self._running = False
        if self._optimization_task:
            self._optimization_task.cancel()
            with contextlib.suppress(asyncio.CancelledError):
                await self._optimization_task
        
        # Terminate all agents
        agent_ids = list(self.agents.keys())
        for agent_id in agent_ids:
            await self.terminate_agent(agent_id)
        
        logger.info(f"Agent Manager shutdown complete. Terminated {len(agent_ids)} agents")
    
    def _start_optimization_loop(self) -> None:
        """Start background optimization task."""
        async def optimization_loop():
            while self._running:
                try:
                    if self.performance_optimizer.should_optimize():
                        await self.performance_optimizer.optimize_system(self)
                    await asyncio.sleep(5.0)  # Check every 5 seconds
                except asyncio.CancelledError:
                    break
                except Exception as e:
                    logger.error(f"Error in optimization loop: {e}")
                    await asyncio.sleep(10.0)  # Longer delay on error
        
        self._optimization_task = asyncio.create_task(optimization_loop())
    
    def _estimate_agent_resource_usage(self, agent: Agent) -> float:
        """Estimate current resource usage for an agent."""
        # Simple heuristic based on agent state and metrics
        base_usage = 0.1  # Base usage
        
        if agent.status == AgentStatus.BUSY:
            base_usage += 0.3
        
        # Factor in processing power
        base_usage += (agent.capabilities.processing_power - 1.0) * 0.2
        
        # Factor in recent activity
        time_since_activity = time.time() - agent.metrics.last_activity
        if time_since_activity < 60:  # Active in last minute
            base_usage += 0.2
        
        return min(1.0, base_usage)  # Cap at 100%
    
    def get_performance_report(self) -> Dict[str, Any]:
        """Get comprehensive performance report including optimization metrics."""
        basic_stats = self.get_system_stats()
        optimization_report = self.performance_optimizer.get_performance_report()
        
        return {
            **basic_stats,
            'optimization': optimization_report,
            'timestamp': time.time()
        }
    
    def get_optimization_suggestions(self) -> List[Dict[str, Any]]:
        """Get current optimization suggestions."""
        return self.performance_optimizer.suggest_optimizations()
    
    async def trigger_system_optimization(self) -> Dict[str, Any]:
        """Manually trigger system optimization."""
        return await self.performance_optimizer.optimize_system(self)