"""
Distributed Network Registry for Agent Zero Networks

This module implements a distributed network registry that enables agent discovery,
network topology management, cognitive compatibility assessment, dynamic 
reconfiguration, and network analytics.

The registry supports multiple network topologies and provides real-time 
network monitoring and management capabilities.
"""

import json
import logging
import time
import threading
import socket
from dataclasses import dataclass, asdict
from typing import Dict, List, Optional, Any, Set, Tuple, Callable
from enum import Enum
from collections import defaultdict
import uuid

logger = logging.getLogger(__name__)


class NetworkTopology(Enum):
    """Supported network topology types."""
    MESH = "mesh"
    STAR = "star"
    RING = "ring"
    TREE = "tree"
    HYBRID = "hybrid"


class AgentStatus(Enum):
    """Agent status in the network."""
    ACTIVE = "active"
    INACTIVE = "inactive"
    BUSY = "busy"
    ERROR = "error"
    JOINING = "joining"
    LEAVING = "leaving"


class CommunicationProtocol(Enum):
    """Supported communication protocols."""
    HTTP = "http"
    WEBSOCKET = "websocket"
    TCP = "tcp"
    UDP = "udp"
    INFERNO_9P = "inferno_9p"


@dataclass
class AgentCapability:
    """Represents an agent's capability."""
    name: str
    version: str
    description: str
    parameters: Dict[str, Any]
    resource_requirements: Dict[str, Any]


@dataclass
class NetworkAgent:
    """Represents an agent in the distributed network."""
    agent_id: str
    hostname: str
    port: int
    capabilities: List[AgentCapability]
    status: AgentStatus
    protocols: List[CommunicationProtocol]
    metadata: Dict[str, Any]
    last_heartbeat: float
    join_timestamp: float
    cognitive_compatibility_score: float = 0.0
    load_metrics: Dict[str, float] = None

    def __post_init__(self):
        if self.load_metrics is None:
            self.load_metrics = {
                'cpu_usage': 0.0,
                'memory_usage': 0.0,
                'network_usage': 0.0,
                'task_queue_length': 0
            }

    def to_dict(self) -> Dict[str, Any]:
        """Convert agent to dictionary representation."""
        result = asdict(self)
        result['status'] = self.status.value
        result['protocols'] = [p.value for p in self.protocols]
        return result

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'NetworkAgent':
        """Create agent from dictionary representation."""
        capabilities = [
            AgentCapability(**cap) if isinstance(cap, dict) else cap
            for cap in data.get('capabilities', [])
        ]
        
        protocols = [
            CommunicationProtocol(p) if isinstance(p, str) else p
            for p in data.get('protocols', [])
        ]
        
        status = AgentStatus(data['status']) if isinstance(data['status'], str) else data['status']
        
        return cls(
            agent_id=data['agent_id'],
            hostname=data['hostname'],
            port=data['port'],
            capabilities=capabilities,
            status=status,
            protocols=protocols,
            metadata=data.get('metadata', {}),
            last_heartbeat=data.get('last_heartbeat', time.time()),
            join_timestamp=data.get('join_timestamp', time.time()),
            cognitive_compatibility_score=data.get('cognitive_compatibility_score', 0.0),
            load_metrics=data.get('load_metrics', {})
        )


@dataclass
class NetworkConnection:
    """Represents a connection between two agents."""
    source_agent_id: str
    target_agent_id: str
    protocol: CommunicationProtocol
    bandwidth: float
    latency: float
    reliability: float
    established_at: float
    last_activity: float


@dataclass
class NetworkStats:
    """Network statistics and analytics."""
    total_agents: int
    active_agents: int
    total_connections: int
    avg_latency: float
    total_bandwidth: float
    message_throughput: float
    error_rate: float
    topology_efficiency: float
    cognitive_compatibility_avg: float


class DistributedNetworkRegistry:
    """
    Main registry class for managing distributed agent networks.
    """

    def __init__(self, 
                 node_id: str,
                 topology: NetworkTopology = NetworkTopology.MESH,
                 heartbeat_interval: float = 30.0,
                 compatibility_threshold: float = 0.7):
        self.node_id = node_id
        self.topology = topology
        self.heartbeat_interval = heartbeat_interval
        self.compatibility_threshold = compatibility_threshold
        
        # Core data structures
        self.agents: Dict[str, NetworkAgent] = {}
        self.connections: Dict[Tuple[str, str], NetworkConnection] = {}
        self.topology_rules: Dict[NetworkTopology, Callable] = {
            NetworkTopology.MESH: self._apply_mesh_topology,
            NetworkTopology.STAR: self._apply_star_topology,
            NetworkTopology.RING: self._apply_ring_topology,
            NetworkTopology.TREE: self._apply_tree_topology,
            NetworkTopology.HYBRID: self._apply_hybrid_topology
        }
        
        # Network analytics
        self.message_counts: Dict[str, int] = defaultdict(int)
        self.error_counts: Dict[str, int] = defaultdict(int)
        self.performance_history: List[Dict[str, Any]] = []
        
        # Threading and lifecycle
        self._running = False
        self._heartbeat_thread: Optional[threading.Thread] = None
        self._analytics_thread: Optional[threading.Thread] = None
        self._lock = threading.RLock()
        
        logger.info(f"Initialized network registry for node {node_id} with {topology.value} topology")

    def start(self):
        """Start the network registry."""
        with self._lock:
            if self._running:
                return
                
            self._running = True
            
            # Start heartbeat monitoring
            self._heartbeat_thread = threading.Thread(target=self._heartbeat_monitor, daemon=True)
            self._heartbeat_thread.start()
            
            # Start analytics collection
            self._analytics_thread = threading.Thread(target=self._analytics_collector, daemon=True)
            self._analytics_thread.start()
            
            logger.info("Network registry started")

    def stop(self):
        """Stop the network registry."""
        with self._lock:
            self._running = False
            
            if self._heartbeat_thread:
                self._heartbeat_thread.join(timeout=5.0)
                
            if self._analytics_thread:
                self._analytics_thread.join(timeout=5.0)
                
            logger.info("Network registry stopped")

    def register_agent(self, agent: NetworkAgent) -> bool:
        """Register a new agent in the network."""
        try:
            with self._lock:
                if agent.agent_id in self.agents:
                    logger.warning(f"Agent {agent.agent_id} already registered")
                    return False
                
                # Assess cognitive compatibility
                agent.cognitive_compatibility_score = self._assess_cognitive_compatibility(agent)
                
                if agent.cognitive_compatibility_score < self.compatibility_threshold:
                    logger.warning(f"Agent {agent.agent_id} does not meet compatibility threshold")
                    return False
                
                # Add to registry
                agent.status = AgentStatus.JOINING
                agent.join_timestamp = time.time()
                agent.last_heartbeat = time.time()
                self.agents[agent.agent_id] = agent
                
                # Apply topology rules
                self._apply_topology()
                
                # Update status to active
                agent.status = AgentStatus.ACTIVE
                
                logger.info(f"Registered agent {agent.agent_id} with compatibility score {agent.cognitive_compatibility_score:.2f}")
                return True
                
        except Exception as e:
            logger.error(f"Error registering agent {agent.agent_id}: {e}")
            return False

    def unregister_agent(self, agent_id: str) -> bool:
        """Unregister an agent from the network."""
        try:
            with self._lock:
                if agent_id not in self.agents:
                    logger.warning(f"Agent {agent_id} not found in registry")
                    return False
                
                # Mark as leaving
                self.agents[agent_id].status = AgentStatus.LEAVING
                
                # Remove connections
                self._remove_agent_connections(agent_id)
                
                # Remove from registry
                del self.agents[agent_id]
                
                # Reapply topology
                self._apply_topology()
                
                logger.info(f"Unregistered agent {agent_id}")
                return True
                
        except Exception as e:
            logger.error(f"Error unregistering agent {agent_id}: {e}")
            return False

    def discover_agents(self, capabilities: List[str], max_agents: int = 10) -> List[NetworkAgent]:
        """Discover agents with specific capabilities."""
        try:
            with self._lock:
                matching_agents = []
                
                for agent in self.agents.values():
                    if agent.status != AgentStatus.ACTIVE:
                        continue
                    
                    # Check if agent has all required capabilities
                    agent_capability_names = {cap.name for cap in agent.capabilities}
                    if all(cap in agent_capability_names for cap in capabilities):
                        matching_agents.append(agent)
                
                # Sort by compatibility score and load
                matching_agents.sort(
                    key=lambda a: (a.cognitive_compatibility_score, -a.load_metrics.get('task_queue_length', 0)),
                    reverse=True
                )
                
                return matching_agents[:max_agents]
                
        except Exception as e:
            logger.error(f"Error discovering agents: {e}")
            return []

    def update_agent_status(self, agent_id: str, status: AgentStatus, metadata: Optional[Dict[str, Any]] = None) -> bool:
        """Update an agent's status."""
        try:
            with self._lock:
                if agent_id not in self.agents:
                    return False
                
                agent = self.agents[agent_id]
                agent.status = status
                agent.last_heartbeat = time.time()
                
                if metadata:
                    agent.metadata.update(metadata)
                
                logger.debug(f"Updated agent {agent_id} status to {status.value}")
                return True
                
        except Exception as e:
            logger.error(f"Error updating agent status: {e}")
            return False

    def update_agent_load_metrics(self, agent_id: str, load_metrics: Dict[str, float]) -> bool:
        """Update an agent's load metrics."""
        try:
            with self._lock:
                if agent_id not in self.agents:
                    return False
                
                self.agents[agent_id].load_metrics.update(load_metrics)
                self.agents[agent_id].last_heartbeat = time.time()
                
                return True
                
        except Exception as e:
            logger.error(f"Error updating agent load metrics: {e}")
            return False

    def get_network_topology(self) -> Dict[str, Any]:
        """Get current network topology information."""
        try:
            with self._lock:
                topology_info = {
                    'topology_type': self.topology.value,
                    'agents': list(self.agents.keys()),
                    'connections': [
                        {
                            'source': conn.source_agent_id,
                            'target': conn.target_agent_id,
                            'protocol': conn.protocol.value,
                            'latency': conn.latency,
                            'bandwidth': conn.bandwidth
                        }
                        for conn in self.connections.values()
                    ],
                    'stats': self.get_network_stats()._asdict() if hasattr(self.get_network_stats(), '_asdict') else vars(self.get_network_stats())
                }
                
                return topology_info
                
        except Exception as e:
            logger.error(f"Error getting network topology: {e}")
            return {}

    def get_network_stats(self) -> NetworkStats:
        """Get comprehensive network statistics."""
        try:
            with self._lock:
                active_agents = sum(1 for agent in self.agents.values() if agent.status == AgentStatus.ACTIVE)
                
                latencies = [conn.latency for conn in self.connections.values()]
                avg_latency = sum(latencies) / len(latencies) if latencies else 0.0
                
                total_bandwidth = sum(conn.bandwidth for conn in self.connections.values())
                
                total_messages = sum(self.message_counts.values())
                total_errors = sum(self.error_counts.values())
                error_rate = total_errors / total_messages if total_messages > 0 else 0.0
                
                compatibility_scores = [agent.cognitive_compatibility_score for agent in self.agents.values()]
                avg_compatibility = sum(compatibility_scores) / len(compatibility_scores) if compatibility_scores else 0.0
                
                # Calculate topology efficiency (simplified metric)
                topology_efficiency = self._calculate_topology_efficiency()
                
                return NetworkStats(
                    total_agents=len(self.agents),
                    active_agents=active_agents,
                    total_connections=len(self.connections),
                    avg_latency=avg_latency,
                    total_bandwidth=total_bandwidth,
                    message_throughput=total_messages / 60.0,  # messages per minute
                    error_rate=error_rate,
                    topology_efficiency=topology_efficiency,
                    cognitive_compatibility_avg=avg_compatibility
                )
                
        except Exception as e:
            logger.error(f"Error calculating network stats: {e}")
            return NetworkStats(0, 0, 0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

    def reconfigure_topology(self, new_topology: NetworkTopology) -> bool:
        """Dynamically reconfigure the network topology."""
        try:
            with self._lock:
                old_topology = self.topology
                self.topology = new_topology
                
                # Clear existing connections
                self.connections.clear()
                
                # Apply new topology
                self._apply_topology()
                
                logger.info(f"Reconfigured topology from {old_topology.value} to {new_topology.value}")
                return True
                
        except Exception as e:
            logger.error(f"Error reconfiguring topology: {e}")
            return False

    def _assess_cognitive_compatibility(self, agent: NetworkAgent) -> float:
        """Assess cognitive compatibility of an agent with the network."""
        try:
            # Simplified compatibility assessment
            score = 0.0
            
            # Base score for having any capabilities
            if agent.capabilities:
                score += 0.3
            
            # Score for common capabilities with existing agents
            if self.agents:
                existing_capabilities = set()
                for existing_agent in self.agents.values():
                    existing_capabilities.update(cap.name for cap in existing_agent.capabilities)
                
                agent_capabilities = {cap.name for cap in agent.capabilities}
                common_capabilities = agent_capabilities.intersection(existing_capabilities)
                
                if existing_capabilities:
                    score += 0.4 * (len(common_capabilities) / len(existing_capabilities))
            else:
                score += 0.4  # First agent gets full score
            
            # Score for protocol compatibility
            if CommunicationProtocol.INFERNO_9P in agent.protocols:
                score += 0.2
            elif any(p in [CommunicationProtocol.HTTP, CommunicationProtocol.WEBSOCKET] for p in agent.protocols):
                score += 0.1
            
            # Score for metadata completeness
            if agent.metadata:
                score += 0.1
            
            return min(score, 1.0)
            
        except Exception as e:
            logger.error(f"Error assessing cognitive compatibility: {e}")
            return 0.0

    def _apply_topology(self):
        """Apply the current network topology rules."""
        try:
            # Clear existing connections before applying new topology
            self.connections.clear()
            
            topology_func = self.topology_rules.get(self.topology)
            if topology_func:
                topology_func()
            else:
                logger.warning(f"No topology rules defined for {self.topology.value}")
                
        except Exception as e:
            logger.error(f"Error applying topology: {e}")

    def _apply_mesh_topology(self):
        """Apply mesh topology - all agents connected to all others."""
        agent_ids = list(self.agents.keys())
        
        for i, source_id in enumerate(agent_ids):
            for j, target_id in enumerate(agent_ids):
                if i != j:
                    self._create_connection(source_id, target_id)

    def _apply_star_topology(self):
        """Apply star topology - one central hub connected to all others."""
        agent_ids = list(self.agents.keys())
        if not agent_ids:
            return
        
        # Use first agent as hub (could be more sophisticated)
        hub_id = agent_ids[0]
        
        for agent_id in agent_ids[1:]:
            self._create_connection(hub_id, agent_id)
            self._create_connection(agent_id, hub_id)

    def _apply_ring_topology(self):
        """Apply ring topology - agents connected in a ring."""
        agent_ids = list(self.agents.keys())
        
        for i in range(len(agent_ids)):
            next_i = (i + 1) % len(agent_ids)
            self._create_connection(agent_ids[i], agent_ids[next_i])

    def _apply_tree_topology(self):
        """Apply tree topology - hierarchical structure."""
        agent_ids = list(self.agents.keys())
        if not agent_ids:
            return
        
        # Simple binary tree structure
        for i, agent_id in enumerate(agent_ids):
            left_child = 2 * i + 1
            right_child = 2 * i + 2
            
            if left_child < len(agent_ids):
                self._create_connection(agent_id, agent_ids[left_child])
                self._create_connection(agent_ids[left_child], agent_id)
            
            if right_child < len(agent_ids):
                self._create_connection(agent_id, agent_ids[right_child])
                self._create_connection(agent_ids[right_child], agent_id)

    def _apply_hybrid_topology(self):
        """Apply hybrid topology - combination of different patterns."""
        # Implement a hybrid approach combining mesh and star
        agent_ids = list(self.agents.keys())
        if len(agent_ids) <= 3:
            self._apply_mesh_topology()
        else:
            # Create star cores with mesh connections between hubs
            core_size = max(2, len(agent_ids) // 3)
            cores = [agent_ids[i:i + core_size] for i in range(0, len(agent_ids), core_size)]
            
            # Mesh within cores
            for core in cores:
                for i, source_id in enumerate(core):
                    for j, target_id in enumerate(core):
                        if i != j:
                            self._create_connection(source_id, target_id)
            
            # Connect core hubs
            if len(cores) > 1:
                for i in range(len(cores) - 1):
                    hub1 = cores[i][0]
                    hub2 = cores[i + 1][0]
                    self._create_connection(hub1, hub2)
                    self._create_connection(hub2, hub1)

    def _create_connection(self, source_id: str, target_id: str):
        """Create a connection between two agents."""
        try:
            if source_id not in self.agents or target_id not in self.agents:
                return
            
            connection_key = (source_id, target_id)
            if connection_key in self.connections:
                return
            
            # Simulate connection metrics
            latency = 10.0 + hash((source_id, target_id)) % 50  # 10-60ms
            bandwidth = 100.0 + hash((source_id, target_id)) % 900  # 100-1000 Mbps
            reliability = 0.95 + (hash((source_id, target_id)) % 5) / 100  # 95-99%
            
            connection = NetworkConnection(
                source_agent_id=source_id,
                target_agent_id=target_id,
                protocol=CommunicationProtocol.INFERNO_9P,  # Default to Inferno protocol
                bandwidth=bandwidth,
                latency=latency,
                reliability=reliability,
                established_at=time.time(),
                last_activity=time.time()
            )
            
            self.connections[connection_key] = connection
            
        except Exception as e:
            logger.error(f"Error creating connection {source_id} -> {target_id}: {e}")

    def _remove_agent_connections(self, agent_id: str):
        """Remove all connections involving a specific agent."""
        connections_to_remove = [
            key for key in self.connections.keys()
            if key[0] == agent_id or key[1] == agent_id
        ]
        
        for key in connections_to_remove:
            del self.connections[key]

    def _calculate_topology_efficiency(self) -> float:
        """Calculate a simple topology efficiency metric."""
        try:
            if not self.agents:
                return 0.0
            
            num_agents = len(self.agents)
            num_connections = len(self.connections)
            
            if num_agents == 1:
                return 1.0
            
            # Efficiency based on connectivity and redundancy
            max_connections = num_agents * (num_agents - 1)  # Full mesh
            connectivity_ratio = num_connections / max_connections if max_connections > 0 else 0.0
            
            # Consider topology type
            topology_efficiency_map = {
                NetworkTopology.MESH: 1.0,
                NetworkTopology.STAR: 0.7,
                NetworkTopology.RING: 0.5,
                NetworkTopology.TREE: 0.6,
                NetworkTopology.HYBRID: 0.8
            }
            
            base_efficiency = topology_efficiency_map.get(self.topology, 0.5)
            
            return base_efficiency * connectivity_ratio
            
        except Exception as e:
            logger.error(f"Error calculating topology efficiency: {e}")
            return 0.0

    def _heartbeat_monitor(self):
        """Monitor agent heartbeats and handle disconnections."""
        while self._running:
            try:
                current_time = time.time()
                timeout_threshold = current_time - (self.heartbeat_interval * 3)
                
                with self._lock:
                    agents_to_remove = []
                    
                    for agent_id, agent in self.agents.items():
                        if agent.last_heartbeat < timeout_threshold:
                            logger.warning(f"Agent {agent_id} heartbeat timeout")
                            agents_to_remove.append(agent_id)
                    
                    for agent_id in agents_to_remove:
                        self.unregister_agent(agent_id)
                
                time.sleep(self.heartbeat_interval)
                
            except Exception as e:
                logger.error(f"Error in heartbeat monitor: {e}")
                time.sleep(1.0)

    def _analytics_collector(self):
        """Collect and store network analytics."""
        while self._running:
            try:
                stats = self.get_network_stats()
                
                analytics_data = {
                    'timestamp': time.time(),
                    'stats': vars(stats),
                    'agent_count': len(self.agents),
                    'connection_count': len(self.connections)
                }
                
                self.performance_history.append(analytics_data)
                
                # Keep only last 1000 entries
                if len(self.performance_history) > 1000:
                    self.performance_history = self.performance_history[-1000:]
                
                time.sleep(60.0)  # Collect every minute
                
            except Exception as e:
                logger.error(f"Error in analytics collector: {e}")
                time.sleep(5.0)


# Example usage and testing functions
def example_usage():
    """Demonstrate the distributed network registry."""
    print("=== Distributed Network Registry Example ===\n")
    
    # Create registry
    registry = DistributedNetworkRegistry("node_001", NetworkTopology.MESH)
    registry.start()
    
    try:
        # Create sample agents
        agent1 = NetworkAgent(
            agent_id="agent_001",
            hostname="localhost",
            port=8080,
            capabilities=[
                AgentCapability("computation", "1.0", "Mathematical computation", {}, {}),
                AgentCapability("planning", "1.0", "Task planning", {}, {})
            ],
            status=AgentStatus.ACTIVE,
            protocols=[CommunicationProtocol.HTTP, CommunicationProtocol.INFERNO_9P],
            metadata={"type": "compute_agent"},
            last_heartbeat=time.time(),
            join_timestamp=time.time()
        )
        
        agent2 = NetworkAgent(
            agent_id="agent_002",
            hostname="localhost",
            port=8081,
            capabilities=[
                AgentCapability("coordination", "1.0", "Agent coordination", {}, {}),
                AgentCapability("planning", "1.0", "Task planning", {}, {})
            ],
            status=AgentStatus.ACTIVE,
            protocols=[CommunicationProtocol.WEBSOCKET, CommunicationProtocol.INFERNO_9P],
            metadata={"type": "coordination_agent"},
            last_heartbeat=time.time(),
            join_timestamp=time.time()
        )
        
        # Register agents
        print("Registering agents...")
        registry.register_agent(agent1)
        registry.register_agent(agent2)
        
        # Discover agents
        print("\nDiscovering agents with 'planning' capability:")
        discovered = registry.discover_agents(["planning"])
        for agent in discovered:
            print(f"  - {agent.agent_id}: {[cap.name for cap in agent.capabilities]}")
        
        # Get network topology
        print("\nNetwork topology:")
        topology = registry.get_network_topology()
        print(f"  Type: {topology['topology_type']}")
        print(f"  Agents: {topology['agents']}")
        print(f"  Connections: {len(topology['connections'])}")
        
        # Get network stats
        print("\nNetwork statistics:")
        stats = registry.get_network_stats()
        print(f"  Total agents: {stats.total_agents}")
        print(f"  Active agents: {stats.active_agents}")
        print(f"  Total connections: {stats.total_connections}")
        print(f"  Avg compatibility: {stats.cognitive_compatibility_avg:.2f}")
        
        # Test topology reconfiguration
        print("\nReconfiguring to star topology...")
        registry.reconfigure_topology(NetworkTopology.STAR)
        
        new_topology = registry.get_network_topology()
        print(f"  New connections: {len(new_topology['connections'])}")
        
    finally:
        registry.stop()


if __name__ == "__main__":
    example_usage()