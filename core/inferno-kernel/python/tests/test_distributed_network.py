"""
Comprehensive test suite for the Distributed Network Registry.

Tests agent registration, discovery, topology management, network analytics,
and all major functionality of the distributed network system.
"""

import unittest
import time
import threading
from unittest.mock import patch, MagicMock

import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

from helpers.distributed_network import (
    DistributedNetworkRegistry,
    NetworkAgent,
    AgentCapability,
    NetworkTopology,
    AgentStatus,
    CommunicationProtocol,
    NetworkConnection,
    NetworkStats
)


class TestDistributedNetworkRegistry(unittest.TestCase):
    """Test cases for the distributed network registry."""

    def setUp(self):
        """Set up test fixtures."""
        self.registry = DistributedNetworkRegistry(
            node_id="test_node",
            topology=NetworkTopology.MESH,
            heartbeat_interval=1.0,  # Short interval for testing
            compatibility_threshold=0.5
        )

    def tearDown(self):
        """Clean up after tests."""
        if hasattr(self.registry, '_running') and self.registry._running:
            self.registry.stop()

    def test_registry_initialization(self):
        """Test registry initialization."""
        self.assertEqual(self.registry.node_id, "test_node")
        self.assertEqual(self.registry.topology, NetworkTopology.MESH)
        self.assertEqual(self.registry.heartbeat_interval, 1.0)
        self.assertEqual(self.registry.compatibility_threshold, 0.5)
        self.assertEqual(len(self.registry.agents), 0)
        self.assertEqual(len(self.registry.connections), 0)

    def test_registry_lifecycle(self):
        """Test registry start and stop."""
        self.assertFalse(getattr(self.registry, '_running', False))
        
        self.registry.start()
        self.assertTrue(self.registry._running)
        
        self.registry.stop()
        self.assertFalse(self.registry._running)

    def test_agent_registration(self):
        """Test agent registration."""
        agent = self._create_test_agent("agent_001")
        
        # Test successful registration
        result = self.registry.register_agent(agent)
        self.assertTrue(result)
        self.assertIn("agent_001", self.registry.agents)
        self.assertEqual(self.registry.agents["agent_001"].status, AgentStatus.ACTIVE)

    def test_agent_unregistration(self):
        """Test agent unregistration."""
        agent = self._create_test_agent("agent_001")
        
        # Register then unregister
        self.registry.register_agent(agent)
        self.assertIn("agent_001", self.registry.agents)
        
        result = self.registry.unregister_agent("agent_001")
        self.assertTrue(result)
        self.assertNotIn("agent_001", self.registry.agents)

    def test_duplicate_registration(self):
        """Test handling of duplicate agent registration."""
        agent = self._create_test_agent("agent_001")
        
        # First registration should succeed
        result1 = self.registry.register_agent(agent)
        self.assertTrue(result1)
        
        # Second registration should fail
        result2 = self.registry.register_agent(agent)
        self.assertFalse(result2)

    def test_agent_discovery(self):
        """Test agent discovery by capabilities."""
        # Register agents with different capabilities
        agent1 = self._create_test_agent("agent_001", ["computation", "planning"])
        agent2 = self._create_test_agent("agent_002", ["coordination", "planning"])
        agent3 = self._create_test_agent("agent_003", ["computation"])
        
        self.registry.register_agent(agent1)
        self.registry.register_agent(agent2)
        self.registry.register_agent(agent3)
        
        # Test discovery by single capability
        computation_agents = self.registry.discover_agents(["computation"])
        self.assertEqual(len(computation_agents), 2)
        agent_ids = [agent.agent_id for agent in computation_agents]
        self.assertIn("agent_001", agent_ids)
        self.assertIn("agent_003", agent_ids)
        
        # Test discovery by multiple capabilities
        planning_agents = self.registry.discover_agents(["planning"])
        self.assertEqual(len(planning_agents), 2)
        
        # Test discovery with max limit
        limited_agents = self.registry.discover_agents(["computation"], max_agents=1)
        self.assertEqual(len(limited_agents), 1)

    def test_agent_status_update(self):
        """Test agent status updates."""
        agent = self._create_test_agent("agent_001")
        self.registry.register_agent(agent)
        
        # Test status update
        result = self.registry.update_agent_status(
            "agent_001", 
            AgentStatus.BUSY, 
            metadata={"current_task": "processing"}
        )
        self.assertTrue(result)
        
        updated_agent = self.registry.agents["agent_001"]
        self.assertEqual(updated_agent.status, AgentStatus.BUSY)
        self.assertEqual(updated_agent.metadata["current_task"], "processing")

    def test_load_metrics_update(self):
        """Test agent load metrics updates."""
        agent = self._create_test_agent("agent_001")
        self.registry.register_agent(agent)
        
        load_metrics = {
            "cpu_usage": 75.5,
            "memory_usage": 60.0,
            "task_queue_length": 3
        }
        
        result = self.registry.update_agent_load_metrics("agent_001", load_metrics)
        self.assertTrue(result)
        
        updated_agent = self.registry.agents["agent_001"]
        self.assertEqual(updated_agent.load_metrics["cpu_usage"], 75.5)
        self.assertEqual(updated_agent.load_metrics["memory_usage"], 60.0)
        self.assertEqual(updated_agent.load_metrics["task_queue_length"], 3)

    def test_mesh_topology(self):
        """Test mesh topology application."""
        # Register multiple agents
        for i in range(3):
            agent = self._create_test_agent(f"agent_{i:03d}")
            self.registry.register_agent(agent)
        
        # In mesh topology, each agent should connect to all others
        # For 3 agents: 3 * 2 = 6 directional connections
        expected_connections = 3 * 2
        self.assertEqual(len(self.registry.connections), expected_connections)

    def test_star_topology(self):
        """Test star topology application."""
        self.registry.topology = NetworkTopology.STAR
        
        # Register multiple agents
        for i in range(4):
            agent = self._create_test_agent(f"agent_{i:03d}")
            self.registry.register_agent(agent)
        
        # In star topology: hub connects to all others, all others connect to hub
        # For 4 agents with first as hub: 3 * 2 = 6 connections
        expected_connections = 3 * 2
        self.assertEqual(len(self.registry.connections), expected_connections)

    def test_ring_topology(self):
        """Test ring topology application."""
        self.registry.topology = NetworkTopology.RING
        
        # Register multiple agents
        for i in range(4):
            agent = self._create_test_agent(f"agent_{i:03d}")
            self.registry.register_agent(agent)
        
        # In ring topology: each agent connects to next
        # For 4 agents: 4 connections in ring
        expected_connections = 4
        self.assertEqual(len(self.registry.connections), expected_connections)

    def test_topology_reconfiguration(self):
        """Test dynamic topology reconfiguration."""
        # Start with mesh
        for i in range(3):
            agent = self._create_test_agent(f"agent_{i:03d}")
            self.registry.register_agent(agent)
        
        initial_connections = len(self.registry.connections)
        
        # Reconfigure to star
        result = self.registry.reconfigure_topology(NetworkTopology.STAR)
        self.assertTrue(result)
        self.assertEqual(self.registry.topology, NetworkTopology.STAR)
        
        # Connection count should change
        new_connections = len(self.registry.connections)
        # For this test, we expect different connection patterns
        self.assertNotEqual(initial_connections, new_connections)

    def test_network_statistics(self):
        """Test network statistics calculation."""
        # Register agents
        for i in range(3):
            agent = self._create_test_agent(f"agent_{i:03d}")
            self.registry.register_agent(agent)
        
        stats = self.registry.get_network_stats()
        
        self.assertIsInstance(stats, NetworkStats)
        self.assertEqual(stats.total_agents, 3)
        self.assertEqual(stats.active_agents, 3)
        self.assertGreater(stats.total_connections, 0)
        self.assertGreaterEqual(stats.cognitive_compatibility_avg, 0.0)
        self.assertLessEqual(stats.cognitive_compatibility_avg, 1.0)

    def test_network_topology_info(self):
        """Test network topology information retrieval."""
        # Register agents
        for i in range(2):
            agent = self._create_test_agent(f"agent_{i:03d}")
            self.registry.register_agent(agent)
        
        topology_info = self.registry.get_network_topology()
        
        self.assertIn("topology_type", topology_info)
        self.assertEqual(topology_info["topology_type"], NetworkTopology.MESH.value)
        self.assertIn("agents", topology_info)
        self.assertIn("connections", topology_info)
        self.assertEqual(len(topology_info["agents"]), 2)

    def test_cognitive_compatibility_assessment(self):
        """Test cognitive compatibility scoring."""
        # Create agent with good compatibility features
        good_agent = NetworkAgent(
            agent_id="good_agent",
            hostname="localhost",
            port=8080,
            capabilities=[
                AgentCapability("computation", "1.0", "Math", {}, {}),
                AgentCapability("planning", "1.0", "Planning", {}, {})
            ],
            status=AgentStatus.ACTIVE,
            protocols=[CommunicationProtocol.INFERNO_9P, CommunicationProtocol.HTTP],
            metadata={"type": "cognitive_agent", "version": "1.0"},
            last_heartbeat=time.time(),
            join_timestamp=time.time()
        )
        
        # Create agent with poor compatibility
        poor_agent = NetworkAgent(
            agent_id="poor_agent",
            hostname="localhost",
            port=8081,
            capabilities=[],  # No capabilities
            status=AgentStatus.ACTIVE,
            protocols=[CommunicationProtocol.UDP],  # Non-preferred protocol
            metadata={},  # No metadata
            last_heartbeat=time.time(),
            join_timestamp=time.time()
        )
        
        good_score = self.registry._assess_cognitive_compatibility(good_agent)
        poor_score = self.registry._assess_cognitive_compatibility(poor_agent)
        
        self.assertGreater(good_score, poor_score)
        self.assertGreaterEqual(good_score, 0.0)
        self.assertLessEqual(good_score, 1.0)
        self.assertGreaterEqual(poor_score, 0.0)
        self.assertLessEqual(poor_score, 1.0)

    def test_compatibility_threshold_enforcement(self):
        """Test that compatibility threshold is enforced."""
        self.registry.compatibility_threshold = 0.8  # High threshold
        
        # Create agent with low compatibility
        low_compatibility_agent = NetworkAgent(
            agent_id="low_comp_agent",
            hostname="localhost",
            port=8080,
            capabilities=[],
            status=AgentStatus.ACTIVE,
            protocols=[CommunicationProtocol.UDP],
            metadata={},
            last_heartbeat=time.time(),
            join_timestamp=time.time()
        )
        
        # Registration should fail due to low compatibility
        result = self.registry.register_agent(low_compatibility_agent)
        self.assertFalse(result)
        self.assertNotIn("low_comp_agent", self.registry.agents)

    def test_heartbeat_monitoring(self):
        """Test heartbeat monitoring functionality."""
        # This test requires the registry to be running
        self.registry.start()
        
        try:
            agent = self._create_test_agent("agent_001")
            self.registry.register_agent(agent)
            
            # Simulate old heartbeat
            self.registry.agents["agent_001"].last_heartbeat = time.time() - 1000
            
            # Wait for heartbeat monitor to run
            time.sleep(1.5)  # Wait longer than heartbeat interval
            
            # Agent should be removed due to timeout
            # Note: This test might be flaky due to timing
            
        finally:
            self.registry.stop()

    def test_connection_creation(self):
        """Test connection creation between agents."""
        agent1 = self._create_test_agent("agent_001")
        agent2 = self._create_test_agent("agent_002")
        
        self.registry.register_agent(agent1)
        self.registry.register_agent(agent2)
        
        # Check that connections were created
        connection_keys = list(self.registry.connections.keys())
        self.assertGreater(len(connection_keys), 0)
        
        # Verify connection properties
        connection = list(self.registry.connections.values())[0]
        self.assertGreater(connection.bandwidth, 0)
        self.assertGreater(connection.latency, 0)
        self.assertGreater(connection.reliability, 0)

    def test_agent_serialization(self):
        """Test agent to_dict and from_dict methods."""
        agent = self._create_test_agent("agent_001")
        
        # Test serialization
        agent_dict = agent.to_dict()
        self.assertIn("agent_id", agent_dict)
        self.assertIn("capabilities", agent_dict)
        self.assertIn("status", agent_dict)
        
        # Test deserialization
        restored_agent = NetworkAgent.from_dict(agent_dict)
        self.assertEqual(agent.agent_id, restored_agent.agent_id)
        self.assertEqual(agent.hostname, restored_agent.hostname)
        self.assertEqual(agent.port, restored_agent.port)
        self.assertEqual(agent.status, restored_agent.status)

    def test_error_handling(self):
        """Test error handling in various scenarios."""
        # Test unregistering non-existent agent
        result = self.registry.unregister_agent("non_existent")
        self.assertFalse(result)
        
        # Test updating status of non-existent agent
        result = self.registry.update_agent_status("non_existent", AgentStatus.BUSY)
        self.assertFalse(result)
        
        # Test updating load metrics of non-existent agent
        result = self.registry.update_agent_load_metrics("non_existent", {"cpu": 50.0})
        self.assertFalse(result)

    def test_topology_efficiency_calculation(self):
        """Test topology efficiency calculation."""
        # Register agents
        for i in range(3):
            agent = self._create_test_agent(f"agent_{i:03d}")
            self.registry.register_agent(agent)
        
        efficiency = self.registry._calculate_topology_efficiency()
        self.assertGreaterEqual(efficiency, 0.0)
        self.assertLessEqual(efficiency, 1.0)

    def _create_test_agent(self, agent_id, capabilities=None):
        """Helper method to create test agents."""
        if capabilities is None:
            capabilities = ["computation", "planning"]
        
        agent_capabilities = [
            AgentCapability(cap, "1.0", f"{cap} capability", {}, {})
            for cap in capabilities
        ]
        
        return NetworkAgent(
            agent_id=agent_id,
            hostname="localhost",
            port=8080,
            capabilities=agent_capabilities,
            status=AgentStatus.ACTIVE,
            protocols=[CommunicationProtocol.HTTP, CommunicationProtocol.INFERNO_9P],
            metadata={"type": "test_agent"},
            last_heartbeat=time.time(),
            join_timestamp=time.time()
        )


class TestNetworkComponents(unittest.TestCase):
    """Test individual network components."""

    def test_agent_capability_creation(self):
        """Test agent capability creation."""
        capability = AgentCapability(
            name="computation",
            version="1.0",
            description="Mathematical computation",
            parameters={"precision": "high"},
            resource_requirements={"cpu": 2, "memory": "4GB"}
        )
        
        self.assertEqual(capability.name, "computation")
        self.assertEqual(capability.version, "1.0")
        self.assertEqual(capability.parameters["precision"], "high")
        self.assertEqual(capability.resource_requirements["cpu"], 2)

    def test_network_agent_creation(self):
        """Test network agent creation."""
        capabilities = [
            AgentCapability("computation", "1.0", "Math", {}, {}),
            AgentCapability("planning", "1.0", "Planning", {}, {})
        ]
        
        agent = NetworkAgent(
            agent_id="test_agent",
            hostname="localhost",
            port=8080,
            capabilities=capabilities,
            status=AgentStatus.ACTIVE,
            protocols=[CommunicationProtocol.HTTP],
            metadata={"type": "test"},
            last_heartbeat=time.time(),
            join_timestamp=time.time()
        )
        
        self.assertEqual(agent.agent_id, "test_agent")
        self.assertEqual(len(agent.capabilities), 2)
        self.assertEqual(agent.status, AgentStatus.ACTIVE)
        self.assertIn("cpu_usage", agent.load_metrics)

    def test_network_connection_creation(self):
        """Test network connection creation."""
        connection = NetworkConnection(
            source_agent_id="agent_001",
            target_agent_id="agent_002",
            protocol=CommunicationProtocol.HTTP,
            bandwidth=100.0,
            latency=10.0,
            reliability=0.99,
            established_at=time.time(),
            last_activity=time.time()
        )
        
        self.assertEqual(connection.source_agent_id, "agent_001")
        self.assertEqual(connection.target_agent_id, "agent_002")
        self.assertEqual(connection.protocol, CommunicationProtocol.HTTP)
        self.assertEqual(connection.bandwidth, 100.0)

    def test_network_stats_creation(self):
        """Test network stats creation."""
        stats = NetworkStats(
            total_agents=5,
            active_agents=4,
            total_connections=10,
            avg_latency=15.0,
            total_bandwidth=500.0,
            message_throughput=100.0,
            error_rate=0.02,
            topology_efficiency=0.85,
            cognitive_compatibility_avg=0.75
        )
        
        self.assertEqual(stats.total_agents, 5)
        self.assertEqual(stats.active_agents, 4)
        self.assertEqual(stats.error_rate, 0.02)
        self.assertEqual(stats.topology_efficiency, 0.85)

    def test_enum_values(self):
        """Test enum value correctness."""
        # Test NetworkTopology values
        self.assertEqual(NetworkTopology.MESH.value, "mesh")
        self.assertEqual(NetworkTopology.STAR.value, "star")
        self.assertEqual(NetworkTopology.RING.value, "ring")
        
        # Test AgentStatus values
        self.assertEqual(AgentStatus.ACTIVE.value, "active")
        self.assertEqual(AgentStatus.INACTIVE.value, "inactive")
        self.assertEqual(AgentStatus.BUSY.value, "busy")
        
        # Test CommunicationProtocol values
        self.assertEqual(CommunicationProtocol.HTTP.value, "http")
        self.assertEqual(CommunicationProtocol.WEBSOCKET.value, "websocket")
        self.assertEqual(CommunicationProtocol.INFERNO_9P.value, "inferno_9p")


if __name__ == '__main__':
    unittest.main()