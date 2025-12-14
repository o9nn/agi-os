"""
Comprehensive test suite for the Cognitive Network Tool.

Tests all 9 core methods and integration functionality for the cognitive
network tool including message sending, coordination, discovery, broadcasting,
and network management.
"""

import unittest
import time
from unittest.mock import patch, MagicMock

import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

from tools.cognitive_network import (
    CognitiveNetworkTool,
    TaskCoordinationRequest,
    CoordinationResponse,
    BroadcastMessage,
    AgentZeroIntegration
)
from helpers.cognitive_grammar import (
    CommunicativeIntent,
    CognitiveFrame,
    CognitiveRole
)
from helpers.distributed_network import (
    DistributedNetworkRegistry,
    NetworkAgent,
    AgentCapability,
    NetworkTopology,
    AgentStatus,
    CommunicationProtocol
)


class TestCognitiveNetworkTool(unittest.TestCase):
    """Test cases for the cognitive network tool."""

    def setUp(self):
        """Set up test fixtures."""
        # Create mock registry to avoid threading issues in tests
        self.mock_registry = MagicMock(spec=DistributedNetworkRegistry)
        self.mock_registry.agents = {}
        self.mock_registry.topology = NetworkTopology.MESH
        self.mock_registry._running = False
        
        # Create cognitive network tool
        self.cognitive_tool = CognitiveNetworkTool(
            agent_id="test_agent",
            network_registry=self.mock_registry
        )

    def tearDown(self):
        """Clean up after tests."""
        pass

    def test_tool_initialization(self):
        """Test cognitive network tool initialization."""
        self.assertEqual(self.cognitive_tool.agent_id, "test_agent")
        self.assertIsNotNone(self.cognitive_tool.network_registry)
        self.assertIsNotNone(self.cognitive_tool.grammar_framework)
        self.assertEqual(len(self.cognitive_tool.active_coordinations), 0)
        self.assertEqual(len(self.cognitive_tool.message_history), 0)

    def test_send_cognitive_message(self):
        """Test Core Method 1: send_cognitive_message."""
        result = self.cognitive_tool.send_cognitive_message(
            target_agent_id="target_agent",
            intent="request",
            frame="task_delegation",
            content={"task": "test task"},
            roles={"agent": "test_agent", "patient": "target_agent"}
        )
        
        self.assertTrue(result["success"])
        self.assertEqual(result["target_agent"], "target_agent")
        self.assertIn("message_id", result)
        self.assertIn("natural_language", result)
        self.assertIn("sent_at", result)
        
        # Check message was added to history
        self.assertEqual(len(self.cognitive_tool.message_history), 1)

    def test_send_cognitive_message_with_enums(self):
        """Test sending cognitive message with enum parameters."""
        result = self.cognitive_tool.send_cognitive_message(
            target_agent_id="target_agent",
            intent=CommunicativeIntent.COORDINATE,
            frame=CognitiveFrame.COORDINATION,
            content={"task": "coordination task"},
            roles={CognitiveRole.AGENT: "test_agent"}
        )
        
        self.assertTrue(result["success"])
        self.assertIn("coordinate", result["natural_language"].lower())

    def test_coordinate_with_agents(self):
        """Test Core Method 2: coordinate_with_agents."""
        # Mock agent discovery
        mock_agents = [
            self._create_mock_agent("agent_001", ["computation", "planning"]),
            self._create_mock_agent("agent_002", ["planning", "coordination"])
        ]
        self.mock_registry.discover_agents.return_value = mock_agents
        
        result = self.cognitive_tool.coordinate_with_agents(
            task_description="Implement distributed system",
            required_capabilities=["computation", "planning"],
            max_partners=2,
            coordination_type="synchronous"
        )
        
        self.assertTrue(result["success"])
        self.assertIn("task_id", result)
        self.assertEqual(len(result["assigned_agents"]), 2)
        self.assertIn("coordination_plan", result)
        
        # Check coordination was stored
        task_id = result["task_id"]
        self.assertIn(task_id, self.cognitive_tool.active_coordinations)

    def test_coordinate_with_no_agents_found(self):
        """Test coordination when no suitable agents are found."""
        self.mock_registry.discover_agents.return_value = []
        
        result = self.cognitive_tool.coordinate_with_agents(
            task_description="Impossible task",
            required_capabilities=["nonexistent_capability"],
            max_partners=2
        )
        
        self.assertFalse(result["success"])
        self.assertIn("error", result)
        self.assertIn("No suitable agents found", result["error"])

    def test_discover_network_agents(self):
        """Test Core Method 3: discover_network_agents."""
        # Mock agents with capabilities
        mock_agents = [
            self._create_mock_agent("agent_001", ["computation"])
        ]
        self.mock_registry.discover_agents.return_value = mock_agents
        
        # Mock network stats
        mock_stats = MagicMock()
        mock_stats.total_agents = 2
        mock_stats.active_agents = 2
        mock_stats.cognitive_compatibility_avg = 0.85
        self.mock_registry.get_network_stats.return_value = mock_stats
        
        result = self.cognitive_tool.discover_network_agents(
            capabilities=["computation"],
            max_agents=10
        )
        
        self.assertTrue(result["success"])
        self.assertEqual(result["total_discovered"], 1)  # Only one has computation
        self.assertIn("discovered_agents", result)
        self.assertIn("network_stats", result)

    def test_discover_all_agents(self):
        """Test discovering all agents without capability filter."""
        # Mock all agents in registry
        mock_agents = {
            "agent_001": self._create_mock_agent("agent_001", ["computation"]),
            "agent_002": self._create_mock_agent("agent_002", ["planning"])
        }
        self.mock_registry.agents = mock_agents
        
        # Mock network stats
        mock_stats = MagicMock()
        mock_stats.total_agents = 2
        mock_stats.active_agents = 2
        mock_stats.cognitive_compatibility_avg = 0.85
        self.mock_registry.get_network_stats.return_value = mock_stats
        
        result = self.cognitive_tool.discover_network_agents()
        
        self.assertTrue(result["success"])
        self.assertEqual(result["total_discovered"], 2)

    def test_broadcast_to_network(self):
        """Test Core Method 4: broadcast_to_network."""
        # Mock target agents
        mock_agents = [
            self._create_mock_agent("agent_001", ["computation"]),
            self._create_mock_agent("agent_002", ["planning"])
        ]
        self.mock_registry.agents = {
            agent.agent_id: agent for agent in mock_agents
        }
        
        result = self.cognitive_tool.broadcast_to_network(
            message_content={"announcement": "System maintenance"},
            priority="high"
        )
        
        self.assertTrue(result["success"])
        self.assertIn("broadcast_id", result)
        self.assertEqual(result["total_targets"], 2)
        self.assertIn("broadcast_results", result)

    def test_broadcast_with_capability_filter(self):
        """Test broadcasting with capability filtering."""
        # Mock agent discovery for specific capabilities
        mock_agents = [
            self._create_mock_agent("agent_001", ["computation"])
        ]
        self.mock_registry.discover_agents.return_value = mock_agents
        
        result = self.cognitive_tool.broadcast_to_network(
            message_content={"task": "computation task"},
            target_capabilities=["computation"],
            priority="normal"
        )
        
        self.assertTrue(result["success"])
        self.assertEqual(result["total_targets"], 1)

    def test_query_agent_capabilities(self):
        """Test Core Method 5: query_agent_capabilities."""
        # Mock specific agent
        mock_agent = self._create_mock_agent("target_agent", ["computation", "planning"])
        self.mock_registry.agents = {"target_agent": mock_agent}
        
        result = self.cognitive_tool.query_agent_capabilities(
            target_agent_id="target_agent"
        )
        
        self.assertTrue(result["success"])
        self.assertEqual(result["target_agent"], "target_agent")
        self.assertEqual(len(result["capabilities"]), 2)
        self.assertIn("query_message", result)

    def test_query_all_agent_capabilities(self):
        """Test querying capabilities of all agents."""
        # Mock multiple agents
        mock_agents = {
            "agent_001": self._create_mock_agent("agent_001", ["computation"]),
            "agent_002": self._create_mock_agent("agent_002", ["planning"])
        }
        self.mock_registry.agents = mock_agents
        
        result = self.cognitive_tool.query_agent_capabilities()
        
        self.assertTrue(result["success"])
        self.assertEqual(result["query_type"], "all_agents")
        self.assertEqual(result["total_agents"], 2)
        self.assertIn("all_capabilities", result)
        self.assertIn("capability_summary", result)

    def test_query_nonexistent_agent(self):
        """Test querying capabilities of non-existent agent."""
        self.mock_registry.agents = {}
        
        result = self.cognitive_tool.query_agent_capabilities(
            target_agent_id="nonexistent_agent"
        )
        
        self.assertFalse(result["success"])
        self.assertIn("error", result)
        self.assertIn("not found", result["error"])

    def test_update_agent_status(self):
        """Test Core Method 6: update_agent_status."""
        # Mock successful status update
        self.mock_registry.update_agent_status.return_value = True
        self.mock_registry.update_agent_load_metrics.return_value = True
        
        result = self.cognitive_tool.update_agent_status(
            new_status="busy",
            metadata={"current_task": "processing"},
            load_metrics={"cpu_usage": 75.0}
        )
        
        self.assertTrue(result["success"])
        self.assertEqual(result["new_status"], "busy")
        self.assertTrue(result["metadata_updated"])
        self.assertTrue(result["load_metrics_updated"])

    def test_update_agent_status_with_enum(self):
        """Test updating agent status with enum parameter."""
        self.mock_registry.update_agent_status.return_value = True
        
        result = self.cognitive_tool.update_agent_status(
            new_status=AgentStatus.BUSY
        )
        
        self.assertTrue(result["success"])
        self.assertEqual(result["new_status"], "busy")

    def test_negotiate_resources(self):
        """Test Core Method 7: negotiate_resources."""
        # Mock target agents
        mock_agents = [
            self._create_mock_agent("agent_001", ["resource_management"]),
            self._create_mock_agent("agent_002", ["coordination"])
        ]
        self.mock_registry.discover_agents.return_value = mock_agents
        
        resource_requirements = {
            "cpu": 4,
            "memory": "8GB",
            "storage": "100GB"
        }
        
        result = self.cognitive_tool.negotiate_resources(
            resource_requirements=resource_requirements,
            negotiation_timeout=30.0
        )
        
        self.assertTrue(result["success"])
        self.assertIn("negotiation_id", result)
        self.assertEqual(result["resource_requirements"], resource_requirements)
        self.assertIn("negotiation_results", result)

    def test_negotiate_resources_with_specific_agents(self):
        """Test resource negotiation with specific target agents."""
        # Mock specific agents
        mock_agents = [
            self._create_mock_agent("agent_001", ["resource_management"])
        ]
        self.mock_registry.agents = {
            agent.agent_id: agent for agent in mock_agents
        }
        
        result = self.cognitive_tool.negotiate_resources(
            resource_requirements={"cpu": 2},
            target_agents=["agent_001"]
        )
        
        self.assertTrue(result["success"])
        self.assertIn("agent_001", result["target_agents"])

    def test_negotiate_resources_no_agents(self):
        """Test resource negotiation when no suitable agents are found."""
        self.mock_registry.discover_agents.return_value = []
        self.mock_registry.agents = {}
        
        result = self.cognitive_tool.negotiate_resources(
            resource_requirements={"cpu": 2}
        )
        
        self.assertFalse(result["success"])
        self.assertIn("No suitable agents found", result["error"])

    def test_monitor_network_health(self):
        """Test Core Method 8: monitor_network_health."""
        # Mock network stats
        mock_stats = MagicMock()
        mock_stats.total_agents = 5
        mock_stats.active_agents = 4
        mock_stats.total_connections = 10
        mock_stats.error_rate = 0.02
        mock_stats.cognitive_compatibility_avg = 0.8
        mock_stats.topology_efficiency = 0.9
        mock_stats.avg_latency = 15.0
        mock_stats.total_bandwidth = 500.0
        mock_stats.message_throughput = 100.0
        
        self.mock_registry.get_network_stats.return_value = mock_stats
        
        # Mock topology info
        mock_topology = {
            "topology_type": "mesh",
            "agents": ["agent_001", "agent_002"],
            "connections": []
        }
        self.mock_registry.get_network_topology.return_value = mock_topology
        
        result = self.cognitive_tool.monitor_network_health(detailed=True)
        
        self.assertTrue(result["success"])
        self.assertIn("overall_health", result)
        self.assertIn("health_score", result)
        self.assertIn("component_health", result)
        self.assertIn("detailed_metrics", result)
        
        # Health should be "healthy" with good stats
        self.assertEqual(result["overall_health"], "healthy")

    def test_monitor_network_health_critical(self):
        """Test network health monitoring with critical conditions."""
        # Mock critical network stats
        mock_stats = MagicMock()
        mock_stats.total_agents = 0  # No agents
        mock_stats.active_agents = 0
        mock_stats.total_connections = 0
        mock_stats.error_rate = 0.5  # High error rate
        mock_stats.cognitive_compatibility_avg = 0.3  # Low compatibility
        mock_stats.topology_efficiency = 0.1
        
        self.mock_registry.get_network_stats.return_value = mock_stats
        self.mock_registry.get_network_topology.return_value = {}
        
        result = self.cognitive_tool.monitor_network_health()
        
        self.assertTrue(result["success"])
        self.assertEqual(result["overall_health"], "critical")
        self.assertLess(result["health_score"], 2.0)

    def test_reconfigure_network_topology(self):
        """Test Core Method 9: reconfigure_network_topology."""
        # Mock current topology
        self.mock_registry.topology = NetworkTopology.MESH
        
        # Mock successful reconfiguration
        self.mock_registry.reconfigure_topology.return_value = True
        
        # Mock network stats before and after
        pre_stats = MagicMock()
        pre_stats.total_connections = 6
        pre_stats.topology_efficiency = 0.8
        
        post_stats = MagicMock()
        post_stats.total_connections = 4
        post_stats.topology_efficiency = 0.7
        
        self.mock_registry.get_network_stats.side_effect = [pre_stats, post_stats]
        
        result = self.cognitive_tool.reconfigure_network_topology("star")
        
        self.assertTrue(result["success"])
        self.assertEqual(result["topology_change"]["from"], "mesh")
        self.assertEqual(result["topology_change"]["to"], "star")
        self.assertIn("impact", result)

    def test_reconfigure_same_topology(self):
        """Test reconfiguring to the same topology."""
        self.mock_registry.topology = NetworkTopology.MESH
        
        result = self.cognitive_tool.reconfigure_network_topology(
            NetworkTopology.MESH,
            force=False
        )
        
        self.assertTrue(result["success"])
        self.assertIn("already matches", result["message"])

    def test_reconfigure_topology_failure(self):
        """Test topology reconfiguration failure."""
        self.mock_registry.reconfigure_topology.return_value = False
        
        # Mock network stats
        mock_stats = MagicMock()
        self.mock_registry.get_network_stats.return_value = mock_stats
        
        result = self.cognitive_tool.reconfigure_network_topology("star")
        
        self.assertFalse(result["success"])
        self.assertIn("Failed to reconfigure", result["error"])

    def test_get_tool_status(self):
        """Test getting tool status."""
        # Add some test data
        self.cognitive_tool.active_coordinations["test_coord"] = MagicMock()
        self.cognitive_tool.message_history.append(MagicMock())
        self.cognitive_tool.broadcast_cache["test_broadcast"] = MagicMock()
        
        status = self.cognitive_tool.get_tool_status()
        
        self.assertEqual(status["agent_id"], "test_agent")
        self.assertEqual(status["active_coordinations"], 1)
        self.assertEqual(status["message_history_size"], 1)
        self.assertEqual(status["broadcast_cache_size"], 1)
        self.assertIn("timestamp", status)

    def test_clear_history(self):
        """Test clearing tool history."""
        # Add some test data with timestamps
        old_time = time.time() - 48 * 3600  # 48 hours ago
        recent_time = time.time()
        
        old_message = MagicMock()
        old_message.timestamp = old_time
        recent_message = MagicMock()
        recent_message.timestamp = recent_time
        
        self.cognitive_tool.message_history = [old_message, recent_message]
        
        result = self.cognitive_tool.clear_history(older_than_hours=24.0)
        
        self.assertTrue(result["success"])
        self.assertEqual(result["messages_cleared"], 1)
        self.assertEqual(len(self.cognitive_tool.message_history), 1)

    def test_error_handling(self):
        """Test error handling in various scenarios."""
        # Test with invalid intent
        result = self.cognitive_tool.send_cognitive_message(
            target_agent_id="target",
            intent="invalid_intent",
            frame="task_delegation",
            content={}
        )
        
        self.assertFalse(result["success"])
        self.assertIn("error", result)

    def _create_mock_agent(self, agent_id, capabilities):
        """Helper method to create mock agents."""
        mock_agent = MagicMock()
        mock_agent.agent_id = agent_id
        mock_agent.hostname = "localhost"
        mock_agent.port = 8080
        mock_agent.status = AgentStatus.ACTIVE
        mock_agent.cognitive_compatibility_score = 0.8
        mock_agent.protocols = [CommunicationProtocol.HTTP]
        mock_agent.metadata = {"type": "test_agent"}
        mock_agent.load_metrics = {"cpu_usage": 50.0, "memory_usage": 60.0, "task_queue_length": 2}
        
        # Mock capabilities
        mock_capabilities = []
        for cap_name in capabilities:
            mock_cap = MagicMock()
            mock_cap.name = cap_name
            mock_cap.version = "1.0"
            mock_cap.description = f"{cap_name} capability"
            mock_capabilities.append(mock_cap)
        
        mock_agent.capabilities = mock_capabilities
        return mock_agent


class TestAgentZeroIntegration(unittest.TestCase):
    """Test Agent Zero integration functionality."""

    def setUp(self):
        """Set up test fixtures."""
        self.mock_registry = MagicMock(spec=DistributedNetworkRegistry)
        self.cognitive_tool = CognitiveNetworkTool("test_agent", self.mock_registry)

    def test_enhanced_call_subordinate(self):
        """Test enhanced call_subordinate functionality."""
        result = AgentZeroIntegration.enhanced_call_subordinate(
            cognitive_tool=self.cognitive_tool,
            subordinate_id="subordinate_001",
            task="Complete data processing",
            cognitive_intent="delegate",
            coordination_type="synchronous"
        )
        
        self.assertTrue(result["success"])
        self.assertEqual(result["subordinate_id"], "subordinate_001")
        self.assertEqual(result["task"], "Complete data processing")
        self.assertIn("cognitive_message", result)
        self.assertIn("message_id", result)

    def test_enhanced_call_subordinate_error(self):
        """Test enhanced call_subordinate error handling."""
        # Force an error by using invalid intent
        result = AgentZeroIntegration.enhanced_call_subordinate(
            cognitive_tool=self.cognitive_tool,
            subordinate_id="subordinate_001",
            task="Test task",
            cognitive_intent="invalid_intent"
        )
        
        self.assertFalse(result["success"])
        self.assertIn("error", result)


class TestDataStructures(unittest.TestCase):
    """Test data structures used by cognitive network tool."""

    def test_task_coordination_request(self):
        """Test TaskCoordinationRequest data structure."""
        request = TaskCoordinationRequest(
            task_id="task_001",
            task_description="Test task",
            required_capabilities=["computation"],
            max_partners=3,
            coordination_type="synchronous",
            deadline="2024-01-15",
            priority="high"
        )
        
        self.assertEqual(request.task_id, "task_001")
        self.assertEqual(request.task_description, "Test task")
        self.assertEqual(request.required_capabilities, ["computation"])
        self.assertEqual(request.max_partners, 3)
        self.assertEqual(request.deadline, "2024-01-15")

    def test_coordination_response(self):
        """Test CoordinationResponse data structure."""
        response = CoordinationResponse(
            success=True,
            assigned_agents=["agent_001", "agent_002"],
            coordination_plan={"type": "parallel"},
            estimated_completion="2024-01-16"
        )
        
        self.assertTrue(response.success)
        self.assertEqual(len(response.assigned_agents), 2)
        self.assertIn("agent_001", response.assigned_agents)

    def test_broadcast_message(self):
        """Test BroadcastMessage data structure."""
        message = BroadcastMessage(
            message_id="broadcast_001",
            sender_id="sender_agent",
            content={"announcement": "System update"},
            target_capabilities=["computation"],
            priority="high",
            ttl=5
        )
        
        self.assertEqual(message.message_id, "broadcast_001")
        self.assertEqual(message.sender_id, "sender_agent")
        self.assertEqual(message.content["announcement"], "System update")
        self.assertEqual(message.ttl, 5)


if __name__ == '__main__':
    unittest.main()