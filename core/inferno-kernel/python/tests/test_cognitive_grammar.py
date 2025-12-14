"""
Comprehensive test suite for the Cognitive Grammar Framework.

Tests all major functionality including message creation, natural language generation,
JSON serialization/deserialization, validation, and parsing.
"""

import unittest
import json
import time
from unittest.mock import patch, MagicMock

import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

from helpers.cognitive_grammar import (
    CognitiveGrammarFramework,
    CognitiveMessage,
    CommunicativeIntent,
    CognitiveFrame,
    CognitiveRole
)


class TestCognitiveGrammarFramework(unittest.TestCase):
    """Test cases for the cognitive grammar framework."""

    def setUp(self):
        """Set up test fixtures."""
        self.framework = CognitiveGrammarFramework()

    def test_create_message(self):
        """Test basic message creation."""
        message = self.framework.create_message(
            intent=CommunicativeIntent.REQUEST,
            frame=CognitiveFrame.TASK_DELEGATION,
            agent_id="test_agent",
            content={"task": "test task"},
            roles={CognitiveRole.AGENT: "test_agent"}
        )
        
        self.assertIsInstance(message, CognitiveMessage)
        self.assertEqual(message.intent, CommunicativeIntent.REQUEST)
        self.assertEqual(message.frame, CognitiveFrame.TASK_DELEGATION)
        self.assertEqual(message.agent_id, "test_agent")
        self.assertEqual(message.content["task"], "test task")
        self.assertIsNotNone(message.message_id)
        self.assertIsNotNone(message.timestamp)

    def test_task_delegation_message(self):
        """Test task delegation message creation."""
        message = self.framework.create_task_delegation_message(
            agent_id="agent_001",
            task_description="Implement feature X",
            assignee="agent_002",
            deadline="2024-01-15",
            priority="high"
        )
        
        self.assertEqual(message.intent, CommunicativeIntent.DELEGATE)
        self.assertEqual(message.frame, CognitiveFrame.TASK_DELEGATION)
        self.assertEqual(message.content["task"], "Implement feature X")
        self.assertEqual(message.content["deadline"], "2024-01-15")
        self.assertEqual(message.content["priority"], "high")
        self.assertIn(CognitiveRole.AGENT, message.roles)
        self.assertIn(CognitiveRole.PATIENT, message.roles)

    def test_information_sharing_message(self):
        """Test information sharing message creation."""
        message = self.framework.create_information_sharing_message(
            agent_id="agent_001",
            topic="system status",
            information="All systems operational",
            recipient="agent_002"
        )
        
        self.assertEqual(message.intent, CommunicativeIntent.INFORM)
        self.assertEqual(message.frame, CognitiveFrame.INFORMATION_SHARING)
        self.assertEqual(message.content["topic"], "system status")
        self.assertEqual(message.content["information"], "All systems operational")

    def test_coordination_message(self):
        """Test coordination message creation."""
        participants = ["agent_002", "agent_003"]
        message = self.framework.create_coordination_message(
            agent_id="agent_001",
            task="database migration",
            participants=participants,
            coordination_type="synchronous"
        )
        
        self.assertEqual(message.intent, CommunicativeIntent.COORDINATE)
        self.assertEqual(message.frame, CognitiveFrame.COORDINATION)
        self.assertEqual(message.content["task"], "database migration")
        self.assertEqual(message.content["coordination_type"], "synchronous")
        self.assertEqual(message.content["participants"], participants)

    def test_capability_query_message(self):
        """Test capability query message creation."""
        capabilities = ["computation", "planning"]
        message = self.framework.create_capability_query_message(
            agent_id="agent_001",
            required_capabilities=capabilities,
            target_agent="agent_002"
        )
        
        self.assertEqual(message.intent, CommunicativeIntent.QUERY)
        self.assertEqual(message.frame, CognitiveFrame.CAPABILITY_NEGOTIATION)
        self.assertEqual(message.content["required_capabilities"], capabilities)

    def test_natural_language_generation(self):
        """Test natural language generation for different message types."""
        # Test task delegation
        task_msg = self.framework.create_task_delegation_message(
            agent_id="agent_001",
            task_description="Implement API",
            assignee="agent_002"
        )
        nl_text = task_msg.to_natural_language()
        self.assertIn("agent_001", nl_text)
        self.assertIn("delegates", nl_text)
        self.assertIn("agent_002", nl_text)
        
        # Test information sharing
        info_msg = self.framework.create_information_sharing_message(
            agent_id="agent_001",
            topic="status",
            information="System ready"
        )
        nl_text = info_msg.to_natural_language()
        self.assertIn("agent_001", nl_text)
        self.assertIn("informs", nl_text)
        self.assertIn("status", nl_text)

    def test_json_serialization(self):
        """Test JSON serialization and deserialization."""
        original_message = self.framework.create_task_delegation_message(
            agent_id="agent_001",
            task_description="Test task",
            assignee="agent_002"
        )
        
        # Serialize to JSON
        json_str = original_message.to_json()
        self.assertIsInstance(json_str, str)
        
        # Verify JSON is valid
        json_data = json.loads(json_str)
        self.assertIn("intent", json_data)
        self.assertIn("frame", json_data)
        self.assertIn("content", json_data)
        
        # Deserialize from JSON
        restored_message = CognitiveMessage.from_json(json_str)
        
        # Verify restoration
        self.assertEqual(original_message.intent, restored_message.intent)
        self.assertEqual(original_message.frame, restored_message.frame)
        self.assertEqual(original_message.agent_id, restored_message.agent_id)
        self.assertEqual(original_message.content, restored_message.content)

    def test_message_validation(self):
        """Test message validation."""
        # Valid message
        valid_message = self.framework.create_message(
            intent=CommunicativeIntent.REQUEST,
            frame=CognitiveFrame.TASK_DELEGATION,
            agent_id="test_agent",
            content={"task": "test"},
            roles={CognitiveRole.AGENT: "test_agent"}
        )
        self.assertTrue(self.framework.validate_message(valid_message))
        
        # Invalid message (no agent_id)
        invalid_message = CognitiveMessage(
            intent=CommunicativeIntent.REQUEST,
            frame=CognitiveFrame.TASK_DELEGATION,
            roles={},
            content={},
            agent_id=""  # Empty agent ID
        )
        self.assertFalse(self.framework.validate_message(invalid_message))

    def test_natural_language_parsing(self):
        """Test parsing natural language into cognitive messages."""
        # Test request parsing
        request_text = "Please complete the task"
        message = self.framework.parse_natural_language(request_text, "agent_001")
        self.assertIsNotNone(message)
        self.assertEqual(message.intent, CommunicativeIntent.REQUEST)
        
        # Test inform parsing
        inform_text = "The system status is operational"
        message = self.framework.parse_natural_language(inform_text, "agent_001")
        self.assertIsNotNone(message)
        self.assertEqual(message.intent, CommunicativeIntent.INFORM)
        
        # Test query parsing
        query_text = "What are the available capabilities?"
        message = self.framework.parse_natural_language(query_text, "agent_001")
        self.assertIsNotNone(message)
        self.assertEqual(message.intent, CommunicativeIntent.QUERY)

    def test_communicative_intents(self):
        """Test all communicative intent types."""
        intents = [
            CommunicativeIntent.REQUEST,
            CommunicativeIntent.INFORM,
            CommunicativeIntent.COORDINATE,
            CommunicativeIntent.DELEGATE,
            CommunicativeIntent.QUERY,
            CommunicativeIntent.CONFIRM,
            CommunicativeIntent.REJECT,
            CommunicativeIntent.NEGOTIATE
        ]
        
        for intent in intents:
            message = self.framework.create_message(
                intent=intent,
                frame=CognitiveFrame.INFORMATION_SHARING,
                agent_id="test_agent",
                content={"test": "content"}
            )
            self.assertEqual(message.intent, intent)
            self.assertTrue(self.framework.validate_message(message))

    def test_cognitive_frames(self):
        """Test all cognitive frame types."""
        frames = [
            CognitiveFrame.TASK_DELEGATION,
            CognitiveFrame.INFORMATION_SHARING,
            CognitiveFrame.COORDINATION,
            CognitiveFrame.CAPABILITY_NEGOTIATION,
            CognitiveFrame.RESOURCE_ALLOCATION,
            CognitiveFrame.ERROR_HANDLING,
            CognitiveFrame.STATUS_REPORTING
        ]
        
        for frame in frames:
            message = self.framework.create_message(
                intent=CommunicativeIntent.INFORM,
                frame=frame,
                agent_id="test_agent",
                content={"test": "content"}
            )
            self.assertEqual(message.frame, frame)
            self.assertTrue(self.framework.validate_message(message))

    def test_cognitive_roles(self):
        """Test cognitive role assignments."""
        roles = {
            CognitiveRole.AGENT: "agent_001",
            CognitiveRole.PATIENT: "agent_002",
            CognitiveRole.INSTRUMENT: "tool_001",
            CognitiveRole.LOCATION: "server_001",
            CognitiveRole.TIME: "2024-01-15",
            CognitiveRole.MANNER: "efficiently",
            CognitiveRole.PURPOSE: "optimization"
        }
        
        message = self.framework.create_message(
            intent=CommunicativeIntent.REQUEST,
            frame=CognitiveFrame.TASK_DELEGATION,
            agent_id="agent_001",
            content={"task": "test task"},
            roles=roles
        )
        
        self.assertEqual(len(message.roles), len(roles))
        for role, value in roles.items():
            self.assertIn(role, message.roles)
            self.assertEqual(message.roles[role], value)

    def test_error_handling(self):
        """Test error handling in various scenarios."""
        # Test invalid JSON deserialization
        with self.assertRaises(Exception):
            CognitiveMessage.from_json("invalid json")
        
        # Test invalid enum values
        with self.assertRaises(ValueError):
            CommunicativeIntent("invalid_intent")
        
        with self.assertRaises(ValueError):
            CognitiveFrame("invalid_frame")

    def test_message_threading(self):
        """Test message threading with response_to field."""
        original_message = self.framework.create_message(
            intent=CommunicativeIntent.QUERY,
            frame=CognitiveFrame.INFORMATION_SHARING,
            agent_id="agent_001",
            content={"question": "What is the status?"}
        )
        
        response_message = self.framework.create_message(
            intent=CommunicativeIntent.INFORM,
            frame=CognitiveFrame.INFORMATION_SHARING,
            agent_id="agent_002",
            content={"answer": "Status is operational"}
        )
        response_message.response_to = original_message.message_id
        
        self.assertEqual(response_message.response_to, original_message.message_id)


class TestCognitiveMessageComponents(unittest.TestCase):
    """Test individual components of cognitive messages."""

    def test_cognitive_message_creation(self):
        """Test direct cognitive message creation."""
        message = CognitiveMessage(
            intent=CommunicativeIntent.REQUEST,
            frame=CognitiveFrame.TASK_DELEGATION,
            roles={CognitiveRole.AGENT: "agent_001"},
            content={"task": "test"},
            agent_id="agent_001",
            timestamp=time.time(),
            message_id="test_msg_001"
        )
        
        self.assertEqual(message.intent, CommunicativeIntent.REQUEST)
        self.assertEqual(message.frame, CognitiveFrame.TASK_DELEGATION)
        self.assertEqual(message.agent_id, "agent_001")
        self.assertEqual(message.message_id, "test_msg_001")

    def test_enum_values(self):
        """Test enum value correctness."""
        # Test CommunicativeIntent values
        self.assertEqual(CommunicativeIntent.REQUEST.value, "request")
        self.assertEqual(CommunicativeIntent.INFORM.value, "inform")
        self.assertEqual(CommunicativeIntent.COORDINATE.value, "coordinate")
        
        # Test CognitiveFrame values
        self.assertEqual(CognitiveFrame.TASK_DELEGATION.value, "task_delegation")
        self.assertEqual(CognitiveFrame.INFORMATION_SHARING.value, "information_sharing")
        
        # Test CognitiveRole values
        self.assertEqual(CognitiveRole.AGENT.value, "agent")
        self.assertEqual(CognitiveRole.PATIENT.value, "patient")


if __name__ == '__main__':
    unittest.main()