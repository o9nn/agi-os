#!/usr/bin/env python3
"""
gRPC Client for Python Autonomous Core
Connects to Go EchoBeats scheduler for unified cognitive orchestration
"""

import grpc
import asyncio
import logging
from typing import Optional, AsyncIterator, Dict, Any, List
from datetime import datetime
from dataclasses import dataclass
from enum import Enum

# Note: These imports will work after generating Python code from proto
# For now, we'll create stub classes to enable the architecture
# Run: python -m grpc_tools.protoc -I. --python_out=. --grpc_python_out=. echobridge/echobridge.proto

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class EventType(Enum):
    """Cognitive event types"""
    THOUGHT = 1
    PERCEPTION = 2
    ACTION = 3
    LEARNING = 4
    MEMORY_CONSOLIDATION = 5
    GOAL_PURSUIT = 6
    SOCIAL_INTERACTION = 7
    INTROSPECTION = 8
    DREAM = 9
    WAKE = 10
    REST = 11
    SKILL_PRACTICE = 12
    KNOWLEDGE_INTEGRATION = 13
    PATTERN_RECOGNITION = 14


class ThoughtType(Enum):
    """Thought classification types"""
    PERCEPTION = 1
    REFLECTION = 2
    PLANNING = 3
    INSIGHT = 4
    QUESTION = 5
    MEMORY = 6
    IMAGINATION = 7


class CognitiveStateEnum(Enum):
    """Cognitive states"""
    INITIALIZING = 1
    WAKING = 2
    ACTIVE = 3
    TIRING = 4
    RESTING = 5
    DREAMING = 6
    SHUTDOWN = 7


@dataclass
class CognitiveEvent:
    """Represents a cognitive event"""
    id: str
    event_type: EventType
    priority: int
    scheduled_at: datetime
    payload: str
    context: Dict[str, str]
    recurring: bool = False
    interval_ms: int = 0
    engine_id: int = 0
    step_id: int = 0


@dataclass
class Thought:
    """Represents a generated thought"""
    content: str
    thought_type: ThoughtType
    energy_level: float
    timestamp: datetime
    state: str
    engine_id: int = 0
    metadata: Dict[str, str] = None


@dataclass
class CognitiveState:
    """Represents cognitive state"""
    energy: float
    fatigue: float
    coherence: float
    curiosity: float
    current_state: CognitiveStateEnum
    last_rest_timestamp: datetime
    cycles_since_rest: int
    current_step: int = 0


@dataclass
class Goal:
    """Represents a cognitive goal"""
    id: str
    name: str
    description: str
    priority: int
    progress: float
    target: float
    deadline: datetime
    status: str
    required_skills: List[str]
    knowledge_gaps: List[str]


class EchoBridgeClient:
    """
    gRPC client for communicating with Go EchoBeats scheduler
    Enables Python autonomous core to leverage Go's efficient event scheduling
    """
    
    def __init__(self, server_address: str = "localhost:50051"):
        self.server_address = server_address
        self.channel: Optional[grpc.aio.Channel] = None
        self.connected = False
        
    async def connect(self) -> bool:
        """Establish connection to EchoBeats gRPC server"""
        try:
            self.channel = grpc.aio.insecure_channel(self.server_address)
            # Test connection
            await self.channel.channel_ready()
            self.connected = True
            logger.info(f"✅ Connected to EchoBeats server at {self.server_address}")
            return True
        except Exception as e:
            logger.error(f"❌ Failed to connect to EchoBeats server: {e}")
            self.connected = False
            return False
    
    async def disconnect(self):
        """Close connection to server"""
        if self.channel:
            await self.channel.close()
            self.connected = False
            logger.info("Disconnected from EchoBeats server")
    
    async def schedule_event(self, event: CognitiveEvent) -> bool:
        """
        Schedule a cognitive event for processing
        
        Args:
            event: CognitiveEvent to schedule
            
        Returns:
            bool: Success status
        """
        if not self.connected:
            logger.warning("Not connected to EchoBeats server")
            return False
        
        try:
            # This would use the generated gRPC stub
            # For now, log the event
            logger.info(f"📅 Scheduling event: {event.event_type.name} (priority={event.priority})")
            
            # TODO: Implement actual gRPC call when proto is compiled
            # stub = echobridge_pb2_grpc.EchoBridgeStub(self.channel)
            # request = self._event_to_proto(event)
            # response = await stub.ScheduleEvent(request)
            # return response.success
            
            return True
        except Exception as e:
            logger.error(f"❌ Error scheduling event: {e}")
            return False
    
    async def get_state(self, include_engine_details: bool = True) -> Optional[CognitiveState]:
        """
        Get current cognitive state from EchoBeats
        
        Args:
            include_engine_details: Whether to include 3-engine state details
            
        Returns:
            CognitiveState or None if error
        """
        if not self.connected:
            logger.warning("Not connected to EchoBeats server")
            return None
        
        try:
            # TODO: Implement actual gRPC call
            logger.info("🔍 Fetching cognitive state from EchoBeats")
            
            # Placeholder return
            return CognitiveState(
                energy=0.8,
                fatigue=0.2,
                coherence=0.9,
                curiosity=0.7,
                current_state=CognitiveStateEnum.ACTIVE,
                last_rest_timestamp=datetime.now(),
                cycles_since_rest=5,
                current_step=3
            )
        except Exception as e:
            logger.error(f"❌ Error getting state: {e}")
            return None
    
    async def update_state(self, state: CognitiveState) -> bool:
        """
        Update cognitive state in EchoBeats
        
        Args:
            state: CognitiveState to update
            
        Returns:
            bool: Success status
        """
        if not self.connected:
            logger.warning("Not connected to EchoBeats server")
            return False
        
        try:
            logger.info(f"📝 Updating cognitive state: energy={state.energy:.2f}, fatigue={state.fatigue:.2f}")
            
            # TODO: Implement actual gRPC call
            return True
        except Exception as e:
            logger.error(f"❌ Error updating state: {e}")
            return False
    
    async def stream_thoughts(self, thoughts: AsyncIterator[Thought]) -> AsyncIterator[Dict[str, Any]]:
        """
        Stream thoughts to EchoBeats for processing
        
        Args:
            thoughts: AsyncIterator of Thought objects
            
        Yields:
            Response dictionaries from EchoBeats
        """
        if not self.connected:
            logger.warning("Not connected to EchoBeats server")
            return
        
        try:
            async for thought in thoughts:
                logger.info(f"💭 Streaming thought: {thought.content[:50]}...")
                
                # TODO: Implement actual gRPC streaming call
                yield {
                    "success": True,
                    "thought_id": f"thought_{datetime.now().timestamp()}",
                    "message": "Thought processed"
                }
        except Exception as e:
            logger.error(f"❌ Error streaming thoughts: {e}")
    
    async def stream_events(self, event_types: List[EventType] = None, engine_id: int = -1) -> AsyncIterator[CognitiveEvent]:
        """
        Stream cognitive events from EchoBeats
        
        Args:
            event_types: Filter by event types (None = all types)
            engine_id: Filter by engine ID (-1 = all engines)
            
        Yields:
            CognitiveEvent objects from EchoBeats
        """
        if not self.connected:
            logger.warning("Not connected to EchoBeats server")
            return
        
        try:
            logger.info(f"📡 Streaming events from EchoBeats (engine_id={engine_id})")
            
            # TODO: Implement actual gRPC streaming call
            # This would receive events from Go scheduler and yield them
            # For now, this is a placeholder
            yield CognitiveEvent(
                id="test_event",
                event_type=EventType.THOUGHT,
                priority=50,
                scheduled_at=datetime.now(),
                payload="Test event from EchoBeats",
                context={},
                engine_id=engine_id if engine_id >= 0 else 0,
                step_id=0
            )
        except Exception as e:
            logger.error(f"❌ Error streaming events: {e}")
    
    async def register_goal(self, goal: Goal) -> bool:
        """
        Register a new goal with EchoBeats
        
        Args:
            goal: Goal to register
            
        Returns:
            bool: Success status
        """
        if not self.connected:
            logger.warning("Not connected to EchoBeats server")
            return False
        
        try:
            logger.info(f"🎯 Registering goal: {goal.name}")
            
            # TODO: Implement actual gRPC call
            return True
        except Exception as e:
            logger.error(f"❌ Error registering goal: {e}")
            return False
    
    async def update_goal_progress(self, goal_id: str, progress: float, message: str = "") -> bool:
        """
        Update progress for a goal
        
        Args:
            goal_id: Goal identifier
            progress: Progress value (0.0 to 1.0)
            message: Optional progress message
            
        Returns:
            bool: Success status
        """
        if not self.connected:
            logger.warning("Not connected to EchoBeats server")
            return False
        
        try:
            logger.info(f"📊 Updating goal {goal_id}: progress={progress:.2%}")
            
            # TODO: Implement actual gRPC call
            return True
        except Exception as e:
            logger.error(f"❌ Error updating goal progress: {e}")
            return False
    
    async def get_active_goals(self) -> List[Goal]:
        """
        Get list of active goals from EchoBeats
        
        Returns:
            List of Goal objects
        """
        if not self.connected:
            logger.warning("Not connected to EchoBeats server")
            return []
        
        try:
            logger.info("🎯 Fetching active goals from EchoBeats")
            
            # TODO: Implement actual gRPC call
            return []
        except Exception as e:
            logger.error(f"❌ Error getting active goals: {e}")
            return []


# Singleton instance
_client_instance: Optional[EchoBridgeClient] = None


def get_bridge_client(server_address: str = "localhost:50051") -> EchoBridgeClient:
    """Get singleton instance of EchoBridge client"""
    global _client_instance
    if _client_instance is None:
        _client_instance = EchoBridgeClient(server_address)
    return _client_instance


async def test_connection():
    """Test gRPC connection to EchoBeats"""
    client = get_bridge_client()
    
    print("🔌 Testing connection to EchoBeats gRPC server...")
    connected = await client.connect()
    
    if connected:
        print("✅ Connection successful!")
        
        # Test state retrieval
        state = await client.get_state()
        if state:
            print(f"📊 Current state: {state.current_state.name}")
            print(f"   Energy: {state.energy:.2%}")
            print(f"   Fatigue: {state.fatigue:.2%}")
            print(f"   Coherence: {state.coherence:.2%}")
        
        # Test event scheduling
        event = CognitiveEvent(
            id="test_event_1",
            event_type=EventType.THOUGHT,
            priority=50,
            scheduled_at=datetime.now(),
            payload="Test thought from Python",
            context={"source": "test"}
        )
        success = await client.schedule_event(event)
        print(f"📅 Event scheduling: {'✅ Success' if success else '❌ Failed'}")
        
        await client.disconnect()
    else:
        print("❌ Connection failed - EchoBeats server may not be running")
        print("   Start the server with: go run core/echobeats/grpc_server.go")


if __name__ == "__main__":
    asyncio.run(test_connection())
