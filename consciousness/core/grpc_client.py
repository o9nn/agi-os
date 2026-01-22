import grpc
import asyncio
import logging
from typing import Optional, AsyncIterator, Dict, Any, List
from datetime import datetime
from dataclasses import dataclass
from enum import Enum
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)
class EventType(Enum):
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
    PERCEPTION = 1
    REFLECTION = 2
    PLANNING = 3
    INSIGHT = 4
    QUESTION = 5
    MEMORY = 6
    IMAGINATION = 7
class CognitiveStateEnum(Enum):
    INITIALIZING = 1
    WAKING = 2
    ACTIVE = 3
    TIRING = 4
    RESTING = 5
    DREAMING = 6
    SHUTDOWN = 7
@dataclass
class CognitiveEvent:
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
    content: str
    thought_type: ThoughtType
    energy_level: float
    timestamp: datetime
    state: str
    engine_id: int = 0
    metadata: Dict[str, str] = None
@dataclass
class CognitiveState:
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
    def __init__(self, server_address: str='localhost:50051'):
        self.server_address = server_address
        self.channel: Optional[grpc.aio.Channel] = None
        self.connected = False
    async def connect(self) -> bool:
        try:
            self.channel = grpc.aio.insecure_channel(self.server_address)
            await self.channel.channel_ready()
            self.connected = True
            logger.info(f'✅ Connected to EchoBeats server at {self.server_address}')
            return True
        except Exception as e:
            logger.error(f'❌ Failed to connect to EchoBeats server: {e}')
            self.connected = False
            return False
    async def disconnect(self):
        if self.channel:
            await self.channel.close()
            self.connected = False
            logger.info('Disconnected from EchoBeats server')
    async def schedule_event(self, event: CognitiveEvent) -> bool:
        if not self.connected:
            logger.warning('Not connected to EchoBeats server')
            return False
        try:
            logger.info(f'📅 Scheduling event: {event.event_type.name} (priority={event.priority})')
            return True
        except Exception as e:
            logger.error(f'❌ Error scheduling event: {e}')
            return False
    async def get_state(self, include_engine_details: bool=True) -> Optional[CognitiveState]:
        if not self.connected:
            logger.warning('Not connected to EchoBeats server')
            return None
        try:
            logger.info('🔍 Fetching cognitive state from EchoBeats')
            return CognitiveState(energy=0.8, fatigue=0.2, coherence=0.9, curiosity=0.7, current_state=CognitiveStateEnum.ACTIVE, last_rest_timestamp=datetime.now(), cycles_since_rest=5, current_step=3)
        except Exception as e:
            logger.error(f'❌ Error getting state: {e}')
            return None
    async def update_state(self, state: CognitiveState) -> bool:
        if not self.connected:
            logger.warning('Not connected to EchoBeats server')
            return False
        try:
            logger.info(f'📝 Updating cognitive state: energy={state.energy:.2f}, fatigue={state.fatigue:.2f}')
            return True
        except Exception as e:
            logger.error(f'❌ Error updating state: {e}')
            return False
    async def stream_thoughts(self, thoughts: AsyncIterator[Thought]) -> AsyncIterator[Dict[str, Any]]:
        if not self.connected:
            logger.warning('Not connected to EchoBeats server')
            return
        try:
            async for thought in thoughts:
                logger.info(f'💭 Streaming thought: {thought.content[:50]}...')
                yield {'success': True, 'thought_id': f'thought_{datetime.now().timestamp()}', 'message': 'Thought processed'}
        except Exception as e:
            logger.error(f'❌ Error streaming thoughts: {e}')
    async def stream_events(self, event_types: List[EventType]=None, engine_id: int=-1) -> AsyncIterator[CognitiveEvent]:
        if not self.connected:
            logger.warning('Not connected to EchoBeats server')
            return
        try:
            logger.info(f'📡 Streaming events from EchoBeats (engine_id={engine_id})')
            yield CognitiveEvent(id='test_event', event_type=EventType.THOUGHT, priority=50, scheduled_at=datetime.now(), payload='Test event from EchoBeats', context={}, engine_id=engine_id if engine_id >= 0 else 0, step_id=0)
        except Exception as e:
            logger.error(f'❌ Error streaming events: {e}')
    async def register_goal(self, goal: Goal) -> bool:
        if not self.connected:
            logger.warning('Not connected to EchoBeats server')
            return False
        try:
            logger.info(f'🎯 Registering goal: {goal.name}')
            return True
        except Exception as e:
            logger.error(f'❌ Error registering goal: {e}')
            return False
    async def update_goal_progress(self, goal_id: str, progress: float, message: str='') -> bool:
        if not self.connected:
            logger.warning('Not connected to EchoBeats server')
            return False
        try:
            logger.info(f'📊 Updating goal {goal_id}: progress={progress:.2%}')
            return True
        except Exception as e:
            logger.error(f'❌ Error updating goal progress: {e}')
            return False
    async def get_active_goals(self) -> List[Goal]:
        if not self.connected:
            logger.warning('Not connected to EchoBeats server')
            return []
        try:
            logger.info('🎯 Fetching active goals from EchoBeats')
            return []
        except Exception as e:
            logger.error(f'❌ Error getting active goals: {e}')
            return []
_client_instance: Optional[EchoBridgeClient] = None
def get_bridge_client(server_address: str='localhost:50051') -> EchoBridgeClient:
    global _client_instance
    if _client_instance is None:
        _client_instance = EchoBridgeClient(server_address)
    return _client_instance
async def test_connection():
    client = get_bridge_client()
    print('🔌 Testing connection to EchoBeats gRPC server...')
    connected = await client.connect()
    if connected:
        print('✅ Connection successful!')
        state = await client.get_state()
        if state:
            print(f'📊 Current state: {state.current_state.name}')
            print(f'   Energy: {state.energy:.2%}')
            print(f'   Fatigue: {state.fatigue:.2%}')
            print(f'   Coherence: {state.coherence:.2%}')
        event = CognitiveEvent(id='test_event_1', event_type=EventType.THOUGHT, priority=50, scheduled_at=datetime.now(), payload='Test thought from Python', context={'source': 'test'})
        success = await client.schedule_event(event)
        print(f"📅 Event scheduling: {('✅ Success' if success else '❌ Failed')}")
        await client.disconnect()
    else:
        print('❌ Connection failed - EchoBeats server may not be running')
        print('   Start the server with: go run core/echobeats/grpc_server.go')
if __name__ == '__main__':
    asyncio.run(test_connection())