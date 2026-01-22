from fastapi import FastAPI, WebSocket, WebSocketDisconnect, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel, Field
from typing import Dict, List, Optional, Any
from datetime import datetime
import asyncio
import json
import logging
from enum import Enum
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)
class CognitiveState(BaseModel):
    timestamp: datetime = Field(default_factory=datetime.now)
    active_processes: int = 0
    memory_usage: float = 0.0
    attention_focus: List[str] = []
    cognitive_load: float = 0.0
    status: str = 'operational'
class CognitiveTask(BaseModel):
    task_id: Optional[str] = None
    description: str
    priority: int = Field(default=5, ge=1, le=10)
    context: Dict[str, Any] = {}
    timeout: Optional[int] = None
class TaskResult(BaseModel):
    task_id: str
    status: str
    result: Optional[Any] = None
    confidence: float = 0.0
    processing_time: float = 0.0
    timestamp: datetime = Field(default_factory=datetime.now)
class AttentionAllocation(BaseModel):
    timestamp: datetime = Field(default_factory=datetime.now)
    allocations: Dict[str, float] = {}
    total_attention: float = 1.0
    focus_target: Optional[str] = None
class AgentInfo(BaseModel):
    agent_id: str
    agent_type: str
    capabilities: List[str] = []
    status: str = 'active'
    connection_time: datetime = Field(default_factory=datetime.now)
    metadata: Dict[str, Any] = {}
class AgentRegistration(BaseModel):
    agent_type: str
    capabilities: List[str] = []
    metadata: Dict[str, Any] = {}
class EventType(str, Enum):
    COGNITIVE_STATE_UPDATE = 'cognitive.state.update'
    ATTENTION_ALLOCATION_CHANGE = 'attention.allocation.change'
    AGENT_REGISTRATION = 'agent.registration'
    TASK_COMPLETION = 'task.completion'
class CognitiveNetworkState:
    def __init__(self):
        self.cognitive_state = CognitiveState()
        self.attention_allocation = AttentionAllocation()
        self.agents: Dict[str, AgentInfo] = {}
        self.tasks: Dict[str, TaskResult] = {}
        self.websocket_connections: List[WebSocket] = []
        self.task_counter = 0
    def generate_task_id(self) -> str:
        self.task_counter += 1
        return f'task_{self.task_counter}_{datetime.now().timestamp()}'
    async def broadcast_event(self, event_type: EventType, data: Any):
        message = {'event': event_type.value, 'data': data, 'timestamp': datetime.now().isoformat()}
        disconnected = []
        for connection in self.websocket_connections:
            try:
                await connection.send_json(message)
            except Exception as e:
                logger.error(f'Error broadcasting to connection: {e}')
                disconnected.append(connection)
        for conn in disconnected:
            self.websocket_connections.remove(conn)
app = FastAPI(title='Distributed Cognitive Mesh API', description='REST and WebSocket API for cognitive network access and embodiment', version='1.0.0')
app.add_middleware(CORSMiddleware, allow_origins=['*'], allow_credentials=True, allow_methods=['*'], allow_headers=['*'])
network_state = CognitiveNetworkState()
@app.get('/')
async def root():
    return {'service': 'Distributed Cognitive Mesh API', 'version': '1.0.0', 'status': 'operational', 'endpoints': {'cognitive_state': '/api/v1/cognitive/state', 'process_task': '/api/v1/cognitive/process', 'attention': '/api/v1/attention/allocation', 'register_agent': '/api/v1/agents/register', 'websocket': '/ws'}}
@app.get('/api/v1/cognitive/state', response_model=CognitiveState)
async def get_cognitive_state():
    return network_state.cognitive_state
@app.post('/api/v1/cognitive/process', response_model=TaskResult)
async def process_cognitive_task(task: CognitiveTask):
    if not task.task_id:
        task.task_id = network_state.generate_task_id()
    result = TaskResult(task_id=task.task_id, status='completed', result={'processed': True, 'description': task.description}, confidence=0.85, processing_time=0.042)
    network_state.tasks[task.task_id] = result
    network_state.cognitive_state.active_processes += 1
    network_state.cognitive_state.cognitive_load += 0.1
    await network_state.broadcast_event(EventType.TASK_COMPLETION, result.model_dump())
    return result
@app.get('/api/v1/cognitive/task/{task_id}', response_model=TaskResult)
async def get_task_result(task_id: str):
    if task_id not in network_state.tasks:
        raise HTTPException(status_code=404, detail='Task not found')
    return network_state.tasks[task_id]
@app.get('/api/v1/attention/allocation', response_model=AttentionAllocation)
async def get_attention_allocation():
    return network_state.attention_allocation
@app.post('/api/v1/attention/focus')
async def set_attention_focus(target: str, weight: float=1.0):
    network_state.attention_allocation.focus_target = target
    network_state.attention_allocation.allocations[target] = weight
    total = sum(network_state.attention_allocation.allocations.values())
    for key in network_state.attention_allocation.allocations:
        network_state.attention_allocation.allocations[key] /= total
    await network_state.broadcast_event(EventType.ATTENTION_ALLOCATION_CHANGE, network_state.attention_allocation.model_dump())
    return {'status': 'success', 'target': target, 'weight': weight}
@app.post('/api/v1/agents/register', response_model=AgentInfo)
async def register_agent(registration: AgentRegistration):
    import uuid
    agent_id = f'agent_{uuid.uuid4().hex[:8]}'
    agent = AgentInfo(agent_id=agent_id, agent_type=registration.agent_type, capabilities=registration.capabilities, metadata=registration.metadata)
    network_state.agents[agent_id] = agent
    await network_state.broadcast_event(EventType.AGENT_REGISTRATION, agent.model_dump())
    return agent
@app.get('/api/v1/agents/{agent_id}', response_model=AgentInfo)
async def get_agent(agent_id: str):
    if agent_id not in network_state.agents:
        raise HTTPException(status_code=404, detail='Agent not found')
    return network_state.agents[agent_id]
@app.get('/api/v1/agents', response_model=List[AgentInfo])
async def list_agents():
    return list(network_state.agents.values())
@app.delete('/api/v1/agents/{agent_id}')
async def unregister_agent(agent_id: str):
    if agent_id not in network_state.agents:
        raise HTTPException(status_code=404, detail='Agent not found')
    agent = network_state.agents.pop(agent_id)
    return {'status': 'unregistered', 'agent_id': agent_id}
@app.get('/api/v1/health')
async def health_check():
    return {'status': 'healthy', 'timestamp': datetime.now().isoformat(), 'agents': len(network_state.agents), 'active_connections': len(network_state.websocket_connections), 'tasks_processed': len(network_state.tasks)}
@app.websocket('/ws')
async def websocket_endpoint(websocket: WebSocket):
    await websocket.accept()
    network_state.websocket_connections.append(websocket)
    logger.info(f'WebSocket connection established. Total connections: {len(network_state.websocket_connections)}')
    try:
        await websocket.send_json({'event': 'connection.established', 'data': {'cognitive_state': network_state.cognitive_state.model_dump(), 'attention_allocation': network_state.attention_allocation.model_dump()}, 'timestamp': datetime.now().isoformat()})
        while True:
            data = await websocket.receive_text()
            message = json.loads(data)
            if message.get('type') == 'ping':
                await websocket.send_json({'type': 'pong', 'timestamp': datetime.now().isoformat()})
            elif message.get('type') == 'subscribe':
                events = message.get('events', [])
                await websocket.send_json({'type': 'subscribed', 'events': events, 'timestamp': datetime.now().isoformat()})
            elif message.get('type') == 'task':
                task_data = message.get('data', {})
                task = CognitiveTask(**task_data)
                result = await process_cognitive_task(task)
                await websocket.send_json({'type': 'task.result', 'data': result.model_dump(), 'timestamp': datetime.now().isoformat()})
    except WebSocketDisconnect:
        logger.info('WebSocket connection closed')
    except Exception as e:
        logger.error(f'WebSocket error: {e}')
    finally:
        if websocket in network_state.websocket_connections:
            network_state.websocket_connections.remove(websocket)
        logger.info(f'WebSocket connection removed. Total connections: {len(network_state.websocket_connections)}')
@app.on_event('startup')
async def startup_event():
    asyncio.create_task(simulate_cognitive_activity())
async def simulate_cognitive_activity():
    while True:
        await asyncio.sleep(5)
        network_state.cognitive_state.timestamp = datetime.now()
        network_state.cognitive_state.cognitive_load = max(0, network_state.cognitive_state.cognitive_load - 0.05)
        await network_state.broadcast_event(EventType.COGNITIVE_STATE_UPDATE, network_state.cognitive_state.model_dump())
if __name__ == '__main__':
    import uvicorn
    uvicorn.run(app, host='0.0.0.0', port=8000)