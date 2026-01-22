from __future__ import annotations
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Dict, List, Optional, Set
class ArenaState(Enum):
    CREATING = 'creating'
    ACTIVE = 'active'
    PAUSED = 'paused'
    CLOSING = 'closing'
    CLOSED = 'closed'
@dataclass
class ArenaEvent:
    id: str
    timestamp: datetime
    event_type: str
    agent_id: Optional[str] = None
    data: Dict[str, Any] = field(default_factory=dict)
    metadata: Dict[str, Any] = field(default_factory=dict)
@dataclass
class ArenaSession:
    id: str
    name: str
    description: Optional[str] = None
    created_at: datetime = field(default_factory=datetime.utcnow)
    state: ArenaState = ArenaState.CREATING
    agents: Set[str] = field(default_factory=set)
    memory_scope_ids: List[str] = field(default_factory=list)
    events: List[ArenaEvent] = field(default_factory=list)
    metadata: Dict[str, Any] = field(default_factory=dict)
    def add_agent(self, agent_id: str) -> None:
        self.agents.add(agent_id)
        self._add_event('agent_joined', agent_id=agent_id, data={'agent_id': agent_id})
    def remove_agent(self, agent_id: str) -> None:
        if agent_id in self.agents:
            self.agents.remove(agent_id)
            self._add_event('agent_left', agent_id=agent_id, data={'agent_id': agent_id})
    def add_memory_scope(self, memory_id: str) -> None:
        if memory_id not in self.memory_scope_ids:
            self.memory_scope_ids.append(memory_id)
            self._add_event('memory_scope_added', data={'memory_id': memory_id})
    def transition_state(self, new_state: ArenaState) -> None:
        old_state = self.state
        self.state = new_state
        self._add_event('state_transition', data={'old_state': old_state.value, 'new_state': new_state.value})
    def _add_event(self, event_type: str, agent_id: Optional[str]=None, data: Optional[Dict[str, Any]]=None) -> None:
        event = ArenaEvent(id=str(uuid.uuid4()), timestamp=datetime.utcnow(), event_type=event_type, agent_id=agent_id, data=data or {})
        self.events.append(event)
    def get_agent_events(self, agent_id: str, limit: int=50) -> List[ArenaEvent]:
        agent_events = [event for event in self.events if event.agent_id == agent_id]
        return agent_events[-limit:]
    def to_dict(self) -> Dict[str, Any]:
        return {'id': self.id, 'name': self.name, 'description': self.description, 'created_at': self.created_at.isoformat(), 'state': self.state.value, 'agents': list(self.agents), 'memory_scope_ids': self.memory_scope_ids, 'event_count': len(self.events), 'metadata': self.metadata}