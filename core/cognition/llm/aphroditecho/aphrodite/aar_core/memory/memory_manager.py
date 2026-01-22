from __future__ import annotations
import uuid
from datetime import timedelta
from typing import Any, Dict, List, Optional
from .memory_types import MemoryRecord, MemoryType
class MemoryManager:
    def __init__(self):
        self._memories: Dict[str, MemoryRecord] = {}
        self._working_buffer: List[MemoryRecord] = []
        self._max_working_size = 1000
    def add_memory(self, content: str, memory_type: MemoryType, metadata: Optional[Dict[str, Any]]=None, ttl: Optional[timedelta]=None, tags: Optional[List[str]]=None, source_agent: Optional[str]=None, arena_id: Optional[str]=None, vector_embedding: Optional[List[float]]=None) -> str:
        memory_id = str(uuid.uuid4())
        record = MemoryRecord(id=memory_id, type=memory_type, content=content, metadata=metadata or {}, ttl=ttl, tags=tags or [], source_agent=source_agent, arena_id=arena_id, vector_embedding=vector_embedding)
        self._memories[memory_id] = record
        if memory_type == MemoryType.WORKING:
            self._working_buffer.append(record)
            if len(self._working_buffer) > self._max_working_size:
                self._working_buffer.pop(0)
        return memory_id
    def get_memory(self, memory_id: str) -> Optional[MemoryRecord]:
        record = self._memories.get(memory_id)
        if record and record.is_expired():
            del self._memories[memory_id]
            return None
        return record
    def query_memory(self, query: str, memory_types: Optional[List[MemoryType]]=None, tags: Optional[List[str]]=None, arena_id: Optional[str]=None, limit: int=10, include_expired: bool=False) -> List[MemoryRecord]:
        results = []
        for record in self._memories.values():
            if not include_expired and record.is_expired():
                continue
            if memory_types and record.type not in memory_types:
                continue
            if tags and (not any((tag in record.tags for tag in tags))):
                continue
            if arena_id and record.arena_id != arena_id:
                continue
            if query.lower() in record.content.lower():
                results.append(record)
            if len(results) >= limit:
                break
        results.sort(key=lambda x: x.created_at, reverse=True)
        return results
    def get_working_memory(self, limit: int=100) -> List[MemoryRecord]:
        return self._working_buffer[-limit:]
    def cleanup_expired(self) -> int:
        expired_ids = [memory_id for memory_id, record in self._memories.items() if record.is_expired()]
        for memory_id in expired_ids:
            del self._memories[memory_id]
        return len(expired_ids)
    def get_memory_stats(self) -> Dict[str, Any]:
        total_count = len(self._memories)
        type_counts = {}
        for record in self._memories.values():
            type_name = record.type.value
            type_counts[type_name] = type_counts.get(type_name, 0) + 1
        return {'total_memories': total_count, 'type_counts': type_counts, 'working_buffer_size': len(self._working_buffer), 'max_working_size': self._max_working_size}