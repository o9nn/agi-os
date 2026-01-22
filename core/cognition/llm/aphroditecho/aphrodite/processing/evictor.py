import enum
import heapq
from abc import ABC, abstractmethod
from typing import Dict, List, Tuple
class EvictionPolicy(enum.Enum):
    LRU = enum.auto()
class Evictor(ABC):
    @abstractmethod
    def __init__(self):
        pass
    @abstractmethod
    def __contains__(self, block_id: int) -> bool:
        pass
    @abstractmethod
    def evict(self) -> Tuple[int, int]:
        pass
    @abstractmethod
    def add(self, block_id: int, content_hash: int, num_hashed_tokens: int, last_accessed: float):
        pass
    @abstractmethod
    def update(self, block_id: int, last_accessed: float):
        pass
    @abstractmethod
    def remove(self, block_id: int):
        pass
    @property
    @abstractmethod
    def num_blocks(self) -> int:
        pass
class BlockMetaData:
    def __init__(self, content_hash: int, num_hashed_tokens: int, last_accessed: float):
        self.content_hash = content_hash
        self.num_hashed_tokens = num_hashed_tokens
        self.last_accessed = last_accessed
class LRUEvictor(Evictor):
    CLEANUP_THRESHOLD = 50
    def __init__(self):
        self.free_table: Dict[int, BlockMetaData] = {}
        self.priority_queue = []
    def __contains__(self, block_id: int) -> bool:
        return block_id in self.free_table
    def evict(self) -> Tuple[int, int]:
        if len(self.free_table) == 0:
            raise ValueError('No usable cache memory left')
        while self.priority_queue:
            last_accessed, _, block_id, content_hash = heapq.heappop(self.priority_queue)
            if block_id in self.free_table and self.free_table[block_id].last_accessed == last_accessed:
                self.free_table.pop(block_id)
                return (block_id, content_hash)
        raise ValueError('No usable cache memory left')
    def add(self, block_id: int, content_hash: int, num_hashed_tokens: int, last_accessed: float):
        self.free_table[block_id] = BlockMetaData(content_hash, num_hashed_tokens, last_accessed)
        heapq.heappush(self.priority_queue, (last_accessed, -num_hashed_tokens, block_id, content_hash))
        self._cleanup_if_necessary()
    def update(self, block_id: int, last_accessed: float):
        self.free_table[block_id].last_accessed = last_accessed
    def _cleanup_if_necessary(self):
        if len(self.priority_queue) > LRUEvictor.CLEANUP_THRESHOLD * len(self.free_table):
            self._cleanup()
    def _cleanup(self):
        new_priority_queue: List[Tuple[float, int, int, int]] = []
        for block_id, block in self.free_table.items():
            new_priority_queue.append((block.last_accessed, -block.num_hashed_tokens, block_id, block.content_hash))
        heapq.heapify(new_priority_queue)
        self.priority_queue = new_priority_queue
    def remove(self, block_id: int):
        if block_id not in self.free_table:
            raise ValueError("Attempting to remove block that's not in the evictor")
        self.free_table.pop(block_id)
    @property
    def num_blocks(self) -> int:
        return len(self.free_table)
def make_evictor(eviction_policy: EvictionPolicy) -> Evictor:
    if eviction_policy == EvictionPolicy.LRU:
        return LRUEvictor()
    else:
        raise ValueError(f'Unknown cache eviction policy: {eviction_policy}')