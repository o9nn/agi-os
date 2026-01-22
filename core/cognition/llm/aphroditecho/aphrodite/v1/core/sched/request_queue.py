from __future__ import annotations
import heapq
from abc import ABC, abstractmethod
from collections import deque
from collections.abc import Iterable, Iterator
from enum import Enum
from aphrodite.v1.request import Request
class SchedulingPolicy(Enum):
    FCFS = 'fcfs'
    PRIORITY = 'priority'
class RequestQueue(ABC):
    @abstractmethod
    def add_request(self, request: Request) -> None:
        pass
    @abstractmethod
    def pop_request(self) -> Request:
        pass
    @abstractmethod
    def peek_request(self) -> Request:
        pass
    @abstractmethod
    def prepend_request(self, request: Request) -> None:
        pass
    @abstractmethod
    def prepend_requests(self, requests: RequestQueue) -> None:
        pass
    @abstractmethod
    def remove_request(self, request: Request) -> None:
        pass
    @abstractmethod
    def remove_requests(self, requests: Iterable[Request]) -> None:
        pass
    @abstractmethod
    def __bool__(self) -> bool:
        pass
    @abstractmethod
    def __len__(self) -> int:
        pass
    @abstractmethod
    def __iter__(self) -> Iterator[Request]:
        pass
    @abstractmethod
    def __reversed__(self) -> Iterator[Request]:
        pass
class FCFSRequestQueue(deque[Request], RequestQueue):
    def add_request(self, request: Request) -> None:
        self.append(request)
    def pop_request(self) -> Request:
        return self.popleft()
    def peek_request(self) -> Request:
        if not self:
            raise IndexError('peek from an empty queue')
        return self[0]
    def prepend_request(self, request: Request) -> None:
        self.appendleft(request)
    def prepend_requests(self, requests: RequestQueue) -> None:
        self.extendleft(reversed(requests))
    def remove_request(self, request: Request) -> None:
        self.remove(request)
    def remove_requests(self, requests: Iterable[Request]) -> None:
        requests_to_remove = set(requests)
        filtered_requests = [req for req in self if req not in requests_to_remove]
        self.clear()
        self.extend(filtered_requests)
    def __bool__(self) -> bool:
        return len(self) > 0
    def __len__(self) -> int:
        return super().__len__()
    def __iter__(self) -> Iterator[Request]:
        return super().__iter__()
    def __reversed__(self) -> Iterator[Request]:
        return super().__reversed__()
class PriorityRequestQueue(RequestQueue):
    def __init__(self) -> None:
        self._heap: list[tuple[int, float, Request]] = []
    def add_request(self, request: Request) -> None:
        heapq.heappush(self._heap, (request.priority, request.arrival_time, request))
    def pop_request(self) -> Request:
        if not self._heap:
            raise IndexError('pop from empty heap')
        _, _, request = heapq.heappop(self._heap)
        return request
    def peek_request(self) -> Request:
        if not self._heap:
            raise IndexError('peek from empty heap')
        _, _, request = self._heap[0]
        return request
    def prepend_request(self, request: Request) -> None:
        self.add_request(request)
    def prepend_requests(self, requests: RequestQueue) -> None:
        for request in requests:
            self.add_request(request)
    def remove_request(self, request: Request) -> None:
        self._heap = [(p, t, r) for p, t, r in self._heap if r != request]
        heapq.heapify(self._heap)
    def remove_requests(self, requests: Iterable[Request]) -> None:
        requests_to_remove = set(requests)
        self._heap = [(p, t, r) for p, t, r in self._heap if r not in requests_to_remove]
        heapq.heapify(self._heap)
    def __bool__(self) -> bool:
        return bool(self._heap)
    def __len__(self) -> int:
        return len(self._heap)
    def __iter__(self) -> Iterator[Request]:
        heap_copy = self._heap[:]
        while heap_copy:
            _, _, request = heapq.heappop(heap_copy)
            yield request
    def __reversed__(self) -> Iterator[Request]:
        return reversed(list(self))
def create_request_queue(policy: SchedulingPolicy) -> RequestQueue:
    if policy == SchedulingPolicy.PRIORITY:
        return PriorityRequestQueue()
    elif policy == SchedulingPolicy.FCFS:
        return FCFSRequestQueue()
    else:
        raise ValueError(f'Unknown scheduling policy: {policy}')