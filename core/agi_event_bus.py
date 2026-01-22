import time
import logging
import threading
import heapq
from typing import Dict, List, Any, Optional, Callable, Set
from dataclasses import dataclass, field
from enum import IntEnum
from queue import PriorityQueue, Empty
from collections import defaultdict
import uuid
import json
import weakref
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger('AGI_EventBus')
class EventPriority(IntEnum):
    CRITICAL = 0
    ATTENTION = 1
    HIGH = 2
    NORMAL = 3
    LOW = 4
class EventCategory(IntEnum):
    SYSTEM = 0
    ATTENTION = 1
    INFERENCE = 2
    LEARNING = 3
    MEMORY = 4
    PERCEPTION = 5
    ACTION = 6
    COMMUNICATION = 7
    TIMER = 8
    USER = 9
@dataclass(order=True)
class CognitiveEvent:
    priority: int = field(compare=True)
    timestamp: float = field(compare=True)
    event_id: str = field(compare=False, default_factory=lambda: str(uuid.uuid4())[:8])
    event_type: str = field(compare=False, default='')
    category: EventCategory = field(compare=False, default=EventCategory.SYSTEM)
    source: str = field(compare=False, default='unknown')
    data: Dict[str, Any] = field(compare=False, default_factory=dict)
    sti: float = field(compare=False, default=0.0)
    lti: float = field(compare=False, default=0.0)
    propagate: bool = field(compare=False, default=True)
    handled: bool = field(compare=False, default=False)
    def to_dict(self) -> Dict[str, Any]:
        return {'event_id': self.event_id, 'event_type': self.event_type, 'category': self.category.name, 'priority': self.priority, 'source': self.source, 'timestamp': self.timestamp, 'data': self.data, 'sti': self.sti, 'lti': self.lti}
@dataclass
class EventHandler:
    handler_id: str
    callback: Callable[[CognitiveEvent], Optional[bool]]
    event_types: Set[str]
    categories: Set[EventCategory]
    priority: int
    source_filter: Optional[str] = None
    async_handler: bool = False
    def matches(self, event: CognitiveEvent) -> bool:
        if self.event_types and event.event_type not in self.event_types:
            return False
        if self.categories and event.category not in self.categories:
            return False
        if self.source_filter and event.source != self.source_filter:
            return False
        return True
class EventBusMetrics:
    def __init__(self):
        self.events_emitted = 0
        self.events_handled = 0
        self.events_dropped = 0
        self.handlers_invoked = 0
        self.handler_errors = 0
        self.category_counts = defaultdict(int)
        self.type_counts = defaultdict(int)
        self.avg_dispatch_time = 0.0
        self._dispatch_times: List[float] = []
    def record_emit(self, event: CognitiveEvent):
        self.events_emitted += 1
        self.category_counts[event.category.name] += 1
        self.type_counts[event.event_type] += 1
    def record_dispatch(self, dispatch_time: float, handlers_called: int):
        self._dispatch_times.append(dispatch_time)
        if len(self._dispatch_times) > 1000:
            self._dispatch_times = self._dispatch_times[-1000:]
        self.avg_dispatch_time = sum(self._dispatch_times) / len(self._dispatch_times)
        self.handlers_invoked += handlers_called
    def record_handled(self):
        self.events_handled += 1
    def record_dropped(self):
        self.events_dropped += 1
    def record_error(self):
        self.handler_errors += 1
    def get_statistics(self) -> Dict[str, Any]:
        return {'events_emitted': self.events_emitted, 'events_handled': self.events_handled, 'events_dropped': self.events_dropped, 'handlers_invoked': self.handlers_invoked, 'handler_errors': self.handler_errors, 'avg_dispatch_time_ms': self.avg_dispatch_time * 1000, 'category_distribution': dict(self.category_counts), 'top_event_types': dict(sorted(self.type_counts.items(), key=lambda x: x[1], reverse=True)[:10])}
class AGIEventBus:
    def __init__(self, max_queue_size: int=10000, dispatch_workers: int=2, enable_history: bool=True, history_size: int=1000):
        self.max_queue_size = max_queue_size
        self.dispatch_workers = dispatch_workers
        self.enable_history = enable_history
        self.history_size = history_size
        self.event_queue: PriorityQueue = PriorityQueue(maxsize=max_queue_size)
        self.handlers: Dict[str, EventHandler] = {}
        self._handlers_lock = threading.RLock()
        self.event_history: List[CognitiveEvent] = []
        self._history_lock = threading.Lock()
        self.running = False
        self.workers: List[threading.Thread] = []
        self.metrics = EventBusMetrics()
        self.isrs: Dict[str, Callable[[CognitiveEvent], None]] = {}
        logger.info('AGI Event Bus initialized')
    def subscribe(self, callback: Callable[[CognitiveEvent], Optional[bool]], event_types: Optional[Set[str]]=None, categories: Optional[Set[EventCategory]]=None, priority: int=100, source_filter: Optional[str]=None, async_handler: bool=False) -> str:
        handler_id = str(uuid.uuid4())[:8]
        handler = EventHandler(handler_id=handler_id, callback=callback, event_types=event_types or set(), categories=categories or set(), priority=priority, source_filter=source_filter, async_handler=async_handler)
        with self._handlers_lock:
            self.handlers[handler_id] = handler
        logger.debug(f'Handler {handler_id} subscribed')
        return handler_id
    def subscribe_isr(self, event_type: str, callback: Callable[[CognitiveEvent], None]) -> str:
        isr_id = f'isr_{event_type}_{str(uuid.uuid4())[:4]}'
        self.isrs[isr_id] = callback
        logger.debug(f'ISR {isr_id} registered for {event_type}')
        return isr_id
    def unsubscribe(self, handler_id: str) -> bool:
        with self._handlers_lock:
            if handler_id in self.handlers:
                del self.handlers[handler_id]
                logger.debug(f'Handler {handler_id} unsubscribed')
                return True
            if handler_id in self.isrs:
                del self.isrs[handler_id]
                return True
        return False
    def emit(self, event_type: str, data: Dict[str, Any]=None, category: EventCategory=EventCategory.SYSTEM, priority: EventPriority=EventPriority.NORMAL, source: str='unknown', sti: float=0.0, lti: float=0.0) -> str:
        event = CognitiveEvent(priority=priority.value, timestamp=time.time(), event_type=event_type, category=category, source=source, data=data or {}, sti=sti, lti=lti)
        self.metrics.record_emit(event)
        self._invoke_isrs(event)
        try:
            self.event_queue.put_nowait(event)
        except:
            self.metrics.record_dropped()
            logger.warning(f'Event dropped (queue full): {event_type}')
            return event.event_id
        return event.event_id
    def emit_attention_signal(self, signal_type: str, target_handle: str, old_sti: float, new_sti: float, source: str='attention_bank') -> str:
        return self.emit(event_type=f'attention.{signal_type}', data={'target_handle': target_handle, 'old_sti': old_sti, 'new_sti': new_sti, 'delta_sti': new_sti - old_sti}, category=EventCategory.ATTENTION, priority=EventPriority.ATTENTION, source=source, sti=new_sti)
    def emit_system_event(self, event_type: str, data: Dict[str, Any]=None, critical: bool=False, source: str='system') -> str:
        return self.emit(event_type=f'system.{event_type}', data=data, category=EventCategory.SYSTEM, priority=EventPriority.CRITICAL if critical else EventPriority.NORMAL, source=source)
    def _invoke_isrs(self, event: CognitiveEvent):
        for isr_id, isr in list(self.isrs.items()):
            if event.event_type in isr_id or 'all' in isr_id:
                try:
                    isr(event)
                except Exception as e:
                    logger.error(f'ISR {isr_id} error: {e}')
    def _dispatch_worker(self, worker_id: int):
        logger.debug(f'Dispatch worker {worker_id} started')
        while self.running:
            try:
                event = self.event_queue.get(timeout=1.0)
            except Empty:
                continue
            self._dispatch_event(event)
            self.event_queue.task_done()
    def _dispatch_event(self, event: CognitiveEvent):
        start_time = time.time()
        handlers_called = 0
        with self._handlers_lock:
            matching = [h for h in self.handlers.values() if h.matches(event)]
            matching.sort(key=lambda h: h.priority)
        for handler in matching:
            if not event.propagate:
                break
            try:
                if handler.async_handler:
                    threading.Thread(target=handler.callback, args=(event,), daemon=True).start()
                else:
                    result = handler.callback(event)
                    if result is True:
                        event.propagate = False
                handlers_called += 1
            except Exception as e:
                self.metrics.record_error()
                logger.error(f'Handler {handler.handler_id} error: {e}')
        event.handled = handlers_called > 0
        if event.handled:
            self.metrics.record_handled()
        dispatch_time = time.time() - start_time
        self.metrics.record_dispatch(dispatch_time, handlers_called)
        if self.enable_history:
            with self._history_lock:
                self.event_history.append(event)
                if len(self.event_history) > self.history_size:
                    self.event_history = self.event_history[-self.history_size:]
    def start(self):
        if self.running:
            return
        self.running = True
        for i in range(self.dispatch_workers):
            worker = threading.Thread(target=self._dispatch_worker, args=(i,), daemon=True)
            worker.start()
            self.workers.append(worker)
        logger.info(f'AGI Event Bus started with {self.dispatch_workers} dispatch workers')
    def stop(self, drain: bool=True, timeout: float=5.0):
        self.running = False
        if drain:
            try:
                self.event_queue.join()
            except:
                pass
        for worker in self.workers:
            worker.join(timeout=timeout)
        self.workers.clear()
        logger.info('AGI Event Bus stopped')
    def get_history(self, event_type: Optional[str]=None, category: Optional[EventCategory]=None, limit: int=100) -> List[Dict[str, Any]]:
        with self._history_lock:
            filtered = self.event_history.copy()
        if event_type:
            filtered = [e for e in filtered if e.event_type == event_type]
        if category:
            filtered = [e for e in filtered if e.category == category]
        return [e.to_dict() for e in filtered[-limit:]]
    def get_metrics(self) -> Dict[str, Any]:
        return self.metrics.get_statistics()
    def get_status(self) -> Dict[str, Any]:
        return {'running': self.running, 'queue_size': self.event_queue.qsize(), 'registered_handlers': len(self.handlers), 'registered_isrs': len(self.isrs), 'history_size': len(self.event_history)}
class EventTypes:
    BOOT_START = 'system.boot.start'
    BOOT_COMPLETE = 'system.boot.complete'
    SHUTDOWN = 'system.shutdown'
    ERROR = 'system.error'
    WARNING = 'system.warning'
    STI_CHANGED = 'attention.sti_changed'
    LTI_CHANGED = 'attention.lti_changed'
    FOCUS_SHIFTED = 'attention.focus_shifted'
    INFERENCE_START = 'inference.start'
    INFERENCE_COMPLETE = 'inference.complete'
    INFERENCE_TIMEOUT = 'inference.timeout'
    LEARNING_EPOCH = 'learning.epoch'
    PATTERN_FOUND = 'learning.pattern_found'
    ATOM_ADDED = 'memory.atom_added'
    ATOM_REMOVED = 'memory.atom_removed'
    ATOM_MODIFIED = 'memory.atom_modified'
    TIMER_TICK = 'timer.tick'
    TIMER_EXPIRED = 'timer.expired'
_event_bus_instance: Optional[AGIEventBus] = None
def get_event_bus() -> AGIEventBus:
    global _event_bus_instance
    if _event_bus_instance is None:
        _event_bus_instance = AGIEventBus()
    return _event_bus_instance
if __name__ == '__main__':
    event_bus = get_event_bus()
    event_bus.start()
    received_events = []
    def sample_handler(event: CognitiveEvent) -> bool:
        received_events.append(event)
        print(f'  Handled: {event.event_type} from {event.source}')
        return False
    def attention_handler(event: CognitiveEvent) -> bool:
        print(f"  [ATTENTION] {event.event_type}: STI delta = {event.data.get('delta_sti', 0):.2f}")
        return False
    event_bus.subscribe(sample_handler, priority=100)
    event_bus.subscribe(attention_handler, categories={EventCategory.ATTENTION}, priority=50)
    def critical_isr(event: CognitiveEvent):
        print(f'  [ISR] Critical event: {event.event_type}')
    event_bus.subscribe_isr('system.error', critical_isr)
    print('\n=== Emitting Events ===')
    event_bus.emit_system_event('boot.start', {'phase': 'init'})
    event_bus.emit(event_type='inference.start', category=EventCategory.INFERENCE, source='pln_engine', data={'query': 'test'})
    event_bus.emit_attention_signal('sti_changed', target_handle='atom_123', old_sti=10.0, new_sti=50.0)
    event_bus.emit_system_event('error', {'message': 'Test error'}, critical=True)
    event_bus.emit_system_event('boot.complete')
    time.sleep(1)
    print('\n=== Event Bus Metrics ===')
    print(json.dumps(event_bus.get_metrics(), indent=2))
    print('\n=== Event Bus Status ===')
    print(json.dumps(event_bus.get_status(), indent=2))
    print('\n=== Event History (last 5) ===')
    for e in event_bus.get_history(limit=5):
        print(f"  {e['event_type']} @ {e['timestamp']:.3f}")
    event_bus.stop()