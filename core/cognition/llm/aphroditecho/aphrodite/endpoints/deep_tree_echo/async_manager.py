import asyncio
import logging
import time
import weakref
from contextlib import asynccontextmanager
from typing import Any, AsyncGenerator, Dict, List, Optional, Set, Union
from dataclasses import dataclass, field
logger = logging.getLogger(__name__)
@dataclass
class ConnectionPoolConfig:
    max_connections: int = 500
    min_connections: int = 50
    connection_timeout: float = 15.0
    idle_timeout: float = 180.0
    max_retries: int = 3
    retry_delay: float = 0.05
    enable_keepalive: bool = True
    keepalive_interval: float = 30.0
    max_concurrent_creates: int = 50
@dataclass
class ResourcePoolStats:
    active_connections: int = 0
    idle_connections: int = 0
    total_requests: int = 0
    failed_requests: int = 0
    avg_response_time: float = 0.0
    pool_utilization: float = 0.0
    last_updated: float = field(default_factory=time.time)
class AsyncConnectionPool:
    def __init__(self, config: Optional[ConnectionPoolConfig]=None):
        self.config = config or ConnectionPoolConfig()
        self._active_connections: Set[str] = set()
        self._idle_connections: asyncio.Queue = asyncio.Queue(maxsize=self.config.max_connections)
        self._connection_semaphore = asyncio.Semaphore(self.config.max_connections)
        self._create_semaphore = asyncio.Semaphore(self.config.max_concurrent_creates)
        self._stats = ResourcePoolStats()
        self._cleanup_task: Optional[asyncio.Task] = None
        self._keepalive_task: Optional[asyncio.Task] = None
        self._lock = asyncio.RLock()
        self._connection_health: Dict[str, float] = {}
        self._pending_creates = 0
    async def start(self):
        logger.info(f'Starting enhanced async connection pool with {self.config.max_connections} max connections')
        create_tasks = []
        batch_size = min(10, self.config.min_connections)
        for i in range(0, self.config.min_connections, batch_size):
            batch_end = min(i + batch_size, self.config.min_connections)
            batch_tasks = [self._create_connection_safe() for _ in range(i, batch_end)]
            batch_results = await asyncio.gather(*batch_tasks, return_exceptions=True)
            for result in batch_results:
                if isinstance(result, str):
                    await self._idle_connections.put((result, time.time()))
                elif isinstance(result, Exception):
                    logger.warning(f'Failed to create initial connection: {result}')
        self._cleanup_task = asyncio.create_task(self._cleanup_idle_connections())
        if self.config.enable_keepalive:
            self._keepalive_task = asyncio.create_task(self._keepalive_connections())
    async def stop(self):
        logger.info('Stopping enhanced async connection pool')
        for task in [self._cleanup_task, self._keepalive_task]:
            if task:
                task.cancel()
                try:
                    await task
                except asyncio.CancelledError:
                    pass
        cleanup_tasks = []
        while not self._idle_connections.empty():
            try:
                connection_id, _ = self._idle_connections.get_nowait()
                cleanup_tasks.append(self._close_connection(connection_id))
            except asyncio.QueueEmpty:
                break
        if cleanup_tasks:
            await asyncio.gather(*cleanup_tasks, return_exceptions=True)
    @asynccontextmanager
    async def get_connection(self) -> AsyncGenerator[str, None]:
        async with self._connection_semaphore:
            self._stats.total_requests += 1
            start_time = time.time()
            try:
                connection_id = await self._get_idle_connection()
                if connection_id is None:
                    connection_id = await self._create_connection()
                async with self._lock:
                    self._active_connections.add(connection_id)
                    self._stats.active_connections = len(self._active_connections)
                try:
                    yield connection_id
                finally:
                    await self._return_connection(connection_id)
                    response_time = time.time() - start_time
                    self._update_response_time(response_time)
            except Exception as e:
                self._stats.failed_requests += 1
                logger.error(f'Connection pool error: {e}')
                raise
    async def _get_idle_connection(self) -> Optional[str]:
        try:
            connection_id, created_time = await asyncio.wait_for(self._idle_connections.get(), timeout=0.1)
            if time.time() - created_time < self.config.idle_timeout:
                return connection_id
            else:
                await self._close_connection(connection_id)
                return None
        except asyncio.TimeoutError:
            return None
    async def _create_connection_safe(self) -> str:
        async with self._create_semaphore:
            self._pending_creates += 1
            try:
                return await self._create_connection()
            finally:
                self._pending_creates -= 1
    async def _create_connection(self) -> str:
        connection_id = f'dtesn_conn_{int(time.time() * 1000000)}'
        self._connection_health[connection_id] = time.time()
        logger.debug(f'Created new connection: {connection_id}')
        return connection_id
    async def _close_connection(self, connection_id: str):
        if connection_id in self._connection_health:
            del self._connection_health[connection_id]
        logger.debug(f'Closing connection: {connection_id}')
    async def _return_connection(self, connection_id: str):
        async with self._lock:
            if connection_id in self._active_connections:
                self._active_connections.remove(connection_id)
                self._stats.active_connections = len(self._active_connections)
        try:
            await asyncio.wait_for(self._idle_connections.put((connection_id, time.time())), timeout=0.1)
            async with self._lock:
                self._stats.idle_connections = self._idle_connections.qsize()
        except asyncio.TimeoutError:
            await self._close_connection(connection_id)
    async def _cleanup_idle_connections(self):
        while True:
            try:
                await asyncio.sleep(60)
                current_time = time.time()
                connections_to_remove = []
                temp_queue = asyncio.Queue()
                while not self._idle_connections.empty():
                    try:
                        connection_id, created_time = self._idle_connections.get_nowait()
                        if current_time - created_time > self.config.idle_timeout:
                            connections_to_remove.append(connection_id)
                        else:
                            await temp_queue.put((connection_id, created_time))
                    except asyncio.QueueEmpty:
                        break
                while not temp_queue.empty():
                    try:
                        item = temp_queue.get_nowait()
                        await self._idle_connections.put(item)
                    except asyncio.QueueEmpty:
                        break
                for connection_id in connections_to_remove:
                    await self._close_connection(connection_id)
                    logger.debug(f'Cleaned up idle connection: {connection_id}')
                async with self._lock:
                    self._stats.idle_connections = self._idle_connections.qsize()
                    self._stats.pool_utilization = len(self._active_connections) / self.config.max_connections
                    self._stats.last_updated = current_time
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f'Connection cleanup error: {e}')
    def _update_response_time(self, response_time: float):
        alpha = 0.1
        if self._stats.avg_response_time == 0:
            self._stats.avg_response_time = response_time
        else:
            self._stats.avg_response_time = alpha * response_time + (1 - alpha) * self._stats.avg_response_time
    async def _keepalive_connections(self):
        while True:
            try:
                await asyncio.sleep(self.config.keepalive_interval)
                current_time = time.time()
                healthy_connections = []
                stale_connections = []
                for conn_id, last_health in self._connection_health.items():
                    if current_time - last_health > self.config.keepalive_interval * 2:
                        stale_connections.append(conn_id)
                    else:
                        healthy_connections.append(conn_id)
                for conn_id in healthy_connections:
                    self._connection_health[conn_id] = current_time
                for conn_id in stale_connections:
                    if conn_id in self._connection_health:
                        del self._connection_health[conn_id]
                        logger.debug(f'Removed stale connection from health tracking: {conn_id}')
                logger.debug(f'Keepalive check: {len(healthy_connections)} healthy, {len(stale_connections)} stale')
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f'Keepalive error: {e}')
    def get_stats(self) -> ResourcePoolStats:
        return self._stats
class ConcurrencyManager:
    def __init__(self, max_concurrent_requests: int=500, max_requests_per_second: float=1000.0, burst_limit: int=100, adaptive_scaling: bool=True, scale_factor: float=1.2):
        self.max_concurrent_requests = max_concurrent_requests
        self.max_requests_per_second = max_requests_per_second
        self.burst_limit = burst_limit
        self.adaptive_scaling = adaptive_scaling
        self.scale_factor = scale_factor
        self._request_semaphore = asyncio.Semaphore(max_concurrent_requests)
        self._rate_limiter = asyncio.Semaphore(burst_limit)
        self._request_times: List[float] = []
        self._lock = asyncio.RLock()
        self._system_load = 0.0
        self._avg_response_time = 0.0
        self._success_rate = 1.0
        self._scale_history: List[float] = []
    @asynccontextmanager
    async def throttle_request(self) -> AsyncGenerator[None, None]:
        start_time = time.time()
        await self._apply_adaptive_rate_limit()
        semaphore = self._get_adaptive_semaphore()
        async with semaphore:
            try:
                yield
            finally:
                response_time = time.time() - start_time
                await self._record_performance_metrics(response_time, success=True)
                await self._cleanup_rate_limit()
    async def _apply_adaptive_rate_limit(self):
        if self.adaptive_scaling:
            load_factor = min(1.5, max(0.5, 1.0 - self._system_load))
            adaptive_rate = self.max_requests_per_second * load_factor
        else:
            adaptive_rate = self.max_requests_per_second
        await self._apply_rate_limit_with_rate(adaptive_rate)
    def _get_adaptive_semaphore(self) -> asyncio.Semaphore:
        if not self.adaptive_scaling:
            return self._request_semaphore
        if self._avg_response_time > 0:
            if self._avg_response_time < 0.1 and self._success_rate > 0.95:
                scale = min(self.scale_factor, 1.5)
            elif self._avg_response_time > 1.0 or self._success_rate < 0.9:
                scale = max(1.0 / self.scale_factor, 0.7)
            else:
                scale = 1.0
            scaled_capacity = int(self.max_concurrent_requests * scale)
            scaled_capacity = max(10, min(scaled_capacity, self.max_concurrent_requests * 2))
            if abs(scaled_capacity - self._request_semaphore._initial_value) > 10:
                current_available = self._request_semaphore._value
                self._request_semaphore = asyncio.Semaphore(scaled_capacity)
                new_available = int(current_available * scaled_capacity / self.max_concurrent_requests)
                for _ in range(scaled_capacity - new_available):
                    try:
                        self._request_semaphore.acquire_nowait()
                    except ValueError:
                        break
        return self._request_semaphore
    async def _record_performance_metrics(self, response_time: float, success: bool):
        async with self._lock:
            alpha = 0.1
            if self._avg_response_time == 0:
                self._avg_response_time = response_time
            else:
                self._avg_response_time = alpha * response_time + (1 - alpha) * self._avg_response_time
            self._success_rate = alpha * (1.0 if success else 0.0) + (1 - alpha) * self._success_rate
            current_load = (self.max_concurrent_requests - self._request_semaphore._value) / self.max_concurrent_requests
            self._system_load = alpha * current_load + (1 - alpha) * self._system_load
    async def _apply_rate_limit_with_rate(self, rate_limit: float):
        current_time = time.time()
        async with self._lock:
            self._request_times = [t for t in self._request_times if current_time - t < 1.0]
            if len(self._request_times) >= rate_limit:
                oldest_time = min(self._request_times)
                delay = 1.0 - (current_time - oldest_time)
                if delay > 0:
                    await asyncio.sleep(delay)
            self._request_times.append(current_time)
    async def _apply_rate_limit(self):
        current_time = time.time()
        async with self._lock:
            self._request_times = [t for t in self._request_times if current_time - t < 1.0]
            if len(self._request_times) >= self.max_requests_per_second:
                oldest_time = min(self._request_times)
                delay = 1.0 - (current_time - oldest_time)
                if delay > 0:
                    await asyncio.sleep(delay)
            self._request_times.append(current_time)
    async def _cleanup_rate_limit(self):
        current_time = time.time()
        async with self._lock:
            self._request_times = [timestamp for timestamp in self._request_times if current_time - timestamp < 60.0]
            if len(self._request_times) > 0:
                logger.debug(f'Rate limit cleanup: {len(self._request_times)} active timestamps remaining')
    def get_current_load(self) -> Dict[str, Any]:
        current_time = time.time()
        recent_requests = len([t for t in self._request_times if current_time - t < 1.0])
        effective_capacity = getattr(self._request_semaphore, '_initial_value', self.max_concurrent_requests)
        return {'concurrent_requests': effective_capacity - self._request_semaphore._value, 'recent_requests_per_second': recent_requests, 'rate_limit_utilization': recent_requests / self.max_requests_per_second, 'concurrency_utilization': (effective_capacity - self._request_semaphore._value) / effective_capacity, 'available_slots': self._request_semaphore._value, 'burst_capacity_remaining': self._rate_limiter._value, 'adaptive_scaling_enabled': self.adaptive_scaling, 'system_load': self._system_load, 'avg_response_time': self._avg_response_time, 'success_rate': self._success_rate, 'effective_capacity': effective_capacity, 'base_capacity': self.max_concurrent_requests}
class AsyncRequestQueue:
    def __init__(self, max_queue_size: int=10000, priority_levels: int=5, circuit_breaker_threshold: int=10, circuit_breaker_timeout: float=30.0, adaptive_timeout: bool=True, batch_processing: bool=True, batch_size: int=10):
        self.max_queue_size = max_queue_size
        self.priority_levels = priority_levels
        self.circuit_breaker_threshold = circuit_breaker_threshold
        self.circuit_breaker_timeout = circuit_breaker_timeout
        self.adaptive_timeout = adaptive_timeout
        self.batch_processing = batch_processing
        self.batch_size = batch_size
        self._priority_queues = [asyncio.Queue(maxsize=max_queue_size // priority_levels) for _ in range(priority_levels)]
        self._circuit_breaker_failures = 0
        self._circuit_breaker_last_failure = 0.0
        self._circuit_breaker_open = False
        self._response_times = []
        self._success_rate = 1.0
        self._lock = asyncio.RLock()
        self._batch_queues = [[] for _ in range(priority_levels)]
        self._batch_timers = [None for _ in range(priority_levels)]
        self._batch_locks = [asyncio.Lock() for _ in range(priority_levels)]
        logger.info(f'AsyncRequestQueue initialized with {priority_levels} priority levels')
    async def enqueue_request(self, request_data: Any, priority: int=1, timeout: Optional[float]=None) -> str:
        if self._circuit_breaker_open:
            current_time = time.time()
            if current_time - self._circuit_breaker_last_failure < self.circuit_breaker_timeout:
                raise RuntimeError('Circuit breaker is open - service temporarily unavailable')
            else:
                self._circuit_breaker_open = False
                self._circuit_breaker_failures = 0
                logger.info('Circuit breaker reset - service available')
        priority = max(0, min(priority, self.priority_levels - 1))
        request_id = f'req_{int(time.time() * 1000000)}_{priority}'
        if timeout is None and self.adaptive_timeout:
            timeout = self._calculate_adaptive_timeout()
        request_item = {'id': request_id, 'data': request_data, 'priority': priority, 'timeout': timeout, 'enqueued_at': time.time(), 'retries': 0}
        try:
            self._priority_queues[priority].put_nowait(request_item)
            logger.debug(f'Enqueued request {request_id} with priority {priority}')
            return request_id
        except asyncio.QueueFull:
            logger.warning(f'Queue full for priority {priority}, rejecting request {request_id}')
            raise RuntimeError(f'Request queue full for priority level {priority}')
    async def dequeue_request(self) -> Optional[Dict[str, Any]]:
        for priority in range(self.priority_levels):
            try:
                request_item = self._priority_queues[priority].get_nowait()
                logger.debug(f"Dequeued request {request_item['id']} with priority {priority}")
                return request_item
            except asyncio.QueueEmpty:
                continue
        return None
    async def record_request_result(self, request_id: str, success: bool, response_time: float, error: Optional[str]=None):
        async with self._lock:
            self._response_times.append(response_time)
            if len(self._response_times) > 100:
                self._response_times = self._response_times[-100:]
            if success:
                self._success_rate = 0.95 * self._success_rate + 0.05 * 1.0
                if self._circuit_breaker_failures > 0:
                    self._circuit_breaker_failures = max(0, self._circuit_breaker_failures - 1)
            else:
                self._success_rate = 0.95 * self._success_rate + 0.05 * 0.0
                self._circuit_breaker_failures += 1
                self._circuit_breaker_last_failure = time.time()
                if self._circuit_breaker_failures >= self.circuit_breaker_threshold:
                    self._circuit_breaker_open = True
                    logger.warning(f'Circuit breaker opened due to {self._circuit_breaker_failures} failures')
                logger.warning(f'Request {request_id} failed: {error}')
    def _calculate_adaptive_timeout(self) -> float:
        if not self._response_times:
            return 30.0
        sorted_times = sorted(self._response_times)
        p95_index = int(0.95 * len(sorted_times))
        p95_time = sorted_times[p95_index] if p95_index < len(sorted_times) else sorted_times[-1]
        buffer_multiplier = 2.0 if self._success_rate < 0.9 else 1.5
        adaptive_timeout = p95_time * buffer_multiplier
        return max(5.0, min(adaptive_timeout, 120.0))
    def get_queue_stats(self) -> Dict[str, Any]:
        total_queued = sum((q.qsize() for q in self._priority_queues))
        queue_sizes = [q.qsize() for q in self._priority_queues]
        avg_response_time = sum(self._response_times) / len(self._response_times) if self._response_times else 0.0
        return {'total_queued_requests': total_queued, 'priority_queue_sizes': queue_sizes, 'queue_utilization': total_queued / self.max_queue_size, 'circuit_breaker_open': self._circuit_breaker_open, 'circuit_breaker_failures': self._circuit_breaker_failures, 'success_rate': self._success_rate, 'avg_response_time': avg_response_time, 'adaptive_timeout': self._calculate_adaptive_timeout(), 'batch_processing_enabled': self.batch_processing, 'batch_sizes': [len(batch) for batch in self._batch_queues] if self.batch_processing else []}
    async def enqueue_batch_request(self, request_data: Any, priority: int=1, timeout: Optional[float]=None) -> str:
        if not self.batch_processing:
            return await self.enqueue_request(request_data, priority, timeout)
        priority = max(0, min(priority, self.priority_levels - 1))
        request_id = f'batch_req_{int(time.time() * 1000000)}_{priority}'
        if timeout is None and self.adaptive_timeout:
            timeout = self._calculate_adaptive_timeout()
        request_item = {'id': request_id, 'data': request_data, 'priority': priority, 'timeout': timeout, 'enqueued_at': time.time(), 'batch': True}
        async with self._batch_locks[priority]:
            self._batch_queues[priority].append(request_item)
            if len(self._batch_queues[priority]) >= self.batch_size:
                await self._flush_batch(priority)
            elif self._batch_timers[priority] is None:
                self._batch_timers[priority] = asyncio.create_task(self._batch_timeout(priority, 0.1))
        logger.debug(f'Enqueued batch request {request_id} with priority {priority}')
        return request_id
    async def _flush_batch(self, priority: int):
        if not self._batch_queues[priority]:
            return
        batch_item = {'id': f'batch_{int(time.time() * 1000000)}_{priority}', 'batch_data': self._batch_queues[priority].copy(), 'priority': priority, 'batch_size': len(self._batch_queues[priority]), 'enqueued_at': time.time()}
        self._batch_queues[priority].clear()
        if self._batch_timers[priority]:
            self._batch_timers[priority].cancel()
            self._batch_timers[priority] = None
        try:
            self._priority_queues[priority].put_nowait(batch_item)
            logger.debug(f"Flushed batch with {batch_item['batch_size']} requests for priority {priority}")
        except asyncio.QueueFull:
            logger.warning(f'Failed to flush batch for priority {priority}: queue full')
            self._batch_queues[priority].extend(batch_item['batch_data'])
    async def _batch_timeout(self, priority: int, timeout: float):
        try:
            await asyncio.sleep(timeout)
            async with self._batch_locks[priority]:
                if self._batch_queues[priority]:
                    await self._flush_batch(priority)
        except asyncio.CancelledError:
            pass
    async def dequeue_batch_request(self) -> Optional[Union[Dict[str, Any], List[Dict[str, Any]]]]:
        for priority in range(self.priority_levels):
            try:
                request_item = self._priority_queues[priority].get_nowait()
                if 'batch_data' in request_item:
                    logger.debug(f"Dequeued batch {request_item['id']} with {request_item['batch_size']} requests")
                    return request_item['batch_data']
                else:
                    logger.debug(f"Dequeued request {request_item['id']} with priority {priority}")
                    return request_item
            except asyncio.QueueEmpty:
                continue
        return None