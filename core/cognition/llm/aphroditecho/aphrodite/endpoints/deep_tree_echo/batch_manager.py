import asyncio
import logging
import time
from collections import deque
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Callable
import numpy as np
logger = logging.getLogger(__name__)
@dataclass
class BatchingMetrics:
    requests_processed: int = 0
    total_processing_time_ms: float = 0.0
    avg_batch_size: float = 0.0
    avg_processing_time_ms: float = 0.0
    server_load_samples: List[float] = field(default_factory=list)
    avg_server_load: float = 0.0
    batch_utilization: float = 0.0
    throughput_improvement: float = 0.0
    batch_wait_times: List[float] = field(default_factory=list)
    avg_batch_wait_time: float = 0.0
    last_updated: float = field(default_factory=time.time)
@dataclass
class BatchConfiguration:
    min_batch_size: int = 1
    max_batch_size: int = 32
    target_batch_size: int = 8
    low_load_threshold: float = 0.3
    high_load_threshold: float = 0.8
    load_adjustment_factor: float = 0.2
    max_batch_wait_ms: float = 50.0
    min_batch_wait_ms: float = 5.0
    enable_adaptive_sizing: bool = True
    performance_window_size: int = 100
    adaptation_rate: float = 0.1
    enable_circuit_breaker: bool = True
    failure_threshold: int = 5
    circuit_breaker_timeout: float = 30.0
class DynamicBatchManager:
    def __init__(self, config: Optional[BatchConfiguration]=None, load_tracker: Optional[Callable[[], float]]=None):
        self.config = config or BatchConfiguration()
        self.load_tracker = load_tracker
        self._pending_requests: List[Dict[str, Any]] = []
        self._batch_lock = asyncio.Lock()
        self._batch_event = asyncio.Event()
        self._processing = False
        self._metrics = BatchingMetrics()
        self._performance_history = deque(maxlen=self.config.performance_window_size)
        self._baseline_throughput: Optional[float] = None
        self._current_batch_size = self.config.target_batch_size
        self._load_samples = deque(maxlen=50)
        self._consecutive_failures = 0
        self._circuit_breaker_open = False
        self._circuit_breaker_opened_at = 0.0
        self._batch_processor_task: Optional[asyncio.Task] = None
        self._metrics_updater_task: Optional[asyncio.Task] = None
        logger.info(f'DynamicBatchManager initialized with target batch size: {self.config.target_batch_size}')
    async def start(self):
        logger.info('Starting dynamic batch manager')
        self._batch_processor_task = asyncio.create_task(self._batch_processor_loop())
        self._metrics_updater_task = asyncio.create_task(self._metrics_updater_loop())
    async def stop(self):
        logger.info('Stopping dynamic batch manager')
        if self._batch_processor_task:
            self._batch_processor_task.cancel()
            try:
                await self._batch_processor_task
            except asyncio.CancelledError:
                pass
        if self._metrics_updater_task:
            self._metrics_updater_task.cancel()
            try:
                await self._metrics_updater_task
            except asyncio.CancelledError:
                pass
        if self._pending_requests:
            logger.warning(f'Processing {len(self._pending_requests)} remaining requests')
            await self._process_pending_batch()
    async def submit_request(self, request_data: Dict[str, Any], priority: int=1, timeout: Optional[float]=None) -> str:
        if self._circuit_breaker_open:
            current_time = time.time()
            if current_time - self._circuit_breaker_opened_at < self.config.circuit_breaker_timeout:
                raise RuntimeError('Batch processing circuit breaker is open')
            else:
                self._circuit_breaker_open = False
                self._consecutive_failures = 0
                logger.info('Batch processing circuit breaker reset')
        request_id = f'batch_req_{int(time.time() * 1000000)}_{priority}'
        request_item = {'id': request_id, 'data': request_data, 'priority': priority, 'timeout': timeout, 'submitted_at': time.time(), 'future': asyncio.Future()}
        async with self._batch_lock:
            self._pending_requests.append(request_item)
            self._pending_requests.sort(key=lambda x: x['priority'])
        self._batch_event.set()
        logger.debug(f'Submitted request {request_id} for batch processing (priority {priority})')
        return await request_item['future']
    def _calculate_dynamic_batch_size(self) -> int:
        if not self.config.enable_adaptive_sizing:
            return self.config.target_batch_size
        current_load = self._get_current_load()
        base_size = self.config.target_batch_size
        if current_load < self.config.low_load_threshold:
            load_factor = 1.0 + self.config.load_adjustment_factor
        elif current_load > self.config.high_load_threshold:
            load_factor = 1.0 - self.config.load_adjustment_factor
        else:
            load_factor = 1.0
        performance_factor = self._calculate_performance_factor()
        adjusted_size = int(base_size * load_factor * performance_factor)
        final_size = max(self.config.min_batch_size, min(adjusted_size, self.config.max_batch_size))
        if hasattr(self, '_current_batch_size'):
            alpha = self.config.adaptation_rate
            final_size = int(alpha * final_size + (1 - alpha) * self._current_batch_size)
        self._current_batch_size = final_size
        logger.debug(f'Dynamic batch size calculated: {final_size} (load: {current_load:.3f}, load_factor: {load_factor:.3f}, perf_factor: {performance_factor:.3f})')
        return final_size
    def _get_current_load(self) -> float:
        if self.load_tracker:
            try:
                load = self.load_tracker()
                self._load_samples.append(load)
                return load
            except Exception as e:
                logger.warning(f'Failed to get server load: {e}')
        if self._load_samples:
            return sum(self._load_samples) / len(self._load_samples)
        return 0.5
    def _calculate_performance_factor(self) -> float:
        if len(self._performance_history) < 5:
            return 1.0
        recent_perf = list(self._performance_history)[-10:]
        older_perf = list(self._performance_history)[-20:-10]
        if not older_perf:
            return 1.0
        recent_avg = np.mean([p['throughput'] for p in recent_perf])
        older_avg = np.mean([p['throughput'] for p in older_perf])
        if older_avg == 0:
            return 1.0
        throughput_ratio = recent_avg / older_avg
        if throughput_ratio > 1.1:
            return 1.1
        elif throughput_ratio < 0.9:
            return 0.9
        else:
            return 1.0
    def _calculate_batch_wait_time(self, pending_count: int, target_size: int) -> float:
        if pending_count >= target_size:
            return 0.0
        fill_ratio = pending_count / target_size
        base_wait = self.config.max_batch_wait_ms
        adjusted_wait = base_wait * (1.0 - fill_ratio) ** 2
        final_wait = max(adjusted_wait, self.config.min_batch_wait_ms)
        return final_wait / 1000.0
    async def _batch_processor_loop(self):
        logger.info('Batch processor loop started')
        while True:
            try:
                try:
                    await asyncio.wait_for(self._batch_event.wait(), timeout=1.0)
                except asyncio.TimeoutError:
                    pass
                self._batch_event.clear()
                async with self._batch_lock:
                    pending_count = len(self._pending_requests)
                if pending_count == 0:
                    continue
                target_batch_size = self._calculate_dynamic_batch_size()
                wait_time = self._calculate_batch_wait_time(pending_count, target_batch_size)
                if wait_time > 0 and pending_count < target_batch_size:
                    await asyncio.sleep(wait_time)
                await self._process_pending_batch()
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f'Batch processor loop error: {e}', exc_info=True)
                self._consecutive_failures += 1
                if self.config.enable_circuit_breaker and self._consecutive_failures >= self.config.failure_threshold:
                    self._circuit_breaker_open = True
                    self._circuit_breaker_opened_at = time.time()
                    logger.error(f'Batch processing circuit breaker opened after {self._consecutive_failures} consecutive failures')
                await asyncio.sleep(1.0)
    async def _process_pending_batch(self):
        async with self._batch_lock:
            if not self._pending_requests:
                return
            batch_requests = self._pending_requests[:]
            self._pending_requests.clear()
        batch_size = len(batch_requests)
        batch_start_time = time.time()
        logger.info(f'Processing batch of {batch_size} requests')
        try:
            input_data = [req['data']['input_data'] for req in batch_requests]
            first_req = batch_requests[0]['data']
            membrane_depth = first_req.get('membrane_depth')
            esn_size = first_req.get('esn_size')
            if hasattr(self, '_dtesn_processor'):
                results = await self._dtesn_processor.process_batch(inputs=input_data, membrane_depth=membrane_depth, esn_size=esn_size, max_concurrent=min(batch_size, 8))
            else:
                results = [{'input_data': inp, 'processed_output': {'result': f'processed_{inp[:10]}'}, 'processing_time_ms': 10.0, 'batch_processed': True} for inp in input_data]
            for req, result in zip(batch_requests, results):
                if not req['future'].done():
                    req['future'].set_result(result)
            batch_time = (time.time() - batch_start_time) * 1000
            throughput = batch_size / (batch_time / 1000.0)
            self._update_performance_metrics(batch_size, batch_time, throughput)
            self._consecutive_failures = 0
            logger.info(f'Batch processing completed: {batch_size} requests in {batch_time:.2f}ms (throughput: {throughput:.1f} req/s)')
        except Exception as e:
            logger.error(f'Batch processing failed: {e}', exc_info=True)
            for req in batch_requests:
                if not req['future'].done():
                    req['future'].set_exception(e)
            self._consecutive_failures += 1
            raise
    def _update_performance_metrics(self, batch_size: int, processing_time_ms: float, throughput: float):
        self._metrics.requests_processed += batch_size
        self._metrics.total_processing_time_ms += processing_time_ms
        total_batches = len(self._performance_history) + 1
        self._metrics.avg_batch_size = (self._metrics.avg_batch_size * (total_batches - 1) + batch_size) / total_batches
        self._metrics.avg_processing_time_ms = (self._metrics.avg_processing_time_ms * (total_batches - 1) + processing_time_ms) / total_batches
        current_load = self._get_current_load()
        self._metrics.server_load_samples.append(current_load)
        if len(self._metrics.server_load_samples) > 100:
            self._metrics.server_load_samples = self._metrics.server_load_samples[-100:]
        self._metrics.avg_server_load = np.mean(self._metrics.server_load_samples)
        if self._baseline_throughput is None:
            self._baseline_throughput = throughput
            self._metrics.throughput_improvement = 0.0
        else:
            self._metrics.throughput_improvement = (throughput - self._baseline_throughput) / self._baseline_throughput * 100
        perf_record = {'timestamp': time.time(), 'batch_size': batch_size, 'processing_time_ms': processing_time_ms, 'throughput': throughput, 'server_load': current_load}
        self._performance_history.append(perf_record)
        self._metrics.last_updated = time.time()
    async def _metrics_updater_loop(self):
        while True:
            try:
                await asyncio.sleep(10.0)
                current_time = time.time()
                cutoff_time = current_time - 3600
                while self._performance_history and self._performance_history[0]['timestamp'] < cutoff_time:
                    self._performance_history.popleft()
                logger.debug(f'Batch metrics - Processed: {self._metrics.requests_processed}, Avg batch size: {self._metrics.avg_batch_size:.1f}, Throughput improvement: {self._metrics.throughput_improvement:.1f}%')
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f'Metrics updater error: {e}')
    def get_metrics(self) -> BatchingMetrics:
        return self._metrics
    def get_current_batch_size(self) -> int:
        return self._current_batch_size
    async def get_pending_count(self) -> int:
        async with self._batch_lock:
            return len(self._pending_requests)
    def set_dtesn_processor(self, processor):
        self._dtesn_processor = processor
        logger.info('DTESN processor configured for batch manager')