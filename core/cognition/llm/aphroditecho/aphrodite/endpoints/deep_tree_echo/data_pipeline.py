import asyncio
import logging
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass, field
from typing import Any, AsyncGenerator, Dict, List, Optional
import numpy as np
try:
    import psutil
    PSUTIL_AVAILABLE = True
except ImportError:
    PSUTIL_AVAILABLE = False
from aphrodite.endpoints.deep_tree_echo.async_manager import AsyncConnectionPool
from aphrodite.endpoints.deep_tree_echo.batch_manager import BatchConfiguration, DynamicBatchManager
logger = logging.getLogger(__name__)
@dataclass
class DataProcessingMetrics:
    items_processed: int = 0
    total_processing_time_ms: float = 0.0
    avg_processing_rate: float = 0.0
    peak_processing_rate: float = 0.0
    active_workers: int = 0
    max_workers: int = 0
    worker_utilization: float = 0.0
    parallel_efficiency: float = 0.0
    memory_usage_mb: float = 0.0
    peak_memory_mb: float = 0.0
    cpu_utilization: float = 0.0
    vectorization_ratio: float = 0.0
    transformation_overhead_ms: float = 0.0
    queue_depth: int = 0
    avg_batch_size: float = 0.0
    batch_processing_efficiency: float = 0.0
    last_updated: float = field(default_factory=time.time)
@dataclass
class PipelineConfiguration:
    max_workers: int = 16
    worker_pool_type: str = 'thread'
    enable_gpu_acceleration: bool = False
    enable_dynamic_batching: bool = True
    max_batch_size: int = 1000
    batch_timeout_ms: float = 100.0
    enable_vectorization: bool = True
    chunk_size: int = 10000
    enable_compression: bool = True
    max_memory_usage_mb: int = 2048
    enable_memory_monitoring: bool = True
    memory_cleanup_threshold: float = 0.8
    enable_caching: bool = True
    cache_size_mb: int = 512
    enable_prefetching: bool = True
    metrics_collection_interval: float = 1.0
    enable_performance_profiling: bool = True
class VectorizedDataTransformer:
    def __init__(self, config: PipelineConfiguration):
        self.config = config
        self._transformation_cache = {}
    def vectorize_text_data(self, text_batch: List[str]) -> np.ndarray:
        if not text_batch:
            return np.array([]).reshape(0, 0)
        max_len = min(max((len(text) for text in text_batch)), 512)
        batch_size = len(text_batch)
        vectors = np.zeros((batch_size, max_len), dtype=np.int16)
        for i, text in enumerate(text_batch):
            text_len = min(len(text), max_len)
            vectors[i, :text_len] = [ord(c) for c in text[:text_len]]
        return vectors
    def parallel_transform_batch(self, data_batch: List[Any], transform_func: callable, chunk_size: Optional[int]=None) -> List[Any]:
        if not data_batch:
            return []
        chunk_size = chunk_size or self.config.chunk_size
        chunks = [data_batch[i:i + chunk_size] for i in range(0, len(data_batch), chunk_size)]
        results = []
        with ThreadPoolExecutor(max_workers=self.config.max_workers) as executor:
            future_to_chunk = {executor.submit(self._process_chunk, chunk, transform_func): chunk for chunk in chunks}
            for future in as_completed(future_to_chunk):
                try:
                    chunk_result = future.result()
                    results.extend(chunk_result)
                except Exception as e:
                    logger.error(f'Chunk processing failed: {e}')
                    failed_chunk = future_to_chunk[future]
                    results.extend([None] * len(failed_chunk))
        return results
    def _process_chunk(self, chunk: List[Any], transform_func: callable) -> List[Any]:
        return [transform_func(item) for item in chunk]
    def streaming_transform(self, data_stream: AsyncGenerator[Any, None], transform_func: callable, buffer_size: int=1000) -> AsyncGenerator[Any, None]:
        buffer = []
        async for item in data_stream:
            buffer.append(item)
            if len(buffer) >= buffer_size:
                transformed_batch = self.parallel_transform_batch(buffer, transform_func)
                for result in transformed_batch:
                    yield result
                buffer = []
        if buffer:
            transformed_batch = self.parallel_transform_batch(buffer, transform_func)
            for result in transformed_batch:
                yield result
class DataProcessingPipeline:
    def __init__(self, config: Optional[PipelineConfiguration]=None, batch_manager: Optional[DynamicBatchManager]=None, connection_pool: Optional[AsyncConnectionPool]=None):
        self.config = config or PipelineConfiguration()
        self.batch_manager = batch_manager
        self.connection_pool = connection_pool
        self.transformer = VectorizedDataTransformer(self.config)
        self.metrics = DataProcessingMetrics(max_workers=self.config.max_workers)
        self._processing_queue = asyncio.Queue(maxsize=10000)
        self._worker_pool = None
        self._monitoring_task = None
        self._is_running = False
        self._processing_times = []
        self._batch_sizes = []
        self._memory_samples = []
        logger.info(f'Initialized data processing pipeline with {self.config.max_workers} workers')
    async def start(self):
        if self._is_running:
            return
        self._is_running = True
        if self.config.worker_pool_type == 'thread':
            self._worker_pool = ThreadPoolExecutor(max_workers=self.config.max_workers, thread_name_prefix='DTESN-DataProcessor')
        if self.config.enable_performance_profiling:
            self._monitoring_task = asyncio.create_task(self._monitor_performance())
        logger.info('Data processing pipeline started')
    async def stop(self):
        if not self._is_running:
            return
        self._is_running = False
        if self._monitoring_task:
            self._monitoring_task.cancel()
            try:
                await self._monitoring_task
            except asyncio.CancelledError:
                pass
        if self._worker_pool:
            self._worker_pool.shutdown(wait=True)
        logger.info('Data processing pipeline stopped')
    async def process_batch(self, data_batch: List[Any], processor_func: callable, enable_parallel: bool=True) -> List[Any]:
        start_time = time.time()
        batch_size = len(data_batch)
        try:
            self.metrics.active_workers = min(batch_size, self.config.max_workers)
            self.metrics.queue_depth = self._processing_queue.qsize()
            if enable_parallel and batch_size > 1:
                results = await self._process_batch_parallel(data_batch, processor_func)
            else:
                results = await self._process_batch_sequential(data_batch, processor_func)
            processing_time = (time.time() - start_time) * 1000
            self._update_processing_metrics(batch_size, processing_time)
            return results
        except Exception as e:
            logger.error(f'Batch processing failed: {e}')
            raise
    async def _process_batch_parallel(self, data_batch: List[Any], processor_func: callable) -> List[Any]:
        if not self._worker_pool:
            raise RuntimeError('Worker pool not initialized')
        loop = asyncio.get_event_loop()
        tasks = [loop.run_in_executor(self._worker_pool, processor_func, item) for item in data_batch]
        results = await asyncio.gather(*tasks, return_exceptions=True)
        final_results = []
        for i, result in enumerate(results):
            if isinstance(result, Exception):
                logger.warning(f'Processing failed for item {i}: {result}')
                final_results.append(None)
            else:
                final_results.append(result)
        return final_results
    async def _process_batch_sequential(self, data_batch: List[Any], processor_func: callable) -> List[Any]:
        results = []
        for item in data_batch:
            try:
                result = processor_func(item)
                results.append(result)
            except Exception as e:
                logger.warning(f'Sequential processing failed for item: {e}')
                results.append(None)
        return results
    async def process_large_dataset(self, dataset_generator: AsyncGenerator[Any, None], processor_func: callable, output_handler: callable) -> Dict[str, Any]:
        start_time = time.time()
        total_processed = 0
        batch_config = BatchConfiguration(max_batch_size=self.config.max_batch_size, max_batch_wait_ms=self.config.batch_timeout_ms)
        if not self.batch_manager:
            self.batch_manager = DynamicBatchManager(batch_config)
        current_batch = []
        async for item in dataset_generator:
            current_batch.append(item)
            if len(current_batch) >= self.config.max_batch_size:
                batch_results = await self.process_batch(current_batch, processor_func)
                for result in batch_results:
                    if result is not None:
                        await output_handler(result)
                        total_processed += 1
                current_batch = []
        if current_batch:
            batch_results = await self.process_batch(current_batch, processor_func)
            for result in batch_results:
                if result is not None:
                    await output_handler(result)
                    total_processed += 1
        total_time = time.time() - start_time
        return {'total_processed': total_processed, 'processing_time_seconds': total_time, 'throughput_items_per_second': total_processed / total_time if total_time > 0 else 0, 'final_metrics': self.get_performance_metrics()}
    def _update_processing_metrics(self, batch_size: int, processing_time_ms: float):
        self.metrics.items_processed += batch_size
        self.metrics.total_processing_time_ms += processing_time_ms
        if processing_time_ms > 0:
            current_rate = batch_size / processing_time_ms * 1000
            self.metrics.avg_processing_rate = (self.metrics.avg_processing_rate + current_rate) / 2 if self.metrics.avg_processing_rate > 0 else current_rate
            self.metrics.peak_processing_rate = max(self.metrics.peak_processing_rate, current_rate)
        self._batch_sizes.append(batch_size)
        self._processing_times.append(processing_time_ms)
        if len(self._batch_sizes) > 0:
            self.metrics.avg_batch_size = np.mean(self._batch_sizes[-100:])
        self.metrics.worker_utilization = self.metrics.active_workers / self.metrics.max_workers if self.metrics.max_workers > 0 else 0
        self.metrics.last_updated = time.time()
    async def _monitor_performance(self):
        while self._is_running:
            try:
                if PSUTIL_AVAILABLE:
                    self.metrics.cpu_utilization = psutil.cpu_percent()
                    memory_info = psutil.virtual_memory()
                    self.metrics.memory_usage_mb = memory_info.used / (1024 * 1024)
                    self.metrics.peak_memory_mb = max(self.metrics.peak_memory_mb, self.metrics.memory_usage_mb)
                if self.config.enable_memory_monitoring and self.metrics.memory_usage_mb > self.config.max_memory_usage_mb:
                    logger.warning(f'Memory usage ({self.metrics.memory_usage_mb:.1f}MB) exceeds limit ({self.config.max_memory_usage_mb}MB)')
                await asyncio.sleep(self.config.metrics_collection_interval)
            except Exception as e:
                logger.error(f'Performance monitoring error: {e}')
                await asyncio.sleep(5.0)
    def get_performance_metrics(self) -> Dict[str, Any]:
        return {'throughput': {'items_processed': self.metrics.items_processed, 'avg_processing_rate': self.metrics.avg_processing_rate, 'peak_processing_rate': self.metrics.peak_processing_rate}, 'parallelization': {'active_workers': self.metrics.active_workers, 'max_workers': self.metrics.max_workers, 'worker_utilization': self.metrics.worker_utilization}, 'resources': {'memory_usage_mb': self.metrics.memory_usage_mb, 'peak_memory_mb': self.metrics.peak_memory_mb, 'cpu_utilization': self.metrics.cpu_utilization}, 'batching': {'queue_depth': self.metrics.queue_depth, 'avg_batch_size': self.metrics.avg_batch_size}, 'last_updated': self.metrics.last_updated}
async def create_data_processing_pipeline(config: Optional[PipelineConfiguration]=None) -> DataProcessingPipeline:
    pipeline = DataProcessingPipeline(config)
    await pipeline.start()
    return pipeline