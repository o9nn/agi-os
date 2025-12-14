"""
Enhanced Backend Data Processing Pipelines for Deep Tree Echo System Network.

Implements Phase 7.1.3 requirements:
- Parallel data processing for large datasets 
- Efficient data transformation algorithms
- Performance monitoring integration

This module provides high-volume, server-side data processing capabilities
with vectorized operations, streaming algorithms, and comprehensive monitoring.
"""

import asyncio
import logging
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass, field
from typing import Any, AsyncGenerator, Dict, List, Optional

import numpy as np

# Performance monitoring imports
try:
    import psutil
    PSUTIL_AVAILABLE = True
except ImportError:
    PSUTIL_AVAILABLE = False

from aphrodite.endpoints.deep_tree_echo.async_manager import (
    AsyncConnectionPool)
from aphrodite.endpoints.deep_tree_echo.batch_manager import (
    BatchConfiguration, DynamicBatchManager)

logger = logging.getLogger(__name__)


@dataclass
class DataProcessingMetrics:
    """Metrics for data processing pipeline performance monitoring."""
    
    # Throughput metrics
    items_processed: int = 0
    total_processing_time_ms: float = 0.0
    avg_processing_rate: float = 0.0  # items/second
    peak_processing_rate: float = 0.0
    
    # Parallelization metrics  
    active_workers: int = 0
    max_workers: int = 0
    worker_utilization: float = 0.0
    parallel_efficiency: float = 0.0  # actual speedup / theoretical speedup
    
    # Memory and resource metrics
    memory_usage_mb: float = 0.0
    peak_memory_mb: float = 0.0
    cpu_utilization: float = 0.0
    
    # Data transformation metrics
    vectorization_ratio: float = 0.0  # % of operations vectorized
    transformation_overhead_ms: float = 0.0
    
    # Queue and batching metrics
    queue_depth: int = 0
    avg_batch_size: float = 0.0
    batch_processing_efficiency: float = 0.0
    
    last_updated: float = field(default_factory=time.time)


@dataclass  
class PipelineConfiguration:
    """Configuration for enhanced data processing pipeline."""
    
    # Parallel processing settings
    max_workers: int = 16  # CPU-bound processing workers
    worker_pool_type: str = "thread"  # "thread" or "process"
    enable_gpu_acceleration: bool = False
    
    # Batch processing settings
    enable_dynamic_batching: bool = True
    max_batch_size: int = 1000
    batch_timeout_ms: float = 100.0
    
    # Data transformation settings
    enable_vectorization: bool = True
    chunk_size: int = 10000  # For streaming large datasets
    enable_compression: bool = True
    
    # Memory management
    max_memory_usage_mb: int = 2048
    enable_memory_monitoring: bool = True
    memory_cleanup_threshold: float = 0.8
    
    # Performance optimization
    enable_caching: bool = True
    cache_size_mb: int = 512
    enable_prefetching: bool = True
    
    # Monitoring settings
    metrics_collection_interval: float = 1.0
    enable_performance_profiling: bool = True


class VectorizedDataTransformer:
    """
    Vectorized data transformation operations for high-performance processing.
    
    Implements efficient algorithms using NumPy vectorization for large dataset
    processing with minimal overhead.
    """
    
    def __init__(self, config: PipelineConfiguration):
        """Initialize vectorized transformer."""
        self.config = config
        self._transformation_cache = {}
        
    def vectorize_text_data(self, text_batch: List[str]) -> np.ndarray:
        """
        Convert batch of text data to vectorized representation.
        
        Uses efficient character-level encoding with padding for uniform shapes.
        """
        if not text_batch:
            return np.array([]).reshape(0, 0)
            
        # Determine maximum length for batch
        # Cap at 512 chars for memory efficiency
        max_len = min(max(len(text) for text in text_batch), 512)
        
        # Vectorized character encoding
        batch_size = len(text_batch)
        vectors = np.zeros((batch_size, max_len), dtype=np.int16)
        
        for i, text in enumerate(text_batch):
            text_len = min(len(text), max_len)
            vectors[i, :text_len] = [ord(c) for c in text[:text_len]]
            
        return vectors
    
    def parallel_transform_batch(
        self, 
        data_batch: List[Any],
        transform_func: callable,
        chunk_size: Optional[int] = None
    ) -> List[Any]:
        """
        Apply transformation function to batch data with parallel processing.
        
        Splits large batches into chunks for optimal parallel processing.
        """
        if not data_batch:
            return []
            
        chunk_size = chunk_size or self.config.chunk_size
        
        # Split into chunks for parallel processing
        chunks = [
            data_batch[i:i + chunk_size] 
            for i in range(0, len(data_batch), chunk_size)
        ]
        
        results = []
        with ThreadPoolExecutor(
            max_workers=self.config.max_workers
        ) as executor:
            future_to_chunk = {
                executor.submit(self._process_chunk, chunk, transform_func): 
                chunk
                for chunk in chunks
            }
            
            for future in as_completed(future_to_chunk):
                try:
                    chunk_result = future.result()
                    results.extend(chunk_result)
                except Exception as e:
                    logger.error(f"Chunk processing failed: {e}")
                    # Add fallback results for failed chunk
                    failed_chunk = future_to_chunk[future]
                    results.extend([None] * len(failed_chunk))
                    
        return results
    
    def _process_chunk(
        self, chunk: List[Any], transform_func: callable
    ) -> List[Any]:
        """Process a single data chunk with transformation function."""
        return [transform_func(item) for item in chunk]
    
    def streaming_transform(
        self, 
        data_stream: AsyncGenerator[Any, None],
        transform_func: callable,
        buffer_size: int = 1000
    ) -> AsyncGenerator[Any, None]:
        """
        Stream processing with buffered transformation for large datasets.
        
        Processes data in buffers to balance memory usage and throughput.
        """
        buffer = []
        
        async for item in data_stream:
            buffer.append(item)
            
            if len(buffer) >= buffer_size:
                # Process current buffer
                transformed_batch = self.parallel_transform_batch(
                    buffer, transform_func
                )
                
                for result in transformed_batch:
                    yield result
                    
                buffer = []
        
        # Process remaining items
        if buffer:
            transformed_batch = self.parallel_transform_batch(
                buffer, transform_func
            )
            for result in transformed_batch:
                yield result


class DataProcessingPipeline:
    """
    Enhanced backend data processing pipeline for high-volume DTESN operations.
    
    Implements parallel processing, efficient transformations, and comprehensive
    monitoring for server-side data processing at scale.
    """
    
    def __init__(
        self,
        config: Optional[PipelineConfiguration] = None,
        batch_manager: Optional[DynamicBatchManager] = None,
        connection_pool: Optional[AsyncConnectionPool] = None
    ):
        """Initialize enhanced data processing pipeline."""
        self.config = config or PipelineConfiguration()
        self.batch_manager = batch_manager
        self.connection_pool = connection_pool
        
        # Initialize components
        self.transformer = VectorizedDataTransformer(self.config)
        self.metrics = DataProcessingMetrics(max_workers=self.config.max_workers)
        
        # Processing state
        self._processing_queue = asyncio.Queue(maxsize=10000)
        self._worker_pool = None
        self._monitoring_task = None
        self._is_running = False
        
        # Performance tracking
        self._processing_times = []
        self._batch_sizes = []
        self._memory_samples = []
        
        logger.info(
            f"Initialized data processing pipeline with "
            f"{self.config.max_workers} workers"
        )
    
    async def start(self):
        """Start the data processing pipeline."""
        if self._is_running:
            return
            
        self._is_running = True
        
        # Initialize worker pool
        if self.config.worker_pool_type == "thread":
            self._worker_pool = ThreadPoolExecutor(
                max_workers=self.config.max_workers,
                thread_name_prefix="DTESN-DataProcessor"
            )
        
        # Start monitoring
        if self.config.enable_performance_profiling:
            self._monitoring_task = asyncio.create_task(self._monitor_performance())
        
        logger.info("Data processing pipeline started")
    
    async def stop(self):
        """Stop the data processing pipeline."""
        if not self._is_running:
            return
            
        self._is_running = False
        
        # Stop monitoring
        if self._monitoring_task:
            self._monitoring_task.cancel()
            try:
                await self._monitoring_task
            except asyncio.CancelledError:
                pass
        
        # Shutdown worker pool
        if self._worker_pool:
            self._worker_pool.shutdown(wait=True)
            
        logger.info("Data processing pipeline stopped")
    
    async def process_batch(
        self,
        data_batch: List[Any],
        processor_func: callable,
        enable_parallel: bool = True
    ) -> List[Any]:
        """
        Process batch of data with parallel execution and monitoring.
        
        Args:
            data_batch: List of data items to process
            processor_func: Function to apply to each item
            enable_parallel: Whether to use parallel processing
        
        Returns:
            List of processed results
        """
        start_time = time.time()
        batch_size = len(data_batch)
        
        try:
            # Update metrics
            self.metrics.active_workers = min(batch_size, self.config.max_workers)
            self.metrics.queue_depth = self._processing_queue.qsize()
            
            # Process batch
            if enable_parallel and batch_size > 1:
                results = await self._process_batch_parallel(data_batch, processor_func)
            else:
                results = await self._process_batch_sequential(data_batch, processor_func)
            
            # Update performance metrics
            processing_time = (time.time() - start_time) * 1000
            self._update_processing_metrics(batch_size, processing_time)
            
            return results
            
        except Exception as e:
            logger.error(f"Batch processing failed: {e}")
            raise
    
    async def _process_batch_parallel(
        self, 
        data_batch: List[Any], 
        processor_func: callable
    ) -> List[Any]:
        """Process batch using parallel workers."""
        if not self._worker_pool:
            raise RuntimeError("Worker pool not initialized")
        
        # Submit tasks to worker pool
        loop = asyncio.get_event_loop()
        tasks = [
            loop.run_in_executor(self._worker_pool, processor_func, item)
            for item in data_batch
        ]
        
        # Wait for all tasks to complete
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        # Handle exceptions
        final_results = []
        for i, result in enumerate(results):
            if isinstance(result, Exception):
                logger.warning(f"Processing failed for item {i}: {result}")
                final_results.append(None)
            else:
                final_results.append(result)
        
        return final_results
    
    async def _process_batch_sequential(
        self, 
        data_batch: List[Any], 
        processor_func: callable
    ) -> List[Any]:
        """Process batch sequentially (fallback)."""
        results = []
        for item in data_batch:
            try:
                result = processor_func(item)
                results.append(result)
            except Exception as e:
                logger.warning(f"Sequential processing failed for item: {e}")
                results.append(None)
        
        return results
    
    async def process_large_dataset(
        self,
        dataset_generator: AsyncGenerator[Any, None],
        processor_func: callable,
        output_handler: callable
    ) -> Dict[str, Any]:
        """
        Process large dataset with streaming and batch optimization.
        
        Args:
            dataset_generator: Async generator yielding data items
            processor_func: Processing function for individual items
            output_handler: Function to handle processed results
        
        Returns:
            Processing statistics and metrics
        """
        start_time = time.time()
        total_processed = 0
        
        # Configure batching
        batch_config = BatchConfiguration(
            max_batch_size=self.config.max_batch_size,
            max_batch_wait_ms=self.config.batch_timeout_ms
        )
        
        if not self.batch_manager:
            self.batch_manager = DynamicBatchManager(batch_config)
        
        current_batch = []
        
        async for item in dataset_generator:
            current_batch.append(item)
            
            # Process when batch is full or timeout reached
            if len(current_batch) >= self.config.max_batch_size:
                batch_results = await self.process_batch(
                    current_batch, processor_func
                )
                
                # Handle results
                for result in batch_results:
                    if result is not None:
                        await output_handler(result)
                        total_processed += 1
                
                current_batch = []
        
        # Process remaining items
        if current_batch:
            batch_results = await self.process_batch(current_batch, processor_func)
            for result in batch_results:
                if result is not None:
                    await output_handler(result)
                    total_processed += 1
        
        # Return processing statistics
        total_time = time.time() - start_time
        return {
            "total_processed": total_processed,
            "processing_time_seconds": total_time,
            "throughput_items_per_second": total_processed / total_time if total_time > 0 else 0,
            "final_metrics": self.get_performance_metrics()
        }
    
    def _update_processing_metrics(self, batch_size: int, processing_time_ms: float):
        """Update performance metrics with latest processing data."""
        self.metrics.items_processed += batch_size
        self.metrics.total_processing_time_ms += processing_time_ms
        
        # Calculate rates
        if processing_time_ms > 0:
            current_rate = (batch_size / processing_time_ms) * 1000  # items/second
            self.metrics.avg_processing_rate = (
                (self.metrics.avg_processing_rate + current_rate) / 2
                if self.metrics.avg_processing_rate > 0
                else current_rate
            )
            self.metrics.peak_processing_rate = max(
                self.metrics.peak_processing_rate, current_rate
            )
        
        # Track batch metrics
        self._batch_sizes.append(batch_size)
        self._processing_times.append(processing_time_ms)
        
        # Calculate efficiency metrics
        if len(self._batch_sizes) > 0:
            self.metrics.avg_batch_size = np.mean(self._batch_sizes[-100:])  # Last 100 batches
            
        # Update worker utilization
        self.metrics.worker_utilization = (
            self.metrics.active_workers / self.metrics.max_workers
            if self.metrics.max_workers > 0 else 0
        )
        
        self.metrics.last_updated = time.time()
    
    async def _monitor_performance(self):
        """Background task to monitor system performance."""
        while self._is_running:
            try:
                # Collect system metrics
                if PSUTIL_AVAILABLE:
                    self.metrics.cpu_utilization = psutil.cpu_percent()
                    memory_info = psutil.virtual_memory()
                    self.metrics.memory_usage_mb = memory_info.used / (1024 * 1024)
                    self.metrics.peak_memory_mb = max(
                        self.metrics.peak_memory_mb, 
                        self.metrics.memory_usage_mb
                    )
                
                # Check memory limits
                if (self.config.enable_memory_monitoring and 
                    self.metrics.memory_usage_mb > self.config.max_memory_usage_mb):
                    logger.warning(
                        f"Memory usage ({self.metrics.memory_usage_mb:.1f}MB) "
                        f"exceeds limit ({self.config.max_memory_usage_mb}MB)"
                    )
                
                await asyncio.sleep(self.config.metrics_collection_interval)
                
            except Exception as e:
                logger.error(f"Performance monitoring error: {e}")
                await asyncio.sleep(5.0)  # Longer sleep on error
    
    def get_performance_metrics(self) -> Dict[str, Any]:
        """Get current performance metrics."""
        return {
            "throughput": {
                "items_processed": self.metrics.items_processed,
                "avg_processing_rate": self.metrics.avg_processing_rate,
                "peak_processing_rate": self.metrics.peak_processing_rate,
            },
            "parallelization": {
                "active_workers": self.metrics.active_workers,
                "max_workers": self.metrics.max_workers,
                "worker_utilization": self.metrics.worker_utilization,
            },
            "resources": {
                "memory_usage_mb": self.metrics.memory_usage_mb,
                "peak_memory_mb": self.metrics.peak_memory_mb,
                "cpu_utilization": self.metrics.cpu_utilization,
            },
            "batching": {
                "queue_depth": self.metrics.queue_depth,
                "avg_batch_size": self.metrics.avg_batch_size,
            },
            "last_updated": self.metrics.last_updated
        }


async def create_data_processing_pipeline(
    config: Optional[PipelineConfiguration] = None
) -> DataProcessingPipeline:
    """
    Factory function to create and initialize a data processing pipeline.
    
    Args:
        config: Optional pipeline configuration
    
    Returns:
        Initialized and started DataProcessingPipeline
    """
    pipeline = DataProcessingPipeline(config)
    await pipeline.start()
    return pipeline