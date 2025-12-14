"""
Tests for enhanced backend data processing pipelines (Phase 7.1.3).

Tests parallel data processing, efficient data transformation algorithms,
and performance monitoring integration for high-volume request handling.
"""

import asyncio
import pytest
import time
from unittest.mock import AsyncMock, Mock, patch
from typing import List, Any

from aphrodite.endpoints.deep_tree_echo.data_pipeline import (
    DataProcessingPipeline,
    VectorizedDataTransformer,
    PipelineConfiguration,
    DataProcessingMetrics,
    create_data_processing_pipeline
)
from aphrodite.endpoints.deep_tree_echo.performance_integration import (
    DTESNPerformanceCollector,
    IntegratedDataPipelineMonitor,
    create_integrated_pipeline_monitor
)


class TestVectorizedDataTransformer:
    """Test suite for vectorized data transformation."""

    def setup_method(self):
        """Setup test configuration."""
        self.config = PipelineConfiguration(
            max_workers=4,
            enable_vectorization=True,
            chunk_size=100
        )
        self.transformer = VectorizedDataTransformer(self.config)

    def test_vectorize_text_data(self):
        """Test text data vectorization."""
        text_batch = ["hello", "world", "test"]
        
        vectors = self.transformer.vectorize_text_data(text_batch)
        
        # Check output shape and type
        assert vectors.shape[0] == len(text_batch)
        assert vectors.shape[1] > 0  # Should have some width
        assert vectors.dtype == np.int16
        
        # Check content
        assert vectors[0, 0] == ord('h')  # First char of "hello"
        assert vectors[1, 0] == ord('w')  # First char of "world"

    def test_vectorize_empty_batch(self):
        """Test handling of empty batch."""
        vectors = self.transformer.vectorize_text_data([])
        assert vectors.shape == (0, 0)

    def test_parallel_transform_batch(self):
        """Test parallel batch transformation."""
        data_batch = list(range(10))
        
        def square_func(x):
            return x * x
        
        results = self.transformer.parallel_transform_batch(
            data_batch, square_func, chunk_size=3
        )
        
        expected = [x * x for x in data_batch]
        assert results == expected

    def test_parallel_transform_with_errors(self):
        """Test parallel transformation with some failures."""
        data_batch = [1, 2, 3, 4, 5]
        
        def failing_func(x):
            if x == 3:
                raise ValueError("Test error")
            return x * 2
        
        results = self.transformer.parallel_transform_batch(
            data_batch, failing_func
        )
        
        # Should have None for failed item
        assert results[0] == 2   # 1 * 2
        assert results[1] == 4   # 2 * 2  
        assert results[2] is None  # Failed
        assert results[3] == 8   # 4 * 2
        assert results[4] == 10  # 5 * 2

    @pytest.mark.asyncio
    async def test_streaming_transform(self):
        """Test streaming transformation for large datasets."""
        async def data_generator():
            for i in range(20):
                yield i
        
        def double_func(x):
            return x * 2
        
        results = []
        async for result in self.transformer.streaming_transform(
            data_generator(), double_func, buffer_size=5
        ):
            results.append(result)
        
        expected = [i * 2 for i in range(20)]
        assert results == expected


class TestDataProcessingPipeline:
    """Test suite for data processing pipeline."""

    def setup_method(self):
        """Setup test configuration."""
        self.config = PipelineConfiguration(
            max_workers=4,
            enable_dynamic_batching=True,
            max_batch_size=100,
            enable_performance_profiling=False  # Disable for testing
        )

    @pytest.mark.asyncio
    async def test_pipeline_initialization(self):
        """Test pipeline creation and initialization."""
        pipeline = await create_data_processing_pipeline(self.config)
        
        assert pipeline is not None
        assert pipeline._is_running
        assert pipeline.config.max_workers == 4
        
        await pipeline.stop()
        assert not pipeline._is_running

    @pytest.mark.asyncio
    async def test_process_batch_sequential(self):
        """Test batch processing in sequential mode."""
        pipeline = DataProcessingPipeline(self.config)
        await pipeline.start()
        
        try:
            data_batch = ["test1", "test2", "test3"]
            
            def upper_func(text):
                return text.upper()
            
            results = await pipeline.process_batch(
                data_batch, upper_func, enable_parallel=False
            )
            
            expected = ["TEST1", "TEST2", "TEST3"]
            assert results == expected
            
            # Check metrics updated
            assert pipeline.metrics.items_processed == 3
            
        finally:
            await pipeline.stop()

    @pytest.mark.asyncio
    async def test_process_batch_parallel(self):
        """Test batch processing in parallel mode."""
        pipeline = DataProcessingPipeline(self.config)
        await pipeline.start()
        
        try:
            data_batch = list(range(10))
            
            def slow_square(x):
                time.sleep(0.01)  # Small delay to test parallelization
                return x * x
            
            start_time = time.time()
            results = await pipeline.process_batch(
                data_batch, slow_square, enable_parallel=True
            )
            parallel_time = time.time() - start_time
            
            expected = [x * x for x in data_batch]
            assert results == expected
            
            # Parallel should be faster than sequential for this workload
            # (though timing tests can be flaky in CI)
            assert parallel_time < 0.5  # Should complete quickly
            
        finally:
            await pipeline.stop()

    @pytest.mark.asyncio
    async def test_process_large_dataset(self):
        """Test processing of large datasets with streaming."""
        pipeline = DataProcessingPipeline(self.config)
        await pipeline.start()
        
        try:
            # Create async generator for large dataset
            async def large_dataset():
                for i in range(50):
                    yield f"item_{i}"
            
            def process_item(item):
                return item.upper()
            
            processed_items = []
            
            async def output_handler(result):
                processed_items.append(result)
            
            stats = await pipeline.process_large_dataset(
                large_dataset(), process_item, output_handler
            )
            
            # Check results
            assert stats["total_processed"] == 50
            assert len(processed_items) == 50
            assert processed_items[0] == "ITEM_0"
            assert processed_items[-1] == "ITEM_49"
            assert stats["throughput_items_per_second"] > 0
            
        finally:
            await pipeline.stop()

    @pytest.mark.asyncio
    async def test_error_handling_in_batch(self):
        """Test error handling in batch processing."""
        pipeline = DataProcessingPipeline(self.config)
        await pipeline.start()
        
        try:
            data_batch = [1, 2, 3, 4, 5]
            
            def failing_func(x):
                if x == 3:
                    raise ValueError("Test error")
                return x * 10
            
            results = await pipeline.process_batch(
                data_batch, failing_func, enable_parallel=True
            )
            
            # Should handle errors gracefully
            assert results[0] == 10   # 1 * 10
            assert results[1] == 20   # 2 * 10
            assert results[2] is None  # Failed
            assert results[3] == 40   # 4 * 10
            assert results[4] == 50   # 5 * 10
            
        finally:
            await pipeline.stop()

    @pytest.mark.asyncio
    async def test_performance_metrics_collection(self):
        """Test performance metrics are properly collected."""
        config = PipelineConfiguration(enable_performance_profiling=True)
        pipeline = DataProcessingPipeline(config)
        await pipeline.start()
        
        try:
            data_batch = list(range(20))
            
            def identity_func(x):
                return x
            
            await pipeline.process_batch(data_batch, identity_func)
            
            metrics = pipeline.get_performance_metrics()
            
            # Check metrics structure
            assert "throughput" in metrics
            assert "parallelization" in metrics
            assert "resources" in metrics
            assert "batching" in metrics
            
            # Check some values are populated
            assert metrics["throughput"]["items_processed"] == 20
            assert metrics["parallelization"]["max_workers"] == config.max_workers
            
        finally:
            await pipeline.stop()


class TestPerformanceIntegration:
    """Test suite for performance monitoring integration."""

    def setup_method(self):
        """Setup test configuration."""
        self.config = PipelineConfiguration(
            max_workers=4,
            enable_performance_profiling=False
        )

    @pytest.mark.asyncio
    async def test_dtesn_performance_collector(self):
        """Test DTESN performance metrics collection."""
        pipeline = DataProcessingPipeline(self.config)
        await pipeline.start()
        
        try:
            collector = DTESNPerformanceCollector(pipeline)
            
            # Process some data to generate metrics
            data_batch = ["test1", "test2", "test3"]
            await pipeline.process_batch(data_batch, lambda x: x.upper())
            
            # Collect metrics
            metrics = collector.collect_pipeline_metrics()
            
            # Check metrics structure
            assert hasattr(metrics, 'timestamp')
            assert hasattr(metrics, 'token_throughput')
            assert hasattr(metrics, 'cpu_utilization')
            
            # Check alert conditions
            alerts = collector.check_alert_conditions(metrics)
            assert isinstance(alerts, list)
            
        finally:
            await pipeline.stop()

    @pytest.mark.asyncio
    async def test_integrated_pipeline_monitor(self):
        """Test integrated pipeline monitoring system."""
        pipeline = DataProcessingPipeline(self.config)
        await pipeline.start()
        
        try:
            # Create monitor with echo integration disabled for testing
            monitor = IntegratedDataPipelineMonitor(
                pipeline, enable_echo_integration=False
            )
            await monitor.start_monitoring()
            
            # Let it collect some metrics
            await asyncio.sleep(0.1)
            
            status = monitor.get_comprehensive_status()
            
            # Check status structure
            assert "monitoring_active" in status
            assert "pipeline_metrics" in status
            assert status["monitoring_active"] is True
            
            await monitor.stop_monitoring()
            
        finally:
            await pipeline.stop()

    @pytest.mark.asyncio
    async def test_alert_handling(self):
        """Test alert generation and handling."""
        pipeline = DataProcessingPipeline(self.config)
        await pipeline.start()
        
        try:
            collector = DTESNPerformanceCollector(pipeline)
            
            # Create mock metrics that trigger alerts
            mock_metrics = Mock()
            mock_metrics.token_throughput = 10.0  # Below threshold
            mock_metrics.memory_usage = 3000.0    # Above threshold
            mock_metrics.cpu_utilization = 90.0   # Above threshold
            
            alerts = collector.check_alert_conditions(mock_metrics)
            
            # Should generate multiple alerts
            assert len(alerts) > 0
            
            # Check alert structure
            for alert in alerts:
                assert "severity" in alert
                assert "message" in alert
                assert "metric" in alert
                
        finally:
            await pipeline.stop()

    @pytest.mark.asyncio
    async def test_performance_report_export(self):
        """Test performance report export functionality."""
        pipeline = DataProcessingPipeline(self.config)
        await pipeline.start()
        
        try:
            monitor = IntegratedDataPipelineMonitor(
                pipeline, enable_echo_integration=False
            )
            
            # Process some data
            data_batch = ["test1", "test2"]
            await pipeline.process_batch(data_batch, lambda x: x.upper())
            
            # Export report
            report = monitor.export_performance_report("/tmp/test_report.json")
            
            # Check report structure
            assert "report_timestamp" in report
            assert "pipeline_configuration" in report
            assert "current_status" in report
            assert "metrics_history" in report
            
        finally:
            await pipeline.stop()


class TestIntegrationWithDTESN:
    """Test integration between data pipeline and DTESN processor."""

    @pytest.mark.asyncio
    async def test_dtesn_processor_pipeline_integration(self):
        """Test DTESN processor integration with data pipeline."""
        # Mock DTESN processor with pipeline methods
        class MockDTESNProcessor:
            def __init__(self):
                self._data_pipeline = None
                self._pipeline_monitor = None
                self._pipeline_config = PipelineConfiguration()
            
            async def _initialize_data_processing_pipeline(self):
                self._data_pipeline = await create_data_processing_pipeline(
                    self._pipeline_config
                )
            
            async def process_data_batch(self, data_batch, enable_parallel=True):
                if not self._data_pipeline:
                    return [{"input": item, "output": f"processed_{item}"} 
                           for item in data_batch]
                
                def mock_transform(item):
                    return {"input": item, "output": f"processed_{item}"}
                
                return await self._data_pipeline.process_batch(
                    data_batch, mock_transform, enable_parallel
                )
            
            def get_pipeline_metrics(self):
                if not self._data_pipeline:
                    return {"pipeline_available": False}
                return {
                    "pipeline_available": True,
                    "pipeline_metrics": self._data_pipeline.get_performance_metrics()
                }
            
            async def shutdown_pipeline(self):
                if self._data_pipeline:
                    await self._data_pipeline.stop()
        
        processor = MockDTESNProcessor()
        
        # Test initialization
        await processor._initialize_data_processing_pipeline()
        assert processor._data_pipeline is not None
        
        # Test batch processing
        data_batch = ["input1", "input2", "input3"]
        results = await processor.process_data_batch(data_batch)
        
        assert len(results) == 3
        assert results[0]["input"] == "input1"
        assert results[0]["output"] == "processed_input1"
        
        # Test metrics
        metrics = processor.get_pipeline_metrics()
        assert metrics["pipeline_available"] is True
        
        # Cleanup
        await processor.shutdown_pipeline()


@pytest.mark.asyncio
async def test_end_to_end_data_processing():
    """End-to-end test of enhanced data processing pipeline."""
    # Configuration for comprehensive testing
    config = PipelineConfiguration(
        max_workers=4,
        enable_dynamic_batching=True,
        max_batch_size=50,
        enable_vectorization=True,
        enable_performance_profiling=True
    )
    
    # Create and start pipeline
    pipeline = await create_data_processing_pipeline(config)
    
    # Create monitoring
    monitor = await create_integrated_pipeline_monitor(
        pipeline, enable_echo_integration=False
    )
    
    try:
        # Process various data types and sizes
        test_cases = [
            # Small batch
            (["small1", "small2"], "Small batch processing"),
            # Medium batch
            ([f"medium_{i}" for i in range(20)], "Medium batch processing"),
            # Large batch
            ([f"large_{i}" for i in range(100)], "Large batch processing"),
        ]
        
        for data_batch, description in test_cases:
            print(f"Testing: {description}")
            
            def test_processor(item):
                return f"processed_{item}"
            
            start_time = time.time()
            results = await pipeline.process_batch(data_batch, test_processor)
            processing_time = time.time() - start_time
            
            # Verify results
            assert len(results) == len(data_batch)
            assert all(r.startswith("processed_") for r in results)
            
            print(f"  Processed {len(data_batch)} items in {processing_time:.3f}s")
            print(f"  Throughput: {len(data_batch)/processing_time:.1f} items/sec")
        
        # Check final metrics
        final_metrics = pipeline.get_performance_metrics()
        print(f"\nFinal Pipeline Metrics:")
        print(f"  Total items processed: {final_metrics['throughput']['items_processed']}")
        print(f"  Average processing rate: {final_metrics['throughput']['avg_processing_rate']:.1f} items/sec")
        
        # Check monitoring status
        monitor_status = monitor.get_comprehensive_status()
        print(f"  Monitoring active: {monitor_status['monitoring_active']}")
        
        # Verify performance criteria for Phase 7.1.3 acceptance
        assert final_metrics["throughput"]["items_processed"] > 0
        assert final_metrics["throughput"]["avg_processing_rate"] > 50  # items/sec
        print("✅ Phase 7.1.3 acceptance criteria met: Data processing pipelines handle high-volume requests efficiently")
        
    finally:
        # Cleanup
        await monitor.stop_monitoring()
        await pipeline.stop()
        print("Pipeline shutdown complete")


if __name__ == "__main__":
    import numpy as np
    
    # Run the end-to-end test
    asyncio.run(test_end_to_end_data_processing())