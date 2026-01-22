import asyncio
import pytest
import time
from unittest.mock import AsyncMock, Mock, patch
from typing import List, Any
from aphrodite.endpoints.deep_tree_echo.data_pipeline import DataProcessingPipeline, VectorizedDataTransformer, PipelineConfiguration, DataProcessingMetrics, create_data_processing_pipeline
from aphrodite.endpoints.deep_tree_echo.performance_integration import DTESNPerformanceCollector, IntegratedDataPipelineMonitor, create_integrated_pipeline_monitor
class TestVectorizedDataTransformer:
    def setup_method(self):
        self.config = PipelineConfiguration(max_workers=4, enable_vectorization=True, chunk_size=100)
        self.transformer = VectorizedDataTransformer(self.config)
    def test_vectorize_text_data(self):
        text_batch = ['hello', 'world', 'test']
        vectors = self.transformer.vectorize_text_data(text_batch)
        assert vectors.shape[0] == len(text_batch)
        assert vectors.shape[1] > 0
        assert vectors.dtype == np.int16
        assert vectors[0, 0] == ord('h')
        assert vectors[1, 0] == ord('w')
    def test_vectorize_empty_batch(self):
        vectors = self.transformer.vectorize_text_data([])
        assert vectors.shape == (0, 0)
    def test_parallel_transform_batch(self):
        data_batch = list(range(10))
        def square_func(x):
            return x * x
        results = self.transformer.parallel_transform_batch(data_batch, square_func, chunk_size=3)
        expected = [x * x for x in data_batch]
        assert results == expected
    def test_parallel_transform_with_errors(self):
        data_batch = [1, 2, 3, 4, 5]
        def failing_func(x):
            if x == 3:
                raise ValueError('Test error')
            return x * 2
        results = self.transformer.parallel_transform_batch(data_batch, failing_func)
        assert results[0] == 2
        assert results[1] == 4
        assert results[2] is None
        assert results[3] == 8
        assert results[4] == 10
    @pytest.mark.asyncio
    async def test_streaming_transform(self):
        async def data_generator():
            for i in range(20):
                yield i
        def double_func(x):
            return x * 2
        results = []
        async for result in self.transformer.streaming_transform(data_generator(), double_func, buffer_size=5):
            results.append(result)
        expected = [i * 2 for i in range(20)]
        assert results == expected
class TestDataProcessingPipeline:
    def setup_method(self):
        self.config = PipelineConfiguration(max_workers=4, enable_dynamic_batching=True, max_batch_size=100, enable_performance_profiling=False)
    @pytest.mark.asyncio
    async def test_pipeline_initialization(self):
        pipeline = await create_data_processing_pipeline(self.config)
        assert pipeline is not None
        assert pipeline._is_running
        assert pipeline.config.max_workers == 4
        await pipeline.stop()
        assert not pipeline._is_running
    @pytest.mark.asyncio
    async def test_process_batch_sequential(self):
        pipeline = DataProcessingPipeline(self.config)
        await pipeline.start()
        try:
            data_batch = ['test1', 'test2', 'test3']
            def upper_func(text):
                return text.upper()
            results = await pipeline.process_batch(data_batch, upper_func, enable_parallel=False)
            expected = ['TEST1', 'TEST2', 'TEST3']
            assert results == expected
            assert pipeline.metrics.items_processed == 3
        finally:
            await pipeline.stop()
    @pytest.mark.asyncio
    async def test_process_batch_parallel(self):
        pipeline = DataProcessingPipeline(self.config)
        await pipeline.start()
        try:
            data_batch = list(range(10))
            def slow_square(x):
                time.sleep(0.01)
                return x * x
            start_time = time.time()
            results = await pipeline.process_batch(data_batch, slow_square, enable_parallel=True)
            parallel_time = time.time() - start_time
            expected = [x * x for x in data_batch]
            assert results == expected
            assert parallel_time < 0.5
        finally:
            await pipeline.stop()
    @pytest.mark.asyncio
    async def test_process_large_dataset(self):
        pipeline = DataProcessingPipeline(self.config)
        await pipeline.start()
        try:
            async def large_dataset():
                for i in range(50):
                    yield f'item_{i}'
            def process_item(item):
                return item.upper()
            processed_items = []
            async def output_handler(result):
                processed_items.append(result)
            stats = await pipeline.process_large_dataset(large_dataset(), process_item, output_handler)
            assert stats['total_processed'] == 50
            assert len(processed_items) == 50
            assert processed_items[0] == 'ITEM_0'
            assert processed_items[-1] == 'ITEM_49'
            assert stats['throughput_items_per_second'] > 0
        finally:
            await pipeline.stop()
    @pytest.mark.asyncio
    async def test_error_handling_in_batch(self):
        pipeline = DataProcessingPipeline(self.config)
        await pipeline.start()
        try:
            data_batch = [1, 2, 3, 4, 5]
            def failing_func(x):
                if x == 3:
                    raise ValueError('Test error')
                return x * 10
            results = await pipeline.process_batch(data_batch, failing_func, enable_parallel=True)
            assert results[0] == 10
            assert results[1] == 20
            assert results[2] is None
            assert results[3] == 40
            assert results[4] == 50
        finally:
            await pipeline.stop()
    @pytest.mark.asyncio
    async def test_performance_metrics_collection(self):
        config = PipelineConfiguration(enable_performance_profiling=True)
        pipeline = DataProcessingPipeline(config)
        await pipeline.start()
        try:
            data_batch = list(range(20))
            def identity_func(x):
                return x
            await pipeline.process_batch(data_batch, identity_func)
            metrics = pipeline.get_performance_metrics()
            assert 'throughput' in metrics
            assert 'parallelization' in metrics
            assert 'resources' in metrics
            assert 'batching' in metrics
            assert metrics['throughput']['items_processed'] == 20
            assert metrics['parallelization']['max_workers'] == config.max_workers
        finally:
            await pipeline.stop()
class TestPerformanceIntegration:
    def setup_method(self):
        self.config = PipelineConfiguration(max_workers=4, enable_performance_profiling=False)
    @pytest.mark.asyncio
    async def test_dtesn_performance_collector(self):
        pipeline = DataProcessingPipeline(self.config)
        await pipeline.start()
        try:
            collector = DTESNPerformanceCollector(pipeline)
            data_batch = ['test1', 'test2', 'test3']
            await pipeline.process_batch(data_batch, lambda x: x.upper())
            metrics = collector.collect_pipeline_metrics()
            assert hasattr(metrics, 'timestamp')
            assert hasattr(metrics, 'token_throughput')
            assert hasattr(metrics, 'cpu_utilization')
            alerts = collector.check_alert_conditions(metrics)
            assert isinstance(alerts, list)
        finally:
            await pipeline.stop()
    @pytest.mark.asyncio
    async def test_integrated_pipeline_monitor(self):
        pipeline = DataProcessingPipeline(self.config)
        await pipeline.start()
        try:
            monitor = IntegratedDataPipelineMonitor(pipeline, enable_echo_integration=False)
            await monitor.start_monitoring()
            await asyncio.sleep(0.1)
            status = monitor.get_comprehensive_status()
            assert 'monitoring_active' in status
            assert 'pipeline_metrics' in status
            assert status['monitoring_active'] is True
            await monitor.stop_monitoring()
        finally:
            await pipeline.stop()
    @pytest.mark.asyncio
    async def test_alert_handling(self):
        pipeline = DataProcessingPipeline(self.config)
        await pipeline.start()
        try:
            collector = DTESNPerformanceCollector(pipeline)
            mock_metrics = Mock()
            mock_metrics.token_throughput = 10.0
            mock_metrics.memory_usage = 3000.0
            mock_metrics.cpu_utilization = 90.0
            alerts = collector.check_alert_conditions(mock_metrics)
            assert len(alerts) > 0
            for alert in alerts:
                assert 'severity' in alert
                assert 'message' in alert
                assert 'metric' in alert
        finally:
            await pipeline.stop()
    @pytest.mark.asyncio
    async def test_performance_report_export(self):
        pipeline = DataProcessingPipeline(self.config)
        await pipeline.start()
        try:
            monitor = IntegratedDataPipelineMonitor(pipeline, enable_echo_integration=False)
            data_batch = ['test1', 'test2']
            await pipeline.process_batch(data_batch, lambda x: x.upper())
            report = monitor.export_performance_report('/tmp/test_report.json')
            assert 'report_timestamp' in report
            assert 'pipeline_configuration' in report
            assert 'current_status' in report
            assert 'metrics_history' in report
        finally:
            await pipeline.stop()
class TestIntegrationWithDTESN:
    @pytest.mark.asyncio
    async def test_dtesn_processor_pipeline_integration(self):
        class MockDTESNProcessor:
            def __init__(self):
                self._data_pipeline = None
                self._pipeline_monitor = None
                self._pipeline_config = PipelineConfiguration()
            async def _initialize_data_processing_pipeline(self):
                self._data_pipeline = await create_data_processing_pipeline(self._pipeline_config)
            async def process_data_batch(self, data_batch, enable_parallel=True):
                if not self._data_pipeline:
                    return [{'input': item, 'output': f'processed_{item}'} for item in data_batch]
                def mock_transform(item):
                    return {'input': item, 'output': f'processed_{item}'}
                return await self._data_pipeline.process_batch(data_batch, mock_transform, enable_parallel)
            def get_pipeline_metrics(self):
                if not self._data_pipeline:
                    return {'pipeline_available': False}
                return {'pipeline_available': True, 'pipeline_metrics': self._data_pipeline.get_performance_metrics()}
            async def shutdown_pipeline(self):
                if self._data_pipeline:
                    await self._data_pipeline.stop()
        processor = MockDTESNProcessor()
        await processor._initialize_data_processing_pipeline()
        assert processor._data_pipeline is not None
        data_batch = ['input1', 'input2', 'input3']
        results = await processor.process_data_batch(data_batch)
        assert len(results) == 3
        assert results[0]['input'] == 'input1'
        assert results[0]['output'] == 'processed_input1'
        metrics = processor.get_pipeline_metrics()
        assert metrics['pipeline_available'] is True
        await processor.shutdown_pipeline()
@pytest.mark.asyncio
async def test_end_to_end_data_processing():
    config = PipelineConfiguration(max_workers=4, enable_dynamic_batching=True, max_batch_size=50, enable_vectorization=True, enable_performance_profiling=True)
    pipeline = await create_data_processing_pipeline(config)
    monitor = await create_integrated_pipeline_monitor(pipeline, enable_echo_integration=False)
    try:
        test_cases = [(['small1', 'small2'], 'Small batch processing'), ([f'medium_{i}' for i in range(20)], 'Medium batch processing'), ([f'large_{i}' for i in range(100)], 'Large batch processing')]
        for data_batch, description in test_cases:
            print(f'Testing: {description}')
            def test_processor(item):
                return f'processed_{item}'
            start_time = time.time()
            results = await pipeline.process_batch(data_batch, test_processor)
            processing_time = time.time() - start_time
            assert len(results) == len(data_batch)
            assert all((r.startswith('processed_') for r in results))
            print(f'  Processed {len(data_batch)} items in {processing_time:.3f}s')
            print(f'  Throughput: {len(data_batch) / processing_time:.1f} items/sec')
        final_metrics = pipeline.get_performance_metrics()
        print(f'\nFinal Pipeline Metrics:')
        print(f"  Total items processed: {final_metrics['throughput']['items_processed']}")
        print(f"  Average processing rate: {final_metrics['throughput']['avg_processing_rate']:.1f} items/sec")
        monitor_status = monitor.get_comprehensive_status()
        print(f"  Monitoring active: {monitor_status['monitoring_active']}")
        assert final_metrics['throughput']['items_processed'] > 0
        assert final_metrics['throughput']['avg_processing_rate'] > 50
        print('✅ Phase 7.1.3 acceptance criteria met: Data processing pipelines handle high-volume requests efficiently')
    finally:
        await monitor.stop_monitoring()
        await pipeline.stop()
        print('Pipeline shutdown complete')
if __name__ == '__main__':
    import numpy as np
    asyncio.run(test_end_to_end_data_processing())