import asyncio
import pytest
import time
from unittest.mock import Mock, AsyncMock, patch
from typing import List, Dict, Any
from aphrodite.endpoints.deep_tree_echo.batch_manager import DynamicBatchManager, BatchConfiguration, BatchingMetrics
from aphrodite.endpoints.deep_tree_echo.load_integration import ServerLoadTracker, LoadMetrics
from aphrodite.endpoints.deep_tree_echo.dtesn_processor import DTESNProcessor
from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig
@pytest.fixture
def batch_config():
    return BatchConfiguration(min_batch_size=1, max_batch_size=16, target_batch_size=4, max_batch_wait_ms=25.0, min_batch_wait_ms=5.0, enable_adaptive_sizing=True, performance_window_size=10, adaptation_rate=0.2)
@pytest.fixture
def mock_load_tracker():
    load_values = [0.2, 0.5, 0.8, 0.3, 0.6]
    counter = [0]
    def get_load():
        load = load_values[counter[0] % len(load_values)]
        counter[0] += 1
        return load
    return get_load
@pytest.fixture
async def batch_manager(batch_config, mock_load_tracker):
    manager = DynamicBatchManager(config=batch_config, load_tracker=mock_load_tracker)
    mock_processor = Mock()
    mock_processor.process_batch = AsyncMock()
    manager.set_dtesn_processor(mock_processor)
    await manager.start()
    yield manager
    await manager.stop()
@pytest.fixture
def mock_dtesn_processor():
    processor = Mock(spec=DTESNProcessor)
    processor.config = DTESNConfig()
    processor.max_concurrent_processes = 8
    processor._processing_stats = {'total_requests': 0, 'concurrent_requests': 0, 'avg_processing_time': 0.0}
    async def mock_process(input_data, **kwargs):
        await asyncio.sleep(0.01)
        return Mock(input_data=input_data, processed_output={'result': f'processed_{input_data}'}, processing_time_ms=10.0, engine_integration={})
    processor.process = mock_process
    async def mock_process_batch(inputs, **kwargs):
        results = []
        for inp in inputs:
            result = await mock_process(inp, **kwargs)
            results.append(result)
        return results
    processor.process_batch = mock_process_batch
    return processor
class TestBatchConfiguration:
    def test_default_configuration(self):
        config = BatchConfiguration()
        assert config.min_batch_size == 1
        assert config.max_batch_size == 32
        assert config.target_batch_size == 8
        assert config.enable_adaptive_sizing == True
        assert config.max_batch_wait_ms == 50.0
    def test_custom_configuration(self):
        config = BatchConfiguration(min_batch_size=2, max_batch_size=64, target_batch_size=16, max_batch_wait_ms=100.0)
        assert config.min_batch_size == 2
        assert config.max_batch_size == 64
        assert config.target_batch_size == 16
        assert config.max_batch_wait_ms == 100.0
class TestServerLoadTracker:
    def test_load_tracker_initialization(self):
        tracker = ServerLoadTracker(update_interval=0.5, history_window=30, enable_system_metrics=True)
        assert tracker.update_interval == 0.5
        assert tracker.history_window == 30
        assert tracker.enable_system_metrics == True
    def test_load_calculation_without_sources(self):
        tracker = ServerLoadTracker()
        load = tracker.get_current_load()
        assert 0.0 <= load <= 1.0
    def test_custom_load_provider(self):
        tracker = ServerLoadTracker()
        def custom_provider():
            return 0.7
        tracker.add_load_provider(custom_provider, weight=1.0)
        load = tracker.get_current_load()
        assert load > 0.0
    def test_load_trend_calculation(self):
        tracker = ServerLoadTracker(history_window=10)
        for i in range(15):
            load_value = i / 14.0
            tracker._load_history.append(load_value)
        trend = tracker.get_load_trend(window_size=10)
        assert trend > 0
    @patch('psutil.cpu_percent')
    @patch('psutil.virtual_memory')
    def test_system_metrics_collection(self, mock_memory, mock_cpu):
        mock_cpu.return_value = 45.0
        mock_memory.return_value = Mock(percent=60.0)
        tracker = ServerLoadTracker(enable_system_metrics=True)
        metrics = tracker._get_system_metrics()
        assert metrics['cpu'] == 0.45
        assert metrics['memory'] == 0.6
class TestDynamicBatchManager:
    @pytest.mark.asyncio
    async def test_batch_manager_initialization(self, batch_config, mock_load_tracker):
        manager = DynamicBatchManager(config=batch_config, load_tracker=mock_load_tracker)
        assert manager.config == batch_config
        assert manager.load_tracker == mock_load_tracker
        assert manager._current_batch_size == batch_config.target_batch_size
    @pytest.mark.asyncio
    async def test_dynamic_batch_sizing(self, batch_manager, mock_load_tracker):
        load_scenarios = [0.2, 0.5, 0.8]
        for expected_load in load_scenarios:
            mock_load_tracker.__code__ = lambda: expected_load
            batch_size = batch_manager._calculate_dynamic_batch_size()
            assert batch_manager.config.min_batch_size <= batch_size <= batch_manager.config.max_batch_size
    @pytest.mark.asyncio
    async def test_request_submission_and_processing(self, batch_manager):
        request_data = {'input_data': 'test_input', 'membrane_depth': 4}
        tasks = []
        for i in range(5):
            task = asyncio.create_task(batch_manager.submit_request(request_data={**request_data, 'input_data': f'test_input_{i}'}, priority=1))
            tasks.append(task)
        results = await asyncio.gather(*tasks, return_exceptions=True)
        assert len(results) == 5
        for result in results:
            assert not isinstance(result, Exception)
    @pytest.mark.asyncio
    async def test_circuit_breaker_functionality(self, batch_config, mock_load_tracker):
        batch_config.failure_threshold = 2
        batch_config.circuit_breaker_timeout = 1.0
        manager = DynamicBatchManager(config=batch_config, load_tracker=mock_load_tracker)
        mock_processor = Mock()
        mock_processor.process_batch = AsyncMock(side_effect=Exception('Processing failed'))
        manager.set_dtesn_processor(mock_processor)
        await manager.start()
        try:
            for i in range(3):
                try:
                    await manager.submit_request({'input_data': f'test_{i}'})
                except Exception:
                    pass
            with pytest.raises(RuntimeError, match='Circuit breaker is open'):
                await manager.submit_request({'input_data': 'test_after_failure'})
        finally:
            await manager.stop()
    @pytest.mark.asyncio
    async def test_adaptive_timeout_calculation(self, batch_manager):
        test_times = [0.1, 0.2, 0.15, 0.3, 0.25]
        batch_manager._performance_history.extend([{'timestamp': time.time(), 'throughput': 10.0} for _ in test_times])
        batch_manager._response_times.extend(test_times)
        timeout = batch_manager._calculate_adaptive_timeout()
        assert timeout > 0.0
        assert timeout <= 120.0
    @pytest.mark.asyncio
    async def test_batch_wait_time_calculation(self, batch_manager):
        target_size = 8
        wait_time = batch_manager._calculate_batch_wait_time(8, target_size)
        assert wait_time == 0.0
        wait_time = batch_manager._calculate_batch_wait_time(4, target_size)
        assert 0.0 < wait_time <= batch_manager.config.max_batch_wait_ms / 1000.0
        wait_time = batch_manager._calculate_batch_wait_time(1, target_size)
        assert wait_time >= batch_manager.config.min_batch_wait_ms / 1000.0
class TestDTESNProcessorBatchingIntegration:
    @pytest.mark.asyncio
    async def test_processor_with_batching_enabled(self, mock_dtesn_processor, batch_config, mock_load_tracker):
        processor = DTESNProcessor(config=DTESNConfig(), enable_dynamic_batching=True, batch_config=batch_config, server_load_tracker=mock_load_tracker)
        processor._initialize_dtesn_components = Mock()
        processor._batch_manager = DynamicBatchManager(batch_config, mock_load_tracker)
        processor._batch_manager.set_dtesn_processor(mock_dtesn_processor)
        await processor.start_batch_manager()
        try:
            result = await processor.process_with_dynamic_batching(input_data='test_input', membrane_depth=4, priority=1)
            assert result is not None
        finally:
            await processor.stop_batch_manager()
    @pytest.mark.asyncio
    async def test_enhanced_batch_processing(self, mock_dtesn_processor):
        processor = DTESNProcessor(config=DTESNConfig())
        processor._initialize_dtesn_components = Mock()
        processor.process = mock_dtesn_processor.process
        test_inputs = [f'input_{i}' for i in range(10)]
        results = await processor.process_batch(inputs=test_inputs, membrane_depth=4, enable_load_balancing=True)
        assert len(results) == len(test_inputs)
        for i, result in enumerate(results):
            assert result.input_data == test_inputs[i]
    @pytest.mark.asyncio
    async def test_batch_metrics_collection(self, mock_dtesn_processor, batch_config, mock_load_tracker):
        processor = DTESNProcessor(config=DTESNConfig(), enable_dynamic_batching=True, batch_config=batch_config, server_load_tracker=mock_load_tracker)
        processor._initialize_dtesn_components = Mock()
        processor._batch_manager = DynamicBatchManager(batch_config, mock_load_tracker)
        processor._batch_manager.set_dtesn_processor(mock_dtesn_processor)
        await processor.start_batch_manager()
        try:
            for i in range(5):
                await processor.process_with_dynamic_batching(input_data=f'test_{i}', priority=1)
            metrics = processor.get_batching_metrics()
            batch_size = processor.get_current_batch_size()
            pending_count = await processor.get_pending_batch_count()
            assert isinstance(metrics, BatchingMetrics)
            assert isinstance(batch_size, int)
            assert isinstance(pending_count, int)
        finally:
            await processor.stop_batch_manager()
class TestPerformanceOptimizations:
    @pytest.mark.asyncio
    async def test_throughput_measurement(self, batch_manager):
        request_count = 20
        start_time = time.time()
        tasks = []
        for i in range(request_count):
            task = asyncio.create_task(batch_manager.submit_request(request_data={'input_data': f'perf_test_{i}'}, priority=1))
            tasks.append(task)
        results = await asyncio.gather(*tasks, return_exceptions=True)
        end_time = time.time()
        total_time = end_time - start_time
        throughput = request_count / total_time
        assert throughput > 0.0
        assert len([r for r in results if not isinstance(r, Exception)]) > 0
    @pytest.mark.asyncio
    async def test_batch_size_adaptation(self, batch_manager):
        initial_batch_size = batch_manager.get_current_batch_size()
        for i in range(10):
            load_factor = i % 3 / 2.0
            batch_manager.load_tracker = lambda: load_factor
            await batch_manager.submit_request(request_data={'input_data': f'adapt_test_{i}'}, priority=1)
        final_batch_size = batch_manager.get_current_batch_size()
        assert batch_manager.config.min_batch_size <= final_batch_size <= batch_manager.config.max_batch_size
    @pytest.mark.asyncio
    async def test_load_aware_concurrency_adjustment(self, mock_dtesn_processor):
        processor = DTESNProcessor(config=DTESNConfig())
        processor._initialize_dtesn_components = Mock()
        processor.process = mock_dtesn_processor.process
        mock_batch_manager = Mock()
        mock_batch_manager._get_current_load = Mock()
        processor._batch_manager = mock_batch_manager
        test_scenarios = [(0.2, 'low_load'), (0.5, 'normal_load'), (0.8, 'high_load')]
        for load_value, scenario in test_scenarios:
            mock_batch_manager._get_current_load.return_value = load_value
            results = await processor.process_batch(inputs=[f'test_{scenario}_{i}' for i in range(5)], enable_load_balancing=True)
            assert len(results) == 5
            for result in results:
                assert hasattr(result, 'engine_integration')
                assert result.engine_integration.get('batch_processed') == True
@pytest.mark.asyncio
class TestIntegrationScenarios:
    async def test_end_to_end_batching_workflow(self, batch_config, mock_load_tracker):
        manager = DynamicBatchManager(batch_config, mock_load_tracker)
        mock_processor = Mock()
        async def realistic_batch_process(inputs, **kwargs):
            await asyncio.sleep(0.05)
            results = []
            for inp in inputs:
                result = Mock(input_data=inp, processed_output={'result': f'processed_{inp}'}, processing_time_ms=45.0 + len(inp) * 0.1, engine_integration={'batch_processed': True})
                results.append(result)
            return results
        mock_processor.process_batch = realistic_batch_process
        manager.set_dtesn_processor(mock_processor)
        await manager.start()
        try:
            high_priority_tasks = [manager.submit_request(request_data={'input_data': f'high_priority_{i}'}, priority=0) for i in range(3)]
            normal_priority_tasks = [manager.submit_request(request_data={'input_data': f'normal_priority_{i}'}, priority=1) for i in range(5)]
            low_priority_tasks = [manager.submit_request(request_data={'input_data': f'low_priority_{i}'}, priority=2) for i in range(2)]
            all_tasks = high_priority_tasks + normal_priority_tasks + low_priority_tasks
            results = await asyncio.gather(*all_tasks, return_exceptions=True)
            assert len(results) == 10
            successful_results = [r for r in results if not isinstance(r, Exception)]
            assert len(successful_results) > 0
            metrics = manager.get_metrics()
            assert metrics.requests_processed >= 10
            assert metrics.throughput_improvement >= 0
        finally:
            await manager.stop()
    async def test_stress_testing_batching_system(self, batch_config, mock_load_tracker):
        stress_config = BatchConfiguration(min_batch_size=1, max_batch_size=64, target_batch_size=16, max_batch_wait_ms=25.0, enable_adaptive_sizing=True)
        manager = DynamicBatchManager(stress_config, mock_load_tracker)
        mock_processor = Mock()
        mock_processor.process_batch = AsyncMock(return_value=[Mock(input_data=f'stress_{i}', processed_output={'result': f'result_{i}'}, processing_time_ms=5.0, engine_integration={}) for i in range(32)])
        manager.set_dtesn_processor(mock_processor)
        await manager.start()
        try:
            request_count = 200
            tasks = []
            start_time = time.time()
            for i in range(request_count):
                task = asyncio.create_task(manager.submit_request(request_data={'input_data': f'stress_test_{i}'}, priority=i % 3))
                tasks.append(task)
            results = await asyncio.gather(*tasks, return_exceptions=True)
            end_time = time.time()
            total_time = end_time - start_time
            throughput = request_count / total_time
            successful_count = len([r for r in results if not isinstance(r, Exception)])
            success_rate = successful_count / request_count
            assert throughput > 10.0
            assert success_rate > 0.9
            metrics = manager.get_metrics()
            assert metrics.requests_processed >= successful_count
            logger.info(f'Stress test completed: {successful_count}/{request_count} successful, throughput: {throughput:.1f} req/s, time: {total_time:.2f}s')
        finally:
            await manager.stop()
if __name__ == '__main__':
    pytest.main([__file__, '-v', '--asyncio-mode=auto'])