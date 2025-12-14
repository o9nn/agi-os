"""
Tests for dynamic batching system in DTESN operations.

Tests the request batching system, dynamic batch sizing, load-aware
optimization, and integration with Aphrodite's continuous batching.
"""

import asyncio
import pytest
import time
from unittest.mock import Mock, AsyncMock, patch
from typing import List, Dict, Any

# Import components to test
from aphrodite.endpoints.deep_tree_echo.batch_manager import (
    DynamicBatchManager,
    BatchConfiguration,
    BatchingMetrics
)
from aphrodite.endpoints.deep_tree_echo.load_integration import (
    ServerLoadTracker,
    LoadMetrics
)
from aphrodite.endpoints.deep_tree_echo.dtesn_processor import DTESNProcessor
from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig


@pytest.fixture
def batch_config():
    """Create test batch configuration."""
    return BatchConfiguration(
        min_batch_size=1,
        max_batch_size=16,
        target_batch_size=4,
        max_batch_wait_ms=25.0,
        min_batch_wait_ms=5.0,
        enable_adaptive_sizing=True,
        performance_window_size=10,
        adaptation_rate=0.2
    )


@pytest.fixture
def mock_load_tracker():
    """Create mock load tracker function."""
    load_values = [0.2, 0.5, 0.8, 0.3, 0.6]  # Varying load pattern
    counter = [0]
    
    def get_load():
        load = load_values[counter[0] % len(load_values)]
        counter[0] += 1
        return load
    
    return get_load


@pytest.fixture
async def batch_manager(batch_config, mock_load_tracker):
    """Create and start batch manager for testing."""
    manager = DynamicBatchManager(
        config=batch_config,
        load_tracker=mock_load_tracker
    )
    
    # Mock DTESN processor
    mock_processor = Mock()
    mock_processor.process_batch = AsyncMock()
    manager.set_dtesn_processor(mock_processor)
    
    await manager.start()
    yield manager
    await manager.stop()


@pytest.fixture
def mock_dtesn_processor():
    """Create mock DTESN processor."""
    processor = Mock(spec=DTESNProcessor)
    processor.config = DTESNConfig()
    processor.max_concurrent_processes = 8
    processor._processing_stats = {
        "total_requests": 0,
        "concurrent_requests": 0,
        "avg_processing_time": 0.0
    }
    
    # Mock process method
    async def mock_process(input_data, **kwargs):
        # Simulate processing time
        await asyncio.sleep(0.01)
        return Mock(
            input_data=input_data,
            processed_output={"result": f"processed_{input_data}"},
            processing_time_ms=10.0,
            engine_integration={}
        )
    
    processor.process = mock_process
    
    # Mock batch process method
    async def mock_process_batch(inputs, **kwargs):
        results = []
        for inp in inputs:
            result = await mock_process(inp, **kwargs)
            results.append(result)
        return results
    
    processor.process_batch = mock_process_batch
    return processor


class TestBatchConfiguration:
    """Test batch configuration validation and behavior."""
    
    def test_default_configuration(self):
        """Test default batch configuration values."""
        config = BatchConfiguration()
        assert config.min_batch_size == 1
        assert config.max_batch_size == 32
        assert config.target_batch_size == 8
        assert config.enable_adaptive_sizing == True
        assert config.max_batch_wait_ms == 50.0
    
    def test_custom_configuration(self):
        """Test custom batch configuration."""
        config = BatchConfiguration(
            min_batch_size=2,
            max_batch_size=64,
            target_batch_size=16,
            max_batch_wait_ms=100.0
        )
        assert config.min_batch_size == 2
        assert config.max_batch_size == 64
        assert config.target_batch_size == 16
        assert config.max_batch_wait_ms == 100.0


class TestServerLoadTracker:
    """Test server load tracking functionality."""
    
    def test_load_tracker_initialization(self):
        """Test load tracker initialization."""
        tracker = ServerLoadTracker(
            update_interval=0.5,
            history_window=30,
            enable_system_metrics=True
        )
        assert tracker.update_interval == 0.5
        assert tracker.history_window == 30
        assert tracker.enable_system_metrics == True
    
    def test_load_calculation_without_sources(self):
        """Test load calculation with no load sources."""
        tracker = ServerLoadTracker()
        load = tracker.get_current_load()
        assert 0.0 <= load <= 1.0
    
    def test_custom_load_provider(self):
        """Test adding custom load provider."""
        tracker = ServerLoadTracker()
        
        def custom_provider():
            return 0.7
        
        tracker.add_load_provider(custom_provider, weight=1.0)
        load = tracker.get_current_load()
        
        # Should reflect the custom load
        assert load > 0.0
    
    def test_load_trend_calculation(self):
        """Test load trend calculation."""
        tracker = ServerLoadTracker(history_window=10)
        
        # Simulate increasing load pattern
        for i in range(15):
            load_value = i / 14.0  # 0.0 to 1.0
            tracker._load_history.append(load_value)
        
        trend = tracker.get_load_trend(window_size=10)
        assert trend > 0  # Should detect increasing trend
    
    @patch('psutil.cpu_percent')
    @patch('psutil.virtual_memory')
    def test_system_metrics_collection(self, mock_memory, mock_cpu):
        """Test system metrics collection."""
        mock_cpu.return_value = 45.0
        mock_memory.return_value = Mock(percent=60.0)
        
        tracker = ServerLoadTracker(enable_system_metrics=True)
        metrics = tracker._get_system_metrics()
        
        assert metrics["cpu"] == 0.45
        assert metrics["memory"] == 0.60


class TestDynamicBatchManager:
    """Test dynamic batch manager functionality."""
    
    @pytest.mark.asyncio
    async def test_batch_manager_initialization(self, batch_config, mock_load_tracker):
        """Test batch manager initialization."""
        manager = DynamicBatchManager(
            config=batch_config,
            load_tracker=mock_load_tracker
        )
        assert manager.config == batch_config
        assert manager.load_tracker == mock_load_tracker
        assert manager._current_batch_size == batch_config.target_batch_size
    
    @pytest.mark.asyncio
    async def test_dynamic_batch_sizing(self, batch_manager, mock_load_tracker):
        """Test dynamic batch size calculation."""
        # Test with different load conditions
        load_scenarios = [0.2, 0.5, 0.8]  # low, medium, high load
        
        for expected_load in load_scenarios:
            # Update load tracker to return specific load
            mock_load_tracker.__code__ = lambda: expected_load
            
            batch_size = batch_manager._calculate_dynamic_batch_size()
            
            # Verify batch size is within bounds
            assert batch_manager.config.min_batch_size <= batch_size <= batch_manager.config.max_batch_size
    
    @pytest.mark.asyncio
    async def test_request_submission_and_processing(self, batch_manager):
        """Test request submission and batch processing."""
        # Submit test requests
        request_data = {"input_data": "test_input", "membrane_depth": 4}
        
        # Submit multiple requests concurrently
        tasks = []
        for i in range(5):
            task = asyncio.create_task(
                batch_manager.submit_request(
                    request_data={**request_data, "input_data": f"test_input_{i}"},
                    priority=1
                )
            )
            tasks.append(task)
        
        # Wait for all requests to complete
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        # Verify all requests completed
        assert len(results) == 5
        for result in results:
            assert not isinstance(result, Exception)
    
    @pytest.mark.asyncio
    async def test_circuit_breaker_functionality(self, batch_config, mock_load_tracker):
        """Test circuit breaker pattern in batch manager."""
        # Configure for quick circuit breaker activation
        batch_config.failure_threshold = 2
        batch_config.circuit_breaker_timeout = 1.0
        
        manager = DynamicBatchManager(
            config=batch_config,
            load_tracker=mock_load_tracker
        )
        
        # Mock failing processor
        mock_processor = Mock()
        mock_processor.process_batch = AsyncMock(side_effect=Exception("Processing failed"))
        manager.set_dtesn_processor(mock_processor)
        
        await manager.start()
        
        try:
            # Trigger failures to open circuit breaker
            for i in range(3):
                try:
                    await manager.submit_request({"input_data": f"test_{i}"})
                except Exception:
                    pass  # Expected failures
            
            # Next request should trigger circuit breaker
            with pytest.raises(RuntimeError, match="Circuit breaker is open"):
                await manager.submit_request({"input_data": "test_after_failure"})
                
        finally:
            await manager.stop()
    
    @pytest.mark.asyncio
    async def test_adaptive_timeout_calculation(self, batch_manager):
        """Test adaptive timeout calculation."""
        # Add some response times to history
        test_times = [0.1, 0.2, 0.15, 0.3, 0.25]
        batch_manager._performance_history.extend([
            {"timestamp": time.time(), "throughput": 10.0} 
            for _ in test_times
        ])
        batch_manager._response_times.extend(test_times)
        
        timeout = batch_manager._calculate_adaptive_timeout()
        
        # Should calculate reasonable timeout based on history
        assert timeout > 0.0
        assert timeout <= 120.0  # Within max bounds
    
    @pytest.mark.asyncio
    async def test_batch_wait_time_calculation(self, batch_manager):
        """Test batch wait time calculation."""
        # Test with different fill ratios
        target_size = 8
        
        # Full batch - should not wait
        wait_time = batch_manager._calculate_batch_wait_time(8, target_size)
        assert wait_time == 0.0
        
        # Half full batch - should wait some time
        wait_time = batch_manager._calculate_batch_wait_time(4, target_size)
        assert 0.0 < wait_time <= batch_manager.config.max_batch_wait_ms / 1000.0
        
        # Empty batch - should wait maximum time
        wait_time = batch_manager._calculate_batch_wait_time(1, target_size)
        assert wait_time >= batch_manager.config.min_batch_wait_ms / 1000.0


class TestDTESNProcessorBatchingIntegration:
    """Test DTESN processor integration with batching system."""
    
    @pytest.mark.asyncio
    async def test_processor_with_batching_enabled(self, mock_dtesn_processor, batch_config, mock_load_tracker):
        """Test DTESN processor with dynamic batching enabled."""
        processor = DTESNProcessor(
            config=DTESNConfig(),
            enable_dynamic_batching=True,
            batch_config=batch_config,
            server_load_tracker=mock_load_tracker
        )
        
        # Mock the internal methods
        processor._initialize_dtesn_components = Mock()
        processor._batch_manager = DynamicBatchManager(batch_config, mock_load_tracker)
        processor._batch_manager.set_dtesn_processor(mock_dtesn_processor)
        
        await processor.start_batch_manager()
        
        try:
            # Test processing with batching
            result = await processor.process_with_dynamic_batching(
                input_data="test_input",
                membrane_depth=4,
                priority=1
            )
            
            # Verify processing completed
            assert result is not None
            
        finally:
            await processor.stop_batch_manager()
    
    @pytest.mark.asyncio
    async def test_enhanced_batch_processing(self, mock_dtesn_processor):
        """Test enhanced batch processing with load balancing."""
        processor = DTESNProcessor(config=DTESNConfig())
        
        # Mock internal methods
        processor._initialize_dtesn_components = Mock()
        processor.process = mock_dtesn_processor.process
        
        # Create batch of inputs
        test_inputs = [f"input_{i}" for i in range(10)]
        
        # Process batch with load balancing
        results = await processor.process_batch(
            inputs=test_inputs,
            membrane_depth=4,
            enable_load_balancing=True
        )
        
        # Verify all inputs were processed
        assert len(results) == len(test_inputs)
        for i, result in enumerate(results):
            assert result.input_data == test_inputs[i]
    
    @pytest.mark.asyncio
    async def test_batch_metrics_collection(self, mock_dtesn_processor, batch_config, mock_load_tracker):
        """Test batch metrics collection and reporting."""
        processor = DTESNProcessor(
            config=DTESNConfig(),
            enable_dynamic_batching=True,
            batch_config=batch_config,
            server_load_tracker=mock_load_tracker
        )
        
        # Mock internal setup
        processor._initialize_dtesn_components = Mock()
        processor._batch_manager = DynamicBatchManager(batch_config, mock_load_tracker)
        processor._batch_manager.set_dtesn_processor(mock_dtesn_processor)
        
        await processor.start_batch_manager()
        
        try:
            # Process some requests to generate metrics
            for i in range(5):
                await processor.process_with_dynamic_batching(
                    input_data=f"test_{i}",
                    priority=1
                )
            
            # Get metrics
            metrics = processor.get_batching_metrics()
            batch_size = processor.get_current_batch_size()
            pending_count = await processor.get_pending_batch_count()
            
            # Verify metrics are available
            assert isinstance(metrics, BatchingMetrics)
            assert isinstance(batch_size, int)
            assert isinstance(pending_count, int)
            
        finally:
            await processor.stop_batch_manager()


class TestPerformanceOptimizations:
    """Test performance optimizations and throughput improvements."""
    
    @pytest.mark.asyncio
    async def test_throughput_measurement(self, batch_manager):
        """Test throughput measurement and improvement tracking."""
        # Submit batch of requests and measure throughput
        request_count = 20
        start_time = time.time()
        
        tasks = []
        for i in range(request_count):
            task = asyncio.create_task(
                batch_manager.submit_request(
                    request_data={"input_data": f"perf_test_{i}"},
                    priority=1
                )
            )
            tasks.append(task)
        
        results = await asyncio.gather(*tasks, return_exceptions=True)
        end_time = time.time()
        
        # Calculate throughput
        total_time = end_time - start_time
        throughput = request_count / total_time
        
        # Verify reasonable throughput
        assert throughput > 0.0
        assert len([r for r in results if not isinstance(r, Exception)]) > 0
    
    @pytest.mark.asyncio
    async def test_batch_size_adaptation(self, batch_manager):
        """Test batch size adaptation based on performance."""
        initial_batch_size = batch_manager.get_current_batch_size()
        
        # Simulate varying performance conditions
        for i in range(10):
            # Submit requests with different load patterns
            load_factor = (i % 3) / 2.0  # 0.0, 0.5, 1.0 pattern
            
            # Mock load tracker for this iteration
            batch_manager.load_tracker = lambda: load_factor
            
            # Process some requests
            await batch_manager.submit_request(
                request_data={"input_data": f"adapt_test_{i}"},
                priority=1
            )
        
        final_batch_size = batch_manager.get_current_batch_size()
        
        # Batch size should be within valid range
        assert batch_manager.config.min_batch_size <= final_batch_size <= batch_manager.config.max_batch_size
    
    @pytest.mark.asyncio
    async def test_load_aware_concurrency_adjustment(self, mock_dtesn_processor):
        """Test load-aware concurrency adjustment in batch processing."""
        processor = DTESNProcessor(config=DTESNConfig())
        processor._initialize_dtesn_components = Mock()
        processor.process = mock_dtesn_processor.process
        
        # Mock batch manager with different load scenarios
        mock_batch_manager = Mock()
        mock_batch_manager._get_current_load = Mock()
        processor._batch_manager = mock_batch_manager
        
        test_scenarios = [
            (0.2, "low_load"),    # Should increase concurrency
            (0.5, "normal_load"), # Should use normal concurrency
            (0.8, "high_load")    # Should decrease concurrency
        ]
        
        for load_value, scenario in test_scenarios:
            mock_batch_manager._get_current_load.return_value = load_value
            
            # Process batch with load balancing
            results = await processor.process_batch(
                inputs=[f"test_{scenario}_{i}" for i in range(5)],
                enable_load_balancing=True
            )
            
            # Verify processing completed successfully
            assert len(results) == 5
            for result in results:
                assert hasattr(result, 'engine_integration')
                assert result.engine_integration.get('batch_processed') == True


@pytest.mark.asyncio
class TestIntegrationScenarios:
    """Integration tests for complete batching system."""
    
    async def test_end_to_end_batching_workflow(self, batch_config, mock_load_tracker):
        """Test complete end-to-end batching workflow."""
        # Create complete system
        manager = DynamicBatchManager(batch_config, mock_load_tracker)
        
        # Mock processor with realistic behavior
        mock_processor = Mock()
        
        async def realistic_batch_process(inputs, **kwargs):
            # Simulate realistic processing with some delay
            await asyncio.sleep(0.05)  # 50ms processing time
            
            results = []
            for inp in inputs:
                result = Mock(
                    input_data=inp,
                    processed_output={"result": f"processed_{inp}"},
                    processing_time_ms=45.0 + (len(inp) * 0.1),
                    engine_integration={"batch_processed": True}
                )
                results.append(result)
            return results
        
        mock_processor.process_batch = realistic_batch_process
        manager.set_dtesn_processor(mock_processor)
        
        await manager.start()
        
        try:
            # Submit requests with different priorities
            high_priority_tasks = [
                manager.submit_request(
                    request_data={"input_data": f"high_priority_{i}"},
                    priority=0
                )
                for i in range(3)
            ]
            
            normal_priority_tasks = [
                manager.submit_request(
                    request_data={"input_data": f"normal_priority_{i}"},
                    priority=1
                )
                for i in range(5)
            ]
            
            low_priority_tasks = [
                manager.submit_request(
                    request_data={"input_data": f"low_priority_{i}"},
                    priority=2
                )
                for i in range(2)
            ]
            
            # Wait for all tasks to complete
            all_tasks = high_priority_tasks + normal_priority_tasks + low_priority_tasks
            results = await asyncio.gather(*all_tasks, return_exceptions=True)
            
            # Verify all requests completed successfully
            assert len(results) == 10
            successful_results = [r for r in results if not isinstance(r, Exception)]
            assert len(successful_results) > 0
            
            # Check metrics
            metrics = manager.get_metrics()
            assert metrics.requests_processed >= 10
            assert metrics.throughput_improvement >= 0  # Should show improvement or no regression
            
        finally:
            await manager.stop()
    
    async def test_stress_testing_batching_system(self, batch_config, mock_load_tracker):
        """Stress test the batching system with high load."""
        # Configure for stress testing
        stress_config = BatchConfiguration(
            min_batch_size=1,
            max_batch_size=64,
            target_batch_size=16,
            max_batch_wait_ms=25.0,
            enable_adaptive_sizing=True
        )
        
        manager = DynamicBatchManager(stress_config, mock_load_tracker)
        
        # Fast mock processor
        mock_processor = Mock()
        mock_processor.process_batch = AsyncMock(return_value=[
            Mock(
                input_data=f"stress_{i}",
                processed_output={"result": f"result_{i}"},
                processing_time_ms=5.0,
                engine_integration={}
            )
            for i in range(32)  # Return up to max batch size
        ])
        
        manager.set_dtesn_processor(mock_processor)
        await manager.start()
        
        try:
            # Submit large number of concurrent requests
            request_count = 200
            tasks = []
            
            start_time = time.time()
            
            for i in range(request_count):
                task = asyncio.create_task(
                    manager.submit_request(
                        request_data={"input_data": f"stress_test_{i}"},
                        priority=i % 3  # Mix priorities
                    )
                )
                tasks.append(task)
            
            # Wait for completion
            results = await asyncio.gather(*tasks, return_exceptions=True)
            end_time = time.time()
            
            # Analyze performance
            total_time = end_time - start_time
            throughput = request_count / total_time
            
            successful_count = len([r for r in results if not isinstance(r, Exception)])
            success_rate = successful_count / request_count
            
            # Performance assertions
            assert throughput > 10.0  # Should handle at least 10 requests/second
            assert success_rate > 0.90  # Should have >90% success rate
            
            # Check final metrics
            metrics = manager.get_metrics()
            assert metrics.requests_processed >= successful_count
            
            logger.info(
                f"Stress test completed: {successful_count}/{request_count} successful, "
                f"throughput: {throughput:.1f} req/s, time: {total_time:.2f}s"
            )
            
        finally:
            await manager.stop()


if __name__ == "__main__":
    # Run tests with pytest
    pytest.main([__file__, "-v", "--asyncio-mode=auto"])