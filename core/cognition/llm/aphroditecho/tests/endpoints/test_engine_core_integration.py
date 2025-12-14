"""
Engine Core Integration Testing for Deep Tree Echo System.

Tests the integration between DTESN processors and Aphrodite Engine core components,
validating server-side model loading, management, and backend processing pipelines
as required for Phase 5.2.2.
"""

import pytest
import asyncio
import time
from unittest.mock import AsyncMock, MagicMock

from aphrodite.endpoints.deep_tree_echo.dtesn_processor import DTESNProcessor
from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig


class TestEngineIntegration:
    """Test suite for Aphrodite Engine core integration with DTESN processing."""

    @pytest.fixture
    def mock_async_aphrodite(self):
        """Mock AsyncAphrodite engine with realistic behavior."""
        engine = AsyncMock()
        
        # Mock engine configuration methods
        engine.get_model_config.return_value = MagicMock(
            model="meta-llama/Meta-Llama-3.1-8B-Instruct",
            max_model_len=4096,
            vocab_size=32000,
            tokenizer="meta-llama/Meta-Llama-3.1-8B-Instruct"
        )
        
        engine.get_aphrodite_config.return_value = MagicMock(
            model_config=MagicMock(model="meta-llama/Meta-Llama-3.1-8B-Instruct"),
            parallel_config=MagicMock(tensor_parallel_size=1, pipeline_parallel_size=1),
            scheduler_config=MagicMock(max_num_seqs=256, max_model_len=4096),
            cache_config=MagicMock(block_size=16, gpu_memory_utilization=0.9)
        )
        
        # Mock processing methods
        engine.generate.return_value = AsyncMock()
        engine.encode.return_value = AsyncMock()
        
        return engine

    @pytest.fixture
    def dtesn_config(self):
        """DTESN configuration for engine integration testing."""
        return DTESNConfig(
            max_membrane_depth=4,
            esn_reservoir_size=256,
            bseries_max_order=8,
            enable_caching=True
        )

    @pytest.fixture
    async def dtesn_processor(self, mock_async_aphrodite, dtesn_config):
        """DTESNProcessor with engine integration."""
        processor = DTESNProcessor(
            config=dtesn_config,
            engine=mock_async_aphrodite,
            max_concurrent_processes=5
        )
        await processor._initialize_engine_integration()
        return processor

    @pytest.mark.asyncio
    async def test_engine_initialization_integration(self, mock_async_aphrodite, dtesn_config):
        """
        Test 1: Engine Core Initialization Integration
        
        Validates proper initialization of DTESN processor with Aphrodite Engine.
        """
        processor = DTESNProcessor(
            config=dtesn_config,
            engine=mock_async_aphrodite,
            max_concurrent_processes=10
        )
        
        # Verify engine is stored
        assert processor.engine is mock_async_aphrodite
        
        # Initialize engine integration
        await processor._initialize_engine_integration()
        
        # Verify engine configurations are fetched
        assert hasattr(processor, 'engine_config')
        assert hasattr(processor, 'model_config')
        
        # Verify configuration calls were made
        mock_async_aphrodite.get_aphrodite_config.assert_called_once()
        mock_async_aphrodite.get_model_config.assert_called_once()

    @pytest.mark.asyncio
    async def test_server_side_model_loading_integration(self, dtesn_processor):
        """
        Test 2: Server-Side Model Loading Integration
        
        Tests integration with model loading and management through engine.
        """
        # Test model configuration access
        model_config = dtesn_processor.model_config
        assert model_config is not None
        assert hasattr(model_config, 'model')
        assert hasattr(model_config, 'max_model_len')
        
        # Test engine configuration access
        engine_config = dtesn_processor.engine_config
        assert engine_config is not None
        assert hasattr(engine_config, 'model_config')
        assert hasattr(engine_config, 'parallel_config')
        
        # Verify model information is accessible
        model_name = getattr(model_config, 'model', 'unknown')
        assert model_name != 'unknown'

    @pytest.mark.asyncio
    async def test_backend_processing_pipeline_integration(self, dtesn_processor):
        """
        Test 3: Backend Processing Pipeline Integration
        
        Tests DTESN processing pipeline integration with engine backend.
        """
        test_input = "test input for backend processing pipeline"
        
        # Process through DTESN with engine integration
        result = await dtesn_processor.process(
            input_data=test_input,
            membrane_depth=3,
            esn_size=128,
            enable_concurrent=True
        )
        
        # Verify result structure
        assert result is not None
        assert hasattr(result, 'output')
        assert hasattr(result, 'processing_time_ms')
        assert hasattr(result, 'membrane_layers')
        
        # Verify backend processing characteristics
        assert result.membrane_layers == 3
        assert result.processing_time_ms >= 0
        assert isinstance(result.output, str)

    @pytest.mark.asyncio
    async def test_engine_context_fetching(self, dtesn_processor):
        """
        Test 4: Engine Context Fetching Integration
        
        Tests comprehensive engine context fetching for DTESN processing.
        """
        engine_context = await dtesn_processor._fetch_comprehensive_engine_context()
        
        # Verify context structure
        assert "model_config" in engine_context
        assert "engine_config" in engine_context
        assert "processing_capabilities" in engine_context
        
        # Verify model configuration in context
        model_config = engine_context["model_config"]
        assert "model_name" in model_config
        assert "max_model_length" in model_config
        
        # Verify processing capabilities
        capabilities = engine_context["processing_capabilities"]
        assert "supports_batching" in capabilities
        assert "supports_streaming" in capabilities

    @pytest.mark.asyncio
    async def test_concurrent_engine_processing(self, dtesn_processor):
        """
        Test 5: Concurrent Engine Processing Integration
        
        Tests concurrent processing through engine with DTESN pipelines.
        """
        # Create multiple concurrent processing tasks
        tasks = []
        for i in range(5):
            task = dtesn_processor.process(
                input_data=f"concurrent test input {i}",
                membrane_depth=2,
                esn_size=64,
                enable_concurrent=True
            )
            tasks.append(task)
        
        # Process concurrently
        start_time = time.time()
        results = await asyncio.gather(*tasks)
        total_time = time.time() - start_time
        
        # Verify all results
        assert len(results) == 5
        for i, result in enumerate(results):
            assert result is not None
            assert result.membrane_layers == 2
            assert f"concurrent test input {i}" in result.output or result.output != ""
        
        # Verify concurrent processing is efficient
        assert total_time < 5.0  # Should complete within reasonable time

    @pytest.mark.asyncio
    async def test_engine_state_synchronization(self, dtesn_processor):
        """
        Test 6: Engine State Synchronization
        
        Tests synchronization between DTESN processor and engine state.
        """
        # Sync with engine state
        await dtesn_processor._sync_with_engine_state()
        
        # Verify state synchronization doesn't break processing
        result = await dtesn_processor.process(
            input_data="state sync test",
            membrane_depth=2,
            esn_size=64
        )
        
        assert result is not None
        assert result.processing_time_ms >= 0

    @pytest.mark.asyncio
    async def test_engine_configuration_serialization(self, dtesn_processor):
        """
        Test 7: Engine Configuration Serialization
        
        Tests serialization of engine configurations for server responses.
        """
        # Test model config serialization
        model_config_serialized = dtesn_processor._serialize_config(dtesn_processor.model_config)
        assert isinstance(model_config_serialized, dict)
        
        # Test engine config serialization
        engine_config_serialized = dtesn_processor._serialize_config(dtesn_processor.engine_config)
        assert isinstance(engine_config_serialized, dict)
        
        # Verify serialized data doesn't contain sensitive information
        config_str = str(model_config_serialized)
        sensitive_terms = ["password", "secret", "token", "key", "auth"]
        for term in sensitive_terms:
            assert term not in config_str.lower()

    @pytest.mark.asyncio 
    async def test_engine_error_handling(self, mock_async_aphrodite, dtesn_config):
        """
        Test 8: Engine Error Handling Integration
        
        Tests error handling when engine integration fails.
        """
        # Configure engine to raise errors
        mock_async_aphrodite.get_model_config.side_effect = Exception("Engine connection failed")
        
        processor = DTESNProcessor(
            config=dtesn_config,
            engine=mock_async_aphrodite
        )
        
        # Test graceful error handling during initialization
        try:
            await processor._initialize_engine_integration()
        except Exception as e:
            # Should handle engine errors gracefully
            assert "Engine connection failed" in str(e)
        
        # Processor should still be functional for basic operations
        assert processor.config is not None

    @pytest.mark.asyncio
    async def test_optimal_configuration_calculation(self, dtesn_processor):
        """
        Test 9: Optimal Configuration Calculation
        
        Tests calculation of optimal DTESN parameters based on engine capabilities.
        """
        # Test optimal membrane depth calculation
        optimal_depth = dtesn_processor._get_optimal_membrane_depth()
        assert isinstance(optimal_depth, int)
        assert 1 <= optimal_depth <= dtesn_processor.config.max_membrane_depth
        
        # Test optimal ESN size calculation
        optimal_esn = dtesn_processor._get_optimal_esn_size()
        assert isinstance(optimal_esn, int)
        assert 32 <= optimal_esn <= dtesn_processor.config.esn_reservoir_size

    @pytest.mark.asyncio
    async def test_engine_integration_metrics(self, dtesn_processor):
        """
        Test 10: Engine Integration Metrics
        
        Tests collection of engine integration metrics and performance data.
        """
        # Process some data to generate metrics
        await dtesn_processor.process(
            input_data="metrics test",
            membrane_depth=3,
            esn_size=128
        )
        
        # Get engine context with metrics
        context = await dtesn_processor._fetch_comprehensive_engine_context()
        
        # Verify metrics are included
        assert "processing_capabilities" in context
        assert "integration_status" in context
        
        capabilities = context["processing_capabilities"]
        assert "max_concurrent_requests" in capabilities
        assert "memory_utilization" in capabilities

    @pytest.mark.asyncio
    async def test_engine_aware_pipeline_setup(self, dtesn_processor):
        """
        Test 11: Engine-Aware Pipeline Setup
        
        Tests setup of engine-aware processing pipelines.
        """
        # Setup engine-aware pipelines
        await dtesn_processor._setup_engine_aware_pipelines()
        
        # Verify pipeline configuration
        assert hasattr(dtesn_processor, 'engine_pipeline_config')
        
        # Test pipeline processing
        result = await dtesn_processor.process(
            input_data="engine pipeline test",
            membrane_depth=2,
            esn_size=64
        )
        
        assert result is not None
        assert hasattr(result, 'engine_integration_metadata')

    def test_engine_integration_without_engine(self, dtesn_config):
        """
        Test 12: DTESN Processing Without Engine
        
        Tests that DTESN processor works without engine integration (fallback mode).
        """
        processor = DTESNProcessor(config=dtesn_config, engine=None)
        
        # Verify processor is created without engine
        assert processor.engine is None
        assert processor.config is dtesn_config
        
        # Should still be able to perform basic processing
        # (This tests graceful degradation when engine is unavailable)
        assert processor._get_optimal_membrane_depth() > 0
        assert processor._get_optimal_esn_size() > 0


class TestEngineIntegrationPerformance:
    """Performance-focused tests for engine integration."""

    @pytest.fixture
    def performance_config(self):
        """Configuration optimized for performance testing."""
        return DTESNConfig(
            max_membrane_depth=8,
            esn_reservoir_size=512,
            bseries_max_order=16,
            enable_caching=True,
            enable_performance_monitoring=True
        )

    @pytest.mark.asyncio
    async def test_engine_initialization_performance(self, mock_async_aphrodite, performance_config):
        """
        Test engine initialization performance under load.
        """
        start_time = time.time()
        
        processor = DTESNProcessor(
            config=performance_config,
            engine=mock_async_aphrodite,
            max_concurrent_processes=20
        )
        await processor._initialize_engine_integration()
        
        initialization_time = time.time() - start_time
        
        # Initialization should be reasonably fast
        assert initialization_time < 1.0  # Should complete within 1 second

    @pytest.mark.asyncio
    async def test_high_concurrency_engine_processing(self, mock_async_aphrodite, performance_config):
        """
        Test high concurrency processing with engine integration.
        """
        processor = DTESNProcessor(
            config=performance_config,
            engine=mock_async_aphrodite,
            max_concurrent_processes=50
        )
        await processor._initialize_engine_integration()
        
        # Create many concurrent tasks
        num_tasks = 20
        tasks = [
            processor.process(
                input_data=f"high concurrency test {i}",
                membrane_depth=4,
                esn_size=256,
                enable_concurrent=True
            )
            for i in range(num_tasks)
        ]
        
        start_time = time.time()
        results = await asyncio.gather(*tasks)
        total_time = time.time() - start_time
        
        # Verify all tasks completed successfully
        assert len(results) == num_tasks
        assert all(r is not None for r in results)
        
        # Performance should be acceptable for high concurrency
        avg_time_per_task = total_time / num_tasks
        assert avg_time_per_task < 0.5  # Average time per task should be reasonable