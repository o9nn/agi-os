import pytest
import asyncio
import time
from unittest.mock import AsyncMock, MagicMock
from aphrodite.endpoints.deep_tree_echo.dtesn_processor import DTESNProcessor
from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig
class TestEngineIntegration:
    @pytest.fixture
    def mock_async_aphrodite(self):
        engine = AsyncMock()
        engine.get_model_config.return_value = MagicMock(model='meta-llama/Meta-Llama-3.1-8B-Instruct', max_model_len=4096, vocab_size=32000, tokenizer='meta-llama/Meta-Llama-3.1-8B-Instruct')
        engine.get_aphrodite_config.return_value = MagicMock(model_config=MagicMock(model='meta-llama/Meta-Llama-3.1-8B-Instruct'), parallel_config=MagicMock(tensor_parallel_size=1, pipeline_parallel_size=1), scheduler_config=MagicMock(max_num_seqs=256, max_model_len=4096), cache_config=MagicMock(block_size=16, gpu_memory_utilization=0.9))
        engine.generate.return_value = AsyncMock()
        engine.encode.return_value = AsyncMock()
        return engine
    @pytest.fixture
    def dtesn_config(self):
        return DTESNConfig(max_membrane_depth=4, esn_reservoir_size=256, bseries_max_order=8, enable_caching=True)
    @pytest.fixture
    async def dtesn_processor(self, mock_async_aphrodite, dtesn_config):
        processor = DTESNProcessor(config=dtesn_config, engine=mock_async_aphrodite, max_concurrent_processes=5)
        await processor._initialize_engine_integration()
        return processor
    @pytest.mark.asyncio
    async def test_engine_initialization_integration(self, mock_async_aphrodite, dtesn_config):
        processor = DTESNProcessor(config=dtesn_config, engine=mock_async_aphrodite, max_concurrent_processes=10)
        assert processor.engine is mock_async_aphrodite
        await processor._initialize_engine_integration()
        assert hasattr(processor, 'engine_config')
        assert hasattr(processor, 'model_config')
        mock_async_aphrodite.get_aphrodite_config.assert_called_once()
        mock_async_aphrodite.get_model_config.assert_called_once()
    @pytest.mark.asyncio
    async def test_server_side_model_loading_integration(self, dtesn_processor):
        model_config = dtesn_processor.model_config
        assert model_config is not None
        assert hasattr(model_config, 'model')
        assert hasattr(model_config, 'max_model_len')
        engine_config = dtesn_processor.engine_config
        assert engine_config is not None
        assert hasattr(engine_config, 'model_config')
        assert hasattr(engine_config, 'parallel_config')
        model_name = getattr(model_config, 'model', 'unknown')
        assert model_name != 'unknown'
    @pytest.mark.asyncio
    async def test_backend_processing_pipeline_integration(self, dtesn_processor):
        test_input = 'test input for backend processing pipeline'
        result = await dtesn_processor.process(input_data=test_input, membrane_depth=3, esn_size=128, enable_concurrent=True)
        assert result is not None
        assert hasattr(result, 'output')
        assert hasattr(result, 'processing_time_ms')
        assert hasattr(result, 'membrane_layers')
        assert result.membrane_layers == 3
        assert result.processing_time_ms >= 0
        assert isinstance(result.output, str)
    @pytest.mark.asyncio
    async def test_engine_context_fetching(self, dtesn_processor):
        engine_context = await dtesn_processor._fetch_comprehensive_engine_context()
        assert 'model_config' in engine_context
        assert 'engine_config' in engine_context
        assert 'processing_capabilities' in engine_context
        model_config = engine_context['model_config']
        assert 'model_name' in model_config
        assert 'max_model_length' in model_config
        capabilities = engine_context['processing_capabilities']
        assert 'supports_batching' in capabilities
        assert 'supports_streaming' in capabilities
    @pytest.mark.asyncio
    async def test_concurrent_engine_processing(self, dtesn_processor):
        tasks = []
        for i in range(5):
            task = dtesn_processor.process(input_data=f'concurrent test input {i}', membrane_depth=2, esn_size=64, enable_concurrent=True)
            tasks.append(task)
        start_time = time.time()
        results = await asyncio.gather(*tasks)
        total_time = time.time() - start_time
        assert len(results) == 5
        for i, result in enumerate(results):
            assert result is not None
            assert result.membrane_layers == 2
            assert f'concurrent test input {i}' in result.output or result.output != ''
        assert total_time < 5.0
    @pytest.mark.asyncio
    async def test_engine_state_synchronization(self, dtesn_processor):
        await dtesn_processor._sync_with_engine_state()
        result = await dtesn_processor.process(input_data='state sync test', membrane_depth=2, esn_size=64)
        assert result is not None
        assert result.processing_time_ms >= 0
    @pytest.mark.asyncio
    async def test_engine_configuration_serialization(self, dtesn_processor):
        model_config_serialized = dtesn_processor._serialize_config(dtesn_processor.model_config)
        assert isinstance(model_config_serialized, dict)
        engine_config_serialized = dtesn_processor._serialize_config(dtesn_processor.engine_config)
        assert isinstance(engine_config_serialized, dict)
        config_str = str(model_config_serialized)
        sensitive_terms = ['password', 'secret', 'token', 'key', 'auth']
        for term in sensitive_terms:
            assert term not in config_str.lower()
    @pytest.mark.asyncio
    async def test_engine_error_handling(self, mock_async_aphrodite, dtesn_config):
        mock_async_aphrodite.get_model_config.side_effect = Exception('Engine connection failed')
        processor = DTESNProcessor(config=dtesn_config, engine=mock_async_aphrodite)
        try:
            await processor._initialize_engine_integration()
        except Exception as e:
            assert 'Engine connection failed' in str(e)
        assert processor.config is not None
    @pytest.mark.asyncio
    async def test_optimal_configuration_calculation(self, dtesn_processor):
        optimal_depth = dtesn_processor._get_optimal_membrane_depth()
        assert isinstance(optimal_depth, int)
        assert 1 <= optimal_depth <= dtesn_processor.config.max_membrane_depth
        optimal_esn = dtesn_processor._get_optimal_esn_size()
        assert isinstance(optimal_esn, int)
        assert 32 <= optimal_esn <= dtesn_processor.config.esn_reservoir_size
    @pytest.mark.asyncio
    async def test_engine_integration_metrics(self, dtesn_processor):
        await dtesn_processor.process(input_data='metrics test', membrane_depth=3, esn_size=128)
        context = await dtesn_processor._fetch_comprehensive_engine_context()
        assert 'processing_capabilities' in context
        assert 'integration_status' in context
        capabilities = context['processing_capabilities']
        assert 'max_concurrent_requests' in capabilities
        assert 'memory_utilization' in capabilities
    @pytest.mark.asyncio
    async def test_engine_aware_pipeline_setup(self, dtesn_processor):
        await dtesn_processor._setup_engine_aware_pipelines()
        assert hasattr(dtesn_processor, 'engine_pipeline_config')
        result = await dtesn_processor.process(input_data='engine pipeline test', membrane_depth=2, esn_size=64)
        assert result is not None
        assert hasattr(result, 'engine_integration_metadata')
    def test_engine_integration_without_engine(self, dtesn_config):
        processor = DTESNProcessor(config=dtesn_config, engine=None)
        assert processor.engine is None
        assert processor.config is dtesn_config
        assert processor._get_optimal_membrane_depth() > 0
        assert processor._get_optimal_esn_size() > 0
class TestEngineIntegrationPerformance:
    @pytest.fixture
    def performance_config(self):
        return DTESNConfig(max_membrane_depth=8, esn_reservoir_size=512, bseries_max_order=16, enable_caching=True, enable_performance_monitoring=True)
    @pytest.mark.asyncio
    async def test_engine_initialization_performance(self, mock_async_aphrodite, performance_config):
        start_time = time.time()
        processor = DTESNProcessor(config=performance_config, engine=mock_async_aphrodite, max_concurrent_processes=20)
        await processor._initialize_engine_integration()
        initialization_time = time.time() - start_time
        assert initialization_time < 1.0
    @pytest.mark.asyncio
    async def test_high_concurrency_engine_processing(self, mock_async_aphrodite, performance_config):
        processor = DTESNProcessor(config=performance_config, engine=mock_async_aphrodite, max_concurrent_processes=50)
        await processor._initialize_engine_integration()
        num_tasks = 20
        tasks = [processor.process(input_data=f'high concurrency test {i}', membrane_depth=4, esn_size=256, enable_concurrent=True) for i in range(num_tasks)]
        start_time = time.time()
        results = await asyncio.gather(*tasks)
        total_time = time.time() - start_time
        assert len(results) == num_tasks
        assert all((r is not None for r in results))
        avg_time_per_task = total_time / num_tasks
        assert avg_time_per_task < 0.5