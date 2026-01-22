import asyncio
import pytest
import time
import torch
import torch.nn as nn
from unittest.mock import Mock, AsyncMock, patch, MagicMock
from typing import Dict, Any, List
from dataclasses import asdict
from aphrodite.engine.server_side_optimizer import ServerSideOptimizer, OptimizationConfig, ServerLoadMetrics, ModelCompiler, LoadBasedParameterTuner, ModelEnsembleManager
from aphrodite.engine.dynamic_model_manager import DynamicModelManager
from aphrodite.common.config import ModelConfig
class MockModel(nn.Module):
    def __init__(self, model_id: str='test_model'):
        super().__init__()
        self.model_id = model_id
        self.linear = nn.Linear(10, 5)
    def forward(self, x):
        return self.linear(x)
class MockEngineClient:
    def __init__(self):
        self.requests_processed = 0
        self.errors = 0
    async def process_request(self, request):
        self.requests_processed += 1
        if 'error' in str(request):
            self.errors += 1
            raise Exception('Mock processing error')
        return {'result': 'success', 'tokens': 100}
@pytest.fixture
def mock_engine_client():
    return MockEngineClient()
@pytest.fixture
def mock_model_config():
    return ModelConfig(model='test-model', tokenizer='test-tokenizer', tokenizer_mode='auto', trust_remote_code=False, download_dir=None, load_format='auto', dtype='auto', seed=0, revision=None, code_revision=None, rope_scaling=None, tokenizer_revision=None, max_model_len=None, quantization=None, enforce_eager=False, max_context_len_to_capture=None, max_seq_len_to_capture=8192, max_logprobs=20, disable_sliding_window=False, skip_tokenizer_init=False, served_model_name=None)
@pytest.fixture
def optimization_config():
    return OptimizationConfig(enable_torch_compile=True, compile_mode='default', enable_dynamic_tuning=True, tuning_interval_sec=1.0, load_threshold_high=0.8, load_threshold_low=0.3, enable_ensemble=True, max_ensemble_size=3, ensemble_strategy='weighted_voting', enable_performance_tracking=True, metrics_history_size=100)
@pytest.fixture
def server_load_metrics():
    return ServerLoadMetrics(timestamp=time.time(), cpu_usage_percent=65.0, memory_usage_percent=70.0, gpu_utilization=80.0, active_requests=25, queue_depth=5, throughput_tokens_per_sec=150.0, avg_latency_ms=120.0, error_rate=0.02)
class TestServerLoadMetrics:
    def test_overall_load_score_calculation(self, server_load_metrics):
        load_score = server_load_metrics.overall_load_score
        assert 0.0 <= load_score <= 1.0
        assert 0.6 < load_score < 0.9
    def test_high_load_score(self):
        high_load_metrics = ServerLoadMetrics(timestamp=time.time(), cpu_usage_percent=95.0, memory_usage_percent=90.0, gpu_utilization=98.0, active_requests=100, queue_depth=50, throughput_tokens_per_sec=50.0, avg_latency_ms=500.0, error_rate=0.1)
        assert high_load_metrics.overall_load_score > 0.8
    def test_low_load_score(self):
        low_load_metrics = ServerLoadMetrics(timestamp=time.time(), cpu_usage_percent=15.0, memory_usage_percent=20.0, gpu_utilization=25.0, active_requests=2, queue_depth=0, throughput_tokens_per_sec=200.0, avg_latency_ms=50.0, error_rate=0.001)
        assert low_load_metrics.overall_load_score < 0.4
class TestOptimizationConfig:
    def test_default_configuration(self):
        config = OptimizationConfig()
        assert config.enable_torch_compile is True
        assert config.compile_mode == 'default'
        assert config.enable_dynamic_tuning is True
        assert config.tuning_interval_sec == 30.0
        assert config.load_threshold_high == 0.8
        assert config.load_threshold_low == 0.3
        assert config.enable_ensemble is True
        assert config.max_ensemble_size == 3
    def test_custom_configuration(self):
        config = OptimizationConfig(enable_torch_compile=False, compile_mode='max-autotune', tuning_interval_sec=60.0, max_ensemble_size=5)
        assert config.enable_torch_compile is False
        assert config.compile_mode == 'max-autotune'
        assert config.tuning_interval_sec == 60.0
        assert config.max_ensemble_size == 5
class TestModelCompiler:
    def test_compilation_disabled(self, optimization_config):
        config = optimization_config
        config.enable_torch_compile = False
        compiler = ModelCompiler(config)
        model = MockModel()
        compiled_model = compiler.compile_model(model, 'test_model')
        assert compiled_model is model
        assert 'test_model' not in compiler.compiled_models
    @patch('torch.compile')
    def test_compilation_enabled(self, mock_torch_compile, optimization_config):
        mock_compiled = Mock()
        mock_torch_compile.return_value = mock_compiled
        compiler = ModelCompiler(optimization_config)
        model = MockModel()
        with patch.object(compiler, '_get_current_load_score', return_value=0.5):
            compiled_model = compiler.compile_model(model, 'test_model')
        mock_torch_compile.assert_called_once()
        assert compiled_model is mock_compiled
        assert 'test_model' in compiler.compiled_models
        assert 'test_model' in compiler.compilation_stats
    @patch('torch.compile')
    def test_compilation_error_handling(self, mock_torch_compile, optimization_config):
        mock_torch_compile.side_effect = Exception('Compilation failed')
        compiler = ModelCompiler(optimization_config)
        model = MockModel()
        compiled_model = compiler.compile_model(model, 'test_model')
        assert compiled_model is model
        assert 'test_model' not in compiler.compiled_models
    def test_get_compiled_model(self, optimization_config):
        compiler = ModelCompiler(optimization_config)
        assert compiler.get_compiled_model('nonexistent') is None
        mock_model = Mock()
        compiler.compiled_models['test_model'] = mock_model
        assert compiler.get_compiled_model('test_model') is mock_model
class TestLoadBasedParameterTuner:
    @pytest.fixture
    def mock_model_manager(self):
        return Mock(spec=DynamicModelManager)
    @pytest.fixture
    def parameter_tuner(self, optimization_config, mock_model_manager):
        return LoadBasedParameterTuner(optimization_config, mock_model_manager)
    @patch('psutil.cpu_percent')
    @patch('psutil.virtual_memory')
    def test_collect_load_metrics(self, mock_virtual_memory, mock_cpu_percent, parameter_tuner):
        mock_cpu_percent.return_value = 75.0
        mock_memory = Mock()
        mock_memory.percent = 60.0
        mock_virtual_memory.return_value = mock_memory
        with patch('aphrodite.engine.server_side_optimizer.metrics_collector') as mock_collector:
            mock_metrics = Mock()
            mock_metrics.active_requests = 10
            mock_metrics.throughput_rps = 2.0
            mock_metrics.avg_response_time_ms = 150.0
            mock_metrics.error_rate = 0.01
            mock_collector.get_current_metrics.return_value = mock_metrics
            metrics = parameter_tuner.collect_load_metrics()
        assert isinstance(metrics, ServerLoadMetrics)
        assert metrics.cpu_usage_percent == 75.0
        assert metrics.memory_usage_percent == 60.0
        assert metrics.active_requests == 10
        assert metrics.error_rate == 0.01
        assert len(parameter_tuner.load_history) == 1
        assert parameter_tuner.load_history[0] == metrics
    def test_determine_optimization_strategy_high_load(self, parameter_tuner):
        high_load_metrics = ServerLoadMetrics(timestamp=time.time(), cpu_usage_percent=95.0, memory_usage_percent=90.0, gpu_utilization=95.0, active_requests=100, queue_depth=20, throughput_tokens_per_sec=50.0, avg_latency_ms=300.0, error_rate=0.05)
        optimization = parameter_tuner.determine_optimization_strategy(high_load_metrics)
        assert optimization['strategy'] == 'performance'
        assert optimization['load_score'] > parameter_tuner.config.load_threshold_high
        params = optimization['parameters']
        assert params['max_tokens'] <= parameter_tuner.performance_params['max_tokens']
        assert params['top_k'] <= parameter_tuner.performance_params['top_k']
    def test_determine_optimization_strategy_low_load(self, parameter_tuner):
        low_load_metrics = ServerLoadMetrics(timestamp=time.time(), cpu_usage_percent=10.0, memory_usage_percent=15.0, gpu_utilization=20.0, active_requests=2, queue_depth=0, throughput_tokens_per_sec=200.0, avg_latency_ms=50.0, error_rate=0.001)
        optimization = parameter_tuner.determine_optimization_strategy(low_load_metrics)
        assert optimization['strategy'] == 'quality'
        assert optimization['load_score'] < parameter_tuner.config.load_threshold_low
        params = optimization['parameters']
        assert params['max_tokens'] == parameter_tuner.quality_params['max_tokens']
        assert params['top_k'] == parameter_tuner.quality_params['top_k']
    def test_determine_optimization_strategy_balanced_load(self, parameter_tuner):
        balanced_load_metrics = ServerLoadMetrics(timestamp=time.time(), cpu_usage_percent=50.0, memory_usage_percent=55.0, gpu_utilization=60.0, active_requests=15, queue_depth=2, throughput_tokens_per_sec=120.0, avg_latency_ms=100.0, error_rate=0.02)
        optimization = parameter_tuner.determine_optimization_strategy(balanced_load_metrics)
        assert optimization['strategy'] == 'balanced'
        assert parameter_tuner.config.load_threshold_low < optimization['load_score'] < parameter_tuner.config.load_threshold_high
        params = optimization['parameters']
        assert parameter_tuner.performance_params['max_tokens'] < params['max_tokens'] < parameter_tuner.quality_params['max_tokens']
    async def test_apply_parameter_adjustments(self, parameter_tuner):
        optimization = {'strategy': 'performance', 'parameters': {'max_tokens': 256, 'temperature': 0.7}, 'load_score': 0.85, 'timestamp': time.time()}
        result = await parameter_tuner.apply_parameter_adjustments(optimization)
        assert result is True
        assert parameter_tuner.parameter_adjustments == optimization
    def test_get_current_strategy(self, parameter_tuner):
        assert parameter_tuner.get_current_strategy() is None
        strategy = {'strategy': 'performance', 'parameters': {'max_tokens': 256}, 'load_score': 0.8, 'timestamp': time.time()}
        parameter_tuner.parameter_adjustments = strategy
        current = parameter_tuner.get_current_strategy()
        assert current == strategy
        assert current is not strategy
class TestModelEnsembleManager:
    @pytest.fixture
    def ensemble_manager(self, optimization_config):
        return ModelEnsembleManager(optimization_config)
    def test_add_model_to_ensemble(self, ensemble_manager):
        model1 = MockModel('model1')
        model2 = MockModel('model2')
        ensemble_manager.add_model_to_ensemble('model1', model1, 1.0, 1.0)
        ensemble_manager.add_model_to_ensemble('model2', model2, 0.8, 0.9)
        assert len(ensemble_manager.ensemble_models) == 2
        assert 'model1' in ensemble_manager.ensemble_weights
        assert 'model2' in ensemble_manager.ensemble_weights
        assert ensemble_manager.ensemble_weights['model1'] == 1.0
        assert ensemble_manager.ensemble_weights['model2'] == 0.8
    def test_ensemble_size_limit(self, ensemble_manager):
        for i in range(ensemble_manager.config.max_ensemble_size):
            model = MockModel(f'model{i}')
            ensemble_manager.add_model_to_ensemble(f'model{i}', model)
        assert len(ensemble_manager.ensemble_models) == ensemble_manager.config.max_ensemble_size
        extra_model = MockModel('extra_model')
        ensemble_manager.add_model_to_ensemble('extra_model', extra_model)
        assert len(ensemble_manager.ensemble_models) == ensemble_manager.config.max_ensemble_size
    def test_select_model_weighted_voting(self, ensemble_manager):
        ensemble_manager.config.ensemble_strategy = 'weighted_voting'
        model1 = MockModel('model1')
        model2 = MockModel('model2')
        ensemble_manager.add_model_to_ensemble('model1', model1, 1.0, 1.0)
        ensemble_manager.add_model_to_ensemble('model2', model2, 0.5, 0.8)
        selections = []
        for _ in range(100):
            selected = ensemble_manager.select_model_for_request({})
            selections.append(selected['model_id'])
        model1_count = selections.count('model1')
        model2_count = selections.count('model2')
        assert model1_count > model2_count
    def test_select_model_best_of_n(self, ensemble_manager):
        ensemble_manager.config.ensemble_strategy = 'best_of_n'
        model1 = MockModel('model1')
        model2 = MockModel('model2')
        ensemble_manager.add_model_to_ensemble('model1', model1)
        ensemble_manager.add_model_to_ensemble('model2', model2)
        ensemble_manager.ensemble_models[0]['request_count'] = 100
        ensemble_manager.ensemble_models[0]['total_latency'] = 5000
        ensemble_manager.ensemble_models[0]['error_count'] = 2
        ensemble_manager.ensemble_models[1]['request_count'] = 100
        ensemble_manager.ensemble_models[1]['total_latency'] = 10000
        ensemble_manager.ensemble_models[1]['error_count'] = 5
        selected = ensemble_manager.select_model_for_request({})
        assert selected['model_id'] == 'model1'
    def test_update_model_performance(self, ensemble_manager):
        model = MockModel('test_model')
        ensemble_manager.add_model_to_ensemble('test_model', model)
        ensemble_manager.update_model_performance('test_model', 150.0, True)
        ensemble_manager.update_model_performance('test_model', 200.0, False)
        model_info = ensemble_manager.ensemble_models[0]
        assert model_info['request_count'] == 2
        assert model_info['total_latency'] == 350.0
        assert model_info['error_count'] == 1
        assert 'test_model' in ensemble_manager.performance_history
        assert len(ensemble_manager.performance_history['test_model']) == 2
    def test_get_ensemble_status(self, ensemble_manager):
        status = ensemble_manager.get_ensemble_status()
        assert status['enabled'] == ensemble_manager.config.enable_ensemble
        assert status['model_count'] == 0
        assert status['models'] == []
        model = MockModel('test_model')
        ensemble_manager.add_model_to_ensemble('test_model', model, 1.0, 0.9)
        ensemble_manager.update_model_performance('test_model', 100.0, True)
        status = ensemble_manager.get_ensemble_status()
        assert status['model_count'] == 1
        assert len(status['models']) == 1
        model_status = status['models'][0]
        assert model_status['model_id'] == 'test_model'
        assert model_status['weight'] == 1.0
        assert model_status['request_count'] == 1
        assert model_status['avg_latency_ms'] == 100.0
        assert model_status['error_rate'] == 0.0
class TestServerSideOptimizer:
    @pytest.fixture
    async def optimizer(self, mock_engine_client, mock_model_config, optimization_config):
        with patch('aphrodite.engine.server_side_optimizer.DynamicModelManager') as mock_dm:
            mock_dm.return_value = Mock(spec=DynamicModelManager)
            mock_dm.return_value.create_initial_version = AsyncMock()
            mock_dm.return_value.get_status = Mock(return_value={'status': 'active'})
            optimizer = ServerSideOptimizer(mock_engine_client, mock_model_config, None, optimization_config)
            return optimizer
    @pytest.mark.asyncio
    async def test_optimizer_initialization(self, optimizer):
        assert optimizer.model_compiler is not None
        assert optimizer.parameter_tuner is not None
        assert optimizer.ensemble_manager is not None
        assert optimizer.model_manager is not None
        assert optimizer._is_running is False
    @pytest.mark.asyncio
    async def test_start_stop_optimization(self, optimizer):
        assert optimizer._is_running is False
        assert optimizer._optimization_task is None
        await optimizer.start_optimization()
        assert optimizer._is_running is True
        assert optimizer._optimization_task is not None
        await optimizer.stop_optimization()
        assert optimizer._is_running is False
    @pytest.mark.asyncio
    async def test_optimize_model_for_request(self, optimizer):
        model = MockModel('test_model')
        request_context = {'model_id': 'test_model', 'user_id': 'user123'}
        with patch.object(optimizer.model_compiler, 'compile_model') as mock_compile:
            mock_compiled = Mock()
            mock_compile.return_value = mock_compiled
            result = await optimizer.optimize_model_for_request(model, request_context)
        mock_compile.assert_called_once_with(model, 'test_model')
        assert result is mock_compiled
    def test_select_optimal_model_disabled(self, optimizer):
        optimizer.config.enable_ensemble = False
        result = optimizer.select_optimal_model({'model_id': 'test'})
        assert result is None
    def test_select_optimal_model_enabled(self, optimizer):
        test_model = MockModel('test_model')
        optimizer.ensemble_manager.add_model_to_ensemble('test_model', test_model)
        result = optimizer.select_optimal_model({'model_id': 'test_model'})
        assert result is test_model
    def test_record_request_performance(self, optimizer):
        test_model = MockModel('test_model')
        optimizer.ensemble_manager.add_model_to_ensemble('test_model', test_model)
        optimizer.record_request_performance('test_model', 150.0, True, {'tokens': 100, 'user': 'test'})
        model_info = optimizer.ensemble_manager.ensemble_models[0]
        assert model_info['request_count'] == 1
        assert model_info['total_latency'] == 150.0
        assert model_info['error_count'] == 0
        assert len(optimizer._performance_metrics) > 0
    def test_get_optimization_status(self, optimizer):
        status = optimizer.get_optimization_status()
        assert isinstance(status, dict)
        assert 'running' in status
        assert 'configuration' in status
        assert 'compilation_stats' in status
        assert 'ensemble_status' in status
        assert 'model_manager_status' in status
        config = status['configuration']
        assert 'torch_compile_enabled' in config
        assert 'dynamic_tuning_enabled' in config
        assert 'ensemble_enabled' in config
    def test_export_performance_report(self, optimizer):
        optimizer._performance_metrics = [{'timestamp': time.time(), 'model_id': 'test_model', 'latency_ms': 100.0, 'success': True}]
        with patch('builtins.open', mock_open()) as mock_file:
            report = optimizer.export_performance_report('/tmp/test_report.json')
        assert isinstance(report, dict)
        assert 'report_timestamp' in report
        assert 'optimization_config' in report
        assert 'status' in report
        assert 'performance_history' in report
        mock_file.assert_called_once()
class TestOptimizationIntegration:
    @pytest.fixture
    async def full_system(self, mock_engine_client, mock_model_config):
        config = OptimizationConfig(tuning_interval_sec=0.1, enable_torch_compile=False)
        with patch('aphrodite.engine.server_side_optimizer.DynamicModelManager') as mock_dm:
            mock_dm.return_value = Mock(spec=DynamicModelManager)
            mock_dm.return_value.create_initial_version = AsyncMock()
            mock_dm.return_value.get_status = Mock(return_value={'status': 'active'})
            optimizer = ServerSideOptimizer(mock_engine_client, mock_model_config, None, config)
            await optimizer.start_optimization()
            yield optimizer
            await optimizer.stop_optimization()
    @pytest.mark.asyncio
    async def test_end_to_end_optimization_cycle(self, full_system):
        optimizer = full_system
        model = MockModel('integration_test')
        request_context = {'model_id': 'integration_test', 'priority': 'high'}
        optimized_model = await optimizer.optimize_model_for_request(model, request_context)
        assert optimized_model is not None
        optimizer.record_request_performance('integration_test', 125.0, True)
        status = optimizer.get_optimization_status()
        assert status['running'] is True
        ensemble_status = optimizer.ensemble_manager.get_ensemble_status()
        assert ensemble_status['model_count'] >= 1
        await asyncio.sleep(0.2)
        assert len(optimizer._performance_metrics) > 0
    @pytest.mark.asyncio
    async def test_load_based_adaptation(self, full_system):
        optimizer = full_system
        high_load_metrics = ServerLoadMetrics(timestamp=time.time(), cpu_usage_percent=95.0, memory_usage_percent=90.0, gpu_utilization=95.0, active_requests=100, queue_depth=25, throughput_tokens_per_sec=30.0, avg_latency_ms=400.0, error_rate=0.08)
        with patch.object(optimizer.parameter_tuner, 'collect_load_metrics', return_value=high_load_metrics):
            optimization = optimizer.parameter_tuner.determine_optimization_strategy(high_load_metrics)
            await optimizer.parameter_tuner.apply_parameter_adjustments(optimization)
        current_strategy = optimizer.parameter_tuner.get_current_strategy()
        assert current_strategy['strategy'] == 'performance'
        assert current_strategy['load_score'] > 0.8
        low_load_metrics = ServerLoadMetrics(timestamp=time.time(), cpu_usage_percent=15.0, memory_usage_percent=20.0, gpu_utilization=25.0, active_requests=3, queue_depth=0, throughput_tokens_per_sec=180.0, avg_latency_ms=60.0, error_rate=0.005)
        with patch.object(optimizer.parameter_tuner, 'collect_load_metrics', return_value=low_load_metrics):
            optimization = optimizer.parameter_tuner.determine_optimization_strategy(low_load_metrics)
            await optimizer.parameter_tuner.apply_parameter_adjustments(optimization)
        current_strategy = optimizer.parameter_tuner.get_current_strategy()
        assert current_strategy['strategy'] == 'quality'
        assert current_strategy['load_score'] < 0.4
    @pytest.mark.asyncio
    async def test_ensemble_learning(self, full_system):
        optimizer = full_system
        fast_model = MockModel('fast_model')
        accurate_model = MockModel('accurate_model')
        optimizer.ensemble_manager.add_model_to_ensemble('fast_model', fast_model)
        optimizer.ensemble_manager.add_model_to_ensemble('accurate_model', accurate_model)
        for _ in range(50):
            optimizer.ensemble_manager.update_model_performance('fast_model', 75.0, True)
        for _ in range(5):
            optimizer.ensemble_manager.update_model_performance('fast_model', 80.0, False)
        for _ in range(48):
            optimizer.ensemble_manager.update_model_performance('accurate_model', 150.0, True)
        for _ in range(2):
            optimizer.ensemble_manager.update_model_performance('accurate_model', 160.0, False)
        status = optimizer.ensemble_manager.get_ensemble_status()
        models = {m['model_id']: m for m in status['models']}
        fast_stats = models['fast_model']
        accurate_stats = models['accurate_model']
        assert fast_stats['avg_latency_ms'] < accurate_stats['avg_latency_ms']
        assert fast_stats['error_rate'] > accurate_stats['error_rate']
        high_load_context = {'load_score': 0.9}
        with patch.object(optimizer.ensemble_manager, '_select_adaptive_model') as mock_adaptive:
            mock_adaptive.return_value = {'model_id': 'fast_model', 'model_instance': fast_model}
            optimizer.ensemble_manager.config.ensemble_strategy = 'adaptive'
            selected = optimizer.ensemble_manager.select_model_for_request(high_load_context)
            mock_adaptive.assert_called_once()
def mock_open():
    from unittest.mock import mock_open as _mock_open
    return _mock_open()
if __name__ == '__main__':
    pytest.main([__file__])