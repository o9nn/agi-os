import asyncio
import json
import tempfile
import time
from pathlib import Path
from unittest.mock import AsyncMock, MagicMock, patch
import pytest
try:
    from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig
    from aphrodite.endpoints.deep_tree_echo.dynamic_config_manager import DynamicConfigurationManager, ConfigurationUpdateRequest, ConfigurationEnvironment, ConfigurationValidator, ConfigurationSnapshot, get_dynamic_config_manager, initialize_dynamic_config_manager
    DTESN_CONFIG_AVAILABLE = True
except ImportError:
    DTESN_CONFIG_AVAILABLE = False
    pytestmark = pytest.mark.skip('DTESN configuration not available')
@pytest.fixture
def initial_config():
    return DTESNConfig(max_membrane_depth=4, esn_reservoir_size=512, bseries_max_order=8, enable_caching=True, cache_ttl_seconds=300, enable_performance_monitoring=True)
@pytest.fixture
def temp_backup_dir():
    with tempfile.TemporaryDirectory() as temp_dir:
        yield Path(temp_dir)
@pytest.fixture
def config_manager(initial_config, temp_backup_dir):
    manager = DynamicConfigurationManager(initial_config=initial_config, max_snapshots=10, backup_directory=temp_backup_dir, enable_auto_backup=True)
    yield manager
class TestConfigurationValidator:
    def test_validator_initialization(self):
        validator = ConfigurationValidator()
        assert len(validator._validators) > 0
        assert 'max_membrane_depth' in validator._validators
        assert 'esn_reservoir_size' in validator._validators
    def test_validate_valid_parameters(self):
        validator = ConfigurationValidator()
        assert validator.validate_parameter('max_membrane_depth', 4) == []
        assert validator.validate_parameter('esn_reservoir_size', 1024) == []
        assert validator.validate_parameter('enable_caching', True) == []
    def test_validate_invalid_parameters(self):
        validator = ConfigurationValidator()
        errors = validator.validate_parameter('max_membrane_depth', 0)
        assert len(errors) > 0
        errors = validator.validate_parameter('esn_reservoir_size', 32)
        assert len(errors) > 0
        errors = validator.validate_parameter('unknown_param', 123)
        assert len(errors) > 0
    def test_validate_configuration_dependencies(self):
        validator = ConfigurationValidator()
        config = {'max_membrane_depth': 8, 'esn_reservoir_size': 100}
        errors = validator.validate_configuration(config)
        assert len(errors) > 0
        config = {'max_membrane_depth': 4, 'esn_reservoir_size': 512}
        errors = validator.validate_configuration(config)
        assert len(errors) == 0
class TestDynamicConfigurationManager:
    def test_manager_initialization(self, initial_config, temp_backup_dir):
        manager = DynamicConfigurationManager(initial_config=initial_config, backup_directory=temp_backup_dir)
        assert manager.current_config == initial_config
        assert len(manager.get_snapshots()) == 1
        assert manager.environment == ConfigurationEnvironment.DEVELOPMENT
    def test_environment_management(self, config_manager):
        config_manager.set_environment(ConfigurationEnvironment.PRODUCTION)
        assert config_manager.environment == ConfigurationEnvironment.PRODUCTION
        status = config_manager.get_current_status()
        assert status['environment'] == 'production'
    @pytest.mark.asyncio
    async def test_single_parameter_update(self, config_manager):
        request = ConfigurationUpdateRequest(parameter_path='esn_reservoir_size', new_value=1024, description='Increase reservoir size')
        result = await config_manager.update_parameter(request)
        assert result['success'] is True
        assert result['old_value'] == 512
        assert result['new_value'] == 1024
        assert config_manager.current_config.esn_reservoir_size == 1024
    @pytest.mark.asyncio
    async def test_parameter_update_validation_failure(self, config_manager):
        request = ConfigurationUpdateRequest(parameter_path='max_membrane_depth', new_value=100, description='Invalid depth')
        result = await config_manager.update_parameter(request)
        assert result['success'] is False
        assert 'validation_errors' in result
        assert len(result['validation_errors']) > 0
        assert config_manager.current_config.max_membrane_depth == 4
    @pytest.mark.asyncio
    async def test_validation_only_mode(self, config_manager):
        request = ConfigurationUpdateRequest(parameter_path='esn_reservoir_size', new_value=2048, validate_only=True)
        result = await config_manager.update_parameter(request)
        assert result['validate_only'] is True
        assert result['success'] is True
        assert config_manager.current_config.esn_reservoir_size == 512
    @pytest.mark.asyncio
    async def test_batch_parameter_updates(self, config_manager):
        updates = [ConfigurationUpdateRequest(parameter_path='max_membrane_depth', new_value=6), ConfigurationUpdateRequest(parameter_path='esn_reservoir_size', new_value=1024), ConfigurationUpdateRequest(parameter_path='cache_ttl_seconds', new_value=600)]
        result = await config_manager.update_multiple_parameters(updates)
        assert result['success'] is True
        assert len(result['updated_parameters']) == 3
        config = config_manager.current_config
        assert config.max_membrane_depth == 6
        assert config.esn_reservoir_size == 1024
        assert config.cache_ttl_seconds == 600
    @pytest.mark.asyncio
    async def test_batch_update_validation_failure(self, config_manager):
        updates = [ConfigurationUpdateRequest(parameter_path='max_membrane_depth', new_value=6), ConfigurationUpdateRequest(parameter_path='esn_reservoir_size', new_value=50)]
        result = await config_manager.update_multiple_parameters(updates)
        assert result['success'] is False
        assert 'validation_errors' in result
        config = config_manager.current_config
        assert config.max_membrane_depth == 4
        assert config.esn_reservoir_size == 512
    @pytest.mark.asyncio
    async def test_configuration_rollback(self, config_manager):
        request = ConfigurationUpdateRequest(parameter_path='max_membrane_depth', new_value=8)
        update_result = await config_manager.update_parameter(request)
        assert update_result['success'] is True
        rollback_snapshot = update_result['rollback_snapshot']
        assert config_manager.current_config.max_membrane_depth == 8
        rollback_result = await config_manager.rollback_to_snapshot(rollback_snapshot)
        assert rollback_result['success'] is True
        assert config_manager.current_config.max_membrane_depth == 4
    def test_snapshot_management(self, config_manager):
        initial_snapshots = len(config_manager.get_snapshots())
        for i in range(5):
            config_manager._create_snapshot(f'Test snapshot {i}')
        snapshots = config_manager.get_snapshots()
        assert len(snapshots) == initial_snapshots + 5
        for snapshot in snapshots:
            assert 'snapshot_id' in snapshot
            assert 'timestamp' in snapshot
            assert 'description' in snapshot
    def test_callback_registration(self, config_manager):
        callback_called = False
        received_config = None
        def test_callback(config):
            nonlocal callback_called, received_config
            callback_called = True
            received_config = config
        config_manager.register_update_callback(test_callback)
        assert len(config_manager._update_callbacks) == 1
    @pytest.mark.asyncio
    async def test_async_callback_execution(self, config_manager):
        callback_called = False
        received_config = None
        async def async_callback(config):
            nonlocal callback_called, received_config
            callback_called = True
            received_config = config
        config_manager.register_update_callback(async_callback)
        request = ConfigurationUpdateRequest(parameter_path='esn_reservoir_size', new_value=1024)
        await config_manager.update_parameter(request)
        assert callback_called is True
        assert received_config is not None
        assert received_config.esn_reservoir_size == 1024
    def test_backup_functionality(self, config_manager):
        request = ConfigurationUpdateRequest(parameter_path='esn_reservoir_size', new_value=1024)
        asyncio.run(config_manager.update_parameter(request))
        backup_files = list(config_manager.backup_directory.glob('*.json'))
        assert len(backup_files) > 0
        with open(backup_files[0], 'r') as f:
            backup_data = json.load(f)
            assert 'snapshot_id' in backup_data
            assert 'config_data' in backup_data
@pytest.mark.skipif(not DTESN_CONFIG_AVAILABLE, reason='DTESN config not available')
class TestConfigurationIntegration:
    def test_global_manager_singleton(self):
        manager1 = get_dynamic_config_manager()
        manager2 = get_dynamic_config_manager()
        assert manager1 is manager2
    def test_manager_initialization_with_config(self, initial_config):
        manager = initialize_dynamic_config_manager(initial_config=initial_config, max_snapshots=20)
        assert manager.current_config == initial_config
        assert manager.max_snapshots == 20
    @pytest.mark.asyncio
    async def test_dtesn_processor_integration(self, initial_config):
        try:
            from aphrodite.endpoints.deep_tree_echo.dtesn_processor import DTESNProcessor
            processor = DTESNProcessor(config=initial_config, enable_dynamic_config=True, max_concurrent_processes=10)
            assert hasattr(processor, 'config_manager')
        except ImportError:
            pytest.skip('DTESNProcessor not available')
@pytest.mark.skipif(not DTESN_CONFIG_AVAILABLE, reason='DTESN config not available')
class TestConfigurationAPI:
    @pytest.fixture
    def mock_app(self):
        from fastapi.testclient import TestClient
        from fastapi import FastAPI
        try:
            from aphrodite.endpoints.deep_tree_echo.config_routes import config_router
            app = FastAPI()
            app.include_router(config_router)
            return TestClient(app)
        except ImportError:
            pytest.skip('FastAPI routes not available')
    def test_get_current_configuration(self, mock_app):
        response = mock_app.get('/v1/config/current')
        assert response.status_code == 200
        data = response.json()
        assert data['success'] is True
        assert 'configuration' in data
        assert 'environment' in data
    def test_get_configuration_status(self, mock_app):
        response = mock_app.get('/v1/config/status')
        assert response.status_code == 200
        data = response.json()
        assert data['success'] is True
        assert 'status' in data
    def test_update_configuration_parameter(self, mock_app):
        update_data = {'parameter': 'esn_reservoir_size', 'value': 1024, 'description': 'Test update', 'validate_only': False}
        response = mock_app.post('/v1/config/update', json=update_data)
        assert response.status_code == 200
        data = response.json()
        assert data['success'] is True
    def test_validate_configuration_parameter(self, mock_app):
        validate_data = {'parameter': 'max_membrane_depth', 'value': 100}
        response = mock_app.post('/v1/config/validate', json=validate_data)
        assert response.status_code == 200
        data = response.json()
        assert data['success'] is False
        assert data['errors'] is not None
    def test_get_configuration_snapshots(self, mock_app):
        response = mock_app.get('/v1/config/snapshots')
        assert response.status_code == 200
        data = response.json()
        assert data['success'] is True
        assert 'snapshots' in data
        assert 'total_count' in data
    def test_configuration_health_check(self, mock_app):
        response = mock_app.get('/v1/config/health')
        assert response.status_code == 200
        data = response.json()
        assert data['success'] is True
        assert 'health' in data
if __name__ == '__main__':
    pytest.main([__file__, '-v'])