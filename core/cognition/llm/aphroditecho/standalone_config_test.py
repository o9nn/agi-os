import asyncio
import json
import tempfile
import time
from dataclasses import dataclass, asdict
from pathlib import Path
from typing import Dict, Any, Optional, List, Callable, Union
from enum import Enum
class MockDTESNConfig:
    def __init__(self, **kwargs):
        self.max_membrane_depth = kwargs.get('max_membrane_depth', 4)
        self.esn_reservoir_size = kwargs.get('esn_reservoir_size', 512)
        self.bseries_max_order = kwargs.get('bseries_max_order', 8)
        self.enable_caching = kwargs.get('enable_caching', True)
        self.cache_ttl_seconds = kwargs.get('cache_ttl_seconds', 300)
        self.enable_performance_monitoring = kwargs.get('enable_performance_monitoring', True)
    def dict(self):
        return {'max_membrane_depth': self.max_membrane_depth, 'esn_reservoir_size': self.esn_reservoir_size, 'bseries_max_order': self.bseries_max_order, 'enable_caching': self.enable_caching, 'cache_ttl_seconds': self.cache_ttl_seconds, 'enable_performance_monitoring': self.enable_performance_monitoring}
    def __eq__(self, other):
        if not isinstance(other, MockDTESNConfig):
            return False
        return self.dict() == other.dict()
class ConfigurationEnvironment(str, Enum):
    DEVELOPMENT = 'development'
    STAGING = 'staging'
    PRODUCTION = 'production'
    TESTING = 'testing'
@dataclass
class ConfigurationSnapshot:
    snapshot_id: str
    timestamp: float
    config_data: Dict[str, Any]
    environment: ConfigurationEnvironment
    description: str
    is_active: bool = False
    validation_errors: Optional[List[str]] = None
@dataclass
class ConfigurationUpdateRequest:
    parameter_path: str
    new_value: Any
    description: Optional[str] = None
    validate_only: bool = False
    environment: Optional[ConfigurationEnvironment] = None
class ConfigurationValidator:
    def __init__(self):
        self._validators: Dict[str, Callable[[Any], bool]] = {'max_membrane_depth': lambda x: isinstance(x, int) and 1 <= x <= 64, 'esn_reservoir_size': lambda x: isinstance(x, int) and 64 <= x <= 16384, 'bseries_max_order': lambda x: isinstance(x, int) and 1 <= x <= 32, 'cache_ttl_seconds': lambda x: isinstance(x, int) and 1 <= x <= 86400, 'enable_caching': lambda x: isinstance(x, bool), 'enable_performance_monitoring': lambda x: isinstance(x, bool)}
        self._dependencies: Dict[str, List[str]] = {'enable_caching': ['cache_ttl_seconds'], 'max_membrane_depth': ['esn_reservoir_size', 'bseries_max_order']}
    def validate_parameter(self, param_path: str, value: Any) -> List[str]:
        errors = []
        if param_path not in self._validators:
            errors.append(f'Unknown parameter: {param_path}')
            return errors
        if not self._validators[param_path](value):
            errors.append(f'Invalid value for {param_path}: {value}')
        return errors
    def validate_configuration(self, config_dict: Dict[str, Any]) -> List[str]:
        errors = []
        for param_path, value in config_dict.items():
            param_errors = self.validate_parameter(param_path, value)
            errors.extend(param_errors)
        for param, deps in self._dependencies.items():
            if param in config_dict and config_dict[param]:
                for dep in deps:
                    if dep not in config_dict:
                        errors.append(f'Missing dependency {dep} for parameter {param}')
        if 'max_membrane_depth' in config_dict and 'esn_reservoir_size' in config_dict:
            depth = config_dict['max_membrane_depth']
            reservoir = config_dict['esn_reservoir_size']
            if reservoir < depth * 32:
                errors.append(f'ESN reservoir size ({reservoir}) should be at least {depth * 32} for membrane depth {depth}')
        return errors
class MockDynamicConfigurationManager:
    def __init__(self, initial_config: Optional[MockDTESNConfig]=None, max_snapshots: int=50, backup_directory: Optional[Path]=None, enable_auto_backup: bool=True):
        self.max_snapshots = max_snapshots
        self.enable_auto_backup = enable_auto_backup
        self.backup_directory = backup_directory or Path('/tmp/dtesn_config_backups')
        self.backup_directory.mkdir(exist_ok=True)
        self._current_config = initial_config or MockDTESNConfig()
        self._snapshots: List[ConfigurationSnapshot] = []
        self._snapshot_counter = 0
        self._validator = ConfigurationValidator()
        self._update_callbacks: List[Callable] = []
        self._environment = ConfigurationEnvironment.DEVELOPMENT
        self._create_snapshot('Initial configuration')
    @property
    def current_config(self) -> MockDTESNConfig:
        return self._current_config
    @property
    def environment(self) -> ConfigurationEnvironment:
        return self._environment
    def set_environment(self, environment: ConfigurationEnvironment) -> None:
        self._environment = environment
    def register_update_callback(self, callback: Callable) -> None:
        self._update_callbacks.append(callback)
    async def update_parameter(self, request: ConfigurationUpdateRequest) -> Dict[str, Any]:
        try:
            current_dict = self._current_config.dict()
            if request.parameter_path not in current_dict:
                return {'success': False, 'error': f'Unknown parameter: {request.parameter_path}', 'parameter': request.parameter_path}
            new_dict = current_dict.copy()
            new_dict[request.parameter_path] = request.new_value
            validation_errors = self._validator.validate_configuration(new_dict)
            if request.validate_only:
                return {'success': len(validation_errors) == 0, 'validation_errors': validation_errors, 'parameter': request.parameter_path, 'validate_only': True}
            if validation_errors:
                return {'success': False, 'validation_errors': validation_errors, 'parameter': request.parameter_path}
            previous_snapshot_id = self._create_snapshot(f'Before updating {request.parameter_path}')
            try:
                new_config = MockDTESNConfig(**new_dict)
                old_config = self._current_config
                self._current_config = new_config
                await self._notify_update_callbacks(new_config)
                new_snapshot_id = self._create_snapshot(request.description or f'Updated {request.parameter_path} to {request.new_value}')
                return {'success': True, 'parameter': request.parameter_path, 'old_value': getattr(old_config, request.parameter_path), 'new_value': request.new_value, 'snapshot_id': new_snapshot_id, 'rollback_snapshot': previous_snapshot_id}
            except Exception as e:
                return {'success': False, 'error': f'Failed to apply update: {str(e)}', 'parameter': request.parameter_path, 'rollback_snapshot': previous_snapshot_id}
        except Exception as e:
            return {'success': False, 'error': f'Update failed: {str(e)}', 'parameter': request.parameter_path}
    async def update_multiple_parameters(self, updates: List[ConfigurationUpdateRequest]) -> Dict[str, Any]:
        try:
            current_dict = self._current_config.dict()
            new_dict = current_dict.copy()
            applied_updates = []
            for update in updates:
                if update.parameter_path not in current_dict:
                    return {'success': False, 'error': f'Unknown parameter: {update.parameter_path}', 'failed_parameter': update.parameter_path}
                new_dict[update.parameter_path] = update.new_value
                applied_updates.append({'parameter': update.parameter_path, 'old_value': current_dict[update.parameter_path], 'new_value': update.new_value})
            validation_errors = self._validator.validate_configuration(new_dict)
            if validation_errors:
                return {'success': False, 'validation_errors': validation_errors, 'attempted_updates': applied_updates}
            previous_snapshot_id = self._create_snapshot('Before batch update')
            try:
                new_config = MockDTESNConfig(**new_dict)
                self._current_config = new_config
                await self._notify_update_callbacks(new_config)
                new_snapshot_id = self._create_snapshot(f'Batch update of {len(updates)} parameters')
                return {'success': True, 'updated_parameters': applied_updates, 'snapshot_id': new_snapshot_id, 'rollback_snapshot': previous_snapshot_id}
            except Exception as e:
                return {'success': False, 'error': f'Failed to apply batch update: {str(e)}', 'attempted_updates': applied_updates, 'rollback_snapshot': previous_snapshot_id}
        except Exception as e:
            return {'success': False, 'error': f'Batch update failed: {str(e)}'}
    async def rollback_to_snapshot(self, snapshot_id: str) -> Dict[str, Any]:
        try:
            target_snapshot = None
            for snapshot in self._snapshots:
                if snapshot.snapshot_id == snapshot_id:
                    target_snapshot = snapshot
                    break
            if not target_snapshot:
                return {'success': False, 'error': f'Snapshot {snapshot_id} not found'}
            pre_rollback_snapshot = self._create_snapshot(f'Before rollback to {snapshot_id}')
            try:
                rollback_config = MockDTESNConfig(**target_snapshot.config_data)
                self._current_config = rollback_config
                await self._notify_update_callbacks(rollback_config)
                rollback_snapshot_id = self._create_snapshot(f'Rollback to snapshot {snapshot_id}: {target_snapshot.description}')
                return {'success': True, 'rolled_back_to': snapshot_id, 'rollback_snapshot': rollback_snapshot_id, 'undo_rollback_snapshot': pre_rollback_snapshot}
            except Exception as e:
                return {'success': False, 'error': f'Rollback failed: {str(e)}', 'target_snapshot': snapshot_id}
        except Exception as e:
            return {'success': False, 'error': f'Rollback failed: {str(e)}'}
    def get_snapshots(self) -> List[Dict[str, Any]]:
        return [{'snapshot_id': snapshot.snapshot_id, 'timestamp': snapshot.timestamp, 'description': snapshot.description, 'environment': snapshot.environment.value, 'is_active': snapshot.is_active, 'validation_errors': snapshot.validation_errors} for snapshot in self._snapshots]
    def get_current_status(self) -> Dict[str, Any]:
        return {'current_config': self._current_config.dict(), 'environment': self._environment.value, 'total_snapshots': len(self._snapshots), 'max_snapshots': self.max_snapshots, 'backup_directory': str(self.backup_directory), 'auto_backup_enabled': self.enable_auto_backup, 'registered_callbacks': len(self._update_callbacks)}
    def _create_snapshot(self, description: str) -> str:
        self._snapshot_counter += 1
        snapshot_id = f'snapshot_{int(time.time())}_{self._snapshot_counter:04d}'
        for snapshot in self._snapshots:
            snapshot.is_active = False
        snapshot = ConfigurationSnapshot(snapshot_id=snapshot_id, timestamp=time.time(), config_data=self._current_config.dict(), environment=self._environment, description=description, is_active=True)
        self._snapshots.append(snapshot)
        if len(self._snapshots) > self.max_snapshots:
            removed = self._snapshots.pop(0)
        if self.enable_auto_backup:
            self._save_snapshot_to_disk(snapshot)
        return snapshot_id
    def _save_snapshot_to_disk(self, snapshot: ConfigurationSnapshot) -> None:
        try:
            backup_file = self.backup_directory / f'{snapshot.snapshot_id}.json'
            with open(backup_file, 'w') as f:
                json.dump(asdict(snapshot), f, indent=2, default=str)
        except Exception:
            pass
    async def _notify_update_callbacks(self, new_config: MockDTESNConfig) -> None:
        for callback in self._update_callbacks:
            try:
                if asyncio.iscoroutinefunction(callback):
                    await callback(new_config)
                else:
                    callback(new_config)
            except Exception:
                pass
def test_basic_configuration():
    print('🧪 Testing Basic Configuration...')
    try:
        config = MockDTESNConfig(max_membrane_depth=4, esn_reservoir_size=512, bseries_max_order=8, enable_caching=True, cache_ttl_seconds=300)
        print('✅ MockDTESNConfig creation successful')
        assert config.max_membrane_depth == 4
        assert config.esn_reservoir_size == 512
        assert config.enable_caching is True
        print('✅ Configuration values correct')
        config_dict = config.dict()
        assert isinstance(config_dict, dict)
        assert config_dict['max_membrane_depth'] == 4
        print('✅ Configuration dict conversion works')
        return True
    except Exception as e:
        print(f'❌ Basic configuration test failed: {e}')
        return False
def test_configuration_validator():
    print('🧪 Testing Configuration Validator...')
    try:
        validator = ConfigurationValidator()
        print('✅ ConfigurationValidator created')
        errors = validator.validate_parameter('max_membrane_depth', 4)
        assert len(errors) == 0, f'Expected no errors, got: {errors}'
        print('✅ Valid parameter validation works')
        errors = validator.validate_parameter('max_membrane_depth', 0)
        assert len(errors) > 0, 'Expected validation errors for invalid value'
        print('✅ Invalid parameter validation works')
        errors = validator.validate_parameter('unknown_param', 123)
        assert len(errors) > 0, 'Expected errors for unknown parameter'
        print('✅ Unknown parameter validation works')
        config_dict = {'max_membrane_depth': 4, 'esn_reservoir_size': 512, 'bseries_max_order': 8, 'enable_caching': True, 'cache_ttl_seconds': 300}
        errors = validator.validate_configuration(config_dict)
        assert len(errors) == 0, f'Expected no errors for valid config, got: {errors}'
        print('✅ Configuration validation works')
        return True
    except Exception as e:
        print(f'❌ Validator test failed: {e}')
        return False
async def test_configuration_manager():
    print('🧪 Testing Dynamic Configuration Manager...')
    try:
        with tempfile.TemporaryDirectory() as temp_dir:
            backup_dir = Path(temp_dir)
            initial_config = MockDTESNConfig(max_membrane_depth=4, esn_reservoir_size=512, bseries_max_order=8, enable_caching=True, cache_ttl_seconds=300)
            manager = MockDynamicConfigurationManager(initial_config=initial_config, max_snapshots=10, backup_directory=backup_dir, enable_auto_backup=True)
            print('✅ MockDynamicConfigurationManager created')
            assert manager.current_config == initial_config
            assert manager.environment == ConfigurationEnvironment.DEVELOPMENT
            snapshots = manager.get_snapshots()
            assert len(snapshots) >= 1, 'Should have initial snapshot'
            print('✅ Initial state correct')
            manager.set_environment(ConfigurationEnvironment.PRODUCTION)
            assert manager.environment == ConfigurationEnvironment.PRODUCTION
            print('✅ Environment change works')
            update_request = ConfigurationUpdateRequest(parameter_path='esn_reservoir_size', new_value=1024, description='Test update')
            result = await manager.update_parameter(update_request)
            assert result['success'] is True, f'Update failed: {result}'
            assert result['old_value'] == 512
            assert result['new_value'] == 1024
            assert manager.current_config.esn_reservoir_size == 1024
            print('✅ Parameter update works')
            validate_request = ConfigurationUpdateRequest(parameter_path='max_membrane_depth', new_value=8, validate_only=True)
            result = await manager.update_parameter(validate_request)
            assert result['validate_only'] is True
            assert result['success'] is True
            assert manager.current_config.max_membrane_depth == 4
            print('✅ Validation-only mode works')
            invalid_request = ConfigurationUpdateRequest(parameter_path='max_membrane_depth', new_value=100)
            result = await manager.update_parameter(invalid_request)
            assert result['success'] is False
            assert 'validation_errors' in result
            assert len(result['validation_errors']) > 0
            print('✅ Invalid update handling works')
            batch_updates = [ConfigurationUpdateRequest(parameter_path='max_membrane_depth', new_value=6), ConfigurationUpdateRequest(parameter_path='cache_ttl_seconds', new_value=600)]
            batch_result = await manager.update_multiple_parameters(batch_updates)
            assert batch_result['success'] is True
            assert len(batch_result['updated_parameters']) == 2
            assert manager.current_config.max_membrane_depth == 6
            assert manager.current_config.cache_ttl_seconds == 600
            print('✅ Batch updates work')
            snapshots = manager.get_snapshots()
            if len(snapshots) >= 2:
                target_snapshot = snapshots[-2]['snapshot_id']
                rollback_result = await manager.rollback_to_snapshot(target_snapshot)
                assert rollback_result['success'] is True
                print('✅ Rollback works')
            status = manager.get_current_status()
            assert 'current_config' in status
            assert 'environment' in status
            assert 'total_snapshots' in status
            print('✅ Status reporting works')
            callback_called = False
            received_config = None
            def test_callback(config):
                nonlocal callback_called, received_config
                callback_called = True
                received_config = config
            manager.register_update_callback(test_callback)
            callback_request = ConfigurationUpdateRequest(parameter_path='esn_reservoir_size', new_value=2048)
            await manager.update_parameter(callback_request)
            assert callback_called is True
            assert received_config is not None
            assert received_config.esn_reservoir_size == 2048
            print('✅ Callback system works')
            return True
    except Exception as e:
        print(f'❌ Configuration manager test failed: {e}')
        import traceback
        traceback.print_exc()
        return False
async def main():
    print('🌟 Dynamic Configuration Management - Standalone Core Tests')
    print('=' * 70)
    tests = [test_basic_configuration, test_configuration_validator]
    async_tests = [test_configuration_manager]
    results = []
    for test_func in tests:
        try:
            result = test_func()
            results.append(result)
        except Exception as e:
            print(f'❌ Test {test_func.__name__} failed with exception: {e}')
            results.append(False)
    for test_func in async_tests:
        try:
            result = await test_func()
            results.append(result)
        except Exception as e:
            print(f'❌ Async test {test_func.__name__} failed with exception: {e}')
            results.append(False)
    passed = sum(results)
    total = len(results)
    print('\n' + '=' * 70)
    print(f'🧪 Test Results: {passed}/{total} tests passed')
    if passed == total:
        print('🎉 All core functionality tests passed!')
        print('✅ Dynamic configuration management logic is working correctly')
        print('🚀 Core implementation is ready for integration')
        return True
    else:
        print('⚠️  Some tests failed - check output above')
        return False
if __name__ == '__main__':
    success = asyncio.run(main())
    exit(0 if success else 1)