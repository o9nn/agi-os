import asyncio
import json
import tempfile
from pathlib import Path
def test_basic_configuration():
    print('🧪 Testing Basic Configuration...')
    try:
        from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig
        print('✅ DTESNConfig import successful')
        config = DTESNConfig(max_membrane_depth=4, esn_reservoir_size=512, bseries_max_order=8, enable_caching=True, cache_ttl_seconds=300)
        print('✅ DTESNConfig creation successful')
        assert config.max_membrane_depth == 4
        assert config.esn_reservoir_size == 512
        assert config.enable_caching is True
        print('✅ Configuration values correct')
        env_config = DTESNConfig.from_env()
        print('✅ Environment configuration loading works')
        return True
    except Exception as e:
        print(f'❌ Basic configuration test failed: {e}')
        return False
def test_configuration_validator():
    print('🧪 Testing Configuration Validator...')
    try:
        from aphrodite.endpoints.deep_tree_echo.dynamic_config_manager import ConfigurationValidator
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
        from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig
        from aphrodite.endpoints.deep_tree_echo.dynamic_config_manager import DynamicConfigurationManager, ConfigurationUpdateRequest, ConfigurationEnvironment
        with tempfile.TemporaryDirectory() as temp_dir:
            backup_dir = Path(temp_dir)
            initial_config = DTESNConfig(max_membrane_depth=4, esn_reservoir_size=512, bseries_max_order=8, enable_caching=True, cache_ttl_seconds=300)
            manager = DynamicConfigurationManager(initial_config=initial_config, max_snapshots=10, backup_directory=backup_dir, enable_auto_backup=True)
            print('✅ DynamicConfigurationManager created')
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
def test_api_routes():
    print('🧪 Testing API Routes...')
    try:
        from aphrodite.endpoints.deep_tree_echo.config_routes import config_router
        print('✅ Config router import successful')
        routes = [route.path for route in config_router.routes]
        expected_routes = ['/current', '/status', '/update', '/batch-update', '/validate', '/rollback', '/snapshots', '/environment', '/schema', '/health']
        for expected_route in expected_routes:
            full_path = f'/v1/config{expected_route}'
            found = any((full_path in route for route in routes))
            assert found, f'Expected route {full_path} not found in {routes}'
        print('✅ All expected API routes present')
        return True
    except Exception as e:
        print(f'❌ API routes test failed: {e}')
        return False
async def main():
    print('🌟 Dynamic Configuration Management - Core Functionality Tests')
    print('=' * 70)
    tests = [test_basic_configuration, test_configuration_validator, test_configuration_manager, test_api_routes]
    async_tests = [test_configuration_manager]
    results = []
    for test_func in tests:
        if test_func not in async_tests:
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
        print('✅ Dynamic configuration management is working correctly')
        return True
    else:
        print('⚠️  Some tests failed - check output above')
        return False
if __name__ == '__main__':
    success = asyncio.run(main())
    exit(0 if success else 1)