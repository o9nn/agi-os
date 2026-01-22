import sys
import logging
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))
from echo_component_base import MemoryEchoComponent, ProcessingEchoComponent, EchoConfig, create_echo_component, validate_echo_component, get_echo_component_info
def test_echo_config():
    print('🧪 Testing EchoConfig...')
    config = EchoConfig(component_name='test_component', version='1.0.0', echo_threshold=0.8, debug_mode=True)
    assert config.component_name == 'test_component'
    assert config.version == '1.0.0'
    assert config.echo_threshold == 0.8
    assert config.debug_mode is True
    print('  ✅ EchoConfig tests passed')
def test_basic_echo_component():
    print('🧪 Testing Basic EchoComponent...')
    config = EchoConfig(component_name='basic_test', debug_mode=False)
    component = create_echo_component('basic', config)
    assert validate_echo_component(component)
    assert component.config.component_name == 'basic_test'
    init_response = component.initialize()
    assert init_response.success
    assert component._initialized
    test_data = {'test': 'data'}
    process_response = component.process(test_data)
    assert process_response.success
    assert process_response.data == test_data
    echo_response = component.echo(test_data, echo_value=0.75)
    assert echo_response.success
    assert echo_response.data == test_data
    status_response = component.get_status()
    assert status_response.success
    assert status_response.data['initialized'] is True
    reset_response = component.reset()
    assert reset_response.success
    assert component._initialized is False
    print('  ✅ Basic EchoComponent tests passed')
def test_memory_echo_component():
    print('🧪 Testing MemoryEchoComponent...')
    config = EchoConfig(component_name='memory_test', debug_mode=False)
    component = create_echo_component('memory', config)
    assert isinstance(component, MemoryEchoComponent)
    assert validate_echo_component(component)
    store_response = component.store_memory('key1', {'data': 'test'})
    assert store_response.success
    assert component.memory_stats['size'] == 1
    retrieve_response = component.retrieve_memory('key1')
    assert retrieve_response.success
    assert retrieve_response.data == {'data': 'test'}
    missing_response = component.retrieve_memory('missing_key')
    assert not missing_response.success
    clear_response = component.clear_memory()
    assert clear_response.success
    assert component.memory_stats['size'] == 0
    print('  ✅ MemoryEchoComponent tests passed')
def test_processing_echo_component():
    print('🧪 Testing ProcessingEchoComponent...')
    config = EchoConfig(component_name='processing_test', debug_mode=False)
    component = create_echo_component('processing', config)
    assert isinstance(component, ProcessingEchoComponent)
    assert validate_echo_component(component)
    def add_one(x):
        return x + 1
    def multiply_two(x):
        return x * 2
    component.add_processing_step(add_one, 'add_one')
    component.add_processing_step(multiply_two, 'multiply_two')
    assert len(component.processing_pipeline) == 2
    pipeline_response = component.execute_pipeline(5)
    assert pipeline_response.success
    assert pipeline_response.data == 12
    assert component.processing_stats['successful_operations'] == 1
    print('  ✅ ProcessingEchoComponent tests passed')
def test_component_info():
    print('🧪 Testing component info functionality...')
    config = EchoConfig(component_name='info_test', version='2.0.0')
    component = create_echo_component('memory', config)
    component.initialize()
    component.store_memory('test', 'data')
    info = get_echo_component_info(component)
    assert info['component_name'] == 'info_test'
    assert info['version'] == '2.0.0'
    assert info['type'] == 'MemoryEchoComponent'
    assert info['initialized'] is True
    assert info['has_memory'] is True
    assert info['has_processing'] is False
    assert 'memory_stats' in info
    print('  ✅ Component info tests passed')
def test_error_handling():
    print('🧪 Testing error handling...')
    config = EchoConfig(component_name='error_test', debug_mode=False)
    component = create_echo_component('basic', config)
    validation_response = component.validate_input(None)
    assert not validation_response.success
    assert 'cannot be None' in validation_response.message
    valid_response = component.validate_input('valid data')
    assert valid_response.success
    test_error = ValueError('Test error')
    error_response = component.handle_error(test_error, 'test context')
    assert not error_response.success
    assert 'Test error' in error_response.message
    assert error_response.metadata['error_type'] == 'ValueError'
    print('  ✅ Error handling tests passed')
def run_all_tests():
    print('🚀 Starting Echo Component Base Class Tests')
    print('=' * 50)
    try:
        test_echo_config()
        test_basic_echo_component()
        test_memory_echo_component()
        test_processing_echo_component()
        test_component_info()
        test_error_handling()
        print('\n' + '=' * 50)
        print('✅ All tests passed! Echo Component Base Classes are working correctly.')
        print('\n🎯 Ready for integration with existing Echo components:')
        print('  - deep_tree_echo.py can inherit from EchoComponent')
        print('  - echo9ml.py can use ProcessingEchoComponent')
        print('  - Memory components can use MemoryEchoComponent')
        print('  - All components will have standardized interfaces')
        return True
    except Exception as e:
        print(f'\n❌ Test failed: {e}')
        import traceback
        traceback.print_exc()
        return False
if __name__ == '__main__':
    logging.getLogger().setLevel(logging.WARNING)
    success = run_all_tests()
    sys.exit(0 if success else 1)