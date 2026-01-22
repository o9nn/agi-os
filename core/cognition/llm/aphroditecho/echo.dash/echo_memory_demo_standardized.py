from echo_component_base import MemoryEchoComponent, EchoConfig, EchoResponse
from typing import Any, Dict
import time
from datetime import datetime
class EchoMemoryDemoStandardized(MemoryEchoComponent):
    def __init__(self, config: EchoConfig):
        super().__init__(config)
        self.demo_data = {}
        self.operation_count = 0
        self.last_operation_time = None
    def initialize(self) -> EchoResponse:
        try:
            parent_result = super().initialize()
            if not parent_result.success:
                return parent_result
            self.demo_data = {'created_at': datetime.now().isoformat(), 'initialized': True, 'demo_memories': {}}
            self.logger.info(f"Echo Memory Demo '{self.config.component_name}' initialized")
            return EchoResponse(success=True, message='Echo Memory Demo initialized successfully', metadata={'component_name': self.config.component_name, 'version': self.config.version, 'echo_threshold': self.config.echo_threshold})
        except Exception as e:
            return self.handle_error(e, 'initialize')
    def process(self, input_data: Any, **kwargs) -> EchoResponse:
        try:
            validation = self.validate_input(input_data)
            if not validation.success:
                return validation
            self.operation_count += 1
            self.last_operation_time = datetime.now()
            if isinstance(input_data, dict):
                action = input_data.get('action', 'demo')
                if action == 'store':
                    return self._handle_store_operation(input_data)
                elif action == 'retrieve':
                    return self._handle_retrieve_operation(input_data)
                elif action == 'list':
                    return self._handle_list_operation()
                elif action == 'demo':
                    return self._handle_demo_operation(input_data)
                else:
                    return EchoResponse(success=False, message=f'Unknown action: {action}')
            else:
                return self._handle_demo_operation({'demo_type': 'basic'})
        except Exception as e:
            return self.handle_error(e, 'process')
    def echo(self, data: Any, echo_value: float=0.0) -> EchoResponse:
        try:
            echo_data = {'component_name': self.config.component_name, 'echo_value': echo_value, 'memory_state': {'total_memories': len(self.memory_store), 'demo_memories': len(self.demo_data.get('demo_memories', {})), 'operation_count': self.operation_count, 'last_operation': self.last_operation_time.isoformat() if self.last_operation_time else None}, 'input_echo': data, 'timestamp': datetime.now().isoformat(), 'memory_summary': list(self.memory_store.keys())[:5]}
            return EchoResponse(success=True, data=echo_data, message=f'Echo from {self.config.component_name} (value: {echo_value})', metadata={'echo_value': echo_value, 'memory_count': len(self.memory_store)})
        except Exception as e:
            return self.handle_error(e, 'echo')
    def _handle_store_operation(self, input_data: Dict) -> EchoResponse:
        key = input_data.get('key')
        data = input_data.get('data')
        if not key:
            return EchoResponse(success=False, message="Store operation requires 'key'")
        self.demo_data['demo_memories'][key] = {'data': data, 'stored_at': datetime.now().isoformat(), 'operation_id': self.operation_count}
        self.store_memory(key, data)
        return EchoResponse(success=True, data={'key': key, 'stored': True}, message=f'Successfully stored memory with key: {key}', metadata={'key': key, 'operation_count': self.operation_count})
    def _handle_retrieve_operation(self, input_data: Dict) -> EchoResponse:
        key = input_data.get('key')
        if not key:
            return EchoResponse(success=False, message="Retrieve operation requires 'key'")
        retrieve_result = self.retrieve_memory(key)
        if retrieve_result.success:
            demo_meta = self.demo_data['demo_memories'].get(key, {})
            return EchoResponse(success=True, data={'key': key, 'data': retrieve_result.data, 'demo_metadata': demo_meta}, message=f'Successfully retrieved memory with key: {key}', metadata={'key': key, 'found': True})
        else:
            return EchoResponse(success=False, message=f'Memory not found for key: {key}', metadata={'key': key, 'found': False})
    def _handle_list_operation(self) -> EchoResponse:
        return EchoResponse(success=True, data={'total_memories': len(self.memory_store), 'memory_keys': list(self.memory_store.keys()), 'demo_memories': list(self.demo_data['demo_memories'].keys()), 'operation_count': self.operation_count}, message=f'Listed {len(self.memory_store)} memories', metadata={'memory_count': len(self.memory_store)})
    def _handle_demo_operation(self, input_data: Dict) -> EchoResponse:
        demo_type = input_data.get('demo_type', 'basic')
        if demo_type == 'basic':
            demo_key = f'demo_memory_{self.operation_count}'
            demo_data = {'message': 'Hello from Echo Memory Demo!', 'timestamp': datetime.now().isoformat(), 'demo_type': 'basic', 'random_value': time.time() % 100}
            self.store_memory(demo_key, demo_data)
            self.demo_data['demo_memories'][demo_key] = {'data': demo_data, 'stored_at': datetime.now().isoformat(), 'operation_id': self.operation_count}
            return EchoResponse(success=True, data={'demo_type': demo_type, 'demo_key': demo_key, 'demo_data': demo_data, 'total_memories': len(self.memory_store)}, message=f'Executed {demo_type} demo successfully', metadata={'demo_type': demo_type, 'operation_count': self.operation_count})
        elif demo_type == 'performance':
            start_time = time.time()
            for i in range(5):
                key = f'perf_test_{self.operation_count}_{i}'
                data = {'test_data': i, 'timestamp': time.time()}
                self.store_memory(key, data)
            end_time = time.time()
            duration = end_time - start_time
            return EchoResponse(success=True, data={'demo_type': demo_type, 'operations_performed': 5, 'duration_seconds': duration, 'operations_per_second': 5 / duration if duration > 0 else 0, 'total_memories': len(self.memory_store)}, message=f'Executed {demo_type} demo in {duration:.3f} seconds', metadata={'demo_type': demo_type, 'duration': duration})
        else:
            return EchoResponse(success=False, message=f'Unknown demo type: {demo_type}')
def create_memory_demo_system() -> EchoMemoryDemoStandardized:
    config = EchoConfig(component_name='EchoMemoryDemo', version='1.0.0', echo_threshold=0.75, debug_mode=True, custom_params={'demo_mode': True})
    demo = EchoMemoryDemoStandardized(config)
    result = demo.initialize()
    if not result.success:
        raise RuntimeError(f'Failed to initialize demo: {result.message}')
    return demo
def demonstrate_api_consistency():
    print('🔧 Echo API Standardization Demonstration')
    print('=' * 60)
    demo = create_memory_demo_system()
    print('✅ Component initialized with standardized interface')
    print(f'   Component: {demo.config.component_name} v{demo.config.version}')
    print(f'   Echo threshold: {demo.config.echo_threshold}')
    print('\n📝 Demonstrating standardized process() method:')
    store_result = demo.process({'action': 'store', 'key': 'api_demo', 'data': {'message': 'This shows standardized storage', 'priority': 'high'}})
    print(f'   Store: {store_result.success} - {store_result.message}')
    retrieve_result = demo.process({'action': 'retrieve', 'key': 'api_demo'})
    print(f'   Retrieve: {retrieve_result.success} - {retrieve_result.message}')
    list_result = demo.process({'action': 'list'})
    print(f"   List: {list_result.success} - Found {list_result.data['total_memories']} memories")
    print('\n🔊 Demonstrating standardized echo() method:')
    echo_result = demo.echo({'test': 'echo data'}, echo_value=0.8)
    if echo_result.success:
        echo_data = echo_result.data
        print(f"   Echo value: {echo_data['echo_value']}")
        print(f"   Memory state: {echo_data['memory_state']['total_memories']} memories")
        print(f"   Operations: {echo_data['memory_state']['operation_count']}")
    print('\n⚡ Demonstrating performance demo:')
    perf_result = demo.process({'action': 'demo', 'demo_type': 'performance'})
    if perf_result.success:
        perf_data = perf_result.data
        print(f"   Performed {perf_data['operations_performed']} operations")
        print(f"   Duration: {perf_data['duration_seconds']:.3f} seconds")
        print(f"   Rate: {perf_data['operations_per_second']:.1f} ops/sec")
    print('\n🎯 API Consistency Benefits:')
    print('   ✅ Standardized EchoResponse format')
    print('   ✅ Consistent error handling')
    print('   ✅ Unified logging approach')
    print('   ✅ Standard configuration with EchoConfig')
    print('   ✅ Memory component inheritance')
    print('   ✅ Processing pipeline support')
    return demo
if __name__ == '__main__':
    try:
        demo = demonstrate_api_consistency()
        print('\n✅ Standardization demonstration completed successfully!')
    except Exception as e:
        print(f'\n❌ Demo failed: {e}')
        import traceback
        traceback.print_exc()