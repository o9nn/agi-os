import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from deep_tree_echo import DeepTreeEcho
def demonstrate_membrane_system():
    print('🎪 P-System Membranes Demonstration')
    print('=' * 50)
    print('\n1. Initializing Deep Tree Echo with P-System Membranes...')
    echo = DeepTreeEcho()
    print('   ✓ System initialized with membrane boundaries')
    print('\n2. Membrane Status:')
    status = echo.get_membrane_status()
    for membrane_name, membrane_status in status.items():
        print(f"   🧬 {membrane_name}: {membrane_status['state']}, Queue: {membrane_status['message_queue_size']}, Children: {membrane_status['child_count']}")
    print('\n3. Testing Inter-Membrane Communication...')
    print('   📤 Cognitive → Extension: Sending processing request...')
    success = echo.send_membrane_message('cognitive', 'extension', 'process_thought', {'thought': 'Implement neural-symbolic integration', 'priority': 'high'})
    print(f'   ✓ Message sent: {success}')
    print('   📤 Extension → Security: Requesting validation...')
    success = echo.send_membrane_message('extension', 'security', 'security_check', {'operation': 'neural_integration', 'trust_level': 'medium'})
    print(f'   ✓ Security check requested: {success}')
    print('   📤 Security → Cognitive: Validation result...')
    success = echo.send_membrane_message('security', 'cognitive', 'validation_result', {'status': 'approved', 'restrictions': ['memory_limit=100MB']})
    print(f'   ✓ Validation sent: {success}')
    print('\n4. Processing Messages...')
    results = echo.process_membrane_messages()
    for membrane_name, membrane_results in results.items():
        if membrane_results:
            print(f'   🔄 {membrane_name}: Processed {len(membrane_results)} messages')
            for result in membrane_results:
                print(f"      • {result.get('status', 'processed')}")
    print('\n5. Loading Extensions into Extension Membrane...')
    extensions = [('neural_bridge', {'version': '2.1', 'capabilities': ['symbolic_reasoning']}), ('hypergraph_engine', {'version': '1.5', 'capabilities': ['graph_operations']}), ('memory_optimizer', {'version': '3.0', 'capabilities': ['memory_management']})]
    for ext_name, ext_data in extensions:
        success = echo.load_extension_to_membrane(ext_name, ext_data)
        print(f"   📦 {ext_name}: {('✓ Loaded' if success else '✗ Failed')}")
    print('\n6. Updated Membrane Status:')
    status = echo.get_membrane_status()
    for membrane_name, membrane_status in status.items():
        resources = membrane_status.get('resources', {})
        print(f"   🧬 {membrane_name}: Memory={resources.get('memory', 0)}, CPU={resources.get('cpu', 0)}, IO={resources.get('io', 0)}")
    print('\n7. Integrating with Deep Tree Echo functionality...')
    root = echo.create_tree('P-System computational boundaries enable secure processing')
    print(f"   🌳 Created root node: '{root.content[:30]}...'")
    print(f'   🎵 Root echo value: {root.echo_value:.3f}')
    children = ['Membrane isolation provides security boundaries', 'Inter-membrane communication enables coordination', 'Extension membranes contain plugin functionality']
    for child_content in children:
        child = echo.add_child(root, child_content)
        print(f"   🌿 Child: '{child.content[:30]}...', Echo: {child.echo_value:.3f}")
    echo.propagate_echoes()
    print(f'   🔄 After propagation - Root echo: {root.echo_value:.3f}')
    echo.send_membrane_message('cognitive', 'extension', 'tree_analysis', {'total_nodes': len(root.children) + 1, 'max_echo': root.echo_value, 'analysis': 'P-System integration successful'})
    final_results = echo.process_membrane_messages()
    processed_count = sum((len(results) for results in final_results.values()))
    print(f'   🎯 Final processing: {processed_count} messages handled')
    print('\n' + '=' * 50)
    print('🎉 P-System Membranes demonstration complete!')
    print('\nKey Features Demonstrated:')
    print('• Computational boundary isolation')
    print('• Secure inter-membrane communication')
    print('• Extension loading and management')
    print('• Integration with existing Echo functionality')
    print('• Resource allocation and monitoring')
if __name__ == '__main__':
    demonstrate_membrane_system()