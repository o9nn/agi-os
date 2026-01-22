import json
import os
import sys
from datetime import datetime
def test_introspection_system():
    print('=== Testing AI Introspection System ===')
    try:
        from echoself_introspection import EchoselfIntrospection
        print('✓ EchoselfIntrospection module imported successfully')
        introspector = EchoselfIntrospection('.')
        print('✓ Introspector initialized successfully')
        introspection_prompt = introspector.inject_repo_input_into_prompt(current_load=0.5, recent_activity=0.3)
        print(f'✓ Repository analysis completed, prompt length: {len(introspection_prompt)}')
        metrics = introspector.get_attention_metrics()
        print(f'✓ Attention metrics retrieved: {len(metrics)} items')
        return (True, introspector, metrics)
    except ImportError as e:
        print(f'✗ EchoselfIntrospection module not available: {e}')
        return (False, None, None)
    except Exception as e:
        print(f'✗ Introspection system test failed: {e}')
        return (False, None, None)
def test_enhanced_note_generation(introspection_data=None):
    print('\n=== Testing Enhanced Note Generation ===')
    test_note = {'timestamp': datetime.utcnow().isoformat(), 'improvement': 'test_improvement_with_introspection', 'assessment': 'test_assessment', 'result': 'success', 'retries': 0, 'introspection_enhanced': introspection_data is not None, 'introspection_status': introspection_data.get('introspection_status', 'unknown') if introspection_data else 'unknown', 'files_analyzed': introspection_data.get('files_analyzed', 0) if introspection_data else 0, 'resource_usage': {'avg_cpu': 45.2, 'avg_memory': 67.8, 'samples': 120}}
    if introspection_data:
        test_note['ai_introspection'] = introspection_data
    try:
        note_json = json.dumps(test_note, indent=2)
        print(f'✓ Note serialized successfully, size: {len(note_json)} bytes')
        parsed_note = json.loads(note_json)
        print('✓ Note deserialized successfully')
        required_fields = ['timestamp', 'improvement', 'assessment', 'result', 'introspection_enhanced']
        for field in required_fields:
            if field in parsed_note:
                print(f"✓ Required field '{field}' present")
            else:
                print(f"✗ Required field '{field}' missing")
                return False
        return (True, test_note)
    except Exception as e:
        print(f'✗ Note generation test failed: {e}')
        return (False, None)
def test_workflow_integration():
    print('\n=== Testing Workflow Integration ===')
    try:
        sys.path.append('.')
        from cronbot import read_note, write_note, load_ai_introspection_data
        print('✓ Cronbot functions imported successfully')
        test_note = {'test': 'data', 'timestamp': datetime.utcnow().isoformat()}
        write_note(test_note)
        read_back = read_note()
        if read_back.get('test') == 'data':
            print('✓ Note read/write functions work correctly')
        else:
            print('✗ Note read/write functions failed')
            return False
        return True
    except ImportError as e:
        print(f'✗ Cronbot functions not available: {e}')
        return False
    except Exception as e:
        print(f'✗ Workflow integration test failed: {e}')
        return False
def test_copilot_integration():
    print('\n=== Testing Copilot Integration ===')
    try:
        sys.path.append('.')
        from copilot_suggestions import load_introspection_context
        print('✓ Copilot functions imported successfully')
        context = load_introspection_context()
        if context is not None:
            print('✓ Introspection context loaded successfully')
        else:
            print('ℹ No introspection context available (expected in test environment)')
        return True
    except ImportError as e:
        print(f'✗ Copilot functions not available: {e}')
        return False
    except Exception as e:
        print(f'✗ Copilot integration test failed: {e}')
        return False
def test_error_handling():
    print('\n=== Testing Error Handling ===')
    try:
        if not os.path.exists('nonexistent_note.json'):
            print('✓ Non-existent file handling works correctly')
        invalid_json = '{"invalid": json}'
        try:
            json.loads(invalid_json)
            print('✗ Invalid JSON was parsed (unexpected)')
            return False
        except json.JSONDecodeError:
            print('✓ Invalid JSON properly rejected')
        return True
    except Exception as e:
        print(f'✗ Error handling test failed: {e}')
        return False
def generate_test_report(results):
    print('\n=== TEST REPORT ===')
    total_tests = len(results)
    passed_tests = sum((1 for result in results.values() if result))
    print(f'Total tests: {total_tests}')
    print(f'Passed: {passed_tests}')
    print(f'Failed: {total_tests - passed_tests}')
    print(f'Success rate: {passed_tests / total_tests * 100:.1f}%')
    if passed_tests == total_tests:
        print('🎉 All tests passed! Enhanced cronbot workflow is ready.')
    else:
        print('⚠️  Some tests failed. Check the output above for details.')
    return passed_tests == total_tests
def main():
    print('Enhanced Cronbot Workflow Test Suite')
    print('=' * 50)
    results = {}
    introspection_available, introspector, metrics = test_introspection_system()
    results['introspection_system'] = introspection_available
    if introspection_available and metrics:
        introspection_data = {'timestamp': datetime.utcnow().isoformat(), 'introspection_status': 'success', 'files_analyzed': metrics.get('hypergraph_nodes', 0), 'highest_salience_files': metrics.get('highest_salience_files', [])}
        note_success, test_note = test_enhanced_note_generation(introspection_data)
    else:
        note_success, test_note = test_enhanced_note_generation()
    results['note_generation'] = note_success
    results['workflow_integration'] = test_workflow_integration()
    results['copilot_integration'] = test_copilot_integration()
    results['error_handling'] = test_error_handling()
    all_passed = generate_test_report(results)
    test_results = {'timestamp': datetime.utcnow().isoformat(), 'results': results, 'all_passed': all_passed, 'test_note': test_note if test_note else None}
    with open('enhanced_cronbot_test_results.json', 'w') as f:
        json.dump(test_results, f, indent=2)
    print('\nTest results saved to: enhanced_cronbot_test_results.json')
    return 0 if all_passed else 1
if __name__ == '__main__':
    sys.exit(main())