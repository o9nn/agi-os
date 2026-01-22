import sys
import os
import time
import json
import subprocess
from typing import Dict, Any
import requests
from urllib.parse import urljoin
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from tests.real_time_test_framework import RealTimeTestFramework, create_test_framework
class InteractiveTestSuite:
    def __init__(self, framework: RealTimeTestFramework, base_url: str='http://localhost:8000'):
        self.framework = framework
        self.base_url = base_url
        self.server_process = None
        self.test_results = []
    def start_web_server(self, port: int=8000) -> bool:
        try:
            response = requests.get(self.base_url, timeout=2)
            if response.status_code == 200:
                print(f'✅ Web server already running at {self.base_url}')
                return True
        except requests.exceptions.RequestException:
            pass
        try:
            server_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
            self.server_process = subprocess.Popen(['python3', '-m', 'http.server', str(port)], cwd=server_dir, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            for _ in range(10):
                try:
                    response = requests.get(self.base_url, timeout=1)
                    if response.status_code == 200:
                        print(f'✅ Web server started at {self.base_url}')
                        return True
                except requests.exceptions.RequestException:
                    time.sleep(0.5)
            print('❌ Failed to start web server')
            return False
        except Exception as e:
            print(f'❌ Error starting web server: {e}')
            return False
    def stop_web_server(self):
        if self.server_process:
            self.server_process.terminate()
            self.server_process.wait()
            print('🛑 Web server stopped')
    def test_basic_application_load(self) -> Dict[str, Any]:
        print('Testing basic application load...')
        results = {'scenario': 'basic_application_load', 'tests': {}, 'overall_passed': True}
        try:
            start_time = time.perf_counter()
            response = requests.get(self.base_url, timeout=5)
            load_time = time.perf_counter() - start_time
            results['tests']['page_load'] = {'passed': response.status_code == 200, 'load_time_ms': load_time * 1000, 'status_code': response.status_code, 'content_length': len(response.content)}
            title_found = 'Deep Tree Echo' in response.text
            results['tests']['title_check'] = {'passed': title_found, 'expected': 'Deep Tree Echo - Living Memory & Distributed Cognition'}
        except Exception as e:
            results['tests']['page_load'] = {'passed': False, 'error': str(e)}
            results['overall_passed'] = False
        resources = ['app.js', 'style.css']
        for resource in resources:
            try:
                url = urljoin(self.base_url, resource)
                response = requests.get(url, timeout=2)
                results['tests'][f'{resource}_load'] = {'passed': response.status_code == 200, 'status_code': response.status_code, 'size_bytes': len(response.content)}
                if response.status_code != 200:
                    results['overall_passed'] = False
            except Exception as e:
                results['tests'][f'{resource}_load'] = {'passed': False, 'error': str(e)}
                results['overall_passed'] = False
        try:
            project_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
            result = subprocess.run(['node', '-c', 'app.js'], cwd=project_dir, capture_output=True, timeout=5)
            results['tests']['javascript_syntax'] = {'passed': result.returncode == 0, 'return_code': result.returncode, 'stderr': result.stderr.decode() if result.stderr else None}
            if result.returncode != 0:
                results['overall_passed'] = False
        except Exception as e:
            results['tests']['javascript_syntax'] = {'passed': False, 'error': str(e)}
            results['overall_passed'] = False
        return results
    def test_web_application_performance(self) -> Dict[str, Any]:
        print('Testing web application performance...')
        results = {'scenario': 'performance_testing', 'metrics': {}, 'overall_passed': True}
        response_times = []
        successful_requests = 0
        for i in range(10):
            try:
                start_time = time.perf_counter()
                response = requests.get(self.base_url, timeout=2)
                end_time = time.perf_counter()
                if response.status_code == 200:
                    successful_requests += 1
                    response_times.append((end_time - start_time) * 1000)
            except Exception:
                pass
        if response_times:
            results['metrics']['response_times'] = {'min_ms': min(response_times), 'max_ms': max(response_times), 'avg_ms': sum(response_times) / len(response_times), 'success_rate': successful_requests / 10 * 100}
            avg_response_time = sum(response_times) / len(response_times)
            results['metrics']['performance_check'] = {'passed': avg_response_time < 100, 'avg_response_ms': avg_response_time, 'threshold_ms': 100}
            if avg_response_time >= 100:
                results['overall_passed'] = False
        else:
            results['overall_passed'] = False
            results['metrics']['error'] = 'No successful requests'
        return results
    def test_api_endpoints(self) -> Dict[str, Any]:
        print('Testing API endpoints and dynamic features...')
        results = {'scenario': 'api_endpoints', 'tests': {}, 'overall_passed': True}
        endpoints = {'/': 'text/html', '/app.js': 'application/javascript', '/style.css': 'text/css'}
        for endpoint, expected_content_type in endpoints.items():
            try:
                url = urljoin(self.base_url, endpoint)
                response = requests.get(url, timeout=2)
                content_type = response.headers.get('content-type', '').split(';')[0]
                results['tests'][f"endpoint_{endpoint.replace('/', '_')}"] = {'passed': response.status_code == 200, 'status_code': response.status_code, 'content_type': content_type, 'expected_content_type': expected_content_type, 'content_type_match': expected_content_type in content_type}
                if response.status_code != 200:
                    results['overall_passed'] = False
            except Exception as e:
                results['tests'][f"endpoint_{endpoint.replace('/', '_')}"] = {'passed': False, 'error': str(e)}
                results['overall_passed'] = False
        return results
    def test_application_structure(self) -> Dict[str, Any]:
        print('Testing application structure...')
        results = {'scenario': 'application_structure', 'tests': {}, 'overall_passed': True}
        try:
            response = requests.get(self.base_url, timeout=5)
            content = response.text
            essential_elements = ['Deep Tree Echo', 'svg', 'class="node"', 'class="reflection-panel"', 'id="echo-input"', 'DeepTreeEcho']
            for element in essential_elements:
                found = element in content
                results['tests'][f"element_{element.replace(' ', '_').replace('.', '_')}"] = {'passed': found, 'element': element}
                if not found:
                    results['overall_passed'] = False
            js_response = requests.get(urljoin(self.base_url, 'app.js'), timeout=2)
            js_content = js_response.text
            js_features = ['class DeepTreeEcho', 'addUserEcho', 'handleNodeClick', 'handleNodeHover']
            for feature in js_features:
                found = feature in js_content
                results['tests'][f"js_feature_{feature.replace(' ', '_')}"] = {'passed': found, 'feature': feature}
                if not found:
                    results['overall_passed'] = False
            js_lines = len(js_content.split('\n'))
            css_response = requests.get(urljoin(self.base_url, 'style.css'), timeout=2)
            css_lines = len(css_response.text.split('\n'))
            results['tests']['file_sizes'] = {'js_lines': js_lines, 'css_lines': css_lines, 'js_expected': 703, 'css_expected': 1314, 'js_size_check': abs(js_lines - 703) < 50, 'css_size_check': abs(css_lines - 1314) < 100}
        except Exception as e:
            results['tests']['structure_error'] = {'passed': False, 'error': str(e)}
            results['overall_passed'] = False
        return results
    def run_interactive_test_suite(self) -> Dict[str, Any]:
        print('Echo.Kern Interactive Test Suite')
        print('=' * 40)
        if not self.start_web_server():
            return {'error': 'Failed to start web server', 'overall_passed': False}
        try:
            test_scenarios = [self.test_basic_application_load, self.test_web_application_performance, self.test_api_endpoints, self.test_application_structure]
            all_results = {'test_suite': 'interactive_testing', 'start_time': time.time(), 'scenarios': {}, 'summary': {}}
            total_tests = 0
            passed_tests = 0
            for test_func in test_scenarios:
                scenario_result = test_func()
                scenario_name = scenario_result['scenario']
                all_results['scenarios'][scenario_name] = scenario_result
                if 'tests' in scenario_result:
                    scenario_tests = len(scenario_result['tests'])
                    scenario_passed = sum((1 for test in scenario_result['tests'].values() if test.get('passed', False)))
                    total_tests += scenario_tests
                    passed_tests += scenario_passed
                status = '✅' if scenario_result.get('overall_passed', False) else '❌'
                print(f"{status} {scenario_name}: {('PASSED' if scenario_result.get('overall_passed', False) else 'FAILED')}")
            pass_rate = passed_tests / total_tests * 100 if total_tests > 0 else 0
            all_results['summary'] = {'total_tests': total_tests, 'passed_tests': passed_tests, 'pass_rate': pass_rate, 'overall_passed': pass_rate > 80, 'end_time': time.time()}
            print('\nInteractive Test Summary:')
            print(f'Total Tests: {total_tests}')
            print(f'Passed: {passed_tests}')
            print(f'Pass Rate: {pass_rate:.1f}%')
            print(f"Overall: {('✅ PASSED' if pass_rate > 80 else '❌ FAILED')}")
            return all_results
        except Exception as e:
            return {'error': f'Test suite failed: {e}', 'overall_passed': False}
def run_interactive_tests(base_url: str='http://localhost:8000', output_file: str=None) -> bool:
    framework = create_test_framework()
    test_suite = InteractiveTestSuite(framework, base_url)
    results = test_suite.run_interactive_test_suite()
    if output_file:
        with open(output_file, 'w') as f:
            json.dump(results, f, indent=2)
        print(f'Results saved to {output_file}')
    test_suite.stop_web_server()
    return results.get('summary', {}).get('overall_passed', False)
if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Run interactive web application tests')
    parser.add_argument('--url', default='http://localhost:8000', help='Base URL for testing (default: http://localhost:8000)')
    parser.add_argument('--output', type=str, help='Output file for test results')
    args = parser.parse_args()
    success = run_interactive_tests(args.url, args.output)
    exit(0 if success else 1)