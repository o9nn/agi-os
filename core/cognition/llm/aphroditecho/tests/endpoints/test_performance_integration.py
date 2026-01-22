import pytest
import time
import statistics
import concurrent.futures
from typing import Dict, Any
from unittest.mock import AsyncMock, MagicMock
from fastapi.testclient import TestClient
from aphrodite.endpoints.deep_tree_echo import create_app
from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig
class TestBackendPerformance:
    @pytest.fixture
    def performance_engine(self):
        engine = AsyncMock()
        engine.get_model_config.return_value = MagicMock(model='performance-test-model', max_model_len=8192, vocab_size=50000)
        engine.get_aphrodite_config.return_value = MagicMock(model_config=MagicMock(model='performance-test-model'), parallel_config=MagicMock(tensor_parallel_size=2), scheduler_config=MagicMock(max_num_seqs=512))
        return engine
    @pytest.fixture
    def performance_config(self):
        return DTESNConfig(enable_docs=False, max_membrane_depth=8, esn_reservoir_size=1024, bseries_max_order=16, enable_caching=True, enable_performance_monitoring=True)
    @pytest.fixture
    def performance_app(self, performance_engine, performance_config):
        return create_app(engine=performance_engine, config=performance_config, enable_async_resources=True)
    @pytest.fixture
    def performance_client(self, performance_app):
        return TestClient(performance_app)
    def test_single_request_response_time(self, performance_client):
        endpoints_to_test = [('/health', 'GET', None), ('/deep_tree_echo/', 'GET', None), ('/deep_tree_echo/status', 'GET', None), ('/deep_tree_echo/membrane_info', 'GET', None), ('/deep_tree_echo/engine_integration', 'GET', None)]
        performance_results = []
        for endpoint, method, data in endpoints_to_test:
            times = []
            for _ in range(10):
                start_time = time.time()
                if method == 'GET':
                    response = performance_client.get(endpoint)
                else:
                    response = performance_client.post(endpoint, json=data)
                end_time = time.time()
                response_time = (end_time - start_time) * 1000
                assert response.status_code == 200
                times.append(response_time)
            avg_time = statistics.mean(times)
            median_time = statistics.median(times)
            std_dev = statistics.stdev(times) if len(times) > 1 else 0
            performance_results.append({'endpoint': endpoint, 'avg_response_time_ms': avg_time, 'median_response_time_ms': median_time, 'std_dev_ms': std_dev, 'max_time_ms': max(times), 'min_time_ms': min(times)})
            assert avg_time < 100, f'Average response time too slow for {endpoint}: {avg_time}ms'
            assert max(times) < 200, f'Maximum response time too slow for {endpoint}: {max(times)}ms'
        return performance_results
    def test_dtesn_processing_performance_scaling(self, performance_client):
        test_cases = [{'depth': 2, 'esn_size': 64, 'expected_max': 500}, {'depth': 4, 'esn_size': 128, 'expected_max': 1000}, {'depth': 6, 'esn_size': 256, 'expected_max': 2000}, {'depth': 8, 'esn_size': 512, 'expected_max': 4000}]
        scaling_results = []
        for case in test_cases:
            times = []
            for iteration in range(5):
                request_data = {'input_data': f"scaling test depth={case['depth']} iteration={iteration}", 'membrane_depth': case['depth'], 'esn_size': case['esn_size'], 'processing_mode': 'server_side', 'output_format': 'json'}
                start_time = time.time()
                response = performance_client.post('/deep_tree_echo/process', json=request_data)
                end_time = time.time()
                assert response.status_code == 200
                data = response.json()
                backend_time = data['processing_time_ms']
                total_time = (end_time - start_time) * 1000
                times.append({'backend_processing_ms': backend_time, 'total_request_ms': total_time})
            avg_backend = statistics.mean([t['backend_processing_ms'] for t in times])
            avg_total = statistics.mean([t['total_request_ms'] for t in times])
            scaling_results.append({'complexity': case, 'avg_backend_ms': avg_backend, 'avg_total_ms': avg_total, 'within_expected': avg_backend <= case['expected_max']})
            assert avg_backend <= case['expected_max'], f"Backend processing too slow for complexity {case}: {avg_backend}ms > {case['expected_max']}ms"
        backend_times = [r['avg_backend_ms'] for r in scaling_results]
        for i in range(1, len(backend_times)):
            ratio = backend_times[i] / backend_times[i - 1]
            assert ratio <= 3.0, f'Poor scaling ratio: {ratio}'
    def test_concurrent_request_performance(self, performance_client):
        def make_concurrent_request(request_id: int) -> Dict[str, Any]:
            start_time = time.time()
            response = performance_client.post('/deep_tree_echo/process', json={'input_data': f'concurrent test {request_id}', 'membrane_depth': 3, 'esn_size': 128, 'processing_mode': 'server_side'})
            end_time = time.time()
            return {'request_id': request_id, 'status_code': response.status_code, 'response_time_ms': (end_time - start_time) * 1000, 'backend_time_ms': response.json()['processing_time_ms'] if response.status_code == 200 else None, 'success': response.status_code == 200}
        concurrency_levels = [5, 10, 20]
        concurrency_results = []
        for concurrency in concurrency_levels:
            start_time = time.time()
            with concurrent.futures.ThreadPoolExecutor(max_workers=concurrency) as executor:
                futures = [executor.submit(make_concurrent_request, i) for i in range(concurrency)]
                results = [future.result() for future in concurrent.futures.as_completed(futures)]
            total_time = time.time() - start_time
            successful_requests = [r for r in results if r['success']]
            success_rate = len(successful_requests) / len(results)
            if successful_requests:
                avg_response_time = statistics.mean([r['response_time_ms'] for r in successful_requests])
                avg_backend_time = statistics.mean([r['backend_time_ms'] for r in successful_requests if r['backend_time_ms'] is not None])
            else:
                avg_response_time = float('inf')
                avg_backend_time = float('inf')
            concurrency_results.append({'concurrency_level': concurrency, 'total_time_s': total_time, 'success_rate': success_rate, 'avg_response_time_ms': avg_response_time, 'avg_backend_time_ms': avg_backend_time, 'requests_per_second': concurrency / total_time if total_time > 0 else 0})
            assert success_rate >= 0.95, f'Success rate too low at concurrency {concurrency}: {success_rate}'
            assert avg_response_time < 2000, f'Response time too slow at concurrency {concurrency}: {avg_response_time}ms'
        return concurrency_results
    def test_batch_processing_performance(self, performance_client):
        batch_sizes = [1, 3, 5, 10, 15]
        batch_results = []
        for batch_size in batch_sizes:
            batch_data = {'inputs': [f'batch item {i}' for i in range(batch_size)], 'membrane_depth': 3, 'esn_size': 128, 'parallel_processing': True, 'max_batch_size': 20}
            times = []
            for _ in range(3):
                start_time = time.time()
                response = performance_client.post('/deep_tree_echo/batch_process', json=batch_data)
                end_time = time.time()
                assert response.status_code == 200
                data = response.json()
                total_time = (end_time - start_time) * 1000
                backend_time = data['total_processing_time_ms']
                times.append({'total_ms': total_time, 'backend_ms': backend_time, 'time_per_item_ms': backend_time / batch_size})
            avg_total = statistics.mean([t['total_ms'] for t in times])
            avg_backend = statistics.mean([t['backend_ms'] for t in times])
            avg_per_item = statistics.mean([t['time_per_item_ms'] for t in times])
            batch_results.append({'batch_size': batch_size, 'avg_total_ms': avg_total, 'avg_backend_ms': avg_backend, 'avg_time_per_item_ms': avg_per_item, 'throughput_items_per_sec': batch_size * 1000 / avg_backend if avg_backend > 0 else 0})
            assert avg_per_item < 200, f'Time per item too high for batch size {batch_size}: {avg_per_item}ms'
        per_item_times = [r['avg_time_per_item_ms'] for r in batch_results]
        for i in range(1, min(3, len(per_item_times))):
            efficiency_improvement = per_item_times[0] / per_item_times[i]
            assert efficiency_improvement >= 0.8, 'Batch processing not improving efficiency'
    def test_streaming_performance(self, performance_client):
        stream_request = {'input_data': 'streaming performance test with longer input data to simulate realistic workload', 'membrane_depth': 4, 'esn_size': 256, 'processing_mode': 'streaming'}
        streaming_times = []
        for _ in range(5):
            start_time = time.time()
            response = performance_client.post('/deep_tree_echo/stream_process', json=stream_request)
            first_byte_time = time.time()
            assert response.status_code == 200
            assert response.headers['content-type'] == 'text/event-stream; charset=utf-8'
            assert response.headers.get('X-Server-Rendered') == 'true'
            assert response.headers.get('X-Stream-Enhanced') == 'true'
            assert response.headers.get('X-Backpressure-Enabled') == 'true'
            content_length = len(response.text)
            end_time = time.time()
            streaming_times.append({'time_to_first_byte_ms': (first_byte_time - start_time) * 1000, 'total_streaming_time_ms': (end_time - start_time) * 1000, 'content_length': content_length, 'streaming_rate_bytes_per_sec': content_length / (end_time - first_byte_time) if end_time > first_byte_time else 0})
        avg_first_byte = statistics.mean([t['time_to_first_byte_ms'] for t in streaming_times])
        avg_streaming_rate = statistics.mean([t['streaming_rate_bytes_per_sec'] for t in streaming_times])
        assert avg_first_byte < 500, f'Time to first byte too slow: {avg_first_byte}ms'
        assert avg_streaming_rate > 1000, f'Streaming rate too slow: {avg_streaming_rate} bytes/sec'
    def test_memory_usage_performance(self, performance_app):
        import psutil
        import os
        process = psutil.Process(os.getpid())
        initial_memory = process.memory_info().rss / 1024 / 1024
        with TestClient(performance_app) as client:
            for i in range(20):
                response = client.post('/deep_tree_echo/process', json={'input_data': f'memory test iteration {i}', 'membrane_depth': 4, 'esn_size': 256, 'processing_mode': 'server_side'})
                assert response.status_code == 200
            final_memory = process.memory_info().rss / 1024 / 1024
            memory_increase = final_memory - initial_memory
            assert memory_increase < 100, f'Memory usage increased too much: {memory_increase}MB'
    def test_error_response_performance(self, performance_client):
        error_requests = [{'input_data': 'test', 'membrane_depth': 100, 'esn_size': 128}, {'input_data': 'test', 'membrane_depth': 4, 'esn_size': 50000}, {'membrane_depth': 4}]
        error_times = []
        for error_request in error_requests:
            start_time = time.time()
            response = performance_client.post('/deep_tree_echo/process', json=error_request)
            end_time = time.time()
            response_time = (end_time - start_time) * 1000
            error_times.append(response_time)
            assert response.status_code in [400, 422]
            assert response_time < 50, f'Error response too slow: {response_time}ms'
        avg_error_time = statistics.mean(error_times)
        assert avg_error_time < 30, f'Average error response time too slow: {avg_error_time}ms'
    def test_cache_performance_impact(self, performance_client):
        test_request = {'input_data': 'cache performance test identical input', 'membrane_depth': 3, 'esn_size': 128, 'processing_mode': 'server_side'}
        first_times = []
        for _ in range(3):
            start_time = time.time()
            response = performance_client.post('/deep_tree_echo/process', json=test_request)
            end_time = time.time()
            assert response.status_code == 200
            first_times.append((end_time - start_time) * 1000)
        avg_first_time = statistics.mean(first_times)
        cached_times = []
        for _ in range(5):
            start_time = time.time()
            response = performance_client.post('/deep_tree_echo/process', json=test_request)
            end_time = time.time()
            assert response.status_code == 200
            cached_times.append((end_time - start_time) * 1000)
        avg_cached_time = statistics.mean(cached_times)
        cache_improvement = avg_first_time / avg_cached_time if avg_cached_time > 0 else 1.0
        assert cache_improvement >= 0.8, f'Caching not providing expected performance benefit: {cache_improvement}'
    def test_enhanced_chunked_streaming_performance(self, performance_client):
        import json
        chunk_stream_request = {'input_data': 'A' * 10000, 'membrane_depth': 3, 'esn_size': 256, 'processing_mode': 'streaming'}
        response = performance_client.post('/deep_tree_echo/stream_chunks', json=chunk_stream_request, params={'chunk_size': 1024, 'enable_compression': True, 'timeout_prevention': True})
        assert response.status_code == 200
        assert response.headers['content-type'] == 'text/event-stream; charset=utf-8'
        assert response.headers.get('X-Server-Rendered') == 'true'
        assert response.headers.get('X-Chunk-Streaming') == 'true'
        assert response.headers.get('X-Backpressure-Enabled') == 'true'
        events = response.text.split('\n\n')
        data_events = [e for e in events if e.startswith('data: ')]
        assert len(data_events) > 5, 'Should have multiple streaming chunks'
        for event in data_events[:3]:
            if event.startswith('data: '):
                event_data = json.loads(event[6:])
                if event_data.get('type') == 'metadata':
                    assert 'timeout_prevention_enabled' in event_data
                    assert 'compression_enabled' in event_data
                    assert 'large_dataset_mode' in event_data
    def test_large_dataset_streaming_performance(self, performance_client):
        large_dataset_request = {'input_data': 'Large dataset test: ' + 'X' * 100000, 'membrane_depth': 4, 'esn_size': 512, 'processing_mode': 'streaming'}
        start_time = time.time()
        response = performance_client.post('/deep_tree_echo/stream_large_dataset', json=large_dataset_request, params={'max_chunk_size': 4096, 'compression_level': 2})
        first_byte_time = time.time()
        assert response.status_code == 200
        assert response.headers['content-type'] == 'text/event-stream; charset=utf-8'
        assert response.headers.get('X-Large-Dataset-Optimized') == 'true'
        assert response.headers.get('X-Compression-Level') == '2'
        assert response.headers.get('X-Timeout-Prevention') == 'enhanced'
        content_length = len(response.text)
        end_time = time.time()
        time_to_first_byte = (first_byte_time - start_time) * 1000
        total_time = (end_time - start_time) * 1000
        throughput = len(large_dataset_request['input_data']) / max(end_time - start_time, 0.001)
        assert time_to_first_byte < 1000, f'Large dataset first byte too slow: {time_to_first_byte}ms'
        assert throughput > 50000, f'Large dataset throughput too low: {throughput} bytes/sec'
        events = response.text.split('\n\n')
        event_lines = [e for e in events if e.strip()]
        heartbeat_events = [e for e in event_lines if 'heartbeat' in e]
        assert len(heartbeat_events) >= 1, 'Should have heartbeat events for timeout prevention'
    def test_streaming_timeout_prevention(self, performance_client):
        import json
        timeout_test_request = {'input_data': 'Timeout prevention test: ' + 'Y' * 60000, 'membrane_depth': 5, 'esn_size': 1024, 'processing_mode': 'streaming'}
        response = performance_client.post('/deep_tree_echo/stream_process', json=timeout_test_request)
        assert response.status_code == 200
        events = response.text.split('\n\n')
        data_events = [e for e in events if e.startswith('data: ')]
        heartbeat_found = False
        for event in data_events:
            if event.startswith('data: '):
                try:
                    event_data = json.loads(event[6:])
                    if 'heartbeat' in event_data.get('status', ''):
                        heartbeat_found = True
                        assert 'estimated_completion_sec' in event_data
                        break
                except json.JSONDecodeError:
                    continue
        assert heartbeat_found, 'Should have heartbeat message for large dataset processing'
class TestLoadTesting:
    @pytest.fixture
    def load_test_client(self, performance_app):
        return TestClient(performance_app)
    @pytest.mark.slow
    def test_sustained_load_performance(self, load_test_client):
        duration_seconds = 30
        request_rate = 2
        total_requests = duration_seconds * request_rate
        results = []
        start_time = time.time()
        for i in range(total_requests):
            request_start = time.time()
            response = load_test_client.post('/deep_tree_echo/process', json={'input_data': f'sustained load test {i}', 'membrane_depth': 2, 'esn_size': 64, 'processing_mode': 'server_side'})
            request_end = time.time()
            results.append({'request_id': i, 'response_time_ms': (request_end - request_start) * 1000, 'success': response.status_code == 200, 'timestamp': request_start - start_time})
            elapsed = time.time() - start_time
            expected_elapsed = (i + 1) / request_rate
            if elapsed < expected_elapsed:
                time.sleep(expected_elapsed - elapsed)
        successful_requests = [r for r in results if r['success']]
        success_rate = len(successful_requests) / len(results)
        response_times = [r['response_time_ms'] for r in successful_requests]
        avg_response_time = statistics.mean(response_times)
        p95_response_time = sorted(response_times)[int(0.95 * len(response_times))]
        assert success_rate >= 0.98, f'Success rate under load too low: {success_rate}'
        assert avg_response_time < 1000, f'Average response time under load too high: {avg_response_time}ms'
        assert p95_response_time < 2000, f'95th percentile response time too high: {p95_response_time}ms'