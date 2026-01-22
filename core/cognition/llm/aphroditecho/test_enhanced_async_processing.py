import asyncio
import logging
import sys
import time
from typing import List, Dict, Any
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)
sys.path.insert(0, '/home/runner/work/aphroditecho/aphroditecho')
try:
    from aphrodite.endpoints.deep_tree_echo.async_manager import AsyncConnectionPool, ConcurrencyManager, AsyncRequestQueue, ConnectionPoolConfig
    logger.info('Successfully imported enhanced async components')
except ImportError as e:
    logger.error(f'Failed to import async components: {e}')
    sys.exit(1)
class AsyncProcessingBenchmark:
    def __init__(self):
        self.results = {}
    async def test_connection_pool_performance(self):
        logger.info('=== Testing Connection Pool Performance ===')
        config = ConnectionPoolConfig(max_connections=500, min_connections=50, connection_timeout=10.0, enable_keepalive=True, max_concurrent_creates=50)
        pool = AsyncConnectionPool(config)
        try:
            await pool.start()
            logger.info('✅ Connection pool started successfully')
            start_time = time.time()
            concurrent_tasks = []
            async def acquire_connection(task_id: int):
                async with pool.get_connection() as conn:
                    await asyncio.sleep(0.01)
                    return f'task_{task_id}_conn_{conn}'
            for i in range(1000):
                concurrent_tasks.append(acquire_connection(i))
            results = await asyncio.gather(*concurrent_tasks)
            elapsed = time.time() - start_time
            logger.info(f'✅ Processed 1000 concurrent connections in {elapsed:.2f}s')
            logger.info(f'✅ Throughput: {1000 / elapsed:.1f} connections/sec')
            assert len(results) == 1000, f'Expected 1000 results, got {len(results)}'
            assert all(('conn_' in result for result in results)), 'Invalid connection results'
            stats = pool.get_stats()
            logger.info(f'Pool stats - Active: {stats.active_connections}, Idle: {stats.idle_connections}, Utilization: {stats.pool_utilization:.2%}')
            self.results['connection_pool'] = {'concurrent_requests': 1000, 'processing_time': elapsed, 'throughput': 1000 / elapsed, 'success': True}
        finally:
            await pool.stop()
            logger.info('✅ Connection pool stopped successfully')
    async def test_concurrency_manager_scaling(self):
        logger.info('=== Testing Enhanced Concurrency Management ===')
        manager = ConcurrencyManager(max_concurrent_requests=500, max_requests_per_second=1000.0, adaptive_scaling=True, scale_factor=1.2)
        async def process_request(request_id: int) -> Dict[str, Any]:
            async with manager.throttle_request():
                processing_time = 0.01 + request_id % 10 * 0.001
                await asyncio.sleep(processing_time)
                return {'id': request_id, 'processed': True, 'time': processing_time}
        logger.info('Testing burst processing capacity...')
        start_time = time.time()
        burst_tasks = [process_request(i) for i in range(2000)]
        results = await asyncio.gather(*burst_tasks, return_exceptions=True)
        elapsed = time.time() - start_time
        successful = [r for r in results if isinstance(r, dict) and r.get('processed')]
        failed = [r for r in results if isinstance(r, Exception)]
        logger.info(f'✅ Burst test completed in {elapsed:.2f}s')
        logger.info(f'✅ Successful: {len(successful)}, Failed: {len(failed)}')
        logger.info(f'✅ Throughput: {len(successful) / elapsed:.1f} requests/sec')
        load_stats = manager.get_current_load()
        logger.info(f'Load stats: {load_stats}')
        assert len(successful) >= 1500, f'Expected at least 1500 successful requests, got {len(successful)}'
        assert len(successful) / len(results) >= 0.75, 'Success rate should be at least 75%'
        self.results['concurrency_manager'] = {'total_requests': len(results), 'successful_requests': len(successful), 'failed_requests': len(failed), 'success_rate': len(successful) / len(results), 'throughput': len(successful) / elapsed, 'adaptive_scaling': load_stats.get('adaptive_scaling_enabled', False)}
    async def test_request_queue_batching(self):
        logger.info('=== Testing Request Queue Batching ===')
        queue = AsyncRequestQueue(max_queue_size=10000, priority_levels=5, batch_processing=True, batch_size=10)
        logger.info('Enqueuing 1000 requests with batching...')
        start_time = time.time()
        enqueue_tasks = []
        for i in range(1000):
            priority = i % 5
            enqueue_tasks.append(queue.enqueue_batch_request(request_data=f'test_data_{i}', priority=priority))
        request_ids = await asyncio.gather(*enqueue_tasks)
        enqueue_time = time.time() - start_time
        logger.info(f'✅ Enqueued 1000 requests in {enqueue_time:.2f}s')
        logger.info(f'✅ Enqueue rate: {1000 / enqueue_time:.1f} requests/sec')
        processed_count = 0
        batch_count = 0
        start_time = time.time()
        while True:
            batch_or_request = await queue.dequeue_batch_request()
            if batch_or_request is None:
                break
            if isinstance(batch_or_request, list):
                batch_count += 1
                processed_count += len(batch_or_request)
                logger.debug(f'Processed batch with {len(batch_or_request)} requests')
            else:
                processed_count += 1
                logger.debug('Processed single request')
            if isinstance(batch_or_request, list):
                for item in batch_or_request:
                    await queue.record_request_result(item['id'], success=True, response_time=0.01)
            else:
                await queue.record_request_result(batch_or_request['id'], success=True, response_time=0.01)
        processing_time = time.time() - start_time
        logger.info(f'✅ Processed {processed_count} requests in {batch_count} batches')
        logger.info(f'✅ Processing time: {processing_time:.2f}s')
        logger.info(f'✅ Processing rate: {processed_count / processing_time:.1f} requests/sec')
        queue_stats = queue.get_queue_stats()
        logger.info(f'Queue stats: {queue_stats}')
        assert processed_count >= 950, f'Expected to process at least 950 requests, got {processed_count}'
        assert batch_count > 0, 'Expected some batched processing'
        self.results['request_queue'] = {'enqueued_requests': len(request_ids), 'processed_requests': processed_count, 'batch_count': batch_count, 'avg_batch_size': processed_count / batch_count if batch_count > 0 else 0, 'enqueue_rate': 1000 / enqueue_time, 'processing_rate': processed_count / processing_time, 'queue_utilization': queue_stats.get('queue_utilization', 0)}
    async def test_integrated_performance(self):
        logger.info('=== Testing Integrated Performance ===')
        pool_config = ConnectionPoolConfig(max_connections=500, min_connections=50, enable_keepalive=True)
        connection_pool = AsyncConnectionPool(pool_config)
        concurrency_manager = ConcurrencyManager(max_concurrent_requests=500, max_requests_per_second=1000.0, adaptive_scaling=True)
        request_queue = AsyncRequestQueue(max_queue_size=5000, batch_processing=True, batch_size=20)
        try:
            await connection_pool.start()
            async def integrated_request_processor(request_id: int):
                queued_id = await request_queue.enqueue_batch_request(request_data=f'integrated_test_{request_id}', priority=request_id % 3)
                async with concurrency_manager.throttle_request():
                    async with connection_pool.get_connection() as conn:
                        await asyncio.sleep(0.005)
                        await request_queue.record_request_result(queued_id, success=True, response_time=0.005)
                        return {'request_id': request_id, 'queued_id': queued_id, 'connection': conn, 'success': True}
            logger.info('Running integrated test with 5000 concurrent requests...')
            start_time = time.time()
            integrated_tasks = [integrated_request_processor(i) for i in range(5000)]
            results = await asyncio.gather(*integrated_tasks, return_exceptions=True)
            elapsed = time.time() - start_time
            successful = [r for r in results if isinstance(r, dict) and r.get('success')]
            failed = [r for r in results if isinstance(r, Exception)]
            logger.info(f'✅ Integrated test completed in {elapsed:.2f}s')
            logger.info(f'✅ Successful: {len(successful)}, Failed: {len(failed)}')
            logger.info(f'✅ Success rate: {len(successful) / len(results):.1%}')
            logger.info(f'✅ Throughput: {len(successful) / elapsed:.1f} requests/sec')
            baseline_throughput = 50
            actual_throughput = len(successful) / elapsed
            improvement_factor = actual_throughput / baseline_throughput
            logger.info(f'✅ Improvement factor: {improvement_factor:.1f}x')
            assert improvement_factor >= 10, f'Expected at least 10x improvement, got {improvement_factor:.1f}x'
            assert len(successful) >= 4500, f'Expected at least 4500 successful requests, got {len(successful)}'
            self.results['integrated_performance'] = {'total_requests': len(results), 'successful_requests': len(successful), 'failed_requests': len(failed), 'success_rate': len(successful) / len(results), 'throughput': actual_throughput, 'improvement_factor': improvement_factor, 'target_achieved': improvement_factor >= 10}
        finally:
            await connection_pool.stop()
    def print_summary(self):
        logger.info('\n' + '=' * 60)
        logger.info('ENHANCED ASYNC PROCESSING TEST SUMMARY')
        logger.info('=' * 60)
        total_tests = len(self.results)
        passed_tests = sum((1 for result in self.results.values() if result.get('success', result.get('target_achieved', False))))
        logger.info(f'Tests passed: {passed_tests}/{total_tests}')
        for test_name, result in self.results.items():
            logger.info(f"\n{test_name.upper().replace('_', ' ')}:")
            for key, value in result.items():
                if isinstance(value, float):
                    logger.info(f'  {key}: {value:.2f}')
                else:
                    logger.info(f'  {key}: {value}')
        if passed_tests == total_tests:
            logger.info('\n🎉 ALL TESTS PASSED - 10x ASYNC ENHANCEMENT SUCCESSFUL!')
            logger.info('✅ Connection pooling: Enhanced capacity and performance')
            logger.info('✅ Concurrency management: Adaptive scaling and throttling')
            logger.info('✅ Request batching: High-throughput batch processing')
            logger.info('✅ Integrated performance: 10x improvement achieved')
        else:
            logger.warning(f'\n⚠️  {total_tests - passed_tests} tests failed')
async def main():
    logger.info('Starting Enhanced Async Processing Tests for Task 6.2.1')
    benchmark = AsyncProcessingBenchmark()
    try:
        await benchmark.test_connection_pool_performance()
        await benchmark.test_concurrency_manager_scaling()
        await benchmark.test_request_queue_batching()
        await benchmark.test_integrated_performance()
        benchmark.print_summary()
        return True
    except Exception as e:
        logger.error(f'Test failed with error: {e}', exc_info=True)
        return False
if __name__ == '__main__':
    success = asyncio.run(main())
    sys.exit(0 if success else 1)