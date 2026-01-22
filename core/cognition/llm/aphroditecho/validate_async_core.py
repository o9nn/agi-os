import asyncio
import sys
import time
from pathlib import Path
project_root = Path(__file__).parent
sys.path.insert(0, str(project_root))
def test_imports():
    print('--- Testing Component Imports ---')
    try:
        from aphrodite.endpoints.deep_tree_echo.async_manager import AsyncConnectionPool, ConcurrencyManager, ConnectionPoolConfig, ResourcePoolStats
        print('✓ AsyncConnectionPool imported successfully')
        print('✓ ConcurrencyManager imported successfully')
        print('✓ ConnectionPoolConfig imported successfully')
        print('✓ ResourcePoolStats imported successfully')
        config = ConnectionPoolConfig(max_connections=10, min_connections=2)
        assert config.max_connections == 10, 'Config creation failed'
        print('✓ ConnectionPoolConfig creation works')
        stats = ResourcePoolStats()
        assert stats.active_connections == 0, 'Stats creation failed'
        print('✓ ResourcePoolStats creation works')
        return True
    except ImportError as e:
        print(f'✗ Import failed: {e}')
        return False
    except Exception as e:
        print(f'✗ Component test failed: {e}')
        return False
async def test_connection_pool_basic():
    print('\n--- Testing AsyncConnectionPool Basic Functionality ---')
    try:
        from aphrodite.endpoints.deep_tree_echo.async_manager import AsyncConnectionPool, ConnectionPoolConfig
        config = ConnectionPoolConfig(max_connections=5, min_connections=1)
        pool = AsyncConnectionPool(config)
        assert pool.config.max_connections == 5, 'Pool config not set correctly'
        assert pool._connection_semaphore._value == 5, 'Semaphore not initialized correctly'
        print('✓ Connection pool initialization works')
        await pool.start()
        print('✓ Connection pool startup works')
        stats = pool.get_stats()
        assert hasattr(stats, 'total_requests'), 'Stats missing total_requests'
        assert hasattr(stats, 'active_connections'), 'Stats missing active_connections'
        print('✓ Connection pool stats retrieval works')
        async with pool.get_connection() as conn_id:
            assert conn_id is not None, 'Connection ID is None'
            assert isinstance(conn_id, str), 'Connection ID is not string'
            print(f'✓ Connection context manager works (conn: {conn_id[:20]}...)')
        connections_used = []
        async def use_connection():
            async with pool.get_connection() as conn_id:
                connections_used.append(conn_id)
                await asyncio.sleep(0.01)
        tasks = [use_connection() for _ in range(3)]
        await asyncio.gather(*tasks)
        assert len(connections_used) == 3, f'Expected 3 connections, got {len(connections_used)}'
        print('✓ Concurrent connection handling works')
        final_stats = pool.get_stats()
        assert final_stats.total_requests >= 3, f'Expected at least 3 requests, got {final_stats.total_requests}'
        print('✓ Connection pool statistics tracking works')
        await pool.stop()
        print('✓ Connection pool shutdown works')
        return True
    except Exception as e:
        print(f'✗ Connection pool test failed: {e}')
        import traceback
        traceback.print_exc()
        return False
async def test_concurrency_manager_basic():
    print('\n--- Testing ConcurrencyManager Basic Functionality ---')
    try:
        from aphrodite.endpoints.deep_tree_echo.async_manager import ConcurrencyManager
        manager = ConcurrencyManager(max_concurrent_requests=3, max_requests_per_second=10.0, burst_limit=5)
        assert manager.max_concurrent_requests == 3, 'Manager config not set correctly'
        assert manager._request_semaphore._value == 3, 'Request semaphore not initialized'
        print('✓ Concurrency manager initialization works')
        load_stats = manager.get_current_load()
        assert 'concurrent_requests' in load_stats, 'Load stats missing concurrent_requests'
        assert 'rate_limit_utilization' in load_stats, 'Load stats missing rate_limit_utilization'
        print('✓ Concurrency manager load stats work')
        request_times = []
        async def throttled_request():
            start_time = time.time()
            async with manager.throttle_request():
                await asyncio.sleep(0.01)
            end_time = time.time()
            request_times.append(end_time - start_time)
        await throttled_request()
        assert len(request_times) == 1, 'Request not processed'
        assert request_times[0] >= 0.01, 'Request took too little time'
        print('✓ Single throttled request works')
        tasks = [throttled_request() for _ in range(2)]
        await asyncio.gather(*tasks)
        assert len(request_times) == 3, f'Expected 3 total requests, got {len(request_times)}'
        print('✓ Multiple throttled requests work')
        final_load_stats = manager.get_current_load()
        assert final_load_stats['concurrent_requests'] == 0, 'Concurrent requests not cleaned up'
        print('✓ Concurrency manager cleanup works')
        return True
    except Exception as e:
        print(f'✗ Concurrency manager test failed: {e}')
        import traceback
        traceback.print_exc()
        return False
def test_middleware_imports():
    print('\n--- Testing Middleware Imports ---')
    try:
        from aphrodite.endpoints.deep_tree_echo.middleware import DTESNMiddleware, PerformanceMonitoringMiddleware, AsyncResourceMiddleware
        print('✓ DTESNMiddleware imported successfully')
        print('✓ PerformanceMonitoringMiddleware imported successfully')
        print('✓ AsyncResourceMiddleware imported successfully')
        assert DTESNMiddleware is not None, 'DTESNMiddleware class not defined'
        assert PerformanceMonitoringMiddleware is not None, 'PerformanceMonitoringMiddleware class not defined'
        assert AsyncResourceMiddleware is not None, 'AsyncResourceMiddleware class not defined'
        print('✓ Middleware classes are properly defined')
        return True
    except ImportError as e:
        print(f'✗ Middleware import failed: {e}')
        return False
    except Exception as e:
        print(f'✗ Middleware test failed: {e}')
        return False
async def main():
    print('=== Simplified Async Processing Validation ===')
    import_success = test_imports()
    if not import_success:
        print('✗ Cannot continue - import tests failed')
        return False
    middleware_success = test_middleware_imports()
    async_tests = [test_connection_pool_basic(), test_concurrency_manager_basic()]
    results = await asyncio.gather(*async_tests, return_exceptions=True)
    test_results = [import_success, middleware_success]
    for result in results:
        if isinstance(result, Exception):
            print(f'Async test exception: {result}')
            test_results.append(False)
        else:
            test_results.append(result)
    passed = sum((1 for r in test_results if r is True))
    total = len(test_results)
    print('\n=== Validation Results ===')
    print(f'Passed: {passed}/{total}')
    if passed == total:
        print('✓ ALL CORE TESTS PASSED - Async management components are working!')
        print('✓ Implementation ready for integration testing')
        return True
    else:
        print('✗ Some tests failed - Review implementation')
        return False
if __name__ == '__main__':
    success = asyncio.run(main())
    sys.exit(0 if success else 1)