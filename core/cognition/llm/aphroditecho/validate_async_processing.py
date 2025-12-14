#!/usr/bin/env python3
"""
Simple validation script for async server-side processing implementation.

Tests the key components without requiring pytest framework.
"""

import asyncio
import sys
import time
from pathlib import Path

# Add project root to path
project_root = Path(__file__).parent
sys.path.insert(0, str(project_root))

try:
    from aphrodite.endpoints.deep_tree_echo.async_manager import (
        AsyncConnectionPool,
        ConcurrencyManager,
        ConnectionPoolConfig
    )
    from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig
    from aphrodite.endpoints.deep_tree_echo import create_app
    print("✓ Successfully imported async processing components")
except ImportError as e:
    print(f"✗ Failed to import components: {e}")
    sys.exit(1)


async def test_connection_pool():
    """Test async connection pool functionality."""
    print("\n--- Testing AsyncConnectionPool ---")
    
    try:
        config = ConnectionPoolConfig(max_connections=10, min_connections=2)
        pool = AsyncConnectionPool(config)
        await pool.start()
        
        # Test basic connection usage
        connections_used = []
        async def use_connection():
            async with pool.get_connection() as conn_id:
                connections_used.append(conn_id)
                await asyncio.sleep(0.01)
        
        # Test concurrent connections
        tasks = [use_connection() for _ in range(5)]
        await asyncio.gather(*tasks)
        
        assert len(connections_used) == 5, f"Expected 5 connections, got {len(connections_used)}"
        
        # Test stats
        stats = pool.get_stats()
        assert stats.total_requests == 5, f"Expected 5 total requests, got {stats.total_requests}"
        assert stats.failed_requests == 0, f"Expected 0 failed requests, got {stats.failed_requests}"
        
        await pool.stop()
        print("✓ AsyncConnectionPool tests passed")
        return True
        
    except Exception as e:
        print(f"✗ AsyncConnectionPool test failed: {e}")
        return False


async def test_concurrency_manager():
    """Test concurrency manager functionality."""
    print("\n--- Testing ConcurrencyManager ---")
    
    try:
        manager = ConcurrencyManager(
            max_concurrent_requests=5,
            max_requests_per_second=10.0,
            burst_limit=5
        )
        
        request_times = []
        async def throttled_request():
            start_time = time.time()
            async with manager.throttle_request():
                await asyncio.sleep(0.01)
            end_time = time.time()
            request_times.append(end_time - start_time)
        
        # Test concurrent requests
        tasks = [throttled_request() for _ in range(3)]
        await asyncio.gather(*tasks)
        
        assert len(request_times) == 3, f"Expected 3 requests, got {len(request_times)}"
        
        # Test load stats
        load_stats = manager.get_current_load()
        assert "concurrent_requests" in load_stats, "Missing concurrent_requests in load stats"
        assert "rate_limit_utilization" in load_stats, "Missing rate_limit_utilization in load stats"
        
        print("✓ ConcurrencyManager tests passed")
        return True
        
    except Exception as e:
        print(f"✗ ConcurrencyManager test failed: {e}")
        return False


def test_app_creation():
    """Test FastAPI app creation with async resources."""
    print("\n--- Testing FastAPI App Creation ---")
    
    try:
        config = DTESNConfig()
        app = create_app(config=config, enable_async_resources=True)
        
        assert app is not None, "App creation failed"
        assert hasattr(app.state, 'connection_pool'), "Connection pool not initialized"
        assert hasattr(app.state, 'concurrency_manager'), "Concurrency manager not initialized"
        
        print("✓ FastAPI app creation tests passed")
        return True
        
    except Exception as e:
        print(f"✗ FastAPI app creation test failed: {e}")
        return False


async def test_dtesn_processor():
    """Test enhanced DTESN processor functionality."""
    print("\n--- Testing Enhanced DTESN Processor ---")
    
    try:
        from aphrodite.endpoints.deep_tree_echo.dtesn_processor import DTESNProcessor
        
        config = DTESNConfig()
        processor = DTESNProcessor(config=config, max_concurrent_processes=5)
        
        # Test basic processing
        result = await processor.process("test input", membrane_depth=2, esn_size=64)
        
        assert result is not None, "Processing result is None"
        assert result.input_data == "test input", f"Input data mismatch: {result.input_data}"
        assert result.membrane_layers == 2, f"Membrane layers mismatch: {result.membrane_layers}"
        
        # Test batch processing
        batch_results = await processor.process_batch(
            ["input1", "input2", "input3"],
            membrane_depth=2,
            esn_size=64
        )
        
        assert len(batch_results) == 3, f"Expected 3 batch results, got {len(batch_results)}"
        
        # Test stats
        stats = processor.get_processing_stats()
        assert "total_requests" in stats, "Missing total_requests in stats"
        assert stats["total_requests"] >= 4, f"Expected at least 4 requests, got {stats['total_requests']}"
        
        await processor.cleanup_resources()
        print("✓ Enhanced DTESN Processor tests passed")
        return True
        
    except Exception as e:
        print(f"✗ Enhanced DTESN Processor test failed: {e}")
        return False


async def main():
    """Run all validation tests."""
    print("=== Async Server-Side Processing Validation ===")
    
    tests = [
        test_connection_pool(),
        test_concurrency_manager(),
        test_dtesn_processor(),
    ]
    
    sync_tests = [
        test_app_creation(),
    ]
    
    # Run async tests
    results = await asyncio.gather(*tests, return_exceptions=True)
    
    # Run sync tests
    sync_results = []
    for test in sync_tests:
        try:
            sync_results.append(test)
        except Exception as e:
            print(f"Sync test error: {e}")
            sync_results.append(False)
    
    # Combine results
    all_results = []
    for result in results:
        if isinstance(result, Exception):
            print(f"Async test exception: {result}")
            all_results.append(False)
        else:
            all_results.append(result)
    
    all_results.extend(sync_results)
    
    passed = sum(1 for r in all_results if r is True)
    total = len(all_results)
    
    print("\n=== Validation Results ===")
    print(f"Passed: {passed}/{total}")
    
    if passed == total:
        print("✓ ALL TESTS PASSED - Async server-side processing implementation is working!")
        return True
    else:
        print("✗ Some tests failed - Review implementation")
        return False


if __name__ == "__main__":
    success = asyncio.run(main())
    sys.exit(0 if success else 1)