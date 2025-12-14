"""Simple test for batch manager functionality."""

import asyncio
import sys
import os

# Add the current directory to path for imports
sys.path.insert(0, os.path.dirname(__file__))

from aphrodite.endpoints.deep_tree_echo.batch_manager import (
    BatchConfiguration, 
    DynamicBatchManager,
    BatchingMetrics
)

def test_batch_configuration():
    """Test batch configuration creation."""
    config = BatchConfiguration(
        min_batch_size=2,
        max_batch_size=16,
        target_batch_size=8,
        max_batch_wait_ms=30.0
    )
    
    assert config.min_batch_size == 2
    assert config.max_batch_size == 16
    assert config.target_batch_size == 8
    assert config.max_batch_wait_ms == 30.0
    
    print("✓ Batch configuration test passed")


def test_batching_metrics():
    """Test batching metrics structure."""
    metrics = BatchingMetrics(
        requests_processed=100,
        avg_batch_size=8.5,
        throughput_improvement=25.0
    )
    
    assert metrics.requests_processed == 100
    assert metrics.avg_batch_size == 8.5
    assert metrics.throughput_improvement == 25.0
    
    print("✓ Batching metrics test passed")


async def test_dynamic_batch_manager():
    """Test dynamic batch manager initialization."""
    config = BatchConfiguration(target_batch_size=4)
    
    def mock_load_tracker():
        return 0.5  # 50% load
    
    manager = DynamicBatchManager(
        config=config,
        load_tracker=mock_load_tracker
    )
    
    # Test basic functionality
    assert manager.config == config
    assert manager.load_tracker == mock_load_tracker
    assert manager._current_batch_size == config.target_batch_size
    
    # Test dynamic batch size calculation
    batch_size = manager._calculate_dynamic_batch_size()
    assert config.min_batch_size <= batch_size <= config.max_batch_size
    
    print("✓ Dynamic batch manager test passed")


async def test_load_integration():
    """Test server load integration."""
    from aphrodite.endpoints.deep_tree_echo.load_integration import (
        ServerLoadTracker,
        LoadMetrics
    )
    
    tracker = ServerLoadTracker(update_interval=0.1, history_window=10)
    
    # Test basic load calculation
    load = tracker.get_current_load()
    assert 0.0 <= load <= 1.0
    
    # Test custom load provider
    def custom_provider():
        return 0.7
    
    tracker.add_load_provider(custom_provider, weight=1.0)
    load = tracker.get_current_load()
    assert load > 0.0
    
    # Test load metrics
    metrics = tracker.get_load_metrics()
    assert isinstance(metrics, LoadMetrics)
    
    print("✓ Load integration test passed")


async def test_batch_size_adaptation():
    """Test batch size adaptation logic."""
    config = BatchConfiguration(
        min_batch_size=1,
        max_batch_size=32,
        target_batch_size=8,
        enable_adaptive_sizing=True
    )
    
    # Test with different load scenarios
    load_scenarios = [0.2, 0.5, 0.8]  # Low, medium, high load
    
    for load in load_scenarios:
        manager = DynamicBatchManager(
            config=config,
            load_tracker=lambda: load
        )
        
        batch_size = manager._calculate_dynamic_batch_size()
        assert config.min_batch_size <= batch_size <= config.max_batch_size
        
        print(f"  Load {load}: batch size {batch_size}")
    
    print("✓ Batch size adaptation test passed")


async def main():
    """Run all tests."""
    print("Running batch system tests...")
    
    try:
        # Run synchronous tests
        test_batch_configuration()
        test_batching_metrics()
        
        # Run async tests
        await test_dynamic_batch_manager()
        await test_load_integration()
        await test_batch_size_adaptation()
        
        print("\n🎉 All batch system tests passed!")
        return True
        
    except Exception as e:
        print(f"\n❌ Test failed: {e}")
        import traceback
        traceback.print_exc()
        return False


if __name__ == "__main__":
    success = asyncio.run(main())
    sys.exit(0 if success else 1)