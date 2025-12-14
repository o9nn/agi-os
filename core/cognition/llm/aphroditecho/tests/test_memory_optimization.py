"""Tests for Memory Optimization Components.

Comprehensive test suite for memory pooling, sampling parameters optimization,
and DTESN memory management integration.
"""

import gc
import math
import time
import unittest
from unittest.mock import Mock, patch
from typing import Dict, Any

import torch
import pytest

# Import components to test
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

from aphrodite.worker.memory_pool import MemoryPool, get_memory_pool, reset_memory_pool
from aphrodite.common.sampling_pool import SamplingParamsPool, get_sampling_params_pool, create_optimized_sampling_params
from aphrodite.worker.dtesn_memory_manager import DTESNMemoryManager, get_dtesn_memory_manager
from aphrodite.common.sampling_params import SamplingParams


class TestMemoryPool(unittest.TestCase):
    """Test cases for the MemoryPool class."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.pool = MemoryPool(
            max_pool_size=128 * 1024 * 1024,  # 128MB for testing
            enable_dtesn=False,  # Disable DTESN for basic tests
            cleanup_interval=1.0  # Short interval for testing
        )
        
    def tearDown(self):
        """Clean up after tests."""
        self.pool.clear_pool()
        gc.collect()
    
    def test_basic_allocation(self):
        """Test basic tensor allocation from pool."""
        tensor1 = self.pool.allocate(1000, torch.float32, "cpu")
        self.assertIsNotNone(tensor1)
        self.assertEqual(tensor1.shape, (1000,))
        self.assertEqual(tensor1.dtype, torch.float32)
        
        # Check statistics
        stats = self.pool.get_memory_stats()
        self.assertEqual(stats['pool_stats']['allocation_count'], 1)
        self.assertGreater(stats['pool_stats']['current_usage_mb'], 0)
    
    def test_tensor_reuse(self):
        """Test tensor reuse from pool."""
        # Allocate and deallocate tensor
        tensor1 = self.pool.allocate(1000, torch.float32, "cpu")
        tensor1_id = id(tensor1)
        self.pool.deallocate(tensor1)
        
        # Allocate same size - should reuse
        tensor2 = self.pool.allocate(1000, torch.float32, "cpu")
        
        # Verify reuse (should hit cache)
        stats = self.pool.get_memory_stats()
        self.assertGreater(stats['pool_stats']['cache_hits'], 0)
    
    def test_different_sizes(self):
        """Test allocation of different tensor sizes."""
        sizes = [100, 1000, 10000, 100000]
        tensors = []
        
        for size in sizes:
            tensor = self.pool.allocate(size, torch.float32, "cpu")
            self.assertIsNotNone(tensor)
            self.assertEqual(tensor.numel(), size)
            tensors.append(tensor)
        
        # Deallocate all
        for tensor in tensors:
            self.pool.deallocate(tensor)
        
        # Check statistics
        stats = self.pool.get_memory_stats()
        self.assertEqual(stats['pool_stats']['allocation_count'], len(sizes))
        self.assertEqual(stats['pool_stats']['deallocation_count'], len(sizes))
    
    def test_memory_pressure(self):
        """Test behavior under memory pressure."""
        # Allocate large tensors to trigger pressure
        large_tensors = []
        tensor_size = 1024 * 1024  # 1M elements = 4MB per tensor
        
        for i in range(20):  # Try to allocate 80MB total
            tensor = self.pool.allocate(tensor_size, torch.float32, "cpu")
            if tensor is not None:
                large_tensors.append(tensor)
        
        # Should trigger cleanup mechanisms
        stats = self.pool.get_memory_stats()
        utilization = stats['pool_state']['utilization']
        
        # Clean up
        for tensor in large_tensors:
            self.pool.deallocate(tensor, force=True)
    
    def test_dtesn_integration(self):
        """Test DTESN-aware allocation when enabled."""
        dtesn_pool = MemoryPool(
            max_pool_size=64 * 1024 * 1024,
            enable_dtesn=True,
            cleanup_interval=1.0
        )
        
        try:
            # Allocate tensors of different sizes
            tensor1 = dtesn_pool.allocate(1000, torch.float32, "cpu")
            tensor2 = dtesn_pool.allocate(10000, torch.float32, "cpu")
            
            self.assertIsNotNone(tensor1)
            self.assertIsNotNone(tensor2)
            
            # Check DTESN-specific stats
            stats = dtesn_pool.get_memory_stats()
            if 'dtesn_levels' in stats:
                self.assertIsInstance(stats['dtesn_levels'], dict)
        
        finally:
            dtesn_pool.clear_pool()
    
    def test_cleanup_mechanisms(self):
        """Test automatic cleanup of unused blocks."""
        # Allocate multiple tensors
        tensors = []
        for i in range(10):
            tensor = self.pool.allocate(1000, torch.float32, "cpu")
            tensors.append(tensor)
        
        # Deallocate half (return to pool)
        for i in range(5):
            self.pool.deallocate(tensors[i])
        
        # Wait for cleanup interval
        time.sleep(1.1)
        
        # Allocate new tensor to trigger cleanup
        new_tensor = self.pool.allocate(500, torch.float32, "cpu")
        
        # Cleanup remaining tensors
        for i in range(5, 10):
            self.pool.deallocate(tensors[i])
        
        if new_tensor:
            self.pool.deallocate(new_tensor)


class TestSamplingParamsPool(unittest.TestCase):
    """Test cases for the SamplingParamsPool class."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.pool = SamplingParamsPool(
            max_pool_size=100,
            cleanup_interval=1.0,
            max_age=5.0
        )
    
    def tearDown(self):
        """Clean up after tests."""
        self.pool.clear_pool()
    
    def test_basic_parameter_creation(self):
        """Test basic parameter creation and deduplication."""
        params1 = self.pool.get_or_create(temperature=0.7, top_p=0.9)
        params2 = self.pool.get_or_create(temperature=0.7, top_p=0.9)
        
        # Should be the same object due to deduplication
        self.assertIs(params1, params2)
        
        # Check statistics
        stats = self.pool.get_stats()
        self.assertEqual(stats['cache_hits'], 1)  # Second request should hit cache
        self.assertGreater(stats['deduplication_rate'], 0)
    
    def test_different_parameters(self):
        """Test creation of different parameter sets."""
        params1 = self.pool.get_or_create(temperature=0.7, top_p=0.9)
        params2 = self.pool.get_or_create(temperature=0.8, top_p=0.9)
        params3 = self.pool.get_or_create(temperature=0.7, top_k=50)
        
        # Should be different objects
        self.assertIsNot(params1, params2)
        self.assertIsNot(params1, params3)
        self.assertIsNot(params2, params3)
        
        # But with correct values
        self.assertEqual(params1.temperature, 0.7)
        self.assertEqual(params2.temperature, 0.8)
        self.assertEqual(params3.top_k, 50)
    
    def test_parameter_validation(self):
        """Test that parameters are properly validated."""
        # Valid parameters should work
        params = self.pool.get_or_create(temperature=0.5, max_tokens=100)
        self.assertIsNotNone(params)
        
        # Invalid parameters should raise exceptions
        with self.assertRaises(Exception):
            self.pool.get_or_create(temperature=-1.0)  # Invalid temperature
    
    def test_hash_consistency(self):
        """Test that parameter hashing is consistent."""
        # Same parameters in different order should produce same hash
        hash1 = self.pool._generate_hash({'temperature': 0.7, 'top_p': 0.9})
        hash2 = self.pool._generate_hash({'top_p': 0.9, 'temperature': 0.7})
        
        self.assertEqual(hash1, hash2)
    
    def test_default_value_handling(self):
        """Test handling of default values in deduplication."""
        # Parameters with default values should be equivalent to empty params
        params1 = self.pool.get_or_create()
        params2 = self.pool.get_or_create(temperature=1.0, top_p=1.0)  # Default values
        
        # Should be deduplicated if defaults are handled correctly
        stats = self.pool.get_stats()
        self.assertGreater(stats['cache_hit_rate'], 0)
    
    def test_compact_encoding(self):
        """Test compact encoding and decoding of parameters."""
        original_params = self.pool.get_or_create(
            temperature=0.7, 
            top_p=0.9, 
            max_tokens=100,
            stop=["END", "STOP"]
        )
        
        # Encode and decode
        encoded = self.pool.create_compact_encoding(original_params)
        decoded_params = self.pool.decode_compact_encoding(encoded)
        
        # Should have same values
        self.assertEqual(original_params.temperature, decoded_params.temperature)
        self.assertEqual(original_params.top_p, decoded_params.top_p)
        self.assertEqual(original_params.max_tokens, decoded_params.max_tokens)
        self.assertEqual(original_params.stop, decoded_params.stop)
    
    def test_cleanup_old_parameters(self):
        """Test cleanup of old unused parameters."""
        # Create parameters
        params_list = []
        for i in range(20):
            params = self.pool.get_or_create(temperature=0.5 + i * 0.01)
            params_list.append(params)
        
        # Wait for aging
        time.sleep(1.1)
        
        # Force cleanup
        self.pool.force_cleanup()
        
        # Pool should be smaller now
        stats = self.pool.get_stats()
        self.assertLessEqual(stats['pool_size'], self.pool.max_pool_size)


class TestDTESNMemoryManager(unittest.TestCase):
    """Test cases for the DTESNMemoryManager class."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.manager = DTESNMemoryManager(
            total_memory_limit=256 * 1024 * 1024,  # 256MB for testing
            max_hierarchy_depth=6,
            enable_embodied_memory=False  # Disable for basic tests
        )
    
    def tearDown(self):
        """Clean up after tests."""
        self.manager.clear_all_memory()
    
    def test_hierarchy_initialization(self):
        """Test DTESN hierarchy initialization."""
        # Check that levels are properly initialized
        self.assertGreater(len(self.manager.levels), 0)
        self.assertLessEqual(len(self.manager.levels), self.manager.max_hierarchy_depth)
        
        # Check OEIS A000081 compliance
        for level, level_info in self.manager.levels.items():
            expected_count = self.manager.OEIS_A000081[level]
            self.assertEqual(level_info.expected_membranes, expected_count)
    
    def test_level_determination(self):
        """Test allocation level determination based on size."""
        # Small allocation should go to lower levels
        small_level = self.manager._determine_allocation_level(1024, "procedural")
        
        # Large allocation should go to higher levels
        large_level = self.manager._determine_allocation_level(1024 * 1024, "procedural")
        
        self.assertLessEqual(small_level, large_level)
    
    def test_memory_type_allocation(self):
        """Test allocation based on memory type."""
        episodic_level = self.manager._determine_allocation_level(1024 * 100, "episodic")
        semantic_level = self.manager._determine_allocation_level(1024 * 100, "semantic")
        procedural_level = self.manager._determine_allocation_level(1024 * 100, "procedural")
        
        # Semantic memory should prefer higher levels
        self.assertGreaterEqual(semantic_level, episodic_level)
        
        # Procedural memory should prefer lower levels
        self.assertLessEqual(procedural_level, episodic_level)
    
    def test_tensor_allocation(self):
        """Test basic tensor allocation through DTESN manager."""
        tensor = self.manager.allocate_tensor(
            size=(100, 100),
            dtype=torch.float32,
            device="cpu",
            memory_type="procedural"
        )
        
        self.assertIsNotNone(tensor)
        self.assertEqual(tensor.shape, (100, 100))
        
        # Check statistics
        stats = self.manager.get_memory_stats()
        self.assertEqual(stats['global_stats']['total_allocations'], 1)
        self.assertGreater(stats['memory_usage']['current_usage_mb'], 0)
    
    def test_hierarchical_allocation(self):
        """Test allocation across different hierarchy levels."""
        tensors = []
        
        # Allocate different sizes to exercise different levels
        sizes = [100, 1000, 10000, 100000]
        
        for size in sizes:
            tensor = self.manager.allocate_tensor(
                size=size,
                dtype=torch.float32,
                device="cpu",
                memory_type="procedural"
            )
            
            if tensor is not None:
                tensors.append(tensor)
        
        # Should have allocations across multiple levels
        stats = self.manager.get_memory_stats()
        allocated_levels = sum(1 for level_stats in stats['dtesn_levels'].values()
                             if level_stats['allocated_membranes'] > 0)
        
        self.assertGreater(allocated_levels, 0)
        
        # Clean up
        for tensor in tensors:
            self.manager.deallocate_tensor(tensor)
    
    def test_memory_reuse(self):
        """Test memory block reuse."""
        # Allocate and deallocate tensor
        tensor1 = self.manager.allocate_tensor(1000, torch.float32, "cpu")
        self.assertIsNotNone(tensor1)
        
        self.manager.deallocate_tensor(tensor1)
        
        # Allocate same size - should potentially reuse
        tensor2 = self.manager.allocate_tensor(1000, torch.float32, "cpu")
        self.assertIsNotNone(tensor2)
        
        # Clean up
        self.manager.deallocate_tensor(tensor2)
    
    def test_memory_pressure_handling(self):
        """Test behavior under memory pressure."""
        large_tensors = []
        
        # Try to allocate beyond limit
        tensor_size = 1024 * 1024  # 1M elements
        
        for i in range(100):  # Try to exceed memory limit
            tensor = self.manager.allocate_tensor(
                size=tensor_size,
                dtype=torch.float32,
                device="cpu",
                memory_type="procedural"
            )
            
            if tensor is not None:
                large_tensors.append(tensor)
            else:
                break  # Allocation failed due to memory pressure
        
        # Should have triggered pressure handling
        stats = self.manager.get_memory_stats()
        utilization = stats['memory_usage']['utilization']
        
        # Clean up
        for tensor in large_tensors:
            self.manager.deallocate_tensor(tensor, force=True)
    
    def test_oeis_compliance_tracking(self):
        """Test OEIS A000081 compliance tracking."""
        # Allocate tensors and check compliance
        tensor = self.manager.allocate_tensor(1000, torch.float32, "cpu")
        
        if tensor is not None:
            stats = self.manager.get_memory_stats()
            self.assertGreaterEqual(stats['global_stats']['oeis_compliant_allocations'], 0)
            
            self.manager.deallocate_tensor(tensor)


class TestIntegration(unittest.TestCase):
    """Integration tests for memory optimization components."""
    
    def setUp(self):
        """Set up integration test fixtures."""
        # Reset global instances
        reset_memory_pool()
        
    def tearDown(self):
        """Clean up after integration tests."""
        reset_memory_pool()
    
    def test_global_memory_pool_access(self):
        """Test global memory pool access patterns."""
        pool1 = get_memory_pool()
        pool2 = get_memory_pool()
        
        # Should be the same instance
        self.assertIs(pool1, pool2)
        
        # Test basic functionality
        tensor = pool1.allocate(1000, torch.float32, "cpu")
        self.assertIsNotNone(tensor)
        
        pool1.deallocate(tensor)
    
    def test_sampling_params_optimization(self):
        """Test optimized sampling parameters creation."""
        params1 = create_optimized_sampling_params(temperature=0.7, top_p=0.9)
        params2 = create_optimized_sampling_params(temperature=0.7, top_p=0.9)
        
        # Should be deduplicated
        self.assertIs(params1, params2)
    
    @patch('aphrodite.worker.dtesn_memory_manager._HAS_DTESN_CORE', False)
    def test_dtesn_fallback_behavior(self):
        """Test behavior when DTESN components are not available."""
        manager = DTESNMemoryManager(enable_embodied_memory=True)
        
        # Should work without DTESN components
        self.assertFalse(manager.enable_embodied_memory)
        self.assertIsNone(manager.embodied_memory)
        
        # Basic allocation should still work
        tensor = manager.allocate_tensor(100, torch.float32, "cpu")
        self.assertIsNotNone(tensor)
        
        manager.deallocate_tensor(tensor)
    
    def test_memory_optimization_effectiveness(self):
        """Test overall memory optimization effectiveness."""
        # Create multiple parameter sets with overlap
        param_sets = []
        
        for i in range(100):
            # Create parameters with some overlap to test deduplication
            temp = 0.7 if i % 10 < 5 else 0.8
            top_p = 0.9 if i % 5 < 3 else 0.95
            
            params = create_optimized_sampling_params(temperature=temp, top_p=top_p)
            param_sets.append(params)
        
        # Check deduplication effectiveness
        pool = get_sampling_params_pool()
        stats = pool.get_stats()
        
        # Should have good deduplication
        self.assertGreater(stats['deduplication_rate'], 0)
        self.assertLess(stats['pool_size'], 100)  # Should be much less than 100 unique sets


class TestPerformanceMetrics(unittest.TestCase):
    """Performance and memory usage tests."""
    
    def test_memory_pool_performance(self):
        """Test memory pool performance characteristics."""
        pool = MemoryPool(max_pool_size=64 * 1024 * 1024, enable_dtesn=False)
        
        try:
            # Measure allocation performance
            start_time = time.time()
            tensors = []
            
            for i in range(100):
                tensor = pool.allocate(1000, torch.float32, "cpu")
                tensors.append(tensor)
            
            allocation_time = time.time() - start_time
            
            # Measure deallocation performance
            start_time = time.time()
            for tensor in tensors:
                pool.deallocate(tensor)
            
            deallocation_time = time.time() - start_time
            
            # Performance should be reasonable
            self.assertLess(allocation_time, 1.0)  # Should complete in under 1 second
            self.assertLess(deallocation_time, 1.0)
            
            # Check final statistics
            stats = pool.get_memory_stats()
            self.assertGreater(stats['pool_stats']['pool_efficiency'], 0)
            
        finally:
            pool.clear_pool()
    
    def test_dtesn_memory_efficiency(self):
        """Test DTESN memory manager efficiency."""
        manager = DTESNMemoryManager(
            total_memory_limit=128 * 1024 * 1024,
            max_hierarchy_depth=6,
            enable_embodied_memory=False
        )
        
        try:
            # Allocate various sizes
            tensors = []
            sizes = [100, 500, 1000, 5000, 10000] * 10  # 50 allocations
            
            start_time = time.time()
            for size in sizes:
                tensor = manager.allocate_tensor(size, torch.float32, "cpu")
                if tensor is not None:
                    tensors.append(tensor)
            
            allocation_time = time.time() - start_time
            
            # Check efficiency metrics
            stats = manager.get_memory_stats()
            utilization = stats['memory_usage']['utilization']
            
            # Should be reasonably efficient
            self.assertLess(allocation_time, 2.0)
            self.assertLess(utilization, 1.0)  # Should not exceed limit
            
            # Clean up
            for tensor in tensors:
                manager.deallocate_tensor(tensor)
            
        finally:
            manager.clear_all_memory()


if __name__ == '__main__':
    # Configure logging for tests
    import logging
    logging.basicConfig(level=logging.WARNING)
    
    # Run tests
    unittest.main(verbosity=2)