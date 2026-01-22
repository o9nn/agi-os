import gc
import math
import time
import unittest
from unittest.mock import Mock, patch
from typing import Dict, Any
import torch
import pytest
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))
from aphrodite.worker.memory_pool import MemoryPool, get_memory_pool, reset_memory_pool
from aphrodite.common.sampling_pool import SamplingParamsPool, get_sampling_params_pool, create_optimized_sampling_params
from aphrodite.worker.dtesn_memory_manager import DTESNMemoryManager, get_dtesn_memory_manager
from aphrodite.common.sampling_params import SamplingParams
class TestMemoryPool(unittest.TestCase):
    def setUp(self):
        self.pool = MemoryPool(max_pool_size=128 * 1024 * 1024, enable_dtesn=False, cleanup_interval=1.0)
    def tearDown(self):
        self.pool.clear_pool()
        gc.collect()
    def test_basic_allocation(self):
        tensor1 = self.pool.allocate(1000, torch.float32, 'cpu')
        self.assertIsNotNone(tensor1)
        self.assertEqual(tensor1.shape, (1000,))
        self.assertEqual(tensor1.dtype, torch.float32)
        stats = self.pool.get_memory_stats()
        self.assertEqual(stats['pool_stats']['allocation_count'], 1)
        self.assertGreater(stats['pool_stats']['current_usage_mb'], 0)
    def test_tensor_reuse(self):
        tensor1 = self.pool.allocate(1000, torch.float32, 'cpu')
        tensor1_id = id(tensor1)
        self.pool.deallocate(tensor1)
        tensor2 = self.pool.allocate(1000, torch.float32, 'cpu')
        stats = self.pool.get_memory_stats()
        self.assertGreater(stats['pool_stats']['cache_hits'], 0)
    def test_different_sizes(self):
        sizes = [100, 1000, 10000, 100000]
        tensors = []
        for size in sizes:
            tensor = self.pool.allocate(size, torch.float32, 'cpu')
            self.assertIsNotNone(tensor)
            self.assertEqual(tensor.numel(), size)
            tensors.append(tensor)
        for tensor in tensors:
            self.pool.deallocate(tensor)
        stats = self.pool.get_memory_stats()
        self.assertEqual(stats['pool_stats']['allocation_count'], len(sizes))
        self.assertEqual(stats['pool_stats']['deallocation_count'], len(sizes))
    def test_memory_pressure(self):
        large_tensors = []
        tensor_size = 1024 * 1024
        for i in range(20):
            tensor = self.pool.allocate(tensor_size, torch.float32, 'cpu')
            if tensor is not None:
                large_tensors.append(tensor)
        stats = self.pool.get_memory_stats()
        utilization = stats['pool_state']['utilization']
        for tensor in large_tensors:
            self.pool.deallocate(tensor, force=True)
    def test_dtesn_integration(self):
        dtesn_pool = MemoryPool(max_pool_size=64 * 1024 * 1024, enable_dtesn=True, cleanup_interval=1.0)
        try:
            tensor1 = dtesn_pool.allocate(1000, torch.float32, 'cpu')
            tensor2 = dtesn_pool.allocate(10000, torch.float32, 'cpu')
            self.assertIsNotNone(tensor1)
            self.assertIsNotNone(tensor2)
            stats = dtesn_pool.get_memory_stats()
            if 'dtesn_levels' in stats:
                self.assertIsInstance(stats['dtesn_levels'], dict)
        finally:
            dtesn_pool.clear_pool()
    def test_cleanup_mechanisms(self):
        tensors = []
        for i in range(10):
            tensor = self.pool.allocate(1000, torch.float32, 'cpu')
            tensors.append(tensor)
        for i in range(5):
            self.pool.deallocate(tensors[i])
        time.sleep(1.1)
        new_tensor = self.pool.allocate(500, torch.float32, 'cpu')
        for i in range(5, 10):
            self.pool.deallocate(tensors[i])
        if new_tensor:
            self.pool.deallocate(new_tensor)
class TestSamplingParamsPool(unittest.TestCase):
    def setUp(self):
        self.pool = SamplingParamsPool(max_pool_size=100, cleanup_interval=1.0, max_age=5.0)
    def tearDown(self):
        self.pool.clear_pool()
    def test_basic_parameter_creation(self):
        params1 = self.pool.get_or_create(temperature=0.7, top_p=0.9)
        params2 = self.pool.get_or_create(temperature=0.7, top_p=0.9)
        self.assertIs(params1, params2)
        stats = self.pool.get_stats()
        self.assertEqual(stats['cache_hits'], 1)
        self.assertGreater(stats['deduplication_rate'], 0)
    def test_different_parameters(self):
        params1 = self.pool.get_or_create(temperature=0.7, top_p=0.9)
        params2 = self.pool.get_or_create(temperature=0.8, top_p=0.9)
        params3 = self.pool.get_or_create(temperature=0.7, top_k=50)
        self.assertIsNot(params1, params2)
        self.assertIsNot(params1, params3)
        self.assertIsNot(params2, params3)
        self.assertEqual(params1.temperature, 0.7)
        self.assertEqual(params2.temperature, 0.8)
        self.assertEqual(params3.top_k, 50)
    def test_parameter_validation(self):
        params = self.pool.get_or_create(temperature=0.5, max_tokens=100)
        self.assertIsNotNone(params)
        with self.assertRaises(Exception):
            self.pool.get_or_create(temperature=-1.0)
    def test_hash_consistency(self):
        hash1 = self.pool._generate_hash({'temperature': 0.7, 'top_p': 0.9})
        hash2 = self.pool._generate_hash({'top_p': 0.9, 'temperature': 0.7})
        self.assertEqual(hash1, hash2)
    def test_default_value_handling(self):
        params1 = self.pool.get_or_create()
        params2 = self.pool.get_or_create(temperature=1.0, top_p=1.0)
        stats = self.pool.get_stats()
        self.assertGreater(stats['cache_hit_rate'], 0)
    def test_compact_encoding(self):
        original_params = self.pool.get_or_create(temperature=0.7, top_p=0.9, max_tokens=100, stop=['END', 'STOP'])
        encoded = self.pool.create_compact_encoding(original_params)
        decoded_params = self.pool.decode_compact_encoding(encoded)
        self.assertEqual(original_params.temperature, decoded_params.temperature)
        self.assertEqual(original_params.top_p, decoded_params.top_p)
        self.assertEqual(original_params.max_tokens, decoded_params.max_tokens)
        self.assertEqual(original_params.stop, decoded_params.stop)
    def test_cleanup_old_parameters(self):
        params_list = []
        for i in range(20):
            params = self.pool.get_or_create(temperature=0.5 + i * 0.01)
            params_list.append(params)
        time.sleep(1.1)
        self.pool.force_cleanup()
        stats = self.pool.get_stats()
        self.assertLessEqual(stats['pool_size'], self.pool.max_pool_size)
class TestDTESNMemoryManager(unittest.TestCase):
    def setUp(self):
        self.manager = DTESNMemoryManager(total_memory_limit=256 * 1024 * 1024, max_hierarchy_depth=6, enable_embodied_memory=False)
    def tearDown(self):
        self.manager.clear_all_memory()
    def test_hierarchy_initialization(self):
        self.assertGreater(len(self.manager.levels), 0)
        self.assertLessEqual(len(self.manager.levels), self.manager.max_hierarchy_depth)
        for level, level_info in self.manager.levels.items():
            expected_count = self.manager.OEIS_A000081[level]
            self.assertEqual(level_info.expected_membranes, expected_count)
    def test_level_determination(self):
        small_level = self.manager._determine_allocation_level(1024, 'procedural')
        large_level = self.manager._determine_allocation_level(1024 * 1024, 'procedural')
        self.assertLessEqual(small_level, large_level)
    def test_memory_type_allocation(self):
        episodic_level = self.manager._determine_allocation_level(1024 * 100, 'episodic')
        semantic_level = self.manager._determine_allocation_level(1024 * 100, 'semantic')
        procedural_level = self.manager._determine_allocation_level(1024 * 100, 'procedural')
        self.assertGreaterEqual(semantic_level, episodic_level)
        self.assertLessEqual(procedural_level, episodic_level)
    def test_tensor_allocation(self):
        tensor = self.manager.allocate_tensor(size=(100, 100), dtype=torch.float32, device='cpu', memory_type='procedural')
        self.assertIsNotNone(tensor)
        self.assertEqual(tensor.shape, (100, 100))
        stats = self.manager.get_memory_stats()
        self.assertEqual(stats['global_stats']['total_allocations'], 1)
        self.assertGreater(stats['memory_usage']['current_usage_mb'], 0)
    def test_hierarchical_allocation(self):
        tensors = []
        sizes = [100, 1000, 10000, 100000]
        for size in sizes:
            tensor = self.manager.allocate_tensor(size=size, dtype=torch.float32, device='cpu', memory_type='procedural')
            if tensor is not None:
                tensors.append(tensor)
        stats = self.manager.get_memory_stats()
        allocated_levels = sum((1 for level_stats in stats['dtesn_levels'].values() if level_stats['allocated_membranes'] > 0))
        self.assertGreater(allocated_levels, 0)
        for tensor in tensors:
            self.manager.deallocate_tensor(tensor)
    def test_memory_reuse(self):
        tensor1 = self.manager.allocate_tensor(1000, torch.float32, 'cpu')
        self.assertIsNotNone(tensor1)
        self.manager.deallocate_tensor(tensor1)
        tensor2 = self.manager.allocate_tensor(1000, torch.float32, 'cpu')
        self.assertIsNotNone(tensor2)
        self.manager.deallocate_tensor(tensor2)
    def test_memory_pressure_handling(self):
        large_tensors = []
        tensor_size = 1024 * 1024
        for i in range(100):
            tensor = self.manager.allocate_tensor(size=tensor_size, dtype=torch.float32, device='cpu', memory_type='procedural')
            if tensor is not None:
                large_tensors.append(tensor)
            else:
                break
        stats = self.manager.get_memory_stats()
        utilization = stats['memory_usage']['utilization']
        for tensor in large_tensors:
            self.manager.deallocate_tensor(tensor, force=True)
    def test_oeis_compliance_tracking(self):
        tensor = self.manager.allocate_tensor(1000, torch.float32, 'cpu')
        if tensor is not None:
            stats = self.manager.get_memory_stats()
            self.assertGreaterEqual(stats['global_stats']['oeis_compliant_allocations'], 0)
            self.manager.deallocate_tensor(tensor)
class TestIntegration(unittest.TestCase):
    def setUp(self):
        reset_memory_pool()
    def tearDown(self):
        reset_memory_pool()
    def test_global_memory_pool_access(self):
        pool1 = get_memory_pool()
        pool2 = get_memory_pool()
        self.assertIs(pool1, pool2)
        tensor = pool1.allocate(1000, torch.float32, 'cpu')
        self.assertIsNotNone(tensor)
        pool1.deallocate(tensor)
    def test_sampling_params_optimization(self):
        params1 = create_optimized_sampling_params(temperature=0.7, top_p=0.9)
        params2 = create_optimized_sampling_params(temperature=0.7, top_p=0.9)
        self.assertIs(params1, params2)
    @patch('aphrodite.worker.dtesn_memory_manager._HAS_DTESN_CORE', False)
    def test_dtesn_fallback_behavior(self):
        manager = DTESNMemoryManager(enable_embodied_memory=True)
        self.assertFalse(manager.enable_embodied_memory)
        self.assertIsNone(manager.embodied_memory)
        tensor = manager.allocate_tensor(100, torch.float32, 'cpu')
        self.assertIsNotNone(tensor)
        manager.deallocate_tensor(tensor)
    def test_memory_optimization_effectiveness(self):
        param_sets = []
        for i in range(100):
            temp = 0.7 if i % 10 < 5 else 0.8
            top_p = 0.9 if i % 5 < 3 else 0.95
            params = create_optimized_sampling_params(temperature=temp, top_p=top_p)
            param_sets.append(params)
        pool = get_sampling_params_pool()
        stats = pool.get_stats()
        self.assertGreater(stats['deduplication_rate'], 0)
        self.assertLess(stats['pool_size'], 100)
class TestPerformanceMetrics(unittest.TestCase):
    def test_memory_pool_performance(self):
        pool = MemoryPool(max_pool_size=64 * 1024 * 1024, enable_dtesn=False)
        try:
            start_time = time.time()
            tensors = []
            for i in range(100):
                tensor = pool.allocate(1000, torch.float32, 'cpu')
                tensors.append(tensor)
            allocation_time = time.time() - start_time
            start_time = time.time()
            for tensor in tensors:
                pool.deallocate(tensor)
            deallocation_time = time.time() - start_time
            self.assertLess(allocation_time, 1.0)
            self.assertLess(deallocation_time, 1.0)
            stats = pool.get_memory_stats()
            self.assertGreater(stats['pool_stats']['pool_efficiency'], 0)
        finally:
            pool.clear_pool()
    def test_dtesn_memory_efficiency(self):
        manager = DTESNMemoryManager(total_memory_limit=128 * 1024 * 1024, max_hierarchy_depth=6, enable_embodied_memory=False)
        try:
            tensors = []
            sizes = [100, 500, 1000, 5000, 10000] * 10
            start_time = time.time()
            for size in sizes:
                tensor = manager.allocate_tensor(size, torch.float32, 'cpu')
                if tensor is not None:
                    tensors.append(tensor)
            allocation_time = time.time() - start_time
            stats = manager.get_memory_stats()
            utilization = stats['memory_usage']['utilization']
            self.assertLess(allocation_time, 2.0)
            self.assertLess(utilization, 1.0)
            for tensor in tensors:
                manager.deallocate_tensor(tensor)
        finally:
            manager.clear_all_memory()
if __name__ == '__main__':
    import logging
    logging.basicConfig(level=logging.WARNING)
    unittest.main(verbosity=2)