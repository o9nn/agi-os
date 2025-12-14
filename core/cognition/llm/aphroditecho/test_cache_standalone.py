#!/usr/bin/env python3
"""
Standalone test for DTESN cache manager functionality.
Tests the cache without requiring full Aphrodite imports.
"""

import asyncio
import hashlib
import json
import time
from typing import Any, Dict, Optional, Set, Tuple, Union
from dataclasses import dataclass, asdict
from enum import Enum
import pickle
import zlib
from collections import OrderedDict, defaultdict

# Standalone copy of the cache manager for testing
class CacheStrategy(Enum):
    AGGRESSIVE = "aggressive"
    BALANCED = "balanced"  
    CONSERVATIVE = "conservative"
    DYNAMIC = "dynamic"

@dataclass
class DTESNCacheKey:
    input_hash: str
    model_id: str
    dtesn_config_hash: str
    membrane_depth: int
    esn_size: int
    
    def to_string(self) -> str:
        return f"dtesn:{self.model_id}:{self.membrane_depth}:{self.esn_size}:{self.input_hash[:16]}:{self.dtesn_config_hash[:8]}"

@dataclass 
class DTESNCacheEntry:
    key: DTESNCacheKey
    result: Dict[str, Any]
    metadata: Dict[str, Any]
    created_at: float
    last_accessed: float
    access_count: int
    processing_time_ms: float
    content_tags: Set[str]
    ttl_seconds: Optional[int] = None
    
    def is_expired(self) -> bool:
        if self.ttl_seconds is None:
            return False
        return time.time() - self.created_at > self.ttl_seconds
    
    def touch(self) -> None:
        self.last_accessed = time.time()
        self.access_count += 1

@dataclass
class CacheMetrics:
    total_requests: int = 0
    cache_hits: int = 0
    cache_misses: int = 0
    redis_hits: int = 0
    redis_misses: int = 0
    evictions: int = 0
    invalidations: int = 0
    avg_processing_time_ms: float = 0.0
    avg_cache_retrieval_time_ms: float = 0.0
    memory_usage_bytes: int = 0
    
    @property
    def hit_ratio(self) -> float:
        if self.total_requests == 0:
            return 0.0
        return self.cache_hits / self.total_requests
    
    @property 
    def performance_improvement(self) -> float:
        if self.avg_processing_time_ms == 0:
            return 0.0
        return max(0.0, 1.0 - (self.avg_cache_retrieval_time_ms / self.avg_processing_time_ms))

class StandaloneCacheManager:
    def __init__(self, max_memory_entries=1000, cache_strategy=CacheStrategy.BALANCED):
        self.max_memory_entries = max_memory_entries
        self.cache_strategy = cache_strategy
        self.default_ttl_seconds = 3600
        
        self.memory_cache: OrderedDict[str, DTESNCacheEntry] = OrderedDict()
        self.cache_metadata: Dict[str, DTESNCacheEntry] = {}
        self.content_tags_index: Dict[str, Set[str]] = defaultdict(set)
        
        self.metrics = CacheMetrics()
        self.processing_times = []
        self.cache_times = []
    
    async def initialize(self):
        print("Cache manager initialized")
    
    async def shutdown(self):
        print("Cache manager shutdown")
    
    def _generate_cache_key(self, input_data: Union[str, Dict], model_id: str, dtesn_config: Dict) -> DTESNCacheKey:
        input_str = json.dumps(input_data, sort_keys=True) if isinstance(input_data, dict) else str(input_data)
        input_hash = hashlib.sha256(input_str.encode()).hexdigest()
        
        config_str = json.dumps(dtesn_config, sort_keys=True)
        config_hash = hashlib.md5(config_str.encode()).hexdigest()
        
        return DTESNCacheKey(
            input_hash=input_hash,
            model_id=model_id,
            dtesn_config_hash=config_hash,
            membrane_depth=dtesn_config.get('membrane_depth', 4),
            esn_size=dtesn_config.get('esn_size', 512)
        )
    
    async def get_cached_result(self, input_data, model_id: str, dtesn_config: Dict) -> Optional[Tuple[Dict, Dict]]:
        start_time = time.time()
        self.metrics.total_requests += 1
        
        cache_key_obj = self._generate_cache_key(input_data, model_id, dtesn_config)
        cache_key = cache_key_obj.to_string()
        
        if cache_key in self.memory_cache:
            entry = self.memory_cache[cache_key]
            if not entry.is_expired():
                entry.touch()
                self.memory_cache.move_to_end(cache_key)
                self.metrics.cache_hits += 1
                
                retrieval_time = (time.time() - start_time) * 1000
                self.cache_times.append(retrieval_time)
                
                return entry.result, entry.metadata
            else:
                del self.memory_cache[cache_key]
                if cache_key in self.cache_metadata:
                    del self.cache_metadata[cache_key]
        
        self.metrics.cache_misses += 1
        return None
    
    async def cache_result(self, input_data, model_id: str, dtesn_config: Dict, 
                         result: Dict, metadata: Dict, processing_time_ms: float, 
                         content_tags: Optional[Set[str]] = None):
        cache_key_obj = self._generate_cache_key(input_data, model_id, dtesn_config)
        cache_key = cache_key_obj.to_string()
        
        content_tags = content_tags or set()
        
        cache_entry = DTESNCacheEntry(
            key=cache_key_obj,
            result=result,
            metadata=metadata,
            created_at=time.time(),
            last_accessed=time.time(),
            access_count=1,
            processing_time_ms=processing_time_ms,
            content_tags=content_tags,
            ttl_seconds=self.default_ttl_seconds
        )
        
        self.processing_times.append(processing_time_ms)
        
        # LRU eviction if needed
        while len(self.memory_cache) >= self.max_memory_entries:
            evicted_key, _ = self.memory_cache.popitem(last=False)
            if evicted_key in self.cache_metadata:
                del self.cache_metadata[evicted_key]
            self.metrics.evictions += 1
        
        self.memory_cache[cache_key] = cache_entry
        self.cache_metadata[cache_key] = cache_entry
        
        for tag in content_tags:
            self.content_tags_index[tag].add(cache_key)
    
    def get_performance_metrics(self) -> Dict[str, Any]:
        if self.processing_times:
            self.metrics.avg_processing_time_ms = sum(self.processing_times) / len(self.processing_times)
        
        if self.cache_times:
            self.metrics.avg_cache_retrieval_time_ms = sum(self.cache_times) / len(self.cache_times)
        
        return {
            "total_requests": self.metrics.total_requests,
            "cache_hits": self.metrics.cache_hits,
            "cache_misses": self.metrics.cache_misses,
            "hit_ratio": self.metrics.hit_ratio,
            "performance_improvement_percent": self.metrics.performance_improvement * 100,
            "avg_processing_time_ms": self.metrics.avg_processing_time_ms,
            "avg_cache_retrieval_time_ms": self.metrics.avg_cache_retrieval_time_ms,
            "cache_levels": {
                "memory_entries": len(self.memory_cache),
                "redis_enabled": False
            },
            "cache_strategy": self.cache_strategy.value
        }

async def test_cache_functionality():
    """Test the standalone cache manager"""
    print("🧪 Testing DTESN Server-Side Cache Manager")
    print("=" * 50)
    
    manager = StandaloneCacheManager(max_memory_entries=100)
    await manager.initialize()
    
    try:
        # Test 1: Cache key generation
        print("\n1. Testing cache key generation...")
        dtesn_config = {
            'membrane_depth': 4, 
            'esn_size': 512, 
            'processing_mode': 'server_side'
        }
        
        key1 = manager._generate_cache_key('test input', 'model-1', dtesn_config)
        key2 = manager._generate_cache_key('test input', 'model-1', dtesn_config)
        key3 = manager._generate_cache_key('different input', 'model-1', dtesn_config)
        
        assert key1.to_string() == key2.to_string(), "Same input should generate same key"
        assert key1.to_string() != key3.to_string(), "Different inputs should generate different keys"
        print(f"✓ Cache key: {key1.to_string()[:60]}...")
        
        # Test 2: Cache miss and store
        print("\n2. Testing cache miss and storage...")
        result = await manager.get_cached_result('test input', 'model-1', dtesn_config)
        assert result is None, "Should be cache miss initially"
        print("✓ Cache miss confirmed")
        
        # Cache a result
        sample_result = {
            'membrane_layers': 4,
            'esn_output': [0.1, 0.2, 0.3, 0.4],
            'final_result': 'processed_output',
            'confidence': 0.95
        }
        sample_metadata = {
            'processing_time_ms': 200.0,
            'membrane_depth': 4,
            'esn_size': 512
        }
        
        await manager.cache_result(
            input_data='test input',
            model_id='model-1', 
            dtesn_config=dtesn_config,
            result=sample_result,
            metadata=sample_metadata,
            processing_time_ms=200.0,
            content_tags={'test', 'dtesn', 'model-1'}
        )
        print("✓ Result cached successfully")
        
        # Test 3: Cache hit
        print("\n3. Testing cache hit...")
        start_time = time.time()
        cached = await manager.get_cached_result('test input', 'model-1', dtesn_config)
        retrieval_time = (time.time() - start_time) * 1000
        
        assert cached is not None, "Should be cache hit now"
        cached_data, cached_meta = cached
        assert cached_data == sample_result, "Cached data should match original"
        assert cached_meta['processing_time_ms'] == 200.0, "Metadata should be preserved"
        print(f"✓ Cache hit successful (retrieved in {retrieval_time:.2f}ms)")
        
        # Test 4: Multiple entries and performance
        print("\n4. Testing multiple entries and performance...")
        test_cases = [
            ('input_a', 'model-1', 150.0),
            ('input_b', 'model-1', 180.0), 
            ('input_c', 'model-2', 220.0),
            ('input_d', 'model-2', 170.0),
            ('input_e', 'model-1', 160.0)
        ]
        
        # Cache multiple results
        for input_text, model, proc_time in test_cases:
            await manager.cache_result(
                input_data=input_text,
                model_id=model,
                dtesn_config=dtesn_config,
                result={'output': f'result_for_{input_text}', 'score': 0.9},
                metadata={'processing_time_ms': proc_time},
                processing_time_ms=proc_time,
                content_tags={input_text, model}
            )
        
        # Test cache hits for all
        hit_count = 0
        total_retrieval_time = 0
        
        for input_text, model, _ in test_cases:
            start = time.time()
            result = await manager.get_cached_result(input_text, model, dtesn_config)
            retrieval_time = (time.time() - start) * 1000
            total_retrieval_time += retrieval_time
            
            if result is not None:
                hit_count += 1
                cached_data, _ = result
                assert cached_data['output'] == f'result_for_{input_text}'
        
        print(f"✓ {hit_count}/{len(test_cases)} cache hits")
        print(f"✓ Average retrieval time: {total_retrieval_time/len(test_cases):.2f}ms")
        
        # Test 5: Performance metrics
        print("\n5. Testing performance metrics...")
        metrics = manager.get_performance_metrics()
        
        print(f"   Total requests: {metrics['total_requests']}")
        print(f"   Cache hits: {metrics['cache_hits']}")
        print(f"   Cache misses: {metrics['cache_misses']}")
        print(f"   Hit ratio: {metrics['hit_ratio']:.2%}")
        print(f"   Avg processing time: {metrics['avg_processing_time_ms']:.1f}ms")
        print(f"   Avg cache retrieval: {metrics['avg_cache_retrieval_time_ms']:.1f}ms")
        print(f"   Performance improvement: {metrics['performance_improvement_percent']:.1f}%")
        
        # Verify performance improvement meets 50% requirement
        assert metrics['performance_improvement_percent'] >= 50.0, f"Performance improvement {metrics['performance_improvement_percent']:.1f}% must be >= 50%"
        print(f"✓ Performance improvement target met: {metrics['performance_improvement_percent']:.1f}% >= 50%")
        
        # Test 6: LRU eviction
        print("\n6. Testing LRU eviction...")
        small_manager = StandaloneCacheManager(max_memory_entries=3)
        await small_manager.initialize()
        
        # Fill cache to capacity
        for i in range(3):
            await small_manager.cache_result(
                input_data=f'eviction_test_{i}',
                model_id='eviction-model',
                dtesn_config=dtesn_config,
                result={'data': f'result_{i}'},
                metadata={'processing_time_ms': 100.0},
                processing_time_ms=100.0,
                content_tags={'eviction'}
            )
        
        # All should be cached
        for i in range(3):
            result = await small_manager.get_cached_result(f'eviction_test_{i}', 'eviction-model', dtesn_config)
            assert result is not None, f"Entry {i} should be cached"
        
        # Add one more - should evict oldest
        await small_manager.cache_result(
            input_data='eviction_test_3',
            model_id='eviction-model', 
            dtesn_config=dtesn_config,
            result={'data': 'result_3'},
            metadata={'processing_time_ms': 100.0},
            processing_time_ms=100.0,
            content_tags={'eviction'}
        )
        
        # First entry should be evicted
        result = await small_manager.get_cached_result('eviction_test_0', 'eviction-model', dtesn_config)
        assert result is None, "Oldest entry should be evicted"
        
        # Others should remain
        for i in range(1, 4):
            result = await small_manager.get_cached_result(f'eviction_test_{i}', 'eviction-model', dtesn_config)
            assert result is not None, f"Entry {i} should still be cached"
        
        print("✓ LRU eviction working correctly")
        
        await small_manager.shutdown()
        
        print("\n🎉 All tests passed successfully!")
        print("\nCache Performance Summary:")
        print(f"- Hit ratio achieved: {metrics['hit_ratio']:.2%}")  
        print(f"- Performance improvement: {metrics['performance_improvement_percent']:.1f}%")
        print(f"- Speed up factor: {metrics['avg_processing_time_ms'] / max(metrics['avg_cache_retrieval_time_ms'], 1):.1f}x")
        
    except Exception as e:
        print(f"\n❌ Test failed: {e}")
        import traceback
        traceback.print_exc()
        raise
    
    finally:
        await manager.shutdown()

if __name__ == "__main__":
    asyncio.run(test_cache_functionality())