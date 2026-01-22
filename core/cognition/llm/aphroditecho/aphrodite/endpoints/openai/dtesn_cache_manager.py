import asyncio
import hashlib
import json
import logging
import time
from typing import Any, Dict, List, Optional, Set, Tuple, Union
from dataclasses import dataclass, asdict
from enum import Enum
import pickle
import zlib
from collections import OrderedDict, defaultdict
try:
    import aioredis
    REDIS_AVAILABLE = True
except ImportError:
    REDIS_AVAILABLE = False
    aioredis = None
logger = logging.getLogger(__name__)
class CacheStrategy(Enum):
    AGGRESSIVE = 'aggressive'
    BALANCED = 'balanced'
    CONSERVATIVE = 'conservative'
    DYNAMIC = 'dynamic'
@dataclass
class DTESNCacheKey:
    input_hash: str
    model_id: str
    dtesn_config_hash: str
    membrane_depth: int
    esn_size: int
    def to_string(self) -> str:
        return f'dtesn:{self.model_id}:{self.membrane_depth}:{self.esn_size}:{self.input_hash[:16]}:{self.dtesn_config_hash[:8]}'
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
        return max(0.0, 1.0 - self.avg_cache_retrieval_time_ms / self.avg_processing_time_ms)
class DTESNServerSideCacheManager:
    def __init__(self, max_memory_entries: int=1000, max_compressed_entries: int=5000, redis_url: Optional[str]=None, default_ttl_seconds: int=3600, cache_strategy: CacheStrategy=CacheStrategy.BALANCED, enable_compression: bool=True, compression_threshold: int=1024):
        self.max_memory_entries = max_memory_entries
        self.max_compressed_entries = max_compressed_entries
        self.default_ttl_seconds = default_ttl_seconds
        self.cache_strategy = cache_strategy
        self.enable_compression = enable_compression
        self.compression_threshold = compression_threshold
        self.memory_cache: OrderedDict[str, DTESNCacheEntry] = OrderedDict()
        self.compressed_cache: OrderedDict[str, bytes] = OrderedDict()
        self.cache_metadata: Dict[str, DTESNCacheEntry] = {}
        self.redis_url = redis_url
        self.redis_client: Optional[aioredis.Redis] = None
        self.redis_enabled = False
        self.content_tags_index: Dict[str, Set[str]] = defaultdict(set)
        self.model_dependency_graph: Dict[str, Set[str]] = defaultdict(set)
        self.metrics = CacheMetrics()
        self.processing_times: List[float] = []
        self.cache_times: List[float] = []
        self.cleanup_task: Optional[asyncio.Task] = None
        self.metrics_task: Optional[asyncio.Task] = None
        logger.info(f'DTESN Server-side Cache Manager initialized with strategy: {cache_strategy.value}')
    async def initialize(self) -> None:
        if self.redis_url and REDIS_AVAILABLE:
            try:
                self.redis_client = aioredis.from_url(self.redis_url, decode_responses=False)
                await self.redis_client.ping()
                self.redis_enabled = True
                logger.info('✅ Connected to Redis for distributed DTESN caching')
            except Exception as e:
                logger.warning(f'Redis connection failed: {e}')
                self.redis_client = None
        self.cleanup_task = asyncio.create_task(self._cleanup_expired_entries())
        self.metrics_task = asyncio.create_task(self._collect_performance_metrics())
        logger.info('DTESN Cache Manager fully initialized')
    async def shutdown(self) -> None:
        logger.info('Shutting down DTESN Cache Manager...')
        for task in [self.cleanup_task, self.metrics_task]:
            if task and (not task.done()):
                task.cancel()
                try:
                    await task
                except asyncio.CancelledError:
                    pass
        if self.redis_client:
            await self.redis_client.close()
        logger.info('DTESN Cache Manager shutdown complete')
    def _generate_cache_key(self, input_data: Union[str, Dict[str, Any]], model_id: str, dtesn_config: Dict[str, Any]) -> DTESNCacheKey:
        input_str = json.dumps(input_data, sort_keys=True) if isinstance(input_data, dict) else str(input_data)
        input_hash = hashlib.sha256(input_str.encode()).hexdigest()
        config_str = json.dumps(dtesn_config, sort_keys=True)
        config_hash = hashlib.md5(config_str.encode()).hexdigest()
        return DTESNCacheKey(input_hash=input_hash, model_id=model_id, dtesn_config_hash=config_hash, membrane_depth=dtesn_config.get('membrane_depth', 4), esn_size=dtesn_config.get('esn_size', 512))
    async def get_cached_result(self, input_data: Union[str, Dict[str, Any]], model_id: str, dtesn_config: Dict[str, Any]) -> Optional[Tuple[Dict[str, Any], Dict[str, Any]]]:
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
                logger.debug(f'L1 cache hit for key: {cache_key[:32]}...')
                return (entry.result, entry.metadata)
            else:
                self._remove_from_all_caches(cache_key)
        if cache_key in self.compressed_cache and cache_key in self.cache_metadata:
            entry_metadata = self.cache_metadata[cache_key]
            if not entry_metadata.is_expired():
                try:
                    compressed_data = self.compressed_cache[cache_key]
                    decompressed_data = zlib.decompress(compressed_data)
                    result_data = pickle.loads(decompressed_data)
                    entry_metadata.touch()
                    self.compressed_cache.move_to_end(cache_key)
                    self.metrics.cache_hits += 1
                    await self._promote_to_memory_cache(cache_key, entry_metadata, result_data)
                    retrieval_time = (time.time() - start_time) * 1000
                    self.cache_times.append(retrieval_time)
                    logger.debug(f'L2 cache hit for key: {cache_key[:32]}...')
                    return (result_data, entry_metadata.metadata)
                except Exception as e:
                    logger.error(f'L2 cache decompression failed: {e}')
                    self._remove_from_all_caches(cache_key)
            else:
                self._remove_from_all_caches(cache_key)
        if self.redis_enabled and self.redis_client:
            try:
                redis_key = f'dtesn:{cache_key}'
                redis_data = await self.redis_client.get(redis_key)
                if redis_data:
                    try:
                        cache_entry_dict = pickle.loads(redis_data)
                        entry = DTESNCacheEntry(**cache_entry_dict)
                        if not entry.is_expired():
                            entry.touch()
                            self.metrics.redis_hits += 1
                            self.metrics.cache_hits += 1
                            await self._promote_to_local_caches(cache_key, entry)
                            retrieval_time = (time.time() - start_time) * 1000
                            self.cache_times.append(retrieval_time)
                            logger.debug(f'Redis cache hit for key: {cache_key[:32]}...')
                            return (entry.result, entry.metadata)
                        else:
                            await self.redis_client.delete(redis_key)
                    except Exception as e:
                        logger.error(f'Redis cache deserialization failed: {e}')
                        await self.redis_client.delete(redis_key)
                else:
                    self.metrics.redis_misses += 1
            except Exception as e:
                logger.error(f'Redis cache retrieval failed: {e}')
        self.metrics.cache_misses += 1
        logger.debug(f'Cache miss for key: {cache_key[:32]}...')
        return None
    async def cache_result(self, input_data: Union[str, Dict[str, Any]], model_id: str, dtesn_config: Dict[str, Any], result: Dict[str, Any], metadata: Dict[str, Any], processing_time_ms: float, content_tags: Optional[Set[str]]=None) -> None:
        cache_key_obj = self._generate_cache_key(input_data, model_id, dtesn_config)
        cache_key = cache_key_obj.to_string()
        content_tags = content_tags or set()
        ttl = self._determine_ttl(processing_time_ms, content_tags)
        cache_entry = DTESNCacheEntry(key=cache_key_obj, result=result, metadata=metadata, created_at=time.time(), last_accessed=time.time(), access_count=1, processing_time_ms=processing_time_ms, content_tags=content_tags, ttl_seconds=ttl)
        self.processing_times.append(processing_time_ms)
        if len(self.processing_times) > 1000:
            self.processing_times = self.processing_times[-500:]
        await self._store_in_cache_levels(cache_key, cache_entry)
        for tag in content_tags:
            self.content_tags_index[tag].add(cache_key)
        logger.debug(f'Cached result for key: {cache_key[:32]}... (TTL: {ttl}s)')
    async def invalidate_by_tags(self, tags: Set[str]) -> int:
        invalidated_count = 0
        keys_to_invalidate = set()
        for tag in tags:
            keys_to_invalidate.update(self.content_tags_index.get(tag, set()))
        for cache_key in keys_to_invalidate:
            if await self._invalidate_cache_entry(cache_key):
                invalidated_count += 1
        for tag in tags:
            self.content_tags_index[tag].clear()
        self.metrics.invalidations += invalidated_count
        logger.info(f'Invalidated {invalidated_count} cache entries by tags: {tags}')
        return invalidated_count
    async def invalidate_by_model(self, model_id: str) -> int:
        invalidated_count = 0
        keys_to_invalidate = []
        for key, entry in self.memory_cache.items():
            if entry.key.model_id == model_id:
                keys_to_invalidate.append(key)
        for key, entry in self.cache_metadata.items():
            if entry.key.model_id == model_id:
                keys_to_invalidate.append(key)
        for cache_key in keys_to_invalidate:
            if await self._invalidate_cache_entry(cache_key):
                invalidated_count += 1
        if self.redis_enabled and self.redis_client:
            try:
                redis_pattern = f'dtesn:*{model_id}*'
                async for key in self.redis_client.scan_iter(match=redis_pattern):
                    await self.redis_client.delete(key)
                    invalidated_count += 1
            except Exception as e:
                logger.error(f'Redis pattern invalidation failed: {e}')
        self.metrics.invalidations += invalidated_count
        logger.info(f'Invalidated {invalidated_count} cache entries for model: {model_id}')
        return invalidated_count
    def get_performance_metrics(self) -> Dict[str, Any]:
        if self.processing_times:
            self.metrics.avg_processing_time_ms = sum(self.processing_times) / len(self.processing_times)
        if self.cache_times:
            self.metrics.avg_cache_retrieval_time_ms = sum(self.cache_times) / len(self.cache_times)
        memory_size = sum((len(pickle.dumps(entry)) for entry in self.memory_cache.values()))
        compressed_size = sum((len(data) for data in self.compressed_cache.values()))
        self.metrics.memory_usage_bytes = memory_size + compressed_size
        return {**asdict(self.metrics), 'cache_levels': {'memory_entries': len(self.memory_cache), 'compressed_entries': len(self.compressed_cache), 'redis_enabled': self.redis_enabled}, 'performance_improvement_percent': self.metrics.performance_improvement * 100, 'cache_strategy': self.cache_strategy.value}
    def _determine_ttl(self, processing_time_ms: float, content_tags: Set[str]) -> int:
        base_ttl = self.default_ttl_seconds
        if self.cache_strategy == CacheStrategy.AGGRESSIVE:
            return base_ttl * 2
        elif self.cache_strategy == CacheStrategy.CONSERVATIVE:
            return base_ttl // 2
        elif self.cache_strategy == CacheStrategy.DYNAMIC:
            if processing_time_ms > 1000:
                return base_ttl * 3
            elif processing_time_ms > 100:
                return base_ttl
            else:
                return base_ttl // 3
        else:
            return base_ttl
    async def _store_in_cache_levels(self, cache_key: str, entry: DTESNCacheEntry) -> None:
        await self._store_in_memory_cache(cache_key, entry)
        if self.redis_enabled and self.redis_client:
            if entry.processing_time_ms > 50 or self.cache_strategy in [CacheStrategy.AGGRESSIVE, CacheStrategy.BALANCED]:
                await self._store_in_redis(cache_key, entry)
    async def _store_in_memory_cache(self, cache_key: str, entry: DTESNCacheEntry) -> None:
        while len(self.memory_cache) >= self.max_memory_entries:
            self._evict_lru_memory_entry()
        self.memory_cache[cache_key] = entry
        self.cache_metadata[cache_key] = entry
    async def _store_in_compressed_cache(self, cache_key: str, entry: DTESNCacheEntry) -> None:
        if not self.enable_compression:
            return
        try:
            serialized = pickle.dumps(entry.result)
            if len(serialized) >= self.compression_threshold:
                compressed = zlib.compress(serialized, level=6)
                while len(self.compressed_cache) >= self.max_compressed_entries:
                    self._evict_lru_compressed_entry()
                self.compressed_cache[cache_key] = compressed
                self.cache_metadata[cache_key] = entry
                logger.debug(f'Compressed cache entry: {len(serialized)} -> {len(compressed)} bytes')
        except Exception as e:
            logger.error(f'Compression failed for cache entry: {e}')
    async def _store_in_redis(self, cache_key: str, entry: DTESNCacheEntry) -> None:
        try:
            redis_key = f'dtesn:{cache_key}'
            redis_value = pickle.dumps(asdict(entry))
            if entry.ttl_seconds:
                await self.redis_client.setex(redis_key, entry.ttl_seconds, redis_value)
            else:
                await self.redis_client.set(redis_key, redis_value)
        except Exception as e:
            logger.error(f'Redis storage failed: {e}')
    async def _promote_to_memory_cache(self, cache_key: str, entry: DTESNCacheEntry, result_data: Dict[str, Any]) -> None:
        if len(self.memory_cache) < self.max_memory_entries:
            entry.result = result_data
            self.memory_cache[cache_key] = entry
            logger.debug(f'Promoted cache entry to L1: {cache_key[:32]}...')
    async def _promote_to_local_caches(self, cache_key: str, entry: DTESNCacheEntry) -> None:
        if len(self.memory_cache) < self.max_memory_entries:
            self.memory_cache[cache_key] = entry
            self.cache_metadata[cache_key] = entry
        else:
            await self._store_in_compressed_cache(cache_key, entry)
    def _evict_lru_memory_entry(self) -> None:
        if self.memory_cache:
            evicted_key, _ = self.memory_cache.popitem(last=False)
            if evicted_key in self.cache_metadata:
                del self.cache_metadata[evicted_key]
            self.metrics.evictions += 1
            logger.debug(f'Evicted LRU memory entry: {evicted_key[:32]}...')
    def _evict_lru_compressed_entry(self) -> None:
        if self.compressed_cache:
            evicted_key, _ = self.compressed_cache.popitem(last=False)
            if evicted_key in self.cache_metadata:
                del self.cache_metadata[evicted_key]
            self.metrics.evictions += 1
            logger.debug(f'Evicted LRU compressed entry: {evicted_key[:32]}...')
    async def _invalidate_cache_entry(self, cache_key: str) -> bool:
        invalidated = False
        if cache_key in self.memory_cache:
            del self.memory_cache[cache_key]
            invalidated = True
        if cache_key in self.compressed_cache:
            del self.compressed_cache[cache_key]
            invalidated = True
        if cache_key in self.cache_metadata:
            del self.cache_metadata[cache_key]
        if self.redis_enabled and self.redis_client:
            try:
                redis_key = f'dtesn:{cache_key}'
                await self.redis_client.delete(redis_key)
                invalidated = True
            except Exception as e:
                logger.error(f'Redis invalidation failed: {e}')
        return invalidated
    def _remove_from_all_caches(self, cache_key: str) -> None:
        if cache_key in self.memory_cache:
            del self.memory_cache[cache_key]
        if cache_key in self.compressed_cache:
            del self.compressed_cache[cache_key]
        if cache_key in self.cache_metadata:
            del self.cache_metadata[cache_key]
    async def _cleanup_expired_entries(self) -> None:
        while True:
            try:
                await asyncio.sleep(300)
                expired_keys = []
                current_time = time.time()
                for key, entry in self.memory_cache.items():
                    if entry.is_expired():
                        expired_keys.append(key)
                for key, entry in self.cache_metadata.items():
                    if key not in expired_keys and entry.is_expired():
                        expired_keys.append(key)
                for key in expired_keys:
                    self._remove_from_all_caches(key)
                if expired_keys:
                    logger.info(f'Cleaned up {len(expired_keys)} expired cache entries')
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f'Cache cleanup error: {e}')
    async def _collect_performance_metrics(self) -> None:
        while True:
            try:
                await asyncio.sleep(60)
                metrics = self.get_performance_metrics()
                logger.info(f"DTESN Cache Performance - Hit Ratio: {metrics['hit_ratio']:.2%}, Performance Improvement: {metrics['performance_improvement_percent']:.1f}%, Memory Entries: {metrics['cache_levels']['memory_entries']}, Compressed Entries: {metrics['cache_levels']['compressed_entries']}")
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f'Metrics collection error: {e}')
_global_cache_manager: Optional[DTESNServerSideCacheManager] = None
def get_cache_manager() -> Optional[DTESNServerSideCacheManager]:
    return _global_cache_manager
def initialize_global_cache_manager(**kwargs) -> DTESNServerSideCacheManager:
    global _global_cache_manager
    _global_cache_manager = DTESNServerSideCacheManager(**kwargs)
    return _global_cache_manager
async def shutdown_global_cache_manager() -> None:
    global _global_cache_manager
    if _global_cache_manager:
        await _global_cache_manager.shutdown()
        _global_cache_manager = None