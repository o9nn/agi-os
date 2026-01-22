import asyncio
import hashlib
import json
import logging
import time
from typing import Any, Dict, List, Optional, Set
from dataclasses import dataclass
from collections import OrderedDict, defaultdict
import pickle
import zlib
try:
    import aioredis
    REDIS_AVAILABLE = True
except ImportError:
    REDIS_AVAILABLE = False
    aioredis = None
logger = logging.getLogger(__name__)
@dataclass
class TemplateCacheMetrics:
    cache_hits: int = 0
    cache_misses: int = 0
    template_compilations: int = 0
    rendered_results_cached: int = 0
    cache_evictions: int = 0
    total_render_time_ms: float = 0.0
    total_cache_time_ms: float = 0.0
    avg_template_size_bytes: float = 0.0
    memory_usage_bytes: int = 0
@dataclass
class CacheEntry:
    key: str
    content: Any
    creation_time: float
    last_access: float
    access_count: int
    size_bytes: int
    ttl_seconds: Optional[float] = None
    content_hash: str = ''
    compression_ratio: float = 1.0
class DTESNTemplateCacheManager:
    def __init__(self, max_template_cache_size: int=100, max_rendered_cache_size: int=500, default_ttl_seconds: float=3600, enable_compression: bool=True, redis_url: Optional[str]=None):
        self.max_template_cache_size = max_template_cache_size
        self.max_rendered_cache_size = max_rendered_cache_size
        self.default_ttl_seconds = default_ttl_seconds
        self.enable_compression = enable_compression
        self.template_cache: OrderedDict[str, CacheEntry] = OrderedDict()
        self.rendered_cache: OrderedDict[str, CacheEntry] = OrderedDict()
        self.metrics = TemplateCacheMetrics()
        self.invalidation_tags: Dict[str, Set[str]] = defaultdict(set)
        self.redis_client = None
        if redis_url and REDIS_AVAILABLE:
            asyncio.create_task(self._init_redis(redis_url))
        self.performance_history: List[Dict[str, Any]] = []
        self.max_history_length = 100
    async def _init_redis(self, redis_url: str):
        try:
            self.redis_client = await aioredis.from_url(redis_url)
            logger.info('Redis client initialized for template caching')
        except Exception as e:
            logger.warning(f'Redis initialization failed: {e}. Using in-memory caching only.')
            self.redis_client = None
    async def get_compiled_template(self, template_key: str) -> Optional[Any]:
        start_time = time.time()
        if template_key in self.template_cache:
            entry = self.template_cache[template_key]
            if self._is_expired(entry):
                del self.template_cache[template_key]
                self.metrics.cache_evictions += 1
            else:
                entry.last_access = time.time()
                entry.access_count += 1
                self.template_cache.move_to_end(template_key)
                self.metrics.cache_hits += 1
                self.metrics.total_cache_time_ms += (time.time() - start_time) * 1000
                return entry.content
        if self.redis_client:
            try:
                redis_key = f'template_compiled:{template_key}'
                cached_data = await self.redis_client.get(redis_key)
                if cached_data:
                    template_data = self._deserialize_content(cached_data)
                    await self._store_template_local(template_key, template_data)
                    self.metrics.cache_hits += 1
                    self.metrics.total_cache_time_ms += (time.time() - start_time) * 1000
                    return template_data
            except Exception as e:
                logger.warning(f'Redis template retrieval failed: {e}')
        self.metrics.cache_misses += 1
        return None
    async def store_compiled_template(self, template_key: str, compiled_template: Any, ttl_seconds: Optional[float]=None, invalidation_tags: Optional[Set[str]]=None):
        start_time = time.time()
        ttl = ttl_seconds or self.default_ttl_seconds
        invalidation_tags = invalidation_tags or set()
        content_str = str(compiled_template)
        content_hash = hashlib.sha256(content_str.encode()).hexdigest()[:16]
        size_bytes = len(content_str.encode())
        entry = CacheEntry(key=template_key, content=compiled_template, creation_time=time.time(), last_access=time.time(), access_count=1, size_bytes=size_bytes, ttl_seconds=ttl, content_hash=content_hash)
        await self._store_template_local(template_key, compiled_template, entry)
        if self.redis_client:
            try:
                redis_key = f'template_compiled:{template_key}'
                serialized_data = self._serialize_content(compiled_template)
                await self.redis_client.setex(redis_key, int(ttl), serialized_data)
            except Exception as e:
                logger.warning(f'Redis template storage failed: {e}')
        for tag in invalidation_tags:
            self.invalidation_tags[tag].add(template_key)
        self.metrics.template_compilations += 1
        self.metrics.total_cache_time_ms += (time.time() - start_time) * 1000
        self._update_size_metrics()
    async def get_rendered_result(self, result_key: str) -> Optional[str]:
        start_time = time.time()
        if result_key in self.rendered_cache:
            entry = self.rendered_cache[result_key]
            if self._is_expired(entry):
                del self.rendered_cache[result_key]
                self.metrics.cache_evictions += 1
            else:
                entry.last_access = time.time()
                entry.access_count += 1
                self.rendered_cache.move_to_end(result_key)
                self.metrics.cache_hits += 1
                self.metrics.total_cache_time_ms += (time.time() - start_time) * 1000
                return entry.content
        if self.redis_client:
            try:
                redis_key = f'template_rendered:{result_key}'
                cached_data = await self.redis_client.get(redis_key)
                if cached_data:
                    html_content = self._deserialize_content(cached_data)
                    await self._store_rendered_local(result_key, html_content)
                    self.metrics.cache_hits += 1
                    self.metrics.total_cache_time_ms += (time.time() - start_time) * 1000
                    return html_content
            except Exception as e:
                logger.warning(f'Redis rendered result retrieval failed: {e}')
        self.metrics.cache_misses += 1
        return None
    async def store_rendered_result(self, result_key: str, html_content: str, ttl_seconds: Optional[float]=None, invalidation_tags: Optional[Set[str]]=None):
        start_time = time.time()
        ttl = ttl_seconds or self.default_ttl_seconds
        invalidation_tags = invalidation_tags or set()
        content_hash = hashlib.sha256(html_content.encode()).hexdigest()[:16]
        original_size = len(html_content.encode())
        stored_content = html_content
        compression_ratio = 1.0
        if self.enable_compression and original_size > 1024:
            try:
                compressed = zlib.compress(html_content.encode(), level=6)
                compression_ratio = len(compressed) / original_size
                if compression_ratio < 0.8:
                    stored_content = compressed
                else:
                    stored_content = html_content
                    compression_ratio = 1.0
            except Exception as e:
                logger.warning(f'Compression failed for {result_key}: {e}')
                stored_content = html_content
                compression_ratio = 1.0
        entry = CacheEntry(key=result_key, content=stored_content, creation_time=time.time(), last_access=time.time(), access_count=1, size_bytes=len(str(stored_content).encode()) if isinstance(stored_content, str) else len(stored_content), ttl_seconds=ttl, content_hash=content_hash, compression_ratio=compression_ratio)
        await self._store_rendered_local(result_key, html_content, entry)
        if self.redis_client:
            try:
                redis_key = f'template_rendered:{result_key}'
                serialized_data = self._serialize_content(stored_content)
                await self.redis_client.setex(redis_key, int(ttl), serialized_data)
            except Exception as e:
                logger.warning(f'Redis rendered result storage failed: {e}')
        for tag in invalidation_tags:
            self.invalidation_tags[tag].add(result_key)
        self.metrics.rendered_results_cached += 1
        self.metrics.total_cache_time_ms += (time.time() - start_time) * 1000
        self._update_size_metrics()
    async def invalidate_by_tags(self, tags: Set[str]) -> int:
        invalidated_count = 0
        for tag in tags:
            if tag in self.invalidation_tags:
                keys_to_invalidate = self.invalidation_tags[tag].copy()
                for key in keys_to_invalidate:
                    if key in self.template_cache:
                        del self.template_cache[key]
                        invalidated_count += 1
                    if key in self.rendered_cache:
                        del self.rendered_cache[key]
                        invalidated_count += 1
                    if self.redis_client:
                        try:
                            await self.redis_client.delete(f'template_compiled:{key}', f'template_rendered:{key}')
                        except Exception as e:
                            logger.warning(f'Redis invalidation failed for {key}: {e}')
                del self.invalidation_tags[tag]
        self.metrics.cache_evictions += invalidated_count
        logger.info(f'Invalidated {invalidated_count} cache entries for tags: {tags}')
        return invalidated_count
    async def _store_template_local(self, key: str, template: Any, entry: Optional[CacheEntry]=None):
        if entry is None:
            entry = CacheEntry(key=key, content=template, creation_time=time.time(), last_access=time.time(), access_count=1, size_bytes=len(str(template).encode()))
        while len(self.template_cache) >= self.max_template_cache_size:
            oldest_key, _ = self.template_cache.popitem(last=False)
            self.metrics.cache_evictions += 1
        self.template_cache[key] = entry
    async def _store_rendered_local(self, key: str, content: str, entry: Optional[CacheEntry]=None):
        if entry is None:
            entry = CacheEntry(key=key, content=content, creation_time=time.time(), last_access=time.time(), access_count=1, size_bytes=len(content.encode()))
        while len(self.rendered_cache) >= self.max_rendered_cache_size:
            oldest_key, _ = self.rendered_cache.popitem(last=False)
            self.metrics.cache_evictions += 1
        self.rendered_cache[key] = entry
    def _is_expired(self, entry: CacheEntry) -> bool:
        if entry.ttl_seconds is None:
            return False
        age = time.time() - entry.creation_time
        return age > entry.ttl_seconds
    def _serialize_content(self, content: Any) -> bytes:
        try:
            serialized = pickle.dumps(content)
            if self.enable_compression and len(serialized) > 1024:
                return zlib.compress(serialized)
            else:
                return serialized
        except Exception as e:
            logger.warning(f'Serialization failed: {e}')
            return json.dumps(str(content)).encode()
    def _deserialize_content(self, data: bytes) -> Any:
        try:
            try:
                decompressed = zlib.decompress(data)
                return pickle.loads(decompressed)
            except:
                return pickle.loads(data)
        except Exception as e:
            logger.warning(f'Deserialization failed: {e}')
            try:
                return json.loads(data.decode())
            except:
                return data.decode()
    def _update_size_metrics(self):
        total_template_size = sum((entry.size_bytes for entry in self.template_cache.values()))
        total_rendered_size = sum((entry.size_bytes for entry in self.rendered_cache.values()))
        total_entries = len(self.template_cache) + len(self.rendered_cache)
        self.metrics.memory_usage_bytes = total_template_size + total_rendered_size
        self.metrics.avg_template_size_bytes = total_template_size / max(len(self.template_cache), 1)
    async def cleanup_expired(self) -> int:
        removed_count = 0
        current_time = time.time()
        expired_template_keys = [key for key, entry in self.template_cache.items() if self._is_expired(entry)]
        for key in expired_template_keys:
            del self.template_cache[key]
            removed_count += 1
        expired_rendered_keys = [key for key, entry in self.rendered_cache.items() if self._is_expired(entry)]
        for key in expired_rendered_keys:
            del self.rendered_cache[key]
            removed_count += 1
        self.metrics.cache_evictions += removed_count
        if removed_count > 0:
            logger.info(f'Cleaned up {removed_count} expired cache entries')
        return removed_count
    def get_cache_statistics(self) -> Dict[str, Any]:
        hit_rate = self.metrics.cache_hits / max(self.metrics.cache_hits + self.metrics.cache_misses, 1)
        total_compressed = sum((1 for entry in self.rendered_cache.values() if entry.compression_ratio < 1.0))
        avg_compression_ratio = sum((entry.compression_ratio for entry in self.rendered_cache.values())) / max(len(self.rendered_cache), 1)
        return {'cache_performance': {'hit_rate': hit_rate, 'cache_hits': self.metrics.cache_hits, 'cache_misses': self.metrics.cache_misses, 'evictions': self.metrics.cache_evictions}, 'cache_sizes': {'template_cache_entries': len(self.template_cache), 'rendered_cache_entries': len(self.rendered_cache), 'max_template_cache_size': self.max_template_cache_size, 'max_rendered_cache_size': self.max_rendered_cache_size}, 'memory_usage': {'total_memory_bytes': self.metrics.memory_usage_bytes, 'avg_template_size_bytes': self.metrics.avg_template_size_bytes}, 'performance': {'total_render_time_ms': self.metrics.total_render_time_ms, 'total_cache_time_ms': self.metrics.total_cache_time_ms, 'template_compilations': self.metrics.template_compilations, 'rendered_results_cached': self.metrics.rendered_results_cached}, 'compression': {'compression_enabled': self.enable_compression, 'compressed_entries': total_compressed, 'avg_compression_ratio': avg_compression_ratio}, 'distributed_cache': {'redis_available': self.redis_client is not None, 'invalidation_tags_count': len(self.invalidation_tags)}}
    async def optimize_performance(self):
        start_time = time.time()
        expired_cleaned = await self.cleanup_expired()
        self._update_size_metrics()
        performance_snapshot = {'timestamp': time.time(), 'hit_rate': self.metrics.cache_hits / max(self.metrics.cache_hits + self.metrics.cache_misses, 1), 'memory_usage_mb': self.metrics.memory_usage_bytes / (1024 * 1024), 'cache_entries': len(self.template_cache) + len(self.rendered_cache), 'expired_cleaned': expired_cleaned}
        self.performance_history.append(performance_snapshot)
        if len(self.performance_history) > self.max_history_length:
            self.performance_history = self.performance_history[-self.max_history_length:]
        optimization_time = (time.time() - start_time) * 1000
        logger.info(f'Cache optimization completed in {optimization_time:.2f}ms, cleaned {expired_cleaned} entries')
        return {'optimization_time_ms': optimization_time, 'expired_entries_cleaned': expired_cleaned, 'current_cache_size': len(self.template_cache) + len(self.rendered_cache), 'memory_usage_mb': self.metrics.memory_usage_bytes / (1024 * 1024)}