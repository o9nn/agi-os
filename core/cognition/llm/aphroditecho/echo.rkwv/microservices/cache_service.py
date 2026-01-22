import asyncio
import time
import logging
import json
import os
import zlib
import pickle
from typing import Dict, Any, Optional, Tuple, Set
from dataclasses import dataclass, field
from enum import Enum
from aiohttp import web
import aioredis
from collections import OrderedDict, defaultdict
import psutil
logger = logging.getLogger(__name__)
class EvictionPolicy(Enum):
    LRU = 'lru'
    LFU = 'lfu'
    FIFO = 'fifo'
    TTL = 'ttl'
class CompressionType(Enum):
    NONE = 'none'
    ZLIB = 'zlib'
    GZIP = 'gzip'
@dataclass
class CacheItem:
    key: str
    value: Any
    created_at: float
    last_accessed: float
    access_count: int = 0
    ttl_seconds: Optional[int] = None
    tags: Set[str] = field(default_factory=set)
    compressed: bool = False
    compression_type: CompressionType = CompressionType.NONE
    original_size: int = 0
    compressed_size: int = 0
    @property
    def is_expired(self) -> bool:
        if self.ttl_seconds is None:
            return False
        return time.time() - self.created_at > self.ttl_seconds
    @property
    def age_seconds(self) -> float:
        return time.time() - self.created_at
    def touch(self):
        self.last_accessed = time.time()
        self.access_count += 1
class CacheLevel:
    def __init__(self, name: str, max_size: int, eviction_policy: EvictionPolicy=EvictionPolicy.LRU, default_ttl: Optional[int]=None, enable_compression: bool=False, compression_threshold: int=1024):
        self.name = name
        self.max_size = max_size
        self.eviction_policy = eviction_policy
        self.default_ttl = default_ttl
        self.enable_compression = enable_compression
        self.compression_threshold = compression_threshold
        if eviction_policy == EvictionPolicy.LRU:
            self.storage: OrderedDict[str, CacheItem] = OrderedDict()
        else:
            self.storage: Dict[str, CacheItem] = {}
        self.tag_index: Dict[str, Set[str]] = defaultdict(set)
        self.access_frequency: Dict[str, int] = defaultdict(int)
        self.hits = 0
        self.misses = 0
        self.evictions = 0
        self.compressions = 0
    def _compress_value(self, value: Any) -> Tuple[Any, CompressionType, int, int]:
        if not self.enable_compression:
            return (value, CompressionType.NONE, 0, 0)
        try:
            serialized = pickle.dumps(value)
            original_size = len(serialized)
            if original_size < self.compression_threshold:
                return (value, CompressionType.NONE, original_size, original_size)
            compressed = zlib.compress(serialized)
            compressed_size = len(compressed)
            if compressed_size < original_size * 0.8:
                self.compressions += 1
                return (compressed, CompressionType.ZLIB, original_size, compressed_size)
            else:
                return (value, CompressionType.NONE, original_size, original_size)
        except Exception as e:
            logger.warning(f'Compression failed: {e}')
            return (value, CompressionType.NONE, 0, 0)
    def _decompress_value(self, compressed_value: Any, compression_type: CompressionType) -> Any:
        if compression_type == CompressionType.NONE:
            return compressed_value
        try:
            if compression_type == CompressionType.ZLIB:
                decompressed = zlib.decompress(compressed_value)
                return pickle.loads(decompressed)
            else:
                return compressed_value
        except Exception as e:
            logger.error(f'Decompression failed: {e}')
            return compressed_value
    def get(self, key: str) -> Optional[Any]:
        item = self.storage.get(key)
        if item is None:
            self.misses += 1
            return None
        if item.is_expired:
            self._remove_item(key)
            self.misses += 1
            return None
        item.touch()
        self.access_frequency[key] += 1
        if self.eviction_policy == EvictionPolicy.LRU:
            self.storage.move_to_end(key)
        self.hits += 1
        return self._decompress_value(item.value, item.compression_type)
    def put(self, key: str, value: Any, ttl_seconds: Optional[int]=None, tags: Optional[Set[str]]=None) -> bool:
        ttl = ttl_seconds or self.default_ttl
        tags = tags or set()
        compressed_value, compression_type, original_size, compressed_size = self._compress_value(value)
        item = CacheItem(key=key, value=compressed_value, created_at=time.time(), last_accessed=time.time(), ttl_seconds=ttl, tags=tags, compressed=compression_type != CompressionType.NONE, compression_type=compression_type, original_size=original_size, compressed_size=compressed_size)
        if key in self.storage:
            self._remove_item(key)
        while len(self.storage) >= self.max_size:
            self._evict_item()
        self.storage[key] = item
        self.access_frequency[key] = 1
        for tag in tags:
            self.tag_index[tag].add(key)
        return True
    def delete(self, key: str) -> bool:
        if key in self.storage:
            self._remove_item(key)
            return True
        return False
    def delete_by_tags(self, tags: Set[str]) -> int:
        keys_to_delete = set()
        for tag in tags:
            keys_to_delete.update(self.tag_index.get(tag, set()))
        deleted_count = 0
        for key in keys_to_delete:
            if self.delete(key):
                deleted_count += 1
        return deleted_count
    def clear(self):
        self.storage.clear()
        self.tag_index.clear()
        self.access_frequency.clear()
    def _remove_item(self, key: str):
        item = self.storage.pop(key, None)
        if item:
            for tag in item.tags:
                self.tag_index[tag].discard(key)
                if not self.tag_index[tag]:
                    del self.tag_index[tag]
            self.access_frequency.pop(key, None)
    def _evict_item(self):
        if not self.storage:
            return
        key_to_evict = None
        if self.eviction_policy == EvictionPolicy.LRU:
            key_to_evict = next(iter(self.storage))
        elif self.eviction_policy == EvictionPolicy.LFU:
            key_to_evict = min(self.storage.keys(), key=lambda k: self.access_frequency.get(k, 0))
        elif self.eviction_policy == EvictionPolicy.FIFO:
            key_to_evict = min(self.storage.keys(), key=lambda k: self.storage[k].created_at)
        elif self.eviction_policy == EvictionPolicy.TTL:
            now = time.time()
            candidates = []
            for k, item in self.storage.items():
                if item.ttl_seconds:
                    time_to_expire = item.ttl_seconds - (now - item.created_at)
                    candidates.append((time_to_expire, k))
            if candidates:
                key_to_evict = min(candidates)[1]
            else:
                key_to_evict = next(iter(self.storage))
        if key_to_evict:
            self._remove_item(key_to_evict)
            self.evictions += 1
    def get_stats(self) -> Dict[str, Any]:
        total_requests = self.hits + self.misses
        hit_ratio = self.hits / total_requests if total_requests > 0 else 0.0
        total_original_size = sum((item.original_size for item in self.storage.values()))
        total_compressed_size = sum((item.compressed_size for item in self.storage.values()))
        compression_ratio = total_original_size / total_compressed_size if total_compressed_size > 0 else 1.0
        return {'name': self.name, 'size': len(self.storage), 'max_size': self.max_size, 'hits': self.hits, 'misses': self.misses, 'hit_ratio': hit_ratio, 'evictions': self.evictions, 'compressions': self.compressions, 'total_original_size': total_original_size, 'total_compressed_size': total_compressed_size, 'compression_ratio': compression_ratio, 'eviction_policy': self.eviction_policy.value}
class CacheService:
    def __init__(self, port: int=8002, l1_size: int=1000, l2_size: int=5000, l3_size: int=10000, default_ttl: int=300, eviction_policy: str='lru', enable_compression: bool=True):
        self.port = port
        try:
            policy = EvictionPolicy(eviction_policy.lower())
        except ValueError:
            logger.warning(f"Invalid eviction policy '{eviction_policy}', using LRU")
            policy = EvictionPolicy.LRU
        self.l1_cache = CacheLevel(name='L1_Memory', max_size=l1_size, eviction_policy=policy, default_ttl=default_ttl, enable_compression=False)
        self.l2_cache = CacheLevel(name='L2_Compressed', max_size=l2_size, eviction_policy=policy, default_ttl=default_ttl * 2, enable_compression=enable_compression)
        self.l3_cache = CacheLevel(name='L3_Persistent', max_size=l3_size, eviction_policy=policy, default_ttl=default_ttl * 4, enable_compression=enable_compression)
        self.cache_levels = [self.l1_cache, self.l2_cache, self.l3_cache]
        self.redis: Optional[aioredis.Redis] = None
        self.redis_enabled = False
        self.cleanup_task: Optional[asyncio.Task] = None
        self.metrics_task: Optional[asyncio.Task] = None
        self.total_requests = 0
        self.total_hits = 0
        self.cache_promotions = 0
    async def initialize(self):
        logger.info('Initializing Cache Service...')
        redis_url = os.getenv('REDIS_URL', 'redis://localhost:6379')
        try:
            self.redis = aioredis.from_url(redis_url, decode_responses=False)
            await self.redis.ping()
            self.redis_enabled = True
            logger.info('✅ Connected to Redis (L4 cache)')
        except Exception as e:
            logger.warning(f'Redis connection failed: {e}, continuing without L4 distributed cache')
        self.cleanup_task = asyncio.create_task(self._cleanup_loop())
        self.metrics_task = asyncio.create_task(self._metrics_collection_loop())
        logger.info('Cache Service initialized')
        logger.info(f'Cache levels: L1({self.l1_cache.max_size}) -> L2({self.l2_cache.max_size}) -> L3({self.l3_cache.max_size})')
    async def shutdown(self):
        logger.info('Shutting down Cache Service...')
        for task in [self.cleanup_task, self.metrics_task]:
            if task:
                task.cancel()
                try:
                    await task
                except asyncio.CancelledError:
                    pass
        if self.redis:
            await self.redis.close()
        logger.info('Cache Service shut down')
    async def get(self, key: str) -> Optional[Any]:
        self.total_requests += 1
        for i, cache_level in enumerate(self.cache_levels):
            value = cache_level.get(key)
            if value is not None:
                self.total_hits += 1
                if i > 0:
                    await self._promote_to_higher_levels(key, value, i)
                return value
        if self.redis_enabled:
            try:
                redis_value = await self.redis.get(f'cache:{key}')
                if redis_value:
                    value = pickle.loads(redis_value)
                    self.total_hits += 1
                    await self._promote_to_higher_levels(key, value, len(self.cache_levels))
                    return value
            except Exception as e:
                logger.error(f'Redis get error: {e}')
        return None
    async def put(self, key: str, value: Any, ttl_seconds: Optional[int]=None, tags: Optional[Set[str]]=None, cache_level: Optional[int]=None) -> bool:
        if cache_level is not None and 0 <= cache_level < len(self.cache_levels):
            target_levels = [self.cache_levels[cache_level]]
        else:
            target_levels = self.cache_levels
        success = False
        for level in target_levels:
            if level.put(key, value, ttl_seconds, tags):
                success = True
        if self.redis_enabled:
            try:
                redis_key = f'cache:{key}'
                redis_value = pickle.dumps(value)
                ttl = ttl_seconds or self.l3_cache.default_ttl
                if ttl:
                    await self.redis.setex(redis_key, ttl, redis_value)
                else:
                    await self.redis.set(redis_key, redis_value)
                success = True
            except Exception as e:
                logger.error(f'Redis put error: {e}')
        return success
    async def delete(self, key: str) -> int:
        deleted_count = 0
        for cache_level in self.cache_levels:
            if cache_level.delete(key):
                deleted_count += 1
        if self.redis_enabled:
            try:
                redis_deleted = await self.redis.delete(f'cache:{key}')
                deleted_count += redis_deleted
            except Exception as e:
                logger.error(f'Redis delete error: {e}')
        return deleted_count
    async def delete_by_tags(self, tags: Set[str]) -> int:
        total_deleted = 0
        for cache_level in self.cache_levels:
            deleted = cache_level.delete_by_tags(tags)
            total_deleted += deleted
        return total_deleted
    async def clear(self, cache_level: Optional[int]=None):
        if cache_level is not None and 0 <= cache_level < len(self.cache_levels):
            self.cache_levels[cache_level].clear()
        else:
            for level in self.cache_levels:
                level.clear()
            if self.redis_enabled:
                try:
                    keys = await self.redis.keys('cache:*')
                    if keys:
                        await self.redis.delete(*keys)
                except Exception as e:
                    logger.error(f'Redis clear error: {e}')
    async def _promote_to_higher_levels(self, key: str, value: Any, from_level: int):
        for i in range(from_level):
            cache_level = self.cache_levels[i]
            cache_level.put(key, value)
            self.cache_promotions += 1
    async def _cleanup_loop(self):
        while True:
            try:
                await self._cleanup_expired_items()
                await asyncio.sleep(60)
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f'Cleanup error: {e}')
                await asyncio.sleep(60)
    async def _cleanup_expired_items(self):
        for cache_level in self.cache_levels:
            expired_keys = []
            for key, item in cache_level.storage.items():
                if item.is_expired:
                    expired_keys.append(key)
            for key in expired_keys:
                cache_level._remove_item(key)
    async def _metrics_collection_loop(self):
        while True:
            try:
                await self._collect_metrics()
                await asyncio.sleep(30)
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f'Metrics collection error: {e}')
                await asyncio.sleep(30)
    async def _collect_metrics(self):
        metrics = await self.get_detailed_stats()
        if self.redis_enabled:
            try:
                await self.redis.setex('cache_metrics', 300, json.dumps(metrics))
            except Exception as e:
                logger.error(f'Failed to store metrics in Redis: {e}')
    async def get_detailed_stats(self) -> Dict[str, Any]:
        total_hit_ratio = self.total_hits / self.total_requests if self.total_requests > 0 else 0.0
        level_stats = []
        for i, cache_level in enumerate(self.cache_levels):
            stats = cache_level.get_stats()
            stats['level'] = i + 1
            level_stats.append(stats)
        memory = psutil.virtual_memory()
        return {'timestamp': time.time(), 'global_stats': {'total_requests': self.total_requests, 'total_hits': self.total_hits, 'global_hit_ratio': total_hit_ratio, 'cache_promotions': self.cache_promotions, 'redis_enabled': self.redis_enabled}, 'cache_levels': level_stats, 'system': {'memory_usage': memory.percent, 'memory_available': memory.available}}
    async def health_handler(self, request):
        return web.json_response({'status': 'healthy', 'timestamp': time.time(), 'cache_levels': len(self.cache_levels), 'redis_enabled': self.redis_enabled})
    async def stats_handler(self, request):
        stats = await self.get_detailed_stats()
        return web.json_response(stats)
    async def get_handler(self, request):
        try:
            data = await request.json()
            key = data.get('key')
            if not key:
                return web.json_response({'error': 'Key required'}, status=400)
            value = await self.get(key)
            if value is not None:
                return web.json_response({'key': key, 'value': value, 'found': True})
            else:
                return web.json_response({'key': key, 'found': False}, status=404)
        except Exception as e:
            logger.error(f'Get handler error: {e}')
            return web.json_response({'error': 'Internal server error'}, status=500)
    async def put_handler(self, request):
        try:
            data = await request.json()
            key = data.get('key')
            value = data.get('value')
            ttl_seconds = data.get('ttl_seconds')
            tags = set(data.get('tags', []))
            cache_level = data.get('cache_level')
            if not key or value is None:
                return web.json_response({'error': 'Key and value required'}, status=400)
            success = await self.put(key, value, ttl_seconds, tags, cache_level)
            return web.json_response({'success': success, 'key': key})
        except Exception as e:
            logger.error(f'Put handler error: {e}')
            return web.json_response({'error': 'Internal server error'}, status=500)
    async def delete_handler(self, request):
        try:
            data = await request.json()
            key = data.get('key')
            tags = data.get('tags')
            if key:
                deleted_count = await self.delete(key)
                return web.json_response({'deleted_count': deleted_count, 'key': key})
            elif tags:
                deleted_count = await self.delete_by_tags(set(tags))
                return web.json_response({'deleted_count': deleted_count, 'tags': tags})
            else:
                return web.json_response({'error': 'Key or tags required'}, status=400)
        except Exception as e:
            logger.error(f'Delete handler error: {e}')
            return web.json_response({'error': 'Internal server error'}, status=500)
    async def clear_handler(self, request):
        try:
            data = await request.json() if request.content_length else {}
            cache_level = data.get('cache_level')
            await self.clear(cache_level)
            return web.json_response({'success': True, 'cache_level': cache_level})
        except Exception as e:
            logger.error(f'Clear handler error: {e}')
            return web.json_response({'error': 'Internal server error'}, status=500)
    def create_app(self):
        app = web.Application()
        app.router.add_get('/health', self.health_handler)
        app.router.add_get('/stats', self.stats_handler)
        app.router.add_post('/get', self.get_handler)
        app.router.add_post('/put', self.put_handler)
        app.router.add_post('/delete', self.delete_handler)
        app.router.add_post('/clear', self.clear_handler)
        return app
    async def run(self):
        await self.initialize()
        app = self.create_app()
        runner = web.AppRunner(app)
        await runner.setup()
        site = web.TCPSite(runner, '0.0.0.0', self.port)
        await site.start()
        logger.info(f'🚀 Cache Service running on port {self.port}')
        logger.info(f'Cache levels: L1({self.l1_cache.max_size}) -> L2({self.l2_cache.max_size}) -> L3({self.l3_cache.max_size})')
        logger.info(f"Redis L4 cache: {('enabled' if self.redis_enabled else 'disabled')}")
        try:
            await asyncio.Future()
        finally:
            await self.shutdown()
async def main():
    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    port = int(os.getenv('CACHE_SERVICE_PORT', 8002))
    max_memory_mb = int(os.getenv('MAX_CACHE_MEMORY_MB', 512))
    default_ttl = int(os.getenv('DEFAULT_TTL_SECONDS', 300))
    eviction_policy = os.getenv('EVICTION_POLICY', 'lru')
    enable_compression = os.getenv('ENABLE_COMPRESSION', 'true').lower() == 'true'
    total_items = max_memory_mb * 10
    l1_size = total_items // 10
    l2_size = total_items // 2
    l3_size = total_items - l1_size - l2_size
    cache_service = CacheService(port=port, l1_size=l1_size, l2_size=l2_size, l3_size=l3_size, default_ttl=default_ttl, eviction_policy=eviction_policy, enable_compression=enable_compression)
    await cache_service.run()
if __name__ == '__main__':
    asyncio.run(main())