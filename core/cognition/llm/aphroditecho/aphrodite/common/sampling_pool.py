import hashlib
import pickle
import threading
import time
import weakref
from collections import defaultdict, deque
from dataclasses import dataclass, field
from typing import Any, Dict, Optional, Set, Tuple
import msgspec
from loguru import logger
from aphrodite.common.sampling_params import SamplingParams
@dataclass
class SamplingParamsInfo:
    params: SamplingParams
    hash_key: str
    creation_time: float
    last_used: float
    usage_count: int = 0
    def __post_init__(self):
        if self.last_used == 0:
            self.last_used = self.creation_time
class SamplingParamsPool:
    def __init__(self, max_pool_size: int=10000, cleanup_interval: float=300.0, max_age: float=3600.0):
        self.max_pool_size = max_pool_size
        self.cleanup_interval = cleanup_interval
        self.max_age = max_age
        self._params_cache: Dict[str, SamplingParamsInfo] = {}
        self._usage_queue: deque = deque()
        self._hash_to_params: Dict[str, str] = {}
        self._active_refs: weakref.WeakSet = weakref.WeakSet()
        self._stats = {'cache_hits': 0, 'cache_misses': 0, 'total_created': 0, 'total_deduplicated': 0, 'memory_saved_bytes': 0, 'cleanup_count': 0}
        self._lock = threading.RLock()
        self._last_cleanup = time.time()
    def get_or_create(self, params_data: Optional[Dict[str, Any]]=None, **kwargs) -> SamplingParams:
        with self._lock:
            if params_data is None:
                params_data = {}
            params_data.update(kwargs)
            content_hash = self._generate_hash(params_data)
            if content_hash in self._hash_to_params:
                params_id = self._hash_to_params[content_hash]
                if params_id in self._params_cache:
                    return self._get_cached_params(params_id, content_hash)
            return self._create_new_params(params_data, content_hash)
    def _generate_hash(self, params_data: Dict[str, Any]) -> str:
        normalized_data = {}
        defaults = SamplingParams.default_values
        for key, value in params_data.items():
            if key in defaults:
                default_value = defaults[key]
                if value != default_value:
                    normalized_data[key] = value
            else:
                normalized_data[key] = value
        sorted_items = sorted(normalized_data.items())
        hash_string = repr(sorted_items)
        return hashlib.md5(hash_string.encode('utf-8')).hexdigest()
    def _get_cached_params(self, params_id: str, content_hash: str) -> SamplingParams:
        params_info = self._params_cache[params_id]
        params_info.last_used = time.time()
        params_info.usage_count += 1
        try:
            self._usage_queue.remove((params_info.last_used, params_id))
        except ValueError:
            pass
        self._usage_queue.append((time.time(), params_id))
        self._stats['cache_hits'] += 1
        self._stats['total_deduplicated'] += 1
        self._stats['memory_saved_bytes'] += self._estimate_params_size(params_info.params)
        self._active_refs.add(params_info.params)
        return params_info.params
    def _create_new_params(self, params_data: Dict[str, Any], content_hash: str) -> SamplingParams:
        params = SamplingParams.from_optional(**params_data)
        params_id = f'sp_{int(time.time() * 1000000)}_{len(self._params_cache)}'
        current_time = time.time()
        params_info = SamplingParamsInfo(params=params, hash_key=content_hash, creation_time=current_time, last_used=current_time, usage_count=1)
        self._params_cache[params_id] = params_info
        self._hash_to_params[content_hash] = params_id
        self._usage_queue.append((current_time, params_id))
        self._stats['cache_misses'] += 1
        self._stats['total_created'] += 1
        self._active_refs.add(params)
        if len(self._params_cache) > self.max_pool_size * 0.9 or current_time - self._last_cleanup > self.cleanup_interval:
            self._cleanup_old_params()
        return params
    def _cleanup_old_params(self):
        current_time = time.time()
        cleanup_threshold = current_time - self.max_age
        to_remove = []
        for params_id, params_info in self._params_cache.items():
            if params_info.last_used < cleanup_threshold and params_info.params not in self._active_refs:
                to_remove.append(params_id)
        if len(self._params_cache) - len(to_remove) > self.max_pool_size:
            usage_times = [(info.last_used, params_id) for params_id, info in self._params_cache.items() if params_id not in to_remove and info.params not in self._active_refs]
            usage_times.sort()
            excess_count = len(self._params_cache) - len(to_remove) - self.max_pool_size
            to_remove.extend([params_id for _, params_id in usage_times[:excess_count]])
        removed_count = 0
        for params_id in to_remove:
            if params_id in self._params_cache:
                params_info = self._params_cache[params_id]
                if params_info.hash_key in self._hash_to_params:
                    del self._hash_to_params[params_info.hash_key]
                del self._params_cache[params_id]
                removed_count += 1
        self._usage_queue = deque(((timestamp, params_id) for timestamp, params_id in self._usage_queue if params_id in self._params_cache))
        self._last_cleanup = current_time
        self._stats['cleanup_count'] += 1
        if removed_count > 0:
            logger.debug(f'Cleaned up {removed_count} old sampling parameters')
    def _estimate_params_size(self, params: SamplingParams) -> int:
        try:
            encoded = msgspec.msgpack.encode(params)
            return len(encoded)
        except Exception:
            return 1024
    def create_compact_encoding(self, params: SamplingParams) -> bytes:
        try:
            return msgspec.msgpack.encode(params)
        except Exception:
            return pickle.dumps(params, protocol=pickle.HIGHEST_PROTOCOL)
    def decode_compact_encoding(self, encoded_data: bytes) -> SamplingParams:
        try:
            return msgspec.msgpack.decode(encoded_data, type=SamplingParams)
        except Exception:
            return pickle.loads(encoded_data)
    def get_stats(self) -> Dict[str, Any]:
        with self._lock:
            hit_rate = 0.0
            total_requests = self._stats['cache_hits'] + self._stats['cache_misses']
            if total_requests > 0:
                hit_rate = self._stats['cache_hits'] / total_requests
            dedup_rate = 0.0
            if self._stats['total_created'] > 0:
                dedup_rate = self._stats['total_deduplicated'] / (self._stats['total_created'] + self._stats['total_deduplicated'])
            return {'pool_size': len(self._params_cache), 'max_pool_size': self.max_pool_size, 'active_references': len(self._active_refs), 'hash_mappings': len(self._hash_to_params), 'cache_hit_rate': hit_rate, 'deduplication_rate': dedup_rate, 'memory_saved_mb': self._stats['memory_saved_bytes'] / (1024 * 1024), 'cleanup_count': self._stats['cleanup_count'], 'usage_queue_size': len(self._usage_queue), 'stats': self._stats.copy()}
    def clear_pool(self):
        with self._lock:
            self._params_cache.clear()
            self._hash_to_params.clear()
            self._usage_queue.clear()
            self._active_refs.clear()
            self._stats = {'cache_hits': 0, 'cache_misses': 0, 'total_created': 0, 'total_deduplicated': 0, 'memory_saved_bytes': 0, 'cleanup_count': 0}
    def force_cleanup(self):
        with self._lock:
            self._cleanup_old_params()
_global_params_pool: Optional[SamplingParamsPool] = None
_pool_lock = threading.Lock()
def get_sampling_params_pool(max_pool_size: Optional[int]=None) -> SamplingParamsPool:
    global _global_params_pool
    with _pool_lock:
        if _global_params_pool is None:
            pool_size = max_pool_size or 10000
            _global_params_pool = SamplingParamsPool(max_pool_size=pool_size)
            logger.info(f'Initialized global sampling parameters pool (max_size={pool_size})')
        return _global_params_pool
def reset_sampling_params_pool():
    global _global_params_pool
    with _pool_lock:
        if _global_params_pool is not None:
            _global_params_pool.clear_pool()
            _global_params_pool = None
def create_optimized_sampling_params(**kwargs) -> SamplingParams:
    pool = get_sampling_params_pool()
    return pool.get_or_create(**kwargs)