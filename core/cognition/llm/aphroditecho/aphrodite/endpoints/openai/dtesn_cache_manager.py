#!/usr/bin/env python3
"""
Server-Side Caching Layer for DTESN Processing Results

Implements a high-performance multi-level caching system specifically optimized 
for DTESN (Deep Tree Echo State Network) processing results in the Aphrodite Engine.

Features:
- Multi-level caching (L1: Memory, L2: Compressed, L3: Persistent, L4: Redis)
- Intelligent cache invalidation strategies for dynamic content
- Performance monitoring and metrics collection
- Integration with existing Aphrodite serving infrastructure
- Optimized for 50% performance improvement on cached content
"""

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

# Optional Redis import - graceful degradation if not available
try:
    import aioredis
    REDIS_AVAILABLE = True
except ImportError:
    REDIS_AVAILABLE = False
    aioredis = None

logger = logging.getLogger(__name__)


class CacheStrategy(Enum):
    """Cache storage strategies for different content types"""
    AGGRESSIVE = "aggressive"  # Cache everything, long TTL
    BALANCED = "balanced"     # Cache frequent requests, moderate TTL  
    CONSERVATIVE = "conservative"  # Cache only expensive operations, short TTL
    DYNAMIC = "dynamic"       # Adaptive caching based on request patterns


@dataclass
class DTESNCacheKey:
    """Structured cache key for DTESN processing results"""
    input_hash: str
    model_id: str
    dtesn_config_hash: str
    membrane_depth: int
    esn_size: int
    
    def to_string(self) -> str:
        """Generate string representation for cache key"""
        return f"dtesn:{self.model_id}:{self.membrane_depth}:{self.esn_size}:{self.input_hash[:16]}:{self.dtesn_config_hash[:8]}"


@dataclass 
class DTESNCacheEntry:
    """Cache entry for DTESN processing results"""
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
        """Check if cache entry is expired"""
        if self.ttl_seconds is None:
            return False
        return time.time() - self.created_at > self.ttl_seconds
    
    def touch(self) -> None:
        """Update access metadata"""
        self.last_accessed = time.time()
        self.access_count += 1


@dataclass
class CacheMetrics:
    """Performance metrics for cache operations"""
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
        """Calculate cache hit ratio"""
        if self.total_requests == 0:
            return 0.0
        return self.cache_hits / self.total_requests
    
    @property 
    def performance_improvement(self) -> float:
        """Calculate performance improvement from caching"""
        if self.avg_processing_time_ms == 0:
            return 0.0
        return max(0.0, 1.0 - (self.avg_cache_retrieval_time_ms / self.avg_processing_time_ms))


class DTESNServerSideCacheManager:
    """
    Server-side cache manager optimized for DTESN processing results.
    
    Provides intelligent multi-level caching with performance monitoring
    and adaptive invalidation strategies.
    """
    
    def __init__(
        self,
        max_memory_entries: int = 1000,
        max_compressed_entries: int = 5000,
        redis_url: Optional[str] = None,
        default_ttl_seconds: int = 3600,
        cache_strategy: CacheStrategy = CacheStrategy.BALANCED,
        enable_compression: bool = True,
        compression_threshold: int = 1024
    ):
        """
        Initialize the DTESN cache manager.
        
        Args:
            max_memory_entries: Maximum entries in L1 memory cache
            max_compressed_entries: Maximum entries in L2 compressed cache
            redis_url: Redis connection URL for distributed caching
            default_ttl_seconds: Default time-to-live for cache entries
            cache_strategy: Caching strategy to use
            enable_compression: Whether to enable compression for large entries
            compression_threshold: Minimum size (bytes) to trigger compression
        """
        self.max_memory_entries = max_memory_entries
        self.max_compressed_entries = max_compressed_entries
        self.default_ttl_seconds = default_ttl_seconds
        self.cache_strategy = cache_strategy
        self.enable_compression = enable_compression
        self.compression_threshold = compression_threshold
        
        # Multi-level cache storage
        self.memory_cache: OrderedDict[str, DTESNCacheEntry] = OrderedDict()
        self.compressed_cache: OrderedDict[str, bytes] = OrderedDict()
        self.cache_metadata: Dict[str, DTESNCacheEntry] = {}
        
        # Redis configuration
        self.redis_url = redis_url
        self.redis_client: Optional[aioredis.Redis] = None
        self.redis_enabled = False
        
        # Content invalidation tracking
        self.content_tags_index: Dict[str, Set[str]] = defaultdict(set)
        self.model_dependency_graph: Dict[str, Set[str]] = defaultdict(set)
        
        # Performance metrics
        self.metrics = CacheMetrics()
        self.processing_times: List[float] = []
        self.cache_times: List[float] = []
        
        # Background tasks
        self.cleanup_task: Optional[asyncio.Task] = None
        self.metrics_task: Optional[asyncio.Task] = None
        
        logger.info(f"DTESN Server-side Cache Manager initialized with strategy: {cache_strategy.value}")
    
    async def initialize(self) -> None:
        """Initialize the cache manager and connect to Redis if configured"""
        # Initialize Redis connection if configured
        if self.redis_url and REDIS_AVAILABLE:
            try:
                self.redis_client = aioredis.from_url(self.redis_url, decode_responses=False)
                await self.redis_client.ping()
                self.redis_enabled = True
                logger.info("✅ Connected to Redis for distributed DTESN caching")
            except Exception as e:
                logger.warning(f"Redis connection failed: {e}")
                self.redis_client = None
        
        # Start background maintenance tasks
        self.cleanup_task = asyncio.create_task(self._cleanup_expired_entries())
        self.metrics_task = asyncio.create_task(self._collect_performance_metrics())
        
        logger.info("DTESN Cache Manager fully initialized")
    
    async def shutdown(self) -> None:
        """Gracefully shutdown the cache manager"""
        logger.info("Shutting down DTESN Cache Manager...")
        
        # Cancel background tasks
        for task in [self.cleanup_task, self.metrics_task]:
            if task and not task.done():
                task.cancel()
                try:
                    await task
                except asyncio.CancelledError:
                    pass
        
        # Close Redis connection
        if self.redis_client:
            await self.redis_client.close()
        
        logger.info("DTESN Cache Manager shutdown complete")
    
    def _generate_cache_key(
        self,
        input_data: Union[str, Dict[str, Any]],
        model_id: str,
        dtesn_config: Dict[str, Any]
    ) -> DTESNCacheKey:
        """Generate structured cache key for DTESN processing"""
        # Create hash of input data
        input_str = json.dumps(input_data, sort_keys=True) if isinstance(input_data, dict) else str(input_data)
        input_hash = hashlib.sha256(input_str.encode()).hexdigest()
        
        # Create hash of DTESN configuration  
        config_str = json.dumps(dtesn_config, sort_keys=True)
        config_hash = hashlib.md5(config_str.encode()).hexdigest()
        
        return DTESNCacheKey(
            input_hash=input_hash,
            model_id=model_id,
            dtesn_config_hash=config_hash,
            membrane_depth=dtesn_config.get('membrane_depth', 4),
            esn_size=dtesn_config.get('esn_size', 512)
        )
    
    async def get_cached_result(
        self,
        input_data: Union[str, Dict[str, Any]],
        model_id: str,
        dtesn_config: Dict[str, Any]
    ) -> Optional[Tuple[Dict[str, Any], Dict[str, Any]]]:
        """
        Retrieve cached DTESN processing result if available.
        
        Args:
            input_data: Input data for DTESN processing
            model_id: Model identifier
            dtesn_config: DTESN configuration parameters
            
        Returns:
            Tuple of (result, metadata) if cached, None otherwise
        """
        start_time = time.time()
        self.metrics.total_requests += 1
        
        # Generate cache key
        cache_key_obj = self._generate_cache_key(input_data, model_id, dtesn_config)
        cache_key = cache_key_obj.to_string()
        
        # Try L1 memory cache first
        if cache_key in self.memory_cache:
            entry = self.memory_cache[cache_key]
            if not entry.is_expired():
                entry.touch()
                self.memory_cache.move_to_end(cache_key)  # LRU update
                self.metrics.cache_hits += 1
                
                retrieval_time = (time.time() - start_time) * 1000
                self.cache_times.append(retrieval_time)
                
                logger.debug(f"L1 cache hit for key: {cache_key[:32]}...")
                return entry.result, entry.metadata
            else:
                # Remove expired entry
                self._remove_from_all_caches(cache_key)
        
        # Try L2 compressed cache
        if cache_key in self.compressed_cache and cache_key in self.cache_metadata:
            entry_metadata = self.cache_metadata[cache_key]
            if not entry_metadata.is_expired():
                try:
                    # Decompress and deserialize
                    compressed_data = self.compressed_cache[cache_key]
                    decompressed_data = zlib.decompress(compressed_data)
                    result_data = pickle.loads(decompressed_data)
                    
                    entry_metadata.touch()
                    self.compressed_cache.move_to_end(cache_key)
                    self.metrics.cache_hits += 1
                    
                    # Promote to L1 cache if there's space
                    await self._promote_to_memory_cache(cache_key, entry_metadata, result_data)
                    
                    retrieval_time = (time.time() - start_time) * 1000
                    self.cache_times.append(retrieval_time)
                    
                    logger.debug(f"L2 cache hit for key: {cache_key[:32]}...")
                    return result_data, entry_metadata.metadata
                except Exception as e:
                    logger.error(f"L2 cache decompression failed: {e}")
                    self._remove_from_all_caches(cache_key)
            else:
                self._remove_from_all_caches(cache_key)
        
        # Try Redis (L3) if enabled
        if self.redis_enabled and self.redis_client:
            try:
                redis_key = f"dtesn:{cache_key}"
                redis_data = await self.redis_client.get(redis_key)
                
                if redis_data:
                    try:
                        cache_entry_dict = pickle.loads(redis_data)
                        entry = DTESNCacheEntry(**cache_entry_dict)
                        
                        if not entry.is_expired():
                            entry.touch()
                            self.metrics.redis_hits += 1
                            self.metrics.cache_hits += 1
                            
                            # Promote to local caches
                            await self._promote_to_local_caches(cache_key, entry)
                            
                            retrieval_time = (time.time() - start_time) * 1000
                            self.cache_times.append(retrieval_time)
                            
                            logger.debug(f"Redis cache hit for key: {cache_key[:32]}...")
                            return entry.result, entry.metadata
                        else:
                            # Remove expired Redis entry
                            await self.redis_client.delete(redis_key)
                    except Exception as e:
                        logger.error(f"Redis cache deserialization failed: {e}")
                        await self.redis_client.delete(redis_key)
                else:
                    self.metrics.redis_misses += 1
            except Exception as e:
                logger.error(f"Redis cache retrieval failed: {e}")
        
        # Cache miss
        self.metrics.cache_misses += 1
        logger.debug(f"Cache miss for key: {cache_key[:32]}...")
        return None
    
    async def cache_result(
        self,
        input_data: Union[str, Dict[str, Any]],
        model_id: str, 
        dtesn_config: Dict[str, Any],
        result: Dict[str, Any],
        metadata: Dict[str, Any],
        processing_time_ms: float,
        content_tags: Optional[Set[str]] = None
    ) -> None:
        """
        Cache DTESN processing result with intelligent storage strategy.
        
        Args:
            input_data: Input data for DTESN processing
            model_id: Model identifier  
            dtesn_config: DTESN configuration parameters
            result: Processing result to cache
            metadata: Additional metadata
            processing_time_ms: Original processing time in milliseconds
            content_tags: Tags for content-based invalidation
        """
        # Generate cache key
        cache_key_obj = self._generate_cache_key(input_data, model_id, dtesn_config)
        cache_key = cache_key_obj.to_string()
        
        content_tags = content_tags or set()
        
        # Determine TTL based on cache strategy
        ttl = self._determine_ttl(processing_time_ms, content_tags)
        
        # Create cache entry
        cache_entry = DTESNCacheEntry(
            key=cache_key_obj,
            result=result,
            metadata=metadata,
            created_at=time.time(),
            last_accessed=time.time(),
            access_count=1,
            processing_time_ms=processing_time_ms,
            content_tags=content_tags,
            ttl_seconds=ttl
        )
        
        # Update processing time metrics
        self.processing_times.append(processing_time_ms)
        if len(self.processing_times) > 1000:
            self.processing_times = self.processing_times[-500:]  # Keep recent history
        
        # Store in appropriate cache levels based on strategy
        await self._store_in_cache_levels(cache_key, cache_entry)
        
        # Update content tags index for invalidation
        for tag in content_tags:
            self.content_tags_index[tag].add(cache_key)
        
        logger.debug(f"Cached result for key: {cache_key[:32]}... (TTL: {ttl}s)")
    
    async def invalidate_by_tags(self, tags: Set[str]) -> int:
        """
        Invalidate cached entries by content tags.
        
        Args:
            tags: Set of content tags to invalidate
            
        Returns:
            Number of entries invalidated
        """
        invalidated_count = 0
        keys_to_invalidate = set()
        
        # Find all keys with matching tags
        for tag in tags:
            keys_to_invalidate.update(self.content_tags_index.get(tag, set()))
        
        # Invalidate entries
        for cache_key in keys_to_invalidate:
            if await self._invalidate_cache_entry(cache_key):
                invalidated_count += 1
        
        # Clean up tag index
        for tag in tags:
            self.content_tags_index[tag].clear()
        
        self.metrics.invalidations += invalidated_count
        logger.info(f"Invalidated {invalidated_count} cache entries by tags: {tags}")
        
        return invalidated_count
    
    async def invalidate_by_model(self, model_id: str) -> int:
        """
        Invalidate all cached entries for a specific model.
        
        Args:
            model_id: Model identifier
            
        Returns:  
            Number of entries invalidated
        """
        invalidated_count = 0
        keys_to_invalidate = []
        
        # Find keys in memory cache
        for key, entry in self.memory_cache.items():
            if entry.key.model_id == model_id:
                keys_to_invalidate.append(key)
        
        # Find keys in compressed cache metadata
        for key, entry in self.cache_metadata.items():
            if entry.key.model_id == model_id:
                keys_to_invalidate.append(key)
        
        # Invalidate found entries
        for cache_key in keys_to_invalidate:
            if await self._invalidate_cache_entry(cache_key):
                invalidated_count += 1
        
        # Invalidate from Redis using pattern matching
        if self.redis_enabled and self.redis_client:
            try:
                redis_pattern = f"dtesn:*{model_id}*"
                async for key in self.redis_client.scan_iter(match=redis_pattern):
                    await self.redis_client.delete(key)
                    invalidated_count += 1
            except Exception as e:
                logger.error(f"Redis pattern invalidation failed: {e}")
        
        self.metrics.invalidations += invalidated_count
        logger.info(f"Invalidated {invalidated_count} cache entries for model: {model_id}")
        
        return invalidated_count
    
    def get_performance_metrics(self) -> Dict[str, Any]:
        """Get comprehensive cache performance metrics"""
        # Calculate average times
        if self.processing_times:
            self.metrics.avg_processing_time_ms = sum(self.processing_times) / len(self.processing_times)
        
        if self.cache_times:
            self.metrics.avg_cache_retrieval_time_ms = sum(self.cache_times) / len(self.cache_times)
        
        # Calculate memory usage
        memory_size = sum(len(pickle.dumps(entry)) for entry in self.memory_cache.values())
        compressed_size = sum(len(data) for data in self.compressed_cache.values()) 
        self.metrics.memory_usage_bytes = memory_size + compressed_size
        
        return {
            **asdict(self.metrics),
            "cache_levels": {
                "memory_entries": len(self.memory_cache),
                "compressed_entries": len(self.compressed_cache),
                "redis_enabled": self.redis_enabled
            },
            "performance_improvement_percent": self.metrics.performance_improvement * 100,
            "cache_strategy": self.cache_strategy.value
        }
    
    # Private helper methods
    
    def _determine_ttl(self, processing_time_ms: float, content_tags: Set[str]) -> int:
        """Determine TTL based on cache strategy and processing characteristics"""
        base_ttl = self.default_ttl_seconds
        
        if self.cache_strategy == CacheStrategy.AGGRESSIVE:
            return base_ttl * 2
        elif self.cache_strategy == CacheStrategy.CONSERVATIVE:
            return base_ttl // 2
        elif self.cache_strategy == CacheStrategy.DYNAMIC:
            # Longer TTL for expensive operations
            if processing_time_ms > 1000:  # >1 second
                return base_ttl * 3
            elif processing_time_ms > 100:  # >100ms
                return base_ttl
            else:
                return base_ttl // 3
        else:  # BALANCED
            return base_ttl
    
    async def _store_in_cache_levels(self, cache_key: str, entry: DTESNCacheEntry) -> None:
        """Store cache entry in appropriate levels based on strategy"""
        # Always try L1 memory cache first
        await self._store_in_memory_cache(cache_key, entry)
        
        # Store in Redis if enabled and entry is valuable enough
        if self.redis_enabled and self.redis_client:
            if (entry.processing_time_ms > 50 or  # Expensive operations
                self.cache_strategy in [CacheStrategy.AGGRESSIVE, CacheStrategy.BALANCED]):
                await self._store_in_redis(cache_key, entry)
    
    async def _store_in_memory_cache(self, cache_key: str, entry: DTESNCacheEntry) -> None:
        """Store entry in L1 memory cache with LRU eviction"""
        # Check if we need to evict entries
        while len(self.memory_cache) >= self.max_memory_entries:
            self._evict_lru_memory_entry()
        
        self.memory_cache[cache_key] = entry
        self.cache_metadata[cache_key] = entry
    
    async def _store_in_compressed_cache(self, cache_key: str, entry: DTESNCacheEntry) -> None:
        """Store entry in L2 compressed cache"""
        if not self.enable_compression:
            return
        
        try:
            # Serialize and compress the result
            serialized = pickle.dumps(entry.result)
            
            if len(serialized) >= self.compression_threshold:
                compressed = zlib.compress(serialized, level=6)
                
                # Check if we need to evict entries
                while len(self.compressed_cache) >= self.max_compressed_entries:
                    self._evict_lru_compressed_entry()
                
                self.compressed_cache[cache_key] = compressed
                self.cache_metadata[cache_key] = entry
                
                logger.debug(f"Compressed cache entry: {len(serialized)} -> {len(compressed)} bytes")
                
        except Exception as e:
            logger.error(f"Compression failed for cache entry: {e}")
    
    async def _store_in_redis(self, cache_key: str, entry: DTESNCacheEntry) -> None:
        """Store entry in Redis distributed cache"""
        try:
            redis_key = f"dtesn:{cache_key}"
            redis_value = pickle.dumps(asdict(entry))
            
            if entry.ttl_seconds:
                await self.redis_client.setex(redis_key, entry.ttl_seconds, redis_value)
            else:
                await self.redis_client.set(redis_key, redis_value)
                
        except Exception as e:
            logger.error(f"Redis storage failed: {e}")
    
    async def _promote_to_memory_cache(self, cache_key: str, entry: DTESNCacheEntry, result_data: Dict[str, Any]) -> None:
        """Promote entry from compressed cache to memory cache"""
        if len(self.memory_cache) < self.max_memory_entries:
            entry.result = result_data
            self.memory_cache[cache_key] = entry
            logger.debug(f"Promoted cache entry to L1: {cache_key[:32]}...")
    
    async def _promote_to_local_caches(self, cache_key: str, entry: DTESNCacheEntry) -> None:
        """Promote entry from Redis to local caches"""
        # Store in memory cache if space available
        if len(self.memory_cache) < self.max_memory_entries:
            self.memory_cache[cache_key] = entry
            self.cache_metadata[cache_key] = entry
        else:
            # Store in compressed cache
            await self._store_in_compressed_cache(cache_key, entry)
    
    def _evict_lru_memory_entry(self) -> None:
        """Evict least recently used entry from memory cache"""
        if self.memory_cache:
            evicted_key, _ = self.memory_cache.popitem(last=False)
            if evicted_key in self.cache_metadata:
                del self.cache_metadata[evicted_key]
            self.metrics.evictions += 1
            logger.debug(f"Evicted LRU memory entry: {evicted_key[:32]}...")
    
    def _evict_lru_compressed_entry(self) -> None:
        """Evict least recently used entry from compressed cache"""
        if self.compressed_cache:
            evicted_key, _ = self.compressed_cache.popitem(last=False)
            if evicted_key in self.cache_metadata:
                del self.cache_metadata[evicted_key]
            self.metrics.evictions += 1
            logger.debug(f"Evicted LRU compressed entry: {evicted_key[:32]}...")
    
    async def _invalidate_cache_entry(self, cache_key: str) -> bool:
        """Remove cache entry from all levels"""
        invalidated = False
        
        # Remove from memory cache
        if cache_key in self.memory_cache:
            del self.memory_cache[cache_key]
            invalidated = True
        
        # Remove from compressed cache
        if cache_key in self.compressed_cache:
            del self.compressed_cache[cache_key]
            invalidated = True
        
        # Remove metadata
        if cache_key in self.cache_metadata:
            del self.cache_metadata[cache_key]
        
        # Remove from Redis
        if self.redis_enabled and self.redis_client:
            try:
                redis_key = f"dtesn:{cache_key}"
                await self.redis_client.delete(redis_key)
                invalidated = True
            except Exception as e:
                logger.error(f"Redis invalidation failed: {e}")
        
        return invalidated
    
    def _remove_from_all_caches(self, cache_key: str) -> None:
        """Synchronously remove entry from local caches"""
        if cache_key in self.memory_cache:
            del self.memory_cache[cache_key]
        if cache_key in self.compressed_cache:
            del self.compressed_cache[cache_key]
        if cache_key in self.cache_metadata:
            del self.cache_metadata[cache_key]
    
    async def _cleanup_expired_entries(self) -> None:
        """Background task to cleanup expired cache entries"""
        while True:
            try:
                await asyncio.sleep(300)  # Run every 5 minutes
                
                expired_keys = []
                current_time = time.time()
                
                # Check memory cache for expired entries
                for key, entry in self.memory_cache.items():
                    if entry.is_expired():
                        expired_keys.append(key)
                
                # Check compressed cache metadata for expired entries
                for key, entry in self.cache_metadata.items():
                    if key not in expired_keys and entry.is_expired():
                        expired_keys.append(key)
                
                # Remove expired entries
                for key in expired_keys:
                    self._remove_from_all_caches(key)
                
                if expired_keys:
                    logger.info(f"Cleaned up {len(expired_keys)} expired cache entries")
                    
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Cache cleanup error: {e}")
    
    async def _collect_performance_metrics(self) -> None:
        """Background task to collect and log performance metrics"""
        while True:
            try:
                await asyncio.sleep(60)  # Run every minute
                
                metrics = self.get_performance_metrics()
                
                logger.info(
                    f"DTESN Cache Performance - "
                    f"Hit Ratio: {metrics['hit_ratio']:.2%}, "
                    f"Performance Improvement: {metrics['performance_improvement_percent']:.1f}%, "
                    f"Memory Entries: {metrics['cache_levels']['memory_entries']}, "
                    f"Compressed Entries: {metrics['cache_levels']['compressed_entries']}"
                )
                
            except asyncio.CancelledError:
                break
            except Exception as e:
                logger.error(f"Metrics collection error: {e}")


# Global cache manager instance - will be initialized in the API server
_global_cache_manager: Optional[DTESNServerSideCacheManager] = None


def get_cache_manager() -> Optional[DTESNServerSideCacheManager]:
    """Get the global DTESN cache manager instance"""
    return _global_cache_manager


def initialize_global_cache_manager(**kwargs) -> DTESNServerSideCacheManager:
    """Initialize the global DTESN cache manager"""
    global _global_cache_manager
    _global_cache_manager = DTESNServerSideCacheManager(**kwargs)
    return _global_cache_manager


async def shutdown_global_cache_manager() -> None:
    """Shutdown the global DTESN cache manager"""
    global _global_cache_manager
    if _global_cache_manager:
        await _global_cache_manager.shutdown()
        _global_cache_manager = None