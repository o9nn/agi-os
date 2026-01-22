from collections import defaultdict
from dataclasses import dataclass
from typing import Optional
from aphrodite.distributed.kv_events import KVCacheEvent
from aphrodite.utils import sha256, sha256_cbor_64bit
from aphrodite.v1.core.kv_cache_coordinator import get_kv_cache_coordinator
from aphrodite.v1.core.kv_cache_utils import BlockHash, KVCacheBlock, hash_request_tokens, init_none_hash
from aphrodite.v1.kv_cache_interface import KVCacheConfig
from aphrodite.v1.metrics.stats import PrefixCacheStats
from aphrodite.v1.request import Request, RequestStatus
@dataclass
class KVCacheBlocks:
    blocks: tuple[list[KVCacheBlock], ...]
    "\n    blocks[i][j] refers to the i-th kv_cache_group and the j-th block of tokens.\n    We don't use block of tokens as the outer dimension because it assumes all\n    kv_cache_groups have the same number of blocks, which is true for now but \n    will be broken if we want to give different block_size to different \n    kv_cache_groups in the future.\n    "
    def __add__(self, other: 'KVCacheBlocks') -> 'KVCacheBlocks':
        return KVCacheBlocks(tuple((blk1 + blk2 for blk1, blk2 in zip(self.blocks, other.blocks))))
    def get_block_ids(self) -> tuple[list[int], ...]:
        return tuple(([blk.block_id for blk in group] for group in self.blocks))
    def get_unhashed_block_ids(self) -> list[int]:
        assert len(self.blocks) == 1, 'Only one group is supported'
        return [block.block_id for block in self.blocks[0] if block.block_hash is None]
    def new_empty(self) -> 'KVCacheBlocks':
        return KVCacheBlocks(tuple(([] for _ in range(len(self.blocks)))))
class KVCacheManager:
    def __init__(self, kv_cache_config: KVCacheConfig, max_model_len: int, enable_caching: bool=True, caching_hash_algo: str='builtin', use_eagle: bool=False, log_stats: bool=False, enable_kv_cache_events: bool=False) -> None:
        self.max_model_len = max_model_len
        if len(kv_cache_config.kv_cache_groups) == 0:
            enable_caching = False
        self.enable_caching = enable_caching
        self.caching_hash_fn = sha256_cbor_64bit if caching_hash_algo == 'sha256_cbor_64bit' else sha256 if caching_hash_algo == 'sha256' else hash
        init_none_hash(self.caching_hash_fn)
        self.use_eagle = use_eagle
        self.log_stats = log_stats
        self.prefix_cache_stats = PrefixCacheStats() if log_stats else None
        self.block_size: Optional[int] = None
        if self.enable_caching:
            assert len(set((g.kv_cache_spec.block_size for g in kv_cache_config.kv_cache_groups))) == 1, 'Only one block size is supported for now'
            self.block_size = kv_cache_config.kv_cache_groups[0].kv_cache_spec.block_size
        self.coordinator = get_kv_cache_coordinator(kv_cache_config=kv_cache_config, max_model_len=self.max_model_len, use_eagle=self.use_eagle, enable_caching=self.enable_caching, caching_hash_fn=self.caching_hash_fn, enable_kv_cache_events=enable_kv_cache_events)
        self.num_kv_cache_groups = len(kv_cache_config.kv_cache_groups)
        self.block_pool = self.coordinator.block_pool
        self.kv_cache_config = kv_cache_config
        self.req_to_block_hashes: defaultdict[str, list[BlockHash]] = defaultdict(list)
    @property
    def usage(self) -> float:
        return self.block_pool.get_usage()
    def make_prefix_cache_stats(self) -> Optional[PrefixCacheStats]:
        if not self.log_stats:
            return None
        stats = self.prefix_cache_stats
        self.prefix_cache_stats = PrefixCacheStats()
        return stats
    def get_computed_blocks(self, request: Request) -> tuple[KVCacheBlocks, int]:
        if not self.enable_caching or (request.sampling_params is not None and request.sampling_params.prompt_logprobs is not None):
            return (self.create_empty_block_list(), 0)
        block_hashes = self.req_to_block_hashes[request.request_id]
        if not block_hashes:
            assert self.block_size is not None
            block_hashes = hash_request_tokens(self.caching_hash_fn, self.block_size, request)
            self.req_to_block_hashes[request.request_id] = block_hashes
        max_cache_hit_length = request.num_tokens - 1
        computed_blocks, num_new_computed_tokens = self.coordinator.find_longest_cache_hit(block_hashes, max_cache_hit_length)
        if self.log_stats:
            assert self.prefix_cache_stats is not None
            self.prefix_cache_stats.requests += 1
            self.prefix_cache_stats.queries += request.num_tokens
            self.prefix_cache_stats.hits += num_new_computed_tokens
        return (KVCacheBlocks(computed_blocks), num_new_computed_tokens)
    def allocate_slots(self, request: Request, num_new_tokens: int, num_new_computed_tokens: int=0, new_computed_blocks: Optional[KVCacheBlocks]=None, num_lookahead_tokens: int=0, delay_cache_blocks: bool=False) -> Optional[KVCacheBlocks]:
        if num_new_tokens == 0:
            raise ValueError('num_new_tokens must be greater than 0')
        if new_computed_blocks is not None:
            new_computed_block_list = new_computed_blocks.blocks
        else:
            new_computed_block_list = tuple(([] for _ in range(len(self.kv_cache_config.kv_cache_groups))))
        self.coordinator.remove_skipped_blocks(request.request_id, request.num_computed_tokens)
        num_computed_tokens = request.num_computed_tokens + num_new_computed_tokens
        num_tokens_need_slot = min(num_computed_tokens + num_new_tokens + num_lookahead_tokens, self.max_model_len)
        num_blocks_to_allocate = self.coordinator.get_num_blocks_to_allocate(request_id=request.request_id, num_tokens=num_tokens_need_slot, new_computed_blocks=new_computed_block_list)
        if num_blocks_to_allocate > self.block_pool.get_num_free_blocks():
            return None
        if self.enable_caching:
            self.block_pool.touch(new_computed_block_list)
        else:
            assert not any(new_computed_block_list), 'Computed blocks should be empty when prefix caching is disabled'
        self.coordinator.save_new_computed_blocks(request.request_id, new_computed_block_list)
        new_blocks = self.coordinator.allocate_new_blocks(request.request_id, num_tokens_need_slot)
        if not self.enable_caching or delay_cache_blocks:
            return KVCacheBlocks(new_blocks)
        num_tokens_to_cache = min(num_computed_tokens + num_new_tokens, request.num_tokens)
        self.coordinator.cache_blocks(request, self.req_to_block_hashes[request.request_id], num_tokens_to_cache)
        return KVCacheBlocks(new_blocks)
    def free(self, request: Request) -> None:
        self.coordinator.free(request.request_id)
    def reset_prefix_cache(self) -> bool:
        if not self.block_pool.reset_prefix_cache():
            return False
        if self.log_stats:
            assert self.prefix_cache_stats is not None
            self.prefix_cache_stats.reset = True
        return True
    def get_num_common_prefix_blocks(self, request: Request, num_running_requests: int) -> list[int]:
        assert request.status == RequestStatus.RUNNING
        return self.coordinator.get_num_common_prefix_blocks(request.request_id, num_running_requests)
    def free_block_hashes(self, request: Request) -> None:
        self.req_to_block_hashes.pop(request.request_id, None)
    def take_events(self) -> list[KVCacheEvent]:
        return self.block_pool.take_events()
    def get_block_ids(self, request_id: str) -> tuple[list[int], ...]:
        return KVCacheBlocks(self.coordinator.get_blocks(request_id)).get_block_ids()
    def cache_blocks(self, request: Request, num_computed_tokens: int) -> None:
        if self.enable_caching:
            block_hashes = self.req_to_block_hashes[request.request_id]
            self.coordinator.cache_blocks(request, block_hashes, num_computed_tokens)
    def create_empty_block_list(self) -> KVCacheBlocks:
        return KVCacheBlocks(tuple(([] for _ in range(self.num_kv_cache_groups))))