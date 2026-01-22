import os
from collections import defaultdict, deque
from collections.abc import Iterable, Sequence
from dataclasses import astuple, dataclass
from typing import Any, Callable, NamedTuple, Optional
from loguru import logger
from aphrodite.common.config import AphroditeConfig
from aphrodite.utils import GiB_bytes, cdiv, sha256_cbor_64bit
from aphrodite.v1.kv_cache_interface import ChunkedLocalAttentionSpec, FullAttentionSpec, KVCacheConfig, KVCacheGroupSpec, KVCacheSpec, KVCacheTensor, SlidingWindowSpec
from aphrodite.v1.metrics.stats import PrefixCacheStats
from aphrodite.v1.request import Request
class BlockHash(NamedTuple):
    hash_value: int
    token_ids: tuple[int, ...]
    extra_keys: Optional[Any] = None
class BlockHashWithGroupId(NamedTuple):
    block_hash: BlockHash
    group_id: int
    def get_hash_value(self) -> int:
        return self.block_hash.hash_value
NONE_HASH: int
def init_none_hash(hash_fn: Callable):
    global NONE_HASH
    hash_seed = os.getenv('PYTHONHASHSEED')
    if hash_seed is None and hash_fn is sha256_cbor_64bit:
        logger.warning('PYTHONHASHSEED is not set. This will lead to non-reproducible block-hashes when using sha256_cbor_64bit as the hash function.Consider setting PYTHONHASHSEED to a fixed value for reproducibility.')
    NONE_HASH = int.from_bytes(os.urandom(32), byteorder='big') if hash_seed is None else hash_fn(hash_seed)
class PrefixCachingMetrics:
    def __init__(self, max_recent_requests: int=1000):
        self.max_recent_requests = max_recent_requests
        self.aggregated_requests = 0
        self.aggregated_query_total = 0
        self.aggregated_query_hit = 0
        self.query_queue: deque[tuple[int, int, int]] = deque()
    def observe(self, stats: PrefixCacheStats):
        if stats.reset:
            self.reset()
        self.query_queue.append((stats.requests, stats.queries, stats.hits))
        self.aggregated_requests += stats.requests
        self.aggregated_query_total += stats.queries
        self.aggregated_query_hit += stats.hits
        if self.aggregated_requests > self.max_recent_requests:
            old_requests, old_queries, old_hits = self.query_queue.popleft()
            self.aggregated_requests -= old_requests
            self.aggregated_query_total -= old_queries
            self.aggregated_query_hit -= old_hits
    def reset(self):
        self.aggregated_requests = 0
        self.aggregated_query_total = 0
        self.aggregated_query_hit = 0
        self.query_queue.clear()
    @property
    def hit_rate(self) -> float:
        if self.aggregated_query_total == 0:
            return 0.0
        return self.aggregated_query_hit / self.aggregated_query_total
@dataclass
class KVCacheBlock:
    block_id: int
    ref_cnt: int = 0
    _block_hash: Optional[BlockHashWithGroupId] = None
    prev_free_block: Optional['KVCacheBlock'] = None
    next_free_block: Optional['KVCacheBlock'] = None
    is_null: bool = False
    @property
    def block_hash(self) -> Optional[BlockHashWithGroupId]:
        return self._block_hash
    @block_hash.setter
    def block_hash(self, block_hash: BlockHashWithGroupId):
        assert self.block_hash is None, 'The block already has a hash. This should not happen.'
        self._block_hash = block_hash
    def reset_hash(self):
        self._block_hash = None
    def __repr__(self) -> str:
        prev_block_id = self.prev_free_block.block_id if self.prev_free_block else None
        next_block_id = self.next_free_block.block_id if self.next_free_block else None
        return f'KVCacheBlock(block_id={self.block_id}, ref_cnt={self.ref_cnt}, _block_hash={self._block_hash}, prev_free_block={prev_block_id}, next_free_block={next_block_id})'
class FreeKVCacheBlockQueue:
    def __init__(self, blocks: list[KVCacheBlock]) -> None:
        self.num_free_blocks = len(blocks)
        for i in range(self.num_free_blocks):
            if i > 0:
                blocks[i].prev_free_block = blocks[i - 1]
            if i < self.num_free_blocks - 1:
                blocks[i].next_free_block = blocks[i + 1]
        self.fake_free_list_head = KVCacheBlock(block_id=-1)
        self.fake_free_list_tail = KVCacheBlock(block_id=-1)
        if self.num_free_blocks > 0:
            self.fake_free_list_head.next_free_block = blocks[0]
            blocks[0].prev_free_block = self.fake_free_list_head
            self.fake_free_list_tail.prev_free_block = blocks[-1]
            blocks[-1].next_free_block = self.fake_free_list_tail
        else:
            self.fake_free_list_head.next_free_block = self.fake_free_list_tail
            self.fake_free_list_tail.prev_free_block = self.fake_free_list_head
    def popleft(self) -> KVCacheBlock:
        if self.fake_free_list_head.next_free_block is self.fake_free_list_tail or self.fake_free_list_head.next_free_block is None:
            assert self.num_free_blocks == 0, f'num_free_blocks ({self.num_free_blocks}) is out of sync with the free list.'
            raise ValueError('No free blocks available')
        first_block: KVCacheBlock = self.fake_free_list_head.next_free_block
        if first_block.next_free_block is None:
            raise RuntimeError("Invalid block found in popleft() which doesn't have a valid next_free_block")
        self.fake_free_list_head.next_free_block = first_block.next_free_block
        first_block.next_free_block.prev_free_block = self.fake_free_list_head
        first_block.prev_free_block = first_block.next_free_block = None
        self.num_free_blocks -= 1
        return first_block
    def popleft_n(self, n: int) -> list[KVCacheBlock]:
        if n == 0:
            return []
        assert self.num_free_blocks >= n
        self.num_free_blocks -= n
        curr_block = self.fake_free_list_head.next_free_block
        ret = []
        for _ in range(n):
            assert curr_block is not None
            ret.append(curr_block)
            last_block = curr_block
            curr_block = curr_block.next_free_block
            last_block.prev_free_block = None
            last_block.next_free_block = None
        if curr_block is not None:
            self.fake_free_list_head.next_free_block = curr_block
            curr_block.prev_free_block = self.fake_free_list_head
        return ret
    def remove(self, block: KVCacheBlock) -> None:
        if block.prev_free_block is None or block.next_free_block is None:
            raise RuntimeError(f'remove() called on an invalid block: {block}')
        block.prev_free_block.next_free_block = block.next_free_block
        block.next_free_block.prev_free_block = block.prev_free_block
        block.prev_free_block = block.next_free_block = None
        self.num_free_blocks -= 1
    def append(self, block: KVCacheBlock) -> None:
        if self.fake_free_list_tail.prev_free_block is None:
            raise RuntimeError('prev_free_block of fake_free_list_tail should always exist')
        last_block: KVCacheBlock = self.fake_free_list_tail.prev_free_block
        last_block.next_free_block = block
        block.prev_free_block = last_block
        block.next_free_block = self.fake_free_list_tail
        self.fake_free_list_tail.prev_free_block = block
        self.num_free_blocks += 1
    def append_n(self, blocks: list[KVCacheBlock]) -> None:
        if len(blocks) == 0:
            return
        self.num_free_blocks += len(blocks)
        last_block = self.fake_free_list_tail.prev_free_block
        assert last_block is not None, 'prev_free_block of fake_free_list_tail should always exist'
        for block in blocks:
            block.prev_free_block = last_block
            last_block.next_free_block = block
            last_block = block
        last_block.next_free_block = self.fake_free_list_tail
        self.fake_free_list_tail.prev_free_block = last_block
    def get_all_free_blocks(self) -> list[KVCacheBlock]:
        ret = []
        if self.fake_free_list_head.next_free_block is None:
            raise RuntimeError('next_free_block of fake_free_list_head should always exist')
        curr_block: KVCacheBlock = self.fake_free_list_head.next_free_block
        while curr_block.next_free_block is not None:
            ret.append(curr_block)
            curr_block = curr_block.next_free_block
        return ret
def need_extra_keys(request: Request) -> bool:
    return bool(request.mm_hashes) or request.lora_request is not None or request.cache_salt is not None
def _gen_mm_extra_hash_keys(request: Request, start_token_idx: int, end_token_idx: int, start_mm_idx: int) -> tuple[list[Any], int]:
    extra_keys: list[Any] = []
    mm_positions, mm_hashes = (request.mm_positions, request.mm_hashes)
    if not mm_positions:
        return (extra_keys, start_mm_idx)
    if mm_positions and len(mm_positions) != len(mm_hashes):
        raise ValueError('The number of multi-modal positions and hashes must match. This is likely because you do not enable MM preprocessor hashing. Please set disable_mm_preprocessor_cache=False.')
    if mm_positions[-1].offset + mm_positions[-1].length < start_token_idx:
        return (extra_keys, start_mm_idx)
    if start_mm_idx < 0:
        assert -start_mm_idx <= len(mm_positions)
        start_mm_idx = len(mm_positions) + start_mm_idx
    curr_mm_idx = start_mm_idx
    while mm_positions and curr_mm_idx < len(mm_positions):
        assert mm_hashes[curr_mm_idx] is not None
        offset = mm_positions[curr_mm_idx].offset
        length = mm_positions[curr_mm_idx].length
        if end_token_idx > offset:
            if start_token_idx > offset + length:
                curr_mm_idx += 1
                continue
            extra_keys.append(mm_hashes[curr_mm_idx])
            if end_token_idx >= offset + length:
                curr_mm_idx += 1
            else:
                break
        else:
            break
    return (extra_keys, curr_mm_idx)
def _gen_lora_extra_hash_keys(request: Request) -> list[int]:
    if not request.lora_request:
        return []
    return [request.lora_request.lora_int_id]
def generate_block_hash_extra_keys(request: Request, start_token_idx: int, end_token_idx: int, start_mm_idx: int) -> tuple[Optional[tuple[Any, ...]], int]:
    mm_extra_keys: list[Any]
    mm_extra_keys, new_start_mm_idx = _gen_mm_extra_hash_keys(request, start_token_idx, end_token_idx, start_mm_idx)
    lora_extra_keys: list[int] = _gen_lora_extra_hash_keys(request)
    cache_salt_keys: list[str] = [request.cache_salt] if start_token_idx == 0 and request.cache_salt else []
    extra_keys: list[Any] = lora_extra_keys + mm_extra_keys + cache_salt_keys
    if not extra_keys:
        return (None, new_start_mm_idx)
    return (tuple(extra_keys), new_start_mm_idx)
def hash_block_tokens(hash_function: Callable, parent_block_hash: Optional[int], curr_block_token_ids: Sequence[int], extra_keys: Optional[tuple[Any, ...]]=None) -> BlockHash:
    if not parent_block_hash:
        parent_block_hash = NONE_HASH
    curr_block_token_ids_tuple = tuple(curr_block_token_ids)
    return BlockHash(hash_function((parent_block_hash, curr_block_token_ids_tuple, extra_keys)), curr_block_token_ids_tuple, extra_keys)
def hash_request_tokens(hash_function: Any, block_size: int, request: Request) -> list[BlockHash]:
    token_ids = request.all_token_ids
    req_need_extra_keys = need_extra_keys(request)
    req_extra_keys = None
    curr_mm_idx = 0
    ret = []
    parent_block_hash_value = None
    for start in range(0, len(token_ids) - block_size + 1, block_size):
        end = start + block_size
        block_token_ids = token_ids[start:end]
        if req_need_extra_keys:
            req_extra_keys, curr_mm_idx = generate_block_hash_extra_keys(request, start, end, curr_mm_idx)
        block_hash = hash_block_tokens(hash_function, parent_block_hash_value, block_token_ids, req_extra_keys)
        ret.append(block_hash)
        parent_block_hash_value = block_hash.hash_value
    return ret
def max_memory_usage_bytes(aphrodite_config: AphroditeConfig, kv_cache_specs: Iterable[KVCacheSpec]) -> int:
    return sum((spec.max_memory_usage_bytes(aphrodite_config) for spec in kv_cache_specs))
def estimate_max_model_len(aphrodite_config: AphroditeConfig, kv_cache_spec: dict[str, KVCacheSpec], available_memory: int) -> int:
    def fits_in_memory(model_len: int) -> bool:
        aphrodite_config.model_config.max_model_len = model_len
        memory_needed = max_memory_usage_bytes(aphrodite_config, kv_cache_spec.values())
        return memory_needed <= available_memory
    current_max = aphrodite_config.model_config.max_model_len
    left, right = (1, current_max)
    if not fits_in_memory(left):
        return 0
    result = 1
    while left <= right:
        mid = (left + right) // 2
        if fits_in_memory(mid):
            result = mid
            left = mid + 1
        else:
            right = mid - 1
    return result
def check_enough_kv_cache_memory(aphrodite_config: AphroditeConfig, kv_cache_spec: dict[str, KVCacheSpec], available_memory: int):
    if not kv_cache_spec:
        return
    if available_memory <= 0:
        raise ValueError('No available memory for the cache blocks. Try increasing `gpu_memory_utilization` when initializing the engine.')
    max_model_len = aphrodite_config.model_config.max_model_len
    needed_memory = max_memory_usage_bytes(aphrodite_config, kv_cache_spec.values())
    if needed_memory > available_memory:
        estimated_max_len = estimate_max_model_len(aphrodite_config, kv_cache_spec, available_memory)
        estimated_msg = ''
        if estimated_max_len > 0:
            estimated_msg = f'Based on the available memory, the estimated maximum model length is {estimated_max_len}.'
        raise ValueError(f"To serve at least one request with the models's max seq len ({max_model_len}), ({needed_memory / GiB_bytes:.2f} GiB KV cache is needed, which is larger than the available KV cache memory ({available_memory / GiB_bytes:.2f} GiB). {estimated_msg} Try increasing `gpu_memory_utilization` or decreasing `max_model_len` when initializing the engine.")
def create_kv_cache_group_specs(kv_cache_spec: dict[str, KVCacheSpec], grouped_layer_names: list[list[str]]) -> list[KVCacheGroupSpec]:
    kv_cache_groups = []
    for layer_names_one_group in grouped_layer_names:
        layer_specs = [kv_cache_spec[layer_name] for layer_name in layer_names_one_group]
        merged_layer_spec = layer_specs[0].merge(layer_specs)
        kv_cache_groups.append(KVCacheGroupSpec(layer_names_one_group, merged_layer_spec))
    return kv_cache_groups
def is_kv_cache_type_uniform(kv_cache_spec: dict[str, KVCacheSpec]) -> bool:
    try:
        kv_cache_spec_values = list(kv_cache_spec.values())
        _ = kv_cache_spec_values[0].merge(kv_cache_spec_values)
    except AssertionError:
        return False
    return True
def get_max_concurrency_for_kv_cache_config(aphrodite_config: AphroditeConfig, kv_cache_config: KVCacheConfig) -> float:
    num_layer_per_group = max((len(group.layer_names) for group in kv_cache_config.kv_cache_groups))
    max_memory_usage_per_request = num_layer_per_group * max_memory_usage_bytes(aphrodite_config, (group.kv_cache_spec for group in kv_cache_config.kv_cache_groups))
    memory_per_block = kv_cache_config.kv_cache_groups[0].kv_cache_spec.page_size_bytes * num_layer_per_group
    num_block_per_request = cdiv(max_memory_usage_per_request, memory_per_block)
    max_concurrency = kv_cache_config.num_blocks / num_block_per_request
    return max_concurrency
def get_num_blocks(aphrodite_config: AphroditeConfig, num_layers: int, available_memory: int, page_size: int) -> int:
    num_blocks = int(available_memory // page_size // num_layers)
    num_blocks = max(num_blocks, 0)
    if aphrodite_config.cache_config.num_gpu_blocks_override is not None:
        num_gpu_blocks_override = aphrodite_config.cache_config.num_gpu_blocks_override
        logger.info('Overriding num_gpu_blocks={} with num_gpu_blocks_override={}', num_blocks, num_gpu_blocks_override)
        num_blocks = num_gpu_blocks_override
    return num_blocks
def get_uniform_page_size(kv_cache_spec: dict[str, KVCacheSpec]) -> int:
    page_sizes = set((layer.page_size_bytes for layer in kv_cache_spec.values()))
    assert len(page_sizes) == 1
    return page_sizes.pop()
def _get_kv_cache_config_uniform_type(aphrodite_config: AphroditeConfig, kv_cache_spec: dict[str, KVCacheSpec], available_memory: int) -> KVCacheConfig:
    page_size = get_uniform_page_size(kv_cache_spec)
    num_blocks = get_num_blocks(aphrodite_config, len(kv_cache_spec), available_memory, page_size)
    per_layer_size = page_size * num_blocks
    grouped_layer_names = [list(kv_cache_spec.keys())]
    kv_cache_tensors = [KVCacheTensor(size=per_layer_size, shared_by=[layer_name]) for layer_name in kv_cache_spec]
    kv_cache_config = KVCacheConfig(num_blocks=num_blocks, kv_cache_tensors=kv_cache_tensors, kv_cache_groups=create_kv_cache_group_specs(kv_cache_spec, grouped_layer_names))
    num_tokens = num_blocks * aphrodite_config.cache_config.block_size
    num_tokens_str = f'{num_tokens:,}'
    max_concurrency = get_max_concurrency_for_kv_cache_config(aphrodite_config, kv_cache_config)
    total_kv_cache_memory = available_memory
    total_kv_cache_memory_gib = total_kv_cache_memory / GiB_bytes
    logger.info('GPU KV cache size: {} tokens ({:.2f} GiB, {:.1f}x concurrency)', num_tokens_str, total_kv_cache_memory_gib, max_concurrency)
    return kv_cache_config
def is_kv_cache_page_size_uniform(kv_cache_spec: dict[str, KVCacheSpec]) -> bool:
    page_sizes = {layer.page_size_bytes for layer in kv_cache_spec.values()}
    return len(page_sizes) == 1
def is_kv_cache_type_attention_free(kv_cache_spec: dict[str, KVCacheSpec]) -> bool:
    return not kv_cache_spec
def _get_kv_cache_config_uniform_page_size(aphrodite_config: AphroditeConfig, kv_cache_spec: dict[str, KVCacheSpec], available_memory: int) -> KVCacheConfig:
    same_type_layers: dict[KVCacheSpec, list[str]] = defaultdict(list)
    for layer_name, layer_spec in kv_cache_spec.items():
        same_type_layers[layer_spec].append(layer_name)
    group_size = min([len(layers) for layers in same_type_layers.values()])
    grouped_layers = []
    for layers in same_type_layers.values():
        num_padding_layers = group_size - len(layers) % group_size
        if num_padding_layers != group_size:
            logger.warning('Add {} padding layers, may waste at most {:.2f}% KV cache memory', num_padding_layers, num_padding_layers / len(layers) * 100)
        for i in range(0, len(layers), group_size):
            grouped_layers.append(layers[i:i + group_size])
    kv_cache_groups = create_kv_cache_group_specs(kv_cache_spec, grouped_layers)
    page_size = get_uniform_page_size(kv_cache_spec)
    num_blocks = get_num_blocks(aphrodite_config, group_size, available_memory, page_size)
    per_memory_pool_size = page_size * num_blocks
    kv_cache_tensors = []
    for i in range(group_size):
        shared_by = []
        for j in range(len(kv_cache_groups)):
            if i < len(grouped_layers[j]):
                shared_by.append(grouped_layers[j][i])
        kv_cache_tensors.append(KVCacheTensor(size=per_memory_pool_size, shared_by=shared_by))
    kv_cache_config = KVCacheConfig(num_blocks=num_blocks, kv_cache_tensors=kv_cache_tensors, kv_cache_groups=kv_cache_groups)
    min_block_size = min([group.kv_cache_spec.block_size for group in kv_cache_groups])
    num_tokens = num_blocks // len(grouped_layers) * min_block_size
    num_tokens_str = f'{num_tokens:,}'
    logger.info('GPU KV cache size: {} tokens', num_tokens_str)
    max_model_len_str = f'{aphrodite_config.model_config.max_model_len:,}'
    max_concurrency = get_max_concurrency_for_kv_cache_config(aphrodite_config, kv_cache_config)
    logger.info('Maximum concurrency for {} tokens per request: {:.2f}x', max_model_len_str, max_concurrency)
    return kv_cache_config
def _get_kv_cache_config_attention_free() -> KVCacheConfig:
    return KVCacheConfig(num_blocks=1, kv_cache_tensors=[], kv_cache_groups=[])
def unify_hybrid_kv_cache_specs(kv_cache_spec: dict[str, KVCacheSpec]):
    if is_kv_cache_type_uniform(kv_cache_spec):
        return
    logger.warning('Hybrid KV cache manager is disabled for this hybrid model, This means we do not enable any optimizations for saving KV cache memory (e.g., dropping the KV cache outside the sliding window). The compute of layers like sliding window is still saved.')
    has_full_attention = any((isinstance(spec, FullAttentionSpec) for spec in kv_cache_spec.values()))
    has_sliding_window = any((isinstance(spec, SlidingWindowSpec) for spec in kv_cache_spec.values()))
    has_chunked_local_attention = any((isinstance(spec, ChunkedLocalAttentionSpec) for spec in kv_cache_spec.values()))
    if has_full_attention and (has_sliding_window or has_chunked_local_attention):
        for layer_name, spec in kv_cache_spec.items():
            if isinstance(spec, SlidingWindowSpec):
                kv_cache_spec[layer_name] = FullAttentionSpec(block_size=spec.block_size, num_kv_heads=spec.num_kv_heads, head_size=spec.head_size, dtype=spec.dtype, use_mla=spec.use_mla, sliding_window=spec.sliding_window)
            elif isinstance(spec, ChunkedLocalAttentionSpec):
                kv_cache_spec[layer_name] = FullAttentionSpec(block_size=spec.block_size, num_kv_heads=spec.num_kv_heads, head_size=spec.head_size, dtype=spec.dtype, use_mla=spec.use_mla, attention_chunk_size=spec.attention_chunk_size)
    if not is_kv_cache_type_uniform(kv_cache_spec):
        raise ValueError('Hybrid KV cache manager is disabled but failed to convert the KV cache specs to one unified type.')
def get_kv_cache_config(aphrodite_config: AphroditeConfig, kv_cache_spec: dict[str, KVCacheSpec], available_memory: int) -> KVCacheConfig:
    check_enough_kv_cache_memory(aphrodite_config, kv_cache_spec, available_memory)
    if aphrodite_config.scheduler_config.disable_hybrid_kv_cache_manager:
        unify_hybrid_kv_cache_specs(kv_cache_spec)
    if is_kv_cache_type_attention_free(kv_cache_spec):
        return _get_kv_cache_config_attention_free()
    elif is_kv_cache_type_uniform(kv_cache_spec):
        return _get_kv_cache_config_uniform_type(aphrodite_config, kv_cache_spec, available_memory)
    elif is_kv_cache_page_size_uniform(kv_cache_spec):
        return _get_kv_cache_config_uniform_page_size(aphrodite_config, kv_cache_spec, available_memory)
    raise NotImplementedError
def unify_kv_cache_configs(kv_cache_configs: list[KVCacheConfig]):
    for kv_cache_config in kv_cache_configs:
        kv_cache_config.kv_cache_groups.sort(key=lambda x: (type(x.kv_cache_spec).__name__, astuple(x.kv_cache_spec)))
    for kv_cache_config in kv_cache_configs[1:]:
        for group_rank_0, group_rank_i in zip(kv_cache_configs[0].kv_cache_groups, kv_cache_config.kv_cache_groups):
            assert group_rank_0.kv_cache_spec == group_rank_i.kv_cache_spec
    min_num_blocks = min((kv_cache_config.num_blocks for kv_cache_config in kv_cache_configs))
    for kv_cache_config in kv_cache_configs:
        kv_cache_config.num_blocks = min_num_blocks
    return kv_cache_configs