from collections import defaultdict
from collections.abc import Iterable
from typing import Callable, Optional
from loguru import logger
from aphrodite.distributed.kv_events import AllBlocksCleared, BlockRemoved, BlockStored, KVCacheEvent
from aphrodite.v1.core.kv_cache_utils import BlockHash, BlockHashWithGroupId, FreeKVCacheBlockQueue, KVCacheBlock, generate_block_hash_extra_keys, hash_block_tokens
from aphrodite.v1.request import Request
class BlockPool:
    def __init__(self, num_gpu_blocks: int, enable_caching: bool, enable_kv_cache_events: bool=False):
        assert isinstance(num_gpu_blocks, int) and num_gpu_blocks > 0
        self.num_gpu_blocks = num_gpu_blocks
        self.enable_caching = enable_caching
        self.blocks: list[KVCacheBlock] = [KVCacheBlock(idx) for idx in range(num_gpu_blocks)]
        self.free_block_queue = FreeKVCacheBlockQueue(self.blocks)
        self.cached_block_hash_to_block: dict[BlockHashWithGroupId, dict[int, KVCacheBlock]] = defaultdict(dict)
        self.null_block = self.free_block_queue.popleft()
        self.null_block.is_null = True
        self.enable_kv_cache_events = enable_kv_cache_events
        self.kv_event_queue: list[KVCacheEvent] = []
    def get_cached_block(self, block_hash: BlockHash, kv_cache_group_ids: list[int]) -> Optional[list[KVCacheBlock]]:
        cached_blocks = []
        for group_id in kv_cache_group_ids:
            cached_blocks_one_group = self.cached_block_hash_to_block.get(BlockHashWithGroupId(block_hash, group_id))
            if not cached_blocks_one_group:
                return None
            first_block = next(iter(cached_blocks_one_group.values()))
            cached_blocks.append(first_block)
        return cached_blocks
    def cache_full_blocks(self, request: Request, blocks: list[KVCacheBlock], block_hashes: list[BlockHash], num_cached_blocks: int, num_full_blocks: int, block_size: int, kv_cache_group_id: int, hash_fn: Callable) -> None:
        if num_cached_blocks == num_full_blocks:
            return
        new_full_blocks = blocks[num_cached_blocks:num_full_blocks]
        assert len(block_hashes) >= num_cached_blocks
        new_block_hashes = block_hashes[num_cached_blocks:]
        if num_cached_blocks == 0:
            prev_block_hash_value = None
        else:
            prev_block = blocks[num_cached_blocks - 1]
            assert prev_block.block_hash is not None
            prev_block_hash_value = prev_block.block_hash.get_hash_value()
        parent_block_hash = prev_block_hash_value
        new_hashes: Optional[list[int]] = [] if self.enable_kv_cache_events else None
        for i, blk in enumerate(new_full_blocks):
            assert blk.block_hash is None
            if i < len(new_block_hashes):
                block_hash = new_block_hashes[i]
            else:
                blk_idx = num_cached_blocks + i
                start_token_idx = blk_idx * block_size
                end_token_idx = (blk_idx + 1) * block_size
                block_tokens = request.all_token_ids[start_token_idx:end_token_idx]
                assert len(block_tokens) == block_size, f'Expected {block_size} tokens, got {len(block_tokens)} at {blk_idx}th block for request {request.request_id}({request})'
                extra_keys, _ = generate_block_hash_extra_keys(request, start_token_idx, end_token_idx, -1)
                block_hash = hash_block_tokens(hash_fn, prev_block_hash_value, block_tokens, extra_keys)
                block_hashes.append(block_hash)
            block_hash_with_group_id = BlockHashWithGroupId(block_hash, kv_cache_group_id)
            blk.block_hash = block_hash_with_group_id
            self.cached_block_hash_to_block[block_hash_with_group_id][blk.block_id] = blk
            if new_hashes is not None:
                new_hashes.append(block_hash.hash_value)
            prev_block_hash_value = block_hash.hash_value
        if self.enable_kv_cache_events:
            self.kv_event_queue.append(BlockStored(block_hashes=new_hashes, parent_block_hash=parent_block_hash, token_ids=request.all_token_ids[num_cached_blocks * block_size:num_full_blocks * block_size], block_size=block_size, lora_id=request.lora_request.id if request.lora_request else None))
    def get_new_blocks(self, num_blocks: int) -> list[KVCacheBlock]:
        if num_blocks > self.get_num_free_blocks():
            raise ValueError(f'Cannot get {num_blocks} free blocks from the pool')
        ret: list[KVCacheBlock] = self.free_block_queue.popleft_n(num_blocks)
        if self.enable_caching:
            for block in ret:
                self._maybe_evict_cached_block(block)
                assert block.ref_cnt == 0
                block.ref_cnt += 1
        else:
            for block in ret:
                assert block.ref_cnt == 0
                block.ref_cnt += 1
        return ret
    def _maybe_evict_cached_block(self, block: KVCacheBlock) -> bool:
        block_hash = block.block_hash
        if block_hash is None:
            return False
        blocks_by_id = self.cached_block_hash_to_block.get(block_hash)
        if blocks_by_id is None:
            return False
        block.reset_hash()
        blocks_by_id.pop(block.block_id, None)
        if len(blocks_by_id) == 0:
            del self.cached_block_hash_to_block[block_hash]
        if self.enable_kv_cache_events:
            self.kv_event_queue.append(BlockRemoved(block_hashes=[block_hash.get_hash_value()]))
        return True
    def touch(self, blocks: tuple[list[KVCacheBlock], ...]) -> None:
        for blocks_per_group in blocks:
            for block in blocks_per_group:
                if block.ref_cnt == 0 and (not block.is_null):
                    self.free_block_queue.remove(block)
                block.ref_cnt += 1
    def free_blocks(self, ordered_blocks: Iterable[KVCacheBlock]) -> None:
        blocks_list = list(ordered_blocks)
        for block in blocks_list:
            block.ref_cnt -= 1
        self.free_block_queue.append_n([block for block in blocks_list if block.ref_cnt == 0 and (not block.is_null)])
    def reset_prefix_cache(self) -> bool:
        num_used_blocks = self.num_gpu_blocks - self.get_num_free_blocks()
        if num_used_blocks != 1:
            logger.warning('Failed to reset prefix cache because some blocks ({}) are not freed yet', num_used_blocks - 1)
            return False
        self.cached_block_hash_to_block = defaultdict(dict)
        for block in self.blocks:
            block.reset_hash()
        logger.info('Successfully reset prefix cache')
        if self.enable_kv_cache_events:
            self.kv_event_queue.append(AllBlocksCleared())
        return True
    def get_num_free_blocks(self) -> int:
        return self.free_block_queue.num_free_blocks
    def get_usage(self) -> float:
        return 1.0 - self.get_num_free_blocks() / self.num_gpu_blocks
    def take_events(self) -> list[KVCacheEvent]:
        if not self.enable_kv_cache_events:
            return []
        events = self.kv_event_queue
        self.kv_event_queue = []
        return events