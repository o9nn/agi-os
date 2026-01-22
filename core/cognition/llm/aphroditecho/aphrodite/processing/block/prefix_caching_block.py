import sys
from bisect import bisect_left
from os.path import commonprefix
from typing import Callable, Dict, FrozenSet, Iterable, List, Optional, Set, Tuple
from loguru import logger
from aphrodite.common.sequence import Sequence
from aphrodite.processing.block.common import CacheMetricData, CopyOnWriteTracker, get_all_blocks_recursively
from aphrodite.processing.block.interfaces import Block, BlockAllocator, BlockId, Device, DeviceAwareBlockAllocator
from aphrodite.processing.block.naive_block import BlockPool, NaiveBlock, NaiveBlockAllocator
from aphrodite.processing.evictor import EvictionPolicy, Evictor, make_evictor
PrefixHash = int
_DEFAULT_LAST_ACCESSED_TIME = -1
class BlockTracker:
    __slots__ = ('active', 'last_accessed', 'computed')
    def reset(self):
        self.last_accessed: float = _DEFAULT_LAST_ACCESSED_TIME
        self.computed: bool = False
    def __init__(self):
        self.active: bool = False
        self.reset()
    def enable(self):
        assert not self.active
        self.active = True
        self.reset()
    def disable(self):
        assert self.active
        self.active = False
        self.reset()
class PrefixCachingBlockAllocator(BlockAllocator):
    _none_hash: int = hash('None')
    def __init__(self, num_blocks: int, block_size: int, block_ids: Optional[Iterable[int]]=None, eviction_policy: EvictionPolicy=EvictionPolicy.LRU):
        if block_ids is None:
            block_ids = range(num_blocks)
        self._block_size = block_size
        self._cached_blocks: Dict[PrefixHash, BlockId] = {}
        self._touched_blocks: Set[BlockId] = set()
        self._block_tracker: Dict[BlockId, BlockTracker] = {}
        for block_id in block_ids:
            self._block_tracker[block_id] = BlockTracker()
        extra_factor = 4
        self._block_pool = BlockPool(self._block_size, self._create_block, self, num_blocks * extra_factor)
        self._hashless_allocator = NaiveBlockAllocator(create_block=self._create_block, num_blocks=num_blocks, block_size=block_size, block_ids=block_ids, block_pool=self._block_pool)
        self.eviction_policy = eviction_policy
        self.evictor: Evictor = make_evictor(self.eviction_policy)
        self._refcounter = self._hashless_allocator.refcounter
        self._cow_tracker = CopyOnWriteTracker(refcounter=self._refcounter.as_readonly())
        self.metric_data = CacheMetricData()
    def _create_block(self, prev_block: Optional[Block], token_ids: List[int], block_size: int, allocator: BlockAllocator, block_id: Optional[int]=None, computed: bool=False, extra_hash: Optional[int]=None) -> Block:
        allocator = self
        return PrefixCachingBlock(prev_block=prev_block, token_ids=token_ids, block_size=block_size, block_id=block_id, allocator=allocator, computed=computed, extra_hash=extra_hash)
    def allocate_immutable_block(self, prev_block: Optional[Block], token_ids: List[int], extra_hash: Optional[int]=None, device: Optional[Device]=None) -> Block:
        assert device is None
        assert_prefix_caching_block_or_none(prev_block)
        block = self._block_pool.init_block(prev_block=prev_block, token_ids=token_ids, block_size=self._block_size, physical_block_id=None, extra_hash=extra_hash)
        assert block.content_hash is not None
        cached_block_id = self._cached_blocks.get(block.content_hash, None)
        if cached_block_id is not None:
            self.metric_data.query(hit=True)
            block.block_id = cached_block_id
            self._incr_refcount_cached_block(block)
            return block
        self.metric_data.query(hit=False)
        self._block_pool.free_block(block)
        block = self.allocate_mutable_block(prev_block, extra_hash=extra_hash)
        block.append_token_ids(token_ids)
        return block
    def allocate_immutable_blocks(self, prev_block: Optional[Block], block_token_ids: List[List[int]], extra_hash: Optional[int]=None, device: Optional[Device]=None) -> List[Block]:
        blocks = []
        for token_ids in block_token_ids:
            prev_block = self.allocate_immutable_block(prev_block=prev_block, token_ids=token_ids, device=device, extra_hash=extra_hash)
            blocks.append(prev_block)
        return blocks
    def allocate_mutable_block(self, prev_block: Optional[Block], extra_hash: Optional[int]=None, device: Optional[Device]=None) -> Block:
        assert device is None
        assert_prefix_caching_block_or_none(prev_block)
        block_id = self._allocate_block_id()
        block = self._block_pool.init_block(prev_block=prev_block, token_ids=[], block_size=self._block_size, physical_block_id=block_id, extra_hash=extra_hash)
        assert not block.computed
        assert block.content_hash is None
        return block
    def _incr_refcount_cached_block(self, block: Block) -> None:
        block.computed = True
        block_id = block.block_id
        assert block_id is not None
        refcount = self._refcounter.incr(block_id)
        if refcount == 1:
            if block_id in self.evictor:
                self.evictor.remove(block_id)
            self._track_block_id(block_id, computed=True)
    def _decr_refcount_cached_block(self, block: Block) -> None:
        assert block.content_hash is not None
        block_id = block.block_id
        assert block_id is not None
        refcount = self._refcounter.decr(block_id)
        if refcount > 0:
            block.block_id = None
            return
        else:
            assert refcount == 0
        assert block.content_hash in self._cached_blocks
        self.evictor.add(block_id, block.content_hash, block.num_tokens_total, self._block_tracker[block_id].last_accessed)
        self._untrack_block_id(block_id)
        block.block_id = None
    def _decr_refcount_hashless_block(self, block: Block) -> None:
        block_id = block.block_id
        assert block_id is not None
        refcount = self._refcounter.get(block_id)
        if refcount == 1:
            self._untrack_block_id(block_id)
        self._hashless_allocator.free(block, keep_block_object=True)
    def _allocate_block_id(self) -> BlockId:
        hashless_block_id = self._maybe_allocate_hashless_block_id()
        if hashless_block_id is not None:
            return hashless_block_id
        evicted_block_id = self._maybe_allocate_evicted_block_id()
        if evicted_block_id is not None:
            return evicted_block_id
        raise BlockAllocator.NoFreeBlocksError()
    def _maybe_allocate_hashless_block_id(self) -> Optional[BlockId]:
        try:
            block = self._hashless_allocator.allocate_mutable_block(prev_block=None)
            block_id = block.block_id
            self._block_pool.free_block(block)
            self._track_block_id(block_id, computed=False)
            return block_id
        except BlockAllocator.NoFreeBlocksError:
            return None
    def _maybe_allocate_evicted_block_id(self) -> Optional[BlockId]:
        if self.evictor.num_blocks == 0:
            return None
        block_id, content_hash_to_evict = self.evictor.evict()
        assert content_hash_to_evict in self._cached_blocks
        _block_id = self._cached_blocks[content_hash_to_evict]
        assert self._refcounter.get(_block_id) == 0
        assert _block_id == block_id
        self._cached_blocks.pop(content_hash_to_evict)
        self._refcounter.incr(block_id)
        self._track_block_id(block_id, computed=False)
        return block_id
    def _free_block_id(self, block: Block) -> None:
        block_id = block.block_id
        assert block_id is not None, 'Freeing unallocated block is undefined'
        if block.content_hash is not None:
            self._decr_refcount_cached_block(block)
        else:
            self._decr_refcount_hashless_block(block)
        assert block.block_id is None
    def free(self, block: Block, keep_block_object: bool=False) -> None:
        self._free_block_id(block)
        if not keep_block_object:
            self._block_pool.free_block(block)
    def fork(self, last_block: Block) -> List[Block]:
        source_blocks = get_all_blocks_recursively(last_block)
        forked_blocks: List[Block] = []
        prev_block = None
        for block in source_blocks:
            block_id = block.block_id
            assert block_id is not None
            refcount = self._refcounter.incr(block_id)
            assert refcount != 1, "can't fork free'd block_id = {}".format(block_id)
            forked_block = self._block_pool.init_block(prev_block=prev_block, token_ids=block.token_ids, block_size=self._block_size, physical_block_id=block_id, extra_hash=block.extra_hash)
            forked_blocks.append(forked_block)
            prev_block = forked_blocks[-1]
        return forked_blocks
    def get_num_free_blocks(self, device: Optional[Device]=None) -> int:
        assert device is None
        return self._hashless_allocator.get_num_free_blocks() + self.evictor.num_blocks
    def get_num_total_blocks(self) -> int:
        return self._hashless_allocator.get_num_total_blocks()
    def get_physical_block_id(self, absolute_id: int) -> int:
        return sorted(self.all_block_ids).index(absolute_id)
    @property
    def all_block_ids(self) -> FrozenSet[int]:
        return self._hashless_allocator.all_block_ids
    def get_prefix_cache_hit_rate(self) -> float:
        return self.metric_data.get_hit_rate()
    def reset_prefix_cache(self) -> bool:
        num_used_blocks = self.get_num_total_blocks() - self.get_num_free_blocks()
        if num_used_blocks > 0:
            logger.warning('Failed to reset prefix cache because some blocks ({}) are not freed yet', num_used_blocks)
            return False
        while (block_id := self._maybe_allocate_evicted_block_id()) is not None:
            self._hashless_allocator.free_block_id(block_id)
        assert not self._cached_blocks
        self.evictor = make_evictor(self.eviction_policy)
        for block_id in self._block_tracker:
            self._block_tracker[block_id] = BlockTracker()
        self.metric_data = CacheMetricData()
        logger.info('Successfully reset prefix cache')
        return True
    def is_block_cached(self, block: Block) -> bool:
        assert block.content_hash is not None
        return block.content_hash in self._cached_blocks
    def promote_to_immutable_block(self, block: Block) -> BlockId:
        assert block.content_hash is not None
        assert block.block_id is not None
        assert self._refcounter.get(block.block_id) > 0
        if block.content_hash not in self._cached_blocks:
            self._cached_blocks[block.content_hash] = block.block_id
            self._touched_blocks.add(block.block_id)
            return block.block_id
        self._decr_refcount_hashless_block(block)
        block.block_id = self._cached_blocks[block.content_hash]
        self._incr_refcount_cached_block(block)
        return block.block_id
    def cow_block_if_not_appendable(self, block: Block) -> BlockId:
        src_block_id = block.block_id
        assert src_block_id is not None
        if self._cow_tracker.is_appendable(block):
            return src_block_id
        self._free_block_id(block)
        trg_block_id = self._allocate_block_id()
        self._cow_tracker.record_cow(src_block_id, trg_block_id)
        return trg_block_id
    def clear_copy_on_writes(self) -> List[Tuple[BlockId, BlockId]]:
        return self._cow_tracker.clear_cows()
    def mark_blocks_as_accessed(self, block_ids: List[int], now: float) -> None:
        for block_id in block_ids:
            if self._block_tracker[block_id].active:
                self._block_tracker[block_id].last_accessed = now
            elif block_id in self.evictor:
                self.evictor.update(block_id, now)
            else:
                raise ValueError('Mark block as accessed which is not belonged to GPU')
    def mark_blocks_as_computed(self, block_ids: List[int]) -> None:
        for block_id in self._touched_blocks:
            self._block_tracker[block_id].computed = True
        self._touched_blocks.clear()
    def _track_block_id(self, block_id: Optional[BlockId], computed: bool) -> None:
        assert block_id is not None
        self._block_tracker[block_id].enable()
        self._block_tracker[block_id].computed = computed
    def _untrack_block_id(self, block_id: Optional[BlockId]) -> None:
        assert block_id is not None
        self._block_tracker[block_id].disable()
    def block_is_computed(self, block_id: int) -> bool:
        if self._block_tracker[block_id].active:
            return self._block_tracker[block_id].computed
        else:
            return block_id in self.evictor
    def get_common_computed_block_ids(self, computed_seq_block_ids: List[List[int]]) -> List[int]:
        if len(computed_seq_block_ids) == 1:
            return computed_seq_block_ids[0]
        return commonprefix([ids for ids in computed_seq_block_ids if ids])
    def get_num_full_blocks_touched(self, blocks: List[Block]) -> int:
        num_touched_blocks: int = 0
        for block in blocks:
            if block.is_full and (not self.is_block_cached(block) or (block.content_hash is not None and self._cached_blocks[block.content_hash] in self.evictor)):
                num_touched_blocks += 1
        return num_touched_blocks
    def swap_out(self, blocks: List[Block]) -> None:
        for block in blocks:
            self._free_block_id(block)
    def swap_in(self, blocks: List[Block]) -> None:
        for block in blocks:
            if block.is_full:
                tmp_block = self.allocate_immutable_block(prev_block=block.prev_block, token_ids=block.token_ids, extra_hash=block.extra_hash)
            else:
                tmp_block = self.allocate_mutable_block(prev_block=block.prev_block, extra_hash=block.extra_hash)
                tmp_block.append_token_ids(block.token_ids)
            block_id = tmp_block.block_id
            self._block_pool.free_block(tmp_block)
            block.block_id = block_id
    def find_cached_blocks_prefix(self, block_hashes: List[int]) -> List[int]:
        def _block_is_cached(block_hash: PrefixHash) -> bool:
            if block_hash not in self._cached_blocks:
                return False
            cached_block_id = self._cached_blocks[block_hash]
            return self.block_is_computed(cached_block_id)
        def _bisect_left(a, x, key: Callable[[PrefixHash], bool]) -> int:
            if sys.version_info < (3, 10):
                a = [key(e) for e in a]
                return bisect_left(a, x)
            else:
                return bisect_left(a, x, key=key)
        idx = _bisect_left(block_hashes, True, key=lambda x: not _block_is_cached(x))
        return block_hashes[:idx]
class PrefixCachingBlock(Block):
    _none_hash: int = hash('None')
    def __init__(self, prev_block: Optional[Block], token_ids: List[int], block_size: int, allocator: BlockAllocator, block_id: Optional[int]=None, computed: bool=False, extra_hash: Optional[int]=None):
        assert isinstance(allocator, PrefixCachingBlockAllocator), 'Currently this class is only tested with PrefixCachingBlockAllocator. Got instead allocator = {}'.format(allocator)
        assert_prefix_caching_block_or_none(prev_block)
        self._prev_block = prev_block
        self._cached_content_hash: Optional[int] = None
        self._cached_num_tokens_total: int = 0
        self._allocator = allocator
        self._last_accessed: float = _DEFAULT_LAST_ACCESSED_TIME
        self._computed = computed
        self._extra_hash = extra_hash
        if hasattr(self, '_block'):
            self._block.__init__(prev_block=prev_block, token_ids=token_ids, block_size=block_size, block_id=block_id, allocator=self._allocator)
        else:
            self._block = NaiveBlock(prev_block=prev_block, token_ids=token_ids, block_size=block_size, block_id=block_id, allocator=self._allocator)
        self._update_num_tokens_total()
    def _update_num_tokens_total(self):
        res = 0
        if self._prev_block is not None:
            res += self._prev_block.num_tokens_total
        res += len(self.token_ids)
        self._cached_num_tokens_total = res
    @property
    def computed(self) -> bool:
        return self._computed
    @computed.setter
    def computed(self, value) -> None:
        self._computed = value
    @property
    def last_accessed(self) -> float:
        return self._last_accessed
    @last_accessed.setter
    def last_accessed(self, last_accessed_ts: float):
        self._last_accessed = last_accessed_ts
    def append_token_ids(self, token_ids: List[int]) -> None:
        assert self.content_hash is None
        assert not self.computed
        if len(token_ids) == 0:
            return
        assert token_ids, 'Got token_ids = {}'.format(token_ids)
        self._block.append_token_ids(token_ids)
        self._update_num_tokens_total()
        if self.content_hash is not None:
            self.block_id = self._allocator.promote_to_immutable_block(self)
    @property
    def block_id(self) -> Optional[int]:
        return self._block.block_id
    @block_id.setter
    def block_id(self, value) -> None:
        self._block.block_id = value
    @property
    def is_full(self) -> bool:
        return self._block.is_full
    @property
    def num_empty_slots(self) -> int:
        return self._block.num_empty_slots
    @property
    def num_tokens_total(self) -> int:
        return self._cached_num_tokens_total
    @property
    def block_size(self) -> int:
        return self._block.block_size
    @property
    def token_ids(self) -> List[int]:
        return self._block.token_ids
    @property
    def prev_block(self) -> Optional[Block]:
        return self._prev_block
    @property
    def extra_hash(self) -> Optional[int]:
        return self._extra_hash
    @property
    def content_hash(self) -> Optional[int]:
        if self._cached_content_hash is not None:
            return self._cached_content_hash
        if not self.is_full:
            return None
        is_first_block = self._prev_block is None
        prev_block_hash = self._none_hash if is_first_block else self._prev_block.content_hash
        if prev_block_hash == self._none_hash and (not is_first_block):
            return None
        self._cached_content_hash = PrefixCachingBlock.hash_block_tokens(is_first_block, prev_block_hash, cur_block_token_ids=self.token_ids, extra_hash=self._extra_hash)
        return self._cached_content_hash
    @classmethod
    def hash_block_tokens(cls, is_first_block: bool, prev_block_hash: Optional[int], cur_block_token_ids: List[int], extra_hash: Optional[int]=None) -> int:
        if is_first_block and prev_block_hash is None:
            prev_block_hash = cls._none_hash
        return hash((is_first_block, prev_block_hash, *cur_block_token_ids, extra_hash))
class ComputedBlocksTracker:
    _none_hash: int = hash('None')
    def __init__(self, allocator: DeviceAwareBlockAllocator, block_size: int, enable_caching: bool):
        self._allocator = allocator
        self._block_size = block_size
        self._enable_caching = enable_caching
        self._seq_id_to_blocks_hashes: Dict[int, List[int]] = {}
        self._seq_id_to_num_tokens_computed: Dict[int, int] = {}
    def _update_seq_hashes(self, seq: Sequence) -> None:
        assert self._enable_caching
        block_hashes_recorded = self._seq_id_to_blocks_hashes.get(seq.seq_id, [])
        cur_num_blocks_recorded = len(block_hashes_recorded)
        token_ids = seq.get_token_ids()
        assert len(token_ids) >= cur_num_blocks_recorded * self._block_size, f'The sequence has {len(token_ids)} tokens, but already recorded {cur_num_blocks_recorded} blocks. This should not happen since we assume blocks are only appended other than recomputation. When the sequence is recomputed, we should have removed the info of the old blocks.'
        num_computed_blocks = len(token_ids) // self._block_size
        prev_block_hash = self._none_hash if cur_num_blocks_recorded == 0 else block_hashes_recorded[-1]
        for i in range(cur_num_blocks_recorded, num_computed_blocks):
            assert len(token_ids) >= (i + 1) * self._block_size
            block_token_ids = token_ids[i * self._block_size:(i + 1) * self._block_size]
            extra_hash = seq.extra_hash()
            block_hash = PrefixCachingBlock.hash_block_tokens(is_first_block=prev_block_hash == self._none_hash, prev_block_hash=prev_block_hash, cur_block_token_ids=block_token_ids, extra_hash=extra_hash)
            block_hashes_recorded.append(block_hash)
            prev_block_hash = block_hash
        self._seq_id_to_blocks_hashes[seq.seq_id] = block_hashes_recorded
    def get_num_cached_tokens(self, seq: Sequence) -> int:
        if not self._enable_caching:
            return 0
        self._update_seq_hashes(seq)
        num_computed_tokens_prev = self._seq_id_to_num_tokens_computed.get(seq.seq_id, None)
        if num_computed_tokens_prev is not None and seq.is_prefill():
            return num_computed_tokens_prev
        block_hashes = self._seq_id_to_blocks_hashes[seq.seq_id]
        num_cached_blocks = len(self._allocator.find_cached_blocks_prefix(block_hashes))
        num_cached_tokens = num_cached_blocks * self._block_size
        self._seq_id_to_num_tokens_computed[seq.seq_id] = num_cached_tokens
        return num_cached_tokens
    def remove_seq(self, seq_id: int) -> None:
        if not self._enable_caching:
            return
        assert seq_id in self._seq_id_to_blocks_hashes
        del self._seq_id_to_blocks_hashes[seq_id]
        assert seq_id in self._seq_id_to_num_tokens_computed
        del self._seq_id_to_num_tokens_computed[seq_id]
class LastAccessBlocksTracker:
    def __init__(self, allocator):
        self._allocator = allocator
        self._seq_last_access: Dict[int, Optional[float]] = {}
    def add_seq(self, seq_id: int) -> None:
        assert seq_id not in self._seq_last_access
        self._seq_last_access[seq_id] = None
    def remove_seq(self, seq_id: int) -> None:
        assert seq_id in self._seq_last_access
        del self._seq_last_access[seq_id]
    def update_last_access(self, seq_id: int, time: float) -> None:
        assert seq_id in self._seq_last_access
        self._seq_last_access[seq_id] = time
    def update_seq_blocks_last_access(self, seq_id: int, block_ids: List[int]) -> None:
        assert seq_id in self._seq_last_access
        ts = self._seq_last_access[seq_id]
        if ts is None:
            return
        self._allocator.mark_blocks_as_accessed(block_ids, ts)
def assert_prefix_caching_block_or_none(block: Optional[Block]):
    if block is None:
        return
    assert isinstance(block, PrefixCachingBlock), 'Got block = {}'.format(block)