"""Production block manager with real memory allocation."""
from typing import Dict, List, Optional
from typing import Sequence as GenericSequence
from typing import Tuple

from aphrodite.common.sequence import Sequence, SequenceGroup, SequenceStatus
from aphrodite.utils import Device
from aphrodite.processing.block.block_table import BlockTable
from aphrodite.processing.block.cpu_gpu_block_allocator import (
    CpuGpuBlockAllocator)
from aphrodite.processing.block.prefix_caching_block import (
    ComputedBlocksTracker, LastAccessBlocksTracker)
from aphrodite.processing.interfaces import AllocStatus, BlockSpaceManager

SeqId = int


class ProductionBlockSpaceManager(BlockSpaceManager):
    """Production BlockSpaceManager with real GPU/CPU memory allocation.
    
    Replaces PlaceholderBlockSpaceManager with actual memory management,
    block tracking, and cache management capabilities.
    """

    def __init__(
        self,
        block_size: int = 16,
        num_gpu_blocks: int = 1000,
        num_cpu_blocks: int = 1000,
        watermark: float = 0.01,
        enable_caching: bool = False,
        **kwargs,
    ) -> None:
        self.block_size = block_size
        self.num_total_gpu_blocks = num_gpu_blocks
        self.num_total_cpu_blocks = num_cpu_blocks
        self.watermark = watermark
        self.enable_caching = enable_caching
        
        self.watermark_blocks = int(watermark * num_gpu_blocks)
        
        self.block_allocator = CpuGpuBlockAllocator.create(
            allocator_type="prefix_caching" if enable_caching else "naive",
            num_gpu_blocks=num_gpu_blocks,
            num_cpu_blocks=num_cpu_blocks,
            block_size=block_size,
        )
        
        self.block_tables: Dict[SeqId, BlockTable] = {}
        self._computed_blocks_tracker = ComputedBlocksTracker(
            self.block_allocator, self.block_size, self.enable_caching)
        self._last_access_blocks_tracker = LastAccessBlocksTracker(
            self.block_allocator)

    def can_allocate(self,
                     seq_group: SequenceGroup,
                     num_lookahead_slots: int = 0) -> AllocStatus:
        num_required_blocks = 0
        for seq in seq_group.get_seqs(status=SequenceStatus.WAITING):
            num_required_blocks += seq.n_blocks
        
        num_required_blocks += ((num_lookahead_slots + self.block_size - 1) // 
                                self.block_size)
        
        num_free_gpu_blocks = self.block_allocator.get_num_free_blocks(
            Device.GPU)
        
        if num_free_gpu_blocks - num_required_blocks >= self.watermark_blocks:
            return AllocStatus.OK
        elif num_required_blocks <= self.num_total_gpu_blocks:
            return AllocStatus.LATER
        else:
            return AllocStatus.NEVER

    def allocate(self, seq_group: SequenceGroup) -> None:
        waiting_seqs = seq_group.get_seqs(status=SequenceStatus.WAITING)
        assert not (set(seq.seq_id for seq in waiting_seqs) & 
                    self.block_tables.keys()), "block table already exists"
        
        seq = waiting_seqs[0]
        block_table = self._allocate_sequence(seq)
        self.block_tables[seq.seq_id] = block_table
        
        if self.enable_caching:
            self._last_access_blocks_tracker.add_seq(seq.seq_id)
        
        for seq in waiting_seqs[1:]:
            self.block_tables[seq.seq_id] = block_table.fork()
            if self.enable_caching:
                self._last_access_blocks_tracker.add_seq(seq.seq_id)

    def _allocate_sequence(self, seq: Sequence) -> BlockTable:
        block_table = BlockTable(
            block_size=self.block_size,
            block_allocator=self.block_allocator
        )
        
        if seq.get_token_ids():
            block_table.allocate(token_ids=seq.get_token_ids(),
                               extra_hash=seq.extra_hash())
        
        return block_table

    def can_append_slots(self, seq_group: SequenceGroup,
                         num_lookahead_slots: int) -> bool:
        num_touched_blocks = 0
        for seq in seq_group.get_seqs(status=SequenceStatus.RUNNING):
            if seq.seq_id in self.block_tables:
                block_table = self.block_tables[seq.seq_id]
                unseen_token_ids = block_table.get_unseen_token_ids(
                    seq.get_token_ids())
                num_touched_blocks += (
                    block_table.get_num_blocks_touched_by_append_slots(
                        token_ids=unseen_token_ids,
                        num_lookahead_slots=num_lookahead_slots
                    ))
        
        num_free_gpu_blocks = self.block_allocator.get_num_free_blocks(
            Device.GPU)
        return num_touched_blocks <= num_free_gpu_blocks

    def append_slots(
        self,
        seq: Sequence,
        num_lookahead_slots: int,
    ) -> List[Tuple[int, int]]:
        if seq.seq_id not in self.block_tables:
            return []
        
        block_table = self.block_tables[seq.seq_id]
        unseen_token_ids = block_table.get_unseen_token_ids(seq.get_token_ids())
        
        block_table.append_token_ids(
            token_ids=unseen_token_ids,
            num_lookahead_slots=num_lookahead_slots,
            num_computed_slots=seq.data.get_num_computed_tokens(),
            extra_hash=seq.extra_hash()
        )
        
        return self.block_allocator.clear_copy_on_writes()

    def fork(self, parent_seq: Sequence, child_seq: Sequence) -> None:
        if parent_seq.seq_id not in self.block_tables:
            return
        
        parent_block_table = self.block_tables[parent_seq.seq_id]
        child_block_table = parent_block_table.fork()
        self.block_tables[child_seq.seq_id] = child_block_table
        
        if self.enable_caching:
            self._last_access_blocks_tracker.add_seq(child_seq.seq_id)

    def can_swap_in(self, seq_group: SequenceGroup,
                    num_lookahead_slots: int) -> AllocStatus:
        num_blocks_touched = 0
        blocks = []
        for seq in seq_group.get_seqs(status=SequenceStatus.SWAPPED):
            if seq.seq_id in self.block_tables:
                block_table = self.block_tables[seq.seq_id]
                unseen_token_ids = block_table.get_unseen_token_ids(
                    seq.get_token_ids())
                num_blocks_touched += (
                    block_table.get_num_blocks_touched_by_append_slots(
                        token_ids=unseen_token_ids,
                        num_lookahead_slots=num_lookahead_slots
                    ))
                blocks.extend(block_table.blocks)
        
        num_blocks_touched += self.block_allocator.get_num_full_blocks_touched(
            blocks, device=Device.GPU)
        
        if (self.block_allocator.get_num_total_blocks(Device.GPU) < 
                num_blocks_touched):
            return AllocStatus.NEVER
        elif (self.block_allocator.get_num_free_blocks(Device.GPU) - 
              num_blocks_touched >= self.watermark_blocks):
            return AllocStatus.OK
        else:
            return AllocStatus.LATER

    def swap_in(self, seq_group: SequenceGroup) -> List[Tuple[int, int]]:
        physical_block_id_mapping = []
        for seq in seq_group.get_seqs(status=SequenceStatus.SWAPPED):
            if seq.seq_id in self.block_tables:
                block_table = self.block_tables[seq.seq_id]
                blocks = block_table.blocks
                if len(blocks) == 0:
                    continue
                
                seq_swap_mapping = self.block_allocator.swap(
                    blocks=blocks,
                    src_device=Device.CPU,
                    dst_device=Device.GPU
                )
                
                block_table.update(blocks)
                
                seq_physical_block_id_mapping = {
                    self.block_allocator.get_physical_block_id(
                        Device.CPU, cpu_block_id):
                    self.block_allocator.get_physical_block_id(
                        Device.GPU, gpu_block_id)
                    for cpu_block_id, gpu_block_id in seq_swap_mapping.items()
                }
                
                physical_block_id_mapping.extend(
                    list(seq_physical_block_id_mapping.items()))
        
        return physical_block_id_mapping

    def can_swap_out(self, seq_group: SequenceGroup) -> bool:
        num_blocks_touched = 0
        blocks = []
        for seq in seq_group.get_seqs(status=SequenceStatus.RUNNING):
            if seq.seq_id in self.block_tables:
                block_table = self.block_tables[seq.seq_id]
                unseen_token_ids = block_table.get_unseen_token_ids(
                    seq.get_token_ids())
                num_blocks_touched += (
                    block_table.get_num_blocks_touched_by_append_slots(
                        token_ids=unseen_token_ids,
                        num_lookahead_slots=0
                    ))
                blocks.extend(block_table.blocks)
        
        num_blocks_touched += self.block_allocator.get_num_full_blocks_touched(
            blocks, device=Device.CPU)
        
        return (self.block_allocator.get_num_free_blocks(Device.CPU) >= 
                num_blocks_touched)

    def swap_out(self, seq_group: SequenceGroup) -> List[Tuple[int, int]]:
        physical_block_id_mapping = []
        for seq in seq_group.get_seqs(status=SequenceStatus.RUNNING):
            if seq.seq_id in self.block_tables:
                block_table = self.block_tables[seq.seq_id]
                blocks = block_table.blocks
                if len(blocks) == 0:
                    continue
                
                seq_swap_mapping = self.block_allocator.swap(
                    blocks=blocks,
                    src_device=Device.GPU,
                    dst_device=Device.CPU
                )
                
                block_table.update(blocks)
                
                seq_physical_block_id_mapping = {
                    self.block_allocator.get_physical_block_id(
                        Device.GPU, gpu_block_id):
                    self.block_allocator.get_physical_block_id(
                        Device.CPU, cpu_block_id)
                    for gpu_block_id, cpu_block_id in seq_swap_mapping.items()
                }
                
                physical_block_id_mapping.extend(
                    list(seq_physical_block_id_mapping.items()))
        
        return physical_block_id_mapping

    def free(self, seq: Sequence) -> None:
        seq_id = seq.seq_id
        
        if seq_id not in self.block_tables:
            return
        
        if self.enable_caching:
            self._last_access_blocks_tracker.update_seq_blocks_last_access(
                seq_id, self.block_tables[seq_id].physical_block_ids)
            self._last_access_blocks_tracker.remove_seq(seq_id)
            self._computed_blocks_tracker.remove_seq(seq_id)
        
        self.block_tables[seq_id].free()
        del self.block_tables[seq_id]

    def get_block_table(self, seq: Sequence) -> List[int]:
        if seq.seq_id in self.block_tables:
            block_table = self.block_tables[seq.seq_id]
            return block_table.physical_block_ids
        return []

    def get_num_free_gpu_blocks(self) -> int:
        return self.block_allocator.get_num_free_blocks(Device.GPU)

    def get_num_free_cpu_blocks(self) -> int:
        return self.block_allocator.get_num_free_blocks(Device.CPU)

    def access_all_blocks_in_seq(
        self,
        seq: Sequence,
        access_time: float,
    ) -> None:
        if self.enable_caching and seq.seq_id in self.block_tables:
            self._last_access_blocks_tracker.update_last_access(
                seq.seq_id, access_time)

    def get_common_computed_block_ids(
            self, seqs: List[Sequence]) -> GenericSequence[int]:
        computed_seq_block_ids = []
        for seq in seqs:
            if seq.seq_id in self.block_tables:
                all_blocks = self.block_tables[seq.seq_id].physical_block_ids
                num_cached_tokens = (
                    self._computed_blocks_tracker.get_num_cached_tokens(seq))
                assert num_cached_tokens % self.block_size == 0
                num_cached_blocks = num_cached_tokens // self.block_size
                computed_block_ids = all_blocks[:num_cached_blocks]
                computed_seq_block_ids.append(computed_block_ids)
        
        return self.block_allocator.get_common_computed_block_ids(
            computed_seq_block_ids)

    def mark_blocks_as_computed(self, seq_group: SequenceGroup,
                                token_chunk_size: int):
        self.block_allocator.mark_blocks_as_computed([])

    def get_prefix_cache_hit_rate(self, device: Device) -> float:
        if self.enable_caching:
            return self.block_allocator.get_prefix_cache_hit_rate(device)
        return -1

    def reset_prefix_cache(self, device: Optional[Device] = None) -> bool:
        if self.enable_caching:
            return self.block_allocator.reset_prefix_cache(device)
        return True

    def get_num_cached_tokens(self, seq: Sequence) -> int:
        return self._computed_blocks_tracker.get_num_cached_tokens(seq)

    def remove_seq_from_computed_blocks_tracker(self, seq: Sequence) -> None:
        self._computed_blocks_tracker.remove_seq(seq.seq_id)


PlaceholderBlockSpaceManager = ProductionBlockSpaceManager
