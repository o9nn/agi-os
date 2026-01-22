import enum
from abc import ABC, abstractmethod
from typing import List, Optional
from typing import Sequence as GenericSequence
from typing import Tuple
from aphrodite.common.sequence import Sequence, SequenceGroup
from aphrodite.utils import Device
class AllocStatus(enum.Enum):
    OK = enum.auto()
    LATER = enum.auto()
    NEVER = enum.auto()
class BlockSpaceManager(ABC):
    @staticmethod
    def get_block_space_manager_class(version: str):
        version = version.lower()
        if version == 'selfattn':
            from aphrodite.processing.block_manager import SelfAttnBlockSpaceManager
            return SelfAttnBlockSpaceManager
        if version == 'placeholder':
            from aphrodite.processing.placeholder_block_space_manager import ProductionBlockSpaceManager
            return ProductionBlockSpaceManager
        raise ValueError(f'Unknown version version={version!r}')
    @abstractmethod
    def can_allocate(self, seq_group: SequenceGroup, num_lookahead_slots: int=0) -> AllocStatus:
        pass
    @abstractmethod
    def allocate(self, seq_group: SequenceGroup) -> None:
        pass
    @abstractmethod
    def can_append_slots(self, seq_group: SequenceGroup, num_lookahead_slots: int) -> bool:
        pass
    @abstractmethod
    def append_slots(self, seq: Sequence, num_lookahead_slots: int) -> List[Tuple[int, int]]:
        pass
    @abstractmethod
    def fork(self, parent_seq: Sequence, child_seq: Sequence) -> None:
        pass
    @abstractmethod
    def can_swap_in(self, seq_group: SequenceGroup, num_lookahead_slots: int) -> AllocStatus:
        pass
    @abstractmethod
    def swap_in(self, seq_group: SequenceGroup) -> List[Tuple[int, int]]:
        pass
    @abstractmethod
    def can_swap_out(self, seq_group: SequenceGroup) -> bool:
        pass
    @abstractmethod
    def swap_out(self, seq_group: SequenceGroup) -> List[Tuple[int, int]]:
        pass
    @abstractmethod
    def free(self, seq: Sequence) -> None:
        pass
    @abstractmethod
    def get_block_table(self, seq: Sequence) -> List[int]:
        pass
    @abstractmethod
    def get_num_free_gpu_blocks(self) -> int:
        pass
    @abstractmethod
    def get_num_free_cpu_blocks(self) -> int:
        pass
    @abstractmethod
    def access_all_blocks_in_seq(self, seq: Sequence, access_time: float) -> None:
        pass
    @abstractmethod
    def get_common_computed_block_ids(self, seqs: List[Sequence]) -> GenericSequence[int]:
        pass
    @abstractmethod
    def mark_blocks_as_computed(self, seq_group: SequenceGroup, token_chunk_size: int):
        pass
    @abstractmethod
    def get_prefix_cache_hit_rate(self, device: Device) -> float:
        pass
    @abstractmethod
    def reset_prefix_cache(self, device: Optional[Device]=None) -> bool:
        pass
    @abstractmethod
    def get_num_cached_tokens(self, seq: Sequence) -> int:
        pass
    @abstractmethod
    def remove_seq_from_computed_blocks_tracker(self, seq: Sequence) -> None:
        pass