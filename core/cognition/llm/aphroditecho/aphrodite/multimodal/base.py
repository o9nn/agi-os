from abc import ABC, abstractmethod
from collections.abc import Sequence
from pathlib import Path
from typing import TYPE_CHECKING, Generic, NamedTuple, TypeVar
if TYPE_CHECKING:
    from aphrodite.common.sequence import SequenceGroupMetadata
from .inputs import MultiModalKwargs, PlaceholderRange
_T = TypeVar('_T')
class MultiModalPlaceholderMap:
    class IndexMap(NamedTuple):
        src: list[int]
        dest: list[int]
    src_ranges: list[range]
    '\n    The indices of the multi-modal embeddings that will replace the\n    corresponding placeholder embeddings pointed to by ``dest_ranges``.\n    '
    src_len: int
    '\n    The total number of flattened multi-modal embeddings.\n    '
    dest_ranges: list[range]
    '\n    The indices of the placeholder embeddings that will be replaced by the\n    multimodal embeddings.\n    '
    dest_len: int
    '\n    The total number of embeddings in the destination tensor.\n    '
    def __init__(self):
        self.src_ranges = []
        self.src_len = 0
        self.dest_ranges = []
        self.dest_len = 0
    @classmethod
    def from_seq_group(cls, seq_group: 'SequenceGroupMetadata', positions: range) -> tuple[MultiModalKwargs, dict[str, 'MultiModalPlaceholderMap']]:
        seq_mm_data = seq_group.multi_modal_data
        seq_mm_placeholders = seq_group.multi_modal_placeholders
        if not seq_mm_data or not seq_mm_placeholders:
            return (MultiModalKwargs({}), {})
        placeholder_maps = dict[str, MultiModalPlaceholderMap]()
        for modality, placeholders in seq_mm_placeholders.items():
            placeholder_map = MultiModalPlaceholderMap()
            if positions:
                placeholder_map.append_items_from_seq_group(positions, [None] * len(placeholders), placeholders)
            placeholder_maps[modality] = placeholder_map
        return (seq_mm_data, placeholder_maps)
    def append_items_from_seq_group(self, positions: range, multi_modal_items: list[_T], multi_modal_placeholders: Sequence[PlaceholderRange]) -> list[_T]:
        intersecting_items = []
        if len(multi_modal_items) != len(multi_modal_placeholders):
            raise ValueError('Multi-modal placeholders and items must have the same length.')
        for placeholder_dict, mm_item in zip(multi_modal_placeholders, multi_modal_items):
            placeholder = range(placeholder_dict.offset, placeholder_dict.offset + placeholder_dict.length)
            intersection = range(max(positions.start, placeholder.start), min(positions.stop, placeholder.stop))
            if not intersection:
                continue
            token_embedding_range = range(intersection.start - positions.start, intersection.stop - positions.start)
            multimodal_embedding_range = range(intersection.start - placeholder.start + self.src_len, intersection.stop - placeholder.start + self.src_len)
            intersecting_items.append(mm_item)
            self.dest_ranges.append(token_embedding_range)
            self.src_ranges.append(multimodal_embedding_range)
            self.src_len += len(placeholder)
        self.dest_len += len(positions)
        return intersecting_items
    def extend(self, other: 'MultiModalPlaceholderMap'):
        self.src_ranges.extend((range(self.src_len + r.start, self.src_len + r.stop) for r in other.src_ranges))
        self.src_len += other.src_len
        self.dest_ranges.extend((range(self.dest_len + r.start, self.dest_len + r.stop) for r in other.dest_ranges))
        self.dest_len += other.dest_len
    def index_map(self) -> 'IndexMap':
        src_indices = [i for r in self.src_ranges for i in r]
        dest_indices = [i for r in self.dest_ranges for i in r]
        if len(src_indices) != len(dest_indices):
            raise ValueError(f'The number of source ({len(src_indices)}) and destination indices ({len(dest_indices)}) must be the same.')
        return self.IndexMap(src=src_indices, dest=dest_indices)
class MediaIO(ABC, Generic[_T]):
    @abstractmethod
    def load_bytes(self, data: bytes) -> _T:
        raise NotImplementedError
    @abstractmethod
    def load_base64(self, media_type: str, data: str) -> _T:
        raise NotImplementedError
    @abstractmethod
    def load_file(self, filepath: Path) -> _T:
        raise NotImplementedError