from dataclasses import dataclass
from typing import Any
import torch
from aphrodite.common.pooling_params import PoolingParams
from aphrodite.utils import is_pin_memory_available
class PoolingMetadata:
    def __init__(self, seq_groups: list[tuple[list[int], PoolingParams]], seq_data: dict[int, Any], prompt_lens: list[int]) -> None:
        self.seq_groups = seq_groups
        self.seq_data = seq_data
        self.prompt_lens = prompt_lens
    def __repr__(self) -> str:
        return f'PoolingMetadata(seq_groups={self.seq_groups}, seq_data={self.seq_data}, prompt_lens={self.prompt_lens})'
    def __getitem__(self, indices: slice):
        return PoolingMetadata(seq_groups=self.seq_groups[indices], seq_data=dict(list(self.seq_data.items())[indices]), prompt_lens=self.prompt_lens[indices])
@dataclass
class PoolingTensors:
    prompt_lens: torch.Tensor
    @classmethod
    def from_pooling_metadata(cls, pooling_metadata: 'PoolingMetadata', device: torch.device) -> 'PoolingTensors':
        pin_memory = is_pin_memory_available()
        prompt_lens_t = torch.tensor(pooling_metadata.prompt_lens, device='cpu', dtype=torch.long, pin_memory=pin_memory)
        return cls(prompt_lens=prompt_lens_t.to(device=device, non_blocking=True))