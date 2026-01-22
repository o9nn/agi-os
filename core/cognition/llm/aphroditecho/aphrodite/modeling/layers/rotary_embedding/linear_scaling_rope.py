from typing import Union
import torch
from .base import RotaryEmbedding
class LinearScalingRotaryEmbedding(RotaryEmbedding):
    def __init__(self, head_size: int, rotary_dim: int, max_position_embeddings: int, base: float, is_neox_style: bool, scaling_factors: Union[list[float], float], dtype: torch.dtype) -> None:
        if isinstance(scaling_factors, float):
            scaling_factors = [scaling_factors]
        self.scaling_factors: list[float] = scaling_factors
        super().__init__(head_size, rotary_dim, max_position_embeddings, base, is_neox_style, dtype)
        self._scaling_factor_to_offset: dict[float, int]
    def _compute_cos_sin_cache(self) -> torch.Tensor:
        inv_freq = self._compute_inv_freq(self.base)
        cache_list: list[torch.Tensor] = []
        offsets: list[int] = []
        for scaling_factor in self.scaling_factors:
            max_len = self.max_position_embeddings * scaling_factor
            t = torch.arange(max_len, dtype=torch.float)
            t = t / scaling_factor
            freqs = torch.einsum('i,j -> ij', t, inv_freq)
            cos = freqs.cos()
            sin = freqs.sin()
            cache = torch.cat((cos, sin), dim=-1)
            if not cache_list:
                offset = 0
            else:
                last_offset = offsets[-1]
                next_max_len = cache_list[-1].shape[0]
                offset = last_offset + next_max_len
            offsets.append(offset)
            cache_list.append(cache)
        self._scaling_factor_to_offset = {float(scaling_factor): offsets[i] for i, scaling_factor in enumerate(self.scaling_factors)}
        assert len(self.scaling_factors) == len(offsets)
        return torch.cat(cache_list, dim=0)
    @property
    def scaling_factor_to_offset(self) -> dict[float, int]:
        return self._scaling_factor_to_offset