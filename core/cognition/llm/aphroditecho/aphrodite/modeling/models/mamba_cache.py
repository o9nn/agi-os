from dataclasses import dataclass
import torch
from aphrodite.attention.backends.utils import PAD_SLOT_ID
from aphrodite.common.config import AphroditeConfig
from aphrodite.modeling.models.constant_size_cache import ConstantSizeCache
@dataclass
class MambaCacheParams:
    conv_state: torch.Tensor = torch.Tensor()
    ssm_state: torch.Tensor = torch.Tensor()
    state_indices_tensor: torch.Tensor = torch.Tensor()
    def at_layer_idx(self, layer_idx):
        return MambaCacheParams(self.conv_state[layer_idx], self.ssm_state[layer_idx], self.state_indices_tensor)
class MambaCacheManager(ConstantSizeCache):
    def __init__(self, aphrodite_config: AphroditeConfig, dtype: torch.dtype, num_mamba_layers: int, conv_state_shape: tuple[int, int], temporal_state_shape: tuple[int, int]):
        max_batch_size = aphrodite_config.scheduler_config.max_num_seqs
        if not aphrodite_config.model_config.enforce_eager:
            max_batch_size = aphrodite_config.pad_for_cudagraph(max_batch_size)
        super().__init__(max_batch_size)
        assert conv_state_shape[0] > conv_state_shape[1]
        conv_state = torch.empty(size=(num_mamba_layers, max_batch_size) + (conv_state_shape[1], conv_state_shape[0]), dtype=dtype, device='cuda').transpose(-1, -2)
        temporal_state = torch.empty(size=(num_mamba_layers, max_batch_size) + temporal_state_shape, dtype=dtype, device='cuda')
        self._mamba_cache = (conv_state, temporal_state)
    @property
    def cache(self):
        return self._mamba_cache
    def _copy_cache(self, from_index: int, to_index: int):
        for cache_t in self.cache:
            cache_t[:, to_index].copy_(cache_t[:, from_index], non_blocking=True)
    def current_run_tensors(self, **kwargs) -> MambaCacheParams:
        cache_tensors, state_indices_tensor = super().current_run_tensors(**kwargs)
        return MambaCacheParams(cache_tensors[0], cache_tensors[1], state_indices_tensor)
    def get_seqlen_agnostic_capture_inputs(self, batch_size: int):
        return (self._mamba_cache, torch.as_tensor([PAD_SLOT_ID] * batch_size, dtype=torch.int32, device='cuda'))