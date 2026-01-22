from typing import Dict, Union
import torch
from torch import nn
class GlobalAvailMixin:
    _instances: Dict[str, Dict[Union[str, nn.Module], 'GlobalAvailMixin']] = {'default': {}}
    def global_available(self, key: Union[str, nn.Module]='default', group: str='default') -> None:
        self._save_instance(self, key, group)
    @classmethod
    def _save_instance(cls, instance: 'GlobalAvailMixin', key: Union[str, nn.Module]='default', group: str='default') -> None:
        if group not in cls._instances:
            assert isinstance(group, str)
            cls._instances[group] = {}
        cls._instances[group][key] = instance
    @classmethod
    def find(cls, key: Union[str, nn.Module]='default', group: str='default') -> Union[None, 'GlobalAvailMixin']:
        return cls._instances.get(group, {}).get(key)
    @classmethod
    def find_group(cls, group: str) -> Dict[Union[str, nn.Module], 'GlobalAvailMixin']:
        return cls._instances.get(group, {})
    @classmethod
    def instances(cls) -> Dict[str, Dict[Union[str, nn.Module], 'GlobalAvailMixin']]:
        return cls._instances
class KVCacheObserver(GlobalAvailMixin):
    def __init__(self, num_head: int, head_dim: int) -> None:
        self.num_head = num_head
        self.head_dim = head_dim
        self.max_val = torch.full((num_head, head_dim), -torch.inf, dtype=torch.float16)
        self.min_val = torch.full((num_head, head_dim), torch.inf, dtype=torch.float16)
        self.absmax_val = torch.full((num_head, head_dim), 0, dtype=torch.float16)
    @torch.no_grad()
    def observe(self, x: torch.Tensor) -> None:
        assert len(x.shape) == 4
        if x.size(1) == self.num_head and x.size(3) == self.head_dim:
            x = x.transpose(1, 2)
        elif x.size(2) != self.num_head or x.size(3) != self.head_dim:
            raise RuntimeError('Unexpected dimensions for x, expected (bs, num_head, seqlen, head_dim) or (bs, seqlen, num_head, head_dim)')
        cur_max = x.flatten(0, 1).max(0)[0].cpu()
        cur_min = x.flatten(0, 1).min(0)[0].cpu()
        cur_absmax = x.flatten(0, 1).abs().max(0)[0].cpu()
        self.max_val = torch.maximum(self.max_val, cur_max)
        self.min_val = torch.minimum(self.min_val, cur_min)
        self.absmax_val = torch.maximum(self.absmax_val, cur_absmax)
class ActivationObserver(GlobalAvailMixin):
    def __init__(self, dim: int) -> None:
        self.dim = dim
        self.max_val = torch.full((dim,), -torch.inf, dtype=torch.float16)
        self.min_val = torch.full((dim,), torch.inf, dtype=torch.float16)
        self.absmax_val = torch.full((dim,), 0, dtype=torch.float16)
        self.absmean_val = torch.full((dim,), 0, dtype=torch.float16)
        self.mean_val = torch.full((dim,), 0, dtype=torch.float16)
        self.num_batches_tracked = 0
    @torch.no_grad()
    def observe(self, x: torch.Tensor) -> None:
        assert len(x.shape) == 3
        assert x.size(2) == self.dim
        cur_val = x.flatten(0, 1)
        cur_max = cur_val.max(0)[0].cpu()
        cur_min = cur_val.min(0)[0].cpu()
        cur_mean = cur_val.mean(0).cpu()
        cur_abs = cur_val.abs()
        cur_absmax = cur_abs.max(0)[0].cpu()
        cur_absmean = cur_abs.mean(0).cpu()
        self.max_val = torch.maximum(self.max_val, cur_max)
        self.min_val = torch.minimum(self.min_val, cur_min)
        self.absmax_val = torch.maximum(self.absmax_val, cur_absmax)
        self.mean_val = (self.mean_val * self.num_batches_tracked + cur_mean) / (self.num_batches_tracked + 1)
        self.absmean_val = (self.absmean_val * self.num_batches_tracked + cur_absmean) / (self.num_batches_tracked + 1)
        self.num_batches_tracked += 1