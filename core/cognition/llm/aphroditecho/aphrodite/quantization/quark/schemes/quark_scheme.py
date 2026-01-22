from abc import ABC, abstractmethod
from typing import Optional
import torch
__all__ = ['QuarkScheme']
class QuarkScheme(ABC):
    @classmethod
    @abstractmethod
    def get_min_capability(cls) -> int:
        raise NotImplementedError
    @abstractmethod
    def create_weights(self, *args, **kwargs):
        raise NotImplementedError
    @abstractmethod
    def apply_weights(self, layer: torch.nn.Module, x: torch.Tensor, bias: Optional[torch.Tensor]):
        raise NotImplementedError
    @abstractmethod
    def process_weights_after_loading(self, layer: torch.nn.Module):
        raise NotImplementedError