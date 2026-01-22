from abc import ABC, abstractmethod
from collections.abc import Iterable
import torch
class MambaBase(ABC):
    kv_cache: list[Iterable[torch.Tensor]]
    @abstractmethod
    def get_state_shape(self) -> Iterable[tuple[int, ...]]:
        pass
    @property
    @abstractmethod
    def mamba_type(self) -> str:
        pass