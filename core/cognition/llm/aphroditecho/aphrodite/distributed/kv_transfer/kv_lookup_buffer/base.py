from abc import ABC, abstractmethod
from typing import Optional
import torch
class KVCacheBufferBase(ABC):
    @abstractmethod
    def close(self) -> None:
        raise NotImplementedError
class KVLookupBufferBase(KVCacheBufferBase):
    @abstractmethod
    def insert(self, input_tokens: torch.Tensor, roi: torch.Tensor, key: torch.Tensor, value: torch.Tensor, hidden: torch.Tensor) -> None:
        raise NotImplementedError
    @abstractmethod
    def drop_select(self, input_tokens: Optional[torch.Tensor], roi: Optional[torch.Tensor]) -> list[Optional[torch.Tensor]]:
        raise NotImplementedError
class KVStoreBufferBase(KVCacheBufferBase):
    @abstractmethod
    def put(self, key: str, value: Optional[torch.Tensor]) -> None:
        raise NotImplementedError
    @abstractmethod
    def get(self, key: str) -> Optional[torch.Tensor]:
        raise NotImplementedError