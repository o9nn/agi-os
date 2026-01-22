from abc import ABC, abstractmethod
from typing import Any, Optional
import torch
class Torch25CustomGraphPass(ABC):
    @abstractmethod
    def __call__(self, graph: torch.fx.graph.Graph) -> None:
    @abstractmethod
    def uuid(self) -> Optional[Any]:
    def __getstate__(self):
        return self.uuid()
    def __setstate__(self, state):
        raise ValueError('Cannot unpickle CustomGraphPass because pickling is used for cache key uuid. Use torch>=2.6 with native uuid support for custom passes.')