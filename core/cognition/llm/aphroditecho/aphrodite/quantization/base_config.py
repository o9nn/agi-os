import inspect
from abc import ABC, abstractmethod
from typing import TYPE_CHECKING, Any, Optional
import torch
from torch import nn
if TYPE_CHECKING:
    from aphrodite.modeling.models.utils import WeightsMapper
    from aphrodite.quantization import QuantizationMethods
else:
    QuantizationMethods = str
class QuantizeMethodBase(ABC):
    @abstractmethod
    def create_weights(self, layer: torch.nn.Module, *weight_args, **extra_weight_attrs):
        raise NotImplementedError
    @abstractmethod
    def apply(self, layer: torch.nn.Module, *args, **kwargs) -> torch.Tensor:
        raise NotImplementedError
    def embedding(self, layer: torch.nn.Module, *args, **kwargs) -> torch.Tensor:
        raise NotImplementedError
    def process_weights_after_loading(self, layer: nn.Module) -> None:
        return
def method_has_implemented_embedding(method_class: type[QuantizeMethodBase]) -> bool:
    base_embedding = inspect.getattr_static(QuantizeMethodBase, 'embedding', None)
    class_embedding = inspect.getattr_static(method_class, 'embedding', None)
    return class_embedding is not None and class_embedding is not base_embedding
class QuantizationConfig(ABC):
    def __init__(self):
        super().__init__()
        self.packed_modules_mapping: dict[str, list[str]] = dict()
    @abstractmethod
    def get_name(self) -> QuantizationMethods:
        raise NotImplementedError
    @abstractmethod
    def get_supported_act_dtypes(self) -> list[torch.dtype]:
        raise NotImplementedError
    @classmethod
    @abstractmethod
    def get_min_capability(cls) -> int:
        raise NotImplementedError
    @staticmethod
    @abstractmethod
    def get_config_filenames() -> list[str]:
        raise NotImplementedError
    @classmethod
    @abstractmethod
    def from_config(cls, config: dict[str, Any]) -> 'QuantizationConfig':
        raise NotImplementedError
    @classmethod
    def override_quantization_method(cls, hf_quant_cfg, user_quant) -> Optional[QuantizationMethods]:
        return None
    @staticmethod
    def get_from_keys(config: dict[str, Any], keys: list[str]) -> Any:
        for key in keys:
            if key in config:
                return config[key]
        raise ValueError(f"Cannot find any of {keys} in the model's quantization config.")
    @staticmethod
    def get_from_keys_or(config: dict[str, Any], keys: list[str], default: Any) -> Any:
        try:
            return QuantizationConfig.get_from_keys(config, keys)
        except ValueError:
            return default
    @abstractmethod
    def get_quant_method(self, layer: torch.nn.Module, prefix: str) -> Optional[QuantizeMethodBase]:
        raise NotImplementedError
    def get_cache_scale(self, name: str) -> Optional[str]:
        return None
    def apply_aphrodite_mapper(self, hf_to_aphrodite_mapper: 'WeightsMapper'):
        pass