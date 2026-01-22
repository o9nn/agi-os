from typing import Any, Optional
import torch
from aphrodite.modeling.layers.fused_moe.layer import FusedMoE, UnquantizedFusedMoEMethod
from aphrodite.modeling.layers.linear import LinearBase, UnquantizedLinearMethod
from aphrodite.quantization import QuantizationMethods
from aphrodite.quantization.base_config import QuantizationConfig, QuantizeMethodBase
class INCConfig(QuantizationConfig):
    @classmethod
    def get_name(cls) -> QuantizationMethods:
        return 'inc'
    @classmethod
    def get_supported_act_dtypes(cls) -> list[torch.dtype]:
        return [torch.bfloat16]
    @classmethod
    def from_config(cls, config: dict[str, Any]) -> 'INCConfig':
        raise AssertionError
    def get_quant_method(self, layer: torch.nn.Module, prefix: str) -> Optional['QuantizeMethodBase']:
        if isinstance(layer, LinearBase):
            return UnquantizedLinearMethod()
        elif isinstance(layer, FusedMoE):
            return UnquantizedFusedMoEMethod(layer.moe_config)
        return None
    @classmethod
    def get_min_capability(cls) -> int:
        raise AssertionError
    @staticmethod
    def get_config_filenames() -> list[str]:
        return []