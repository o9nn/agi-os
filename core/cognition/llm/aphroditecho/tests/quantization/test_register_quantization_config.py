from typing import Any, Optional
import pytest
import torch
import torch.nn.functional as F
from aphrodite.modeling.layers.linear import LinearBase
from aphrodite.modeling.layers.linear import UnquantizedLinearMethod
from aphrodite.quantization import QuantizationMethods, get_quantization_config, register_quantization_config
from aphrodite.quantization.base_config import QuantizationConfig
class FakeQuantLinearMethod(UnquantizedLinearMethod):
    def __init__(self, num_bits: int=8) -> None:
        super().__init__()
        self.num_bits = num_bits
    def apply(self, layer: 'torch.nn.Module', x: 'torch.Tensor', bias: Optional['torch.Tensor']=None) -> 'torch.Tensor':
        max_val = torch.amax(x, dim=(0, -1), keepdims=True)
        min_val = torch.amin(x, dim=(0, -1), keepdims=True)
        scales = (max_val - min_val) / (2 ** self.num_bits - 1)
        quant_x = torch.clamp(torch.round(x / scales), -2 ** (self.num_bits - 1), 2 ** (self.num_bits - 1) - 1)
        dequant_x = quant_x * scales
        return F.linear(dequant_x, layer.weight, bias)
@register_quantization_config('custom_quant')
class CustomQuantConfig(QuantizationConfig):
    def __init__(self, num_bits: int=8) -> None:
        self.num_bits = num_bits
    def get_name(self) -> QuantizationMethods:
        return 'custom_quant'
    def get_supported_act_dtypes(self) -> list['torch.dtype']:
        return [torch.float16, torch.bfloat16]
    @classmethod
    def get_min_capability(cls) -> int:
        return -1
    @staticmethod
    def get_config_filenames() -> list[str]:
        return []
    @classmethod
    def from_config(cls, config: dict[str, Any]) -> 'CustomQuantConfig':
        return CustomQuantConfig(num_bits=config.get('num_bits', 8))
    def get_quant_method(self, layer: 'torch.nn.Module', prefix: str) -> Optional['FakeQuantLinearMethod']:
        if isinstance(layer, LinearBase):
            return FakeQuantLinearMethod(num_bits=self.num_bits)
        return None
def test_register_quantization_config():
    assert get_quantization_config('custom_quant') == CustomQuantConfig
    with pytest.raises(ValueError):
        register_quantization_config('custom_quant')(CustomQuantConfig)
@pytest.mark.parametrize(argnames='model', argvalues=['meta-llama/Llama-3.2-1B-Instruct'])
def test_custom_quant(aphrodite_runner, model, monkeypatch):
    monkeypatch.setenv('APHRODITE_USE_V1', '0')
    with aphrodite_runner(model_name=model, quantization='custom_quant', enforce_eager=True) as llm:
        model = llm.model.llm_engine.model_executor.driver_worker.model_runner.model
        layer = model.model.layers[0]
        qkv_proj = layer.self_attn.qkv_proj
        assert isinstance(qkv_proj.quant_method, FakeQuantLinearMethod)
        output = llm.generate_greedy('Hello my name is', max_tokens=20)
        assert output