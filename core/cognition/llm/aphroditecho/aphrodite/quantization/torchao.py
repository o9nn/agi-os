from typing import Any, Optional
import torch
import torch.nn.functional as F
from torch.nn.parameter import Parameter
from aphrodite.modeling.layers.linear import LinearBase, LinearMethodBase, UnquantizedLinearMethod
from aphrodite.modeling.utils import set_weight_attrs
from aphrodite.quantization import QuantizationMethods
from aphrodite.quantization.base_config import QuantizationConfig, QuantizeMethodBase
def should_skip(prefix: str, skip_modules: list[str]) -> bool:
    for s in skip_modules:
        if prefix == s:
            return True
        if f'.{s}.' in f'.{prefix}.':
            return True
    return False
class TorchAOConfig(QuantizationConfig):
    def __init__(self, torchao_config, skip_modules: Optional[list[str]]=None) -> None:
        super().__init__()
        self.torchao_config = torchao_config
        self.skip_modules = skip_modules or []
    def __repr__(self) -> str:
        return f'TorchAOConfig({self.torchao_config})'
    def get_name(self) -> QuantizationMethods:
        return 'torchao'
    def get_supported_act_dtypes(self) -> list[torch.dtype]:
        return [torch.float32, torch.float16, torch.bfloat16]
    @classmethod
    def get_min_capability(cls) -> int:
        return 75
    @staticmethod
    def get_config_filenames() -> list[str]:
        return ['config.json']
    @classmethod
    def from_config(cls, config: dict[str, Any]) -> 'TorchAOConfig':
        try:
            from torchao.core.config import config_from_dict
        except ImportError as err:
            raise ImportError('Please install torchao>=0.10.0 via `pip install torchao>=0.10.0` to use torchao quantization.') from err
        hf_config = cls.get_from_keys_or(config, ['quant_type'], None)
        assert hf_config is not None, 'quant_type must be specified'
        assert len(hf_config) == 1 and 'default' in hf_config, "Expected only one key 'default' in quant_type dictionary"
        quant_type = hf_config['default']
        ao_config = config_from_dict(quant_type)
        skip_modules = config.get('modules_to_not_convert', []) or []
        _data = quant_type.get('_data', {})
        if not isinstance(_data, dict):
            _data = {}
        module_fqn = _data.get('module_fqn_to_config', {})
        if not isinstance(module_fqn, dict):
            module_fqn = {}
        for layer, layer_cfg in module_fqn.items():
            if layer_cfg is None:
                skip_modules.append(layer)
        return cls(ao_config, skip_modules)
    def get_quant_method(self, layer: torch.nn.Module, prefix: str) -> Optional['QuantizeMethodBase']:
        if not isinstance(layer, LinearBase):
            return None
        from torchao.quantization import ModuleFqnToConfig
        if should_skip(prefix, self.skip_modules):
            return UnquantizedLinearMethod()
        module_fqn = prefix
        if isinstance(self.torchao_config, ModuleFqnToConfig):
            module_fqn_to_config = self.torchao_config.module_fqn_to_config
            c = module_fqn_to_config.get(module_fqn) or module_fqn_to_config.get('_default', None)
            if c is not None:
                current_torchao_config = TorchAOConfig(c, self.skip_modules)
                return TorchAOLinearMethod(current_torchao_config)
            else:
                return UnquantizedLinearMethod()
        return TorchAOLinearMethod(self)
    def get_scaled_act_names(self) -> list[str]:
        return []
def torchao_quantize_param_data(param: torch.Tensor, torchao_config: Any) -> torch.nn.Parameter:
    from torchao.core.config import AOBaseConfig
    from torchao.quantization import quantize_
    assert isinstance(torchao_config, AOBaseConfig), f'{torchao_config}'
    ' \n    Avoid real weight allocation for faster load, since we will \n    end up setting it to param.\n    '
    with torch.device('meta'):
        dummy_linear = torch.nn.Linear(param.shape[1], param.shape[0], bias=False)
    dummy_linear.weight = param
    quantize_(dummy_linear, torchao_config)
    return dummy_linear.weight
class TorchAOLinearMethod(LinearMethodBase):
    def __init__(self, quant_config: TorchAOConfig):
        self.quant_config = quant_config
    def create_weights(self, layer: torch.nn.Module, input_size_per_partition: int, output_partition_sizes: list[int], input_size: int, output_size: int, params_dtype: torch.dtype, **extra_weight_attrs):
        weight = Parameter(torch.empty(sum(output_partition_sizes), input_size_per_partition, dtype=params_dtype), requires_grad=False)
        weight = torchao_quantize_param_data(weight, self.quant_config.torchao_config)
        set_weight_attrs(weight, {'input_dim': 1, 'output_dim': 0})
        layer.register_parameter('weight', weight)
        set_weight_attrs(weight, extra_weight_attrs)
    def apply(self, layer: torch.nn.Module, x: torch.Tensor, bias: Optional[torch.Tensor]=None) -> torch.Tensor:
        return F.linear(x, layer.weight, bias)