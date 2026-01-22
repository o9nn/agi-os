from typing import Any, Dict, List, Tuple, Union
import torch
from torch import nn
def split_decoder_layer_inputs(*args: Union[torch.Tensor, Any], **kwargs: Union[torch.Tensor, Any]) -> Tuple[List[List[Any]], List[Dict[str, Any]]]:
    if not isinstance(args[0], torch.Tensor):
        raise ValueError('The first argument must be a Tensor')
    bs = args[0].size(0)
    batch_args = []
    batch_kwargs = []
    for i in range(bs):
        new_args = []
        for val in args:
            if isinstance(val, torch.Tensor) and val.size(0) == bs:
                new_args.append(val[i:i + 1])
            else:
                new_args.append(val)
        new_kwargs = {}
        for name, val in kwargs.items():
            if isinstance(val, torch.Tensor) and val.size(0) == bs:
                new_kwargs[name] = val[i:i + 1]
            else:
                new_kwargs[name] = val
        batch_args.append(new_args)
        batch_kwargs.append(new_kwargs)
    return (batch_args, batch_kwargs)
def concat_decoder_layer_outputs(batch_outputs: List[Tuple[Any]]) -> Tuple[Any]:
    num_returns = len(batch_outputs[0])
    def is_past_key_value(data: Any) -> bool:
        flag = isinstance(data, tuple)
        flag = flag and len(data) == 2
        flag = flag and isinstance(data[0], torch.Tensor)
        flag = flag and isinstance(data[1], torch.Tensor)
        return flag
    new_outputs = []
    for i in range(num_returns):
        flag = is_past_key_value(batch_outputs[0][i])
        if flag:
            key = torch.cat([out[i][0] for out in batch_outputs])
            value = torch.cat([out[i][1] for out in batch_outputs])
            out_i = (key, value)
        else:
            out_i = torch.cat([out[i] for out in batch_outputs])
        new_outputs.append(out_i)
    return tuple(new_outputs)
def collect_target_modules(model: nn.Module, target: str, skip_names: List[str]=None, prefix: str='') -> Dict[str, nn.Module]:
    if skip_names is None:
        skip_names = []
    if not isinstance(target, (type, str)):
        raise TypeError('Target must be a string (name of the module) or a type (class of the module)')
    def _is_target(n, m):
        if isinstance(target, str):
            return target == type(m).__name__ and n not in skip_names
        return isinstance(m, target) and n not in skip_names
    name2mod = {}
    for name, mod in model.named_modules():
        m_name = f'{prefix}.{name}' if prefix else name
        if _is_target(name, mod):
            name2mod[m_name] = mod
    return name2mod
def bimap_name_mod(name2mod_mappings: List[Dict[str, nn.Module]]) -> Tuple[Dict[str, nn.Module], Dict[nn.Module, str]]:
    name2mod = {}
    mod2name = {}
    for mapping in name2mod_mappings:
        mod2name.update({v: k for k, v in mapping.items()})
        name2mod.update(mapping)
    return (name2mod, mod2name)