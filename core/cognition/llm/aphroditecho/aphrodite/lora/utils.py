import os
import re
from typing import TYPE_CHECKING, Optional, Union
import huggingface_hub
from huggingface_hub.utils import EntryNotFoundError, HfHubHTTPError, HFValidationError, RepositoryNotFoundError
from loguru import logger
from torch import nn
from transformers import PretrainedConfig
from aphrodite.common.config import LoRAConfig
from aphrodite.lora.fully_sharded_layers import ColumnParallelLinearWithShardedLoRA, MergedColumnParallelLinearWithShardedLoRA, MergedQKVParallelLinearWithShardedLoRA, QKVParallelLinearWithShardedLoRA, RowParallelLinearWithShardedLoRA
from aphrodite.lora.layers import BaseLayerWithLoRA, ColumnParallelLinearWithLoRA, LogitsProcessorWithLoRA, MergedColumnParallelLinearWithLoRA, MergedQKVParallelLinearWithLoRA, QKVParallelLinearWithLoRA, ReplicatedLinearWithLoRA, RowParallelLinearWithLoRA, VocabParallelEmbeddingWithLoRA
from aphrodite.modeling.layers.linear import LinearBase
if TYPE_CHECKING:
    from aphrodite.modeling.layers.logits_processor import LogitsProcessor
    from aphrodite.modeling.layers.vocab_parallel_embedding import ParallelLMHead
    from aphrodite.modeling.models.utils import WeightsMapper
_all_lora_classes: set[type[BaseLayerWithLoRA]] = {VocabParallelEmbeddingWithLoRA, ColumnParallelLinearWithLoRA, MergedColumnParallelLinearWithLoRA, QKVParallelLinearWithLoRA, MergedQKVParallelLinearWithLoRA, RowParallelLinearWithLoRA, ReplicatedLinearWithLoRA, LogitsProcessorWithLoRA, ColumnParallelLinearWithShardedLoRA, QKVParallelLinearWithShardedLoRA, MergedColumnParallelLinearWithShardedLoRA, MergedQKVParallelLinearWithShardedLoRA, RowParallelLinearWithShardedLoRA}
def from_layer(layer: nn.Module, max_loras: int, lora_config: LoRAConfig, packed_modules_list: list, model_config: Optional[PretrainedConfig]=None) -> nn.Module:
    for lora_cls in _all_lora_classes:
        if lora_cls.can_replace_layer(source_layer=layer, lora_config=lora_config, packed_modules_list=packed_modules_list, model_config=model_config):
            instance_layer = lora_cls(layer)
            instance_layer.create_lora_weights(max_loras, lora_config, model_config)
            return instance_layer
    return layer
def from_layer_logits_processor(layer: 'LogitsProcessor', lm_head: 'ParallelLMHead', max_loras: int, lora_config: LoRAConfig, model_config: Optional[PretrainedConfig]=None) -> LogitsProcessorWithLoRA:
    ret = LogitsProcessorWithLoRA(layer, lm_head.embedding_dim, lm_head.weight.dtype, lm_head.weight.device, lm_head.get_sharded_to_full_mapping())
    ret.create_lora_weights(max_loras, lora_config, model_config)
    return ret
def replace_submodule(model: nn.Module, module_name: str, new_module: nn.Module) -> nn.Module:
    parent = model.get_submodule('.'.join(module_name.split('.')[:-1]))
    target_name = module_name.split('.')[-1]
    setattr(parent, target_name, new_module)
    return new_module
def parse_fine_tuned_lora_name(name: str, weights_mapper: Optional['WeightsMapper']=None) -> Optional[tuple[str, bool, bool]]:
    if name.startswith('base_model.model.'):
        name = name.replace('base_model.model.', '')
        name = weights_mapper._map_name(name) if weights_mapper else name
        name = 'base_model.model.' + name
    else:
        name = weights_mapper._map_name(name) if weights_mapper else name
    start_index = 2 if name.startswith('base_model.model.') else 0
    parts = name.split('.')
    if parts[-1] == 'weight' and (parts[-2] == 'lora_A' or parts[-2] == 'lora_B'):
        new_name = '.'.join(parts[start_index:-2])
        return (new_name, parts[-2] == 'lora_A', False)
    if parts[-1] == 'lora_embedding_A' or parts[-1] == 'lora_embedding_B':
        new_name = '.'.join(parts[start_index:-1])
        return (new_name, parts[-1] == 'lora_embedding_A', False)
    if parts[-1] == 'bias':
        new_name = '.'.join(parts[start_index:-2])
        return (new_name, False, True)
    return None
def is_supported_lora_weight(name: str) -> bool:
    return parse_fine_tuned_lora_name(name) is not None
def is_regex_target_modules(load_modules: Union[str, list[str]], expected_lora_modules: list[str]) -> bool:
    def is_valid_regex(pattern):
        try:
            re.compile(pattern)
            return True
        except re.error:
            return False
    def is_subset(sub_list, full_list):
        return set(sub_list).issubset(set(full_list))
    if not isinstance(load_modules, str):
        return False
    if is_valid_regex(load_modules):
        match = re.search('\\((.*?)\\)\\$?$', load_modules)
        if match:
            suffix = match.group(1).split('|')
            return is_subset(suffix, expected_lora_modules)
    return False
def get_supported_lora_modules(model: nn.Module) -> list[str]:
    supported_lora_modules: set[str] = set()
    for name, module in model.named_modules():
        embedding_modules = getattr(module, 'embedding_modules', None)
        if embedding_modules is not None:
            for name in embedding_modules:
                supported_lora_modules.add(name)
        if isinstance(module, (LinearBase,)):
            supported_lora_modules.add(name.split('.')[-1])
    return list(supported_lora_modules)
def get_adapter_absolute_path(lora_path: str) -> str:
    if os.path.isabs(lora_path):
        return lora_path
    if lora_path.startswith('~'):
        return os.path.expanduser(lora_path)
    if os.path.exists(lora_path):
        return os.path.abspath(lora_path)
    try:
        local_snapshot_path = huggingface_hub.snapshot_download(repo_id=lora_path)
    except (HfHubHTTPError, RepositoryNotFoundError, EntryNotFoundError, HFValidationError):
        logger.exception('Error downloading the HuggingFace model')
        return lora_path
    return local_snapshot_path