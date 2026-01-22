from collections import defaultdict
from typing import TYPE_CHECKING, Optional
import torch
from aphrodite.common.config import ModelConfig, SchedulerConfig
from aphrodite.modeling.models.interfaces import MultiModalEmbeddings
from aphrodite.modeling.models.utils import extract_layer_index
from aphrodite.multimodal.registry import MultiModalRegistry
from aphrodite.v1.core.encoder_cache_manager import compute_encoder_budget
from aphrodite.v1.kv_cache_interface import KVCacheGroupSpec
if TYPE_CHECKING:
    from aphrodite.attention.layer import Attention
class MultiModalBudget:
    def __init__(self, model_config: ModelConfig, scheduler_config: SchedulerConfig, mm_registry: MultiModalRegistry, *, max_model_len: int, max_num_reqs: int) -> None:
        super().__init__()
        self.model_config = model_config
        self.scheduler_config = scheduler_config
        self.mm_registry = mm_registry
        encoder_compute_budget, encoder_cache_size = compute_encoder_budget(model_config=model_config, scheduler_config=scheduler_config, mm_registry=mm_registry)
        self.max_num_encoder_input_tokens = encoder_compute_budget
        self.encoder_cache_size = encoder_cache_size
        self.max_model_len = max_model_len
        self.max_num_reqs = max_num_reqs
        self.mm_limits = mm_registry.get_mm_limits_per_prompt(model_config)
        max_items_per_prompt_by_modality = dict[str, int]()
        max_items_per_batch_by_modality = dict[str, int]()
        max_tokens_by_modality = mm_registry.get_max_tokens_per_item_by_nonzero_modality(model_config)
        for modality, max_tokens in max_tokens_by_modality.items():
            max_items_per_prompt, max_items_per_batch = self.get_max_items(modality, max_tokens)
            max_items_per_prompt_by_modality[modality] = max_items_per_prompt
            max_items_per_batch_by_modality[modality] = max_items_per_batch
        self.max_tokens_by_modality = max_tokens_by_modality
        self.max_items_per_prompt_by_modality = max_items_per_prompt_by_modality
        self.max_items_per_batch_by_modality = max_items_per_batch_by_modality
    def get_modality_with_max_tokens(self) -> tuple[str, int]:
        max_tokens_by_modality = self.max_tokens_by_modality
        modality, max_tokens = max(max_tokens_by_modality.items(), key=lambda item: item[1])
        return (modality, max_tokens)
    def get_encoder_budget(self) -> int:
        return min(self.max_num_encoder_input_tokens, self.encoder_cache_size)
    def get_max_items(self, modality: str, max_tokens_per_item: int) -> tuple[int, int]:
        if max_tokens_per_item == 0:
            return (0, 0)
        encoder_budget = self.get_encoder_budget()
        if encoder_budget == 0:
            return (0, 0)
        max_encoder_items_per_batch = encoder_budget // max_tokens_per_item
        mm_limit = self.mm_limits[modality]
        max_items_per_prompt = max(1, min(mm_limit, self.max_model_len // max_tokens_per_item))
        scheduler_config = self.scheduler_config
        max_num_reqs = self.max_num_reqs
        if not scheduler_config.enable_chunked_prefill:
            max_num_reqs = min(max_num_reqs, scheduler_config.max_num_batched_tokens // max_tokens_per_item)
        max_decoder_items_per_batch = max_num_reqs * max_items_per_prompt
        max_items_per_batch = max(1, min(max_encoder_items_per_batch, max_decoder_items_per_batch))
        return (max_items_per_prompt, max_items_per_batch)
def sanity_check_mm_encoder_outputs(mm_embeddings: MultiModalEmbeddings, expected_num_items: int) -> None:
    assert isinstance(mm_embeddings, (list, tuple, torch.Tensor)), f"Expected multimodal embeddings to be a list/tuple of 2D tensors, or a single 3D tensor, but got {type(mm_embeddings)} instead. This is most likely due to incorrect implementation of the model's `get_multimodal_embeddings` method."
    assert len(mm_embeddings) == expected_num_items, f"Expected number of multimodal embeddings to match number of input items: {expected_num_items}, but got len(mm_embeddings)={len(mm_embeddings)!r} instead. This is most likely due to incorrect implementation of the model's `get_multimodal_embeddings` method."
    assert all((e.ndim == 2 for e in mm_embeddings)), f"Expected multimodal embeddings to be a sequence of 2D tensors, but got tensors with shapes {[e.shape for e in mm_embeddings]} instead. This is most likely due to incorrect implementation of the model's `get_multimodal_embeddings` method."
def scatter_mm_placeholders(embeds: torch.Tensor, is_embed: Optional[torch.Tensor]) -> torch.Tensor:
    if is_embed is None:
        return embeds
    placeholders = embeds.new_full((is_embed.shape[0], embeds.shape[-1]), fill_value=torch.nan)
    placeholders[is_embed] = embeds
    return placeholders
def gather_mm_placeholders(placeholders: torch.Tensor, is_embed: Optional[torch.Tensor]) -> torch.Tensor:
    if is_embed is None:
        return placeholders
    return placeholders[is_embed]
def initialize_kv_cache_for_kv_sharing(shared_kv_cache_layers: dict[str, str], kv_cache_groups: list[KVCacheGroupSpec], kv_caches: dict[str, torch.Tensor]) -> None:
    layer_to_kv_cache_group_idx: dict[str, int] = {}
    for i, kv_cache_group in enumerate(kv_cache_groups):
        for layer_name in kv_cache_group.layer_names:
            layer_to_kv_cache_group_idx[layer_name] = i
    for layer_name, target_layer_name in shared_kv_cache_layers.items():
        kv_caches[layer_name] = kv_caches[target_layer_name]
        group_idx = layer_to_kv_cache_group_idx[target_layer_name]
        kv_cache_groups[group_idx].layer_names.append(layer_name)
def bind_kv_cache(kv_caches: dict[str, torch.Tensor], forward_context: dict[str, 'Attention'], runner_kv_caches: list[torch.Tensor]) -> None:
    assert len(runner_kv_caches) == 0
    index2name = defaultdict(list)
    for layer_name in kv_caches:
        index2name[extract_layer_index(layer_name)].append(layer_name)
    for layer_index in sorted(index2name.keys()):
        layer_names = index2name[layer_index]
        if len(layer_names) > 1:
            raise NotImplementedError
        layer_name = layer_names[0]
        runner_kv_caches.append(kv_caches[layer_name])
    for layer_name, kv_cache in kv_caches.items():
        forward_context[layer_name].kv_cache = [kv_cache]