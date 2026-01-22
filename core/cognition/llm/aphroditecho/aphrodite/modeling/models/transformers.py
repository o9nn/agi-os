from collections.abc import Iterable, Mapping
from contextlib import contextmanager, nullcontext
from typing import Literal, Optional, Union
import regex as re
import torch
from loguru import logger
from torch import nn
from transformers import AutoModel, PretrainedConfig, PreTrainedModel
from transformers.modeling_utils import ALL_ATTENTION_FUNCTIONS
from aphrodite.attention import Attention
from aphrodite.common.config import AphroditeConfig, CacheConfig, DeviceConfig, ModelConfig, ParallelConfig
from aphrodite.common.sequence import IntermediateTensors
from aphrodite.compilation.decorators import support_torch_compile
from aphrodite.distributed import get_pp_group, get_tensor_model_parallel_world_size
from aphrodite.distributed.utils import get_pp_indices
from aphrodite.modeling.layers.linear import ColumnParallelLinear, ReplicatedLinear, RowParallelLinear
from aphrodite.modeling.layers.logits_processor import LogitsProcessor
from aphrodite.modeling.layers.vocab_parallel_embedding import ParallelLMHead, VocabParallelEmbedding
from aphrodite.modeling.sampling_metadata import SamplingMetadata
from aphrodite.multimodal import MULTIMODAL_REGISTRY, MultiModalKwargs
from aphrodite.multimodal.inputs import MultiModalDataDict, MultiModalFieldConfig, MultiModalInputs, PlaceholderRange
from aphrodite.multimodal.parse import ImageProcessorItems, MultiModalDataItems
from aphrodite.multimodal.processing import BaseMultiModalProcessor, BaseProcessingInfo
from aphrodite.multimodal.profiling import BaseDummyInputsBuilder
from aphrodite.quantization import QuantizationConfig
from aphrodite.utils import is_list_of
from .interfaces import SupportsLoRA, SupportsMultiModal, SupportsPP, SupportsQuant
from .utils import AutoWeightsLoader, PPMissingLayer, WeightsMapper, flatten_bn, make_empty_intermediate_tensors_factory, maybe_prefix
def aphrodite_flash_attention_forward(module: torch.nn.Module, query: torch.Tensor, key: torch.Tensor, value: torch.Tensor, attention_mask: torch.Tensor, scaling: Optional[float]=None, attention_instances: Optional[dict[Attention]]=None, **kwargs):
    self_attn = attention_instances[module.layer_idx]
    if scaling is not None:
        self_attn.impl.scale = float(scaling)
    hidden = query.shape[-2]
    query, key, value = (x.transpose(1, 2) for x in (query, key, value))
    query, key, value = (x.reshape(hidden, -1) for x in (query, key, value))
    return (self_attn.forward(query, key, value), None)
ALL_ATTENTION_FUNCTIONS['aphrodite'] = aphrodite_flash_attention_forward
def log_replacement(name: str, old_module: nn.Module, new_module: nn.Module):
    logger.debug('{}: {} -> {}', name, old_module, new_module)
def replace_linear_class(linear: nn.Linear, style: Literal['colwise', 'rowwise'], quant_config: QuantizationConfig) -> Union[ColumnParallelLinear, RowParallelLinear, ReplicatedLinear]:
    if not isinstance(style, str):
        raise ValueError(f'Unsupported parallel style type {type(style)}, expected str')
    aphrodite_linear_cls = {'colwise': ColumnParallelLinear, 'rowwise': RowParallelLinear}.get(style, ReplicatedLinear)
    return aphrodite_linear_cls(input_size=linear.in_features, output_size=linear.out_features, bias=linear.bias is not None, quant_config=quant_config, return_bias=False)
@contextmanager
def init_on_device_without_buffers(device: torch.device):
    old_register_parameter = nn.Module.register_parameter
    def register_empty_parameter(module, name, param):
        old_register_parameter(module, name, param)
        if param is not None:
            param_cls = type(module._parameters[name])
            kwargs = module._parameters[name].__dict__
            kwargs['requires_grad'] = param.requires_grad
            module._parameters[name] = param_cls(module._parameters[name].to(device), **kwargs)
    tensor_constructors_to_patch = {}
    def patch_tensor_constructor(fn):
        def wrapper(*args, **kwargs):
            kwargs['device'] = device
            return fn(*args, **kwargs)
        return wrapper
    try:
        nn.Module.register_parameter = register_empty_parameter
        for torch_function_name in tensor_constructors_to_patch:
            setattr(torch, torch_function_name, patch_tensor_constructor(getattr(torch, torch_function_name)))
        yield
    finally:
        nn.Module.register_parameter = old_register_parameter
        for torch_function_name, old_torch_function in tensor_constructors_to_patch.items():
            setattr(torch, torch_function_name, old_torch_function)
class MultiModalProcessingInfo(BaseProcessingInfo):
    def get_hf_config(self):
        return self.ctx.model_config.hf_config
    def get_supported_mm_limits(self):
        return {'image': None}
    def get_mm_max_tokens_per_item(self, seq_len, mm_counts):
        return {'image': self.get_max_image_tokens()}
    def get_max_image_tokens(self) -> int:
        width, height = self.get_max_image_size()
        processor = self.get_hf_processor()
        mm_processor_kwargs = self.ctx.model_config.mm_processor_kwargs or {}
        mm_tokens = processor._get_num_multimodal_tokens(image_sizes=([height, width],), **mm_processor_kwargs)
        image_tokens = mm_tokens['num_image_tokens'][0]
        return image_tokens
    def get_max_image_size(self):
        return (10000, 10000)
class MultiModalDummyInputsBuilder(BaseDummyInputsBuilder[MultiModalProcessingInfo]):
    def get_dummy_text(self, mm_counts: Mapping[str, int]) -> str:
        num_images = mm_counts.get('image', 0)
        processor = self.info.get_hf_processor()
        if 'gemma3' in processor.__class__.__name__.lower():
            image_token = processor.boi_token
        else:
            image_token = getattr(processor, 'image_token', '')
        return image_token * num_images
    def get_dummy_mm_data(self, seq_len: int, mm_counts: Mapping[str, int]) -> MultiModalDataDict:
        num_images = mm_counts.get('image', 0)
        target_width, target_height = self.info.get_max_image_size()
        return {'image': self._get_dummy_images(width=target_width, height=target_height, num_images=num_images)}
class MultiModalProcessor(BaseMultiModalProcessor[MultiModalProcessingInfo]):
    def _get_prompt_updates(self, mm_items: MultiModalDataItems, hf_processor_mm_kwargs: Mapping[str, object], out_mm_kwargs: MultiModalKwargs):
        return None
    def _get_mm_fields_config(self, hf_inputs, hf_processor_mm_kwargs, num_image_patches: torch.Tensor=None):
        hf_inputs.pop('attention_mask', None)
        mm_fields = {key: MultiModalFieldConfig.flat_from_sizes('image', num_image_patches) for key in hf_inputs}
        mm_fields['image_embeds'] = MultiModalFieldConfig.flat_from_sizes('image', num_image_patches)
        mm_fields['num_image_patches'] = MultiModalFieldConfig.batched('image')
        return mm_fields
    def _apply_hf_processor_text_mm(self, prompt_text: str, mm_items: MultiModalDataItems, hf_processor_mm_kwargs: Mapping[str, object], tokenization_kwargs: Mapping[str, object]):
        processor_data, passthrough_data = self._get_hf_mm_data(mm_items)
        processor_data['return_mm_token_type_ids'] = True
        processed_data = self._call_hf_processor(prompt=prompt_text, mm_data=processor_data, mm_kwargs=hf_processor_mm_kwargs, tok_kwargs=tokenization_kwargs)
        processed_data.update(passthrough_data)
        prompt_ids, = processed_data.pop('input_ids').tolist()
        mm_token_type_ids = processed_data.pop('mm_token_type_ids') if 'mm_token_type_ids' in processed_data else processed_data.pop('token_type_ids')
        return (prompt_ids, processed_data, mm_token_type_ids)
    def apply(self, prompt: Union[str, list[int]], mm_data: MultiModalDataDict, hf_processor_mm_kwargs: Mapping[str, object], tokenization_kwargs: Optional[Mapping[str, object]]=None, return_mm_hashes: bool=False) -> MultiModalInputs:
        if tokenization_kwargs is None:
            tokenization_kwargs = {}
        mm_items = self._to_mm_items(mm_data)
        hf_processor = self.info.get_hf_processor(**hf_processor_mm_kwargs)
        if not isinstance(prompt, str):
            prompt = hf_processor.decode(prompt)
        prompt_ids, processed_data, mm_token_type_ids = self._apply_hf_processor_text_mm(prompt_text=prompt, mm_items=mm_items, hf_processor_mm_kwargs=hf_processor_mm_kwargs, tokenization_kwargs=tokenization_kwargs)
        mm_positions = torch.where(mm_token_type_ids == 1)[1]
        images = mm_items.get_items('image', ImageProcessorItems)
        mm_processor_kwargs = self.info.ctx.model_config.mm_processor_kwargs or {}
        image_sizes = []
        for item_idx in range(len(images)):
            image_size = images.get_image_size(item_idx)
            image_sizes.append((image_size.height, image_size.width))
        mm_tokens_per_modality = hf_processor._get_num_multimodal_tokens(image_sizes=image_sizes, **mm_processor_kwargs)
        mm_placeholders = {}
        split_sizes = mm_tokens_per_modality['num_image_tokens']
        if split_sizes:
            chunked_mm_positions = torch.split(mm_positions, split_sizes)
            mm_tokens = torch.tensor(prompt_ids)[mm_token_type_ids[0].bool()]
            chunked_mm_tokens = torch.split(mm_tokens, split_sizes)
            ranges = [PlaceholderRange(offset=positions[0].item(), length=positions.shape[0], is_embed=(mm_tokens == hf_processor.image_token_id).bool()) for positions, mm_tokens in zip(chunked_mm_positions, chunked_mm_tokens)]
            mm_placeholders = {'image': ranges}
        num_image_patches = torch.tensor(mm_tokens_per_modality['num_image_patches']) if 'num_image_patches' in mm_tokens_per_modality else None
        processed_data['num_image_patches'] = num_image_patches
        mm_kwargs = MultiModalKwargs.from_hf_inputs(processed_data, self._get_mm_fields_config(processed_data, hf_processor_mm_kwargs, num_image_patches))
        mm_hashes = self._hash_mm_items(mm_items, hf_processor_mm_kwargs, tokenization_kwargs)
        return MultiModalInputs(type='multimodal', prompt=prompt, prompt_token_ids=prompt_ids, mm_kwargs=mm_kwargs, mm_hashes=mm_hashes, mm_placeholders=mm_placeholders)
class ConfigOverride:
    def __init__(self, config: PretrainedConfig, **kwargs):
        self.config = config
        self.kwargs = kwargs
        self.kwargs_original = {}
        self.kwargs_delete = set()
    def __enter__(self):
        for key, value in self.kwargs.items():
            if not hasattr(self.config, key):
                self.kwargs_delete.add(key)
            self.kwargs_original[key] = getattr(self.config, key, None)
            setattr(self.config, key, value)
        return self.config
    def __exit__(self, exc_type, exc_value, traceback):
        for key, value in self.kwargs_original.items():
            if key in self.kwargs_delete:
                delattr(self.config, key)
            else:
                setattr(self.config, key, value)
class TransformersBase(nn.Module, SupportsQuant, SupportsLoRA, SupportsPP):
    embedding_padding_modules = ['lm_head']
    embedding_modules = ['embed_tokens']
    def __init__(self, *, aphrodite_config: AphroditeConfig, prefix: str=''):
        super().__init__()
        logger.info('Using Transformers backend.')
        self.config: PretrainedConfig = aphrodite_config.model_config.hf_config
        self.text_config: PretrainedConfig = self.config.get_text_config()
        self.cache_config: CacheConfig = aphrodite_config.cache_config
        self.device_config: DeviceConfig = aphrodite_config.device_config
        self.model_config: ModelConfig = aphrodite_config.model_config
        self.parallel_config: ParallelConfig = aphrodite_config.parallel_config
        self.quant_config: QuantizationConfig = aphrodite_config.quant_config
        self.pp_group = get_pp_group()
        self.pp_size = self.pp_group.world_size
        self.pp_rank = self.pp_group.rank_in_group
        self.tp_size = get_tensor_model_parallel_world_size()
        self.skip_prefixes: Optional[list[str]] = None
        config_override = nullcontext()
        if hasattr(self.config, 'interleaved_sliding_window'):
            config_override = ConfigOverride(self.config, sliding_window=self.config.interleaved_sliding_window)
        self.text_config._attn_implementation = 'aphrodite'
        with init_on_device_without_buffers('meta'), config_override:
            self.model: PreTrainedModel = AutoModel.from_config(self.config, torch_dtype=self.model_config.dtype, trust_remote_code=self.model_config.trust_remote_code)
        self.pipeline_parallel()
        self.tensor_parallel()
        if not isinstance(self.model.get_input_embeddings(), PPMissingLayer):
            self.model.set_input_embeddings(VocabParallelEmbedding(self.text_config.vocab_size, self.text_config.hidden_size, org_num_embeddings=self.text_config.vocab_size, quant_config=self.quant_config))
        self.attention_instances = self.create_attention_instances()
        self.init_parameters(self.model)
        self.make_empty_intermediate_tensors = make_empty_intermediate_tensors_factory(['hidden_states'], self.text_config.hidden_size)
    def pipeline_parallel(self):
        if self.pp_size <= 1:
            return
        if not self.model.supports_pp_plan:
            raise ValueError(f'{type(self.model)} does not support pipeline parallel yet!')
        module_lists = []
        module_list_idx = None
        pp_plan = list(self.model._pp_plan.keys())
        for i, name in enumerate(pp_plan):
            if isinstance(getattr(self.model, name), nn.ModuleList):
                module_lists.append(name)
                module_list_idx = i
        if len(module_lists) > 1:
            raise ValueError('Pipeline parallel of models with multiple `ModuleList`s in the base model are not supported yet!')
        if module_list_idx is None:
            raise ValueError(f'Could not find `ModuleList` in {type(self.model)}')
        for name in pp_plan[:module_list_idx]:
            if self.pp_group.is_first_rank or (self.text_config.tie_word_embeddings and self.pp_group.is_last_rank):
                continue
            setattr(self.model, name, PPMissingLayer())
        start_layer, end_layer = get_pp_indices(self.text_config.num_hidden_layers, self.pp_rank, self.pp_size)
        layers_name = pp_plan[module_list_idx]
        layers = getattr(self.model, layers_name)
        for i in range(len(layers)):
            if start_layer <= i and i < end_layer:
                continue
            layers[i] = PPMissingLayer()
        for name in pp_plan[module_list_idx + 1:]:
            if not self.pp_group.is_last_rank:
                setattr(self.model, name, PPMissingLayer())
    def tensor_parallel(self):
        tp_plan = getattr(self.model.config, 'base_model_tp_plan', None) or {}
        if not tp_plan and self.tp_size > 1:
            raise ValueError(f'{type(self.model)} does not support tensor parallel yet!')
        tp_plan['.*'] = 'replicated'
        def _tensor_parallel(module: nn.Module, prefix: str=''):
            for child_name, child_module in module.named_children():
                qual_name = maybe_prefix(prefix, child_name)
                for pattern, style in tp_plan.items():
                    if re.match(pattern, qual_name) and isinstance(child_module, nn.Linear):
                        new_module = replace_linear_class(child_module, style, self.quant_config)
                        setattr(module, child_name, new_module)
                        log_replacement(qual_name, child_module, new_module)
                        break
                else:
                    _tensor_parallel(child_module, prefix=qual_name)
        _tensor_parallel(self.model)
    def create_attention_instances(self) -> dict[int, Attention]:
        num_heads = self.model_config.get_num_attention_heads(self.parallel_config)
        head_size = self.model_config.get_head_size()
        num_kv_heads = self.model_config.get_num_kv_heads(self.parallel_config)
        start, end = get_pp_indices(self.text_config.num_hidden_layers, self.pp_rank, self.pp_size)
        attention_instances = {}
        for i in range(start, end):
            sliding_window = None
            if hasattr(self.config, 'interleaved_sliding_window') and hasattr(self.config, 'sliding_window_pattern') and ((i + 1) % self.config.sliding_window_pattern > 0):
                sliding_window = self.config.interleaved_sliding_window
            attention_instances[i] = Attention(num_heads=num_heads, head_size=head_size, scale=head_size ** (-0.5), num_kv_heads=num_kv_heads, cache_config=self.cache_config, quant_config=self.quant_config, per_layer_sliding_window=sliding_window, prefix=f'{i}.attn')
        return attention_instances
    def init_parameters(self, module: nn.Module):
        for name, param in module.named_parameters(recurse=False):
            if param.device == torch.device('meta'):
                new_param = nn.Parameter(torch.empty_like(param.data, dtype=self.model_config.dtype, device=self.device_config.device))
                setattr(module, name, new_param)
        for child in module.children():
            self.init_parameters(child)
    def forward(self, input_ids: Optional[torch.Tensor], positions: torch.Tensor, intermediate_tensors: Optional[IntermediateTensors]=None, inputs_embeds: Optional[torch.Tensor]=None) -> Union[torch.Tensor, IntermediateTensors]:
        if not get_pp_group().is_first_rank:
            assert intermediate_tensors is not None
            input_ids = None
            inputs_embeds = intermediate_tensors['hidden_states']
        if input_ids is not None:
            input_ids = input_ids[None, ...]
        if inputs_embeds is not None:
            inputs_embeds = inputs_embeds[None, ...]
        if self.model_config.uses_mrope:
            position_ids = positions[:, None]
        else:
            position_ids = positions[None, ...]
        hidden_states = self.model(input_ids=input_ids, inputs_embeds=inputs_embeds, use_cache=False, position_ids=position_ids, attention_instances=self.attention_instances, return_dict=False)[0][0, ...]
        if not get_pp_group().is_last_rank:
            return IntermediateTensors({'hidden_states': hidden_states})
        return hidden_states
    def load_weights(self, weights: Iterable[tuple[str, torch.Tensor]]) -> set[str]:
        loader = AutoWeightsLoader(self, skip_prefixes=self.skip_prefixes)
        return loader.load_weights(weights, mapper=self.hf_to_aphrodite_mapper)
@support_torch_compile
class TransformersModel(TransformersBase):
    hf_to_aphrodite_mapper = WeightsMapper(orig_to_new_prefix={'': 'model.', 'model.model.': 'model.', 'model.score': 'score'})
@support_torch_compile
class TransformersForCausalLM(TransformersBase):
    def __init__(self, *, aphrodite_config: AphroditeConfig, prefix: str=''):
        super().__init__(aphrodite_config=aphrodite_config, prefix=prefix)
        if self.text_config.tie_word_embeddings:
            self.skip_prefixes = ['lm_head.']
        if get_pp_group().is_last_rank:
            self.unpadded_vocab_size = self.text_config.vocab_size
            self.lm_head = ParallelLMHead(self.text_config.vocab_size, self.text_config.hidden_size, quant_config=self.quant_config, prefix=maybe_prefix(prefix, 'lm_head'))
            if self.text_config.tie_word_embeddings:
                self.lm_head = self.lm_head.tie_weights(self.model.get_input_embeddings())
            logit_scale = getattr(self.text_config, 'logit_scale', 1.0)
            self.logits_processor = LogitsProcessor(self.unpadded_vocab_size, self.text_config.vocab_size, logit_scale)
        else:
            self.lm_head = PPMissingLayer()
    def compute_logits(self, hidden_states: torch.Tensor, sampling_metadata: SamplingMetadata) -> Optional[torch.Tensor]:
        logits = self.logits_processor(self.lm_head, hidden_states, sampling_metadata)
        return logits
@MULTIMODAL_REGISTRY.register_processor(MultiModalProcessor, info=MultiModalProcessingInfo, dummy_inputs=MultiModalDummyInputsBuilder)
class TransformersForMultimodalLM(TransformersForCausalLM, SupportsMultiModal):
    hf_to_aphrodite_mapper = WeightsMapper(orig_to_new_prefix={'language_model.model': 'model.language_model', 'text_model.model': 'model.text_model', 'vision_tower': 'model.vision_tower', 'vqmodel': 'model.vqmodel', 'visual': 'model.visual', 'vision_model': 'model.vision_model', 'vision_embed_tokens': 'model.vision_embed_tokens', 'image_newline': 'model.image_newline', 'multi_modal_projector': 'model.multi_modal_projector', 'text_model.lm_head': 'lm_head', 'language_model.lm_head': 'lm_head', 'model.embed_tokens': 'model.language_model.embed_tokens', 'model.layers': 'model.language_model.layers', 'model.norm': 'model.language_model.norm'})
    def __init__(self, *, aphrodite_config: AphroditeConfig, prefix: str=''):
        super().__init__(aphrodite_config=aphrodite_config, prefix=prefix)
        self.dtype = aphrodite_config.model_config.dtype
    def forward(self, input_ids: Optional[torch.Tensor], positions: torch.Tensor, intermediate_tensors: Optional[IntermediateTensors]=None, inputs_embeds: Optional[torch.Tensor]=None, **kwargs: object) -> Union[torch.Tensor, IntermediateTensors]:
        if inputs_embeds is None:
            multimodal_embeds = self.get_multimodal_embeddings(**kwargs)
            if multimodal_embeds is not None:
                inputs_embeds = self.get_input_embeddings(input_ids, multimodal_embeds)
                input_ids = None
        model_output = super().forward(input_ids, positions, intermediate_tensors, inputs_embeds)
        return model_output
    def get_multimodal_embeddings(self, **kwargs):
        pixel_values = kwargs.pop('pixel_values', None)
        pixel_values = pixel_values if pixel_values is not None else kwargs.pop('image_patches', None)
        image_embeds = kwargs.pop('image_embeds', None)
        if image_embeds is not None:
            return image_embeds
        if pixel_values is None and image_embeds is None:
            return None
        num_image_patches = kwargs.pop('num_image_patches')
        if pixel_values is not None:
            if isinstance(pixel_values, torch.Tensor):
                pixel_values = flatten_bn(pixel_values).to(self.dtype)
            elif is_list_of(pixel_values, torch.Tensor):
                pixel_values = flatten_bn(flatten_bn(pixel_values), concat=True).to(self.dtype)
            else:
                raise ValueError(f'Unsupported pixel_values type {type(pixel_values)}. Expected `torch.Tensor` or list of `torch.Tensor`.')
            if isinstance(num_image_patches, list):
                num_image_patches = torch.cat(num_image_patches)
            vision_embeddings = self.model.get_image_features(pixel_values, **{k: v.flatten(0, 1) for k, v in kwargs.items()})
            if isinstance(vision_embeddings, torch.Tensor):
                if vision_embeddings.ndim == 2:
                    vision_embeddings = vision_embeddings.unsqueeze(0)
                vision_embeddings = torch.split(vision_embeddings, num_image_patches.flatten().tolist())
                vision_embeddings = [embed.flatten(start_dim=0, end_dim=-2) for embed in vision_embeddings]
            return vision_embeddings
    def get_input_embeddings(self, input_ids: torch.Tensor, multimodal_embeddings=None) -> torch.Tensor:
        inputs_embeds = self.model.get_input_embeddings()(input_ids)
        if multimodal_embeddings is not None and len(multimodal_embeddings) != 0:
            mask = input_ids == self.config.image_token_id
            mask = mask.unsqueeze(-1).expand_as(inputs_embeds)
            multimodal_embeddings = torch.cat(multimodal_embeddings)
            inputs_embeds = inputs_embeds.masked_scatter(mask, multimodal_embeddings)
        return inputs_embeds