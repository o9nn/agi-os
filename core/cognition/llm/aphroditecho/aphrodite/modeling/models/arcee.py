from collections.abc import Iterable
from typing import Any, Optional, Union
import torch
from torch import nn
from transformers import LlamaConfig
from aphrodite.common.sequence import IntermediateTensors
from aphrodite.compilation.decorators import support_torch_compile
from aphrodite.distributed import get_pp_group
from aphrodite.modeling.layers.activation import ReLUSquaredActivation
from aphrodite.modeling.layers.layernorm import RMSNorm
from aphrodite.modeling.layers.linear import ColumnParallelLinear, RowParallelLinear
from aphrodite.modeling.layers.logits_processor import LogitsProcessor
from aphrodite.modeling.layers.vocab_parallel_embedding import DEFAULT_VOCAB_PADDING_SIZE, ParallelLMHead, VocabParallelEmbedding
from aphrodite.modeling.model_loader.weight_utils import default_weight_loader, maybe_remap_kv_scale_name
from .interfaces import SupportsLoRA, SupportsPP
from .utils import AutoWeightsLoader, PPMissingLayer, is_pp_missing_parameter, make_empty_intermediate_tensors_factory, make_layers
class ArceeMLP(nn.Module):
    def __init__(self, hidden_size: int, intermediate_size: int, hidden_act: str, quant_config: Optional[Any]=None, bias: bool=False, prefix: str='', reduce_results: bool=True) -> None:
        super().__init__()
        self.up_proj = ColumnParallelLinear(input_size=hidden_size, output_size=intermediate_size, bias=bias, quant_config=quant_config, prefix=f'{prefix}.up_proj')
        self.down_proj = RowParallelLinear(input_size=intermediate_size, output_size=hidden_size, bias=bias, quant_config=quant_config, reduce_results=reduce_results, prefix=f'{prefix}.down_proj')
        if hidden_act != 'relu2':
            raise ValueError(f"Unsupported activation: {hidden_act}. Only 'relu2' is supported for AFM.")
        self.act_fn = ReLUSquaredActivation()
    def forward(self, x: torch.Tensor) -> torch.Tensor:
        x, _ = self.up_proj(x)
        x = self.act_fn(x)
        x, _ = self.down_proj(x)
        return x
class ArceeDecoderLayer(nn.Module):
    def __init__(self, config: LlamaConfig, cache_config: Optional[Any]=None, quant_config: Optional[Any]=None, prefix: str='') -> None:
        super().__init__()
        self.hidden_size = config.hidden_size
        rope_theta = getattr(config, 'rope_theta', 10000)
        rope_scaling = getattr(config, 'rope_scaling', None)
        if rope_scaling is not None and getattr(config, 'original_max_position_embeddings', None):
            rope_scaling['original_max_position_embeddings'] = config.original_max_position_embeddings
        max_position_embeddings = getattr(config, 'max_position_embeddings', 8192)
        attention_bias = getattr(config, 'attention_bias', False) or getattr(config, 'bias', False)
        bias_o_proj = attention_bias
        if hasattr(config, 'qkv_bias'):
            attention_bias = config.qkv_bias
        from aphrodite.modeling.models.llama import LlamaAttention
        self.self_attn = LlamaAttention(config=config, hidden_size=self.hidden_size, num_heads=config.num_attention_heads, num_kv_heads=getattr(config, 'num_key_value_heads', config.num_attention_heads), rope_theta=rope_theta, rope_scaling=rope_scaling, max_position_embeddings=max_position_embeddings, quant_config=quant_config, bias=attention_bias, bias_o_proj=bias_o_proj, cache_config=cache_config, prefix=f'{prefix}.self_attn', attn_type=getattr(config, 'attn_type', 'decoder'))
        self.mlp = ArceeMLP(hidden_size=self.hidden_size, intermediate_size=config.intermediate_size, hidden_act=config.hidden_act, quant_config=quant_config, bias=getattr(config, 'mlp_bias', False), prefix=f'{prefix}.mlp')
        self.input_layernorm = RMSNorm(config.hidden_size, eps=config.rms_norm_eps)
        self.post_attention_layernorm = RMSNorm(config.hidden_size, eps=config.rms_norm_eps)
    def forward(self, positions: torch.Tensor, hidden_states: torch.Tensor, residual: Optional[torch.Tensor]) -> tuple[torch.Tensor, torch.Tensor]:
        if residual is None:
            residual = hidden_states
            hidden_states = self.input_layernorm(hidden_states)
        else:
            hidden_states, residual = self.input_layernorm(hidden_states, residual)
        hidden_states = self.self_attn(positions=positions, hidden_states=hidden_states)
        hidden_states, residual = self.post_attention_layernorm(hidden_states, residual)
        hidden_states = self.mlp(hidden_states)
        return (hidden_states, residual)
@support_torch_compile
class ArceeModel(nn.Module):
    def __init__(self, *, aphrodite_config, prefix: str='', layer_type: type[nn.Module]=ArceeDecoderLayer) -> None:
        super().__init__()
        config: LlamaConfig = aphrodite_config.model_config.hf_config
        cache_config = aphrodite_config.cache_config
        quant_config = aphrodite_config.quant_config
        self.quant_config = quant_config
        self.config = config
        self.vocab_size = config.vocab_size
        self.org_vocab_size = config.vocab_size
        if get_pp_group().is_first_rank or (config.tie_word_embeddings and get_pp_group().is_last_rank):
            self.embed_tokens = VocabParallelEmbedding(self.vocab_size, config.hidden_size, org_num_embeddings=config.vocab_size, quant_config=quant_config)
        else:
            self.embed_tokens = PPMissingLayer()
        self.start_layer, self.end_layer, self.layers = make_layers(config.num_hidden_layers, lambda prefix: layer_type(config=config, cache_config=cache_config, quant_config=quant_config, prefix=prefix), prefix=f'{prefix}.layers')
        if get_pp_group().is_last_rank:
            self.norm = RMSNorm(config.hidden_size, eps=config.rms_norm_eps)
        else:
            self.norm = PPMissingLayer()
        self.aux_hidden_state_layers: tuple[int, ...] = tuple()
        self.make_empty_intermediate_tensors = make_empty_intermediate_tensors_factory(['hidden_states', 'residual'], config.hidden_size)
    def get_input_embeddings(self, input_ids: torch.Tensor) -> torch.Tensor:
        return self.embed_tokens(input_ids)
    def forward(self, input_ids: Optional[torch.Tensor], positions: torch.Tensor, intermediate_tensors: Optional[IntermediateTensors], inputs_embeds: Optional[torch.Tensor]=None) -> Union[torch.Tensor, IntermediateTensors, tuple[torch.Tensor, list[torch.Tensor]]]:
        if get_pp_group().is_first_rank:
            hidden_states = inputs_embeds if inputs_embeds is not None else self.get_input_embeddings(input_ids)
            residual = None
        else:
            assert intermediate_tensors is not None, 'IntermediateTensors must be provided for non-first pipeline ranks'
            hidden_states = intermediate_tensors['hidden_states']
            residual = intermediate_tensors['residual']
        aux_hidden_states: list[torch.Tensor] = []
        for idx, layer in enumerate(self.layers[self.start_layer:self.end_layer]):
            if idx in self.aux_hidden_state_layers:
                aux_hidden_states.append(hidden_states + residual)
            hidden_states, residual = layer(positions, hidden_states, residual)
        if not get_pp_group().is_last_rank:
            return IntermediateTensors({'hidden_states': hidden_states, 'residual': residual})
        hidden_states, _ = self.norm(hidden_states, residual)
        if len(aux_hidden_states) > 0:
            return (hidden_states, aux_hidden_states)
        return hidden_states
    def load_weights(self, weights: Iterable[tuple[str, torch.Tensor]]) -> set[str]:
        stacked_params_mapping = [('.qkv_proj', '.q_proj', 'q'), ('.qkv_proj', '.k_proj', 'k'), ('.qkv_proj', '.v_proj', 'v')]
        params_dict = dict(self.named_parameters())
        loaded_params: set[str] = set()
        for name, loaded_weight in weights:
            if 'rotary_emb.inv_freq' in name:
                continue
            if 'rotary_emb.cos_cached' in name or 'rotary_emb.sin_cached' in name:
                continue
            if self.quant_config is not None and (scale_name := self.quant_config.get_cache_scale(name)):
                param = params_dict[scale_name]
                weight_loader = getattr(param, 'weight_loader', default_weight_loader)
                loaded_weight = loaded_weight if loaded_weight.dim() == 0 else loaded_weight[0]
                weight_loader(param, loaded_weight)
                loaded_params.add(scale_name)
                continue
            if 'scale' in name:
                remapped_name = maybe_remap_kv_scale_name(name, params_dict)
                if remapped_name is None:
                    continue
                name = remapped_name
            mapped = False
            for param_name, weight_name, shard_id in stacked_params_mapping:
                if weight_name not in name:
                    continue
                name = name.replace(weight_name, param_name)
                if name.endswith('.bias') and name not in params_dict:
                    mapped = True
                    break
                if is_pp_missing_parameter(name, self):
                    mapped = True
                    break
                param = params_dict[name]
                weight_loader = param.weight_loader
                weight_loader(param, loaded_weight, shard_id)
                loaded_params.add(name)
                mapped = True
                break
            if mapped:
                continue
            if name.endswith('.bias') and name not in params_dict:
                continue
            if is_pp_missing_parameter(name, self):
                continue
            param = params_dict[name]
            weight_loader = getattr(param, 'weight_loader', default_weight_loader)
            weight_loader(param, loaded_weight)
            loaded_params.add(name)
        return loaded_params
class ArceeForCausalLM(nn.Module, SupportsLoRA, SupportsPP):
    packed_modules_mapping = {'qkv_proj': ['q_proj', 'k_proj', 'v_proj']}
    def __init__(self, *, aphrodite_config, prefix: str='') -> None:
        super().__init__()
        config = aphrodite_config.model_config.hf_config
        self.config = config
        self.model = ArceeModel(aphrodite_config=aphrodite_config, prefix=f'{prefix}.model')
        if get_pp_group().is_last_rank:
            self.unpadded_vocab_size = config.vocab_size
            self.lm_head = ParallelLMHead(self.unpadded_vocab_size, config.hidden_size, org_num_embeddings=config.vocab_size, padding_size=DEFAULT_VOCAB_PADDING_SIZE, quant_config=aphrodite_config.quant_config, bias=getattr(config, 'lm_head_bias', False), prefix=f'{prefix}.lm_head')
            if config.tie_word_embeddings:
                self.lm_head = self.lm_head.tie_weights(self.model.embed_tokens)
            logit_scale = getattr(config, 'logit_scale', 1.0)
            self.logits_processor = LogitsProcessor(self.unpadded_vocab_size, config.vocab_size, logit_scale)
        else:
            self.lm_head = PPMissingLayer()
        self.make_empty_intermediate_tensors = self.model.make_empty_intermediate_tensors
    def forward(self, input_ids: torch.Tensor, positions: torch.Tensor, intermediate_tensors: Optional[IntermediateTensors]=None, inputs_embeds: Optional[torch.Tensor]=None) -> Union[torch.Tensor, IntermediateTensors]:
        model_output = self.model(input_ids=input_ids, positions=positions, intermediate_tensors=intermediate_tensors, inputs_embeds=inputs_embeds)
        return model_output
    def compute_logits(self, hidden_states: torch.Tensor, sampling_metadata) -> Optional[torch.Tensor]:
        logits = self.logits_processor(self.lm_head, hidden_states, sampling_metadata)
        return logits
    def get_input_embeddings(self, input_ids: torch.Tensor) -> torch.Tensor:
        return self.model.get_input_embeddings(input_ids)
    def load_weights(self, weights: Iterable[tuple[str, torch.Tensor]]) -> set[str]:
        loader = AutoWeightsLoader(self, skip_prefixes=['lm_head.'] if self.config.tie_word_embeddings else None, skip_substrs=['gate_proj'])
        return loader.load_weights(weights)