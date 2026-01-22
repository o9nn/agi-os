from collections.abc import Iterable
from typing import Optional, Union
import torch
import torch.nn as nn
from aphrodite.common.config import AphroditeConfig
from aphrodite.common.sequence import IntermediateTensors
from aphrodite.compilation.decorators import support_torch_compile
from aphrodite.distributed import get_pp_group
from aphrodite.modeling.layers.logits_processor import LogitsProcessor
from aphrodite.modeling.layers.vocab_parallel_embedding import ParallelLMHead
from aphrodite.modeling.model_loader.weight_utils import default_weight_loader, maybe_remap_kv_scale_name
from aphrodite.modeling.models.qwen2 import Qwen2ForCausalLM, Qwen2Model
from aphrodite.modeling.sampling_metadata import SamplingMetadata
from .utils import PPMissingLayer, is_pp_missing_parameter, maybe_prefix
@support_torch_compile(dynamic_arg_dims={'input_ids': 0, 'positions': -1, 'intermediate_tensors': 0, 'inputs_embeds': 0})
class MiMoModel(Qwen2Model):
    def forward(self, input_ids: torch.Tensor, positions: torch.Tensor, intermediate_tensors: Optional[IntermediateTensors]=None, inputs_embeds: Optional[torch.Tensor]=None) -> Union[torch.Tensor, IntermediateTensors]:
        if get_pp_group().is_first_rank:
            if inputs_embeds is not None:
                hidden_states = inputs_embeds
            else:
                hidden_states = self.get_input_embeddings(input_ids)
            residual = None
        else:
            assert intermediate_tensors is not None
            hidden_states = intermediate_tensors['hidden_states']
            residual = intermediate_tensors['residual']
        for layer in self.layers[self.start_layer:self.end_layer]:
            hidden_states, residual = layer(positions, hidden_states, residual)
        if not get_pp_group().is_last_rank:
            return IntermediateTensors({'hidden_states': hidden_states, 'residual': residual})
        hidden_states = hidden_states + residual
        return hidden_states
    def load_weights(self, weights: Iterable[tuple[str, torch.Tensor]]) -> set[str]:
        stacked_params_mapping = [('qkv_proj', 'q_proj', 'q'), ('qkv_proj', 'k_proj', 'k'), ('qkv_proj', 'v_proj', 'v'), ('gate_up_proj', 'gate_proj', 0), ('gate_up_proj', 'up_proj', 1)]
        params_dict = dict(self.named_parameters(remove_duplicate=False))
        loaded_params: set[str] = set()
        for name, loaded_weight in weights:
            if 'mtp_layers' in name:
                continue
            if 'rotary_emb.inv_freq' in name:
                continue
            if self.quant_config is not None and (scale_name := self.quant_config.get_cache_scale(name)):
                param = params_dict[scale_name]
                weight_loader = getattr(param, 'weight_loader', default_weight_loader)
                loaded_weight = loaded_weight if loaded_weight.dim() == 0 else loaded_weight[0]
                weight_loader(param, loaded_weight)
                loaded_params.add(scale_name)
                continue
            for param_name, weight_name, shard_id in stacked_params_mapping:
                if weight_name not in name:
                    continue
                name = name.replace(weight_name, param_name)
                if name.endswith('.bias') and name not in params_dict:
                    continue
                if is_pp_missing_parameter(name, self):
                    continue
                param = params_dict[name]
                weight_loader = param.weight_loader
                weight_loader(param, loaded_weight, shard_id)
                break
            else:
                if name.endswith('.bias') and name not in params_dict:
                    continue
                name = maybe_remap_kv_scale_name(name, params_dict)
                if name is None:
                    continue
                if is_pp_missing_parameter(name, self):
                    continue
                param = params_dict[name]
                weight_loader = getattr(param, 'weight_loader', default_weight_loader)
                weight_loader(param, loaded_weight)
            loaded_params.add(name)
        return loaded_params
class MiMoForCausalLM(Qwen2ForCausalLM, nn.Module):
    def __init__(self, *, aphrodite_config: AphroditeConfig, prefix: str=''):
        nn.Module.__init__(self)
        config = aphrodite_config.model_config.hf_config
        quant_config = aphrodite_config.quant_config
        lora_config = aphrodite_config.lora_config
        self.config = config
        self.lora_config = lora_config
        self.quant_config = quant_config
        self.model = MiMoModel(aphrodite_config=aphrodite_config, prefix=maybe_prefix(prefix, 'model'))
        if get_pp_group().is_last_rank:
            if config.tie_word_embeddings:
                self.lm_head = self.model.embed_tokens
            else:
                self.lm_head = ParallelLMHead(config.vocab_size, config.hidden_size, quant_config=quant_config, prefix=maybe_prefix(prefix, 'lm_head'))
        else:
            self.lm_head = PPMissingLayer()
        self.logits_processor = LogitsProcessor(config.vocab_size)
        self.make_empty_intermediate_tensors = self.model.make_empty_intermediate_tensors
    def compute_logits(self, hidden_states: torch.Tensor, sampling_metadata: SamplingMetadata) -> Optional[torch.Tensor]:
        hidden_states = self.model.norm(hidden_states)
        logits = self.logits_processor(self.lm_head, hidden_states, sampling_metadata)
        return logits