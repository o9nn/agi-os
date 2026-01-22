from collections.abc import Iterable
import torch
from torch.nn import Parameter
from aphrodite.common.config import AphroditeConfig
from aphrodite.distributed import get_tensor_model_parallel_rank, get_tensor_model_parallel_world_size
from aphrodite.modeling.layers.linear import set_weight_attrs
from aphrodite.modeling.models.llama import LlamaForCausalLM
from .utils import AutoWeightsLoader, WeightsMapper
class Fairseq2LlamaForCausalLM(LlamaForCausalLM):
    def __init__(self, *, aphrodite_config: AphroditeConfig, prefix: str=''):
        super().__init__(aphrodite_config=aphrodite_config, prefix=prefix)
        self.tp_rank = get_tensor_model_parallel_rank()
        self.tp_size = get_tensor_model_parallel_world_size()
        self.allow_patterns_overrides = ['model.pt', f'model.{self.tp_rank}.pt']
    def load_weights(self, weights: Iterable[tuple[str, torch.Tensor]]) -> set[str]:
        weights_wrapped = dict(weights)
        weights = weights_wrapped[weights_wrapped['model_key']].items()
        fs2_to_aphrodite_mapper = WeightsMapper(orig_to_new_prefix={'decoder_frontend.embed.': 'model.embed_tokens.', 'decoder.': 'model.', 'final_proj.': 'lm_head.'}, orig_to_new_substr={'.self_attn_layer_norm.': '.input_layernorm.', '.ffn_layer_norm.': '.post_attention_layernorm.', '.self_attn.output_proj.': '.self_attn.o_proj.', '.ffn.gate_proj.': '.mlp.gate_proj.', '.ffn.inner_proj.': '.mlp.up_proj.', '.ffn.output_proj.': '.mlp.down_proj.', '.layer_norm.': '.norm.'})
        weights = fs2_to_aphrodite_mapper.apply(weights)
        params = dict(self.named_parameters())
        loader = AutoWeightsLoader(self, skip_prefixes=['lm_head.'] if self.config.tie_word_embeddings else None)
        return loader.load_weights((self.reshape_fairseq2_weights(name, loaded_weight, params) for name, loaded_weight in weights))
    def flag_sharded_weights(self, params: dict[str, Parameter]):
        for name, param in params.items():
            modules = name.split('.')
            if 'norm' in name and len(param.size()) < 2:
                continue
            elif any((emb in modules for emb in ['embed_tokens', 'lm_head'])):
                continue
            else:
                set_weight_attrs(param, {'is_sharded_weight': True})
    def reshape_fairseq2_weights(self, name: str, loaded_weight: torch.Tensor, params: dict[str, Parameter]) -> tuple[str, torch.Tensor]:
        def permute(w: torch.Tensor, n_heads: int) -> torch.Tensor:
            attn_in = self.config.head_dim * n_heads
            if attn_in // self.tp_size == w.size()[0]:
                attn_in //= self.tp_size
                n_heads //= self.tp_size
            attn_out = self.config.hidden_size
            return w.view(n_heads, attn_in // n_heads // 2, 2, attn_out).transpose(1, 2).reshape(attn_in, attn_out)
        modules = name.split('.')
        if 'k_proj' in modules:
            loaded_weight = permute(loaded_weight, self.config.num_key_value_heads)
        elif 'q_proj' in modules:
            loaded_weight = permute(loaded_weight, self.config.num_attention_heads)
        if any((emb in modules for emb in ['embed_tokens', 'lm_head'])):
            dim = 0
            if self.tp_size > 1 and loaded_weight.shape[dim] < self.config.vocab_size:
                assert loaded_weight.shape[dim] * self.tp_size == self.config.vocab_size, 'vocab_size should be divisible by tp_size.'
                repeats = [1] * len(loaded_weight.size())
                repeats[dim] = self.tp_size
                loaded_weight = loaded_weight.repeat(repeats)
                set_weight_attrs(params[name], {'is_sharded_weight': False})
                if 'embed_tokens' in modules:
                    self.flag_sharded_weights(params)
        return (name, loaded_weight)