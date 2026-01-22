from aphrodite.common.config import AphroditeConfig
from aphrodite.modeling.models.llama import LlamaForCausalLM
from .utils import PPMissingLayer
class Ernie4_5ForCausalLM(LlamaForCausalLM):
    def __init__(self, *, aphrodite_config: AphroditeConfig, prefix: str=''):
        super().__init__(aphrodite_config=aphrodite_config, prefix=prefix)
        for layer in self.model.layers:
            if not isinstance(layer, PPMissingLayer):
                layer.self_attn.rotary_emb.is_neox_style = False
                layer.self_attn.o_proj.bias = None
                layer.self_attn.o_proj.skip_bias_add = True