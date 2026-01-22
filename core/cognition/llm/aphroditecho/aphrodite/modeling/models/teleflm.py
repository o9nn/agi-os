import torch
import torch.nn as nn
from aphrodite.common.config import AphroditeConfig
from aphrodite.modeling.layers.logits_processor import LogitsProcessor
from aphrodite.modeling.models.llama import LlamaDecoderLayer, LlamaForCausalLM, LlamaModel
class TeleFLMModel(LlamaModel):
    def __init__(self, *, aphrodite_config: AphroditeConfig, prefix: str='', layer_type: type[nn.Module]=LlamaDecoderLayer):
        super().__init__(aphrodite_config=aphrodite_config, prefix=prefix, layer_type=layer_type)
        '\n        This implementation is based on the µScaling paper presented at  \n        the ICLR 2025 Workshop:  \n        NanoLM: An Affordable LLM Study Benchmark         via Accurate Loss Prediction across Scales\n        by Yiqun Yao et al.  \n        Available at: https://openreview.net/forum?id=IwaPYg1SCA  \n        arXiv preprint: https://arxiv.org/abs/2304.06875\n        '
        self.use_mup = self.config.use_mup
        if self.use_mup:
            self.input_mult = self.config.input_mult
    def get_input_embeddings(self, input_ids: torch.Tensor) -> torch.Tensor:
        embedding = self.embed_tokens(input_ids)
        if self.use_mup:
            embedding = embedding * self.input_mult
        return embedding
class TeleFLMForCausalLM(LlamaForCausalLM):
    def __init__(self, *, aphrodite_config: AphroditeConfig, prefix: str=''):
        super().__init__(aphrodite_config=aphrodite_config, prefix=prefix)
        self.use_mup = self.config.use_mup
        if self.use_mup:
            self.mup_scale_factor = self.config.mup_scale_factor
            self.output_mult = self.config.output_mult / self.mup_scale_factor
            logit_scale = self.output_mult
            self.logits_processor = LogitsProcessor(self.unpadded_vocab_size, self.config.vocab_size, logit_scale)