from dataclasses import asdict, dataclass
from typing import Any
from transformers.configuration_utils import PretrainedConfig
from transformers.utils import logging
logger = logging.get_logger(__name__)
ARCTIC_PRETRAINED_CONFIG_ARCHIVE_MAP = {'arctic': 'https://huggingface.co/Snowflake/snowflake-arctic-instruct/tree/main/config.json'}
@dataclass
class ArcticLoRAConfig:
    lora_r: int = 64
    lora_alpha: float = 16
    shard_base_weights: bool = False
@dataclass
class ArcticQuantizationConfig:
    q_bits: int = 8
    rounding: str = 'nearest'
    mantissa_bits: int = 3
    group_size: int = 128
class ArcticConfig(PretrainedConfig):
    model_type = 'arctic'
    keys_to_ignore_at_inference = ['past_key_values']
    def __init__(self, vocab_size=32000, hidden_size=4096, intermediate_size=14336, num_hidden_layers=32, num_attention_heads=32, num_key_value_heads=None, hidden_act='silu', max_position_embeddings=4096, initializer_range=0.02, rms_norm_eps=1e-05, use_cache=True, pad_token_id=None, bos_token_id=1, eos_token_id=2, tie_word_embeddings=False, rope_theta=1000000.0, sliding_window=None, attention_dropout=0.0, num_experts_per_tok=1, num_local_experts=8, router_aux_loss_coef=0.001, moe_layer_frequency=2, parallel_attn_mlp_res=False, moe_train_capacity_factor=1, moe_eval_capacity_factor=1, enable_expert_tensor_parallelism=False, moe_min_capacity=0, moe_token_dropping=True, quantization=None, **kwargs):
        self.vocab_size = vocab_size
        self.max_position_embeddings = max_position_embeddings
        self.hidden_size = hidden_size
        self.intermediate_size = intermediate_size
        self.num_hidden_layers = num_hidden_layers
        self.num_attention_heads = num_attention_heads
        self.sliding_window = sliding_window
        if num_key_value_heads is None:
            num_key_value_heads = num_attention_heads
        self.num_key_value_heads = num_key_value_heads
        self.hidden_act = hidden_act
        self.initializer_range = initializer_range
        self.rms_norm_eps = rms_norm_eps
        self.use_cache = use_cache
        self.rope_theta = rope_theta
        self.attention_dropout = attention_dropout
        self.num_experts_per_tok = num_experts_per_tok
        self.num_local_experts = num_local_experts
        self.router_aux_loss_coef = router_aux_loss_coef
        self.moe_layer_frequency = moe_layer_frequency
        self.moe_train_capacity_factor = moe_train_capacity_factor
        self.moe_eval_capacity_factor = moe_eval_capacity_factor
        self.enable_expert_tensor_parallelism = enable_expert_tensor_parallelism
        self.moe_min_capacity = moe_min_capacity
        self.moe_token_dropping = moe_token_dropping
        self.parallel_attn_mlp_res = parallel_attn_mlp_res
        if isinstance(quantization, dict):
            self.quantization = ArcticQuantizationConfig(**quantization)
        else:
            self.quantization = quantization
        super().__init__(pad_token_id=pad_token_id, bos_token_id=bos_token_id, eos_token_id=eos_token_id, tie_word_embeddings=tie_word_embeddings, **kwargs)
    @classmethod
    def from_dict(cls, config_dict: dict[str, Any], **kwargs) -> 'ArcticConfig':
        result = super().from_dict(config_dict, **kwargs)
        config = result[0] if isinstance(result, tuple) else result
        if isinstance(config.quantization, dict):
            config.quantization = ArcticQuantizationConfig(**config.quantization)
        return result
    def to_dict(self) -> dict[str, Any]:
        ret = super().to_dict()
        if isinstance(ret['quantization'], ArcticQuantizationConfig):
            ret['quantization'] = asdict(ret['quantization'])
        return ret