from transformers import PretrainedConfig
from transformers.utils import logging
logger = logging.get_logger(__name__)
class NemotronConfig(PretrainedConfig):
    model_type = 'nemotron'
    keys_to_ignore_at_inference = ['past_key_values']
    def __init__(self, vocab_size=256000, hidden_size=6144, intermediate_size=24576, num_hidden_layers=32, num_attention_heads=48, head_dim=None, num_key_value_heads=None, hidden_act='relu2', max_position_embeddings=4096, initializer_range=0.0134, norm_eps=1e-05, use_cache=True, pad_token_id=None, bos_token_id=2, eos_token_id=3, tie_word_embeddings=False, rope_theta=10000.0, rope_scaling=None, partial_rotary_factor=0.5, attention_bias=False, attention_dropout=0.0, mlp_bias=False, **kwargs):
        self.vocab_size = vocab_size
        self.max_position_embeddings = max_position_embeddings
        self.hidden_size = hidden_size
        self.intermediate_size = intermediate_size
        self.num_hidden_layers = num_hidden_layers
        self.num_attention_heads = num_attention_heads
        head_dim = head_dim or kwargs.get('kv_channels')
        self.head_dim = head_dim if head_dim is not None else hidden_size // num_attention_heads
        if num_key_value_heads is None:
            num_key_value_heads = num_attention_heads
        self.num_key_value_heads = num_key_value_heads
        self.hidden_act = hidden_act
        self.initializer_range = initializer_range
        self.norm_eps = norm_eps
        self.use_cache = use_cache
        self.rope_theta = rope_theta
        self.rope_scaling = rope_scaling
        partial_rotary_factor = kwargs.get('rope_percent') or kwargs.get('rope_percentage') or partial_rotary_factor
        self.partial_rotary_factor = partial_rotary_factor
        self._rope_scaling_validation()
        self.attention_bias = attention_bias
        self.attention_dropout = attention_dropout
        self.mlp_bias = mlp_bias
        super().__init__(pad_token_id=pad_token_id, bos_token_id=bos_token_id, eos_token_id=eos_token_id, tie_word_embeddings=tie_word_embeddings, **kwargs)
    def _rope_scaling_validation(self):
        if self.rope_scaling is None:
            return
        if not isinstance(self.rope_scaling, dict) or len(self.rope_scaling) != 2:
            raise ValueError(f'`rope_scaling` must be a dictionary with two fields, `type` and `factor`, got {self.rope_scaling}')
        rope_scaling_type = self.rope_scaling.get('type', None)
        rope_scaling_factor = self.rope_scaling.get('factor', None)
        if rope_scaling_type is None or rope_scaling_type not in ['linear', 'dynamic']:
            raise ValueError(f"`rope_scaling`'s type field must be one of ['linear', 'dynamic'], got {rope_scaling_type}")
        if rope_scaling_factor is None or not isinstance(rope_scaling_factor, float) or rope_scaling_factor <= 1.0:
            raise ValueError(f"`rope_scaling`'s factor field must be a float > 1, got {rope_scaling_factor}")