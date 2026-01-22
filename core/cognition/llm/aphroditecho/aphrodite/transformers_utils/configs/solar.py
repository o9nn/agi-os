from transformers import PretrainedConfig
from transformers.utils import logging
logger = logging.get_logger(__name__)
class SolarConfig(PretrainedConfig):
    model_type = 'solar'
    keys_to_ignore_at_inference = ['past_key_values']
    def __init__(self, vocab_size=32000, hidden_size=4096, intermediate_size=11008, num_hidden_layers=32, num_attention_heads=32, num_key_value_heads=None, hidden_act='silu', max_position_embeddings=2048, initializer_range=0.02, rms_norm_eps=1e-06, use_cache=True, pad_token_id=None, bos_token_id=1, eos_token_id=2, pretraining_tp=1, tie_word_embeddings=False, rope_theta=10000.0, rope_scaling=None, attention_bias=False, attention_dropout=0.0, mlp_bias=False, sliding_window=2047, bskcn_1=None, bskcn_2=None, bskcn_3=None, bskcn_4=None, bskcn_tv=None, **kwargs):
        self.vocab_size = vocab_size
        self.max_position_embeddings = max_position_embeddings
        self.hidden_size = hidden_size
        self.intermediate_size = intermediate_size
        self.num_hidden_layers = num_hidden_layers
        self.num_attention_heads = num_attention_heads
        if num_key_value_heads is None:
            num_key_value_heads = num_attention_heads
        self.num_key_value_heads = num_key_value_heads
        self.hidden_act = hidden_act
        self.initializer_range = initializer_range
        self.rms_norm_eps = rms_norm_eps
        self.pretraining_tp = pretraining_tp
        self.use_cache = use_cache
        self.rope_theta = rope_theta
        self.rope_scaling = rope_scaling
        self._rope_scaling_validation()
        self.attention_bias = attention_bias
        self.attention_dropout = attention_dropout
        self.mlp_bias = mlp_bias
        self.sliding_window = sliding_window
        self.bskcn_1 = bskcn_1 if bskcn_1 is not None else [12, 20, 32, 44]
        self.bskcn_2 = bskcn_2 if bskcn_2 is not None else [20, 32]
        self.bskcn_3 = bskcn_3 if bskcn_3 is not None else [16, 24, 36, 48]
        self.bskcn_4 = bskcn_4 if bskcn_4 is not None else [28, 40]
        self.bskcn_tv = bskcn_tv if bskcn_tv is not None else [0.9, 0.8]
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