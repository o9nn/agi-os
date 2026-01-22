from typing import Dict
from transformers.configuration_utils import PretrainedConfig
from transformers.utils import logging
logger = logging.get_logger(__name__)
EXAONE_PRETRAINED_CONFIG_ARCHIVE_MAP: Dict[str, str] = {}
class ExaoneConfig(PretrainedConfig):
    model_type = 'exaone'
    keys_to_ignore_at_inference = ['past_key_values']
    attribute_map = {'num_hidden_layers': 'num_layers'}
    def __init__(self, vocab_size=102400, max_position_embeddings=2048, hidden_size=2048, num_layers=32, num_attention_heads=32, num_key_value_heads=None, intermediate_size=None, activation_function='silu', rotary_pct=0.25, resid_dropout=0.0, embed_dropout=0.0, attention_dropout=0.0, layer_norm_epsilon=1e-06, initializer_range=0.02, use_cache=True, bos_token_id=0, eos_token_id=2, tie_word_embeddings=True, **kwargs):
        super().__init__(bos_token_id=bos_token_id, eos_token_id=eos_token_id, tie_word_embeddings=tie_word_embeddings, **kwargs)
        self.vocab_size = vocab_size
        self.max_position_embeddings = max_position_embeddings
        self.hidden_size = hidden_size
        self.num_layers = num_layers
        self.num_attention_heads = num_attention_heads
        self.num_hidden_layers = num_layers
        if num_key_value_heads is None:
            num_key_value_heads = num_attention_heads
        self.num_key_value_heads = num_key_value_heads
        if intermediate_size:
            self.intermediate_size = intermediate_size
        else:
            self.intermediate_size = hidden_size * 4
        self.activation_function = activation_function
        self.resid_dropout = resid_dropout
        self.embed_dropout = embed_dropout
        self.attention_dropout = attention_dropout
        self.layer_norm_epsilon = layer_norm_epsilon
        self.initializer_range = initializer_range
        self.use_cache = use_cache
        self.rotary_pct = rotary_pct
        self.bos_token_id = bos_token_id
        self.eos_token_id = eos_token_id
        self.use_logit_cap = kwargs.pop('use_logit_cap', False)
        self.ln_no_scale = kwargs.pop('ln_no_scale', False)
        self.use_gated = kwargs.pop('use_gated', False)
        self.use_emb_norm = kwargs.pop('use_emb_norm', False)
        self.use_rotary_pos = kwargs.pop('use_rotary_pos', False)
        self.rotary_type = kwargs.pop('rotary_type', None)
        self.scaling_factor = kwargs.pop('scaling_factor', 1)
        self.use_absolute_pos = kwargs.pop('use_absolute_pos', True)
        self.use_extra_logit = kwargs.pop('use_extra_logit', True)
        self.rotary_expand_length = kwargs.pop('rotary_expand_length', None)
        self.rotary_base = kwargs.pop('rotary_base', 10000.0)
        self.use_qkv_fuse = kwargs.pop('use_qkv_fuse', False)
        self.rescale_before_lm_head = kwargs.pop('rescale_before_lm_head', rotary_pct == 0.25)
        if self.use_rotary_pos:
            self.use_absolute_pos = False