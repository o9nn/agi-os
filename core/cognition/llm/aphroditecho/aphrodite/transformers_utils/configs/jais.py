from transformers.configuration_utils import PretrainedConfig
from transformers.utils import logging
logger = logging.get_logger(__name__)
class JAISConfig(PretrainedConfig):
    model_type = 'jais'
    keys_to_ignore_at_inference = ['past_key_values']
    attribute_map = {'hidden_size': 'n_embd', 'max_position_embeddings': 'n_positions', 'num_attention_heads': 'n_head', 'num_hidden_layers': 'n_layer'}
    def __init__(self, vocab_size=50257, n_positions=1024, n_embd=768, n_layer=12, n_head=12, n_inner=None, activation_function='gelu_new', resid_pdrop=0.1, embd_pdrop=0.1, attn_pdrop=0.1, layer_norm_epsilon=1e-05, initializer_range=0.02, scale_attn_weights=True, use_cache=True, bos_token_id=50256, eos_token_id=50256, scale_attn_by_inverse_layer_idx=False, reorder_and_upcast_attn=False, position_embedding_type='learned', mup_width_scale=1.0, mup_embeddings_scale=1.0, mup_output_alpha=1.0, mup_scale_qk_dot_by_d=False, alibi_scaling=None, architectures=None, **kwargs):
        self.vocab_size = vocab_size
        self.n_positions = n_positions
        self.n_embd = n_embd
        self.n_layer = n_layer
        self.n_head = n_head
        self.n_inner = n_inner
        self.activation_function = activation_function
        self.resid_pdrop = resid_pdrop
        self.embd_pdrop = embd_pdrop
        self.attn_pdrop = attn_pdrop
        self.layer_norm_epsilon = layer_norm_epsilon
        self.initializer_range = initializer_range
        self.scale_attn_weights = scale_attn_weights
        self.use_cache = use_cache
        self.scale_attn_by_inverse_layer_idx = scale_attn_by_inverse_layer_idx
        self.reorder_and_upcast_attn = reorder_and_upcast_attn
        self.bos_token_id = bos_token_id
        self.eos_token_id = eos_token_id
        self.position_embedding_type = position_embedding_type
        self.mup_width_scale = mup_width_scale
        self.mup_embeddings_scale = mup_embeddings_scale
        self.mup_output_alpha = mup_output_alpha
        self.mup_scale_qk_dot_by_d = mup_scale_qk_dot_by_d
        self.alibi_scaling = alibi_scaling
        self._alibi_scaling_validation()
        if architectures is None:
            architectures = ['JAISLMHeadModel']
        super().__init__(bos_token_id=bos_token_id, eos_token_id=eos_token_id, architectures=architectures, **kwargs)
    def _alibi_scaling_validation(self):
        if self.alibi_scaling is None:
            return
        if not isinstance(self.alibi_scaling, dict) or len(self.alibi_scaling) != 2:
            raise ValueError(f'`alibi_scaling` must be a dictionary with two fields, `type` and `factor` or `type` and `train_seq_len`, got {self.alibi_scaling}')
        alibi_scaling_type = self.alibi_scaling.get('type', None)
        alibi_scaling_factor = self.alibi_scaling.get('factor', None)
        alibi_dynamic_scaling = self.alibi_scaling.get('train_seq_len', None)
        if alibi_scaling_type is None or alibi_scaling_type != 'linear':
            raise ValueError(f"`alibi_scaling`'s type field must be 'linear', got {alibi_scaling_type}")
        if alibi_scaling_factor is not None and (not isinstance(alibi_scaling_factor, float)) or (alibi_scaling_factor is not None and alibi_scaling_factor <= 1.0):
            raise ValueError(f"`alibi_scaling`'s factor field must be a float > 1.0, got {alibi_scaling_factor}")
        if alibi_dynamic_scaling is not None and (not isinstance(alibi_dynamic_scaling, int)) or (alibi_dynamic_scaling is not None and alibi_dynamic_scaling <= 1):
            raise ValueError(f"`alibi_scaling`'s `train_seq_len` field must be an integer > 1, got {alibi_dynamic_scaling}")