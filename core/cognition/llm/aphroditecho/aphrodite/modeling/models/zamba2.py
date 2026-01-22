from collections.abc import Iterable
from itertools import cycle
from typing import Optional, Union
import torch
from torch import nn
from transformers import Zamba2Config
from aphrodite.attention.layer import Attention
from aphrodite.common import envs
from aphrodite.common.config import AphroditeConfig, CacheConfig
from aphrodite.common.sequence import IntermediateTensors
from aphrodite.compilation.decorators import support_torch_compile
from aphrodite.distributed import get_tensor_model_parallel_world_size
from aphrodite.forward_context import get_forward_context
from aphrodite.modeling.layers.activation import GeluAndMul
from aphrodite.modeling.layers.layernorm import RMSNorm
from aphrodite.modeling.layers.linear import ColumnParallelLinear, MergedColumnParallelLinear, QKVParallelLinear, ReplicatedLinear, RowParallelLinear
from aphrodite.modeling.layers.logits_processor import LogitsProcessor
from aphrodite.modeling.layers.mamba.mamba2_metadata import Mamba2Metadata, prepare_mamba2_metadata
from aphrodite.modeling.layers.mamba.mamba_mixer2 import MambaMixer2
from aphrodite.modeling.layers.mamba.mamba_utils import MambaStateShapeCalculator
from aphrodite.modeling.layers.rotary_embedding import get_rope
from aphrodite.modeling.layers.vocab_parallel_embedding import DEFAULT_VOCAB_PADDING_SIZE, ParallelLMHead, VocabParallelEmbedding
from aphrodite.modeling.model_loader.weight_utils import default_weight_loader
from aphrodite.modeling.models.mamba_cache import MambaCacheManager, MambaCacheParams
from aphrodite.modeling.sampling_metadata import SamplingMetadata
from aphrodite.quantization import QuantizationConfig
from .interfaces import HasInnerState, IsHybrid
from .utils import AutoWeightsLoader, WeightsMapper, maybe_prefix
class Zamba2LoRA(nn.Module):
    def __init__(self, input_dim: int, rank: int, output_dim: Union[int, list[int]], quant_config: Optional[QuantizationConfig]=None, prefix: str=''):
        super().__init__()
        self.A = ColumnParallelLinear(input_dim, rank, bias=False, quant_config=quant_config, gather_output=True)
        if isinstance(output_dim, list):
            B_class = MergedColumnParallelLinear
        else:
            B_class = ColumnParallelLinear
        self.B = B_class(rank, output_dim, bias=False, quant_config=quant_config)
    def forward(self, hidden_states: torch.Tensor):
        lora_output, _ = self.A(hidden_states)
        lora_output, _ = self.B(lora_output)
        return lora_output
class Zamba2Attention(nn.Module):
    def __init__(self, config: Zamba2Config, bare_block_idx: int, num_hybrid_layers: int, cache_config: Optional[CacheConfig]=None, quant_config: Optional[QuantizationConfig]=None, prefix: str='') -> None:
        super().__init__()
        tp_size = get_tensor_model_parallel_world_size()
        self.config = config
        self.num_hybrid_layers = num_hybrid_layers
        self.rope_theta = config.rope_theta
        self.attention_hidden_size = config.attention_hidden_size
        self.total_num_attention_heads = config.num_attention_heads
        assert self.total_num_attention_heads % tp_size == 0
        self.num_attention_heads = config.num_attention_heads // tp_size
        self.attention_head_dim = config.attention_head_dim
        self.qkv_size = self.attention_hidden_size // tp_size
        self.scale = (self.attention_head_dim / 2) ** (-0.5)
        if self.attention_head_dim * self.total_num_attention_heads != self.attention_hidden_size:
            raise ValueError(f'attention_hidden_size must be divisible by num_attention_heads (got `attention_hidden_size`: {self.attention_hidden_size} and `num_heads`: {self.num_attention_heads}).')
        self.qkv_proj = QKVParallelLinear(self.attention_hidden_size, self.attention_head_dim, self.total_num_attention_heads, bias=False, quant_config=quant_config)
        self.o_proj = RowParallelLinear(self.attention_hidden_size, config.hidden_size, bias=False, quant_config=quant_config)
        self.dpa_list = nn.ModuleList([])
        j = bare_block_idx * (self.num_hybrid_layers + config.num_mem_blocks - 1) // config.num_mem_blocks
        for block_idx in range(self.num_hybrid_layers):
            if block_idx % config.num_mem_blocks == bare_block_idx:
                dpa = Attention(self.num_attention_heads, self.attention_head_dim, self.scale, cache_config=cache_config, prefix=f'{prefix}.attn.{j}')
                j += 1
            else:
                dpa = nn.Identity()
            self.dpa_list.append(dpa)
        if config.use_shared_attention_adapter:
            self.linear_q_adapter_list = nn.ModuleList([])
            self.linear_k_adapter_list = nn.ModuleList([])
            self.linear_v_adapter_list = nn.ModuleList([])
            for block_idx in range(self.num_hybrid_layers):
                if block_idx % config.num_mem_blocks == bare_block_idx:
                    linear_q_adapter = Zamba2LoRA(self.attention_hidden_size, config.adapter_rank, self.attention_hidden_size, quant_config=quant_config)
                    linear_k_adapter = Zamba2LoRA(self.attention_hidden_size, config.adapter_rank, self.attention_hidden_size, quant_config=quant_config)
                    linear_v_adapter = Zamba2LoRA(self.attention_hidden_size, config.adapter_rank, self.attention_hidden_size, quant_config=quant_config)
                else:
                    linear_q_adapter = nn.Identity()
                    linear_k_adapter = nn.Identity()
                    linear_v_adapter = nn.Identity()
                self.linear_q_adapter_list.append(linear_q_adapter)
                self.linear_k_adapter_list.append(linear_k_adapter)
                self.linear_v_adapter_list.append(linear_v_adapter)
        if config.use_mem_rope:
            self.rotary_emb = get_rope(head_size=self.attention_head_dim, rotary_dim=self.attention_head_dim, max_position=config.max_position_embeddings, base=self.rope_theta, rope_scaling=None, is_neox_style=True)
    def forward(self, hidden_states: torch.Tensor, block_idx: int, position_ids: torch.Tensor) -> torch.Tensor:
        qkv, _ = self.qkv_proj(hidden_states)
        query_states, key_states, value_states = qkv.split([self.qkv_size] * 3, dim=-1)
        if self.config.use_shared_attention_adapter:
            q_adapter = self.linear_q_adapter_list[block_idx]
            assert not isinstance(q_adapter, nn.Identity)
            q_lora_output = q_adapter(hidden_states)
            query_states = query_states + q_lora_output
            k_adapter = self.linear_k_adapter_list[block_idx]
            assert not isinstance(k_adapter, nn.Identity)
            k_lora_output = k_adapter(hidden_states)
            key_states = key_states + k_lora_output
            v_adapter = self.linear_v_adapter_list[block_idx]
            assert not isinstance(v_adapter, nn.Identity)
            v_lora_output = v_adapter(hidden_states)
            value_states = value_states + v_lora_output
        if self.config.use_mem_rope:
            query_states, key_states = self.rotary_emb(position_ids, query_states, key_states)
        y = self.dpa_list[block_idx](query_states, key_states, value_states)
        y, _ = self.o_proj(y)
        return y
class Zamba2MLP(nn.Module):
    def __init__(self, config: Zamba2Config, bare_block_idx: int, num_hybrid_layers: dict[int, int], quant_config: Optional[QuantizationConfig]=None, prefix: str='') -> None:
        super().__init__()
        self.config = config
        self.tp_size = get_tensor_model_parallel_world_size()
        self.num_hybrid_layers = num_hybrid_layers
        self.hidden_size = config.hidden_size
        self.intermediate_size = config.intermediate_size
        self.gate_up_proj = MergedColumnParallelLinear(self.hidden_size, 2 * [self.intermediate_size], bias=self.config.add_bias_linear, quant_config=quant_config)
        self.down_proj = RowParallelLinear(self.intermediate_size, self.hidden_size, bias=self.config.add_bias_linear, quant_config=quant_config)
        if config.hidden_act != 'gelu':
            raise ValueError(f'Only GELU activation is supported (got `hidden_act`: {config.hidden_act})')
        self.act_fn = GeluAndMul()
        self.gate_up_proj_adapter_list = nn.ModuleList([])
        for block_idx in range(self.num_hybrid_layers):
            if block_idx % config.num_mem_blocks == bare_block_idx:
                gate_up_proj_adapter = Zamba2LoRA(config.hidden_size, config.adapter_rank, 2 * [self.intermediate_size], quant_config)
            else:
                gate_up_proj_adapter = nn.Identity()
            self.gate_up_proj_adapter_list.append(gate_up_proj_adapter)
    def forward(self, hidden_states: torch.Tensor, block_idx: int) -> torch.Tensor:
        gate_up_states, _ = self.gate_up_proj(hidden_states)
        adapter = self.gate_up_proj_adapter_list[block_idx]
        assert not isinstance(adapter, nn.Identity)
        lora_output = adapter(hidden_states)
        gate_up_states = gate_up_states + lora_output
        hidden_states = self.act_fn(gate_up_states)
        output, _ = self.down_proj(hidden_states)
        return output
class Zamba2AttentionDecoderLayer(nn.Module):
    def __init__(self, config: Zamba2Config, bare_block_idx: int, num_hybrid_layers: int, cache_config: Optional[CacheConfig]=None, quant_config: Optional[QuantizationConfig]=None, prefix: str='') -> None:
        super().__init__()
        self.self_attn = Zamba2Attention(config, bare_block_idx=bare_block_idx, num_hybrid_layers=num_hybrid_layers, cache_config=cache_config, quant_config=quant_config, prefix=prefix)
        self.feed_forward = Zamba2MLP(config, bare_block_idx=bare_block_idx, num_hybrid_layers=num_hybrid_layers, quant_config=quant_config)
        self.input_layernorm = RMSNorm(2 * config.hidden_size, eps=config.rms_norm_eps)
        self.pre_ff_layernorm = RMSNorm(config.hidden_size, eps=config.rms_norm_eps)
    def forward(self, hidden_states: torch.Tensor, original_hidden_states: torch.Tensor, block_idx: int, positions: torch.Tensor) -> torch.Tensor:
        hidden_states = torch.concatenate([hidden_states, original_hidden_states], dim=-1)
        hidden_states = self.input_layernorm(hidden_states)
        hidden_states = self.self_attn(hidden_states, position_ids=positions, block_idx=block_idx)
        hidden_states = self.pre_ff_layernorm(hidden_states)
        hidden_states = self.feed_forward(hidden_states, block_idx=block_idx)
        return hidden_states
class Zamba2MambaDecoderLayer(nn.Module):
    def __init__(self, config: Zamba2Config, quant_config: Optional[QuantizationConfig]=None, prefix: str='') -> None:
        super().__init__()
        intermediate_size = config.mamba_expand * config.hidden_size
        self.mamba = MambaMixer2(hidden_size=config.hidden_size, ssm_state_size=config.mamba_d_state, conv_kernel_size=config.mamba_d_conv, intermediate_size=intermediate_size, use_conv_bias=config.use_conv_bias, use_bias=config.add_bias_linear, n_groups=config.mamba_ngroups, num_heads=config.n_mamba_heads, head_dim=intermediate_size // config.n_mamba_heads, rms_norm_eps=config.rms_norm_eps, activation='silu', quant_config=quant_config, prefix=f'{prefix}.mixer')
        self.input_layernorm = RMSNorm(config.hidden_size, eps=config.rms_norm_eps)
    def forward(self, hidden_states: torch.Tensor, mamba_cache_params: MambaCacheParams, mamba2_metadata: Mamba2Metadata, transformer_hidden_states: Optional[torch.Tensor]=None, positions: Optional[torch.Tensor]=None, original_hidden_states: Optional[torch.Tensor]=None) -> torch.Tensor:
        residual = hidden_states
        if transformer_hidden_states is not None:
            hidden_states = hidden_states + transformer_hidden_states
        hidden_states = self.input_layernorm(hidden_states)
        output = torch.empty_like(hidden_states)
        self.mamba(hidden_states, output, mamba_cache_params=mamba_cache_params, mamba2_metadata=mamba2_metadata)
        hidden_states = residual + output
        return hidden_states
class Zamba2HybridLayer(nn.Module):
    def __init__(self, shared_transformer: Zamba2AttentionDecoderLayer, config: Zamba2Config, block_idx: int, quant_config: Optional[QuantizationConfig]=None, prefix: str='') -> None:
        super().__init__()
        self.block_idx = block_idx
        self.shared_transformer = shared_transformer
        self.linear = ReplicatedLinear(config.hidden_size, config.hidden_size, bias=False, quant_config=quant_config)
        self.mamba_decoder = Zamba2MambaDecoderLayer(config, quant_config=quant_config, prefix=prefix)
    def forward(self, hidden_states: torch.Tensor, original_hidden_states: torch.Tensor, positions: torch.Tensor, mamba_cache_params: MambaCacheParams, mamba2_metadata: Mamba2Metadata) -> torch.Tensor:
        transformer_hidden_states = self.shared_transformer(hidden_states, original_hidden_states=original_hidden_states, block_idx=self.block_idx, positions=positions)
        transformer_hidden_states, _ = self.linear(transformer_hidden_states)
        layer_outputs = self.mamba_decoder(hidden_states, transformer_hidden_states=transformer_hidden_states, mamba_cache_params=mamba_cache_params, mamba2_metadata=mamba2_metadata)
        return layer_outputs
@support_torch_compile
class Zamba2Model(nn.Module):
    def __init__(self, *, aphrodite_config: AphroditeConfig, prefix: str='') -> None:
        super().__init__()
        config = aphrodite_config.model_config.hf_config
        cache_config = aphrodite_config.cache_config
        quant_config = aphrodite_config.quant_config
        lora_config = aphrodite_config.lora_config
        is_lora_enabled = bool(lora_config)
        assert not is_lora_enabled
        self.config = config
        lora_vocab = lora_config.lora_extra_vocab_size * (lora_config.max_loras or 1) if lora_config else 0
        self.vocab_size = config.vocab_size + lora_vocab
        self.org_vocab_size = config.vocab_size
        self.embed_tokens = VocabParallelEmbedding(self.vocab_size, config.hidden_size, org_num_embeddings=config.vocab_size)
        layer2block_map = {layer_idx: block_idx for block_idx, layer_idx in enumerate(config.hybrid_layer_ids)}
        blocks = cycle([Zamba2AttentionDecoderLayer(config, bare_block_idx=idx, num_hybrid_layers=len(layer2block_map), cache_config=cache_config, quant_config=quant_config, prefix=f'{prefix}') for idx in range(config.num_mem_blocks)])
        layers = []
        for layer_idx, layer_type in enumerate(config.layers_block_type):
            prefix = str(len(layer2block_map) + layer_idx)
            if layer_type == 'hybrid':
                block = next(blocks)
                block_idx = layer2block_map[layer_idx]
                layers.append(Zamba2HybridLayer(block, config, block_idx, quant_config, prefix=prefix))
            else:
                layers.append(Zamba2MambaDecoderLayer(config, quant_config=quant_config, prefix=prefix))
        self.layers = nn.ModuleList(layers)
        self.final_layernorm = RMSNorm(config.hidden_size, eps=config.rms_norm_eps)
    def get_input_embeddings(self, input_ids: torch.Tensor) -> torch.Tensor:
        return self.embed_tokens(input_ids)
    def forward(self, input_ids: torch.Tensor, positions: torch.Tensor, mamba_cache_params: MambaCacheParams, inputs_embeds: Optional[torch.Tensor]=None) -> Union[torch.Tensor, IntermediateTensors]:
        if inputs_embeds is None:
            inputs_embeds = self.get_input_embeddings(input_ids)
        hidden_states = inputs_embeds
        attn_metadata = get_forward_context().attn_metadata
        if not envs.APHRODITE_USE_V1:
            mamba2_metadata = prepare_mamba2_metadata(chunk_size=self.config.chunk_size, attn_metadata=attn_metadata)
        else:
            mamba2_metadata = None
        original_hidden_states = torch.clone(hidden_states)
        for layer_idx, layer in enumerate(self.layers):
            layer_mamba_cache_params = None
            if isinstance(layer, (Zamba2HybridLayer, Zamba2MambaDecoderLayer)) and mamba_cache_params:
                layer_mamba_cache_params = mamba_cache_params.at_layer_idx(layer_idx)
            layer_outputs = layer(hidden_states, original_hidden_states=original_hidden_states, positions=positions, mamba_cache_params=layer_mamba_cache_params, mamba2_metadata=mamba2_metadata)
            hidden_states = layer_outputs
        hidden_states = self.final_layernorm(hidden_states)
        return hidden_states
    def load_weights(self, weights: Iterable[tuple[str, torch.Tensor]]) -> set[str]:
        stacked_params_mapping = [('qkv_proj', 'q_proj', 'q'), ('qkv_proj', 'k_proj', 'k'), ('qkv_proj', 'v_proj', 'v')]
        params_dict = dict(self.named_parameters())
        loaded_params: set[str] = set()
        for chkpt_weight_name, loaded_weight in weights:
            for param_name, weight_name, shard_id in stacked_params_mapping:
                if weight_name not in chkpt_weight_name:
                    continue
                chkpt_weight_name = chkpt_weight_name.replace(weight_name, param_name)
                param = params_dict[chkpt_weight_name]
                weight_loader = param.weight_loader
                weight_loader(param, loaded_weight, shard_id)
                break
            else:
                if chkpt_weight_name not in params_dict:
                    continue
                param = params_dict[chkpt_weight_name]
                weight_loader = getattr(param, 'weight_loader', default_weight_loader)
                weight_loader(param, loaded_weight)
            loaded_params.add(chkpt_weight_name)
        return loaded_params
class Zamba2ForCausalLM(nn.Module, HasInnerState, IsHybrid):
    hf_to_aphrodite_mapper = WeightsMapper(orig_to_new_substr={'A_log': 'A', '0.weight': 'A.weight', '1.weight': 'B.weight'})
    @classmethod
    def get_mamba_state_shape_from_config(cls, aphrodite_config: 'AphroditeConfig', use_v1: bool=True) -> tuple[tuple[int, int], tuple[int, int, int]]:
        parallel_config = aphrodite_config.parallel_config
        hf_config = aphrodite_config.model_config.hf_config
        intermediate_size = hf_config.mamba_expand * hf_config.hidden_size
        return MambaStateShapeCalculator.mamba2_state_shape(intermediate_size=intermediate_size, tp_world_size=parallel_config.tensor_parallel_size, n_groups=hf_config.mamba_ngroups, num_heads=hf_config.n_mamba_heads, head_dim=hf_config.mamba_headdim, state_size=hf_config.mamba_d_state, conv_kernel=hf_config.mamba_d_conv, use_v1=use_v1)
    def __init__(self, *, aphrodite_config: AphroditeConfig, prefix: str='') -> None:
        config = aphrodite_config.model_config.hf_config
        cache_config = aphrodite_config.cache_config
        lora_config = aphrodite_config.lora_config
        scheduler_config = aphrodite_config.scheduler_config
        assert not cache_config.enable_prefix_caching, 'Mamba does not support prefix caching'
        super().__init__()
        self.config = config
        self.aphrodite_config = aphrodite_config
        self.scheduler_config = scheduler_config
        self.model_config = aphrodite_config.model_config
        self.unpadded_vocab_size = config.vocab_size
        if lora_config:
            self.unpadded_vocab_size += lora_config.lora_extra_vocab_size
        self.model = Zamba2Model(aphrodite_config=aphrodite_config, prefix=maybe_prefix(prefix, 'model'))
        self.lm_head = ParallelLMHead(self.unpadded_vocab_size, config.hidden_size, org_num_embeddings=config.vocab_size, padding_size=DEFAULT_VOCAB_PADDING_SIZE if not lora_config else lora_config.lora_vocab_padding_size)
        self.lm_head = self.lm_head.tie_weights(self.model.embed_tokens)
        self.mamba_cache: Optional[MambaCacheManager] = None
        self.logits_processor = LogitsProcessor(self.unpadded_vocab_size, config.vocab_size)
    def get_input_embeddings(self, input_ids: torch.Tensor) -> torch.Tensor:
        return self.model.get_input_embeddings(input_ids)
    def forward(self, input_ids: torch.Tensor, positions: torch.Tensor, inputs_embeds: Optional[torch.Tensor]=None, **kwargs) -> torch.Tensor:
        mamba_cache_params = None
        if not envs.APHRODITE_USE_V1:
            if self.mamba_cache is None:
                num_mamba_layers = self.config.num_hidden_layers
                mamba_state_shape = self.get_mamba_state_shape_from_config(self.aphrodite_config, use_v1=False)
                self.mamba_cache = MambaCacheManager(self.aphrodite_config, self.lm_head.weight.dtype, num_mamba_layers, *mamba_state_shape)
            mamba_cache_params = self.mamba_cache.current_run_tensors(**kwargs)
        hidden_states = self.model(input_ids, positions, mamba_cache_params, inputs_embeds)
        return hidden_states
    def copy_inputs_before_cuda_graphs(self, input_buffers: dict[str, torch.Tensor], **kwargs) -> dict[str, torch.Tensor]:
        return self.mamba_cache.copy_inputs_before_cuda_graphs(input_buffers, **kwargs)
    def get_seqlen_agnostic_capture_inputs(self, batch_size: int) -> dict[str, torch.Tensor]:
        return self.mamba_cache.get_seqlen_agnostic_capture_inputs(batch_size)
    def compute_logits(self, hidden_states: torch.Tensor, sampling_metadata: SamplingMetadata) -> Optional[torch.Tensor]:
        logits = self.logits_processor(self.lm_head, hidden_states, sampling_metadata)
        return logits
    def load_weights(self, weights: Iterable[tuple[str, torch.Tensor]]) -> set[str]:
        loader = AutoWeightsLoader(self)
        return loader.load_weights(weights, mapper=self.hf_to_aphrodite_mapper)