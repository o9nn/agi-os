from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import Enum
from math import prod
from typing import Any, Optional, final
import torch
import aphrodite.common.envs as envs
from aphrodite.modeling.layers.fused_moe.config import FusedMoEQuantConfig
from aphrodite.modeling.layers.fused_moe.utils import _resize_cache, count_expert_num_tokens
from aphrodite.utils import cdiv
def _moe_problem_size(a1: torch.Tensor, w1: torch.Tensor, w2: torch.Tensor, topk_ids: torch.Tensor) -> tuple[int, int, int, int, int]:
    assert w1.dim() == 3 and w2.dim() == 3
    E, N, _ = w1.size()
    K = w2.size(1)
    if a1.dim() == 2:
        assert topk_ids.size(0) == a1.size(0), f'{topk_ids.size(0)} != {a1.size(0)}'
        M = a1.size(0)
    else:
        assert a1.dim() == 3
        assert a1.size(0) == E, f'{a1.size(0)} == {E}'
        M = a1.size(1)
    assert topk_ids.dim() == 2
    topk = topk_ids.size(1)
    return (E, M, N, K, topk)
class FusedMoEActivationFormat(Enum):
    Standard = ('standard',)
    '\n    The batched experts format (num experts, max tokens per expert, hidden dim)\n    '
    BatchedExperts = ('batched_experts',)
@dataclass
class ExpertTokensMetadata:
    expert_num_tokens: torch.Tensor
    expert_num_tokens_cpu: Optional[torch.Tensor]
    @staticmethod
    def make_from_list(expert_num_tokens_list: list[int], device: str) -> 'ExpertTokensMetadata':
        expert_num_tokens_cpu = torch.tensor(expert_num_tokens_list, device='cpu', dtype=torch.int32)
        return ExpertTokensMetadata(expert_num_tokens=expert_num_tokens_cpu.to(device, non_blocking=True), expert_num_tokens_cpu=expert_num_tokens_cpu)
class TopKWeightAndReduce(ABC):
    @abstractmethod
    def apply(self, output: Optional[torch.Tensor], fused_expert_output: torch.Tensor, topk_weights: torch.Tensor, topk_ids: torch.Tensor, apply_router_weight_on_input: bool) -> torch.Tensor:
        raise NotImplementedError
class FusedMoEPrepareAndFinalize(ABC):
    @abstractmethod
    def prepare(self, a1: torch.Tensor, a1_scale: Optional[torch.Tensor], a2_scale: Optional[torch.Tensor], topk_weights: torch.Tensor, topk_ids: torch.Tensor, num_experts: int, expert_map: Optional[torch.Tensor], apply_router_weight_on_input: bool, quant_config: FusedMoEQuantConfig, extra_prepare_args: Optional[dict[str, Any]]) -> tuple[torch.Tensor, Optional[torch.Tensor], Optional[ExpertTokensMetadata], Optional[torch.Tensor], Optional[torch.Tensor]]:
        raise NotImplementedError
    @abstractmethod
    def finalize(self, output: torch.Tensor, fused_expert_output: torch.Tensor, topk_weights: torch.Tensor, topk_ids: torch.Tensor, apply_router_weight_on_input: bool, weight_and_reduce_impl: TopKWeightAndReduce, extra_finalize_args: Optional[dict[str, Any]]) -> None:
        raise NotImplementedError
    @property
    @abstractmethod
    def activation_format(self) -> FusedMoEActivationFormat:
        raise NotImplementedError
    @abstractmethod
    def topk_indices_dtype(self) -> Optional[torch.dtype]:
        raise NotImplementedError
    @abstractmethod
    def max_num_tokens_per_rank(self) -> Optional[int]:
        raise NotImplementedError
    @abstractmethod
    def num_dispatchers(self) -> int:
        raise NotImplementedError
class FusedMoEPermuteExpertsUnpermute(ABC):
    def __init__(self, quant_config: Optional[FusedMoEQuantConfig]):
        if quant_config is not None:
            self.quant_config = quant_config
        else:
            self.quant_config = FusedMoEQuantConfig()
    @property
    @abstractmethod
    def activation_formats(self) -> tuple[FusedMoEActivationFormat, FusedMoEActivationFormat]:
        raise NotImplementedError
    @property
    def quant_dtype(self) -> Optional[torch.dtype]:
        return self.quant_config.quant_dtype
    @property
    def block_shape(self) -> Optional[list[int]]:
        return self.quant_config.block_shape
    @property
    def per_act_token_quant(self) -> bool:
        return self.quant_config.per_act_token_quant
    @property
    def per_out_ch_quant(self) -> bool:
        return self.quant_config.per_out_ch_quant
    @abstractmethod
    def supports_chunking(self) -> bool:
        raise NotImplementedError
    @abstractmethod
    def supports_expert_map(self) -> bool:
        raise NotImplementedError
    @abstractmethod
    def workspace_shapes(self, a: torch.Tensor, aq: torch.Tensor, M: int, N: int, K: int, topk: int, global_num_experts: int, local_num_experts: int, expert_tokens_meta: Optional[ExpertTokensMetadata]) -> tuple[tuple[int, ...], tuple[int, ...], tuple[int, ...], torch.dtype]:
        raise NotImplementedError
    def activation(self, activation: str, output: torch.Tensor, input: torch.Tensor) -> None:
        assert output.size(-1) * 2 == input.size(-1)
        if activation == 'silu':
            torch.ops._C.silu_and_mul(output, input)
        elif activation == 'gelu':
            torch.ops._C.gelu_and_mul(output, input)
        else:
            raise ValueError(f'Unsupported FusedMoe activation: {activation}')
    def enable_chunking(self):
        return envs.APHRODITE_ENABLE_FUSED_MOE_ACTIVATION_CHUNKING and self.supports_chunking()
    def finalize_weight_and_reduce_impl(self) -> TopKWeightAndReduce:
        raise NotImplementedError
    @abstractmethod
    def apply(self, output: torch.Tensor, hidden_states: torch.Tensor, w1: torch.Tensor, w2: torch.Tensor, topk_weights: torch.Tensor, topk_ids: torch.Tensor, activation: str, global_num_experts: int, expert_map: Optional[torch.Tensor], w1_scale: Optional[torch.Tensor], w2_scale: Optional[torch.Tensor], w1_zp: Optional[torch.Tensor], w2_zp: Optional[torch.Tensor], a1q_scale: Optional[torch.Tensor], a2_scale: Optional[torch.Tensor], workspace13: torch.Tensor, workspace2: torch.Tensor, expert_tokens_meta: Optional[ExpertTokensMetadata], apply_router_weight_on_input: bool, extra_expert_args: Optional[dict[str, Any]]):
        raise NotImplementedError
def _chunk_scales(scales: Optional[torch.Tensor], start: int, end: int) -> Optional[torch.Tensor]:
    if scales is not None:
        if scales.numel() == 1:
            return scales
        else:
            return scales[start:end]
    return None
@final
class FusedMoEModularKernel(torch.nn.Module):
    def __init__(self, prepare_finalize: FusedMoEPrepareAndFinalize, fused_experts: FusedMoEPermuteExpertsUnpermute):
        super().__init__()
        self.prepare_finalize = prepare_finalize
        self.fused_experts = fused_experts
        assert prepare_finalize.activation_format == fused_experts.activation_formats[0], f'{prepare_finalize.__class__.__name__}.{prepare_finalize.activation_format} == {fused_experts.__class__.__name__}.{fused_experts.activation_formats[0]}'
    def _do_fused_experts(self, fused_out: Optional[torch.Tensor], a1: torch.Tensor, a1q: torch.Tensor, w1: torch.Tensor, w2: torch.Tensor, topk_weights: torch.Tensor, topk_ids: torch.Tensor, activation: str, global_num_experts: int, local_num_experts: int, expert_map: Optional[torch.Tensor], w1_scale: Optional[torch.Tensor], w2_scale: Optional[torch.Tensor], w1_zp: Optional[torch.Tensor], w2_zp: Optional[torch.Tensor], a1q_scale: Optional[torch.Tensor], a2_scale: Optional[torch.Tensor], expert_tokens_meta: Optional[ExpertTokensMetadata], apply_router_weight_on_input: bool, extra_expert_args: Optional[dict[str, Any]]) -> torch.Tensor:
        _, M, N, K, top_k = _moe_problem_size(a1q, w1, w2, topk_ids)
        workspace13_shape, workspace2_shape, fused_out_shape, workspace_dtype = self.fused_experts.workspace_shapes(a1, a1q, M, N, K, top_k, global_num_experts, local_num_experts, expert_tokens_meta)
        workspace13 = torch.empty(prod(workspace13_shape), device=a1.device, dtype=workspace_dtype)
        workspace2 = torch.empty(prod(workspace2_shape), device=a1.device, dtype=workspace_dtype)
        assert fused_out is None or fused_out.shape == fused_out_shape, f'fused_out {fused_out.shape} but expected {fused_out_shape}'
        if fused_out is None:
            fused_out = _resize_cache(workspace13, fused_out_shape)
        self.fused_experts.apply(fused_out, a1q, w1, w2, topk_weights=topk_weights, topk_ids=topk_ids, activation=activation, global_num_experts=global_num_experts, expert_map=expert_map, w1_scale=w1_scale, w2_scale=w2_scale, w1_zp=w1_zp, w2_zp=w2_zp, a1q_scale=a1q_scale, a2_scale=a2_scale, workspace13=workspace13, workspace2=workspace2, expert_tokens_meta=expert_tokens_meta, apply_router_weight_on_input=apply_router_weight_on_input, extra_expert_args=extra_expert_args)
        return fused_out
    def _maybe_chunk_fused_experts(self, a1: torch.Tensor, a1q: torch.Tensor, w1: torch.Tensor, w2: torch.Tensor, topk_weights: torch.Tensor, topk_ids: torch.Tensor, activation: str, global_num_experts: int, local_num_experts: int, expert_map: Optional[torch.Tensor], w1_scale: Optional[torch.Tensor], w2_scale: Optional[torch.Tensor], w1_zp: Optional[torch.Tensor], w2_zp: Optional[torch.Tensor], a1q_scale: Optional[torch.Tensor], a2_scale: Optional[torch.Tensor], expert_tokens_meta: Optional[ExpertTokensMetadata], apply_router_weight_on_input: bool, extra_expert_args: Optional[dict[str, Any]]) -> torch.Tensor:
        _, M, N, K, top_k = _moe_problem_size(a1q, w1, w2, topk_ids)
        CHUNK_SIZE = envs.APHRODITE_FUSED_MOE_CHUNK_SIZE
        num_chunks = cdiv(M, CHUNK_SIZE)
        if not self.fused_experts.supports_chunking() or num_chunks == 1:
            return self._do_fused_experts(fused_out=None, a1=a1, a1q=a1q, w1=w1, w2=w2, topk_weights=topk_weights, topk_ids=topk_ids, activation=activation, global_num_experts=global_num_experts, local_num_experts=local_num_experts, expert_map=expert_map, w1_scale=w1_scale, w2_scale=w2_scale, w1_zp=w1_zp, w2_zp=w2_zp, a1q_scale=a1q_scale, a2_scale=a2_scale, expert_tokens_meta=expert_tokens_meta, apply_router_weight_on_input=apply_router_weight_on_input, extra_expert_args=extra_expert_args)
        assert num_chunks > 1
        _, _, fused_out_shape, _ = self.fused_experts.workspace_shapes(a1, a1q, M, N, K, top_k, global_num_experts, local_num_experts, expert_tokens_meta)
        fused_out = torch.empty(fused_out_shape, device=a1q.device, dtype=a1.dtype)
        def slice_input_tensors(chunk_idx: int) -> tuple[torch.Tensor, Optional[torch.Tensor], Optional[torch.Tensor], torch.Tensor, torch.Tensor]:
            s = chunk_idx * CHUNK_SIZE
            e = min(s + CHUNK_SIZE, M)
            return (a1q[s:e], _chunk_scales(a1q_scale, s, e), _chunk_scales(a2_scale, s, e), topk_ids[s:e], topk_weights[s:e])
        def slice_output_tensor(chunk_idx: int) -> torch.Tensor:
            assert fused_out.size(0) % M == 0, f'fused_out shape {fused_out.shape} vs M {M}'
            factor = fused_out.size(0) // M
            out_chunk_size = CHUNK_SIZE * factor
            s = chunk_idx * out_chunk_size
            e = min(s + out_chunk_size, fused_out.size(0))
            return fused_out[s:e]
        def slice_expert_tokens_metadata(full_expert_tokens_meta: ExpertTokensMetadata, chunk_topk_ids: torch.Tensor, local_num_experts: int, expert_map: Optional[torch.Tensor]) -> ExpertTokensMetadata:
            c_expert_num_tokens = count_expert_num_tokens(chunk_topk_ids, local_num_experts, expert_map)
            c_expert_num_tokens_cpu = None
            need_expert_num_tokens_cpu = full_expert_tokens_meta.expert_num_tokens_cpu is not None
            if need_expert_num_tokens_cpu:
                c_expert_num_tokens_cpu = c_expert_num_tokens.to('cpu', non_blocking=False)
            return ExpertTokensMetadata(expert_num_tokens=c_expert_num_tokens, expert_num_tokens_cpu=c_expert_num_tokens_cpu)
        m = None
        if extra_expert_args is not None and 'm' in extra_expert_args:
            m = extra_expert_args.get('m')
        if extra_expert_args is not None:
            chunked_extra_expert_args = extra_expert_args
        else:
            chunked_extra_expert_args = {}
        for chunk_idx in range(num_chunks):
            c_a1q, c_a1q_scale, c_a2_scale, c_topk_ids, c_topk_weights = slice_input_tensors(chunk_idx)
            c_expert_tokens_meta = None
            if expert_tokens_meta is not None:
                c_expert_tokens_meta = slice_expert_tokens_metadata(expert_tokens_meta, c_topk_ids, local_num_experts, expert_map)
            s = chunk_idx * CHUNK_SIZE
            e = min(s + CHUNK_SIZE, M)
            if m is not None:
                chunked_extra_expert_args['m'] = e - s
            self._do_fused_experts(fused_out=slice_output_tensor(chunk_idx), a1=a1, a1q=c_a1q, w1=w1, w2=w2, topk_weights=c_topk_weights, topk_ids=c_topk_ids, activation=activation, global_num_experts=global_num_experts, local_num_experts=local_num_experts, expert_map=expert_map, w1_scale=w1_scale, w2_scale=w2_scale, w1_zp=w1_zp, w2_zp=w2_zp, a1q_scale=c_a1q_scale, a2_scale=c_a2_scale, expert_tokens_meta=c_expert_tokens_meta, apply_router_weight_on_input=apply_router_weight_on_input, extra_expert_args=chunked_extra_expert_args)
        return fused_out
    def forward(self, hidden_states: torch.Tensor, w1: torch.Tensor, w2: torch.Tensor, topk_weights: torch.Tensor, topk_ids: torch.Tensor, inplace: bool=False, activation: str='silu', global_num_experts: int=-1, expert_map: Optional[torch.Tensor]=None, w1_scale: Optional[torch.Tensor]=None, w2_scale: Optional[torch.Tensor]=None, w1_zp: Optional[torch.Tensor]=None, w2_zp: Optional[torch.Tensor]=None, a1_scale: Optional[torch.Tensor]=None, a2_scale: Optional[torch.Tensor]=None, apply_router_weight_on_input: bool=False, extra_expert_args: Optional[dict]=None, extra_prepare_args: Optional[dict]=None, extra_finalize_args: Optional[dict]=None) -> torch.Tensor:
        a1 = hidden_states
        output = a1 if inplace else torch.zeros_like(a1)
        local_num_experts = w1.size(0)
        if global_num_experts == -1:
            global_num_experts = local_num_experts
        a1q, a1q_scale, expert_tokens_meta, _expert_topk_ids, _expert_topk_weights = self.prepare_finalize.prepare(a1, a1_scale, a2_scale, topk_weights, topk_ids, global_num_experts, expert_map, apply_router_weight_on_input, self.fused_experts.quant_config, extra_prepare_args)
        topk_ids = topk_ids if _expert_topk_ids is None else _expert_topk_ids
        topk_weights = topk_weights if _expert_topk_weights is None else _expert_topk_weights
        fused_out = None
        if a1q.numel() == 0:
            fused_out = torch.empty_like(a1q).to(dtype=a1.dtype)
        else:
            fused_out = self._maybe_chunk_fused_experts(a1=a1, a1q=a1q, w1=w1, w2=w2, topk_weights=topk_weights, topk_ids=topk_ids, activation=activation, global_num_experts=global_num_experts, local_num_experts=local_num_experts, expert_map=expert_map, w1_scale=w1_scale, w2_scale=w2_scale, w1_zp=w1_zp, w2_zp=w2_zp, a1q_scale=a1q_scale, a2_scale=a2_scale, expert_tokens_meta=expert_tokens_meta, apply_router_weight_on_input=apply_router_weight_on_input, extra_expert_args=extra_expert_args)
        self.prepare_finalize.finalize(output, fused_out, topk_weights, topk_ids, apply_router_weight_on_input, self.fused_experts.finalize_weight_and_reduce_impl(), extra_finalize_args)
        return output