from typing import Any, Optional
import torch
import aphrodite.modeling.layers.fused_moe.modular_kernel as mk
from aphrodite.common.logger import log_once
from aphrodite.modeling.layers.fused_moe.config import FusedMoEQuantConfig
from aphrodite.modeling.layers.fused_moe.topk_weight_and_reduce import TopKWeightAndReduceDelegate
from aphrodite.modeling.layers.fused_moe.utils import extract_required_args
from aphrodite.utils.flashinfer import flashinfer_cutlass_fused_moe, has_flashinfer_cutlass_fused_moe
def is_valid_flashinfer_cutlass_fused_moe(hidden_states: torch.Tensor, w1: torch.Tensor, w2: torch.Tensor) -> bool:
    if not has_flashinfer_cutlass_fused_moe():
        log_once('DEBUG', 'FlashInferExperts disabled: flashinfer_cutlass_fused_moe not available.')
        return False
    if w1.dtype != torch.uint8 or w2.dtype != torch.uint8 or hidden_states.dtype not in [torch.float32, torch.float16, torch.bfloat16]:
        log_once('DEBUG', 'FlashInferExperts disabled: w1/w2 must be torch.uint8 (got w1={}, w2={}), hidden_states must be float32, float16, or bfloat16 (got {}).', w1.dtype, w2.dtype, hidden_states.dtype)
        return False
    return True
class FlashInferExperts(mk.FusedMoEPermuteExpertsUnpermute):
    def __init__(self, use_nvfp4_w4a4: bool=False, use_fp8_w8a8: bool=False, use_dp: bool=False, ep_rank: int=0, ep_size: int=1, tp_rank: int=0, tp_size: int=1, num_dispatchers: Optional[int]=None, use_batched_format: bool=False):
        super().__init__(FusedMoEQuantConfig(quant_dtype=torch.uint8, per_act_token_quant=False, block_shape=None))
        self.use_nvfp4_w4a4 = use_nvfp4_w4a4
        self.use_fp8_w8a8 = use_fp8_w8a8
        self.ep_rank = ep_rank
        self.ep_size = ep_size
        self.tp_rank = tp_rank
        self.tp_size = tp_size
        self.use_dp = use_dp
        assert not use_batched_format or num_dispatchers is not None
        self.num_dispatchers = num_dispatchers
    @property
    def activation_formats(self) -> tuple[mk.FusedMoEActivationFormat, mk.FusedMoEActivationFormat]:
        return (mk.FusedMoEActivationFormat.Standard, mk.FusedMoEActivationFormat.Standard)
    def supports_expert_map(self) -> bool:
        return False
    def supports_chunking(self) -> bool:
        return True
    def finalize_weight_and_reduce_impl(self) -> mk.TopKWeightAndReduce:
        return TopKWeightAndReduceDelegate()
    def workspace_shapes(self, a: torch.Tensor, aq: torch.Tensor, M: int, N: int, K: int, topk: int, global_num_experts: int, local_num_experts: int, expert_tokens_meta: Optional[mk.ExpertTokensMetadata]) -> tuple[tuple[int, ...], tuple[int, ...], tuple[int, ...], torch.dtype]:
        assert self.use_nvfp4_w4a4 is True, 'Only nvfp4 quantization is currently supported.'
        aq_m, aq_n = aq.shape
        workspace2 = ()
        output_shape = (aq_m, aq_n * 2)
        workspace_dtype = a.dtype
        workspace1 = output_shape
        return (workspace1, workspace2, output_shape, workspace_dtype)
    def apply(self, output: torch.Tensor, hidden_states: torch.Tensor, w1: torch.Tensor, w2: torch.Tensor, topk_weights: torch.Tensor, topk_ids: torch.Tensor, activation: str, global_num_experts: int, expert_map: Optional[torch.Tensor], w1_scale: Optional[torch.Tensor], w2_scale: Optional[torch.Tensor], w1_zp: Optional[torch.Tensor], w2_zp: Optional[torch.Tensor], a1q_scale: Optional[torch.Tensor], a2_scale: Optional[torch.Tensor], workspace13: Optional[torch.Tensor], workspace2: Optional[torch.Tensor], expert_tokens_meta: Optional[mk.ExpertTokensMetadata], apply_router_weight_on_input: Optional[bool], extra_expert_args: Optional[dict[str, Any]]):
        assert extra_expert_args is not None, 'extra_expert_args must be provided'
        required_keys = ['g1_alphas', 'g2_alphas', 'a1_gscale', 'a2_gscale', 'out_dtype']
        g1_alphas, g2_alphas, a1_gscale, a2_gscale, out_dtype = extract_required_args(extra_expert_args, required_keys)
        assert self.use_nvfp4_w4a4 is True, 'Only nvfp4 quantization is currently supported.'
        assert w1_scale is not None and w2_scale is not None, 'w1_scale and w2_scale must not be None for FlashInferExperts'
        assert not apply_router_weight_on_input
        quant_scales = [a1_gscale, w1_scale.view(torch.int32), g1_alphas, a2_gscale, w2_scale.view(torch.int32), g2_alphas]
        _ = flashinfer_cutlass_fused_moe(input=hidden_states, token_selected_experts=topk_ids.to(torch.int), token_final_scales=topk_weights, fc1_expert_weights=w1.view(torch.long), fc2_expert_weights=w2.view(torch.long), output_dtype=out_dtype, quant_scales=quant_scales, input_sf=a1q_scale, tp_size=self.tp_size, tp_rank=self.tp_rank, ep_size=self.ep_size, ep_rank=self.ep_rank, output=output)