import torch
from loguru import logger
from torch._higher_order_ops.auto_functionalize import auto_functionalized
from torch._inductor.pattern_matcher import PatternMatcherPass, fwd_only, register_replacement
from aphrodite.common.config import AphroditeConfig
from aphrodite.platforms import current_platform
from .aphrodite_inductor_pass import AphroditeInductorPass
def silu_mul_pattern_static(result: torch.Tensor, result_silu_mul: torch.Tensor, input: torch.Tensor, scale: torch.Tensor):
    at1 = auto_functionalized(torch.ops._C.silu_and_mul.default, result=result_silu_mul, input=input)
    at2 = auto_functionalized(torch.ops._C.static_scaled_fp8_quant.default, result=result, input=at1[1], scale=scale)
    return at2[1]
def silu_mul_replacement_static(result: torch.Tensor, result_silu_mul: torch.Tensor, input: torch.Tensor, scale: torch.Tensor):
    at = auto_functionalized(torch.ops._C.silu_and_mul_quant.default, result=result, input=input, scale=scale)
    return at[1]
def empty_bf16(*args, **kwargs):
    return torch.empty(*args, **kwargs, dtype=torch.bfloat16, device='cuda')
def empty_fp8(*args, **kwargs):
    fp8 = current_platform.fp8_dtype()
    return torch.empty(*args, **kwargs, dtype=fp8, device='cuda')
def empty_fp32(*args, **kwargs):
    return torch.empty(*args, **kwargs, dtype=torch.float32, device='cuda')
class ActivationQuantFusionPass(AphroditeInductorPass):
    def __init__(self, config: AphroditeConfig):
        super().__init__(config)
        self.patterns: PatternMatcherPass = PatternMatcherPass(pass_name='activation_quant_fusion_pass')
        inputs = [empty_fp8(5, 4), empty_bf16(5, 4), empty_bf16(5, 4), empty_fp32(1, 1)]
        register_replacement(silu_mul_pattern_static, silu_mul_replacement_static, inputs, fwd_only, self.patterns)
    def __call__(self, graph: torch.fx.Graph):
        self.begin()
        self.dump_graph(graph, 'before_act_quant_fusion')
        count = self.patterns.apply(graph)
        logger.debug('Replaced {} patterns in ActivationQuantFusionPass', count)
        self.dump_graph(graph, 'after_act_quant_fusion')
        self.end_and_log()