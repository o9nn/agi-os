import os
from typing import Optional
from aphrodite.platforms import PlatformEnum, current_platform
from aphrodite.quantization.kernels.scaled_mm.aiter import AiterScaledMMLinearKernel
from aphrodite.quantization.kernels.scaled_mm.cutlass import CutlassScaledMMLinearKernel
from aphrodite.quantization.kernels.scaled_mm.ScaledMMLinearKernel import ScaledMMLinearKernel, ScaledMMLinearLayerConfig
from aphrodite.quantization.kernels.scaled_mm.triton import TritonScaledMMLinearKernel
from aphrodite.quantization.kernels.scaled_mm.xla import XLAScaledMMLinearKernel
_POSSIBLE_KERNELS: dict[PlatformEnum, list[type[ScaledMMLinearKernel]]] = {PlatformEnum.CPU: [CutlassScaledMMLinearKernel], PlatformEnum.CUDA: [CutlassScaledMMLinearKernel], PlatformEnum.ROCM: [AiterScaledMMLinearKernel, TritonScaledMMLinearKernel], PlatformEnum.TPU: [XLAScaledMMLinearKernel]}
def choose_scaled_mm_linear_kernel(config: ScaledMMLinearLayerConfig, compute_capability: Optional[int]=None) -> type[ScaledMMLinearKernel]:
    if compute_capability is None:
        _cc = current_platform.get_device_capability()
        if _cc is not None:
            compute_capability = _cc[0] * 10 + _cc[1]
    failure_reasons = []
    for kernel in _POSSIBLE_KERNELS[current_platform._enum]:
        if kernel.__name__ in os.environ.get('APHRODITE_DISABLED_KERNELS', '').split(','):
            failure_reasons.append(f' {kernel.__name__} disabled by environment variable')
            continue
        if compute_capability is not None:
            kernel_min_capability = kernel.get_min_capability()
            if kernel_min_capability is not None and kernel_min_capability > compute_capability:
                failure_reasons.append(f'{kernel.__name__} requires capability {kernel_min_capability}, current compute capability is {compute_capability}')
                continue
        can_implement, failure_reason = kernel.can_implement(config)
        if can_implement:
            return kernel
        else:
            failure_reasons.append(f' {kernel.__name__} cannot implement due to: {failure_reason}')
    raise ValueError('Failed to find a kernel that can implement the ScaledMM linear layer. Reasons: \n' + '\n'.join(failure_reasons))