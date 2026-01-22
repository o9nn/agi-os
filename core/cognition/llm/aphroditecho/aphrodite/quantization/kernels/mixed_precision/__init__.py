from typing import Optional
import aphrodite.common.envs as envs
from aphrodite.platforms import current_platform
from aphrodite.quantization.kernels.mixed_precision.allspark import AllSparkLinearKernel
from aphrodite.quantization.kernels.mixed_precision.bitblas import BitBLASLinearKernel
from aphrodite.quantization.kernels.mixed_precision.conch import ConchLinearKernel
from aphrodite.quantization.kernels.mixed_precision.dynamic_4bit import Dynamic4bitLinearKernel
from aphrodite.quantization.kernels.mixed_precision.exllama import ExllamaLinearKernel
from aphrodite.quantization.kernels.mixed_precision.machete import MacheteLinearKernel
from aphrodite.quantization.kernels.mixed_precision.marlin import MarlinLinearKernel
from aphrodite.quantization.kernels.mixed_precision.MPLinearKernel import MPLinearKernel, MPLinearLayerConfig
_POSSIBLE_KERNELS: list[type[MPLinearKernel]] = [MacheteLinearKernel, AllSparkLinearKernel, MarlinLinearKernel, Dynamic4bitLinearKernel, BitBLASLinearKernel, ConchLinearKernel, ExllamaLinearKernel]
def choose_mp_linear_kernel(config: MPLinearLayerConfig, compute_capability: Optional[int]=None) -> type[MPLinearKernel]:
    if compute_capability is None:
        if current_platform is None:
            raise ValueError('Cannot determine compute capability')
        _cc = current_platform.get_device_capability()
        if _cc is not None:
            compute_capability = _cc[0] * 10 + _cc[1]
    failure_reasons = []
    for kernel in _POSSIBLE_KERNELS:
        if kernel.__name__ in envs.APHRODITE_DISABLED_KERNELS:
            failure_reasons.append(f' {kernel.__name__} disabled by environment variable')
            continue
        if compute_capability is not None and kernel.get_min_capability() > compute_capability:
            failure_reasons.append(f'{kernel.__name__} requires capability {kernel.get_min_capability()}, current compute  capability is {compute_capability}')
            continue
        can_implement, failure_reason = kernel.can_implement(config)
        if can_implement:
            return kernel
        else:
            failure_reasons.append(f' {kernel.__name__} cannot implement due to: {failure_reason}')
    raise ValueError('Failed to find a kernel that can implement the WNA16 linear layer. Reasons: \n' + '\n'.join(failure_reasons))