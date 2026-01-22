import enum
import os
import platform
import random
import sys
from datetime import timedelta
from platform import uname
from typing import TYPE_CHECKING, NamedTuple, Optional, Union
import numpy as np
import torch
from loguru import logger
from torch.distributed import PrefixStore, ProcessGroup
from aphrodite.inputs import ProcessorInputs, PromptType
if TYPE_CHECKING:
    from aphrodite.common.config import AphroditeConfig, ModelConfig
    from aphrodite.common.pooling_params import PoolingParams
    from aphrodite.common.sampling_params import SamplingParams
    from aphrodite.lora.request import LoRARequest
    from aphrodite.utils import FlexibleArgumentParser
else:
    ModelConfig = None
    AphroditeConfig = None
    LoRARequest = None
    PoolingParams = None
    SamplingParams = None
    FlexibleArgumentParser = None
def in_wsl() -> bool:
    return 'microsoft' in ' '.join(uname()).lower()
class _Backend(enum.Enum):
    FLASH_ATTN = enum.auto()
    FLASH_ATTN_APHRODITE_V1 = enum.auto()
    TRITON_ATTN_APHRODITE_V1 = enum.auto()
    XFORMERS = enum.auto()
    ROCM_FLASH = enum.auto()
    ROCM_AITER_MLA = enum.auto()
    ROCM_AITER_MLA_APHRODITE_V1 = enum.auto()
    ROCM_AITER_FA = enum.auto()
    TORCH_SDPA = enum.auto()
    FLASHINFER = enum.auto()
    FLASHINFER_APHRODITE_V1 = enum.auto()
    TRITON_MLA = enum.auto()
    TRITON_MLA_APHRODITE_V1 = enum.auto()
    FLASHMLA_APHRODITE_V1 = enum.auto()
    FLASHMLA = enum.auto()
    CUTLASS_MLA = enum.auto()
    PALLAS = enum.auto()
    PALLAS_APHRODITE_V1 = enum.auto()
    IPEX = enum.auto()
    DUAL_CHUNK_FLASH_ATTN = enum.auto()
    DIFFERENTIAL_FLASH_ATTN = enum.auto()
    NO_ATTENTION = enum.auto()
    FLEX_ATTENTION = enum.auto()
    TREE_ATTN = enum.auto()
    XFORMERS_APHRODITE_V1 = enum.auto()
class PlatformEnum(enum.Enum):
    CUDA = enum.auto()
    ROCM = enum.auto()
    TPU = enum.auto()
    XPU = enum.auto()
    CPU = enum.auto()
    NEURON = enum.auto()
    OOT = enum.auto()
    UNSPECIFIED = enum.auto()
class CpuArchEnum(enum.Enum):
    X86 = enum.auto()
    ARM = enum.auto()
    POWERPC = enum.auto()
    OTHER = enum.auto()
    UNKNOWN = enum.auto()
class DeviceCapability(NamedTuple):
    major: int
    minor: int
    def as_version_str(self) -> str:
        return f'{self.major}.{self.minor}'
    def to_int(self) -> int:
        assert 0 <= self.minor < 10
        return self.major * 10 + self.minor
class Platform:
    _enum: PlatformEnum
    device_name: str
    device_type: str
    dispatch_key: str = 'CPU'
    ray_device_key: str = ''
    device_control_env_var: str = 'APHRODITE_DEVICE_CONTROL_ENV_VAR_PLACEHOLDER'
    simple_compile_backend: str = 'inductor'
    dist_backend: str = ''
    supported_quantization: list[str] = []
    additional_env_vars: list[str] = []
    @property
    def supported_dtypes(self) -> list[torch.dtype]:
        return [torch.bfloat16, torch.float16, torch.float32]
    def is_cuda(self) -> bool:
        return self._enum == PlatformEnum.CUDA
    def is_rocm(self) -> bool:
        return self._enum == PlatformEnum.ROCM
    def is_tpu(self) -> bool:
        return self._enum == PlatformEnum.TPU
    def is_xpu(self) -> bool:
        return self._enum == PlatformEnum.XPU
    def is_cpu(self) -> bool:
        return self._enum == PlatformEnum.CPU
    def is_neuron(self) -> bool:
        return self._enum == PlatformEnum.NEURON
    def is_out_of_tree(self) -> bool:
        return self._enum == PlatformEnum.OOT
    def get_max_output_tokens(self, prompt_len: int) -> int:
        return sys.maxsize
    def is_cuda_alike(self) -> bool:
        return self._enum in (PlatformEnum.CUDA, PlatformEnum.ROCM)
    def is_sleep_mode_available(self) -> bool:
        return self._enum == PlatformEnum.CUDA
    @classmethod
    def device_id_to_physical_device_id(cls, device_id: int):
        if cls.device_control_env_var in os.environ and os.environ[cls.device_control_env_var] != '':
            device_ids = os.environ[cls.device_control_env_var].split(',')
            physical_device_id = device_ids[device_id]
            return int(physical_device_id)
        else:
            return device_id
    @classmethod
    def get_vit_attn_backend(cls, support_fa: bool=False) -> _Backend:
        return _Backend.TORCH_SDPA
    @classmethod
    def get_attn_backend_cls(cls, selected_backend: _Backend, head_size: int, dtype: torch.dtype, kv_cache_dtype: Optional[str], block_size: int, use_v1: bool, use_mla: bool) -> str:
        return ''
    @classmethod
    def get_device_capability(cls, device_id: int=0) -> Optional[DeviceCapability]:
        return None
    @classmethod
    def has_device_capability(cls, capability: Union[tuple[int, int], int], device_id: int=0) -> bool:
        current_capability = cls.get_device_capability(device_id=device_id)
        if current_capability is None:
            return False
        if isinstance(capability, tuple):
            return current_capability >= capability
        return current_capability.to_int() >= capability
    @classmethod
    def is_device_capability(cls, capability: Union[tuple[int, int], int], device_id: int=0) -> bool:
        current_capability = cls.get_device_capability(device_id=device_id)
        if current_capability is None:
            return False
        if isinstance(capability, tuple):
            return current_capability == capability
        return current_capability.to_int() == capability
    @classmethod
    def get_device_name(cls, device_id: int=0) -> str:
        raise NotImplementedError
    @classmethod
    def get_device_uuid(cls, device_id: int=0) -> str:
        raise NotImplementedError
    @classmethod
    def get_device_total_memory(cls, device_id: int=0) -> int:
        raise NotImplementedError
    @classmethod
    def is_async_output_supported(cls, enforce_eager: Optional[bool]) -> bool:
        raise NotImplementedError
    @classmethod
    def inference_mode(cls):
        return torch.inference_mode(mode=True)
    @classmethod
    def seed_everything(cls, seed: Optional[int]=None) -> None:
        if seed is not None:
            random.seed(seed)
            np.random.seed(seed)
            torch.manual_seed(seed)
    @classmethod
    def set_device(cls, device: torch.device) -> None:
        raise NotImplementedError
    @classmethod
    def pre_register_and_update(cls, parser: Optional[FlexibleArgumentParser]=None) -> None:
        pass
    @classmethod
    def check_and_update_config(cls, aphrodite_config: AphroditeConfig) -> None:
        pass
    @classmethod
    def verify_model_arch(cls, model_arch: str) -> None:
        pass
    @classmethod
    def verify_quantization(cls, quant: str) -> None:
        if cls.supported_quantization and quant not in cls.supported_quantization:
            raise ValueError(f'{quant} quantization is currently not supported in {cls.device_name}.')
    @classmethod
    def get_cpu_architecture(cls) -> CpuArchEnum:
        machine = platform.machine().lower()
        if machine in ('x86_64', 'amd64', 'i386', 'i686'):
            return CpuArchEnum.X86
        elif machine.startswith('arm') or machine.startswith('aarch'):
            return CpuArchEnum.ARM
        elif machine.startswith('ppc'):
            return CpuArchEnum.POWERPC
        return CpuArchEnum.OTHER if machine else CpuArchEnum.UNKNOWN
    @classmethod
    def is_pin_memory_available(cls) -> bool:
        if in_wsl():
            logger.warning("Using 'pin_memory=False' as WSL is detected. This may slow down the performance.")
            return False
        return True
    @classmethod
    def get_current_memory_usage(cls, device: Optional[torch.types.Device]=None) -> float:
        raise NotImplementedError
    @classmethod
    def get_punica_wrapper(cls) -> str:
        raise NotImplementedError
    @classmethod
    def get_infinity_values(cls, dtype: torch.dtype) -> tuple[float, float]:
        return (float('-inf'), float('inf'))
    @classmethod
    def can_update_inplace(cls) -> bool:
        return True
    @classmethod
    def get_lora_vocab_padding_size(cls) -> int:
        return 256
    @classmethod
    def get_device_communicator_cls(cls) -> str:
        return 'aphrodite.distributed.device_communicators.base_device_communicator.DeviceCommunicatorBase'
    @classmethod
    def supports_mx(cls) -> bool:
        return False
    @classmethod
    def supports_fp8(cls) -> bool:
        return False
    @classmethod
    def is_fp8_fnuz(cls) -> bool:
        return False
    @classmethod
    def fp8_dtype(cls) -> torch.dtype:
        return torch.float8_e4m3fn
    @classmethod
    def use_all_gather(cls) -> bool:
        import aphrodite.common.envs as envs
        from aphrodite.common.config import get_current_aphrodite_config
        parallel_config = get_current_aphrodite_config().parallel_config
        return envs.APHRODITE_USE_V1 or parallel_config.distributed_executor_backend == 'external_launcher'
    @classmethod
    def supports_v1(cls, model_config: ModelConfig) -> bool:
        return False
    @classmethod
    def default_v1(cls, model_config: ModelConfig) -> bool:
        return cls.supports_v1(model_config)
    @classmethod
    def use_custom_allreduce(cls) -> bool:
        return False
    @classmethod
    def validate_request(cls, prompt: PromptType, params: Union[SamplingParams, PoolingParams], processed_inputs: ProcessorInputs) -> None:
    def __getattr__(self, key: str):
        device = getattr(torch, self.device_type, None)
        if device is not None and hasattr(device, key):
            return getattr(device, key)
        else:
            logger.warning("Current platform {} does not have '{}' attribute.", self.device_type, key)
            return None
    @classmethod
    def get_cu_count(cls, device_id: int=0) -> int:
        raise NotImplementedError
    @classmethod
    def get_piecewise_backend_cls(cls) -> str:
        return 'aphrodite.compilation.base_piecewise_backend.AbstractPiecewiseBackend'
    @classmethod
    def stateless_init_device_torch_dist_pg(cls, backend: str, prefix_store: PrefixStore, group_rank: int, group_size: int, timeout: timedelta) -> ProcessGroup:
        raise RuntimeError(f'Unsupported torch distributed backend: {backend}')
    @classmethod
    def is_kv_cache_dtype_supported(cls, kv_cache_dtype: str) -> bool:
        return False
class UnspecifiedPlatform(Platform):
    _enum = PlatformEnum.UNSPECIFIED
    device_type = ''