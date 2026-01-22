import hashlib
import os
import sys
import tempfile
from typing import TYPE_CHECKING, Any, Callable, Optional
if TYPE_CHECKING:
    APHRODITE_HOST_IP: str = ''
    APHRODITE_PORT: Optional[int] = None
    APHRODITE_RPC_BASE_PATH: str = tempfile.gettempdir()
    APHRODITE_USE_MODELSCOPE: bool = False
    APHRODITE_RINGBUFFER_WARNING_INTERVAL: int = 60
    APHRODITE_NCCL_SO_PATH: Optional[str] = None
    LD_LIBRARY_PATH: Optional[str] = None
    APHRODITE_USE_TRITON_FLASH_ATTN: bool = True
    APHRODITE_V1_USE_PREFILL_DECODE_ATTENTION: bool = False
    APHRODITE_USE_AITER_UNIFIED_ATTENTION: bool = False
    APHRODITE_FLASH_ATTN_VERSION: Optional[int] = None
    LOCAL_RANK: int = 0
    CUDA_VISIBLE_DEVICES: Optional[str] = None
    APHRODITE_ENGINE_ITERATION_TIMEOUT_S: int = 60
    APHRODITE_API_KEY: Optional[str] = None
    S3_ACCESS_KEY_ID: Optional[str] = None
    S3_SECRET_ACCESS_KEY: Optional[str] = None
    S3_ENDPOINT_URL: Optional[str] = None
    APHRODITE_MODEL_REDIRECT_PATH: Optional[str] = None
    APHRODITE_CACHE_ROOT: str = os.path.expanduser('~/.cache/aphrodite')
    APHRODITE_CONFIG_ROOT: str = os.path.expanduser('~/.config/aphrodite')
    APHRODITE_USAGE_STATS_SERVER: str = ''
    APHRODITE_NO_USAGE_STATS: bool = False
    APHRODITE_DO_NOT_TRACK: bool = False
    APHRODITE_USAGE_SOURCE: str = ''
    APHRODITE_CONFIGURE_LOGGING: int = 1
    APHRODITE_LOGGING_LEVEL: str = 'INFO'
    APHRODITE_LOGGING_PREFIX: str = ''
    APHRODITE_LOGGING_CONFIG_PATH: Optional[str] = None
    APHRODITE_LOGITS_PROCESSOR_THREADS: Optional[int] = None
    APHRODITE_TRACE_FUNCTION: int = 0
    APHRODITE_ATTENTION_BACKEND: Optional[str] = None
    APHRODITE_USE_FLASHINFER_SAMPLER: Optional[bool] = None
    APHRODITE_FLASHINFER_FORCE_TENSOR_CORES: bool = False
    APHRODITE_PP_LAYER_PARTITION: Optional[str] = None
    APHRODITE_CPU_KVCACHE_SPACE: Optional[int] = 0
    APHRODITE_CPU_OMP_THREADS_BIND: str = ''
    APHRODITE_CPU_NUM_OF_RESERVED_CPU: Optional[int] = None
    APHRODITE_CPU_MOE_PREPACK: bool = True
    APHRODITE_CPU_SGL_KERNEL: bool = False
    APHRODITE_XLA_CACHE_PATH: str = os.path.join(APHRODITE_CACHE_ROOT, 'xla_cache')
    APHRODITE_XLA_CHECK_RECOMPILATION: bool = False
    APHRODITE_FUSED_MOE_CHUNK_SIZE: int = 64 * 1024
    APHRODITE_ENABLE_FUSED_MOE_ACTIVATION_CHUNKING: bool = True
    APHRODITE_USE_RAY_SPMD_WORKER: bool = False
    APHRODITE_USE_RAY_COMPILED_DAG: bool = False
    APHRODITE_USE_RAY_COMPILED_DAG_CHANNEL_TYPE: str = 'auto'
    APHRODITE_USE_RAY_COMPILED_DAG_OVERLAP_COMM: bool = False
    APHRODITE_USE_RAY_WRAPPED_PP_COMM: bool = True
    APHRODITE_XLA_USE_SPMD: bool = False
    APHRODITE_WORKER_MULTIPROC_METHOD: str = 'fork'
    APHRODITE_ASSETS_CACHE: str = os.path.join(APHRODITE_CACHE_ROOT, 'assets')
    APHRODITE_IMAGE_FETCH_TIMEOUT: int = 5
    APHRODITE_VIDEO_FETCH_TIMEOUT: int = 30
    APHRODITE_AUDIO_FETCH_TIMEOUT: int = 10
    APHRODITE_MAX_AUDIO_CLIP_FILESIZE_MB: int = 25
    APHRODITE_VIDEO_LOADER_BACKEND: str = 'opencv'
    APHRODITE_MM_INPUT_CACHE_GIB: int = 8
    APHRODITE_TARGET_DEVICE: str = 'cuda'
    MAX_JOBS: Optional[str] = None
    NVCC_THREADS: Optional[str] = None
    APHRODITE_USE_PRECOMPILED: bool = False
    APHRODITE_TEST_USE_PRECOMPILED_NIGHTLY_WHEEL: bool = False
    APHRODITE_KEEP_ALIVE_ON_ENGINE_DEATH: bool = False
    CMAKE_BUILD_TYPE: Optional[str] = None
    VERBOSE: bool = False
    APHRODITE_ALLOW_LONG_MAX_MODEL_LEN: bool = False
    APHRODITE_RPC_TIMEOUT: int = 10000
    APHRODITE_HTTP_TIMEOUT_KEEP_ALIVE: int = 5
    APHRODITE_PLUGINS: Optional[list[str]] = None
    APHRODITE_LORA_RESOLVER_CACHE_DIR: Optional[str] = None
    APHRODITE_TORCH_PROFILER_DIR: Optional[str] = None
    APHRODITE_TORCH_PROFILER_RECORD_SHAPES: bool = False
    APHRODITE_TORCH_PROFILER_WITH_PROFILE_MEMORY: bool = False
    APHRODITE_TORCH_PROFILER_WITH_STACK: bool = True
    APHRODITE_TORCH_PROFILER_WITH_FLOPS: bool = False
    APHRODITE_USE_TRITON_AWQ: bool = False
    APHRODITE_ALLOW_RUNTIME_LORA_UPDATING: bool = False
    APHRODITE_SKIP_P2P_CHECK: bool = False
    APHRODITE_DISABLED_KERNELS: list[str] = []
    APHRODITE_USE_V1: bool = True
    APHRODITE_ROCM_USE_AITER: bool = False
    APHRODITE_ROCM_USE_AITER_PAGED_ATTN: bool = False
    APHRODITE_ROCM_USE_AITER_LINEAR: bool = True
    APHRODITE_ROCM_USE_AITER_MOE: bool = True
    APHRODITE_ROCM_USE_AITER_RMSNORM: bool = True
    APHRODITE_ROCM_USE_AITER_MLA: bool = True
    APHRODITE_ROCM_USE_AITER_MHA: bool = True
    APHRODITE_ROCM_USE_SKINNY_GEMM: bool = True
    APHRODITE_ROCM_FP8_PADDING: bool = True
    APHRODITE_ROCM_MOE_PADDING: bool = True
    APHRODITE_ROCM_CUSTOM_PAGED_ATTN: bool = True
    APHRODITE_ENABLE_V1_MULTIPROCESSING: bool = True
    APHRODITE_LOG_BATCHSIZE_INTERVAL: float = -1
    APHRODITE_DISABLE_COMPILE_CACHE: bool = False
    Q_SCALE_CONSTANT: int = 200
    K_SCALE_CONSTANT: int = 200
    V_SCALE_CONSTANT: int = 100
    APHRODITE_SERVER_DEV_MODE: bool = False
    APHRODITE_V1_OUTPUT_PROC_CHUNK_SIZE: int = 128
    APHRODITE_MLA_DISABLE: bool = False
    APHRODITE_RAY_PER_WORKER_GPUS: float = 1.0
    APHRODITE_RAY_BUNDLE_INDICES: str = ''
    APHRODITE_CUDART_SO_PATH: Optional[str] = None
    APHRODITE_DP_RANK: int = 0
    APHRODITE_DP_RANK_LOCAL: int = -1
    APHRODITE_DP_SIZE: int = 1
    APHRODITE_DP_MASTER_IP: str = ''
    APHRODITE_DP_MASTER_PORT: int = 0
    APHRODITE_MOE_DP_CHUNK_SIZE: int = 256
    APHRODITE_RANDOMIZE_DP_DUMMY_INPUTS: bool = False
    APHRODITE_MARLIN_USE_ATOMIC_ADD: bool = False
    APHRODITE_V0_USE_OUTLINES_CACHE: bool = False
    APHRODITE_V1_USE_OUTLINES_CACHE: bool = False
    APHRODITE_TPU_BUCKET_PADDING_GAP: int = 0
    APHRODITE_TPU_MOST_MODEL_LEN: Optional[int] = None
    APHRODITE_TPU_USING_PATHWAYS: bool = False
    APHRODITE_USE_DEEP_GEMM: bool = False
    APHRODITE_SKIP_DEEP_GEMM_WARMUP: bool = False
    APHRODITE_USE_FLASHINFER_MOE_FP8: bool = False
    APHRODITE_USE_FLASHINFER_MOE_FP4: bool = False
    APHRODITE_XGRAMMAR_CACHE_MB: int = 0
    APHRODITE_MSGPACK_ZERO_COPY_THRESHOLD: int = 256
    APHRODITE_ALLOW_INSECURE_SERIALIZATION: bool = False
    APHRODITE_NIXL_SIDE_CHANNEL_HOST: str = 'localhost'
    APHRODITE_NIXL_SIDE_CHANNEL_PORT: int = 5557
    APHRODITE_ALL2ALL_BACKEND: str = 'naive'
    APHRODITE_MAX_TOKENS_PER_EXPERT_FP4_MOE: int = 163840
    APHRODITE_TOOL_PARSE_REGEX_TIMEOUT_SECONDS: int = 1
    APHRODITE_SLEEP_WHEN_IDLE: bool = False
    APHRODITE_MQ_MAX_CHUNK_BYTES_MB: int = 16
    APHRODITE_EXECUTE_MODEL_TIMEOUT_SECONDS: int = 300
    APHRODITE_KV_CACHE_LAYOUT: Optional[str] = None
    APHRODITE_COMPUTE_NANS_IN_LOGITS: bool = False
    APHRODITE_USE_NVFP4_CT_EMULATIONS: bool = False
    APHRODITE_ROCM_QUICK_REDUCE_QUANTIZATION: str = 'NONE'
    APHRODITE_ROCM_QUICK_REDUCE_CAST_BF16_TO_FP16: bool = True
    APHRODITE_ROCM_QUICK_REDUCE_MAX_SIZE_BYTES_MB: Optional[int] = None
    APHRODITE_NIXL_ABORT_REQUEST_TIMEOUT: int = 120
    APHRODITE_USE_CUDNN_PREFILL: bool = False
    APHRODITE_ENABLE_CUDAGRAPH_GC: bool = False
    APHRODITE_LOOPBACK_IP: str = ''
    APHRODITE_ALLOW_CHUNKED_LOCAL_ATTN_WITH_HYBRID_KV_CACHE: bool = False
    APHRODITE_ENABLE_RESPONSES_API_STORE: bool = False
    APHRODITE_USE_TRTLLM_CONTEXT_ATTENTION: bool = False
    APHRODITE_USE_TRTLLM_DECODE_ATTENTION: bool = False
    APHRODITE_USE_TRTLLM_ATTENTION: bool = False
    APHRODITE_KOBOLD_API: bool = False
    APHRODITE_REQUEST_LEVEL_METRICS: bool = False
    APHRODITE_USE_SAMPLING_KERNELS: bool = False
    APHRODITE_NO_DEPRECATION_WARNING: bool = False
    APHRODITE_DYNAMIC_ROPE_SCALING: bool = False
    APHRODITE_USE_FLASHINFER_MOE_MXFP4_MXFP8: bool = False
    APHRODITE_USE_FLASHINFER_MOE_MXFP4_BF16: bool = False
def get_default_cache_root():
    return os.getenv('XDG_CACHE_HOME', os.path.join(os.path.expanduser('~'), '.cache'))
def get_default_config_root():
    return os.getenv('XDG_CONFIG_HOME', os.path.join(os.path.expanduser('~'), '.config'))
def maybe_convert_int(value: Optional[str]) -> Optional[int]:
    if value is None:
        return None
    return int(value)
def get_aphrodite_port() -> Optional[int]:
    if 'APHRODITE_PORT' not in os.environ:
        return None
    port = os.getenv('APHRODITE_PORT', '0')
    try:
        return int(port)
    except ValueError as err:
        from urllib.parse import urlparse
        parsed = urlparse(port)
        if parsed.scheme:
            raise ValueError(f"APHRODITE_PORT '{port}' appears to be a URI. This may be caused by a Kubernetes service discovery issue.") from None
        raise ValueError(f"APHRODITE_PORT '{port}' must be a valid integer") from err
environment_variables: dict[str, Callable[[], Any]] = {'APHRODITE_TARGET_DEVICE': lambda: os.getenv('APHRODITE_TARGET_DEVICE', 'cuda').lower(), 'MAX_JOBS': lambda: os.getenv('MAX_JOBS', None), 'NVCC_THREADS': lambda: os.getenv('NVCC_THREADS', None), 'APHRODITE_USE_PRECOMPILED': lambda: bool(os.environ.get('APHRODITE_USE_PRECOMPILED')) or bool(os.environ.get('APHRODITE_PRECOMPILED_WHEEL_LOCATION')), 'APHRODITE_TEST_USE_PRECOMPILED_NIGHTLY_WHEEL': lambda: bool(int(os.getenv('APHRODITE_TEST_USE_PRECOMPILED_NIGHTLY_WHEEL', '0'))), 'CMAKE_BUILD_TYPE': lambda: os.getenv('CMAKE_BUILD_TYPE'), 'VERBOSE': lambda: bool(int(os.getenv('VERBOSE', '0'))), 'APHRODITE_CONFIG_ROOT': lambda: os.path.expanduser(os.getenv('APHRODITE_CONFIG_ROOT', os.path.join(get_default_config_root(), 'aphrodite'))), 'APHRODITE_CACHE_ROOT': lambda: os.path.expanduser(os.getenv('APHRODITE_CACHE_ROOT', os.path.join(get_default_cache_root(), 'aphrodite'))), 'APHRODITE_HOST_IP': lambda: os.getenv('APHRODITE_HOST_IP', ''), 'APHRODITE_PORT': get_aphrodite_port, 'APHRODITE_RPC_BASE_PATH': lambda: os.getenv('APHRODITE_RPC_BASE_PATH', tempfile.gettempdir()), 'APHRODITE_USE_MODELSCOPE': lambda: os.environ.get('APHRODITE_USE_MODELSCOPE', 'False').lower() == 'true', 'APHRODITE_RINGBUFFER_WARNING_INTERVAL': lambda: int(os.environ.get('APHRODITE_RINGBUFFER_WARNING_INTERVAL', '60')), 'CUDA_HOME': lambda: os.environ.get('CUDA_HOME', None), 'APHRODITE_NCCL_SO_PATH': lambda: os.environ.get('APHRODITE_NCCL_SO_PATH', None), 'LD_LIBRARY_PATH': lambda: os.environ.get('LD_LIBRARY_PATH', None), 'APHRODITE_USE_TRITON_FLASH_ATTN': lambda: os.environ.get('APHRODITE_USE_TRITON_FLASH_ATTN', 'True').lower() in ('true', '1'), 'APHRODITE_V1_USE_PREFILL_DECODE_ATTENTION': lambda: os.getenv('APHRODITE_V1_USE_PREFILL_DECODE_ATTENTION', 'False').lower() in ('true', '1'), 'APHRODITE_USE_AITER_UNIFIED_ATTENTION': lambda: os.getenv('APHRODITE_USE_AITER_UNIFIED_ATTENTION', 'False').lower() in ('true', '1'), 'APHRODITE_FLASH_ATTN_VERSION': lambda: maybe_convert_int(os.environ.get('APHRODITE_FLASH_ATTN_VERSION', None)), 'APHRODITE_TEST_DYNAMO_FULLGRAPH_CAPTURE': lambda: bool(os.environ.get('APHRODITE_TEST_DYNAMO_FULLGRAPH_CAPTURE', '1') != '0'), 'APHRODITE_USE_STANDALONE_COMPILE': lambda: os.environ.get('APHRODITE_USE_STANDALONE_COMPILE', '1') == '1', 'LOCAL_RANK': lambda: int(os.environ.get('LOCAL_RANK', '0')), 'CUDA_VISIBLE_DEVICES': lambda: os.environ.get('CUDA_VISIBLE_DEVICES', None), 'APHRODITE_ENGINE_ITERATION_TIMEOUT_S': lambda: int(os.environ.get('APHRODITE_ENGINE_ITERATION_TIMEOUT_S', '60')), 'APHRODITE_API_KEY': lambda: os.environ.get('APHRODITE_API_KEY', None), 'APHRODITE_DEBUG_LOG_API_SERVER_RESPONSE': lambda: os.environ.get('APHRODITE_DEBUG_LOG_API_SERVER_RESPONSE', 'False').lower() == 'true', 'S3_ACCESS_KEY_ID': lambda: os.environ.get('S3_ACCESS_KEY_ID', None), 'S3_SECRET_ACCESS_KEY': lambda: os.environ.get('S3_SECRET_ACCESS_KEY', None), 'S3_ENDPOINT_URL': lambda: os.environ.get('S3_ENDPOINT_URL', None), 'APHRODITE_USAGE_STATS_SERVER': lambda: os.environ.get('APHRODITE_USAGE_STATS_SERVER', 'https://stats.aphrodite.ai'), 'APHRODITE_NO_USAGE_STATS': lambda: os.environ.get('APHRODITE_NO_USAGE_STATS', '0') == '1', 'APHRODITE_DO_NOT_TRACK': lambda: (os.environ.get('APHRODITE_DO_NOT_TRACK', None) or os.environ.get('DO_NOT_TRACK', None) or '0') == '1', 'APHRODITE_USAGE_SOURCE': lambda: os.environ.get('APHRODITE_USAGE_SOURCE', 'production'), 'APHRODITE_CONFIGURE_LOGGING': lambda: int(os.getenv('APHRODITE_CONFIGURE_LOGGING', '1')), 'APHRODITE_LOGGING_CONFIG_PATH': lambda: os.getenv('APHRODITE_LOGGING_CONFIG_PATH'), 'APHRODITE_LOGGING_LEVEL': lambda: os.getenv('APHRODITE_LOGGING_LEVEL', 'INFO').upper(), 'APHRODITE_LOGGING_PREFIX': lambda: os.getenv('APHRODITE_LOGGING_PREFIX', ''), 'APHRODITE_LOGITS_PROCESSOR_THREADS': lambda: int(os.getenv('APHRODITE_LOGITS_PROCESSOR_THREADS', '0')) if 'APHRODITE_LOGITS_PROCESSOR_THREADS' in os.environ else None, 'APHRODITE_TRACE_FUNCTION': lambda: int(os.getenv('APHRODITE_TRACE_FUNCTION', '0')), 'APHRODITE_ATTENTION_BACKEND': lambda: os.getenv('APHRODITE_ATTENTION_BACKEND', None), 'APHRODITE_USE_FLASHINFER_SAMPLER': lambda: bool(int(os.environ['APHRODITE_USE_FLASHINFER_SAMPLER'])) if 'APHRODITE_USE_FLASHINFER_SAMPLER' in os.environ else None, 'APHRODITE_FLASHINFER_FORCE_TENSOR_CORES': lambda: bool(int(os.getenv('APHRODITE_FLASHINFER_FORCE_TENSOR_CORES', '0'))), 'APHRODITE_PP_LAYER_PARTITION': lambda: os.getenv('APHRODITE_PP_LAYER_PARTITION', None), 'APHRODITE_CPU_KVCACHE_SPACE': lambda: int(os.getenv('APHRODITE_CPU_KVCACHE_SPACE', '0')) if 'APHRODITE_CPU_KVCACHE_SPACE' in os.environ else None, 'APHRODITE_CPU_OMP_THREADS_BIND': lambda: os.getenv('APHRODITE_CPU_OMP_THREADS_BIND', 'auto'), 'APHRODITE_CPU_NUM_OF_RESERVED_CPU': lambda: int(os.getenv('APHRODITE_CPU_NUM_OF_RESERVED_CPU', '0')) if 'APHRODITE_CPU_NUM_OF_RESERVED_CPU' in os.environ else None, 'APHRODITE_CPU_MOE_PREPACK': lambda: bool(int(os.getenv('APHRODITE_CPU_MOE_PREPACK', '1'))), 'APHRODITE_CPU_SGL_KERNEL': lambda: bool(int(os.getenv('APHRODITE_CPU_SGL_KERNEL', '0'))), 'APHRODITE_USE_RAY_SPMD_WORKER': lambda: bool(int(os.getenv('APHRODITE_USE_RAY_SPMD_WORKER', '0'))), 'APHRODITE_USE_RAY_COMPILED_DAG': lambda: bool(int(os.getenv('APHRODITE_USE_RAY_COMPILED_DAG', '0'))), 'APHRODITE_USE_RAY_COMPILED_DAG_CHANNEL_TYPE': lambda: os.getenv('APHRODITE_USE_RAY_COMPILED_DAG_CHANNEL_TYPE', 'auto'), 'APHRODITE_USE_RAY_COMPILED_DAG_OVERLAP_COMM': lambda: bool(int(os.getenv('APHRODITE_USE_RAY_COMPILED_DAG_OVERLAP_COMM', '0'))), 'APHRODITE_USE_RAY_WRAPPED_PP_COMM': lambda: bool(int(os.getenv('APHRODITE_USE_RAY_WRAPPED_PP_COMM', '1'))), 'APHRODITE_WORKER_MULTIPROC_METHOD': lambda: os.getenv('APHRODITE_WORKER_MULTIPROC_METHOD', 'fork'), 'APHRODITE_ASSETS_CACHE': lambda: os.path.expanduser(os.getenv('APHRODITE_ASSETS_CACHE', os.path.join(get_default_cache_root(), 'aphrodite', 'assets'))), 'APHRODITE_IMAGE_FETCH_TIMEOUT': lambda: int(os.getenv('APHRODITE_IMAGE_FETCH_TIMEOUT', '5')), 'APHRODITE_VIDEO_FETCH_TIMEOUT': lambda: int(os.getenv('APHRODITE_VIDEO_FETCH_TIMEOUT', '30')), 'APHRODITE_AUDIO_FETCH_TIMEOUT': lambda: int(os.getenv('APHRODITE_AUDIO_FETCH_TIMEOUT', '10')), 'APHRODITE_MAX_AUDIO_CLIP_FILESIZE_MB': lambda: int(os.getenv('APHRODITE_MAX_AUDIO_CLIP_FILESIZE_MB', '25')), 'APHRODITE_VIDEO_LOADER_BACKEND': lambda: os.getenv('APHRODITE_VIDEO_LOADER_BACKEND', 'opencv'), 'APHRODITE_MM_INPUT_CACHE_GIB': lambda: int(os.getenv('APHRODITE_MM_INPUT_CACHE_GIB', '4')), 'APHRODITE_XLA_CACHE_PATH': lambda: os.path.expanduser(os.getenv('APHRODITE_XLA_CACHE_PATH', os.path.join(get_default_cache_root(), 'aphrodite', 'xla_cache'))), 'APHRODITE_XLA_CHECK_RECOMPILATION': lambda: bool(int(os.getenv('APHRODITE_XLA_CHECK_RECOMPILATION', '0'))), 'APHRODITE_XLA_USE_SPMD': lambda: bool(int(os.getenv('APHRODITE_XLA_USE_SPMD', '0'))), 'APHRODITE_FUSED_MOE_CHUNK_SIZE': lambda: int(os.getenv('APHRODITE_FUSED_MOE_CHUNK_SIZE', '32768')), 'APHRODITE_ENABLE_FUSED_MOE_ACTIVATION_CHUNKING': lambda: bool(int(os.getenv('APHRODITE_ENABLE_FUSED_MOE_ACTIVATION_CHUNKING', '1'))), 'APHRODITE_KEEP_ALIVE_ON_ENGINE_DEATH': lambda: bool(os.getenv('APHRODITE_KEEP_ALIVE_ON_ENGINE_DEATH', 0)), 'APHRODITE_ALLOW_LONG_MAX_MODEL_LEN': lambda: os.environ.get('APHRODITE_ALLOW_LONG_MAX_MODEL_LEN', '0').strip().lower() in ('1', 'true'), 'APHRODITE_TEST_FORCE_FP8_MARLIN': lambda: os.environ.get('APHRODITE_TEST_FORCE_FP8_MARLIN', '0').strip().lower() in ('1', 'true'), 'APHRODITE_TEST_FORCE_LOAD_FORMAT': lambda: os.getenv('APHRODITE_TEST_FORCE_LOAD_FORMAT', 'dummy'), 'APHRODITE_RPC_TIMEOUT': lambda: int(os.getenv('APHRODITE_RPC_TIMEOUT', '10000')), 'APHRODITE_HTTP_TIMEOUT_KEEP_ALIVE': lambda: int(os.environ.get('APHRODITE_HTTP_TIMEOUT_KEEP_ALIVE', '5')), 'APHRODITE_PLUGINS': lambda: None if 'APHRODITE_PLUGINS' not in os.environ else os.environ['APHRODITE_PLUGINS'].split(','), 'APHRODITE_LORA_RESOLVER_CACHE_DIR': lambda: os.getenv('APHRODITE_LORA_RESOLVER_CACHE_DIR', None), 'APHRODITE_TORCH_PROFILER_DIR': lambda: None if os.getenv('APHRODITE_TORCH_PROFILER_DIR', None) is None else os.path.expanduser(os.getenv('APHRODITE_TORCH_PROFILER_DIR', '.')), 'APHRODITE_TORCH_PROFILER_RECORD_SHAPES': lambda: bool(os.getenv('APHRODITE_TORCH_PROFILER_RECORD_SHAPES', '0') != '0'), 'APHRODITE_TORCH_PROFILER_WITH_PROFILE_MEMORY': lambda: bool(os.getenv('APHRODITE_TORCH_PROFILER_WITH_PROFILE_MEMORY', '0') != '0'), 'APHRODITE_TORCH_PROFILER_WITH_STACK': lambda: bool(os.getenv('APHRODITE_TORCH_PROFILER_WITH_STACK', '1') != '0'), 'APHRODITE_TORCH_PROFILER_WITH_FLOPS': lambda: bool(os.getenv('APHRODITE_TORCH_PROFILER_WITH_FLOPS', '0') != '0'), 'APHRODITE_USE_TRITON_AWQ': lambda: bool(int(os.getenv('APHRODITE_USE_TRITON_AWQ', '0'))), 'APHRODITE_ALLOW_RUNTIME_LORA_UPDATING': lambda: os.environ.get('APHRODITE_ALLOW_RUNTIME_LORA_UPDATING', '0').strip().lower() in ('1', 'true'), 'APHRODITE_SKIP_P2P_CHECK': lambda: os.getenv('APHRODITE_SKIP_P2P_CHECK', '1') == '1', 'APHRODITE_DISABLED_KERNELS': lambda: [] if 'APHRODITE_DISABLED_KERNELS' not in os.environ else os.environ['APHRODITE_DISABLED_KERNELS'].split(','), 'APHRODITE_USE_V1': lambda: bool(int(os.getenv('APHRODITE_USE_V1', '1'))), 'APHRODITE_ROCM_USE_AITER': lambda: os.getenv('APHRODITE_ROCM_USE_AITER', 'False').lower() in ('true', '1'), 'APHRODITE_ROCM_USE_AITER_PAGED_ATTN': lambda: os.getenv('APHRODITE_ROCM_USE_AITER_PAGED_ATTN', 'False').lower() in ('true', '1'), 'APHRODITE_ROCM_USE_AITER_LINEAR': lambda: os.getenv('APHRODITE_ROCM_USE_AITER_LINEAR', 'True').lower() in ('true', '1'), 'APHRODITE_ROCM_USE_AITER_MOE': lambda: os.getenv('APHRODITE_ROCM_USE_AITER_MOE', 'True').lower() in ('true', '1'), 'APHRODITE_ROCM_USE_AITER_RMSNORM': lambda: os.getenv('APHRODITE_ROCM_USE_AITER_RMSNORM', 'True').lower() in ('true', '1'), 'APHRODITE_ROCM_USE_AITER_MLA': lambda: os.getenv('APHRODITE_ROCM_USE_AITER_MLA', 'True').lower() in ('true', '1'), 'APHRODITE_ROCM_USE_AITER_MHA': lambda: os.getenv('APHRODITE_ROCM_USE_AITER_MHA', 'True').lower() in ('true', '1'), 'APHRODITE_ROCM_USE_SKINNY_GEMM': lambda: os.getenv('APHRODITE_ROCM_USE_SKINNY_GEMM', 'True').lower() in ('true', '1'), 'APHRODITE_ROCM_FP8_PADDING': lambda: bool(int(os.getenv('APHRODITE_ROCM_FP8_PADDING', '1'))), 'APHRODITE_ROCM_MOE_PADDING': lambda: bool(int(os.getenv('APHRODITE_ROCM_MOE_PADDING', '1'))), 'APHRODITE_ROCM_CUSTOM_PAGED_ATTN': lambda: os.getenv('APHRODITE_ROCM_CUSTOM_PAGED_ATTN', 'True').lower() in ('true', '1'), 'APHRODITE_ROCM_QUICK_REDUCE_QUANTIZATION': lambda: os.getenv('APHRODITE_ROCM_QUICK_REDUCE_QUANTIZATION', 'NONE').upper(), 'APHRODITE_ROCM_QUICK_REDUCE_CAST_BF16_TO_FP16': lambda: os.getenv('APHRODITE_ROCM_QUICK_REDUCE_CAST_BF16_TO_FP16', 'True').lower() in ('true', '1'), 'APHRODITE_ROCM_QUICK_REDUCE_MAX_SIZE_BYTES_MB': lambda: maybe_convert_int(os.environ.get('APHRODITE_ROCM_QUICK_REDUCE_MAX_SIZE_BYTES_MB', None)), 'Q_SCALE_CONSTANT': lambda: int(os.getenv('Q_SCALE_CONSTANT', '200')), 'K_SCALE_CONSTANT': lambda: int(os.getenv('K_SCALE_CONSTANT', '200')), 'V_SCALE_CONSTANT': lambda: int(os.getenv('V_SCALE_CONSTANT', '100')), 'APHRODITE_ENABLE_V1_MULTIPROCESSING': lambda: bool(int(os.getenv('APHRODITE_ENABLE_V1_MULTIPROCESSING', '1'))), 'APHRODITE_LOG_BATCHSIZE_INTERVAL': lambda: float(os.getenv('APHRODITE_LOG_BATCHSIZE_INTERVAL', '-1')), 'APHRODITE_DISABLE_COMPILE_CACHE': lambda: bool(int(os.getenv('APHRODITE_DISABLE_COMPILE_CACHE', '0'))), 'APHRODITE_SERVER_DEV_MODE': lambda: bool(int(os.getenv('APHRODITE_SERVER_DEV_MODE', '0'))), 'APHRODITE_V1_OUTPUT_PROC_CHUNK_SIZE': lambda: int(os.getenv('APHRODITE_V1_OUTPUT_PROC_CHUNK_SIZE', '128')), 'APHRODITE_MLA_DISABLE': lambda: bool(int(os.getenv('APHRODITE_MLA_DISABLE', '0'))), 'APHRODITE_RAY_PER_WORKER_GPUS': lambda: float(os.getenv('APHRODITE_RAY_PER_WORKER_GPUS', '1.0')), 'APHRODITE_RAY_BUNDLE_INDICES': lambda: os.getenv('APHRODITE_RAY_BUNDLE_INDICES', ''), 'APHRODITE_CUDART_SO_PATH': lambda: os.getenv('APHRODITE_CUDART_SO_PATH', None), 'APHRODITE_DP_RANK': lambda: int(os.getenv('APHRODITE_DP_RANK', '0')), 'APHRODITE_DP_RANK_LOCAL': lambda: int(os.getenv('APHRODITE_DP_RANK_LOCAL', sys.modules[__name__].APHRODITE_DP_RANK)), 'APHRODITE_DP_SIZE': lambda: int(os.getenv('APHRODITE_DP_SIZE', '1')), 'APHRODITE_DP_MASTER_IP': lambda: os.getenv('APHRODITE_DP_MASTER_IP', '127.0.0.1'), 'APHRODITE_DP_MASTER_PORT': lambda: int(os.getenv('APHRODITE_DP_MASTER_PORT', '0')), 'APHRODITE_MOE_DP_CHUNK_SIZE': lambda: int(os.getenv('APHRODITE_MOE_DP_CHUNK_SIZE', '256')), 'APHRODITE_RANDOMIZE_DP_DUMMY_INPUTS': lambda: os.environ.get('APHRODITE_RANDOMIZE_DP_DUMMY_INPUTS', '0') == '1', 'APHRODITE_CI_USE_S3': lambda: os.environ.get('APHRODITE_CI_USE_S3', '0') == '1', 'APHRODITE_MODEL_REDIRECT_PATH': lambda: os.environ.get('APHRODITE_MODEL_REDIRECT_PATH', None), 'APHRODITE_MARLIN_USE_ATOMIC_ADD': lambda: os.environ.get('APHRODITE_MARLIN_USE_ATOMIC_ADD', '0') == '1', 'APHRODITE_V0_USE_OUTLINES_CACHE': lambda: os.environ.get('APHRODITE_V0_USE_OUTLINES_CACHE', '0') == '1', 'APHRODITE_V1_USE_OUTLINES_CACHE': lambda: os.environ.get('APHRODITE_V1_USE_OUTLINES_CACHE', '0') == '1', 'APHRODITE_TPU_BUCKET_PADDING_GAP': lambda: int(os.environ['APHRODITE_TPU_BUCKET_PADDING_GAP']) if 'APHRODITE_TPU_BUCKET_PADDING_GAP' in os.environ else 0, 'APHRODITE_TPU_MOST_MODEL_LEN': lambda: maybe_convert_int(os.environ.get('APHRODITE_TPU_MOST_MODEL_LEN', None)), 'APHRODITE_TPU_USING_PATHWAYS': lambda: bool('proxy' in os.getenv('JAX_PLATFORMS', '').lower()), 'APHRODITE_USE_DEEP_GEMM': lambda: bool(int(os.getenv('APHRODITE_USE_DEEP_GEMM', '0'))), 'APHRODITE_SKIP_DEEP_GEMM_WARMUP': lambda: bool(int(os.getenv('APHRODITE_SKIP_DEEP_GEMM_WARMUP', '0'))), 'APHRODITE_USE_FLASHINFER_MOE_FP8': lambda: bool(int(os.getenv('APHRODITE_USE_FLASHINFER_MOE_FP8', '0'))), 'APHRODITE_USE_FLASHINFER_MOE_FP4': lambda: bool(int(os.getenv('APHRODITE_USE_FLASHINFER_MOE_FP4', '0'))), 'APHRODITE_XGRAMMAR_CACHE_MB': lambda: int(os.getenv('APHRODITE_XGRAMMAR_CACHE_MB', '512')), 'APHRODITE_MSGPACK_ZERO_COPY_THRESHOLD': lambda: int(os.getenv('APHRODITE_MSGPACK_ZERO_COPY_THRESHOLD', '256')), 'APHRODITE_ALLOW_INSECURE_SERIALIZATION': lambda: bool(int(os.getenv('APHRODITE_ALLOW_INSECURE_SERIALIZATION', '0'))), 'APHRODITE_NIXL_SIDE_CHANNEL_HOST': lambda: os.getenv('APHRODITE_NIXL_SIDE_CHANNEL_HOST', 'localhost'), 'APHRODITE_NIXL_SIDE_CHANNEL_PORT': lambda: int(os.getenv('APHRODITE_NIXL_SIDE_CHANNEL_PORT', '5557')), 'APHRODITE_ALL2ALL_BACKEND': lambda: os.getenv('APHRODITE_ALL2ALL_BACKEND', 'naive'), 'APHRODITE_MAX_TOKENS_PER_EXPERT_FP4_MOE': lambda: int(os.getenv('APHRODITE_MAX_TOKENS_PER_EXPERT_FP4_MOE', '163840')), 'APHRODITE_TOOL_PARSE_REGEX_TIMEOUT_SECONDS': lambda: int(os.getenv('APHRODITE_TOOL_PARSE_REGEX_TIMEOUT_SECONDS', '1')), 'APHRODITE_SLEEP_WHEN_IDLE': lambda: bool(int(os.getenv('APHRODITE_SLEEP_WHEN_IDLE', '0'))), 'APHRODITE_MQ_MAX_CHUNK_BYTES_MB': lambda: int(os.getenv('APHRODITE_MQ_MAX_CHUNK_BYTES_MB', '16')), 'APHRODITE_EXECUTE_MODEL_TIMEOUT_SECONDS': lambda: int(os.getenv('APHRODITE_EXECUTE_MODEL_TIMEOUT_SECONDS', '300')), 'APHRODITE_KV_CACHE_LAYOUT': lambda: os.getenv('APHRODITE_KV_CACHE_LAYOUT', None), 'APHRODITE_COMPUTE_NANS_IN_LOGITS': lambda: bool(int(os.getenv('APHRODITE_COMPUTE_NANS_IN_LOGITS', '0'))), 'APHRODITE_USE_NVFP4_CT_EMULATIONS': lambda: bool(int(os.getenv('APHRODITE_USE_NVFP4_CT_EMULATIONS', '0'))), 'APHRODITE_NIXL_ABORT_REQUEST_TIMEOUT': lambda: int(os.getenv('APHRODITE_NIXL_ABORT_REQUEST_TIMEOUT', '120')), 'APHRODITE_USE_CUDNN_PREFILL': lambda: bool(int(os.getenv('APHRODITE_USE_CUDNN_PREFILL', '0'))), 'APHRODITE_USE_TRTLLM_CONTEXT_ATTENTION': lambda: bool(int(os.getenv('APHRODITE_USE_TRTLLM_CONTEXT_ATTENTION', '0'))), 'APHRODITE_USE_TRTLLM_DECODE_ATTENTION': lambda: bool(int(os.getenv('APHRODITE_USE_TRTLLM_DECODE_ATTENTION', '0'))), 'APHRODITE_USE_TRTLLM_ATTENTION': lambda: bool(int(os.getenv('APHRODITE_USE_TRTLLM_ATTENTION', '0'))), 'APHRODITE_ENABLE_CUDAGRAPH_GC': lambda: bool(int(os.getenv('APHRODITE_ENABLE_CUDAGRAPH_GC', '0'))), 'APHRODITE_LOOPBACK_IP': lambda: os.getenv('APHRODITE_LOOPBACK_IP', ''), 'APHRODITE_PROCESS_NAME_PREFIX': lambda: os.getenv('APHRODITE_PROCESS_NAME_PREFIX', 'APHRODITE'), 'APHRODITE_ALLOW_CHUNKED_LOCAL_ATTN_WITH_HYBRID_KV_CACHE': lambda: bool(int(os.getenv('APHRODITE_ALLOW_CHUNKED_LOCAL_ATTN_WITH_HYBRID_KV_CACHE', '0'))), 'APHRODITE_ENABLE_RESPONSES_API_STORE': lambda: bool(int(os.getenv('APHRODITE_ENABLE_RESPONSES_API_STORE', '0'))), 'APHRODITE_KOBOLD_API': lambda: bool(int(os.getenv('APHRODITE_KOBOLD_API', '0'))), 'APHRODITE_REQUEST_LEVEL_METRICS': lambda: bool(int(os.getenv('APHRODITE_REQUEST_LEVEL_METRICS', '0'))), 'APHRODITE_USE_SAMPLING_KERNELS': lambda: bool(int(os.getenv('APHRODITE_USE_SAMPLING_KERNELS', '0'))), 'APHRODITE_NO_DEPRECATION_WARNING': lambda: bool(int(os.getenv('APHRODITE_NO_DEPRECATION_WARNING', '0'))), 'APHRODITE_DYNAMIC_ROPE_SCALING': lambda: bool(int(os.getenv('APHRODITE_DYNAMIC_ROPE_SCALING', '0'))), 'APHRODITE_USE_FLASHINFER_MOE_MXFP4_MXFP8': lambda: bool(int(os.getenv('APHRODITE_USE_FLASHINFER_MOE_MXFP4_MXFP8', '0'))), 'APHRODITE_USE_FLASHINFER_MOE_MXFP4_BF16': lambda: bool(int(os.getenv('APHRODITE_USE_FLASHINFER_MOE_MXFP4_BF16', '0')))}
def __getattr__(name: str):
    if name in environment_variables:
        return environment_variables[name]()
    raise AttributeError(f'module {__name__!r} has no attribute {name!r}')
def __dir__():
    return list(environment_variables.keys())
def is_set(name: str):
    if name in environment_variables:
        return name in os.environ
    raise AttributeError(f'module {__name__!r} has no attribute {name!r}')
def set_aphrodite_use_v1(use_v1: bool):
    if is_set('APHRODITE_USE_V1'):
        raise ValueError('Should not call set_aphrodite_use_v1() if APHRODITE_USE_V1 is set explicitly by the user. Please raise this as a Github Issue and explicitly set APHRODITE_USE_V1=0 or 1.')
    os.environ['APHRODITE_USE_V1'] = '1' if use_v1 else '0'
def compute_hash() -> str:
    factors: list[Any] = []
    def factorize(name: str):
        if __getattr__(name):
            factors.append(__getattr__(name))
        else:
            factors.append('None')
    environment_variables_to_hash = ['APHRODITE_PP_LAYER_PARTITION', 'APHRODITE_MLA_DISABLE', 'APHRODITE_USE_TRITON_FLASH_ATTN', 'APHRODITE_USE_TRITON_AWQ', 'APHRODITE_DP_RANK', 'APHRODITE_DP_SIZE', 'APHRODITE_USE_STANDALONE_COMPILE', 'APHRODITE_FUSED_MOE_CHUNK_SIZE']
    for key in environment_variables_to_hash:
        if key in environment_variables:
            factorize(key)
    hash_str = hashlib.md5(str(factors).encode(), usedforsecurity=False).hexdigest()
    return hash_str