import enum
import time
from collections.abc import Sequence
from typing import Any, Optional, Union
import msgspec
import torch
from aphrodite.common.pooling_params import PoolingParams
from aphrodite.common.sampling_params import SamplingParams
from aphrodite.lora.request import LoRARequest
from aphrodite.multimodal import MultiModalKwargs
from aphrodite.multimodal.inputs import PlaceholderRange
from aphrodite.v1.metrics.stats import SchedulerStats
from aphrodite.v1.outputs import LogprobsLists, LogprobsTensors
FINISH_REASON_STRINGS = ('stop', 'length', 'abort')
class FinishReason(enum.IntEnum):
    STOP = 0
    LENGTH = 1
    ABORT = 2
    def __str__(self):
        return FINISH_REASON_STRINGS[self.value]
class EngineCoreRequest(msgspec.Struct, array_like=True, omit_defaults=True, gc=False):
    request_id: str
    prompt_token_ids: list[int]
    mm_inputs: Optional[Sequence[Optional[MultiModalKwargs]]]
    mm_hashes: Optional[list[str]]
    mm_placeholders: Optional[list[PlaceholderRange]]
    sampling_params: Optional[SamplingParams]
    pooling_params: Optional[PoolingParams]
    eos_token_id: Optional[int]
    arrival_time: float
    lora_request: Optional[LoRARequest]
    cache_salt: Optional[str]
    data_parallel_rank: Optional[int]
    client_index: int = 0
    current_wave: int = 0
    priority: int = 0
class EngineCoreEventType(enum.IntEnum):
    QUEUED = 1
    SCHEDULED = 2
    PREEMPTED = 3
class EngineCoreEvent(msgspec.Struct):
    type: EngineCoreEventType
    timestamp: float
    @classmethod
    def new_event(cls, event_type: EngineCoreEventType, timestamp: Optional[float]=None) -> 'EngineCoreEvent':
        timestamp = time.monotonic() if timestamp is None else timestamp
        return cls(event_type, timestamp)
class EngineCoreOutput(msgspec.Struct, array_like=True, omit_defaults=True, gc=False):
    request_id: str
    new_token_ids: list[int]
    new_logprobs: Optional[LogprobsLists] = None
    new_prompt_logprobs_tensors: Optional[LogprobsTensors] = None
    pooling_output: Optional[torch.Tensor] = None
    finish_reason: Optional[FinishReason] = None
    stop_reason: Union[int, str, None] = None
    events: Optional[list[EngineCoreEvent]] = None
    kv_transfer_params: Optional[dict[str, Any]] = None
    num_cached_tokens: int = 0
    @property
    def finished(self) -> bool:
        return self.finish_reason is not None
class UtilityResult:
    def __init__(self, r: Any=None):
        self.result = r
class UtilityOutput(msgspec.Struct, array_like=True, gc=False):
    call_id: int
    failure_message: Optional[str] = None
    result: Optional[UtilityResult] = None
class EngineCoreOutputs(msgspec.Struct, array_like=True, omit_defaults=True, gc=False):
    engine_index: int = 0
    outputs: list[EngineCoreOutput] = []
    scheduler_stats: Optional[SchedulerStats] = None
    timestamp: float = 0.0
    utility_output: Optional[UtilityOutput] = None
    finished_requests: Optional[set[str]] = None
    wave_complete: Optional[int] = None
    start_wave: Optional[int] = None
    def __post_init__(self):
        if self.timestamp == 0.0:
            self.timestamp = time.monotonic()
class EngineCoreRequestType(enum.Enum):
    ADD = b'\x00'
    ABORT = b'\x01'
    START_DP_WAVE = b'\x02'
    UTILITY = b'\x03'
    EXECUTOR_FAILED = b'\x04'
class ReconfigureDistributedRequest(msgspec.Struct):
    new_data_parallel_size: int
    new_data_parallel_rank: int
    new_data_parallel_rank_local: int
    new_data_parallel_master_ip: str
    new_data_parallel_master_port: int
class ReconfigureRankType(enum.IntEnum):
    KEEP_CURRENT_RANK = -1
    SHUTDOWN_CURRENT_RANK = -2