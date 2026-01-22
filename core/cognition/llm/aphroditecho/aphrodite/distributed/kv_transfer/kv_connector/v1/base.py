import enum
from abc import ABC, abstractmethod
from typing import TYPE_CHECKING, Any, Callable, Literal, Optional
import torch
from loguru import logger
from aphrodite.v1.core.sched.output import SchedulerOutput
if TYPE_CHECKING:
    from aphrodite.attention.backends.abstract import AttentionMetadata
    from aphrodite.common.config import AphroditeConfig
    from aphrodite.forward_context import ForwardContext
    from aphrodite.v1.core.kv_cache_manager import KVCacheBlocks
    from aphrodite.v1.request import Request
CopyBlocksOp = Callable[[dict[str, torch.Tensor], dict[str, torch.Tensor], list[int], list[int], Literal['h2d', 'd2h']], None]
class KVConnectorRole(enum.Enum):
    SCHEDULER = 0
    WORKER = 1
class KVConnectorMetadata(ABC):
    pass
class KVConnectorBase_V1(ABC):
    def __init__(self, aphrodite_config: 'AphroditeConfig', role: KVConnectorRole):
        logger.warning('Initializing KVConnectorBase_V1. This API is experimental and subject to change in the future as we iterate the design.')
        self._connector_metadata: Optional[KVConnectorMetadata] = None
        self._aphrodite_config = aphrodite_config
        self._role = role
    @property
    def role(self) -> KVConnectorRole:
        return self._role
    def bind_connector_metadata(self, connector_metadata: KVConnectorMetadata) -> None:
        self._connector_metadata = connector_metadata
    def clear_connector_metadata(self) -> None:
        self._connector_metadata = None
    def _get_connector_metadata(self) -> KVConnectorMetadata:
        assert self._connector_metadata is not None
        return self._connector_metadata
    def register_kv_caches(self, kv_caches: dict[str, torch.Tensor]):
        return
    def set_host_xfer_buffer_ops(self, copy_operation: CopyBlocksOp):
        return
    @abstractmethod
    def start_load_kv(self, forward_context: 'ForwardContext', **kwargs) -> None:
        pass
    @abstractmethod
    def wait_for_layer_load(self, layer_name: str) -> None:
        pass
    @abstractmethod
    def save_kv_layer(self, layer_name: str, kv_layer: torch.Tensor, attn_metadata: 'AttentionMetadata', **kwargs) -> None:
        pass
    @abstractmethod
    def wait_for_save(self):
        pass
    def get_finished(self, finished_req_ids: set[str]) -> tuple[Optional[set[str]], Optional[set[str]]]:
        return (None, None)
    @abstractmethod
    def get_num_new_matched_tokens(self, request: 'Request', num_computed_tokens: int) -> tuple[int, bool]:
        pass
    @abstractmethod
    def update_state_after_alloc(self, request: 'Request', blocks: 'KVCacheBlocks', num_external_tokens: int):
        pass
    @abstractmethod
    def build_connector_meta(self, scheduler_output: SchedulerOutput) -> KVConnectorMetadata:
        pass
    def request_finished(self, request: 'Request', block_ids: list[int]) -> tuple[bool, Optional[dict[str, Any]]]:
        return (False, None)
    @classmethod
    def get_required_kvcache_layout(cls, aphrodite_config: 'AphroditeConfig') -> Optional[str]:
        return None