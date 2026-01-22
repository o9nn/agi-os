from typing import TYPE_CHECKING, Any, Optional
import torch
from aphrodite.common.config import AphroditeConfig
from aphrodite.distributed.kv_transfer.kv_connector.v1.base import KVConnectorBase_V1, KVConnectorMetadata, KVConnectorRole
from aphrodite.distributed.kv_transfer.kv_connector.v1.lmcache_integration.aphrodite_v1_adapter import LMCacheConnectorV1Impl
from aphrodite.v1.core.sched.output import SchedulerOutput
if TYPE_CHECKING:
    from aphrodite.attention.backends.abstract import AttentionMetadata
    from aphrodite.forward_context import ForwardContext
    from aphrodite.v1.core.kv_cache_manager import KVCacheBlocks
    from aphrodite.v1.request import Request
class LMCacheConnectorV1(KVConnectorBase_V1):
    def __init__(self, aphrodite_config: 'AphroditeConfig', role: KVConnectorRole):
        super().__init__(aphrodite_config=aphrodite_config, role=role)
        self._lmcache_engine = LMCacheConnectorV1Impl(aphrodite_config, role, self)
    def start_load_kv(self, forward_context: 'ForwardContext', **kwargs) -> None:
        self._lmcache_engine.start_load_kv(forward_context, **kwargs)
    def wait_for_layer_load(self, layer_name: str) -> None:
        self._lmcache_engine.wait_for_layer_load(layer_name)
    def save_kv_layer(self, layer_name: str, kv_layer: torch.Tensor, attn_metadata: 'AttentionMetadata', **kwargs) -> None:
        self._lmcache_engine.save_kv_layer(layer_name, kv_layer, attn_metadata, **kwargs)
    def wait_for_save(self):
        self._lmcache_engine.wait_for_save()
    def get_finished(self, finished_req_ids: set[str]) -> tuple[Optional[set[str]], Optional[set[str]]]:
        return self._lmcache_engine.get_finished(finished_req_ids)
    def get_num_new_matched_tokens(self, request: 'Request', num_computed_tokens: int) -> tuple[int, bool]:
        return (self._lmcache_engine.get_num_new_matched_tokens(request, num_computed_tokens), False)
    def update_state_after_alloc(self, request: 'Request', blocks: 'KVCacheBlocks', num_external_tokens: int):
        self._lmcache_engine.update_state_after_alloc(request, num_external_tokens)
    def build_connector_meta(self, scheduler_output: SchedulerOutput) -> KVConnectorMetadata:
        return self._lmcache_engine.build_connector_meta(scheduler_output)
    def request_finished(self, request: 'Request', block_ids: list[int]) -> tuple[bool, Optional[dict[str, Any]]]:
        return self._lmcache_engine.request_finished(request, block_ids)