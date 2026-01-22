import hashlib
import os
from dataclasses import dataclass
from typing import TYPE_CHECKING
import safetensors
import torch
from aphrodite.common.config import AphroditeConfig
from aphrodite.distributed.kv_transfer.kv_connector.v1.base import KVConnectorBase_V1, KVConnectorMetadata, KVConnectorRole
from loguru import logger
from aphrodite.v1.attention.backends.mla.common import MLACommonMetadata
from aphrodite.v1.core.sched.output import SchedulerOutput
if TYPE_CHECKING:
    from aphrodite.attention.backends.abstract import AttentionMetadata
    from aphrodite.forward_context import ForwardContext
    from aphrodite.v1.core.kv_cache_manager import KVCacheBlocks
    from aphrodite.v1.request import Request
@dataclass
class ReqMeta:
    token_ids: torch.Tensor
    slot_mapping: torch.Tensor
    is_store: bool
    mm_hashes: list[str]
    @staticmethod
    def make_meta(token_ids: list[int], block_ids: list[int], block_size: int, is_store: bool, mm_hashes: list[str]) -> 'ReqMeta':
        valid_num_tokens = align_to_block_size(len(token_ids), block_size)
        token_ids_tensor = torch.tensor(token_ids)[:valid_num_tokens]
        block_ids_tensor = torch.tensor(block_ids)
        num_blocks = block_ids_tensor.shape[0]
        block_offsets = torch.arange(0, block_size)
        slot_mapping = block_offsets.reshape((1, block_size)) + block_ids_tensor.reshape((num_blocks, 1)) * block_size
        slot_mapping = slot_mapping.flatten()[:valid_num_tokens]
        return ReqMeta(token_ids=token_ids_tensor, slot_mapping=slot_mapping, is_store=is_store, mm_hashes=mm_hashes)
@dataclass
class SharedStorageConnectorMetadata(KVConnectorMetadata):
    requests: list[ReqMeta]
    def __init__(self):
        self.requests = []
    def add_request(self, token_ids: list[int], block_ids: list[int], block_size: int, is_store: bool, mm_hashes: list[str]) -> None:
        self.requests.append(ReqMeta.make_meta(token_ids, block_ids, block_size, is_store, mm_hashes))
class SharedStorageConnector(KVConnectorBase_V1):
    def __init__(self, aphrodite_config: 'AphroditeConfig', role: KVConnectorRole):
        super().__init__(aphrodite_config=aphrodite_config, role=role)
        self._block_size = aphrodite_config.cache_config.block_size
        self._requests_need_load: dict[str, Request] = {}
        transfer_config = aphrodite_config.kv_transfer_config
        self._storage_path = transfer_config.get_from_extra_config('shared_storage_path', '/tmp')
        logger.info(aphrodite_config.kv_transfer_config)
        logger.info('Shared storage path is {}', self._storage_path)
    def start_load_kv(self, forward_context: 'ForwardContext', **kwargs) -> None:
        attn_metadata = forward_context.attn_metadata
        def inject_kv_into_layer(dst_kv_cache_layer: torch.Tensor, src_kv_cache: torch.Tensor, slot_mapping: torch.Tensor) -> None:
            dst_kv_cache_layer_shape = dst_kv_cache_layer.shape
            if isinstance(attn_metadata, MLACommonMetadata):
                num_pages = dst_kv_cache_layer_shape[0]
                page_size = dst_kv_cache_layer_shape[1]
                dst_kv_cache_layer = dst_kv_cache_layer.reshape(num_pages * page_size, -1)
                dst_kv_cache_layer[slot_mapping, ...] = src_kv_cache
                dst_kv_cache_layer.reshape(dst_kv_cache_layer_shape)
            else:
                num_pages = dst_kv_cache_layer_shape[1]
                page_size = dst_kv_cache_layer_shape[2]
                dst_kv_cache_layer = dst_kv_cache_layer.reshape(2, num_pages * page_size, -1)
                dst_kv_cache_layer[:, slot_mapping, ...] = src_kv_cache
                dst_kv_cache_layer.reshape(dst_kv_cache_layer_shape)
        metadata: KVConnectorMetadata = self._get_connector_metadata()
        assert isinstance(metadata, SharedStorageConnectorMetadata)
        if metadata is None:
            logger.warning('In connector.start_load_kv, but the connector metadata is None')
            return
        attn_metadata = forward_context.attn_metadata
        if attn_metadata is None:
            logger.warning('In connector.start_load_kv, but the attn_metadata is None')
            return
        for request in metadata.requests:
            if request.is_store:
                continue
            logger.info('Inject KV cache of {} tokens to the paged memory', len(request.slot_mapping))
            for layer_name in forward_context.no_compile_layers:
                layer = forward_context.no_compile_layers[layer_name]
                kv_cache_attr = getattr(layer, 'kv_cache', None)
                if kv_cache_attr is None:
                    continue
                kv_cache_layer = kv_cache_attr[forward_context.virtual_engine]
                filename = self._generate_filename_debug(layer_name, request.token_ids, request.mm_hashes)
                kv_cache = safetensors.torch.load_file(filename)['kv_cache'].cuda()
                inject_kv_into_layer(kv_cache_layer, kv_cache, request.slot_mapping)
    def wait_for_layer_load(self, layer_name: str) -> None:
        return
    def save_kv_layer(self, layer_name: str, kv_layer: torch.Tensor, attn_metadata: 'AttentionMetadata', **kwargs) -> None:
        def extract_kv_from_layer(layer: torch.Tensor, slot_mapping: torch.Tensor) -> torch.Tensor:
            if isinstance(attn_metadata, MLACommonMetadata):
                num_pages, page_size = (layer.shape[0], layer.shape[1])
                return layer.reshape(num_pages * page_size, -1)[slot_mapping, ...]
            num_pages, page_size = (layer.shape[1], layer.shape[2])
            return layer.reshape(2, num_pages * page_size, -1)[:, slot_mapping, ...]
        connector_metadata = self._get_connector_metadata()
        assert isinstance(connector_metadata, SharedStorageConnectorMetadata)
        for request in connector_metadata.requests:
            if request.is_store:
                filename = self._generate_filename_debug(layer_name, request.token_ids, request.mm_hashes)
                kv_cache = extract_kv_from_layer(kv_layer, request.slot_mapping)
                tensors = {'kv_cache': kv_cache.detach().cpu()}
                safetensors.torch.save_file(tensors, filename)
    def wait_for_save(self):
        return
    def get_num_new_matched_tokens(self, request: 'Request', num_computed_tokens: int) -> tuple[int, bool]:
        if not self._found_match_for_request(request):
            return (0, False)
        logger.info('External Cache Hit!')
        num_tokens_to_check = align_to_block_size(len(request.prompt_token_ids) - 1, self._block_size)
        return (num_tokens_to_check - num_computed_tokens, False)
    def update_state_after_alloc(self, request: 'Request', blocks: 'KVCacheBlocks', num_external_tokens: int):
        if num_external_tokens > 0:
            self._requests_need_load[request.request_id] = request
    def build_connector_meta(self, scheduler_output: SchedulerOutput) -> KVConnectorMetadata:
        meta = SharedStorageConnectorMetadata()
        total_need_load = 0
        for new_req in scheduler_output.scheduled_new_reqs:
            if new_req.req_id in self._requests_need_load:
                meta.add_request(token_ids=new_req.prompt_token_ids, block_ids=new_req.block_ids[0], block_size=self._block_size, is_store=False, mm_hashes=new_req.mm_hashes)
                total_need_load += 1
            elif not self._found_match_for_request(new_req):
                meta.add_request(token_ids=new_req.prompt_token_ids, block_ids=new_req.block_ids[0], block_size=self._block_size, is_store=True, mm_hashes=new_req.mm_hashes)
        cached_reqs = scheduler_output.scheduled_cached_reqs
        for i, req_id in enumerate(cached_reqs.req_ids):
            num_computed_tokens = cached_reqs.num_computed_tokens[i]
            num_new_tokens = scheduler_output.num_scheduled_tokens[req_id]
            new_block_ids = cached_reqs.new_block_ids[i]
            resumed_from_preemption = cached_reqs.resumed_from_preemption[i]
            if not resumed_from_preemption:
                break
            if req_id in self._requests_need_load:
                request = self._requests_need_load[req_id]
                total_tokens = num_computed_tokens + num_new_tokens
                token_ids = request.all_token_ids[:total_tokens]
                block_ids = new_block_ids[0]
                meta.add_request(token_ids=token_ids, block_ids=block_ids, block_size=self._block_size, is_store=False, mm_hashes=request.mm_hashes)
                total_need_load += 1
        assert total_need_load == len(self._requests_need_load)
        self._requests_need_load.clear()
        return meta
    def _found_match_for_request(self, request: 'Request') -> bool:
        num_tokens_to_check = align_to_block_size(len(request.prompt_token_ids) - 1, self._block_size)
        foldername = self._generate_foldername_debug(torch.tensor(request.prompt_token_ids)[:num_tokens_to_check], request.mm_hashes, create_folder=False)
        return os.path.exists(foldername)
    def _generate_foldername_debug(self, token_ids: torch.Tensor, mm_hashes: list[str], create_folder=False) -> str:
        token_bytes = token_ids.numpy().tobytes()
        if mm_hashes:
            mm_str = '-'.join(mm_hashes)
            token_bytes += mm_str.encode('utf-8')
        input_ids_hash = hashlib.md5(token_bytes, usedforsecurity=False).hexdigest()
        foldername = os.path.join(self._storage_path, input_ids_hash)
        if create_folder:
            os.makedirs(foldername, exist_ok=True)
        return foldername
    def _generate_filename_debug(self, layer_name: str, token_ids: torch.Tensor, mm_hashes: list[str]) -> str:
        foldername = self._generate_foldername_debug(token_ids, mm_hashes=mm_hashes, create_folder=True)
        return os.path.join(foldername, f'{layer_name}.safetensors')
def align_to_block_size(num_tokens: int, block_size) -> int:
    return (num_tokens - 1) // block_size * block_size