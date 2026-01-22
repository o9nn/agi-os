from dataclasses import dataclass
from typing import TYPE_CHECKING, Any, Optional
import regex as re
import torch
from loguru import logger
from aphrodite.common.config import AphroditeConfig
from aphrodite.distributed.kv_transfer.kv_connector.v1.base import KVConnectorBase_V1, KVConnectorMetadata, KVConnectorRole
from aphrodite.distributed.kv_transfer.kv_connector.v1.p2p.p2p_nccl_engine import P2pNcclEngine
from aphrodite.distributed.parallel_state import get_world_group
from aphrodite.v1.attention.backends.mla.common import MLACommonMetadata
from aphrodite.v1.core.sched.output import SchedulerOutput
if TYPE_CHECKING:
    from aphrodite.attention.backends.abstract import AttentionMetadata
    from aphrodite.forward_context import ForwardContext
    from aphrodite.v1.core.kv_cache_manager import KVCacheBlocks
    from aphrodite.v1.request import Request
@dataclass
class ReqMeta:
    request_id: str
    token_ids: torch.Tensor
    slot_mapping: torch.Tensor
    @staticmethod
    def make_meta(request_id: str, token_ids: list[int], block_ids: list[int], block_size: int) -> 'ReqMeta':
        valid_num_tokens = len(token_ids)
        token_ids_tensor = torch.tensor(token_ids)
        block_ids_tensor = torch.tensor(block_ids)
        num_blocks = block_ids_tensor.shape[0]
        block_offsets = torch.arange(0, block_size)
        slot_mapping = block_offsets.reshape((1, block_size)) + block_ids_tensor.reshape((num_blocks, 1)) * block_size
        slot_mapping = slot_mapping.flatten()[:valid_num_tokens]
        return ReqMeta(request_id=request_id, token_ids=token_ids_tensor, slot_mapping=slot_mapping)
@dataclass
class P2pNcclConnectorMetadata(KVConnectorMetadata):
    requests: list[ReqMeta]
    def __init__(self):
        self.requests = []
    def add_request(self, request_id: str, token_ids: list[int], block_ids: list[int], block_size: int) -> None:
        self.requests.append(ReqMeta.make_meta(request_id, token_ids, block_ids, block_size))
class P2pNcclConnector(KVConnectorBase_V1):
    def __init__(self, aphrodite_config: 'AphroditeConfig', role: KVConnectorRole):
        super().__init__(aphrodite_config=aphrodite_config, role=role)
        self._block_size = aphrodite_config.cache_config.block_size
        self._requests_need_load: dict[str, Any] = {}
        self.config = aphrodite_config.kv_transfer_config
        self.is_producer = self.config.is_kv_producer
        self.chunked_prefill: dict[str, Any] = {}
        self._rank = get_world_group().rank if role == KVConnectorRole.WORKER else 0
        self._local_rank = get_world_group().local_rank if role == KVConnectorRole.WORKER else 0
        self.p2p_nccl_engine = P2pNcclEngine(local_rank=self._local_rank, config=self.config, hostname='', port_offset=self._rank) if role == KVConnectorRole.WORKER else None
    def start_load_kv(self, forward_context: 'ForwardContext', **kwargs) -> None:
        if self.is_producer:
            return
        assert self.p2p_nccl_engine is not None
        attn_metadata = forward_context.attn_metadata
        if attn_metadata is None:
            return
        def inject_kv_into_layer(dst_kv_cache_layer: torch.Tensor, src_kv_cache: torch.Tensor, slot_mapping: torch.Tensor, request_id: str) -> None:
            dst_kv_cache_layer_shape = dst_kv_cache_layer.shape
            if isinstance(attn_metadata, MLACommonMetadata):
                num_pages = dst_kv_cache_layer_shape[0]
                page_size = dst_kv_cache_layer_shape[1]
                dst_kv_cache_layer = dst_kv_cache_layer.reshape(num_pages * page_size, -1)
                self.check_tensors_except_dim(dst_kv_cache_layer, src_kv_cache, 0)
                num_token = src_kv_cache.shape[0]
                if len(slot_mapping) == num_token:
                    dst_kv_cache_layer[slot_mapping, ...] = src_kv_cache
                else:
                    dst_kv_cache_layer[slot_mapping[:num_token], ...] = src_kv_cache
                    logger.warning('🚧src_kv_cache does not match, num_slot:{}, num_token:{}, request_id:{}', len(slot_mapping), num_token, request_id)
                dst_kv_cache_layer.reshape(dst_kv_cache_layer_shape)
            else:
                num_pages = dst_kv_cache_layer_shape[1]
                page_size = dst_kv_cache_layer_shape[2]
                dst_kv_cache_layer = dst_kv_cache_layer.reshape(2, num_pages * page_size, -1)
                self.check_tensors_except_dim(dst_kv_cache_layer, src_kv_cache, 1)
                num_token = src_kv_cache.shape[1]
                if len(slot_mapping) == num_token:
                    dst_kv_cache_layer[:, slot_mapping, ...] = src_kv_cache
                else:
                    dst_kv_cache_layer[:, slot_mapping[:num_token], ...] = src_kv_cache
                    logger.warning('🚧src_kv_cache does not match, num_slot:{}, num_token:{}, request_id:{}', len(slot_mapping), num_token, request_id)
                dst_kv_cache_layer.reshape(dst_kv_cache_layer_shape)
        metadata: KVConnectorMetadata = self._get_connector_metadata()
        assert isinstance(metadata, P2pNcclConnectorMetadata)
        if metadata is None:
            return
        for request in metadata.requests:
            for layer_name in forward_context.no_compile_layers:
                layer = forward_context.no_compile_layers[layer_name]
                kv_cache = getattr(layer, 'kv_cache', None)
                if kv_cache is None:
                    continue
                kv_cache_layer = kv_cache[forward_context.virtual_engine]
                kv_cache = self.p2p_nccl_engine.recv_tensor(request.request_id + '#' + layer_name)
                if kv_cache is None:
                    logger.warning('🚧src_kv_cache is None, {}', request.request_id)
                    continue
                inject_kv_into_layer(kv_cache_layer, kv_cache, request.slot_mapping, request.request_id)
    def wait_for_layer_load(self, layer_name: str) -> None:
        return
    def save_kv_layer(self, layer_name: str, kv_layer: torch.Tensor, attn_metadata: 'AttentionMetadata', **kwargs) -> None:
        if not self.is_producer:
            return
        assert self.p2p_nccl_engine is not None
        connector_metadata = self._get_connector_metadata()
        assert isinstance(connector_metadata, P2pNcclConnectorMetadata)
        for request in connector_metadata.requests:
            request_id = request.request_id
            ip, port = self.parse_request_id(request_id, True)
            remote_address = ip + ':' + str(port + self._rank)
            self.p2p_nccl_engine.send_tensor(request_id + '#' + layer_name, kv_layer, remote_address, request.slot_mapping, isinstance(attn_metadata, MLACommonMetadata))
    def wait_for_save(self):
        if self.is_producer:
            assert self.p2p_nccl_engine is not None
            self.p2p_nccl_engine.wait_for_sent()
    def get_finished(self, finished_req_ids: set[str], **kwargs) -> tuple[Optional[set[str]], Optional[set[str]]]:
        assert self.p2p_nccl_engine is not None
        no_compile_layers = self._aphrodite_config.compilation_config.static_forward_context
        return self.p2p_nccl_engine.get_finished(finished_req_ids, no_compile_layers)
    def get_num_new_matched_tokens(self, request: 'Request', num_computed_tokens: int) -> tuple[int, bool]:
        if self.is_producer:
            return (0, False)
        num_external_tokens = len(request.prompt_token_ids) - 1 - num_computed_tokens
        if num_external_tokens < 0:
            num_external_tokens = 0
        return (num_external_tokens, False)
    def update_state_after_alloc(self, request: 'Request', blocks: 'KVCacheBlocks', num_external_tokens: int):
        if not self.is_producer and num_external_tokens > 0:
            self._requests_need_load[request.request_id] = (request, blocks.get_block_ids()[0])
    def build_connector_meta(self, scheduler_output: SchedulerOutput) -> KVConnectorMetadata:
        meta = P2pNcclConnectorMetadata()
        for new_req in scheduler_output.scheduled_new_reqs:
            if self.is_producer:
                num_scheduled_tokens = scheduler_output.num_scheduled_tokens[new_req.req_id]
                num_tokens = num_scheduled_tokens + new_req.num_computed_tokens
                if num_tokens < len(new_req.prompt_token_ids):
                    self.chunked_prefill[new_req.req_id] = (new_req.block_ids[0], new_req.prompt_token_ids)
                    continue
                meta.add_request(request_id=new_req.req_id, token_ids=new_req.prompt_token_ids, block_ids=new_req.block_ids[0], block_size=self._block_size)
                continue
            if new_req.req_id in self._requests_need_load:
                meta.add_request(request_id=new_req.req_id, token_ids=new_req.prompt_token_ids, block_ids=new_req.block_ids[0], block_size=self._block_size)
                self._requests_need_load.pop(new_req.req_id)
        cached_reqs = scheduler_output.scheduled_cached_reqs
        for i, req_id in enumerate(cached_reqs.req_ids):
            num_computed_tokens = cached_reqs.num_computed_tokens[i]
            new_block_ids = cached_reqs.new_block_ids[i]
            resumed_from_preemption = cached_reqs.resumed_from_preemption[i]
            if self.is_producer:
                num_scheduled_tokens = scheduler_output.num_scheduled_tokens[req_id]
                num_tokens = num_scheduled_tokens + num_computed_tokens
                assert req_id in self.chunked_prefill
                block_ids = new_block_ids[0]
                if not resumed_from_preemption:
                    block_ids = self.chunked_prefill[req_id][0] + block_ids
                prompt_token_ids = self.chunked_prefill[req_id][1]
                if num_tokens < len(prompt_token_ids):
                    self.chunked_prefill[req_id] = (block_ids, prompt_token_ids)
                    continue
                meta.add_request(request_id=req_id, token_ids=prompt_token_ids, block_ids=block_ids, block_size=self._block_size)
                self.chunked_prefill.pop(req_id, None)
                continue
            if not resumed_from_preemption:
                break
            if req_id in self._requests_need_load:
                request, _ = self._requests_need_load.pop(req_id)
                total_tokens = num_computed_tokens + 1
                token_ids = request.all_token_ids[:total_tokens]
                block_ids = new_block_ids[0]
                meta.add_request(request_id=req_id, token_ids=token_ids, block_ids=block_ids, block_size=self._block_size)
        self._requests_need_load.clear()
        return meta
    def request_finished(self, request: 'Request', block_ids: list[int]) -> tuple[bool, Optional[dict[str, Any]]]:
        self.chunked_prefill.pop(request.request_id, None)
        return (False, None)
    @staticmethod
    def parse_request_id(request_id: str, is_prefill=True) -> tuple[str, int]:
        if is_prefill:
            pattern = '___decode_addr_(.*):(\\d+)'
        else:
            pattern = '___prefill_addr_(.*):(\\d+)___'
        match = re.search(pattern, request_id)
        if match:
            ip = match.group(1)
            port = int(match.group(2))
            return (ip, port)
        raise ValueError(f'Request id {request_id} does not contain hostname and port')
    @staticmethod
    def check_tensors_except_dim(tensor1, tensor2, dim):
        shape1 = tensor1.size()
        shape2 = tensor2.size()
        if len(shape1) != len(shape2) or not all((s1 == s2 for i, (s1, s2) in enumerate(zip(shape1, shape2)) if i != dim)):
            raise NotImplementedError('Currently, only symmetric TP is supported. Asymmetric TP, PP,and others will be supported in future PRs.')