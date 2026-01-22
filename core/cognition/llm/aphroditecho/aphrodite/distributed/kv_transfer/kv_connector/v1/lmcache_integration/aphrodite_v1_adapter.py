import os
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any, Optional, Union
import torch
from lmcache.logging import init_logger
from lmcache.utils import _lmcache_nvtx_annotate
from lmcache.v1.compute.blend import LMCBlenderBuilder
from lmcache.v1.lookup_client import LookupClientFactory
from lmcache.v1.offload_server.zmq_server import ZMQOffloadServer
from lmcache.v1.storage_backend.connector.nixl_connector_v3 import NixlReceiverInfo
from aphrodite.common.config import AphroditeConfig
from aphrodite.utils import cdiv
from aphrodite.distributed.kv_transfer.kv_connector.v1.base import KVConnectorBase_V1, KVConnectorMetadata, KVConnectorRole
from aphrodite.distributed.parallel_state import get_tensor_model_parallel_rank
from aphrodite.v1.core.sched.output import SchedulerOutput
from .aphrodite_adapter import init_lmcache_engine
from .utils import ENGINE_NAME, apply_mm_hashes_to_token_ids, lmcache_get_config
if TYPE_CHECKING:
    from aphrodite.attention.backends.abstract import AttentionMetadata
    from aphrodite.forward_context import ForwardContext
    from aphrodite.multimodal.inputs import PlaceholderRange
    from aphrodite.v1.core.kv_cache_manager import KVCacheManager
    from aphrodite.v1.core.sched.output import NewRequestData
    from aphrodite.v1.request import Request
@dataclass
class LoadSpec:
    aphrodite_cached_tokens: int
    lmcache_cached_tokens: int
    can_load: bool
@dataclass
class SaveSpec:
    skip_leading_tokens: int
    can_save: bool
@dataclass
class DisaggSpec:
    req_id: str
    receiver_info: NixlReceiverInfo
    is_last_prefill: bool = False
    num_transferred_tokens: int = 0
tmp_disagg_tracker: dict[str, DisaggSpec] = {}
@dataclass
class RequestTracker:
    req_id: str
    prompt_len: int
    token_ids: list[int]
    allocated_block_ids: list[int]
    num_saved_tokens: int = 0
    disagg_spec: Optional[DisaggSpec] = None
    mm_hashes: Optional[list[str]] = None
    mm_positions: Optional[list['PlaceholderRange']] = None
    is_decode_phase = False
    @_lmcache_nvtx_annotate
    @staticmethod
    def from_new_request(new_request: 'NewRequestData', num_tokens_to_compute: int, lmcache_cached_tokens: int) -> 'RequestTracker':
        unfolded_block_ids = []
        if not isinstance(new_request.block_ids[0], list):
            unfolded_block_ids = new_request.block_ids.copy()
        else:
            unfolded_block_ids = new_request.block_ids[0].copy()
        disagg_spec = tmp_disagg_tracker.pop(new_request.req_id, None)
        return RequestTracker(req_id=new_request.req_id, prompt_len=len(new_request.prompt_token_ids), token_ids=new_request.prompt_token_ids[:num_tokens_to_compute].copy(), allocated_block_ids=unfolded_block_ids, num_saved_tokens=lmcache_cached_tokens, disagg_spec=disagg_spec, mm_hashes=new_request.mm_hashes.copy(), mm_positions=new_request.mm_positions.copy())
    def update(self, new_token_ids: list[int], new_block_ids: Union[tuple[list[int], ...], list[int]]) -> None:
        self.token_ids.extend(new_token_ids)
        if len(new_block_ids) == 0:
            new_block_ids = []
        elif isinstance(new_block_ids, tuple):
            new_block_ids = new_block_ids[0]
        elif isinstance(new_block_ids, list):
            pass
        else:
            raise ValueError(f'Unsupported new_block_ids type {type(new_block_ids)}')
        self.allocated_block_ids.extend(new_block_ids)
        if len(new_token_ids) == 1:
            self.is_decode_phase = True
@dataclass
class ReqMeta:
    req_id: str
    token_ids: torch.Tensor
    slot_mapping: torch.Tensor
    is_last_prefill: bool = False
    save_spec: Optional[SaveSpec] = None
    load_spec: Optional[LoadSpec] = None
    disagg_spec: Optional[DisaggSpec] = None
    @staticmethod
    def from_request_tracker(tracker: RequestTracker, block_size: int, lmcache_chunk_size: int=256, load_spec: Optional[LoadSpec]=None, skip_save: bool=False, discard_partial_chunks: bool=True, save_decode_cache: bool=False) -> Optional['ReqMeta']:
        input_token_ids = tracker.token_ids
        input_token_len = len(input_token_ids)
        is_last_prefill = False
        if input_token_len == tracker.prompt_len:
            is_last_prefill = True
        skip_leading_tokens = tracker.num_saved_tokens
        chunk_boundary = cdiv(tracker.num_saved_tokens + 1, lmcache_chunk_size) * lmcache_chunk_size
        skip_save = tracker.disagg_spec is None and (skip_save or (tracker.num_saved_tokens > 0 and input_token_len < chunk_boundary) or (tracker.is_decode_phase and (not save_decode_cache)))
        if skip_save and load_spec is None:
            return None
        num_tokens_to_save = input_token_len // lmcache_chunk_size * lmcache_chunk_size if not is_last_prefill or discard_partial_chunks else input_token_len
        if not skip_save:
            tracker.num_saved_tokens = num_tokens_to_save
        save_spec = SaveSpec(skip_leading_tokens, not skip_save)
        token_ids = torch.tensor(input_token_ids)[:num_tokens_to_save]
        if tracker.mm_hashes:
            apply_mm_hashes_to_token_ids(token_ids, tracker.mm_hashes, tracker.mm_positions)
        num_blocks = len(tracker.allocated_block_ids)
        block_ids = torch.tensor(tracker.allocated_block_ids, dtype=torch.long)
        if len(token_ids) > num_blocks * block_size:
            logger.error('The number of tokens is more than the number of blocks.Something might be wrong in scheduling logic!')
            logger.error('Num tokens: {}, num blocks: {}, block size: {}', len(token_ids), num_blocks, block_size)
        block_offsets = torch.arange(0, block_size, dtype=torch.long)
        slot_mapping = block_offsets.reshape((1, block_size)) + block_ids.reshape((num_blocks, 1)) * block_size
        slot_mapping = slot_mapping.flatten()[:len(token_ids)]
        assert slot_mapping.dtype == torch.long
        if load_spec is not None and load_spec.can_load:
            logger.debug('Scheduled to load {} tokens for request {}', load_spec.lmcache_cached_tokens, tracker.req_id)
        else:
            load_spec = None
        return ReqMeta(req_id=tracker.req_id, token_ids=token_ids, slot_mapping=slot_mapping, is_last_prefill=is_last_prefill, save_spec=save_spec, load_spec=load_spec, disagg_spec=tracker.disagg_spec)
@dataclass
class LMCacheConnectorMetadata(KVConnectorMetadata):
    requests: list[ReqMeta] = field(default_factory=list)
    lookup_requests_in_step: list[str] = field(default_factory=list)
    @_lmcache_nvtx_annotate
    def add_request(self, req_meta: ReqMeta) -> None:
        self.requests.append(req_meta)
class LMCacheConnectorV1Impl:
    def __init__(self, aphrodite_config: 'AphroditeConfig', role: KVConnectorRole, parent: KVConnectorBase_V1):
        self._parent = parent
        self.kv_role = aphrodite_config.kv_transfer_config.kv_role
        config = lmcache_get_config()
        self.layerwise_retrievers = []
        if role == KVConnectorRole.SCHEDULER:
            self.lookup_client = LookupClientFactory.create_lookup_client(aphrodite_config, config)
            self._unfinished_requests: dict[str, Request] = {}
            self._lookup_requests_in_step: list[str] = []
        else:
            self.lmcache_engine = init_lmcache_engine(aphrodite_config.model_config, aphrodite_config.parallel_config, aphrodite_config.cache_config, aphrodite_config.scheduler_config)
            self.use_layerwise = config.use_layerwise
            self.enable_blending = config.enable_blending
            if self.enable_blending:
                self.blender = LMCBlenderBuilder.get_or_create(ENGINE_NAME, self.lmcache_engine, self.lmcache_engine.gpu_connector)
            assert self.lmcache_engine is not None
            self.lookup_server = LookupClientFactory.create_lookup_server(self.lmcache_engine, aphrodite_config)
            self.offload_server = ZMQOffloadServer(self.lmcache_engine, aphrodite_config, get_tensor_model_parallel_rank())
        self.kv_caches: dict[str, torch.Tensor] = {}
        self._block_size = aphrodite_config.cache_config.block_size
        self.load_specs: dict[str, LoadSpec] = {}
        self.kv_cache_manager: Optional[KVCacheManager] = None
        self._request_trackers: dict[str, RequestTracker] = {}
        self._discard_partial_chunks = aphrodite_config.kv_transfer_config.get_from_extra_config('discard_partial_chunks', False) or not config.save_unfull_chunk
        self._lmcache_chunk_size = config.chunk_size
        self._save_decode_cache = config.save_decode_cache
        self.skip_last_n_tokens = aphrodite_config.kv_transfer_config.get_from_extra_config('skip_last_n_tokens', 0)
        self.num_layers = aphrodite_config.model_config.get_num_layers(aphrodite_config.parallel_config)
        self.current_layer = 0
        self.force_skip_save = bool(os.environ.get('LMCACHE_FORCE_SKIP_SAVE', False))
    @_lmcache_nvtx_annotate
    def _init_kv_caches_from_forward_context(self, forward_context: 'ForwardContext'):
        for layer_name in forward_context.no_compile_layers:
            attn_layer = forward_context.no_compile_layers[layer_name]
            if not hasattr(attn_layer, 'kv_cache'):
                logger.debug('The layer {} does not have kv_cache, skip it', layer_name)
                continue
            if layer_name not in self.kv_caches:
                self.kv_caches[layer_name] = attn_layer.kv_cache[forward_context.virtual_engine]
    @_lmcache_nvtx_annotate
    def start_load_kv(self, forward_context: 'ForwardContext', **kwargs) -> None:
        self.current_layer = 0
        if len(self.kv_caches) == 0:
            self._init_kv_caches_from_forward_context(forward_context)
        metadata = self._parent._get_connector_metadata()
        assert isinstance(metadata, LMCacheConnectorMetadata)
        assert len(self.kv_caches) > 0
        kvcaches = list(self.kv_caches.values())
        attn_metadata = forward_context.attn_metadata
        if attn_metadata is None:
            logger.warning('In connector.start_load_kv, but the attn_metadata is None')
            return
        assert self.lmcache_engine is not None
        self.lmcache_engine.post_init(kvcaches=kvcaches)
        self.layerwise_retrievers = []
        for idx, request in enumerate(metadata.requests):
            if request.load_spec is None:
                continue
            last_idx = idx
        for idx, request in enumerate(metadata.requests):
            if request.load_spec is None:
                continue
            tokens = request.token_ids
            slot_mapping = request.slot_mapping.cuda()
            assert len(tokens) == len(slot_mapping)
            token_mask = torch.ones_like(tokens, dtype=torch.bool)
            masked_token_count = request.load_spec.aphrodite_cached_tokens // self._lmcache_chunk_size * self._lmcache_chunk_size
            token_mask[:masked_token_count] = False
            lmcache_cached_tokens = request.load_spec.lmcache_cached_tokens
            if self.use_layerwise:
                if idx == last_idx:
                    sync = True
                else:
                    sync = False
                if self.enable_blending:
                    self.blender.blend(tokens[:lmcache_cached_tokens], token_mask[:lmcache_cached_tokens], kvcaches=kvcaches, slot_mapping=slot_mapping[:lmcache_cached_tokens])
                else:
                    layerwise_retriever = self.lmcache_engine.retrieve_layer(tokens[:lmcache_cached_tokens], token_mask[:lmcache_cached_tokens], kvcaches=kvcaches, slot_mapping=slot_mapping[:lmcache_cached_tokens], sync=sync)
                    next(layerwise_retriever)
                    next(layerwise_retriever)
                    self.layerwise_retrievers.append(layerwise_retriever)
            else:
                ret_token_mask = self.lmcache_engine.retrieve(tokens[:lmcache_cached_tokens], token_mask[:lmcache_cached_tokens], kvcaches=kvcaches, slot_mapping=slot_mapping[:lmcache_cached_tokens])
                num_retrieved_tokens = ret_token_mask.sum().item()
                num_expected_tokens = lmcache_cached_tokens - request.load_spec.aphrodite_cached_tokens
                if num_retrieved_tokens < num_expected_tokens:
                    logger.error('The number of retrieved tokens is less than the expected number of tokens! This should not happen!')
                    logger.error('Num retrieved tokens: {}, num expected tokens: {}', num_retrieved_tokens, num_expected_tokens)
    @_lmcache_nvtx_annotate
    def wait_for_layer_load(self, layer_name: str) -> None:
        if self.layerwise_retrievers:
            logger.debug(f'Waiting for layer {self.current_layer} to be loaded')
        for layerwise_retriever in self.layerwise_retrievers:
            ret_token_mask = next(layerwise_retriever)
            if self.current_layer == self.num_layers - 1:
                assert ret_token_mask is not None
                num_retrieved_tokens = ret_token_mask.sum().item()
                logger.info(f'Retrieved {num_retrieved_tokens} tokens')
        return
    @_lmcache_nvtx_annotate
    def save_kv_layer(self, layer_name: str, kv_layer: torch.Tensor, attn_metadata: 'AttentionMetadata', **kwargs) -> None:
        if not self.use_layerwise:
            return
        if self.kv_role == 'kv_consumer':
            return
        connector_metadata = self._parent._get_connector_metadata()
        assert isinstance(connector_metadata, LMCacheConnectorMetadata)
        assert len(self.kv_caches) > 0
        kvcaches = list(self.kv_caches.values())
        if self.current_layer == 0:
            self.layerwise_storers = []
            is_first = True
            for idx, request in enumerate(connector_metadata.requests):
                save_spec = request.save_spec
                if save_spec is None or not save_spec.can_save:
                    continue
                token_ids = request.token_ids
                assert isinstance(token_ids, torch.Tensor)
                assert token_ids.is_cpu
                slot_mapping = request.slot_mapping
                assert isinstance(slot_mapping, torch.Tensor)
                assert len(slot_mapping) == len(token_ids)
                slot_mapping = slot_mapping.cuda()
                if self.kv_role == 'kv_producer':
                    skip_leading_tokens = 0
                else:
                    skip_leading_tokens = save_spec.skip_leading_tokens
                    if skip_leading_tokens == len(token_ids):
                        continue
                    skip_leading_tokens = skip_leading_tokens // self._lmcache_chunk_size * self._lmcache_chunk_size
                store_mask = torch.ones_like(token_ids, dtype=torch.bool)
                store_mask[:skip_leading_tokens] = False
                logger.info('Storing KV cache for {} out of {} tokens (skip_leading_tokens={}) for request {}', len(token_ids) - skip_leading_tokens, len(token_ids), skip_leading_tokens, request.req_id)
                layerwise_storer = self.lmcache_engine.store_layer(token_ids, mask=store_mask, kvcaches=kvcaches, slot_mapping=slot_mapping, offset=skip_leading_tokens, sync=is_first)
                self.layerwise_storers.append(layerwise_storer)
                if is_first:
                    is_first = False
        for layerwise_storer in self.layerwise_storers:
            next(layerwise_storer)
        self.current_layer += 1
    @_lmcache_nvtx_annotate
    def wait_for_save(self):
        connector_metadata = self._parent._get_connector_metadata()
        assert isinstance(connector_metadata, LMCacheConnectorMetadata)
        self.lmcache_engine.lookup_unpin(connector_metadata.lookup_requests_in_step)
        if self.kv_role == 'kv_consumer':
            return
        if self.use_layerwise:
            for layerwise_storer in self.layerwise_storers:
                next(layerwise_storer)
            return
        assert len(self.kv_caches) > 0
        kvcaches = list(self.kv_caches.values())
        assert self.lmcache_engine is not None
        for request in connector_metadata.requests:
            save_spec = request.save_spec
            if (save_spec is None or not save_spec.can_save) and self.kv_role != 'kv_producer':
                continue
            token_ids = request.token_ids
            assert isinstance(token_ids, torch.Tensor)
            assert token_ids.is_cpu
            slot_mapping = request.slot_mapping
            assert isinstance(slot_mapping, torch.Tensor)
            assert len(slot_mapping) == len(token_ids)
            slot_mapping = slot_mapping.cuda()
            skip_leading_tokens = save_spec.skip_leading_tokens
            if self.kv_role == 'kv_producer':
                skip_leading_tokens = min(skip_leading_tokens, request.disagg_spec.num_transferred_tokens)
            if skip_leading_tokens == len(token_ids):
                continue
            skip_leading_tokens = skip_leading_tokens // self._lmcache_chunk_size * self._lmcache_chunk_size
            store_mask = torch.ones_like(token_ids, dtype=torch.bool)
            store_mask[:skip_leading_tokens] = False
            logger.info('Storing KV cache for {} out of {} tokens (skip_leading_tokens={}) for request {}', len(token_ids) - skip_leading_tokens, len(token_ids), skip_leading_tokens, request.req_id)
            is_last_prefill = request.is_last_prefill
            if is_last_prefill:
                if request.disagg_spec:
                    request.disagg_spec.is_last_prefill = True
            else:
                token_len = len(token_ids)
                aligned_token_len = token_len // self._lmcache_chunk_size * self._lmcache_chunk_size
                token_ids = token_ids[:aligned_token_len]
                store_mask = store_mask[:aligned_token_len]
                slot_mapping = slot_mapping[:aligned_token_len]
            self.lmcache_engine.store(token_ids, mask=store_mask, kvcaches=kvcaches, slot_mapping=slot_mapping, offset=skip_leading_tokens, transfer_spec=request.disagg_spec)
            save_spec.skip_leading_tokens = len(token_ids)
            if request.disagg_spec:
                request.disagg_spec.num_transferred_tokens = len(token_ids)
    @_lmcache_nvtx_annotate
    def get_finished(self, finished_req_ids: set[str]) -> tuple[Optional[set[str]], Optional[set[str]]]:
        return (None, None)
    @_lmcache_nvtx_annotate
    def get_num_new_matched_tokens(self, request: 'Request', num_computed_tokens: int) -> int:
        if self.kv_role == 'kv_producer' and (not hasattr(self.lookup_client, 'supports_producer_reuse')):
            return 0
        token_ids = torch.tensor(request.prompt_token_ids)
        if request.mm_hashes:
            apply_mm_hashes_to_token_ids(token_ids, request.mm_hashes, request.mm_positions)
        self._lookup_requests_in_step.append(request.request_id)
        if self.skip_last_n_tokens > 0:
            num_external_hit_tokens = self.lookup_client.lookup(token_ids[:-self.skip_last_n_tokens], request_id=request.request_id)
        else:
            num_external_hit_tokens = self.lookup_client.lookup(token_ids, request_id=request.request_id)
        need_to_allocate = num_external_hit_tokens - num_computed_tokens
        if num_external_hit_tokens == request.num_tokens:
            need_to_allocate -= 1
        logger.info('Reqid: {}, Total tokens {}, LMCache hit tokens: {}, need to load: {}', request.request_id, request.num_tokens, num_external_hit_tokens, need_to_allocate)
        self.load_specs[request.request_id] = LoadSpec(aphrodite_cached_tokens=num_computed_tokens, lmcache_cached_tokens=num_external_hit_tokens, can_load=False)
        if need_to_allocate <= 0:
            return 0
        return need_to_allocate
    @_lmcache_nvtx_annotate
    def update_state_after_alloc(self, request: 'Request', num_external_tokens: int):
        kv_transfer_params = request.kv_transfer_params if hasattr(request, 'kv_transfer_params') else None
        if kv_transfer_params is not None and 'disagg_spec' in kv_transfer_params:
            req_disagg_spec = kv_transfer_params['disagg_spec']
            receiver_id = req_disagg_spec['receiver_host'] + str(req_disagg_spec['receiver_init_port'])
            receiver_info = NixlReceiverInfo(receiver_id=receiver_id, receiver_host=req_disagg_spec['receiver_host'], receiver_init_port=req_disagg_spec['receiver_init_port'], receiver_alloc_port=req_disagg_spec['receiver_alloc_port'])
            disagg_spec = DisaggSpec(req_id=req_disagg_spec['req_id'], receiver_info=receiver_info)
            tmp_disagg_tracker[request.request_id] = disagg_spec
        self._unfinished_requests[request.request_id] = request
        if request.request_id not in self.load_specs:
            return
        if num_external_tokens == 0:
            self.load_specs[request.request_id].can_load = False
            return
        if self.load_specs[request.request_id].lmcache_cached_tokens != request.num_tokens:
            assert num_external_tokens > 0 and num_external_tokens == self.load_specs[request.request_id].lmcache_cached_tokens - self.load_specs[request.request_id].aphrodite_cached_tokens, f'Mismatch in number of tokens: {num_external_tokens} vs {self.load_specs[request.request_id].lmcache_cached_tokens} - {self.load_specs[request.request_id].aphrodite_cached_tokens} for request {request.request_id}'
        self.load_specs[request.request_id].can_load = True
    @_lmcache_nvtx_annotate
    def build_connector_meta(self, scheduler_output: SchedulerOutput) -> KVConnectorMetadata:
        force_skip_save = self.kv_role == 'kv_consumer' or self.force_skip_save
        meta = LMCacheConnectorMetadata()
        for finished_req_id in scheduler_output.finished_req_ids:
            self._request_trackers.pop(finished_req_id, None)
            self._unfinished_requests.pop(finished_req_id, None)
        for request in scheduler_output.scheduled_new_reqs:
            load_spec = self.load_specs.pop(request.req_id, None)
            num_tokens_to_compute = request.num_computed_tokens + scheduler_output.num_scheduled_tokens[request.req_id]
            lmcache_cached_tokens = 0
            if load_spec is not None:
                lmcache_cached_tokens = load_spec.lmcache_cached_tokens
            request_tracker = RequestTracker.from_new_request(request, num_tokens_to_compute, lmcache_cached_tokens)
            self._request_trackers[request.req_id] = request_tracker
            req_meta = ReqMeta.from_request_tracker(request_tracker, self._block_size, self._lmcache_chunk_size, load_spec=load_spec, skip_save=force_skip_save, discard_partial_chunks=self._discard_partial_chunks, save_decode_cache=self._save_decode_cache)
            if req_meta is not None:
                meta.add_request(req_meta)
        cached_reqs = scheduler_output.scheduled_cached_reqs
        if isinstance(cached_reqs, list):
            for i, req in enumerate(cached_reqs):
                request_tracker = self._request_trackers[req.req_id]
                request_tracker.update(req.new_token_ids, req.new_block_ids)
                req_meta = ReqMeta.from_request_tracker(request_tracker, self._block_size, self._lmcache_chunk_size, load_spec=None, skip_save=force_skip_save, discard_partial_chunks=self._discard_partial_chunks)
                if req_meta is not None:
                    meta.add_request(req_meta)
            return meta
        for i, req_id in enumerate(cached_reqs.req_ids):
            request_tracker = self._request_trackers[req_id]
            num_new_tokens = scheduler_output.num_scheduled_tokens[req_id]
            if (request := self._unfinished_requests.get(req_id)):
                num_current_tokens = len(request_tracker.token_ids)
                new_token_ids = request.all_token_ids[num_current_tokens:num_current_tokens + num_new_tokens]
            else:
                raise ValueError(f'Request {req_id} is not in _unfinished_requests, but it is scheduled to be cached')
            new_block_ids = cached_reqs.new_block_ids[i]
            request_tracker.update(new_token_ids, new_block_ids)
            req_meta = ReqMeta.from_request_tracker(request_tracker, self._block_size, self._lmcache_chunk_size, load_spec=None, skip_save=force_skip_save, discard_partial_chunks=self._discard_partial_chunks, save_decode_cache=self._save_decode_cache)
            if req_meta is not None:
                meta.add_request(req_meta)
        meta.lookup_requests_in_step = self._lookup_requests_in_step
        self._lookup_requests_in_step = []
        return meta
    @_lmcache_nvtx_annotate
    def request_finished(self, request: 'Request', block_ids: list[int]) -> tuple[bool, Optional[dict[str, Any]]]:
        params = request.kv_transfer_params if hasattr(request, 'kv_transfer_params') else None
        return_params = None
        if params is not None and 'ret_first_tok' in params:
            return_params = {'first_tok': request._output_token_ids[0]}
        return (0, return_params)