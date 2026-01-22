import contextlib
import logging
import math
import queue
import threading
import time
import uuid
from collections import defaultdict
from collections.abc import Iterator
from concurrent.futures import Future, ThreadPoolExecutor
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any, Optional
import msgspec
import torch
import zmq
from loguru import logger
from aphrodite.attention.selector import backend_name_to_enum, get_attn_backend
from aphrodite.common import envs
from aphrodite.common.config import AphroditeConfig
from aphrodite.common.logger import log_once
from aphrodite.distributed.kv_transfer.kv_connector.v1.base import CopyBlocksOp, KVConnectorBase_V1, KVConnectorMetadata, KVConnectorRole
from aphrodite.distributed.parallel_state import get_tensor_model_parallel_rank, get_tensor_model_parallel_world_size, get_tp_group
from aphrodite.distributed.utils import divide
from aphrodite.forward_context import ForwardContext
from aphrodite.platforms import _Backend, current_platform
from aphrodite.utils import make_zmq_path, make_zmq_socket, round_down
from aphrodite.v1.core.sched.output import SchedulerOutput
from aphrodite.v1.request import RequestStatus
if TYPE_CHECKING:
    from aphrodite.attention.backends.abstract import AttentionMetadata
    from aphrodite.v1.core.kv_cache_manager import KVCacheBlocks
    from aphrodite.v1.request import Request
Transfer = tuple[int, float]
EngineId = str
ReqId = str
GET_META_MSG = b'get_meta_msg'
try:
    from nixl._api import nixl_agent as NixlWrapper
    logger.info('NIXL is available')
except ImportError:
    logger.warning('NIXL is not available')
    NixlWrapper = None
_NIXL_SUPPORTED_XPUS = {'cuda': ('cuda',), 'tpu': ('cpu',)}
class NixlAgentMetadata(msgspec.Struct, omit_defaults=True, dict=True):
    engine_id: str
    agent_metadata: bytes
    kv_caches_base_addr: list[int]
    num_blocks: int
    block_len: int
    attn_backend_name: str
@dataclass
class ReqMeta:
    local_block_ids: list[int]
    remote_block_ids: list[int]
    remote_host: str
    remote_port: int
    remote_engine_id: str
    tp_size: int
class NixlConnectorMetadata(KVConnectorMetadata):
    def __init__(self):
        self.reqs_to_recv: dict[ReqId, ReqMeta] = {}
        self.reqs_to_save: dict[ReqId, ReqMeta] = {}
        self.reqs_to_send: dict[ReqId, float] = {}
    def add_new_req(self, request_id: ReqId, local_block_ids: list[int], kv_transfer_params: dict[str, Any], load_remote_cache: bool=True, save_to_host: bool=False):
        assert load_remote_cache ^ save_to_host
        _req = ReqMeta(local_block_ids=local_block_ids, remote_block_ids=kv_transfer_params['remote_block_ids'], remote_engine_id=kv_transfer_params['remote_engine_id'], remote_host=kv_transfer_params['remote_host'], remote_port=kv_transfer_params['remote_port'], tp_size=kv_transfer_params.get('tp_size', 1))
        if save_to_host:
            self.reqs_to_save[request_id] = _req
        if load_remote_cache:
            self.reqs_to_recv[request_id] = _req
class NixlConnector(KVConnectorBase_V1):
    def __init__(self, aphrodite_config: AphroditeConfig, role: KVConnectorRole):
        assert aphrodite_config.kv_transfer_config is not None
        assert aphrodite_config.kv_transfer_config.engine_id is not None
        self.engine_id: EngineId = aphrodite_config.kv_transfer_config.engine_id
        if role == KVConnectorRole.SCHEDULER:
            self.connector_scheduler: Optional[NixlConnectorScheduler] = NixlConnectorScheduler(aphrodite_config, self.engine_id)
            self.connector_worker: Optional[NixlConnectorWorker] = None
        elif role == KVConnectorRole.WORKER:
            self.connector_scheduler = None
            self.connector_worker = NixlConnectorWorker(aphrodite_config, self.engine_id)
    @classmethod
    def get_required_kvcache_layout(cls, aphrodite_config: AphroditeConfig):
        if aphrodite_config.model_config is None:
            log_once('WARNING', 'Unable to detect current Aphrodite config. Fallback to default kv cache layout.')
            return None
        use_mla = aphrodite_config.model_config.use_mla
        if use_mla:
            return None
        log_once('INFO', 'NixlConnector setting KV cache layout to HND for better xfer performance.')
        return 'HND'
    def get_num_new_matched_tokens(self, request: 'Request', num_computed_tokens: int) -> tuple[int, bool]:
        assert self.connector_scheduler is not None
        return self.connector_scheduler.get_num_new_matched_tokens(request, num_computed_tokens)
    def update_state_after_alloc(self, request: 'Request', blocks: 'KVCacheBlocks', num_external_tokens: int):
        assert self.connector_scheduler is not None
        return self.connector_scheduler.update_state_after_alloc(request, blocks, num_external_tokens)
    def build_connector_meta(self, scheduler_output: SchedulerOutput) -> KVConnectorMetadata:
        assert self.connector_scheduler is not None
        return self.connector_scheduler.build_connector_meta(scheduler_output)
    def request_finished(self, request: 'Request', block_ids: list[int]) -> tuple[bool, Optional[dict[str, Any]]]:
        assert self.connector_scheduler is not None
        return self.connector_scheduler.request_finished(request, block_ids)
    def register_kv_caches(self, kv_caches: dict[str, torch.Tensor]):
        assert self.connector_worker is not None
        self.connector_worker.register_kv_caches(kv_caches)
    def set_host_xfer_buffer_ops(self, copy_operation: CopyBlocksOp):
        assert self.connector_worker is not None
        self.connector_worker.set_host_xfer_buffer_ops(copy_operation)
    def get_finished(self, finished_req_ids: set[str]) -> tuple[set[str], set[str]]:
        assert self.connector_worker is not None
        return self.connector_worker.get_finished()
    def start_load_kv(self, forward_context: 'ForwardContext', **kwargs) -> None:
        assert self.connector_worker is not None
        assert isinstance(self._connector_metadata, NixlConnectorMetadata)
        self.connector_worker.start_load_kv(self._connector_metadata)
    def wait_for_layer_load(self, layer_name: str) -> None:
        pass
    def save_kv_layer(self, layer_name: str, kv_layer: torch.Tensor, attn_metadata: 'AttentionMetadata', **kwargs) -> None:
        pass
    def wait_for_save(self):
        assert self.connector_worker is not None
        assert isinstance(self._connector_metadata, NixlConnectorMetadata)
        if self.connector_worker.use_host_buffer and self.connector_worker.copy_blocks:
            self.connector_worker.save_kv_to_host(self._connector_metadata)
class NixlConnectorScheduler:
    def __init__(self, aphrodite_config: AphroditeConfig, engine_id: str):
        self.aphrodite_config = aphrodite_config
        self.block_size = aphrodite_config.cache_config.block_size
        self.engine_id: EngineId = engine_id
        self.side_channel_host = envs.APHRODITE_NIXL_SIDE_CHANNEL_HOST
        self.side_channel_port = envs.APHRODITE_NIXL_SIDE_CHANNEL_PORT + aphrodite_config.parallel_config.data_parallel_rank * aphrodite_config.parallel_config.tensor_parallel_size
        self.use_host_buffer = aphrodite_config.kv_transfer_config.kv_buffer_device == 'cpu'
        logger.info('Initializing NIXL Scheduler {}', engine_id)
        self._reqs_need_recv: dict[ReqId, tuple[Request, list[int]]] = {}
        self._reqs_need_save: dict[ReqId, tuple[Request, list[int]]] = {}
        self._reqs_need_send: dict[ReqId, float] = {}
    def get_num_new_matched_tokens(self, request: 'Request', num_computed_tokens: int) -> tuple[int, bool]:
        params = request.kv_transfer_params
        logger.debug('NIXLConnector get_num_new_matched_tokens: num_computed_tokens={}, kv_transfer_params={}', num_computed_tokens, params)
        if params is not None and params.get('do_remote_prefill'):
            assert num_computed_tokens % self.block_size == 0
            rounded_num_prompt_tokens = round_down(len(request.prompt_token_ids), self.block_size)
            count = max(rounded_num_prompt_tokens - num_computed_tokens, 0)
            if count > 0:
                return (count, True)
        return (0, False)
    def update_state_after_alloc(self, request: 'Request', blocks: 'KVCacheBlocks', num_external_tokens: int):
        params = request.kv_transfer_params
        logger.debug('NIXLConnector update_state_after_alloc: num_external_tokens={}, kv_transfer_params={}', num_external_tokens, params)
        if not params:
            return
        if self.use_host_buffer and params.get('do_remote_decode'):
            block_ids = blocks.get_block_ids()[0]
            all_full = request.num_tokens % self.block_size == 0
            full_block_ids = block_ids if all_full else block_ids[:-1]
            if full_block_ids:
                self._reqs_need_save[request.request_id] = (request, full_block_ids)
        elif params.get('do_remote_prefill'):
            if params.get('remote_block_ids'):
                if all((p in params for p in ('remote_engine_id', 'remote_host', 'remote_port'))):
                    local_block_ids = blocks.get_unhashed_block_ids() if num_external_tokens > 0 else []
                    self._reqs_need_recv[request.request_id] = (request, local_block_ids)
                else:
                    logger.warning('Got invalid KVTransferParams: {}. This request will not utilize KVTransfer', params)
            else:
                assert num_external_tokens == 0
            params['do_remote_prefill'] = False
    def build_connector_meta(self, scheduler_output: SchedulerOutput) -> KVConnectorMetadata:
        meta = NixlConnectorMetadata()
        for req_id, (req, block_ids) in self._reqs_need_recv.items():
            assert req.kv_transfer_params is not None
            meta.add_new_req(request_id=req_id, local_block_ids=block_ids, kv_transfer_params=req.kv_transfer_params)
        for req_id, (req, block_ids) in self._reqs_need_save.items():
            assert req.kv_transfer_params is not None
            meta.add_new_req(request_id=req_id, local_block_ids=block_ids, kv_transfer_params=req.kv_transfer_params, load_remote_cache=False, save_to_host=True)
        meta.reqs_to_send = self._reqs_need_send
        self._reqs_need_recv.clear()
        self._reqs_need_save.clear()
        self._reqs_need_send = {}
        return meta
    def request_finished(self, request: 'Request', block_ids: list[int]) -> tuple[bool, Optional[dict[str, Any]]]:
        params = request.kv_transfer_params
        logger.debug('NIXLConnector request_finished, request_status={}, kv_transfer_params={}', request.status, params)
        if not params:
            return (False, None)
        if params.get('do_remote_prefill'):
            self._reqs_need_recv[request.request_id] = (request, [])
            params['do_remote_prefill'] = False
            return (False, None)
        if not params.get('do_remote_decode') or request.status != RequestStatus.FINISHED_LENGTH_CAPPED:
            return (False, None)
        all_full = request.num_computed_tokens % self.block_size == 0
        computed_block_ids = block_ids if all_full else block_ids[:-1]
        delay_free_blocks = len(computed_block_ids) > 0
        if delay_free_blocks:
            self._reqs_need_send[request.request_id] = time.perf_counter() + envs.APHRODITE_NIXL_ABORT_REQUEST_TIMEOUT
        return (delay_free_blocks, dict(do_remote_prefill=True, do_remote_decode=False, remote_block_ids=computed_block_ids, remote_engine_id=self.engine_id, remote_host=self.side_channel_host, remote_port=self.side_channel_port, tp_size=self.aphrodite_config.parallel_config.tensor_parallel_size))
class NixlConnectorWorker:
    def __init__(self, aphrodite_config: AphroditeConfig, engine_id: str):
        if NixlWrapper is None:
            logger.error('NIXL is not available')
            raise RuntimeError('NIXL is not available')
        logger.info('Initializing NIXL wrapper')
        logger.info('Initializing NIXL worker {}', engine_id)
        self.aphrodite_config = aphrodite_config
        self.block_size = aphrodite_config.cache_config.block_size
        self.nixl_wrapper = NixlWrapper(str(uuid.uuid4()), None)
        self._remote_agents: dict[EngineId, dict[int, str]] = defaultdict(dict)
        self.side_channel_port: int = envs.APHRODITE_NIXL_SIDE_CHANNEL_PORT + aphrodite_config.parallel_config.data_parallel_rank * aphrodite_config.parallel_config.tensor_parallel_size
        self.engine_id: EngineId = engine_id
        self.tp_rank = get_tensor_model_parallel_rank()
        self.world_size = get_tensor_model_parallel_world_size()
        self.tp_group = get_tp_group()
        self.num_blocks = 0
        self.device_type = current_platform.device_type
        self.kv_buffer_device: str = aphrodite_config.kv_transfer_config.kv_buffer_device
        if self.device_type not in _NIXL_SUPPORTED_XPUS:
            raise RuntimeError(f'{self.device_type} is not supported.')
        elif self.kv_buffer_device not in _NIXL_SUPPORTED_XPUS[self.device_type]:
            raise RuntimeError(f'{self.device_type} with {self.kv_buffer_device} kv_buffer is not supported.')
        self.device_kv_caches: dict[str, torch.Tensor] = {}
        self.host_xfer_buffers: dict[str, torch.Tensor] = {}
        self.use_host_buffer = self.kv_buffer_device == 'cpu'
        if self.kv_buffer_device == 'cuda':
            self.nixl_memory_type = 'VRAM'
        elif self.kv_buffer_device == 'cpu':
            self.nixl_memory_type = 'DRAM'
        else:
            raise RuntimeError(f'{self.device_type} with {self.kv_buffer_device} kv_buffer is not supported.')
        self.copy_blocks: Optional[CopyBlocksOp] = None
        self.kv_caches_base_addr: dict[EngineId, list[int]] = {}
        self.num_regions = 0
        self.num_layers = 0
        self.src_xfer_side_handle: int = 0
        self.dst_xfer_side_handles: dict[EngineId, int] = {}
        self.dst_num_blocks: dict[EngineId, int] = {}
        self._registered_descs: list[Any] = []
        self._recving_metadata: dict[ReqId, ReqMeta] = {}
        self._recving_transfers = defaultdict[ReqId, list[Transfer]](list)
        self._reqs_to_send: dict[ReqId, float] = {}
        self._nixl_handshake_listener_t: Optional[threading.Thread] = None
        self._handshake_initiation_executor = ThreadPoolExecutor(max_workers=1, thread_name_prefix='aphrodite-nixl-handshake-initiator')
        self._ready_requests = queue.Queue[tuple[ReqId, ReqMeta]]()
        self._handshake_futures: dict[EngineId, Future[dict[int, str]]] = {}
        self._handshake_lock = threading.RLock()
        self.aphrodite_config = aphrodite_config
        self.block_size = aphrodite_config.cache_config.block_size
        self.model_config = aphrodite_config.model_config
        self.cache_config = aphrodite_config.cache_config
        self.block_window_per_layer: list[Optional[int]] = []
        self.use_mla = self.model_config.use_mla
        backend = get_attn_backend(self.model_config.get_head_size(), self.model_config.dtype, self.cache_config.cache_dtype, self.block_size, self.model_config.is_attention_free, use_mla=self.use_mla)
        self.backend_name = backend.get_name()
        attn_backend = backend_name_to_enum(self.backend_name)
        self._use_flashinfer = attn_backend == _Backend.FLASHINFER_APHRODITE_V1
        self._use_pallas_v1 = attn_backend == _Backend.PALLAS_APHRODITE_V1
        logger.debug('Detected attention backend {}', self.backend_name)
        self._tp_size: dict[EngineId, int] = {self.engine_id: self.world_size}
        self.consumer_notification_counts_by_req = defaultdict[ReqId, int](int)
    def __del__(self):
        self._handshake_initiation_executor.shutdown(wait=False)
        if self._nixl_handshake_listener_t:
            self._nixl_handshake_listener_t.join(timeout=0)
    @staticmethod
    def _nixl_handshake_listener(metadata: NixlAgentMetadata, ready_event: threading.Event, base_port: int, tp_rank: int):
        encoder = msgspec.msgpack.Encoder()
        encoded_data = encoder.encode(metadata)
        size_in_bytes = len(encoded_data)
        logger.debug('Size of encoded NixlAgentMetadata: {} bytes', str(size_in_bytes))
        host = envs.APHRODITE_NIXL_SIDE_CHANNEL_HOST
        path = make_zmq_path('tcp', host, base_port + tp_rank)
        logger.debug('Starting listening on path: {}', path)
        with zmq_ctx(zmq.ROUTER, path) as sock:
            ready_event.set()
            while True:
                identity, _, msg = sock.recv_multipart()
                if msg != GET_META_MSG:
                    logger.warning('Connection listener got unexpected message {}', msg)
                sock.send_multipart((identity, b'', encoded_data))
    def _nixl_handshake(self, host: str, port: int, remote_tp_size: int, expected_engine_id: str) -> dict[int, str]:
        start_time = time.perf_counter()
        tp_ratio = self._tp_size[self.engine_id] // remote_tp_size
        p_remote_rank = self.tp_rank // tp_ratio
        path = make_zmq_path('tcp', host, port + p_remote_rank)
        logger.debug('Querying metadata on path: {} at remote rank {}', path, p_remote_rank)
        with zmq_ctx(zmq.REQ, path) as sock:
            sock.send(GET_META_MSG)
            metadata_bytes = sock.recv()
            decoder = msgspec.msgpack.Decoder(NixlAgentMetadata)
            metadata = decoder.decode(metadata_bytes)
            got_metadata_time = time.perf_counter()
            logger.debug('NIXL handshake: get metadata took: {}', got_metadata_time - start_time)
            if metadata.engine_id != expected_engine_id:
                raise RuntimeError(f'Remote NIXL agent engine ID mismatch. Expected {expected_engine_id},received {metadata.engine_id}.')
            remote_agent_name = self.add_remote_agent(metadata, p_remote_rank, remote_tp_size)
            setup_agent_time = time.perf_counter()
            logger.debug('NIXL handshake: add agent took: {}', setup_agent_time - got_metadata_time)
        return {p_remote_rank: remote_agent_name}
    def initialize_host_xfer_buffer(self, kv_caches: dict[str, torch.Tensor]) -> None:
        xfer_buffers: dict[str, torch.Tensor] = {}
        try:
            for layer_name, kv_cache in kv_caches.items():
                kv_shape = kv_cache.shape
                kv_dtype = kv_cache.dtype
                xfer_buffers[layer_name] = torch.empty(kv_shape, dtype=kv_dtype, device='cpu')
        except MemoryError as e:
            logger.error('NIXLConnectorWorker gets {}.', e)
            raise
        self.host_xfer_buffers = xfer_buffers
    def set_host_xfer_buffer_ops(self, copy_operation: CopyBlocksOp):
        assert self.use_host_buffer
        self.copy_blocks = copy_operation
    def _background_nixl_handshake(self, req_id: str, remote_engine_id: EngineId, meta: ReqMeta):
        fut = self._handshake_futures.get(remote_engine_id)
        if fut is None:
            fut = self._handshake_initiation_executor.submit(self._nixl_handshake, meta.remote_host, meta.remote_port, meta.tp_size, remote_engine_id)
            self._handshake_futures[remote_engine_id] = fut
            def done_callback(f: Future[dict[int, str]], eid=remote_engine_id):
                with self._handshake_lock:
                    del self._handshake_futures[eid]
                    try:
                        self._remote_agents[eid] = f.result()
                    except Exception:
                        logger.exception('Handshake with {} failed', eid)
            fut.add_done_callback(done_callback)
        def request_ready(_f: Future[Any], entry=(req_id, meta)):
            self._ready_requests.put(entry)
        fut.add_done_callback(request_ready)
    def register_kv_caches(self, kv_caches: dict[str, torch.Tensor]):
        _, first_kv_cache = next(iter(kv_caches.items()))
        kv_elem_size = first_kv_cache.element_size()
        if self.use_host_buffer:
            self.initialize_host_xfer_buffer(kv_caches=kv_caches)
            assert len(self.host_xfer_buffers) == len(kv_caches), f'host_buffer: {len(self.host_xfer_buffers)}, kv_caches: {len(kv_caches)}'
            xfer_buffers = self.host_xfer_buffers
        else:
            xfer_buffers = kv_caches
            assert not self.host_xfer_buffers, f'host_xfer_buffer should not be initialized when kv_buffer_device is {self.kv_buffer_device}'
        use_mla = len(first_kv_cache.shape) == 3
        if self.device_type == 'tpu':
            assert not use_mla, f'{self.kv_buffer_device} does not support MLA.'
            assert self._use_pallas_v1, f'attn backend: {self.backend_name}'
            self.num_blocks = first_kv_cache.shape[0]
            block_rank = 3
            block_shape = first_kv_cache.shape[-block_rank:]
            block_size, n_kv_heads_x_2, head_dim = block_shape
            self.slot_size_bytes = kv_elem_size * n_kv_heads_x_2 * head_dim
        elif self.device_type == 'cuda':
            assert use_mla == self.use_mla
            if use_mla:
                self.num_blocks = first_kv_cache.shape[0]
                block_rank = 2
                block_shape = first_kv_cache.shape[-block_rank:]
                block_size, kv_latent_dim = block_shape
                self.slot_size_bytes = kv_elem_size * kv_latent_dim
            else:
                if self._use_flashinfer:
                    self.num_blocks = first_kv_cache.shape[0]
                    block_rank = 4
                else:
                    self.num_blocks = first_kv_cache.shape[1]
                    block_rank = 3
                block_shape = first_kv_cache.shape[-block_rank:]
                block_size, n_kv_heads, head_dim = block_shape[-3:]
                self.slot_size_bytes = kv_elem_size * n_kv_heads * head_dim
            assert block_size == self.block_size
        else:
            raise RuntimeError(f'{self.device_type} ({self.backend_name}) is not supported.')
        self.block_len = kv_elem_size * math.prod(block_shape)
        logger.info('Registering KV_Caches. use_mla: {}, kv_buffer_device: {}, use_host_buffer: {}, num_blocks: {}, block_shape: {}, per_layer_kv_cache_shape: {}', use_mla, self.kv_buffer_device, self.use_host_buffer, self.num_blocks, block_shape, first_kv_cache.shape)
        self.dst_num_blocks[self.engine_id] = self.num_blocks
        self.device_kv_caches = kv_caches
        kv_caches_base_addr = []
        caches_data = []
        for cache_or_caches in xfer_buffers.values():
            cache_list = [cache_or_caches] if use_mla or self._use_pallas_v1 or self._use_flashinfer else cache_or_caches
            for cache in cache_list:
                base_addr = cache.data_ptr()
                region_len = self.num_blocks * self.block_len
                caches_data.append((base_addr, region_len, self.tp_rank, ''))
                kv_caches_base_addr.append(base_addr)
        self.kv_caches_base_addr[self.engine_id] = kv_caches_base_addr
        self.num_regions = len(caches_data)
        self.num_layers = len(xfer_buffers.keys())
        if self.aphrodite_config.model_config.hf_config.model_type == 'llama4':
            from transformers import Llama4TextConfig
            assert isinstance(self.aphrodite_config.model_config.hf_text_config, Llama4TextConfig)
            llama4_config = self.aphrodite_config.model_config.hf_text_config
            no_rope_layers = llama4_config.no_rope_layers
            chunk_size = llama4_config.attention_chunk_size
            chunk_block_size = math.ceil(chunk_size / self.block_size)
            for layer_idx in range(self.num_layers):
                is_local_attention = no_rope_layers[layer_idx] != 0
                block_window = chunk_block_size if is_local_attention else None
                self.block_window_per_layer.append(block_window)
            logger.debug('Llama 4 block window per layer mapping: {}', self.block_window_per_layer)
            assert len(self.block_window_per_layer) == self.num_layers
        descs = self.nixl_wrapper.get_reg_descs(caches_data, self.nixl_memory_type)
        logger.debug('Registering descs: {}', caches_data)
        self.nixl_wrapper.register_memory(descs)
        logger.debug('Done registering descs')
        self._registered_descs.append(descs)
        blocks_data = []
        for base_addr in self.kv_caches_base_addr[self.engine_id]:
            for block_id in range(self.num_blocks):
                block_offset = block_id * self.block_len
                addr = base_addr + block_offset
                blocks_data.append((addr, self.block_len, self.tp_rank))
        logger.debug('Created {} blocks for src engine {} and rank {}', len(blocks_data), self.engine_id, self.tp_rank)
        descs = self.nixl_wrapper.get_xfer_descs(blocks_data, self.nixl_memory_type)
        self.src_xfer_side_handle = self.nixl_wrapper.prep_xfer_dlist('NIXL_INIT_AGENT', descs)
        metadata = NixlAgentMetadata(engine_id=self.engine_id, agent_metadata=self.nixl_wrapper.get_agent_metadata(), kv_caches_base_addr=self.kv_caches_base_addr[self.engine_id], num_blocks=self.num_blocks, block_len=self.block_len, attn_backend_name=self.backend_name)
        ready_event = threading.Event()
        self._nixl_handshake_listener_t = threading.Thread(target=self._nixl_handshake_listener, args=(metadata, ready_event, self.side_channel_port, self.tp_rank), daemon=True, name='nixl_handshake_listener')
        self._nixl_handshake_listener_t.start()
        ready_event.wait()
    def add_remote_agent(self, nixl_agent_meta: NixlAgentMetadata, remote_tp_rank: int=0, remote_tp_size: int=1) -> str:
        engine_id = nixl_agent_meta.engine_id
        if remote_tp_rank in self._remote_agents.get(engine_id, {}):
            return self._remote_agents[engine_id][remote_tp_rank]
        if engine_id not in self._tp_size:
            self._tp_size[engine_id] = remote_tp_size
        else:
            assert self._tp_size[engine_id] == remote_tp_size
        assert nixl_agent_meta.attn_backend_name == self.backend_name
        remote_agent_name = self.nixl_wrapper.add_remote_agent(nixl_agent_meta.agent_metadata)
        tp_ratio = divide(self._tp_size[self.engine_id], self._tp_size[engine_id])
        assert tp_ratio > 0, 'Decode TP cannot be smaller than prefill TP'
        assert not self._use_pallas_v1 or tp_ratio == 1, 'TPU (pallas_v1) DOES NOT support heterogeneous TP yet.'
        total_num_kv_heads = self.model_config.get_total_num_kv_heads()
        is_kv_replicated = self._tp_size[engine_id] // total_num_kv_heads >= 1
        if self.use_mla or is_kv_replicated:
            remote_block_size = nixl_agent_meta.block_len // self.slot_size_bytes
            assert self.block_len == nixl_agent_meta.block_len
        else:
            remote_block_size = nixl_agent_meta.block_len // (self.slot_size_bytes * tp_ratio)
            if self._use_flashinfer:
                remote_block_size //= 2
            assert nixl_agent_meta.block_len == self.block_len * tp_ratio, 'Remote P worker KV layer cache must be of shape [2, N, local_kv_heads*tp_ratio, block_size, head_dim] and same dtype.'
        assert self.block_size == remote_block_size, f'Remote P worker with different block size is not supported self.block_size={self.block_size!r} remote_block_size={remote_block_size!r}'
        if engine_id in self.dst_num_blocks:
            assert self.dst_num_blocks[engine_id] == nixl_agent_meta.num_blocks
        else:
            self.dst_num_blocks[engine_id] = nixl_agent_meta.num_blocks
        blocks_data = []
        self.kv_caches_base_addr[engine_id] = nixl_agent_meta.kv_caches_base_addr
        rank_offset = self.tp_rank % tp_ratio * self.block_len if not (self.use_mla or is_kv_replicated) else 0
        for base_addr in nixl_agent_meta.kv_caches_base_addr:
            for block_id in range(nixl_agent_meta.num_blocks):
                block_offset = block_id * nixl_agent_meta.block_len
                addr = base_addr + block_offset + rank_offset
                blocks_data.append((addr, self.block_len, remote_tp_rank))
        logger.debug('Created {} blocks for dst engine {} with remote rank {} and local rank {}', len(blocks_data), engine_id, remote_tp_rank, self.tp_rank)
        descs = self.nixl_wrapper.get_xfer_descs(blocks_data, self.nixl_memory_type)
        self.dst_xfer_side_handles[engine_id] = self.nixl_wrapper.prep_xfer_dlist(remote_agent_name, descs)
        return remote_agent_name
    def sync_recved_kv_to_device(self, req_id: str, meta: ReqMeta):
        assert self.use_host_buffer
        assert self.copy_blocks is not None
        local_block_ids = meta.local_block_ids
        self.copy_blocks(self.host_xfer_buffers, self.device_kv_caches, local_block_ids, local_block_ids, 'h2d')
        if logging.getLogger().isEnabledFor(logging.DEBUG):
            logger.debug('synced recved kv of request[{}] to device kv buffer,local_block_ids: {}. ', req_id, ','.join(map(str, meta.local_block_ids)))
    def save_kv_to_host(self, metadata: NixlConnectorMetadata):
        assert self.use_host_buffer
        assert self.copy_blocks is not None
        for req_id, meta in metadata.reqs_to_save.items():
            if logging.getLogger().isEnabledFor(logging.DEBUG):
                logger.debug('save_load_kv for request[{}] to host xfer buffer.local_block_ids: {}. ', req_id, ','.join(map(str, meta.local_block_ids)))
            self.copy_blocks(self.device_kv_caches, self.host_xfer_buffers, meta.local_block_ids, meta.local_block_ids, 'd2h')
    def get_finished(self) -> tuple[set[str], set[str]]:
        done_sending = self._get_new_notifs()
        done_recving = self._pop_done_transfers(self._recving_transfers)
        if len(done_sending) > 0 or len(done_recving) > 0:
            logger.debug('Rank {}, get_finished: {} requests done sending and {} requests done recving', self.tp_rank, len(done_sending), len(done_recving))
        if self.use_host_buffer:
            for req_id in done_recving:
                meta = self._recving_metadata.pop(req_id)
                assert meta, f'{req_id} not found in recving_metadata list'
                self.sync_recved_kv_to_device(req_id, meta)
        now = time.perf_counter()
        while self._reqs_to_send:
            req_id, expires = next(iter(self._reqs_to_send.items()))
            if now < expires:
                break
            count = self.consumer_notification_counts_by_req.pop(req_id, 0)
            logger.warning('Releasing expired KV blocks for request {} which were retrieved by {} decode worker(s) within {} seconds.', req_id, count, envs.APHRODITE_NIXL_ABORT_REQUEST_TIMEOUT)
            del self._reqs_to_send[req_id]
            done_sending.add(req_id)
        return (done_sending, done_recving)
    def _get_new_notifs(self) -> set[str]:
        notified_req_ids: set[str] = set()
        for notifs in self.nixl_wrapper.get_new_notifs().values():
            for notif in notifs:
                req_id, tp_ratio = notif.decode('utf-8').rsplit(':', 1)
                if req_id not in self._reqs_to_send:
                    logger.error('Potentially invalid KV blocks for unrecognized request {} were retrieved by a decode worker. They may have expired.', req_id)
                    continue
                self.consumer_notification_counts_by_req[req_id] += 1
                if self.consumer_notification_counts_by_req[req_id] == int(tp_ratio):
                    notified_req_ids.add(req_id)
                    del self.consumer_notification_counts_by_req[req_id]
                    del self._reqs_to_send[req_id]
        return notified_req_ids
    def _pop_done_transfers(self, transfers: dict[str, list[tuple[int, float]]]) -> set[str]:
        done_req_ids: set[str] = set()
        for req_id, handles in list(transfers.items()):
            in_progress = False
            for handle, _xfer_stime in handles:
                xfer_state = self.nixl_wrapper.check_xfer_state(handle)
                if xfer_state == 'DONE':
                    self.nixl_wrapper.release_xfer_handle(handle)
                elif xfer_state == 'PROC':
                    in_progress = True
                    continue
                else:
                    raise RuntimeError('Transfer failed with state {}', xfer_state)
            if not in_progress:
                done_req_ids.add(req_id)
                del transfers[req_id]
        return done_req_ids
    def start_load_kv(self, metadata: NixlConnectorMetadata):
        for req_id, meta in metadata.reqs_to_recv.items():
            remote_engine_id = meta.remote_engine_id
            logger.debug('start_load_kv for request {} from remote engine {}. Num local_block_ids: {}. Num remote_block_ids: {}. ', req_id, remote_engine_id, len(meta.local_block_ids), len(meta.remote_block_ids))
            if self.use_host_buffer:
                self._recving_metadata[req_id] = meta
            if remote_engine_id not in self._remote_agents:
                with self._handshake_lock:
                    if remote_engine_id not in self._remote_agents:
                        self._background_nixl_handshake(req_id, remote_engine_id, meta)
                        continue
            self._read_blocks_for_req(req_id, meta)
        while not self._ready_requests.empty():
            self._read_blocks_for_req(*self._ready_requests.get_nowait())
        self._reqs_to_send.update(metadata.reqs_to_send)
    def _read_blocks_for_req(self, req_id: str, meta: ReqMeta):
        logger.debug('Remote agent {} available, calling _read_blocks for req {}', meta.remote_engine_id, req_id)
        self._read_blocks(request_id=req_id, dst_engine_id=meta.remote_engine_id, local_block_ids=meta.local_block_ids, remote_block_ids=meta.remote_block_ids)
    def _read_blocks(self, local_block_ids: list[int], remote_block_ids: list[int], dst_engine_id: str, request_id: str):
        tp_ratio = self._tp_size[self.engine_id] // self._tp_size[dst_engine_id]
        notif_id = f'{request_id}:{tp_ratio}'.encode()
        num_local_blocks = len(local_block_ids)
        if num_local_blocks == 0:
            remote_rank = self.tp_rank // tp_ratio
            agent_name = self._remote_agents[dst_engine_id][remote_rank]
            self.nixl_wrapper.send_notif(agent_name, notif_msg=notif_id)
            return
        num_remote_blocks = len(remote_block_ids)
        assert num_local_blocks <= num_remote_blocks
        if num_local_blocks < num_remote_blocks:
            remote_block_ids = remote_block_ids[-num_local_blocks:]
        local_xfer_side_handle = self.src_xfer_side_handle
        remote_xfer_side_handle = self.dst_xfer_side_handles[dst_engine_id]
        local_block_descs_ids: list[int] = []
        remote_block_descs_ids: list[int] = []
        if not self.block_window_per_layer:
            remote_block_descs_ids = self._get_block_descs_ids(dst_engine_id, remote_block_ids)
            local_block_descs_ids = self._get_block_descs_ids(self.engine_id, local_block_ids)
        else:
            for layer_idx, block_window in enumerate(self.block_window_per_layer):
                if block_window is None:
                    layer_local_block_ids = local_block_ids
                    layer_remote_block_ids = remote_block_ids
                else:
                    layer_local_block_ids = local_block_ids[-block_window:]
                    layer_remote_block_ids = remote_block_ids[-block_window:]
                layer_local_desc_ids = self._get_block_descs_ids(self.engine_id, layer_local_block_ids, layer_idx)
                layer_remote_desc_ids = self._get_block_descs_ids(dst_engine_id, layer_remote_block_ids, layer_idx)
                local_block_descs_ids.extend(layer_local_desc_ids)
                remote_block_descs_ids.extend(layer_remote_desc_ids)
        assert len(local_block_descs_ids) == len(remote_block_descs_ids)
        handle = self.nixl_wrapper.make_prepped_xfer('READ', local_xfer_side_handle, local_block_descs_ids, remote_xfer_side_handle, remote_block_descs_ids, notif_msg=notif_id)
        self.nixl_wrapper.transfer(handle)
        self._recving_transfers[request_id].append((handle, time.perf_counter()))
    def _get_block_descs_ids(self, engine_id: str, block_ids: list[int], layer_idx: Optional[int]=None) -> list[int]:
        if layer_idx is None:
            region_ids = range(self.num_regions)
        else:
            assert layer_idx < self.num_layers
            if self.num_layers < self.num_regions:
                assert 2 * self.num_layers == self.num_regions
                region_ids = range(2 * layer_idx, 2 * layer_idx + 2)
            else:
                assert self.num_layers == self.num_regions
                region_ids = range(layer_idx, layer_idx + 1)
        num_blocks = self.dst_num_blocks[engine_id]
        descs_ids: list[int] = []
        for reg_id in region_ids:
            for block_id in block_ids:
                descs_ids.append(reg_id * num_blocks + block_id)
        return descs_ids
@contextlib.contextmanager
def zmq_ctx(socket_type: Any, addr: str) -> Iterator[zmq.Socket]:
    if socket_type not in (zmq.ROUTER, zmq.REQ):
        raise ValueError(f'Unexpected socket type: {socket_type}')
    ctx: Optional[zmq.Context] = None
    try:
        ctx = zmq.Context()
        yield make_zmq_socket(ctx=ctx, path=addr, socket_type=socket_type, bind=socket_type == zmq.ROUTER)
    finally:
        if ctx is not None:
            ctx.destroy(linger=0)