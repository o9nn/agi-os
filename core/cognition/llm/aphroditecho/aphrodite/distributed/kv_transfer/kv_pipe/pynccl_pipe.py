import threading
import time
from concurrent.futures import ThreadPoolExecutor
from typing import Callable, Optional
import torch
from loguru import logger
from aphrodite.common.config import KVTransferConfig
from aphrodite.distributed.device_communicators.pynccl import PyNcclCommunicator
from aphrodite.distributed.kv_transfer.kv_pipe.base import KVPipeBase
from aphrodite.distributed.utils import StatelessProcessGroup
class BrokenPipeException(Exception):
    def __init__(self, message):
        self.message = message
        super().__init__(self.message)
Metadata = dict[str, Optional[torch.Tensor]]
class PyNcclPipe(KVPipeBase):
    METADATA_LENGTH = 16
    MAX_TENSOR_DIMENSIONS = 14
    METADATA_DTYPE = torch.int64
    def __init__(self, local_rank: int, config: KVTransferConfig, device: Optional[str]=None, port_offset: int=0):
        self.config = config
        self.local_rank = local_rank
        self.kv_rank = self.config.kv_rank
        self.kv_parallel_size = self.config.kv_parallel_size
        if device is None:
            self.device = self._select_device(self.config.kv_buffer_device)
        else:
            self.device = self._select_device(device)
        store_timeout = self.config.get_from_extra_config('store_timeout', 300)
        self.group = StatelessProcessGroup.create(host=self.config.kv_ip, port=self.config.kv_port + port_offset, rank=self.kv_rank, world_size=self.kv_parallel_size, store_timeout=store_timeout)
        self.group.barrier()
        impl = self._get_device_send_recv_impl(self.group)
        self.device_send_func, self.device_recv_func = impl
        self.target_rank_for_send = (self.kv_rank + 1) % self.kv_parallel_size
        self.target_rank_for_recv = (self.kv_rank - 1) % self.kv_parallel_size
        self.transport_thread: Optional[ThreadPoolExecutor] = None
        self.buffer_size = 0
        self.buffer_size_lock = threading.Lock()
        self.buffer_size_thresh = self.config.kv_buffer_size
    def _get_device_send_recv_impl(self, group: StatelessProcessGroup) -> tuple[Callable[[torch.Tensor, int], None], Callable[[torch.Tensor, int], None]]:
        send: Callable[[torch.Tensor, int], None]
        recv: Callable[[torch.Tensor, int], None]
        if self.device.type == 'cuda':
            comm = PyNcclCommunicator(group, device=self.local_rank)
            comm.disabled = False
            send, recv = (comm.send, comm.recv)
        else:
            send = group.send_obj
            def my_recv(x, src):
                x[...] = group.recv_obj(src)
            recv = my_recv
        return (send, recv)
    def _select_device(self, device: str):
        logger.info('Selecting device: {}', device)
        if device == 'cuda':
            return torch.device(f'cuda:{self.local_rank}')
        else:
            return torch.device('cpu')
    def _make_metadata(self, tensor: Optional[torch.Tensor]) -> Metadata:
        if tensor is None:
            return {'dtype': None, 'shape': None}
        else:
            return {'dtype': tensor.dtype, 'shape': tensor.shape}
    def _prepare_recv_buffer(self, metadata: Metadata) -> torch.Tensor:
        return torch.empty(metadata['shape'], dtype=metadata['dtype'], device=self.device)
    def _send_metadata(self, metadata: Metadata):
        self.group.send_obj(metadata, self.target_rank_for_send)
    def _recv_metadata(self) -> Metadata:
        return self.group.recv_obj(self.target_rank_for_recv)
    def _send_impl(self, tensor: Optional[torch.Tensor]) -> None:
        metadata = self._make_metadata(tensor)
        self._send_metadata(metadata)
        if tensor is not None:
            self.device_send_func(tensor.to(self.device), self.target_rank_for_send)
    def _recv_impl(self) -> Optional[torch.Tensor]:
        metadata = self._recv_metadata()
        if metadata['dtype'] is None:
            return None
        buffer = self._prepare_recv_buffer(metadata)
        self.device_recv_func(buffer, self.target_rank_for_recv)
        return buffer
    def send_tensor_wrapper(self, tensor: Optional[torch.Tensor], tensor_size: int) -> None:
        try:
            self._send_impl(tensor)
            with self.buffer_size_lock:
                self.buffer_size -= tensor_size
        except Exception as e:
            logger.error('[rank{}]: Exception when trying to send {}, msg: {}', torch.distributed.get_rank(), str(tensor), str(e))
            import traceback
            traceback.print_exc()
    def block_if_full(self):
        while self.buffer_size > self.buffer_size_thresh:
            logger.debug('KV cache transfer pipe is full. Waiting...')
            time.sleep(0.05)
    def send_tensor(self, tensor: Optional[torch.Tensor]) -> None:
        if self.transport_thread is None:
            self.transport_thread = ThreadPoolExecutor(max_workers=1)
        if tensor is not None:
            tensor_size = tensor.element_size() * tensor.numel()
        else:
            tensor_size = 0
        self.block_if_full()
        with self.buffer_size_lock:
            self.buffer_size += tensor_size
        self.transport_thread.submit(self.send_tensor_wrapper, tensor, tensor_size)
    def recv_tensor(self) -> Optional[torch.Tensor]:
        if self.transport_thread is None:
            self.transport_thread = ThreadPoolExecutor(max_workers=1)
        future = self.transport_thread.submit(self._recv_impl)
        try:
            tensor = future.result()
        except Exception as e:
            logger.error('Encountering exception in KV receiving thread')
            logger.error('{}', e)
            logger.error('My device: {}', self.device)
            import traceback
            traceback.print_exc()
            raise e
        return tensor
    def close(self):
        if hasattr(self, 'transport_thread') and self.transport_thread is not None:
            self.transport_thread.shutdown()