import dataclasses
import os
import pickle
import socket
import sys
import time
import uuid
from collections import deque
from collections.abc import Sequence
from datetime import timedelta
from typing import Any, Optional
import torch
from loguru import logger
from torch.distributed import ProcessGroup, TCPStore
from torch.distributed.distributed_c10d import Backend, PrefixStore, _get_default_timeout, _unregister_process_group
from torch.distributed.rendezvous import rendezvous
import aphrodite.common.envs as envs
from aphrodite.utils import get_tcp_uri, is_torch_equal_or_newer
USE_SCHED_YIELD = sys.version_info[:3] >= (3, 11, 1) or (sys.version_info[:2] == (3, 10) and sys.version_info[2] >= 8)
def sched_yield():
    if USE_SCHED_YIELD:
        os.sched_yield()
    else:
        time.sleep(0)
def ensure_divisibility(numerator, denominator):
    assert numerator % denominator == 0, '{} is not divisible by {}'.format(numerator, denominator)
def divide(numerator, denominator):
    ensure_divisibility(numerator, denominator)
    return numerator // denominator
def split_tensor_along_last_dim(tensor: torch.Tensor, num_partitions: int, contiguous_split_chunks: bool=False) -> Sequence[torch.Tensor]:
    last_dim = tensor.dim() - 1
    last_dim_size = divide(tensor.size()[last_dim], num_partitions)
    tensor_list = torch.split(tensor, last_dim_size, dim=last_dim)
    if contiguous_split_chunks:
        return tuple((chunk.contiguous() for chunk in tensor_list))
    return tensor_list
def get_pp_indices(num_hidden_layers: int, pp_rank: int, pp_size: int) -> tuple[int, int]:
    partition_list_str = envs.APHRODITE_PP_LAYER_PARTITION
    if partition_list_str is not None:
        try:
            partitions = [int(layer) for layer in partition_list_str.split(',')]
        except ValueError as err:
            raise ValueError('Invalid partition string: {}'.format(partition_list_str)) from err
        if len(partitions) != pp_size:
            raise ValueError(f'len(partitions)={len(partitions)!r} does not match pp_size={pp_size!r}.')
        if sum(partitions) != num_hidden_layers:
            raise ValueError(f'sum(partitions)={sum(partitions)!r} does not match num_hidden_layers={num_hidden_layers!r}.')
    else:
        layers_per_partition = num_hidden_layers // pp_size
        partitions = [layers_per_partition for _ in range(pp_size)]
        if (remaining_layers := (num_hidden_layers % pp_size)):
            for i in range(2, remaining_layers + 2):
                partitions[-i] += 1
            logger.info('Hidden layers were unevenly partitioned: [{}]. This can be manually overridden using the APHRODITE_PP_LAYER_PARTITION environment variable', ','.join((str(p) for p in partitions)))
    start_layer = sum(partitions[:pp_rank])
    end_layer = start_layer + partitions[pp_rank]
    return (start_layer, end_layer)
@dataclasses.dataclass
class StatelessProcessGroup:
    rank: int
    world_size: int
    store: torch._C._distributed_c10d.Store
    socket: Optional[socket.socket]
    data_expiration_seconds: int = 3600
    send_dst_counter: dict[int, int] = dataclasses.field(default_factory=dict)
    recv_src_counter: dict[int, int] = dataclasses.field(default_factory=dict)
    broadcast_send_counter: int = 0
    broadcast_recv_src_counter: dict[int, int] = dataclasses.field(default_factory=dict)
    entries: deque[tuple[str, float]] = dataclasses.field(default_factory=deque)
    def __post_init__(self):
        assert self.rank < self.world_size
        self.send_dst_counter = {i: 0 for i in range(self.world_size)}
        self.recv_src_counter = {i: 0 for i in range(self.world_size)}
        self.broadcast_recv_src_counter = {i: 0 for i in range(self.world_size)}
    def send_obj(self, obj: Any, dst: int):
        self.expire_data()
        key = f'send_to/{dst}/{self.send_dst_counter[dst]}'
        self.store.set(key, pickle.dumps(obj))
        self.send_dst_counter[dst] += 1
        self.entries.append((key, time.time()))
    def expire_data(self):
        while self.entries:
            key, timestamp = self.entries[0]
            if time.time() - timestamp > self.data_expiration_seconds:
                self.store.delete_key(key)
                self.entries.popleft()
            else:
                break
    def recv_obj(self, src: int) -> Any:
        obj = pickle.loads(self.store.get(f'send_to/{self.rank}/{self.recv_src_counter[src]}'))
        self.recv_src_counter[src] += 1
        return obj
    def broadcast_obj(self, obj: Optional[Any], src: int) -> Any:
        if self.rank == src:
            self.expire_data()
            key = f'broadcast_from/{src}/{self.broadcast_send_counter}'
            self.store.set(key, pickle.dumps(obj))
            self.broadcast_send_counter += 1
            self.entries.append((key, time.time()))
            return obj
        else:
            key = f'broadcast_from/{src}/{self.broadcast_recv_src_counter[src]}'
            recv_obj = pickle.loads(self.store.get(key))
            self.broadcast_recv_src_counter[src] += 1
            return recv_obj
    def all_gather_obj(self, obj: Any) -> list[Any]:
        gathered_objs = []
        for i in range(self.world_size):
            if i == self.rank:
                gathered_objs.append(obj)
                self.broadcast_obj(obj, src=self.rank)
            else:
                recv_obj = self.broadcast_obj(None, src=i)
                gathered_objs.append(recv_obj)
        return gathered_objs
    def barrier(self, timeout: float=30.0):
        try:
            if self.rank == 0:
                barrier_id = f'barrier_{uuid.uuid4()}'
                self.broadcast_obj(barrier_id, src=0)
            else:
                barrier_id = self.broadcast_obj(None, src=0)
        except Exception as e:
            raise RuntimeError('Failed to broadcast barrier_id') from e
        arrival_key = f'arrival_{barrier_id}_{self.rank}'
        try:
            self.store.set(arrival_key, b'1')
        except Exception as e:
            raise RuntimeError('Failed to signal barrier arrival') from e
        start_time = time.time()
        processes_arrived: set[int] = set()
        while len(processes_arrived) < self.world_size:
            cur_time = time.time()
            if cur_time - start_time > timeout:
                raise RuntimeError('Barrier timed out after {} seconds', timeout)
            for i in range(self.world_size):
                if i in processes_arrived:
                    continue
                key = f'arrival_{barrier_id}_{i}'
                try:
                    self.store.get(key)
                    processes_arrived.add(i)
                except KeyError:
                    pass
                except Exception as check_e:
                    logger.debug('Error checking key existence: {}', check_e)
                    sched_yield()
            if len(processes_arrived) < self.world_size:
                sched_yield()
        departure_key = f'departure_{barrier_id}_{self.rank}'
        try:
            self.store.set(departure_key, b'1')
        except Exception as e:
            raise RuntimeError('Failed to signal barrier departure') from e
        if self.rank != 0:
            return
        start_time = time.time()
        processes_departed: set[int] = set()
        while len(processes_departed) < self.world_size:
            if time.time() - start_time > timeout:
                raise RuntimeError('Barrier departure timed out after {} s', timeout)
            for i in range(self.world_size):
                if i in processes_departed:
                    continue
                key = f'departure_{barrier_id}_{i}'
                try:
                    self.store.get(key)
                    processes_departed.add(i)
                except KeyError:
                    pass
                except Exception as check_e:
                    logger.debug('Error checking key existence: {}', check_e)
                    sched_yield()
            if len(processes_departed) < self.world_size:
                sched_yield()
        for i in range(self.world_size):
            try:
                self.store.delete_key(f'arrival_{barrier_id}_{i}')
            except Exception:
                logger.debug('Error deleting key: {}', f'arrival_{barrier_id}_{i}')
            try:
                self.store.delete_key(f'departure_{barrier_id}_{i}')
            except Exception:
                logger.debug('Error deleting key: {}', f'departure_{barrier_id}_{i}')
    @staticmethod
    def create(host: str, port: int, rank: int, world_size: int, data_expiration_seconds: int=3600, store_timeout: int=300) -> 'StatelessProcessGroup':
        launch_server = rank == 0
        if launch_server:
            listen_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            listen_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
            listen_socket.bind((host, port))
            listen_socket.listen()
            listen_fd = listen_socket.fileno()
        else:
            listen_socket = None
            listen_fd = None
        store = TCPStore(host_name=host, port=port, world_size=world_size, is_master=launch_server, timeout=timedelta(seconds=store_timeout), use_libuv=False, master_listen_fd=listen_fd)
        return StatelessProcessGroup(rank=rank, world_size=world_size, store=store, socket=listen_socket, data_expiration_seconds=data_expiration_seconds)
def init_gloo_process_group(backend: Backend, prefix_store: PrefixStore, group_rank: int, group_size: int, timeout: timedelta) -> ProcessGroup:
    if is_torch_equal_or_newer('2.6'):
        pg = ProcessGroup(prefix_store, group_rank, group_size)
    else:
        options = ProcessGroup.Options(backend=backend)
        pg = ProcessGroup(prefix_store, group_rank, group_size, options)
    from torch.distributed.distributed_c10d import ProcessGroupGloo
    backend_class = ProcessGroupGloo(prefix_store, group_rank, group_size, timeout=timeout)
    backend_type = ProcessGroup.BackendType.GLOO
    device = torch.device('cpu')
    if is_torch_equal_or_newer('2.6'):
        pg._set_default_backend(backend_type)
    backend_class._set_sequence_number_for_group()
    pg._register_backend(device, backend_type, backend_class)
    return pg
def stateless_init_torch_distributed_process_group(host: str, port: int, rank: int, world_size: int, backend: str) -> ProcessGroup:
    init_method = get_tcp_uri(host, port)
    backend = Backend(backend)
    timeout = _get_default_timeout(backend)
    store, rank, world_size = next(rendezvous(init_method, rank, world_size, timeout=timeout))
    store.set_timeout(timeout)
    group_rank = rank
    group_size = world_size
    prefix_store = PrefixStore(init_method, store)
    if backend == 'gloo':
        return init_gloo_process_group(backend=backend, prefix_store=prefix_store, group_rank=group_rank, group_size=group_size, timeout=timeout)
    from aphrodite.platforms import current_platform
    return current_platform.stateless_init_device_torch_dist_pg(backend=backend, prefix_store=prefix_store, group_rank=group_rank, group_size=group_size, timeout=timeout)
def stateless_destroy_torch_distributed_process_group(pg: ProcessGroup) -> None:
    if is_torch_equal_or_newer('2.7'):
        pg.shutdown()
    else:
        from torch.distributed.distributed_c10d import _shutdown_backend
        _shutdown_backend(pg)
    _unregister_process_group(pg.group_name)