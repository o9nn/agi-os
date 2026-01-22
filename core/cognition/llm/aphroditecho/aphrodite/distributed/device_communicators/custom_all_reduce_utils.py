import ctypes
import json
import os
import pickle
import subprocess
import sys
import tempfile
from collections.abc import Sequence
from itertools import product
from typing import Optional
import torch.distributed as dist
import torch.multiprocessing as mp
from loguru import logger
import aphrodite.common.envs as envs
from aphrodite.utils import cuda_device_count_stateless, update_environment_variables
from aphrodite.distributed.device_communicators.cuda_wrapper import CudaRTLibrary
def producer(batch_src: Sequence[int], producer_queue, consumer_queue, result_queue, cuda_visible_devices: Optional[str]=None):
    if cuda_visible_devices is not None:
        update_environment_variables({'CUDA_VISIBLE_DEVICES': cuda_visible_devices})
    lib = CudaRTLibrary()
    for i in batch_src:
        lib.cudaSetDevice(i)
        pointer = lib.cudaMalloc(1024)
        lib.cudaMemset(pointer, 1, 1024)
        lib.cudaDeviceSynchronize()
        handle = lib.cudaIpcGetMemHandle(pointer)
        producer_queue.put(handle)
        open_success = consumer_queue.get()
        if open_success:
            producer_queue.put(0)
            consumer_queue.get()
            host_data = (ctypes.c_char * 1024)()
            lib.cudaMemcpy(host_data, pointer, 1024)
            for i in range(1024):
                if ord(host_data[i]) != 2:
                    open_success = False
                    break
        result_queue.put(open_success)
        lib.cudaDeviceReset()
def consumer(batch_tgt: Sequence[int], producer_queue, consumer_queue, result_queue, cuda_visible_devices: Optional[str]=None):
    if cuda_visible_devices is not None:
        update_environment_variables({'CUDA_VISIBLE_DEVICES': cuda_visible_devices})
    lib = CudaRTLibrary()
    for j in batch_tgt:
        lib.cudaSetDevice(j)
        handle = producer_queue.get()
        open_success = False
        try:
            pointer = lib.cudaIpcOpenMemHandle(handle)
            open_success = True
        except RuntimeError:
            pass
        consumer_queue.put(open_success)
        if open_success:
            lib.cudaMemset(pointer, 2, 1024)
            lib.cudaDeviceSynchronize()
            producer_queue.get()
            consumer_queue.put(0)
            host_data = (ctypes.c_char * 1024)()
            lib.cudaMemcpy(host_data, pointer, 1024)
            for i in range(1024):
                if ord(host_data[i]) != 2:
                    open_success = False
                    break
        result_queue.put(open_success)
        lib.cudaDeviceReset()
def can_actually_p2p(batch_src: Sequence[int], batch_tgt: Sequence[int]) -> Sequence[bool]:
    cuda_visible_devices = envs.CUDA_VISIBLE_DEVICES
    smp = mp.get_context('spawn')
    producer_queue = smp.Queue()
    consumer_queue = smp.Queue()
    result_queue = smp.Queue()
    p_src = smp.Process(target=producer, args=(batch_src, producer_queue, consumer_queue, result_queue, cuda_visible_devices))
    p_tgt = smp.Process(target=consumer, args=(batch_tgt, producer_queue, consumer_queue, result_queue, cuda_visible_devices))
    p_src.start()
    p_tgt.start()
    p_src.join()
    p_tgt.join()
    assert p_src.exitcode == 0 and p_tgt.exitcode == 0
    result: list[bool] = []
    for src, tgt in zip(batch_src, batch_tgt):
        a = result_queue.get()
        b = result_queue.get()
        if a != b:
            logger.warning('Two processes do not agree on the P2P access status on {} -> {}, treat as disabled.', src, tgt)
            result.append(False)
        else:
            result.append(a)
    return result
_gpu_p2p_access_cache: Optional[dict[str, bool]] = None
def gpu_p2p_access_check(src: int, tgt: int) -> bool:
    global _gpu_p2p_access_cache
    if _gpu_p2p_access_cache is not None:
        return _gpu_p2p_access_cache[f'{src}->{tgt}']
    is_distributed = dist.is_initialized()
    num_dev = cuda_device_count_stateless()
    cuda_visible_devices = envs.CUDA_VISIBLE_DEVICES
    if cuda_visible_devices is None:
        cuda_visible_devices = ','.join((str(i) for i in range(num_dev)))
    path = os.path.join(envs.APHRODITE_CACHE_ROOT, f'gpu_p2p_access_cache_for_{cuda_visible_devices}.json')
    os.makedirs(os.path.dirname(path), exist_ok=True)
    from aphrodite.distributed.parallel_state import get_world_group
    if (not is_distributed or get_world_group().local_rank == 0) and (not os.path.exists(path)):
        logger.debug('generating GPU P2P access cache in {}', path)
        cache: dict[str, bool] = {}
        ids = list(range(num_dev))
        batch_src, batch_tgt = zip(*list(product(ids, ids)))
        with tempfile.NamedTemporaryFile() as output_file:
            input_bytes = pickle.dumps((batch_src, batch_tgt, output_file.name))
            returned = subprocess.run([sys.executable, __file__], input=input_bytes, capture_output=True)
            try:
                returned.check_returncode()
            except Exception as e:
                raise RuntimeError(f'Error happened when batch testing peer-to-peer access from {batch_src} to {batch_tgt}:\n{returned.stderr.decode()}') from e
            with open(output_file.name, 'rb') as f:
                result = pickle.load(f)
        for _i, _j, r in zip(batch_src, batch_tgt, result):
            cache[f'{_i}->{_j}'] = r
        with open(path, 'w') as f:
            json.dump(cache, f, indent=4)
    if is_distributed:
        get_world_group().barrier()
    logger.debug('reading GPU P2P access cache from {}', path)
    with open(path) as f:
        cache = json.load(f)
    _gpu_p2p_access_cache = cache
    return _gpu_p2p_access_cache[f'{src}->{tgt}']
__all__ = ['gpu_p2p_access_check']
if __name__ == '__main__':
    batch_src, batch_tgt, output_file = pickle.loads(sys.stdin.buffer.read())
    result = can_actually_p2p(batch_src, batch_tgt)
    with open(output_file, 'wb') as f:
        f.write(pickle.dumps(result))