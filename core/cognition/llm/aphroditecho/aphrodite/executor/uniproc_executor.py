import os
from typing import Any, Callable, Dict, List, Optional, Tuple, Union
import torch
import torch.distributed as dist
import aphrodite.common.envs as envs
from aphrodite.utils import get_distributed_init_method, get_ip, get_open_port, run_method
from aphrodite.executor.executor_base import ExecutorBase
from aphrodite.v1.engine import ReconfigureDistributedRequest, ReconfigureRankType
from aphrodite.worker.worker_base import WorkerWrapperBase
class UniProcExecutor(ExecutorBase):
    uses_ray: bool = False
    def _init_executor(self) -> None:
        self.driver_worker = WorkerWrapperBase(aphrodite_config=self.aphrodite_config, rpc_rank=0)
        distributed_init_method = get_distributed_init_method(get_ip(), get_open_port())
        local_rank = 0
        device_info = self.aphrodite_config.device_config.device.__str__().split(':')
        if len(device_info) > 1:
            local_rank = int(device_info[1])
        rank = 0
        is_driver_worker = True
        kwargs = dict(aphrodite_config=self.aphrodite_config, local_rank=local_rank, rank=rank, distributed_init_method=distributed_init_method, is_driver_worker=is_driver_worker)
        self.collective_rpc('init_worker', args=([kwargs],))
        self.collective_rpc('init_device')
        self.collective_rpc('load_model')
    def collective_rpc(self, method: Union[str, Callable], timeout: Optional[float]=None, args: Tuple=(), kwargs: Optional[Dict]=None) -> List[Any]:
        if kwargs is None:
            kwargs = {}
        answer = run_method(self.driver_worker, method, args, kwargs)
        return [answer]
    def check_health(self) -> None:
        return
    def reinitialize_distributed(self, reconfig_request: ReconfigureDistributedRequest) -> None:
        self.driver_worker.reinitialize_distributed(reconfig_request)
        if reconfig_request.new_data_parallel_rank == ReconfigureRankType.SHUTDOWN_CURRENT_RANK:
            self.shutdown()
        return
UniProcExecutorAsync = UniProcExecutor
class ExecutorWithExternalLauncher(UniProcExecutor):
    uses_ray: bool = False
    def _init_executor(self) -> None:
        assert self.aphrodite_config.scheduler_config.delay_factor == 0.0, 'ExecutorWithExternalLauncher needs deterministic execution, so itdoes not support delay_factor in scheduling'
        if envs.APHRODITE_USE_V1:
            assert not envs.APHRODITE_ENABLE_V1_MULTIPROCESSING, 'To get deterministic execution in V1, please set APHRODITE_ENABLE_V1_MULTIPROCESSING=0'
        self.driver_worker = WorkerWrapperBase(aphrodite_config=self.aphrodite_config, rpc_rank=0)
        distributed_init_method = 'env://'
        rank = int(os.environ['RANK'])
        local_rank = int(os.environ['LOCAL_RANK'])
        is_driver_worker = True
        kwargs = dict(aphrodite_config=self.aphrodite_config, local_rank=local_rank, rank=rank, distributed_init_method=distributed_init_method, is_driver_worker=is_driver_worker)
        self.collective_rpc('init_worker', args=([kwargs],))
        self.collective_rpc('init_device')
        self.collective_rpc('load_model')
    def determine_num_available_blocks(self) -> Tuple[int, int]:
        a, b = super().determine_num_available_blocks()
        from aphrodite.distributed.parallel_state import get_world_group
        cpu_group = get_world_group().cpu_group
        a_tensor = torch.tensor([a], device='cpu', dtype=torch.int64)
        b_tensor = torch.tensor([b], device='cpu', dtype=torch.int64)
        dist.all_reduce(a_tensor, group=cpu_group, op=dist.ReduceOp.MIN)
        dist.all_reduce(b_tensor, group=cpu_group, op=dist.ReduceOp.MIN)
        return (a_tensor.item(), b_tensor.item())