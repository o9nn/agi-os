import threading
from typing import Optional, Union
from weakref import WeakValueDictionary
import torch
import torch.distributed as dist
from torch.distributed import ProcessGroup
class Cache:
    def __init__(self):
        self._cache: WeakValueDictionary = WeakValueDictionary()
        self._lock = threading.RLock()
    def get_or_create(self, kwargs, func):
        key = tuple(sorted(((k, v) for k, v in kwargs.items())))
        with self._lock:
            instance = self._cache.get(key)
            if instance is None:
                instance = func(**kwargs)
                self._cache[key] = instance
            return instance
class All2AllManagerBase:
    def __init__(self, cpu_group):
        self.cpu_group = cpu_group
        from aphrodite.distributed.parallel_state import get_dp_group, get_tp_group, in_the_same_node_as
        self.dp_group = get_dp_group()
        self.tp_group = get_tp_group()
        self.dp_rank = self.dp_group.rank_in_group
        self.dp_world_size = self.dp_group.world_size
        self.rank = dist.get_rank(cpu_group)
        self.world_size = dist.get_world_size(cpu_group)
        self.internode = not all(in_the_same_node_as(cpu_group, source_rank=0))
    def get_handle(self, kwargs):
        raise NotImplementedError
    def dispatch(self, hidden_states: torch.Tensor, router_logits: torch.Tensor):
        raise NotImplementedError
    def combine(self, hidden_states: torch.Tensor) -> torch.Tensor:
        raise NotImplementedError
    def destroy(self):
        pass
class DeviceCommunicatorBase:
    def __init__(self, cpu_group: ProcessGroup, device: Optional[torch.device]=None, device_group: Optional[ProcessGroup]=None, unique_name: str=''):
        self.device = device or torch.device('cpu')
        self.cpu_group = cpu_group
        self.device_group = device_group
        self.unique_name = unique_name
        self.rank = dist.get_rank(cpu_group)
        self.world_size = dist.get_world_size(cpu_group)
        self.ranks = dist.get_process_group_ranks(cpu_group)
        self.global_rank = dist.get_rank()
        self.global_world_size = dist.get_world_size()
        self.rank_in_group = dist.get_group_rank(self.cpu_group, self.global_rank)
        use_ep = False
        from aphrodite.common.config import get_current_aphrodite_config
        config = get_current_aphrodite_config()
        if config is not None:
            use_ep = config.parallel_config.data_parallel_size > 1
        self.use_all2all = 'ep' in unique_name and use_ep
        self.all2all_manager: Optional[All2AllManagerBase] = None
    def all_reduce(self, input_: torch.Tensor) -> torch.Tensor:
        dist.all_reduce(input_, group=self.device_group)
        return input_
    def all_gather(self, input_: torch.Tensor, dim: int=-1) -> torch.Tensor:
        if dim < 0:
            dim += input_.dim()
        input_size = input_.size()
        output_size = (input_size[0] * self.world_size,) + input_size[1:]
        output_tensor = torch.empty(output_size, dtype=input_.dtype, device=input_.device)
        dist.all_gather_into_tensor(output_tensor, input_, group=self.device_group)
        output_tensor = output_tensor.reshape((self.world_size,) + input_size)
        output_tensor = output_tensor.movedim(0, dim)
        output_tensor = output_tensor.reshape(input_size[:dim] + (self.world_size * input_size[dim],) + input_size[dim + 1:])
        return output_tensor
    def all_gatherv(self, input_: Union[torch.Tensor, list[torch.Tensor]], dim: int=0, sizes: Optional[list[int]]=None) -> Union[torch.Tensor, list[torch.Tensor]]:
        raise NotImplementedError
    def reduce_scatter(self, input_: torch.Tensor, dim: int=-1) -> torch.Tensor:
        world_size = self.world_size
        if world_size == 1:
            return input_
        assert -input_.dim() <= dim < input_.dim(), f'Invalid dim ({dim}) for input tensor with shape {input_.size()}'
        if dim < 0:
            dim += input_.dim()
        input_tensor = input_.movedim(0, dim).contiguous()
        assert input_tensor.shape[0] % world_size == 0
        chunk_size = input_tensor.shape[0] // world_size
        output_shape = (chunk_size,) + input_tensor.shape[1:]
        output_tensor = torch.empty(output_shape, dtype=input_tensor.dtype, device=input_tensor.device)
        torch.distributed.reduce_scatter_tensor(output_tensor, input_tensor, group=self.device_group)
        return output_tensor.movedim(0, dim).contiguous()
    def reduce_scatterv(self, input_: torch.Tensor, dim: int=-1, sizes: Optional[list[int]]=None) -> torch.Tensor:
        raise NotImplementedError
    def gather(self, input_: torch.Tensor, dst: int=0, dim: int=-1) -> Optional[torch.Tensor]:
        world_size = self.world_size
        assert -input_.dim() <= dim < input_.dim(), f'Invalid dim ({dim}) for input tensor with shape {input_.size()}'
        if dim < 0:
            dim += input_.dim()
        if self.rank_in_group == dst:
            gather_list = [torch.empty_like(input_) for _ in range(world_size)]
        else:
            gather_list = None
        torch.distributed.gather(input_, gather_list, dst=self.ranks[dst], group=self.device_group)
        if self.rank_in_group == dst:
            output_tensor = torch.cat(gather_list, dim=dim)
        else:
            output_tensor = None
        return output_tensor
    def send(self, tensor: torch.Tensor, dst: Optional[int]=None) -> None:
        """NOTE: `dst` is the local rank of the destination rank."""
        if dst is None:
            dst = (self.rank_in_group + 1) % self.world_size
        torch.distributed.send(tensor, self.ranks[dst], self.device_group)
    def recv(self, size: torch.Size, dtype: torch.dtype, src: Optional[int]=None) -> torch.Tensor:
        """NOTE: `src` is the local rank of the source rank."""
        if src is None:
            src = (self.rank_in_group - 1) % self.world_size
        tensor = torch.empty(size, dtype=dtype, device=self.device)
        torch.distributed.recv(tensor, self.ranks[src], self.device_group)
        return tensor
    def destroy(self):
        pass
    def prepare_communication_buffer_for_model(self, model: torch.nn.Module) -> None:
        if not self.use_all2all:
            return
        moe_modules = [module for module in model.modules() if module.__class__.__name__ == 'FusedMoE']
        for module in moe_modules:
            module.quant_method.init_prepare_finalize(module.moe_config)
    def dispatch(self, hidden_states: torch.Tensor, router_logits: torch.Tensor) -> tuple[torch.Tensor, torch.Tensor]:
        return (hidden_states, router_logits)
    def combine(self, hidden_states: torch.Tensor) -> torch.Tensor:
        return hidden_states