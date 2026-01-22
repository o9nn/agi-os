import dataclasses
import os
import time
from abc import abstractmethod
from typing import Any, Dict, List, Optional, Set, Tuple, Type, Union
import cloudpickle
import torch
import torch.nn as nn
from loguru import logger
from aphrodite.common.config import AphroditeConfig, ObservabilityConfig, set_current_aphrodite_config
from aphrodite.common.sequence import ExecuteModelRequest, IntermediateTensors
from aphrodite.utils import enable_trace_function_call_for_thread, resolve_obj_by_qualname, run_method, update_environment_variables, warn_for_unimplemented_methods
from aphrodite.distributed import broadcast_tensor_dict, get_pp_group, get_tp_group
from aphrodite.lora.request import LoRARequest
from aphrodite.modeling.layers.sampler import SamplerOutput
from aphrodite.worker.model_runner_base import BroadcastableModelInput, ModelRunnerBase, ModelRunnerInputBase
@warn_for_unimplemented_methods
class WorkerBase:
    def __init__(self, aphrodite_config: AphroditeConfig) -> None:
        self.aphrodite_config = aphrodite_config
        self.model_config = aphrodite_config.model_config
        self.cache_config = aphrodite_config.cache_config
        self.lora_config = aphrodite_config.lora_config
        self.load_config = aphrodite_config.load_config
        self.parallel_config = aphrodite_config.parallel_config
        self.scheduler_config = aphrodite_config.scheduler_config
        self.device_config = aphrodite_config.device_config
        self.speculative_config = aphrodite_config.speculative_config
        self.observability_config = aphrodite_config.observability_config
        self.kv_transfer_config = aphrodite_config.kv_transfer_config
        self.compilation_config = aphrodite_config.compilation_config
        from aphrodite.platforms import current_platform
        self.current_platform = current_platform
    def init_device(self) -> None:
        raise NotImplementedError
    def initialize_cache(self, num_gpu_blocks: int, num_cpu_blocks: int) -> None:
        raise NotImplementedError
    def get_model(self) -> nn.Module:
        raise NotImplementedError
    def load_model(self) -> None:
        raise NotImplementedError
    def execute_model(self, execute_model_req: Optional[ExecuteModelRequest]=None) -> Optional[List[SamplerOutput]]:
        raise NotImplementedError
    def start_worker_execution_loop(self) -> None:
        with self.current_platform.inference_mode():
            while True:
                output = self.execute_model(execute_model_req=None)
                if output is None:
                    return None
    def determine_num_available_blocks(self) -> Tuple[int, int]:
        raise NotImplementedError
    def get_cache_block_size_bytes(self) -> int:
        raise NotImplementedError
    def add_lora(self, lora_request: LoRARequest) -> bool:
        raise NotImplementedError
    def remove_lora(self, lora_id: int) -> bool:
        raise NotImplementedError
    def pin_lora(self, lora_id: int) -> bool:
        raise NotImplementedError
    def list_loras(self) -> Set[int]:
        raise NotImplementedError
    @property
    def vocab_size(self) -> int:
        return self.model_config.get_vocab_size()
class DelegateWorkerBase(WorkerBase):
    worker: WorkerBase
    def __init__(self, *args, **kwargs) -> None:
        aphrodite_config: AphroditeConfig = kwargs.get('aphrodite_config')
        cls = resolve_obj_by_qualname(aphrodite_config.parallel_config.worker_cls)
        self.worker = cls(*args, **kwargs)
    def init_device(self) -> None:
        self.worker.init_device()
    def determine_num_available_blocks(self) -> Tuple[int, int]:
        return self.worker.determine_num_available_blocks()
    def initialize_cache(self, num_gpu_blocks: int, num_cpu_blocks: int) -> None:
        self.worker.initialize_cache(num_gpu_blocks, num_cpu_blocks)
    def load_model(self) -> None:
        self.worker.load_model()
    def get_model(self) -> nn.Module:
        return self.worker.get_model()
    def execute_model(self, execute_model_req: Optional[ExecuteModelRequest]=None) -> Optional[List[SamplerOutput]]:
        return self.worker.execute_model(execute_model_req)
    def get_cache_block_size_bytes(self) -> int:
        return self.worker.get_cache_block_size_bytes()
    def add_lora(self, lora_request: LoRARequest) -> bool:
        return self.worker.add_lora(lora_request)
    def remove_lora(self, lora_id: int) -> bool:
        return self.worker.remove_lora(lora_id)
    def pin_lora(self, lora_id: int) -> bool:
        return self.worker.pin_lora(lora_id)
    def list_loras(self) -> Set[int]:
        return self.worker.list_loras()
    def __getattr__(self, attr):
        return getattr(self.worker, attr)
class LoRANotSupportedWorkerBase(WorkerBase):
    def add_lora(self, lora_request: LoRARequest) -> bool:
        raise ValueError(f'{type(self)} does not support LoRA')
    def remove_lora(self, lora_id: int) -> bool:
        raise ValueError(f'{type(self)} does not support LoRA')
    def pin_lora(self, lora_id: int) -> bool:
        raise ValueError(f'{type(self)} does not support LoRA')
    def list_loras(self) -> Set[int]:
        raise ValueError(f'{type(self)} does not support LoRA')
@dataclasses.dataclass(frozen=True)
class WorkerInput:
    num_seq_groups: Optional[int] = None
    blocks_to_swap_in: Optional[torch.Tensor] = None
    blocks_to_swap_out: Optional[torch.Tensor] = None
    blocks_to_copy: Optional[torch.Tensor] = None
    virtual_engine: int = 0
    num_steps: int = 1
    @classmethod
    def from_broadcasted_tensor_dict(cls: Type['WorkerInput'], tensor_dict: Dict[str, Any]) -> 'WorkerInput':
        return cls(num_seq_groups=tensor_dict.pop('num_seq_groups'), blocks_to_swap_in=tensor_dict.pop('blocks_to_swap_in'), blocks_to_swap_out=tensor_dict.pop('blocks_to_swap_out'), blocks_to_copy=tensor_dict.pop('blocks_to_copy'), virtual_engine=tensor_dict['virtual_engine'], num_steps=tensor_dict.pop('num_steps'))
    def as_broadcastable_tensor_dict(self) -> Dict[str, Union[int, torch.Tensor]]:
        tensor_dict = {'num_seq_groups': self.num_seq_groups, 'blocks_to_swap_in': self.blocks_to_swap_in, 'blocks_to_swap_out': self.blocks_to_swap_out, 'blocks_to_copy': self.blocks_to_copy, 'virtual_engine': self.virtual_engine, 'num_steps': self.num_steps}
        return tensor_dict
class LocalOrDistributedWorkerBase(WorkerBase):
    is_driver_worker: bool
    model_runner: ModelRunnerBase
    observability_config: Optional[ObservabilityConfig] = None
    @property
    @abstractmethod
    def do_metadata_broadcast(self) -> bool:
        raise NotImplementedError
    @property
    @abstractmethod
    def kv_cache(self) -> Optional[List[List[torch.Tensor]]]:
        raise NotImplementedError
    @abstractmethod
    def prepare_worker_input(self, execute_model_req: ExecuteModelRequest) -> WorkerInput:
        raise NotImplementedError
    @abstractmethod
    def execute_worker(self, worker_input: WorkerInput) -> None:
        raise NotImplementedError
    def _get_worker_input_from_broadcast(self) -> Optional[Tuple[BroadcastableModelInput, WorkerInput, Dict[str, torch.Tensor]]]:
        assert self.do_metadata_broadcast
        assert not self.is_driver_worker
        broadcast_data = broadcast_tensor_dict(src=0)
        if not broadcast_data:
            return None
        worker_input = WorkerInput.from_broadcasted_tensor_dict(broadcast_data)
        model_input = self.model_runner.make_model_input_from_broadcasted_tensor_dict(broadcast_data)
        kwargs = extract_previous_hidden_states(broadcast_data)
        return (model_input, worker_input, kwargs)
    def _get_driver_input_and_broadcast(self, execute_model_req: ExecuteModelRequest) -> Tuple[BroadcastableModelInput, WorkerInput, Dict[str, torch.Tensor]]:
        assert self.is_driver_worker
        worker_input: WorkerInput = self.prepare_worker_input(execute_model_req=execute_model_req)
        model_input: ModelRunnerInputBase = self.model_runner.prepare_model_input(execute_model_req.seq_group_metadata_list, execute_model_req.virtual_engine, execute_model_req.finished_requests_ids)
        kwargs = extract_previous_hidden_states(execute_model_req)
        if self.do_metadata_broadcast:
            broadcast_data = worker_input.as_broadcastable_tensor_dict()
            broadcast_data.update(model_input.as_broadcastable_tensor_dict())
            broadcast_data.update(kwargs)
            broadcast_tensor_dict(broadcast_data, src=0)
        if execute_model_req.async_callback:
            model_input = dataclasses.replace(model_input, async_callback=execute_model_req.async_callback)
        return (model_input, worker_input, kwargs)
    def prepare_input(self, execute_model_req: Optional[ExecuteModelRequest]=None) -> Optional[Tuple[BroadcastableModelInput, WorkerInput, Dict[str, torch.Tensor]]]:
        if self.is_driver_worker:
            if execute_model_req is None:
                if self.do_metadata_broadcast:
                    broadcast_tensor_dict({}, src=0)
                return None
            return self._get_driver_input_and_broadcast(execute_model_req)
        else:
            return self._get_worker_input_from_broadcast()
    def get_model(self) -> nn.Module:
        return self.model_runner.get_model()
    def execute_model(self, execute_model_req: Optional[ExecuteModelRequest]=None) -> Optional[List[SamplerOutput]]:
        start_time = time.perf_counter()
        inputs = self.prepare_input(execute_model_req)
        if inputs is None:
            return None
        model_input, worker_input, kwargs = inputs
        num_steps = worker_input.num_steps
        self.execute_worker(worker_input)
        if worker_input.num_seq_groups == 0:
            return []
        intermediate_tensors = None
        orig_model_execute_time = 0.0
        if not get_pp_group().is_first_rank:
            intermediate_tensors = IntermediateTensors(get_pp_group().recv_tensor_dict(all_gather_group=get_tp_group()))
            if self.observability_config is not None and self.observability_config.collect_model_execute_time:
                orig_model_execute_time = intermediate_tensors.tensors.get('model_execute_time', torch.tensor(0)).item()
        output = self.model_runner.execute_model(model_input=model_input, kv_caches=self.kv_cache[worker_input.virtual_engine] if self.kv_cache is not None else None, intermediate_tensors=intermediate_tensors, num_steps=num_steps, **kwargs)
        model_execute_time = time.perf_counter() - start_time
        if not get_pp_group().is_last_rank:
            assert isinstance(output, IntermediateTensors)
            if self.observability_config is not None and self.observability_config.collect_model_execute_time:
                output.tensors['model_execute_time'] = torch.tensor(model_execute_time + orig_model_execute_time)
            get_pp_group().send_tensor_dict(output.tensors, all_gather_group=get_tp_group())
            return [None]
        if self.observability_config is not None and self.observability_config.collect_model_execute_time and (output is not None):
            for o in output:
                o.model_execute_time = orig_model_execute_time + model_execute_time
        return output
    def _execute_model_spmd(self, execute_model_req: ExecuteModelRequest, intermediate_tensors: Optional[IntermediateTensors]=None) -> Optional[List[SamplerOutput]]:
        assert execute_model_req is not None, '_execute_model_spmd() requires each worker to take in an ExecuteModelRequest'
        worker_input: WorkerInput = self.prepare_worker_input(execute_model_req=execute_model_req)
        model_input: ModelRunnerInputBase = self.model_runner.prepare_model_input(execute_model_req.seq_group_metadata_list)
        self.execute_worker(worker_input)
        if worker_input.num_seq_groups == 0:
            return []
        kwargs = extract_previous_hidden_states(execute_model_req)
        return self.model_runner.execute_model(model_input=model_input, kv_caches=self.kv_cache[worker_input.virtual_engine] if self.kv_cache is not None else None, intermediate_tensors=intermediate_tensors, **kwargs)
class WorkerWrapperBase:
    def __init__(self, aphrodite_config: AphroditeConfig, rpc_rank: int=0) -> None:
        self.rpc_rank = rpc_rank
        self.worker: Optional[WorkerBase] = None
        self.aphrodite_config: Optional[AphroditeConfig] = None
        if aphrodite_config.model_config is not None:
            trust_remote_code = aphrodite_config.model_config.trust_remote_code
            if trust_remote_code:
                from aphrodite.utils import init_cached_hf_modules
                init_cached_hf_modules()
    def adjust_rank(self, rank_mapping: Dict[int, int]) -> None:
        if self.rpc_rank in rank_mapping:
            self.rpc_rank = rank_mapping[self.rpc_rank]
    def update_environment_variables(self, envs_list: List[Dict[str, str]]) -> None:
        envs = envs_list[self.rpc_rank]
        key = 'CUDA_VISIBLE_DEVICES'
        if key in envs and key in os.environ:
            del os.environ[key]
        update_environment_variables(envs)
    def init_worker(self, all_kwargs: List[Dict[str, Any]]) -> None:
        kwargs = all_kwargs[self.rpc_rank]
        self.aphrodite_config = kwargs.get('aphrodite_config', None)
        assert self.aphrodite_config is not None, 'aphrodite_config is required to initialize the worker'
        enable_trace_function_call_for_thread(self.aphrodite_config)
        from aphrodite.plugins import load_general_plugins
        load_general_plugins()
        if isinstance(self.aphrodite_config.parallel_config.worker_cls, str):
            worker_class = resolve_obj_by_qualname(self.aphrodite_config.parallel_config.worker_cls)
        else:
            logger.warning('passing worker_cls as a class object is strongly deprecated, as the serialization of class objects can be tricky and error-prone. To be safe, please keep the class in a separate module and pass the qualified name of the class as a string.')
            assert isinstance(self.aphrodite_config.parallel_config.worker_cls, bytes)
            worker_class = cloudpickle.loads(self.aphrodite_config.parallel_config.worker_cls)
        if self.aphrodite_config.parallel_config.worker_extension_cls:
            worker_extension_cls = resolve_obj_by_qualname(self.aphrodite_config.parallel_config.worker_extension_cls)
            extended_calls = []
            if worker_extension_cls not in worker_class.__bases__:
                for attr in dir(worker_extension_cls):
                    if attr.startswith('__'):
                        continue
                    assert not hasattr(worker_class, attr), f'Worker class {worker_class} already has an attribute {attr}, which conflicts with the worker extension class {worker_extension_cls}.'
                    if callable(getattr(worker_extension_cls, attr)):
                        extended_calls.append(attr)
                worker_class.__bases__ = worker_class.__bases__ + (worker_extension_cls,)
                logger.info('Injected {} into {} for extended collective_rpc calls {}', worker_extension_cls, worker_class, extended_calls)
        with set_current_aphrodite_config(self.aphrodite_config):
            self.worker = worker_class(**kwargs)
            assert self.worker is not None
    def initialize_from_config(self, kv_cache_configs: List[Any]) -> None:
        kv_cache_config = kv_cache_configs[self.rpc_rank]
        with set_current_aphrodite_config(self.aphrodite_config):
            self.worker.initialize_from_config(kv_cache_config)
    def init_device(self):
        with set_current_aphrodite_config(self.aphrodite_config):
            self.worker.init_device()
    def execute_method(self, method: Union[str, bytes], *args, **kwargs):
        try:
            return run_method(self, method, args, kwargs)
        except Exception as e:
            msg = f'Error executing method {method!r}. This might cause deadlock in distributed execution.'
            logger.exception(msg)
            raise e
    def __getattr__(self, attr):
        return getattr(self.worker, attr)
def extract_previous_hidden_states(data: Union[ExecuteModelRequest, Dict[str, torch.Tensor]]) -> Dict[str, torch.Tensor]:
    output = {}
    if isinstance(data, dict):
        if 'previous_hidden_states' in data:
            output['previous_hidden_states'] = data['previous_hidden_states']
    elif data.previous_hidden_states is not None:
        output['previous_hidden_states'] = data.previous_hidden_states.hidden_states
    return output