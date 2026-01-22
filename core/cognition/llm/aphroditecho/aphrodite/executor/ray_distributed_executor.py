import asyncio
import os
from collections import defaultdict
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any, Callable, Dict, List, Optional, Union
import cloudpickle
import msgspec
from loguru import logger
import aphrodite.common.envs as envs
from aphrodite.common.sequence import ExecuteModelRequest
from aphrodite.utils import _run_task_with_lock, get_distributed_init_method, get_ip, get_open_port, make_async
from aphrodite.executor.executor_base import DistributedExecutorBase
from aphrodite.executor.msgspec_utils import encode_hook
from aphrodite.executor.ray_utils import RayWorkerWrapper, initialize_ray_cluster, ray
from aphrodite.modeling.layers.sampler import SamplerOutput
from aphrodite.platforms import current_platform
if ray is not None:
    from ray.actor import ActorHandle
    from ray.util.scheduling_strategies import PlacementGroupSchedulingStrategy
else:
    ActorHandle = None
if TYPE_CHECKING:
    from ray.util.placement_group import PlacementGroup
@dataclass
class RayWorkerMetaData:
    worker: ActorHandle
    created_rank: int
    adjusted_rank: int = -1
    ip: str = ''
class RayDistributedExecutor(DistributedExecutorBase):
    WORKER_SPECIFIC_ENV_VARS = {'APHRODITE_HOST_IP', 'APHRODITE_HOST_PORT', 'LOCAL_RANK', 'CUDA_VISIBLE_DEVICES'}
    ADDITIONAL_ENV_VARS = {'HF_TOKEN', 'HUGGING_FACE_HUB_TOKEN'}
    uses_ray: bool = True
    def _init_executor(self) -> None:
        self.forward_dag: Optional[ray.dag.CompiledDAG] = None
        if envs.APHRODITE_USE_V1:
            os.environ['APHRODITE_USE_RAY_SPMD_WORKER'] = '1'
            os.environ['APHRODITE_USE_RAY_COMPILED_DAG'] = '1'
            if current_platform.is_tpu() or current_platform.is_xpu():
                os.environ['APHRODITE_USE_RAY_COMPILED_DAG_CHANNEL_TYPE'] = 'shm'
        self.use_ray_compiled_dag = envs.APHRODITE_USE_RAY_COMPILED_DAG
        self.use_ray_spmd_worker = envs.APHRODITE_USE_RAY_SPMD_WORKER
        if self.use_ray_compiled_dag:
            assert self.use_ray_spmd_worker, 'APHRODITE_USE_RAY_COMPILED_DAG=1 requires APHRODITE_USE_RAY_SPMD_WORKER=1'
        if self.use_ray_spmd_worker:
            assert self.use_ray_compiled_dag, 'APHRODITE_USE_RAY_SPMD_WORKER=1 requires APHRODITE_USE_RAY_COMPILED_DAG=1'
        assert self.uses_ray
        initialize_ray_cluster(self.parallel_config)
        placement_group = self.parallel_config.placement_group
        ray_usage = os.environ.get('RAY_USAGE_STATS_ENABLED', '0')
        if ray_usage != '1':
            os.environ['RAY_USAGE_STATS_ENABLED'] = '0'
        self._init_workers_ray(placement_group)
        self.input_encoder = msgspec.msgpack.Encoder(enc_hook=encode_hook)
        self.output_decoder = msgspec.msgpack.Decoder(Optional[List[SamplerOutput]])
        self.use_v1 = envs.APHRODITE_USE_V1
        self.pp_locks: Optional[List[asyncio.Lock]] = None
        if not self.use_ray_compiled_dag:
            self.driver_exec_method = make_async(self.driver_worker.execute_method)
    def shutdown(self) -> None:
        logger.info('Shutting down Ray distributed executor. If you see error log from logging.cc regarding SIGTERM received, please ignore because this is the expected termination process in Ray.')
        if hasattr(self, 'forward_dag') and self.forward_dag is not None:
            self.forward_dag.teardown()
            import ray
            for worker in self.workers:
                ray.kill(worker)
            self.forward_dag = None
    def _configure_ray_workers_use_nsight(self, ray_remote_kwargs) -> Dict[str, Any]:
        runtime_env = ray_remote_kwargs.setdefault('runtime_env', {})
        runtime_env.update({'nsight': {'t': 'cuda,cudnn,cublas', 'o': "'worker_process_%p'", 'cuda-graph-trace': 'node'}})
        return ray_remote_kwargs
    def _get_env_vars_to_be_updated(self):
        return self._env_vars_for_all_workers
    def _init_workers_ray(self, placement_group: 'PlacementGroup', **ray_remote_kwargs):
        num_gpus = envs.APHRODITE_RAY_PER_WORKER_GPUS
        self.driver_dummy_worker: Optional[RayWorkerWrapper] = None
        self.workers: List[RayWorkerWrapper] = []
        self.pp_tp_workers: List[List[RayWorkerWrapper]] = []
        if self.parallel_config.ray_workers_use_nsight:
            ray_remote_kwargs = self._configure_ray_workers_use_nsight(ray_remote_kwargs)
        logger.info('use_ray_spmd_worker: {}', self.use_ray_spmd_worker)
        bundle_indices: List[int]
        if envs.APHRODITE_RAY_BUNDLE_INDICES:
            bundle_indices = list(map(int, envs.APHRODITE_RAY_BUNDLE_INDICES.split(',')))
            assert len(bundle_indices) == self.parallel_config.world_size, f'APHRODITE_RAY_BUNDLE_INDICES must have the same size as the world size, but got bundle_indices={bundle_indices!r} and self.parallel_config.world_size={self.parallel_config.world_size!r}'
            assert len(set(bundle_indices)) == len(bundle_indices), f'APHRODITE_RAY_BUNDLE_INDICES cannot have duplicate values, but got bundle_indices={bundle_indices!r}'
        else:
            bundle_indices = []
            for bundle_id, bundle in enumerate(placement_group.bundle_specs):
                if bundle.get(current_platform.ray_device_key, 0):
                    bundle_indices.append(bundle_id)
            bundle_indices = bundle_indices[:self.parallel_config.world_size]
        worker_metadata: List[RayWorkerMetaData] = []
        driver_ip = get_ip()
        for rank, bundle_id in enumerate(bundle_indices):
            scheduling_strategy = PlacementGroupSchedulingStrategy(placement_group=placement_group, placement_group_capture_child_tasks=True, placement_group_bundle_index=bundle_id)
            if current_platform.ray_device_key == 'GPU':
                worker = ray.remote(num_cpus=0, num_gpus=num_gpus, scheduling_strategy=scheduling_strategy, **ray_remote_kwargs)(RayWorkerWrapper).remote(aphrodite_config=self.aphrodite_config, rpc_rank=rank)
            else:
                worker = ray.remote(num_cpus=0, num_gpus=0, resources={current_platform.ray_device_key: num_gpus}, scheduling_strategy=scheduling_strategy, **ray_remote_kwargs)(RayWorkerWrapper).remote(aphrodite_config=self.aphrodite_config, rpc_rank=rank)
            worker_metadata.append(RayWorkerMetaData(worker=worker, created_rank=rank))
        worker_ips = ray.get([each.worker.get_node_ip.remote() for each in worker_metadata])
        for each, ip in zip(worker_metadata, worker_ips):
            each.ip = ip
        if not self.use_ray_spmd_worker:
            for i, each in enumerate(worker_metadata):
                worker = each.worker
                worker_ip = each.ip
                if self.driver_dummy_worker is None and worker_ip == driver_ip:
                    self.driver_dummy_worker = worker
                    self.driver_worker = RayWorkerWrapper(aphrodite_config=self.aphrodite_config, rpc_rank=0)
                    worker_metadata.pop(i)
                    break
        logger.debug('workers: {}', worker_metadata)
        logger.debug('driver_dummy_worker: {}', self.driver_dummy_worker)
        if not self.use_ray_spmd_worker and self.driver_dummy_worker is None:
            raise ValueError(f'Ray does not allocate any GPUs on the driver node.Driver IP: {driver_ip}, worker IPs: {worker_ips}.Consider adjusting the Ray placement group or running the driver on a GPU node.')
        ip_counts: Dict[str, int] = {}
        for ip in worker_ips:
            ip_counts[ip] = ip_counts.get(ip, 0) + 1
        def sort_by_driver_then_worker_ip(item: RayWorkerMetaData):
            ip = item.ip
            return (0 if ip == driver_ip else 1, ip_counts[ip], ip)
        sorted_worker_metadata = sorted(worker_metadata, key=sort_by_driver_then_worker_ip)
        start_rank = 0 if self.use_ray_spmd_worker else 1
        for i, item in enumerate(sorted_worker_metadata):
            item.adjusted_rank = i + start_rank
        self.workers = [item.worker for item in sorted_worker_metadata]
        rerank_mapping = {item.created_rank: item.adjusted_rank for item in sorted_worker_metadata}
        self._run_workers('adjust_rank', rerank_mapping)
        worker_node_and_gpu_ids = []
        for worker in [self.driver_dummy_worker] + self.workers:
            if worker is None:
                continue
            worker_node_and_gpu_ids.append(ray.get(worker.get_node_and_gpu_ids.remote()))
        node_workers = defaultdict(list)
        node_gpus = defaultdict(list)
        for i, (node_id, gpu_ids) in enumerate(worker_node_and_gpu_ids):
            node_workers[node_id].append(i)
            gpu_ids = [int(x) for x in gpu_ids]
            node_gpus[node_id].extend(gpu_ids)
        for node_id, gpu_ids in node_gpus.items():
            node_gpus[node_id] = sorted(gpu_ids)
        all_ips = set(worker_ips + [driver_ip])
        n_ips = len(all_ips)
        n_nodes = len(node_workers)
        if n_nodes != n_ips:
            raise RuntimeError(f'Every node should have a unique IP address. Got {n_nodes} nodes with node ids {list(node_workers.keys())} and {n_ips} unique IP addresses {all_ips}. Please check your network configuration. If you set `APHRODITE_HOST_IP` environment variable, make sure it is unique for each node.')
        all_args_to_update_environment_variables = [{current_platform.device_control_env_var: ','.join(map(str, node_gpus[node_id]))} for node_id, _ in worker_node_and_gpu_ids]
        env_vars_to_copy = get_env_vars_to_copy(exclude_vars=self.WORKER_SPECIFIC_ENV_VARS, additional_vars=set(current_platform.additional_env_vars).union(self.ADDITIONAL_ENV_VARS), destination='workers')
        for args in all_args_to_update_environment_variables:
            for name in env_vars_to_copy:
                if name in os.environ:
                    args[name] = os.environ[name]
        self._env_vars_for_all_workers = all_args_to_update_environment_variables
        self._run_workers('update_environment_variables', self._get_env_vars_to_be_updated())
        if len(node_gpus) == 1:
            driver_ip = '127.0.0.1'
        distributed_init_method = get_distributed_init_method(driver_ip, get_open_port())
        all_kwargs = []
        for rank, (node_id, _) in enumerate(worker_node_and_gpu_ids):
            local_rank = node_workers[node_id].index(rank)
            kwargs = dict(aphrodite_config=self.aphrodite_config, local_rank=local_rank, rank=rank, distributed_init_method=distributed_init_method, is_driver_worker=not self.parallel_config or rank % self.parallel_config.tensor_parallel_size == 0)
            all_kwargs.append(kwargs)
        self._run_workers('init_worker', all_kwargs)
        self._run_workers('init_device')
        self._run_workers('load_model', max_concurrent_workers=self.parallel_config.max_parallel_loading_workers)
        if self.use_ray_spmd_worker:
            for pp_rank in range(self.parallel_config.pipeline_parallel_size):
                self.pp_tp_workers.append([])
                for tp_rank in range(self.parallel_config.tensor_parallel_size):
                    rank = pp_rank * self.parallel_config.tensor_parallel_size + tp_rank
                    assert len(self.pp_tp_workers[pp_rank]) == tp_rank
                    assert pp_rank < len(self.pp_tp_workers)
                    self.pp_tp_workers[pp_rank].append(self.workers[rank])
        self.tp_driver_workers: List[RayWorkerWrapper] = []
        self.non_driver_workers: List[RayWorkerWrapper] = []
        for index, worker in enumerate(self.workers):
            rank = index + 1
            if rank % self.parallel_config.tensor_parallel_size == 0:
                self.tp_driver_workers.append(worker)
            else:
                self.non_driver_workers.append(worker)
    def _driver_execute_model(self, execute_model_req: Optional[ExecuteModelRequest]) -> Optional[List[SamplerOutput]]:
        assert not self.use_ray_spmd_worker, 'driver_worker does not exist for APHRODITE_USE_RAY_SPMD_WORKER=1'
        return self.driver_worker.execute_method('execute_model', execute_model_req)
    def execute_model(self, execute_model_req: ExecuteModelRequest) -> List[SamplerOutput]:
        if not self.use_ray_spmd_worker:
            return super().execute_model(execute_model_req)
        if self.forward_dag is None:
            self.forward_dag = self._compiled_ray_dag(enable_asyncio=False)
        if self.use_v1:
            serialized_data = execute_model_req
        else:
            serialized_data = self.input_encoder.encode(execute_model_req)
        outputs = ray.get(self.forward_dag.execute(serialized_data))
        if self.use_v1:
            output = outputs[0]
        else:
            output = self.output_decoder.decode(outputs[0])
        return output
    def _run_workers(self, method: Union[str, Callable], *args, async_run_tensor_parallel_workers_only: bool=False, max_concurrent_workers: Optional[int]=None, **kwargs) -> Any:
        if isinstance(method, str):
            sent_method = method
        else:
            sent_method = cloudpickle.dumps(method)
        del method
        if self.use_ray_spmd_worker:
            assert not async_run_tensor_parallel_workers_only, 'async_run_tensor_parallel_workers_only is not supported for spmd mode.'
        if max_concurrent_workers:
            raise NotImplementedError('max_concurrent_workers is not supported yet.')
        ray_workers = self.workers
        if async_run_tensor_parallel_workers_only:
            ray_workers = self.non_driver_workers
        ray_worker_outputs = [worker.execute_method.remote(sent_method, *args, **kwargs) for worker in ray_workers]
        if async_run_tensor_parallel_workers_only:
            return ray_worker_outputs
        driver_worker_output = []
        if not self.use_ray_spmd_worker:
            driver_worker_output = [self.driver_worker.execute_method(sent_method, *args, **kwargs)]
        if self.workers:
            ray_worker_outputs = ray.get(ray_worker_outputs)
        return driver_worker_output + ray_worker_outputs
    def _wait_for_tasks_completion(self, parallel_worker_tasks: Any) -> None:
        ray.get(parallel_worker_tasks)
    def _check_ray_cgraph_installation(self):
        import importlib.metadata
        from packaging import version
        required_version = version.parse('2.43.0')
        current_version = version.parse(importlib.metadata.version('ray'))
        if current_version < required_version:
            raise ValueError(f'Ray version {required_version} is required, but found {current_version}')
        import importlib.util
        cgraph_spec = importlib.util.find_spec('ray.experimental.compiled_dag_ref')
        if cgraph_spec is None:
            raise ValueError('Ray Compiled Graph is not installed. Run `pip install ray[cgraph]` to install it.')
        cupy_spec = importlib.util.find_spec('cupy')
        if cupy_spec is None and envs.APHRODITE_USE_RAY_COMPILED_DAG_CHANNEL_TYPE == 'nccl':
            raise ValueError("cupy is not installed but required since APHRODITE_USE_RAY_COMPILED_DAG_CHANNEL_TYPE is set to 'nccl'. Run `pip install ray[cgraph]` and check cupy installation.")
    def _compiled_ray_dag(self, enable_asyncio: bool):
        assert self.parallel_config.use_ray
        self._check_ray_cgraph_installation()
        os.environ.setdefault('RAY_CGRAPH_get_timeout', '300')
        from ray.dag import InputNode, MultiOutputNode
        logger.info('RAY_CGRAPH_get_timeout is set to {}', os.environ['RAY_CGRAPH_get_timeout'])
        logger.info('APHRODITE_USE_RAY_COMPILED_DAG_CHANNEL_TYPE = {}', envs.APHRODITE_USE_RAY_COMPILED_DAG_CHANNEL_TYPE)
        logger.info('APHRODITE_USE_RAY_COMPILED_DAG_OVERLAP_COMM = {}', envs.APHRODITE_USE_RAY_COMPILED_DAG_OVERLAP_COMM)
        channel_type = envs.APHRODITE_USE_RAY_COMPILED_DAG_CHANNEL_TYPE
        if channel_type not in ('auto', 'nccl', 'shm'):
            raise ValueError(f"Invalid value for APHRODITE_USE_RAY_COMPILED_DAG_CHANNEL_TYPE: {channel_type}. Valid values are: 'auto', 'nccl', or 'shm'.")
        with InputNode() as input_data:
            outputs = [input_data for _ in self.pp_tp_workers[0]]
            for pp_rank, tp_group in enumerate(self.pp_tp_workers):
                if self.use_v1:
                    outputs = [worker.execute_model_ray.bind(outputs[i]) for i, worker in enumerate(tp_group)]
                else:
                    outputs = [worker.execute_model_spmd.bind(outputs[i]) for i, worker in enumerate(tp_group)]
                last_pp_rank = len(self.pp_tp_workers) - 1
                if pp_rank < last_pp_rank and envs.APHRODITE_USE_RAY_COMPILED_DAG_CHANNEL_TYPE != 'shm':
                    transport = envs.APHRODITE_USE_RAY_COMPILED_DAG_CHANNEL_TYPE
                    outputs = [output.with_tensor_transport(transport=transport) for output in outputs]
            forward_dag = MultiOutputNode(outputs)
        if envs.APHRODITE_USE_RAY_WRAPPED_PP_COMM:
            from ray.experimental.channel.accelerator_context import register_accelerator_context
            from aphrodite.distributed.device_communicators.ray_communicator import RayPPCommunicator
            register_accelerator_context(torch_module_name='cuda', communicator_cls=RayPPCommunicator)
            logger.info('Using RayPPCommunicator (which wraps Aphrodite _PP GroupCoordinator) for Ray Compiled Graph communication.')
        else:
            logger.info("Using Ray's NCCL communicator for Ray Compiled Graph communication.")
        return forward_dag.experimental_compile(enable_asyncio=enable_asyncio, _overlap_gpu_communication=envs.APHRODITE_USE_RAY_COMPILED_DAG_OVERLAP_COMM)
    def __del__(self):
        self.shutdown()
    async def execute_model_async(self, execute_model_req: ExecuteModelRequest) -> List[SamplerOutput]:
        if not self.use_ray_spmd_worker:
            return await super().execute_model_async(execute_model_req)
        if self.forward_dag is None:
            self.forward_dag = self._compiled_ray_dag(enable_asyncio=True)
        serialized_data = self.input_encoder.encode(execute_model_req)
        dag_future = await self.forward_dag.execute_async(serialized_data)
        output = await dag_future[0]
        return self.output_decoder.decode(output)
    async def _driver_execute_model_async(self, execute_model_req: Optional[ExecuteModelRequest]=None) -> List[SamplerOutput]:
        assert not self.use_ray_spmd_worker, 'driver_worker does not exist for APHRODITE_USE_RAY_SPMD_WORKER=1'
        if not self.tp_driver_workers:
            return await self.driver_exec_method('execute_model', execute_model_req)
        if self.pp_locks is None:
            self.pp_locks = [asyncio.Lock() for _ in range(self.parallel_config.pipeline_parallel_size)]
        tasks = [asyncio.create_task(_run_task_with_lock(self.driver_exec_method, self.pp_locks[0], 'execute_model', execute_model_req))]
        for pp_rank, driver_worker in enumerate(self.tp_driver_workers, start=1):
            tasks.append(asyncio.create_task(_run_task_with_lock(driver_worker.execute_method.remote, self.pp_locks[pp_rank], 'execute_model', execute_model_req)))
        results = await asyncio.gather(*tasks)
        return results[-1]
    async def _start_worker_execution_loop(self):
        assert not self.use_ray_spmd_worker, 'worker loop is disabled for APHRODITE_USE_RAY_SPMD_WORKER=1'
        coros = [worker.execute_method.remote('start_worker_execution_loop') for worker in self.non_driver_workers]
        return await asyncio.gather(*coros)
    def check_health(self) -> None:
        return