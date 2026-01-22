from concurrent.futures import Future
from typing import Optional, Union
from aphrodite.distributed.kv_transfer.kv_connector.utils import KVOutputAggregator
from aphrodite.executor.ray_distributed_executor import RayDistributedExecutor as RayDistributedExecutorV0
from aphrodite.v1.engine import ReconfigureDistributedRequest, ReconfigureRankType
from aphrodite.v1.executor.abstract import Executor
from aphrodite.v1.outputs import ModelRunnerOutput
class FutureWrapper(Future):
    def __init__(self, refs, aggregator: Optional[KVOutputAggregator]=None):
        super().__init__()
        self.refs = refs
        self.aggregator = aggregator
    def result(self, timeout=None):
        if timeout is not None:
            raise NotImplementedError('timeout is not supported')
        if self.aggregator is None:
            return self.refs[0].get()
        outputs = [ref.get() for ref in self.refs]
        return self.aggregator.aggregate(outputs, output_rank=0)
class RayDistributedExecutor(RayDistributedExecutorV0, Executor):
    supports_pp: bool = True
    def _init_executor(self) -> None:
        super()._init_executor()
        self.has_connector = self.aphrodite_config.kv_transfer_config is not None
        self.kv_output_aggregator = KVOutputAggregator(self.parallel_config.world_size)
    @property
    def max_concurrent_batches(self) -> int:
        if self.scheduler_config.async_scheduling:
            return 2
        return self.parallel_config.pipeline_parallel_size
    def execute_model(self, scheduler_output) -> Union[ModelRunnerOutput, Future[ModelRunnerOutput]]:
        if self.forward_dag is None:
            self.forward_dag = self._compiled_ray_dag(enable_asyncio=False)
        refs = self.forward_dag.execute(scheduler_output)
        if not self.has_connector:
            if self.max_concurrent_batches == 1:
                return refs[0].get()
            return FutureWrapper(refs)
        if self.max_concurrent_batches == 1:
            outputs = [ref.get() for ref in refs]
            return self.kv_output_aggregator.aggregate(outputs)
        return FutureWrapper(refs, self.kv_output_aggregator)
    def reinitialize_distributed(self, reconfig_request: ReconfigureDistributedRequest) -> None:
        self._run_workers('reinitialize_distributed', reconfig_request)
        if reconfig_request.new_data_parallel_rank == ReconfigureRankType.SHUTDOWN_CURRENT_RANK:
            self.shutdown()
        return