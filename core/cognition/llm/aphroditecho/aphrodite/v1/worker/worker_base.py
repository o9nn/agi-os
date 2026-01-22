from typing import Optional
import torch
import torch.nn as nn
from aphrodite.common.config import AphroditeConfig
from aphrodite.v1.kv_cache_interface import KVCacheSpec
from aphrodite.worker.worker_base import WorkerBase as WorkerBaseV0
class WorkerBase(WorkerBaseV0):
    def __init__(self, aphrodite_config: AphroditeConfig, local_rank: int, rank: int, distributed_init_method: str, is_driver_worker: bool=False):
        super().__init__(aphrodite_config=aphrodite_config)
        self.parallel_config.rank = rank
        self.local_rank = local_rank
        self.rank = rank
        self.distributed_init_method = distributed_init_method
        self.is_driver_worker = is_driver_worker
        self.device: Optional[torch.device] = None
        self.model_runner: Optional[nn.Module] = None
    def get_kv_cache_spec(self) -> dict[str, KVCacheSpec]:
        raise NotImplementedError
    def compile_or_warm_up_model(self) -> None:
        raise NotImplementedError
    def check_health(self) -> None:
        return