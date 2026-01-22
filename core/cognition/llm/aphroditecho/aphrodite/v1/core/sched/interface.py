from abc import ABC, abstractmethod
from collections.abc import Iterable
from typing import TYPE_CHECKING, Optional, Union
if TYPE_CHECKING:
    from aphrodite.distributed.kv_transfer.kv_connector.v1 import KVConnectorBase_V1
    from aphrodite.v1.core.sched.output import SchedulerOutput
    from aphrodite.v1.engine import EngineCoreOutputs
    from aphrodite.v1.metrics.stats import SchedulerStats
    from aphrodite.v1.outputs import ModelRunnerOutput
    from aphrodite.v1.request import Request, RequestStatus
class SchedulerInterface(ABC):
    @abstractmethod
    def schedule(self) -> 'SchedulerOutput':
        raise NotImplementedError
    @abstractmethod
    def update_from_output(self, scheduler_output: 'SchedulerOutput', model_runner_output: 'ModelRunnerOutput') -> dict[int, 'EngineCoreOutputs']:
        raise NotImplementedError
    @abstractmethod
    def add_request(self, request: 'Request') -> None:
        raise NotImplementedError
    @abstractmethod
    def finish_requests(self, request_ids: Union[str, Iterable[str]], finished_status: 'RequestStatus') -> None:
        raise NotImplementedError
    @abstractmethod
    def get_num_unfinished_requests(self) -> int:
        raise NotImplementedError
    def has_unfinished_requests(self) -> bool:
        return self.get_num_unfinished_requests() > 0
    @abstractmethod
    def has_finished_requests(self) -> bool:
        raise NotImplementedError
    def has_requests(self) -> bool:
        return self.has_unfinished_requests() or self.has_finished_requests()
    @abstractmethod
    def reset_prefix_cache(self) -> bool:
        raise NotImplementedError
    @abstractmethod
    def get_request_counts(self) -> tuple[int, int]:
        raise NotImplementedError
    @abstractmethod
    def make_stats(self) -> Optional['SchedulerStats']:
        raise NotImplementedError
    @abstractmethod
    def shutdown(self) -> None:
        raise NotImplementedError
    def get_kv_connector(self) -> Optional['KVConnectorBase_V1']:
        return None