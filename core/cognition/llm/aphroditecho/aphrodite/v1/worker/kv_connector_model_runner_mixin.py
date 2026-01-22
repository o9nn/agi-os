import copy
from contextlib import AbstractContextManager, contextmanager, nullcontext
from typing import Generator
from typing import TYPE_CHECKING, Optional
from aphrodite.common.config import AphroditeConfig
from aphrodite.distributed.kv_transfer import get_kv_transfer_group, has_kv_transfer_group
from aphrodite.distributed.kv_transfer.kv_connector.base import KVConnectorBase
from aphrodite.forward_context import get_forward_context, set_forward_context
from aphrodite.v1.outputs import EMPTY_MODEL_RUNNER_OUTPUT, KVConnectorOutput, ModelRunnerOutput
if TYPE_CHECKING:
    from aphrodite.v1.core.sched.output import SchedulerOutput
class KVConnectorModelRunnerMixin:
    @staticmethod
    def maybe_setup_kv_connector(scheduler_output: 'SchedulerOutput'):
        if has_kv_transfer_group():
            kv_connector = get_kv_transfer_group()
            assert isinstance(kv_connector, KVConnectorBase)
            assert scheduler_output.kv_connector_metadata is not None
            kv_connector.bind_connector_metadata(scheduler_output.kv_connector_metadata)
            kv_connector.start_load_kv(get_forward_context())
    @staticmethod
    def maybe_wait_for_kv_save() -> None:
        if has_kv_transfer_group():
            get_kv_transfer_group().wait_for_save()
    @staticmethod
    def get_finished_kv_transfers(scheduler_output: 'SchedulerOutput') -> tuple[Optional[set[str]], Optional[set[str]]]:
        if has_kv_transfer_group():
            return get_kv_transfer_group().get_finished(scheduler_output.finished_req_ids)
        return (None, None)
    @staticmethod
    def kv_connector_no_forward(scheduler_output: 'SchedulerOutput', aphrodite_config: AphroditeConfig) -> ModelRunnerOutput:
        with set_forward_context(None, aphrodite_config), KVConnectorModelRunnerMixin._get_kv_connector_output(scheduler_output, wait_for_save=False) as kv_connector_output:
            pass
        if not kv_connector_output.finished_sending and (not kv_connector_output.finished_recving):
            return EMPTY_MODEL_RUNNER_OUTPUT
        output = copy.copy(EMPTY_MODEL_RUNNER_OUTPUT)
        output.kv_connector_output = kv_connector_output
        return output
    @staticmethod
    def maybe_get_kv_connector_output(scheduler_output: 'SchedulerOutput') -> AbstractContextManager[Optional[KVConnectorOutput]]:
        return KVConnectorModelRunnerMixin._get_kv_connector_output(scheduler_output) if has_kv_transfer_group() else nullcontext()
    @staticmethod
    @contextmanager
    def _get_kv_connector_output(scheduler_output: 'SchedulerOutput', wait_for_save: bool=True) -> Generator[KVConnectorOutput, None, None]:
        output = KVConnectorOutput()
        kv_connector = get_kv_transfer_group()
        assert isinstance(kv_connector, KVConnectorBase)
        assert scheduler_output.kv_connector_metadata is not None
        kv_connector.bind_connector_metadata(scheduler_output.kv_connector_metadata)
        kv_connector.start_load_kv(get_forward_context())
        try:
            yield output
        finally:
            if wait_for_save:
                kv_connector.wait_for_save()
            output.finished_sending, output.finished_recving = kv_connector.get_finished(scheduler_output.finished_req_ids)
            kv_connector.clear_connector_metadata()