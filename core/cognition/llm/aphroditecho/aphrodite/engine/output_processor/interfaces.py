from abc import ABC, abstractmethod
from typing import Callable, List
from aphrodite.common.config import SchedulerConfig
from aphrodite.common.sequence import Sequence, SequenceGroup, SequenceGroupOutput
from aphrodite.utils import Counter
from aphrodite.engine.output_processor.stop_checker import StopChecker
from aphrodite.processing.scheduler import Scheduler
from aphrodite.transformers_utils.detokenizer import Detokenizer
from aphrodite.transformers_utils.tokenizer import AnyTokenizer
class SequenceGroupOutputProcessor(ABC):
    @staticmethod
    def create_output_processor(scheduler_config: SchedulerConfig, detokenizer: Detokenizer, scheduler: List[Scheduler], seq_counter: Counter, get_tokenizer_for_seq: Callable[[Sequence], AnyTokenizer], stop_checker: 'StopChecker'):
        if scheduler_config.num_lookahead_slots == 0:
            from aphrodite.engine.output_processor.single_step import SingleStepOutputProcessor
            return SingleStepOutputProcessor(scheduler_config, detokenizer, scheduler, seq_counter, stop_checker)
        else:
            from aphrodite.engine.output_processor.multi_step import MultiStepOutputProcessor
            return MultiStepOutputProcessor(detokenizer, scheduler, seq_counter, get_tokenizer_for_seq, stop_checker)
    @abstractmethod
    def process_outputs(self, sequence_group: SequenceGroup, outputs: List[SequenceGroupOutput], is_async: bool) -> None:
        pass
    @abstractmethod
    def process_prompt_logprob(self, seq_group: SequenceGroup, outputs: List[SequenceGroupOutput]) -> None:
        pass