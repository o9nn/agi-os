from typing import List
from aphrodite.common.config import SchedulerConfig
from aphrodite.common.sequence import CompletionSequenceGroupOutput, SequenceGroup, SequenceGroupOutput
from aphrodite.utils import Counter
from aphrodite.engine.output_processor.interfaces import SequenceGroupOutputProcessor
from aphrodite.engine.output_processor.stop_checker import StopChecker
from aphrodite.processing.scheduler import Scheduler
from aphrodite.transformers_utils.detokenizer import Detokenizer
def single_step_process_prompt_logprob(sg_output_proc: SequenceGroupOutputProcessor, seq_group: SequenceGroup, output: CompletionSequenceGroupOutput) -> None:
    prompt_logprobs = output.prompt_logprobs
    if prompt_logprobs is not None:
        if not seq_group.prompt_logprobs:
            prompt_logprobs = [None] + prompt_logprobs
            seq_group.prompt_logprobs = []
        assert hasattr(sg_output_proc, 'detokenizer')
        if seq_group.sampling_params.detokenize and sg_output_proc.detokenizer:
            sg_output_proc.detokenizer.decode_prompt_logprobs_inplace(seq_group, prompt_logprobs, position_offset=len(seq_group.prompt_logprobs))
        seq_group.prompt_logprobs.extend(prompt_logprobs)
class SingleStepOutputProcessor(SequenceGroupOutputProcessor):
    def __init__(self, scheduler_config: SchedulerConfig, detokenizer: Detokenizer, scheduler: List[Scheduler], seq_counter: Counter, stop_checker: StopChecker):
        self.scheduler_config = scheduler_config
        self.detokenizer = detokenizer
        self.scheduler = scheduler
        self.seq_counter = seq_counter
        self.stop_checker = stop_checker
    def process_outputs(self, sequence_group: SequenceGroup, outputs: List[SequenceGroupOutput], is_async: bool) -> None:
        assert len(outputs) == 1, f'{type(self)} does not support multiple outputs per step'
        return self._process_sequence_group_outputs(sequence_group, outputs[0], is_async)
    def process_prompt_logprob(self, seq_group: SequenceGroup, outputs: List[SequenceGroupOutput]) -> None:
        assert len(outputs) == 1, 'Single step should only have 1 output.'
        output = outputs[0]
        assert isinstance(output, CompletionSequenceGroupOutput)
        single_step_process_prompt_logprob(self, seq_group, output)
    def _process_sequence_group_outputs(self, seq_group: SequenceGroup, outputs: SequenceGroupOutput, is_async: bool) -> None:
        sampling_params = seq_group.sampling_params
        sample = outputs.samples[0]
        seq = seq_group.first_seq
        if not is_async:
            seq.append_token_id(sample.output_token, sample.logprobs, sample.output_embed)
        if sampling_params.detokenize and self.detokenizer:
            new_char_count = self.detokenizer.decode_sequence_inplace(seq, sampling_params)
        else:
            new_char_count = 0
        self.stop_checker.maybe_stop_sequence(seq, new_char_count, sampling_params, lora_req=seq_group.lora_request)
        if seq.is_finished():
            for scheduler in self.scheduler:
                scheduler.free_seq(seq)