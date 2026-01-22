from typing import List
from typing import Sequence as GenericSequence
from typing import cast
from aphrodite.common.sequence import CompletionSequenceGroupOutput, SequenceGroupOutput
from aphrodite.modeling.layers.sampler import SamplerOutput
def create_output_by_sequence_group(outputs: GenericSequence[SamplerOutput], num_seq_groups: int) -> List[List[SequenceGroupOutput]]:
    output_by_sequence_group: List[List[CompletionSequenceGroupOutput]] = [[] for _ in range(num_seq_groups)]
    for step in outputs:
        sequence_group_output: CompletionSequenceGroupOutput
        for i, sequence_group_output in enumerate(step):
            output_by_sequence_group[i].append(sequence_group_output)
    return cast(List[List[SequenceGroupOutput]], output_by_sequence_group)