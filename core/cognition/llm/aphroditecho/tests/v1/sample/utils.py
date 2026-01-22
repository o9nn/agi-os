import re
from enum import Enum
from typing import Optional
from aphrodite import CompletionOutput
class BatchLogprobsComposition(Enum):
    NONE = 0
    SAMPLE = 1
    PROMPT = 2
    SAMPLE_PROMPT = 3
BatchLogprobsSpecType = list[tuple[Optional[int], Optional[int]]]
def get_test_batch(batch_logprobs_composition: BatchLogprobsComposition) -> BatchLogprobsSpecType:
    if batch_logprobs_composition == BatchLogprobsComposition.NONE:
        return [(None, None)]
    elif batch_logprobs_composition == BatchLogprobsComposition.SAMPLE:
        return [(None, None), (0, None), (5, None), (3, None)]
    elif batch_logprobs_composition == BatchLogprobsComposition.PROMPT:
        return [(None, None), (None, 0), (None, 6), (None, 5)]
    elif batch_logprobs_composition == BatchLogprobsComposition.SAMPLE_PROMPT:
        return [(None, None), (0, None), (5, None), (3, None), (0, 3), (6, 0), (6, 3), (None, 6), (None, 5), (None, 0)]
    else:
        raise ValueError('Invalid logprobs batch configuration for test.')
def assert_incr_detok_str_matches_non_incr_detok_str(incremental_detokenization_str: str, non_incremental_detokenization_str: str, msg: str) -> None:
    rgx = '[^a-zA-Z0-9]+'
    assert re.sub(rgx, '', incremental_detokenization_str) == re.sub(rgx, '', non_incremental_detokenization_str), msg
def compute_correct_cumulative_logprob(completion_output: CompletionOutput) -> float:
    token_ids = completion_output.token_ids
    logprobs = completion_output.logprobs
    assert logprobs is not None
    return sum([lp[tok_id].logprob for tok_id, lp in zip(token_ids, logprobs)])