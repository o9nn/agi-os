from dataclasses import dataclass
from typing import TYPE_CHECKING, Any, Optional, Union
from aphrodite.common.sequence import Logprob
from aphrodite.lora.request import LoRARequest
if TYPE_CHECKING:
    from aphrodite.multimodal import MultiModalDataDict
@dataclass
class BeamSearchSequence:
    tokens: list[int]
    logprobs: list[dict[int, Logprob]]
    lora_request: Optional[LoRARequest] = None
    cum_logprob: float = 0.0
    text: Optional[str] = None
    finish_reason: Optional[str] = None
    stop_reason: Union[int, str, None] = None
    multi_modal_data: Optional['MultiModalDataDict'] = None
    mm_processor_kwargs: Optional[dict[str, Any]] = None
@dataclass
class BeamSearchOutput:
    sequences: list[BeamSearchSequence]
class BeamSearchInstance:
    def __init__(self, prompt_tokens: list[int], lora_request: Optional[LoRARequest]=None, logprobs: Optional[list[dict[int, Logprob]]]=None, **kwargs):
        self.beams: list[BeamSearchSequence] = [BeamSearchSequence(tokens=prompt_tokens, logprobs=[] if logprobs is None else list(logprobs), lora_request=lora_request, **kwargs)]
        self.completed: list[BeamSearchSequence] = []
def get_beam_search_score(tokens: list[int], cumulative_logprob: float, eos_token_id: int, length_penalty: float=1.0) -> float:
    seq_len = len(tokens)
    if tokens[-1] == eos_token_id:
        seq_len -= 1
    return cumulative_logprob / seq_len ** length_penalty
def create_sort_beams_key_function(eos_token_id: int, length_penalty: float):
    def sort_beams_key(x: BeamSearchSequence) -> float:
        return get_beam_search_score(x.tokens, x.cum_logprob, eos_token_id, length_penalty)
    return sort_beams_key