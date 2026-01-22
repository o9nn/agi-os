import itertools
from collections.abc import Iterable
from dataclasses import dataclass
from typing import Optional
from aphrodite.common.sequence import Logprob, PromptLogprobs, SampleLogprobs
from aphrodite.transformers_utils.detokenizer_utils import AnyTokenizer, convert_ids_list_to_tokens
from aphrodite.v1.engine import EngineCoreOutput, EngineCoreRequest
from aphrodite.v1.outputs import LogprobsLists, LogprobsTensors
NONES = itertools.repeat(None)
@dataclass
class LogprobsProcessor:
    tokenizer: Optional[AnyTokenizer]
    logprobs: Optional[SampleLogprobs]
    prompt_logprobs: Optional[PromptLogprobs]
    cumulative_logprob: Optional[float]
    num_logprobs: Optional[int]
    num_prompt_logprobs: Optional[int]
    @classmethod
    def from_new_request(cls, tokenizer: Optional[AnyTokenizer], request: EngineCoreRequest) -> 'LogprobsProcessor':
        assert request.sampling_params is not None
        num_logprobs = request.sampling_params.logprobs
        num_prompt_logprobs = request.sampling_params.prompt_logprobs
        return cls(tokenizer=tokenizer, cumulative_logprob=None if num_logprobs is None else 0.0, logprobs=None if num_logprobs is None else [], prompt_logprobs=None if num_prompt_logprobs is None else [None], num_prompt_logprobs=num_prompt_logprobs, num_logprobs=num_logprobs)
    def _update_sample_logprobs(self, logprobs_lists: LogprobsLists) -> None:
        assert self.num_logprobs is not None
        assert self.logprobs is not None
        assert self.cumulative_logprob is not None
        token_ids_lst, logprobs_lst, ranks_lst = logprobs_lists
        for rank, logprobs, token_ids in zip(ranks_lst, logprobs_lst, token_ids_lst):
            decoded_tokens = NONES if self.tokenizer is None else convert_ids_list_to_tokens(self.tokenizer, token_ids)
            sampled_token_logprob = logprobs[0]
            self.cumulative_logprob += sampled_token_logprob
            self.logprobs.append(self._make_logprob_dict(logprobs, token_ids, decoded_tokens, rank, self.num_logprobs))
    def _update_prompt_logprobs(self, prompt_logprobs_tensors: LogprobsTensors) -> None:
        assert self.num_prompt_logprobs is not None
        assert self.prompt_logprobs is not None
        token_ids, logprobs, ranks = prompt_logprobs_tensors
        decoded_tokens = None if self.tokenizer is None else convert_ids_list_to_tokens(self.tokenizer, token_ids.flatten().tolist())
        num_prompt_tokens, num_logprobs = logprobs.shape
        prompt_token_ranks = ranks.tolist()
        prompt_logprobs = logprobs.tolist()
        token_ids = token_ids.tolist()
        for pos in range(num_prompt_tokens):
            offset = pos * num_logprobs
            offset_end = offset + num_logprobs
            decoded_tokens_for_pos = NONES if decoded_tokens is None else decoded_tokens[offset:offset_end]
            self.prompt_logprobs.append(self._make_logprob_dict(prompt_logprobs[pos], token_ids[pos], decoded_tokens_for_pos, prompt_token_ranks[pos], self.num_prompt_logprobs))
    def pop_prompt_logprobs(self) -> Optional[PromptLogprobs]:
        plp = self.prompt_logprobs
        if plp:
            self.prompt_logprobs = []
        return plp
    @staticmethod
    def _make_logprob_dict(logprobs: list[float], logprob_token_ids: list[int], decoded_tokens: Iterable[Optional[str]], rank: int, num_logprobs: int) -> dict[int, Logprob]:
        if num_logprobs == -1:
            num_logprobs = len(logprobs)
        topk_ranks = range(1, num_logprobs + 1)
        ranks = itertools.chain((rank,), topk_ranks)
        return {token_id: Logprob(logprob=logprob, rank=rank, decoded_token=token) for token_id, logprob, rank, token in zip(logprob_token_ids, logprobs, ranks, decoded_tokens)}
    def update_from_output(self, output: EngineCoreOutput) -> None:
        if output.new_logprobs is not None:
            self._update_sample_logprobs(output.new_logprobs)
        if output.new_prompt_logprobs_tensors is not None:
            self._update_prompt_logprobs(output.new_prompt_logprobs_tensors)