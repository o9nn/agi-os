import random
from dataclasses import dataclass
from typing import Optional, Union
import torch
from transformers import PreTrainedTokenizer, PreTrainedTokenizerFast
from aphrodite.engine.args_tools import EngineArgs
from aphrodite.transformers_utils.tokenizer_group import TokenizerGroup
from aphrodite.v1.engine import EngineCoreOutput, FinishReason
from aphrodite.v1.outputs import LogprobsLists, LogprobsTensors
GeneralTokenizerType = Union[PreTrainedTokenizer, PreTrainedTokenizerFast]
NUM_SAMPLE_LOGPROBS_UNDER_TEST = 5
NUM_PROMPT_LOGPROBS_UNDER_TEST = 7
TOKENIZER_NAME = 'meta-llama/Llama-3.2-1B'
FULL_STRINGS = ['My name is Robert from Neural Magic and I love working on Aphrodite so much!', 'Red Hat is the best open source company by far across Linux, K8s, and AI.', 'Nick is the name of my brother in addition to my colleague from Red Hat.']
STOP_STRINGS = ['I love working on', 'company by far', 'brother in']
PROMPT_LEN = 5
random.seed(42)
def _create_random_top_logprob_test_vector(num_logprobs: int, lower: float, upper: float) -> torch.Tensor:
    return torch.rand(num_logprobs) * (upper - lower) + lower
def _create_random_top_logprob_test_matrix(shape: tuple, lower: float, upper: float) -> torch.Tensor:
    return torch.rand(*shape) * (upper - lower) + lower
def _create_random_top_token_test_vector(num_logprobs: int, lower: int, upper: int, sampled_token_id: int, adjust_num_logprobs: bool=True) -> tuple[torch.Tensor, int]:
    total_logprobs = num_logprobs + 1 if adjust_num_logprobs else num_logprobs
    choice_tensor = torch.randperm(upper - lower)[:total_logprobs] + lower
    choice_tensor[0] = sampled_token_id
    if sampled_token_id in choice_tensor[1:]:
        sampled_token_rank = (choice_tensor[1:] == sampled_token_id).nonzero(as_tuple=True)[0].item()
    else:
        sampled_token_rank = random.randint(num_logprobs, 50700)
    return (choice_tensor, sampled_token_rank)
def _create_random_top_token_test_matrix(shape: tuple[int, int], lower: int, upper: int, tokens_list: list[int]) -> tuple[torch.Tensor, torch.Tensor]:
    num_elements = shape[0] * shape[1]
    choice_tensor = torch.randperm(upper - lower)[:num_elements] + lower
    matrix = torch.cat((torch.tensor(tokens_list, dtype=torch.int).unsqueeze(-1), choice_tensor.view(shape)), dim=1)
    prompt_token_ranks = torch.empty(shape[0], dtype=torch.int)
    for rdx in range(shape[0]):
        row = matrix[rdx, 1:]
        token_index = (row == tokens_list[rdx]).nonzero(as_tuple=True)[0]
        if token_index.numel() > 0:
            prompt_token_ranks[rdx] = token_index.item()
        else:
            prompt_token_ranks[rdx] = random.randint(shape[1], 50700)
    return (matrix, prompt_token_ranks)
def decode_token(tok_id: int, tokenizer: PreTrainedTokenizer) -> str:
    return tokenizer.convert_ids_to_tokens(tok_id)
def generate_dummy_sample_logprobs(sampled_tokens_list: list, num_logprobs: int, tokenizer: PreTrainedTokenizer) -> list[tuple[list[int], list[float], int]]:
    res = []
    for sampled_token_id in sampled_tokens_list:
        token_vector, sampled_token_rank = _create_random_top_token_test_vector(num_logprobs, 0, len(tokenizer.vocab) - 1, sampled_token_id)
        res.append((token_vector, _create_random_top_logprob_test_vector(num_logprobs + 1, -100, 0), sampled_token_rank))
    res_list_format = [(log_probs_tensor.tolist(), token_ids_tensor.tolist(), sampled_token_rank) for log_probs_tensor, token_ids_tensor, sampled_token_rank in res]
    return res_list_format
def generate_dummy_prompt_logprobs_tensors(prompt_tokens_list: list, num_logprobs: int, tokenizer: PreTrainedTokenizer) -> LogprobsTensors:
    num_prompt_logprobs = len(prompt_tokens_list) - 1
    token_vector, prompt_token_ranks = _create_random_top_token_test_matrix((num_prompt_logprobs, num_logprobs), 0, len(tokenizer.vocab) - 1, prompt_tokens_list[1:])
    return LogprobsTensors(token_vector, _create_random_top_logprob_test_matrix((num_prompt_logprobs, num_logprobs + 1), -100, 0), prompt_token_ranks)
@dataclass
class DummyOutputProcessorTestVectors:
    tokenizer: GeneralTokenizerType
    tokenizer_group: TokenizerGroup
    aphrodite_config: EngineArgs
    full_tokens: list[list[int]]
    prompt_tokens: list[list[int]]
    generation_tokens: list[list[int]]
    prompt_logprobs: list[LogprobsTensors]
    generation_logprobs: list[list[tuple[list[int], list[float], int]]]
    prompt_strings: list[str]
    prompt_strings_len: list[int]
    generation_strings: list[str]
class MockEngineCore:
    def __init__(self, tokens_list: list[list[int]], generated_logprobs_raw: Optional[list[list[tuple[list[int], list[float], int]]]]=None, prompt_logprobs_raw: Optional[list[LogprobsTensors]]=None, eos_token_id: Optional[int]=None, stop_token_ids: Optional[list[int]]=None, ignore_eos: bool=False) -> None:
        self.num_requests = len(tokens_list)
        self.tokens_list = tokens_list
        self.current_idx = 0
        self.generated_logprobs_raw = generated_logprobs_raw
        self.do_logprobs = generated_logprobs_raw is not None
        self.prompt_logprobs_raw = prompt_logprobs_raw
        self.do_prompt_logprobs = prompt_logprobs_raw is not None
        self.request_finished = [False for _ in range(self.num_requests)]
        self.eos_token_id = eos_token_id
        self.stop_token_ids = stop_token_ids
        self.ignore_eos = ignore_eos
    def get_outputs(self) -> list[EngineCoreOutput]:
        do_logprobs = self.do_logprobs
        do_prompt_logprobs = self.do_prompt_logprobs
        token_idx = self.current_idx
        outputs = []
        for req_idx, token_ids in enumerate(self.tokens_list):
            if not self.request_finished[req_idx]:
                if do_logprobs:
                    assert self.generated_logprobs_raw is not None
                    logprobs_token_ids_, logprobs_, sampled_token_ranks_ = self.generated_logprobs_raw[req_idx][token_idx]
                    logprobs = LogprobsLists([logprobs_token_ids_], [logprobs_], [sampled_token_ranks_])
                else:
                    logprobs = None
                if do_prompt_logprobs:
                    if self.current_idx == 0:
                        assert self.prompt_logprobs_raw is not None
                        prompt_logprobs = self.prompt_logprobs_raw[req_idx]
                    else:
                        prompt_logprobs = None
                else:
                    prompt_logprobs = None
                new_token_id = token_ids[token_idx]
                output = EngineCoreOutput(request_id=f'request-{req_idx}', new_token_ids=[new_token_id], new_logprobs=logprobs, new_prompt_logprobs_tensors=prompt_logprobs)
                if token_idx == len(token_ids) - 1:
                    output.finish_reason = FinishReason.LENGTH
                    self.request_finished[req_idx] = True
                if not self.ignore_eos and new_token_id == self.eos_token_id:
                    output.finish_reason = FinishReason.STOP
                    self.request_finished[req_idx] = True
                if new_token_id in (self.stop_token_ids or ()):
                    output.finish_reason = FinishReason.STOP
                    output.stop_reason = new_token_id
                    self.request_finished[req_idx] = True
                outputs.append(output)
        self.current_idx += 1
        return outputs