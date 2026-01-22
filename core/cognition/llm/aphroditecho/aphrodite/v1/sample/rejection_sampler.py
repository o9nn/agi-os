from typing import Optional
import torch
import torch.nn as nn
from aphrodite.triton_utils import tl, triton
from aphrodite.v1.sample.metadata import SamplingMetadata
from aphrodite.v1.sample.ops.topk_topp_sampler import apply_top_k_top_p
from aphrodite.v1.spec_decode.metadata import SpecDecodeMetadata
PLACEHOLDER_TOKEN_ID: tl.constexpr = -1
GREEDY_TEMPERATURE: tl.constexpr = -1
MAX_SPEC_LEN = 32
class RejectionSampler(nn.Module):
    def forward(self, metadata: SpecDecodeMetadata, draft_probs: Optional[torch.Tensor], target_logits: torch.Tensor, bonus_token_ids: torch.Tensor, sampling_metadata: SamplingMetadata) -> torch.Tensor:
        assert metadata.max_spec_len <= MAX_SPEC_LEN
        target_probs = compute_probs(target_logits, metadata.cu_num_draft_tokens, sampling_metadata)
        output_token_ids = rejection_sample(metadata.draft_token_ids, metadata.num_draft_tokens, metadata.max_spec_len, metadata.cu_num_draft_tokens, draft_probs, target_probs, bonus_token_ids, sampling_metadata)
        return output_token_ids
    @staticmethod
    def parse_output(output_token_ids: torch.Tensor, vocab_size: int) -> list[list[int]]:
        output_token_ids_np = output_token_ids.cpu().numpy()
        valid_mask = (output_token_ids_np != PLACEHOLDER_TOKEN_ID) & (output_token_ids_np < vocab_size)
        outputs = [row[valid_mask[i]].tolist() for i, row in enumerate(output_token_ids_np)]
        return outputs
def rejection_sample(draft_token_ids: torch.Tensor, num_draft_tokens: list[int], max_spec_len: int, cu_num_draft_tokens: torch.Tensor, draft_probs: Optional[torch.Tensor], target_probs: torch.Tensor, bonus_token_ids: torch.Tensor, sampling_metadata: SamplingMetadata) -> torch.Tensor:
    assert draft_token_ids.ndim == 1
    assert draft_probs is None or draft_probs.ndim == 2
    assert cu_num_draft_tokens.ndim == 1
    assert target_probs.ndim == 2
    batch_size = len(num_draft_tokens)
    num_tokens = draft_token_ids.shape[0]
    vocab_size = target_probs.shape[-1]
    device = target_probs.device
    assert draft_token_ids.is_contiguous()
    assert draft_probs is None or draft_probs.is_contiguous()
    assert target_probs.is_contiguous()
    assert bonus_token_ids.is_contiguous()
    assert target_probs.shape == (num_tokens, vocab_size)
    output_token_ids = torch.empty((batch_size, max_spec_len + 1), dtype=torch.int32, device=device)
    output_token_ids.fill_(PLACEHOLDER_TOKEN_ID)
    if sampling_metadata.all_greedy:
        is_greedy = None
    else:
        is_greedy = sampling_metadata.temperature == GREEDY_TEMPERATURE
    if not sampling_metadata.all_random:
        target_argmax = target_probs.argmax(dim=-1)
        rejection_greedy_sample_kernel[batch_size,](output_token_ids, cu_num_draft_tokens, draft_token_ids, target_argmax, bonus_token_ids, is_greedy, max_spec_len, num_warps=1)
        if sampling_metadata.all_greedy:
            return output_token_ids
    uniform_probs = generate_uniform_probs(num_tokens, num_draft_tokens, sampling_metadata.generators, device)
    recovered_token_ids = sample_recovered_tokens(max_spec_len, num_draft_tokens, cu_num_draft_tokens, draft_token_ids, draft_probs, target_probs, sampling_metadata, device)
    rejection_random_sample_kernel[batch_size,](output_token_ids, cu_num_draft_tokens, draft_token_ids, draft_probs, target_probs, bonus_token_ids, recovered_token_ids, uniform_probs, is_greedy, max_spec_len, vocab_size, NO_DRAFT_PROBS=draft_probs is None, num_warps=1)
    return output_token_ids
def compute_probs(logits: torch.Tensor, cu_num_draft_tokens: torch.Tensor, sampling_metadata: SamplingMetadata) -> torch.Tensor:
    assert logits.ndim == 2
    assert cu_num_draft_tokens.ndim == 1
    if sampling_metadata.all_greedy:
        return logits
    num_tokens = logits.shape[0]
    temperature = expand_batch_to_tokens(sampling_metadata.temperature, cu_num_draft_tokens, num_tokens, replace_from=GREEDY_TEMPERATURE, replace_to=1)
    logits.div_(temperature.unsqueeze(-1))
    top_k = None
    if sampling_metadata.top_k is not None:
        top_k = expand_batch_to_tokens(sampling_metadata.top_k, cu_num_draft_tokens, num_tokens)
    top_p = None
    if sampling_metadata.top_p is not None:
        top_p = expand_batch_to_tokens(sampling_metadata.top_p, cu_num_draft_tokens, num_tokens)
    logits = apply_top_k_top_p(logits, top_k, top_p)
    output_prob = logits.softmax(dim=-1, dtype=torch.float32)
    return output_prob
def expand_batch_to_tokens(x: torch.Tensor, cu_num_tokens: torch.Tensor, num_tokens: int, replace_from: int=0, replace_to: int=0) -> torch.Tensor:
    batch_size = x.shape[0]
    assert cu_num_tokens.shape[0] == batch_size
    expanded_x = x.new_empty(num_tokens)
    expand_kernel[batch_size,](expanded_x, x, cu_num_tokens, replace_from, replace_to, MAX_NUM_TOKENS=MAX_SPEC_LEN, num_warps=1)
    return expanded_x
def generate_uniform_probs(num_tokens: int, num_draft_tokens: list[int], generators: dict[int, torch.Generator], device: torch.device) -> torch.Tensor:
    uniform_probs = torch.rand((num_tokens,), dtype=torch.float32, device=device)
    start_idx = 0
    for req_idx, n in enumerate(num_draft_tokens):
        if n == 0:
            continue
        end_idx = start_idx + n
        generator = generators.get(req_idx)
        if generator is not None:
            uniform_probs[start_idx:end_idx].uniform_(generator=generator)
        start_idx = end_idx
    return uniform_probs
def sample_recovered_tokens(max_spec_len: int, num_draft_tokens: list[int], cu_num_draft_tokens: torch.Tensor, draft_token_ids: torch.Tensor, draft_probs: Optional[torch.Tensor], target_probs: torch.Tensor, sampling_metadata: SamplingMetadata, device: torch.device) -> torch.Tensor:
    batch_size = len(num_draft_tokens)
    vocab_size = target_probs.shape[-1]
    q = torch.empty((batch_size, vocab_size), dtype=torch.float32, device=device)
    q.exponential_()
    for i, generator in sampling_metadata.generators.items():
        if num_draft_tokens[i] > 0:
            q[i].exponential_(generator=generator)
    recovered_token_ids = torch.empty_like(draft_token_ids)
    sample_recovered_tokens_kernel[batch_size, max_spec_len](recovered_token_ids, cu_num_draft_tokens, draft_token_ids, draft_probs, target_probs, q, vocab_size, triton.next_power_of_2(vocab_size), NO_DRAFT_PROBS=draft_probs is None)
    return recovered_token_ids
@triton.jit(do_not_specialize=['max_spec_len'])
def rejection_greedy_sample_kernel(output_token_ids_ptr, cu_num_draft_tokens_ptr, draft_token_ids_ptr, target_argmax_ptr, bonus_token_ids_ptr, is_greedy_ptr, max_spec_len):
    req_idx = tl.program_id(0)
    if is_greedy_ptr is None:
        is_greedy = True
    else:
        is_greedy = tl.load(is_greedy_ptr + req_idx)
    if not is_greedy:
        return
    if req_idx == 0:
        start_idx = 0
    else:
        start_idx = tl.load(cu_num_draft_tokens_ptr + req_idx - 1)
    end_idx = tl.load(cu_num_draft_tokens_ptr + req_idx)
    num_draft_tokens = end_idx - start_idx
    rejected = False
    for pos in range(num_draft_tokens):
        if not rejected:
            draft_token_id = tl.load(draft_token_ids_ptr + start_idx + pos)
            target_argmax_id = tl.load(target_argmax_ptr + start_idx + pos)
            tl.store(output_token_ids_ptr + req_idx * (max_spec_len + 1) + pos, target_argmax_id)
            if draft_token_id != target_argmax_id:
                rejected = True
    if not rejected:
        bonus_token_id = tl.load(bonus_token_ids_ptr + req_idx)
        tl.store(output_token_ids_ptr + req_idx * (max_spec_len + 1) + num_draft_tokens, bonus_token_id)
@triton.jit(do_not_specialize=['max_spec_len'])
def rejection_random_sample_kernel(output_token_ids_ptr, cu_num_draft_tokens_ptr, draft_token_ids_ptr, draft_probs_ptr, target_probs_ptr, bonus_token_ids_ptr, recovered_token_ids_ptr, uniform_probs_ptr, is_greedy_ptr, max_spec_len, vocab_size, NO_DRAFT_PROBS: tl.constexpr):
    req_idx = tl.program_id(0)
    is_greedy = tl.load(is_greedy_ptr + req_idx)
    if is_greedy:
        return
    if req_idx == 0:
        start_idx = 0
    else:
        start_idx = tl.load(cu_num_draft_tokens_ptr + req_idx - 1)
    end_idx = tl.load(cu_num_draft_tokens_ptr + req_idx)
    num_draft_tokens = end_idx - start_idx
    rejected = False
    for pos in range(num_draft_tokens):
        if not rejected:
            draft_token_id = tl.load(draft_token_ids_ptr + start_idx + pos)
            if NO_DRAFT_PROBS:
                draft_prob = 1
            else:
                draft_prob = tl.load(draft_probs_ptr + (start_idx + pos) * vocab_size + draft_token_id)
            target_prob = tl.load(target_probs_ptr + (start_idx + pos) * vocab_size + draft_token_id)
            uniform_prob = tl.load(uniform_probs_ptr + start_idx + pos)
            if draft_prob > 0 and target_prob / draft_prob >= uniform_prob:
                token_id = draft_token_id
            else:
                rejected = True
                token_id = tl.load(recovered_token_ids_ptr + start_idx + pos)
            tl.store(output_token_ids_ptr + req_idx * (max_spec_len + 1) + pos, token_id)
    if not rejected:
        bonus_token_id = tl.load(bonus_token_ids_ptr + req_idx)
        tl.store(output_token_ids_ptr + req_idx * (max_spec_len + 1) + num_draft_tokens, bonus_token_id)
@triton.jit(do_not_specialize=['replace_from', 'replace_to'])
def expand_kernel(output_ptr, input_ptr, cu_num_tokens_ptr, replace_from, replace_to, MAX_NUM_TOKENS: tl.constexpr):
    req_idx = tl.program_id(0)
    if req_idx == 0:
        start_idx = 0
    else:
        start_idx = tl.load(cu_num_tokens_ptr + req_idx - 1)
    end_idx = tl.load(cu_num_tokens_ptr + req_idx)
    num_tokens = end_idx - start_idx
    src_val = tl.load(input_ptr + req_idx)
    src_val = tl.where(src_val == replace_from, replace_to, src_val)
    offset = tl.arange(0, MAX_NUM_TOKENS)
    tl.store(output_ptr + start_idx + offset, src_val, mask=offset < num_tokens)
@triton.jit
def sample_recovered_tokens_kernel(output_token_ids_ptr, cu_num_draft_tokens_ptr, draft_token_ids_ptr, draft_probs_ptr, target_probs_ptr, q_ptr, vocab_size, PADDED_VOCAB_SIZE: tl.constexpr, NO_DRAFT_PROBS: tl.constexpr):
    req_idx = tl.program_id(0)
    if req_idx == 0:
        start_idx = 0
    else:
        start_idx = tl.load(cu_num_draft_tokens_ptr + req_idx - 1)
    end_idx = tl.load(cu_num_draft_tokens_ptr + req_idx)
    num_draft_tokens = end_idx - start_idx
    pos = tl.program_id(1)
    if pos >= num_draft_tokens:
        return
    vocab_offset = tl.arange(0, PADDED_VOCAB_SIZE)
    if NO_DRAFT_PROBS:
        draft_token_id = tl.load(draft_token_ids_ptr + start_idx + pos)
        orig_prob = tl.load(target_probs_ptr + (start_idx + pos) * vocab_size + draft_token_id)
        tl.store(target_probs_ptr + (start_idx + pos) * vocab_size + draft_token_id, 0)
        prob = tl.load(target_probs_ptr + (start_idx + pos) * vocab_size + vocab_offset, mask=vocab_offset < vocab_size, other=0)
    else:
        draft_prob = tl.load(draft_probs_ptr + (start_idx + pos) * vocab_size + vocab_offset, mask=vocab_offset < vocab_size, other=0)
        target_prob = tl.load(target_probs_ptr + (start_idx + pos) * vocab_size + vocab_offset, mask=vocab_offset < vocab_size, other=0)
        prob = tl.maximum(target_prob - draft_prob, 0)
    q = tl.load(q_ptr + req_idx * vocab_size + vocab_offset, mask=vocab_offset < vocab_size, other=float('-inf'))
    recovered_id = tl.argmax(prob / q, axis=-1)
    tl.store(output_token_ids_ptr + start_idx + pos, recovered_id)
    if NO_DRAFT_PROBS:
        tl.store(target_probs_ptr + (start_idx + pos) * vocab_size + draft_token_id, orig_prob)