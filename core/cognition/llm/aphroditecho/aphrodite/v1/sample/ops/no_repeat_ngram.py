import torch
from aphrodite.utils import is_pin_memory_available, make_tensor_with_pad
from aphrodite.v1.sample.metadata import SamplingMetadata
def _get_ngrams(ngram_size: int, prev_input_ids: torch.Tensor) -> dict[tuple[int, ...], list[int]]:
    generated_ngrams = {}
    gen_tokens = prev_input_ids.tolist()
    for i in range(len(gen_tokens) - ngram_size + 1):
        ngram = tuple(gen_tokens[i:i + ngram_size - 1])
        next_token = gen_tokens[i + ngram_size - 1]
        if ngram in generated_ngrams:
            generated_ngrams[ngram].append(next_token)
        else:
            generated_ngrams[ngram] = [next_token]
    return generated_ngrams
def _get_generated_ngrams(banned_ngrams: dict[tuple[int, ...], list[int]], prev_input_ids: torch.Tensor, ngram_size: int, cur_len: int) -> list[int]:
    start_idx = cur_len + 1 - ngram_size
    current_ngram = tuple(prev_input_ids[start_idx:cur_len].tolist())
    return banned_ngrams.get(current_ngram, [])
def _calc_banned_ngram_tokens(ngram_size: int, prev_input_ids: torch.Tensor, cur_len: int) -> list[int]:
    if cur_len + 1 < ngram_size:
        return []
    generated_ngrams = _get_ngrams(ngram_size, prev_input_ids)
    banned_tokens = _get_generated_ngrams(generated_ngrams, prev_input_ids, ngram_size, cur_len)
    return banned_tokens
def no_repeat_ngram(logits: torch.Tensor, sampling_metadata: SamplingMetadata) -> torch.Tensor:
    ngram_size = sampling_metadata.no_repeat_ngram_size
    if ngram_size is None or torch.all(ngram_size == 0):
        return logits
    batch_size = logits.shape[0]
    for i in range(batch_size):
        size = int(ngram_size[i].item())
        if size == 0:
            continue
        _, vocab_size = logits.shape
        output_tokens_t = make_tensor_with_pad(sampling_metadata.output_token_ids, pad=vocab_size, device='cpu', dtype=torch.int64, pin_memory=is_pin_memory_available()).to(logits.device, non_blocking=True)
        if sampling_metadata.prompt_token_ids is not None:
            prompt_tokens = sampling_metadata.prompt_token_ids[i]
            output_tokens = output_tokens_t[i]
            output_tokens = output_tokens[output_tokens != vocab_size]
            input_ids = torch.cat([prompt_tokens, output_tokens])
        else:
            input_ids = output_tokens_t[i]
            input_ids = input_ids[input_ids != vocab_size]
        cur_len = len(input_ids)
        if cur_len < size:
            continue
        banned_tokens = _calc_banned_ngram_tokens(ngram_size=size, prev_input_ids=input_ids, cur_len=cur_len - 1)
        if banned_tokens:
            logits[i, banned_tokens] = -float('inf')
    return logits