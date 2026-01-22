import torch
from aphrodite.modeling.layers.utils import apply_penalties
from aphrodite.utils import is_pin_memory_available, make_tensor_with_pad
def apply_all_penalties(logits: torch.Tensor, prompt_token_ids: torch.Tensor, presence_penalties: torch.Tensor, frequency_penalties: torch.Tensor, repetition_penalties: torch.Tensor, output_token_ids: list[list[int]]) -> torch.Tensor:
    _, vocab_size = logits.shape
    output_tokens_t = _convert_to_tensors(output_token_ids, vocab_size, logits.device)
    return apply_penalties(logits, prompt_token_ids, output_tokens_t, presence_penalties, frequency_penalties, repetition_penalties)
def _convert_to_tensors(output_token_ids: list[list[int]], vocab_size: int, device: torch.device) -> torch.Tensor:
    output_tokens_tensor = make_tensor_with_pad(output_token_ids, pad=vocab_size, device='cpu', dtype=torch.int64, pin_memory=is_pin_memory_available())
    return output_tokens_tensor.to(device, non_blocking=True)