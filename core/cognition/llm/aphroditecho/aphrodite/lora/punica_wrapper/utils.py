from typing import TYPE_CHECKING, Optional, Union
import torch
if TYPE_CHECKING:
    from aphrodite.lora.layers import LoRAMapping
def compute_meta(token_lora_tensor: torch.Tensor) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor, int, int, int, bool]:
    lora_indices_tensor, seq_length_tensor = torch.unique_consecutive(token_lora_tensor, return_counts=True)
    cum_result = torch.cumsum(seq_length_tensor, dim=0)
    b_seq_start_tensor = torch.zeros_like(seq_length_tensor)
    b_seq_start_tensor[1:].copy_(cum_result[:-1])
    max_length = seq_length_tensor.max().item()
    token_nums = seq_length_tensor.sum().item()
    batch_size = lora_indices_tensor.size(0)
    no_lora = False
    if batch_size == 1 and lora_indices_tensor == -1:
        no_lora = True
    return (b_seq_start_tensor, seq_length_tensor, lora_indices_tensor, batch_size, max_length, token_nums, no_lora)
def convert_mapping(mapping: 'LoRAMapping', lora_index_to_id: list[Optional[int]], max_loras: int, vocab_size: int, extra_vocab_size: int, device: torch.device) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor, list[int]]:
    index_mapping_indices: list[int] = list(mapping.index_mapping).copy()
    embedding_indices = index_mapping_indices.copy()
    lora_indices = index_mapping_indices.copy()
    prompt_mapping: list[int] = [lora_index_to_id.index(x) if x > 0 else -1 for x in mapping.prompt_mapping]
    lora_idx = None
    for i in range(len(index_mapping_indices)):
        lora_idx = lora_index_to_id.index(index_mapping_indices[i]) if index_mapping_indices[i] > 0 else -1
        embedding_indices[i] = lora_idx if index_mapping_indices[i] > 0 else 0
        lora_indices[i] = lora_idx
    indices_list: list[Union[list[int], torch.Tensor]] = [index_mapping_indices, lora_indices, embedding_indices]
    indices = torch.tensor(indices_list, dtype=torch.long, device=device)
    prompt_mapping_tensor = torch.tensor(prompt_mapping, dtype=torch.long, device=device)
    embeddings_indices = torch.stack([indices[2] * extra_vocab_size, indices[2] * (vocab_size + extra_vocab_size)])
    embeddings_indices = torch.where(embeddings_indices == -1, max_loras - 1, embeddings_indices)
    base_indices = indices[1]
    sampler_indices = prompt_mapping_tensor
    sampler_indices_padded = sampler_indices.clone()
    sampler_indices_padded = torch.where(sampler_indices_padded == -1, max_loras - 1, sampler_indices_padded)
    sampler_indices_padded = torch.arange(0, len(sampler_indices_padded), device=device, dtype=torch.long) + sampler_indices_padded * len(sampler_indices_padded)
    indices_len = [base_indices.shape[-1], sampler_indices.shape[-1], sampler_indices_padded.shape[-1], embeddings_indices.shape[-1]]
    return (base_indices, sampler_indices, sampler_indices_padded, embeddings_indices, indices_len)