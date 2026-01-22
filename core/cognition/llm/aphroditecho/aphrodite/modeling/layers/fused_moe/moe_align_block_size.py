from typing import Optional
import torch
from aphrodite import _custom_ops as ops
from aphrodite.triton_utils import triton
from aphrodite.utils import round_up
def moe_align_block_size(topk_ids: torch.Tensor, block_size: int, num_experts: int, expert_map: Optional[torch.Tensor]=None, pad_sorted_ids: bool=False) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
    max_num_tokens_padded = topk_ids.numel() + num_experts * (block_size - 1)
    if pad_sorted_ids:
        max_num_tokens_padded = round_up(max_num_tokens_padded, block_size)
    sorted_ids = torch.empty((max_num_tokens_padded,), dtype=torch.int32, device=topk_ids.device)
    max_num_m_blocks = triton.cdiv(max_num_tokens_padded, block_size)
    expert_ids = torch.empty((max_num_m_blocks,), dtype=torch.int32, device=topk_ids.device)
    num_tokens_post_pad = torch.empty(1, dtype=torch.int32, device=topk_ids.device)
    ops.moe_align_block_size(topk_ids, num_experts, block_size, sorted_ids, expert_ids, num_tokens_post_pad)
    if expert_map is not None:
        expert_ids = expert_map[expert_ids]
    return (sorted_ids, expert_ids, num_tokens_post_pad)