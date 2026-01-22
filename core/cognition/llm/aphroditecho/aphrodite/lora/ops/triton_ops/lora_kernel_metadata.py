from dataclasses import dataclass
from typing import Union
import torch
@dataclass
class LoRAKernelMeta:
    token_lora_mapping: torch.Tensor
    token_indices_sorted_by_lora_ids: torch.Tensor
    active_lora_ids: torch.Tensor
    num_tokens_per_lora: torch.Tensor
    lora_token_start_loc: torch.Tensor
    no_lora_flag_cpu: torch.Tensor
    @staticmethod
    def make(max_loras: int, max_num_tokens: int, device: Union[torch.device, str]) -> 'LoRAKernelMeta':
        token_lora_mapping = torch.empty(max_num_tokens, dtype=torch.int32, device=device)
        token_indices_sorted_by_lora_ids = torch.empty(max_num_tokens, dtype=torch.int32, device=device)
        active_lora_ids = torch.empty(max_loras + 1, dtype=torch.int32, device=device)
        num_tokens_per_lora = torch.zeros(max_loras + 1, dtype=torch.int32, device=device)
        lora_token_start_loc = torch.zeros(max_loras + 2, dtype=torch.int32, device=device)
        no_lora_flag_cpu = torch.tensor([False], dtype=torch.bool, device='cpu')
        return LoRAKernelMeta(token_lora_mapping=token_lora_mapping, token_indices_sorted_by_lora_ids=token_indices_sorted_by_lora_ids, active_lora_ids=active_lora_ids, num_tokens_per_lora=num_tokens_per_lora, lora_token_start_loc=lora_token_start_loc, no_lora_flag_cpu=no_lora_flag_cpu)
    def _reset(self):
        self.active_lora_ids.fill_(-1)
        self.num_tokens_per_lora.fill_(0)
        self.lora_token_start_loc.fill_(0)
        self.no_lora_flag_cpu.fill_(False)
    def prepare_tensors(self, token_lora_mapping: torch.Tensor) -> None:
        self._reset()
        no_lora = torch.all(token_lora_mapping == -1)
        self.no_lora_flag_cpu[0] = no_lora
        if no_lora:
            return
        num_tokens = token_lora_mapping.size(0)
        self.token_lora_mapping[:num_tokens].copy_(token_lora_mapping, non_blocking=True)
        _, token_indices_sorted_by_lora_ids = torch.sort(token_lora_mapping, stable=True)
        self.token_indices_sorted_by_lora_ids[:num_tokens].copy_(token_indices_sorted_by_lora_ids, non_blocking=True)
        lora_ids, num_tokens_per_lora = torch.unique(token_lora_mapping, sorted=True, return_counts=True)
        self.active_lora_ids[:lora_ids.size(0)].copy_(lora_ids, non_blocking=True)
        self.num_tokens_per_lora[:num_tokens_per_lora.size(0)].copy_(num_tokens_per_lora, non_blocking=True)
        lora_token_start_loc = torch.cumsum(num_tokens_per_lora, dim=0)
        self.lora_token_start_loc[1:1 + lora_token_start_loc.size(0)].copy_(lora_token_start_loc, non_blocking=True)
    def meta_args(self, token_nums: int) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
        return (self.token_lora_mapping[:token_nums], self.token_indices_sorted_by_lora_ids[:token_nums], self.num_tokens_per_lora, self.lora_token_start_loc, self.active_lora_ids, self.no_lora_flag_cpu)