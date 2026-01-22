from abc import ABC, abstractmethod
from typing import TYPE_CHECKING, Optional, Union
import torch
from .utils import compute_meta, convert_mapping
if TYPE_CHECKING:
    from aphrodite.lora.layers import LoRAMapping
class PunicaWrapperABC(ABC):
    @abstractmethod
    def update_metadata(self, mapping: 'LoRAMapping', lora_index_to_id: list[Optional[int]], max_loras: int, vocab_size: int, extra_vocab_size: int, **kwargs) -> None:
        raise NotImplementedError
    @abstractmethod
    def add_shrink(self, y: Union[tuple[torch.Tensor, ...], torch.Tensor], x: torch.Tensor, lora_a_stacked: tuple[torch.Tensor, ...], scale: float, **kwargs) -> Optional[torch.Tensor]:
        raise NotImplementedError
    @abstractmethod
    def add_expand(self, y: torch.Tensor, x: Union[tuple[torch.Tensor, ...], torch.Tensor], lora_b_stacked: tuple[torch.Tensor, ...], lora_bias_stacked: Optional[tuple[torch.Tensor, ...]], output_slices: tuple[int, ...], offset_start: int=0, add_inputs=True, **kwargs) -> Optional[torch.Tensor]:
        raise NotImplementedError
    @abstractmethod
    def add_lora_embedding(self, y: torch.Tensor, x: torch.Tensor, lora_b_stacked: torch.Tensor, add_inputs: bool=True, **kwargs) -> Optional[torch.Tensor]:
        raise NotImplementedError
    @abstractmethod
    def add_lora_linear(self, y: torch.Tensor, x: torch.Tensor, lora_a_stacked: tuple[torch.Tensor, ...], lora_b_stacked: tuple[torch.Tensor, ...], lora_bias_stacked: Optional[tuple[torch.Tensor, ...]], scale: float, output_slices: tuple[int, ...], *, buffer: Optional[tuple[torch.Tensor, ...]]=None, **kwargs) -> Optional[torch.Tensor]:
        raise NotImplementedError
    @abstractmethod
    def add_lora_logits(self, y: torch.Tensor, x: torch.Tensor, lora_a_stacked: torch.Tensor, lora_b_stacked: torch.Tensor, scale, *, buffer: Optional[torch.Tensor]=None, **kwargs) -> Optional[torch.Tensor]:
        raise NotImplementedError
class PunicaWrapperBase(PunicaWrapperABC):
    def __init__(self, max_num_batched_tokens: int, max_batches: int, device: Union[torch.device, str], **kwargs):
        self._token_lora_indices = torch.empty(max_num_batched_tokens, dtype=torch.long, device=device)
        self._sampler_indices = torch.empty(max_num_batched_tokens, dtype=torch.long, device=device)
        self._sampler_indices_padded = torch.empty(max_num_batched_tokens, dtype=torch.long, device=device)
        self._embeddings_indices = torch.empty(2, max_num_batched_tokens, dtype=torch.long, device=device)
        self.indices_len: list[Optional[int]] = [None] * 4
        self._seq_start_locs = torch.empty(max_batches, dtype=torch.long, device=device)
        self._seq_lengths = torch.empty(max_batches, dtype=torch.long, device=device)
        self._lora_indices_per_batch = torch.empty(max_batches, dtype=torch.long, device=device)
        self.device: torch.device = device
        self.max_length: int = 0
        self.token_nums: int = 0
        self.batch_size: int = -1
        self.is_prefill = False
        self.no_lora = False
    def _update_base_metadata(self, mapping: 'LoRAMapping', lora_index_to_id: list[Optional[int]], max_loras: int, vocab_size: int, extra_vocab_size: int):
        base_indices, sampler_indices, sampler_indices_padded, embeddings_indices, indices_len = convert_mapping(mapping, lora_index_to_id, max_loras, vocab_size, extra_vocab_size, self.device)
        self._token_lora_indices[:base_indices.shape[0]].copy_(base_indices)
        self._sampler_indices[:sampler_indices.shape[0]].copy_(sampler_indices)
        self._sampler_indices_padded[:sampler_indices_padded.shape[0]].copy_(sampler_indices_padded)
        self._embeddings_indices[:embeddings_indices.shape[0], :embeddings_indices.shape[1]].copy_(embeddings_indices)
        self.indices_len[:] = indices_len
    def _update_prefill_metadata(self, token_lora_tensor: torch.Tensor) -> None:
        b_seq_start_tensor, seq_length_tensor, lora_indices_tensor, batch_size, max_length, token_nums, no_lora = compute_meta(token_lora_tensor)
        self._seq_start_locs[:b_seq_start_tensor.shape[0]].copy_(b_seq_start_tensor)
        self._seq_lengths[:seq_length_tensor.shape[0]].copy_(seq_length_tensor)
        self._lora_indices_per_batch[:lora_indices_tensor.shape[0]].copy_(lora_indices_tensor)
        self.batch_size = batch_size
        self.max_length = max_length
        self.token_nums = token_nums
        self.no_lora = no_lora
    def _apply_bias(self, indices: torch.Tensor, output: torch.Tensor, output_slices: tuple[int, ...], lora_bias_stacked: tuple[Optional[torch.Tensor], ...]):
        org_output = output
        output = output.view(-1, output.shape[-1])
        indices = indices.view(-1)
        offset_left = 0
        for slice_idx, slice in enumerate(output_slices):
            bias = lora_bias_stacked[slice_idx]
            if bias is not None:
                bias = bias.view(-1, bias.shape[-1])
                bias = bias[indices]
                bias[indices == -1] = 0
                output[:, offset_left:offset_left + slice] += bias
            offset_left += slice
        return output.view_as(org_output)
    @property
    def prefill_metadata(self) -> tuple[torch.Tensor, torch.Tensor, torch.Tensor, int, int, int]:
        return (self._seq_start_locs[:self.batch_size], self._seq_lengths[:self.batch_size], self._lora_indices_per_batch[:self.batch_size], self.batch_size, self.max_length, self.token_nums)
    @property
    def token_lora_indices(self) -> torch.Tensor:
        token_lora_len = self.indices_len[0]
        return self._token_lora_indices[:token_lora_len]
    @property
    def sampler_indices(self) -> torch.Tensor:
        sampler_indices_len = self.indices_len[1]
        return self._sampler_indices[:sampler_indices_len]
    @property
    def sampler_indices_padded(self) -> torch.Tensor:
        indices_padded_len = self.indices_len[2]
        return self._sampler_indices_padded[:indices_padded_len]
    @property
    def embeddings_indices(self) -> torch.Tensor:
        embeddings_indices_len = self.indices_len[3]
        return self._embeddings_indices[:, :embeddings_indices_len]
    def update_metadata(self, mapping: 'LoRAMapping', lora_index_to_id: list[Optional[int]], max_loras: int, vocab_size: int, extra_vocab_size: int, **kwargs):
        self._update_base_metadata(mapping, lora_index_to_id, max_loras, vocab_size, extra_vocab_size)
        if mapping.is_prefill:
            self._update_prefill_metadata(self.token_lora_indices)
            self.is_prefill = True
        else:
            self.is_prefill = False
    @abstractmethod
    def add_shrink(self, y: Union[tuple[torch.Tensor, ...], torch.Tensor], x: torch.Tensor, lora_a_stacked: tuple[torch.Tensor, ...], scale: float, **kwargs) -> Optional[torch.Tensor]:
        raise NotImplementedError
    @abstractmethod
    def add_expand(self, y: torch.Tensor, x: Union[tuple[torch.Tensor, ...], torch.Tensor], lora_b_stacked: tuple[torch.Tensor, ...], lora_bias_stacked: Optional[tuple[torch.Tensor, ...]], output_slices: tuple[int, ...], offset_start: int=0, add_inputs=True, **kwargs) -> Optional[torch.Tensor]:
        raise NotImplementedError
    @abstractmethod
    def add_lora_embedding(self, y: torch.Tensor, x: torch.Tensor, lora_b_stacked: torch.Tensor, add_inputs: bool=True, **kwargs) -> Optional[torch.Tensor]:
        raise NotImplementedError
    @abstractmethod
    def add_lora_linear(self, y: torch.Tensor, x: torch.Tensor, lora_a_stacked: tuple[torch.Tensor, ...], lora_b_stacked: tuple[torch.Tensor, ...], lora_bias_stacked: Optional[tuple[torch.Tensor, ...]], scale: float, output_slices: tuple[int, ...], *, buffer: Optional[tuple[torch.Tensor, ...]]=None, **kwargs) -> Optional[torch.Tensor]:
        raise NotImplementedError
    @abstractmethod
    def add_lora_logits(self, y: torch.Tensor, x: torch.Tensor, lora_a_stacked: torch.Tensor, lora_b_stacked: torch.Tensor, scale, *, buffer: Optional[torch.Tensor]=None, **kwargs) -> Optional[torch.Tensor]:
        raise NotImplementedError