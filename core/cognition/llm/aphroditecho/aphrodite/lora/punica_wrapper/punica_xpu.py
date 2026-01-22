from typing import Optional, Union, final
import torch
from aphrodite.lora.layers import LoRAMapping
from aphrodite.lora.ops.ipex_ops import bgmv_expand, bgmv_expand_slice, bgmv_shrink
from .punica_base import PunicaWrapperBase
@final
class PunicaWrapperXPU(PunicaWrapperBase):
    def __init__(self, max_num_batched_tokens: int, max_batches: int, device: Union[torch.device, str], **kwargs):
        PunicaWrapperBase.__init__(self, max_num_batched_tokens, max_batches, device)
        torch._dynamo.mark_dynamic(self._token_lora_indices, 0)
        torch._dynamo.mark_dynamic(self._embeddings_indices, 1)
        torch._dynamo.mark_dynamic(self._sampler_indices_padded, 0)
    def update_metadata(self, mapping: LoRAMapping, lora_index_to_id: list[Optional[int]], max_loras: int, vocab_size: int, extra_vocab_size: int, **kwargs):
        self.is_prefill = mapping.is_prefill
        self._update_base_metadata(mapping, lora_index_to_id, max_loras, vocab_size, extra_vocab_size)
    def _get_token_lora_indices(self, x: torch.Tensor) -> torch.IntTensor:
        return torch.narrow(self._token_lora_indices, 0, 0, x.size(0))
    def _apply_shrink(self, y: torch.Tensor, x: torch.Tensor, w_t_all: torch.Tensor, scale: float):
        bgmv_shrink(x, w_t_all, y, self._get_token_lora_indices(x), scale)
    def _apply_expand(self, y: torch.Tensor, x: torch.Tensor, w_t_all: torch.Tensor, y_offset: int, y_slice_size: int, add_inputs: bool):
        token_lora_indices = self._get_token_lora_indices(x)
        bgmv_expand_slice(x, w_t_all, y, token_lora_indices, y_offset, y_slice_size, add_inputs)
    def add_shrink(self, y: torch.Tensor, x: torch.Tensor, lora_a_stacked: tuple[torch.Tensor, ...], scale: float, **kwargs):
        x = x.view(-1, x.shape[-1])
        for slice_idx in range(len(lora_a_stacked)):
            self._apply_shrink(y[slice_idx], x, lora_a_stacked[slice_idx], scale)
    def add_expand(self, y: torch.Tensor, x: torch.Tensor, lora_b_stacked: tuple[torch.Tensor, ...], lora_bias_stacked: Optional[tuple[torch.Tensor, ...]], output_slices: tuple[int, ...], offset_start: int=0, add_inputs=True, **kwargs) -> None:
        y_org = y
        y = y.view(-1, y.shape[-1])
        if lora_bias_stacked is not None:
            token_lora_indices = self._get_token_lora_indices(y)
            self._apply_bias(token_lora_indices, y, output_slices, lora_bias_stacked)
        assert x.ndim == 3
        assert x.size(0) == len(output_slices)
        for slice_idx in range(len(lora_b_stacked)):
            self._apply_expand(y, x[slice_idx], lora_b_stacked[slice_idx], offset_start, output_slices[slice_idx], add_inputs=add_inputs)
            offset_start += output_slices[slice_idx]
        y.view_as(y_org)
    def add_lora_embedding(self, y: torch.Tensor, x: torch.Tensor, lora_b_stacked: torch.Tensor, add_inputs: bool=True, **kwargs) -> None:
        token_lora_indices = self._get_token_lora_indices(x)
        bgmv_expand(x, lora_b_stacked, y, token_lora_indices, add_inputs)
    def add_lora_linear(self, y: torch.Tensor, x: torch.Tensor, lora_a_stacked: tuple[torch.Tensor, ...], lora_b_stacked: tuple[torch.Tensor, ...], lora_bias_stacked: Optional[tuple[torch.Tensor, ...]], scale: float, output_slices: tuple[int, ...], *, buffer: Optional[torch.Tensor]=None, **kwargs) -> None:
        assert len(lora_a_stacked) == len(lora_b_stacked) == len(output_slices)
        if lora_bias_stacked is not None:
            assert len(lora_bias_stacked) == len(output_slices)
            token_lora_indices = self._get_token_lora_indices(y)
            y = self._apply_bias(token_lora_indices, y, output_slices, lora_bias_stacked)
        if buffer is None:
            r = lora_b_stacked[0].size(-1)
            buffer = torch.zeros((len(output_slices), x.size(0), r), dtype=torch.float32, device=x.device)
        self.add_shrink(buffer, x, lora_a_stacked, scale, **kwargs)
        self.add_expand(y, buffer, lora_b_stacked, None, output_slices, add_inputs=True, **kwargs)
    def add_lora_logits(self, y: torch.Tensor, x: torch.Tensor, lora_a_stacked: torch.Tensor, lora_b_stacked: torch.Tensor, scale, *, buffer: Optional[torch.Tensor]=None, **kwargs) -> None:
        y_org = y
        y = y.view(-1, y.shape[-1])
        x = x.view(-1, x.shape[-1])
        r = lora_b_stacked.size(-1)
        if buffer is None:
            buffer = torch.zeros((x.size(0), r), dtype=torch.float32, device=x.device)
        bgmv_shrink(x, lora_a_stacked, buffer, self.sampler_indices, scale)
        bgmv_expand(buffer, lora_b_stacked, y, self.sampler_indices, add_inputs=True)
        return y.view_as(y_org)