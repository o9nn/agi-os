from typing import Optional, Union, final
import torch
import aphrodite.common.envs as envs
from aphrodite.lora.layers import LoRAMapping
from aphrodite.triton_utils import HAS_TRITON
if HAS_TRITON:
    from aphrodite.lora.ops.triton_ops import LoRAKernelMeta, lora_expand, lora_shrink
from .punica_base import PunicaWrapperBase
@final
class PunicaWrapperGPU(PunicaWrapperBase):
    def __init__(self, max_num_batched_tokens: int, max_batches: int, device: Union[torch.device, str], **kwargs):
        PunicaWrapperBase.__init__(self, max_num_batched_tokens, max_batches, device)
        self.max_loras = kwargs['max_loras']
        self.token_mapping_meta = LoRAKernelMeta.make(self.max_loras, max_num_batched_tokens, device=device)
        max_num_prompts = max_batches if envs.APHRODITE_USE_V1 else max_num_batched_tokens
        self.prompt_mapping_meta = LoRAKernelMeta.make(self.max_loras, max_num_prompts, device=device)
    def update_metadata(self, mapping: LoRAMapping, lora_index_to_id: list[Optional[int]], max_loras: int, vocab_size: int, extra_vocab_size: int, **kwargs):
        self.is_prefill = mapping.is_prefill
        self._update_base_metadata(mapping, lora_index_to_id, max_loras, vocab_size, extra_vocab_size)
        self.token_mapping_meta.prepare_tensors(self.token_lora_indices)
        self.prompt_mapping_meta.prepare_tensors(self.sampler_indices)
    def add_shrink(self, y: torch.Tensor, x: torch.Tensor, lora_a_stacked: tuple[torch.Tensor, ...], scale: float, **kwargs):
        x = x.view(-1, x.shape[-1])
        lora_shrink(x, lora_a_stacked, y, *self.token_mapping_meta.meta_args(x.size(0)), scale)
    def add_expand(self, y: torch.Tensor, x: torch.Tensor, lora_b_stacked: tuple[torch.Tensor, ...], lora_bias_stacked: Optional[tuple[torch.Tensor, ...]], output_slices: tuple[int, ...], offset_start: int=0, add_inputs=True, **kwargs) -> None:
        y_org = y
        y = y.view(-1, y.shape[-1])
        if lora_bias_stacked is not None:
            token_lora_indices = torch.narrow(self._token_lora_indices, 0, 0, y.size(0))
            self._apply_bias(token_lora_indices, y, output_slices, lora_bias_stacked)
        assert x.ndim == 3
        assert x.size(0) == len(output_slices)
        num_tokens = x.size(1)
        lora_expand(x, lora_b_stacked, y, *self.token_mapping_meta.meta_args(num_tokens), offset_start=offset_start, add_inputs=True)
        y = y.view_as(y_org)
    def add_lora_embedding(self, y: torch.Tensor, x: torch.Tensor, lora_b_stacked: torch.Tensor, add_inputs: bool=True, **kwargs) -> None:
        lora_expand(x.unsqueeze(dim=0), (lora_b_stacked,), y, *self.token_mapping_meta.meta_args(x.size(0)), offset_start=0, add_inputs=add_inputs)
    def add_lora_linear(self, y: torch.Tensor, x: torch.Tensor, lora_a_stacked: tuple[torch.Tensor, ...], lora_b_stacked: tuple[torch.Tensor, ...], lora_bias_stacked: Optional[tuple[torch.Tensor, ...]], scale: float, output_slices: tuple[int, ...], *, buffer: Optional[torch.Tensor]=None, **kwargs) -> None:
        assert len(lora_a_stacked) == len(lora_b_stacked) == len(output_slices)
        if lora_bias_stacked is not None:
            assert len(lora_bias_stacked) == len(output_slices)
            token_lora_indices = torch.narrow(self._token_lora_indices, 0, 0, y.size(0))
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
        lora_shrink(x, [lora_a_stacked], buffer.unsqueeze(dim=0), *self.prompt_mapping_meta.meta_args(x.size(0)), scale)
        lora_expand(buffer.unsqueeze(dim=0), [lora_b_stacked], y, *self.prompt_mapping_meta.meta_args(buffer.size(0)), add_inputs=True)
        y = y.view_as(y_org)