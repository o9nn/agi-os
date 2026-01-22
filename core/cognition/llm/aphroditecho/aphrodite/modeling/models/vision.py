from abc import ABC, abstractmethod
from typing import Final, Generic, Optional, Protocol, TypeVar, Union
import torch
from transformers import PretrainedConfig
from aphrodite.attention.selector import get_env_variable_attn_backend
from aphrodite.platforms import _Backend, current_platform
_C = TypeVar('_C', bound=PretrainedConfig)
class VisionEncoderInfo(ABC, Generic[_C]):
    def __init__(self, hf_config: _C) -> None:
        super().__init__()
        self.hf_config = hf_config
        self.vision_config = hf_config.vision_config
    @abstractmethod
    def get_num_image_tokens(self, *, image_width: int, image_height: int) -> int:
        raise NotImplementedError
    @abstractmethod
    def get_image_size(self) -> int:
        raise NotImplementedError
    @abstractmethod
    def get_patch_size(self) -> int:
        raise NotImplementedError
    @abstractmethod
    def get_patch_grid_length(self) -> int:
        raise NotImplementedError
class VisionLanguageConfig(Protocol):
    vision_config: Final[PretrainedConfig]
def get_vision_encoder_info(hf_config: VisionLanguageConfig) -> VisionEncoderInfo:
    from .clip import CLIPEncoderInfo, CLIPVisionConfig
    from .pixtral import PixtralHFEncoderInfo, PixtralVisionConfig
    from .siglip import SiglipEncoderInfo, SiglipVisionConfig
    if isinstance(hf_config.vision_config, CLIPVisionConfig):
        return CLIPEncoderInfo(hf_config)
    if isinstance(hf_config.vision_config, PixtralVisionConfig):
        return PixtralHFEncoderInfo(hf_config)
    if isinstance(hf_config.vision_config, SiglipVisionConfig):
        return SiglipEncoderInfo(hf_config)
    msg = f'Unsupported vision config: {type(hf_config.vision_config)}'
    raise NotImplementedError(msg)
def get_vit_attn_backend(support_fa: bool=False) -> _Backend:
    selected_backend: Optional[_Backend] = get_env_variable_attn_backend()
    if selected_backend is not None:
        return selected_backend
    return current_platform.get_vit_attn_backend(support_fa)
def resolve_visual_encoder_outputs(encoder_outputs: Union[torch.Tensor, list[torch.Tensor]], feature_sample_layers: Optional[list[int]], post_layer_norm: Optional[torch.nn.LayerNorm], max_possible_layers: int) -> torch.Tensor:
    if feature_sample_layers is None:
        if post_layer_norm is not None:
            return post_layer_norm(encoder_outputs)
        return encoder_outputs
    num_loaded_layers = len(encoder_outputs) - 1
    offset = max_possible_layers - num_loaded_layers
    hs_pool = [encoder_outputs[layer_idx] if layer_idx >= 0 else encoder_outputs[layer_idx + offset] for layer_idx in feature_sample_layers]
    uses_last_layer = feature_sample_layers[-1] in (len(hs_pool) - 1, -1)
    if post_layer_norm is not None and uses_last_layer:
        hs_pool[-1] = post_layer_norm(encoder_outputs)
    return torch.cat(hs_pool, dim=-1)