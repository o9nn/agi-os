from typing import Any, Optional
import transformers
class UltravoxConfig(transformers.PretrainedConfig):
    model_type = 'ultravox'
    audio_token = '<|audio|>'
    is_composition = False
    def __init__(self, audio_config: Optional[dict[str, Any]]=None, text_config: Optional[dict[str, Any]]=None, audio_model_id: Optional[str]=None, text_model_id: Optional[str]=None, ignore_index: int=-100, audio_token_index: int=32000, hidden_size: int=4096, stack_factor: int=8, norm_init: float=0.4, projector_act: str='swiglu', text_model_lora_config: Optional[dict[str, Any]]=None, audio_model_lora_config: Optional[dict[str, Any]]=None, projector_ln_mid: bool=False, **kwargs):
        self.ignore_index = ignore_index
        self.audio_model_id = audio_model_id
        self.text_model_id = text_model_id
        self.audio_token_index = audio_token_index
        self.hidden_size = hidden_size
        self.stack_factor = stack_factor
        self.norm_init = norm_init
        self.projector_act = projector_act
        self.projector_ln_mid = projector_ln_mid
        if text_model_id is not None:
            from aphrodite.transformers_utils.config import get_config
            text_config_obj = get_config(text_model_id, trust_remote_code=False)
        else:
            text_config = text_config or {}
            text_config_obj = transformers.CONFIG_MAPPING[text_config.get('model_type', 'llama')](**text_config)
        inner_text_config = text_config_obj.get_text_config()
        if audio_model_id is not None:
            from aphrodite.transformers_utils.config import get_config
            audio_config = get_config(audio_model_id, trust_remote_code=False)
        else:
            audio_config = audio_config or {}
            audio_config = transformers.CONFIG_MAPPING[audio_config.get('model_type', 'whisper')](**audio_config)
        self.text_config = text_config_obj
        self.audio_config = audio_config
        self.text_model_lora_config = text_model_lora_config or {}
        self.audio_model_lora_config = audio_model_lora_config or {}
        self.vocab_size = inner_text_config.vocab_size
        self.initializer_range = inner_text_config.initializer_range
        self.text_hidden_size = inner_text_config.hidden_size
        super().__init__(**kwargs)