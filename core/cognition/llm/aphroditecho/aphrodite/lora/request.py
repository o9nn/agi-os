import warnings
from typing import Optional
import msgspec
from aphrodite.adapter_commons.request import AdapterRequest
class LoRARequest(msgspec.Struct, omit_defaults=True, array_like=True):
    __metaclass__ = AdapterRequest
    lora_name: str
    lora_int_id: int
    lora_path: str = ''
    lora_local_path: Optional[str] = msgspec.field(default=None)
    long_lora_max_len: Optional[int] = None
    base_model_name: Optional[str] = msgspec.field(default=None)
    tensorizer_config_dict: Optional[dict] = None
    def __post_init__(self):
        if self.lora_local_path:
            warnings.warn("The 'lora_local_path' attribute is deprecated and will be removed in a future version. Please use 'lora_path' instead.", DeprecationWarning, stacklevel=2)
            if not self.lora_path:
                self.lora_path = self.lora_local_path or ''
        assert self.lora_path, 'lora_path cannot be empty'
    @property
    def adapter_id(self):
        return self.lora_int_id
    @property
    def name(self):
        return self.lora_name
    @property
    def path(self):
        return self.lora_path
    @property
    def local_path(self):
        warnings.warn("The 'local_path' attribute is deprecated and will be removed in a future version. Please use 'path' instead.", DeprecationWarning, stacklevel=2)
        return self.lora_path
    @local_path.setter
    def local_path(self, value):
        warnings.warn("The 'local_path' attribute is deprecated and will be removed in a future version. Please use 'path' instead.", DeprecationWarning, stacklevel=2)
        self.lora_path = value
    def __eq__(self, value: object) -> bool:
        return isinstance(value, self.__class__) and self.lora_name == value.lora_name
    def __hash__(self) -> int:
        return hash(self.lora_name)