import os
import multiprocessing
try:
    import torch
except ModuleNotFoundError:
    pass
try:
    import rwkv_cpp_shared_library
except ModuleNotFoundError:
    from . import rwkv_cpp_shared_library
from typing import TypeVar, Optional, Tuple, List
NumpyArrayOrPyTorchTensor: TypeVar = TypeVar('NumpyArrayOrPyTorchTensor')
class RWKVModel:
    def __init__(self, shared_library: rwkv_cpp_shared_library.RWKVSharedLibrary, model_path: str, thread_count: int=max(1, multiprocessing.cpu_count() // 2), gpu_layer_count: int=0, **kwargs) -> None:
        if 'gpu_layers_count' in kwargs:
            gpu_layer_count = kwargs['gpu_layers_count']
        if not os.path.isfile(model_path):
            raise ValueError(f'{model_path} is not a file')
        if not thread_count > 0:
            raise ValueError('Thread count must be > 0')
        if not gpu_layer_count >= 0:
            raise ValueError('GPU layer count must be >= 0')
        self._library: rwkv_cpp_shared_library.RWKVSharedLibrary = shared_library
        self._ctx: rwkv_cpp_shared_library.RWKVContext = self._library.rwkv_init_from_file(model_path, thread_count, gpu_layer_count)
        self._state_buffer_element_count: int = self._library.rwkv_get_state_buffer_element_count(self._ctx)
        self._logits_buffer_element_count: int = self._library.rwkv_get_logits_buffer_element_count(self._ctx)
        self._valid: bool = True
    @property
    def n_vocab(self) -> int:
        return self._library.rwkv_get_n_vocab(self._ctx)
    @property
    def n_embed(self) -> int:
        return self._library.rwkv_get_n_embed(self._ctx)
    @property
    def n_layer(self) -> int:
        return self._library.rwkv_get_n_layer(self._ctx)
    def eval(self, token: int, state_in: Optional[NumpyArrayOrPyTorchTensor], state_out: Optional[NumpyArrayOrPyTorchTensor]=None, logits_out: Optional[NumpyArrayOrPyTorchTensor]=None, use_numpy: bool=False) -> Tuple[NumpyArrayOrPyTorchTensor, NumpyArrayOrPyTorchTensor]:
        if not self._valid:
            raise ValueError('Model was freed')
        use_numpy = self._detect_numpy_usage([state_in, state_out, logits_out], use_numpy)
        if state_in is not None:
            self._validate_tensor(state_in, 'state_in', self._state_buffer_element_count)
            state_in_ptr = self._get_data_ptr(state_in)
        else:
            state_in_ptr = 0
        if state_out is not None:
            self._validate_tensor(state_out, 'state_out', self._state_buffer_element_count)
        else:
            state_out = self._zeros_float32(self._state_buffer_element_count, use_numpy)
        if logits_out is not None:
            self._validate_tensor(logits_out, 'logits_out', self._logits_buffer_element_count)
        else:
            logits_out = self._zeros_float32(self._logits_buffer_element_count, use_numpy)
        self._library.rwkv_eval(self._ctx, token, state_in_ptr, self._get_data_ptr(state_out), self._get_data_ptr(logits_out))
        return (logits_out, state_out)
    def eval_sequence(self, tokens: List[int], state_in: Optional[NumpyArrayOrPyTorchTensor], state_out: Optional[NumpyArrayOrPyTorchTensor]=None, logits_out: Optional[NumpyArrayOrPyTorchTensor]=None, use_numpy: bool=False) -> Tuple[NumpyArrayOrPyTorchTensor, NumpyArrayOrPyTorchTensor]:
        if not self._valid:
            raise ValueError('Model was freed')
        use_numpy = self._detect_numpy_usage([state_in, state_out, logits_out], use_numpy)
        if state_in is not None:
            self._validate_tensor(state_in, 'state_in', self._state_buffer_element_count)
            state_in_ptr = self._get_data_ptr(state_in)
        else:
            state_in_ptr = 0
        if state_out is not None:
            self._validate_tensor(state_out, 'state_out', self._state_buffer_element_count)
        else:
            state_out = self._zeros_float32(self._state_buffer_element_count, use_numpy)
        if logits_out is not None:
            self._validate_tensor(logits_out, 'logits_out', self._logits_buffer_element_count)
        else:
            logits_out = self._zeros_float32(self._logits_buffer_element_count, use_numpy)
        self._library.rwkv_eval_sequence(self._ctx, tokens, state_in_ptr, self._get_data_ptr(state_out), self._get_data_ptr(logits_out))
        return (logits_out, state_out)
    def eval_sequence_in_chunks(self, tokens: List[int], state_in: Optional[NumpyArrayOrPyTorchTensor], state_out: Optional[NumpyArrayOrPyTorchTensor]=None, logits_out: Optional[NumpyArrayOrPyTorchTensor]=None, chunk_size: int=16, use_numpy: bool=False) -> Tuple[NumpyArrayOrPyTorchTensor, NumpyArrayOrPyTorchTensor]:
        if not self._valid:
            raise ValueError('Model was freed')
        use_numpy = self._detect_numpy_usage([state_in, state_out, logits_out], use_numpy)
        if state_in is not None:
            self._validate_tensor(state_in, 'state_in', self._state_buffer_element_count)
            state_in_ptr = self._get_data_ptr(state_in)
        else:
            state_in_ptr = 0
        if state_out is not None:
            self._validate_tensor(state_out, 'state_out', self._state_buffer_element_count)
        else:
            state_out = self._zeros_float32(self._state_buffer_element_count, use_numpy)
        if logits_out is not None:
            self._validate_tensor(logits_out, 'logits_out', self._logits_buffer_element_count)
        else:
            logits_out = self._zeros_float32(self._logits_buffer_element_count, use_numpy)
        self._library.rwkv_eval_sequence_in_chunks(self._ctx, tokens, chunk_size, state_in_ptr, self._get_data_ptr(state_out), self._get_data_ptr(logits_out))
        return (logits_out, state_out)
    def free(self) -> None:
        if not self._valid:
            raise ValueError('Already freed')
        self._valid = False
        self._library.rwkv_free(self._ctx)
    def __del__(self) -> None:
        if hasattr(self, '_valid') and self._valid:
            self.free()
    def _is_pytorch_tensor(self, tensor: NumpyArrayOrPyTorchTensor) -> bool:
        return hasattr(tensor, '__module__') and tensor.__module__ == 'torch'
    def _detect_numpy_usage(self, tensors: List[Optional[NumpyArrayOrPyTorchTensor]], use_numpy_by_default: bool) -> bool:
        for tensor in tensors:
            if tensor is not None:
                return False if self._is_pytorch_tensor(tensor) else True
        return use_numpy_by_default
    def _validate_tensor(self, tensor: NumpyArrayOrPyTorchTensor, name: str, size: int) -> None:
        if self._is_pytorch_tensor(tensor):
            tensor: torch.Tensor = tensor
            if tensor.device != torch.device('cpu'):
                raise ValueError(f'{name} is not on CPU')
            if tensor.dtype != torch.float32:
                raise ValueError(f'{name} is not of type float32')
            if tensor.shape != (size,):
                raise ValueError(f'{name} has invalid shape {tensor.shape}, expected ({size})')
            if not tensor.is_contiguous():
                raise ValueError(f'{name} is not contiguous')
        else:
            import numpy as np
            tensor: np.ndarray = tensor
            if tensor.dtype != np.float32:
                raise ValueError(f'{name} is not of type float32')
            if tensor.shape != (size,):
                raise ValueError(f'{name} has invalid shape {tensor.shape}, expected ({size})')
            if not tensor.data.contiguous:
                raise ValueError(f'{name} is not contiguous')
    def _get_data_ptr(self, tensor: NumpyArrayOrPyTorchTensor):
        if self._is_pytorch_tensor(tensor):
            return tensor.data_ptr()
        else:
            return tensor.ctypes.data
    def _zeros_float32(self, element_count: int, use_numpy: bool) -> NumpyArrayOrPyTorchTensor:
        if use_numpy:
            import numpy as np
            return np.zeros(element_count, dtype=np.float32)
        else:
            return torch.zeros(element_count, dtype=torch.float32, device='cpu')