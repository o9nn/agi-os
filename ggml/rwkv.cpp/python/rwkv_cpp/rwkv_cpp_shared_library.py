import os
import sys
import ctypes
import pathlib
import platform
from typing import Optional, List, Tuple, Callable
QUANTIZED_FORMAT_NAMES: Tuple[str, str, str, str, str] = ('Q4_0', 'Q4_1', 'Q4_K', 'Q5_0', 'Q5_1', 'Q5_K', 'Q8_0')
P_FLOAT = ctypes.POINTER(ctypes.c_float)
P_INT = ctypes.POINTER(ctypes.c_int32)
class RWKVContext:
    def __init__(self, ptr: ctypes.pointer) -> None:
        self.ptr: ctypes.pointer = ptr
class RWKVSharedLibrary:
    def __init__(self, shared_library_path: str) -> None:
        if platform.system().lower() == 'windows':
            self.library = ctypes.CDLL(shared_library_path, winmode=0)
        else:
            self.library = ctypes.cdll.LoadLibrary(shared_library_path)
        self.library.rwkv_init_from_file.argtypes = [ctypes.c_char_p, ctypes.c_uint32, ctypes.c_uint32]
        self.library.rwkv_init_from_file.restype = ctypes.c_void_p
        self.library.rwkv_eval.argtypes = [ctypes.c_void_p, ctypes.c_int32, P_FLOAT, P_FLOAT, P_FLOAT]
        self.library.rwkv_eval.restype = ctypes.c_bool
        self.library.rwkv_eval_sequence.argtypes = [ctypes.c_void_p, P_INT, ctypes.c_size_t, P_FLOAT, P_FLOAT, P_FLOAT]
        self.library.rwkv_eval_sequence.restype = ctypes.c_bool
        self.library.rwkv_eval_sequence_in_chunks.argtypes = [ctypes.c_void_p, P_INT, ctypes.c_size_t, ctypes.c_size_t, P_FLOAT, P_FLOAT, P_FLOAT]
        self.library.rwkv_eval_sequence_in_chunks.restype = ctypes.c_bool
        self.library.rwkv_get_n_vocab.argtypes = [ctypes.c_void_p]
        self.library.rwkv_get_n_vocab.restype = ctypes.c_size_t
        self.library.rwkv_get_n_embed.argtypes = [ctypes.c_void_p]
        self.library.rwkv_get_n_embed.restype = ctypes.c_size_t
        self.library.rwkv_get_n_layer.argtypes = [ctypes.c_void_p]
        self.library.rwkv_get_n_layer.restype = ctypes.c_size_t
        self.library.rwkv_get_state_buffer_element_count.argtypes = [ctypes.c_void_p]
        self.library.rwkv_get_state_buffer_element_count.restype = ctypes.c_uint32
        self.library.rwkv_get_logits_buffer_element_count.argtypes = [ctypes.c_void_p]
        self.library.rwkv_get_logits_buffer_element_count.restype = ctypes.c_uint32
        self.library.rwkv_free.argtypes = [ctypes.c_void_p]
        self.library.rwkv_free.restype = None
        self.library.rwkv_free.argtypes = [ctypes.c_void_p]
        self.library.rwkv_free.restype = None
        self.library.rwkv_quantize_model_file.argtypes = [ctypes.c_char_p, ctypes.c_char_p, ctypes.c_char_p]
        self.library.rwkv_quantize_model_file.restype = ctypes.c_bool
        self.library.rwkv_get_system_info_string.argtypes = []
        self.library.rwkv_get_system_info_string.restype = ctypes.c_char_p
        self.nullptr = ctypes.cast(0, ctypes.c_void_p)
    def rwkv_init_from_file(self, model_file_path: str, thread_count: int, offload_layers: int) -> RWKVContext:
        ptr = self.library.rwkv_init_from_file(model_file_path.encode('utf-8'), ctypes.c_uint32(thread_count), ctypes.c_uint32(offload_layers))
        if ptr is None:
            raise ValueError('rwkv_init_from_file failed, check stderr')
        return RWKVContext(ptr)
    def rwkv_eval(self, ctx: RWKVContext, token: int, state_in_address: Optional[int], state_out_address: int, logits_out_address: int) -> None:
        if not self.library.rwkv_eval(ctx.ptr, ctypes.c_int32(token), ctypes.cast(0 if state_in_address is None else state_in_address, P_FLOAT), ctypes.cast(state_out_address, P_FLOAT), ctypes.cast(logits_out_address, P_FLOAT)):
            raise ValueError('rwkv_eval failed, check stderr')
    def rwkv_eval_sequence(self, ctx: RWKVContext, tokens: List[int], state_in_address: Optional[int], state_out_address: int, logits_out_address: int) -> None:
        if not self.library.rwkv_eval_sequence(ctx.ptr, ctypes.cast((ctypes.c_int32 * len(tokens))(*tokens), P_INT), ctypes.c_size_t(len(tokens)), ctypes.cast(0 if state_in_address is None else state_in_address, P_FLOAT), ctypes.cast(state_out_address, P_FLOAT), ctypes.cast(logits_out_address, P_FLOAT)):
            raise ValueError('rwkv_eval_sequence failed, check stderr')
    def rwkv_eval_sequence_in_chunks(self, ctx: RWKVContext, tokens: List[int], chunk_size: int, state_in_address: Optional[int], state_out_address: int, logits_out_address: int) -> None:
        if not self.library.rwkv_eval_sequence_in_chunks(ctx.ptr, ctypes.cast((ctypes.c_int32 * len(tokens))(*tokens), P_INT), ctypes.c_size_t(len(tokens)), ctypes.c_size_t(chunk_size), ctypes.cast(0 if state_in_address is None else state_in_address, P_FLOAT), ctypes.cast(state_out_address, P_FLOAT), ctypes.cast(logits_out_address, P_FLOAT)):
            raise ValueError('rwkv_eval_sequence_in_chunks failed, check stderr')
    def rwkv_get_n_vocab(self, ctx: RWKVContext) -> int:
        return self.library.rwkv_get_n_vocab(ctx.ptr)
    def rwkv_get_n_embed(self, ctx: RWKVContext) -> int:
        return self.library.rwkv_get_n_embed(ctx.ptr)
    def rwkv_get_n_layer(self, ctx: RWKVContext) -> int:
        return self.library.rwkv_get_n_layer(ctx.ptr)
    def rwkv_get_state_buffer_element_count(self, ctx: RWKVContext) -> int:
        return self.library.rwkv_get_state_buffer_element_count(ctx.ptr)
    def rwkv_get_logits_buffer_element_count(self, ctx: RWKVContext) -> int:
        return self.library.rwkv_get_logits_buffer_element_count(ctx.ptr)
    def rwkv_free(self, ctx: RWKVContext) -> None:
        self.library.rwkv_free(ctx.ptr)
        ctx.ptr = self.nullptr
    def rwkv_quantize_model_file(self, model_file_path_in: str, model_file_path_out: str, format_name: str) -> None:
        if format_name not in QUANTIZED_FORMAT_NAMES:
            raise ValueError(f'Unknown format name {format_name}, use one of {QUANTIZED_FORMAT_NAMES}')
        if not self.library.rwkv_quantize_model_file(model_file_path_in.encode('utf-8'), model_file_path_out.encode('utf-8'), format_name.encode('utf-8')):
            raise ValueError('rwkv_quantize_model_file failed, check stderr')
    def rwkv_get_system_info_string(self) -> str:
        return self.library.rwkv_get_system_info_string().decode('utf-8')
def load_rwkv_shared_library() -> RWKVSharedLibrary:
    file_name: str
    if 'win32' in sys.platform or 'cygwin' in sys.platform:
        file_name = 'rwkv.dll'
    elif 'darwin' in sys.platform:
        file_name = 'librwkv.dylib'
    else:
        file_name = 'librwkv.so'
    child_paths: List[Callable[[pathlib.Path], pathlib.Path]] = [lambda p: p / 'bin' / 'Release' / file_name, lambda p: p / 'bin' / file_name, lambda p: p / 'build' / 'bin' / 'Release' / file_name, lambda p: p / 'build' / 'bin' / file_name, lambda p: p / 'build' / file_name, lambda p: p / file_name]
    working_dir: pathlib.Path = pathlib.Path(os.path.abspath(os.getcwd()))
    parent_paths: List[pathlib.Path] = [working_dir.parent.parent, working_dir.parent, working_dir, pathlib.Path(os.path.abspath(__file__)).parent.parent.parent]
    for parent_path in parent_paths:
        for child_path in child_paths:
            full_path: pathlib.Path = child_path(parent_path)
            if os.path.isfile(full_path):
                return RWKVSharedLibrary(str(full_path))
    raise ValueError(f'Failed to find {file_name} automatically; you need to find the library and create RWKVSharedLibrary specifying the path to it')