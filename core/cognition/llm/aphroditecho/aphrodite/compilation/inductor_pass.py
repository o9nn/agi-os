import hashlib
import inspect
import json
import types
from contextlib import contextmanager
from typing import Any, Callable, Optional, Union
import torch
from torch import fx
from aphrodite.utils import is_torch_equal_or_newer
if is_torch_equal_or_newer('2.6'):
    from torch._inductor.custom_graph_pass import CustomGraphPass
else:
    from .torch25_custom_graph_pass import Torch25CustomGraphPass as CustomGraphPass
_pass_context = None
class PassContext:
    def __init__(self, runtime_shape: Optional[int]):
        self.runtime_shape = runtime_shape
def get_pass_context() -> PassContext:
    assert _pass_context is not None
    return _pass_context
@contextmanager
def pass_context(runtime_shape: Optional[int]):
    global _pass_context
    prev_context = _pass_context
    _pass_context = PassContext(runtime_shape)
    try:
        yield
    finally:
        _pass_context = prev_context
class InductorPass(CustomGraphPass):
    def uuid(self) -> Any:
        return InductorPass.hash_source(self)
    @staticmethod
    def hash_source(*srcs: Union[str, Any]):
        hasher = hashlib.sha256()
        for src in srcs:
            if isinstance(src, str):
                src_str = src
            elif isinstance(src, (types.FunctionType, type)):
                src_str = inspect.getsource(src)
            else:
                src_str = inspect.getsource(src.__class__)
            hasher.update(src_str.encode('utf-8'))
        return hasher.hexdigest()
    @staticmethod
    def hash_dict(dict_: dict[Any, Any]):
        encoded = json.dumps(dict_, sort_keys=True).encode('utf-8')
        return hashlib.sha256(encoded).hexdigest()
    def is_applicable_for_shape(self, shape: Optional[int]):
        return True
class CallableInductorPass(InductorPass):
    def __init__(self, callable: Callable[[fx.Graph], None], uuid: Optional[Any]=None):
        self.callable = callable
        self._uuid = self.hash_source(callable) if uuid is None else uuid
    def __call__(self, graph: torch.fx.Graph):
        self.callable(graph)
    def uuid(self) -> Any:
        return self._uuid