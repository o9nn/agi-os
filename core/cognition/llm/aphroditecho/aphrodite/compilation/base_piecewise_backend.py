from typing import Any, Callable, Protocol
import torch.fx as fx
from aphrodite.common.config import AphroditeConfig
from aphrodite.compilation.backends import AphroditeBackend
class AbstractPiecewiseBackend(Protocol):
    def __init__(self, graph: fx.GraphModule, aphrodite_config: AphroditeConfig, graph_pool: Any, piecewise_compile_index: int, total_piecewise_compiles: int, sym_shape_indices: list[int], compiled_graph_for_general_shape: Callable, aphrodite_backend: AphroditeBackend, **kwargs):
        raise NotImplementedError
    def __call__(self, *args) -> Any:
        raise NotImplementedError