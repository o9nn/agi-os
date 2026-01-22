import contextlib
import copy
import hashlib
import os
from contextlib import ExitStack
from typing import Any, Callable, Optional
from unittest.mock import patch
import torch
import torch._inductor.compile_fx
import torch.fx as fx
import aphrodite.common.envs as envs
from aphrodite.common.config import AphroditeConfig
from aphrodite.utils import is_torch_equal_or_newer
from aphrodite.compilation.counter import compilation_counter
from .inductor_pass import pass_context
class CompilerInterface:
    name: str
    def initialize_cache(self, cache_dir: str, disable_cache: bool=False, prefix: str=''):
        pass
    def compute_hash(self, aphrodite_config: AphroditeConfig) -> str:
        return ''
    def compile(self, graph: fx.GraphModule, example_inputs: list[Any], compiler_config: dict[str, Any], runtime_shape: Optional[int]=None, key: Optional[str]=None) -> tuple[Optional[Callable], Optional[Any]]:
        return (None, None)
    def load(self, handle: Any, graph: fx.GraphModule, example_inputs: list[Any], graph_index: int, runtime_shape: Optional[int]=None) -> Callable:
        raise NotImplementedError('caching is not supported')
class AlwaysHitShapeEnv:
    def __init__(self) -> None:
        self.guards: list[Any] = []
    def evaluate_guards_expression(self, *args, **kwargs):
        return True
    def get_pruned_guards(self, *args, **kwargs):
        return []
    def produce_guards_expression(self, *args, **kwargs):
        return ''
def get_inductor_factors() -> list[Any]:
    factors: list[Any] = []
    from torch._inductor.codecache import CacheBase
    system_factors = CacheBase.get_system()
    factors.append(system_factors)
    from torch._inductor.codecache import torch_key
    torch_factors = torch_key()
    factors.append(torch_factors)
    return factors
class InductorStandaloneAdaptor(CompilerInterface):
    name = 'inductor_standalone'
    def compute_hash(self, aphrodite_config: AphroditeConfig) -> str:
        factors = get_inductor_factors()
        hash_str = hashlib.md5(str(factors).encode(), usedforsecurity=False).hexdigest()[:10]
        return hash_str
    def initialize_cache(self, cache_dir: str, disable_cache: bool=False, prefix: str=''):
        self.cache_dir = cache_dir
    def compile(self, graph: fx.GraphModule, example_inputs: list[Any], compiler_config: dict[str, Any], runtime_shape: Optional[int]=None, key: Optional[str]=None) -> tuple[Optional[Callable], Optional[Any]]:
        compilation_counter.num_inductor_compiles += 1
        current_config = {}
        if compiler_config is not None:
            current_config.update(compiler_config)
        set_inductor_config(current_config, runtime_shape)
        if isinstance(runtime_shape, int):
            dynamic_shapes = 'from_example_inputs'
        else:
            dynamic_shapes = 'from_tracing_context'
        from torch._inductor import standalone_compile
        with pass_context(runtime_shape):
            compiled_graph = standalone_compile(graph, example_inputs, dynamic_shapes=dynamic_shapes, options={'config_patches': current_config})
        assert key is not None
        path = os.path.join(self.cache_dir, key)
        if not envs.APHRODITE_DISABLE_COMPILE_CACHE:
            compiled_graph.save(path=path, format='unpacked')
            compilation_counter.num_compiled_artifacts_saved += 1
        return (compiled_graph, (key, path))
    def load(self, handle: Any, graph: fx.GraphModule, example_inputs: list[Any], graph_index: int, runtime_shape: Optional[int]=None) -> Callable:
        assert isinstance(handle, tuple)
        assert isinstance(handle[0], str)
        assert isinstance(handle[1], str)
        path = handle[1]
        inductor_compiled_graph = torch._inductor.CompiledArtifact.load(path=path, format='unpacked')
        from torch._inductor.compile_fx import graph_returns_tuple
        returns_tuple = graph_returns_tuple(graph)
        def compiled_graph_wrapper(*args):
            graph_output = inductor_compiled_graph(*args)
            if returns_tuple:
                return graph_output
            else:
                return graph_output[0]
        return compiled_graph_wrapper
class InductorAdaptor(CompilerInterface):
    name = 'inductor'
    def compute_hash(self, aphrodite_config: AphroditeConfig) -> str:
        factors = get_inductor_factors()
        hash_str = hashlib.md5(str(factors).encode(), usedforsecurity=False).hexdigest()[:10]
        return hash_str
    def initialize_cache(self, cache_dir: str, disable_cache: bool=False, prefix: str=''):
        self.cache_dir = cache_dir
        self.prefix = prefix
        self.base_cache_dir = cache_dir[:-len(prefix)] if prefix else cache_dir
        if disable_cache:
            return
        inductor_cache = os.path.join(self.base_cache_dir, 'inductor_cache')
        os.makedirs(inductor_cache, exist_ok=True)
        os.environ['TORCHINDUCTOR_CACHE_DIR'] = inductor_cache
        triton_cache = os.path.join(self.base_cache_dir, 'triton_cache')
        os.makedirs(triton_cache, exist_ok=True)
        os.environ['TRITON_CACHE_DIR'] = triton_cache
    def compile(self, graph: fx.GraphModule, example_inputs: list[Any], compiler_config: dict[str, Any], runtime_shape: Optional[int]=None, key: Optional[str]=None) -> tuple[Optional[Callable], Optional[Any]]:
        compilation_counter.num_inductor_compiles += 1
        from torch._inductor.compile_fx import compile_fx
        current_config = {}
        if compiler_config is not None:
            current_config.update(compiler_config)
        current_config['fx_graph_cache'] = True
        current_config['fx_graph_remote_cache'] = False
        set_inductor_config(current_config, runtime_shape)
        graph = copy.deepcopy(graph)
        hash_str, file_path = (None, None)
        from torch._inductor.codecache import FxGraphCache, compiled_fx_graph_hash
        if torch.__version__.startswith('2.5'):
            original_load = FxGraphCache.load
            original_load_name = 'torch._inductor.codecache.FxGraphCache.load'
            def hijack_load(*args, **kwargs):
                inductor_compiled_graph = original_load(*args, **kwargs)
                nonlocal file_path
                compiled_fn = inductor_compiled_graph.current_callable
                file_path = compiled_fn.__code__.co_filename
                if not file_path.startswith(self.base_cache_dir):
                    for cell in compiled_fn.__closure__:
                        if not callable(cell.cell_contents):
                            continue
                        if cell.cell_contents.__code__.co_filename.startswith(self.base_cache_dir):
                            file_path = cell.cell_contents.__code__.co_filename
                            break
                return inductor_compiled_graph
            hijacked_compile_fx_inner = torch._inductor.compile_fx.compile_fx_inner
        elif torch.__version__ >= '2.6':
            original_load_name = None
            def hijacked_compile_fx_inner(*args, **kwargs):
                output = torch._inductor.compile_fx.compile_fx_inner(*args, **kwargs)
                nonlocal hash_str
                inductor_compiled_graph = output
                if inductor_compiled_graph is not None:
                    nonlocal file_path
                    compiled_fn = inductor_compiled_graph.current_callable
                    file_path = compiled_fn.__code__.co_filename
                    if not file_path.startswith(self.base_cache_dir):
                        for cell in compiled_fn.__closure__:
                            if not callable(cell.cell_contents):
                                continue
                            code = cell.cell_contents.__code__
                            if code.co_filename.startswith(self.base_cache_dir):
                                file_path = code.co_filename
                                break
                    hash_str = inductor_compiled_graph._fx_graph_cache_key
                return output
        def hijack_compiled_fx_graph_hash(*args, **kwargs):
            out = compiled_fx_graph_hash(*args, **kwargs)
            nonlocal hash_str
            hash_str = out[0]
            return out
        def _check_can_cache(*args, **kwargs):
            return
        def _get_shape_env() -> AlwaysHitShapeEnv:
            return AlwaysHitShapeEnv()
        with ExitStack() as stack:
            if original_load_name is not None:
                stack.enter_context(patch(original_load_name, hijack_load))
            stack.enter_context(patch('torch._inductor.codecache.compiled_fx_graph_hash', hijack_compiled_fx_graph_hash))
            stack.enter_context(patch('torch._inductor.codecache.FxGraphCache._get_shape_env', _get_shape_env))
            from torch._functorch._aot_autograd.autograd_cache import AOTAutogradCache
            if hasattr(AOTAutogradCache, '_get_shape_env'):
                stack.enter_context(patch('torch._functorch._aot_autograd.autograd_cache.AOTAutogradCache._get_shape_env', _get_shape_env))
            stack.enter_context(patch('torch._inductor.codecache.FxGraphCache._check_can_cache', _check_can_cache))
            stack.enter_context(self.metrics_context())
            if is_torch_equal_or_newer('2.6'):
                stack.enter_context(torch._inductor.config.patch(fx_graph_remote_cache=False))
                stack.enter_context(torch._functorch.config.patch(enable_autograd_cache=False))
                stack.enter_context(torch._functorch.config.patch(enable_remote_autograd_cache=False))
            with pass_context(runtime_shape):
                compiled_graph = compile_fx(graph, example_inputs, inner_compile=hijacked_compile_fx_inner, config_patches=current_config)
        if not envs.APHRODITE_DISABLE_COMPILE_CACHE:
            if hash_str is None:
                raise RuntimeError('Aphrodite failed to compile the model. The most likely reason for this is that a previous compilation failed, leading to a corrupted compilation artifact. We recommend trying to remove ~/.cache/aphrodite/torch_compile_cache and try again to see the real issue. ')
            assert file_path is not None, 'failed to get the file path of the compiled graph'
        return (compiled_graph, (hash_str, file_path))
    def load(self, handle: Any, graph: fx.GraphModule, example_inputs: list[Any], graph_index: int, runtime_shape: Optional[int]=None) -> Callable:
        assert isinstance(handle, tuple)
        assert isinstance(handle[0], str)
        assert isinstance(handle[1], str)
        hash_str = handle[0]
        from torch._functorch._aot_autograd.autograd_cache import AOTAutogradCache
        from torch._inductor.codecache import FxGraphCache
        with ExitStack() as exit_stack:
            exit_stack.enter_context(patch('torch._inductor.codecache.FxGraphCache._get_shape_env', lambda *args, **kwargs: AlwaysHitShapeEnv()))
            if hasattr(AOTAutogradCache, '_get_shape_env'):
                exit_stack.enter_context(patch('torch._functorch._aot_autograd.autograd_cache.AOTAutogradCache._get_shape_env', lambda *args, **kwargs: AlwaysHitShapeEnv()))
            exit_stack.enter_context(self.metrics_context())
            if torch.__version__.startswith('2.5'):
                inductor_compiled_graph = FxGraphCache._lookup_graph(hash_str, example_inputs, True, False)
                assert inductor_compiled_graph is not None, f'Inductor cache lookup failed. Please removethe cache directory and try again.'
            elif torch.__version__ >= '2.6':
                from torch._inductor.output_code import CompiledFxGraphConstantsWithGm
                constants = CompiledFxGraphConstantsWithGm(graph)
                inductor_compiled_graph, _ = FxGraphCache._lookup_graph(hash_str, example_inputs, True, None, constants)
                assert inductor_compiled_graph is not None, f'Inductor cache lookup failed. Please removethe cache directory and try again.'
        from torch._inductor.compile_fx import graph_returns_tuple
        returns_tuple = graph_returns_tuple(graph)
        def compiled_graph(*args):
            list_args = list(args)
            graph_output = inductor_compiled_graph(list_args)
            if returns_tuple:
                return graph_output
            else:
                return graph_output[0]
        return compiled_graph
    def metrics_context(self) -> contextlib.AbstractContextManager:
        if is_torch_equal_or_newer('2.6'):
            import torch._dynamo.utils
            return torch._dynamo.utils.get_metrics_context()
        else:
            return contextlib.nullcontext()
def set_inductor_config(config, runtime_shape):
    if isinstance(runtime_shape, int):
        config['max_autotune'] = True
        config['coordinate_descent_tuning'] = True
class EagerAdaptor(CompilerInterface):
    name = 'eager'
    def compile(self, graph: fx.GraphModule, example_inputs: list[Any], compiler_config: dict[str, Any], runtime_shape: Optional[int]=None, key: Optional[str]=None) -> tuple[Optional[Callable], Optional[Any]]:
        compilation_counter.num_eager_compiles += 1
        return (graph, None)