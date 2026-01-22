import inspect
from typing import Callable, Optional, TypeVar, Union, overload
from unittest.mock import patch
import torch
import torch.nn as nn
from loguru import logger
from torch._dynamo.symbolic_convert import InliningInstructionTranslator
from aphrodite.common.config import AphroditeConfig, CompilationLevel
from aphrodite.common.sequence import IntermediateTensors
from aphrodite.utils import supports_dynamo
from aphrodite.compilation.counter import compilation_counter
from aphrodite.compilation.wrapper import TorchCompileWrapperWithCustomDispatcher
from .monitor import start_monitoring_torch_compile
IGNORE_COMPILE_KEY = '_ignore_compile_aphrodite'
_T = TypeVar('_T', bound=type[nn.Module])
def ignore_torch_compile(cls: _T) -> _T:
    setattr(cls, IGNORE_COMPILE_KEY, True)
    return cls
def _should_ignore_torch_compile(cls) -> bool:
    return getattr(cls, IGNORE_COMPILE_KEY, False)
@overload
def support_torch_compile(*, dynamic_arg_dims: Optional[dict[str, Union[int, list[int]]]]) -> Callable[[_T], _T]:
    ...
@overload
def support_torch_compile(cls: _T) -> _T:
    ...
def support_torch_compile(cls: Optional[_T]=None, *, dynamic_arg_dims: Optional[dict[str, Union[int, list[int]]]]=None) -> Union[Callable[[_T], _T], _T]:
    def cls_decorator_helper(cls: _T) -> _T:
        if not hasattr(cls, 'forward'):
            raise TypeError('decorated class should have a forward method.')
        sig = inspect.signature(cls.forward)
        inferred_dynamic_arg_dims = dynamic_arg_dims
        if inferred_dynamic_arg_dims is None:
            inferred_dynamic_arg_dims = {}
            for k, v in sig.parameters.items():
                if v.annotation in [torch.Tensor, Optional[torch.Tensor], IntermediateTensors, Optional[IntermediateTensors]]:
                    inferred_dynamic_arg_dims[k] = 0
            logger.debug('Inferred dynamic dimensions for forward method of {}: {}', cls, list(inferred_dynamic_arg_dims.keys()))
        if len(inferred_dynamic_arg_dims) == 0:
            raise ValueError(f'No dynamic dimensions found in the forward method of {cls}. Please provide dynamic_arg_dims explicitly.')
        for k in inferred_dynamic_arg_dims:
            if k not in sig.parameters:
                raise ValueError(f'Argument {k} not found in the forward method of {cls}')
        return _support_torch_compile(cls, inferred_dynamic_arg_dims)
    if cls is not None:
        assert isinstance(cls, type)
        return cls_decorator_helper(cls)
    return cls_decorator_helper
def _support_torch_compile(cls: _T, dynamic_arg_dims: dict[str, Union[int, list[int]]]) -> _T:
    if TorchCompileWrapperWithCustomDispatcher in cls.__bases__:
        return cls
    cls.__bases__ = cls.__bases__ + (TorchCompileWrapperWithCustomDispatcher,)
    old_init = cls.__init__
    setattr(cls, IGNORE_COMPILE_KEY, False)
    def __init__(self, *, aphrodite_config: AphroditeConfig, prefix: str='', **kwargs):
        old_init(self, aphrodite_config=aphrodite_config, prefix=prefix, **kwargs)
        self.aphrodite_config = aphrodite_config
        self.do_not_compile = aphrodite_config.compilation_config.level in [CompilationLevel.NO_COMPILATION, CompilationLevel.DYNAMO_AS_IS] or not supports_dynamo() or _should_ignore_torch_compile(self.__class__)
        if self.do_not_compile:
            return
        compilation_counter.num_models_seen += 1
        TorchCompileWrapperWithCustomDispatcher.__init__(self, compilation_level=aphrodite_config.compilation_config.level)
    cls.__init__ = __init__
    def __call__(self, *args, **kwargs):
        if self.do_not_compile or torch.compiler.is_compiling():
            return self.forward(*args, **kwargs)
        if len(self.compiled_codes) < 1:
            sig = inspect.signature(self.__class__.forward)
            bound_args = sig.bind(self, *args, **kwargs)
            bound_args.apply_defaults()
            for k, dims in dynamic_arg_dims.items():
                arg = bound_args.arguments.get(k)
                if arg is not None:
                    dims = [dims] if isinstance(dims, int) else dims
                    if isinstance(arg, torch.Tensor):
                        dims = [arg.ndim + dim if dim < 0 else dim for dim in dims]
                        torch._dynamo.mark_dynamic(arg, dims)
                    elif isinstance(arg, IntermediateTensors):
                        for tensor in arg.tensors.values():
                            dims = [tensor.ndim + dim if dim < 0 else dim for dim in dims]
                            torch._dynamo.mark_dynamic(tensor, dims)
                    else:
                        raise ValueError(f'Unsupported dynamic dimensions {dims} for argument {k} with type {type(arg)}.')
            start_monitoring_torch_compile(self.aphrodite_config)
            logger.debug('Start compiling function {}', self.original_code_object)
        if len(self.compiled_codes) < 1 or not self.use_custom_dispatcher:
            torch._dynamo.eval_frame.remove_from_cache(self.original_code_object)
            self.aphrodite_config.compilation_config.traced_files.add(self.original_code_object.co_filename)
            inline_call = InliningInstructionTranslator.inline_call
            def patched_inline_call(parent, func, args, kwargs):
                code = func.get_code()
                self.aphrodite_config.compilation_config.traced_files.add(code.co_filename)
                return inline_call(parent, func, args, kwargs)
            with patch.object(InliningInstructionTranslator, 'inline_call', patched_inline_call):
                output = self.compiled_callable(*args, **kwargs)
            return output
        with self.dispatch_to_code(0):
            model_output = self.forward(*args, **kwargs)
            return model_output
    cls.__call__ = __call__
    return cls