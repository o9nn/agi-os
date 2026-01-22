import os
import sys
from abc import abstractmethod
from contextlib import contextmanager
from types import CodeType
from typing import Callable, Optional
import torch
from loguru import logger
import aphrodite.common.envs as envs
from aphrodite.common.config import CompilationLevel, get_current_aphrodite_config
class TorchCompileWrapperWithCustomDispatcher:
    def __init__(self, compiled_callable: Optional[Callable]=None, compilation_level: int=0):
        aphrodite_config = get_current_aphrodite_config()
        self.aphrodite_config = aphrodite_config
        if compiled_callable is None:
            backend = aphrodite_config.compilation_config.init_backend(aphrodite_config)
            options = None
            if isinstance(backend, str) and backend == 'inductor':
                options = get_current_aphrodite_config().compilation_config.inductor_compile_config
            compiled_callable = torch.compile(self.forward, fullgraph=envs.APHRODITE_TEST_DYNAMO_FULLGRAPH_CAPTURE, backend=backend, options=options)
        self.compiled_callable = compiled_callable
        self.original_code_object = self.__class__.forward.__code__
        self.compiled_codes: list[CodeType] = []
        torch._dynamo.convert_frame.register_bytecode_hook(self.bytecode_hook)
        self.use_custom_dispatcher: bool = compilation_level >= CompilationLevel.DYNAMO_ONCE
    def __call__(self, *args, **kwargs):
        return self.compiled_callable(*args, **kwargs)
    @abstractmethod
    def forward(self, *args, **kwargs):
        ...
    def bytecode_hook(self, old_code: CodeType, new_code: CodeType):
        if old_code is not self.original_code_object:
            return
        frame = sys._getframe()
        while frame and frame.f_back:
            frame = frame.f_back
            code_name = frame.f_code.co_name
            file_name = frame.f_code.co_filename.split(os.path.sep)[-1]
            if code_name == '_compile' and file_name == 'convert_frame.py':
                break
        frame = frame.f_locals['frame']
        assert frame.f_code == old_code
        if frame.f_locals['self'] is not self:
            return
        self.compiled_codes.append(new_code)
        debug_dump_dir = self.aphrodite_config.compilation_config.debug_dump_path
        if isinstance(debug_dump_dir, str) and debug_dump_dir != '':
            rank = self.aphrodite_config.parallel_config.rank
            decompiled_file = os.path.join(debug_dump_dir, f'rank_{rank}', 'transformed_code.py')
            if not os.path.exists(decompiled_file):
                try:
                    import depyf
                    src = depyf.decompile(new_code)
                    with open(decompiled_file, 'w') as f:
                        f.write(src)
                    logger.debug('Dynamo transformed code saved to {}', decompiled_file)
                except Exception:
                    pass
        if self.aphrodite_config.compilation_config.use_cudagraph and 'update' in new_code.co_names:
            import depyf
            src = depyf.decompile(new_code)
            msg = 'Assigning / modifying buffers of nn.Module during forward pass is not allowed when using cudagraph inside the compiler because it will cause silent errors. Please use eager mode or fix the code. The following code contains clues about which buffer is being modified (please search for the usage of the function `update`):\n' + src
            raise RuntimeError(msg)
    @contextmanager
    def dispatch_to_code(self, index: int):
        self.__class__.forward.__code__ = self.compiled_codes[index]
        yield
        self.__class__.forward.__code__ = self.original_code_object