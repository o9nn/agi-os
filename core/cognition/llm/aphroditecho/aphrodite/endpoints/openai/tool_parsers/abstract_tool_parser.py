import os
from collections.abc import Sequence
from functools import cached_property
from typing import Callable, Optional, Union
from loguru import logger
from aphrodite.utils import import_from_path, is_list_of
from aphrodite.endpoints.openai.protocol import ChatCompletionRequest, DeltaMessage, ExtractedToolCallInformation
from aphrodite.transformers_utils.tokenizer import AnyTokenizer
class ToolParser:
    def __init__(self, tokenizer: AnyTokenizer):
        self.prev_tool_call_arr: list[dict] = []
        self.current_tool_id: int = -1
        self.current_tool_name_sent: bool = False
        self.streamed_args_for_tool: list[str] = []
        self.model_tokenizer = tokenizer
    @cached_property
    def vocab(self) -> dict[str, int]:
        return self.model_tokenizer.get_vocab()
    def adjust_request(self, request: ChatCompletionRequest) -> ChatCompletionRequest:
        return request
    def extract_tool_calls(self, model_output: str, request: ChatCompletionRequest) -> ExtractedToolCallInformation:
        raise NotImplementedError('AbstractToolParser.extract_tool_calls has not been implemented!')
    def extract_tool_calls_streaming(self, previous_text: str, current_text: str, delta_text: str, previous_token_ids: Sequence[int], current_token_ids: Sequence[int], delta_token_ids: Sequence[int], request: ChatCompletionRequest) -> Union[DeltaMessage, None]:
        raise NotImplementedError('AbstractToolParser.extract_tool_calls_streaming has not been implemented!')
class ToolParserManager:
    tool_parsers: dict[str, type] = {}
    @classmethod
    def get_tool_parser(cls, name) -> type:
        if name in cls.tool_parsers:
            return cls.tool_parsers[name]
        raise KeyError(f"tool helper: '{name}' not found in tool_parsers")
    @classmethod
    def _register_module(cls, module: type, module_name: Optional[Union[str, list[str]]]=None, force: bool=True) -> None:
        if not issubclass(module, ToolParser):
            raise TypeError(f'module must be subclass of ToolParser, but got {type(module)}')
        if module_name is None:
            module_name = module.__name__
        if isinstance(module_name, str):
            module_name = [module_name]
        for name in module_name:
            if not force and name in cls.tool_parsers:
                existed_module = cls.tool_parsers[name]
                raise KeyError(f'{name} is already registered at {existed_module.__module__}')
            cls.tool_parsers[name] = module
    @classmethod
    def register_module(cls, name: Optional[Union[str, list[str]]]=None, force: bool=True, module: Union[type, None]=None) -> Union[type, Callable]:
        if not isinstance(force, bool):
            raise TypeError(f'force must be a boolean, but got {type(force)}')
        if not (name is None or isinstance(name, str) or is_list_of(name, str)):
            raise TypeError(f'name must be None, an instance of str, or a sequence of str, but got {type(name)}')
        if module is not None:
            cls._register_module(module=module, module_name=name, force=force)
            return module
        def _register(module):
            cls._register_module(module=module, module_name=name, force=force)
            return module
        return _register
    @classmethod
    def import_tool_parser(cls, plugin_path: str) -> None:
        module_name = os.path.splitext(os.path.basename(plugin_path))[0]
        try:
            import_from_path(module_name, plugin_path)
        except Exception:
            logger.exception("Failed to load module '{}' from {}.", module_name, plugin_path)
            return