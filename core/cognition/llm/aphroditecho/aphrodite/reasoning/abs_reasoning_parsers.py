from __future__ import annotations
import os
from abc import abstractmethod
from collections.abc import Sequence
from functools import cached_property
from typing import TYPE_CHECKING, Any, Callable, Optional, Union
from loguru import logger
from aphrodite.utils import import_from_path, is_list_of
if TYPE_CHECKING:
    from aphrodite.endpoints.openai.protocol import ChatCompletionRequest, DeltaMessage, ResponsesRequest
    from aphrodite.transformers_utils.tokenizer import AnyTokenizer
else:
    ChatCompletionRequest = Any
    DeltaMessage = Any
    ResponsesRequest = Any
    AnyTokenizer = Any
class ReasoningParser:
    def __init__(self, tokenizer: AnyTokenizer):
        self.model_tokenizer = tokenizer
    @cached_property
    def vocab(self) -> dict[str, int]:
        return self.model_tokenizer.get_vocab()
    @abstractmethod
    def is_reasoning_end(self, input_ids: Sequence[int]) -> bool:
    @abstractmethod
    def extract_content_ids(self, input_ids: list[int]) -> list[int]:
    @abstractmethod
    def extract_reasoning_content(self, model_output: str, request: Union[ChatCompletionRequest, ResponsesRequest]) -> tuple[Optional[str], Optional[str]]:
    @abstractmethod
    def extract_reasoning_content_streaming(self, previous_text: str, current_text: str, delta_text: str, previous_token_ids: Sequence[int], current_token_ids: Sequence[int], delta_token_ids: Sequence[int]) -> Union[DeltaMessage, None]:
class ReasoningParserManager:
    reasoning_parsers: dict[str, type] = {}
    @classmethod
    def get_reasoning_parser(cls, name: str | None) -> type[ReasoningParser]:
        if name in cls.reasoning_parsers:
            return cls.reasoning_parsers[name]
        raise KeyError(f"reasoning helper: '{name}' not found in reasoning_parsers")
    @classmethod
    def _register_module(cls, module: type, module_name: Optional[Union[str, list[str]]]=None, force: bool=True) -> None:
        if not issubclass(module, ReasoningParser):
            raise TypeError(f'module must be subclass of ReasoningParser, but got {type(module)}')
        if module_name is None:
            module_name = module.__name__
        if isinstance(module_name, str):
            module_name = [module_name]
        for name in module_name:
            if not force and name in cls.reasoning_parsers:
                existed_module = cls.reasoning_parsers[name]
                raise KeyError(f'{name} is already registered at {existed_module.__module__}')
            cls.reasoning_parsers[name] = module
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
    def import_reasoning_parser(cls, plugin_path: str) -> None:
        module_name = os.path.splitext(os.path.basename(plugin_path))[0]
        try:
            import_from_path(module_name, plugin_path)
        except Exception:
            logger.exception("Failed to load module '{}' from {}.", module_name, plugin_path)
            return