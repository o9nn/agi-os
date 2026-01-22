import json
from collections.abc import Sequence
from typing import Any, Optional
import regex as re
from loguru import logger
from transformers import PreTrainedTokenizerBase
from aphrodite.endpoints.chat_utils import random_tool_call_id
from aphrodite.endpoints.openai.protocol import ChatCompletionRequest, DeltaMessage, ExtractedToolCallInformation, FunctionCall, ToolCall
from aphrodite.endpoints.openai.tool_parsers.abstract_tool_parser import ToolParser, ToolParserManager
@ToolParserManager.register_module('phi4_mini_json')
class Phi4MiniJsonToolParser(ToolParser):
    def __init__(self, tokenizer: PreTrainedTokenizerBase) -> None:
        super().__init__(tokenizer)
        self.prev_tool_call_arr: list[dict[str, Any]] = []
        self.current_tool_id: int = -1
        self.current_tool_name_sent: bool = False
        self.streamed_args_for_tool: list[str] = []
        self.bot_token: str = 'functools'
    def extract_tool_calls(self, model_output: str, request: ChatCompletionRequest) -> ExtractedToolCallInformation:
        logger.debug('Model output: {}', model_output)
        pattern = 'functools\\[(.*?)\\]'
        matches = re.search(pattern, model_output, re.DOTALL)
        if not matches:
            logger.debug('No function calls found')
            return ExtractedToolCallInformation(tools_called=False, tool_calls=[], content=model_output)
        try:
            function_call_arr: list[dict[str, Any]] = []
            try:
                json_content = '[' + matches.group(1) + ']'
                function_call_arr = json.loads(json_content)
                logger.debug('Successfully extracted {} function calls', len(function_call_arr))
            except json.JSONDecodeError as e:
                logger.error('Failed to parse function calls from model output. Error: {}', str(e))
            tool_calls: list[ToolCall] = [ToolCall(id=random_tool_call_id(), type='function', function=FunctionCall(name=raw_function_call['name'], arguments=json.dumps(raw_function_call['arguments'] if 'arguments' in raw_function_call else raw_function_call['parameters'], ensure_ascii=False))) for raw_function_call in function_call_arr]
            ret = ExtractedToolCallInformation(tools_called=True, tool_calls=tool_calls, content=None)
            return ret
        except Exception:
            return ExtractedToolCallInformation(tools_called=False, tool_calls=[], content=model_output)
    def extract_tool_calls_streaming(self, previous_text: str, current_text: str, delta_text: str, previous_token_ids: Sequence[int], current_token_ids: Sequence[int], delta_token_ids: Sequence[int], request: ChatCompletionRequest) -> Optional[DeltaMessage]:
        return None