import json
from collections.abc import Sequence
from typing import Union
import partial_json_parser
from loguru import logger
from partial_json_parser.core.options import Allow
from aphrodite.endpoints.chat_utils import random_tool_call_id
from aphrodite.endpoints.openai.protocol import ChatCompletionRequest, DeltaFunctionCall, DeltaMessage, DeltaToolCall, ExtractedToolCallInformation, FunctionCall, ToolCall
from aphrodite.endpoints.openai.tool_parsers.abstract_tool_parser import ToolParser, ToolParserManager
from aphrodite.endpoints.openai.tool_parsers.utils import consume_space, find_common_prefix, is_complete_json, partial_json_loads
from aphrodite.transformers_utils.tokenizer import AnyTokenizer
@ToolParserManager.register_module('granite')
class GraniteToolParser(ToolParser):
    def __init__(self, tokenizer: AnyTokenizer):
        super().__init__(tokenizer)
        self.bot_token = '<|tool_call|>'
        self.bot_string = '<tool_call>'
    def extract_tool_calls(self, model_output: str, request: ChatCompletionRequest) -> ExtractedToolCallInformation:
        stripped = model_output.strip().removeprefix(self.bot_token).removeprefix(self.bot_string).lstrip()
        if not stripped or stripped[0] != '[':
            return ExtractedToolCallInformation(tools_called=False, tool_calls=[], content=model_output)
        try:
            raw_function_calls = json.loads(stripped)
            if not isinstance(raw_function_calls, list):
                raise Exception(f'Expected dict or list, got {type(raw_function_calls)}')
            logger.debug('Extracted {} tool calls', len(raw_function_calls))
            tool_calls = [ToolCall(type='function', function=FunctionCall(name=function_call['name'], arguments=json.dumps(function_call['arguments'], ensure_ascii=False))) for function_call in raw_function_calls]
            return ExtractedToolCallInformation(tools_called=True, tool_calls=tool_calls, content=None)
        except Exception as e:
            logger.error('Error in extracting tool call from response {}', e)
            return ExtractedToolCallInformation(tools_called=False, tool_calls=[], content=model_output)
    def extract_tool_calls_streaming(self, previous_text: str, current_text: str, delta_text: str, previous_token_ids: Sequence[int], current_token_ids: Sequence[int], delta_token_ids: Sequence[int], request: ChatCompletionRequest) -> Union[DeltaMessage, None]:
        start_idx = consume_space(0, current_text)
        if current_text[start_idx:].startswith(self.bot_token):
            start_idx = consume_space(start_idx + len(self.bot_token), current_text)
        if current_text[start_idx:].startswith(self.bot_string):
            start_idx = consume_space(start_idx + len(self.bot_string), current_text)
        if not current_text or start_idx >= len(current_text) or current_text[start_idx] != '[':
            return DeltaMessage(content=delta_text)
        flags = Allow.ALL if self.current_tool_name_sent else Allow.ALL & ~Allow.STR
        try:
            tool_call_arr = None
            is_complete = None
            try:
                tool_calls, end_idx = partial_json_loads(current_text[start_idx:], flags)
                if type(tool_calls) is list:
                    tool_call_arr = tool_calls
                else:
                    return DeltaMessage(content=delta_text)
                is_complete = [True] * len(tool_calls)
                if not is_complete_json(current_text[start_idx:start_idx + end_idx]):
                    is_complete[-1] = False
            except partial_json_parser.core.exceptions.MalformedJSON:
                logger.debug('not enough tokens to parse into JSON yet')
                return None
            if not tool_call_arr:
                return None
            current_tool_call: dict = tool_call_arr[self.current_tool_id]
            delta = None
            if len(tool_call_arr) > self.current_tool_id + 1:
                if self.current_tool_id >= 0:
                    cur_arguments = current_tool_call.get('arguments')
                    if cur_arguments:
                        cur_args_json = json.dumps(cur_arguments, ensure_ascii=False)
                        sent = len(self.streamed_args_for_tool[self.current_tool_id])
                        argument_diff = cur_args_json[sent:]
                        logger.debug('got arguments diff: {}', argument_diff)
                        delta = DeltaMessage(tool_calls=[DeltaToolCall(index=self.current_tool_id, function=DeltaFunctionCall(arguments=argument_diff).model_dump(exclude_none=True))])
                        self.streamed_args_for_tool[self.current_tool_id] += argument_diff
                self.current_tool_id = len(tool_call_arr) - 1
                self.current_tool_name_sent = False
                self.streamed_args_for_tool.append('')
                logger.debug('starting on new tool {}', self.current_tool_id)
                return delta
            elif not self.current_tool_name_sent:
                function_name = current_tool_call.get('name')
                if function_name:
                    delta = DeltaMessage(tool_calls=[DeltaToolCall(index=self.current_tool_id, type='function', id=random_tool_call_id(), function=DeltaFunctionCall(name=function_name).model_dump(exclude_none=True))])
                    self.current_tool_name_sent = True
            else:
                cur_arguments = current_tool_call.get('arguments')
                if cur_arguments:
                    sent = len(self.streamed_args_for_tool[self.current_tool_id])
                    cur_args_json = json.dumps(cur_arguments, ensure_ascii=False)
                    prev_arguments = self.prev_tool_call_arr[self.current_tool_id].get('arguments')
                    argument_diff = None
                    if is_complete[self.current_tool_id]:
                        argument_diff = cur_args_json[sent:]
                    elif prev_arguments:
                        prev_args_json = json.dumps(prev_arguments, ensure_ascii=False)
                        if cur_args_json != prev_args_json:
                            prefix = find_common_prefix(prev_args_json, cur_args_json)
                            argument_diff = prefix[sent:]
                    if argument_diff is not None:
                        delta = DeltaMessage(tool_calls=[DeltaToolCall(index=self.current_tool_id, function=DeltaFunctionCall(arguments=argument_diff).model_dump(exclude_none=True))])
                        self.streamed_args_for_tool[self.current_tool_id] += argument_diff
            self.prev_tool_call_arr = tool_call_arr
            return delta
        except Exception as e:
            logger.error('Error trying to handle streaming tool call: {}', e)
            logger.debug('Skipping chunk as a result of tool streaming extraction error')
            return None