import json
from collections.abc import AsyncGenerator
from typing import Any, Optional, Union
import jinja2
from fastapi import Request
from loguru import logger
from aphrodite.common.config import ModelConfig
from aphrodite.utils import random_uuid
from aphrodite.endpoints.logger import RequestLogger
from aphrodite.endpoints.openai.protocol import AnthropicContentBlockDelta, AnthropicContentBlockStart, AnthropicContentBlockStop, AnthropicImageBlock, AnthropicMessage, AnthropicMessageDelta, AnthropicMessagesRequest, AnthropicMessagesResponse, AnthropicMessageStart, AnthropicMessageStop, AnthropicTextBlock, AnthropicThinkingBlock, AnthropicToolResultBlock, AnthropicToolUseBlock, AnthropicUsage, ChatCompletionMessageParam, ChatCompletionRequest, ErrorResponse
from aphrodite.endpoints.openai.serving_engine import OpenAIServing
from aphrodite.endpoints.openai.serving_models import OpenAIServingModels
from aphrodite.engine.protocol import EngineClient
class OpenAIServingMessages(OpenAIServing):
    def __init__(self, engine_client: EngineClient, model_config: ModelConfig, models: OpenAIServingModels, response_role: str, *, request_logger: Optional[RequestLogger], chat_template: Optional[str], return_tokens_as_token_ids: bool=False, reasoning_parser: str='', enable_auto_tools: bool=False, tool_parser: Optional[str]=None) -> None:
        super().__init__(engine_client=engine_client, model_config=model_config, models=models, request_logger=request_logger, return_tokens_as_token_ids=return_tokens_as_token_ids)
        self.response_role = response_role
        self.chat_template = chat_template
        self.enable_auto_tools = enable_auto_tools
        self.tool_parser = tool_parser
        self.reasoning_parser = reasoning_parser
        from aphrodite.endpoints.openai.serving_chat import OpenAIServingChat
        self.chat_serving = OpenAIServingChat(engine_client=engine_client, model_config=model_config, models=models, response_role=response_role, request_logger=request_logger, chat_template=chat_template, chat_template_content_format='string', return_tokens_as_token_ids=return_tokens_as_token_ids, reasoning_parser=reasoning_parser, enable_auto_tools=enable_auto_tools, tool_parser=tool_parser)
        self.default_sampling_params = self.model_config.get_diff_sampling_param()
        if self.default_sampling_params:
            source = self.model_config.generation_config
            source = 'model' if source == 'auto' else source
            logger.info('Using default messages sampling params from {}: {}', source, self.default_sampling_params)
    async def create_message(self, request: AnthropicMessagesRequest, raw_request: Optional[Request]=None) -> Union[AsyncGenerator[str, None], AnthropicMessagesResponse, ErrorResponse]:
        error_check_ret = await self._check_model(request)
        if error_check_ret is not None:
            return error_check_ret
        if self.engine_client.errored:
            raise self.engine_client.dead_error
        try:
            chat_request = self._convert_to_chat_request(request)
            chat_response = await self.chat_serving.create_chat_completion(chat_request, raw_request)
            if request.stream:
                return self._convert_streaming_response(chat_response, request, raw_request)
            else:
                return self._convert_response(chat_response, request, raw_request)
        except (ValueError, TypeError, RuntimeError, jinja2.TemplateError) as e:
            logger.exception('Error in Messages API processing')
            return self.create_error_response(str(e))
    def _convert_to_chat_request(self, request: AnthropicMessagesRequest) -> ChatCompletionRequest:
        messages = []
        if request.system:
            if isinstance(request.system, str):
                system_content = request.system
            else:
                system_content = '\n'.join((block.text for block in request.system))
            messages.append({'role': 'system', 'content': system_content})
        for msg in request.messages:
            openai_msg = self._convert_message(msg)
            messages.append(openai_msg)
        tools = None
        if request.tools:
            tools = []
            for tool in request.tools:
                openai_tool = {'type': 'function', 'function': {'name': tool.name, 'description': tool.description or '', 'parameters': tool.input_schema}}
                tools.append(openai_tool)
        tool_choice = 'none'
        if request.tool_choice:
            if request.tool_choice.type == 'auto':
                tool_choice = 'auto'
            elif request.tool_choice.type == 'any':
                tool_choice = 'auto'
            elif request.tool_choice.type == 'tool':
                tool_choice = {'type': 'function', 'function': {'name': request.tool_choice.name}}
        elif request.tools:
            tool_choice = 'auto'
        stop = request.stop_sequences or []
        chat_request_dict = {'model': request.model, 'messages': messages, 'max_tokens': request.max_tokens, 'temperature': request.temperature or 0.7, 'top_p': request.top_p or 1.0, 'top_k': request.top_k or -1, 'frequency_penalty': request.frequency_penalty or 0.0, 'presence_penalty': request.presence_penalty or 0.0, 'stop': stop, 'stream': request.stream or False, 'seed': request.seed, 'tools': tools, 'tool_choice': tool_choice}
        if request.thinking and request.thinking.type == 'enabled':
            pass
        return ChatCompletionRequest(**chat_request_dict)
    def _convert_message(self, msg: AnthropicMessage) -> ChatCompletionMessageParam:
        if isinstance(msg.content, str):
            return {'role': msg.role, 'content': msg.content}
        if msg.role == 'user':
            content_parts = []
            for block in msg.content:
                if isinstance(block, AnthropicTextBlock):
                    content_parts.append({'type': 'text', 'text': block.text})
                elif isinstance(block, AnthropicImageBlock):
                    content_parts.append({'type': 'image_url', 'image_url': {'url': f'data:{block.source.media_type};base64,{block.source.data}'}})
                elif isinstance(block, AnthropicToolResultBlock):
                    content_text = block.content if isinstance(block.content, str) else 'Tool result'
                    content_parts.append({'type': 'text', 'text': f'Tool result: {content_text}'})
            return {'role': 'user', 'content': content_parts if content_parts else ''}
        elif msg.role == 'assistant':
            content_text = ''
            tool_calls = []
            for block in msg.content:
                if isinstance(block, AnthropicTextBlock):
                    content_text += block.text
                elif isinstance(block, AnthropicThinkingBlock):
                    content_text += f'<thinking>{block.thinking}</thinking>'
                elif isinstance(block, AnthropicToolUseBlock):
                    tool_calls.append({'id': block.id, 'type': 'function', 'function': {'name': block.name, 'arguments': json.dumps(block.input)}})
            result = {'role': 'assistant', 'content': content_text or None}
            if tool_calls:
                result['tool_calls'] = tool_calls
            return result
        return {'role': msg.role, 'content': str(msg.content)}
    def _convert_response(self, chat_response: Any, request: AnthropicMessagesRequest, raw_request: Optional[Request]=None) -> Union[AnthropicMessagesResponse, ErrorResponse]:
        if isinstance(chat_response, ErrorResponse):
            return chat_response
        message_id = f'msg_{random_uuid()}'
        content_blocks = []
        if hasattr(chat_response, 'choices') and chat_response.choices:
            choice = chat_response.choices[0]
            message = choice.message
            if hasattr(message, 'reasoning_content') and message.reasoning_content:
                content_blocks.append(AnthropicThinkingBlock(thinking=message.reasoning_content))
            if message.content:
                content_blocks.append(AnthropicTextBlock(text=message.content))
            if hasattr(message, 'tool_calls') and message.tool_calls:
                for tool_call in message.tool_calls:
                    if hasattr(tool_call, 'function'):
                        try:
                            input_data = json.loads(tool_call.function.arguments)
                        except (json.JSONDecodeError, AttributeError):
                            input_data = {}
                        content_blocks.append(AnthropicToolUseBlock(id=tool_call.id, name=tool_call.function.name, input=input_data))
            stop_reason = 'end_turn'
            if choice.finish_reason == 'length':
                stop_reason = 'max_tokens'
            elif choice.finish_reason == 'stop':
                stop_reason = 'end_turn'
            elif choice.finish_reason == 'tool_calls':
                stop_reason = 'tool_use'
        usage = AnthropicUsage(input_tokens=chat_response.usage.prompt_tokens, output_tokens=chat_response.usage.completion_tokens)
        return AnthropicMessagesResponse(id=message_id, content=content_blocks or [AnthropicTextBlock(text='')], model=request.model, stop_reason=stop_reason, usage=usage)
    async def _convert_streaming_response(self, chat_stream: AsyncGenerator[str, None], request: AnthropicMessagesRequest, raw_request: Optional[Request]=None) -> AsyncGenerator[str, None]:
        message_id = f'msg_{random_uuid()}'
        content_index = 0
        current_text = ''
        total_input_tokens = 0
        total_output_tokens = 0
        first_chunk = True
        try:
            async for chunk_str in chat_stream:
                if not chunk_str.strip():
                    continue
                if chunk_str.startswith('data: '):
                    data_str = chunk_str[6:].strip()
                    if data_str == '[DONE]':
                        stop_event = AnthropicMessageStop()
                        yield f'data: {stop_event.model_dump_json()}\n\n'
                        return
                    try:
                        chunk_data = json.loads(data_str)
                    except json.JSONDecodeError:
                        continue
                    if first_chunk:
                        first_chunk = False
                        initial_message = AnthropicMessagesResponse(id=message_id, content=[], model=request.model, stop_reason=None, usage=AnthropicUsage(input_tokens=0, output_tokens=0))
                        start_event = AnthropicMessageStart(message=initial_message)
                        yield f'data: {start_event.model_dump_json()}\n\n'
                    if 'choices' in chunk_data and chunk_data['choices']:
                        choice = chunk_data['choices'][0]
                        delta = choice.get('delta', {})
                        if 'content' in delta and delta['content']:
                            if not current_text:
                                content_start = AnthropicContentBlockStart(index=content_index, content_block=AnthropicTextBlock(text=''))
                                yield f'data: {content_start.model_dump_json()}\n\n'
                            content_delta = AnthropicContentBlockDelta(index=content_index, delta={'type': 'text_delta', 'text': delta['content']})
                            yield f'data: {content_delta.model_dump_json()}\n\n'
                            current_text += delta['content']
                        if 'tool_calls' in delta and delta['tool_calls']:
                            pass
                        if choice.get('finish_reason'):
                            if current_text:
                                content_stop = AnthropicContentBlockStop(index=content_index)
                                yield f'data: {content_stop.model_dump_json()}\n\n'
                    if 'usage' in chunk_data:
                        usage = chunk_data['usage']
                        total_input_tokens = usage.get('prompt_tokens', 0)
                        total_output_tokens = usage.get('completion_tokens', 0)
                        message_delta = AnthropicMessageDelta(delta={}, usage=AnthropicUsage(input_tokens=total_input_tokens, output_tokens=total_output_tokens))
                        yield f'data: {message_delta.model_dump_json()}\n\n'
        except Exception as e:
            error_event = AnthropicError(error={'type': 'api_error', 'message': str(e)})
            yield f'data: {error_event.model_dump_json()}\n\n'
        stop_event = AnthropicMessageStop()
        yield f'data: {stop_event.model_dump_json()}\n\n'