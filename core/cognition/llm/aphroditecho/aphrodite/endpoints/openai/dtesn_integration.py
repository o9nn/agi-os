import logging
import time
from typing import Any, Dict, Optional, Union
from collections.abc import AsyncGenerator
from fastapi import Request
from pydantic import BaseModel, Field
from aphrodite.common.config import ModelConfig
from aphrodite.endpoints.logger import RequestLogger
from aphrodite.endpoints.openai.protocol import ChatCompletionRequest, ChatCompletionResponse, CompletionRequest, CompletionResponse, ErrorResponse
from aphrodite.endpoints.openai.serving_engine import OpenAIServing
from aphrodite.endpoints.openai.serving_models import OpenAIServingModels
from aphrodite.engine.protocol import EngineClient
logger = logging.getLogger(__name__)
try:
    from aphrodite.endpoints.deep_tree_echo.dtesn_processor import DTESNProcessor
    from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig
    DTESN_AVAILABLE = True
    logger.info('DTESN components successfully imported')
except ImportError as e:
    logger.warning(f'DTESN components not available: {e}')
    DTESN_AVAILABLE = False
class DTESNEnhancedRequest(BaseModel):
    enable_dtesn: bool = Field(default=False, description='Enable DTESN processing')
    dtesn_membrane_depth: int = Field(default=4, ge=1, le=16, description='DTESN membrane depth')
    dtesn_esn_size: int = Field(default=512, ge=32, le=4096, description='DTESN ESN reservoir size')
    dtesn_processing_mode: str = Field(default='server_side', description='DTESN processing mode')
    enable_caching: bool = Field(default=True, description='Enable server-side caching')
    cache_strategy: str = Field(default='balanced', description='Caching strategy (aggressive/balanced/conservative/dynamic)')
    cache_ttl_seconds: Optional[int] = Field(default=None, description='Custom TTL for cache entries')
    invalidation_tags: Optional[Set[str]] = Field(default=None, description='Tags for content-based cache invalidation')
class DTESNIntegrationMixin:
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.dtesn_processor: Optional[DTESNProcessor] = None
        if DTESN_AVAILABLE:
            try:
                dtesn_config = DTESNConfig()
                engine = getattr(self, 'engine_client', None)
                self.dtesn_processor = DTESNProcessor(config=dtesn_config, engine=engine)
                logger.info('DTESN processor initialized successfully')
            except Exception as e:
                logger.warning(f'Could not initialize DTESN processor: {e}')
                self.dtesn_processor = None
        else:
            logger.info('DTESN processor not available - continuing without DTESN capabilities')
    def is_dtesn_available(self) -> bool:
        return self.dtesn_processor is not None
    async def _preprocess_with_dtesn(self, request_data: Union[Dict[str, Any], str], dtesn_options: Optional[DTESNEnhancedRequest]=None, model_id: str='default') -> Dict[str, Any]:
        result = {'original_data': request_data, 'dtesn_processed': False, 'dtesn_result': None, 'processing_metadata': {}, 'cache_metadata': {'cache_hit': False, 'cache_enabled': False, 'performance_improvement': 0.0}}
        if not (dtesn_options and dtesn_options.enable_dtesn and self.is_dtesn_available()):
            return result
        try:
            input_text = self._extract_text_for_dtesn(request_data)
            if not input_text:
                logger.warning('No text found for DTESN processing')
                return result
            dtesn_config = {'membrane_depth': dtesn_options.dtesn_membrane_depth, 'esn_size': dtesn_options.dtesn_esn_size, 'processing_mode': dtesn_options.dtesn_processing_mode}
            cache_manager = get_cache_manager()
            cached_result = None
            if dtesn_options.enable_caching and cache_manager:
                result['cache_metadata']['cache_enabled'] = True
                cache_start = time.time()
                cached_result = await cache_manager.get_cached_result(input_data=input_text, model_id=model_id, dtesn_config=dtesn_config)
                if cached_result:
                    cache_retrieval_time = (time.time() - cache_start) * 1000
                    cached_data, cached_metadata = cached_result
                    result.update({'dtesn_processed': True, 'dtesn_result': cached_data, 'processing_metadata': cached_metadata, 'cache_metadata': {'cache_hit': True, 'cache_enabled': True, 'cache_retrieval_time_ms': cache_retrieval_time, 'original_processing_time_ms': cached_metadata.get('processing_time_ms', 0), 'performance_improvement': max(0.0, 1.0 - cache_retrieval_time / max(cached_metadata.get('processing_time_ms', cache_retrieval_time), 1.0))}})
                    logger.info(f"DTESN cache hit - retrieved in {cache_retrieval_time:.2f}ms (vs {cached_metadata.get('processing_time_ms', 0):.2f}ms original)")
                    return result
            start_time = time.time()
            dtesn_result = await self.dtesn_processor.process(input_data=input_text, membrane_depth=dtesn_options.dtesn_membrane_depth, esn_size=dtesn_options.dtesn_esn_size)
            processing_time_ms = (time.time() - start_time) * 1000
            processing_metadata = {'processing_time_ms': processing_time_ms, 'membrane_depth': dtesn_options.dtesn_membrane_depth, 'esn_size': dtesn_options.dtesn_esn_size, 'processing_mode': dtesn_options.dtesn_processing_mode, 'cache_miss': cache_manager is not None and dtesn_options.enable_caching}
            dtesn_result_dict = dtesn_result.to_dict() if hasattr(dtesn_result, 'to_dict') else dtesn_result
            result.update({'dtesn_processed': True, 'dtesn_result': dtesn_result_dict, 'processing_metadata': processing_metadata})
            if dtesn_options.enable_caching and cache_manager:
                content_tags = dtesn_options.invalidation_tags or {'dtesn', f'model_{model_id}'}
                await cache_manager.cache_result(input_data=input_text, model_id=model_id, dtesn_config=dtesn_config, result=dtesn_result_dict, metadata=processing_metadata, processing_time_ms=processing_time_ms, content_tags=content_tags)
                logger.debug(f'Cached DTESN result with tags: {content_tags}')
            logger.info(f'DTESN preprocessing completed in {processing_time_ms:.2f}ms')
        except Exception as e:
            logger.error(f'DTESN preprocessing failed: {e}')
            result['processing_metadata']['error'] = str(e)
        return result
    def _extract_text_for_dtesn(self, request_data: Union[Dict[str, Any], str]) -> Optional[str]:
        if isinstance(request_data, str):
            return request_data
        if isinstance(request_data, dict):
            if 'messages' in request_data:
                messages = request_data['messages']
                if isinstance(messages, list) and messages:
                    for msg in reversed(messages):
                        if isinstance(msg, dict) and msg.get('role') == 'user':
                            content = msg.get('content')
                            if isinstance(content, str):
                                return content
                            elif isinstance(content, list):
                                text_parts = []
                                for part in content:
                                    if isinstance(part, dict) and part.get('type') == 'text':
                                        text_parts.append(part.get('text', ''))
                                return '\n'.join(text_parts) if text_parts else None
            if 'prompt' in request_data:
                prompt = request_data['prompt']
                if isinstance(prompt, str):
                    return prompt
                elif isinstance(prompt, list) and prompt:
                    return str(prompt[0]) if prompt else None
        return None
    async def _enhance_response_with_dtesn(self, response: Union[ChatCompletionResponse, CompletionResponse], dtesn_result: Optional[Dict[str, Any]]=None) -> Union[ChatCompletionResponse, CompletionResponse]:
        if not dtesn_result or not dtesn_result.get('dtesn_processed'):
            return response
        if hasattr(response, '__dict__'):
            dtesn_metadata = {'dtesn_processed': True, 'dtesn_membrane_layers': dtesn_result.get('dtesn_result', {}).get('membrane_layers', 0), 'dtesn_processing_time_ms': dtesn_result.get('processing_metadata', {}).get('processing_time_ms', 0), 'dtesn_server_rendered': True}
            response.__dict__['dtesn_metadata'] = dtesn_metadata
        return response
class DTESNEnhancedOpenAIServingChat(DTESNIntegrationMixin, OpenAIServing):
    def __init__(self, engine_client: EngineClient, model_config: ModelConfig, models: OpenAIServingModels, response_role: str='assistant', *, request_logger: Optional[RequestLogger]=None, **kwargs):
        super().__init__(engine_client=engine_client, model_config=model_config, models=models, request_logger=request_logger, **kwargs)
        self.response_role = response_role
    async def create_chat_completion_with_dtesn(self, request: ChatCompletionRequest, raw_request: Optional[Request]=None, dtesn_options: Optional[DTESNEnhancedRequest]=None) -> Union[AsyncGenerator[str, None], ChatCompletionResponse, ErrorResponse]:
        try:
            dtesn_result = await self._preprocess_with_dtesn(request_data=request.dict(), dtesn_options=dtesn_options)
            logger.info(f"Chat completion with DTESN processing: {dtesn_result['dtesn_processed']}")
            return await self._create_dtesn_enhanced_response(request, dtesn_result)
        except Exception as e:
            logger.error(f'DTESN-enhanced chat completion failed: {e}')
            return ErrorResponse(message=f'DTESN-enhanced processing failed: {e}', type='dtesn_processing_error', code=500)
    async def _create_dtesn_enhanced_response(self, request: ChatCompletionRequest, dtesn_result: Dict[str, Any]) -> ChatCompletionResponse:
        from aphrodite.endpoints.openai.protocol import ChatCompletionResponseChoice, ChatMessage, UsageInfo
        content = 'DTESN-processed response'
        if dtesn_result['dtesn_processed']:
            dtesn_data = dtesn_result['dtesn_result']
            content = f"DTESN processed: {dtesn_data.get('final_result', 'completed')}"
        choice = ChatCompletionResponseChoice(index=0, message=ChatMessage(role='assistant', content=content), finish_reason='stop')
        response = ChatCompletionResponse(id=f'chatcmpl-dtesn-{int(time.time())}', choices=[choice], created=int(time.time()), model=request.model, usage=UsageInfo(prompt_tokens=len(str(request.messages)), completion_tokens=len(content), total_tokens=len(str(request.messages)) + len(content)))
        return await self._enhance_response_with_dtesn(response, dtesn_result)
class DTESNEnhancedOpenAIServingCompletion(DTESNIntegrationMixin, OpenAIServing):
    def __init__(self, engine_client: EngineClient, model_config: ModelConfig, models: OpenAIServingModels, *, request_logger: Optional[RequestLogger]=None, **kwargs):
        super().__init__(engine_client=engine_client, model_config=model_config, models=models, request_logger=request_logger, **kwargs)
    async def create_completion_with_dtesn(self, request: CompletionRequest, raw_request: Optional[Request]=None, dtesn_options: Optional[DTESNEnhancedRequest]=None) -> Union[AsyncGenerator[str, None], CompletionResponse, ErrorResponse]:
        try:
            dtesn_result = await self._preprocess_with_dtesn(request_data=request.dict(), dtesn_options=dtesn_options)
            logger.info(f"Completion with DTESN processing: {dtesn_result['dtesn_processed']}")
            return await self._create_dtesn_enhanced_completion_response(request, dtesn_result)
        except Exception as e:
            logger.error(f'DTESN-enhanced completion failed: {e}')
            return ErrorResponse(message=f'DTESN-enhanced processing failed: {e}', type='dtesn_processing_error', code=500)
    async def _create_dtesn_enhanced_completion_response(self, request: CompletionRequest, dtesn_result: Dict[str, Any]) -> CompletionResponse:
        from aphrodite.endpoints.openai.protocol import CompletionResponseChoice, UsageInfo
        text = 'DTESN-processed completion'
        if dtesn_result['dtesn_processed']:
            dtesn_data = dtesn_result['dtesn_result']
            text = f"DTESN processed: {dtesn_data.get('final_result', 'completed')}"
        choice = CompletionResponseChoice(index=0, text=text, finish_reason='stop')
        response = CompletionResponse(id=f'cmpl-dtesn-{int(time.time())}', choices=[choice], created=int(time.time()), model=request.model, usage=UsageInfo(prompt_tokens=len(str(request.prompt)), completion_tokens=len(text), total_tokens=len(str(request.prompt)) + len(text)))
        return await self._enhance_response_with_dtesn(response, dtesn_result)
def create_dtesn_enhanced_chat_serving(engine_client: EngineClient, model_config: ModelConfig, models: OpenAIServingModels, **kwargs) -> DTESNEnhancedOpenAIServingChat:
    return DTESNEnhancedOpenAIServingChat(engine_client=engine_client, model_config=model_config, models=models, **kwargs)
def create_dtesn_enhanced_completion_serving(engine_client: EngineClient, model_config: ModelConfig, models: OpenAIServingModels, **kwargs) -> DTESNEnhancedOpenAIServingCompletion:
    return DTESNEnhancedOpenAIServingCompletion(engine_client=engine_client, model_config=model_config, models=models, **kwargs)
def is_dtesn_request(request_data: Dict[str, Any]) -> bool:
    return request_data.get('enable_dtesn', False) or request_data.get('dtesn_enhance', False)
def extract_dtesn_options(request_data: Dict[str, Any]) -> Optional[DTESNEnhancedRequest]:
    if not is_dtesn_request(request_data):
        return None
    return DTESNEnhancedRequest(enable_dtesn=request_data.get('enable_dtesn', False), dtesn_membrane_depth=request_data.get('dtesn_membrane_depth', 4), dtesn_esn_size=request_data.get('dtesn_esn_size', 512), dtesn_processing_mode=request_data.get('dtesn_processing_mode', 'server_side'))