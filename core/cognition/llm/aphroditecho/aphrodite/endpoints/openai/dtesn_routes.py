"""
DTESN-enhanced OpenAI-compatible route handlers.

Provides server-side route handlers that integrate DTESN processing with OpenAI
endpoints while maintaining full API compatibility.
"""

import json
import logging
import time
from typing import Any, Dict, Optional, Union
from collections.abc import AsyncGenerator

from fastapi import APIRouter, Depends, HTTPException, Request, Response
from fastapi.responses import StreamingResponse

from aphrodite.common.config import ModelConfig
from aphrodite.endpoints.logger import RequestLogger
from aphrodite.endpoints.openai.protocol import (
    ChatCompletionRequest,
    ChatCompletionResponse,
    CompletionRequest,
    CompletionResponse,
    ErrorResponse
)
from aphrodite.endpoints.openai.serving_chat import OpenAIServingChat
from aphrodite.endpoints.openai.serving_completions import OpenAIServingCompletion
from aphrodite.endpoints.openai.serving_models import OpenAIServingModels
from aphrodite.engine.protocol import EngineClient

logger = logging.getLogger(__name__)

# Import DTESN components
try:
    from aphrodite.endpoints.deep_tree_echo.dtesn_processor import DTESNProcessor
    from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig
    DTESN_AVAILABLE = True
    logger.info("DTESN components available for OpenAI route integration")
except ImportError as e:
    logger.warning(f"DTESN components not available: {e}")
    DTESN_AVAILABLE = False

# Create router for DTESN-enhanced OpenAI endpoints
router = APIRouter(tags=["dtesn_openai"])


class DTESNOpenAIHandler:
    """Handler class for DTESN-enhanced OpenAI endpoints."""
    
    def __init__(
        self,
        engine_client: EngineClient,
        model_config: ModelConfig,
        models: OpenAIServingModels,
        request_logger: Optional[RequestLogger] = None
    ):
        self.engine_client = engine_client
        self.model_config = model_config
        self.models = models
        self.request_logger = request_logger
        
        # Initialize standard OpenAI serving classes
        self.chat_serving = OpenAIServingChat(
            engine_client=engine_client,
            model_config=model_config,
            models=models,
            response_role="assistant",
            request_logger=request_logger
        )
        
        self.completion_serving = OpenAIServingCompletion(
            engine_client=engine_client,
            model_config=model_config,
            models=models,
            request_logger=request_logger
        )
        
        # Initialize DTESN processor if available
        self.dtesn_processor: Optional[DTESNProcessor] = None
        if DTESN_AVAILABLE:
            try:
                dtesn_config = DTESNConfig()
                self.dtesn_processor = DTESNProcessor(
                    config=dtesn_config, 
                    engine=engine_client
                )
                logger.info("DTESN processor initialized for OpenAI integration")
            except Exception as e:
                logger.warning(f"Could not initialize DTESN processor: {e}")
                self.dtesn_processor = None
    
    def is_dtesn_available(self) -> bool:
        """Check if DTESN processing is available."""
        return self.dtesn_processor is not None
    
    async def _extract_dtesn_options(self, request: Request) -> Dict[str, Any]:
        """Extract DTESN processing options from request."""
        dtesn_options = {
            "enable_dtesn": False,
            "membrane_depth": 4,
            "esn_size": 512,
            "processing_mode": "server_side"
        }
        
        try:
            # Check for DTESN options in headers
            if request.headers.get("X-DTESN-Enable", "").lower() == "true":
                dtesn_options["enable_dtesn"] = True
                dtesn_options["membrane_depth"] = int(
                    request.headers.get("X-DTESN-Membrane-Depth", "4")
                )
                dtesn_options["esn_size"] = int(
                    request.headers.get("X-DTESN-ESN-Size", "512")
                )
                dtesn_options["processing_mode"] = request.headers.get(
                    "X-DTESN-Processing-Mode", "server_side"
                )
                
        except Exception as e:
            logger.debug(f"Could not parse DTESN options from headers: {e}")
        
        return dtesn_options
    
    async def _preprocess_with_dtesn(
        self,
        input_text: str,
        dtesn_options: Dict[str, Any]
    ) -> Dict[str, Any]:
        """Preprocess input through DTESN if enabled."""
        if not (dtesn_options["enable_dtesn"] and self.is_dtesn_available()):
            return {
                "dtesn_processed": False,
                "original_text": input_text,
                "enhanced_text": input_text
            }
        
        try:
            start_time = time.time()
            dtesn_result = await self.dtesn_processor.process(
                input_data=input_text,
                membrane_depth=dtesn_options["membrane_depth"],
                esn_size=dtesn_options["esn_size"]
            )
            
            processing_time = (time.time() - start_time) * 1000
            
            # Extract processed content for use in generation
            enhanced_text = input_text  # Could be enhanced based on DTESN result
            
            logger.info(f"DTESN preprocessing completed in {processing_time:.2f}ms")
            
            return {
                "dtesn_processed": True,
                "original_text": input_text,
                "enhanced_text": enhanced_text,
                "dtesn_result": dtesn_result.to_dict(),
                "processing_time_ms": processing_time,
                "server_rendered": True
            }
            
        except Exception as e:
            logger.error(f"DTESN preprocessing failed: {e}")
            return {
                "dtesn_processed": False,
                "original_text": input_text,
                "enhanced_text": input_text,
                "error": str(e)
            }
    
    def _add_dtesn_headers(self, response: Response, dtesn_context: Dict[str, Any]):
        """Add DTESN processing headers to response."""
        if dtesn_context.get("dtesn_processed"):
            response.headers["X-DTESN-Processed"] = "true"
            response.headers["X-DTESN-Processing-Time"] = str(
                dtesn_context.get("processing_time_ms", 0)
            )
            response.headers["X-DTESN-Server-Rendered"] = "true"
        else:
            response.headers["X-DTESN-Processed"] = "false"
    
    async def chat_completion_with_dtesn(
        self,
        request: ChatCompletionRequest,
        raw_request: Request,
        response: Response
    ) -> Union[ChatCompletionResponse, ErrorResponse, StreamingResponse]:
        """
        Handle chat completion with optional DTESN preprocessing.
        
        Provides server-side DTESN processing integration while maintaining
        full compatibility with OpenAI Chat Completion API.
        """
        try:
            # Extract DTESN options
            dtesn_options = await self._extract_dtesn_options(raw_request)
            
            # Apply DTESN preprocessing if requested
            dtesn_context = {"dtesn_processed": False}
            if dtesn_options["enable_dtesn"]:
                # Extract text from messages for DTESN processing
                input_text = self._extract_text_from_chat_messages(request.messages)
                if input_text:
                    dtesn_context = await self._preprocess_with_dtesn(input_text, dtesn_options)
            
            # Process through standard chat completion
            result = await self.chat_serving.create_chat_completion(request, raw_request)
            
            # Add DTESN headers to response
            self._add_dtesn_headers(response, dtesn_context)
            
            # If result is a streaming response, we need to handle it specially
            if isinstance(result, AsyncGenerator):
                return StreamingResponse(
                    self._stream_with_dtesn_headers(result, dtesn_context),
                    media_type="text/event-stream"
                )
            
            # For non-streaming responses, enhance with DTESN metadata
            if isinstance(result, ChatCompletionResponse):
                # Add DTESN metadata to the response object
                if hasattr(result, '__dict__'):
                    result.__dict__['dtesn_metadata'] = {
                        "dtesn_processed": dtesn_context.get("dtesn_processed", False),
                        "processing_time_ms": dtesn_context.get("processing_time_ms", 0),
                        "server_rendered": True
                    }
            
            return result
            
        except Exception as e:
            logger.error(f"DTESN-enhanced chat completion failed: {e}")
            error_response = ErrorResponse(
                message=f"DTESN-enhanced processing failed: {e}",
                type="dtesn_processing_error",
                code=500
            )
            self._add_dtesn_headers(response, {"dtesn_processed": False, "error": str(e)})
            return error_response
    
    async def completion_with_dtesn(
        self,
        request: CompletionRequest,
        raw_request: Request,
        response: Response
    ) -> Union[CompletionResponse, ErrorResponse, StreamingResponse]:
        """
        Handle completion with optional DTESN preprocessing.
        
        Provides server-side DTESN processing integration while maintaining
        full compatibility with OpenAI Completion API.
        """
        try:
            # Extract DTESN options
            dtesn_options = await self._extract_dtesn_options(raw_request)
            
            # Apply DTESN preprocessing if requested
            dtesn_context = {"dtesn_processed": False}
            if dtesn_options["enable_dtesn"]:
                # Extract text from prompt for DTESN processing
                input_text = self._extract_text_from_prompt(request.prompt)
                if input_text:
                    dtesn_context = await self._preprocess_with_dtesn(input_text, dtesn_options)
            
            # Process through standard completion
            result = await self.completion_serving.create_completion(request, raw_request)
            
            # Add DTESN headers to response
            self._add_dtesn_headers(response, dtesn_context)
            
            # Handle streaming responses
            if isinstance(result, AsyncGenerator):
                return StreamingResponse(
                    self._stream_with_dtesn_headers(result, dtesn_context),
                    media_type="text/event-stream"
                )
            
            # For non-streaming responses, enhance with DTESN metadata
            if isinstance(result, CompletionResponse):
                if hasattr(result, '__dict__'):
                    result.__dict__['dtesn_metadata'] = {
                        "dtesn_processed": dtesn_context.get("dtesn_processed", False),
                        "processing_time_ms": dtesn_context.get("processing_time_ms", 0),
                        "server_rendered": True
                    }
            
            return result
            
        except Exception as e:
            logger.error(f"DTESN-enhanced completion failed: {e}")
            error_response = ErrorResponse(
                message=f"DTESN-enhanced processing failed: {e}",
                type="dtesn_processing_error",
                code=500
            )
            self._add_dtesn_headers(response, {"dtesn_processed": False, "error": str(e)})
            return error_response
    
    def _extract_text_from_chat_messages(self, messages) -> Optional[str]:
        """Extract text content from chat messages."""
        try:
            text_parts = []
            for message in messages:
                if hasattr(message, 'content'):
                    content = message.content
                elif isinstance(message, dict):
                    content = message.get('content', '')
                else:
                    continue
                
                if isinstance(content, str):
                    text_parts.append(content)
                elif isinstance(content, list):
                    for part in content:
                        if isinstance(part, dict) and part.get('type') == 'text':
                            text_parts.append(part.get('text', ''))
            
            return "\n".join(text_parts) if text_parts else None
        except Exception as e:
            logger.debug(f"Could not extract text from messages: {e}")
            return None
    
    def _extract_text_from_prompt(self, prompt) -> Optional[str]:
        """Extract text content from completion prompt."""
        try:
            if isinstance(prompt, str):
                return prompt
            elif isinstance(prompt, list) and prompt:
                # Handle list of prompts by using the first one
                return str(prompt[0]) if prompt[0] else None
            return None
        except Exception as e:
            logger.debug(f"Could not extract text from prompt: {e}")
            return None
    
    async def _stream_with_dtesn_headers(
        self, 
        stream: AsyncGenerator[str, None],
        dtesn_context: Dict[str, Any]
    ) -> AsyncGenerator[str, None]:
        """Stream response with DTESN metadata injected."""
        # First yield DTESN metadata if processed
        if dtesn_context.get("dtesn_processed"):
            metadata = {
                "dtesn_processed": True,
                "processing_time_ms": dtesn_context.get("processing_time_ms", 0),
                "server_rendered": True
            }
            yield f"data: {json.dumps({'dtesn_metadata': metadata})}\n\n"
        
        # Then yield the original stream
        async for chunk in stream:
            yield chunk


# Global handler instance (will be initialized by the API server)
dtesn_handler: Optional[DTESNOpenAIHandler] = None


def get_dtesn_handler() -> DTESNOpenAIHandler:
    """Get the global DTESN handler instance."""
    if dtesn_handler is None:
        raise HTTPException(
            status_code=503,
            detail="DTESN handler not initialized"
        )
    return dtesn_handler


def initialize_dtesn_handler(
    engine_client: EngineClient,
    model_config: ModelConfig,
    models: OpenAIServingModels,
    request_logger: Optional[RequestLogger] = None
):
    """Initialize the global DTESN handler."""
    global dtesn_handler
    dtesn_handler = DTESNOpenAIHandler(
        engine_client=engine_client,
        model_config=model_config,
        models=models,
        request_logger=request_logger
    )


@router.post("/v1/chat/completions/dtesn")
async def create_chat_completion_with_dtesn(
    request: ChatCompletionRequest,
    raw_request: Request,
    response: Response,
    handler: DTESNOpenAIHandler = Depends(get_dtesn_handler)
) -> Union[ChatCompletionResponse, ErrorResponse]:
    """
    Create chat completion with DTESN preprocessing.
    
    Compatible with OpenAI Chat Completion API with additional DTESN capabilities.
    Enable DTESN processing by including the header: X-DTESN-Enable: true
    
    Additional headers:
    - X-DTESN-Membrane-Depth: Membrane hierarchy depth (default: 4)
    - X-DTESN-ESN-Size: Echo State Network reservoir size (default: 512)
    - X-DTESN-Processing-Mode: Processing mode (default: server_side)
    """
    return await handler.chat_completion_with_dtesn(request, raw_request, response)


@router.post("/v1/completions/dtesn")
async def create_completion_with_dtesn(
    request: CompletionRequest,
    raw_request: Request,
    response: Response,
    handler: DTESNOpenAIHandler = Depends(get_dtesn_handler)
) -> Union[CompletionResponse, ErrorResponse]:
    """
    Create completion with DTESN preprocessing.
    
    Compatible with OpenAI Completion API with additional DTESN capabilities.
    Enable DTESN processing by including the header: X-DTESN-Enable: true
    
    Additional headers:
    - X-DTESN-Membrane-Depth: Membrane hierarchy depth (default: 4)
    - X-DTESN-ESN-Size: Echo State Network reservoir size (default: 512)
    - X-DTESN-Processing-Mode: Processing mode (default: server_side)
    """
    return await handler.completion_with_dtesn(request, raw_request, response)


@router.get("/v1/dtesn/status")
async def dtesn_status(
    handler: DTESNOpenAIHandler = Depends(get_dtesn_handler)
) -> Dict[str, Any]:
    """Get DTESN integration status for OpenAI endpoints."""
    return {
        "dtesn_available": handler.is_dtesn_available(),
        "integration_type": "openai_compatible",
        "endpoints": [
            "/v1/chat/completions/dtesn",
            "/v1/completions/dtesn"
        ],
        "server_rendered": True,
        "processing_capabilities": {
            "membrane_computing": True,
            "echo_state_networks": True,
            "bseries_computation": True,
            "server_side_processing": True
        }
    }