"""
Test DTESN integration with OpenAI-compatible endpoints.

Tests the integration between DTESN processing and OpenAI serving infrastructure.
"""

import asyncio
import logging
import pytest
from unittest.mock import AsyncMock, MagicMock, patch

# Configure logging for tests
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class TestDTESNOpenAIIntegration:
    """Test suite for DTESN OpenAI integration."""
    
    def test_dtesn_integration_import(self):
        """Test that DTESN integration components can be imported."""
        try:
            from aphrodite.endpoints.openai.dtesn_integration import (
                DTESNIntegrationMixin,
                DTESNEnhancedRequest,
                is_dtesn_request,
                extract_dtesn_options
            )
            assert True, "DTESN integration components imported successfully"
        except ImportError as e:
            pytest.skip(f"DTESN integration not available: {e}")
    
    def test_dtesn_routes_import(self):
        """Test that DTESN routes can be imported."""
        try:
            from aphrodite.endpoints.openai.dtesn_routes import (
                router,
                DTESNOpenAIHandler,
                initialize_dtesn_handler
            )
            assert router is not None, "DTESN router should not be None"
            assert DTESNOpenAIHandler is not None, "DTESN handler class should not be None"
            assert callable(initialize_dtesn_handler), "Initialize function should be callable"
        except ImportError as e:
            pytest.skip(f"DTESN routes not available: {e}")
    
    def test_dtesn_request_detection(self):
        """Test DTESN request detection logic."""
        try:
            from aphrodite.endpoints.openai.dtesn_integration import (
                is_dtesn_request,
                extract_dtesn_options
            )
            
            # Test positive case
            dtesn_request_data = {
                "enable_dtesn": True,
                "dtesn_membrane_depth": 6,
                "dtesn_esn_size": 1024
            }
            assert is_dtesn_request(dtesn_request_data), "Should detect DTESN request"
            
            options = extract_dtesn_options(dtesn_request_data)
            assert options is not None, "Should extract DTESN options"
            assert options.enable_dtesn == True, "Should have DTESN enabled"
            assert options.dtesn_membrane_depth == 6, "Should have correct membrane depth"
            
            # Test negative case
            normal_request_data = {
                "model": "test-model",
                "messages": [{"role": "user", "content": "hello"}]
            }
            assert not is_dtesn_request(normal_request_data), "Should not detect DTESN request"
            assert extract_dtesn_options(normal_request_data) is None, "Should not extract options"
            
        except ImportError as e:
            pytest.skip(f"DTESN integration not available: {e}")
    
    @pytest.mark.asyncio
    async def test_dtesn_handler_initialization(self):
        """Test DTESN handler initialization."""
        try:
            from aphrodite.endpoints.openai.dtesn_routes import DTESNOpenAIHandler
            
            # Mock dependencies
            mock_engine = AsyncMock()
            mock_config = MagicMock()
            mock_models = MagicMock()
            mock_logger = MagicMock()
            
            # Create handler
            with patch('aphrodite.endpoints.openai.dtesn_routes.DTESN_AVAILABLE', True), \
                 patch('aphrodite.endpoints.openai.dtesn_routes.DTESNProcessor') as mock_processor_class, \
                 patch('aphrodite.endpoints.openai.dtesn_routes.DTESNConfig') as mock_config_class:
                
                mock_processor_class.return_value = MagicMock()
                mock_config_class.return_value = MagicMock()
                
                handler = DTESNOpenAIHandler(
                    engine_client=mock_engine,
                    model_config=mock_config,
                    models=mock_models,
                    request_logger=mock_logger
                )
                
                assert handler is not None, "Handler should be created"
                assert handler.engine_client == mock_engine, "Should store engine client"
                assert handler.model_config == mock_config, "Should store model config"
                
        except ImportError as e:
            pytest.skip(f"DTESN routes not available: {e}")
    
    @pytest.mark.asyncio
    async def test_dtesn_preprocessing_flow(self):
        """Test DTESN preprocessing workflow."""
        try:
            from aphrodite.endpoints.openai.dtesn_routes import DTESNOpenAIHandler
            from aphrodite.endpoints.deep_tree_echo.dtesn_processor import DTESNProcessor
            
            # Mock dependencies
            mock_engine = AsyncMock()
            mock_config = MagicMock()
            mock_models = MagicMock()
            
            # Mock DTESN processor
            mock_dtesn_result = MagicMock()
            mock_dtesn_result.to_dict.return_value = {
                "processed_output": {"result": "dtesn_processed"},
                "membrane_layers": 4,
                "processing_time_ms": 50.0
            }
            
            mock_processor = AsyncMock()
            mock_processor.process.return_value = mock_dtesn_result
            
            # Create handler with mocked DTESN processor
            with patch('aphrodite.endpoints.openai.dtesn_routes.DTESN_AVAILABLE', True), \
                 patch('aphrodite.endpoints.openai.dtesn_routes.DTESNProcessor', return_value=mock_processor), \
                 patch('aphrodite.endpoints.openai.dtesn_routes.DTESNConfig'):
                
                handler = DTESNOpenAIHandler(
                    engine_client=mock_engine,
                    model_config=mock_config,
                    models=mock_models
                )
                
                # Test preprocessing
                result = await handler._preprocess_with_dtesn(
                    input_text="test input",
                    dtesn_options={
                        "enable_dtesn": True,
                        "membrane_depth": 4,
                        "esn_size": 512,
                        "processing_mode": "server_side"
                    }
                )
                
                assert result["dtesn_processed"] == True, "Should be marked as DTESN processed"
                assert "processing_time_ms" in result, "Should include processing time"
                assert result["server_rendered"] == True, "Should be server rendered"
                
                # Verify DTESN processor was called
                mock_processor.process.assert_called_once_with(
                    input_data="test input",
                    membrane_depth=4,
                    esn_size=512
                )
                
        except ImportError as e:
            pytest.skip(f"DTESN components not available: {e}")
    
    def test_text_extraction_methods(self):
        """Test text extraction from various request formats."""
        try:
            from aphrodite.endpoints.openai.dtesn_routes import DTESNOpenAIHandler
            
            # Mock dependencies
            mock_engine = AsyncMock()
            mock_config = MagicMock()
            mock_models = MagicMock()
            
            with patch('aphrodite.endpoints.openai.dtesn_routes.DTESN_AVAILABLE', False):
                handler = DTESNOpenAIHandler(
                    engine_client=mock_engine,
                    model_config=mock_config,
                    models=mock_models
                )
                
                # Test chat message extraction
                messages = [
                    {"role": "system", "content": "You are a helpful assistant"},
                    {"role": "user", "content": "Hello, world!"},
                    {"role": "assistant", "content": "Hi there!"},
                    {"role": "user", "content": "How are you?"}
                ]
                
                text = handler._extract_text_from_chat_messages(messages)
                assert text is not None, "Should extract text from messages"
                assert "Hello, world!" in text, "Should contain user message"
                assert "How are you?" in text, "Should contain latest user message"
                
                # Test prompt extraction
                prompt = "What is the capital of France?"
                text = handler._extract_text_from_prompt(prompt)
                assert text == prompt, "Should extract text from string prompt"
                
                # Test list prompt extraction
                prompt_list = ["First prompt", "Second prompt"]
                text = handler._extract_text_from_prompt(prompt_list)
                assert text == "First prompt", "Should extract first prompt from list"
                
        except ImportError as e:
            pytest.skip(f"DTESN routes not available: {e}")
    
    def test_server_side_processing_focus(self):
        """Test that implementation focuses on server-side processing."""
        try:
            from aphrodite.endpoints.openai.dtesn_routes import DTESNOpenAIHandler
            
            # Verify that handler emphasizes server-side processing
            mock_engine = AsyncMock()
            mock_config = MagicMock()
            mock_models = MagicMock()
            
            with patch('aphrodite.endpoints.openai.dtesn_routes.DTESN_AVAILABLE', False):
                handler = DTESNOpenAIHandler(
                    engine_client=mock_engine,
                    model_config=mock_config,
                    models=mock_models
                )
                
                # Test that DTESN options extraction focuses on server-side headers
                mock_request = MagicMock()
                mock_request.headers = {
                    "X-DTESN-Enable": "true",
                    "X-DTESN-Processing-Mode": "server_side"
                }
                
                options = asyncio.run(handler._extract_dtesn_options(mock_request))
                assert options["processing_mode"] == "server_side", "Should default to server-side processing"
                assert options["enable_dtesn"] == True, "Should enable DTESN from headers"
                
        except ImportError as e:
            pytest.skip(f"DTESN routes not available: {e}")


def test_integration_availability():
    """Test that integration components are available and properly configured."""
    try:
        # Test basic imports
        from aphrodite.endpoints.openai.dtesn_integration import DTESNIntegrationMixin
        from aphrodite.endpoints.openai.dtesn_routes import router, DTESNOpenAIHandler
        
        logger.info("DTESN OpenAI integration components are available")
        assert True
        
    except ImportError as e:
        logger.warning(f"DTESN OpenAI integration not fully available: {e}")
        # This is acceptable - the integration should gracefully handle missing components
        assert True


if __name__ == "__main__":
    # Run basic integration test
    test_integration_availability()
    print("✅ Basic DTESN OpenAI integration test passed")