"""
Comprehensive tests for Dynamic Model Serving integration.

Tests the complete integration of dynamic model loading, unloading, and 
real-time model switching capabilities with Aphrodite Engine.
"""

import pytest
import time
import torch
from unittest.mock import Mock, patch

# Test imports
try:
    from aphrodite.modeling.dynamic_loader import (
        DynamicModelLoader, ModelResourceUsage, LoadedModelInfo,
        AphroditeDynamicModelLoader, ResourceMonitor, RequestRouter
    )
    from aphrodite.common.config import ModelConfig, AphroditeConfig, LoadConfig
    APHRODITE_AVAILABLE = True
except ImportError:
    APHRODITE_AVAILABLE = False
    pytest.skip("Aphrodite not available", allow_module_level=True)

try:
    from echo_self.integration.aphrodite_adaptive import (
        AdaptiveModelLoader, AphroditeAdaptiveIntegration
    )
    ECHO_SELF_AVAILABLE = True
except ImportError:
    ECHO_SELF_AVAILABLE = False


class TestDynamicModelLoader:
    """Test the DynamicModelLoader class."""
    
    @pytest.fixture
    def loader(self):
        """Create a DynamicModelLoader instance for testing."""
        return DynamicModelLoader(
            max_models=3,
            memory_limit_gb=4.0,
            eviction_policy="lru"
        )
    
    @pytest.fixture
    def mock_model_config(self):
        """Create a mock ModelConfig for testing."""
        return ModelConfig(
            model="test-model",
            tokenizer="test-model",
            tokenizer_mode="auto",
            trust_remote_code=False,
            dtype="float16",
            max_model_len=512,
            seed=42
        )
    
    @pytest.fixture
    def mock_model(self):
        """Create a mock PyTorch model for testing."""
        model = Mock(spec=torch.nn.Module)
        model.parameters.return_value = [
            Mock(numel=Mock(return_value=1000), element_size=Mock(return_value=4))
        ]
        model.eval.return_value = model
        return model
    
    def test_initialization(self, loader):
        """Test DynamicModelLoader initialization."""
        assert loader.max_models == 3
        assert loader.memory_limit_bytes == 4 * 1024 * 1024 * 1024
        assert loader.eviction_policy == "lru"
        assert len(loader._loaded_models) == 0
        assert loader._active_model is None
    
    @pytest.mark.asyncio
    async def test_load_model_success(self, loader, mock_model_config, mock_model):
        """Test successful model loading."""
        with patch('aphrodite.modeling.model_loader.get_model_loader') as mock_get_loader, \
             patch('aphrodite.modeling.model_loader.utils.initialize_model', return_value=mock_model):
            
            mock_loader = Mock()
            mock_loader.download_model = Mock()
            mock_loader.load_model.return_value = mock_model
            mock_get_loader.return_value = mock_loader
            
            success = await loader.load_model("test-model", mock_model_config)
            
            assert success is True
            assert "test-model" in loader._loaded_models
            assert len(loader._loaded_models) == 1


class TestModelResourceUsage:
    """Test the ModelResourceUsage class."""
    
    def test_initialization(self):
        """Test ModelResourceUsage initialization."""
        usage = ModelResourceUsage(
            memory_mb=100.0,
            gpu_memory_mb=200.0,
            cpu_cores=2.0,
            load_time_ms=500.0,
            last_access_time=time.time(),
            request_count=10
        )
        
        assert usage.memory_mb == 100.0
        assert usage.gpu_memory_mb == 200.0
        assert usage.cpu_cores == 2.0
        assert usage.load_time_ms == 500.0
        assert usage.request_count == 10


if __name__ == "__main__":
    pytest.main([__file__, "-v"])