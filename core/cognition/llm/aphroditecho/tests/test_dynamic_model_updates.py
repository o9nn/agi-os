"""
Tests for Dynamic Model Updates functionality.

Tests online parameter updates, incremental learning, model versioning,
and rollback capabilities with zero service interruption.
"""

import pytest
import tempfile
import torch
from unittest.mock import MagicMock, AsyncMock

from aphrodite.dynamic_model_manager import (
    DynamicModelManager,
    IncrementalUpdateRequest,
    DynamicUpdateConfig,
    ModelVersion
)
from aphrodite.endpoints.openai.serving_dynamic_updates import (
    OpenAIServingDynamicUpdates
)
from aphrodite.endpoints.openai.protocol import (
    IncrementalUpdateRequest as APIIncrementalUpdateRequest,
    ModelVersionRequest,
    ModelRollbackRequest
)
from aphrodite.common.config import ModelConfig, LoRAConfig
from aphrodite.engine.protocol import EngineClient


@pytest.fixture
def mock_engine_client():
    """Mock engine client for testing."""
    client = MagicMock(spec=EngineClient)
    client.get_model_parameters = AsyncMock(return_value={
        "timestamp": 1000.0,
        "parameter_count": 1000000,
        "model_state": {"layer1.weight": torch.randn(10, 10)}
    })
    return client


@pytest.fixture
def mock_model_config():
    """Mock model config for testing."""
    config = MagicMock(spec=ModelConfig)
    config.model = "test-model"
    config.max_model_len = 2048
    return config


@pytest.fixture
def mock_lora_config():
    """Mock LoRA config for testing."""
    config = MagicMock(spec=LoRAConfig)
    config.max_loras = 10
    config.max_lora_rank = 64
    return config


@pytest.fixture
def dynamic_config():
    """Dynamic update configuration for testing."""
    with tempfile.TemporaryDirectory() as tmpdir:
        yield DynamicUpdateConfig(
            max_versions=5,
            checkpoint_interval=10,
            auto_rollback_threshold=0.1,
            backup_dir=tmpdir
        )


@pytest.fixture
def dynamic_manager(mock_engine_client, mock_model_config, mock_lora_config, dynamic_config):
    """Dynamic model manager instance for testing."""
    manager = DynamicModelManager(
        engine_client=mock_engine_client,
        model_config=mock_model_config,
        lora_config=mock_lora_config,
        config=dynamic_config
    )
    
    # Mock the actual model parameter operations
    manager._get_model_parameters = AsyncMock(return_value={
        "timestamp": 1000.0,
        "parameter_count": 1000000,
        "model_state": {"layer1.weight": torch.randn(10, 10)}
    })
    
    manager._load_model_parameters = AsyncMock()
    manager._apply_parameter_update = AsyncMock()
    
    return manager


@pytest.fixture
def serving_dynamic_updates(mock_engine_client, mock_model_config, mock_lora_config, dynamic_config):
    """OpenAI serving dynamic updates instance for testing."""
    serving = OpenAIServingDynamicUpdates(
        engine_client=mock_engine_client,
        model_config=mock_model_config,
        lora_config=mock_lora_config,
        dynamic_config=dynamic_config
    )
    return serving


class TestDynamicModelManager:
    """Test cases for DynamicModelManager."""
    
    @pytest.mark.asyncio
    async def test_create_initial_version(self, dynamic_manager):
        """Test creating initial model version."""
        version_id = await dynamic_manager.create_initial_version("Initial test version")
        
        assert version_id.startswith("v0_")
        assert version_id in dynamic_manager.versions
        assert dynamic_manager.current_version_id == version_id
        
        version = dynamic_manager.versions[version_id]
        assert version.is_active
        assert version.description == "Initial test version"
        assert version.parameters is not None
    
    @pytest.mark.asyncio
    async def test_create_version(self, dynamic_manager):
        """Test creating a new model version."""
        # Create initial version first
        await dynamic_manager.create_initial_version("Initial")
        
        # Create new version
        version_id = await dynamic_manager.create_version("New version")
        
        assert version_id.startswith("v1_")
        assert version_id in dynamic_manager.versions
        assert dynamic_manager.current_version_id == version_id
        
        # Check that previous version is deactivated
        initial_versions = [v for v in dynamic_manager.versions.values() if v.version_id.startswith("v0_")]
        assert len(initial_versions) == 1
        assert not initial_versions[0].is_active
    
    @pytest.mark.asyncio
    async def test_incremental_update_success(self, dynamic_manager):
        """Test successful incremental parameter update."""
        await dynamic_manager.create_initial_version("Initial")
        
        # Mock performance metrics to simulate successful update
        dynamic_manager._get_performance_metrics = AsyncMock(side_effect=[
            {"accuracy": 0.85, "latency_ms": 100.0, "throughput": 50.0},  # Pre-update
            {"accuracy": 0.87, "latency_ms": 95.0, "throughput": 52.0}    # Post-update (improved)
        ])
        
        request = IncrementalUpdateRequest(
            parameter_name="layer1.weight",
            update_data=torch.randn(10, 10),
            learning_rate=0.01,
            update_type="additive"
        )
        
        result = await dynamic_manager.apply_incremental_update(request)
        
        assert result["success"] is True
        assert "update_id" in result
        assert result["update_count"] == 1
        assert "pre_metrics" in result
        assert "post_metrics" in result
    
    @pytest.mark.asyncio
    async def test_incremental_update_rollback(self, dynamic_manager):
        """Test automatic rollback on performance degradation."""
        await dynamic_manager.create_initial_version("Initial")
        
        # Mock performance metrics to simulate degradation requiring rollback
        dynamic_manager._get_performance_metrics = AsyncMock(side_effect=[
            {"accuracy": 0.85, "latency_ms": 100.0, "throughput": 50.0},  # Pre-update
            {"accuracy": 0.70, "latency_ms": 150.0, "throughput": 30.0}   # Post-update (degraded)
        ])
        
        dynamic_manager._rollback_update = AsyncMock()
        
        request = IncrementalUpdateRequest(
            parameter_name="layer1.weight",
            update_data=torch.randn(10, 10),
            learning_rate=0.01,
            update_type="additive"
        )
        
        result = await dynamic_manager.apply_incremental_update(request)
        
        assert result["success"] is False
        assert "Automatic rollback" in result["reason"]
        dynamic_manager._rollback_update.assert_called_once()
    
    @pytest.mark.asyncio
    async def test_rollback_to_version(self, dynamic_manager):
        """Test rolling back to a specific version."""
        # Create initial version
        initial_version_id = await dynamic_manager.create_initial_version("Initial")
        
        # Create another version
        new_version_id = await dynamic_manager.create_version("New version")
        
        # Rollback to initial version
        result = await dynamic_manager.rollback_to_version(initial_version_id)
        
        assert result["success"] is True
        assert result["rolled_back_to"] == initial_version_id
        assert dynamic_manager.current_version_id == initial_version_id
        
        # Check that initial version is now active
        initial_version = dynamic_manager.versions[initial_version_id]
        assert initial_version.is_active
        
        # Check that new version is deactivated
        new_version = dynamic_manager.versions[new_version_id]
        assert not new_version.is_active
    
    @pytest.mark.asyncio
    async def test_rollback_to_nonexistent_version(self, dynamic_manager):
        """Test rollback to non-existent version fails gracefully."""
        await dynamic_manager.create_initial_version("Initial")
        
        result = await dynamic_manager.rollback_to_version("nonexistent_version")
        
        assert result["success"] is False
        assert "not found" in result["reason"]
    
    def test_list_versions(self, dynamic_manager):
        """Test listing all versions."""
        # Initially empty
        versions = dynamic_manager.list_versions()
        assert len(versions) == 0
        
        # Add some versions (synchronous for this test)
        version1 = ModelVersion("v1", 1000.0, {}, {}, "Version 1", True)
        version2 = ModelVersion("v2", 2000.0, {}, {}, "Version 2", False)
        
        dynamic_manager.versions = {"v1": version1, "v2": version2}
        
        versions = dynamic_manager.list_versions()
        assert len(versions) == 2
        
        # Should be sorted by timestamp, newest first
        assert versions[0]["version_id"] == "v2"
        assert versions[1]["version_id"] == "v1"
        
        assert versions[1]["is_active"] is True
        assert versions[0]["is_active"] is False
    
    def test_get_status(self, dynamic_manager):
        """Test getting manager status."""
        # Set up some state
        dynamic_manager.current_version_id = "v1"
        dynamic_manager.versions = {"v1": ModelVersion("v1", 1000.0, {}, {}, "Version 1", True)}
        dynamic_manager.update_count = 5
        dynamic_manager.performance_history = [
            {"accuracy": 0.85, "timestamp": 1000.0},
            {"accuracy": 0.87, "timestamp": 1001.0}
        ]
        
        status = dynamic_manager.get_status()
        
        assert status["current_version"] == "v1"
        assert status["total_versions"] == 1
        assert status["total_updates"] == 5
        assert "config" in status
        assert len(status["recent_performance"]) == 2
    
    @pytest.mark.asyncio
    async def test_version_cleanup(self, dynamic_manager):
        """Test automatic cleanup of old versions."""
        # Create more versions than max_versions allows
        versions_to_create = dynamic_manager.config.max_versions + 2
        
        version_ids = []
        for i in range(versions_to_create):
            version_id = await dynamic_manager.create_version(f"Version {i}")
            version_ids.append(version_id)
        
        # Should only have max_versions
        assert len(dynamic_manager.versions) == dynamic_manager.config.max_versions
        
        # Most recent version should still exist
        assert version_ids[-1] in dynamic_manager.versions
        
        # Oldest versions should be cleaned up
        assert version_ids[0] not in dynamic_manager.versions


class TestOpenAIServingDynamicUpdates:
    """Test cases for OpenAI serving dynamic updates."""
    
    @pytest.mark.asyncio
    async def test_apply_incremental_update(self, serving_dynamic_updates):
        """Test API endpoint for incremental updates."""
        # Mock the dynamic manager
        serving_dynamic_updates.dynamic_manager = MagicMock()
        serving_dynamic_updates.dynamic_manager.create_initial_version = AsyncMock(return_value="v0_1000")
        serving_dynamic_updates.dynamic_manager.apply_incremental_update = AsyncMock(
            return_value={
                "success": True,
                "update_id": "update_1_1000",
                "update_count": 1,
                "pre_metrics": {"accuracy": 0.85},
                "post_metrics": {"accuracy": 0.87}
            }
        )
        
        request = APIIncrementalUpdateRequest(
            parameter_name="layer1.weight",
            update_data=[0.1, 0.2, 0.3],
            learning_rate=0.01,
            update_type="additive"
        )
        
        response = await serving_dynamic_updates.apply_incremental_update(request)
        
        assert response.success is True
        assert "Successfully applied" in response.message
        assert "update_1_1000" in response.data["update_id"]
    
    @pytest.mark.asyncio
    async def test_create_version(self, serving_dynamic_updates):
        """Test API endpoint for creating versions."""
        serving_dynamic_updates.dynamic_manager = MagicMock()
        serving_dynamic_updates.dynamic_manager.create_initial_version = AsyncMock(return_value="v0_1000")
        serving_dynamic_updates.dynamic_manager.create_version = AsyncMock(
            return_value="v1_1000"
        )
        
        request = ModelVersionRequest(description="Test version")
        
        response = await serving_dynamic_updates.create_version(request)
        
        assert response.success is True
        assert "v1_1000" in response.message
        assert response.data["version_id"] == "v1_1000"
    
    @pytest.mark.asyncio
    async def test_rollback_to_version(self, serving_dynamic_updates):
        """Test API endpoint for rollback."""
        serving_dynamic_updates.dynamic_manager = MagicMock()
        serving_dynamic_updates.dynamic_manager.create_initial_version = AsyncMock(return_value="v0_1000")
        serving_dynamic_updates.dynamic_manager.rollback_to_version = AsyncMock(
            return_value={
                "success": True,
                "rolled_back_to": "v0_1000",
                "timestamp": 1000.0,
                "description": "Initial version"
            }
        )
        
        request = ModelRollbackRequest(version_id="v0_1000")
        
        response = await serving_dynamic_updates.rollback_to_version(request)
        
        assert response.success is True
        assert "Successfully rolled back" in response.message
        assert response.data["rolled_back_to"] == "v0_1000"
    
    @pytest.mark.asyncio
    async def test_list_versions(self, serving_dynamic_updates):
        """Test API endpoint for listing versions."""
        serving_dynamic_updates.dynamic_manager = MagicMock()
        serving_dynamic_updates.dynamic_manager.create_initial_version = AsyncMock(return_value="v0_1000")
        serving_dynamic_updates.dynamic_manager.list_versions = MagicMock(
            return_value=[
                {
                    "version_id": "v1_1000",
                    "timestamp": 1000.0,
                    "description": "Version 1",
                    "is_active": True,
                    "performance_metrics": {"accuracy": 0.85}
                },
                {
                    "version_id": "v0_1000",
                    "timestamp": 999.0,
                    "description": "Version 0",
                    "is_active": False,
                    "performance_metrics": {"accuracy": 0.83}
                }
            ]
        )
        
        response = await serving_dynamic_updates.list_versions()
        
        assert len(response.versions) == 2
        assert response.total_count == 2
        assert response.versions[0].version_id == "v1_1000"
        assert response.versions[0].is_active is True
    
    @pytest.mark.asyncio
    async def test_get_status(self, serving_dynamic_updates):
        """Test API endpoint for getting status."""
        serving_dynamic_updates.dynamic_manager = MagicMock()
        serving_dynamic_updates.dynamic_manager.create_initial_version = AsyncMock(return_value="v0_1000")
        serving_dynamic_updates.dynamic_manager.get_status = MagicMock(
            return_value={
                "current_version": "v1_1000",
                "total_versions": 2,
                "total_updates": 5,
                "config": {"max_versions": 10},
                "recent_performance": [{"accuracy": 0.85}]
            }
        )
        
        response = await serving_dynamic_updates.get_status()
        
        assert response.current_version == "v1_1000"
        assert response.total_versions == 2
        assert response.total_updates == 5
        assert response.config["max_versions"] == 10
        assert len(response.recent_performance) == 1
    
    def test_process_update_data(self, serving_dynamic_updates):
        """Test processing of update data."""
        # Test list input
        result = serving_dynamic_updates._process_update_data([1.0, 2.0, 3.0])
        assert isinstance(result, torch.Tensor)
        assert result.shape == (3,)
        
        # Test single value
        result = serving_dynamic_updates._process_update_data(5.0)
        assert isinstance(result, torch.Tensor)
        assert result.shape == (1,)
        
        # Test tensor input
        tensor_input = torch.tensor([1.0, 2.0])
        result = serving_dynamic_updates._process_update_data(tensor_input)
        assert isinstance(result, torch.Tensor)
        assert torch.equal(result, tensor_input)
        
        # Test invalid input
        with pytest.raises(ValueError):
            serving_dynamic_updates._process_update_data("invalid")


class TestIntegration:
    """Integration tests for dynamic model updates."""
    
    @pytest.mark.asyncio
    async def test_end_to_end_update_workflow(self, serving_dynamic_updates):
        """Test complete workflow from API request to model update."""
        # Initialize the service
        await serving_dynamic_updates.initialize()
        
        # Create a version
        version_request = ModelVersionRequest(description="Baseline model")
        version_response = await serving_dynamic_updates.create_version(version_request)
        assert version_response.success is True
        baseline_version = version_response.data["version_id"]
        
        # Apply incremental update
        update_request = APIIncrementalUpdateRequest(
            parameter_name="layer1.weight",
            update_data=[0.1, 0.2, 0.3, 0.4],
            learning_rate=0.01,
            update_type="additive"
        )
        update_response = await serving_dynamic_updates.apply_incremental_update(update_request)
        assert update_response.success is True
        
        # Create another version after updates
        post_update_version_request = ModelVersionRequest(description="After incremental updates")
        post_update_response = await serving_dynamic_updates.create_version(post_update_version_request)
        assert post_update_response.success is True
        
        # List versions
        versions_response = await serving_dynamic_updates.list_versions()
        assert versions_response.total_count >= 2
        
        # Rollback to baseline
        rollback_request = ModelRollbackRequest(version_id=baseline_version)
        rollback_response = await serving_dynamic_updates.rollback_to_version(rollback_request)
        assert rollback_response.success is True
        
        # Check status
        status_response = await serving_dynamic_updates.get_status()
        assert status_response.current_version == baseline_version
        assert status_response.total_updates >= 1
    
    @pytest.mark.asyncio
    async def test_performance_monitoring_and_rollback(self, dynamic_manager):
        """Test integrated performance monitoring and automatic rollback."""
        await dynamic_manager.create_initial_version("Initial")
        
        # Simulate a series of updates with degrading performance
        performance_metrics = [
            {"accuracy": 0.85, "latency_ms": 100.0},  # Initial
            {"accuracy": 0.87, "latency_ms": 95.0},   # Good update
            {"accuracy": 0.70, "latency_ms": 150.0},  # Bad update (should trigger rollback)
        ]
        
        metric_call_count = 0
        
        async def mock_get_performance():
            nonlocal metric_call_count
            result = performance_metrics[min(metric_call_count, len(performance_metrics) - 1)]
            metric_call_count += 1
            return result
        
        dynamic_manager._get_performance_metrics = mock_get_performance
        dynamic_manager._rollback_update = AsyncMock()
        
        # First update (good)
        request1 = IncrementalUpdateRequest(
            parameter_name="layer1.weight",
            update_data=torch.randn(10, 10),
            learning_rate=0.01
        )
        result1 = await dynamic_manager.apply_incremental_update(request1)
        assert result1["success"] is True
        
        # Second update (bad, should trigger rollback)
        request2 = IncrementalUpdateRequest(
            parameter_name="layer2.weight", 
            update_data=torch.randn(10, 10),
            learning_rate=0.05  # Higher learning rate causing issues
        )
        result2 = await dynamic_manager.apply_incremental_update(request2)
        assert result2["success"] is False
        assert "rollback" in result2["reason"].lower()
        
        # Verify rollback was called
        dynamic_manager._rollback_update.assert_called_once()