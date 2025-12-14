"""
Tests for Dynamic Configuration Management System.

Tests dynamic DTESN parameter updates, validation, rollback mechanisms,
and environment-specific configuration handling without service restart.
"""

import asyncio
import json
import tempfile
import time
from pathlib import Path
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

try:
    from aphrodite.endpoints.deep_tree_echo.config import DTESNConfig
    from aphrodite.endpoints.deep_tree_echo.dynamic_config_manager import (
        DynamicConfigurationManager,
        ConfigurationUpdateRequest,
        ConfigurationEnvironment,
        ConfigurationValidator,
        ConfigurationSnapshot,
        get_dynamic_config_manager,
        initialize_dynamic_config_manager
    )
    DTESN_CONFIG_AVAILABLE = True
except ImportError:
    DTESN_CONFIG_AVAILABLE = False
    pytestmark = pytest.mark.skip("DTESN configuration not available")


@pytest.fixture
def initial_config():
    """Create initial DTESN configuration for testing."""
    return DTESNConfig(
        max_membrane_depth=4,
        esn_reservoir_size=512,
        bseries_max_order=8,
        enable_caching=True,
        cache_ttl_seconds=300,
        enable_performance_monitoring=True
    )


@pytest.fixture
def temp_backup_dir():
    """Create temporary backup directory for testing."""
    with tempfile.TemporaryDirectory() as temp_dir:
        yield Path(temp_dir)


@pytest.fixture
def config_manager(initial_config, temp_backup_dir):
    """Create configuration manager for testing."""
    manager = DynamicConfigurationManager(
        initial_config=initial_config,
        max_snapshots=10,
        backup_directory=temp_backup_dir,
        enable_auto_backup=True
    )
    yield manager


class TestConfigurationValidator:
    """Test configuration validation functionality."""
    
    def test_validator_initialization(self):
        """Test validator initializes correctly."""
        validator = ConfigurationValidator()
        assert len(validator._validators) > 0
        assert "max_membrane_depth" in validator._validators
        assert "esn_reservoir_size" in validator._validators
    
    def test_validate_valid_parameters(self):
        """Test validation of valid parameters."""
        validator = ConfigurationValidator()
        
        # Valid parameters
        assert validator.validate_parameter("max_membrane_depth", 4) == []
        assert validator.validate_parameter("esn_reservoir_size", 1024) == []
        assert validator.validate_parameter("enable_caching", True) == []
    
    def test_validate_invalid_parameters(self):
        """Test validation of invalid parameters."""
        validator = ConfigurationValidator()
        
        # Invalid parameters
        errors = validator.validate_parameter("max_membrane_depth", 0)
        assert len(errors) > 0
        
        errors = validator.validate_parameter("esn_reservoir_size", 32)
        assert len(errors) > 0
        
        errors = validator.validate_parameter("unknown_param", 123)
        assert len(errors) > 0
    
    def test_validate_configuration_dependencies(self):
        """Test validation of configuration dependencies."""
        validator = ConfigurationValidator()
        
        # Test reservoir size vs depth relationship
        config = {
            "max_membrane_depth": 8,
            "esn_reservoir_size": 100  # Too small for depth 8
        }
        errors = validator.validate_configuration(config)
        assert len(errors) > 0
        
        # Valid relationship
        config = {
            "max_membrane_depth": 4,
            "esn_reservoir_size": 512  # Appropriate size
        }
        errors = validator.validate_configuration(config)
        assert len(errors) == 0


class TestDynamicConfigurationManager:
    """Test dynamic configuration manager functionality."""
    
    def test_manager_initialization(self, initial_config, temp_backup_dir):
        """Test manager initializes correctly."""
        manager = DynamicConfigurationManager(
            initial_config=initial_config,
            backup_directory=temp_backup_dir
        )
        
        assert manager.current_config == initial_config
        assert len(manager.get_snapshots()) == 1  # Initial snapshot
        assert manager.environment == ConfigurationEnvironment.DEVELOPMENT
    
    def test_environment_management(self, config_manager):
        """Test environment setting and getting."""
        # Test setting environment
        config_manager.set_environment(ConfigurationEnvironment.PRODUCTION)
        assert config_manager.environment == ConfigurationEnvironment.PRODUCTION
        
        # Test status includes environment
        status = config_manager.get_current_status()
        assert status["environment"] == "production"
    
    @pytest.mark.asyncio
    async def test_single_parameter_update(self, config_manager):
        """Test updating a single configuration parameter."""
        # Test valid update
        request = ConfigurationUpdateRequest(
            parameter_path="esn_reservoir_size",
            new_value=1024,
            description="Increase reservoir size"
        )
        
        result = await config_manager.update_parameter(request)
        
        assert result["success"] is True
        assert result["old_value"] == 512
        assert result["new_value"] == 1024
        assert config_manager.current_config.esn_reservoir_size == 1024
    
    @pytest.mark.asyncio
    async def test_parameter_update_validation_failure(self, config_manager):
        """Test parameter update with validation failure."""
        request = ConfigurationUpdateRequest(
            parameter_path="max_membrane_depth",
            new_value=100,  # Invalid - too large
            description="Invalid depth"
        )
        
        result = await config_manager.update_parameter(request)
        
        assert result["success"] is False
        assert "validation_errors" in result
        assert len(result["validation_errors"]) > 0
        # Configuration should remain unchanged
        assert config_manager.current_config.max_membrane_depth == 4
    
    @pytest.mark.asyncio
    async def test_validation_only_mode(self, config_manager):
        """Test validation-only parameter updates."""
        request = ConfigurationUpdateRequest(
            parameter_path="esn_reservoir_size",
            new_value=2048,
            validate_only=True
        )
        
        result = await config_manager.update_parameter(request)
        
        assert result["validate_only"] is True
        assert result["success"] is True
        # Configuration should remain unchanged
        assert config_manager.current_config.esn_reservoir_size == 512
    
    @pytest.mark.asyncio
    async def test_batch_parameter_updates(self, config_manager):
        """Test updating multiple parameters atomically."""
        updates = [
            ConfigurationUpdateRequest(
                parameter_path="max_membrane_depth",
                new_value=6
            ),
            ConfigurationUpdateRequest(
                parameter_path="esn_reservoir_size", 
                new_value=1024
            ),
            ConfigurationUpdateRequest(
                parameter_path="cache_ttl_seconds",
                new_value=600
            )
        ]
        
        result = await config_manager.update_multiple_parameters(updates)
        
        assert result["success"] is True
        assert len(result["updated_parameters"]) == 3
        
        # Verify all parameters were updated
        config = config_manager.current_config
        assert config.max_membrane_depth == 6
        assert config.esn_reservoir_size == 1024
        assert config.cache_ttl_seconds == 600
    
    @pytest.mark.asyncio
    async def test_batch_update_validation_failure(self, config_manager):
        """Test batch update with validation failure."""
        updates = [
            ConfigurationUpdateRequest(
                parameter_path="max_membrane_depth",
                new_value=6
            ),
            ConfigurationUpdateRequest(
                parameter_path="esn_reservoir_size",
                new_value=50  # Invalid - too small
            )
        ]
        
        result = await config_manager.update_multiple_parameters(updates)
        
        assert result["success"] is False
        assert "validation_errors" in result
        
        # No parameters should have been updated
        config = config_manager.current_config
        assert config.max_membrane_depth == 4  # Original value
        assert config.esn_reservoir_size == 512  # Original value
    
    @pytest.mark.asyncio
    async def test_configuration_rollback(self, config_manager):
        """Test configuration rollback functionality."""
        # Make an update
        request = ConfigurationUpdateRequest(
            parameter_path="max_membrane_depth",
            new_value=8
        )
        
        update_result = await config_manager.update_parameter(request)
        assert update_result["success"] is True
        rollback_snapshot = update_result["rollback_snapshot"]
        
        # Verify update was applied
        assert config_manager.current_config.max_membrane_depth == 8
        
        # Rollback
        rollback_result = await config_manager.rollback_to_snapshot(rollback_snapshot)
        
        assert rollback_result["success"] is True
        assert config_manager.current_config.max_membrane_depth == 4  # Original value
    
    def test_snapshot_management(self, config_manager):
        """Test configuration snapshot creation and management."""
        initial_snapshots = len(config_manager.get_snapshots())
        
        # Create several snapshots by updating configuration
        for i in range(5):
            config_manager._create_snapshot(f"Test snapshot {i}")
        
        snapshots = config_manager.get_snapshots()
        assert len(snapshots) == initial_snapshots + 5
        
        # Test snapshot metadata
        for snapshot in snapshots:
            assert "snapshot_id" in snapshot
            assert "timestamp" in snapshot
            assert "description" in snapshot
    
    def test_callback_registration(self, config_manager):
        """Test configuration update callback registration."""
        callback_called = False
        received_config = None
        
        def test_callback(config):
            nonlocal callback_called, received_config
            callback_called = True
            received_config = config
        
        config_manager.register_update_callback(test_callback)
        
        # Test callback is in the list
        assert len(config_manager._update_callbacks) == 1
    
    @pytest.mark.asyncio
    async def test_async_callback_execution(self, config_manager):
        """Test that callbacks are executed on configuration updates."""
        callback_called = False
        received_config = None
        
        async def async_callback(config):
            nonlocal callback_called, received_config
            callback_called = True
            received_config = config
        
        config_manager.register_update_callback(async_callback)
        
        # Make an update
        request = ConfigurationUpdateRequest(
            parameter_path="esn_reservoir_size",
            new_value=1024
        )
        
        await config_manager.update_parameter(request)
        
        # Verify callback was called
        assert callback_called is True
        assert received_config is not None
        assert received_config.esn_reservoir_size == 1024
    
    def test_backup_functionality(self, config_manager):
        """Test configuration backup to disk."""
        # Update configuration to trigger backup
        request = ConfigurationUpdateRequest(
            parameter_path="esn_reservoir_size",
            new_value=1024
        )
        
        # This should create a backup file
        asyncio.run(config_manager.update_parameter(request))
        
        # Check that backup files were created
        backup_files = list(config_manager.backup_directory.glob("*.json"))
        assert len(backup_files) > 0
        
        # Verify backup content
        with open(backup_files[0], 'r') as f:
            backup_data = json.load(f)
            assert "snapshot_id" in backup_data
            assert "config_data" in backup_data


@pytest.mark.skipif(not DTESN_CONFIG_AVAILABLE, reason="DTESN config not available")
class TestConfigurationIntegration:
    """Test integration with existing DTESN components."""
    
    def test_global_manager_singleton(self):
        """Test global manager singleton pattern."""
        manager1 = get_dynamic_config_manager()
        manager2 = get_dynamic_config_manager()
        
        # Should be the same instance
        assert manager1 is manager2
    
    def test_manager_initialization_with_config(self, initial_config):
        """Test initializing global manager with specific config."""
        manager = initialize_dynamic_config_manager(
            initial_config=initial_config,
            max_snapshots=20
        )
        
        assert manager.current_config == initial_config
        assert manager.max_snapshots == 20
    
    @pytest.mark.asyncio
    async def test_dtesn_processor_integration(self, initial_config):
        """Test integration with DTESNProcessor."""
        try:
            from aphrodite.endpoints.deep_tree_echo.dtesn_processor import DTESNProcessor
            
            # Create processor with dynamic config enabled
            processor = DTESNProcessor(
                config=initial_config,
                enable_dynamic_config=True,
                max_concurrent_processes=10
            )
            
            # Verify dynamic config is set up
            assert hasattr(processor, 'config_manager')
            
        except ImportError:
            pytest.skip("DTESNProcessor not available")


@pytest.mark.skipif(not DTESN_CONFIG_AVAILABLE, reason="DTESN config not available")
class TestConfigurationAPI:
    """Test configuration API endpoints."""
    
    @pytest.fixture
    def mock_app(self):
        """Create mock FastAPI app for testing."""
        from fastapi.testclient import TestClient
        from fastapi import FastAPI
        
        try:
            from aphrodite.endpoints.deep_tree_echo.config_routes import config_router
            
            app = FastAPI()
            app.include_router(config_router)
            
            return TestClient(app)
        except ImportError:
            pytest.skip("FastAPI routes not available")
    
    def test_get_current_configuration(self, mock_app):
        """Test GET /v1/config/current endpoint."""
        response = mock_app.get("/v1/config/current")
        assert response.status_code == 200
        
        data = response.json()
        assert data["success"] is True
        assert "configuration" in data
        assert "environment" in data
    
    def test_get_configuration_status(self, mock_app):
        """Test GET /v1/config/status endpoint."""
        response = mock_app.get("/v1/config/status")
        assert response.status_code == 200
        
        data = response.json()
        assert data["success"] is True
        assert "status" in data
    
    def test_update_configuration_parameter(self, mock_app):
        """Test POST /v1/config/update endpoint."""
        update_data = {
            "parameter": "esn_reservoir_size",
            "value": 1024,
            "description": "Test update",
            "validate_only": False
        }
        
        response = mock_app.post("/v1/config/update", json=update_data)
        assert response.status_code == 200
        
        data = response.json()
        assert data["success"] is True
    
    def test_validate_configuration_parameter(self, mock_app):
        """Test POST /v1/config/validate endpoint."""
        validate_data = {
            "parameter": "max_membrane_depth",
            "value": 100  # Invalid value
        }
        
        response = mock_app.post("/v1/config/validate", json=validate_data)
        assert response.status_code == 200
        
        data = response.json()
        assert data["success"] is False
        assert data["errors"] is not None
    
    def test_get_configuration_snapshots(self, mock_app):
        """Test GET /v1/config/snapshots endpoint.""" 
        response = mock_app.get("/v1/config/snapshots")
        assert response.status_code == 200
        
        data = response.json()
        assert data["success"] is True
        assert "snapshots" in data
        assert "total_count" in data
    
    def test_configuration_health_check(self, mock_app):
        """Test GET /v1/config/health endpoint."""
        response = mock_app.get("/v1/config/health")
        assert response.status_code == 200
        
        data = response.json()
        assert data["success"] is True
        assert "health" in data


if __name__ == "__main__":
    pytest.main([__file__, "-v"])