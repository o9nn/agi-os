"""
Tests for DTESN-specific data validation.

This module tests the comprehensive validation system for DTESN data structures,
ensuring all schemas and validation rules work correctly.
"""

import pytest
from unittest.mock import patch
from pydantic import ValidationError

from aphrodite.endpoints.security.dtesn_validation import (
    DTESNDataType,
    DTESNValidationConfig,
    ESNReservoirConfigSchema,
    PSystemMembraneSchema,
    BSeriesParametersSchema,
    OEISTopologySchema,
    DTESNIntegrationConfigSchema,
    validate_dtesn_data_structure,
    normalize_dtesn_configuration,
    validate_dtesn_integration_consistency
)
from fastapi import HTTPException


class TestESNReservoirConfigValidation:
    """Test ESN reservoir configuration validation."""
    
    def test_valid_esn_config(self):
        """Test validation of valid ESN configuration."""
        valid_config = {
            "reservoir_size": 100,
            "input_dimension": 10,
            "spectral_radius": 0.95,
            "leak_rate": 0.1,
            "input_scaling": 1.0,
            "noise_level": 0.01
        }
        
        schema = ESNReservoirConfigSchema(**valid_config)
        assert schema.reservoir_size == 100
        assert schema.spectral_radius == 0.95
    
    def test_invalid_reservoir_size(self):
        """Test validation fails for invalid reservoir size."""
        invalid_config = {
            "reservoir_size": -1,  # Invalid: negative size
            "input_dimension": 10,
            "spectral_radius": 0.95,
            "leak_rate": 0.1,
            "input_scaling": 1.0,
            "noise_level": 0.01
        }
        
        with pytest.raises(ValidationError):
            ESNReservoirConfigSchema(**invalid_config)
    
    def test_spectral_radius_warning(self):
        """Test warning for spectral radius >= 1.0."""
        config_with_warning = {
            "reservoir_size": 100,
            "input_dimension": 10,
            "spectral_radius": 1.1,  # May cause instability
            "leak_rate": 0.1,
            "input_scaling": 1.0,
            "noise_level": 0.01
        }
        
        # Should validate but may log warning
        schema = ESNReservoirConfigSchema(**config_with_warning)
        assert schema.spectral_radius == 1.1


class TestPSystemMembraneValidation:
    """Test P-System membrane configuration validation."""
    
    def test_valid_membrane_config(self):
        """Test validation of valid membrane configuration."""
        valid_config = {
            "membrane_id": "membrane_1",
            "parent_id": "root",
            "depth": 2,
            "capacity": 1000,
            "rules": [
                {"type": "evolution", "action": "multiply"},
                {"type": "transport", "action": "move_up"}
            ]
        }
        
        schema = PSystemMembraneSchema(**valid_config)
        assert schema.membrane_id == "membrane_1"
        assert len(schema.rules) == 2
    
    def test_invalid_membrane_id(self):
        """Test validation fails for invalid membrane ID."""
        invalid_config = {
            "membrane_id": "membrane@1",  # Invalid: contains @
            "depth": 2,
            "capacity": 1000,
            "rules": [{"type": "evolution", "action": "multiply"}]
        }
        
        with pytest.raises(ValidationError):
            PSystemMembraneSchema(**invalid_config)
    
    def test_invalid_rules_structure(self):
        """Test validation fails for invalid rules."""
        invalid_config = {
            "membrane_id": "membrane_1", 
            "depth": 2,
            "capacity": 1000,
            "rules": [
                {"type": "evolution"},  # Missing 'action' field
                "invalid_rule"  # Not a dictionary
            ]
        }
        
        with pytest.raises(ValidationError):
            PSystemMembraneSchema(**invalid_config)


class TestBSeriesParametersValidation:
    """Test B-Series parameters validation."""
    
    def test_valid_bseries_config(self):
        """Test validation of valid B-Series configuration."""
        valid_config = {
            "order": 3,
            "timestep": 0.01,
            "method": "rk4",
            "tolerance": 1e-6,
            "coefficients": [1.0, 0.5, 0.25, 0.125, 0.0625, 0.03125]  # Length = 1+2+3 = 6
        }
        
        schema = BSeriesParametersSchema(**valid_config)
        assert schema.order == 3
        assert schema.method == "rk4"
    
    def test_coefficients_length_mismatch(self):
        """Test validation fails when coefficients length doesn't match order."""
        invalid_config = {
            "order": 3,
            "timestep": 0.01,
            "method": "rk4", 
            "tolerance": 1e-6,
            "coefficients": [1.0, 0.5]  # Too few coefficients for order 3
        }
        
        with pytest.raises(ValidationError):
            BSeriesParametersSchema(**invalid_config)


class TestOEISTopologyValidation:
    """Test OEIS A000081 topology validation."""
    
    def test_valid_oeis_sequence(self):
        """Test validation of valid OEIS sequence."""
        valid_config = {
            "topology_sequence": [1, 1, 2, 4, 9],
            "max_depth": 4,
            "branching_factor": [1, 1, 2, 4]
        }
        
        schema = OEISTopologySchema(**valid_config)
        assert schema.topology_sequence == [1, 1, 2, 4, 9]
    
    def test_invalid_oeis_sequence(self):
        """Test validation fails for invalid OEIS sequence."""
        invalid_config = {
            "topology_sequence": [1, 2, 3, 4, 5],  # Incorrect sequence
            "max_depth": 4,
            "branching_factor": [1, 2, 3, 4]
        }
        
        with pytest.raises(ValidationError):
            OEISTopologySchema(**invalid_config)


class TestDTESNIntegrationConfigValidation:
    """Test DTESN integration configuration validation."""
    
    def test_valid_integration_config(self):
        """Test validation of valid integration configuration."""
        valid_config = {
            "integration_mode": "full_dtesn",
            "coupling_strength": 0.5,
            "update_synchronization": True,
            "performance_monitoring": True,
            "esn_config": {
                "reservoir_size": 100,
                "input_dimension": 10,
                "spectral_radius": 0.95,
                "leak_rate": 0.1,
                "input_scaling": 1.0,
                "noise_level": 0.01
            },
            "membrane_configs": [{
                "membrane_id": "mem1",
                "depth": 1,
                "capacity": 1000,
                "rules": [{"type": "evolution", "action": "multiply"}]
            }],
            "bseries_config": {
                "order": 2,
                "timestep": 0.01,
                "method": "rk2",
                "tolerance": 1e-6,
                "coefficients": [1.0, 0.5, 0.25]
            },
            "oeis_topology": {
                "topology_sequence": [1, 1, 2],
                "max_depth": 2,
                "branching_factor": [1, 1]
            }
        }
        
        schema = DTESNIntegrationConfigSchema(**valid_config)
        assert schema.integration_mode == "full_dtesn"
        assert schema.coupling_strength == 0.5
    
    def test_standalone_mode_validation(self):
        """Test validation for standalone mode."""
        config_with_coupling = {
            "integration_mode": "standalone",
            "coupling_strength": 0.5,  # Should be 0 for standalone
            "update_synchronization": True,
            "esn_config": {
                "reservoir_size": 100,
                "input_dimension": 10,
                "spectral_radius": 0.95,
                "leak_rate": 0.1,
                "input_scaling": 1.0,
                "noise_level": 0.01
            }
        }
        
        with pytest.raises(ValidationError):
            DTESNIntegrationConfigSchema(**config_with_coupling)


class TestDTESNValidationFunctions:
    """Test DTESN validation utility functions."""
    
    def test_validate_dtesn_data_structure_success(self):
        """Test successful DTESN data structure validation."""
        esn_data = {
            "reservoir_size": 100,
            "input_dimension": 10,
            "spectral_radius": 0.95,
            "leak_rate": 0.1,
            "input_scaling": 1.0,
            "noise_level": 0.01
        }
        
        result = validate_dtesn_data_structure(
            esn_data, 
            DTESNDataType.ESN_RESERVOIR_CONFIG
        )
        
        assert result["reservoir_size"] == 100
        assert "_validation_metadata" in result
    
    def test_validate_dtesn_data_structure_failure(self):
        """Test DTESN data structure validation failure."""
        invalid_data = {
            "reservoir_size": -1,  # Invalid
            "input_dimension": 10,
            "spectral_radius": 0.95
        }
        
        with pytest.raises(HTTPException) as exc_info:
            validate_dtesn_data_structure(
                invalid_data,
                DTESNDataType.ESN_RESERVOIR_CONFIG
            )
        
        assert exc_info.value.status_code == 400
    
    def test_normalize_dtesn_configuration(self):
        """Test DTESN configuration normalization."""
        config_data = {
            "reservoirSize": 100,  # camelCase
            "inputDimension": 10,
            "spectralRadius": 0.95,
            "customField": "value"
        }
        
        normalized = normalize_dtesn_configuration(config_data)
        
        assert "reservoir_size" in normalized
        assert "input_dimension" in normalized  
        assert "spectral_radius" in normalized
        assert normalized["performance_monitoring"] is True  # Default added
    
    def test_validate_dtesn_integration_consistency_success(self):
        """Test successful DTESN integration consistency validation."""
        esn_config = {
            "input_dimension": 20,
            "leak_rate": 0.1
        }
        
        membrane_configs = [
            {"depth": 1, "membrane_id": "mem1"},
            {"depth": 2, "membrane_id": "mem2"}
        ]
        
        bseries_config = {
            "timestep": 0.05,  # Less than leak_rate
            "order": 2
        }
        
        is_consistent, issues = validate_dtesn_integration_consistency(
            esn_config, membrane_configs, bseries_config
        )
        
        assert is_consistent is True
        assert len(issues) == 0
    
    def test_validate_dtesn_integration_consistency_failure(self):
        """Test DTESN integration consistency validation failure."""
        esn_config = {
            "input_dimension": 5,  # Too small for membrane count
            "leak_rate": 0.05
        }
        
        membrane_configs = [
            {"depth": 1}, {"depth": 2}, {"depth": 3}, 
            {"depth": 4}, {"depth": 5}, {"depth": 6}  # 6 membranes
        ]
        
        bseries_config = {
            "timestep": 0.1,  # Greater than leak_rate  
            "order": 2
        }
        
        is_consistent, issues = validate_dtesn_integration_consistency(
            esn_config, membrane_configs, bseries_config
        )
        
        assert is_consistent is False
        assert len(issues) > 0
        assert any("input dimension" in issue.lower() for issue in issues)
        assert any("timestep" in issue.lower() for issue in issues)


class TestDTESNValidationConfig:
    """Test DTESN validation configuration."""
    
    def test_default_config(self):
        """Test default DTESN validation configuration."""
        config = DTESNValidationConfig()
        
        assert config.max_reservoir_size == 10000
        assert config.min_reservoir_size == 1
        assert config.max_membrane_depth == 10
        assert config.enable_performance_tracking is True
    
    def test_custom_config(self):
        """Test custom DTESN validation configuration."""
        config = DTESNValidationConfig(
            max_reservoir_size=5000,
            enable_deep_validation=False,
            max_validation_time_ms=200
        )
        
        assert config.max_reservoir_size == 5000
        assert config.enable_deep_validation is False
        assert config.max_validation_time_ms == 200


@pytest.mark.asyncio 
class TestDTESNValidationPerformance:
    """Test performance aspects of DTESN validation."""
    
    async def test_validation_performance_tracking(self):
        """Test that validation performance is tracked."""
        large_config = {
            "reservoir_size": 1000,
            "input_dimension": 100,
            "spectral_radius": 0.95,
            "leak_rate": 0.1,
            "input_scaling": 1.0,
            "noise_level": 0.01
        }
        
        config = DTESNValidationConfig(enable_performance_tracking=True)
        
        result = validate_dtesn_data_structure(
            large_config,
            DTESNDataType.ESN_RESERVOIR_CONFIG,
            config
        )
        
        assert "_validation_metadata" in result
        assert "validation_time_ms" in result["_validation_metadata"]
        assert result["_validation_metadata"]["validation_time_ms"] > 0
    
    @patch('aphrodite.endpoints.security.dtesn_validation.time.perf_counter')
    async def test_validation_timeout_warning(self, mock_time):
        """Test warning for slow validation."""
        # Mock slow validation
        mock_time.side_effect = [0.0, 0.2]  # 200ms elapsed
        
        config_data = {
            "reservoir_size": 100,
            "input_dimension": 10,
            "spectral_radius": 0.95,
            "leak_rate": 0.1,
            "input_scaling": 1.0,
            "noise_level": 0.01
        }
        
        config = DTESNValidationConfig(max_validation_time_ms=100)  # 100ms limit
        
        with patch('aphrodite.endpoints.security.dtesn_validation.logger') as mock_logger:
            result = validate_dtesn_data_structure(
                config_data,
                DTESNDataType.ESN_RESERVOIR_CONFIG, 
                config
            )
            
            # Should complete but log warning
            assert result is not None
            mock_logger.warning.assert_called()