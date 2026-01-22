import pytest
from unittest.mock import patch
from pydantic import ValidationError
from aphrodite.endpoints.security.dtesn_validation import DTESNDataType, DTESNValidationConfig, ESNReservoirConfigSchema, PSystemMembraneSchema, BSeriesParametersSchema, OEISTopologySchema, DTESNIntegrationConfigSchema, validate_dtesn_data_structure, normalize_dtesn_configuration, validate_dtesn_integration_consistency
from fastapi import HTTPException
class TestESNReservoirConfigValidation:
    def test_valid_esn_config(self):
        valid_config = {'reservoir_size': 100, 'input_dimension': 10, 'spectral_radius': 0.95, 'leak_rate': 0.1, 'input_scaling': 1.0, 'noise_level': 0.01}
        schema = ESNReservoirConfigSchema(**valid_config)
        assert schema.reservoir_size == 100
        assert schema.spectral_radius == 0.95
    def test_invalid_reservoir_size(self):
        invalid_config = {'reservoir_size': -1, 'input_dimension': 10, 'spectral_radius': 0.95, 'leak_rate': 0.1, 'input_scaling': 1.0, 'noise_level': 0.01}
        with pytest.raises(ValidationError):
            ESNReservoirConfigSchema(**invalid_config)
    def test_spectral_radius_warning(self):
        config_with_warning = {'reservoir_size': 100, 'input_dimension': 10, 'spectral_radius': 1.1, 'leak_rate': 0.1, 'input_scaling': 1.0, 'noise_level': 0.01}
        schema = ESNReservoirConfigSchema(**config_with_warning)
        assert schema.spectral_radius == 1.1
class TestPSystemMembraneValidation:
    def test_valid_membrane_config(self):
        valid_config = {'membrane_id': 'membrane_1', 'parent_id': 'root', 'depth': 2, 'capacity': 1000, 'rules': [{'type': 'evolution', 'action': 'multiply'}, {'type': 'transport', 'action': 'move_up'}]}
        schema = PSystemMembraneSchema(**valid_config)
        assert schema.membrane_id == 'membrane_1'
        assert len(schema.rules) == 2
    def test_invalid_membrane_id(self):
        invalid_config = {'membrane_id': 'membrane@1', 'depth': 2, 'capacity': 1000, 'rules': [{'type': 'evolution', 'action': 'multiply'}]}
        with pytest.raises(ValidationError):
            PSystemMembraneSchema(**invalid_config)
    def test_invalid_rules_structure(self):
        invalid_config = {'membrane_id': 'membrane_1', 'depth': 2, 'capacity': 1000, 'rules': [{'type': 'evolution'}, 'invalid_rule']}
        with pytest.raises(ValidationError):
            PSystemMembraneSchema(**invalid_config)
class TestBSeriesParametersValidation:
    def test_valid_bseries_config(self):
        valid_config = {'order': 3, 'timestep': 0.01, 'method': 'rk4', 'tolerance': 1e-06, 'coefficients': [1.0, 0.5, 0.25, 0.125, 0.0625, 0.03125]}
        schema = BSeriesParametersSchema(**valid_config)
        assert schema.order == 3
        assert schema.method == 'rk4'
    def test_coefficients_length_mismatch(self):
        invalid_config = {'order': 3, 'timestep': 0.01, 'method': 'rk4', 'tolerance': 1e-06, 'coefficients': [1.0, 0.5]}
        with pytest.raises(ValidationError):
            BSeriesParametersSchema(**invalid_config)
class TestOEISTopologyValidation:
    def test_valid_oeis_sequence(self):
        valid_config = {'topology_sequence': [1, 1, 2, 4, 9], 'max_depth': 4, 'branching_factor': [1, 1, 2, 4]}
        schema = OEISTopologySchema(**valid_config)
        assert schema.topology_sequence == [1, 1, 2, 4, 9]
    def test_invalid_oeis_sequence(self):
        invalid_config = {'topology_sequence': [1, 2, 3, 4, 5], 'max_depth': 4, 'branching_factor': [1, 2, 3, 4]}
        with pytest.raises(ValidationError):
            OEISTopologySchema(**invalid_config)
class TestDTESNIntegrationConfigValidation:
    def test_valid_integration_config(self):
        valid_config = {'integration_mode': 'full_dtesn', 'coupling_strength': 0.5, 'update_synchronization': True, 'performance_monitoring': True, 'esn_config': {'reservoir_size': 100, 'input_dimension': 10, 'spectral_radius': 0.95, 'leak_rate': 0.1, 'input_scaling': 1.0, 'noise_level': 0.01}, 'membrane_configs': [{'membrane_id': 'mem1', 'depth': 1, 'capacity': 1000, 'rules': [{'type': 'evolution', 'action': 'multiply'}]}], 'bseries_config': {'order': 2, 'timestep': 0.01, 'method': 'rk2', 'tolerance': 1e-06, 'coefficients': [1.0, 0.5, 0.25]}, 'oeis_topology': {'topology_sequence': [1, 1, 2], 'max_depth': 2, 'branching_factor': [1, 1]}}
        schema = DTESNIntegrationConfigSchema(**valid_config)
        assert schema.integration_mode == 'full_dtesn'
        assert schema.coupling_strength == 0.5
    def test_standalone_mode_validation(self):
        config_with_coupling = {'integration_mode': 'standalone', 'coupling_strength': 0.5, 'update_synchronization': True, 'esn_config': {'reservoir_size': 100, 'input_dimension': 10, 'spectral_radius': 0.95, 'leak_rate': 0.1, 'input_scaling': 1.0, 'noise_level': 0.01}}
        with pytest.raises(ValidationError):
            DTESNIntegrationConfigSchema(**config_with_coupling)
class TestDTESNValidationFunctions:
    def test_validate_dtesn_data_structure_success(self):
        esn_data = {'reservoir_size': 100, 'input_dimension': 10, 'spectral_radius': 0.95, 'leak_rate': 0.1, 'input_scaling': 1.0, 'noise_level': 0.01}
        result = validate_dtesn_data_structure(esn_data, DTESNDataType.ESN_RESERVOIR_CONFIG)
        assert result['reservoir_size'] == 100
        assert '_validation_metadata' in result
    def test_validate_dtesn_data_structure_failure(self):
        invalid_data = {'reservoir_size': -1, 'input_dimension': 10, 'spectral_radius': 0.95}
        with pytest.raises(HTTPException) as exc_info:
            validate_dtesn_data_structure(invalid_data, DTESNDataType.ESN_RESERVOIR_CONFIG)
        assert exc_info.value.status_code == 400
    def test_normalize_dtesn_configuration(self):
        config_data = {'reservoirSize': 100, 'inputDimension': 10, 'spectralRadius': 0.95, 'customField': 'value'}
        normalized = normalize_dtesn_configuration(config_data)
        assert 'reservoir_size' in normalized
        assert 'input_dimension' in normalized
        assert 'spectral_radius' in normalized
        assert normalized['performance_monitoring'] is True
    def test_validate_dtesn_integration_consistency_success(self):
        esn_config = {'input_dimension': 20, 'leak_rate': 0.1}
        membrane_configs = [{'depth': 1, 'membrane_id': 'mem1'}, {'depth': 2, 'membrane_id': 'mem2'}]
        bseries_config = {'timestep': 0.05, 'order': 2}
        is_consistent, issues = validate_dtesn_integration_consistency(esn_config, membrane_configs, bseries_config)
        assert is_consistent is True
        assert len(issues) == 0
    def test_validate_dtesn_integration_consistency_failure(self):
        esn_config = {'input_dimension': 5, 'leak_rate': 0.05}
        membrane_configs = [{'depth': 1}, {'depth': 2}, {'depth': 3}, {'depth': 4}, {'depth': 5}, {'depth': 6}]
        bseries_config = {'timestep': 0.1, 'order': 2}
        is_consistent, issues = validate_dtesn_integration_consistency(esn_config, membrane_configs, bseries_config)
        assert is_consistent is False
        assert len(issues) > 0
        assert any(('input dimension' in issue.lower() for issue in issues))
        assert any(('timestep' in issue.lower() for issue in issues))
class TestDTESNValidationConfig:
    def test_default_config(self):
        config = DTESNValidationConfig()
        assert config.max_reservoir_size == 10000
        assert config.min_reservoir_size == 1
        assert config.max_membrane_depth == 10
        assert config.enable_performance_tracking is True
    def test_custom_config(self):
        config = DTESNValidationConfig(max_reservoir_size=5000, enable_deep_validation=False, max_validation_time_ms=200)
        assert config.max_reservoir_size == 5000
        assert config.enable_deep_validation is False
        assert config.max_validation_time_ms == 200
@pytest.mark.asyncio
class TestDTESNValidationPerformance:
    async def test_validation_performance_tracking(self):
        large_config = {'reservoir_size': 1000, 'input_dimension': 100, 'spectral_radius': 0.95, 'leak_rate': 0.1, 'input_scaling': 1.0, 'noise_level': 0.01}
        config = DTESNValidationConfig(enable_performance_tracking=True)
        result = validate_dtesn_data_structure(large_config, DTESNDataType.ESN_RESERVOIR_CONFIG, config)
        assert '_validation_metadata' in result
        assert 'validation_time_ms' in result['_validation_metadata']
        assert result['_validation_metadata']['validation_time_ms'] > 0
    @patch('aphrodite.endpoints.security.dtesn_validation.time.perf_counter')
    async def test_validation_timeout_warning(self, mock_time):
        mock_time.side_effect = [0.0, 0.2]
        config_data = {'reservoir_size': 100, 'input_dimension': 10, 'spectral_radius': 0.95, 'leak_rate': 0.1, 'input_scaling': 1.0, 'noise_level': 0.01}
        config = DTESNValidationConfig(max_validation_time_ms=100)
        with patch('aphrodite.endpoints.security.dtesn_validation.logger') as mock_logger:
            result = validate_dtesn_data_structure(config_data, DTESNDataType.ESN_RESERVOIR_CONFIG, config)
            assert result is not None
            mock_logger.warning.assert_called()