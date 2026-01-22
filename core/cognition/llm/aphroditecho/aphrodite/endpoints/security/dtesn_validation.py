import re
import time
from typing import Any, Dict, List, Optional, Union, Tuple
from enum import Enum
import logging
from fastapi import HTTPException
from pydantic import BaseModel, Field, field_validator, model_validator
logger = logging.getLogger(__name__)
class DTESNDataType(str, Enum):
    ESN_RESERVOIR_CONFIG = 'esn_reservoir_config'
    PSYSTEM_MEMBRANE = 'psystem_membrane'
    BSERIES_PARAMETERS = 'bseries_parameters'
    INTEGRATION_CONFIG = 'integration_config'
    OEIS_TOPOLOGY = 'oeis_topology'
    MEMORY_LAYOUT = 'memory_layout'
class DTESNValidationConfig(BaseModel):
    max_reservoir_size: int = 10000
    min_reservoir_size: int = 1
    max_input_dimension: int = 1000
    min_spectral_radius: float = 0.1
    max_spectral_radius: float = 1.5
    max_membrane_depth: int = 10
    max_membranes_per_level: int = 100
    max_total_membranes: int = 1000
    max_bseries_order: int = 10
    min_integration_timestep: float = 1e-06
    max_integration_timestep: float = 1.0
    min_coupling_strength: float = 0.0
    max_coupling_strength: float = 1.0
    max_validation_time_ms: int = 100
    enable_deep_validation: bool = True
    enable_performance_tracking: bool = True
class ESNReservoirConfigSchema(BaseModel):
    reservoir_size: int = Field(ge=1, le=10000, description='Size of the ESN reservoir')
    input_dimension: int = Field(ge=1, le=1000, description='Input vector dimension')
    spectral_radius: float = Field(ge=0.1, le=1.5, description='Spectral radius of reservoir matrix')
    leak_rate: float = Field(ge=0.0, le=1.0, description='Leak rate for reservoir dynamics')
    input_scaling: float = Field(ge=0.0, le=10.0, description='Input scaling factor')
    noise_level: float = Field(ge=0.0, le=1.0, description='Noise level in reservoir')
    @field_validator('spectral_radius')
    @classmethod
    def validate_spectral_radius(cls, v):
        if v >= 1.0:
            logger.warning(f'Spectral radius {v} may cause instability')
        return v
class PSystemMembraneSchema(BaseModel):
    membrane_id: str = Field(pattern='^[a-zA-Z0-9_-]+$', description='Membrane identifier')
    parent_id: Optional[str] = Field(default=None, description='Parent membrane ID')
    depth: int = Field(ge=0, le=10, description='Membrane depth in hierarchy')
    capacity: int = Field(ge=1, le=1000000, description='Membrane object capacity')
    rules: List[Dict[str, Any]] = Field(description='Membrane evolution rules')
    @field_validator('rules')
    @classmethod
    def validate_rules(cls, v):
        for rule in v:
            if not isinstance(rule, dict):
                raise ValueError('Rules must be dictionary objects')
            if 'type' not in rule or 'action' not in rule:
                raise ValueError("Rules must have 'type' and 'action' fields")
        return v
class BSeriesParametersSchema(BaseModel):
    order: int = Field(ge=1, le=10, description='B-Series expansion order')
    timestep: float = Field(ge=1e-06, le=1.0, description='Integration timestep')
    method: str = Field(pattern='^(euler|rk2|rk4|dopri)$', description='Integration method')
    tolerance: float = Field(ge=1e-12, le=0.001, description='Integration tolerance')
    coefficients: List[float] = Field(description='B-Series coefficients')
    @field_validator('coefficients')
    @classmethod
    def validate_coefficients(cls, v, info):
        if info.data and 'order' in info.data:
            expected_length = sum(range(1, info.data['order'] + 1))
            if len(v) != expected_length:
                raise ValueError(f"Coefficients length {len(v)} doesn't match order {info.data['order']}")
        return v
class OEISTopologySchema(BaseModel):
    topology_sequence: List[int] = Field(description='OEIS A000081 sequence values')
    max_depth: int = Field(ge=1, le=10, description='Maximum tree depth')
    branching_factor: List[int] = Field(description='Branching factors per level')
    @field_validator('topology_sequence')
    @classmethod
    def validate_oeis_sequence(cls, v):
        known_sequence = [1, 1, 2, 4, 9, 20, 48, 115, 286, 719, 1842]
        for i, val in enumerate(v):
            if i < len(known_sequence) and val != known_sequence[i]:
                raise ValueError(f'Invalid OEIS A000081 sequence at position {i}: expected {known_sequence[i]}, got {val}')
        return v
class DTESNIntegrationConfigSchema(BaseModel):
    integration_mode: str = Field(pattern='^(standalone|membrane_coupled|full_dtesn)$')
    coupling_strength: float = Field(ge=0.0, le=1.0, description='Coupling strength between components')
    update_synchronization: bool = Field(description='Whether to synchronize component updates')
    performance_monitoring: bool = Field(default=True, description='Enable performance monitoring')
    esn_config: ESNReservoirConfigSchema = Field(description='ESN reservoir configuration')
    membrane_configs: List[PSystemMembraneSchema] = Field(description='P-System membrane configurations')
    bseries_config: BSeriesParametersSchema = Field(description='B-Series integration parameters')
    oeis_topology: OEISTopologySchema = Field(description='OEIS topology configuration')
    @model_validator(mode='before')
    @classmethod
    def validate_integration_consistency(cls, values):
        if isinstance(values, dict):
            mode = values.get('integration_mode')
            if mode == 'standalone' and values.get('coupling_strength', 0) > 0:
                raise ValueError('Standalone mode should have zero coupling strength')
            if mode == 'full_dtesn':
                required_fields = ['esn_config', 'membrane_configs', 'bseries_config', 'oeis_topology']
                for field in required_fields:
                    if not values.get(field):
                        raise ValueError(f'Full DTESN mode requires {field} configuration')
        return values
def validate_dtesn_data_structure(data: Dict[str, Any], data_type: DTESNDataType, config: DTESNValidationConfig=None) -> Dict[str, Any]:
    if config is None:
        config = DTESNValidationConfig()
    start_time = time.perf_counter()
    try:
        schema_mapping = {DTESNDataType.ESN_RESERVOIR_CONFIG: ESNReservoirConfigSchema, DTESNDataType.PSYSTEM_MEMBRANE: PSystemMembraneSchema, DTESNDataType.BSERIES_PARAMETERS: BSeriesParametersSchema, DTESNDataType.OEIS_TOPOLOGY: OEISTopologySchema, DTESNDataType.INTEGRATION_CONFIG: DTESNIntegrationConfigSchema}
        if data_type not in schema_mapping:
            raise HTTPException(status_code=400, detail=f'Unsupported DTESN data type: {data_type}')
        schema_class = schema_mapping[data_type]
        validated_model = schema_class(**data)
        validated_data = validated_model.model_dump()
        validation_time = (time.perf_counter() - start_time) * 1000
        if validation_time > config.max_validation_time_ms:
            logger.warning(f'DTESN validation took {validation_time:.2f}ms, exceeds limit of {config.max_validation_time_ms}ms')
        if config.enable_performance_tracking:
            validated_data['_validation_metadata'] = {'validation_time_ms': validation_time, 'data_type': data_type.value, 'timestamp': time.time()}
        logger.info(f'Successfully validated {data_type.value} structure in {validation_time:.2f}ms')
        return validated_data
    except Exception as e:
        validation_time = (time.perf_counter() - start_time) * 1000
        logger.error(f'DTESN validation failed for {data_type.value} after {validation_time:.2f}ms: {str(e)}')
        if isinstance(e, HTTPException):
            raise e
        else:
            raise HTTPException(status_code=400, detail=f'Invalid {data_type.value} structure: {str(e)}')
def sanitize_dtesn_numeric_data(data: Union[int, float, List, Dict]) -> Union[int, float, List, Dict]:
    if isinstance(data, (int, float)):
        if isinstance(data, float):
            if data != data:
                return 0.0
            if abs(data) == float('inf'):
                return 1.0 if data > 0 else -1.0
        return data
    elif isinstance(data, list):
        return [sanitize_dtesn_numeric_data(item) for item in data]
    elif isinstance(data, dict):
        return {key: sanitize_dtesn_numeric_data(value) for key, value in data.items()}
    else:
        return data
def normalize_dtesn_configuration(config_data: Dict[str, Any]) -> Dict[str, Any]:
    normalized = {}
    for key, value in config_data.items():
        normalized_key = re.sub('([a-z0-9])([A-Z])', '\\1_\\2', key).lower()
        normalized[normalized_key] = sanitize_dtesn_numeric_data(value)
    defaults = {'performance_monitoring': True, 'validation_enabled': True, 'error_handling': 'strict'}
    for key, default_value in defaults.items():
        if key not in normalized:
            normalized[key] = default_value
    return normalized
def validate_dtesn_integration_consistency(esn_config: Dict[str, Any], membrane_configs: List[Dict[str, Any]], bseries_config: Dict[str, Any]) -> Tuple[bool, List[str]]:
    issues = []
    esn_dimension = esn_config.get('input_dimension', 0)
    membrane_count = len(membrane_configs)
    if esn_dimension < membrane_count:
        issues.append(f'ESN input dimension ({esn_dimension}) insufficient for membrane count ({membrane_count})')
    timestep = bseries_config.get('timestep', 0)
    esn_leak_rate = esn_config.get('leak_rate', 1.0)
    if timestep > esn_leak_rate:
        issues.append(f'B-Series timestep ({timestep}) exceeds ESN leak rate ({esn_leak_rate})')
    max_depth = max((m.get('depth', 0) for m in membrane_configs), default=0)
    bseries_order = bseries_config.get('order', 1)
    if max_depth > bseries_order * 2:
        issues.append(f'Membrane depth ({max_depth}) exceeds recommended B-Series order limit ({bseries_order * 2})')
    return (len(issues) == 0, issues)
__all__ = ['DTESNDataType', 'DTESNValidationConfig', 'validate_dtesn_data_structure', 'sanitize_dtesn_numeric_data', 'normalize_dtesn_configuration', 'validate_dtesn_integration_consistency']