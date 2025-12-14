# Server-Side Data Validation Implementation

## Task 7.1.2: Build Server-Side Data Validation

This document describes the comprehensive server-side data validation system implemented for the Aphrodite Engine's Deep Tree Echo architecture.

## Overview

The server-side validation system provides comprehensive input validation, data sanitization, and schema validation for all endpoints, with specialized support for DTESN (Deep Tree Echo State Network) data structures.

## Implementation Components

### 1. DTESN Schema Validation (`dtesn_validation.py`)

Provides validation schemas and functions for complex DTESN data structures:

#### Key Schemas:
- **ESNReservoirConfigSchema**: Validates ESN reservoir configurations
  - `reservoir_size`: 1-10,000 nodes
  - `spectral_radius`: 0.1-1.5 (warns if ≥1.0 for stability)
  - `input_dimension`: 1-1,000 inputs
  - `leak_rate`, `input_scaling`, `noise_level`: Bounded numeric values

- **PSystemMembraneSchema**: Validates P-System membrane configurations
  - `membrane_id`: Alphanumeric identifier with dashes/underscores
  - `depth`: 0-10 levels deep
  - `capacity`: 1-1,000,000 objects
  - `rules`: Array of evolution rules with required 'type' and 'action' fields

- **BSeriesParametersSchema**: Validates B-Series integration parameters
  - `order`: 1-10 expansion order
  - `timestep`: 1e-6 to 1.0 seconds
  - `method`: euler|rk2|rk4|dopri integration methods
  - `coefficients`: Length must match triangular number for order

- **OEISTopologySchema**: Validates OEIS A000081 compliant topologies
  - `topology_sequence`: Must match known OEIS A000081 sequence
  - `max_depth`: 1-10 tree levels
  - `branching_factor`: Per-level branching specifications

#### Key Functions:
- `validate_dtesn_data_structure()`: Validates data against DTESN schemas
- `normalize_dtesn_configuration()`: Converts camelCase to snake_case, adds defaults
- `validate_dtesn_integration_consistency()`: Checks component compatibility

### 2. Data Sanitization (`data_sanitization.py`)

Comprehensive data cleaning and normalization pipelines:

#### Sanitization Levels:
- **STRICT**: Maximum sanitization, may alter data significantly
- **MODERATE**: Balanced sanitization, preserves structure
- **LENIENT**: Minimal sanitization, preserves original data

#### Key Functions:
- `sanitize_string()`: HTML escaping, control character removal, XSS protection
- `sanitize_numeric()`: NaN/infinity handling, precision limits
- `sanitize_array()`: Length limits, recursive element sanitization
- `sanitize_object()`: Depth limits, key/value sanitization
- `sanitize_data_value()`: Universal data type sanitization

#### Specialized Pipelines:
- `dtesn_sanitizer`: DTESN-optimized sanitization
- `json_sanitizer`: JSON structure sanitization
- `html_sanitizer`: HTML content sanitization (strict)
- `numeric_sanitizer`: Numeric array sanitization (lenient)

### 3. Enhanced Input Validation (`input_validation.py`)

Extended the existing input validation middleware:

#### New Features:
- DTESN endpoint detection and specialized validation
- Integration with DTESN schemas for endpoint-specific validation
- Configuration normalization for Deep Tree Echo endpoints
- Backward compatibility with existing security middleware

#### Endpoint Integration:
- `/dtesn/reservoir/config` → ESN reservoir validation
- `/dtesn/membrane/create` → P-System membrane validation
- `/dtesn/bseries/parameters` → B-Series parameter validation
- `/dtesn/integration/config` → Full integration validation
- `/deep_tree_echo/*` → General DTESN normalization

## Security Features

### XSS Protection
- HTML entity escaping for all string inputs
- JavaScript protocol removal (`javascript:`, `vbscript:`, `data:`)
- Dangerous HTML tag filtering
- Script tag content removal

### Injection Attack Prevention
- SQL injection pattern detection
- Command injection protection
- Path traversal prevention
- Malformed data structure validation

### Data Integrity
- NaN and infinity value handling
- Numeric precision limits
- Unicode normalization (NFKC)
- Control character removal

## Configuration Options

### DTESNValidationConfig
```python
DTESNValidationConfig(
    max_reservoir_size=10000,
    max_membrane_depth=10,
    max_bseries_order=10,
    enable_performance_tracking=True,
    max_validation_time_ms=100
)
```

### SanitizationConfig  
```python
SanitizationConfig(
    sanitization_level=SanitizationLevel.MODERATE,
    handle_nan=True,
    handle_infinity=True,
    escape_html=True,
    numeric_precision=10
)
```

### ValidationConfig (Enhanced)
```python
ValidationConfig(
    enable_dtesn_validation=True,
    dtesn_config=DTESNValidationConfig(),
    # ... existing security options
)
```

## Performance Features

- Validation time tracking and warnings
- Configurable timeout limits (default: 100ms)
- Efficient nested structure processing
- Optional performance metadata in responses

## Usage Examples

### Basic DTESN Validation
```python
from aphrodite.endpoints.security import validate_dtesn_data_structure, DTESNDataType

esn_config = {
    "reservoir_size": 100,
    "input_dimension": 10,
    "spectral_radius": 0.95,
    "leak_rate": 0.1,
    "input_scaling": 1.0,
    "noise_level": 0.01
}

validated = validate_dtesn_data_structure(
    esn_config, 
    DTESNDataType.ESN_RESERVOIR_CONFIG
)
```

### Data Sanitization
```python
from aphrodite.endpoints.security import dtesn_sanitizer

dangerous_data = {
    "user_input": "<script>alert('xss')</script>",
    "numeric_data": [1.0, float('nan'), float('inf')]
}

safe_data = dtesn_sanitizer(dangerous_data)
```

### Middleware Integration
```python
from aphrodite.endpoints.security import InputValidationMiddleware, ValidationConfig, DTESNValidationConfig

app.add_middleware(
    InputValidationMiddleware,
    config=ValidationConfig(
        enable_dtesn_validation=True,
        dtesn_config=DTESNValidationConfig(max_reservoir_size=5000)
    )
)
```

## Testing

Comprehensive test suites validate:
- Schema validation for all DTESN components
- Data sanitization across all data types
- Error handling and edge cases
- Performance characteristics
- Integration with existing security systems

Test files:
- `tests/entrypoints/security/test_dtesn_validation.py`
- `tests/entrypoints/security/test_data_sanitization.py`

## Task 7.1.2 Acceptance Criteria ✅

- ✅ **Comprehensive input validation for all endpoints**: Implemented with DTESN-specific schemas and general validation middleware
- ✅ **Data sanitization and normalization pipelines**: Built with configurable sanitization levels and specialized pipelines
- ✅ **Schema validation for complex DTESN data structures**: Created schemas for ESN, P-System, B-Series, and OEIS components
- ✅ **All input data validated and sanitized server-side**: Integrated validation runs automatically on all requests

## Integration with Phase 7 Architecture

This validation system integrates seamlessly with the Phase 7 server-side data processing architecture:

- **Phase 7.1.1** (Multi-Source Data Integration): Validates data from multiple engine components
- **Phase 7.1.3** (Backend Data Processing): Ensures clean data enters processing pipelines
- **Phase 7.2** (Template & Response Generation): Sanitizes data before template rendering
- **Phase 7.3** (Backend Integration): Provides middleware for comprehensive request processing

## Future Enhancements

- Custom validation rules for domain-specific DTESN configurations
- Real-time validation performance monitoring and optimization
- Advanced ML-based anomaly detection in DTESN parameters
- Integration with distributed validation across DTESN clusters