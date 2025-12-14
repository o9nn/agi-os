"""
Input validation middleware for server-side security.

Implements comprehensive input validation and sanitization for all FastAPI endpoints,
protecting against common server-side vulnerabilities including injection attacks,
XSS, and malformed data.
"""

import json
import re
import time
from typing import Any, Callable, Dict, List, Optional
import html
import logging

from fastapi import HTTPException, Request, Response
from starlette.middleware.base import BaseHTTPMiddleware
from pydantic import BaseModel, ValidationError

# Import DTESN validation capabilities
from .dtesn_validation import (
    DTESNDataType, 
    DTESNValidationConfig,
    validate_dtesn_data_structure,
    normalize_dtesn_configuration
)

logger = logging.getLogger(__name__)

# Security patterns for common vulnerabilities
SECURITY_PATTERNS = {
    'sql_injection': [
        r'(?i)(union\s+select|drop\s+table|delete\s+from|insert\s+into)',
        r'(?i)(select.*from|union.*select)',
        r'(?i)(exec\s*\(|execute\s*\()',
        r'(?i)(script\s*:|javascript\s*:|vbscript\s*:)',
        r'(?i)["\'].*?(?:or|and).*?[=<>]',
    ],
    'xss': [
        r'<\s*script[^>]*>.*?</\s*script\s*>',
        r'javascript\s*:',
        r'on\w+\s*=',
        r'<\s*iframe[^>]*>',
        r'<\s*object[^>]*>',
        r'<\s*embed[^>]*>',
        r'<\s*link[^>]*>',
        r'<\s*meta[^>]*>',
        r'expression\s*\(',
    ],
    'path_traversal': [
        r'\.\.[\\/]',
        r'[\\/]\.\.[\\/]',
        r'%2e%2e%2f',
        r'%2e%2e%5c',
        r'\.\.%2f',
        r'\.\.%5c',
    ],
    'command_injection': [
        r'[\|&;`$\(\)]',
        r'(?i)(cat|ls|pwd|whoami|id|uname|ps|netstat|ifconfig)',
        r'(?i)(rm\s+-rf|sudo|su\s+)',
        r'(?i)(wget|curl|nc|telnet)',
    ]
}

# Maximum allowed sizes for different request components
MAX_SIZES = {
    'total_request': 10 * 1024 * 1024,  # 10MB
    'individual_field': 1024 * 1024,    # 1MB
    'filename': 255,
    'header_value': 8192,
    'url_length': 2048,
    'json_depth': 10,
}

class ValidationConfig(BaseModel):
    """Configuration for input validation."""
    enable_sql_injection_protection: bool = True
    enable_xss_protection: bool = True
    enable_path_traversal_protection: bool = True
    enable_command_injection_protection: bool = True
    enable_size_limits: bool = True
    enable_content_type_validation: bool = True
    enable_dtesn_validation: bool = True
    max_request_size: int = MAX_SIZES['total_request']
    allowed_content_types: List[str] = [
        'application/json',
        'application/x-www-form-urlencoded',
        'text/plain',
        'multipart/form-data'
    ]
    dtesn_config: Optional[DTESNValidationConfig] = None

def validate_string_content(content: str, field_name: str = "input") -> str:
    """
    Validate and sanitize string content against security vulnerabilities.
    
    Args:
        content: String content to validate
        field_name: Name of the field being validated
        
    Returns:
        Sanitized string content
        
    Raises:
        HTTPException: If dangerous patterns are detected
    """
    if not isinstance(content, str):
        return str(content)
    
    # Check size limits
    if len(content) > MAX_SIZES['individual_field']:
        raise HTTPException(
            status_code=413,
            detail=f"Field '{field_name}' exceeds maximum size limit"
        )
    
    # Check for security vulnerabilities
    for vulnerability_type, patterns in SECURITY_PATTERNS.items():
        for pattern in patterns:
            if re.search(pattern, content, re.IGNORECASE | re.DOTALL):
                logger.warning(
                    f"Security violation detected in field '{field_name}': "
                    f"{vulnerability_type} pattern matched"
                )
                raise HTTPException(
                    status_code=400,
                    detail=f"Invalid input detected in field '{field_name}': "
                           f"potential {vulnerability_type.replace('_', ' ')} attack"
                )
    
    # HTML encode to prevent XSS
    sanitized = html.escape(content, quote=True)
    
    return sanitized

def validate_json_structure(data: Any, max_depth: int = MAX_SIZES['json_depth'], 
                          current_depth: int = 0) -> Any:
    """
    Validate JSON structure for depth and content.
    
    Args:
        data: JSON data to validate
        max_depth: Maximum allowed nesting depth
        current_depth: Current nesting depth
        
    Returns:
        Validated JSON data
        
    Raises:
        HTTPException: If structure is invalid or too deep
    """
    if current_depth > max_depth:
        raise HTTPException(
            status_code=400,
            detail=f"JSON structure exceeds maximum depth of {max_depth}"
        )
    
    if isinstance(data, dict):
        validated = {}
        for key, value in data.items():
            # Validate key
            if not isinstance(key, str):
                raise HTTPException(
                    status_code=400,
                    detail="JSON keys must be strings"
                )
            
            validated_key = validate_string_content(key, "json_key")
            validated_value = validate_json_structure(value, max_depth, current_depth + 1)
            validated[validated_key] = validated_value
        return validated
    
    elif isinstance(data, list):
        if len(data) > 10000:  # Prevent massive arrays
            raise HTTPException(
                status_code=400,
                detail="Array size exceeds maximum allowed length"
            )
        return [validate_json_structure(item, max_depth, current_depth + 1) for item in data]
    
    elif isinstance(data, str):
        return validate_string_content(data, "json_value")
    
    else:
        return data


def validate_dtesn_endpoint_data(data: Dict[str, Any], endpoint_path: str, config: ValidationConfig = None) -> Dict[str, Any]:
    """
    Validate DTESN-specific endpoint data structures.
    
    Args:
        data: Request data to validate
        endpoint_path: Endpoint path to determine validation type
        config: Validation configuration
        
    Returns:
        Validated and normalized data
        
    Raises:
        HTTPException: If DTESN validation fails
    """
    if config is None or not config.enable_dtesn_validation:
        return data
        
    dtesn_config = config.dtesn_config or DTESNValidationConfig()
    
    # Determine DTESN data type based on endpoint path
    dtesn_type_mapping = {
        '/dtesn/reservoir/config': DTESNDataType.ESN_RESERVOIR_CONFIG,
        '/dtesn/membrane/create': DTESNDataType.PSYSTEM_MEMBRANE,
        '/dtesn/bseries/parameters': DTESNDataType.BSERIES_PARAMETERS,
        '/dtesn/integration/config': DTESNDataType.INTEGRATION_CONFIG,
        '/dtesn/topology/oeis': DTESNDataType.OEIS_TOPOLOGY,
    }
    
    # Check if this is a DTESN endpoint
    for path_pattern, dtesn_type in dtesn_type_mapping.items():
        if path_pattern in endpoint_path:
            logger.info(f"Validating DTESN {dtesn_type.value} data for endpoint {endpoint_path}")
            
            # Normalize configuration data first
            if 'config' in data or 'configuration' in data:
                config_data = data.get('config') or data.get('configuration')
                normalized_config = normalize_dtesn_configuration(config_data)
                data['config'] = normalized_config
            
            # Validate against DTESN schema
            return validate_dtesn_data_structure(data, dtesn_type, dtesn_config)
    
    # For Deep Tree Echo endpoints, apply general DTESN normalization
    if '/deep_tree_echo' in endpoint_path or '/dte/' in endpoint_path:
        logger.info(f"Applying DTESN normalization to Deep Tree Echo endpoint: {endpoint_path}")
        return normalize_dtesn_configuration(data)
    
    return data


def validate_file_upload(filename: str, content_type: str, file_size: int) -> None:
    """
    Validate file upload parameters.
    
    Args:
        filename: Name of uploaded file
        content_type: Content type of uploaded file
        file_size: Size of uploaded file in bytes
        
    Raises:
        HTTPException: If file upload is invalid or dangerous
    """
    # Check filename
    if len(filename) > MAX_SIZES['filename']:
        raise HTTPException(
            status_code=400,
            detail=f"Filename too long (max {MAX_SIZES['filename']} characters)"
        )
    
    # Check for path traversal in filename
    if re.search(SECURITY_PATTERNS['path_traversal'][0], filename):
        raise HTTPException(
            status_code=400,
            detail="Invalid filename: path traversal detected"
        )
    
    # Validate filename characters
    if re.search(r'[<>:"/\\|?*\x00-\x1f]', filename):
        raise HTTPException(
            status_code=400,
            detail="Invalid filename: contains forbidden characters"
        )
    
    # Check file size
    if file_size > MAX_SIZES['total_request']:
        raise HTTPException(
            status_code=413,
            detail=f"File size exceeds maximum limit of {MAX_SIZES['total_request']} bytes"
        )
    
    # Basic content type validation
    dangerous_types = [
        'application/x-executable',
        'application/x-msdownload',
        'application/x-msdos-program',
        'text/javascript',
        'application/javascript'
    ]
    
    if content_type.lower() in dangerous_types:
        raise HTTPException(
            status_code=400,
            detail=f"Dangerous file type not allowed: {content_type}"
        )

async def validate_request_input(request: Request, config: ValidationConfig = None) -> Dict[str, Any]:
    """
    Comprehensive request input validation.
    
    Args:
        request: FastAPI request object
        config: Validation configuration
        
    Returns:
        Dictionary containing validated request data
        
    Raises:
        HTTPException: If request fails validation
    """
    if config is None:
        config = ValidationConfig()
    
    validation_result = {
        'headers': {},
        'query_params': {},
        'path_params': {},
        'body': None,
        'files': {},
        'validation_time': time.time()
    }
    
    # Validate URL length
    if config.enable_size_limits and len(str(request.url)) > MAX_SIZES['url_length']:
        raise HTTPException(
            status_code=414,
            detail="URL too long"
        )
    
    # Validate headers
    for name, value in request.headers.items():
        if len(value) > MAX_SIZES['header_value']:
            raise HTTPException(
                status_code=400,
                detail=f"Header '{name}' value too long"
            )
        validation_result['headers'][name] = validate_string_content(value, f"header_{name}")
    
    # Validate query parameters
    for name, value in request.query_params.items():
        validation_result['query_params'][name] = validate_string_content(value, f"query_{name}")
    
    # Validate path parameters
    for name, value in request.path_params.items():
        validation_result['path_params'][name] = validate_string_content(value, f"path_{name}")
    
    # Validate content type
    content_type = request.headers.get('content-type', '').split(';')[0].strip()
    if (config.enable_content_type_validation and 
        content_type and 
        content_type not in config.allowed_content_types):
        raise HTTPException(
            status_code=415,
            detail=f"Unsupported content type: {content_type}"
        )
    
    # Validate request body if present
    if request.method in ['POST', 'PUT', 'PATCH']:
        try:
            if content_type == 'application/json':
                body = await request.json()
                validation_result['body'] = validate_json_structure(body)
            elif content_type == 'application/x-www-form-urlencoded':
                form_data = await request.form()
                validated_form = {}
                for key, value in form_data.items():
                    validated_form[validate_string_content(key, "form_key")] = \
                        validate_string_content(str(value), f"form_{key}")
                validation_result['body'] = validated_form
            elif content_type == 'multipart/form-data':
                form_data = await request.form()
                validated_form = {}
                validated_files = {}
                
                for key, value in form_data.items():
                    if hasattr(value, 'filename'):  # File upload
                        validate_file_upload(value.filename, value.content_type, len(await value.read()))
                        validated_files[validate_string_content(key, "file_key")] = {
                            'filename': validate_string_content(value.filename, "filename"),
                            'content_type': value.content_type,
                            'size': len(await value.read())
                        }
                    else:  # Regular form field
                        validated_form[validate_string_content(key, "form_key")] = \
                            validate_string_content(str(value), f"form_{key}")
                
                validation_result['body'] = validated_form
                validation_result['files'] = validated_files
            elif content_type == 'text/plain':
                body = await request.body()
                validation_result['body'] = validate_string_content(body.decode('utf-8'), "text_body")
        
        except ValidationError as e:
            raise HTTPException(
                status_code=400,
                detail=f"Request validation failed: {str(e)}"
            )
        except json.JSONDecodeError as e:
            raise HTTPException(
                status_code=400,
                detail=f"Invalid JSON in request body: {str(e)}"
            )
        except Exception as e:
            logger.error(f"Request validation error: {str(e)}")
            raise HTTPException(
                status_code=400,
                detail="Request validation failed"
            )
    
    # Apply DTESN-specific validation if enabled and body is present
    if validation_result['body'] and config.enable_dtesn_validation:
        try:
            validation_result['body'] = validate_dtesn_endpoint_data(
                validation_result['body'], 
                str(request.url.path),
                config
            )
            logger.info(f"DTESN validation completed for endpoint: {request.url.path}")
        except Exception as e:
            logger.error(f"DTESN validation failed for {request.url.path}: {str(e)}")
            # Don't fail the entire request if DTESN validation fails, just log it
            # This ensures backward compatibility
    
    return validation_result

class InputValidationMiddleware(BaseHTTPMiddleware):
    """
    FastAPI middleware for comprehensive input validation.
    
    This middleware validates all incoming requests for security vulnerabilities
    and enforces size limits and content type restrictions.
    """
    
    def __init__(self, app, config: ValidationConfig = None):
        """Initialize input validation middleware."""
        super().__init__(app)
        self.config = config or ValidationConfig()
        
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        """
        Validate request input before processing.
        
        Args:
            request: FastAPI request object
            call_next: Next middleware or route handler
            
        Returns:
            Response from downstream handlers
            
        Raises:
            HTTPException: If request fails security validation
        """
        start_time = time.time()
        
        try:
            # Skip validation for health checks and static files
            if request.url.path in ['/health', '/metrics'] or request.url.path.startswith('/static/'):
                response = await call_next(request)
                return response
            
            # Validate request input
            validation_result = await validate_request_input(request, self.config)
            
            # Store validation result in request state for downstream use
            request.state.validation_result = validation_result
            request.state.input_validated = True
            
            # Process request
            response = await call_next(request)
            
            # Add validation headers
            validation_time = time.time() - start_time
            response.headers["X-Input-Validated"] = "true"
            response.headers["X-Validation-Time"] = f"{validation_time:.3f}"
            
            logger.info(f"Request validated successfully: {request.url.path} in {validation_time:.3f}s")
            
            return response
            
        except HTTPException:
            # Re-raise HTTP exceptions from validation
            raise
        except Exception as e:
            logger.error(f"Input validation middleware error: {str(e)}")
            raise HTTPException(
                status_code=500,
                detail="Internal validation error"
            )