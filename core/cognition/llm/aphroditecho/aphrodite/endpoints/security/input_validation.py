import json
import re
import time
from typing import Any, Callable, Dict, List, Optional
import html
import logging
from fastapi import HTTPException, Request, Response
from starlette.middleware.base import BaseHTTPMiddleware
from pydantic import BaseModel, ValidationError
from .dtesn_validation import DTESNDataType, DTESNValidationConfig, validate_dtesn_data_structure, normalize_dtesn_configuration
logger = logging.getLogger(__name__)
SECURITY_PATTERNS = {'sql_injection': ['(?i)(union\\s+select|drop\\s+table|delete\\s+from|insert\\s+into)', '(?i)(select.*from|union.*select)', '(?i)(exec\\s*\\(|execute\\s*\\()', '(?i)(script\\s*:|javascript\\s*:|vbscript\\s*:)', '(?i)["\\\'].*?(?:or|and).*?[=<>]'], 'xss': ['<\\s*script[^>]*>.*?</\\s*script\\s*>', 'javascript\\s*:', 'on\\w+\\s*=', '<\\s*iframe[^>]*>', '<\\s*object[^>]*>', '<\\s*embed[^>]*>', '<\\s*link[^>]*>', '<\\s*meta[^>]*>', 'expression\\s*\\('], 'path_traversal': ['\\.\\.[\\\\/]', '[\\\\/]\\.\\.[\\\\/]', '%2e%2e%2f', '%2e%2e%5c', '\\.\\.%2f', '\\.\\.%5c'], 'command_injection': ['[\\|&;`$\\(\\)]', '(?i)(cat|ls|pwd|whoami|id|uname|ps|netstat|ifconfig)', '(?i)(rm\\s+-rf|sudo|su\\s+)', '(?i)(wget|curl|nc|telnet)']}
MAX_SIZES = {'total_request': 10 * 1024 * 1024, 'individual_field': 1024 * 1024, 'filename': 255, 'header_value': 8192, 'url_length': 2048, 'json_depth': 10}
class ValidationConfig(BaseModel):
    enable_sql_injection_protection: bool = True
    enable_xss_protection: bool = True
    enable_path_traversal_protection: bool = True
    enable_command_injection_protection: bool = True
    enable_size_limits: bool = True
    enable_content_type_validation: bool = True
    enable_dtesn_validation: bool = True
    max_request_size: int = MAX_SIZES['total_request']
    allowed_content_types: List[str] = ['application/json', 'application/x-www-form-urlencoded', 'text/plain', 'multipart/form-data']
    dtesn_config: Optional[DTESNValidationConfig] = None
def validate_string_content(content: str, field_name: str='input') -> str:
    if not isinstance(content, str):
        return str(content)
    if len(content) > MAX_SIZES['individual_field']:
        raise HTTPException(status_code=413, detail=f"Field '{field_name}' exceeds maximum size limit")
    for vulnerability_type, patterns in SECURITY_PATTERNS.items():
        for pattern in patterns:
            if re.search(pattern, content, re.IGNORECASE | re.DOTALL):
                logger.warning(f"Security violation detected in field '{field_name}': {vulnerability_type} pattern matched")
                raise HTTPException(status_code=400, detail=f"Invalid input detected in field '{field_name}': potential {vulnerability_type.replace('_', ' ')} attack")
    sanitized = html.escape(content, quote=True)
    return sanitized
def validate_json_structure(data: Any, max_depth: int=MAX_SIZES['json_depth'], current_depth: int=0) -> Any:
    if current_depth > max_depth:
        raise HTTPException(status_code=400, detail=f'JSON structure exceeds maximum depth of {max_depth}')
    if isinstance(data, dict):
        validated = {}
        for key, value in data.items():
            if not isinstance(key, str):
                raise HTTPException(status_code=400, detail='JSON keys must be strings')
            validated_key = validate_string_content(key, 'json_key')
            validated_value = validate_json_structure(value, max_depth, current_depth + 1)
            validated[validated_key] = validated_value
        return validated
    elif isinstance(data, list):
        if len(data) > 10000:
            raise HTTPException(status_code=400, detail='Array size exceeds maximum allowed length')
        return [validate_json_structure(item, max_depth, current_depth + 1) for item in data]
    elif isinstance(data, str):
        return validate_string_content(data, 'json_value')
    else:
        return data
def validate_dtesn_endpoint_data(data: Dict[str, Any], endpoint_path: str, config: ValidationConfig=None) -> Dict[str, Any]:
    if config is None or not config.enable_dtesn_validation:
        return data
    dtesn_config = config.dtesn_config or DTESNValidationConfig()
    dtesn_type_mapping = {'/dtesn/reservoir/config': DTESNDataType.ESN_RESERVOIR_CONFIG, '/dtesn/membrane/create': DTESNDataType.PSYSTEM_MEMBRANE, '/dtesn/bseries/parameters': DTESNDataType.BSERIES_PARAMETERS, '/dtesn/integration/config': DTESNDataType.INTEGRATION_CONFIG, '/dtesn/topology/oeis': DTESNDataType.OEIS_TOPOLOGY}
    for path_pattern, dtesn_type in dtesn_type_mapping.items():
        if path_pattern in endpoint_path:
            logger.info(f'Validating DTESN {dtesn_type.value} data for endpoint {endpoint_path}')
            if 'config' in data or 'configuration' in data:
                config_data = data.get('config') or data.get('configuration')
                normalized_config = normalize_dtesn_configuration(config_data)
                data['config'] = normalized_config
            return validate_dtesn_data_structure(data, dtesn_type, dtesn_config)
    if '/deep_tree_echo' in endpoint_path or '/dte/' in endpoint_path:
        logger.info(f'Applying DTESN normalization to Deep Tree Echo endpoint: {endpoint_path}')
        return normalize_dtesn_configuration(data)
    return data
def validate_file_upload(filename: str, content_type: str, file_size: int) -> None:
    if len(filename) > MAX_SIZES['filename']:
        raise HTTPException(status_code=400, detail=f"Filename too long (max {MAX_SIZES['filename']} characters)")
    if re.search(SECURITY_PATTERNS['path_traversal'][0], filename):
        raise HTTPException(status_code=400, detail='Invalid filename: path traversal detected')
    if re.search('[<>:"/\\\\|?*\\x00-\\x1f]', filename):
        raise HTTPException(status_code=400, detail='Invalid filename: contains forbidden characters')
    if file_size > MAX_SIZES['total_request']:
        raise HTTPException(status_code=413, detail=f"File size exceeds maximum limit of {MAX_SIZES['total_request']} bytes")
    dangerous_types = ['application/x-executable', 'application/x-msdownload', 'application/x-msdos-program', 'text/javascript', 'application/javascript']
    if content_type.lower() in dangerous_types:
        raise HTTPException(status_code=400, detail=f'Dangerous file type not allowed: {content_type}')
async def validate_request_input(request: Request, config: ValidationConfig=None) -> Dict[str, Any]:
    if config is None:
        config = ValidationConfig()
    validation_result = {'headers': {}, 'query_params': {}, 'path_params': {}, 'body': None, 'files': {}, 'validation_time': time.time()}
    if config.enable_size_limits and len(str(request.url)) > MAX_SIZES['url_length']:
        raise HTTPException(status_code=414, detail='URL too long')
    for name, value in request.headers.items():
        if len(value) > MAX_SIZES['header_value']:
            raise HTTPException(status_code=400, detail=f"Header '{name}' value too long")
        validation_result['headers'][name] = validate_string_content(value, f'header_{name}')
    for name, value in request.query_params.items():
        validation_result['query_params'][name] = validate_string_content(value, f'query_{name}')
    for name, value in request.path_params.items():
        validation_result['path_params'][name] = validate_string_content(value, f'path_{name}')
    content_type = request.headers.get('content-type', '').split(';')[0].strip()
    if config.enable_content_type_validation and content_type and (content_type not in config.allowed_content_types):
        raise HTTPException(status_code=415, detail=f'Unsupported content type: {content_type}')
    if request.method in ['POST', 'PUT', 'PATCH']:
        try:
            if content_type == 'application/json':
                body = await request.json()
                validation_result['body'] = validate_json_structure(body)
            elif content_type == 'application/x-www-form-urlencoded':
                form_data = await request.form()
                validated_form = {}
                for key, value in form_data.items():
                    validated_form[validate_string_content(key, 'form_key')] = validate_string_content(str(value), f'form_{key}')
                validation_result['body'] = validated_form
            elif content_type == 'multipart/form-data':
                form_data = await request.form()
                validated_form = {}
                validated_files = {}
                for key, value in form_data.items():
                    if hasattr(value, 'filename'):
                        validate_file_upload(value.filename, value.content_type, len(await value.read()))
                        validated_files[validate_string_content(key, 'file_key')] = {'filename': validate_string_content(value.filename, 'filename'), 'content_type': value.content_type, 'size': len(await value.read())}
                    else:
                        validated_form[validate_string_content(key, 'form_key')] = validate_string_content(str(value), f'form_{key}')
                validation_result['body'] = validated_form
                validation_result['files'] = validated_files
            elif content_type == 'text/plain':
                body = await request.body()
                validation_result['body'] = validate_string_content(body.decode('utf-8'), 'text_body')
        except ValidationError as e:
            raise HTTPException(status_code=400, detail=f'Request validation failed: {str(e)}')
        except json.JSONDecodeError as e:
            raise HTTPException(status_code=400, detail=f'Invalid JSON in request body: {str(e)}')
        except Exception as e:
            logger.error(f'Request validation error: {str(e)}')
            raise HTTPException(status_code=400, detail='Request validation failed')
    if validation_result['body'] and config.enable_dtesn_validation:
        try:
            validation_result['body'] = validate_dtesn_endpoint_data(validation_result['body'], str(request.url.path), config)
            logger.info(f'DTESN validation completed for endpoint: {request.url.path}')
        except Exception as e:
            logger.error(f'DTESN validation failed for {request.url.path}: {str(e)}')
    return validation_result
class InputValidationMiddleware(BaseHTTPMiddleware):
    def __init__(self, app, config: ValidationConfig=None):
        super().__init__(app)
        self.config = config or ValidationConfig()
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        start_time = time.time()
        try:
            if request.url.path in ['/health', '/metrics'] or request.url.path.startswith('/static/'):
                response = await call_next(request)
                return response
            validation_result = await validate_request_input(request, self.config)
            request.state.validation_result = validation_result
            request.state.input_validated = True
            response = await call_next(request)
            validation_time = time.time() - start_time
            response.headers['X-Input-Validated'] = 'true'
            response.headers['X-Validation-Time'] = f'{validation_time:.3f}'
            logger.info(f'Request validated successfully: {request.url.path} in {validation_time:.3f}s')
            return response
        except HTTPException:
            raise
        except Exception as e:
            logger.error(f'Input validation middleware error: {str(e)}')
            raise HTTPException(status_code=500, detail='Internal validation error')