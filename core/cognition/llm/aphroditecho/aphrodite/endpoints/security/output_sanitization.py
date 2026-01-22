import json
import re
import time
import logging
from typing import Any, Callable, Dict, List
from fastapi import Request, Response, HTTPException
from fastapi.responses import JSONResponse, StreamingResponse
from starlette.middleware.base import BaseHTTPMiddleware
from pydantic import BaseModel
logger = logging.getLogger(__name__)
SECURITY_HEADERS = {'X-Content-Type-Options': 'nosniff', 'X-Frame-Options': 'DENY', 'X-XSS-Protection': '1; mode=block', 'Strict-Transport-Security': 'max-age=31536000; includeSubDomains', 'Referrer-Policy': 'strict-origin-when-cross-origin', 'Content-Security-Policy': "default-src 'self'; script-src 'self' 'unsafe-inline'; style-src 'self' 'unsafe-inline'", 'Permissions-Policy': 'geolocation=(), microphone=(), camera=()', 'Cache-Control': 'no-cache, no-store, must-revalidate', 'Pragma': 'no-cache', 'Expires': '0'}
SENSITIVE_DATA_PATTERNS = ['(?i)(password|passwd|pwd)[\\s]*[:=][\\s]*["\\\']?([^"\\\'\\s]+)', '(?i)(api[_-]?key|apikey|access[_-]?token)[\\s]*[:=][\\s]*["\\\']?([^"\\\'\\s]+)', '(?i)(secret[_-]?key|secret)[\\s]*[:=][\\s]*["\\\']?([^"\\\'\\s]+)', '(?i)(private[_-]?key|priv[_-]?key)[\\s]*[:=][\\s]*["\\\']?([^"\\\'\\s]+)', '(?i)(session[_-]?id|sessionid)[\\s]*[:=][\\s]*["\\\']?([^"\\\'\\s]+)', '(?i)(auth[_-]?token|authorization)[\\s]*[:=][\\s]*["\\\']?([^"\\\'\\s]+)', '(?i)(database[_-]?url|db[_-]?url)[\\s]*[:=][\\s]*["\\\']?([^"\\\'\\s]+)', '[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}', '\\b\\d{4}[\\s-]?\\d{4}[\\s-]?\\d{4}[\\s-]?\\d{4}\\b', '\\b\\d{3}-\\d{2}-\\d{4}\\b']
SENSITIVE_FIELD_NAMES = ['password', 'passwd', 'pwd', 'secret', 'token', 'key', 'auth', 'authorization', 'session', 'sessionid', 'cookie', 'private', 'confidential', 'internal', 'debug', 'trace', 'log', 'error', 'exception', 'stack', 'backtrace']
class SanitizationConfig(BaseModel):
    enable_html_sanitization: bool = True
    enable_json_sanitization: bool = True
    enable_sensitive_data_filtering: bool = True
    enable_security_headers: bool = True
    enable_error_message_sanitization: bool = True
    max_response_size: int = 50 * 1024 * 1024
    allowed_html_tags: List[str] = ['p', 'br', 'strong', 'em', 'ul', 'ol', 'li', 'h1', 'h2', 'h3']
    custom_security_headers: Dict[str, str] = {}
def sanitize_html_content(content: str, allowed_tags: List[str]=None) -> str:
    if not isinstance(content, str):
        return str(content)
    if allowed_tags is None:
        allowed_tags = ['p', 'br', 'strong', 'em']
    content = re.sub('<\\s*script[^>]*>.*?</\\s*script\\s*>', '', content, flags=re.IGNORECASE | re.DOTALL)
    dangerous_attrs = ['onclick', 'onload', 'onerror', 'onmouseover', 'onfocus', 'onblur', 'onchange']
    for attr in dangerous_attrs:
        content = re.sub(f"""{attr}\\s*=\\s*["'][^"']*["']""", '', content, flags=re.IGNORECASE)
    content = re.sub('(javascript|data|vbscript):', '', content, flags=re.IGNORECASE)
    if allowed_tags:
        tag_pattern = '|'.join(allowed_tags)
        content = re.sub(f'<(?!/?(?:{tag_pattern})\\b)[^>]*>', '', content, flags=re.IGNORECASE)
    return content
def filter_sensitive_data(content: str) -> str:
    if not isinstance(content, str):
        return str(content)
    filtered_content = content
    for pattern in SENSITIVE_DATA_PATTERNS:
        filtered_content = re.sub(pattern, lambda m: m.group(0).replace(m.group(-1), '[REDACTED]'), filtered_content, flags=re.IGNORECASE)
    return filtered_content
def sanitize_json_data(data: Any, max_depth: int=10, current_depth: int=0) -> Any:
    if current_depth > max_depth:
        return '[DEPTH_LIMIT_EXCEEDED]'
    if isinstance(data, dict):
        sanitized = {}
        for key, value in data.items():
            key_lower = str(key).lower()
            if any((sensitive in key_lower for sensitive in SENSITIVE_FIELD_NAMES)):
                sanitized[key] = '[REDACTED]'
            else:
                sanitized[key] = sanitize_json_data(value, max_depth, current_depth + 1)
        return sanitized
    elif isinstance(data, list):
        return [sanitize_json_data(item, max_depth, current_depth + 1) for item in data]
    elif isinstance(data, str):
        return filter_sensitive_data(data)
    else:
        return data
def sanitize_error_message(error_message: str) -> str:
    if not isinstance(error_message, str):
        return str(error_message)
    dangerous_patterns = ['File "([^"]+)"', 'line \\d+', 'in <module>', 'Traceback \\(most recent call last\\):.*', 'at \\w+\\.\\w+\\([^)]+\\)', '(?i)(database|db|sql) error:.*', '(?i)connection (refused|failed|timeout).*']
    sanitized = error_message
    for pattern in dangerous_patterns:
        sanitized = re.sub(pattern, '[INTERNAL_ERROR_DETAILS_REMOVED]', sanitized, flags=re.DOTALL)
    sanitized = filter_sensitive_data(sanitized)
    if len(sanitized) > 500:
        sanitized = sanitized[:500] + '... [TRUNCATED]'
    return sanitized
async def sanitize_response_output(response: Response, config: SanitizationConfig=None) -> Response:
    if config is None:
        config = SanitizationConfig()
    try:
        if config.enable_security_headers:
            for name, value in SECURITY_HEADERS.items():
                response.headers[name] = value
            for name, value in config.custom_security_headers.items():
                response.headers[name] = value
        content_type = response.headers.get('content-type', '').split(';')[0].strip()
        if hasattr(response, 'body') and response.body:
            body_content = response.body.decode('utf-8') if isinstance(response.body, bytes) else str(response.body)
            if len(body_content) > config.max_response_size:
                logger.warning(f'Response size {len(body_content)} exceeds limit {config.max_response_size}')
                return Response(content='Response too large', status_code=413, headers=dict(response.headers))
            if content_type == 'application/json' and config.enable_json_sanitization:
                try:
                    json_data = json.loads(body_content)
                    sanitized_data = sanitize_json_data(json_data)
                    sanitized_content = json.dumps(sanitized_data, ensure_ascii=False, separators=(',', ':'))
                    response.body = sanitized_content.encode('utf-8')
                except json.JSONDecodeError:
                    if config.enable_sensitive_data_filtering:
                        sanitized_content = filter_sensitive_data(body_content)
                        response.body = sanitized_content.encode('utf-8')
            elif content_type == 'text/html' and config.enable_html_sanitization:
                sanitized_content = sanitize_html_content(body_content, config.allowed_html_tags)
                if config.enable_sensitive_data_filtering:
                    sanitized_content = filter_sensitive_data(sanitized_content)
                response.body = sanitized_content.encode('utf-8')
            elif config.enable_sensitive_data_filtering:
                sanitized_content = filter_sensitive_data(body_content)
                response.body = sanitized_content.encode('utf-8')
        if hasattr(response, 'body') and response.body:
            response.headers['content-length'] = str(len(response.body))
        return response
    except Exception as e:
        logger.error(f'Output sanitization error: {str(e)}')
        return Response(content='Response sanitization error', status_code=500, headers=SECURITY_HEADERS)
class OutputSanitizationMiddleware(BaseHTTPMiddleware):
    def __init__(self, app, config: SanitizationConfig=None):
        super().__init__(app)
        self.config = config or SanitizationConfig()
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        start_time = time.time()
        try:
            response = await call_next(request)
            if isinstance(response, StreamingResponse):
                if self.config.enable_security_headers:
                    for name, value in SECURITY_HEADERS.items():
                        response.headers[name] = value
                    for name, value in self.config.custom_security_headers.items():
                        response.headers[name] = value
                response.headers['X-Output-Sanitized'] = 'headers-only'
                return response
            sanitized_response = await sanitize_response_output(response, self.config)
            sanitization_time = time.time() - start_time
            sanitized_response.headers['X-Output-Sanitized'] = 'true'
            sanitized_response.headers['X-Sanitization-Time'] = f'{sanitization_time:.3f}'
            logger.info(f'Response sanitized: {request.url.path} in {sanitization_time:.3f}s')
            return sanitized_response
        except HTTPException:
            raise
        except Exception as e:
            logger.error(f'Output sanitization middleware error: {str(e)}')
            sanitized_error = sanitize_error_message(str(e))
            return Response(content=f'Output sanitization error: {sanitized_error}', status_code=500, headers=SECURITY_HEADERS)
class ErrorSanitizationMiddleware(BaseHTTPMiddleware):
    def __init__(self, app, config: SanitizationConfig=None):
        super().__init__(app)
        self.config = config or SanitizationConfig()
    async def dispatch(self, request: Request, call_next: Callable) -> Response:
        try:
            response = await call_next(request)
            if response.status_code >= 400:
                if hasattr(response, 'body') and response.body:
                    body_content = response.body.decode('utf-8') if isinstance(response.body, bytes) else str(response.body)
                    try:
                        json_data = json.loads(body_content)
                        if 'detail' in json_data:
                            json_data['detail'] = sanitize_error_message(json_data['detail'])
                        if 'message' in json_data:
                            json_data['message'] = sanitize_error_message(json_data['message'])
                        sanitized_content = json.dumps(json_data, ensure_ascii=False)
                        response.body = sanitized_content.encode('utf-8')
                        response.headers['content-length'] = str(len(response.body))
                    except json.JSONDecodeError:
                        sanitized_content = sanitize_error_message(body_content)
                        response.body = sanitized_content.encode('utf-8')
                        response.headers['content-length'] = str(len(response.body))
                if self.config.enable_security_headers:
                    for name, value in SECURITY_HEADERS.items():
                        response.headers[name] = value
                response.headers['X-Error-Sanitized'] = 'true'
            return response
        except Exception as e:
            logger.error(f'Error sanitization middleware error: {str(e)}')
            return JSONResponse(content={'detail': 'An internal error occurred'}, status_code=500, headers=SECURITY_HEADERS)