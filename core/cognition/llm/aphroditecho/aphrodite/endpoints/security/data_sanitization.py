"""
Comprehensive Data Sanitization and Normalization Pipelines.

This module implements data sanitization and normalization pipelines for 
server-side processing, ensuring all input data is properly cleaned and 
formatted before being processed by the Aphrodite Engine.

Implements Task 7.1.2 requirements:
- Data sanitization and normalization pipelines
- Support for multiple data formats and structures
- Integration with DTESN validation systems
"""

import re
import html
import urllib.parse
from typing import Any, Dict, List, Union, Callable
from enum import Enum
import logging
import time
from decimal import Decimal, InvalidOperation

from fastapi import HTTPException
from pydantic import BaseModel

logger = logging.getLogger(__name__)


class SanitizationLevel(str, Enum):
    """Levels of data sanitization to apply."""
    STRICT = "strict"      # Maximum sanitization, may alter data significantly
    MODERATE = "moderate"  # Balanced sanitization, preserves most data structure  
    LENIENT = "lenient"   # Minimal sanitization, preserves original data as much as possible


class DataFormat(str, Enum):
    """Supported data formats for sanitization."""
    JSON = "json"
    XML = "xml"
    HTML = "html"
    TEXT = "text"
    URL_ENCODED = "url_encoded"
    DTESN_CONFIG = "dtesn_config"
    NUMERIC_ARRAY = "numeric_array"


class SanitizationConfig(BaseModel):
    """Configuration for data sanitization pipelines."""
    
    # General sanitization settings
    sanitization_level: SanitizationLevel = SanitizationLevel.MODERATE
    preserve_structure: bool = True
    max_string_length: int = 10000
    max_array_length: int = 1000
    max_object_depth: int = 20
    
    # HTML/XSS sanitization
    allow_html_tags: List[str] = []
    escape_html: bool = True
    remove_javascript: bool = True
    
    # Numeric data sanitization
    handle_infinity: bool = True
    handle_nan: bool = True
    numeric_precision: int = 10
    
    # String sanitization
    normalize_unicode: bool = True
    remove_control_chars: bool = True
    trim_whitespace: bool = True
    
    # URL/Path sanitization
    decode_url_encoding: bool = True
    normalize_paths: bool = True
    
    # Performance settings
    max_processing_time_ms: int = 500
    enable_caching: bool = True


def sanitize_string(
    value: str, 
    config: SanitizationConfig = None,
    field_name: str = "string_field"
) -> str:
    """
    Sanitize string content based on configuration.
    
    Args:
        value: String value to sanitize
        config: Sanitization configuration  
        field_name: Name of the field for logging
        
    Returns:
        Sanitized string value
    """
    if config is None:
        config = SanitizationConfig()
    
    if not isinstance(value, str):
        value = str(value)
    
    original_length = len(value)
    
    # Apply length limits
    if len(value) > config.max_string_length:
        logger.warning(f"Truncating {field_name} from {len(value)} to {config.max_string_length} characters")
        value = value[:config.max_string_length]
    
    # Remove control characters
    if config.remove_control_chars:
        value = re.sub(r'[\x00-\x08\x0b\x0c\x0e-\x1f\x7f]', '', value)
    
    # Normalize Unicode
    if config.normalize_unicode:
        import unicodedata
        value = unicodedata.normalize('NFKC', value)
    
    # Handle URL decoding
    if config.decode_url_encoding:
        try:
            decoded = urllib.parse.unquote(value)
            # Only use decoded version if it's different and safer
            if decoded != value and len(decoded) <= len(value) * 2:
                value = decoded
        except Exception:
            pass  # Keep original value if decoding fails
    
    # HTML escaping
    if config.escape_html:
        # Always escape basic HTML entities
        value = html.escape(value, quote=True)
        
        # Remove JavaScript protocols
        if config.remove_javascript:
            value = re.sub(r'javascript\s*:', '', value, flags=re.IGNORECASE)
            value = re.sub(r'vbscript\s*:', '', value, flags=re.IGNORECASE)
            value = re.sub(r'data\s*:', '', value, flags=re.IGNORECASE)
    
    # Trim whitespace
    if config.trim_whitespace:
        value = value.strip()
    
    # Apply sanitization level-specific rules
    if config.sanitization_level == SanitizationLevel.STRICT:
        # Remove all non-printable characters
        value = ''.join(char for char in value if char.isprintable() or char.isspace())
        # Remove potentially dangerous patterns
        dangerous_patterns = [
            r'<[^>]+>',  # HTML tags
            r'&[#\w]+;', # HTML entities (except basic ones)
            r'\$\([^)]*\)', # Shell command substitution
            r'`[^`]*`',  # Backticks
        ]
        for pattern in dangerous_patterns:
            value = re.sub(pattern, '', value, flags=re.IGNORECASE)
    
    if original_length != len(value):
        logger.info(f"Sanitized {field_name}: {original_length} -> {len(value)} characters")
    
    return value


def sanitize_numeric(
    value: Union[int, float, str], 
    config: SanitizationConfig = None,
    field_name: str = "numeric_field"
) -> Union[int, float]:
    """
    Sanitize numeric values, handling special cases like NaN, infinity.
    
    Args:
        value: Numeric value to sanitize
        config: Sanitization configuration
        field_name: Name of the field for logging
        
    Returns:
        Sanitized numeric value
        
    Raises:
        ValueError: If value cannot be converted to a valid number
    """
    if config is None:
        config = SanitizationConfig()
    
    # Convert string representations to numbers
    if isinstance(value, str):
        # Remove whitespace and common currency symbols
        cleaned = value.strip().replace(',', '').replace('$', '').replace('%', '')
        
        try:
            # Try integer first
            if '.' not in cleaned and 'e' not in cleaned.lower():
                value = int(cleaned)
            else:
                value = float(cleaned)
        except ValueError:
            raise ValueError(f"Cannot convert '{value}' to numeric value in field '{field_name}'")
    
    # Handle special float values
    if isinstance(value, float):
        # Handle NaN
        if value != value:  # NaN check
            if config.handle_nan:
                logger.warning(f"NaN value detected in {field_name}, replacing with 0.0")
                return 0.0
            else:
                raise ValueError(f"NaN value not allowed in field '{field_name}'")
        
        # Handle infinity
        if abs(value) == float('inf'):
            if config.handle_infinity:
                max_val = 10 ** config.numeric_precision
                replacement = max_val if value > 0 else -max_val
                logger.warning(f"Infinity value detected in {field_name}, replacing with {replacement}")
                return replacement
            else:
                raise ValueError(f"Infinity value not allowed in field '{field_name}'")
        
        # Apply precision limits
        if config.numeric_precision > 0:
            try:
                decimal_val = Decimal(str(value))
                # Quantize to specified precision
                quantized = decimal_val.quantize(Decimal(10) ** -config.numeric_precision)
                return float(quantized)
            except (InvalidOperation, ValueError):
                pass  # Keep original value if quantization fails
    
    return value


def sanitize_array(
    value: List[Any], 
    config: SanitizationConfig = None,
    field_name: str = "array_field"
) -> List[Any]:
    """
    Sanitize array/list data structures.
    
    Args:
        value: List to sanitize
        config: Sanitization configuration
        field_name: Name of the field for logging
        
    Returns:
        Sanitized list
    """
    if config is None:
        config = SanitizationConfig()
    
    if not isinstance(value, list):
        raise ValueError(f"Expected list type for field '{field_name}', got {type(value)}")
    
    original_length = len(value)
    
    # Apply length limits
    if len(value) > config.max_array_length:
        logger.warning(f"Truncating {field_name} from {len(value)} to {config.max_array_length} elements")
        value = value[:config.max_array_length]
    
    # Recursively sanitize elements
    sanitized = []
    for i, item in enumerate(value):
        try:
            sanitized_item = sanitize_data_value(
                item, 
                config, 
                f"{field_name}[{i}]"
            )
            sanitized.append(sanitized_item)
        except Exception as e:
            logger.warning(f"Failed to sanitize {field_name}[{i}]: {str(e)}")
            if config.sanitization_level != SanitizationLevel.STRICT:
                sanitized.append(None)  # Keep placeholder
    
    if original_length != len(sanitized):
        logger.info(f"Sanitized {field_name}: {original_length} -> {len(sanitized)} elements")
    
    return sanitized


def sanitize_object(
    value: Dict[str, Any], 
    config: SanitizationConfig = None,
    field_name: str = "object_field",
    current_depth: int = 0
) -> Dict[str, Any]:
    """
    Sanitize dictionary/object data structures.
    
    Args:
        value: Dictionary to sanitize
        config: Sanitization configuration
        field_name: Name of the field for logging
        current_depth: Current nesting depth
        
    Returns:
        Sanitized dictionary
    """
    if config is None:
        config = SanitizationConfig()
    
    if not isinstance(value, dict):
        raise ValueError(f"Expected dict type for field '{field_name}', got {type(value)}")
    
    # Check depth limits
    if current_depth > config.max_object_depth:
        logger.warning(f"Object depth limit exceeded for {field_name} at depth {current_depth}")
        return {"__truncated__": True, "__depth__": current_depth}
    
    sanitized = {}
    
    for key, val in value.items():
        # Sanitize key names
        try:
            sanitized_key = sanitize_string(str(key), config, f"{field_name}.key")
            
            # Sanitize values recursively
            sanitized_value = sanitize_data_value(
                val, 
                config, 
                f"{field_name}.{key}",
                current_depth + 1
            )
            
            sanitized[sanitized_key] = sanitized_value
            
        except Exception as e:
            logger.warning(f"Failed to sanitize {field_name}.{key}: {str(e)}")
            if config.sanitization_level != SanitizationLevel.STRICT:
                sanitized[str(key)] = None  # Keep placeholder
    
    return sanitized


def sanitize_data_value(
    value: Any, 
    config: SanitizationConfig = None,
    field_name: str = "data",
    current_depth: int = 0
) -> Any:
    """
    Sanitize any data value based on its type.
    
    Args:
        value: Value to sanitize
        config: Sanitization configuration
        field_name: Name of the field for logging
        current_depth: Current nesting depth
        
    Returns:
        Sanitized value
    """
    if config is None:
        config = SanitizationConfig()
    
    if value is None:
        return None
    
    try:
        if isinstance(value, str):
            return sanitize_string(value, config, field_name)
        
        elif isinstance(value, (int, float)):
            return sanitize_numeric(value, config, field_name)
        
        elif isinstance(value, list):
            return sanitize_array(value, config, field_name)
        
        elif isinstance(value, dict):
            return sanitize_object(value, config, field_name, current_depth)
        
        elif isinstance(value, bool):
            return bool(value)
        
        else:
            # Convert unknown types to string and sanitize
            logger.info(f"Converting unknown type {type(value)} to string for {field_name}")
            return sanitize_string(str(value), config, field_name)
            
    except Exception as e:
        logger.error(f"Failed to sanitize {field_name}: {str(e)}")
        
        if config.sanitization_level == SanitizationLevel.STRICT:
            return None
        else:
            # Try to convert to safe string representation
            try:
                return sanitize_string(str(value), config, field_name)
            except:
                return None


def create_sanitization_pipeline(
    data_format: DataFormat,
    sanitization_level: SanitizationLevel = SanitizationLevel.MODERATE
) -> Callable[[Any], Any]:
    """
    Create a specialized sanitization pipeline for a specific data format.
    
    Args:
        data_format: Type of data format to handle
        sanitization_level: Level of sanitization to apply
        
    Returns:
        Sanitization function configured for the specified format
    """
    
    def pipeline(data: Any) -> Any:
        """Execute the sanitization pipeline."""
        start_time = time.perf_counter()
        
        config = SanitizationConfig(sanitization_level=sanitization_level)
        
        # Apply format-specific configurations
        if data_format == DataFormat.DTESN_CONFIG:
            config.preserve_structure = True
            config.handle_nan = True
            config.handle_infinity = True
            config.numeric_precision = 6
            
        elif data_format == DataFormat.HTML:
            config.escape_html = True
            config.remove_javascript = True
            config.allow_html_tags = ['p', 'br', 'strong', 'em']
            
        elif data_format == DataFormat.NUMERIC_ARRAY:
            config.preserve_structure = True
            config.max_array_length = 100000
            config.handle_nan = True
            config.handle_infinity = True
            
        elif data_format == DataFormat.URL_ENCODED:
            config.decode_url_encoding = True
            config.normalize_paths = True
        
        try:
            sanitized_data = sanitize_data_value(data, config, f"{data_format.value}_data")
            
            processing_time = (time.perf_counter() - start_time) * 1000
            logger.info(f"Sanitization pipeline for {data_format.value} completed in {processing_time:.2f}ms")
            
            return sanitized_data
            
        except Exception as e:
            processing_time = (time.perf_counter() - start_time) * 1000
            logger.error(f"Sanitization pipeline failed for {data_format.value} after {processing_time:.2f}ms: {str(e)}")
            raise HTTPException(
                status_code=400,
                detail=f"Data sanitization failed for {data_format.value}: {str(e)}"
            )
    
    return pipeline


# Predefined sanitization pipelines for common use cases
dtesn_sanitizer = create_sanitization_pipeline(DataFormat.DTESN_CONFIG, SanitizationLevel.MODERATE)
json_sanitizer = create_sanitization_pipeline(DataFormat.JSON, SanitizationLevel.MODERATE)
html_sanitizer = create_sanitization_pipeline(DataFormat.HTML, SanitizationLevel.STRICT)
numeric_sanitizer = create_sanitization_pipeline(DataFormat.NUMERIC_ARRAY, SanitizationLevel.LENIENT)


# Export main sanitization functions
__all__ = [
    'SanitizationLevel',
    'DataFormat', 
    'SanitizationConfig',
    'sanitize_string',
    'sanitize_numeric',
    'sanitize_array',
    'sanitize_object',
    'sanitize_data_value',
    'create_sanitization_pipeline',
    'dtesn_sanitizer',
    'json_sanitizer', 
    'html_sanitizer',
    'numeric_sanitizer'
]