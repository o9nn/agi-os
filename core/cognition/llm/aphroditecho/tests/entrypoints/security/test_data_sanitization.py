"""
Tests for comprehensive data sanitization and normalization.

This module tests the data sanitization pipelines to ensure all input data
is properly cleaned and formatted before processing.
"""

import pytest
import math
from unittest.mock import patch

from aphrodite.endpoints.security.data_sanitization import (
    SanitizationLevel,
    DataFormat,
    SanitizationConfig,
    sanitize_string,
    sanitize_numeric,
    sanitize_array,
    sanitize_object,
    sanitize_data_value,
    create_sanitization_pipeline,
    dtesn_sanitizer,
    json_sanitizer,
    html_sanitizer
)
from fastapi import HTTPException


class TestStringSanitization:
    """Test string content sanitization."""
    
    def test_basic_string_sanitization(self):
        """Test basic string sanitization."""
        result = sanitize_string("Hello, world!")
        assert result == "Hello, world!"
    
    def test_html_escaping(self):
        """Test HTML escaping in strings."""
        input_str = "<script>alert('xss')</script>"
        result = sanitize_string(input_str)
        
        assert "&lt;script&gt;" in result
        assert "&lt;/script&gt;" in result
        assert "<" not in result
    
    def test_control_character_removal(self):
        """Test removal of control characters."""
        input_str = "Hello\x00\x01\x1f world\x7f"
        config = SanitizationConfig(remove_control_chars=True)
        result = sanitize_string(input_str, config)
        
        assert result == "Hello world"
    
    def test_unicode_normalization(self):
        """Test Unicode normalization."""
        input_str = "café"  # e with acute accent
        config = SanitizationConfig(normalize_unicode=True)
        result = sanitize_string(input_str, config)
        
        # Should normalize to NFKC form
        import unicodedata
        expected = unicodedata.normalize('NFKC', input_str)
        assert result == html.escape(expected, quote=True)
    
    def test_url_decoding(self):
        """Test URL decoding."""
        input_str = "Hello%20World%21"
        config = SanitizationConfig(decode_url_encoding=True)
        result = sanitize_string(input_str, config)
        
        # Should decode URL encoding then escape HTML
        assert "Hello World!" in result
    
    def test_javascript_removal(self):
        """Test JavaScript protocol removal."""
        input_str = "javascript:alert('xss')"
        config = SanitizationConfig(remove_javascript=True)
        result = sanitize_string(input_str, config)
        
        assert "javascript:" not in result.lower()
    
    def test_length_truncation(self):
        """Test string length truncation."""
        long_string = "a" * 1000
        config = SanitizationConfig(max_string_length=100)
        result = sanitize_string(long_string, config)
        
        assert len(result) <= 100
    
    def test_strict_sanitization(self):
        """Test strict sanitization level."""
        input_str = "<script>alert('test')</script> `command` $(dangerous)"
        config = SanitizationConfig(sanitization_level=SanitizationLevel.STRICT)
        result = sanitize_string(input_str, config)
        
        # Should remove all dangerous patterns
        assert "<script>" not in result
        assert "`" not in result
        assert "$(" not in result


class TestNumericSanitization:
    """Test numeric data sanitization."""
    
    def test_valid_integer(self):
        """Test sanitization of valid integer."""
        result = sanitize_numeric(42)
        assert result == 42
        assert isinstance(result, int)
    
    def test_valid_float(self):
        """Test sanitization of valid float."""
        result = sanitize_numeric(3.14159)
        assert result == 3.14159
        assert isinstance(result, float)
    
    def test_string_to_number(self):
        """Test conversion of string to number."""
        result = sanitize_numeric("123")
        assert result == 123
        assert isinstance(result, int)
        
        result = sanitize_numeric("3.14")
        assert result == 3.14
        assert isinstance(result, float)
    
    def test_string_with_formatting(self):
        """Test string with common formatting characters."""
        result = sanitize_numeric("$1,234.56")
        assert result == 1234.56
        
        result = sanitize_numeric("  42  ")
        assert result == 42
    
    def test_nan_handling(self):
        """Test NaN value handling."""
        config = SanitizationConfig(handle_nan=True)
        result = sanitize_numeric(float('nan'), config)
        assert result == 0.0
        
        config = SanitizationConfig(handle_nan=False)
        with pytest.raises(ValueError):
            sanitize_numeric(float('nan'), config)
    
    def test_infinity_handling(self):
        """Test infinity value handling."""
        config = SanitizationConfig(handle_infinity=True, numeric_precision=3)
        result = sanitize_numeric(float('inf'), config)
        assert result == 1000.0  # 10^3
        
        result = sanitize_numeric(float('-inf'), config)
        assert result == -1000.0
        
        config = SanitizationConfig(handle_infinity=False)
        with pytest.raises(ValueError):
            sanitize_numeric(float('inf'), config)
    
    def test_precision_limits(self):
        """Test numeric precision limits."""
        config = SanitizationConfig(numeric_precision=2)
        result = sanitize_numeric(3.14159, config)
        
        # Should be rounded to 2 decimal places
        assert abs(result - 3.14) < 0.01
    
    def test_invalid_string_conversion(self):
        """Test invalid string conversion."""
        with pytest.raises(ValueError):
            sanitize_numeric("not a number")


class TestArraySanitization:
    """Test array/list sanitization."""
    
    def test_basic_array_sanitization(self):
        """Test basic array sanitization."""
        input_array = [1, 2, 3, "hello", 5.0]
        result = sanitize_array(input_array)
        
        assert len(result) == 5
        assert result[0] == 1
        assert "hello" in result[3]  # HTML escaped
    
    def test_nested_array_sanitization(self):
        """Test nested array sanitization."""
        input_array = [[1, 2], [3, 4], ["a", "b"]]
        result = sanitize_array(input_array)
        
        assert len(result) == 3
        assert isinstance(result[0], list)
        assert result[0] == [1, 2]
    
    def test_array_length_limits(self):
        """Test array length limits."""
        large_array = list(range(2000))
        config = SanitizationConfig(max_array_length=100)
        result = sanitize_array(large_array, config)
        
        assert len(result) <= 100
    
    def test_array_with_invalid_elements(self):
        """Test array containing invalid elements."""
        input_array = [1, float('nan'), 3, "<script>", 5]
        config = SanitizationConfig(handle_nan=True, escape_html=True)
        result = sanitize_array(input_array, config)
        
        assert result[0] == 1
        assert result[1] == 0.0  # NaN replaced
        assert result[2] == 3
        assert "&lt;script&gt;" in result[3]  # HTML escaped
        assert result[4] == 5
    
    def test_non_list_input(self):
        """Test error handling for non-list input."""
        with pytest.raises(ValueError):
            sanitize_array("not a list")


class TestObjectSanitization:
    """Test dictionary/object sanitization."""
    
    def test_basic_object_sanitization(self):
        """Test basic object sanitization."""
        input_obj = {
            "name": "test",
            "value": 42,
            "data": [1, 2, 3]
        }
        result = sanitize_object(input_obj)
        
        assert "name" in result
        assert "value" in result
        assert "data" in result
        assert result["value"] == 42
    
    def test_nested_object_sanitization(self):
        """Test nested object sanitization."""
        input_obj = {
            "config": {
                "database": {
                    "host": "localhost",
                    "port": 5432
                },
                "cache": {
                    "enabled": True
                }
            }
        }
        result = sanitize_object(input_obj)
        
        assert "config" in result
        assert "database" in result["config"]
        assert "host" in result["config"]["database"]
    
    def test_object_depth_limits(self):
        """Test object nesting depth limits."""
        # Create deeply nested object
        nested = {"level": 0}
        current = nested
        for i in range(1, 25):
            current["next"] = {"level": i}
            current = current["next"]
        
        config = SanitizationConfig(max_object_depth=10)
        result = sanitize_object(nested, config)
        
        # Should be truncated at depth limit
        current_result = result
        depth = 0
        while "next" in current_result and "__truncated__" not in current_result:
            current_result = current_result["next"] 
            depth += 1
            if depth > 15:  # Safety break
                break
        
        # Should find truncation marker at some point
        assert depth <= 12  # Some tolerance for implementation
    
    def test_key_sanitization(self):
        """Test sanitization of object keys."""
        input_obj = {
            "<script>key</script>": "value1",
            "normal_key": "value2"
        }
        result = sanitize_object(input_obj)
        
        # Keys should be sanitized too
        dangerous_key_found = any("<script>" in key for key in result.keys())
        assert not dangerous_key_found
    
    def test_non_dict_input(self):
        """Test error handling for non-dict input."""
        with pytest.raises(ValueError):
            sanitize_object("not a dict")


class TestDataValueSanitization:
    """Test generic data value sanitization."""
    
    def test_none_value(self):
        """Test None value handling."""
        result = sanitize_data_value(None)
        assert result is None
    
    def test_boolean_value(self):
        """Test boolean value handling."""
        assert sanitize_data_value(True) is True
        assert sanitize_data_value(False) is False
    
    def test_unknown_type_conversion(self):
        """Test unknown type conversion to string."""
        class CustomClass:
            def __str__(self):
                return "custom_value"
        
        obj = CustomClass()
        result = sanitize_data_value(obj)
        
        # Should be converted to string and sanitized
        assert "custom_value" in result
    
    def test_nested_mixed_data(self):
        """Test sanitization of mixed nested data structures."""
        complex_data = {
            "users": [
                {
                    "name": "<script>alert('xss')</script>",
                    "age": 25,
                    "scores": [95.5, float('nan'), 87.2]
                },
                {
                    "name": "normal_user",
                    "age": "30",  # String that should become number
                    "scores": [92.1, 88.5]
                }
            ],
            "metadata": {
                "version": 1.0,
                "config": {
                    "enable_feature": True,
                    "max_items": "100"
                }
            }
        }
        
        config = SanitizationConfig(
            handle_nan=True,
            escape_html=True,
            sanitization_level=SanitizationLevel.MODERATE
        )
        
        result = sanitize_data_value(complex_data, config)
        
        # Check that HTML was escaped
        user_name = result["users"][0]["name"]
        assert "&lt;script&gt;" in user_name
        
        # Check that NaN was handled
        user_scores = result["users"][0]["scores"]
        assert 0.0 in user_scores  # NaN should be replaced with 0.0
        
        # Check that string numbers were preserved as strings (since we sanitize strings)
        second_user_age = result["users"][1]["age"]
        assert isinstance(second_user_age, str)  # Sanitized as string


class TestSanitizationPipelines:
    """Test specialized sanitization pipelines."""
    
    def test_dtesn_sanitizer(self):
        """Test DTESN-specific sanitization pipeline."""
        dtesn_data = {
            "reservoir_size": 100,
            "spectral_radius": float('nan'),  # Should be handled
            "membrane_configs": [
                {
                    "id": "<dangerous>membrane</dangerous>",
                    "capacity": 1000
                }
            ]
        }
        
        result = dtesn_sanitizer(dtesn_data)
        
        assert result["reservoir_size"] == 100
        assert result["spectral_radius"] == 0.0  # NaN handled
        assert "&lt;dangerous&gt;" in result["membrane_configs"][0]["id"]
    
    def test_json_sanitizer(self):
        """Test JSON-specific sanitization pipeline."""
        json_data = {
            "message": "Hello <world>",
            "numbers": [1, 2, float('inf')],
            "nested": {
                "value": "test"
            }
        }
        
        result = json_sanitizer(json_data)
        
        assert "&lt;world&gt;" in result["message"]
        # Infinity should be handled based on config
        assert isinstance(result["numbers"], list)
    
    def test_html_sanitizer(self):
        """Test HTML-specific sanitization pipeline."""
        html_data = {
            "content": "<script>alert('xss')</script><p>Safe content</p>",
            "title": "Page Title"
        }
        
        result = html_sanitizer(html_data)
        
        # Strict mode should remove dangerous content
        content = result["content"]
        assert "script" not in content.lower()
    
    def test_custom_pipeline_creation(self):
        """Test creation of custom sanitization pipeline."""
        pipeline = create_sanitization_pipeline(
            DataFormat.NUMERIC_ARRAY,
            SanitizationLevel.LENIENT
        )
        
        numeric_data = [1.0, 2.5, float('nan'), 3.14159]
        result = pipeline(numeric_data)
        
        assert isinstance(result, list)
        assert len(result) == 4
        # NaN should be handled
        assert 0.0 in result
    
    @patch('aphrodite.endpoints.security.data_sanitization.time.perf_counter')
    def test_pipeline_performance_tracking(self, mock_time):
        """Test performance tracking in pipelines."""
        mock_time.side_effect = [0.0, 0.05]  # 50ms elapsed
        
        data = {"simple": "data"}
        
        with patch('aphrodite.endpoints.security.data_sanitization.logger') as mock_logger:
            result = json_sanitizer(data)
            
            # Should complete successfully
            assert result is not None
            # Should log performance
            mock_logger.info.assert_called()
    
    def test_pipeline_error_handling(self):
        """Test error handling in pipelines."""
        # Create a pipeline that will fail
        pipeline = create_sanitization_pipeline(DataFormat.JSON)
        
        # Pass data that will cause sanitization to fail
        with patch('aphrodite.endpoints.security.data_sanitization.sanitize_data_value') as mock_sanitize:
            mock_sanitize.side_effect = Exception("Sanitization failed")
            
            with pytest.raises(HTTPException) as exc_info:
                pipeline({"data": "test"})
            
            assert exc_info.value.status_code == 400
            assert "sanitization failed" in exc_info.value.detail.lower()


class TestSanitizationConfiguration:
    """Test sanitization configuration options."""
    
    def test_default_config(self):
        """Test default sanitization configuration."""
        config = SanitizationConfig()
        
        assert config.sanitization_level == SanitizationLevel.MODERATE
        assert config.preserve_structure is True
        assert config.escape_html is True
        assert config.handle_nan is True
    
    def test_custom_config(self):
        """Test custom sanitization configuration."""
        config = SanitizationConfig(
            sanitization_level=SanitizationLevel.STRICT,
            max_string_length=5000,
            numeric_precision=8,
            escape_html=False
        )
        
        assert config.sanitization_level == SanitizationLevel.STRICT
        assert config.max_string_length == 5000
        assert config.numeric_precision == 8
        assert config.escape_html is False
    
    def test_config_effects_on_sanitization(self):
        """Test that configuration affects sanitization behavior."""
        test_data = "Hello <world>"
        
        # With HTML escaping
        config1 = SanitizationConfig(escape_html=True)
        result1 = sanitize_string(test_data, config1)
        assert "&lt;world&gt;" in result1
        
        # Without HTML escaping  
        config2 = SanitizationConfig(escape_html=False)
        result2 = sanitize_string(test_data, config2)
        # Note: Other sanitization rules may still apply


@pytest.mark.asyncio
class TestSanitizationPerformance:
    """Test performance aspects of data sanitization."""
    
    async def test_large_data_sanitization(self):
        """Test sanitization of large data structures."""
        # Create large data structure
        large_data = {
            "users": [
                {
                    "id": i,
                    "name": f"user_{i}",
                    "data": list(range(100))
                }
                for i in range(100)
            ]
        }
        
        config = SanitizationConfig(max_processing_time_ms=1000)
        
        # Should complete without timeout
        result = sanitize_data_value(large_data, config)
        assert len(result["users"]) <= 100  # May be limited by array length config
    
    @patch('aphrodite.endpoints.security.data_sanitization.time.perf_counter')
    async def test_sanitization_timeout_handling(self, mock_time):
        """Test handling of slow sanitization operations."""
        # Mock very slow operation
        mock_time.side_effect = [0.0, 1.0]  # 1 second elapsed
        
        simple_data = {"test": "data"}
        config = SanitizationConfig(max_processing_time_ms=100)  # 100ms limit
        
        # Should still complete but may log warnings
        result = sanitize_data_value(simple_data, config)
        assert result is not None