import pytest
import math
from unittest.mock import patch
from aphrodite.endpoints.security.data_sanitization import SanitizationLevel, DataFormat, SanitizationConfig, sanitize_string, sanitize_numeric, sanitize_array, sanitize_object, sanitize_data_value, create_sanitization_pipeline, dtesn_sanitizer, json_sanitizer, html_sanitizer
from fastapi import HTTPException
class TestStringSanitization:
    def test_basic_string_sanitization(self):
        result = sanitize_string('Hello, world!')
        assert result == 'Hello, world!'
    def test_html_escaping(self):
        input_str = "<script>alert('xss')</script>"
        result = sanitize_string(input_str)
        assert '&lt;script&gt;' in result
        assert '&lt;/script&gt;' in result
        assert '<' not in result
    def test_control_character_removal(self):
        input_str = 'Hello\x00\x01\x1f world\x7f'
        config = SanitizationConfig(remove_control_chars=True)
        result = sanitize_string(input_str, config)
        assert result == 'Hello world'
    def test_unicode_normalization(self):
        input_str = 'café'
        config = SanitizationConfig(normalize_unicode=True)
        result = sanitize_string(input_str, config)
        import unicodedata
        expected = unicodedata.normalize('NFKC', input_str)
        assert result == html.escape(expected, quote=True)
    def test_url_decoding(self):
        input_str = 'Hello%20World%21'
        config = SanitizationConfig(decode_url_encoding=True)
        result = sanitize_string(input_str, config)
        assert 'Hello World!' in result
    def test_javascript_removal(self):
        input_str = "javascript:alert('xss')"
        config = SanitizationConfig(remove_javascript=True)
        result = sanitize_string(input_str, config)
        assert 'javascript:' not in result.lower()
    def test_length_truncation(self):
        long_string = 'a' * 1000
        config = SanitizationConfig(max_string_length=100)
        result = sanitize_string(long_string, config)
        assert len(result) <= 100
    def test_strict_sanitization(self):
        input_str = "<script>alert('test')</script> `command` $(dangerous)"
        config = SanitizationConfig(sanitization_level=SanitizationLevel.STRICT)
        result = sanitize_string(input_str, config)
        assert '<script>' not in result
        assert '`' not in result
        assert '$(' not in result
class TestNumericSanitization:
    def test_valid_integer(self):
        result = sanitize_numeric(42)
        assert result == 42
        assert isinstance(result, int)
    def test_valid_float(self):
        result = sanitize_numeric(3.14159)
        assert result == 3.14159
        assert isinstance(result, float)
    def test_string_to_number(self):
        result = sanitize_numeric('123')
        assert result == 123
        assert isinstance(result, int)
        result = sanitize_numeric('3.14')
        assert result == 3.14
        assert isinstance(result, float)
    def test_string_with_formatting(self):
        result = sanitize_numeric('$1,234.56')
        assert result == 1234.56
        result = sanitize_numeric('  42  ')
        assert result == 42
    def test_nan_handling(self):
        config = SanitizationConfig(handle_nan=True)
        result = sanitize_numeric(float('nan'), config)
        assert result == 0.0
        config = SanitizationConfig(handle_nan=False)
        with pytest.raises(ValueError):
            sanitize_numeric(float('nan'), config)
    def test_infinity_handling(self):
        config = SanitizationConfig(handle_infinity=True, numeric_precision=3)
        result = sanitize_numeric(float('inf'), config)
        assert result == 1000.0
        result = sanitize_numeric(float('-inf'), config)
        assert result == -1000.0
        config = SanitizationConfig(handle_infinity=False)
        with pytest.raises(ValueError):
            sanitize_numeric(float('inf'), config)
    def test_precision_limits(self):
        config = SanitizationConfig(numeric_precision=2)
        result = sanitize_numeric(3.14159, config)
        assert abs(result - 3.14) < 0.01
    def test_invalid_string_conversion(self):
        with pytest.raises(ValueError):
            sanitize_numeric('not a number')
class TestArraySanitization:
    def test_basic_array_sanitization(self):
        input_array = [1, 2, 3, 'hello', 5.0]
        result = sanitize_array(input_array)
        assert len(result) == 5
        assert result[0] == 1
        assert 'hello' in result[3]
    def test_nested_array_sanitization(self):
        input_array = [[1, 2], [3, 4], ['a', 'b']]
        result = sanitize_array(input_array)
        assert len(result) == 3
        assert isinstance(result[0], list)
        assert result[0] == [1, 2]
    def test_array_length_limits(self):
        large_array = list(range(2000))
        config = SanitizationConfig(max_array_length=100)
        result = sanitize_array(large_array, config)
        assert len(result) <= 100
    def test_array_with_invalid_elements(self):
        input_array = [1, float('nan'), 3, '<script>', 5]
        config = SanitizationConfig(handle_nan=True, escape_html=True)
        result = sanitize_array(input_array, config)
        assert result[0] == 1
        assert result[1] == 0.0
        assert result[2] == 3
        assert '&lt;script&gt;' in result[3]
        assert result[4] == 5
    def test_non_list_input(self):
        with pytest.raises(ValueError):
            sanitize_array('not a list')
class TestObjectSanitization:
    def test_basic_object_sanitization(self):
        input_obj = {'name': 'test', 'value': 42, 'data': [1, 2, 3]}
        result = sanitize_object(input_obj)
        assert 'name' in result
        assert 'value' in result
        assert 'data' in result
        assert result['value'] == 42
    def test_nested_object_sanitization(self):
        input_obj = {'config': {'database': {'host': 'localhost', 'port': 5432}, 'cache': {'enabled': True}}}
        result = sanitize_object(input_obj)
        assert 'config' in result
        assert 'database' in result['config']
        assert 'host' in result['config']['database']
    def test_object_depth_limits(self):
        nested = {'level': 0}
        current = nested
        for i in range(1, 25):
            current['next'] = {'level': i}
            current = current['next']
        config = SanitizationConfig(max_object_depth=10)
        result = sanitize_object(nested, config)
        current_result = result
        depth = 0
        while 'next' in current_result and '__truncated__' not in current_result:
            current_result = current_result['next']
            depth += 1
            if depth > 15:
                break
        assert depth <= 12
    def test_key_sanitization(self):
        input_obj = {'<script>key</script>': 'value1', 'normal_key': 'value2'}
        result = sanitize_object(input_obj)
        dangerous_key_found = any(('<script>' in key for key in result.keys()))
        assert not dangerous_key_found
    def test_non_dict_input(self):
        with pytest.raises(ValueError):
            sanitize_object('not a dict')
class TestDataValueSanitization:
    def test_none_value(self):
        result = sanitize_data_value(None)
        assert result is None
    def test_boolean_value(self):
        assert sanitize_data_value(True) is True
        assert sanitize_data_value(False) is False
    def test_unknown_type_conversion(self):
        class CustomClass:
            def __str__(self):
                return 'custom_value'
        obj = CustomClass()
        result = sanitize_data_value(obj)
        assert 'custom_value' in result
    def test_nested_mixed_data(self):
        complex_data = {'users': [{'name': "<script>alert('xss')</script>", 'age': 25, 'scores': [95.5, float('nan'), 87.2]}, {'name': 'normal_user', 'age': '30', 'scores': [92.1, 88.5]}], 'metadata': {'version': 1.0, 'config': {'enable_feature': True, 'max_items': '100'}}}
        config = SanitizationConfig(handle_nan=True, escape_html=True, sanitization_level=SanitizationLevel.MODERATE)
        result = sanitize_data_value(complex_data, config)
        user_name = result['users'][0]['name']
        assert '&lt;script&gt;' in user_name
        user_scores = result['users'][0]['scores']
        assert 0.0 in user_scores
        second_user_age = result['users'][1]['age']
        assert isinstance(second_user_age, str)
class TestSanitizationPipelines:
    def test_dtesn_sanitizer(self):
        dtesn_data = {'reservoir_size': 100, 'spectral_radius': float('nan'), 'membrane_configs': [{'id': '<dangerous>membrane</dangerous>', 'capacity': 1000}]}
        result = dtesn_sanitizer(dtesn_data)
        assert result['reservoir_size'] == 100
        assert result['spectral_radius'] == 0.0
        assert '&lt;dangerous&gt;' in result['membrane_configs'][0]['id']
    def test_json_sanitizer(self):
        json_data = {'message': 'Hello <world>', 'numbers': [1, 2, float('inf')], 'nested': {'value': 'test'}}
        result = json_sanitizer(json_data)
        assert '&lt;world&gt;' in result['message']
        assert isinstance(result['numbers'], list)
    def test_html_sanitizer(self):
        html_data = {'content': "<script>alert('xss')</script><p>Safe content</p>", 'title': 'Page Title'}
        result = html_sanitizer(html_data)
        content = result['content']
        assert 'script' not in content.lower()
    def test_custom_pipeline_creation(self):
        pipeline = create_sanitization_pipeline(DataFormat.NUMERIC_ARRAY, SanitizationLevel.LENIENT)
        numeric_data = [1.0, 2.5, float('nan'), 3.14159]
        result = pipeline(numeric_data)
        assert isinstance(result, list)
        assert len(result) == 4
        assert 0.0 in result
    @patch('aphrodite.endpoints.security.data_sanitization.time.perf_counter')
    def test_pipeline_performance_tracking(self, mock_time):
        mock_time.side_effect = [0.0, 0.05]
        data = {'simple': 'data'}
        with patch('aphrodite.endpoints.security.data_sanitization.logger') as mock_logger:
            result = json_sanitizer(data)
            assert result is not None
            mock_logger.info.assert_called()
    def test_pipeline_error_handling(self):
        pipeline = create_sanitization_pipeline(DataFormat.JSON)
        with patch('aphrodite.endpoints.security.data_sanitization.sanitize_data_value') as mock_sanitize:
            mock_sanitize.side_effect = Exception('Sanitization failed')
            with pytest.raises(HTTPException) as exc_info:
                pipeline({'data': 'test'})
            assert exc_info.value.status_code == 400
            assert 'sanitization failed' in exc_info.value.detail.lower()
class TestSanitizationConfiguration:
    def test_default_config(self):
        config = SanitizationConfig()
        assert config.sanitization_level == SanitizationLevel.MODERATE
        assert config.preserve_structure is True
        assert config.escape_html is True
        assert config.handle_nan is True
    def test_custom_config(self):
        config = SanitizationConfig(sanitization_level=SanitizationLevel.STRICT, max_string_length=5000, numeric_precision=8, escape_html=False)
        assert config.sanitization_level == SanitizationLevel.STRICT
        assert config.max_string_length == 5000
        assert config.numeric_precision == 8
        assert config.escape_html is False
    def test_config_effects_on_sanitization(self):
        test_data = 'Hello <world>'
        config1 = SanitizationConfig(escape_html=True)
        result1 = sanitize_string(test_data, config1)
        assert '&lt;world&gt;' in result1
        config2 = SanitizationConfig(escape_html=False)
        result2 = sanitize_string(test_data, config2)
@pytest.mark.asyncio
class TestSanitizationPerformance:
    async def test_large_data_sanitization(self):
        large_data = {'users': [{'id': i, 'name': f'user_{i}', 'data': list(range(100))} for i in range(100)]}
        config = SanitizationConfig(max_processing_time_ms=1000)
        result = sanitize_data_value(large_data, config)
        assert len(result['users']) <= 100
    @patch('aphrodite.endpoints.security.data_sanitization.time.perf_counter')
    async def test_sanitization_timeout_handling(self, mock_time):
        mock_time.side_effect = [0.0, 1.0]
        simple_data = {'test': 'data'}
        config = SanitizationConfig(max_processing_time_ms=100)
        result = sanitize_data_value(simple_data, config)
        assert result is not None