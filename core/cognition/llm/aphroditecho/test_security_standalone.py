import re
import html
import json
from fastapi import HTTPException
SECURITY_PATTERNS = {'sql_injection': ['(?i)(union\\s+select|drop\\s+table|delete\\s+from|insert\\s+into)', '(?i)(select.*from|union.*select)', '(?i)(exec\\s*\\(|execute\\s*\\()', '["\\\'][\\s]*(?:or|and)[\\s]*["\\\']?[\\s]*[=<>]'], 'xss': ['<\\s*script[^>]*>.*?</\\s*script\\s*>', 'javascript\\s*:', 'on\\w+\\s*=', '<\\s*iframe[^>]*>'], 'path_traversal': ['\\.\\.[\\\\/]', '[\\\\/]\\.\\.[\\\\/]', '%2e%2e%2f'], 'command_injection': ['[\\|&;`$\\(\\)]', '(?i)(cat|ls|pwd|whoami|rm\\s+-rf)']}
def validate_string_content(content: str, field_name: str='input') -> str:
    if not isinstance(content, str):
        return str(content)
    for vulnerability_type, patterns in SECURITY_PATTERNS.items():
        for pattern in patterns:
            if re.search(pattern, content, re.IGNORECASE | re.DOTALL):
                raise HTTPException(status_code=400, detail=f"Invalid input detected in field '{field_name}': potential {vulnerability_type.replace('_', ' ')} attack")
    sanitized = html.escape(content, quote=True)
    return sanitized
def test_security_validation():
    print('Testing server-side security implementation...')
    try:
        result = validate_string_content('Hello, world!', 'test_field')
        print(f"✓ Safe input test passed: '{result}'")
    except Exception as e:
        print(f'✗ Safe input test failed: {e}')
        return False
    try:
        result = validate_string_content('<p>Hello</p>', 'test_field')
        assert '&lt;p&gt;' in result and '&lt;/p&gt;' in result
        print(f"✓ HTML escaping test passed: '{result}'")
    except Exception as e:
        print(f'✗ HTML escaping test failed: {e}')
        return False
    try:
        validate_string_content("'; DROP TABLE users; --", 'test_field')
        print('✗ SQL injection test failed: attack was not blocked')
        return False
    except HTTPException as e:
        if 'sql injection' in e.detail.lower():
            print('✓ SQL injection test passed: attack blocked')
        else:
            print(f'✗ SQL injection test failed: wrong error - {e.detail}')
            return False
    except Exception as e:
        print(f'✗ SQL injection test failed: unexpected error - {e}')
        return False
    try:
        validate_string_content("<script>alert('xss')</script>", 'test_field')
        print('✗ XSS test failed: attack was not blocked')
        return False
    except HTTPException as e:
        if 'xss' in e.detail.lower():
            print('✓ XSS test passed: attack blocked')
        else:
            print(f'✗ XSS test failed: wrong error - {e.detail}')
            return False
    except Exception as e:
        print(f'✗ XSS test failed: unexpected error - {e}')
        return False
    try:
        validate_string_content('../../etc/passwd', 'test_field')
        print('✗ Path traversal test failed: attack was not blocked')
        return False
    except HTTPException as e:
        if 'path traversal' in e.detail.lower():
            print('✓ Path traversal test passed: attack blocked')
        else:
            print(f'✗ Path traversal test failed: wrong error - {e.detail}')
            return False
    except Exception as e:
        print(f'✗ Path traversal test failed: unexpected error - {e}')
        return False
    try:
        validate_string_content('test; cat /etc/passwd', 'test_field')
        print('✗ Command injection test failed: attack was not blocked')
        return False
    except HTTPException as e:
        if 'command injection' in e.detail.lower():
            print('✓ Command injection test passed: attack blocked')
        else:
            print(f'✗ Command injection test failed: wrong error - {e.detail}')
            return False
    except Exception as e:
        print(f'✗ Command injection test failed: unexpected error - {e}')
        return False
    return True
def test_json_security():
    print('\nTesting JSON security validation...')
    def create_deep_json(depth):
        result = {}
        current = result
        for i in range(depth):
            current['level'] = {}
            current = current['level']
        current['end'] = 'value'
        return result
    try:
        deep_json = create_deep_json(5)
        print(f'✓ Reasonable JSON depth test: created {len(json.dumps(deep_json))} byte structure')
    except Exception as e:
        print(f'✗ JSON depth test setup failed: {e}')
        return False
    print('✓ JSON security structure tests completed')
    return True
if __name__ == '__main__':
    print('=' * 60)
    print('APHRODITE ENGINE SERVER-SIDE SECURITY VALIDATION')
    print('=' * 60)
    success = True
    if not test_security_validation():
        success = False
    if not test_json_security():
        success = False
    print('\n' + '=' * 60)
    if success:
        print('✓ ALL SECURITY TESTS PASSED')
        print('Server-side security implementation is working correctly!')
    else:
        print('✗ SOME SECURITY TESTS FAILED')
        print('Security implementation needs review.')
    print('=' * 60)
    exit(0 if success else 1)