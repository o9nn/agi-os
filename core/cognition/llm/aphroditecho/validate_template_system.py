import sys
from pathlib import Path
def test_template_structure():
    print('Testing template directory structure...')
    base_dir = Path(__file__).parent
    templates_dir = base_dir / 'aphrodite' / 'endpoints' / 'deep_tree_echo' / 'templates'
    print(f'Templates directory: {templates_dir}')
    if not templates_dir.exists():
        print('❌ Templates directory does not exist')
        return False
    else:
        print('✅ Templates directory exists')
    required_templates = ['base.html', 'index.html', 'status.html', 'membrane_info.html', 'esn_state.html', 'process_result.html']
    all_exist = True
    for template in required_templates:
        template_path = templates_dir / template
        if not template_path.exists():
            print(f'❌ Template {template} does not exist')
            all_exist = False
        else:
            print(f'✅ Template {template} exists')
    return all_exist
def test_template_content():
    print('\nTesting template content...')
    base_dir = Path(__file__).parent
    templates_dir = base_dir / 'aphrodite' / 'endpoints' / 'deep_tree_echo' / 'templates'
    base_template = templates_dir / 'base.html'
    if base_template.exists():
        content = base_template.read_text()
        checks = [('<!DOCTYPE html>', 'HTML5 doctype'), ('{% block', 'Jinja2 blocks'), ('{% block title %}', 'Jinja2 variables'), ('Deep Tree Echo', 'Title content'), ('Server-side rendered', 'SSR indication')]
        for check, description in checks:
            if check in content:
                print(f'✅ Base template has {description}')
            else:
                print(f'❌ Base template missing {description}')
                return False
    else:
        print('❌ Cannot test base template content - file missing')
        return False
    child_templates = ['index.html', 'status.html', 'membrane_info.html', 'esn_state.html']
    for template_name in child_templates:
        template_path = templates_dir / template_name
        if template_path.exists():
            content = template_path.read_text()
            if '{% extends "base.html" %}' in content:
                print(f'✅ {template_name} extends base template')
            else:
                print(f'❌ {template_name} does not extend base template')
                return False
        else:
            print(f'❌ Cannot check {template_name} - file missing')
            return False
    return True
def test_app_factory_integration():
    print('\nTesting app factory integration...')
    base_dir = Path(__file__).parent
    app_factory_path = base_dir / 'aphrodite' / 'endpoints' / 'deep_tree_echo' / 'app_factory.py'
    if not app_factory_path.exists():
        print('❌ App factory file does not exist')
        return False
    content = app_factory_path.read_text()
    checks = [('from fastapi.templating import Jinja2Templates', 'Jinja2Templates import'), ('TEMPLATES_DIR', 'Templates directory constant'), ('Jinja2Templates(directory', 'Template initialization'), ('app.state.templates', 'Templates stored in app state')]
    for check, description in checks:
        if check in content:
            print(f'✅ App factory has {description}')
        else:
            print(f'❌ App factory missing {description}')
            return False
    return True
def test_routes_integration():
    print('\nTesting routes template integration...')
    base_dir = Path(__file__).parent
    routes_path = base_dir / 'aphrodite' / 'endpoints' / 'deep_tree_echo' / 'routes.py'
    if not routes_path.exists():
        print('❌ Routes file does not exist')
        return False
    content = routes_path.read_text()
    checks = [('from fastapi.templating import Jinja2Templates', 'Jinja2Templates import'), ('from fastapi.responses import HTMLResponse', 'HTML response import'), ('get_templates', 'Template dependency function'), ('wants_html', 'Content negotiation function'), ('TemplateResponse', 'Template response usage')]
    for check, description in checks:
        if check in content:
            print(f'✅ Routes have {description}')
        else:
            print(f'❌ Routes missing {description}')
            return False
    return True
def main():
    print('Deep Tree Echo Server-Side Template System Validation')
    print('=' * 60)
    tests = [test_template_structure, test_template_content, test_app_factory_integration, test_routes_integration]
    all_passed = True
    for test in tests:
        result = test()
        if not result:
            all_passed = False
    print('\n' + '=' * 60)
    if all_passed:
        print('🎉 All template system validation tests passed!')
        print('✅ Server-side template system is properly configured')
        return 0
    else:
        print('❌ Some template system validation tests failed')
        return 1
if __name__ == '__main__':
    sys.exit(main())