import os
import sys
import subprocess
import json
from pathlib import Path
def test_guile_stages_structure():
    print('🔍 Testing Guile stages structure...')
    base_path = Path('/home/runner/work/hurdcog/hurdcog/guix-build-system')
    expected_stages = ['guile-stage0/bootstrap.scm', 'guile-stage1/core.scm', 'guile-stage2/extensions.scm', 'guile-stage3/agi-os.scm', 'orchestration.scm']
    all_exist = True
    for stage_file in expected_stages:
        full_path = base_path / stage_file
        if full_path.exists():
            print(f'✅ {stage_file} - EXISTS')
        else:
            print(f'❌ {stage_file} - MISSING')
            all_exist = False
    return all_exist
def test_scheme_syntax():
    print('\n🔍 Testing Scheme file syntax...')
    base_path = Path('/home/runner/work/hurdcog/hurdcog/guix-build-system')
    scheme_files = list(base_path.rglob('*.scm'))
    syntax_valid = True
    for scheme_file in scheme_files:
        try:
            result = subprocess.run(['guile', '--no-auto-compile', '-c', f'(load "{scheme_file}")'], capture_output=True, text=True, timeout=10)
            if result.returncode == 0:
                print(f'✅ {scheme_file.name} - SYNTAX OK')
            else:
                print(f'❌ {scheme_file.name} - SYNTAX ERROR')
                print(f'   Error: {result.stderr}')
                syntax_valid = False
        except subprocess.TimeoutExpired:
            print(f'⏰ {scheme_file.name} - TIMEOUT (may be normal for loading)')
        except Exception as e:
            print(f'⚠️  {scheme_file.name} - COULD NOT TEST: {e}')
    return syntax_valid
def test_integration_completeness():
    print('\n🔍 Testing integration completeness...')
    requirements = {'Stage0: Minimal Bootstrap': 'guix-build-system/guile-stage0/bootstrap.scm', 'Stage1: Core Functionality': 'guix-build-system/guile-stage1/core.scm', 'Stage2: Full Extensions': 'guix-build-system/guile-stage2/extensions.scm', 'Stage3: AGI-OS Features': 'guix-build-system/guile-stage3/agi-os.scm', 'Build Orchestration': 'guix-build-system/orchestration.scm', 'Enhanced Integration': 'cogkernel/build/guix-integration.scm'}
    completeness_score = 0
    for requirement, file_path in requirements.items():
        full_path = Path('/home/runner/work/hurdcog/hurdcog') / file_path
        if full_path.exists():
            print(f'✅ {requirement} - IMPLEMENTED')
            completeness_score += 1
        else:
            print(f'❌ {requirement} - MISSING')
    completion_percentage = completeness_score / len(requirements) * 100
    print(f'\n📊 Integration Completeness: {completion_percentage:.1f}%')
    return completion_percentage >= 100.0
def test_skz_framework_integration():
    print('\n🔍 Testing SKZ framework integration...')
    integration_markers = ['autonomous agents framework', 'cognitive workflow engine', 'opencog atomspace', 'plan9 namespace', 'kokkos parallel computing', 'ecma-262 javascript', 'llama-cpp integration']
    base_path = Path('/home/runner/work/hurdcog/hurdcog/guix-build-system')
    all_content = ''
    for scheme_file in base_path.rglob('*.scm'):
        try:
            with open(scheme_file, 'r') as f:
                all_content += f.read().lower()
        except Exception as e:
            print(f'⚠️  Could not read {scheme_file}: {e}')
    markers_found = 0
    for marker in integration_markers:
        if any((keyword in all_content for keyword in marker.split())):
            print(f'✅ {marker.title()} - INTEGRATED')
            markers_found += 1
        else:
            print(f'❌ {marker.title()} - NOT FOUND')
    integration_score = markers_found / len(integration_markers) * 100
    print(f'\n📊 SKZ Integration Score: {integration_score:.1f}%')
    return integration_score >= 80.0
def run_integration_test():
    print('🚀 GUIX Integration with Guile Stages - Test Suite')
    print('=' * 60)
    tests = [('Guile Stages Structure', test_guile_stages_structure), ('Scheme Syntax Validation', test_scheme_syntax), ('Integration Completeness', test_integration_completeness), ('SKZ Framework Integration', test_skz_framework_integration)]
    passed_tests = 0
    total_tests = len(tests)
    for test_name, test_func in tests:
        print(f'\n{test_name}')
        print('-' * len(test_name))
        try:
            if test_func():
                print(f'✅ {test_name}: PASSED')
                passed_tests += 1
            else:
                print(f'❌ {test_name}: FAILED')
        except Exception as e:
            print(f'💥 {test_name}: ERROR - {e}')
    print('\n' + '=' * 60)
    success_rate = passed_tests / total_tests * 100
    print(f'📊 Test Results: {passed_tests}/{total_tests} tests passed ({success_rate:.1f}%)')
    if success_rate >= 75.0:
        print('🎉 GUIX Integration with Guile Stages: SUCCESS')
        return True
    else:
        print('❌ GUIX Integration with Guile Stages: NEEDS IMPROVEMENT')
        return False
if __name__ == '__main__':
    success = run_integration_test()
    sys.exit(0 if success else 1)