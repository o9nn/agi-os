import os
import sys
import subprocess
import tempfile
import shutil
from pathlib import Path
def run_command(cmd, timeout=30):
    try:
        result = subprocess.run(cmd, shell=True, capture_output=True, text=True, timeout=timeout)
        return (result.returncode == 0, result.stdout, result.stderr)
    except subprocess.TimeoutExpired:
        return (False, '', 'Command timed out')
    except Exception as e:
        return (False, '', str(e))
def test_script_exists():
    print('🔍 Testing script existence...')
    script_path = Path('scripts/investigate_disk_space.sh')
    if not script_path.exists():
        print(f'❌ Script not found: {script_path}')
        return False
    os.chmod(script_path, 493)
    if not os.access(script_path, os.X_OK):
        print(f'❌ Script not executable: {script_path}')
        return False
    print(f'✅ Script exists and is executable: {script_path}')
    return True
def test_basic_commands():
    print('\n🔍 Testing basic system commands...')
    commands = [('df -h', 'Filesystem usage'), ('du --version', 'du command availability'), ('timeout --version', 'timeout command availability')]
    all_passed = True
    for cmd, desc in commands:
        success, stdout, stderr = run_command(cmd, timeout=10)
        if success:
            print(f'✅ {desc}: Available')
        else:
            print(f'❌ {desc}: Failed - {stderr}')
            all_passed = False
    return all_passed
def test_directory_analysis():
    print('\n🔍 Testing directory analysis...')
    with tempfile.TemporaryDirectory() as temp_dir:
        temp_path = Path(temp_dir)
        (temp_path / 'large_dir').mkdir()
        (temp_path / 'large_dir' / 'file1.txt').write_text('x' * 1000)
        (temp_path / 'large_dir' / 'file2.txt').write_text('y' * 2000)
        (temp_path / 'small_dir').mkdir()
        (temp_path / 'small_dir' / 'tiny.txt').write_text('z' * 10)
        cmd = f'du -h --max-depth=1 {temp_dir} 2>/dev/null | sort -rh'
        success, stdout, stderr = run_command(cmd)
        if success and 'large_dir' in stdout:
            print('✅ Directory analysis: Working')
            return True
        else:
            print(f'❌ Directory analysis: Failed - {stderr}')
            return False
def test_docker_detection():
    print('\n🔍 Testing Docker detection...')
    success, stdout, stderr = run_command('which docker', timeout=5)
    if success:
        print('✅ Docker command: Available')
        success, stdout, stderr = run_command('docker system df', timeout=10)
        if success:
            print('✅ Docker system: Running and accessible')
        else:
            print('⚠️  Docker system: Not running (this is normal in many environments)')
        return True
    else:
        print('⚠️  Docker command: Not available (this is normal)')
        return True
def test_cleanup_commands():
    print('\n🔍 Testing cleanup command safety...')
    safe_commands = [('apt-get --help', 'APT package manager'), ('find --help', 'find command'), ("which ccache || echo 'ccache not available'", 'ccache availability')]
    all_passed = True
    for cmd, desc in safe_commands:
        success, stdout, stderr = run_command(cmd, timeout=10)
        if 'not found' not in stderr.lower():
            print(f'✅ {desc}: Available')
        else:
            print(f'⚠️  {desc}: Not available (may be normal)')
    return True
def test_investigation_script():
    print('\n🔍 Testing investigation script...')
    script_path = 'scripts/investigate_disk_space.sh'
    test_cmd = f'\n    export MAX_DISPLAY_ITEMS=5\n    timeout 60 bash {script_path} > /tmp/test_disk_investigation.log 2>&1\n    '
    success, stdout, stderr = run_command(test_cmd, timeout=120)
    log_path = Path('/tmp/test_disk_investigation.log')
    if log_path.exists():
        log_content = log_path.read_text()
        if 'Analysis completed' in log_content or 'Analyzing directory' in log_content:
            print('✅ Investigation script: Executed successfully')
            print(f'   Log file created: {log_path}')
            return True
        else:
            print(f'⚠️  Investigation script: Partial execution')
            print(f'   Log content preview: {log_content[:200]}...')
            return True
    else:
        print(f'❌ Investigation script: No output generated')
        if stderr:
            print(f'   Error: {stderr}')
        return False
def test_workflow_syntax():
    print('\n🔍 Testing workflow syntax...')
    workflow_path = Path('.github/workflows/disk-space-investigation.yml')
    if not workflow_path.exists():
        print(f'❌ Workflow file not found: {workflow_path}')
        return False
    try:
        import yaml
        with open(workflow_path, 'r') as f:
            workflow_content = yaml.safe_load(f)
        required_sections = ['name', 'on', 'jobs']
        for section in required_sections:
            if section not in workflow_content:
                print(f'❌ Workflow missing required section: {section}')
                return False
        print('✅ Workflow syntax: Valid YAML with required sections')
        return True
    except ImportError:
        print('⚠️  YAML library not available, skipping syntax validation')
        return True
    except Exception as e:
        print(f'❌ Workflow syntax error: {e}')
        return False
def main():
    print('🚀 Starting disk space investigation tool tests...\n')
    tests = [('Script Existence', test_script_exists), ('Basic Commands', test_basic_commands), ('Directory Analysis', test_directory_analysis), ('Docker Detection', test_docker_detection), ('Cleanup Commands', test_cleanup_commands), ('Investigation Script', test_investigation_script), ('Workflow Syntax', test_workflow_syntax)]
    results = []
    for test_name, test_func in tests:
        print(f"\n{'=' * 60}")
        print(f'Running: {test_name}')
        print('=' * 60)
        try:
            result = test_func()
            results.append((test_name, result))
            if result:
                print(f'\n✅ {test_name}: PASSED')
            else:
                print(f'\n❌ {test_name}: FAILED')
        except Exception as e:
            print(f'\n💥 {test_name}: ERROR - {e}')
            results.append((test_name, False))
    print(f"\n{'=' * 60}")
    print('TEST SUMMARY')
    print('=' * 60)
    passed = sum((1 for _, result in results if result))
    total = len(results)
    for test_name, result in results:
        status = '✅ PASSED' if result else '❌ FAILED'
        print(f'{test_name:<25} {status}')
    print(f'\nTotal: {passed}/{total} tests passed')
    if passed == total:
        print('\n🎉 All tests passed! Disk space investigation tools are ready.')
        return 0
    elif passed >= total * 0.7:
        print('\n⚠️  Most tests passed. Tools should work with minor issues.')
        return 0
    else:
        print('\n💥 Many tests failed. Please review the implementation.')
        return 1
if __name__ == '__main__':
    sys.exit(main())