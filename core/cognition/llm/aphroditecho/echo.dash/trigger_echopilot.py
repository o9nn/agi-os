import os
import json
import subprocess
import tempfile
from pathlib import Path
from datetime import datetime
def run_analysis():
    print('🔍 Running EchoPilot Analysis...')
    print('=' * 50)
    with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
        output_file = f.name
    env = os.environ.copy()
    env['GITHUB_OUTPUT'] = output_file
    analysis_script = '\nimport os\nimport json\nimport re\nimport subprocess\nimport sys\nfrom pathlib import Path\nfrom collections import defaultdict\n\ndef run_command(cmd, capture_output=True):\n    try:\n        result = subprocess.run(cmd, shell=True, capture_output=capture_output, text=True, timeout=300)\n        return result.returncode, result.stdout, result.stderr\n    except subprocess.TimeoutExpired:\n        return -1, "", "Command timed out"\n    except Exception as e:\n        return -1, "", str(e)\n\n# Initialize analysis results\nanalysis_results = {\n    \'code_quality_issues\': [],\n    \'architecture_gaps\': [],\n    \'test_coverage_gaps\': [],\n    \'dependency_issues\': [],\n    \'documentation_gaps\': []\n}\n\nrepo_path = Path(\'.\')\n\n# 1. Code Quality Analysis\nprint("🔍 Analyzing code quality...")\n\n# Run pylint on Python files\npython_files = list(repo_path.glob(\'**/*.py\'))\nif python_files:\n    # Test with a few files first\n    test_files = python_files[:5]\n    pylint_cmd = f"pylint {\' \'.join(str(f) for f in test_files)} --output-format=json --exit-zero"\n    returncode, stdout, stderr = run_command(pylint_cmd)\n    \n    if returncode == 0 and stdout:\n        try:\n            pylint_results = json.loads(stdout)\n            for issue in pylint_results:\n                if issue.get(\'type\') in [\'error\', \'warning\']:\n                    analysis_results[\'code_quality_issues\'].append({\n                        \'file\': issue.get(\'path\', \'unknown\'),\n                        \'line\': issue.get(\'line\', 0),\n                        \'message\': issue.get(\'message\', \'\'),\n                        \'type\': issue.get(\'type\', \'\'),\n                        \'symbol\': issue.get(\'symbol\', \'\'),\n                        \'severity\': \'high\' if issue.get(\'type\') == \'error\' else \'medium\'\n                    })\n        except json.JSONDecodeError:\n            pass\n\n# 2. Architecture Gap Analysis\nprint("🏗️ Analyzing architecture gaps...")\n\n# Check for fragmented memory system\nmemory_files = list(repo_path.glob(\'**/*memory*.py\')) + list(repo_path.glob(\'**/*Memory*.py\'))\nif len(memory_files) > 3:\n    analysis_results[\'architecture_gaps\'].append({\n        \'gap\': \'Fragmented Memory System\',\n        \'description\': f\'Found {len(memory_files)} memory-related files that should be unified\',\n        \'files\': [str(f) for f in memory_files],\n        \'priority\': \'high\',\n        \'recommendation\': \'Consolidate memory operations into unified_echo_memory.py\'\n    })\n\n# Check for multiple launch scripts\nlaunch_files = list(repo_path.glob(\'**/launch_*.py\'))\nif len(launch_files) > 3:\n    analysis_results[\'architecture_gaps\'].append({\n        \'gap\': \'Multiple Launch Scripts\',\n        \'description\': f\'Found {len(launch_files)} launch scripts that could be consolidated\',\n        \'files\': [str(f) for f in launch_files],\n        \'priority\': \'medium\',\n        \'recommendation\': \'Consider creating a unified launcher with configuration options\'\n    })\n\n# Check for test file organization\ntest_files = list(repo_path.glob(\'**/test_*.py\'))\nif test_files and not (repo_path / \'tests\').exists():\n    analysis_results[\'architecture_gaps\'].append({\n        \'gap\': \'Test Files Not Organized\',\n        \'description\': f\'Found {len(test_files)} test files scattered throughout the codebase\',\n        \'files\': [str(f) for f in test_files[:10]],\n        \'priority\': \'medium\',\n        \'recommendation\': \'Organize test files into a dedicated tests/ directory\'\n    })\n\n# 3. Error Handling Analysis\nprint("🚨 Analyzing error handling...")\n\nerror_patterns = [\n    r\'except\\s+Exception\\s*:\',  # Generic exception handling\n    r\'except\\s*:\',  # Bare except clauses\n]\n\nerror_issues = []\nfor pattern in error_patterns:\n    for file in python_files[:10]:  # Test with first 10 files\n        try:\n            with open(file, \'r\') as f:\n                content = f.read()\n                matches = re.finditer(pattern, content, re.MULTILINE)\n                for match in matches:\n                    line_num = content[:match.start()].count(\'\\n\') + 1\n                    error_issues.append({\n                        \'file\': str(file),\n                        \'line\': line_num,\n                        \'pattern\': pattern,\n                        \'context\': content.split(\'\\n\')[line_num-1].strip()[:100]\n                    })\n        except Exception:\n            continue\n\nif error_issues:\n    analysis_results[\'code_quality_issues\'].extend(error_issues[:20])\n\n# 4. Large Files Analysis\nprint("📏 Analyzing file sizes...")\nlarge_files = []\nfor file in python_files:\n    try:\n        size = file.stat().st_size\n        if size > 50000:  # Files larger than 50KB\n            with open(file, \'r\') as f:\n                lines = len(f.readlines())\n            large_files.append({\n                \'file\': str(file),\n                \'size_kb\': size // 1024,\n                \'lines\': lines,\n                \'issue\': \'Large file that may need refactoring\',\n                \'priority\': \'medium\'\n            })\n    except Exception:\n        continue\n\nif large_files:\n    analysis_results[\'code_quality_issues\'].extend(large_files[:5])\n\n# 5. Security Analysis\nprint("🔒 Analyzing security patterns...")\n\nsecurity_patterns = [\n    r\'password\\s*=\',  # Hardcoded passwords\n    r\'api_key\\s*=\',  # Hardcoded API keys\n    r\'secret\\s*=\',  # Hardcoded secrets\n    r\'eval\\s*\\(\',  # eval() usage\n    r\'exec\\s*\\(\',  # exec() usage\n    r\'os\\.system\\s*\\(\',  # os.system() usage\n    r\'subprocess\\.call\\s*\\(\',  # subprocess.call() usage\n]\n\nsecurity_issues = []\nfor pattern in security_patterns:\n    for file in python_files:\n        try:\n            with open(file, \'r\') as f:\n                content = f.read()\n                matches = re.finditer(pattern, content, re.MULTILINE)\n                for match in matches:\n                    line_num = content[:match.start()].count(\'\\n\') + 1\n                    context = content.split(\'\\n\')[line_num-1].strip()[:100]\n                    security_issues.append({\n                        \'file\': str(file),\n                        \'line\': line_num,\n                        \'pattern\': pattern,\n                        \'context\': context,\n                        \'issue\': \'Potential security vulnerability\',\n                        \'priority\': \'high\'\n                    })\n        except Exception:\n            continue\n\nif security_issues:\n    analysis_results[\'code_quality_issues\'].extend(security_issues[:10])\n\n# Set GitHub Actions outputs\nfor key, value in analysis_results.items():\n    output_file = os.environ.get(\'GITHUB_OUTPUT\')\n    if output_file:\n        with open(output_file, \'a\') as f:\n            f.write(f"{key}={json.dumps(value)}\\n")\n    else:\n        print(f"Warning: GITHUB_OUTPUT not set, using fallback output")\n        print(f"::set-output name={key}::{json.dumps(value)}")\n\nprint(f"✅ Analysis complete. Found:")\nprint(f"  - {len(analysis_results[\'code_quality_issues\'])} code quality issues")\nprint(f"  - {len(analysis_results[\'architecture_gaps\'])} architecture gaps")\nprint(f"  - {len(analysis_results[\'test_coverage_gaps\'])} test coverage gaps")\nprint(f"  - {len(analysis_results[\'dependency_issues\'])} dependency issues")\nprint(f"  - {len(analysis_results[\'documentation_gaps\'])} documentation gaps")\n'
    result = subprocess.run(['python3', '-c', analysis_script], env=env, capture_output=True, text=True)
    print(result.stdout)
    if result.stderr:
        print('Errors:')
        print(result.stderr)
    outputs = {}
    try:
        with open(output_file, 'r') as f:
            for line in f:
                if '=' in line:
                    key, value = line.strip().split('=', 1)
                    outputs[key] = value
    except Exception as e:
        print(f'Error reading outputs: {e}')
    os.unlink(output_file)
    return outputs
def create_issues(outputs):
    print('\n🔧 Creating GitHub Issues...')
    print('=' * 50)
    def parse_output(output_str):
        try:
            return json.loads(output_str) if output_str else []
        except json.JSONDecodeError:
            print(f'Failed to parse output: {output_str[:200]}...')
            return []
    code_quality_issues = parse_output(outputs.get('code_quality_issues', '[]'))
    architecture_gaps = parse_output(outputs.get('architecture_gaps', '[]'))
    test_coverage_gaps = parse_output(outputs.get('test_coverage_gaps', '[]'))
    dependency_issues = parse_output(outputs.get('dependency_issues', '[]'))
    documentation_gaps = parse_output(outputs.get('documentation_gaps', '[]'))
    print('📊 Analysis Results:')
    print(f'  - Code Quality Issues: {len(code_quality_issues)}')
    print(f'  - Architecture Gaps: {len(architecture_gaps)}')
    print(f'  - Test Coverage Gaps: {len(test_coverage_gaps)}')
    print(f'  - Dependency Issues: {len(dependency_issues)}')
    print(f'  - Documentation Gaps: {len(documentation_gaps)}')
    issues_created = 0
    for gap in architecture_gaps:
        title = f"🏗️ {gap['gap']}"
        print(f'✅ Would create issue: {title}')
        print(f"   Description: {gap['description']}")
        print(f"   Priority: {gap['priority']}")
        print(f"   Recommendation: {gap['recommendation']}")
        print()
        issues_created += 1
    for gap in documentation_gaps:
        title = f"📚 {gap['gap']}"
        print(f'✅ Would create issue: {title}')
        print(f"   Description: {gap['description']}")
        print(f"   Priority: {gap['priority']}")
        print()
        issues_created += 1
    for gap in test_coverage_gaps:
        title = f"🧪 {gap['gap']}"
        print(f'✅ Would create issue: {title}')
        print(f"   Description: {gap['description']}")
        print(f"   Priority: {gap['priority']}")
        print()
        issues_created += 1
    for issue_data in dependency_issues:
        title = f"📦 {issue_data['gap']}"
        print(f'✅ Would create issue: {title}')
        print(f"   Description: {issue_data['description']}")
        print(f"   Priority: {issue_data['priority']}")
        print()
        issues_created += 1
    if code_quality_issues:
        issues_by_file = {}
        for issue in code_quality_issues:
            file = issue.get('file', 'unknown')
            if file not in issues_by_file:
                issues_by_file[file] = []
            issues_by_file[file].append(issue)
        for file, issues in list(issues_by_file.items())[:5]:
            title = f'🔧 Code Quality Issues in {Path(file).name}'
            print(f'✅ Would create issue: {title}')
            print(f'   File: {file}')
            print(f'   Issues Found: {len(issues)}')
            print('   Sample issues:')
            for issue in issues[:3]:
                if 'message' in issue:
                    print(f"     - Line {issue.get('line', 'N/A')}: {issue.get('message', 'Unknown issue')}")
                elif 'issue' in issue:
                    print(f"     - Line {issue.get('line', 'N/A')}: {issue.get('issue', 'Unknown issue')}")
                else:
                    print(f"     - Line {issue.get('line', 'N/A')}: {issue.get('pattern', 'Unknown pattern')}")
            print()
            issues_created += 1
    if issues_created > 0:
        print(f'🎉 Would create {issues_created} issues for dtecho to work on!')
    else:
        print('✅ No issues found! Your codebase appears to be in good shape.')
        print('💡 Consider running manual code reviews or adding more comprehensive tests.')
    return issues_created
def main():
    print('🚀 EchoPilot Manual Trigger')
    print('=' * 50)
    print(f"Started at: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print()
    outputs = run_analysis()
    issues_created = create_issues(outputs)
    print('\n✅ EchoPilot run complete!')
    print(f'Total issues that would be created: {issues_created}')
if __name__ == '__main__':
    main()