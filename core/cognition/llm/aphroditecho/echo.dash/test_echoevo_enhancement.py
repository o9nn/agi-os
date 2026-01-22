import re
import yaml
from pathlib import Path
def test_markdown_structure():
    echoevo_path = Path('Echoevo.md')
    if not echoevo_path.exists():
        print('❌ Echoevo.md file not found')
        return False
    with open(echoevo_path, 'r') as f:
        content = f.read()
    required_sections = ['# 🌳 Echoevo: Neural-Symbolic Self-Evolving Workflow Architecture 🌳', '## Introduction', '## 🧠 Cognitive Flowchart: Recursive Self-Evolution Architecture', '## I. Distributed Cognition: Alternating Self-Modifying Agents', '## II. Implementation: Neural-Symbolic Workflow Pairs', '## III. Enhanced Python Scripts: Cognitive Self-Improvement Logic', '## IV. Enhanced Safety Mechanisms: Multi-Layer Cognitive Protection', '## V. Enriched Potential Experiments: Cognitive Evolution Laboratory', '## VI. Advanced Monitoring and Telemetry Integration', '## Conclusion']
    missing_sections = []
    for section in required_sections:
        if section not in content:
            missing_sections.append(section)
    if missing_sections:
        print(f'❌ Missing required sections: {missing_sections}')
        return False
    print('✅ All required sections present')
    return True
def test_mermaid_diagram():
    echoevo_path = Path('Echoevo.md')
    with open(echoevo_path, 'r') as f:
        content = f.read()
    mermaid_pattern = '```mermaid\\n(.*?)\\n```'
    mermaid_blocks = re.findall(mermaid_pattern, content, re.DOTALL)
    if not mermaid_blocks:
        print('❌ No Mermaid diagrams found')
        return False
    for i, block in enumerate(mermaid_blocks):
        if 'graph TD' not in block and 'graph LR' not in block:
            print(f'❌ Mermaid block {i + 1} missing graph declaration')
            return False
        if '-->' not in block:
            print(f'❌ Mermaid block {i + 1} missing node connections')
            return False
    print(f'✅ Found {len(mermaid_blocks)} properly formatted Mermaid diagram(s)')
    return True
def test_code_blocks():
    echoevo_path = Path('Echoevo.md')
    with open(echoevo_path, 'r') as f:
        content = f.read()
    yaml_pattern = '```yaml\\n(.*?)\\n```'
    yaml_blocks = re.findall(yaml_pattern, content, re.DOTALL)
    if not yaml_blocks:
        print('❌ No YAML code blocks found')
        return False
    valid_yaml_count = 0
    for i, block in enumerate(yaml_blocks):
        try:
            yaml.safe_load(block)
            valid_yaml_count += 1
        except yaml.YAMLError as e:
            print(f'⚠️ YAML block {i + 1} has syntax issues: {e}')
    print(f'✅ Found {len(yaml_blocks)} YAML blocks, {valid_yaml_count} syntactically valid')
    python_pattern = '```python\\n(.*?)\\n```'
    python_blocks = re.findall(python_pattern, content, re.DOTALL)
    if not python_blocks:
        print('❌ No Python code blocks found')
        return False
    print(f'✅ Found {len(python_blocks)} Python code block(s)')
    return True
def test_neural_symbolic_language():
    echoevo_path = Path('Echoevo.md')
    with open(echoevo_path, 'r') as f:
        content = f.read()
    neural_symbolic_terms = ['neural-symbolic', 'cognitive', 'distributed cognition', 'recursive self-improvement', 'pattern recognition', 'symbolic reasoning', 'cognitive architecture', 'cognitive coherence']
    found_terms = []
    for term in neural_symbolic_terms:
        if term.lower() in content.lower():
            found_terms.append(term)
    if len(found_terms) < len(neural_symbolic_terms) * 0.7:
        print(f'❌ Insufficient neural-symbolic terminology. Found: {found_terms}')
        return False
    print(f'✅ Neural-symbolic terminology properly used ({len(found_terms)}/{len(neural_symbolic_terms)} terms)')
    return True
def test_safety_mechanisms():
    echoevo_path = Path('Echoevo.md')
    with open(echoevo_path, 'r') as f:
        content = f.read()
    safety_keywords = ['safety', 'rollback', 'validation', 'emergency', 'security', 'threshold', 'monitoring']
    safety_mentions = 0
    for keyword in safety_keywords:
        safety_mentions += content.lower().count(keyword)
    if safety_mentions < 20:
        print(f'❌ Insufficient safety mechanism documentation ({safety_mentions} mentions)')
        return False
    print(f'✅ Safety mechanisms comprehensively documented ({safety_mentions} safety-related mentions)')
    return True
def test_experimental_framework():
    echoevo_path = Path('Echoevo.md')
    with open(echoevo_path, 'r') as f:
        content = f.read()
    experimental_elements = ['environment variables', 'logging', 'monitoring', 'telemetry', 'metrics', 'experiments']
    found_elements = []
    for element in experimental_elements:
        if element.lower() in content.lower():
            found_elements.append(element)
    if len(found_elements) < len(experimental_elements) * 0.8:
        print(f'❌ Insufficient experimental framework. Found: {found_elements}')
        return False
    print(f'✅ Experimental framework properly enriched ({len(found_elements)}/{len(experimental_elements)} elements)')
    return True
def main():
    print('🧪 Testing enhanced Echoevo.md structure and content...')
    print('=' * 60)
    tests = [('Markdown Structure', test_markdown_structure), ('Mermaid Diagrams', test_mermaid_diagram), ('Code Blocks', test_code_blocks), ('Neural-Symbolic Language', test_neural_symbolic_language), ('Safety Mechanisms', test_safety_mechanisms), ('Experimental Framework', test_experimental_framework)]
    passed_tests = 0
    total_tests = len(tests)
    for test_name, test_func in tests:
        print(f'\n🔍 Testing {test_name}...')
        try:
            if test_func():
                passed_tests += 1
            else:
                print(f'❌ {test_name} test failed')
        except Exception as e:
            print(f'💥 {test_name} test error: {e}')
    print('\n' + '=' * 60)
    print(f'📊 Test Results: {passed_tests}/{total_tests} tests passed')
    if passed_tests == total_tests:
        print('🎉 All tests passed! Enhanced Echoevo.md is ready.')
        return True
    else:
        print('⚠️ Some tests failed. Please review the issues above.')
        return False
if __name__ == '__main__':
    main()