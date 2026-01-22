import json
import sys
import os
from typing import Dict, List, Any
def load_config(config_path: str) -> Dict[str, Any]:
    try:
        with open(config_path, 'r') as f:
            config = json.load(f)
        print(f'✅ Successfully loaded configuration from {config_path}')
        return config
    except FileNotFoundError:
        print(f'❌ Configuration file not found: {config_path}')
        sys.exit(1)
    except json.JSONDecodeError as e:
        print(f'❌ Invalid JSON in configuration: {e}')
        sys.exit(1)
def validate_feature(feature: Dict[str, Any], feature_index: int) -> List[str]:
    errors = []
    required_fields = ['id', 'title', 'category', 'priority', 'complexity', 'estimated_hours', 'description', 'requirements', 'test_requirements', 'dependencies']
    for field in required_fields:
        if field not in feature:
            errors.append(f"Feature {feature_index}: Missing required field '{field}'")
    if 'id' in feature and (not isinstance(feature['id'], str)):
        errors.append(f"Feature {feature_index}: 'id' must be a string")
    if 'priority' in feature and feature['priority'] not in ['high', 'medium', 'low']:
        errors.append(f"Feature {feature_index}: 'priority' must be 'high', 'medium', or 'low'")
    if 'complexity' in feature and feature['complexity'] not in ['high', 'medium', 'low']:
        errors.append(f"Feature {feature_index}: 'complexity' must be 'high', 'medium', or 'low'")
    if 'estimated_hours' in feature and (not isinstance(feature['estimated_hours'], (int, float))):
        errors.append(f"Feature {feature_index}: 'estimated_hours' must be a number")
    if 'requirements' in feature and (not isinstance(feature['requirements'], list)):
        errors.append(f"Feature {feature_index}: 'requirements' must be a list")
    if 'test_requirements' in feature and (not isinstance(feature['test_requirements'], list)):
        errors.append(f"Feature {feature_index}: 'test_requirements' must be a list")
    if 'dependencies' in feature and (not isinstance(feature['dependencies'], list)):
        errors.append(f"Feature {feature_index}: 'dependencies' must be a list")
    return errors
def validate_configuration(config: Dict[str, Any]) -> bool:
    print('\n🔍 Validating configuration structure...')
    errors = []
    if 'features' not in config:
        errors.append("Missing 'features' array in configuration")
    elif not isinstance(config['features'], list):
        errors.append("'features' must be an array")
    if 'workflow_config' not in config:
        errors.append("Missing 'workflow_config' object in configuration")
    if 'features' in config and isinstance(config['features'], list):
        feature_ids = set()
        for i, feature in enumerate(config['features']):
            feature_errors = validate_feature(feature, i)
            errors.extend(feature_errors)
            if 'id' in feature:
                if feature['id'] in feature_ids:
                    errors.append(f"Duplicate feature ID: {feature['id']}")
                feature_ids.add(feature['id'])
    if errors:
        print('❌ Configuration validation failed:')
        for error in errors:
            print(f'   {error}')
        return False
    else:
        print('✅ Configuration validation passed')
        return True
def simulate_issue_generation(config: Dict[str, Any], filters: Dict[str, str]) -> List[Dict[str, Any]]:
    features = config['features']
    filtered_features = features
    if filters.get('category') and filters['category'] != 'all':
        category_filters = [f.strip() for f in filters['category'].split(',')]
        filtered_features = [f for f in filtered_features if f['category'] in category_filters]
    if filters.get('priority') and filters['priority'] != 'all':
        filtered_features = [f for f in filtered_features if f['priority'] == filters['priority']]
    print(f'\n📋 Simulation: {len(filtered_features)} features match filters')
    issues = []
    for feature in filtered_features:
        issue_title = f"[C++ Kernel] {feature['title']}"
        labels = ['cpp-kernel-implementation', 'dtesn', 'kernel', f"priority-{feature['priority']}", f"complexity-{feature['complexity']}", f"category-{feature['category'].lower().replace(' ', '-')}"]
        issue = {'title': issue_title, 'feature_id': feature['id'], 'category': feature['category'], 'priority': feature['priority'], 'complexity': feature['complexity'], 'estimated_hours': feature['estimated_hours'], 'labels': labels, 'requirements_count': len(feature['requirements']), 'test_requirements_count': len(feature['test_requirements']), 'dependencies_count': len(feature['dependencies'])}
        issues.append(issue)
    return issues
def generate_issue_body_preview(feature: Dict[str, Any]) -> str:
    lines = []
    lines.append(f"## {feature['title']}\n")
    lines.append(f"**Category:** {feature['category']} | **Priority:** {feature['priority']} | **Complexity:** {feature['complexity']}")
    lines.append(f"**Estimated Effort:** {feature['estimated_hours']} hours\n")
    lines.append(f"### Description\n{feature['description']}\n")
    lines.append('### Implementation Requirements')
    for i, req in enumerate(feature['requirements'][:3]):
        lines.append(f'- [ ] {req}')
    if len(feature['requirements']) > 3:
        lines.append(f"... and {len(feature['requirements']) - 3} more requirements")
    lines.append('')
    if 'technical_specs' in feature and 'performance_targets' in feature['technical_specs']:
        lines.append('### Performance Targets')
        targets = feature['technical_specs']['performance_targets']
        for key, value in list(targets.items())[:3]:
            lines.append(f"- {key.replace('_', ' ')}: {value}")
        if len(targets) > 3:
            lines.append(f'... and {len(targets) - 3} more targets')
        lines.append('')
    if 'code_template' in feature:
        template = feature['code_template']
        lines.append('### Code Structure')
        if 'header_file' in template:
            lines.append(f"- Header: `{template['header_file']}`")
        if 'source_file' in template:
            lines.append(f"- Source: `{template['source_file']}`")
        lines.append('')
    lines.append('*[Full issue would include complete specifications, testing requirements, and implementation guidelines]*')
    return '\n'.join(lines)
def print_feature_summary(config: Dict[str, Any]):
    features = config['features']
    print(f'\n📊 Feature Summary ({len(features)} total features):')
    print('=' * 80)
    categories = {}
    total_hours = 0
    for feature in features:
        category = feature['category']
        if category not in categories:
            categories[category] = []
        categories[category].append(feature)
        total_hours += feature['estimated_hours']
    for category, cat_features in categories.items():
        cat_hours = sum((f['estimated_hours'] for f in cat_features))
        print(f'\n🏷️  {category} ({len(cat_features)} features, {cat_hours}h total):')
        for feature in sorted(cat_features, key=lambda x: x['priority'], reverse=True):
            priority_icon = '🔴' if feature['priority'] == 'high' else '🟡' if feature['priority'] == 'medium' else '🟢'
            complexity_icon = '⚫' if feature['complexity'] == 'high' else '🔵' if feature['complexity'] == 'medium' else '⚪'
            print(f"   {priority_icon}{complexity_icon} {feature['id']}: {feature['title']}")
            print(f"      └─ {feature['estimated_hours']}h, {len(feature['requirements'])} requirements, {len(feature['test_requirements'])} tests")
    print(f'\n📈 Total Estimated Effort: {total_hours} hours')
    priority_counts = {'high': 0, 'medium': 0, 'low': 0}
    for feature in features:
        priority_counts[feature['priority']] += 1
    print(f"🎯 Priority Distribution: {priority_counts['high']} high, {priority_counts['medium']} medium, {priority_counts['low']} low")
def interactive_mode(config: Dict[str, Any]):
    print('\n🖥️  Interactive Mode')
    print('Commands:')
    print('  list [category] - List features (optionally filtered by category)')
    print('  show <feature_id> - Show detailed feature information')
    print('  preview <feature_id> - Generate issue body preview')
    print('  simulate [category] [priority] - Simulate issue generation')
    print('  categories - Show available categories')
    print('  help - Show this help')
    print('  quit - Exit interactive mode')
    while True:
        try:
            command = input('\n> ').strip().split()
            if not command:
                continue
            cmd = command[0].lower()
            if cmd == 'quit':
                break
            elif cmd == 'help':
                print('Commands: list, show, preview, simulate, categories, help, quit')
            elif cmd == 'categories':
                categories = set((f['category'] for f in config['features']))
                print('Available categories:', ', '.join(sorted(categories)))
            elif cmd == 'list':
                category_filter = command[1] if len(command) > 1 else None
                features = config['features']
                if category_filter:
                    features = [f for f in features if f['category'].lower() == category_filter.lower()]
                for f in features:
                    print(f"  {f['id']}: {f['title']} ({f['category']}, {f['priority']} priority)")
            elif cmd == 'show' and len(command) > 1:
                feature_id = command[1]
                feature = next((f for f in config['features'] if f['id'] == feature_id), None)
                if feature:
                    print(json.dumps(feature, indent=2))
                else:
                    print(f'Feature not found: {feature_id}')
            elif cmd == 'preview' and len(command) > 1:
                feature_id = command[1]
                feature = next((f for f in config['features'] if f['id'] == feature_id), None)
                if feature:
                    print('\n' + '=' * 60)
                    print(generate_issue_body_preview(feature))
                    print('=' * 60)
                else:
                    print(f'Feature not found: {feature_id}')
            elif cmd == 'simulate':
                filters = {'category': command[1] if len(command) > 1 else 'all', 'priority': command[2] if len(command) > 2 else 'all'}
                issues = simulate_issue_generation(config, filters)
                print(f'\nWould generate {len(issues)} issues:')
                for issue in issues:
                    print(f"  📝 {issue['title']}")
                    print(f"     Category: {issue['category']}, Priority: {issue['priority']}, Hours: {issue['estimated_hours']}")
            else:
                print("Unknown command. Type 'help' for available commands.")
        except KeyboardInterrupt:
            print('\nExiting interactive mode...')
            break
        except Exception as e:
            print(f'Error: {e}')
def main():
    print('🚀 C/C++ Kernel Implementation Issue Generator Validator')
    print('=' * 60)
    script_dir = os.path.dirname(os.path.abspath(__file__))
    config_path = os.path.join(script_dir, '.github', 'cpp-kernel-features.json')
    if not os.path.exists(config_path):
        config_path = '.github/cpp-kernel-features.json'
    if not os.path.exists(config_path):
        print('❌ Configuration file not found. Expected location:')
        print('   .github/cpp-kernel-features.json')
        sys.exit(1)
    config = load_config(config_path)
    if not validate_configuration(config):
        sys.exit(1)
    print_feature_summary(config)
    print('\n🧪 Testing Issue Generation Simulation:')
    test_filters = [{'category': 'all', 'priority': 'all'}, {'category': 'Core Kernel', 'priority': 'all'}, {'category': 'all', 'priority': 'high'}, {'category': 'DTESN Core,Neural Computing', 'priority': 'high'}]
    for filters in test_filters:
        print(f"\n   Filter: category={filters['category']}, priority={filters['priority']}")
        issues = simulate_issue_generation(config, filters)
        print(f'   Result: {len(issues)} issues would be generated')
    if len(sys.argv) > 1 and sys.argv[1] == '--interactive':
        interactive_mode(config)
    else:
        print('\n✅ Validation complete!')
        print('\nTo explore features interactively, run:')
        print('   python3 scripts/validate-cpp-kernel-config.py --interactive')
        print('\nTo test the GitHub workflow:')
        print('   1. Go to https://github.com/EchoCog/echo.kern/actions')
        print("   2. Find 'Generate C/C++ Kernel Implementation Issues'")
        print("   3. Click 'Run workflow' and test with dry-run enabled")
if __name__ == '__main__':
    main()