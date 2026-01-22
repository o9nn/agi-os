import sys
import re
from pathlib import Path
def validate_roadmap(roadmap_file):
    if not Path(roadmap_file).exists():
        print(f'❌ Roadmap file not found: {roadmap_file}')
        return False
    with open(roadmap_file, 'r', encoding='utf-8') as f:
        content = f.read()
    errors = []
    warnings = []
    required_sections = ['Executive Summary', 'Architecture Overview', 'Development Phases', 'Implementation Architecture', 'Success Metrics', 'Next Development Steps']
    for section in required_sections:
        if section not in content:
            errors.append(f'Missing required section: {section}')
    phase_pattern = '### Phase \\d+(?:\\.\\d+)?: .+ \\((.+?)\\)'
    phases = re.findall(phase_pattern, content)
    if len(phases) < 4:
        errors.append(f'Expected at least 4 phases, found {len(phases)}')
    task_pattern = '- \\[ \\] \\*\\*Task ([^*]+)\\*\\*: (.+)'
    tasks = re.findall(task_pattern, content)
    print('📊 Roadmap Validation Results:')
    print(f'   Phases found: {len(phases)}')
    print(f'   Tasks found: {len(tasks)}')
    task_ids = [task[0] for task in tasks]
    duplicates = set([tid for tid in task_ids if task_ids.count(tid) > 1])
    if duplicates:
        errors.append(f'Duplicate task IDs: {duplicates}')
    invalid_ids = []
    for task_id, _ in tasks:
        if not re.match('\\d+\\.\\d+\\.\\d+', task_id):
            invalid_ids.append(task_id)
    if invalid_ids:
        warnings.append(f'Invalid task ID format: {invalid_ids}')
    tasks_without_criteria = []
    lines = content.split('\n')
    current_task = None
    for line in lines:
        line = line.strip()
        if line.startswith('- [ ] **Task'):
            task_match = re.match('- \\[ \\] \\*\\*Task ([^*]+)\\*\\*:', line)
            if task_match:
                current_task = task_match.group(1)
        elif '**Acceptance Criteria**:' in line and current_task:
            current_task = None
        elif line.startswith('- [ ] **Task') and current_task:
            tasks_without_criteria.append(current_task)
            current_task = None
    if tasks_without_criteria:
        warnings.append(f'Tasks without acceptance criteria: {tasks_without_criteria}')
    if errors:
        print('\n❌ Errors found:')
        for error in errors:
            print(f'   - {error}')
    if warnings:
        print('\n⚠️  Warnings:')
        for warning in warnings:
            print(f'   - {warning}')
    if not errors and (not warnings):
        print('\n✅ Roadmap validation passed!')
        return True
    elif not errors:
        print('\n✅ Roadmap validation passed with warnings')
        return True
    else:
        print('\n❌ Roadmap validation failed')
        return False
def main():
    roadmap_file = sys.argv[1] if len(sys.argv) > 1 else 'DEEP_TREE_ECHO_ROADMAP.md'
    print(f'🔍 Validating roadmap: {roadmap_file}')
    success = validate_roadmap(roadmap_file)
    sys.exit(0 if success else 1)
if __name__ == '__main__':
    main()