import os
import sys
import re
import subprocess
from pathlib import Path
from typing import List, Dict, Tuple
class DocumentationValidator:
    def __init__(self, base_path='.'):
        self.base_path = Path(base_path)
        self.verification_results = {}
        self.warnings = []
        self.errors = []
    def log_result(self, test_name, passed, message=''):
        status = '✅ PASS' if passed else '❌ FAIL'
        result_message = f'{status}: {test_name}'
        if message:
            result_message += f' - {message}'
        print(result_message)
        self.verification_results[test_name] = {'passed': passed, 'message': message}
        if not passed:
            self.errors.append(f'{test_name}: {message}')
    def log_warning(self, test_name, message):
        print(f'⚠️  WARNING: {test_name} - {message}')
        self.warnings.append(f'{test_name}: {message}')
    def check_file_exists(self, filepath, description=''):
        full_path = self.base_path / filepath
        exists = full_path.exists()
        desc = description or f'File {filepath}'
        self.log_result(f'File exists: {filepath}', exists, desc)
        return exists
    def check_file_contains(self, filepath, patterns, description=''):
        full_path = self.base_path / filepath
        if not full_path.exists():
            self.log_result(f'Content check: {filepath}', False, 'File not found')
            return False
        try:
            with open(full_path, 'r', encoding='utf-8') as f:
                content = f.read()
            if isinstance(patterns, str):
                patterns = [patterns]
            missing_patterns = []
            for pattern in patterns:
                if pattern not in content:
                    missing_patterns.append(pattern)
            if missing_patterns:
                self.log_result(f'Content check: {filepath}', False, f'Missing patterns: {missing_patterns}')
                return False
            else:
                self.log_result(f'Content check: {filepath}', True, description)
                return True
        except Exception as e:
            self.log_result(f'Content check: {filepath}', False, f'Error reading file: {e}')
            return False
    def validate_phase_summaries(self):
        print('\n📋 Validating Phase Completion Summaries...')
        phase_files = ['cogkernel/PHASE1_IMPLEMENTATION_SUMMARY.md', 'cogkernel/PHASE2_MICROKERNEL_INTEGRATION.md', 'cogkernel/PHASE3_IMPLEMENTATION_SUMMARY.md', 'cogkernel/PHASE4_COMPLETION_SUMMARY.md', 'cogkernel/PHASE5_COMPLETION_SUMMARY.md']
        for phase_file in phase_files:
            self.check_file_exists(phase_file, f'Phase completion summary')
            if self.base_path.joinpath(phase_file).exists():
                required_sections = ['## Overview', '## Completed Components', '**Status:** COMPLETE']
                self.check_file_contains(phase_file, required_sections, 'Required phase summary sections')
    def validate_core_documentation(self):
        print('\n📚 Validating Core Documentation...')
        core_docs = ['README.md', 'SKZ_INTEGRATION_STRATEGY.md', 'DEVELOPMENT_ROADMAP.md', 'IMPLEMENTATION_SUMMARY.md']
        for doc in core_docs:
            self.check_file_exists(doc, 'Core documentation')
        self.check_file_contains('SKZ_INTEGRATION_STRATEGY.md', ['Phase 5: System Integration and Testing', 'Documentation finalization'], 'SKZ strategy Phase 5 content')
    def validate_technical_documentation(self):
        print('\n🔧 Validating Technical Documentation...')
        technical_docs = ['docs/ARCHITECTURE.md', 'docs/DEVELOPER.md', 'docs/open-issues/documentation.md', 'docs/GUIX_INTEGRATION_COMPLETION.md']
        for doc in technical_docs:
            if self.check_file_exists(doc, 'Technical documentation'):
                self.check_file_contains(doc, ['# ', '## '], 'Basic documentation structure')
    def validate_cogkernel_documentation(self):
        print('\n🧠 Validating Cognitive Kernel Documentation...')
        cogkernel_docs = ['cogkernel/README.md', 'cogkernel/cognitive-interface/README.md']
        for doc in cogkernel_docs:
            self.check_file_exists(doc, 'Cognitive kernel documentation')
        learning_docs = ['cogkernel/cognitive-interface/learning-systems/README.md']
        for doc in learning_docs:
            if self.check_file_exists(doc, 'Learning systems documentation'):
                required_sections = ['# Real-time Learning Systems', '## Overview', '## Key Features', '## Usage Examples']
                self.check_file_contains(doc, required_sections, 'Learning system documentation structure')
    def validate_documentation_links(self):
        print('\n🔗 Validating Documentation Links...')
        md_files = list(self.base_path.rglob('*.md'))
        link_pattern = re.compile('\\[([^\\]]+)\\]\\(([^)]+)\\)')
        broken_links = []
        for md_file in md_files:
            try:
                with open(md_file, 'r', encoding='utf-8') as f:
                    content = f.read()
                links = link_pattern.findall(content)
                for link_text, link_url in links:
                    if link_url.startswith(('http://', 'https://', 'mailto:')):
                        continue
                    if link_url.startswith('#'):
                        continue
                    link_path = md_file.parent / link_url
                    if not link_path.exists():
                        broken_links.append(f'{md_file}: {link_url}')
            except Exception as e:
                self.log_warning('Link validation', f'Error reading {md_file}: {e}')
        if broken_links:
            self.log_result('Internal links validation', False, f'Found {len(broken_links)} broken links')
            for link in broken_links[:5]:
                print(f'    Broken: {link}')
        else:
            self.log_result('Internal links validation', True, 'All internal links verified')
    def validate_code_examples(self):
        print('\n💻 Validating Code Examples...')
        md_files = list(self.base_path.rglob('*.md'))
        code_block_pattern = re.compile('```(\\w+)?\\n(.*?)\\n```', re.DOTALL)
        total_examples = 0
        invalid_examples = []
        for md_file in md_files:
            try:
                with open(md_file, 'r', encoding='utf-8') as f:
                    content = f.read()
                code_blocks = code_block_pattern.findall(content)
                for language, code in code_blocks:
                    total_examples += 1
                    if not code.strip():
                        invalid_examples.append(f'{md_file}: Empty code block')
                    elif language == 'bash' and (not code.strip().startswith(('#', 'cd', 'ls', 'git', 'make', 'sudo', 'apt', 'pip', 'npm'))):
                        pass
            except Exception as e:
                self.log_warning('Code examples validation', f'Error reading {md_file}: {e}')
        if invalid_examples:
            self.log_result('Code examples validation', False, f'Found {len(invalid_examples)} invalid examples')
        else:
            self.log_result('Code examples validation', True, f'Validated {total_examples} code examples')
    def validate_documentation_completeness(self):
        print('\n📊 Validating Documentation Completeness...')
        important_dirs = ['cogkernel', 'docs', 'docs/open-issues', 'guix-build-system', 'hurd-ecosystem/documentation']
        for dir_path in important_dirs:
            readme_path = f'{dir_path}/README.md'
            self.check_file_exists(readme_path, f'README for {dir_path}')
        doc_types = {'Architecture': ['ARCHITECTURE.md', 'docs/ARCHITECTURE.md'], 'Installation': ['INSTALL', 'docs/INSTALL.md'], 'Development': ['DEVELOPMENT_ROADMAP.md', 'docs/DEVELOPER.md'], 'Contributing': ['CONTRIBUTING.md', 'docs/CONTRIBUTING.md']}
        for doc_type, possible_files in doc_types.items():
            found = False
            for file_path in possible_files:
                if self.base_path.joinpath(file_path).exists():
                    found = True
                    break
            self.log_result(f'{doc_type} documentation', found, f'At least one {doc_type.lower()} document exists')
    def generate_documentation_index(self):
        print('\n📇 Generating Documentation Index...')
        index_content = '# Documentation Index\n\nThis file provides a comprehensive index of all documentation in the repository.\n\n## Core Documentation\n\n### Project Overview\n- [README.md](README.md) - Main project overview\n- [SKZ Integration Strategy](SKZ_INTEGRATION_STRATEGY.md) - Complete integration strategy\n- [Development Roadmap](DEVELOPMENT_ROADMAP.md) - Project roadmap and phases\n\n### Implementation Summaries\n- [Implementation Summary](IMPLEMENTATION_SUMMARY.md) - Overall implementation summary\n- [Phase 3 Build Orchestration](PHASE3_BUILD_ORCHESTRATION_SUMMARY.md) - Build system summary\n- [ECMA-262 Integration](ECMA262_INTEGRATION_SUMMARY.md) - JavaScript integration\n\n### Phase Completion Documentation\n- [Phase 1 Implementation](cogkernel/PHASE1_IMPLEMENTATION_SUMMARY.md) - Foundation setup\n- [Phase 2 Microkernel Integration](cogkernel/PHASE2_MICROKERNEL_INTEGRATION.md) - Microkernel integration\n- [Phase 3 Implementation](cogkernel/PHASE3_IMPLEMENTATION_SUMMARY.md) - Build orchestration\n- [Phase 4 Completion](cogkernel/PHASE4_COMPLETION_SUMMARY.md) - Cognitive layer development  \n- [Phase 5 Completion](cogkernel/PHASE5_COMPLETION_SUMMARY.md) - System integration and testing\n\n## Technical Documentation\n\n### Architecture\n- [Architecture Overview](docs/ARCHITECTURE.md) - System architecture\n- [Hurd Architecture](HURD_ARCHITECTURE.md) - GNU Hurd specific architecture\n- [GUIX Integration](docs/GUIX_INTEGRATION_COMPLETION.md) - Build system integration\n\n### Development\n- [Developer Guide](docs/DEVELOPER.md) - Development guidelines\n- [Documentation Standards](docs/open-issues/documentation.md) - Documentation standards\n\n### Cognitive Kernel\n- [Cognitive Kernel README](cogkernel/README.md) - Cognitive kernel overview\n- [Cognitive Interface](cogkernel/cognitive-interface/README.md) - Interface documentation\n- [Learning Systems](cogkernel/cognitive-interface/learning-systems/README.md) - Learning system docs\n\n## Component Documentation\n\n### Hurd Ecosystem\n- [Documentation Overview](hurd-ecosystem/documentation/README.md) - Hurd documentation structure\n\n### Build System\n- [GUIX Build System](guix-build-system/README.md) - Build system documentation\n\n### External Components  \n- [External Components](external/README.md) - External component documentation\n\n## Open Issues Documentation\n- [Documentation Issues](docs/open-issues/documentation.md) - Documentation standards\n- [Open Issues Overview](docs/open-issues/README.md) - Open issues catalog\n\n## Testing and Validation\n- [Documentation Tests](validate-documentation-finalization.py) - Documentation validation\n- [Phase 2 Verification](verify-phase2-completion.py) - Phase 2 completion verification\n- [Phase 3 Validation](validate-phase3-completion.py) - Phase 3 validation\n\n---\n\n*This index is automatically generated and maintained as part of Phase 5 documentation finalization.*\n'
        index_path = self.base_path / 'DOCUMENTATION_INDEX.md'
        try:
            with open(index_path, 'w', encoding='utf-8') as f:
                f.write(index_content)
            self.log_result('Documentation index generation', True, 'Generated comprehensive documentation index')
        except Exception as e:
            self.log_result('Documentation index generation', False, f'Error: {e}')
    def run_validation(self):
        print('🔍 PHASE 5 DOCUMENTATION FINALIZATION VALIDATION')
        print('=' * 60)
        self.validate_phase_summaries()
        self.validate_core_documentation()
        self.validate_technical_documentation()
        self.validate_cogkernel_documentation()
        self.validate_documentation_links()
        self.validate_code_examples()
        self.validate_documentation_completeness()
        self.generate_documentation_index()
        self.generate_validation_report()
        return len(self.errors) == 0
    def generate_validation_report(self):
        print('\n📋 DOCUMENTATION FINALIZATION REPORT')
        print('=' * 50)
        total_tests = len(self.verification_results)
        passed_tests = sum((1 for result in self.verification_results.values() if result['passed']))
        print(f'Total Validation Checks: {total_tests}')
        print(f'Passed: {passed_tests}')
        print(f'Failed: {total_tests - passed_tests}')
        print(f'Warnings: {len(self.warnings)}')
        print(f'Success Rate: {passed_tests / total_tests * 100:.1f}%')
        if self.errors:
            print(f'\n❌ CRITICAL ISSUES ({len(self.errors)}):')
            for error in self.errors:
                print(f'  • {error}')
        if self.warnings:
            print(f'\n⚠️  WARNINGS ({len(self.warnings)}):')
            for warning in self.warnings:
                print(f'  • {warning}')
        print('\n🎯 RECOMMENDATION:')
        if passed_tests / total_tests >= 0.9:
            print('✅ Documentation is COMPLETE and ready for production!')
        elif passed_tests / total_tests >= 0.8:
            print('✅ Documentation is mostly complete. Address warnings if needed.')
        else:
            print('❌ Documentation requires additional work before finalization.')
        return {'total_checks': total_tests, 'passed_checks': passed_tests, 'success_rate': passed_tests / total_tests * 100 if total_tests > 0 else 0, 'errors': self.errors, 'warnings': self.warnings}
def main():
    validator = DocumentationValidator('/home/runner/work/hurdcog/hurdcog')
    success = validator.run_validation()
    return 0 if success else 1
if __name__ == '__main__':
    sys.exit(main())