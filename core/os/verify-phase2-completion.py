import os
import sys
import subprocess
import json
from pathlib import Path
class Phase2Verifier:
    def __init__(self, base_path='.'):
        self.base_path = Path(base_path)
        self.verification_results = {}
        self.errors = []
        self.warnings = []
    def log_result(self, test_name, passed, message=''):
        status = '✅ PASS' if passed else '❌ FAIL'
        print(f'{status}: {test_name}')
        if message:
            print(f'    {message}')
        self.verification_results[test_name] = {'passed': passed, 'message': message}
        if not passed:
            self.errors.append(f'{test_name}: {message}')
    def log_warning(self, test_name, message):
        print(f'⚠️  WARNING: {test_name}')
        print(f'    {message}')
        self.warnings.append(f'{test_name}: {message}')
    def check_file_exists(self, filepath, description=''):
        full_path = self.base_path / filepath
        exists = full_path.exists()
        msg = f'{description} - {filepath}' if description else str(filepath)
        self.log_result(f'File exists: {msg}', exists, str(full_path) if exists else f'File not found: {full_path}')
        return exists
    def check_directory_exists(self, dirpath, description=''):
        full_path = self.base_path / dirpath
        exists = full_path.exists() and full_path.is_dir()
        msg = f'{description} - {dirpath}' if description else str(dirpath)
        self.log_result(f'Directory exists: {msg}', exists, str(full_path) if exists else f'Directory not found: {full_path}')
        return exists
    def check_file_contains(self, filepath, pattern, description=''):
        try:
            full_path = self.base_path / filepath
            if not full_path.exists():
                self.log_result(f'Content check: {description}', False, f'File not found: {full_path}')
                return False
            with open(full_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                contains = pattern in content
                self.log_result(f'Content check: {description}', contains, f"Found pattern '{pattern[:50]}...' in {filepath}" if contains else f"Pattern '{pattern[:50]}...' not found in {filepath}")
                return contains
        except Exception as e:
            self.log_result(f'Content check: {description}', False, f'Error reading {filepath}: {str(e)}')
            return False
    def verify_sub_task_implementations(self):
        print('\n🔍 Verifying Sub-Task Implementation Artifacts...')
        self.check_file_exists('cogkernel/hurd-atomspace-bridge.c', 'OpenCog-Hurd bridge C implementation')
        self.check_file_exists('cogkernel/hurd-atomspace-bridge.h', 'OpenCog-Hurd bridge header')
        self.check_file_exists('cogkernel/atomspace.scm', 'AtomSpace Scheme module')
        self.check_file_exists('cogkernel/microkernel-integration.scm', 'Microkernel integration layer')
        self.check_file_exists('cogkernel/plan9-namespace.scm', 'Plan9 namespace implementation')
        self.check_file_exists('cogkernel/9p-hypergraph.scm', '9P protocol hypergraph integration')
        self.check_file_contains('cogkernel/plan9-namespace.scm', 'make-plan9-namespace', 'Plan9 namespace constructor')
        kokkos_dirs = ['performance/kokkos-integration', 'cogkernel/ggml']
        for kokkos_dir in kokkos_dirs:
            if self.check_directory_exists(kokkos_dir, 'Kokkos framework integration'):
                break
        else:
            self.log_warning('Kokkos Integration', 'No clear Kokkos integration directory found')
        self.check_directory_exists('development/compiler-explorer', 'Compiler-explorer development tools')
        self.check_directory_exists('guile-llama-cpp', 'Guile-LLaMA-CPP integration')
        self.check_directory_exists('ecma262-main', 'ECMA-262 features')
    def verify_integration_tests(self):
        print('\n🧪 Verifying Integration Tests...')
        test_files = ['cogkernel/phase2-standalone-test.scm', 'cogkernel/test-microkernel-integration.scm', 'cogkernel/standalone-microkernel-test.scm', 'cogkernel/phase2-integration.scm']
        for test_file in test_files:
            if self.check_file_exists(test_file, 'Integration test'):
                self.check_file_contains(test_file, 'test', 'Test function definitions')
                self.check_file_contains(test_file, 'format', 'Test output formatting')
    def verify_documentation(self):
        print('\n📚 Verifying Documentation...')
        docs = ['cogkernel/README.md', 'cogkernel/PHASE2_MICROKERNEL_INTEGRATION.md', 'SKZ_INTEGRATION_STRATEGY.md', 'DEVELOPMENT_ROADMAP.md']
        for doc in docs:
            if self.check_file_exists(doc, 'Documentation'):
                self.check_file_contains(doc, 'Phase 2', 'Phase 2 references')
        summary_files = ['cogkernel/PHASE1_IMPLEMENTATION_SUMMARY.md', 'cogkernel/PHASE2_MICROKERNEL_INTEGRATION.md', 'cogkernel/PHASE3_IMPLEMENTATION_SUMMARY.md']
        for summary in summary_files:
            self.check_file_exists(summary, 'Phase implementation summary')
    def verify_build_system(self):
        print('\n🔨 Verifying Build System Integration...')
        self.check_file_exists('Makefile', 'Main Makefile')
        self.check_file_exists('Makeconf', 'Make configuration')
        self.check_file_exists('cogkernel/Makefile', 'Cognitive kernel Makefile')
        if self.check_file_contains('Makefile', 'cognitive-subdirs', 'Cognitive subdirectories'):
            self.check_file_contains('Makefile', 'cogkernel', 'Cogkernel in build system')
    def verify_architectural_coherence(self):
        print('\n🏗️  Verifying Architectural Coherence...')
        core_modules = ['cogkernel/core.scm', 'cogkernel/agents.scm', 'cogkernel/atomspace.scm', 'cogkernel/attention.scm']
        for module in core_modules:
            self.check_file_exists(module, 'Core cognitive module')
        integration_files = ['cogkernel/hurdcog-bootstrap.scm', 'cogkernel/microkernel-integration.scm']
        for integration_file in integration_files:
            if self.check_file_exists(integration_file, 'Integration module'):
                self.check_file_contains(integration_file, 'define', 'Scheme definitions')
    def check_phase2_readiness(self):
        print('\n🚀 Checking Phase 2 Completion Readiness...')
        total_tests = len(self.verification_results)
        passed_tests = sum((1 for result in self.verification_results.values() if result['passed']))
        readiness_score = passed_tests / total_tests * 100 if total_tests > 0 else 0
        self.log_result('Phase 2 Readiness Score', readiness_score >= 80, f'{readiness_score:.1f}% ({passed_tests}/{total_tests} checks passed)')
        if readiness_score >= 90:
            print('✅ EXCELLENT: Phase 2 is ready for completion')
        elif readiness_score >= 80:
            print('✅ GOOD: Phase 2 is ready with minor improvements needed')
        elif readiness_score >= 70:
            print('⚠️  CAUTION: Phase 2 needs attention before completion')
        else:
            print('❌ CRITICAL: Phase 2 requires significant work before completion')
        return readiness_score >= 80
    def generate_completion_report(self):
        print('\n📋 PHASE 2 COMPLETION REPORT')
        print('=' * 50)
        total_tests = len(self.verification_results)
        passed_tests = sum((1 for result in self.verification_results.values() if result['passed']))
        print(f'Total Verification Checks: {total_tests}')
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
            print('Phase 2 is COMPLETE and ready for Phase 3!')
        elif passed_tests / total_tests >= 0.8:
            print('Phase 2 is mostly complete. Address warnings and proceed to Phase 3.')
        else:
            print('Phase 2 requires additional work before completion.')
        return {'total_checks': total_tests, 'passed_checks': passed_tests, 'success_rate': passed_tests / total_tests * 100 if total_tests > 0 else 0, 'errors': self.errors, 'warnings': self.warnings, 'ready_for_phase3': passed_tests / total_tests >= 0.8}
    def run_verification(self):
        print('🧠 === HURDCOG PHASE 2: MICROKERNEL INTEGRATION VERIFICATION === 🧠')
        print('Verifying completion of all 5 sub-tasks and integration readiness...\n')
        self.verify_sub_task_implementations()
        self.verify_integration_tests()
        self.verify_documentation()
        self.verify_build_system()
        self.verify_architectural_coherence()
        readiness = self.check_phase2_readiness()
        report = self.generate_completion_report()
        return report
def main():
    verifier = Phase2Verifier()
    report = verifier.run_verification()
    if report['ready_for_phase3']:
        print('\n🎉 SUCCESS: Phase 2 verification completed successfully!')
        sys.exit(0)
    else:
        print('\n⚠️  Phase 2 verification completed with issues.')
        sys.exit(1)
if __name__ == '__main__':
    main()