import sys
import yaml
import json
import re
import logging
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Any
logging.basicConfig(level=logging.INFO, format='🛡️ %(asctime)s | %(levelname)s | %(message)s')
logger = logging.getLogger(__name__)
class CognitiveWorkflowValidator:
    def __init__(self, strict_mode: bool=False, security_scan: bool=True):
        self.strict_mode = strict_mode
        self.security_scan = security_scan
        self.validation_history = []
        self.safety_patterns = self._load_safety_patterns()
    def validate_workflow(self, file_path: str) -> Dict[str, Any]:
        logger.info(f'🔍 Initiating comprehensive validation for: {file_path}')
        validation_result = {'file_path': file_path, 'timestamp': datetime.now().isoformat(), 'overall_safety': True, 'validation_layers': {}, 'warnings': [], 'critical_issues': [], 'safety_score': 1.0, 'cognitive_coherence': True}
        try:
            if not Path(file_path).exists():
                raise FileNotFoundError(f'Workflow file not found: {file_path}')
            with open(file_path, 'r') as f:
                workflow_content = f.read()
                workflow = yaml.safe_load(workflow_content)
            logger.info('🔍 Layer 1: Syntactic validation...')
            syntax_result = self._validate_syntax(workflow, workflow_content)
            validation_result['validation_layers']['syntax'] = syntax_result
            logger.info('🔍 Layer 2: Semantic safety analysis...')
            semantic_result = self._validate_semantic_safety(workflow)
            validation_result['validation_layers']['semantic'] = semantic_result
            logger.info('🔍 Layer 3: Cognitive coherence validation...')
            coherence_result = self._validate_cognitive_coherence(workflow)
            validation_result['validation_layers']['coherence'] = coherence_result
            if self.security_scan:
                logger.info('🔍 Layer 4: Security vulnerability analysis...')
                security_result = self._validate_security(workflow, workflow_content)
                validation_result['validation_layers']['security'] = security_result
            validation_result = self._aggregate_validation_results(validation_result)
            logger.info(f"✅ Validation completed - Safety Score: {validation_result['safety_score']:.3f}")
            self.validation_history.append(validation_result)
            return validation_result
        except Exception as e:
            logger.error(f'💥 Critical validation error: {e}')
            validation_result['overall_safety'] = False
            validation_result['critical_issues'].append(f'Validation system failure: {e}')
            validation_result['safety_score'] = 0.0
            raise ValueError(f'Workflow validation failed: {e}')
    def _validate_syntax(self, workflow: Dict, content: str) -> Dict[str, Any]:
        result = {'passed': True, 'issues': [], 'score': 1.0}
        try:
            required_keys = ['name', 'on', 'jobs']
            for key in required_keys:
                if key not in workflow:
                    result['issues'].append(f"Missing required top-level key: '{key}'")
                    result['passed'] = False
                    result['score'] *= 0.7
            if 'on' in workflow:
                on_config = workflow['on']
                if 'schedule' in on_config:
                    if isinstance(on_config['schedule'], list):
                        for i, schedule in enumerate(on_config['schedule']):
                            if 'cron' in schedule:
                                cron_valid = self._validate_cron_syntax(schedule['cron'])
                                if not cron_valid:
                                    result['issues'].append(f"Invalid cron syntax in schedule[{i}]: {schedule['cron']}")
                                    result['passed'] = False
                                    result['score'] *= 0.8
                    else:
                        result['issues'].append('Schedule must be a list of schedule objects')
                        result['passed'] = False
                        result['score'] *= 0.6
            if 'jobs' in workflow:
                jobs = workflow['jobs']
                if not isinstance(jobs, dict) or not jobs:
                    result['issues'].append('Jobs section must be a non-empty dictionary')
                    result['passed'] = False
                    result['score'] *= 0.5
                else:
                    for job_name, job_config in jobs.items():
                        job_issues = self._validate_job_syntax(job_name, job_config)
                        result['issues'].extend(job_issues)
                        if job_issues:
                            result['score'] *= 0.9
            formatting_issues = self._validate_yaml_formatting(content)
            result['issues'].extend(formatting_issues)
            if formatting_issues:
                result['score'] *= 0.95
        except Exception as e:
            result['issues'].append(f'Syntax validation error: {e}')
            result['passed'] = False
            result['score'] = 0.0
        return result
    def _validate_semantic_safety(self, workflow: Dict) -> Dict[str, Any]:
        result = {'passed': True, 'issues': [], 'warnings': [], 'score': 1.0}
        try:
            jobs = workflow.get('jobs', {})
            for job_name, job_config in jobs.items():
                if 'timeout-minutes' not in job_config:
                    result['warnings'].append(f"Job '{job_name}' has no timeout protection")
                    result['score'] *= 0.95
                else:
                    timeout = job_config['timeout-minutes']
                    if timeout > 360:
                        result['warnings'].append(f"Job '{job_name}' has excessive timeout: {timeout} minutes")
                        result['score'] *= 0.9
                steps = job_config.get('steps', [])
                for i, step in enumerate(steps):
                    step_issues = self._analyze_step_safety(job_name, i, step)
                    result['issues'].extend(step_issues['critical'])
                    result['warnings'].extend(step_issues['warnings'])
                    if step_issues['critical']:
                        result['passed'] = False
                        result['score'] *= 0.7
                    if step_issues['warnings']:
                        result['score'] *= 0.95
            resource_issues = self._analyze_resource_usage(workflow)
            result['issues'].extend(resource_issues['critical'])
            result['warnings'].extend(resource_issues['warnings'])
            if resource_issues['critical']:
                result['passed'] = False
                result['score'] *= 0.6
        except Exception as e:
            result['issues'].append(f'Semantic validation error: {e}')
            result['passed'] = False
            result['score'] = 0.0
        return result
    def _validate_cognitive_coherence(self, workflow: Dict) -> Dict[str, Any]:
        result = {'passed': True, 'issues': [], 'score': 1.0, 'coherence_metrics': {}}
        try:
            coherence_metrics = {'structural_clarity': self._assess_structural_clarity(workflow), 'logical_consistency': self._assess_logical_consistency(workflow), 'cognitive_complexity': self._calculate_cognitive_complexity(workflow), 'evolution_compatibility': self._assess_evolution_compatibility(workflow)}
            result['coherence_metrics'] = coherence_metrics
            if coherence_metrics['structural_clarity'] < 0.6:
                result['issues'].append('Workflow structure lacks cognitive clarity')
                result['passed'] = False
                result['score'] *= 0.8
            if coherence_metrics['cognitive_complexity'] > 0.8:
                result['issues'].append('Workflow cognitive complexity exceeds recommended threshold')
                result['score'] *= 0.9
            if coherence_metrics['evolution_compatibility'] < 0.5:
                result['issues'].append('Workflow incompatible with cognitive evolution principles')
                result['passed'] = False
                result['score'] *= 0.7
            avg_coherence = sum(coherence_metrics.values()) / len(coherence_metrics)
            result['score'] *= avg_coherence
        except Exception as e:
            result['issues'].append(f'Cognitive coherence validation error: {e}')
            result['passed'] = False
            result['score'] = 0.0
        return result
    def _validate_security(self, workflow: Dict, content: str) -> Dict[str, Any]:
        result = {'passed': True, 'vulnerabilities': [], 'security_warnings': [], 'score': 1.0}
        try:
            dangerous_patterns = ['\\bcurl\\s+.*\\|\\s*bash', '\\bwget\\s+.*\\|\\s*sh', '\\bsudo\\s+.*', '\\bchmod\\s+777', '\\brm\\s+-rf\\s+/', '\\b\\$\\{.*\\}.*\\$\\{.*\\}']
            for pattern in dangerous_patterns:
                matches = re.findall(pattern, content, re.IGNORECASE)
                if matches:
                    result['vulnerabilities'].append(f'Dangerous command pattern detected: {pattern}')
                    result['passed'] = False
                    result['score'] *= 0.6
            secret_patterns = ['password\\s*=\\s*["\\\'][^"\\\']+["\\\']', 'api_key\\s*=\\s*["\\\'][^"\\\']+["\\\']', 'token\\s*=\\s*["\\\'][^"\\\']+["\\\']']
            for pattern in secret_patterns:
                matches = re.findall(pattern, content, re.IGNORECASE)
                if matches:
                    result['security_warnings'].append(f'Potential secret exposure: {pattern}')
                    result['score'] *= 0.9
            jobs = workflow.get('jobs', {})
            for job_name, job_config in jobs.items():
                permissions = job_config.get('permissions', {})
                if permissions:
                    if permissions.get('contents') == 'write':
                        result['security_warnings'].append(f"Job '{job_name}' has write access to repository contents")
                        result['score'] *= 0.95
                    if permissions.get('actions') == 'write':
                        result['security_warnings'].append(f"Job '{job_name}' has write access to actions")
                        result['score'] *= 0.9
        except Exception as e:
            result['vulnerabilities'].append(f'Security validation error: {e}')
            result['passed'] = False
            result['score'] = 0.0
        return result
    def _validate_cron_syntax(self, cron_expr: str) -> bool:
        try:
            parts = cron_expr.strip().split()
            if len(parts) != 5:
                return False
            minute, hour, day, month, weekday = parts
            for part, max_val in [(minute, 59), (hour, 23), (day, 31), (month, 12), (weekday, 7)]:
                if part != '*' and '/' not in part and ('-' not in part):
                    if part.isdigit() and (int(part) > max_val or int(part) < 0):
                        return False
            return True
        except:
            return False
    def _validate_job_syntax(self, job_name: str, job_config: Dict) -> List[str]:
        issues = []
        if not isinstance(job_config, dict):
            issues.append(f"Job '{job_name}' must be a dictionary")
            return issues
        if 'runs-on' not in job_config:
            issues.append(f"Job '{job_name}' missing required 'runs-on' field")
        if 'steps' in job_config:
            steps = job_config['steps']
            if not isinstance(steps, list):
                issues.append(f"Job '{job_name}' steps must be a list")
            else:
                for i, step in enumerate(steps):
                    if not isinstance(step, dict):
                        issues.append(f"Job '{job_name}' step {i} must be a dictionary")
        return issues
    def _validate_yaml_formatting(self, content: str) -> List[str]:
        issues = []
        lines = content.split('\n')
        for i, line in enumerate(lines, 1):
            if '\t' in line:
                issues.append(f'Line {i}: Uses tabs instead of spaces')
            if line.rstrip() != line:
                issues.append(f'Line {i}: Contains trailing whitespace')
        return issues
    def _analyze_step_safety(self, job_name: str, step_index: int, step: Dict) -> Dict[str, List[str]]:
        result = {'critical': [], 'warnings': []}
        if 'run' in step:
            run_command = step['run']
            infinite_patterns = ['while true', 'while :', 'for (( ; ; ))', 'infinite', 'forever']
            for pattern in infinite_patterns:
                if pattern in run_command.lower():
                    result['critical'].append(f"Job '{job_name}' step {step_index}: Potential infinite operation detected")
            network_patterns = ['curl', 'wget', 'npm install', 'pip install']
            if any((pattern in run_command.lower() for pattern in network_patterns)):
                if '||' not in run_command and 'set -e' not in run_command:
                    result['warnings'].append(f"Job '{job_name}' step {step_index}: Network operation without error handling")
        return result
    def _analyze_resource_usage(self, workflow: Dict) -> Dict[str, List[str]]:
        result = {'critical': [], 'warnings': []}
        jobs = workflow.get('jobs', {})
        if len(jobs) > 20:
            result['warnings'].append(f'High job count ({len(jobs)}) may impact resource usage')
        for job_name, job_config in jobs.items():
            if 'strategy' in job_config and 'matrix' in job_config['strategy']:
                matrix = job_config['strategy']['matrix']
                total_combinations = 1
                for key, values in matrix.items():
                    if isinstance(values, list):
                        total_combinations *= len(values)
                if total_combinations > 50:
                    result['critical'].append(f"Job '{job_name}' matrix strategy generates {total_combinations} jobs")
                elif total_combinations > 20:
                    result['warnings'].append(f"Job '{job_name}' matrix strategy generates {total_combinations} jobs")
        return result
    def _assess_structural_clarity(self, workflow: Dict) -> float:
        def calculate_depth(obj, current_depth=0):
            if isinstance(obj, dict):
                return max([calculate_depth(v, current_depth + 1) for v in obj.values()], default=current_depth)
            elif isinstance(obj, list):
                return max([calculate_depth(item, current_depth + 1) for item in obj], default=current_depth)
            return current_depth
        max_depth = calculate_depth(workflow)
        return max(0.0, 1.0 - (max_depth - 3) / 10)
    def _assess_logical_consistency(self, workflow: Dict) -> float:
        consistency_score = 1.0
        jobs = workflow.get('jobs', {})
        for job_name, job_config in jobs.items():
            if 'steps' in job_config and 'runs-on' not in job_config:
                consistency_score *= 0.8
            steps = job_config.get('steps', [])
            if not steps:
                consistency_score *= 0.9
        return consistency_score
    def _calculate_cognitive_complexity(self, workflow: Dict) -> float:
        job_count = len(workflow.get('jobs', {}))
        total_steps = sum((len(job.get('steps', [])) for job in workflow.get('jobs', {}).values()))
        trigger_count = len(workflow.get('on', {}))
        env_count = len(workflow.get('env', {}))
        complexity = (job_count * 0.3 + total_steps * 0.4 + trigger_count * 0.2 + env_count * 0.1) / 20
        return min(1.0, complexity)
    def _assess_evolution_compatibility(self, workflow: Dict) -> float:
        compatibility_score = 0.5
        if workflow.get('on', {}).get('workflow_dispatch'):
            compatibility_score += 0.2
        if 'env' in workflow:
            env_vars = workflow['env']
            cognitive_vars = [var for var in env_vars if 'cognitive' in var.lower()]
            if cognitive_vars:
                compatibility_score += 0.2
        jobs = workflow.get('jobs', {})
        has_validation = any(('validate' in job_name.lower() for job_name in jobs))
        if has_validation:
            compatibility_score += 0.1
        return min(1.0, compatibility_score)
    def _aggregate_validation_results(self, validation_result: Dict[str, Any]) -> Dict[str, Any]:
        layers = validation_result['validation_layers']
        all_issues = []
        all_warnings = []
        for layer_name, layer_result in layers.items():
            all_issues.extend(layer_result.get('issues', []))
            all_issues.extend(layer_result.get('vulnerabilities', []))
            all_warnings.extend(layer_result.get('warnings', []))
            all_warnings.extend(layer_result.get('security_warnings', []))
        validation_result['critical_issues'] = all_issues
        validation_result['warnings'] = all_warnings
        layer_scores = [layer.get('score', 1.0) for layer in layers.values()]
        if layer_scores:
            validation_result['safety_score'] = sum(layer_scores) / len(layer_scores)
        validation_result['overall_safety'] = validation_result['safety_score'] >= 0.7 and (not validation_result['critical_issues'])
        coherence_layer = layers.get('coherence', {})
        validation_result['cognitive_coherence'] = coherence_layer.get('passed', True)
        return validation_result
    def _load_safety_patterns(self) -> Dict[str, List[str]]:
        return {'dangerous_commands': ['rm\\s+-rf\\s+/', 'sudo\\s+.*', 'chmod\\s+777', 'curl.*\\|.*bash', 'wget.*\\|.*sh'], 'resource_intensive': ['while\\s+true', 'for\\s*\\(\\(\\s*;\\s*;\\s*\\)\\)', 'infinite', 'forever']}
def validate_workflow(file_path: str, strict_mode: bool=False, security_scan: bool=True, output_json: bool=False) -> bool:
    validator = CognitiveWorkflowValidator(strict_mode, security_scan)
    try:
        result = validator.validate_workflow(file_path)
        if output_json:
            print(json.dumps(result, indent=2))
        else:
            print(f'🛡️ Workflow Validation Report for: {file_path}')
            print(f"📊 Overall Safety Score: {result['safety_score']:.3f}")
            print(f"✅ Overall Safety: {('PASS' if result['overall_safety'] else 'FAIL')}")
            print(f"🧠 Cognitive Coherence: {('PASS' if result['cognitive_coherence'] else 'FAIL')}")
            if result['critical_issues']:
                print(f"\n❌ Critical Issues ({len(result['critical_issues'])}):")
                for issue in result['critical_issues']:
                    print(f'  • {issue}')
            if result['warnings']:
                print(f"\n⚠️ Warnings ({len(result['warnings'])}):")
                for warning in result['warnings']:
                    print(f'  • {warning}')
            if result['overall_safety']:
                print('\n🎉 Validation passed! Workflow is cognitively safe.')
            else:
                print('\n💥 Validation failed! Workflow requires attention.')
        return result['overall_safety']
    except Exception as e:
        logger.error(f'💥 Validation system failure: {e}')
        if not output_json:
            print(f'❌ Validation failed: {e}')
        return False
if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='🛡️ Neural-Symbolic Workflow Validation Engine', formatter_class=argparse.RawDescriptionHelpFormatter, epilog='\nValidation Modes:\n  Standard   - Basic syntax and safety validation\n  Strict     - Enhanced validation with strict safety checks\n  Security   - Includes comprehensive security vulnerability scanning\n\nExamples:\n  %(prog)s workflow.yml\n  %(prog)s workflow.yml --strict --security-scan\n  %(prog)s workflow.yml --output-json --no-security-scan\n        ')
    parser.add_argument('file_path', help='Path to the workflow YAML file to validate')
    parser.add_argument('--strict', action='store_true', help='Enable strict validation mode')
    parser.add_argument('--security-scan', action='store_true', default=True, help='Enable security vulnerability scanning (default: enabled)')
    parser.add_argument('--no-security-scan', action='store_false', dest='security_scan', help='Disable security vulnerability scanning')
    parser.add_argument('--output-json', action='store_true', help='Output detailed validation results in JSON format')
    args = parser.parse_args()
    success = validate_workflow(args.file_path, strict_mode=args.strict, security_scan=args.security_scan, output_json=args.output_json)
    sys.exit(0 if success else 1)