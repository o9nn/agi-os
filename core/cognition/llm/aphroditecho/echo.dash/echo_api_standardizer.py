import ast
from pathlib import Path
from typing import Dict, List
from dataclasses import dataclass
@dataclass
class ComponentAnalysis:
    file_path: Path
    class_names: List[str]
    has_init: bool
    has_process_method: bool
    has_echo_method: bool
    current_inheritance: List[str]
    complexity_score: int
    recommended_base_class: str
    migration_steps: List[str]
class EchoAPIStandardizer:
    def __init__(self, repo_path: str='.'):
        self.repo_path = Path(repo_path)
        self.analysis_results = {}
    def analyze_component(self, file_path: Path) -> ComponentAnalysis:
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            tree = ast.parse(content)
            class_names = []
            has_init = False
            has_process_method = False
            has_echo_method = False
            current_inheritance = []
            for node in ast.walk(tree):
                if isinstance(node, ast.ClassDef):
                    class_names.append(node.name)
                    if node.bases:
                        for base in node.bases:
                            if isinstance(base, ast.Name):
                                current_inheritance.append(base.id)
                            elif isinstance(base, ast.Attribute):
                                current_inheritance.append(f'{base.value.id}.{base.attr}')
                    for item in node.body:
                        if isinstance(item, ast.FunctionDef):
                            if item.name == '__init__':
                                has_init = True
                            elif 'process' in item.name.lower():
                                has_process_method = True
                            elif 'echo' in item.name.lower():
                                has_echo_method = True
            complexity_score = len(content.splitlines())
            recommended_base_class = self._recommend_base_class(file_path, content, has_process_method, has_echo_method)
            migration_steps = self._generate_migration_steps(file_path, class_names, current_inheritance, recommended_base_class)
            return ComponentAnalysis(file_path=file_path, class_names=class_names, has_init=has_init, has_process_method=has_process_method, has_echo_method=has_echo_method, current_inheritance=current_inheritance, complexity_score=complexity_score, recommended_base_class=recommended_base_class, migration_steps=migration_steps)
        except Exception as e:
            print(f'❌ Error analyzing {file_path}: {e}')
            return None
    def _recommend_base_class(self, file_path: Path, content: str, has_process: bool, has_echo: bool) -> str:
        file_path.name.lower()
        content_lower = content.lower()
        memory_keywords = ['memory', 'store', 'cache', 'retrieve', 'storage']
        has_memory = any((keyword in content_lower for keyword in memory_keywords))
        processing_keywords = ['pipeline', 'transform', 'process', 'filter', 'analyze']
        has_processing = any((keyword in content_lower for keyword in processing_keywords))
        if has_memory and has_processing:
            return 'MemoryEchoComponent'
        elif has_memory:
            return 'MemoryEchoComponent'
        elif has_processing or has_process:
            return 'ProcessingEchoComponent'
        else:
            return 'EchoComponent'
    def _generate_migration_steps(self, file_path: Path, class_names: List[str], current_inheritance: List[str], recommended_base: str) -> List[str]:
        steps = []
        steps.append(f'Add import: from echo_component_base import {recommended_base}, EchoConfig, EchoResponse')
        if class_names:
            main_class = class_names[0]
            if current_inheritance:
                steps.append(f'Change {main_class} inheritance from {current_inheritance} to {recommended_base}')
            else:
                steps.append(f'Add {recommended_base} as base class for {main_class}')
        steps.append('Update __init__ to accept EchoConfig parameter')
        steps.append('Call super().__init__(config) in __init__')
        steps.append('Ensure initialize() method returns EchoResponse')
        steps.append('Ensure process() method accepts input_data and returns EchoResponse')
        steps.append('Ensure echo() method accepts data, echo_value and returns EchoResponse')
        steps.append('Replace custom error handling with self.handle_error()')
        steps.append('Use self.validate_input() for input validation')
        steps.append('Replace custom logging with self.logger')
        return steps
    def scan_echo_components(self) -> Dict[str, ComponentAnalysis]:
        print('🔍 Scanning for Echo components...')
        echo_patterns = ['*echo*.py', '*Echo*.py']
        components = {}
        for pattern in echo_patterns:
            for file_path in self.repo_path.glob(pattern):
                if file_path.is_file() and (not file_path.name.startswith('test_')) and (file_path.name not in ['echo_component_base.py', 'echo_api_standardizer.py']):
                    print(f'  📄 Analyzing: {file_path.name}')
                    analysis = self.analyze_component(file_path)
                    if analysis:
                        components[str(file_path.relative_to(self.repo_path))] = analysis
        self.analysis_results = components
        return components
    def generate_migration_report(self) -> str:
        if not self.analysis_results:
            self.scan_echo_components()
        report = []
        report.append('# Echo API Standardization Report')
        report.append('=' * 50)
        report.append('')
        total_components = len(self.analysis_results)
        needs_migration = sum((1 for a in self.analysis_results.values() if not any((base in a.current_inheritance for base in ['EchoComponent', 'MemoryEchoComponent', 'ProcessingEchoComponent']))))
        report.append('## Summary')
        report.append(f'- Total Echo components found: {total_components}')
        report.append(f'- Components needing migration: {needs_migration}')
        report.append(f'- Components already standardized: {total_components - needs_migration}')
        report.append('')
        base_class_counts = {}
        for analysis in self.analysis_results.values():
            base_class = analysis.recommended_base_class
            base_class_counts[base_class] = base_class_counts.get(base_class, 0) + 1
        report.append('## Recommended Base Classes')
        for base_class, count in base_class_counts.items():
            report.append(f'- {base_class}: {count} components')
        report.append('')
        report.append('## Component Analysis')
        report.append('')
        sorted_components = sorted(self.analysis_results.items(), key=lambda x: x[1].complexity_score)
        for file_path, analysis in sorted_components:
            report.append(f'### {file_path}')
            report.append(f"- **Classes**: {(', '.join(analysis.class_names) if analysis.class_names else 'None')}")
            report.append(f"- **Current inheritance**: {(', '.join(analysis.current_inheritance) if analysis.current_inheritance else 'None')}")
            report.append(f'- **Recommended base**: {analysis.recommended_base_class}')
            report.append(f'- **Complexity**: {analysis.complexity_score} lines')
            report.append(f"- **Has echo method**: {('✅' if analysis.has_echo_method else '❌')}")
            report.append(f"- **Has process method**: {('✅' if analysis.has_process_method else '❌')}")
            report.append('')
            report.append('**Migration Steps:**')
            for i, step in enumerate(analysis.migration_steps, 1):
                report.append(f'{i}. {step}')
            report.append('')
        return '\n'.join(report)
    def generate_simple_migration_for_component(self, file_path: str) -> str:
        if file_path not in self.analysis_results:
            return f'Component {file_path} not found in analysis results'
        analysis = self.analysis_results[file_path]
        template = f'''# Migration Template for {file_path}\n\n## Before (Current Code):\n```python\n# Existing class structure\nclass {(analysis.class_names[0] if analysis.class_names else 'ExistingClass')}:\n    def __init__(self, ...):\n        # Current initialization\n        pass\n    \n    def some_method(self, data):\n        # Current processing\n        return result\n```\n\n## After (Standardized Code):\n```python\nfrom echo_component_base import {analysis.recommended_base_class}, EchoConfig, EchoResponse\n\nclass {(analysis.class_names[0] if analysis.class_names else 'ExistingClass')}({analysis.recommended_base_class}):\n    def __init__(self, config: EchoConfig):\n        super().__init__(config)\n        # Your specific initialization here\n        \n    def initialize(self) -> EchoResponse:\n        try:\n            self._initialized = True\n            # Component-specific initialization\n            return EchoResponse(success=True, message="Component initialized")\n        except Exception as e:\n            return self.handle_error(e, "initialize")\n    \n    def process(self, input_data: Any, **kwargs) -> EchoResponse:\n        try:\n            validation = self.validate_input(input_data)\n            if not validation.success:\n                return validation\n            \n            # Your processing logic here\n            result = self.some_method(input_data)\n            \n            return EchoResponse(\n                success=True,\n                data=result,\n                message="Processing completed"\n            )\n        except Exception as e:\n            return self.handle_error(e, "process")\n    \n    def echo(self, data: Any, echo_value: float = 0.0) -> EchoResponse:\n        try:\n            # Your echo logic here\n            echoed_data = {{\n                'original_data': data,\n                'echo_value': echo_value,\n                'timestamp': datetime.now().isoformat()\n            }}\n            \n            return EchoResponse(\n                success=True,\n                data=echoed_data,\n                message=f"Echo operation completed (value: {{echo_value}})"\n            )\n        except Exception as e:\n            return self.handle_error(e, "echo")\n    \n    def some_method(self, data):\n        # Migrate your existing logic here\n        # Use self.logger instead of custom logging\n        # Use self.handle_error() for error handling\n        return processed_data\n```\n\n## Usage Example:\n```python\nfrom echo_component_base import EchoConfig\n\n# Create configuration\nconfig = EchoConfig(\n    component_name="{(analysis.class_names[0] if analysis.class_names else 'component')}",\n    version="1.0.0",\n    echo_threshold=0.75\n)\n\n# Create component\ncomponent = {(analysis.class_names[0] if analysis.class_names else 'Component')}(config)\n\n# Initialize\ninit_result = component.initialize()\nif init_result.success:\n    # Process data\n    result = component.process(your_data)\n    \n    # Echo operation\n    echo_result = component.echo(result.data, echo_value=0.8)\n```\n'''
        return template
    def print_migration_summary(self):
        if not self.analysis_results:
            self.scan_echo_components()
        print('\n' + '=' * 60)
        print('📋 ECHO API STANDARDIZATION SUMMARY')
        print('=' * 60)
        simple_migrations = [f for f, a in self.analysis_results.items() if a.complexity_score < 300]
        medium_migrations = [f for f, a in self.analysis_results.items() if 300 <= a.complexity_score < 600]
        complex_migrations = [f for f, a in self.analysis_results.items() if a.complexity_score >= 600]
        print(f'\n🟢 Simple Migrations (< 300 lines): {len(simple_migrations)}')
        for file_path in simple_migrations:
            analysis = self.analysis_results[file_path]
            print(f'   - {file_path} → {analysis.recommended_base_class}')
        print(f'\n🟡 Medium Migrations (300-600 lines): {len(medium_migrations)}')
        for file_path in medium_migrations:
            analysis = self.analysis_results[file_path]
            print(f'   - {file_path} → {analysis.recommended_base_class}')
        print(f'\n🔴 Complex Migrations (> 600 lines): {len(complex_migrations)}')
        for file_path in complex_migrations:
            analysis = self.analysis_results[file_path]
            print(f'   - {file_path} → {analysis.recommended_base_class}')
        print('\n💡 Recommendation: Start with simple migrations first!')
        print('=' * 60)
def main():
    standardizer = EchoAPIStandardizer()
    components = standardizer.scan_echo_components()
    if not components:
        print('❌ No Echo components found!')
        return
    standardizer.print_migration_summary()
    report = standardizer.generate_migration_report()
    report_file = Path('echo_api_migration_report.md')
    with open(report_file, 'w') as f:
        f.write(report)
    print(f'\n📊 Full migration report saved to: {report_file}')
    if standardizer.analysis_results:
        simple_component = min(standardizer.analysis_results.items(), key=lambda x: x[1].complexity_score)
        example_file = Path('example_migration.md')
        example = standardizer.generate_simple_migration_for_component(simple_component[0])
        with open(example_file, 'w') as f:
            f.write(example)
        print(f'📝 Example migration saved to: {example_file}')
    print('\n✅ Analysis complete! Review the reports and start with simple migrations.')
if __name__ == '__main__':
    main()