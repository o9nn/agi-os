import os
import json
import logging
from typing import Dict, List, Any, Optional
from datetime import datetime
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)
class NanoCogEvaluator:
    def __init__(self, config: Optional[Dict[str, Any]]=None):
        self.config = config or self._get_default_config()
        self.evaluation_history = []
    def _get_default_config(self) -> Dict[str, Any]:
        return {'symbolic_accuracy': {'target_syntax_accuracy': 0.95, 'target_semantic_accuracy': 0.8, 'min_constructs_threshold': 5}, 'diagnostic_alignment': {'target_accuracy': 0.85, 'tolerance_percentage': 0.2}, 'emergent_patterns': {'target_novelty_rate': 0.1, 'min_pattern_count': 10}, 'cross_domain_integration': {'target_coverage': 0.7, 'min_active_domains': 3}}
    def evaluate_model_generation(self, generated_samples: List[str], reference_corpus: List[str]=None, atomspace_state: Dict[str, Any]=None) -> Dict[str, Any]:
        logger.info(f'Starting evaluation of {len(generated_samples)} generated samples')
        results = {'timestamp': datetime.now().isoformat(), 'sample_count': len(generated_samples), 'evaluation_metrics': {}}
        try:
            from introspection.atomspace_client import AtomSpaceClient
            client = AtomSpaceClient('mock://localhost')
        except ImportError:
            logger.warning('AtomSpace client not available, using simplified evaluation')
            client = None
        logger.info('Evaluating symbolic accuracy...')
        symbolic_results = self._evaluate_symbolic_accuracy_batch(generated_samples)
        results['evaluation_metrics']['symbolic_accuracy'] = symbolic_results
        if reference_corpus:
            logger.info('Detecting emergent patterns...')
            pattern_results = self._detect_emergent_patterns_batch(generated_samples, reference_corpus)
            results['evaluation_metrics']['emergent_patterns'] = pattern_results
        logger.info('Analyzing cross-domain integration...')
        integration_results = self._analyze_cross_domain_integration_batch(generated_samples)
        results['evaluation_metrics']['cross_domain_integration'] = integration_results
        if atomspace_state and client:
            logger.info('Evaluating diagnostic alignment...')
            diagnostic_results = self._evaluate_diagnostic_alignment_batch(generated_samples, atomspace_state, client)
            results['evaluation_metrics']['diagnostic_alignment'] = diagnostic_results
        results['overall_performance'] = self._calculate_overall_score(results['evaluation_metrics'])
        self.evaluation_history.append(results)
        logger.info('Evaluation complete')
        return results
    def _evaluate_symbolic_accuracy_batch(self, samples: List[str]) -> Dict[str, Any]:
        try:
            from introspection.atomspace_client import AtomSpaceClient
            client = AtomSpaceClient('mock://localhost')
            total_constructs = 0
            total_syntax_correct = 0
            total_semantic_correct = 0
            sample_results = []
            for i, sample in enumerate(samples):
                result = client.evaluate_symbolic_accuracy(sample)
                sample_results.append(result)
                total_constructs += result['total_constructs']
                total_syntax_correct += result['syntax_correct']
                total_semantic_correct += result['semantic_coherent']
            syntax_accuracy = total_syntax_correct / total_constructs if total_constructs > 0 else 0.0
            semantic_accuracy = total_semantic_correct / total_constructs if total_constructs > 0 else 0.0
            syntax_target = self.config['symbolic_accuracy']['target_syntax_accuracy']
            semantic_target = self.config['symbolic_accuracy']['target_semantic_accuracy']
            return {'total_constructs': total_constructs, 'syntax_accuracy': syntax_accuracy, 'semantic_accuracy': semantic_accuracy, 'syntax_target_met': syntax_accuracy >= syntax_target, 'semantic_target_met': semantic_accuracy >= semantic_target, 'sample_count': len(samples), 'detailed_results': sample_results[:5]}
        except Exception as e:
            logger.error(f'Error in symbolic accuracy evaluation: {e}')
            return {'error': str(e), 'syntax_accuracy': 0.0, 'semantic_accuracy': 0.0}
    def _detect_emergent_patterns_batch(self, generated_samples: List[str], reference_corpus: List[str]) -> Dict[str, Any]:
        try:
            from introspection.atomspace_client import AtomSpaceClient
            client = AtomSpaceClient('mock://localhost')
            result = client.detect_emergent_patterns(generated_samples, reference_corpus)
            novelty_target = self.config['emergent_patterns']['target_novelty_rate']
            min_patterns = self.config['emergent_patterns']['min_pattern_count']
            result['novelty_target_met'] = result['novelty_rate'] >= novelty_target
            result['pattern_threshold_met'] = result['novel_pattern_count'] >= min_patterns
            return result
        except Exception as e:
            logger.error(f'Error in emergent pattern detection: {e}')
            return {'error': str(e), 'novelty_rate': 0.0, 'novel_pattern_count': 0}
    def _analyze_cross_domain_integration_batch(self, samples: List[str]) -> Dict[str, Any]:
        try:
            from introspection.atomspace_client import AtomSpaceClient
            client = AtomSpaceClient('mock://localhost')
            combined_text = '\n'.join(samples)
            result = client.analyze_cross_domain_integration(combined_text)
            coverage_target = self.config['cross_domain_integration']['target_coverage']
            min_domains = self.config['cross_domain_integration']['min_active_domains']
            result['coverage_target_met'] = result['domain_coverage'] >= coverage_target
            result['domain_threshold_met'] = result['active_domains'] >= min_domains
            result['sample_count'] = len(samples)
            return result
        except Exception as e:
            logger.error(f'Error in cross-domain integration analysis: {e}')
            return {'error': str(e), 'domain_coverage': 0.0, 'active_domains': 0}
    def _evaluate_diagnostic_alignment_batch(self, samples: List[str], atomspace_state: Dict[str, Any], client) -> Dict[str, Any]:
        try:
            diagnostic_predictions = self._extract_diagnostic_predictions(samples)
            if not diagnostic_predictions:
                return {'error': 'No diagnostic predictions found in samples'}
            result = client.evaluate_diagnostic_alignment(diagnostic_predictions, atomspace_state)
            accuracy_target = self.config['diagnostic_alignment']['target_accuracy']
            result['accuracy_target_met'] = result['overall_accuracy'] >= accuracy_target
            return result
        except Exception as e:
            logger.error(f'Error in diagnostic alignment evaluation: {e}')
            return {'error': str(e), 'overall_accuracy': 0.0}
    def _extract_diagnostic_predictions(self, samples: List[str]) -> Dict[str, Any]:
        import re
        predictions = {'predicted_bottlenecks': [], 'attention_analysis': {}}
        bottleneck_patterns = ['attention[_-]?bottleneck', 'goal[_-]?proliferation', 'low[_-]?schematic[_-]?success', 'atom[_-]?type[_-]?imbalance']
        for sample in samples:
            sample_lower = sample.lower()
            for pattern in bottleneck_patterns:
                if re.search(pattern, sample_lower):
                    bottleneck_type = pattern.replace('[_-]?', '_')
                    predictions['predicted_bottlenecks'].append({'type': bottleneck_type})
        sti_mentions = re.findall('sti.*?(\\d+)', ' '.join(samples), re.IGNORECASE)
        if sti_mentions:
            try:
                avg_sti = sum((int(mention) for mention in sti_mentions[:10])) / len(sti_mentions[:10])
                predictions['attention_analysis']['high_sti_prediction'] = int(avg_sti)
            except:
                pass
        return predictions
    def _calculate_overall_score(self, metrics: Dict[str, Any]) -> Dict[str, Any]:
        scores = []
        weights = {'symbolic_accuracy': 0.3, 'emergent_patterns': 0.25, 'cross_domain_integration': 0.25, 'diagnostic_alignment': 0.2}
        total_weight = 0
        weighted_score = 0
        if 'symbolic_accuracy' in metrics and 'syntax_accuracy' in metrics['symbolic_accuracy']:
            syntax_score = metrics['symbolic_accuracy']['syntax_accuracy']
            semantic_score = metrics['symbolic_accuracy'].get('semantic_accuracy', 0)
            symbolic_score = (syntax_score + semantic_score) / 2
            weighted_score += symbolic_score * weights['symbolic_accuracy']
            total_weight += weights['symbolic_accuracy']
        if 'emergent_patterns' in metrics and 'novelty_rate' in metrics['emergent_patterns']:
            novelty_score = min(1.0, metrics['emergent_patterns']['novelty_rate'] * 5)
            weighted_score += novelty_score * weights['emergent_patterns']
            total_weight += weights['emergent_patterns']
        if 'cross_domain_integration' in metrics and 'domain_coverage' in metrics['cross_domain_integration']:
            integration_score = metrics['cross_domain_integration']['domain_coverage']
            weighted_score += integration_score * weights['cross_domain_integration']
            total_weight += weights['cross_domain_integration']
        if 'diagnostic_alignment' in metrics and 'overall_accuracy' in metrics['diagnostic_alignment']:
            diagnostic_score = metrics['diagnostic_alignment']['overall_accuracy']
            weighted_score += diagnostic_score * weights['diagnostic_alignment']
            total_weight += weights['diagnostic_alignment']
        overall_score = weighted_score / total_weight if total_weight > 0 else 0.0
        if overall_score >= 0.8:
            performance_level = 'excellent'
        elif overall_score >= 0.7:
            performance_level = 'good'
        elif overall_score >= 0.6:
            performance_level = 'acceptable'
        else:
            performance_level = 'needs_improvement'
        return {'overall_score': overall_score, 'performance_level': performance_level, 'component_weights': weights, 'weighted_contributions': {metric: score * weight for metric, (score, weight) in [('symbolic_accuracy', (symbolic_score if 'symbolic_score' in locals() else 0, weights['symbolic_accuracy'])), ('emergent_patterns', (novelty_score if 'novelty_score' in locals() else 0, weights['emergent_patterns'])), ('cross_domain_integration', (integration_score if 'integration_score' in locals() else 0, weights['cross_domain_integration'])), ('diagnostic_alignment', (diagnostic_score if 'diagnostic_score' in locals() else 0, weights['diagnostic_alignment']))]}}
    def generate_evaluation_report(self, results: Dict[str, Any], output_path: Optional[str]=None) -> str:
        report_lines = []
        report_lines.append('=' * 80)
        report_lines.append('NANOCOG EVALUATION REPORT')
        report_lines.append('=' * 80)
        report_lines.append(f"Timestamp: {results['timestamp']}")
        report_lines.append(f"Samples Evaluated: {results['sample_count']}")
        report_lines.append('')
        if 'overall_performance' in results:
            perf = results['overall_performance']
            report_lines.append('OVERALL PERFORMANCE')
            report_lines.append('-' * 20)
            report_lines.append(f"Score: {perf['overall_score']:.3f}")
            report_lines.append(f"Level: {perf['performance_level'].upper()}")
            report_lines.append('')
        metrics = results.get('evaluation_metrics', {})
        if 'symbolic_accuracy' in metrics:
            sa = metrics['symbolic_accuracy']
            report_lines.append('SYMBOLIC ACCURACY')
            report_lines.append('-' * 18)
            report_lines.append(f"Syntax Accuracy: {sa.get('syntax_accuracy', 0):.3f}")
            report_lines.append(f"Semantic Accuracy: {sa.get('semantic_accuracy', 0):.3f}")
            report_lines.append(f"Total Constructs: {sa.get('total_constructs', 0)}")
            report_lines.append(f"Syntax Target Met: {('✓' if sa.get('syntax_target_met', False) else '✗')}")
            report_lines.append(f"Semantic Target Met: {('✓' if sa.get('semantic_target_met', False) else '✗')}")
            report_lines.append('')
        if 'emergent_patterns' in metrics:
            ep = metrics['emergent_patterns']
            report_lines.append('EMERGENT PATTERNS')
            report_lines.append('-' * 17)
            report_lines.append(f"Novelty Rate: {ep.get('novelty_rate', 0):.3f}")
            report_lines.append(f"Novel Patterns: {ep.get('novel_pattern_count', 0)}")
            report_lines.append(f"Pattern Diversity: {ep.get('pattern_diversity', 0)}")
            report_lines.append(f"Novelty Target Met: {('✓' if ep.get('novelty_target_met', False) else '✗')}")
            report_lines.append('')
        if 'cross_domain_integration' in metrics:
            cdi = metrics['cross_domain_integration']
            report_lines.append('CROSS-DOMAIN INTEGRATION')
            report_lines.append('-' * 25)
            report_lines.append(f"Domain Coverage: {cdi.get('domain_coverage', 0):.3f}")
            report_lines.append(f"Active Domains: {cdi.get('active_domains', 0)}")
            report_lines.append(f"Integration Score: {cdi.get('integration_score', 0):.3f}")
            report_lines.append(f"Coverage Target Met: {('✓' if cdi.get('coverage_target_met', False) else '✗')}")
            report_lines.append('')
        if 'diagnostic_alignment' in metrics:
            da = metrics['diagnostic_alignment']
            report_lines.append('DIAGNOSTIC ALIGNMENT')
            report_lines.append('-' * 20)
            report_lines.append(f"Overall Accuracy: {da.get('overall_accuracy', 0):.3f}")
            report_lines.append(f"Total Comparisons: {da.get('total_comparisons', 0)}")
            report_lines.append(f"Accuracy Target Met: {('✓' if da.get('accuracy_target_met', False) else '✗')}")
            report_lines.append('')
        report_lines.append('=' * 80)
        report_text = '\n'.join(report_lines)
        if output_path:
            try:
                with open(output_path, 'w', encoding='utf-8') as f:
                    f.write(report_text)
                logger.info(f'Evaluation report saved to {output_path}')
            except Exception as e:
                logger.error(f'Failed to save report to {output_path}: {e}')
        return report_text
    def save_evaluation_results(self, results: Dict[str, Any], output_dir: str):
        os.makedirs(output_dir, exist_ok=True)
        timestamp = results['timestamp'].replace(':', '-').replace('.', '-')
        json_path = os.path.join(output_dir, f'evaluation_results_{timestamp}.json')
        try:
            with open(json_path, 'w', encoding='utf-8') as f:
                json.dump(results, f, indent=2, ensure_ascii=False)
            logger.info(f'Results saved to {json_path}')
        except Exception as e:
            logger.error(f'Failed to save JSON results: {e}')
        report_path = os.path.join(output_dir, f'evaluation_report_{timestamp}.txt')
        self.generate_evaluation_report(results, report_path)
def main():
    import argparse
    parser = argparse.ArgumentParser(description='NanoCog Model Evaluation')
    parser.add_argument('--samples', type=str, required=True, help='Path to file containing generated samples (one per line)')
    parser.add_argument('--reference', type=str, help='Path to reference corpus file')
    parser.add_argument('--output', type=str, default='evaluation_results', help='Output directory for results')
    parser.add_argument('--config', type=str, help='Path to evaluation configuration JSON file')
    args = parser.parse_args()
    config = None
    if args.config and os.path.exists(args.config):
        with open(args.config, 'r') as f:
            config = json.load(f)
    if not os.path.exists(args.samples):
        print(f"Error: Samples file '{args.samples}' not found")
        return
    with open(args.samples, 'r', encoding='utf-8') as f:
        samples = [line.strip() for line in f if line.strip()]
    reference_corpus = None
    if args.reference and os.path.exists(args.reference):
        with open(args.reference, 'r', encoding='utf-8') as f:
            reference_corpus = [line.strip() for line in f if line.strip()]
    evaluator = NanoCogEvaluator(config)
    results = evaluator.evaluate_model_generation(samples, reference_corpus)
    evaluator.save_evaluation_results(results, args.output)
    report = evaluator.generate_evaluation_report(results)
    print(report)
if __name__ == '__main__':
    main()