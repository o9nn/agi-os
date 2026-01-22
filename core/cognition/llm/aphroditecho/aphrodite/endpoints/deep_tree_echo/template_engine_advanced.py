import hashlib
import json
import logging
import time
from typing import Any, Dict, List, Optional
from dataclasses import dataclass
from pathlib import Path
from collections import OrderedDict
from jinja2 import Environment, FileSystemLoader, Template, TemplateSyntaxError
from fastapi import Request
logger = logging.getLogger(__name__)
@dataclass
class DTESNTemplateContext:
    dtesn_result_type: str
    data_complexity: int
    result_size: int
    processing_time_ms: float
    membrane_layers: int
    has_errors: bool = False
    has_warnings: bool = False
    requires_visualization: bool = False
    client_type: str = 'browser'
@dataclass
class TemplateCache:
    template_key: str
    compiled_template: Template
    rendered_cache: OrderedDict[str, str]
    access_count: int = 0
    last_access: float = 0.0
    creation_time: float = 0.0
    total_render_time: float = 0.0
class DTESNTemplateDynamicGenerator:
    def __init__(self, templates_dir: Path):
        self.templates_dir = templates_dir
        self.base_templates = {}
        self.dynamic_templates = {}
        self.template_cache: Dict[str, TemplateCache] = {}
        self.cache_hits = 0
        self.cache_misses = 0
        self.max_cache_entries = 50
        self.max_rendered_cache_per_template = 20
        self._load_base_templates()
        self._init_template_patterns()
    def _load_base_templates(self):
        base_template_configs = {'dtesn_result_simple': {'extends': 'base.html', 'blocks': ['content'], 'data_display': 'table', 'visualization': False}, 'dtesn_result_medium': {'extends': 'base.html', 'blocks': ['content', 'styles'], 'data_display': 'cards_and_table', 'visualization': 'basic_charts'}, 'dtesn_result_complex': {'extends': 'base.html', 'blocks': ['content', 'styles', 'nav_links'], 'data_display': 'hierarchical_cards', 'visualization': 'interactive_charts'}}
        self.base_templates = base_template_configs
        logger.info(f'Loaded {len(base_template_configs)} base template configurations')
    def _init_template_patterns(self):
        self.template_patterns = {'membrane_evolution': {'title_template': 'Membrane Evolution - Layer {{ membrane_layers }}', 'content_structure': 'hierarchical_list', 'data_sections': ['evolution_steps', 'layer_changes', 'performance_metrics'], 'responsive_breakpoints': {'mobile': 768, 'tablet': 1024}}, 'esn_processing': {'title_template': 'ESN Processing - Reservoir Size {{ reservoir_size }}', 'content_structure': 'matrix_display', 'data_sections': ['reservoir_state', 'activation_patterns', 'output_weights'], 'responsive_breakpoints': {'mobile': 768, 'tablet': 1024}}, 'bseries_computation': {'title_template': 'B-Series Computation - Order {{ computation_order }}', 'content_structure': 'tree_visualization', 'data_sections': ['tree_structure', 'coefficients', 'computation_stats'], 'responsive_breakpoints': {'mobile': 768, 'tablet': 1024}}, 'batch_processing': {'title_template': 'Batch Processing Results - {{ batch_size }} Items', 'content_structure': 'batch_summary_cards', 'data_sections': ['batch_overview', 'individual_results', 'performance_summary'], 'responsive_breakpoints': {'mobile': 768, 'tablet': 1024}}}
    async def generate_template(self, context: DTESNTemplateContext, dtesn_result: Dict[str, Any]) -> str:
        cache_key = self._create_template_cache_key(context, dtesn_result)
        if cache_key in self.template_cache:
            self.cache_hits += 1
            cached = self.template_cache[cache_key]
            cached.access_count += 1
            cached.last_access = time.time()
            return cache_key
        self.cache_misses += 1
        template_content = await self._generate_template_content(context, dtesn_result)
        try:
            env = Environment(loader=FileSystemLoader(str(self.templates_dir)))
            compiled_template = env.from_string(template_content)
            template_cache = TemplateCache(template_key=cache_key, compiled_template=compiled_template, rendered_cache=OrderedDict(), creation_time=time.time())
            self._add_to_cache(cache_key, template_cache)
            return cache_key
        except TemplateSyntaxError as e:
            logger.error(f'Template syntax error in generated template: {e}')
            return await self._get_fallback_template(context)
    async def _generate_template_content(self, context: DTESNTemplateContext, dtesn_result: Dict[str, Any]) -> str:
        result_analysis = self._analyze_dtesn_result(dtesn_result)
        base_config = self._select_base_template_config(context)
        pattern = self.template_patterns.get(context.dtesn_result_type, self.template_patterns['membrane_evolution'])
        template_parts = []
        template_parts.append('{% extends "base.html" %}')
        template_parts.append('')
        title_template = pattern['title_template']
        template_parts.append('{% block title %}')
        template_parts.append(f'Deep Tree Echo - {title_template}')
        template_parts.append('{% endblock %}')
        template_parts.append('')
        if context.data_complexity >= 2:
            template_parts.extend(self._generate_responsive_styles(context, pattern))
        template_parts.extend(self._generate_content_block(context, result_analysis, pattern))
        return '\n'.join(template_parts)
    def _analyze_dtesn_result(self, dtesn_result: Dict[str, Any]) -> Dict[str, Any]:
        analysis = {'data_types': set(), 'nested_levels': 0, 'list_fields': [], 'numeric_fields': [], 'text_fields': [], 'has_timestamps': False, 'has_performance_data': False, 'estimated_render_complexity': 1}
        def analyze_value(value, level=0):
            analysis['nested_levels'] = max(analysis['nested_levels'], level)
            if isinstance(value, dict):
                analysis['data_types'].add('dict')
                for k, v in value.items():
                    if k in ['processing_time_ms', 'timestamp', 'performance']:
                        analysis['has_performance_data'] = True
                    if 'time' in k.lower():
                        analysis['has_timestamps'] = True
                    analyze_value(v, level + 1)
            elif isinstance(value, list):
                analysis['data_types'].add('list')
                analysis['list_fields'].append(level)
                if value:
                    analyze_value(value[0], level + 1)
            elif isinstance(value, (int, float)):
                analysis['data_types'].add('numeric')
                analysis['numeric_fields'].append(level)
            elif isinstance(value, str):
                analysis['data_types'].add('text')
                analysis['text_fields'].append(level)
        analyze_value(dtesn_result)
        complexity_score = analysis['nested_levels'] * 2 + len(analysis['list_fields']) * 1.5 + len(analysis['data_types']) * 0.5
        analysis['estimated_render_complexity'] = min(int(complexity_score), 3)
        return analysis
    def _select_base_template_config(self, context: DTESNTemplateContext) -> Dict[str, Any]:
        if context.data_complexity == 1:
            return self.base_templates['dtesn_result_simple']
        elif context.data_complexity == 2:
            return self.base_templates['dtesn_result_medium']
        else:
            return self.base_templates['dtesn_result_complex']
    def _generate_responsive_styles(self, context: DTESNTemplateContext, pattern: Dict[str, Any]) -> List[str]:
        styles = ['{% block styles %}', '<style>']
        styles.extend(['/* Dynamic responsive styles for DTESN results */', '.dtesn-result-container {', '    display: grid;', '    gap: 1rem;', '    grid-template-columns: 1fr;', '}', ''])
        breakpoints = pattern.get('responsive_breakpoints', {'mobile': 768, 'tablet': 1024})
        styles.extend([f"@media (min-width: {breakpoints['mobile']}px) {{", '    .dtesn-result-container {', '        grid-template-columns: 1fr 1fr;', '    }', '    .dtesn-data-card {', '        padding: 1.5rem;', '    }', '}', ''])
        styles.extend([f"@media (min-width: {breakpoints['tablet']}px) {{", '    .dtesn-result-container {', '        grid-template-columns: repeat(3, 1fr);', '    }', '    .dtesn-performance-section {', '        grid-column: span 3;', '    }', '}', ''])
        if context.data_complexity >= 3:
            styles.extend(['/* Complex data visualization styles */', '.dtesn-hierarchical-display {', '    max-height: 400px;', '    overflow-y: auto;', '    border: 1px solid var(--border-color);', '    border-radius: 4px;', '}', ''])
        styles.extend(['</style>', '{% endblock %}', ''])
        return styles
    def _generate_content_block(self, context: DTESNTemplateContext, analysis: Dict[str, Any], pattern: Dict[str, Any]) -> List[str]:
        content = ['{% block content %}']
        content.extend(['<div class="dtesn-result-container">', '    <!-- Dynamic DTESN Result Display -->', ''])
        title_template = pattern['title_template']
        content.extend(['    <div class="card dtesn-header-card">', f'        <h2>{title_template}</h2>', '        <div class="dtesn-metadata">', '            <span class="status-badge status-success">{{ data.status }}</span>', '            <span class="processing-time">Processing: {{ "%.2f"|format(data.processing_time_ms) }}ms</span>', '        </div>', '    </div>', ''])
        data_sections = pattern.get('data_sections', ['main_data'])
        for section in data_sections:
            content.extend(self._generate_section_content(section, context, analysis))
        if analysis['has_performance_data']:
            content.extend(['    <div class="card dtesn-performance-section">', '        <h3>Performance Metrics</h3>', '        {% if data.performance_metrics %}', '        <div class="performance-grid">', '            {% for key, value in data.performance_metrics.items() %}', '            <div class="metric-item">', '                <span class="metric-label">{{ key|replace("_", " ")|title }}</span>', '                <span class="metric-value">{{ value }}</span>', '            </div>', '            {% endfor %}', '        </div>', '        {% endif %}', '    </div>', ''])
        content.extend(['</div>', '{% endblock %}'])
        return content
    def _generate_section_content(self, section_name: str, context: DTESNTemplateContext, analysis: Dict[str, Any]) -> List[str]:
        section_templates = {'evolution_steps': self._generate_evolution_steps_section, 'reservoir_state': self._generate_reservoir_section, 'tree_structure': self._generate_tree_section, 'batch_overview': self._generate_batch_overview_section, 'main_data': self._generate_main_data_section}
        generator_func = section_templates.get(section_name, self._generate_main_data_section)
        return generator_func(context, analysis)
    def _generate_evolution_steps_section(self, context: DTESNTemplateContext, analysis: Dict[str, Any]) -> List[str]:
        return ['    <div class="card dtesn-data-card">', '        <h3>Membrane Evolution</h3>', '        {% if data.result and data.result.processed_output %}', '        <div class="evolution-display">', '            {% if data.result.processed_output is mapping %}', '                {% for key, value in data.result.processed_output.items() %}', '                <div class="evolution-step">', '                    <strong>{{ key|replace("_", " ")|title }}:</strong>', '                    <span>{{ value }}</span>', '                </div>', '                {% endfor %}', '            {% else %}', '                <div class="evolution-content">{{ data.result.processed_output }}</div>', '            {% endif %}', '        </div>', '        {% endif %}', '    </div>', '']
    def _generate_reservoir_section(self, context: DTESNTemplateContext, analysis: Dict[str, Any]) -> List[str]:
        return ['    <div class="card dtesn-data-card">', '        <h3>ESN Reservoir State</h3>', '        {% if data.result and data.result.esn_state %}', '        <div class="reservoir-display">', '            <div class="reservoir-info">', '                <span><strong>Size:</strong> {{ data.result.esn_state.reservoir_size }}</span>', '                <span><strong>Activation:</strong> {{ data.result.esn_state.activation }}</span>', '                {% if data.result.esn_state.spectral_radius %}', '                <span><strong>Spectral Radius:</strong> {{ "%.3f"|format(data.result.esn_state.spectral_radius) }}</span>', '                {% endif %}', '            </div>', '        </div>', '        {% endif %}', '    </div>', '']
    def _generate_tree_section(self, context: DTESNTemplateContext, analysis: Dict[str, Any]) -> List[str]:
        return ['    <div class="card dtesn-data-card">', '        <h3>B-Series Tree Structure</h3>', '        {% if data.result and data.result.bseries_computation %}', '        <div class="tree-display">', '            <div class="tree-info">', '                <span><strong>Order:</strong> {{ data.result.bseries_computation.order }}</span>', '                <span><strong>Structure:</strong> {{ data.result.bseries_computation.tree_structure }}</span>', '                {% if data.result.bseries_computation.computation_time %}', '                <span><strong>Computation Time:</strong> {{ data.result.bseries_computation.computation_time }}ms</span>', '                {% endif %}', '            </div>', '        </div>', '        {% endif %}', '    </div>', '']
    def _generate_batch_overview_section(self, context: DTESNTemplateContext, analysis: Dict[str, Any]) -> List[str]:
        return ['    <div class="card dtesn-data-card">', '        <h3>Batch Processing Overview</h3>', '        {% if data.batch_size is defined %}', '        <div class="batch-summary">', '            <div class="batch-stats">', '                <span><strong>Batch Size:</strong> {{ data.batch_size }}</span>', '                {% if data.successful_count is defined %}', '                <span><strong>Successful:</strong> {{ data.successful_count }}</span>', '                {% endif %}', '                {% if data.failed_count is defined %}', '                <span><strong>Failed:</strong> {{ data.failed_count }}</span>', '                {% endif %}', '            </div>', '        </div>', '        {% endif %}', '    </div>', '']
    def _generate_main_data_section(self, context: DTESNTemplateContext, analysis: Dict[str, Any]) -> List[str]:
        return ['    <div class="card dtesn-data-card">', '        <h3>Processing Results</h3>', '        <div class="result-display">', '            {% if data.result %}', '            <div class="result-content">', '                {% if data.result is mapping %}', '                    {% for key, value in data.result.items() %}', '                    <div class="result-item">', '                        <strong>{{ key|replace("_", " ")|title }}:</strong>', '                        {% if value is mapping %}', '                            <div class="nested-data">', '                            {% for nested_key, nested_value in value.items() %}', '                                <span>{{ nested_key }}: {{ nested_value }}</span>', '                            {% endfor %}', '                            </div>', '                        {% else %}', '                            <span>{{ value }}</span>', '                        {% endif %}', '                    </div>', '                    {% endfor %}', '                {% else %}', '                    <div class="result-content">{{ data.result }}</div>', '                {% endif %}', '            </div>', '            {% endif %}', '        </div>', '    </div>', '']
    def _create_template_cache_key(self, context: DTESNTemplateContext, dtesn_result: Dict[str, Any]) -> str:
        key_components = [context.dtesn_result_type, str(context.data_complexity), str(context.membrane_layers), context.client_type, str(context.requires_visualization)]
        result_str = json.dumps(dtesn_result, sort_keys=True, default=str)
        result_hash = hashlib.sha256(result_str.encode()).hexdigest()[:16]
        key_components.append(result_hash)
        return '_'.join(key_components)
    def _add_to_cache(self, key: str, template_cache: TemplateCache):
        if len(self.template_cache) >= self.max_cache_entries:
            oldest_key = min(self.template_cache.keys(), key=lambda k: self.template_cache[k].last_access)
            del self.template_cache[oldest_key]
        self.template_cache[key] = template_cache
    async def _get_fallback_template(self, context: DTESNTemplateContext) -> str:
        fallback_key = f'fallback_{context.data_complexity}_{context.client_type}'
        if fallback_key not in self.template_cache:
            fallback_content = '{% extends "base.html" %}\n{% block title %}Deep Tree Echo - Processing Results{% endblock %}\n{% block content %}\n<div class="card">\n    <h2>DTESN Processing Results</h2>\n    <div class="result-display">\n        <pre>{{ data.result | tojsonpretty }}</pre>\n    </div>\n</div>\n{% endblock %}'
            env = Environment(loader=FileSystemLoader(str(self.templates_dir)))
            compiled_template = env.from_string(fallback_content)
            template_cache = TemplateCache(template_key=fallback_key, compiled_template=compiled_template, rendered_cache=OrderedDict(), creation_time=time.time())
            self.template_cache[fallback_key] = template_cache
        return fallback_key
    async def render_template(self, template_key: str, context_data: Dict[str, Any]) -> str:
        if template_key not in self.template_cache:
            raise ValueError(f'Template key not found: {template_key}')
        template_cache = self.template_cache[template_key]
        content_hash = hashlib.sha256(json.dumps(context_data, sort_keys=True, default=str).encode()).hexdigest()[:16]
        if content_hash in template_cache.rendered_cache:
            template_cache.access_count += 1
            template_cache.last_access = time.time()
            return template_cache.rendered_cache[content_hash]
        start_time = time.time()
        try:
            rendered_html = template_cache.compiled_template.render(request=context_data.get('request'), data=context_data.get('data', {}), **context_data)
            render_time = time.time() - start_time
            template_cache.total_render_time += render_time
            self._add_to_rendered_cache(template_cache, content_hash, rendered_html)
            template_cache.access_count += 1
            template_cache.last_access = time.time()
            return rendered_html
        except Exception as e:
            logger.error(f'Template rendering error for {template_key}: {e}')
            return f"""\n            <div class="card">\n                <h2>Template Rendering Error</h2>\n                <p>Error rendering template: {e}</p>\n                <pre>{json.dumps(context_data.get('data', {}), indent=2)}</pre>\n            </div>\n            """
    def _add_to_rendered_cache(self, template_cache: TemplateCache, content_hash: str, rendered_html: str):
        if len(template_cache.rendered_cache) >= self.max_rendered_cache_per_template:
            template_cache.rendered_cache.popitem(last=False)
        template_cache.rendered_cache[content_hash] = rendered_html
    def get_cache_stats(self) -> Dict[str, Any]:
        total_templates = len(self.template_cache)
        total_rendered_entries = sum((len(cache.rendered_cache) for cache in self.template_cache.values()))
        hit_rate = self.cache_hits / (self.cache_hits + self.cache_misses) if self.cache_hits + self.cache_misses > 0 else 0
        return {'template_cache_entries': total_templates, 'rendered_cache_entries': total_rendered_entries, 'cache_hits': self.cache_hits, 'cache_misses': self.cache_misses, 'hit_rate': hit_rate, 'avg_access_count': sum((cache.access_count for cache in self.template_cache.values())) / max(total_templates, 1), 'total_render_time': sum((cache.total_render_time for cache in self.template_cache.values()))}
class AdvancedTemplateEngine:
    def __init__(self, templates_dir: Path):
        self.templates_dir = templates_dir
        self.dynamic_generator = DTESNTemplateDynamicGenerator(templates_dir)
        self.client_detector = self._init_client_detector()
    def _init_client_detector(self) -> Dict[str, Any]:
        return {'mobile_patterns': ['Mobile', 'Android', 'iPhone', 'iPad', 'Windows Phone'], 'tablet_patterns': ['Tablet', 'iPad', 'Android.*Tablet'], 'api_client_patterns': ['curl', 'wget', 'python-requests', 'postman']}
    async def render_dtesn_result(self, request: Request, dtesn_result: Dict[str, Any], result_type: str='membrane_evolution', template_name: Optional[str]=None) -> str:
        client_type = self._detect_client_type(request)
        data_complexity = self._analyze_result_complexity(dtesn_result)
        context = DTESNTemplateContext(dtesn_result_type=result_type, data_complexity=data_complexity, result_size=self._calculate_result_size(dtesn_result), processing_time_ms=dtesn_result.get('processing_time_ms', 0), membrane_layers=dtesn_result.get('membrane_layers', 1), has_errors='error' in str(dtesn_result).lower(), has_warnings='warning' in str(dtesn_result).lower(), requires_visualization=data_complexity >= 3, client_type=client_type)
        if template_name:
            from fastapi.templating import Jinja2Templates
            templates = Jinja2Templates(directory=str(self.templates_dir))
            return templates.TemplateResponse(template_name, {'request': request, 'data': dtesn_result})
        else:
            template_key = await self.dynamic_generator.generate_template(context, dtesn_result)
            context_data = {'request': request, 'data': dtesn_result, 'context': context, 'timestamp': time.time()}
            return await self.dynamic_generator.render_template(template_key, context_data)
    def _detect_client_type(self, request: Request) -> str:
        user_agent = request.headers.get('user-agent', '').lower()
        for pattern in self.client_detector['api_client_patterns']:
            if pattern.lower() in user_agent:
                return 'api_client'
        for pattern in self.client_detector['mobile_patterns']:
            if pattern.lower() in user_agent:
                return 'mobile'
        for pattern in self.client_detector['tablet_patterns']:
            if pattern.lower() in user_agent:
                return 'tablet'
        return 'browser'
    def _analyze_result_complexity(self, dtesn_result: Dict[str, Any]) -> int:
        complexity_score = 0
        def analyze_structure(obj, depth=0):
            nonlocal complexity_score
            if depth > 3:
                complexity_score += 3
                return
            if isinstance(obj, dict):
                complexity_score += len(obj) * 0.1
                for value in obj.values():
                    analyze_structure(value, depth + 1)
            elif isinstance(obj, list):
                complexity_score += len(obj) * 0.05
                if obj:
                    analyze_structure(obj[0], depth + 1)
        analyze_structure(dtesn_result)
        if complexity_score < 2:
            return 1
        elif complexity_score < 8:
            return 2
        else:
            return 3
    def _calculate_result_size(self, dtesn_result: Dict[str, Any]) -> int:
        return len(json.dumps(dtesn_result, default=str))
    def get_performance_stats(self) -> Dict[str, Any]:
        cache_stats = self.dynamic_generator.get_cache_stats()
        return {'advanced_template_engine': True, 'dynamic_generation_enabled': True, 'responsive_adaptation_enabled': True, 'cache_performance': cache_stats, 'supported_result_types': list(self.dynamic_generator.template_patterns.keys()), 'supported_client_types': ['browser', 'mobile', 'tablet', 'api_client']}