import asyncio
import json
import logging
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, field
from pathlib import Path
import hashlib
import time
try:
    import sys
    echo_sys_path = Path(__file__).parent.parent.parent / 'echo.sys'
    if str(echo_sys_path) not in sys.path:
        sys.path.insert(0, str(echo_sys_path))
    from prompt_kernel.prompt_store import PromptStore
except ImportError:
    class PromptStore:
        def __init__(self):
            self.prompts = {}
        def get_prompt(self, name):
            return None
        def list_prompts(self):
            return []
logger = logging.getLogger(__name__)
@dataclass
class SparkPrompt:
    id: str
    name: str
    template: str
    category: str
    tags: List[str] = field(default_factory=list)
    version: str = '1.0.0'
    author: str = 'unknown'
    hash: str = ''
    created_at: float = 0.0
    metadata: Dict[str, Any] = field(default_factory=dict)
    def __post_init__(self):
        if not self.hash:
            self.hash = hashlib.sha256(self.template.encode()).hexdigest()
        if not self.created_at:
            self.created_at = time.time()
@dataclass
class SparkTheme:
    id: str
    name: str
    config: Dict[str, Any]
    css_vars: Dict[str, str] = field(default_factory=dict)
    components: Dict[str, Any] = field(default_factory=dict)
    version: str = '1.0.0'
    metadata: Dict[str, Any] = field(default_factory=dict)
@dataclass
class SparkComponent:
    name: str
    type: str
    props: Dict[str, Any] = field(default_factory=dict)
    template: str = ''
    styles: Dict[str, Any] = field(default_factory=dict)
    metadata: Dict[str, Any] = field(default_factory=dict)
class SparkAdapter:
    def __init__(self, prompt_store: Optional[PromptStore]=None):
        self.prompt_store = prompt_store or PromptStore()
        self.prompts: Dict[str, SparkPrompt] = {}
        self.themes: Dict[str, SparkTheme] = {}
        self.components: Dict[str, SparkComponent] = {}
        self.spark_root = Path(__file__).parent.parent.parent / '2do' / 'spark.sys'
        if self.spark_root.exists():
            self._load_spark_assets()
        else:
            logger.warning('Spark.sys directory not found, running in compatibility mode')
            self._setup_compatibility_mode()
    def _load_spark_assets(self):
        try:
            self._load_prompts()
            self._load_themes()
            self._load_components()
            logger.info('Loaded Spark.sys assets successfully')
        except Exception as e:
            logger.error(f'Failed to load Spark assets: {e}')
            self._setup_compatibility_mode()
    def _load_prompts(self):
        src_dir = self.spark_root / 'src'
        if not src_dir.exists():
            return
        prompt_files = []
        for pattern in ['*.md', '*.txt', '*.json', '*.yaml']:
            prompt_files.extend(src_dir.rglob(pattern))
        for file_path in prompt_files:
            try:
                self._parse_prompt_file(file_path)
            except Exception as e:
                logger.warning(f'Failed to parse prompt file {file_path}: {e}')
    def _parse_prompt_file(self, file_path: Path):
        content = file_path.read_text(encoding='utf-8')
        if file_path.suffix == '.json':
            self._parse_json_prompt(content, file_path)
        elif file_path.suffix == '.md':
            self._parse_markdown_prompt(content, file_path)
        elif file_path.suffix == '.txt':
            self._parse_text_prompt(content, file_path)
    def _parse_json_prompt(self, content: str, file_path: Path):
        try:
            data = json.loads(content)
            if isinstance(data, dict) and 'prompts' in data:
                for prompt_data in data['prompts']:
                    self._create_prompt_from_data(prompt_data, file_path)
            elif isinstance(data, dict) and 'template' in data:
                self._create_prompt_from_data(data, file_path)
        except json.JSONDecodeError as e:
            logger.warning(f'Invalid JSON in {file_path}: {e}')
    def _parse_markdown_prompt(self, content: str, file_path: Path):
        lines = content.split('\n')
        metadata = {}
        template_lines = []
        in_frontmatter = False
        for line in lines:
            if line.strip() == '---':
                in_frontmatter = not in_frontmatter
                continue
            if in_frontmatter:
                if ':' in line:
                    key, value = line.split(':', 1)
                    metadata[key.strip()] = value.strip()
            else:
                template_lines.append(line)
        template = '\n'.join(template_lines).strip()
        prompt = SparkPrompt(id=metadata.get('id', file_path.stem), name=metadata.get('name', file_path.stem.replace('_', ' ').title()), template=template, category=metadata.get('category', 'general'), tags=metadata.get('tags', '').split(',') if metadata.get('tags') else [], version=metadata.get('version', '1.0.0'), author=metadata.get('author', 'unknown'), metadata={'file_path': str(file_path), **metadata})
        self.prompts[prompt.id] = prompt
    def _parse_text_prompt(self, content: str, file_path: Path):
        prompt = SparkPrompt(id=file_path.stem, name=file_path.stem.replace('_', ' ').title(), template=content.strip(), category='general', metadata={'file_path': str(file_path)})
        self.prompts[prompt.id] = prompt
    def _create_prompt_from_data(self, data: Dict[str, Any], file_path: Path):
        prompt = SparkPrompt(id=data.get('id', file_path.stem), name=data.get('name', data.get('id', file_path.stem)), template=data.get('template', ''), category=data.get('category', 'general'), tags=data.get('tags', []), version=data.get('version', '1.0.0'), author=data.get('author', 'unknown'), metadata={'file_path': str(file_path), **data.get('metadata', {})})
        self.prompts[prompt.id] = prompt
    def _load_themes(self):
        [self.spark_root / 'theme.json', self.spark_root / 'tailwind.config.js', self.spark_root / 'src' / 'themes']
        theme_json = self.spark_root / 'theme.json'
        if theme_json.exists():
            try:
                theme_data = json.loads(theme_json.read_text())
                self._parse_theme_data(theme_data)
            except Exception as e:
                logger.warning(f'Failed to load theme.json: {e}')
        tailwind_config = self.spark_root / 'tailwind.config.js'
        if tailwind_config.exists():
            self._parse_tailwind_config(tailwind_config)
    def _parse_theme_data(self, data: Dict[str, Any]):
        theme = SparkTheme(id='default', name='Spark Default Theme', config=data, metadata={'source': 'theme.json'})
        self.themes[theme.id] = theme
    def _parse_tailwind_config(self, config_path: Path):
        try:
            theme = SparkTheme(id='tailwind', name='Tailwind Theme', config={'framework': 'tailwind', 'config_path': str(config_path)}, metadata={'source': 'tailwind.config.js'})
            self.themes[theme.id] = theme
        except Exception as e:
            logger.warning(f'Failed to parse Tailwind config: {e}')
    def _load_components(self):
        components_json = self.spark_root / 'components.json'
        if components_json.exists():
            try:
                data = json.loads(components_json.read_text())
                for comp_name, comp_data in data.items():
                    component = SparkComponent(name=comp_name, type=comp_data.get('type', 'component'), props=comp_data.get('props', {}), template=comp_data.get('template', ''), styles=comp_data.get('styles', {}), metadata=comp_data.get('metadata', {}))
                    self.components[comp_name] = component
                logger.info(f'Loaded {len(self.components)} components')
            except Exception as e:
                logger.error(f'Failed to load components.json: {e}')
    def _setup_compatibility_mode(self):
        default_prompts = [SparkPrompt(id='system_default', name='Default System Prompt', template='You are a helpful AI assistant.', category='system'), SparkPrompt(id='chat_welcome', name='Chat Welcome', template='Hello! How can I help you today?', category='chat'), SparkPrompt(id='error_handler', name='Error Handler', template='I apologize, but I encountered an error. Please try again.', category='system')]
        for prompt in default_prompts:
            self.prompts[prompt.id] = prompt
        default_theme = SparkTheme(id='default', name='Default Theme', config={'colors': {'primary': '#007bff', 'secondary': '#6c757d', 'success': '#28a745', 'danger': '#dc3545'}})
        self.themes[default_theme.id] = default_theme
        logger.info('Spark adapter running in compatibility mode')
    def get_prompt(self, prompt_id: str) -> Optional[SparkPrompt]:
        return self.prompts.get(prompt_id)
    def list_prompts(self, category: Optional[str]=None, tags: Optional[List[str]]=None) -> List[SparkPrompt]:
        prompts = list(self.prompts.values())
        if category:
            prompts = [p for p in prompts if p.category == category]
        if tags:
            prompts = [p for p in prompts if any((tag in p.tags for tag in tags))]
        return prompts
    def create_prompt(self, prompt_spec: Dict[str, Any]) -> SparkPrompt:
        prompt = SparkPrompt(id=prompt_spec['id'], name=prompt_spec['name'], template=prompt_spec['template'], category=prompt_spec.get('category', 'custom'), tags=prompt_spec.get('tags', []), version=prompt_spec.get('version', '1.0.0'), author=prompt_spec.get('author', 'user'), metadata=prompt_spec.get('metadata', {}))
        self.prompts[prompt.id] = prompt
        asyncio.create_task(self.prompt_store.store_prompt(prompt.id, {'template': prompt.template, 'metadata': {'name': prompt.name, 'category': prompt.category, 'tags': prompt.tags, 'version': prompt.version, 'author': prompt.author, 'hash': prompt.hash, 'adapter': 'spark'}}))
        logger.info(f'Created prompt {prompt.id}')
        return prompt
    def update_prompt(self, prompt_id: str, updates: Dict[str, Any]) -> Optional[SparkPrompt]:
        prompt = self.prompts.get(prompt_id)
        if not prompt:
            return None
        for field_name, value in updates.items():
            if hasattr(prompt, field_name):
                setattr(prompt, field_name, value)
        if 'template' in updates:
            prompt.hash = hashlib.sha256(prompt.template.encode()).hexdigest()
        asyncio.create_task(self.prompt_store.store_prompt(prompt.id, {'template': prompt.template, 'metadata': {'name': prompt.name, 'category': prompt.category, 'tags': prompt.tags, 'version': prompt.version, 'author': prompt.author, 'hash': prompt.hash, 'adapter': 'spark'}}))
        logger.info(f'Updated prompt {prompt_id}')
        return prompt
    def get_theme(self, theme_id: str) -> Optional[SparkTheme]:
        return self.themes.get(theme_id)
    def list_themes(self) -> List[SparkTheme]:
        return list(self.themes.values())
    def get_component(self, component_name: str) -> Optional[SparkComponent]:
        return self.components.get(component_name)
    def list_components(self, component_type: Optional[str]=None) -> List[SparkComponent]:
        components = list(self.components.values())
        if component_type:
            components = [c for c in components if c.type == component_type]
        return components
    async def render_prompt(self, prompt_id: str, variables: Optional[Dict[str, Any]]=None) -> Optional[str]:
        prompt = self.get_prompt(prompt_id)
        if not prompt:
            return None
        template = prompt.template
        variables = variables or {}
        try:
            for key, value in variables.items():
                placeholder = f'{{{key}}}'
                template = template.replace(placeholder, str(value))
            return template
        except Exception as e:
            logger.error(f'Failed to render prompt {prompt_id}: {e}')
            return None
    async def sync_with_echo_sys(self):
        try:
            synced_count = 0
            for prompt in self.prompts.values():
                await self.prompt_store.store_prompt(prompt.id, {'template': prompt.template, 'metadata': {'name': prompt.name, 'category': prompt.category, 'tags': prompt.tags, 'version': prompt.version, 'author': prompt.author, 'hash': prompt.hash, 'adapter': 'spark', 'synced_at': time.time()}})
                synced_count += 1
            logger.info(f'Synced {synced_count} prompts with echo.sys')
            return synced_count
        except Exception as e:
            logger.error(f'Failed to sync with echo.sys: {e}')
            return 0
    async def update_assets(self):
        if self.spark_root.exists():
            old_count = len(self.prompts)
            self._load_spark_assets()
            new_count = len(self.prompts)
            logger.info(f'Asset update complete: {old_count} -> {new_count} prompts')
            await self.sync_with_echo_sys()
            return {'old_count': old_count, 'new_count': new_count}
        else:
            logger.warning('Cannot update assets - spark.sys directory not found')
            return {'error': 'spark.sys directory not found'}
    def export_prompts(self, format: str='json') -> str:
        if format.lower() == 'json':
            export_data = {'prompts': [{'id': p.id, 'name': p.name, 'template': p.template, 'category': p.category, 'tags': p.tags, 'version': p.version, 'author': p.author, 'hash': p.hash, 'metadata': p.metadata} for p in self.prompts.values()], 'exported_at': time.time(), 'count': len(self.prompts)}
            return json.dumps(export_data, indent=2)
        else:
            raise ValueError(f'Unsupported export format: {format}')
    def health_check(self) -> Dict[str, Any]:
        return {'status': 'healthy', 'spark_root_exists': self.spark_root.exists(), 'prompts_loaded': len(self.prompts), 'themes_loaded': len(self.themes), 'components_loaded': len(self.components), 'prompt_store_available': self.prompt_store is not None, 'categories': list(set((p.category for p in self.prompts.values())))}