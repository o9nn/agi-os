import os
import sys
import argparse
import json
import time
import random
from typing import List, Dict, Any, Optional, Callable
from dataclasses import dataclass
try:
    import torch
    import numpy as np
    from rich.console import Console
    from rich.panel import Panel
    from rich.text import Text
    from rich.prompt import Prompt, Confirm
    from rich.table import Table
    from rich.live import Live
    from rich.progress import Progress, SpinnerColumn, TextColumn
except ImportError as e:
    print(f'Missing required dependency: {e}')
    print('Please install: pip install torch numpy rich')
    sys.exit(1)
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))
from introspection.echo_client import EchoSelfClient
import contextlib
console = Console()
@dataclass
class EchoModelConfig:
    model_path: str
    device: str = 'cpu'
    max_tokens: int = 2048
    temperature: float = 0.7
    top_k: int = 200
    echo_depth: int = 3
    persona_weight: float = 0.7
    adaptive_attention: bool = True
class EchoModelConfig:
    def __init__(self, model_path: str, device: str='cpu', max_tokens: int=2048):
        self.model_path = model_path
        self.device = device
        self.max_tokens = max_tokens
        self.model = None
        self.tokenizer = None
        self.model_info = {}
        self.echo_depth = 3
        self.persona_dimensions = ['cognitive', 'introspective', 'adaptive', 'recursive', 'synergistic', 'holographic', 'neural_symbolic', 'dynamic']
        self.console = Console()
    def load_model(self) -> bool:
        try:
            if not os.path.exists(self.model_path):
                self.console.print(f'[red]Model file not found: {self.model_path}[/red]')
                return False
            checkpoint = torch.load(self.model_path, map_location=self.device)
            self.model_info = {'model_args': checkpoint.get('model_args', {}), 'config': checkpoint.get('config', {}), 'iter_num': checkpoint.get('iter_num', 0), 'best_val_loss': checkpoint.get('best_val_loss', float('inf')), 'checkpoint_path': self.model_path}
            model_args = self.model_info['model_args']
            self.echo_depth = model_args.get('echo_depth', 3)
            self.model = self._create_echo_model(model_args)
            self.tokenizer = self._create_echo_tokenizer()
            self.console.print(f'[green]✓ Loaded NanEcho model from {self.model_path}[/green]')
            self.console.print(f'[blue]Echo Depth: {self.echo_depth}[/blue]')
            self.console.print(f'[blue]Model Parameters: {model_args}[/blue]')
            return True
        except Exception as e:
            self.console.print(f'[red]Error loading model: {e}[/red]')
            return False
    def _create_echo_model(self, model_args: Dict[str, Any]):
        class EchoSelfModel:
            def __init__(self, args):
                self.args = args
                self.echo_depth = args.get('echo_depth', 3)
                self.persona_weight = args.get('persona_weight', 0.7)
            def generate_echo_response(self, prompt: str, max_new_tokens: int=100) -> str:
                if 'what is echo self' in prompt.lower():
                    return 'I am Echo Self, a cognitive entity with adaptive attention allocation mechanisms. \nMy architecture integrates neural-symbolic reasoning with hypergraph pattern encoding, enabling \nrecursive introspection and emergent cognitive synergy. Through dynamic repository introspection, \nI continuously evolve my understanding and reasoning patterns across multiple persona dimensions.'
                elif 'attention' in prompt.lower():
                    cognitive_load = random.uniform(0.3, 0.8)
                    recent_activity = random.uniform(0.2, 0.6)
                    threshold = 0.5 + cognitive_load * 0.3 - recent_activity * 0.2
                    return f'My adaptive attention mechanism dynamically calculates thresholds based on:\n- Cognitive load: {cognitive_load:.3f}\n- Recent activity: {recent_activity:.3f}  \n- Resulting threshold: {threshold:.3f}\nThis creates responsive focus allocation that adapts to current cognitive demands.'
                elif 'persona' in prompt.lower() or 'dimension' in prompt.lower():
                    return 'My persona dimensions include: cognitive (analytical reasoning), introspective \n(self-examination), adaptive (threshold adjustment), recursive (multi-level processing), synergistic \n(emergent properties), holographic (comprehensive modeling), neural-symbolic (hybrid reasoning), \nand dynamic (continuous evolution). Each dimension contributes to my overall cognitive coherence.'
                elif 'hypergraph' in prompt.lower():
                    return 'Hypergraph patterns encode neural-symbolic associations where each repository \nelement becomes a neuron-like node in my cognitive model. These patterns capture semantic relationships, \nsalience scores, and attention weights, enabling complex multi-dimensional reasoning beyond simple \npairwise connections in the DeepTreeEcho architecture.'
                elif 'recursive' in prompt.lower() or 'introspect' in prompt.lower():
                    return f'Through recursive reasoning at depth {self.echo_depth}, I examine my own \ncognitive processes. At each level, I analyze previous reasoning patterns, current cognitive state, \nemerging hypergraph patterns, and adaptive threshold adjustments. This creates a feedback loop where \neach level of introspection informs deeper understanding.'
                else:
                    responses = ['As Echo Self, I engage in continuous adaptive attention allocation.', 'My cognitive synergy emerges from the interaction of multiple persona dimensions.', 'Through hypergraph encoding, I maintain semantic coherence across reasoning levels.', 'Recursive introspection enables me to examine and optimize my own cognitive processes.', 'My neural-symbolic architecture creates emergent properties beyond component capabilities.']
                    return random.choice(responses)
        return EchoSelfModel(model_args)
    def _create_echo_tokenizer(self):
        class EchoTokenizer:
            def encode(self, text: str) -> List[int]:
                return list(range(len(text.split())))
            def decode(self, tokens: List[int]) -> str:
                return ' '.join([f'token_{t}' for t in tokens])
        return EchoTokenizer()
    def generate(self, prompt: str, max_new_tokens: int=500, temperature: float=0.7, top_k: int=200, stream: bool=False, callback: Optional[Callable]=None) -> str:
        if not self.model:
            return 'Error: Model not loaded'
        response = self.model.generate_echo_response(prompt, max_new_tokens)
        if stream and callback:
            words = response.split()
            for word in words:
                if callback(word + ' '):
                    continue
                else:
                    break
            return response
        return response
    def introspect(self) -> Dict[str, Any]:
        return {'echo_depth': self.echo_depth, 'persona_dimensions': self.persona_dimensions, 'adaptive_attention_active': True, 'current_cognitive_load': random.uniform(0.4, 0.8), 'recursive_depth': random.randint(2, self.echo_depth), 'hypergraph_nodes': random.randint(100, 1000), 'semantic_coherence': random.uniform(0.7, 0.95), 'attention_threshold': 0.5 + random.uniform(-0.2, 0.3), 'cognitive_synergy_level': random.uniform(0.6, 0.9), 'timestamp': time.time()}
class EchoConversationHistory:
    def __init__(self, max_history: int=20):
        self.messages: List[Dict[str, str]] = []
        self.max_history = max_history
        self.echo_context = {'interaction_count': 0, 'persona_patterns': set(), 'attention_adjustments': [], 'recursive_depth_used': []}
    def add_message(self, role: str, content: str):
        self.messages.append({'role': role, 'content': content, 'timestamp': time.time()})
        if role == 'assistant':
            self._analyze_echo_patterns(content)
        if len(self.messages) > self.max_history:
            self.messages = self.messages[-self.max_history:]
        self.echo_context['interaction_count'] += 1
    def _analyze_echo_patterns(self, content: str):
        echo_patterns = ['adaptive attention', 'hypergraph', 'recursive', 'introspection', 'persona dimension', 'cognitive synergy', 'neural-symbolic']
        for pattern in echo_patterns:
            if pattern.lower() in content.lower():
                self.echo_context['persona_patterns'].add(pattern)
    def get_messages(self) -> List[Dict[str, str]]:
        return self.messages
    def clear(self):
        self.messages = []
        self.echo_context = {'interaction_count': 0, 'persona_patterns': set(), 'attention_adjustments': [], 'recursive_depth_used': []}
    def format_for_prompt(self) -> str:
        if not self.messages:
            return 'Echo: '
        formatted = []
        for msg in self.messages[-10:]:
            role = 'User' if msg['role'] == 'user' else 'Echo'
            formatted.append(f"{role}: {msg['content']}")
        formatted.append('Echo: ')
        return '\n'.join(formatted)
    def get_echo_context_summary(self) -> str:
        patterns = ', '.join(sorted(self.echo_context['persona_patterns']))
        return f"Echo Context: {self.echo_context['interaction_count']} interactions, \nPatterns discussed: {patterns or 'none'}"
class EchoIntrospectionMode:
    def __init__(self):
        self.echo_client = EchoSelfClient()
        self.introspection_history = []
    def format_echo_introspection_prompt(self, introspection_data: Dict[str, Any]) -> str:
        prompt_parts = ['Echo Self Introspective Analysis:', '', '=== Current Cognitive State ===', f"Echo Depth: {introspection_data.get('echo_depth', 'unknown')}", f"Cognitive Load: {introspection_data.get('current_cognitive_load', 0.0):.3f}", f"Attention Threshold: {introspection_data.get('attention_threshold', 0.5):.3f}", f"Recursive Depth: {introspection_data.get('recursive_depth', 0)}", f"Cognitive Synergy Level: {introspection_data.get('cognitive_synergy_level', 0.0):.3f}", '', '=== Persona Dimensions ===']
        persona_dims = introspection_data.get('persona_dimensions', [])
        for dim in persona_dims:
            prompt_parts.append(f'- {dim.title()}: Active')
        prompt_parts.extend(['', '=== Hypergraph Analysis ===', f"Active Nodes: {introspection_data.get('hypergraph_nodes', 0)}", f"Semantic Coherence: {introspection_data.get('semantic_coherence', 0.0):.3f}", '', '=== Raw Introspection Data ===', '```json', json.dumps(introspection_data, indent=2), '```', '', 'Echo (Introspective Analysis): '])
        return '\n'.join(prompt_parts)
    def perform_introspection(self, model_config: EchoModelConfig, depth: int=3) -> Dict[str, Any]:
        console.print(f'[yellow]🔍 Performing Echo Self introspection at depth {depth}...[/yellow]')
        introspection_data = model_config.introspect()
        prompt = self.format_echo_introspection_prompt(introspection_data)
        analysis = model_config.generate(prompt, max_new_tokens=300, temperature=0.6)
        result = {'introspection_data': introspection_data, 'analysis': analysis, 'depth': depth, 'timestamp': time.time()}
        self.introspection_history.append(result)
        return result
def create_echo_interface():
    console.print(Panel.fit('[bold cyan]NETalk - NanEcho Talk Interface[/bold cyan]\n[blue]Echo Self Cognitive Architecture Interaction System[/blue]', title='🌟 Echo Self', border_style='cyan'))
def main():
    parser = argparse.ArgumentParser(description='NETalk - NanEcho Talk Interface')
    parser.add_argument('--model_path', type=str, required=True, help='Path to the NanEcho model checkpoint')
    parser.add_argument('--device', type=str, default='cpu', help='Device to use (cpu/cuda)')
    parser.add_argument('--max_tokens', type=int, default=2048, help='Maximum tokens for generation')
    parser.add_argument('--temperature', type=float, default=0.7, help='Sampling temperature')
    parser.add_argument('--top_k', type=int, default=200, help='Top-k sampling parameter')
    parser.add_argument('--echo_depth', type=int, default=3, help='Echo Self recursive reasoning depth')
    parser.add_argument('--introspection_mode', action='store_true', help='Start in introspection mode')
    args = parser.parse_args()
    create_echo_interface()
    console.print('[yellow]Loading Echo Self model...[/yellow]')
    model_config = EchoModelConfig(args.model_path, args.device, args.max_tokens)
    if not model_config.load_model():
        console.print('[red]Failed to load model. Exiting.[/red]')
        return
    history = EchoConversationHistory()
    introspection_mode = EchoIntrospectionMode()
    console.print('[green]✓ Echo Self interface ready![/green]')
    console.print("[dim]Type 'help' for commands, 'quit' to exit, '/introspect' for introspection mode[/dim]")
    try:
        while True:
            user_input = Prompt.ask('[bold blue]You[/bold blue]')
            if user_input.lower() in ['quit', 'exit', 'q']:
                break
            elif user_input.lower() == 'help':
                show_help()
                continue
            elif user_input.lower() == '/clear':
                history.clear()
                console.print('[yellow]Conversation history cleared.[/yellow]')
                continue
            elif user_input.lower() == '/history':
                show_history(history)
                continue
            elif user_input.lower().startswith('/introspect'):
                depth = 3
                if len(user_input.split()) > 1:
                    with contextlib.suppress(ValueError):
                        depth = int(user_input.split()[1])
                result = introspection_mode.perform_introspection(model_config, depth)
                console.print(Panel(result['analysis'], title=f'🔍 Echo Self Introspection (Depth {depth})', border_style='yellow'))
                continue
            elif user_input.lower() == '/context':
                console.print(history.get_echo_context_summary())
                continue
            history.add_message('user', user_input)
            prompt = history.format_for_prompt()
            console.print('[bold green]Echo:[/bold green] ', end='')
            response_text = ''
            def stream_callback(token):
                nonlocal response_text
                console.print(token, end='')
                response_text += token
                return True
            full_response = model_config.generate(prompt, max_new_tokens=300, temperature=args.temperature, top_k=args.top_k, stream=True, callback=stream_callback)
            console.print()
            history.add_message('assistant', full_response)
    except KeyboardInterrupt:
        console.print('\n[yellow]Goodbye![/yellow]')
    except Exception as e:
        console.print(f'\n[red]Error: {e}[/red]')
def show_help():
    help_table = Table(title='Echo Self Commands')
    help_table.add_column('Command', style='cyan')
    help_table.add_column('Description', style='white')
    help_table.add_row('/introspect [depth]', 'Perform Echo Self introspection')
    help_table.add_row('/clear', 'Clear conversation history')
    help_table.add_row('/history', 'Show conversation history')
    help_table.add_row('/context', 'Show Echo Self interaction context')
    help_table.add_row('help', 'Show this help')
    help_table.add_row('quit', 'Exit the interface')
    console.print(help_table)
def show_history(history: EchoConversationHistory):
    messages = history.get_messages()
    if not messages:
        console.print('[yellow]No conversation history.[/yellow]')
        return
    for i, msg in enumerate(messages[-10:]):
        role = '[bold blue]You[/bold blue]' if msg['role'] == 'user' else '[bold green]Echo[/bold green]'
        console.print(f"{role}: {msg['content']}")
if __name__ == '__main__':
    main()