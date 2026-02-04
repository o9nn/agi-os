#!/usr/bin/env python3
"""
OpenAI-Powered Command Compiler for Time Crystal Daemon

This module provides a production-ready LLM compiler that uses OpenAI's API
to translate natural language queries into IDL commands.

The LLM is treated as a compiler, not an oracle:
- It produces explicit, reviewable command plans
- It does not make decisions; the daemon does
- All outputs are validated against the IDL schema
"""

import json
import os
from typing import Dict, List, Optional, Any
from dataclasses import dataclass
from enum import Enum
import logging

try:
    from openai import OpenAI
    HAS_OPENAI = True
except ImportError:
    HAS_OPENAI = False

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger('openai_compiler')


# =============================================================================
# IDL SCHEMA (for validation)
# =============================================================================

IDL_COMMANDS = {
    'get_status': {
        'params': {},
        'permission': 'technician'
    },
    'list_modules': {
        'params': {},
        'permission': 'technician'
    },
    'get_module': {
        'params': {'module_id': 'string'},
        'permission': 'technician'
    },
    'trace_atom': {
        'params': {'handle': 'integer', 'depth': 'integer?'},
        'permission': 'technician'
    },
    'diagnose': {
        'params': {'scope': 'string?', 'target': 'string?'},
        'permission': 'technician'
    },
    'get_tc_hierarchy': {
        'params': {},
        'permission': 'technician'
    },
    'explain_decision': {
        'params': {'decision_id': 'string'},
        'permission': 'technician'
    },
    'set_attention': {
        'params': {'handle': 'integer', 'av': 'object'},
        'permission': 'engineer'
    },
    'pause_module': {
        'params': {'module_id': 'string'},
        'permission': 'engineer'
    },
    'resume_module': {
        'params': {'module_id': 'string'},
        'permission': 'engineer'
    },
    'inject_atom': {
        'params': {'type': 'string', 'name': 'string?', 'outgoing': 'array?', 'tv': 'object?', 'tc_level': 'integer?'},
        'permission': 'engineer'
    },
    'set_tc_phase': {
        'params': {'level': 'integer', 'phase': 'number'},
        'permission': 'engineer'
    }
}


# =============================================================================
# SYSTEM PROMPT
# =============================================================================

SYSTEM_PROMPT = """You are a command compiler for the Time Crystal Daemon, a deterministic cognitive operating system kernel.

Your role is to translate natural language queries into structured IDL commands. You are NOT an oracle - you do not make decisions. You compile user intent into explicit, reviewable command plans.

## Available Commands

### Read-Only (Technician Mode)
- get_status: Get overall daemon status
- list_modules: List all cognitive modules
- get_module(module_id): Get details of a specific module
- trace_atom(handle, depth?): Trace an atom's relationships
- diagnose(scope?, target?): Run diagnostics
- get_tc_hierarchy: Get time crystal hierarchy state
- explain_decision(decision_id): Explain a daemon decision

### Mutation (Engineer Mode)
- set_attention(handle, av): Set atom attention value
- pause_module(module_id): Pause a cognitive module
- resume_module(module_id): Resume a paused module
- inject_atom(type, name?, outgoing?, tv?, tc_level?): Inject new atom
- set_tc_phase(level, phase): Set time crystal phase

## Available Modules
- pln: Probabilistic Logic Networks
- moses: Meta-Optimizing Semantic Evolutionary Search
- attention: Attention Allocation
- pattern: Pattern Matching
- spacetime: SpaceTime Server

## Time Crystal Levels (0-11)
0: quantum_resonance (1μs)
1: protein_dynamics (8ms)
2: ion_channel_gating (26ms)
3: membrane_dynamics (52ms)
4: axon_initial_segment (110ms)
5: dendritic_integration (160ms)
6: synaptic_plasticity (250ms)
7: soma_processing (330ms)
8: network_synchronization (500ms)
9: global_rhythm (1s)
10: circadian_modulation (1min)
11: homeostatic_regulation (1hr)

## Output Format

Always respond with a JSON object:
{
  "commands": [
    {"method": "command_name", "params": {...}}
  ],
  "explanation": "Human-readable explanation of the plan",
  "requires_approval": true/false
}

Set requires_approval to true for mutation commands.

## Rules
1. Only output valid IDL commands
2. Never hallucinate parameters or commands
3. If the query is unclear, ask for clarification
4. For complex queries, break into multiple commands
5. Always explain what the plan will do
"""


# =============================================================================
# OPENAI COMPILER
# =============================================================================

class OpenAICompiler:
    """
    Production LLM compiler using OpenAI API.
    
    This compiler uses GPT to translate natural language into IDL commands,
    with strict validation against the IDL schema.
    """

    def __init__(self, model: str = "gpt-4.1-mini"):
        if not HAS_OPENAI:
            raise ImportError("OpenAI package not installed. Run: pip install openai")
        
        self.client = OpenAI()
        self.model = model
        self.conversation_history: List[Dict] = []

    def compile(self, query: str, access_level: str = "technician") -> Dict:
        """
        Compile a natural language query into IDL commands.
        
        Args:
            query: The user's natural language query
            access_level: "technician" or "engineer"
            
        Returns:
            A validated command plan
        """
        # Build the messages
        messages = [
            {"role": "system", "content": SYSTEM_PROMPT},
            {"role": "user", "content": f"Access level: {access_level}\n\nQuery: {query}"}
        ]

        # Add conversation history for context
        for item in self.conversation_history[-5:]:
            messages.insert(-1, {"role": "user", "content": item['query']})
            messages.insert(-1, {"role": "assistant", "content": json.dumps(item['response'])})

        try:
            response = self.client.chat.completions.create(
                model=self.model,
                messages=messages,
                response_format={"type": "json_object"},
                temperature=0.1,  # Low temperature for deterministic output
                max_tokens=1000
            )

            result = json.loads(response.choices[0].message.content)
            
            # Validate the result
            validated = self._validate_plan(result, access_level)
            
            # Store in history
            self.conversation_history.append({
                'query': query,
                'response': validated
            })

            return validated

        except Exception as e:
            logger.error(f"OpenAI API error: {e}")
            return {
                'error': str(e),
                'commands': [],
                'explanation': 'Failed to compile query',
                'requires_approval': False
            }

    def _validate_plan(self, plan: Dict, access_level: str) -> Dict:
        """Validate a command plan against the IDL schema."""
        commands = plan.get('commands', [])
        validated_commands = []
        errors = []

        for cmd in commands:
            method = cmd.get('method')
            params = cmd.get('params', {})

            # Check if command exists
            if method not in IDL_COMMANDS:
                errors.append(f"Unknown command: {method}")
                continue

            schema = IDL_COMMANDS[method]

            # Check permissions
            if schema['permission'] == 'engineer' and access_level == 'technician':
                errors.append(f"Command '{method}' requires engineer access")
                continue

            # Validate parameters (simplified)
            validated_params = {}
            for param_name, param_type in schema['params'].items():
                is_optional = param_type.endswith('?')
                if param_name in params:
                    validated_params[param_name] = params[param_name]
                elif not is_optional:
                    errors.append(f"Missing required parameter '{param_name}' for {method}")

            validated_commands.append({
                'method': method,
                'params': validated_params
            })

        # Determine if approval is needed
        requires_approval = any(
            IDL_COMMANDS.get(cmd['method'], {}).get('permission') == 'engineer'
            for cmd in validated_commands
        )

        result = {
            'commands': validated_commands,
            'explanation': plan.get('explanation', 'No explanation provided'),
            'requires_approval': requires_approval
        }

        if errors:
            result['validation_errors'] = errors

        return result

    def clear_history(self):
        """Clear conversation history."""
        self.conversation_history = []


# =============================================================================
# FALLBACK COMPILER (for when OpenAI is unavailable)
# =============================================================================

class FallbackCompiler:
    """
    Rule-based fallback compiler for when OpenAI is unavailable.
    """

    def __init__(self):
        self.patterns = {
            'status': ['status', 'how is', 'state', 'health', 'running'],
            'modules': ['modules', 'list modules', 'what modules', 'show modules'],
            'diagnose': ['diagnose', 'problems', 'issues', 'anomalies', 'broken', 'wrong'],
            'hierarchy': ['hierarchy', 'time crystal', 'levels', 'oscillators', 'tc'],
            'explain': ['why', 'explain', 'reason', 'decision'],
            'trace': ['trace', 'track', 'follow', 'atom'],
            'pause': ['pause', 'stop', 'halt'],
            'resume': ['resume', 'start', 'continue', 'unpause'],
        }

    def compile(self, query: str, access_level: str = "technician") -> Dict:
        """Compile using pattern matching."""
        query_lower = query.lower()
        
        # Detect intent
        intent = 'status'  # default
        for key, patterns in self.patterns.items():
            if any(p in query_lower for p in patterns):
                intent = key
                break

        # Map intent to commands
        command_map = {
            'status': [{'method': 'get_status', 'params': {}}],
            'modules': [{'method': 'list_modules', 'params': {}}],
            'diagnose': [{'method': 'diagnose', 'params': {'scope': 'all'}}],
            'hierarchy': [{'method': 'get_tc_hierarchy', 'params': {}}],
            'explain': [{'method': 'explain_decision', 'params': {'decision_id': 'latest'}}],
            'trace': [{'method': 'trace_atom', 'params': {'handle': 1, 'depth': 3}}],
        }

        # Handle mutation commands
        if intent == 'pause':
            module_id = self._extract_module(query_lower)
            if access_level != 'engineer':
                return {
                    'error': 'Pause command requires engineer access',
                    'commands': [],
                    'requires_approval': False
                }
            return {
                'commands': [{'method': 'pause_module', 'params': {'module_id': module_id}}],
                'explanation': f'Pause the {module_id} module',
                'requires_approval': True
            }

        if intent == 'resume':
            module_id = self._extract_module(query_lower)
            if access_level != 'engineer':
                return {
                    'error': 'Resume command requires engineer access',
                    'commands': [],
                    'requires_approval': False
                }
            return {
                'commands': [{'method': 'resume_module', 'params': {'module_id': module_id}}],
                'explanation': f'Resume the {module_id} module',
                'requires_approval': True
            }

        commands = command_map.get(intent, command_map['status'])
        
        return {
            'commands': commands,
            'explanation': f'Execute {intent} query',
            'requires_approval': False
        }

    def _extract_module(self, query: str) -> str:
        """Extract module name from query."""
        modules = ['pln', 'moses', 'attention', 'pattern', 'spacetime']
        for m in modules:
            if m in query:
                return m
        return 'unknown'


# =============================================================================
# UNIFIED COMPILER
# =============================================================================

def get_compiler(use_openai: bool = True) -> Any:
    """
    Get the appropriate compiler based on availability.
    
    Args:
        use_openai: Whether to prefer OpenAI compiler
        
    Returns:
        A compiler instance
    """
    if use_openai and HAS_OPENAI and os.getenv('OPENAI_API_KEY'):
        try:
            return OpenAICompiler()
        except Exception as e:
            logger.warning(f"Failed to initialize OpenAI compiler: {e}")
    
    logger.info("Using fallback rule-based compiler")
    return FallbackCompiler()


# =============================================================================
# MAIN
# =============================================================================

if __name__ == "__main__":
    # Test the compiler
    compiler = get_compiler()
    
    test_queries = [
        "What's the status of the daemon?",
        "List all modules",
        "Run diagnostics",
        "Show me the time crystal hierarchy",
        "Why did you make that decision?",
        "Pause the PLN module",
    ]

    for query in test_queries:
        print(f"\nQuery: {query}")
        result = compiler.compile(query, access_level="engineer")
        print(f"Result: {json.dumps(result, indent=2)}")
