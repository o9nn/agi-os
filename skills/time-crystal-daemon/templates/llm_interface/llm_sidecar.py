#!/usr/bin/env python3
"""
LLM Interface Sidecar for Time Crystal Daemon

This sidecar service provides a natural language interface to the Time Crystal Daemon.
It compiles user intents into IDL commands and narrates daemon responses.

The LLM acts as a controlled interpreter, NOT an oracle:
- It compiles user intent into explicit, reviewable plans
- It does not make decisions; the deterministic daemon does
- It narrates structured output from the daemon
"""

import json
import socket
import os
from typing import Dict, List, Optional, Any
from dataclasses import dataclass, field
from enum import Enum
import logging

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('llm_sidecar')


# =============================================================================
# ACCESS CONTROL
# =============================================================================

class AccessLevel(Enum):
    """User access levels."""
    TECHNICIAN = "technician"
    ENGINEER = "engineer"


# Commands allowed for each access level
TECHNICIAN_COMMANDS = {
    'get_status', 'list_modules', 'get_module', 'trace_atom',
    'diagnose', 'get_tc_hierarchy', 'explain_decision'
}

ENGINEER_COMMANDS = TECHNICIAN_COMMANDS | {
    'set_attention', 'pause_module', 'resume_module',
    'inject_atom', 'set_tc_phase'
}


@dataclass
class UserSession:
    """Represents a user session with access control."""
    user_id: str
    access_level: AccessLevel = AccessLevel.TECHNICIAN
    history: List[Dict] = field(default_factory=list)


# =============================================================================
# COMMAND COMPILER
# =============================================================================

class CommandCompiler:
    """
    Compiles natural language intents into IDL commands.
    
    This is the core of the LLM interface. It uses pattern matching
    and intent classification to translate user queries into
    structured daemon commands.
    """

    def __init__(self):
        # Intent patterns (in production, this would use an LLM)
        self.intent_patterns = {
            'status': ['status', 'how is', 'what is the state', 'health'],
            'modules': ['modules', 'list modules', 'what modules'],
            'diagnose': ['diagnose', 'problems', 'issues', 'anomalies', 'broken'],
            'explain': ['why', 'explain', 'reason'],
            'trace': ['trace', 'track', 'follow', 'provenance'],
            'hierarchy': ['hierarchy', 'time crystal', 'levels', 'oscillators'],
            'pause': ['pause', 'stop', 'halt'],
            'resume': ['resume', 'start', 'continue'],
            'attention': ['attention', 'focus', 'importance'],
        }

    def classify_intent(self, query: str) -> str:
        """Classify the user's intent from their query."""
        query_lower = query.lower()
        
        for intent, patterns in self.intent_patterns.items():
            for pattern in patterns:
                if pattern in query_lower:
                    return intent
        
        return 'unknown'

    def compile(self, query: str, access_level: AccessLevel) -> Dict:
        """
        Compile a natural language query into an IDL command plan.
        
        Returns a plan with:
        - commands: List of IDL commands to execute
        - requires_approval: Whether the plan needs user approval
        - explanation: Human-readable explanation of the plan
        """
        intent = self.classify_intent(query)
        
        # Map intents to commands
        command_map = {
            'status': [{'method': 'get_status', 'params': {}}],
            'modules': [{'method': 'list_modules', 'params': {}}],
            'diagnose': [{'method': 'diagnose', 'params': {'scope': 'all'}}],
            'hierarchy': [{'method': 'get_tc_hierarchy', 'params': {}}],
        }

        # Handle more complex intents
        if intent == 'explain':
            # Extract decision ID if present
            commands = [{'method': 'explain_decision', 'params': {'decision_id': 'latest'}}]
        elif intent == 'trace':
            # Would need to extract atom handle from query
            commands = [{'method': 'trace_atom', 'params': {'handle': 1, 'depth': 3}}]
        elif intent == 'pause':
            # Extract module name
            module_id = self._extract_module_id(query)
            commands = [{'method': 'pause_module', 'params': {'module_id': module_id}}]
        elif intent == 'resume':
            module_id = self._extract_module_id(query)
            commands = [{'method': 'resume_module', 'params': {'module_id': module_id}}]
        elif intent == 'attention':
            # This would require more parsing
            commands = [{'method': 'diagnose', 'params': {'scope': 'all'}}]
        else:
            commands = command_map.get(intent, [{'method': 'get_status', 'params': {}}])

        # Check permissions
        requires_approval = False
        for cmd in commands:
            method = cmd['method']
            if method not in TECHNICIAN_COMMANDS:
                if access_level == AccessLevel.TECHNICIAN:
                    return {
                        'error': f"Command '{method}' requires engineer access",
                        'commands': [],
                        'requires_approval': False
                    }
                requires_approval = True

        return {
            'commands': commands,
            'requires_approval': requires_approval,
            'explanation': self._generate_explanation(commands, intent)
        }

    def _extract_module_id(self, query: str) -> str:
        """Extract module ID from query (simplified)."""
        modules = ['pln', 'moses', 'attention', 'pattern', 'spacetime']
        query_lower = query.lower()
        for module in modules:
            if module in query_lower:
                return module
        return 'unknown'

    def _generate_explanation(self, commands: List[Dict], intent: str) -> str:
        """Generate a human-readable explanation of the plan."""
        if not commands:
            return "No commands to execute."
        
        explanations = {
            'get_status': "Check the overall daemon status",
            'list_modules': "List all cognitive modules",
            'diagnose': "Run diagnostics to identify anomalies",
            'get_tc_hierarchy': "Get the time crystal hierarchy state",
            'trace_atom': "Trace an atom's relationships",
            'explain_decision': "Explain a daemon decision",
            'pause_module': "Pause a cognitive module",
            'resume_module': "Resume a paused module",
        }

        steps = []
        for i, cmd in enumerate(commands, 1):
            method = cmd['method']
            desc = explanations.get(method, f"Execute {method}")
            steps.append(f"{i}. {desc}")

        return "Plan:\n" + "\n".join(steps)


# =============================================================================
# RESPONSE NARRATOR
# =============================================================================

class ResponseNarrator:
    """
    Narrates daemon responses in natural language.
    
    This translates structured JSON responses from the daemon
    into human-readable explanations.
    """

    def narrate(self, method: str, result: Dict) -> str:
        """Narrate a daemon response."""
        narrators = {
            'get_status': self._narrate_status,
            'list_modules': self._narrate_modules,
            'diagnose': self._narrate_diagnose,
            'get_tc_hierarchy': self._narrate_hierarchy,
            'trace_atom': self._narrate_trace,
            'explain_decision': self._narrate_decision,
            'pause_module': self._narrate_action,
            'resume_module': self._narrate_action,
        }

        narrator = narrators.get(method, self._narrate_generic)
        return narrator(result)

    def _narrate_status(self, result: Dict) -> str:
        """Narrate daemon status."""
        status = result.get('status', 'unknown')
        uptime = result.get('uptime_seconds', 0)
        atoms = result.get('atom_count', 0)
        attention = result.get('total_attention', 0)
        modules = result.get('active_modules', [])
        tc_state = result.get('tc_state', {})

        hours, remainder = divmod(uptime, 3600)
        minutes, seconds = divmod(remainder, 60)

        narrative = f"""**Daemon Status: {status.upper()}**

The Time Crystal Daemon has been running for {hours}h {minutes}m {seconds}s.

**Resources:**
- Atoms in AtomSpace: {atoms:,}
- Total Attention: {attention:,}
- Active Modules: {', '.join(modules) if modules else 'None'}

**Time Crystal State:**
- Current Level: {tc_state.get('current_level', 'N/A')} (Global Rhythm)
- Global Phase: {tc_state.get('global_phase', 0):.2%}
"""
        return narrative

    def _narrate_modules(self, result: List[Dict]) -> str:
        """Narrate module list."""
        if not result:
            return "No cognitive modules are currently loaded."

        lines = ["**Cognitive Modules:**\n"]
        for module in result:
            status_icon = "🟢" if module['status'] == 'running' else "🟡" if module['status'] == 'paused' else "🔴"
            lines.append(f"- {status_icon} **{module['name']}** (`{module['id']}`)")
            lines.append(f"  - Status: {module['status']}")
            lines.append(f"  - TC Levels: {module['tc_levels']}")
            lines.append(f"  - Attention Usage: {module['attention_usage']:.1%}")
            lines.append("")

        return "\n".join(lines)

    def _narrate_diagnose(self, result: Dict) -> str:
        """Narrate diagnostic results."""
        anomalies = result.get('anomalies', [])
        metrics = result.get('metrics', {})
        recommendations = result.get('recommendations', [])

        narrative = ["**Diagnostic Report**\n"]

        # Metrics
        narrative.append("**Metrics:**")
        for key, value in metrics.items():
            narrative.append(f"- {key.replace('_', ' ').title()}: {value:,}")
        narrative.append("")

        # Anomalies
        if anomalies:
            narrative.append(f"**Anomalies Detected: {len(anomalies)}**")
            for anomaly in anomalies:
                severity = anomaly['severity'].upper()
                narrative.append(f"- [{severity}] {anomaly['module']}: {anomaly['description']}")
        else:
            narrative.append("**No anomalies detected.** ✓")
        narrative.append("")

        # Recommendations
        if recommendations:
            narrative.append("**Recommendations:**")
            for rec in recommendations:
                narrative.append(f"- {rec}")

        return "\n".join(narrative)

    def _narrate_hierarchy(self, result: Dict) -> str:
        """Narrate time crystal hierarchy."""
        levels = result.get('levels', [])
        
        narrative = ["**Time Crystal Hierarchy**\n"]
        narrative.append("| Level | Name | Period | Phase | Atoms |")
        narrative.append("|-------|------|--------|-------|-------|")
        
        for level in levels:
            period = level['period_ms']
            if period >= 1000:
                period_str = f"{period/1000:.1f}s"
            else:
                period_str = f"{period:.1f}ms"
            
            phase_bar = "█" * int(level['current_phase'] * 10) + "░" * (10 - int(level['current_phase'] * 10))
            
            narrative.append(
                f"| {level['id']} | {level['name']} | {period_str} | {phase_bar} | {level['atom_count']} |"
            )

        return "\n".join(narrative)

    def _narrate_trace(self, result: Dict) -> str:
        """Narrate atom trace."""
        if not result:
            return "Atom not found."

        atom = result.get('atom', {})
        incoming = result.get('incoming', [])
        outgoing = result.get('outgoing', [])
        provenance = result.get('provenance', [])

        narrative = [f"**Atom Trace: {atom.get('handle')}**\n"]
        narrative.append(f"- Type: {atom.get('type')}")
        narrative.append(f"- Name: {atom.get('name', 'N/A')}")
        narrative.append(f"- TC Level: {atom.get('tc_level')}")
        narrative.append(f"- Truth Value: strength={atom.get('tv', {}).get('strength', 0):.2f}, confidence={atom.get('tv', {}).get('confidence', 0):.2f}")
        narrative.append("")

        if incoming:
            narrative.append(f"**Incoming Links ({len(incoming)}):**")
            for link in incoming[:5]:
                narrative.append(f"- {link.get('type')} ({link.get('handle')})")
        
        if outgoing:
            narrative.append(f"\n**Outgoing Links ({len(outgoing)}):**")
            for link in outgoing[:5]:
                narrative.append(f"- {link.get('type')} ({link.get('handle')})")

        if provenance:
            narrative.append("\n**Provenance:**")
            for p in provenance:
                narrative.append(f"- {p}")

        return "\n".join(narrative)

    def _narrate_decision(self, result: Dict) -> str:
        """Narrate decision explanation."""
        thought = result.get('thought', 'No explanation available.')
        constraints = result.get('constraints', [])
        alternatives = result.get('alternatives', [])
        confidence = result.get('confidence', 0)

        narrative = ["**Decision Explanation**\n"]
        narrative.append(f"**Reasoning:** {thought}")
        narrative.append(f"\n**Confidence:** {confidence:.1%}")

        if constraints:
            narrative.append("\n**Constraints:**")
            for c in constraints:
                narrative.append(f"- {c}")

        if alternatives:
            narrative.append("\n**Alternatives Considered:**")
            for a in alternatives:
                narrative.append(f"- {a}")

        return "\n".join(narrative)

    def _narrate_action(self, result: Dict) -> str:
        """Narrate action result."""
        success = result.get('success', False)
        if success:
            return "✓ Action completed successfully."
        else:
            error = result.get('error', 'Unknown error')
            return f"✗ Action failed: {error}"

    def _narrate_generic(self, result: Dict) -> str:
        """Generic narration for unknown methods."""
        return f"```json\n{json.dumps(result, indent=2)}\n```"


# =============================================================================
# DAEMON CLIENT
# =============================================================================

class DaemonClient:
    """Client for communicating with the Time Crystal Daemon."""

    def __init__(self, socket_path: str = "/tmp/tc_daemon.sock"):
        self.socket_path = socket_path

    def send_command(self, method: str, params: Dict = None) -> Dict:
        """Send a command to the daemon and return the response."""
        request = {
            'jsonrpc': '2.0',
            'id': 1,
            'method': method,
            'params': params or {}
        }

        try:
            sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            sock.connect(self.socket_path)
            sock.sendall(json.dumps(request).encode('utf-8') + b'\n')
            
            response = b''
            while True:
                chunk = sock.recv(4096)
                if not chunk:
                    break
                response += chunk
                if b'\n' in response:
                    break
            
            sock.close()
            
            result = json.loads(response.decode('utf-8'))
            if 'error' in result:
                return {'error': result['error']}
            return result.get('result', {})
            
        except FileNotFoundError:
            return {'error': 'Daemon not running. Start with: python time_crystal_daemon.py'}
        except Exception as e:
            return {'error': str(e)}


# =============================================================================
# LLM SIDECAR
# =============================================================================

class LLMSidecar:
    """
    The main LLM interface sidecar.
    
    This class orchestrates the interaction between the user,
    the command compiler, the daemon, and the response narrator.
    """

    def __init__(self, socket_path: str = "/tmp/tc_daemon.sock"):
        self.compiler = CommandCompiler()
        self.narrator = ResponseNarrator()
        self.client = DaemonClient(socket_path)
        self.sessions: Dict[str, UserSession] = {}

    def get_session(self, user_id: str) -> UserSession:
        """Get or create a user session."""
        if user_id not in self.sessions:
            self.sessions[user_id] = UserSession(user_id=user_id)
        return self.sessions[user_id]

    def set_access_level(self, user_id: str, level: AccessLevel):
        """Set the access level for a user."""
        session = self.get_session(user_id)
        session.access_level = level

    def process_query(self, user_id: str, query: str, auto_approve: bool = False) -> str:
        """
        Process a natural language query.
        
        Args:
            user_id: The user's ID
            query: The natural language query
            auto_approve: Whether to auto-approve mutation commands
            
        Returns:
            A natural language response
        """
        session = self.get_session(user_id)

        # Compile the query into a command plan
        plan = self.compiler.compile(query, session.access_level)

        if 'error' in plan:
            return f"⚠️ {plan['error']}"

        if not plan['commands']:
            return "I'm not sure what you're asking. Try asking about status, modules, or diagnostics."

        # Check if approval is needed
        if plan['requires_approval'] and not auto_approve:
            return f"""This action requires approval.

{plan['explanation']}

Reply with "approve" to execute, or "cancel" to abort."""

        # Execute the commands
        responses = []
        for cmd in plan['commands']:
            result = self.client.send_command(cmd['method'], cmd['params'])
            
            if 'error' in result:
                responses.append(f"Error: {result['error']}")
            else:
                narrative = self.narrator.narrate(cmd['method'], result)
                responses.append(narrative)

        # Record in history
        session.history.append({
            'query': query,
            'plan': plan,
            'responses': responses
        })

        return "\n\n---\n\n".join(responses)

    def suggest_next_actions(self, context: str) -> List[str]:
        """Suggest next actions based on context."""
        suggestions = {
            'anomaly': [
                "Diagnose the affected module",
                "Check attention distribution",
                "Pause the problematic module"
            ],
            'normal': [
                "View module details",
                "Check time crystal hierarchy",
                "Run full diagnostics"
            ]
        }
        
        if 'anomal' in context.lower():
            return suggestions['anomaly']
        return suggestions['normal']


# =============================================================================
# INTERACTIVE CLI
# =============================================================================

def interactive_cli():
    """Run an interactive CLI for the LLM sidecar."""
    sidecar = LLMSidecar()
    user_id = "cli_user"
    
    print("=" * 60)
    print("Time Crystal Daemon - LLM Interface")
    print("=" * 60)
    print("\nCommands:")
    print("  /engineer  - Switch to engineer mode")
    print("  /tech      - Switch to technician mode")
    print("  /help      - Show help")
    print("  /quit      - Exit")
    print("\nOr ask a question in natural language.")
    print("-" * 60)

    while True:
        try:
            query = input("\n> ").strip()
        except (EOFError, KeyboardInterrupt):
            print("\nGoodbye!")
            break

        if not query:
            continue

        if query == '/quit':
            print("Goodbye!")
            break
        elif query == '/engineer':
            sidecar.set_access_level(user_id, AccessLevel.ENGINEER)
            print("Switched to ENGINEER mode.")
            continue
        elif query == '/tech':
            sidecar.set_access_level(user_id, AccessLevel.TECHNICIAN)
            print("Switched to TECHNICIAN mode.")
            continue
        elif query == '/help':
            print("""
Available questions:
- "What's the status?"
- "List all modules"
- "Run diagnostics"
- "Show time crystal hierarchy"
- "Why did you make that decision?"
- "Pause the PLN module" (engineer only)
- "Resume the PLN module" (engineer only)
""")
            continue

        response = sidecar.process_query(user_id, query)
        print("\n" + response)

        # Suggest next actions
        suggestions = sidecar.suggest_next_actions(response)
        if suggestions:
            print("\n**Suggested next actions:**")
            for i, s in enumerate(suggestions, 1):
                print(f"  {i}. {s}")


# =============================================================================
# MAIN
# =============================================================================

if __name__ == "__main__":
    interactive_cli()
