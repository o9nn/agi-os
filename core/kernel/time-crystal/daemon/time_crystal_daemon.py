#!/usr/bin/env python3
"""
Time Crystal Daemon - Core Implementation

A deterministic cognitive daemon based on the OpenCog-Inferno kernel architecture
with hierarchical time crystal temporal organization.

This daemon exposes cognitive services through a typed IDL interface,
allowing an LLM sidecar to provide natural language access.
"""

import json
import math
import time
import socket
import threading
import logging
from dataclasses import dataclass, field, asdict
from typing import Dict, List, Optional, Any, Callable
from enum import Enum
from pathlib import Path
import os

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('time_crystal_daemon')


# =============================================================================
# CORE DATA TYPES (from IDL)
# =============================================================================

@dataclass
class TruthValue:
    """Represents uncertainty and confidence."""
    strength: float = 1.0
    confidence: float = 1.0


@dataclass
class AttentionValue:
    """Represents importance and focus."""
    sti: int = 0   # Short-term importance
    lti: int = 0   # Long-term importance
    vlti: int = 0  # Very long-term importance


class AtomType(Enum):
    """Atom type enumeration."""
    NODE = 1
    CONCEPT_NODE = 2
    PREDICATE_NODE = 3
    SCHEMA_NODE = 4
    VARIABLE_NODE = 5
    LINK = 100
    INHERITANCE_LINK = 101
    SIMILARITY_LINK = 102
    EVALUATION_LINK = 103
    EXECUTION_LINK = 104
    LIST_LINK = 105
    AND_LINK = 106
    OR_LINK = 107
    NOT_LINK = 108
    IMPLICATION_LINK = 109


@dataclass
class Atom:
    """Core cognitive primitive."""
    handle: int
    atom_type: AtomType
    name: Optional[str] = None
    outgoing: List[int] = field(default_factory=list)
    tv: TruthValue = field(default_factory=TruthValue)
    av: AttentionValue = field(default_factory=AttentionValue)
    tc_level: int = 0  # Time crystal level assignment
    metadata: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict:
        return {
            'handle': self.handle,
            'type': self.atom_type.name,
            'name': self.name,
            'outgoing': self.outgoing,
            'tv': asdict(self.tv),
            'av': asdict(self.av),
            'tc_level': self.tc_level
        }


# =============================================================================
# TIME CRYSTAL HIERARCHY (from time-crystal-neuron skill)
# =============================================================================

@dataclass
class TimeCrystalLevel:
    """A single level in the time crystal hierarchy."""
    id: int
    name: str
    period_ms: float
    current_phase: float = 0.0
    atom_count: int = 0

    def to_dict(self) -> Dict:
        return {
            'id': self.id,
            'name': self.name,
            'period_ms': self.period_ms,
            'current_phase': self.current_phase,
            'atom_count': self.atom_count
        }


# Time crystal levels based on Nanobrain Fig 7.15 (whole brain model)
TC_LEVELS = [
    TimeCrystalLevel(0, "quantum_resonance", 0.001),      # 1μs - Quantum effects
    TimeCrystalLevel(1, "protein_dynamics", 8.0),         # 8ms - Protein channels
    TimeCrystalLevel(2, "ion_channel_gating", 26.0),      # 26ms - Ion channels
    TimeCrystalLevel(3, "membrane_dynamics", 52.0),       # 52ms - Membrane
    TimeCrystalLevel(4, "axon_initial_segment", 110.0),   # 110ms - AIS
    TimeCrystalLevel(5, "dendritic_integration", 160.0),  # 160ms - Dendrites
    TimeCrystalLevel(6, "synaptic_plasticity", 250.0),    # 250ms - Synapses
    TimeCrystalLevel(7, "soma_processing", 330.0),        # 330ms - Soma
    TimeCrystalLevel(8, "network_synchronization", 500.0),# 500ms - Network
    TimeCrystalLevel(9, "global_rhythm", 1000.0),         # 1s - Global
    TimeCrystalLevel(10, "circadian_modulation", 60000.0),# 1min - Circadian
    TimeCrystalLevel(11, "homeostatic_regulation", 3600000.0) # 1hr - Homeostatic
]


class TimeCrystalHierarchy:
    """Manages the hierarchical time crystal structure."""

    def __init__(self):
        self.levels = {level.id: level for level in TC_LEVELS}
        self.start_time = time.time()
        self.event_callbacks: List[Callable] = []

    def update(self) -> List[Dict]:
        """Update all oscillator phases and emit phase transition events."""
        events = []
        current_time = (time.time() - self.start_time) * 1000  # ms

        for level in self.levels.values():
            old_phase = level.current_phase
            # Calculate new phase (0.0 to 1.0)
            new_phase = (current_time % level.period_ms) / level.period_ms

            # Detect phase transitions (crossing 0)
            if new_phase < old_phase:
                event = {
                    'event': 'tc_phase_transition',
                    'data': {
                        'level': level.id,
                        'old_phase': old_phase,
                        'new_phase': new_phase
                    }
                }
                events.append(event)
                self._emit_event(event)

            level.current_phase = new_phase

        return events

    def get_level(self, level_id: int) -> Optional[TimeCrystalLevel]:
        return self.levels.get(level_id)

    def get_hierarchy(self) -> List[Dict]:
        return [level.to_dict() for level in self.levels.values()]

    def set_phase(self, level_id: int, phase: float) -> bool:
        """Manually set the phase of a level (engineer mode)."""
        if level_id not in self.levels:
            return False
        if not 0.0 <= phase <= 1.0:
            return False
        self.levels[level_id].current_phase = phase
        return True

    def register_callback(self, callback: Callable):
        self.event_callbacks.append(callback)

    def _emit_event(self, event: Dict):
        for callback in self.event_callbacks:
            try:
                callback(event)
            except Exception as e:
                logger.error(f"Event callback error: {e}")


# =============================================================================
# ATOMSPACE (from opencog-inferno-kernel)
# =============================================================================

class AtomSpace:
    """Kernel-level hypergraph knowledge base."""

    def __init__(self):
        self.atoms: Dict[int, Atom] = {}
        self.next_handle: int = 1
        self.name_index: Dict[str, int] = {}
        self.type_index: Dict[AtomType, List[int]] = {}
        self.incoming_index: Dict[int, List[int]] = {}
        self.version: int = 0

    def create_atom(
        self,
        atom_type: AtomType,
        name: Optional[str] = None,
        outgoing: Optional[List[int]] = None,
        tv: Optional[TruthValue] = None,
        tc_level: int = 0
    ) -> int:
        """Create a new atom and return its handle."""
        handle = self.next_handle
        self.next_handle += 1

        atom = Atom(
            handle=handle,
            atom_type=atom_type,
            name=name,
            outgoing=outgoing or [],
            tv=tv or TruthValue(),
            tc_level=tc_level
        )

        self.atoms[handle] = atom
        self.version += 1

        # Update indices
        if name:
            self.name_index[name] = handle

        if atom_type not in self.type_index:
            self.type_index[atom_type] = []
        self.type_index[atom_type].append(handle)

        # Update incoming index for links
        for out_handle in atom.outgoing:
            if out_handle not in self.incoming_index:
                self.incoming_index[out_handle] = []
            self.incoming_index[out_handle].append(handle)

        return handle

    def get_atom(self, handle: int) -> Optional[Atom]:
        return self.atoms.get(handle)

    def delete_atom(self, handle: int) -> bool:
        if handle not in self.atoms:
            return False

        atom = self.atoms[handle]

        # Remove from indices
        if atom.name and atom.name in self.name_index:
            del self.name_index[atom.name]

        if atom.atom_type in self.type_index:
            self.type_index[atom.atom_type].remove(handle)

        for out_handle in atom.outgoing:
            if out_handle in self.incoming_index:
                self.incoming_index[out_handle].remove(handle)

        del self.atoms[handle]
        self.version += 1
        return True

    def set_tv(self, handle: int, tv: TruthValue) -> bool:
        atom = self.atoms.get(handle)
        if not atom:
            return False
        atom.tv = tv
        self.version += 1
        return True

    def set_av(self, handle: int, av: AttentionValue) -> bool:
        atom = self.atoms.get(handle)
        if not atom:
            return False
        atom.av = av
        self.version += 1
        return True

    def get_by_name(self, name: str) -> Optional[int]:
        return self.name_index.get(name)

    def get_by_type(self, atom_type: AtomType) -> List[int]:
        return self.type_index.get(atom_type, [])

    def get_incoming(self, handle: int) -> List[int]:
        return self.incoming_index.get(handle, [])

    def get_atom_count(self) -> int:
        return len(self.atoms)

    def get_total_attention(self) -> int:
        return sum(atom.av.sti for atom in self.atoms.values())


# =============================================================================
# COGNITIVE MODULES
# =============================================================================

class ModuleStatus(Enum):
    RUNNING = "running"
    PAUSED = "paused"
    ERROR = "error"


@dataclass
class CognitiveModule:
    """A cognitive processing module."""
    id: str
    name: str
    status: ModuleStatus = ModuleStatus.RUNNING
    tc_levels: List[int] = field(default_factory=list)
    atoms_owned: int = 0
    attention_usage: float = 0.0

    def to_dict(self) -> Dict:
        return {
            'id': self.id,
            'name': self.name,
            'status': self.status.value,
            'tc_levels': self.tc_levels,
            'atoms_owned': self.atoms_owned,
            'attention_usage': self.attention_usage
        }


# =============================================================================
# TIME CRYSTAL DAEMON
# =============================================================================

class TimeCrystalDaemon:
    """The main daemon process."""

    def __init__(self, socket_path: str = "/tmp/tc_daemon.sock"):
        self.socket_path = socket_path
        self.atomspace = AtomSpace()
        self.tc_hierarchy = TimeCrystalHierarchy()
        self.modules: Dict[str, CognitiveModule] = {}
        self.running = False
        self.start_time = time.time()
        self.event_queue: List[Dict] = []
        self.decisions: Dict[str, Dict] = {}

        # Register event callback
        self.tc_hierarchy.register_callback(self._handle_tc_event)

        # Initialize default modules
        self._init_modules()

    def _init_modules(self):
        """Initialize the default cognitive modules."""
        default_modules = [
            CognitiveModule("pln", "Probabilistic Logic Networks", tc_levels=[5, 6, 7]),
            CognitiveModule("moses", "Meta-Optimizing Semantic Evolutionary Search", tc_levels=[8, 9]),
            CognitiveModule("attention", "Attention Allocation", tc_levels=[3, 4, 5]),
            CognitiveModule("pattern", "Pattern Matching", tc_levels=[2, 3, 4]),
            CognitiveModule("spacetime", "SpaceTime Server", tc_levels=[0, 1, 2]),
        ]
        for module in default_modules:
            self.modules[module.id] = module

    def _handle_tc_event(self, event: Dict):
        """Handle time crystal events."""
        self.event_queue.append(event)

    # =========================================================================
    # IDL COMMAND HANDLERS
    # =========================================================================

    def get_status(self) -> Dict:
        """Get the overall status of the daemon."""
        return {
            'status': 'running' if self.running else 'paused',
            'uptime_seconds': int(time.time() - self.start_time),
            'atom_count': self.atomspace.get_atom_count(),
            'total_attention': self.atomspace.get_total_attention(),
            'active_modules': [m.id for m in self.modules.values() if m.status == ModuleStatus.RUNNING],
            'tc_state': {
                'current_level': 9,  # Global rhythm
                'global_phase': self.tc_hierarchy.levels[9].current_phase
            }
        }

    def list_modules(self) -> List[Dict]:
        """List all cognitive modules."""
        return [m.to_dict() for m in self.modules.values()]

    def get_module(self, module_id: str) -> Optional[Dict]:
        """Get details of a specific module."""
        module = self.modules.get(module_id)
        return module.to_dict() if module else None

    def trace_atom(self, handle: int, depth: int = 3) -> Optional[Dict]:
        """Trace an atom's provenance and relationships."""
        atom = self.atomspace.get_atom(handle)
        if not atom:
            return None

        incoming = [self.atomspace.get_atom(h).to_dict() 
                   for h in self.atomspace.get_incoming(handle)[:depth]]
        outgoing = [self.atomspace.get_atom(h).to_dict() 
                   for h in atom.outgoing[:depth] if self.atomspace.get_atom(h)]

        return {
            'atom': atom.to_dict(),
            'incoming': incoming,
            'outgoing': outgoing,
            'provenance': [f"Created at TC level {atom.tc_level}"]
        }

    def diagnose(self, scope: str = "all", target: Optional[str] = None) -> Dict:
        """Run diagnostics and identify anomalies."""
        anomalies = []
        metrics = {}

        # Check attention distribution
        total_attention = self.atomspace.get_total_attention()
        for module in self.modules.values():
            if module.attention_usage > 0.8:
                anomalies.append({
                    'severity': 'medium',
                    'module': module.id,
                    'description': f"High attention usage: {module.attention_usage:.1%}",
                    'tc_level': module.tc_levels[0] if module.tc_levels else 0
                })

        metrics['total_atoms'] = self.atomspace.get_atom_count()
        metrics['total_attention'] = total_attention
        metrics['active_modules'] = len([m for m in self.modules.values() 
                                        if m.status == ModuleStatus.RUNNING])

        recommendations = []
        if anomalies:
            recommendations.append("Consider pausing high-usage modules for inspection")

        return {
            'anomalies': anomalies,
            'metrics': metrics,
            'recommendations': recommendations
        }

    def get_tc_hierarchy(self) -> Dict:
        """Get the current state of the time crystal hierarchy."""
        self.tc_hierarchy.update()
        return {'levels': self.tc_hierarchy.get_hierarchy()}

    def explain_decision(self, decision_id: str) -> Optional[Dict]:
        """Explain why the daemon made a specific decision."""
        decision = self.decisions.get(decision_id)
        if not decision:
            return {
                'thought': "No decision found with that ID",
                'constraints': [],
                'alternatives': [],
                'confidence': 0.0
            }
        return decision

    def set_attention(self, handle: int, av: Dict) -> Dict:
        """Set the attention value of an atom (engineer mode)."""
        atom = self.atomspace.get_atom(handle)
        if not atom:
            return {'success': False, 'error': 'Atom not found'}

        previous_av = asdict(atom.av)
        new_av = AttentionValue(
            sti=av.get('sti', 0),
            lti=av.get('lti', 0),
            vlti=av.get('vlti', 0)
        )
        self.atomspace.set_av(handle, new_av)

        return {'success': True, 'previous_av': previous_av}

    def pause_module(self, module_id: str) -> Dict:
        """Pause a cognitive module (engineer mode)."""
        module = self.modules.get(module_id)
        if not module:
            return {'success': False, 'error': 'Module not found'}

        module.status = ModuleStatus.PAUSED
        return {'success': True}

    def resume_module(self, module_id: str) -> Dict:
        """Resume a paused cognitive module (engineer mode)."""
        module = self.modules.get(module_id)
        if not module:
            return {'success': False, 'error': 'Module not found'}

        module.status = ModuleStatus.RUNNING
        return {'success': True}

    def inject_atom(
        self,
        atom_type: str,
        name: Optional[str] = None,
        outgoing: Optional[List[int]] = None,
        tv: Optional[Dict] = None,
        tc_level: int = 0
    ) -> Dict:
        """Inject a new atom into the AtomSpace (engineer mode)."""
        try:
            atype = AtomType[atom_type]
        except KeyError:
            return {'error': f'Invalid atom type: {atom_type}'}

        truth_value = TruthValue(
            strength=tv.get('strength', 1.0) if tv else 1.0,
            confidence=tv.get('confidence', 1.0) if tv else 1.0
        )

        handle = self.atomspace.create_atom(
            atom_type=atype,
            name=name,
            outgoing=outgoing,
            tv=truth_value,
            tc_level=tc_level
        )

        return {'handle': handle}

    def set_tc_phase(self, level: int, phase: float) -> Dict:
        """Manually set the phase of a time crystal level (engineer mode)."""
        success = self.tc_hierarchy.set_phase(level, phase)
        return {'success': success}

    # =========================================================================
    # RPC SERVER
    # =========================================================================

    def handle_request(self, request: Dict) -> Dict:
        """Handle a JSON-RPC request."""
        method = request.get('method')
        params = request.get('params', {})
        request_id = request.get('id')

        handlers = {
            'get_status': lambda: self.get_status(),
            'list_modules': lambda: self.list_modules(),
            'get_module': lambda: self.get_module(params.get('module_id')),
            'trace_atom': lambda: self.trace_atom(
                params.get('handle'), params.get('depth', 3)),
            'diagnose': lambda: self.diagnose(
                params.get('scope', 'all'), params.get('target')),
            'get_tc_hierarchy': lambda: self.get_tc_hierarchy(),
            'explain_decision': lambda: self.explain_decision(
                params.get('decision_id')),
            'set_attention': lambda: self.set_attention(
                params.get('handle'), params.get('av', {})),
            'pause_module': lambda: self.pause_module(params.get('module_id')),
            'resume_module': lambda: self.resume_module(params.get('module_id')),
            'inject_atom': lambda: self.inject_atom(
                params.get('type'),
                params.get('name'),
                params.get('outgoing'),
                params.get('tv'),
                params.get('tc_level', 0)
            ),
            'set_tc_phase': lambda: self.set_tc_phase(
                params.get('level'), params.get('phase')),
        }

        handler = handlers.get(method)
        if not handler:
            return {
                'jsonrpc': '2.0',
                'id': request_id,
                'error': {'code': -32601, 'message': f'Method not found: {method}'}
            }

        try:
            result = handler()
            return {
                'jsonrpc': '2.0',
                'id': request_id,
                'result': result
            }
        except Exception as e:
            logger.error(f"Error handling {method}: {e}")
            return {
                'jsonrpc': '2.0',
                'id': request_id,
                'error': {'code': 2001, 'message': str(e)}
            }

    def start(self):
        """Start the daemon and listen for connections."""
        self.running = True

        # Remove existing socket
        if os.path.exists(self.socket_path):
            os.unlink(self.socket_path)

        # Create Unix domain socket
        server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        server.bind(self.socket_path)
        server.listen(5)
        server.settimeout(1.0)

        logger.info(f"Time Crystal Daemon started on {self.socket_path}")

        # Start time crystal update thread
        tc_thread = threading.Thread(target=self._tc_update_loop, daemon=True)
        tc_thread.start()

        try:
            while self.running:
                try:
                    conn, _ = server.accept()
                    threading.Thread(
                        target=self._handle_connection,
                        args=(conn,),
                        daemon=True
                    ).start()
                except socket.timeout:
                    continue
        except KeyboardInterrupt:
            logger.info("Shutting down...")
        finally:
            server.close()
            if os.path.exists(self.socket_path):
                os.unlink(self.socket_path)

    def _handle_connection(self, conn: socket.socket):
        """Handle a client connection."""
        try:
            data = b''
            while True:
                chunk = conn.recv(4096)
                if not chunk:
                    break
                data += chunk
                if b'\n' in data:
                    break

            if data:
                request = json.loads(data.decode('utf-8'))
                response = self.handle_request(request)
                conn.sendall(json.dumps(response).encode('utf-8') + b'\n')
        except Exception as e:
            logger.error(f"Connection error: {e}")
        finally:
            conn.close()

    def _tc_update_loop(self):
        """Background thread to update time crystal phases."""
        while self.running:
            self.tc_hierarchy.update()
            time.sleep(0.001)  # 1ms resolution

    def stop(self):
        """Stop the daemon."""
        self.running = False


# =============================================================================
# MAIN
# =============================================================================

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Time Crystal Daemon")
    parser.add_argument(
        "--socket", "-s",
        default="/tmp/tc_daemon.sock",
        help="Unix socket path"
    )
    args = parser.parse_args()

    daemon = TimeCrystalDaemon(socket_path=args.socket)
    daemon.start()
