#!/usr/bin/env python3
"""
Self-Weaving Cognitive Daemon

This module implements the complete composed skill expression:
    /time-crystal-daemon ( (/o9c -> /topology-weaver [ /opencog-inferno-kernel | /time-crystal-neuron ]) )

The pipeline:
1. topology-weaver: Extracts terminology from opencog-inferno-kernel and time-crystal-neuron,
   generates a contextually-tagged neural topology
2. o9c: Applies recursive self-improvement to the topology, converging to a fixed point
3. time-crystal-daemon: Executes the evolved topology with hierarchical temporal organization

The result is a self-weaving cognitive fabric that dynamically generates and refines
its own neural architecture.
"""

import json
import os
import sys
import time
import socket
import threading
import logging
from dataclasses import dataclass, field, asdict
from typing import Dict, List, Optional, Any, Callable
from enum import Enum

# Add parent directories to path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from o9c.o9c_kernel import (
    O9CKernel, NeuralTopology, TopologyComponent as O9CComponent,
    load_topology, save_topology
)
from topology_weaver.weaver import (
    TopologyWeaver, GeneratedTopology, topology_to_dict
)

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger('self_weaving_daemon')


# =============================================================================
# TIME CRYSTAL HIERARCHY
# =============================================================================

TC_LEVELS = [
    {'id': 0, 'name': 'quantum_resonance', 'period_ms': 0.001},
    {'id': 1, 'name': 'protein_dynamics', 'period_ms': 8.0},
    {'id': 2, 'name': 'ion_channel_gating', 'period_ms': 26.0},
    {'id': 3, 'name': 'membrane_dynamics', 'period_ms': 52.0},
    {'id': 4, 'name': 'axon_initial_segment', 'period_ms': 110.0},
    {'id': 5, 'name': 'dendritic_integration', 'period_ms': 160.0},
    {'id': 6, 'name': 'synaptic_plasticity', 'period_ms': 250.0},
    {'id': 7, 'name': 'soma_processing', 'period_ms': 330.0},
    {'id': 8, 'name': 'network_synchronization', 'period_ms': 500.0},
    {'id': 9, 'name': 'global_rhythm', 'period_ms': 1000.0},
    {'id': 10, 'name': 'circadian_modulation', 'period_ms': 60000.0},
    {'id': 11, 'name': 'homeostatic_regulation', 'period_ms': 3600000.0},
]


# =============================================================================
# DYNAMIC TOPOLOGY EXECUTOR
# =============================================================================

@dataclass
class ExecutionState:
    """State of a component during execution."""
    component_id: str
    activation: float = 0.0
    last_executed: float = 0.0
    execution_count: int = 0
    accumulated_input: float = 0.0


class DynamicTopologyExecutor:
    """
    Executes a neural topology dynamically, using the time crystal hierarchy
    to orchestrate component activation.
    """

    def __init__(self, topology: NeuralTopology):
        self.topology = topology
        self.states: Dict[str, ExecutionState] = {}
        self.start_time = time.time()
        self.running = False
        
        # Initialize states for all components
        for comp_id in topology.components:
            self.states[comp_id] = ExecutionState(component_id=comp_id)
        
        logger.info(f"Initialized executor with {len(topology.components)} components")

    def get_current_phases(self) -> Dict[int, float]:
        """Get current phase (0-1) for each time crystal level."""
        elapsed_ms = (time.time() - self.start_time) * 1000
        phases = {}
        for level in TC_LEVELS:
            phase = (elapsed_ms / level['period_ms']) % 1.0
            phases[level['id']] = phase
        return phases

    def should_execute(self, comp: O9CComponent, phases: Dict[int, float]) -> bool:
        """
        Determine if a component should execute based on its TC level
        and the current phase.
        """
        if comp.tc_level is None:
            return True  # Components without TC level always execute
        
        phase = phases.get(comp.tc_level, 0.0)
        # Execute when phase crosses 0.5 (rising edge)
        state = self.states[comp.id]
        last_phase = state.activation  # Reuse activation to store last phase
        
        if last_phase < 0.5 <= phase:
            return True
        
        state.activation = phase
        return False

    def execute_step(self) -> Dict[str, Any]:
        """
        Execute one step of the topology, activating components based on
        their time crystal levels.
        """
        phases = self.get_current_phases()
        executed = []
        
        for comp_id, comp in self.topology.components.items():
            if self.should_execute(comp, phases):
                # Simulate execution
                state = self.states[comp_id]
                state.last_executed = time.time()
                state.execution_count += 1
                executed.append(comp_id)
        
        return {
            'executed_components': executed,
            'phases': phases,
            'timestamp': time.time()
        }

    def get_status(self) -> Dict[str, Any]:
        """Get current execution status."""
        phases = self.get_current_phases()
        return {
            'running': self.running,
            'uptime_seconds': time.time() - self.start_time,
            'component_count': len(self.topology.components),
            'connection_count': len(self.topology.connections),
            'phases': phases,
            'execution_stats': {
                comp_id: {
                    'execution_count': state.execution_count,
                    'last_executed': state.last_executed
                }
                for comp_id, state in self.states.items()
            }
        }


# =============================================================================
# SELF-WEAVING DAEMON
# =============================================================================

class SelfWeavingDaemon:
    """
    The complete self-weaving cognitive daemon.
    
    Implements the composition:
        /time-crystal-daemon ( (/o9c -> /topology-weaver [ /opencog-inferno-kernel | /time-crystal-neuron ]) )
    """

    def __init__(self, socket_path: str = "/tmp/self_weaving_daemon.sock"):
        self.socket_path = socket_path
        self.running = False
        self.executor: Optional[DynamicTopologyExecutor] = None
        self.topology: Optional[NeuralTopology] = None
        self.weave_history: List[Dict] = []
        
        # Initialize components
        self.weaver = TopologyWeaver()
        self.o9c_kernel = O9CKernel(max_iterations=7)

    def weave_and_evolve(self) -> NeuralTopology:
        """
        Execute the full weaving and evolution pipeline:
        1. topology-weaver generates initial topology
        2. o9c evolves it to a fixed point
        """
        logger.info("Stage 1: Weaving topology from source skills...")
        
        # Stage 1: Weave topology
        woven = self.weaver.weave(name="self_woven_cognitive_topology")
        woven_dict = topology_to_dict(woven)
        
        logger.info(f"  Woven {len(woven.components)} components, {len(woven.connections)} connections")
        
        # Convert to o9c format
        o9c_topology = self._convert_to_o9c_topology(woven)
        
        # Record weaving
        self.weave_history.append({
            'stage': 'weaving',
            'timestamp': time.time(),
            'component_count': len(woven.components),
            'connection_count': len(woven.connections)
        })
        
        logger.info("Stage 2: Evolving topology with o9c kernel...")
        
        # Stage 2: Evolve with o9c
        evolved = self.o9c_kernel.transform(o9c_topology)
        
        logger.info(f"  Evolved to {len(evolved.components)} components, {len(evolved.connections)} connections")
        logger.info(f"  o9c iterations: {evolved.metadata.get('o9c_iterations', 0)}")
        
        # Record evolution
        self.weave_history.append({
            'stage': 'evolution',
            'timestamp': time.time(),
            'component_count': len(evolved.components),
            'connection_count': len(evolved.connections),
            'o9c_iterations': evolved.metadata.get('o9c_iterations', 0),
            'gauge_group': evolved.metadata.get('gauge_group', '')
        })
        
        return evolved

    def _convert_to_o9c_topology(self, woven: GeneratedTopology) -> NeuralTopology:
        """Convert woven topology to o9c format."""
        components = {}
        for comp in woven.components.values():
            components[comp.id] = O9CComponent(
                id=comp.id,
                type=comp.type,
                tags=comp.tags,
                params=comp.params,
                inputs=comp.inputs,
                outputs=comp.outputs,
                tc_level=comp.tc_level
            )
        
        return NeuralTopology(
            name=woven.name,
            components=components,
            connections=woven.connections,
            metadata=woven.metadata
        )

    def start(self):
        """Start the self-weaving daemon."""
        logger.info("Starting Self-Weaving Cognitive Daemon...")
        
        # Weave and evolve topology
        self.topology = self.weave_and_evolve()
        
        # Initialize executor
        self.executor = DynamicTopologyExecutor(self.topology)
        self.executor.running = True
        
        # Start execution loop
        self.running = True
        self._start_execution_loop()
        
        # Start RPC server
        self._start_rpc_server()

    def _start_execution_loop(self):
        """Start the background execution loop."""
        def loop():
            while self.running:
                if self.executor:
                    self.executor.execute_step()
                time.sleep(0.01)  # 10ms tick
        
        thread = threading.Thread(target=loop, daemon=True)
        thread.start()
        logger.info("Execution loop started")

    def _start_rpc_server(self):
        """Start the JSON-RPC server."""
        # Remove existing socket
        if os.path.exists(self.socket_path):
            os.remove(self.socket_path)
        
        server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        server.bind(self.socket_path)
        server.listen(5)
        
        logger.info(f"RPC server listening on {self.socket_path}")
        
        try:
            while self.running:
                conn, _ = server.accept()
                threading.Thread(
                    target=self._handle_connection,
                    args=(conn,),
                    daemon=True
                ).start()
        except KeyboardInterrupt:
            logger.info("Shutting down...")
        finally:
            server.close()
            if os.path.exists(self.socket_path):
                os.remove(self.socket_path)

    def _handle_connection(self, conn: socket.socket):
        """Handle an incoming RPC connection."""
        try:
            data = conn.recv(65536).decode('utf-8')
            request = json.loads(data)
            
            method = request.get('method', '')
            params = request.get('params', {})
            
            result = self._dispatch_method(method, params)
            
            response = {
                'jsonrpc': '2.0',
                'id': request.get('id', 1),
                'result': result
            }
            
            conn.sendall(json.dumps(response).encode('utf-8') + b'\n')
        except Exception as e:
            error_response = {
                'jsonrpc': '2.0',
                'id': 1,
                'error': {'code': -32000, 'message': str(e)}
            }
            conn.sendall(json.dumps(error_response).encode('utf-8') + b'\n')
        finally:
            conn.close()

    def _dispatch_method(self, method: str, params: Dict) -> Any:
        """Dispatch an RPC method call."""
        handlers = {
            'get_status': self._get_status,
            'get_topology': self._get_topology,
            'get_weave_history': self._get_weave_history,
            'get_tc_hierarchy': self._get_tc_hierarchy,
            'get_component': self._get_component,
            'list_components': self._list_components,
            'reweave': self._reweave,
        }
        
        handler = handlers.get(method)
        if handler:
            return handler(params)
        else:
            raise ValueError(f"Unknown method: {method}")

    def _get_status(self, params: Dict) -> Dict:
        """Get daemon status."""
        executor_status = self.executor.get_status() if self.executor else {}
        return {
            'daemon': 'self_weaving_cognitive_daemon',
            'running': self.running,
            'topology_name': self.topology.name if self.topology else None,
            'component_count': len(self.topology.components) if self.topology else 0,
            'weave_count': len(self.weave_history),
            'executor': executor_status
        }

    def _get_topology(self, params: Dict) -> Dict:
        """Get the current topology."""
        if not self.topology:
            return {}
        
        return {
            'name': self.topology.name,
            'version': self.topology.version,
            'metadata': self.topology.metadata,
            'component_count': len(self.topology.components),
            'connection_count': len(self.topology.connections)
        }

    def _get_weave_history(self, params: Dict) -> List[Dict]:
        """Get the weaving history."""
        return self.weave_history

    def _get_tc_hierarchy(self, params: Dict) -> List[Dict]:
        """Get the time crystal hierarchy."""
        phases = self.executor.get_current_phases() if self.executor else {}
        return [
            {**level, 'current_phase': phases.get(level['id'], 0.0)}
            for level in TC_LEVELS
        ]

    def _get_component(self, params: Dict) -> Dict:
        """Get a specific component."""
        comp_id = params.get('component_id')
        if not comp_id or not self.topology:
            return {}
        
        comp = self.topology.components.get(comp_id)
        if not comp:
            return {}
        
        state = self.executor.states.get(comp_id) if self.executor else None
        
        return {
            'id': comp.id,
            'type': comp.type,
            'tags': comp.tags,
            'params': comp.params,
            'tc_level': comp.tc_level,
            'execution_count': state.execution_count if state else 0,
            'last_executed': state.last_executed if state else 0
        }

    def _list_components(self, params: Dict) -> List[Dict]:
        """List all components."""
        if not self.topology:
            return []
        
        return [
            {
                'id': comp.id,
                'type': comp.type,
                'tags': comp.tags[:3],
                'tc_level': comp.tc_level
            }
            for comp in self.topology.components.values()
        ]

    def _reweave(self, params: Dict) -> Dict:
        """Trigger a re-weaving of the topology."""
        logger.info("Re-weaving topology...")
        
        # Re-weave and evolve
        self.topology = self.weave_and_evolve()
        
        # Reinitialize executor
        self.executor = DynamicTopologyExecutor(self.topology)
        self.executor.running = True
        
        return {
            'success': True,
            'new_component_count': len(self.topology.components),
            'new_connection_count': len(self.topology.connections),
            'weave_count': len(self.weave_history)
        }


# =============================================================================
# MAIN
# =============================================================================

def main():
    import argparse
    
    parser = argparse.ArgumentParser(description="Self-Weaving Cognitive Daemon")
    parser.add_argument("--socket", "-s", default="/tmp/self_weaving_daemon.sock",
                       help="Unix socket path")
    parser.add_argument("--save-topology", help="Save evolved topology to file")
    args = parser.parse_args()
    
    daemon = SelfWeavingDaemon(socket_path=args.socket)
    
    if args.save_topology:
        # Just weave and save, don't start daemon
        topology = daemon.weave_and_evolve()
        save_topology(topology, args.save_topology)
        print(f"Saved evolved topology to {args.save_topology}")
    else:
        # Start the full daemon
        daemon.start()


if __name__ == "__main__":
    main()
