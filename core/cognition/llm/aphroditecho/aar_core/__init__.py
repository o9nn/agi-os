__version__ = '0.1.0'
__author__ = 'EchoCog Deep Tree Echo Team'
from .orchestration.core_orchestrator import AARCoreOrchestrator
from .agents.agent_manager import AgentManager
from .arena.simulation_engine import SimulationEngine
from .relations.relation_graph import RelationGraph
from .embodied import VirtualBody, EmbodiedAgent, ProprioceptiveSystem
__all__ = ['AARCoreOrchestrator', 'AgentManager', 'SimulationEngine', 'RelationGraph', 'VirtualBody', 'EmbodiedAgent', 'ProprioceptiveSystem']
DEFAULT_CONFIG = {'max_concurrent_agents': 1000, 'arena_simulation_enabled': True, 'relation_graph_depth': 3, 'resource_allocation_strategy': 'adaptive'}
def get_default_config():
    return DEFAULT_CONFIG.copy()