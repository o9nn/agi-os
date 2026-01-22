__version__ = '1.0.0'
__author__ = 'Echo-Self Evolution Team'
try:
    from .core.evolution_engine import EchoSelfEvolutionEngine
    from .core.interfaces import Individual, Population, FitnessEvaluator
    from .core.operators import MutationOperator, SelectionOperator, CrossoverOperator
    from .neural.topology_individual import NeuralTopologyIndividual
    from .integration.dtesn_bridge import DTESNBridge
    from .integration.aphrodite_bridge import AphroditeBridge
except ImportError:
    from core.evolution_engine import EchoSelfEvolutionEngine
    from core.interfaces import Individual, Population, FitnessEvaluator
    from core.operators import MutationOperator, SelectionOperator, CrossoverOperator
    from neural.topology_individual import NeuralTopologyIndividual
    from integration.dtesn_bridge import DTESNBridge
    from integration.aphrodite_bridge import AphroditeBridge
__all__ = ['EchoSelfEvolutionEngine', 'Individual', 'Population', 'FitnessEvaluator', 'MutationOperator', 'SelectionOperator', 'CrossoverOperator', 'NeuralTopologyIndividual', 'DTESNBridge', 'AphroditeBridge']