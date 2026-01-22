try:
    from .topology_individual import NeuralTopologyIndividual
except ImportError:
    from topology_individual import NeuralTopologyIndividual
__all__ = ['NeuralTopologyIndividual']