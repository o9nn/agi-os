__version__ = '0.1.0'
__author__ = 'EchoCog Deep Tree Echo Team'
from .core.evolution_engine import EchoSelfEvolutionEngine
__all__ = ['EchoSelfEvolutionEngine']
INTEGRATION_STATUS = {'aphrodite_engine': False, 'dtesn_kernel': False, 'aar_orchestrator': False}
def get_integration_status():
    return INTEGRATION_STATUS.copy()
def enable_integration(component: str, enabled: bool=True):
    if component in INTEGRATION_STATUS:
        INTEGRATION_STATUS[component] = enabled
        return True
    return False