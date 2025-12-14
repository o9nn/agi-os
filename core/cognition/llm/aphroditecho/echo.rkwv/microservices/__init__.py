"""
Deep Tree Echo Microservices Package

Scalable microservices for the Deep Tree Echo architecture.
Implements horizontal scaling, load balancing, and resource optimization.
"""

__version__ = "1.0.0"
__author__ = "Deep Tree Echo Team"

from .load_balancer import LoadBalancerService
from .cache_service import CacheService  
from .cognitive_service import CognitiveService

__all__ = [
    'LoadBalancerService',
    'CacheService', 
    'CognitiveService'
]