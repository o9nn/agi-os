"""
Echo Propagation Engine
Part of Deep Tree Echo Core

This package implements the activation spreading, pattern recognition,
and feedback loop mechanisms for the Deep Tree Echo hypergraph.
"""

from .activation_engine import (
    ActivationEngine,
    ActivationMode,
    ActivationState
)

from .pattern_matcher import (
    PatternMatcher,
    Pattern
)

from .feedback_loops import (
    FeedbackLoopManager,
    FeedbackLoop,
    FeedbackType
)

__all__ = [
    # Activation Engine
    'ActivationEngine',
    'ActivationMode',
    'ActivationState',
    
    # Pattern Matcher
    'PatternMatcher',
    'Pattern',
    
    # Feedback Loops
    'FeedbackLoopManager',
    'FeedbackLoop',
    'FeedbackType',
]

__version__ = '0.1.0'
__author__ = 'Deep Tree Echo Team'
