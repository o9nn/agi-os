"""
Component Integration Adapters for 2do Components

This package provides adapters that integrate the various 2do components
with the Aphrodite Engine through the AAR Gateway.
"""

__version__ = "1.0.0"

from .llm_adapter import LLMAdapter
from .aichat_adapter import AiChatAdapter
from .galatea_adapter import GalateaAdapter
from .spark_adapter import SparkAdapter
from .argc_adapter import ArgcAdapter
from .llm_functions_adapter import LLMFunctionsAdapter
from .paphos_adapter import PaphosAdapter

__all__ = [
    "LLMAdapter",
    "AiChatAdapter", 
    "GalateaAdapter",
    "SparkAdapter",
    "ArgcAdapter",
    "LLMFunctionsAdapter",
    "PaphosAdapter",
]