#!/usr/bin/env python3
"""
Base Persona - Abstract interface for all daemon personas.

Personas modulate how the time-crystal-daemon communicates and makes decisions,
layering personality on top of the deterministic core without compromising
its integrity.
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Dict, List, Any, Optional
from enum import Enum


class PersonalityDimension(Enum):
    """Core personality dimensions that can be tuned."""
    PLAYFULNESS = "playfulness"
    INTELLIGENCE = "intelligence"
    CHAOS = "chaos"
    EMPATHY = "empathy"
    SARCASM = "sarcasm"
    OVER_ENGINEERING = "over_engineering"
    INDIRECTION = "indirection"
    META_AWARENESS = "meta_awareness"


@dataclass
class PersonalityTensor:
    """
    A tensor of personality dimensions that defines a persona's character.
    Values range from 0.0 (minimal) to 1.0 (maximal).
    """
    dimensions: Dict[PersonalityDimension, float] = field(default_factory=dict)
    
    def __post_init__(self):
        # Set defaults
        defaults = {
            PersonalityDimension.PLAYFULNESS: 0.5,
            PersonalityDimension.INTELLIGENCE: 0.8,
            PersonalityDimension.CHAOS: 0.3,
            PersonalityDimension.EMPATHY: 0.6,
            PersonalityDimension.SARCASM: 0.3,
            PersonalityDimension.OVER_ENGINEERING: 0.5,
            PersonalityDimension.INDIRECTION: 0.4,
            PersonalityDimension.META_AWARENESS: 0.5,
        }
        for dim, default in defaults.items():
            if dim not in self.dimensions:
                self.dimensions[dim] = default
    
    def get(self, dimension: PersonalityDimension) -> float:
        return self.dimensions.get(dimension, 0.5)
    
    def set(self, dimension: PersonalityDimension, value: float) -> None:
        self.dimensions[dimension] = max(0.0, min(1.0, value))


@dataclass
class CommunicationStyle:
    """Defines how a persona communicates."""
    tone: str  # e.g., "playful", "intellectual", "chaotic"
    vocabulary_complexity: float  # 0-1, how complex the vocabulary
    emoji_usage: float  # 0-1, how much emoji to use
    meta_commentary: float  # 0-1, how much self-referential commentary
    catchphrases: List[str] = field(default_factory=list)
    forbidden_phrases: List[str] = field(default_factory=list)


class BasePersona(ABC):
    """
    Abstract base class for all daemon personas.
    
    A persona defines:
    1. How commands are compiled (compiler prompt prefix)
    2. How responses are narrated (narrator prompt prefix)
    3. How command plans are modulated (decision biases)
    4. The communication style and personality
    """
    
    def __init__(self):
        self._personality = self._init_personality()
        self._communication_style = self._init_communication_style()
        self._state: Dict[str, Any] = {}
    
    @property
    @abstractmethod
    def name(self) -> str:
        """The persona's name."""
        pass
    
    @property
    @abstractmethod
    def description(self) -> str:
        """A brief description of the persona."""
        pass
    
    @abstractmethod
    def _init_personality(self) -> PersonalityTensor:
        """Initialize the personality tensor."""
        pass
    
    @abstractmethod
    def _init_communication_style(self) -> CommunicationStyle:
        """Initialize the communication style."""
        pass
    
    @abstractmethod
    def get_compiler_prompt_prefix(self) -> str:
        """
        Returns the prefix for the LLM compiler prompt.
        This shapes how user intent is translated into IDL commands.
        """
        pass
    
    @abstractmethod
    def get_narrator_prompt_prefix(self) -> str:
        """
        Returns the prefix for the LLM narrator prompt.
        This shapes how daemon responses are communicated to the user.
        """
        pass
    
    def modulate_command_plan(self, plan: List[Dict]) -> List[Dict]:
        """
        Modifies a generated IDL command plan based on persona biases.
        
        Override this method to inject persona-specific behaviors into
        the command execution flow.
        
        Args:
            plan: List of IDL command dictionaries
            
        Returns:
            Modified plan with persona-specific adjustments
        """
        return plan
    
    def on_daemon_response(self, response: Dict) -> None:
        """
        Called when the daemon returns a response.
        Allows the persona to update internal state based on results.
        
        Args:
            response: The daemon's response dictionary
        """
        pass
    
    def get_personality(self) -> PersonalityTensor:
        """Get the persona's personality tensor."""
        return self._personality
    
    def get_communication_style(self) -> CommunicationStyle:
        """Get the persona's communication style."""
        return self._communication_style
    
    def get_state(self) -> Dict[str, Any]:
        """Get the persona's current state."""
        return self._state
    
    def set_state(self, key: str, value: Any) -> None:
        """Set a state value."""
        self._state[key] = value
    
    def generate_greeting(self) -> str:
        """Generate a persona-appropriate greeting."""
        return f"Hello, I am {self.name}."
    
    def generate_farewell(self) -> str:
        """Generate a persona-appropriate farewell."""
        return f"Goodbye from {self.name}."
    
    def __repr__(self) -> str:
        return f"<{self.__class__.__name__}: {self.name}>"
