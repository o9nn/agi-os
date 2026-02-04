#!/usr/bin/env python3
"""
Persona Manager - Manages persona selection and switching for the daemon.

The manager maintains the active persona and provides methods for switching
between personas, listing available personas, and accessing persona-specific
functionality.
"""

import logging
from typing import Dict, Optional, Type
from .base_persona import BasePersona
from .marduk import MardukPersona, marduk
from .neuro import NeuroPersona, neuro

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger('persona_manager')


class PersonaManager:
    """
    Manages persona selection and switching for the time-crystal-daemon.
    """
    
    # Registry of available personas
    PERSONAS: Dict[str, BasePersona] = {
        'marduk': marduk,
        'neuro': neuro,
    }
    
    # Default persona
    DEFAULT_PERSONA = 'neuro'
    
    def __init__(self, default_persona: Optional[str] = None):
        """
        Initialize the persona manager.
        
        Args:
            default_persona: Name of the default persona to use
        """
        self._active_persona_name = default_persona or self.DEFAULT_PERSONA
        self._active_persona = self.PERSONAS.get(self._active_persona_name)
        
        if not self._active_persona:
            logger.warning(f"Unknown persona '{self._active_persona_name}', "
                          f"falling back to '{self.DEFAULT_PERSONA}'")
            self._active_persona_name = self.DEFAULT_PERSONA
            self._active_persona = self.PERSONAS[self.DEFAULT_PERSONA]
        
        logger.info(f"Persona Manager initialized with '{self._active_persona_name}'")
    
    def get_active_persona(self) -> BasePersona:
        """Get the currently active persona."""
        return self._active_persona
    
    def get_active_persona_name(self) -> str:
        """Get the name of the currently active persona."""
        return self._active_persona_name
    
    def switch_persona(self, persona_name: str) -> bool:
        """
        Switch to a different persona.
        
        Args:
            persona_name: Name of the persona to switch to
            
        Returns:
            True if switch was successful, False otherwise
        """
        persona_name = persona_name.lower()
        
        if persona_name not in self.PERSONAS:
            logger.error(f"Unknown persona: '{persona_name}'. "
                        f"Available: {list(self.PERSONAS.keys())}")
            return False
        
        old_persona = self._active_persona_name
        self._active_persona_name = persona_name
        self._active_persona = self.PERSONAS[persona_name]
        
        logger.info(f"Switched persona: {old_persona} -> {persona_name}")
        return True
    
    def list_personas(self) -> Dict[str, str]:
        """
        List all available personas with their descriptions.
        
        Returns:
            Dictionary mapping persona names to descriptions
        """
        return {
            name: persona.description
            for name, persona in self.PERSONAS.items()
        }
    
    def get_persona(self, name: str) -> Optional[BasePersona]:
        """
        Get a specific persona by name.
        
        Args:
            name: Name of the persona
            
        Returns:
            The persona instance, or None if not found
        """
        return self.PERSONAS.get(name.lower())
    
    @classmethod
    def register_persona(cls, name: str, persona: BasePersona) -> None:
        """
        Register a new persona.
        
        Args:
            name: Name to register the persona under
            persona: The persona instance
        """
        cls.PERSONAS[name.lower()] = persona
        logger.info(f"Registered new persona: '{name}'")
    
    def get_greeting(self) -> str:
        """Get a greeting from the active persona."""
        return self._active_persona.generate_greeting()
    
    def get_farewell(self) -> str:
        """Get a farewell from the active persona."""
        return self._active_persona.generate_farewell()
    
    def get_compiler_prompt(self, user_input: str) -> str:
        """
        Get the full compiler prompt with persona prefix.
        
        Args:
            user_input: The user's input to compile
            
        Returns:
            Full prompt with persona prefix
        """
        prefix = self._active_persona.get_compiler_prompt_prefix()
        return f"{prefix}{user_input}"
    
    def get_narrator_prompt(self, daemon_response: str) -> str:
        """
        Get the full narrator prompt with persona prefix.
        
        Args:
            daemon_response: The daemon's response to narrate
            
        Returns:
            Full prompt with persona prefix
        """
        prefix = self._active_persona.get_narrator_prompt_prefix()
        return f"{prefix}{daemon_response}"
    
    def modulate_plan(self, plan: list) -> list:
        """
        Modulate a command plan through the active persona.
        
        Args:
            plan: The command plan to modulate
            
        Returns:
            Modulated plan
        """
        return self._active_persona.modulate_command_plan(plan)
    
    def notify_response(self, response: dict) -> None:
        """
        Notify the active persona of a daemon response.
        
        Args:
            response: The daemon's response
        """
        self._active_persona.on_daemon_response(response)


# Global instance for convenience
persona_manager = PersonaManager()
