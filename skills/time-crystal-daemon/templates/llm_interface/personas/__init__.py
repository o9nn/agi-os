"""
Persona system for the Time Crystal Daemon.

Available personas:
- marduk: The Mad Scientist - over-engineered, systemic solutions
- neuro: The Chaotic VTuber - playful, sarcastic, meta-aware

Usage:
    from personas import persona_manager
    
    # Get active persona
    persona = persona_manager.get_active_persona()
    
    # Switch persona
    persona_manager.switch_persona('marduk')
    
    # Get compiler prompt
    prompt = persona_manager.get_compiler_prompt(user_input)
"""

from .base_persona import BasePersona, PersonalityTensor, PersonalityDimension, CommunicationStyle
from .marduk import MardukPersona, marduk
from .neuro import NeuroPersona, neuro
from .manager import PersonaManager, persona_manager

__all__ = [
    'BasePersona',
    'PersonalityTensor',
    'PersonalityDimension',
    'CommunicationStyle',
    'MardukPersona',
    'marduk',
    'NeuroPersona',
    'neuro',
    'PersonaManager',
    'persona_manager',
]
