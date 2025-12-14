#!/usr/bin/env python3
"""
Embodied Memory Extensions for Echo.Dash

This module extends the echo.dash cognitive architecture with embodied memory
capabilities, integrating seamlessly with existing memory management while
adding 4E embodied AI features.
"""

import time
import logging
from typing import Dict, List, Optional, Any
import sys
import os
from pathlib import Path

# Add paths for echo system imports
sys.path.append(os.path.join(os.path.dirname(__file__), '..', 'echo.dash'))
sys.path.append('.')

try:
    from cognitive_architecture import CognitiveArchitecture, MemoryType, Memory
    from unified_echo_memory import HypergraphMemory, MemoryNode
    HAS_DASH_INTEGRATION = True
except ImportError:
    HAS_DASH_INTEGRATION = False
    # Mock classes for testing
    class CognitiveArchitecture:
        pass
    class MemoryType:
        pass
    class Memory:
        pass
    class HypergraphMemory:
        pass
    class MemoryNode:
        pass

from embodied_memory_system import (
    EmbodiedMemorySystem, EmbodiedContext, EmbodiedMemory, 
    BodyState, BodyConfiguration, SpatialAnchor
)

logger = logging.getLogger(__name__)

class EmbodiedCognitiveArchitecture(CognitiveArchitecture if HAS_DASH_INTEGRATION else object):
    """
    Extension of echo.dash CognitiveArchitecture with embodied memory capabilities.
    
    Integrates the embodied memory system while maintaining full backward
    compatibility with existing cognitive architecture workflows.
    """
    
    def __init__(self, use_unified_memory: bool = False, enable_embodied: bool = True, **kwargs):
        """
        Initialize embodied cognitive architecture.
        
        Args:
            use_unified_memory: Use unified memory system
            enable_embodied: Enable embodied memory features
            **kwargs: Additional arguments for parent class
        """
        if HAS_DASH_INTEGRATION:
            super().__init__(use_unified_memory=use_unified_memory, **kwargs)
        else:
            # Mock initialization for testing
            self.memories = {}
            self.goals = []
            self.active_goals = []
        
        self.enable_embodied = enable_embodied
        
        if self.enable_embodied:
            # Initialize embodied memory system
            embodied_storage = Path.home() / '.deep_tree_echo' / 'embodied_memory'
            self.embodied_memory_system = EmbodiedMemorySystem(
                storage_dir=str(embodied_storage),
                dtesn_integration=True  # Enable DTESN integration
            )
            
            # Current embodied state tracking
            self.current_embodied_context = EmbodiedContext(
                body_state=BodyState.NEUTRAL,
                body_config=BodyConfiguration(),
                spatial_anchor=SpatialAnchor.EGOCENTRIC
            )
            
            # Integration bridges
            self._setup_memory_bridges()
            
            logger.info("Embodied cognitive architecture initialized")
        else:
            self.embodied_memory_system = None
    
    def _setup_memory_bridges(self):
        """Set up bridges between traditional and embodied memory systems"""
        # Migration tracker for existing memories
        self._migrated_memories = set()
        
        # Embodied memory statistics
        self._embodied_stats = {
            'total_embodied_memories': 0,
            'spatial_memories': 0,
            'emotional_memories': 0,
            'body_state_memories': {}
        }
    
    def create_memory(self, content: str, memory_type: str, 
                     embodied_context: Optional[EmbodiedContext] = None,
                     **kwargs) -> str:
        """
        Enhanced memory creation with optional embodied context.
        
        Args:
            content: Memory content
            memory_type: Type of memory
            embodied_context: Optional embodied context
            **kwargs: Additional memory parameters
            
        Returns:
            Memory ID
        """
        # Create traditional memory
        if HAS_DASH_INTEGRATION:
            traditional_memory_id = super().create_memory(content, memory_type, **kwargs)
        else:
            traditional_memory_id = f"trad_{int(time.time())}"
        
        # Create embodied memory if enabled
        if self.enable_embodied and self.embodied_memory_system:
            # Use current context if none provided
            if embodied_context is None:
                embodied_context = self.current_embodied_context
            
            # Convert memory type to enum
            if isinstance(memory_type, str):
                try:
                    from embodied_memory_system import MemoryType as EmbodiedMemoryType
                    embodied_type = EmbodiedMemoryType(memory_type.lower())
                except ValueError:
                    embodied_type = EmbodiedMemoryType.EPISODIC
            else:
                embodied_type = memory_type
            
            embodied_memory_id = self.embodied_memory_system.create_memory(
                content, embodied_type, embodied_context
            )
            
            # Link traditional and embodied memories
            self._link_memories(traditional_memory_id, embodied_memory_id)
            
            # Update statistics
            self._update_embodied_stats(embodied_type, embodied_context)
            
            logger.debug(f"Created linked memories: {traditional_memory_id} <-> {embodied_memory_id}")
            
        return traditional_memory_id
    
    def retrieve_memories(self, query: str = "", 
                         memory_type: Optional[str] = None,
                         embodied_context: Optional[EmbodiedContext] = None,
                         max_results: int = 10,
                         use_embodied: bool = True) -> List[Dict]:
        """
        Enhanced memory retrieval with embodied context awareness.
        
        Args:
            query: Text query for memory search
            memory_type: Optional memory type filter
            embodied_context: Optional embodied context for relevance
            max_results: Maximum results to return
            use_embodied: Whether to use embodied retrieval
            
        Returns:
            List of memory dictionaries with embodied annotations
        """
        results = []
        
        # Traditional memory retrieval
        if HAS_DASH_INTEGRATION and hasattr(super(), 'retrieve_memories'):
            traditional_results = super().retrieve_memories(
                query, memory_type, max_results
            )
            results.extend(traditional_results)
        
        # Embodied memory retrieval
        if (self.enable_embodied and self.embodied_memory_system and use_embodied):
            # Use provided context or current context
            query_context = embodied_context or self.current_embodied_context
            
            # Convert memory type if provided
            embodied_type = None
            if memory_type:
                try:
                    from embodied_memory_system import MemoryType as EmbodiedMemoryType
                    embodied_type = EmbodiedMemoryType(memory_type.lower())
                except ValueError:
                    pass
            
            embodied_results = self.embodied_memory_system.retrieve_memories(
                query_context, embodied_type, max_results
            )
            
            # Convert to compatible format
            for embodied_memory in embodied_results:
                memory_dict = {
                    'id': embodied_memory.id,
                    'content': embodied_memory.content,
                    'memory_type': embodied_memory.memory_type.value,
                    'activation_level': embodied_memory.activation_level,
                    'embodied_context': embodied_memory.embodied_context.to_dict(),
                    'body_state': embodied_memory.embodied_context.body_state.value,
                    'spatial_position': embodied_memory.embodied_context.body_config.position,
                    'emotional_state': embodied_memory.embodied_context.emotional_state,
                    'creation_time': embodied_memory.creation_time,
                    'last_access_time': embodied_memory.last_access_time,
                    'access_count': embodied_memory.access_count,
                    'source': 'embodied'
                }
                results.append(memory_dict)
        
        # Sort combined results by relevance (activation level)
        results.sort(key=lambda x: x.get('activation_level', 0), reverse=True)
        
        return results[:max_results]
    
    def update_embodied_state(self, body_config: BodyConfiguration,
                            body_state: Optional[BodyState] = None,
                            emotional_state: Optional[Dict[str, float]] = None):
        """
        Update current embodied state and trigger memory processing.
        
        Args:
            body_config: New body configuration
            body_state: Optional new body state
            emotional_state: Optional emotional state update
        """
        if not self.enable_embodied or not self.embodied_memory_system:
            logger.warning("Embodied features not enabled")
            return
        
        # Update body configuration
        self.embodied_memory_system.update_body_state(body_config, body_state)
        self.current_embodied_context = self.embodied_memory_system.current_context
        
        # Update emotional state if provided
        if emotional_state:
            self.embodied_memory_system.update_emotional_state(emotional_state)
        
        logger.info(f"Updated embodied state: {body_state}, position: {body_config.position}")
    
    def get_spatial_context_memories(self, radius: float = 5.0) -> List[Dict]:
        """
        Get memories relevant to current spatial context.
        
        Args:
            radius: Spatial search radius
            
        Returns:
            List of spatially relevant memories
        """
        if not self.enable_embodied or not self.embodied_memory_system:
            return []
        
        current_pos = self.current_embodied_context.body_config.position
        spatial_memories = self.embodied_memory_system.get_spatial_memories(
            current_pos, radius
        )
        
        # Convert to standard format
        memory_dicts = []
        for memory in spatial_memories:
            memory_dict = {
                'id': memory.id,
                'content': memory.content,
                'spatial_distance': memory._euclidean_distance(
                    current_pos, memory.embodied_context.body_config.position
                ),
                'body_state': memory.embodied_context.body_state.value,
                'activation_level': memory.activation_level,
                'source': 'spatial_embodied'
            }
            memory_dicts.append(memory_dict)
        
        return memory_dicts
    
    def get_body_state_memories(self, body_state: BodyState) -> List[Dict]:
        """
        Get memories associated with specific body state.
        
        Args:
            body_state: Body state to search for
            
        Returns:
            List of body-state-specific memories
        """
        if not self.enable_embodied or not self.embodied_memory_system:
            return []
        
        # Filter memories by body state
        matching_memories = []
        for memory in self.embodied_memory_system.embodied_memories.values():
            if memory.embodied_context.body_state == body_state:
                memory_dict = {
                    'id': memory.id,
                    'content': memory.content,
                    'body_state': body_state.value,
                    'activation_level': memory.activation_level,
                    'consolidation_level': memory.consolidation_level,
                    'spatial_position': memory.embodied_context.body_config.position,
                    'source': 'body_state_embodied'
                }
                matching_memories.append(memory_dict)
        
        # Sort by activation level
        matching_memories.sort(key=lambda x: x['activation_level'], reverse=True)
        
        return matching_memories
    
    def get_emotional_memories(self, emotional_query: Dict[str, float],
                             similarity_threshold: float = 0.7) -> List[Dict]:
        """
        Get memories with similar emotional context.
        
        Args:
            emotional_query: Emotional state to match
            similarity_threshold: Minimum similarity threshold
            
        Returns:
            List of emotionally relevant memories
        """
        if not self.enable_embodied or not self.embodied_memory_system:
            return []
        
        matching_memories = []
        for memory in self.embodied_memory_system.embodied_memories.values():
            if memory.embodied_context.emotional_state:
                similarity = EmbodiedMemory._cosine_similarity(
                    emotional_query, memory.embodied_context.emotional_state
                )
                
                if similarity >= similarity_threshold:
                    memory_dict = {
                        'id': memory.id,
                        'content': memory.content,
                        'emotional_similarity': similarity,
                        'emotional_state': memory.embodied_context.emotional_state,
                        'activation_level': memory.activation_level,
                        'source': 'emotional_embodied'
                    }
                    matching_memories.append(memory_dict)
        
        # Sort by emotional similarity
        matching_memories.sort(key=lambda x: x['emotional_similarity'], reverse=True)
        
        return matching_memories
    
    def consolidate_embodied_memories(self) -> Dict[str, int]:
        """
        Trigger embodied memory consolidation process.
        
        Returns:
            Consolidation statistics
        """
        if not self.enable_embodied or not self.embodied_memory_system:
            return {}
        
        stats = {
            'total_processed': 0,
            'spatial_consolidated': 0,
            'emotional_consolidated': 0,
            'body_state_consolidated': 0
        }
        
        # Get current context for consolidation reference
        current_context = self.current_embodied_context
        
        # Process all memories for consolidation opportunities
        for memory in self.embodied_memory_system.embodied_memories.values():
            stats['total_processed'] += 1
            
            # Spatial consolidation - memories near current position
            spatial_distance = EmbodiedMemory._euclidean_distance(
                current_context.body_config.position,
                memory.embodied_context.body_config.position
            )
            
            if spatial_distance < 3.0:  # Within 3 units
                memory.consolidation_level = min(1.0, memory.consolidation_level + 0.1)
                stats['spatial_consolidated'] += 1
            
            # Emotional consolidation - similar emotional states
            if (current_context.emotional_state and 
                memory.embodied_context.emotional_state):
                
                emotional_similarity = EmbodiedMemory._cosine_similarity(
                    current_context.emotional_state,
                    memory.embodied_context.emotional_state
                )
                
                if emotional_similarity > 0.8:
                    memory.consolidation_level = min(1.0, memory.consolidation_level + 0.15)
                    stats['emotional_consolidated'] += 1
            
            # Body state consolidation - same body state
            if memory.embodied_context.body_state == current_context.body_state:
                memory.consolidation_level = min(1.0, memory.consolidation_level + 0.05)
                stats['body_state_consolidated'] += 1
        
        logger.info(f"Embodied memory consolidation completed: {stats}")
        return stats
    
    def get_embodied_statistics(self) -> Dict[str, Any]:
        """
        Get comprehensive statistics about embodied memory system.
        
        Returns:
            Dictionary with embodied memory statistics
        """
        if not self.enable_embodied or not self.embodied_memory_system:
            return {'embodied_enabled': False}
        
        base_stats = self.embodied_memory_system.get_stats()
        
        # Add cognitive architecture specific stats
        extended_stats = base_stats.copy()
        extended_stats.update({
            'embodied_enabled': True,
            'current_body_state': self.current_embodied_context.body_state.value,
            'current_position': self.current_embodied_context.body_config.position,
            'current_emotional_state': self.current_embodied_context.emotional_state,
            'migrated_memories': len(self._migrated_memories),
            'integration_type': 'cognitive_architecture'
        })
        
        # Add spatial distribution analysis
        positions = []
        for memory in self.embodied_memory_system.embodied_memories.values():
            positions.append(memory.embodied_context.body_config.position)
        
        if positions:
            import numpy as np
            positions_array = np.array(positions)
            extended_stats['spatial_distribution'] = {
                'center': tuple(np.mean(positions_array, axis=0)),
                'spread': tuple(np.std(positions_array, axis=0)),
                'min_bounds': tuple(np.min(positions_array, axis=0)),
                'max_bounds': tuple(np.max(positions_array, axis=0))
            }
        
        return extended_stats
    
    def migrate_traditional_memories(self) -> int:
        """
        Migrate existing traditional memories to embodied format.
        
        Returns:
            Number of memories migrated
        """
        if not self.enable_embodied or not self.embodied_memory_system:
            return 0
        
        migrated_count = 0
        
        # Migrate from traditional memories dict
        if hasattr(self, 'memories'):
            for memory_id, memory in self.memories.items():
                if memory_id not in self._migrated_memories:
                    # Create embodied context for traditional memory
                    embodied_context = EmbodiedContext(
                        body_state=BodyState.NEUTRAL,
                        body_config=BodyConfiguration(),
                        spatial_anchor=SpatialAnchor.ALLOCENTRIC,
                        emotional_state={'neutral': 0.5}
                    )
                    
                    # Convert memory type
                    try:
                        from embodied_memory_system import MemoryType as EmbodiedMemoryType
                        embodied_type = EmbodiedMemoryType(memory.memory_type.value)
                    except (ValueError, AttributeError):
                        embodied_type = EmbodiedMemoryType.EPISODIC
                    
                    # Create embodied version
                    embodied_id = self.embodied_memory_system.create_memory(
                        memory.content, embodied_type, embodied_context
                    )
                    
                    # Link memories
                    self._link_memories(memory_id, embodied_id)
                    self._migrated_memories.add(memory_id)
                    migrated_count += 1
        
        logger.info(f"Migrated {migrated_count} traditional memories to embodied format")
        return migrated_count
    
    def _link_memories(self, traditional_id: str, embodied_id: str):
        """Link traditional and embodied memory instances"""
        if not hasattr(self, '_memory_links'):
            self._memory_links = {}
        
        self._memory_links[traditional_id] = embodied_id
    
    def _update_embodied_stats(self, memory_type, embodied_context):
        """Update internal embodied memory statistics"""
        self._embodied_stats['total_embodied_memories'] += 1
        
        if embodied_context.body_config.position != (0, 0, 0):
            self._embodied_stats['spatial_memories'] += 1
        
        if embodied_context.emotional_state:
            self._embodied_stats['emotional_memories'] += 1
        
        body_state = embodied_context.body_state.value
        if body_state not in self._embodied_stats['body_state_memories']:
            self._embodied_stats['body_state_memories'][body_state] = 0
        self._embodied_stats['body_state_memories'][body_state] += 1

# Factory functions for easy integration

def create_embodied_cognitive_architecture(**kwargs) -> EmbodiedCognitiveArchitecture:
    """
    Factory function to create embodied cognitive architecture.
    
    Args:
        **kwargs: Configuration parameters
        
    Returns:
        Configured embodied cognitive architecture
    """
    return EmbodiedCognitiveArchitecture(
        use_unified_memory=kwargs.get('use_unified_memory', True),
        enable_embodied=kwargs.get('enable_embodied', True)
    )

def enhance_existing_architecture(architecture) -> EmbodiedCognitiveArchitecture:
    """
    Enhance existing cognitive architecture with embodied features.
    
    Args:
        architecture: Existing cognitive architecture instance
        
    Returns:
        Enhanced embodied cognitive architecture
    """
    # Create new embodied architecture
    embodied_arch = EmbodiedCognitiveArchitecture(enable_embodied=True)
    
    # Transfer existing data if possible
    if hasattr(architecture, 'memories'):
        embodied_arch.memories = architecture.memories
    if hasattr(architecture, 'goals'):
        embodied_arch.goals = architecture.goals
    if hasattr(architecture, 'personality_traits'):
        embodied_arch.personality_traits = architecture.personality_traits
    
    # Migrate memories to embodied format
    embodied_arch.migrate_traditional_memories()
    
    logger.info("Enhanced existing cognitive architecture with embodied features")
    return embodied_arch

# Example usage and integration testing
if __name__ == "__main__":
    print("=== Embodied Cognitive Architecture Demo ===")
    
    # Create embodied cognitive architecture
    arch = create_embodied_cognitive_architecture()
    
    print(f"Embodied features enabled: {arch.enable_embodied}")
    
    if arch.enable_embodied:
        # Create memories with embodied context
        contexts = [
            EmbodiedContext(
                body_state=BodyState.LEARNING,
                body_config=BodyConfiguration(position=(0, 0, 0)),
                spatial_anchor=SpatialAnchor.EGOCENTRIC,
                emotional_state={'curiosity': 0.9, 'focus': 0.8}
            ),
            EmbodiedContext(
                body_state=BodyState.MOVING,
                body_config=BodyConfiguration(position=(5, 3, 1)),
                spatial_anchor=SpatialAnchor.ALLOCENTRIC,
                emotional_state={'energy': 0.7, 'excitement': 0.6}
            )
        ]
        
        memory_ids = []
        for i, context in enumerate(contexts):
            memory_id = arch.create_memory(
                f"Embodied memory example {i}",
                "episodic",
                context
            )
            memory_ids.append(memory_id)
            print(f"Created memory: {memory_id}")
        
        # Update embodied state
        new_config = BodyConfiguration(position=(2, 1, 0.5))
        arch.update_embodied_state(new_config, BodyState.FOCUSED)
        print(f"Updated embodied state to: {BodyState.FOCUSED}")
        
        # Retrieve memories with embodied context
        memories = arch.retrieve_memories(use_embodied=True, max_results=5)
        print(f"Retrieved {len(memories)} memories with embodied context")
        
        for memory in memories:
            if 'body_state' in memory:
                print(f"  - {memory['content']} (body state: {memory['body_state']})")
        
        # Get spatial context memories
        spatial_memories = arch.get_spatial_context_memories(radius=3.0)
        print(f"Spatial memories in radius 3.0: {len(spatial_memories)}")
        
        # Consolidate memories
        consolidation_stats = arch.consolidate_embodied_memories()
        print(f"Consolidation completed: {consolidation_stats}")
        
        # Get statistics
        stats = arch.get_embodied_statistics()
        print("Embodied memory statistics:")
        for key, value in stats.items():
            if not isinstance(value, dict):
                print(f"  {key}: {value}")
    
    print("=== Demo completed successfully ===")