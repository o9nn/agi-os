#!/usr/bin/env python3
"""
Embodied Memory System - Deep Tree Echo State Network (DTESN) Integration

This module implements Task 2.1.3 of the Deep Tree Echo development roadmap:
Create Embodied Memory System with:
- Episodic memory tied to body states
- Spatial memory anchored to body position
- Emotional memory linked to body sensations

Integration with DTESN architecture ensures OEIS A000081 compliance and
real-time performance constraints for neuromorphic computing.
"""

import json
import time
import logging
import numpy as np
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass, field, asdict
from enum import Enum
from pathlib import Path
import threading
import math

# DTESN Core dependencies
try:
    from psystem_evolution_engine import PSystemEvolutionEngine
    from esn_reservoir import ESNReservoir
    from bseries_tree_classifier import BSeriesTreeClassifier
    HAS_DTESN_CORE = True
except ImportError:
    HAS_DTESN_CORE = False

# Echo system integrations
try:
    import sys
    sys.path.append('../echo.dash')
    from unified_echo_memory import MemoryNode, MemoryType, HypergraphMemory
    sys.path.append('../echo.dream')
    from models_memory import MemoryNode as DreamMemoryNode
    HAS_ECHO_INTEGRATION = True
except ImportError:
    HAS_ECHO_INTEGRATION = False
    # Define fallback MemoryType enum
    class MemoryType(Enum):
        EPISODIC = "episodic"
        SEMANTIC = "semantic"
        PROCEDURAL = "procedural"
        EMOTIONAL = "emotional"
    
    # Define fallback classes
    class MemoryNode:
        pass
    
    class HypergraphMemory:
        pass
    
    class DreamMemoryNode:
        pass

# Configure logging
logger = logging.getLogger(__name__)

class BodyState(Enum):
    """Virtual body states for embodied cognition"""
    NEUTRAL = "neutral"
    ACTIVE = "active"
    RESTING = "resting"
    MOVING = "moving"
    INTERACTING = "interacting"
    LEARNING = "learning"
    STRESSED = "stressed"
    FOCUSED = "focused"

class SpatialAnchor(Enum):
    """Spatial reference frames for body-anchored memory"""
    EGOCENTRIC = "egocentric"    # Relative to body center
    ALLOCENTRIC = "allocentric"  # Absolute spatial coordinates
    PROPRIOCEPTIVE = "proprioceptive"  # Internal body position sense
    ENVIRONMENTAL = "environmental"    # Relative to environment features

@dataclass 
class BodyConfiguration:
    """3D body configuration with articulated joints"""
    position: Tuple[float, float, float] = (0.0, 0.0, 0.0)  # World coordinates
    orientation: Tuple[float, float, float, float] = (0.0, 0.0, 0.0, 1.0)  # Quaternion
    joint_angles: Dict[str, float] = field(default_factory=dict)  # Joint positions
    velocity: Tuple[float, float, float] = (0.0, 0.0, 0.0)
    angular_velocity: Tuple[float, float, float] = (0.0, 0.0, 0.0)
    timestamp: float = field(default_factory=time.time)
    
    def to_dict(self) -> Dict:
        """Convert to dictionary for serialization"""
        return asdict(self)
    
    @classmethod
    def from_dict(cls, data: Dict) -> 'BodyConfiguration':
        """Create from dictionary"""
        return cls(**data)

@dataclass
class EmbodiedContext:
    """Complete embodied context for memory formation and retrieval"""
    body_state: BodyState
    body_config: BodyConfiguration
    spatial_anchor: SpatialAnchor
    emotional_state: Dict[str, float] = field(default_factory=dict)  # Arousal, valence, etc.
    sensory_input: Dict[str, Any] = field(default_factory=dict)  # Multi-modal sensor data
    motor_output: Dict[str, Any] = field(default_factory=dict)   # Motor command history
    environment_context: Dict[str, Any] = field(default_factory=dict)  # Environmental state
    social_context: Dict[str, Any] = field(default_factory=dict)       # Social interaction state
    
    def to_dict(self) -> Dict:
        """Convert to dictionary for serialization"""
        data = asdict(self)
        data['body_state'] = self.body_state.value
        data['spatial_anchor'] = self.spatial_anchor.value
        return data
    
    @classmethod
    def from_dict(cls, data: Dict) -> 'EmbodiedContext':
        """Create from dictionary"""
        if isinstance(data['body_state'], str):
            data['body_state'] = BodyState(data['body_state'])
        if isinstance(data['spatial_anchor'], str):
            data['spatial_anchor'] = SpatialAnchor(data['spatial_anchor'])
        data['body_config'] = BodyConfiguration.from_dict(data['body_config'])
        return cls(**data)

@dataclass
class EmbodiedMemory:
    """Memory node with full embodied context integration"""
    id: str
    content: str
    memory_type: MemoryType
    embodied_context: EmbodiedContext
    creation_time: float = field(default_factory=time.time)
    last_access_time: float = field(default_factory=time.time)
    access_count: int = 0
    activation_level: float = 0.0
    consolidation_level: float = 0.0
    association_strength: Dict[str, float] = field(default_factory=dict)
    
    # DTESN-specific fields
    tree_depth: int = 0
    oeis_compliance: bool = False
    membrane_id: Optional[str] = None
    
    def to_dict(self) -> Dict:
        """Convert to dictionary for serialization"""
        data = asdict(self)
        data['memory_type'] = self.memory_type.value
        data['embodied_context'] = self.embodied_context.to_dict()
        return data
    
    @classmethod
    def from_dict(cls, data: Dict) -> 'EmbodiedMemory':
        """Create from dictionary"""
        if isinstance(data['memory_type'], str):
            data['memory_type'] = MemoryType(data['memory_type'])
        data['embodied_context'] = EmbodiedContext.from_dict(data['embodied_context'])
        return cls(**data)
    
    def access(self, current_context: Optional[EmbodiedContext] = None):
        """Mark memory as accessed, with optional context-dependent activation"""
        self.last_access_time = time.time()
        self.access_count += 1
        
        if current_context:
            # Calculate embodied context similarity and boost activation
            similarity = self._calculate_embodied_similarity(current_context)
            self.activation_level = min(1.0, self.activation_level + similarity * 0.3)
    
    def _calculate_embodied_similarity(self, other_context: EmbodiedContext) -> float:
        """Calculate similarity between embodied contexts for retrieval boosting"""
        similarity_factors = []
        
        # Body state similarity
        if self.embodied_context.body_state == other_context.body_state:
            similarity_factors.append(1.0)
        else:
            similarity_factors.append(0.3)
        
        # Spatial distance similarity (closer positions = higher similarity)
        pos_distance = self._euclidean_distance(
            self.embodied_context.body_config.position,
            other_context.body_config.position
        )
        spatial_similarity = 1.0 / (1.0 + pos_distance)  # Inverse distance
        similarity_factors.append(spatial_similarity)
        
        # Emotional state similarity
        emotional_similarity = self._cosine_similarity(
            self.embodied_context.emotional_state,
            other_context.emotional_state
        )
        similarity_factors.append(emotional_similarity)
        
        # Overall similarity is the weighted average
        return sum(similarity_factors) / len(similarity_factors)
    
    @staticmethod
    def _euclidean_distance(pos1: Tuple[float, float, float], 
                          pos2: Tuple[float, float, float]) -> float:
        """Calculate Euclidean distance between two 3D positions"""
        return math.sqrt(sum((a - b) ** 2 for a, b in zip(pos1, pos2)))
    
    @staticmethod
    def _cosine_similarity(dict1: Dict[str, float], dict2: Dict[str, float]) -> float:
        """Calculate cosine similarity between two emotional state vectors"""
        if not dict1 or not dict2:
            return 0.0
        
        # Get common keys
        common_keys = set(dict1.keys()) & set(dict2.keys())
        if not common_keys:
            return 0.0
        
        # Calculate vectors for common dimensions
        vec1 = [dict1[k] for k in common_keys]
        vec2 = [dict2[k] for k in common_keys]
        
        # Cosine similarity calculation
        dot_product = sum(a * b for a, b in zip(vec1, vec2))
        norm1 = math.sqrt(sum(a * a for a in vec1))
        norm2 = math.sqrt(sum(b * b for b in vec2))
        
        if norm1 == 0 or norm2 == 0:
            return 0.0
        
        return dot_product / (norm1 * norm2)

class EmbodiedMemorySystem:
    """
    Core embodied memory system implementing Task 2.1.3 requirements.
    
    Integrates with DTESN architecture for real-time embodied cognition with:
    - Episodic memory tied to body states
    - Spatial memory anchored to body position  
    - Emotional memory linked to body sensations
    """
    
    def __init__(self, storage_dir: str = "embodied_memory", 
                 max_working_memory: int = 7,
                 dtesn_integration: bool = True):
        """
        Initialize the embodied memory system.
        
        Args:
            storage_dir: Directory for persistent memory storage
            max_working_memory: Maximum items in working memory (Miller's Law: 7±2)
            dtesn_integration: Enable DTESN kernel integration
        """
        self.storage_dir = Path(storage_dir)
        self.storage_dir.mkdir(parents=True, exist_ok=True)
        
        # Core memory storage
        self.embodied_memories: Dict[str, EmbodiedMemory] = {}
        self.working_memory: List[str] = []  # IDs of active memories
        self.max_working_memory = max_working_memory
        
        # Current embodied state
        self.current_context = EmbodiedContext(
            body_state=BodyState.NEUTRAL,
            body_config=BodyConfiguration(),
            spatial_anchor=SpatialAnchor.EGOCENTRIC
        )
        
        # Index structures for efficient retrieval
        self.body_state_index: Dict[BodyState, List[str]] = {state: [] for state in BodyState}
        self.spatial_index: Dict[Tuple[int, int, int], List[str]] = {}  # Spatial grid
        self.emotional_index: Dict[str, List[str]] = {}  # Emotional state keys
        self.temporal_index: List[Tuple[float, str]] = []  # Time-based access
        
        # DTESN integration components
        self.dtesn_integration = dtesn_integration and HAS_DTESN_CORE
        if self.dtesn_integration:
            self._init_dtesn_components()
        
        # Thread safety for real-time operations
        self._lock = threading.RLock()
        
        # Load existing memories
        self.load_memories()
    
    def _init_dtesn_components(self):
        """Initialize DTESN kernel components for embodied memory processing"""
        try:
            # P-System membrane for embodied memory computation
            self.p_system = PSystemEvolutionEngine(
                max_membranes=8,  # OEIS A000081 compliant depth
                evolution_steps=1000
            )
            
            # Echo State Network for temporal embodied dynamics
            self.esn = ESNReservoir(
                reservoir_size=512,
                input_size=64,   # Embodied context dimensions
                spectral_radius=0.95,
                leak_rate=0.1
            )
            
            # B-Series tree classifier for embodied pattern recognition
            self.tree_classifier = BSeriesTreeClassifier(
                max_order=16,
                oeis_validation=True
            )
            
            logger.info("DTESN components initialized for embodied memory system")
        except Exception as e:
            logger.error(f"Failed to initialize DTESN components: {e}")
            self.dtesn_integration = False
    
    def create_memory(self, content: str, memory_type: MemoryType,
                     embodied_context: Optional[EmbodiedContext] = None) -> str:
        """
        Create a new embodied memory with full contextual integration.
        
        Args:
            content: Memory content
            memory_type: Type of memory (episodic, procedural, etc.)
            embodied_context: Current embodied context (uses current if None)
        
        Returns:
            Memory ID for future reference
        """
        with self._lock:
            # Use current context if none provided
            if embodied_context is None:
                embodied_context = self.current_context
            
            # Generate unique memory ID
            memory_id = f"emb_{int(time.time() * 1000000)}_{len(self.embodied_memories)}"
            
            # Create embodied memory instance
            memory = EmbodiedMemory(
                id=memory_id,
                content=content,
                memory_type=memory_type,
                embodied_context=embodied_context
            )
            
            # DTESN processing if enabled
            if self.dtesn_integration:
                memory = self._process_with_dtesn(memory)
            
            # Store memory
            self.embodied_memories[memory_id] = memory
            
            # Update indices
            self._update_indices(memory)
            
            # Add to working memory if relevant
            self._update_working_memory(memory_id)
            
            logger.info(f"Created embodied memory: {memory_id} ({memory_type.value})")
            return memory_id
    
    def retrieve_memories(self, query_context: Optional[EmbodiedContext] = None,
                         memory_type: Optional[MemoryType] = None,
                         max_results: int = 10) -> List[EmbodiedMemory]:
        """
        Retrieve memories based on embodied context similarity.
        
        Args:
            query_context: Context for retrieval (uses current if None)
            memory_type: Filter by specific memory type
            max_results: Maximum number of memories to return
        
        Returns:
            List of embodied memories ranked by relevance
        """
        with self._lock:
            if query_context is None:
                query_context = self.current_context
            
            # Get candidate memories
            candidates = list(self.embodied_memories.values())
            
            # Filter by memory type if specified
            if memory_type:
                candidates = [m for m in candidates if m.memory_type == memory_type]
            
            # Calculate relevance scores based on embodied context
            scored_memories = []
            for memory in candidates:
                # Access the memory (updates activation based on context similarity)
                memory.access(query_context)
                
                # Calculate overall relevance score
                relevance = self._calculate_relevance(memory, query_context)
                scored_memories.append((relevance, memory))
            
            # Sort by relevance and return top results
            scored_memories.sort(reverse=True, key=lambda x: x[0])
            return [memory for _, memory in scored_memories[:max_results]]
    
    def update_body_state(self, new_config: BodyConfiguration, 
                         new_state: BodyState = None):
        """
        Update current body configuration and trigger memory consolidation.
        
        Args:
            new_config: New body configuration
            new_state: New body state (optional)
        """
        with self._lock:
            old_context = self.current_context
            
            # Update current context
            self.current_context.body_config = new_config
            if new_state:
                self.current_context.body_state = new_state
            
            # Trigger spatial memory updates for position changes
            if self._significant_position_change(old_context.body_config, new_config):
                self._consolidate_spatial_memories(old_context, self.current_context)
            
            logger.debug(f"Updated body state: {new_state}, position: {new_config.position}")
    
    def update_emotional_state(self, emotional_state: Dict[str, float]):
        """
        Update current emotional state and influence memory consolidation.
        
        Args:
            emotional_state: Dictionary of emotional dimensions (arousal, valence, etc.)
        """
        with self._lock:
            self.current_context.emotional_state.update(emotional_state)
            
            # Strengthen emotional memories based on current state
            self._consolidate_emotional_memories(emotional_state)
            
            logger.debug(f"Updated emotional state: {emotional_state}")
    
    def get_spatial_memories(self, position: Tuple[float, float, float],
                           radius: float = 5.0) -> List[EmbodiedMemory]:
        """
        Retrieve memories within a spatial radius of given position.
        
        Args:
            position: 3D position to search around
            radius: Search radius
        
        Returns:
            List of spatially relevant memories
        """
        spatial_memories = []
        for memory in self.embodied_memories.values():
            memory_pos = memory.embodied_context.body_config.position
            distance = EmbodiedMemory._euclidean_distance(position, memory_pos)
            if distance <= radius:
                spatial_memories.append(memory)
        
        # Sort by spatial proximity
        spatial_memories.sort(key=lambda m: EmbodiedMemory._euclidean_distance(
            position, m.embodied_context.body_config.position))
        
        return spatial_memories
    
    def get_episodic_memories(self, time_range: Optional[Tuple[float, float]] = None,
                            body_state: Optional[BodyState] = None) -> List[EmbodiedMemory]:
        """
        Retrieve episodic memories with optional filtering.
        
        Args:
            time_range: Optional (start_time, end_time) tuple
            body_state: Optional body state filter
        
        Returns:
            List of episodic memories
        """
        memories = [m for m in self.embodied_memories.values() 
                   if m.memory_type == MemoryType.EPISODIC]
        
        # Filter by time range
        if time_range:
            start_time, end_time = time_range
            memories = [m for m in memories 
                       if start_time <= m.creation_time <= end_time]
        
        # Filter by body state
        if body_state:
            memories = [m for m in memories 
                       if m.embodied_context.body_state == body_state]
        
        # Sort by recency
        memories.sort(key=lambda m: m.creation_time, reverse=True)
        
        return memories
    
    def _process_with_dtesn(self, memory: EmbodiedMemory) -> EmbodiedMemory:
        """
        Process memory through DTESN components for enhanced integration.
        
        Args:
            memory: Memory to process
        
        Returns:
            Enhanced memory with DTESN annotations
        """
        try:
            # Convert embodied context to input vector for ESN
            context_vector = self._encode_embodied_context(memory.embodied_context)
            
            # Process through ESN for temporal dynamics
            if hasattr(self, 'esn'):
                esn_output = self.esn.process(context_vector)
                memory.activation_level = float(np.mean(esn_output))
            
            # Process through P-System for membrane assignment
            if hasattr(self, 'p_system'):
                membrane_result = self.p_system.evolve_step(context_vector.tolist())
                memory.membrane_id = f"membrane_{membrane_result.get('active_membrane', 0)}"
            
            # Process through tree classifier for pattern recognition
            if hasattr(self, 'tree_classifier'):
                tree_result = self.tree_classifier.classify(context_vector)
                memory.tree_depth = tree_result.get('depth', 0)
                memory.oeis_compliance = tree_result.get('oeis_compliant', False)
            
            logger.debug(f"DTESN processing completed for memory {memory.id}")
            
        except Exception as e:
            logger.error(f"DTESN processing failed for memory {memory.id}: {e}")
        
        return memory
    
    def _encode_embodied_context(self, context: EmbodiedContext) -> np.ndarray:
        """
        Encode embodied context into a numeric vector for DTESN processing.
        
        Args:
            context: Embodied context to encode
        
        Returns:
            Encoded vector suitable for neural processing
        """
        # Initialize encoding vector (64 dimensions for ESN input)
        encoding = np.zeros(64)
        
        # Encode body state (one-hot)
        body_state_idx = list(BodyState).index(context.body_state)
        encoding[body_state_idx] = 1.0
        
        # Encode spatial position (normalized)
        pos = context.body_config.position
        encoding[8:11] = [pos[0] / 100.0, pos[1] / 100.0, pos[2] / 100.0]
        
        # Encode orientation (quaternion)
        orientation = context.body_config.orientation
        encoding[11:15] = orientation
        
        # Encode velocity
        velocity = context.body_config.velocity
        encoding[15:18] = velocity
        
        # Encode emotional state (first 10 emotional dimensions)
        emotion_keys = list(context.emotional_state.keys())[:10]
        for i, key in enumerate(emotion_keys):
            encoding[18 + i] = context.emotional_state[key]
        
        # Encode spatial anchor type
        anchor_idx = list(SpatialAnchor).index(context.spatial_anchor)
        encoding[28 + anchor_idx] = 1.0
        
        return encoding
    
    def _calculate_relevance(self, memory: EmbodiedMemory, 
                           query_context: EmbodiedContext) -> float:
        """
        Calculate relevance score for memory retrieval.
        
        Args:
            memory: Memory to score
            query_context: Query context for comparison
        
        Returns:
            Relevance score (0.0 to 1.0)
        """
        # Base factors
        recency_factor = 1.0 / (1.0 + (time.time() - memory.last_access_time) / 3600)  # Hour decay
        activation_factor = memory.activation_level
        access_factor = min(1.0, memory.access_count / 10.0)  # Normalize access count
        
        # Embodied similarity factor
        similarity_factor = memory._calculate_embodied_similarity(query_context)
        
        # Weighted combination
        relevance = (0.3 * recency_factor + 
                    0.25 * activation_factor +
                    0.2 * access_factor +
                    0.25 * similarity_factor)
        
        return min(1.0, relevance)
    
    def _update_indices(self, memory: EmbodiedMemory):
        """Update all index structures with new memory"""
        # Body state index
        self.body_state_index[memory.embodied_context.body_state].append(memory.id)
        
        # Spatial index (discretized grid)
        pos = memory.embodied_context.body_config.position
        grid_pos = (int(pos[0] // 10), int(pos[1] // 10), int(pos[2] // 10))
        if grid_pos not in self.spatial_index:
            self.spatial_index[grid_pos] = []
        self.spatial_index[grid_pos].append(memory.id)
        
        # Emotional index
        for emotion_key in memory.embodied_context.emotional_state:
            if emotion_key not in self.emotional_index:
                self.emotional_index[emotion_key] = []
            self.emotional_index[emotion_key].append(memory.id)
        
        # Temporal index
        self.temporal_index.append((memory.creation_time, memory.id))
        self.temporal_index.sort()  # Keep sorted by time
    
    def _update_working_memory(self, memory_id: str):
        """Update working memory with new memory ID"""
        if memory_id in self.working_memory:
            self.working_memory.remove(memory_id)
        
        self.working_memory.append(memory_id)
        
        # Maintain size limit
        while len(self.working_memory) > self.max_working_memory:
            self.working_memory.pop(0)
    
    def _significant_position_change(self, old_config: BodyConfiguration,
                                   new_config: BodyConfiguration) -> bool:
        """Check if position change is significant enough to trigger consolidation"""
        distance = EmbodiedMemory._euclidean_distance(
            old_config.position, new_config.position)
        return distance > 1.0  # 1 unit threshold
    
    def _consolidate_spatial_memories(self, old_context: EmbodiedContext,
                                    new_context: EmbodiedContext):
        """Consolidate spatial memories when position changes significantly"""
        # Find memories near the old position
        old_pos = old_context.body_config.position
        nearby_memories = self.get_spatial_memories(old_pos, radius=5.0)
        
        # Strengthen associations between nearby memories
        for memory in nearby_memories:
            if memory.id in self.working_memory:
                memory.consolidation_level = min(1.0, memory.consolidation_level + 0.1)
    
    def _consolidate_emotional_memories(self, emotional_state: Dict[str, float]):
        """Consolidate memories based on emotional state similarity"""
        for memory in self.embodied_memories.values():
            # Calculate emotional similarity
            similarity = EmbodiedMemory._cosine_similarity(
                memory.embodied_context.emotional_state, emotional_state)
            
            # Boost consolidation for emotionally similar memories
            if similarity > 0.7:
                memory.consolidation_level = min(1.0, memory.consolidation_level + 0.05)
    
    def save_memories(self, filename: str = "embodied_memories.json"):
        """Save all memories to persistent storage"""
        save_path = self.storage_dir / filename
        
        memory_data = {
            'memories': {mem_id: memory.to_dict() 
                        for mem_id, memory in self.embodied_memories.items()},
            'current_context': self.current_context.to_dict(),
            'working_memory': self.working_memory,
            'metadata': {
                'version': '1.0.0',
                'save_time': time.time(),
                'total_memories': len(self.embodied_memories)
            }
        }
        
        with open(save_path, 'w') as f:
            json.dump(memory_data, f, indent=2)
        
        logger.info(f"Saved {len(self.embodied_memories)} embodied memories to {save_path}")
    
    def load_memories(self, filename: str = "embodied_memories.json"):
        """Load memories from persistent storage"""
        load_path = self.storage_dir / filename
        
        if not load_path.exists():
            logger.info("No existing memory file found, starting fresh")
            return
        
        try:
            with open(load_path, 'r') as f:
                memory_data = json.load(f)
            
            # Load memories
            for mem_id, mem_dict in memory_data['memories'].items():
                memory = EmbodiedMemory.from_dict(mem_dict)
                self.embodied_memories[mem_id] = memory
                self._update_indices(memory)
            
            # Load current context
            if 'current_context' in memory_data:
                self.current_context = EmbodiedContext.from_dict(memory_data['current_context'])
            
            # Load working memory
            if 'working_memory' in memory_data:
                self.working_memory = memory_data['working_memory']
            
            logger.info(f"Loaded {len(self.embodied_memories)} embodied memories from {load_path}")
            
        except Exception as e:
            logger.error(f"Failed to load memories: {e}")
    
    def get_stats(self) -> Dict[str, Any]:
        """Get system statistics and performance metrics"""
        stats = {
            'total_memories': len(self.embodied_memories),
            'working_memory_size': len(self.working_memory),
            'memory_types': {},
            'body_states': {},
            'spatial_coverage': len(self.spatial_index),
            'emotional_dimensions': len(self.emotional_index),
            'dtesn_integration': self.dtesn_integration,
            'average_activation': 0.0,
            'average_consolidation': 0.0
        }
        
        if self.embodied_memories:
            # Memory type distribution
            for memory in self.embodied_memories.values():
                mem_type = memory.memory_type.value
                stats['memory_types'][mem_type] = stats['memory_types'].get(mem_type, 0) + 1
                
                body_state = memory.embodied_context.body_state.value
                stats['body_states'][body_state] = stats['body_states'].get(body_state, 0) + 1
            
            # Average activation and consolidation
            activations = [m.activation_level for m in self.embodied_memories.values()]
            consolidations = [m.consolidation_level for m in self.embodied_memories.values()]
            
            stats['average_activation'] = sum(activations) / len(activations)
            stats['average_consolidation'] = sum(consolidations) / len(consolidations)
        
        return stats

# Integration functions for existing Echo systems

def create_embodied_memory_bridge(echo_memory_system) -> EmbodiedMemorySystem:
    """
    Create a bridge between existing Echo memory system and embodied memory.
    
    Args:
        echo_memory_system: Existing Echo memory system instance
    
    Returns:
        Integrated embodied memory system
    """
    embodied_system = EmbodiedMemorySystem()
    
    # Migrate existing memories if possible
    if hasattr(echo_memory_system, 'nodes'):
        for node_id, node in echo_memory_system.nodes.items():
            # Create minimal embodied context for existing memories
            embodied_context = EmbodiedContext(
                body_state=BodyState.NEUTRAL,
                body_config=BodyConfiguration(),
                spatial_anchor=SpatialAnchor.ALLOCENTRIC
            )
            
            # Create embodied memory from existing node
            embodied_memory = EmbodiedMemory(
                id=f"migrated_{node_id}",
                content=node.content,
                memory_type=node.memory_type,
                embodied_context=embodied_context,
                creation_time=getattr(node, 'creation_time', time.time()),
                activation_level=getattr(node, 'salience', 0.5)
            )
            
            embodied_system.embodied_memories[embodied_memory.id] = embodied_memory
            embodied_system._update_indices(embodied_memory)
    
    logger.info("Created embodied memory bridge with existing system")
    return embodied_system

# Example usage and testing functions

def demo_embodied_memory_system():
    """Demonstration of the embodied memory system capabilities"""
    print("=== Embodied Memory System Demo ===")
    
    # Create system
    ems = EmbodiedMemorySystem()
    
    # Create some test memories with different embodied contexts
    contexts = [
        EmbodiedContext(
            body_state=BodyState.LEARNING,
            body_config=BodyConfiguration(position=(0, 0, 0)),
            spatial_anchor=SpatialAnchor.EGOCENTRIC,
            emotional_state={'arousal': 0.7, 'valence': 0.5}
        ),
        EmbodiedContext(
            body_state=BodyState.MOVING,
            body_config=BodyConfiguration(position=(5, 3, 1)),
            spatial_anchor=SpatialAnchor.ALLOCENTRIC,
            emotional_state={'arousal': 0.8, 'valence': 0.3}
        ),
        EmbodiedContext(
            body_state=BodyState.RESTING,
            body_config=BodyConfiguration(position=(2, 1, 0)),
            spatial_anchor=SpatialAnchor.PROPRIOCEPTIVE,
            emotional_state={'arousal': 0.2, 'valence': 0.8}
        )
    ]
    
    memories = [
        ("Learning about neural networks", MemoryType.EPISODIC, contexts[0]),
        ("Walking through the garden", MemoryType.EPISODIC, contexts[1]),
        ("Relaxing after completing task", MemoryType.EMOTIONAL, contexts[2])
    ]
    
    # Create memories
    memory_ids = []
    for content, mem_type, context in memories:
        mem_id = ems.create_memory(content, mem_type, context)
        memory_ids.append(mem_id)
        print(f"Created memory: {mem_id}")
    
    # Update body state and query for similar memories
    ems.update_body_state(BodyConfiguration(position=(1, 0, 0)), BodyState.LEARNING)
    
    # Retrieve memories based on current context
    relevant_memories = ems.retrieve_memories(max_results=5)
    print(f"\nRetrieved {len(relevant_memories)} relevant memories:")
    for memory in relevant_memories:
        print(f"  - {memory.content} (activation: {memory.activation_level:.3f})")
    
    # Get spatial memories around a location
    spatial_memories = ems.get_spatial_memories((0, 0, 0), radius=3.0)
    print(f"\nSpatial memories near origin: {len(spatial_memories)}")
    
    # Get episodic memories
    episodic_memories = ems.get_episodic_memories(body_state=BodyState.LEARNING)
    print(f"Episodic memories while learning: {len(episodic_memories)}")
    
    # Display statistics
    stats = ems.get_stats()
    print("\nSystem Statistics:")
    for key, value in stats.items():
        print(f"  {key}: {value}")
    
    # Save memories
    ems.save_memories()
    print("\nMemories saved to disk")

if __name__ == "__main__":
    demo_embodied_memory_system()