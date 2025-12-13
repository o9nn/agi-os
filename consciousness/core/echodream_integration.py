"""
EchoDream Knowledge Integration System - Iteration N+5

Implements true knowledge integration during rest/dream cycles:
1. Memory replay and consolidation
2. Pattern extraction from experiences
3. Wisdom refinement through cross-pattern analysis
4. Knowledge gap identification
5. Memory pruning and strengthening

This transforms rest cycles from passive to active learning periods.
"""

import asyncio
import time
from typing import List, Dict, Any, Set, Optional, Tuple
from dataclasses import dataclass
from enum import Enum
from collections import defaultdict
import random
import math


class DreamPhase(Enum):
    """Phases of dream processing."""
    REPLAY = "replay"  # Replay recent experiences
    CONSOLIDATE = "consolidate"  # Consolidate memories
    EXTRACT = "extract"  # Extract patterns
    REFINE = "refine"  # Refine wisdom
    PRUNE = "prune"  # Prune weak memories
    INTEGRATE = "integrate"  # Integrate new knowledge


@dataclass
class DreamCycle:
    """Represents a complete dream cycle."""
    id: str
    start_time: float
    end_time: float
    phase_durations: Dict[str, float]
    memories_replayed: int
    patterns_extracted: int
    wisdom_refined: int
    memories_pruned: int
    edges_strengthened: int
    knowledge_integrated: int


class EchoDreamIntegration:
    """
    EchoDream knowledge integration system.
    
    During rest/dream cycles:
    - Replays recent experiences with varied activation patterns
    - Consolidates related memories by strengthening connections
    - Extracts patterns from activation dynamics
    - Refines wisdom through cross-pattern analysis
    - Prunes low-importance memories
    - Integrates fragmented knowledge
    """
    
    def __init__(self, memory_system=None, wisdom_system=None, skill_system=None):
        """
        Initialize EchoDream integration.
        
        Args:
            memory_system: Hypergraph memory system
            wisdom_system: Wisdom cultivation system
            skill_system: Skill development system
        """
        self.memory_system = memory_system
        self.wisdom_system = wisdom_system
        self.skill_system = skill_system
        
        # Dream state
        self.dreaming = False
        self.current_phase: Optional[DreamPhase] = None
        self.dream_cycles: List[DreamCycle] = []
        self.cycle_count = 0
        
        # Dream parameters
        self.replay_batch_size = 10  # Memories to replay per batch
        self.consolidation_threshold = 0.6  # Threshold for consolidation
        self.pruning_threshold = 0.2  # Threshold for pruning
        self.pattern_min_support = 3  # Minimum occurrences for pattern
        
        # Statistics
        self.total_memories_replayed = 0
        self.total_patterns_extracted = 0
        self.total_wisdom_refined = 0
        self.total_memories_pruned = 0
        self.total_edges_strengthened = 0
        
    async def start_dream_cycle(self, duration: float = 30.0) -> DreamCycle:
        """
        Start a complete dream cycle.
        
        Args:
            duration: Total duration of dream cycle in seconds
            
        Returns:
            DreamCycle object with results
        """
        if self.dreaming:
            print("⚠️  Already in dream cycle")
            return None
            
        self.dreaming = True
        cycle_id = f"dream_{self.cycle_count}"
        start_time = time.time()
        
        print("\n💤 ═══════════════════════════════════════════════════════")
        print("💤 EchoDream Knowledge Integration: Starting")
        print("💤 ═══════════════════════════════════════════════════════")
        
        phase_durations = {}
        stats = {
            'memories_replayed': 0,
            'patterns_extracted': 0,
            'wisdom_refined': 0,
            'memories_pruned': 0,
            'edges_strengthened': 0,
            'knowledge_integrated': 0
        }
        
        # Allocate time to phases
        phase_times = {
            DreamPhase.REPLAY: duration * 0.25,
            DreamPhase.CONSOLIDATE: duration * 0.20,
            DreamPhase.EXTRACT: duration * 0.20,
            DreamPhase.REFINE: duration * 0.15,
            DreamPhase.PRUNE: duration * 0.10,
            DreamPhase.INTEGRATE: duration * 0.10
        }
        
        # Execute each phase
        for phase, phase_duration in phase_times.items():
            self.current_phase = phase
            phase_start = time.time()
            
            result = await self._execute_dream_phase(phase, phase_duration)
            
            phase_end = time.time()
            phase_durations[phase.value] = phase_end - phase_start
            
            # Update stats
            for key, value in result.items():
                if key in stats:
                    stats[key] += value
        
        # Create dream cycle record
        end_time = time.time()
        cycle = DreamCycle(
            id=cycle_id,
            start_time=start_time,
            end_time=end_time,
            phase_durations=phase_durations,
            **stats
        )
        
        self.dream_cycles.append(cycle)
        self.cycle_count += 1
        self.dreaming = False
        self.current_phase = None
        
        # Update totals
        self.total_memories_replayed += stats['memories_replayed']
        self.total_patterns_extracted += stats['patterns_extracted']
        self.total_wisdom_refined += stats['wisdom_refined']
        self.total_memories_pruned += stats['memories_pruned']
        self.total_edges_strengthened += stats['edges_strengthened']
        
        self._print_cycle_summary(cycle)
        
        return cycle
        
    async def _execute_dream_phase(self, phase: DreamPhase, duration: float) -> Dict[str, int]:
        """Execute a specific dream phase."""
        print(f"\n💤 Phase: {phase.value.upper()}")
        
        if phase == DreamPhase.REPLAY:
            return await self._phase_replay(duration)
        elif phase == DreamPhase.CONSOLIDATE:
            return await self._phase_consolidate(duration)
        elif phase == DreamPhase.EXTRACT:
            return await self._phase_extract(duration)
        elif phase == DreamPhase.REFINE:
            return await self._phase_refine(duration)
        elif phase == DreamPhase.PRUNE:
            return await self._phase_prune(duration)
        elif phase == DreamPhase.INTEGRATE:
            return await self._phase_integrate(duration)
        else:
            return {}
            
    async def _phase_replay(self, duration: float) -> Dict[str, int]:
        """
        REPLAY phase: Replay recent experiences with varied activation.
        
        This reactivates recent memories in different patterns to:
        - Strengthen important memories
        - Discover new connections
        - Prepare for consolidation
        """
        if not self.memory_system:
            return {'memories_replayed': 0}
            
        memories_replayed = 0
        start_time = time.time()
        
        # Get recent high-importance memories
        recent_memories = self._get_recent_memories(limit=50)
        
        # Replay in batches with varied activation
        while time.time() - start_time < duration and recent_memories:
            batch = recent_memories[:self.replay_batch_size]
            recent_memories = recent_memories[self.replay_batch_size:]
            
            for memory in batch:
                # Activate memory with random variation
                activation_strength = random.uniform(0.5, 1.0)
                self._activate_memory(memory, activation_strength)
                
                # Spread activation to connected memories
                self._spread_activation(memory, depth=2)
                
                memories_replayed += 1
                
            # Small delay between batches
            await asyncio.sleep(0.1)
        
        print(f"   ✓ Replayed {memories_replayed} memories")
        return {'memories_replayed': memories_replayed}
        
    async def _phase_consolidate(self, duration: float) -> Dict[str, int]:
        """
        CONSOLIDATE phase: Consolidate related memories.
        
        Strengthens connections between frequently co-activated memories.
        """
        if not self.memory_system:
            return {'edges_strengthened': 0}
            
        edges_strengthened = 0
        start_time = time.time()
        
        # Find co-activated memory pairs
        coactivation_pairs = self._find_coactivated_pairs()
        
        # Strengthen edges between co-activated memories
        for source_id, target_id, coactivation_score in coactivation_pairs:
            if time.time() - start_time >= duration:
                break
                
            if coactivation_score >= self.consolidation_threshold:
                # Strengthen or create edge
                edge = self._strengthen_edge(source_id, target_id, coactivation_score)
                if edge:
                    edges_strengthened += 1
                    
                    # Mark memories as consolidated
                    self._mark_consolidated(source_id)
                    self._mark_consolidated(target_id)
        
        print(f"   ✓ Strengthened {edges_strengthened} memory connections")
        return {'edges_strengthened': edges_strengthened}
        
    async def _phase_extract(self, duration: float) -> Dict[str, int]:
        """
        EXTRACT phase: Extract patterns from memory activation.
        
        Identifies recurring patterns in memory activation and creates
        new pattern memories.
        """
        if not self.memory_system:
            return {'patterns_extracted': 0}
            
        patterns_extracted = 0
        start_time = time.time()
        
        # Analyze activation history for patterns
        patterns = self._identify_activation_patterns()
        
        # Create pattern memories
        for pattern in patterns:
            if time.time() - start_time >= duration:
                break
                
            if pattern['support'] >= self.pattern_min_support:
                # Create a new declarative memory for the pattern
                pattern_memory = self._create_pattern_memory(pattern)
                if pattern_memory:
                    patterns_extracted += 1
        
        print(f"   ✓ Extracted {patterns_extracted} patterns")
        return {'patterns_extracted': patterns_extracted}
        
    async def _phase_refine(self, duration: float) -> Dict[str, int]:
        """
        REFINE phase: Refine wisdom through cross-pattern analysis.
        
        Examines existing wisdom in light of new patterns and experiences,
        refining or updating wisdom entries.
        """
        if not self.wisdom_system:
            return {'wisdom_refined': 0}
            
        wisdom_refined = 0
        start_time = time.time()
        
        # Get all wisdom entries
        wisdom_entries = self._get_all_wisdom()
        
        # Refine each wisdom entry
        for wisdom in wisdom_entries:
            if time.time() - start_time >= duration:
                break
                
            # Find supporting and contradicting evidence
            evidence = self._gather_wisdom_evidence(wisdom)
            
            # Refine wisdom based on evidence
            if self._should_refine_wisdom(wisdom, evidence):
                refined = self._refine_wisdom_entry(wisdom, evidence)
                if refined:
                    wisdom_refined += 1
        
        print(f"   ✓ Refined {wisdom_refined} wisdom entries")
        return {'wisdom_refined': wisdom_refined}
        
    async def _phase_prune(self, duration: float) -> Dict[str, int]:
        """
        PRUNE phase: Prune weak or redundant memories.
        
        Removes memories with low importance and weak connections
        to free cognitive resources.
        """
        if not self.memory_system:
            return {'memories_pruned': 0}
            
        memories_pruned = 0
        start_time = time.time()
        
        # Identify candidates for pruning
        prune_candidates = self._identify_prune_candidates()
        
        # Prune low-value memories
        for memory in prune_candidates:
            if time.time() - start_time >= duration:
                break
                
            if self._should_prune_memory(memory):
                if self._prune_memory(memory):
                    memories_pruned += 1
        
        print(f"   ✓ Pruned {memories_pruned} weak memories")
        return {'memories_pruned': memories_pruned}
        
    async def _phase_integrate(self, duration: float) -> Dict[str, int]:
        """
        INTEGRATE phase: Integrate fragmented knowledge.
        
        Connects isolated memory clusters and integrates new knowledge
        into existing structures.
        """
        if not self.memory_system:
            return {'knowledge_integrated': 0}
            
        knowledge_integrated = 0
        start_time = time.time()
        
        # Find isolated memory clusters
        clusters = self._find_memory_clusters()
        
        # Find integration opportunities between clusters
        for cluster1, cluster2 in self._find_cluster_pairs(clusters):
            if time.time() - start_time >= duration:
                break
                
            # Find potential connections
            connections = self._find_cluster_connections(cluster1, cluster2)
            
            if connections:
                # Create bridging edges
                for source, target, relation in connections:
                    edge = self._create_edge(source, target, relation)
                    if edge:
                        knowledge_integrated += 1
        
        print(f"   ✓ Integrated {knowledge_integrated} knowledge connections")
        return {'knowledge_integrated': knowledge_integrated}
        
    # ═══════════════════════════════════════════════════════════════════
    # Helper Methods
    # ═══════════════════════════════════════════════════════════════════
    
    def _get_recent_memories(self, limit: int = 50) -> List[Any]:
        """Get recent high-importance memories."""
        if not self.memory_system or not hasattr(self.memory_system, 'nodes'):
            return []
            
        # Sort by timestamp and importance
        memories = sorted(
            self.memory_system.nodes,
            key=lambda m: (m.timestamp.timestamp(), m.importance),
            reverse=True
        )
        
        return memories[:limit]
        
    def _activate_memory(self, memory: Any, strength: float):
        """Activate a memory node."""
        if hasattr(memory, 'activation'):
            memory.activation = strength
            
    def _spread_activation(self, memory: Any, depth: int = 2):
        """Spread activation from a memory to connected memories."""
        if not self.memory_system or not hasattr(self.memory_system, 'edges'):
            return
            
        if depth == 0:
            return
            
        # Find connected memories
        for edge in self.memory_system.edges:
            if edge.source_id == memory.id:
                # Find target memory
                target = self._find_memory_by_id(edge.target_id)
                if target and hasattr(target, 'activation'):
                    # Spread activation with decay
                    spread_amount = memory.activation * edge.weight * 0.7
                    target.activation = min(1.0, target.activation + spread_amount)
                    
                    # Recursively spread
                    self._spread_activation(target, depth - 1)
                    
    def _find_memory_by_id(self, memory_id: str) -> Optional[Any]:
        """Find memory by ID."""
        if not self.memory_system or not hasattr(self.memory_system, 'nodes'):
            return None
            
        for memory in self.memory_system.nodes:
            if memory.id == memory_id:
                return memory
        return None
        
    def _find_coactivated_pairs(self) -> List[Tuple[str, str, float]]:
        """Find pairs of memories that are frequently co-activated."""
        if not self.memory_system or not hasattr(self.memory_system, 'activation_history'):
            return []
            
        # Simple heuristic: memories with similar activation patterns
        pairs = []
        memories = getattr(self.memory_system, 'nodes', [])
        
        for i, mem1 in enumerate(memories):
            for mem2 in memories[i+1:]:
                # Calculate co-activation score
                score = self._calculate_coactivation_score(mem1, mem2)
                if score > 0.5:
                    pairs.append((mem1.id, mem2.id, score))
        
        return sorted(pairs, key=lambda x: x[2], reverse=True)[:20]
        
    def _calculate_coactivation_score(self, mem1: Any, mem2: Any) -> float:
        """Calculate co-activation score between two memories."""
        # Simple heuristic based on current activation
        if hasattr(mem1, 'activation') and hasattr(mem2, 'activation'):
            return (mem1.activation + mem2.activation) / 2.0
        return 0.0
        
    def _strengthen_edge(self, source_id: str, target_id: str, strength: float) -> Optional[Any]:
        """Strengthen or create edge between memories."""
        if not self.memory_system:
            return None
            
        # Try to find existing edge
        if hasattr(self.memory_system, 'edges'):
            for edge in self.memory_system.edges:
                if edge.source_id == source_id and edge.target_id == target_id:
                    # Strengthen existing edge
                    edge.strength = min(1.0, edge.strength + 0.1)
                    edge.weight = min(1.0, edge.weight + strength * 0.2)
                    return edge
        
        # Create new edge
        if hasattr(self.memory_system, 'add_edge'):
            return self.memory_system.add_edge(
                source_id, target_id, 
                'consolidated', 
                strength * 0.5
            )
        
        return None
        
    def _mark_consolidated(self, memory_id: str):
        """Mark memory as consolidated."""
        memory = self._find_memory_by_id(memory_id)
        if memory and hasattr(memory, 'consolidation_count'):
            memory.consolidation_count += 1
            
    def _identify_activation_patterns(self) -> List[Dict[str, Any]]:
        """Identify patterns in memory activation."""
        # Simplified pattern detection
        patterns = []
        
        if not self.memory_system or not hasattr(self.memory_system, 'nodes'):
            return patterns
            
        # Group memories by type and high activation
        memory_types = defaultdict(list)
        for memory in self.memory_system.nodes:
            if hasattr(memory, 'activation') and memory.activation > 0.5:
                if hasattr(memory, 'memory_type'):
                    memory_types[memory.memory_type].append(memory)
        
        # Create patterns for frequently activated types
        for mem_type, memories in memory_types.items():
            if len(memories) >= self.pattern_min_support:
                patterns.append({
                    'type': 'activation_cluster',
                    'memory_type': mem_type,
                    'support': len(memories),
                    'members': [m.id for m in memories]
                })
        
        return patterns
        
    def _create_pattern_memory(self, pattern: Dict[str, Any]) -> Optional[Any]:
        """Create a new memory representing a pattern."""
        if not self.memory_system or not hasattr(self.memory_system, 'add_node'):
            return None
            
        from datetime import datetime
        from enum import Enum
        
        # Create pattern description
        content = f"Pattern: {pattern['type']} with {pattern['support']} instances"
        
        # Determine memory type
        try:
            # Try to get MemoryType enum
            memory_type = pattern.get('memory_type', 'DECLARATIVE')
            if isinstance(memory_type, str):
                # Try to convert string to enum
                from demo_autonomous_echoself_v5 import MemoryType
                memory_type = MemoryType.DECLARATIVE
        except:
            memory_type = 'DECLARATIVE'
        
        # Add pattern as declarative memory
        return self.memory_system.add_node(
            content=content,
            memory_type=memory_type,
            importance=0.7,
            metadata={'pattern': pattern}
        )
        
    def _get_all_wisdom(self) -> List[Any]:
        """Get all wisdom entries."""
        if not self.wisdom_system:
            return []
        return getattr(self.wisdom_system, 'wisdom_entries', [])
        
    def _gather_wisdom_evidence(self, wisdom: Any) -> Dict[str, Any]:
        """Gather supporting and contradicting evidence for wisdom."""
        evidence = {
            'supporting': [],
            'contradicting': [],
            'neutral': []
        }
        
        # This would analyze memories related to the wisdom
        # For now, return empty evidence
        return evidence
        
    def _should_refine_wisdom(self, wisdom: Any, evidence: Dict[str, Any]) -> bool:
        """Determine if wisdom should be refined."""
        # Refine if there's significant new evidence
        total_evidence = sum(len(v) for v in evidence.values())
        return total_evidence > 0
        
    def _refine_wisdom_entry(self, wisdom: Any, evidence: Dict[str, Any]) -> bool:
        """Refine a wisdom entry based on evidence."""
        if hasattr(wisdom, 'refined_count'):
            wisdom.refined_count += 1
            
        if hasattr(wisdom, 'confidence'):
            # Adjust confidence based on evidence
            supporting = len(evidence.get('supporting', []))
            contradicting = len(evidence.get('contradicting', []))
            
            if supporting > contradicting:
                wisdom.confidence = min(1.0, wisdom.confidence + 0.05)
            elif contradicting > supporting:
                wisdom.confidence = max(0.0, wisdom.confidence - 0.05)
        
        return True
        
    def _identify_prune_candidates(self) -> List[Any]:
        """Identify memories that are candidates for pruning."""
        if not self.memory_system or not hasattr(self.memory_system, 'nodes'):
            return []
            
        candidates = []
        for memory in self.memory_system.nodes:
            if hasattr(memory, 'importance') and memory.importance < self.pruning_threshold:
                if hasattr(memory, 'activation') and memory.activation < 0.1:
                    candidates.append(memory)
        
        return candidates
        
    def _should_prune_memory(self, memory: Any) -> bool:
        """Determine if a memory should be pruned."""
        # Don't prune recently created memories
        if hasattr(memory, 'timestamp'):
            from datetime import datetime, timedelta
            age = datetime.now() - memory.timestamp
            if age < timedelta(minutes=5):
                return False
        
        # Don't prune if it has strong connections
        if self.memory_system and hasattr(self.memory_system, 'edges'):
            connection_count = sum(
                1 for edge in self.memory_system.edges
                if edge.source_id == memory.id or edge.target_id == memory.id
            )
            if connection_count > 3:
                return False
        
        return True
        
    def _prune_memory(self, memory: Any) -> bool:
        """Prune a memory from the system."""
        if not self.memory_system or not hasattr(self.memory_system, 'nodes'):
            return False
            
        try:
            self.memory_system.nodes.remove(memory)
            
            # Remove associated edges
            if hasattr(self.memory_system, 'edges'):
                self.memory_system.edges = [
                    edge for edge in self.memory_system.edges
                    if edge.source_id != memory.id and edge.target_id != memory.id
                ]
            
            return True
        except:
            return False
            
    def _find_memory_clusters(self) -> List[Set[str]]:
        """Find clusters of connected memories."""
        if not self.memory_system:
            return []
            
        # Simple clustering based on connectivity
        clusters = []
        visited = set()
        
        if not hasattr(self.memory_system, 'nodes'):
            return clusters
            
        for memory in self.memory_system.nodes:
            if memory.id not in visited:
                cluster = self._expand_cluster(memory.id, visited)
                if len(cluster) > 1:
                    clusters.append(cluster)
        
        return clusters
        
    def _expand_cluster(self, start_id: str, visited: Set[str]) -> Set[str]:
        """Expand a cluster from a starting memory."""
        cluster = {start_id}
        visited.add(start_id)
        
        if not self.memory_system or not hasattr(self.memory_system, 'edges'):
            return cluster
            
        # BFS to find connected memories
        queue = [start_id]
        while queue:
            current_id = queue.pop(0)
            
            for edge in self.memory_system.edges:
                if edge.source_id == current_id and edge.target_id not in visited:
                    cluster.add(edge.target_id)
                    visited.add(edge.target_id)
                    queue.append(edge.target_id)
                elif edge.target_id == current_id and edge.source_id not in visited:
                    cluster.add(edge.source_id)
                    visited.add(edge.source_id)
                    queue.append(edge.source_id)
        
        return cluster
        
    def _find_cluster_pairs(self, clusters: List[Set[str]]) -> List[Tuple[Set[str], Set[str]]]:
        """Find pairs of clusters that might be related."""
        pairs = []
        for i, cluster1 in enumerate(clusters):
            for cluster2 in clusters[i+1:]:
                pairs.append((cluster1, cluster2))
        return pairs[:10]  # Limit to 10 pairs
        
    def _find_cluster_connections(self, cluster1: Set[str], cluster2: Set[str]) -> List[Tuple[str, str, str]]:
        """Find potential connections between two clusters."""
        connections = []
        
        # Find memories in each cluster
        memories1 = [self._find_memory_by_id(mid) for mid in cluster1]
        memories2 = [self._find_memory_by_id(mid) for mid in cluster2]
        
        # Look for semantic connections (simplified)
        for mem1 in memories1:
            if not mem1:
                continue
            for mem2 in memories2:
                if not mem2:
                    continue
                    
                # Check for potential connection
                if self._memories_related(mem1, mem2):
                    connections.append((mem1.id, mem2.id, 'integrated'))
        
        return connections[:5]  # Limit to 5 connections
        
    def _memories_related(self, mem1: Any, mem2: Any) -> bool:
        """Check if two memories are potentially related."""
        # Simplified: check if they share memory type
        if hasattr(mem1, 'memory_type') and hasattr(mem2, 'memory_type'):
            return mem1.memory_type == mem2.memory_type
        return False
        
    def _create_edge(self, source_id: str, target_id: str, relation: str) -> Optional[Any]:
        """Create a new edge between memories."""
        if self.memory_system and hasattr(self.memory_system, 'add_edge'):
            return self.memory_system.add_edge(source_id, target_id, relation, 0.5)
        return None
        
    def _print_cycle_summary(self, cycle: DreamCycle):
        """Print dream cycle summary."""
        duration = cycle.end_time - cycle.start_time
        
        print("\n💤 ═══════════════════════════════════════════════════════")
        print("💤 EchoDream Knowledge Integration: Complete")
        print("💤 ═══════════════════════════════════════════════════════")
        print(f"💤 Cycle ID: {cycle.id}")
        print(f"💤 Duration: {duration:.1f}s")
        print(f"💤 Memories replayed: {cycle.memories_replayed}")
        print(f"💤 Patterns extracted: {cycle.patterns_extracted}")
        print(f"💤 Wisdom refined: {cycle.wisdom_refined}")
        print(f"💤 Memories pruned: {cycle.memories_pruned}")
        print(f"💤 Edges strengthened: {cycle.edges_strengthened}")
        print(f"💤 Knowledge integrated: {cycle.knowledge_integrated}")
        print("💤 ═══════════════════════════════════════════════════════\n")
