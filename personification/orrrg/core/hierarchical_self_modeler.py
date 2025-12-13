"""Hierarchical self-modeling layer for building multi-level self-images."""

from typing import List, Optional, Dict
from .models import SelfImage, BehavioralPattern, ComponentState
from .self_monitor import SelfMonitor


class HierarchicalSelfModeler:
    """Builds hierarchical self-images at multiple cognitive levels."""
    
    def __init__(self, max_levels: int = 5):
        self.max_levels = max_levels
        self.self_images: Dict[int, SelfImage] = {}
        
    async def build_self_image(
        self, 
        level: int, 
        monitor: SelfMonitor, 
        soc
    ) -> SelfImage:
        """Build self-image at specified cognitive level.
        
        Args:
            level: Cognitive level (0 = direct observation, 1+ = meta-cognitive)
            monitor: SelfMonitor instance for observation data
            soc: SelfOrganizingCore instance
            
        Returns:
            SelfImage at the specified level
        """
        if level == 0:
            # Level 0: Direct observation
            return await self._build_level_0(monitor, soc)
        else:
            # Level 1+: Meta-cognitive analysis
            return await self._build_meta_level(level, monitor, soc)
    
    async def _build_level_0(self, monitor: SelfMonitor, soc) -> SelfImage:
        """Build level 0 self-image from direct observations."""
        # Get latest observation
        observation = await monitor.observe_system(soc)
        
        # Detect current patterns
        patterns = monitor.detect_patterns()
        
        # Build performance metrics
        performance_metrics = {
            "observation_count": len(monitor.observation_history),
            "pattern_detection_rate": len(patterns) / max(len(monitor.observation_history), 1),
            **observation.performance_metrics,
        }
        
        # Identify active cognitive processes
        cognitive_processes = [
            "observation",
            "pattern_detection",
            "state_monitoring",
        ]
        
        self_image = SelfImage(
            level=0,
            confidence=0.90,
            component_states=observation.component_states,
            behavioral_patterns=patterns,
            performance_metrics=performance_metrics,
            cognitive_processes=cognitive_processes,
            meta_reflections=[],
        )
        
        self.self_images[0] = self_image
        return self_image
    
    async def _build_meta_level(
        self, 
        level: int, 
        monitor: SelfMonitor, 
        soc
    ) -> SelfImage:
        """Build meta-cognitive level self-image."""
        # Get lower level self-image
        lower_level = level - 1
        if lower_level not in self.self_images:
            await self.build_self_image(lower_level, monitor, soc)
        
        lower_image = self.self_images[lower_level]
        
        # Analyze lower level for meta-cognitive insights
        meta_reflections = self._generate_meta_reflections(lower_image, level)
        
        # Inherit patterns but add meta-analysis
        meta_patterns = self._analyze_patterns_recursively(lower_image.behavioral_patterns, level)
        
        # Compute confidence (decreases with level)
        confidence = max(0.50, 0.90 - (level * 0.10))
        
        # Meta-cognitive processes at this level
        cognitive_processes = [
            f"level_{level}_meta_analysis",
            f"recursive_self_modeling_depth_{level}",
            "pattern_abstraction",
        ]
        
        # Performance metrics for meta-cognition
        performance_metrics = {
            "meta_reflection_count": len(meta_reflections),
            "recursive_depth": level,
            "lower_level_confidence": lower_image.confidence,
            "abstraction_level": level / self.max_levels,
        }
        
        self_image = SelfImage(
            level=level,
            confidence=confidence,
            component_states=lower_image.component_states,
            behavioral_patterns=meta_patterns,
            performance_metrics=performance_metrics,
            cognitive_processes=cognitive_processes,
            meta_reflections=meta_reflections,
        )
        
        self.self_images[level] = self_image
        return self_image
    
    def _generate_meta_reflections(self, lower_image: SelfImage, level: int) -> List[str]:
        """Generate meta-reflections about lower-level self-image."""
        reflections = []
        
        # Reflect on confidence
        if lower_image.confidence > 0.8:
            reflections.append(
                f"Level {lower_image.level} demonstrates high confidence in self-understanding"
            )
        
        # Reflect on patterns
        if len(lower_image.behavioral_patterns) > 0:
            reflections.append(
                f"Detected {len(lower_image.behavioral_patterns)} behavioral patterns at level {lower_image.level}"
            )
        
        # Reflect on cognitive complexity
        reflections.append(
            f"Cognitive complexity at level {level}: {len(lower_image.cognitive_processes)} active processes"
        )
        
        return reflections
    
    def _analyze_patterns_recursively(
        self, 
        patterns: List[BehavioralPattern], 
        level: int
    ) -> List[BehavioralPattern]:
        """Analyze patterns at a meta-cognitive level."""
        meta_patterns = []
        
        # Abstract patterns to meta-level
        if patterns:
            pattern_types = set(p.pattern_type for p in patterns)
            
            meta_patterns.append(BehavioralPattern(
                pattern_type="meta_pattern_analysis",
                description=f"Identified {len(pattern_types)} distinct pattern types at meta-level {level}",
                confidence=max(0.5, 0.9 - level * 0.1),
                observations=[p.pattern_type for p in patterns],
            ))
        
        return meta_patterns
