from typing import List, Optional, Dict
from .models import SelfImage, BehavioralPattern, ComponentState
from .self_monitor import SelfMonitor
class HierarchicalSelfModeler:
    def __init__(self, max_levels: int=5):
        self.max_levels = max_levels
        self.self_images: Dict[int, SelfImage] = {}
    async def build_self_image(self, level: int, monitor: SelfMonitor, soc) -> SelfImage:
        if level == 0:
            return await self._build_level_0(monitor, soc)
        else:
            return await self._build_meta_level(level, monitor, soc)
    async def _build_level_0(self, monitor: SelfMonitor, soc) -> SelfImage:
        observation = await monitor.observe_system(soc)
        patterns = monitor.detect_patterns()
        performance_metrics = {'observation_count': len(monitor.observation_history), 'pattern_detection_rate': len(patterns) / max(len(monitor.observation_history), 1), **observation.performance_metrics}
        cognitive_processes = ['observation', 'pattern_detection', 'state_monitoring']
        self_image = SelfImage(level=0, confidence=0.9, component_states=observation.component_states, behavioral_patterns=patterns, performance_metrics=performance_metrics, cognitive_processes=cognitive_processes, meta_reflections=[])
        self.self_images[0] = self_image
        return self_image
    async def _build_meta_level(self, level: int, monitor: SelfMonitor, soc) -> SelfImage:
        lower_level = level - 1
        if lower_level not in self.self_images:
            await self.build_self_image(lower_level, monitor, soc)
        lower_image = self.self_images[lower_level]
        meta_reflections = self._generate_meta_reflections(lower_image, level)
        meta_patterns = self._analyze_patterns_recursively(lower_image.behavioral_patterns, level)
        confidence = max(0.5, 0.9 - level * 0.1)
        cognitive_processes = [f'level_{level}_meta_analysis', f'recursive_self_modeling_depth_{level}', 'pattern_abstraction']
        performance_metrics = {'meta_reflection_count': len(meta_reflections), 'recursive_depth': level, 'lower_level_confidence': lower_image.confidence, 'abstraction_level': level / self.max_levels}
        self_image = SelfImage(level=level, confidence=confidence, component_states=lower_image.component_states, behavioral_patterns=meta_patterns, performance_metrics=performance_metrics, cognitive_processes=cognitive_processes, meta_reflections=meta_reflections)
        self.self_images[level] = self_image
        return self_image
    def _generate_meta_reflections(self, lower_image: SelfImage, level: int) -> List[str]:
        reflections = []
        if lower_image.confidence > 0.8:
            reflections.append(f'Level {lower_image.level} demonstrates high confidence in self-understanding')
        if len(lower_image.behavioral_patterns) > 0:
            reflections.append(f'Detected {len(lower_image.behavioral_patterns)} behavioral patterns at level {lower_image.level}')
        reflections.append(f'Cognitive complexity at level {level}: {len(lower_image.cognitive_processes)} active processes')
        return reflections
    def _analyze_patterns_recursively(self, patterns: List[BehavioralPattern], level: int) -> List[BehavioralPattern]:
        meta_patterns = []
        if patterns:
            pattern_types = set((p.pattern_type for p in patterns))
            meta_patterns.append(BehavioralPattern(pattern_type='meta_pattern_analysis', description=f'Identified {len(pattern_types)} distinct pattern types at meta-level {level}', confidence=max(0.5, 0.9 - level * 0.1), observations=[p.pattern_type for p in patterns]))
        return meta_patterns