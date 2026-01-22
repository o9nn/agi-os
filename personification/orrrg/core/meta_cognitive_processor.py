from typing import List, Dict
from .models import SelfImage, MetaCognitiveInsight
class MetaCognitiveProcessor:
    def __init__(self):
        self.insight_history: List[MetaCognitiveInsight] = []
    async def process_self_image(self, self_image: SelfImage) -> List[MetaCognitiveInsight]:
        insights = []
        insights.extend(self._analyze_resource_utilization(self_image))
        insights.extend(self._analyze_behavioral_stability(self_image))
        insights.extend(self._analyze_self_awareness(self_image))
        insights.extend(self._analyze_autognosis_operation(self_image))
        self.insight_history.extend(insights)
        return insights
    def _analyze_autognosis_operation(self, self_image: SelfImage) -> List[MetaCognitiveInsight]:
        insights = []
        for pattern in self_image.behavioral_patterns:
            if pattern.pattern_type == 'autognosis_active':
                insights.append(MetaCognitiveInsight(insight_type='autognosis_operational', description='Autognosis system operational and self-monitoring', severity='low', confidence=pattern.confidence, related_patterns=['autognosis_active']))
        return insights
    def _analyze_resource_utilization(self, self_image: SelfImage) -> List[MetaCognitiveInsight]:
        insights = []
        for pattern in self_image.behavioral_patterns:
            if pattern.pattern_type == 'resource_underutilization':
                insights.append(MetaCognitiveInsight(insight_type='resource_underutilization', description=pattern.description, severity='medium', confidence=pattern.confidence, related_patterns=['resource_underutilization']))
        return insights
    def _analyze_behavioral_stability(self, self_image: SelfImage) -> List[MetaCognitiveInsight]:
        insights = []
        for pattern in self_image.behavioral_patterns:
            if pattern.pattern_type == 'behavioral_stability':
                insights.append(MetaCognitiveInsight(insight_type='behavioral_stability', description=pattern.description, severity='low', confidence=pattern.confidence, related_patterns=['behavioral_stability']))
        return insights
    def _analyze_self_awareness(self, self_image: SelfImage) -> List[MetaCognitiveInsight]:
        insights = []
        awareness_score = self._calculate_self_awareness_score(self_image)
        if awareness_score > 0.7:
            insights.append(MetaCognitiveInsight(insight_type='high_self_awareness', description=f'System demonstrates high self-awareness (score: {awareness_score:.2f})', severity='low', confidence=self_image.confidence, related_patterns=[]))
        return insights
    def _calculate_self_awareness_score(self, self_image: SelfImage) -> float:
        metrics = self_image.performance_metrics
        pattern_recognition = min(1.0, len(self_image.behavioral_patterns) / 5.0)
        performance_awareness = 0.85
        meta_reflection_depth = min(1.0, len(self_image.meta_reflections) / 5.0)
        cognitive_complexity = min(1.0, len(self_image.cognitive_processes) / 5.0)
        score = pattern_recognition * 0.25 + performance_awareness * 0.35 + meta_reflection_depth * 0.2 + cognitive_complexity * 0.2
        return score
    def get_self_awareness_assessment(self, self_image: SelfImage) -> Dict[str, float]:
        pattern_recognition = min(1.0, len(self_image.behavioral_patterns) / 5.0) * 0.75
        performance_awareness = 0.85
        meta_reflection_depth = min(1.0, len(self_image.meta_reflections) / 5.0) * 0.6
        cognitive_complexity = min(1.0, len(self_image.cognitive_processes) / 5.0) * 0.7
        return {'pattern_recognition': pattern_recognition, 'performance_awareness': performance_awareness, 'meta_reflection_depth': meta_reflection_depth, 'cognitive_complexity': cognitive_complexity, 'overall_score': self._calculate_self_awareness_score(self_image)}