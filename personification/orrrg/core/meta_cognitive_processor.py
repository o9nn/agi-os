"""Meta-cognitive processing layer for generating insights."""

from typing import List, Dict
from .models import SelfImage, MetaCognitiveInsight


class MetaCognitiveProcessor:
    """Generates meta-cognitive insights from self-images."""
    
    def __init__(self):
        self.insight_history: List[MetaCognitiveInsight] = []
        
    async def process_self_image(self, self_image: SelfImage) -> List[MetaCognitiveInsight]:
        """Process self-image to generate insights.
        
        Args:
            self_image: SelfImage to analyze
            
        Returns:
            List of generated insights
        """
        insights = []
        
        # Analyze resource utilization
        insights.extend(self._analyze_resource_utilization(self_image))
        
        # Analyze behavioral stability
        insights.extend(self._analyze_behavioral_stability(self_image))
        
        # Analyze self-awareness quality (MUST come after patterns are added)
        insights.extend(self._analyze_self_awareness(self_image))
        
        # Analyze autognosis operation
        insights.extend(self._analyze_autognosis_operation(self_image))
        
        # Store insights
        self.insight_history.extend(insights)
        
        return insights
    
    def _analyze_autognosis_operation(self, self_image: SelfImage) -> List[MetaCognitiveInsight]:
        """Analyze autognosis system operation."""
        insights = []
        
        # Check for autognosis activity pattern
        for pattern in self_image.behavioral_patterns:
            if pattern.pattern_type == "autognosis_active":
                insights.append(MetaCognitiveInsight(
                    insight_type="autognosis_operational",
                    description="Autognosis system operational and self-monitoring",
                    severity="low",
                    confidence=pattern.confidence,
                    related_patterns=["autognosis_active"],
                ))
        
        return insights
    
    def _analyze_resource_utilization(self, self_image: SelfImage) -> List[MetaCognitiveInsight]:
        """Analyze resource utilization patterns."""
        insights = []
        
        # Check for underutilization
        for pattern in self_image.behavioral_patterns:
            if pattern.pattern_type == "resource_underutilization":
                insights.append(MetaCognitiveInsight(
                    insight_type="resource_underutilization",
                    description=pattern.description,
                    severity="medium",
                    confidence=pattern.confidence,
                    related_patterns=["resource_underutilization"],
                ))
        
        return insights
    
    def _analyze_behavioral_stability(self, self_image: SelfImage) -> List[MetaCognitiveInsight]:
        """Analyze behavioral stability."""
        insights = []
        
        # Check for stability patterns
        for pattern in self_image.behavioral_patterns:
            if pattern.pattern_type == "behavioral_stability":
                insights.append(MetaCognitiveInsight(
                    insight_type="behavioral_stability",
                    description=pattern.description,
                    severity="low",
                    confidence=pattern.confidence,
                    related_patterns=["behavioral_stability"],
                ))
        
        return insights
    
    def _analyze_self_awareness(self, self_image: SelfImage) -> List[MetaCognitiveInsight]:
        """Analyze self-awareness quality."""
        insights = []
        
        # Calculate self-awareness score
        awareness_score = self._calculate_self_awareness_score(self_image)
        
        if awareness_score > 0.7:
            insights.append(MetaCognitiveInsight(
                insight_type="high_self_awareness",
                description=f"System demonstrates high self-awareness (score: {awareness_score:.2f})",
                severity="low",
                confidence=self_image.confidence,
                related_patterns=[],
            ))
        
        return insights
    
    def _calculate_self_awareness_score(self, self_image: SelfImage) -> float:
        """Calculate overall self-awareness score."""
        metrics = self_image.performance_metrics
        
        # Pattern recognition ability
        pattern_recognition = min(1.0, len(self_image.behavioral_patterns) / 5.0)
        
        # Performance awareness
        performance_awareness = 0.85  # Mock value
        
        # Meta-reflection depth
        meta_reflection_depth = min(1.0, len(self_image.meta_reflections) / 5.0)
        
        # Cognitive complexity
        cognitive_complexity = min(1.0, len(self_image.cognitive_processes) / 5.0)
        
        # Overall score (weighted average)
        score = (
            pattern_recognition * 0.25 +
            performance_awareness * 0.35 +
            meta_reflection_depth * 0.20 +
            cognitive_complexity * 0.20
        )
        
        return score
    
    def get_self_awareness_assessment(self, self_image: SelfImage) -> Dict[str, float]:
        """Get detailed self-awareness assessment."""
        pattern_recognition = min(1.0, len(self_image.behavioral_patterns) / 5.0) * 0.75
        performance_awareness = 0.85
        meta_reflection_depth = min(1.0, len(self_image.meta_reflections) / 5.0) * 0.60
        cognitive_complexity = min(1.0, len(self_image.cognitive_processes) / 5.0) * 0.70
        
        return {
            "pattern_recognition": pattern_recognition,
            "performance_awareness": performance_awareness,
            "meta_reflection_depth": meta_reflection_depth,
            "cognitive_complexity": cognitive_complexity,
            "overall_score": self._calculate_self_awareness_score(self_image),
        }
