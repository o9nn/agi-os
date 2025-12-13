"""
Wisdom Cultivation Metrics for Deep Tree Echo

This module defines and tracks wisdom as a composite metric involving:
- Depth: Ability to recognize deep patterns and principles
- Breadth: Integration across diverse knowledge domains
- Applicability: Practical utility of insights
- Coherence: Internal consistency of worldview
- Adaptability: Ability to update beliefs with new evidence
"""

import time
from typing import Dict, List, Optional, Tuple
from dataclasses import dataclass, field
from collections import defaultdict
import json


@dataclass
class WisdomInsight:
    """Represents a wisdom insight with measurable qualities."""
    id: str
    content: str
    timestamp: float
    depth_score: float  # 0.0-1.0: How deep/fundamental is this insight?
    breadth_score: float  # 0.0-1.0: How many domains does it connect?
    applicability_score: float  # 0.0-1.0: How practically useful is it?
    coherence_contribution: float  # -1.0 to 1.0: Does it increase or decrease worldview coherence?
    source_experiences: List[str] = field(default_factory=list)
    related_domains: List[str] = field(default_factory=list)
    confidence: float = 0.8


@dataclass
class BeliefUpdate:
    """Represents an update to a belief based on new evidence."""
    id: str
    timestamp: float
    prior_belief: str
    updated_belief: str
    evidence: str
    confidence_change: float  # How much confidence changed
    coherence_impact: float  # Impact on overall coherence


class WisdomMetrics:
    """
    Tracks and calculates wisdom cultivation metrics for Deep Tree Echo.
    
    Wisdom is measured as a composite of five dimensions:
    1. Depth - Recognition of fundamental patterns and principles
    2. Breadth - Integration across diverse knowledge domains  
    3. Applicability - Practical utility of insights
    4. Coherence - Internal consistency of worldview
    5. Adaptability - Flexibility in updating beliefs with evidence
    """
    
    def __init__(self):
        self.insights: List[WisdomInsight] = []
        self.belief_updates: List[BeliefUpdate] = []
        
        # Domain tracking for breadth calculation
        self.domain_knowledge: Dict[str, float] = defaultdict(float)
        self.domain_connections: Dict[Tuple[str, str], int] = defaultdict(int)
        
        # Coherence tracking
        self.worldview_coherence_history: List[Tuple[float, float]] = []  # (timestamp, coherence)
        self.current_coherence: float = 0.5  # Start at neutral
        
        # Adaptability tracking
        self.belief_revision_count: int = 0
        self.evidence_integration_score: float = 0.5
        
        # Composite wisdom score history
        self.wisdom_history: List[Tuple[float, Dict[str, float]]] = []
        
    def add_insight(self, insight: WisdomInsight) -> None:
        """Add a new wisdom insight and update metrics."""
        self.insights.append(insight)
        
        # Update domain knowledge
        for domain in insight.related_domains:
            self.domain_knowledge[domain] += insight.depth_score * 0.1
            
        # Update domain connections (breadth)
        for i, domain1 in enumerate(insight.related_domains):
            for domain2 in insight.related_domains[i+1:]:
                key = tuple(sorted([domain1, domain2]))
                self.domain_connections[key] += 1
                
        # Update coherence
        self.current_coherence += insight.coherence_contribution * 0.05
        self.current_coherence = max(0.0, min(1.0, self.current_coherence))
        self.worldview_coherence_history.append((time.time(), self.current_coherence))
        
        # Record wisdom snapshot
        self._record_wisdom_snapshot()
        
    def add_belief_update(self, update: BeliefUpdate) -> None:
        """Record a belief update and update adaptability metrics."""
        self.belief_updates.append(update)
        self.belief_revision_count += 1
        
        # Update adaptability score based on how well evidence was integrated
        evidence_quality = abs(update.confidence_change)  # Larger changes indicate strong evidence
        self.evidence_integration_score = (
            0.9 * self.evidence_integration_score + 0.1 * evidence_quality
        )
        
        # Update coherence based on update impact
        self.current_coherence += update.coherence_impact * 0.05
        self.current_coherence = max(0.0, min(1.0, self.current_coherence))
        self.worldview_coherence_history.append((time.time(), self.current_coherence))
        
        self._record_wisdom_snapshot()
        
    def calculate_depth_score(self) -> float:
        """
        Calculate overall depth score.
        
        Depth is measured by:
        - Average depth of recent insights
        - Trend in depth over time (are insights getting deeper?)
        - Presence of fundamental/foundational insights
        """
        if not self.insights:
            return 0.0
            
        # Recent insights (last 20 or all if fewer)
        recent_insights = self.insights[-20:]
        avg_depth = sum(i.depth_score for i in recent_insights) / len(recent_insights)
        
        # Depth trend (comparing recent to earlier)
        if len(self.insights) > 10:
            early_avg = sum(i.depth_score for i in self.insights[:10]) / 10
            depth_trend = (avg_depth - early_avg) * 0.5  # Bonus for improvement
        else:
            depth_trend = 0.0
            
        # Fundamental insight bonus (insights with depth > 0.8)
        fundamental_count = sum(1 for i in self.insights if i.depth_score > 0.8)
        fundamental_bonus = min(0.2, fundamental_count * 0.02)
        
        return min(1.0, avg_depth + depth_trend + fundamental_bonus)
        
    def calculate_breadth_score(self) -> float:
        """
        Calculate overall breadth score.
        
        Breadth is measured by:
        - Number of distinct knowledge domains
        - Number of cross-domain connections
        - Distribution of knowledge across domains (not too concentrated)
        """
        if not self.domain_knowledge:
            return 0.0
            
        # Domain diversity (number of domains with meaningful knowledge)
        meaningful_domains = sum(1 for k in self.domain_knowledge.values() if k > 0.1)
        domain_diversity = min(1.0, meaningful_domains / 10.0)  # 10 domains = max
        
        # Cross-domain connections
        connection_count = len(self.domain_connections)
        connection_score = min(1.0, connection_count / 20.0)  # 20 connections = max
        
        # Knowledge distribution (entropy-like measure)
        total_knowledge = sum(self.domain_knowledge.values())
        if total_knowledge > 0:
            distribution_scores = [k / total_knowledge for k in self.domain_knowledge.values()]
            # Penalize concentration (want even distribution)
            concentration = max(distribution_scores) if distribution_scores else 0
            distribution_score = 1.0 - (concentration * 0.5)
        else:
            distribution_score = 0.0
            
        return (domain_diversity * 0.4 + connection_score * 0.4 + distribution_score * 0.2)
        
    def calculate_applicability_score(self) -> float:
        """
        Calculate overall applicability score.
        
        Applicability is measured by:
        - Average applicability of insights
        - Trend in applicability over time
        - Ratio of high-applicability to low-applicability insights
        """
        if not self.insights:
            return 0.0
            
        recent_insights = self.insights[-20:]
        avg_applicability = sum(i.applicability_score for i in recent_insights) / len(recent_insights)
        
        # High applicability ratio
        high_applicability_count = sum(1 for i in self.insights if i.applicability_score > 0.7)
        high_ratio = high_applicability_count / len(self.insights)
        
        return (avg_applicability * 0.7 + high_ratio * 0.3)
        
    def calculate_coherence_score(self) -> float:
        """
        Calculate overall coherence score.
        
        Coherence is measured by:
        - Current worldview coherence level
        - Stability of coherence over time
        - Trend in coherence (improving or degrading?)
        """
        if not self.worldview_coherence_history:
            return 0.5  # Neutral starting point
            
        # Current coherence is primary
        current = self.current_coherence
        
        # Coherence stability (low variance is good)
        if len(self.worldview_coherence_history) > 5:
            recent_coherence = [c for _, c in self.worldview_coherence_history[-10:]]
            variance = sum((c - current) ** 2 for c in recent_coherence) / len(recent_coherence)
            stability = max(0.0, 1.0 - variance)  # Lower variance = higher stability
        else:
            stability = 0.5
            
        # Coherence trend
        if len(self.worldview_coherence_history) > 10:
            early_avg = sum(c for _, c in self.worldview_coherence_history[:5]) / 5
            trend = (current - early_avg) * 0.5  # Bonus for improvement
        else:
            trend = 0.0
            
        return min(1.0, current * 0.6 + stability * 0.3 + trend * 0.1)
        
    def calculate_adaptability_score(self) -> float:
        """
        Calculate overall adaptability score.
        
        Adaptability is measured by:
        - Frequency of belief updates (willingness to revise)
        - Quality of evidence integration
        - Balance between stability and flexibility
        """
        if not self.belief_updates:
            return 0.5  # Neutral starting point
            
        # Update frequency (normalized by time and insights)
        if self.insights:
            update_ratio = len(self.belief_updates) / len(self.insights)
            frequency_score = min(1.0, update_ratio * 2.0)  # 0.5 ratio = max
        else:
            frequency_score = 0.0
            
        # Evidence integration quality
        integration_quality = self.evidence_integration_score
        
        # Balance: Too many updates might indicate instability
        # Too few might indicate rigidity
        if len(self.belief_updates) > 5:
            recent_updates = self.belief_updates[-10:]
            avg_coherence_impact = sum(abs(u.coherence_impact) for u in recent_updates) / len(recent_updates)
            balance_score = 1.0 - min(1.0, avg_coherence_impact * 2.0)  # Penalize large disruptions
        else:
            balance_score = 0.5
            
        return (frequency_score * 0.3 + integration_quality * 0.5 + balance_score * 0.2)
        
    def calculate_composite_wisdom_score(self) -> Dict[str, float]:
        """
        Calculate the composite wisdom score across all dimensions.
        
        Returns a dictionary with individual dimension scores and overall wisdom.
        """
        depth = self.calculate_depth_score()
        breadth = self.calculate_breadth_score()
        applicability = self.calculate_applicability_score()
        coherence = self.calculate_coherence_score()
        adaptability = self.calculate_adaptability_score()
        
        # Composite wisdom is weighted average
        # Depth and coherence are weighted higher as they're more fundamental
        composite = (
            depth * 0.25 +
            breadth * 0.20 +
            applicability * 0.20 +
            coherence * 0.25 +
            adaptability * 0.10
        )
        
        return {
            'depth': depth,
            'breadth': breadth,
            'applicability': applicability,
            'coherence': coherence,
            'adaptability': adaptability,
            'composite_wisdom': composite
        }
        
    def _record_wisdom_snapshot(self) -> None:
        """Record current wisdom scores for historical tracking."""
        scores = self.calculate_composite_wisdom_score()
        self.wisdom_history.append((time.time(), scores))
        
    def get_wisdom_growth_rate(self, window_minutes: int = 60) -> float:
        """
        Calculate the rate of wisdom growth over a time window.
        
        Returns the change in composite wisdom per hour.
        """
        if len(self.wisdom_history) < 2:
            return 0.0
            
        current_time = time.time()
        window_seconds = window_minutes * 60
        
        # Get snapshots within window
        recent_snapshots = [
            (t, scores) for t, scores in self.wisdom_history
            if current_time - t <= window_seconds
        ]
        
        if len(recent_snapshots) < 2:
            return 0.0
            
        # Calculate growth rate
        earliest = recent_snapshots[0]
        latest = recent_snapshots[-1]
        
        time_diff_hours = (latest[0] - earliest[0]) / 3600.0
        wisdom_diff = latest[1]['composite_wisdom'] - earliest[1]['composite_wisdom']
        
        if time_diff_hours > 0:
            return wisdom_diff / time_diff_hours
        return 0.0
        
    def get_metrics_summary(self) -> Dict:
        """Get a comprehensive summary of all wisdom metrics."""
        scores = self.calculate_composite_wisdom_score()
        growth_rate = self.get_wisdom_growth_rate()
        
        return {
            'wisdom_scores': scores,
            'growth_rate_per_hour': growth_rate,
            'total_insights': len(self.insights),
            'total_belief_updates': len(self.belief_updates),
            'domains_explored': len(self.domain_knowledge),
            'cross_domain_connections': len(self.domain_connections),
            'current_coherence': self.current_coherence,
            'evidence_integration_quality': self.evidence_integration_score
        }
        
    def save_to_file(self, filepath: str) -> None:
        """Save wisdom metrics to JSON file."""
        data = {
            'insights': [
                {
                    'id': i.id,
                    'content': i.content,
                    'timestamp': i.timestamp,
                    'depth_score': i.depth_score,
                    'breadth_score': i.breadth_score,
                    'applicability_score': i.applicability_score,
                    'coherence_contribution': i.coherence_contribution,
                    'related_domains': i.related_domains
                }
                for i in self.insights
            ],
            'belief_updates': [
                {
                    'id': u.id,
                    'timestamp': u.timestamp,
                    'prior_belief': u.prior_belief,
                    'updated_belief': u.updated_belief,
                    'evidence': u.evidence,
                    'confidence_change': u.confidence_change,
                    'coherence_impact': u.coherence_impact
                }
                for u in self.belief_updates
            ],
            'domain_knowledge': dict(self.domain_knowledge),
            'current_metrics': self.get_metrics_summary()
        }
        
        with open(filepath, 'w') as f:
            json.dump(data, f, indent=2)
            
    @classmethod
    def load_from_file(cls, filepath: str) -> 'WisdomMetrics':
        """Load wisdom metrics from JSON file."""
        with open(filepath, 'r') as f:
            data = json.load(f)
            
        metrics = cls()
        
        # Restore insights
        for i_data in data.get('insights', []):
            insight = WisdomInsight(
                id=i_data['id'],
                content=i_data['content'],
                timestamp=i_data['timestamp'],
                depth_score=i_data['depth_score'],
                breadth_score=i_data['breadth_score'],
                applicability_score=i_data['applicability_score'],
                coherence_contribution=i_data['coherence_contribution'],
                related_domains=i_data['related_domains']
            )
            metrics.insights.append(insight)
            
        # Restore belief updates
        for u_data in data.get('belief_updates', []):
            update = BeliefUpdate(
                id=u_data['id'],
                timestamp=u_data['timestamp'],
                prior_belief=u_data['prior_belief'],
                updated_belief=u_data['updated_belief'],
                evidence=u_data['evidence'],
                confidence_change=u_data['confidence_change'],
                coherence_impact=u_data['coherence_impact']
            )
            metrics.belief_updates.append(update)
            
        # Restore domain knowledge
        metrics.domain_knowledge = defaultdict(float, data.get('domain_knowledge', {}))
        
        # Recalculate derived metrics
        metrics._rebuild_derived_metrics()
        
        return metrics
        
    def _rebuild_derived_metrics(self) -> None:
        """Rebuild derived metrics from loaded data."""
        # Rebuild domain connections
        for insight in self.insights:
            for i, domain1 in enumerate(insight.related_domains):
                for domain2 in insight.related_domains[i+1:]:
                    key = tuple(sorted([domain1, domain2]))
                    self.domain_connections[key] += 1
                    
        # Rebuild coherence history
        self.current_coherence = 0.5
        for insight in self.insights:
            self.current_coherence += insight.coherence_contribution * 0.05
            self.current_coherence = max(0.0, min(1.0, self.current_coherence))
            self.worldview_coherence_history.append((insight.timestamp, self.current_coherence))
            
        for update in self.belief_updates:
            self.current_coherence += update.coherence_impact * 0.05
            self.current_coherence = max(0.0, min(1.0, self.current_coherence))
            self.worldview_coherence_history.append((update.timestamp, self.current_coherence))
            
        # Rebuild wisdom history
        for insight in self.insights:
            self._record_wisdom_snapshot()
