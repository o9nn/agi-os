"""Data models for autognosis system."""

from dataclasses import dataclass, field
from typing import Dict, List, Any, Optional
from datetime import datetime
import hashlib
import json


@dataclass
class BehavioralPattern:
    """Represents a detected behavioral pattern."""
    
    pattern_type: str
    description: str
    confidence: float
    observations: List[str] = field(default_factory=list)
    timestamp: datetime = field(default_factory=datetime.now)


@dataclass
class ComponentState:
    """Represents the state of a system component."""
    
    component_id: str
    status: str  # "active", "idle", "processing", etc.
    metrics: Dict[str, float] = field(default_factory=dict)
    timestamp: datetime = field(default_factory=datetime.now)


@dataclass
class SelfImage:
    """Hierarchical self-image at a specific cognitive level."""
    
    level: int
    confidence: float
    component_states: List[ComponentState] = field(default_factory=list)
    behavioral_patterns: List[BehavioralPattern] = field(default_factory=list)
    performance_metrics: Dict[str, float] = field(default_factory=dict)
    cognitive_processes: List[str] = field(default_factory=list)
    meta_reflections: List[str] = field(default_factory=list)
    timestamp: datetime = field(default_factory=datetime.now)
    
    @property
    def image_id(self) -> str:
        """Generate unique ID for this self-image."""
        data = {
            'level': self.level,
            'timestamp': self.timestamp.isoformat(),
            'confidence': self.confidence,
            'num_patterns': len(self.behavioral_patterns),
            'num_reflections': len(self.meta_reflections),
        }
        return hashlib.sha256(json.dumps(data, sort_keys=True).encode()).hexdigest()[:16]


@dataclass
class MetaCognitiveInsight:
    """Insight generated from self-analysis."""
    
    insight_type: str
    description: str
    severity: str  # "low", "medium", "high"
    confidence: float
    related_patterns: List[str] = field(default_factory=list)
    timestamp: datetime = field(default_factory=datetime.now)


@dataclass
class OptimizationOpportunity:
    """Discovered opportunity for self-optimization."""
    
    opportunity_type: str
    description: str
    priority: str  # "low", "medium", "high"
    risk_level: str  # "low", "medium", "high"
    estimated_impact: float
    proposed_actions: List[str] = field(default_factory=list)
    timestamp: datetime = field(default_factory=datetime.now)


@dataclass
class SystemObservation:
    """Observation of system state at a point in time."""
    
    component_states: List[ComponentState]
    resource_utilization: Dict[str, float]
    event_queue_status: Dict[str, Any]
    performance_metrics: Dict[str, float]
    timestamp: datetime = field(default_factory=datetime.now)
