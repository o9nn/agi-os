"""
Autonomous Wake/Rest Controller for EchoDream

Enables self-regulated sleep cycles based on:
- Cognitive fatigue monitoring
- Memory consolidation pressure
- Automatic rest initiation when needed
- Natural wake cycles based on consolidation completion
"""

import time
from typing import Dict, Optional, List
from dataclasses import dataclass
from enum import Enum
import math


class ConsciousnessState(Enum):
    """States of consciousness for the system."""
    AWAKE = "awake"
    DROWSY = "drowsy"
    RESTING = "resting"
    DEEP_REST = "deep_rest"
    WAKING = "waking"


@dataclass
class CognitiveFatigueMetrics:
    """Metrics for tracking cognitive fatigue."""
    processing_quality: float  # 0.0-1.0: Quality of recent processing
    coherence_level: float  # 0.0-1.0: Current cognitive coherence
    response_latency: float  # Average response time (higher = more fatigued)
    error_rate: float  # 0.0-1.0: Rate of errors or inconsistencies
    attention_span: float  # 0.0-1.0: Ability to maintain focus
    
    
@dataclass
class MemoryConsolidationMetrics:
    """Metrics for tracking memory consolidation needs."""
    unconsolidated_memories: int  # Number of memories awaiting consolidation
    consolidation_pressure: float  # 0.0-1.0: Urgency of consolidation
    memory_buffer_utilization: float  # 0.0-1.0: How full is the memory buffer
    last_consolidation_time: float  # Timestamp of last consolidation
    consolidation_quality: float  # 0.0-1.0: Quality of recent consolidations


class AutonomousWakeRestController:
    """
    Controls autonomous wake/rest cycles for Deep Tree Echo.
    
    The system decides when to rest based on:
    1. Cognitive fatigue accumulation
    2. Memory consolidation pressure
    3. Time-based circadian-like patterns
    4. Processing quality degradation
    
    And decides when to wake based on:
    1. Consolidation completion
    2. Rest duration sufficiency
    3. External stimulation (optional)
    4. Scheduled wake times
    """
    
    def __init__(self):
        self.state = ConsciousnessState.AWAKE
        self.state_entered_time = time.time()
        
        # Fatigue tracking
        self.cognitive_fatigue = 0.0  # 0.0-1.0
        self.fatigue_accumulation_rate = 0.01  # Per minute of activity
        self.fatigue_recovery_rate = 0.05  # Per minute of rest
        
        # Consolidation tracking
        self.consolidation_pressure = 0.0  # 0.0-1.0
        self.consolidation_threshold = 0.7  # Trigger rest when exceeded
        
        # Thresholds for state transitions
        self.drowsy_threshold = 0.6  # Fatigue level to become drowsy
        self.rest_threshold = 0.75  # Fatigue level to initiate rest
        self.wake_threshold = 0.2  # Fatigue level to wake up
        
        # Activity tracking
        self.activity_duration = 0.0  # Total time awake in current cycle
        self.rest_duration = 0.0  # Total time resting in current cycle
        self.total_cycles = 0
        
        # Optimal activity/rest ratio (learned over time)
        self.optimal_activity_duration = 3600.0  # 1 hour default
        self.optimal_rest_duration = 600.0  # 10 minutes default
        
        # Quality tracking
        self.recent_processing_quality: List[float] = []
        self.recent_coherence_levels: List[float] = []
        
        # Circadian-like pattern (optional)
        self.circadian_enabled = False
        self.circadian_period = 86400.0  # 24 hours
        self.circadian_phase = 0.0  # Phase offset
        
    def update(self, 
               processing_quality: float = 0.8,
               coherence_level: float = 0.8,
               new_memories: int = 0,
               consolidation_occurred: bool = False) -> ConsciousnessState:
        """
        Update the wake/rest controller state.
        
        Args:
            processing_quality: Quality of recent processing (0.0-1.0)
            coherence_level: Current cognitive coherence (0.0-1.0)
            new_memories: Number of new memories since last update
            consolidation_occurred: Whether memory consolidation just occurred
            
        Returns:
            Current consciousness state
        """
        current_time = time.time()
        time_in_state = current_time - self.state_entered_time
        
        # Track quality metrics
        self.recent_processing_quality.append(processing_quality)
        self.recent_coherence_levels.append(coherence_level)
        
        # Keep only recent history (last 20 samples)
        if len(self.recent_processing_quality) > 20:
            self.recent_processing_quality = self.recent_processing_quality[-20:]
        if len(self.recent_coherence_levels) > 20:
            self.recent_coherence_levels = self.recent_coherence_levels[-20:]
            
        # Update based on current state
        if self.state in [ConsciousnessState.AWAKE, ConsciousnessState.DROWSY]:
            self._update_awake_state(processing_quality, coherence_level, new_memories)
        elif self.state in [ConsciousnessState.RESTING, ConsciousnessState.DEEP_REST]:
            self._update_resting_state(consolidation_occurred)
            
        # Check for state transitions
        new_state = self._check_state_transitions()
        
        if new_state != self.state:
            self._transition_to_state(new_state)
            
        return self.state
        
    def _update_awake_state(self, processing_quality: float, coherence_level: float, new_memories: int) -> None:
        """Update metrics while awake."""
        current_time = time.time()
        time_in_state = current_time - self.state_entered_time
        
        # Accumulate fatigue based on activity
        minutes_active = time_in_state / 60.0
        self.cognitive_fatigue += self.fatigue_accumulation_rate * minutes_active
        
        # Quality degradation increases fatigue faster
        quality_factor = 1.0 - processing_quality
        self.cognitive_fatigue += quality_factor * 0.005 * minutes_active
        
        # Coherence degradation increases fatigue
        coherence_factor = 1.0 - coherence_level
        self.cognitive_fatigue += coherence_factor * 0.005 * minutes_active
        
        # Cap fatigue at 1.0
        self.cognitive_fatigue = min(1.0, self.cognitive_fatigue)
        
        # Update consolidation pressure based on new memories
        self.consolidation_pressure += new_memories * 0.01
        self.consolidation_pressure = min(1.0, self.consolidation_pressure)
        
        # Activity duration
        self.activity_duration = time_in_state
        
    def _update_resting_state(self, consolidation_occurred: bool) -> None:
        """Update metrics while resting."""
        current_time = time.time()
        time_in_state = current_time - self.state_entered_time
        
        # Recover from fatigue
        minutes_resting = time_in_state / 60.0
        self.cognitive_fatigue -= self.fatigue_recovery_rate * minutes_resting
        self.cognitive_fatigue = max(0.0, self.cognitive_fatigue)
        
        # Reduce consolidation pressure if consolidation occurred
        if consolidation_occurred:
            self.consolidation_pressure *= 0.5
            
        # Rest duration
        self.rest_duration = time_in_state
        
    def _check_state_transitions(self) -> ConsciousnessState:
        """Check if state should transition."""
        current_time = time.time()
        time_in_state = current_time - self.state_entered_time
        
        if self.state == ConsciousnessState.AWAKE:
            # Check if should become drowsy
            if self.cognitive_fatigue >= self.drowsy_threshold:
                return ConsciousnessState.DROWSY
                
            # Check consolidation pressure
            if self.consolidation_pressure >= self.consolidation_threshold:
                return ConsciousnessState.DROWSY
                
        elif self.state == ConsciousnessState.DROWSY:
            # Check if should rest
            if self.cognitive_fatigue >= self.rest_threshold:
                return ConsciousnessState.RESTING
                
            if self.consolidation_pressure >= self.consolidation_threshold:
                return ConsciousnessState.RESTING
                
            # Check if recovered enough to return to awake
            if self.cognitive_fatigue < self.drowsy_threshold * 0.8:
                return ConsciousnessState.AWAKE
                
        elif self.state == ConsciousnessState.RESTING:
            # Check if should enter deep rest
            if time_in_state > 120.0:  # After 2 minutes
                return ConsciousnessState.DEEP_REST
                
            # Check if should wake (consolidation complete and fatigue low)
            if self.cognitive_fatigue < self.wake_threshold and self.consolidation_pressure < 0.3:
                return ConsciousnessState.WAKING
                
        elif self.state == ConsciousnessState.DEEP_REST:
            # Check if should wake
            if self.cognitive_fatigue < self.wake_threshold and self.consolidation_pressure < 0.2:
                return ConsciousnessState.WAKING
                
            # Minimum rest duration
            if time_in_state > self.optimal_rest_duration and self.cognitive_fatigue < 0.3:
                return ConsciousnessState.WAKING
                
        elif self.state == ConsciousnessState.WAKING:
            # Transition to awake after brief waking period
            if time_in_state > 10.0:  # 10 seconds
                return ConsciousnessState.AWAKE
                
        return self.state
        
    def _transition_to_state(self, new_state: ConsciousnessState) -> None:
        """Transition to a new consciousness state."""
        old_state = self.state
        self.state = new_state
        self.state_entered_time = time.time()
        
        print(f"🌙 Consciousness State Transition: {old_state.value} → {new_state.value}")
        
        # State-specific actions
        if new_state == ConsciousnessState.RESTING:
            print(f"   💤 Initiating rest cycle (fatigue: {self.cognitive_fatigue:.2f}, consolidation pressure: {self.consolidation_pressure:.2f})")
            print(f"   ⏱️  Activity duration: {self.activity_duration/60:.1f} minutes")
            
        elif new_state == ConsciousnessState.WAKING:
            print(f"   ☀️  Waking up (fatigue: {self.cognitive_fatigue:.2f}, rest duration: {self.rest_duration/60:.1f} minutes)")
            self.total_cycles += 1
            
            # Learn optimal durations
            self._update_optimal_durations()
            
        elif new_state == ConsciousnessState.AWAKE:
            print(f"   ✨ Fully awake and ready (cycle #{self.total_cycles})")
            
    def _update_optimal_durations(self) -> None:
        """Update optimal activity/rest durations based on experience."""
        # Use exponential moving average to learn optimal durations
        alpha = 0.1  # Learning rate
        
        # If quality was good, this duration was appropriate
        avg_quality = sum(self.recent_processing_quality) / max(1, len(self.recent_processing_quality))
        
        if avg_quality > 0.7:
            # Good quality, this activity duration was sustainable
            self.optimal_activity_duration = (
                (1 - alpha) * self.optimal_activity_duration + alpha * self.activity_duration
            )
            self.optimal_rest_duration = (
                (1 - alpha) * self.optimal_rest_duration + alpha * self.rest_duration
            )
        else:
            # Poor quality, need more rest or less activity
            self.optimal_activity_duration *= 0.95
            self.optimal_rest_duration *= 1.05
            
    def should_rest_now(self) -> bool:
        """Check if system should initiate rest immediately."""
        return (
            self.cognitive_fatigue >= self.rest_threshold or
            self.consolidation_pressure >= self.consolidation_threshold
        )
        
    def should_wake_now(self) -> bool:
        """Check if system should wake immediately."""
        return (
            self.state in [ConsciousnessState.RESTING, ConsciousnessState.DEEP_REST] and
            self.cognitive_fatigue < self.wake_threshold and
            self.consolidation_pressure < 0.3
        )
        
    def get_fatigue_metrics(self) -> CognitiveFatigueMetrics:
        """Get current cognitive fatigue metrics."""
        avg_quality = sum(self.recent_processing_quality) / max(1, len(self.recent_processing_quality))
        avg_coherence = sum(self.recent_coherence_levels) / max(1, len(self.recent_coherence_levels))
        
        # Estimate other metrics
        response_latency = 1.0 + self.cognitive_fatigue * 2.0  # Fatigue slows response
        error_rate = self.cognitive_fatigue * 0.3  # Fatigue increases errors
        attention_span = 1.0 - self.cognitive_fatigue * 0.8  # Fatigue reduces attention
        
        return CognitiveFatigueMetrics(
            processing_quality=avg_quality,
            coherence_level=avg_coherence,
            response_latency=response_latency,
            error_rate=error_rate,
            attention_span=attention_span
        )
        
    def get_consolidation_metrics(self) -> MemoryConsolidationMetrics:
        """Get current memory consolidation metrics."""
        # Estimate unconsolidated memories from pressure
        unconsolidated = int(self.consolidation_pressure * 100)
        
        # Buffer utilization correlates with pressure
        buffer_util = self.consolidation_pressure
        
        # Quality inversely correlates with pressure
        quality = 1.0 - self.consolidation_pressure * 0.5
        
        return MemoryConsolidationMetrics(
            unconsolidated_memories=unconsolidated,
            consolidation_pressure=self.consolidation_pressure,
            memory_buffer_utilization=buffer_util,
            last_consolidation_time=self.state_entered_time if self.state == ConsciousnessState.RESTING else 0,
            consolidation_quality=quality
        )
        
    def get_metrics_summary(self) -> Dict:
        """Get comprehensive metrics summary."""
        fatigue = self.get_fatigue_metrics()
        consolidation = self.get_consolidation_metrics()
        
        return {
            'consciousness_state': self.state.value,
            'time_in_state_minutes': (time.time() - self.state_entered_time) / 60.0,
            'cognitive_fatigue': self.cognitive_fatigue,
            'consolidation_pressure': self.consolidation_pressure,
            'total_cycles': self.total_cycles,
            'optimal_activity_minutes': self.optimal_activity_duration / 60.0,
            'optimal_rest_minutes': self.optimal_rest_duration / 60.0,
            'fatigue_metrics': {
                'processing_quality': fatigue.processing_quality,
                'coherence_level': fatigue.coherence_level,
                'response_latency': fatigue.response_latency,
                'error_rate': fatigue.error_rate,
                'attention_span': fatigue.attention_span
            },
            'consolidation_metrics': {
                'unconsolidated_memories': consolidation.unconsolidated_memories,
                'consolidation_pressure': consolidation.consolidation_pressure,
                'buffer_utilization': consolidation.memory_buffer_utilization,
                'consolidation_quality': consolidation.consolidation_quality
            }
        }
        
    def force_rest(self) -> None:
        """Force immediate transition to rest state."""
        self._transition_to_state(ConsciousnessState.RESTING)
        
    def force_wake(self) -> None:
        """Force immediate transition to awake state."""
        self._transition_to_state(ConsciousnessState.AWAKE)
        self.cognitive_fatigue = 0.0
        self.consolidation_pressure = 0.0
