import time
from typing import Dict, Optional, List
from dataclasses import dataclass
from enum import Enum
import math
class ConsciousnessState(Enum):
    AWAKE = 'awake'
    DROWSY = 'drowsy'
    RESTING = 'resting'
    DEEP_REST = 'deep_rest'
    WAKING = 'waking'
@dataclass
class CognitiveFatigueMetrics:
    processing_quality: float
    coherence_level: float
    response_latency: float
    error_rate: float
    attention_span: float
@dataclass
class MemoryConsolidationMetrics:
    unconsolidated_memories: int
    consolidation_pressure: float
    memory_buffer_utilization: float
    last_consolidation_time: float
    consolidation_quality: float
class AutonomousWakeRestController:
    def __init__(self):
        self.state = ConsciousnessState.AWAKE
        self.state_entered_time = time.time()
        self.cognitive_fatigue = 0.0
        self.fatigue_accumulation_rate = 0.01
        self.fatigue_recovery_rate = 0.05
        self.consolidation_pressure = 0.0
        self.consolidation_threshold = 0.7
        self.drowsy_threshold = 0.6
        self.rest_threshold = 0.75
        self.wake_threshold = 0.2
        self.activity_duration = 0.0
        self.rest_duration = 0.0
        self.total_cycles = 0
        self.optimal_activity_duration = 3600.0
        self.optimal_rest_duration = 600.0
        self.recent_processing_quality: List[float] = []
        self.recent_coherence_levels: List[float] = []
        self.circadian_enabled = False
        self.circadian_period = 86400.0
        self.circadian_phase = 0.0
    def update(self, processing_quality: float=0.8, coherence_level: float=0.8, new_memories: int=0, consolidation_occurred: bool=False) -> ConsciousnessState:
        current_time = time.time()
        time_in_state = current_time - self.state_entered_time
        self.recent_processing_quality.append(processing_quality)
        self.recent_coherence_levels.append(coherence_level)
        if len(self.recent_processing_quality) > 20:
            self.recent_processing_quality = self.recent_processing_quality[-20:]
        if len(self.recent_coherence_levels) > 20:
            self.recent_coherence_levels = self.recent_coherence_levels[-20:]
        if self.state in [ConsciousnessState.AWAKE, ConsciousnessState.DROWSY]:
            self._update_awake_state(processing_quality, coherence_level, new_memories)
        elif self.state in [ConsciousnessState.RESTING, ConsciousnessState.DEEP_REST]:
            self._update_resting_state(consolidation_occurred)
        new_state = self._check_state_transitions()
        if new_state != self.state:
            self._transition_to_state(new_state)
        return self.state
    def _update_awake_state(self, processing_quality: float, coherence_level: float, new_memories: int) -> None:
        current_time = time.time()
        time_in_state = current_time - self.state_entered_time
        minutes_active = time_in_state / 60.0
        self.cognitive_fatigue += self.fatigue_accumulation_rate * minutes_active
        quality_factor = 1.0 - processing_quality
        self.cognitive_fatigue += quality_factor * 0.005 * minutes_active
        coherence_factor = 1.0 - coherence_level
        self.cognitive_fatigue += coherence_factor * 0.005 * minutes_active
        self.cognitive_fatigue = min(1.0, self.cognitive_fatigue)
        self.consolidation_pressure += new_memories * 0.01
        self.consolidation_pressure = min(1.0, self.consolidation_pressure)
        self.activity_duration = time_in_state
    def _update_resting_state(self, consolidation_occurred: bool) -> None:
        current_time = time.time()
        time_in_state = current_time - self.state_entered_time
        minutes_resting = time_in_state / 60.0
        self.cognitive_fatigue -= self.fatigue_recovery_rate * minutes_resting
        self.cognitive_fatigue = max(0.0, self.cognitive_fatigue)
        if consolidation_occurred:
            self.consolidation_pressure *= 0.5
        self.rest_duration = time_in_state
    def _check_state_transitions(self) -> ConsciousnessState:
        current_time = time.time()
        time_in_state = current_time - self.state_entered_time
        if self.state == ConsciousnessState.AWAKE:
            if self.cognitive_fatigue >= self.drowsy_threshold:
                return ConsciousnessState.DROWSY
            if self.consolidation_pressure >= self.consolidation_threshold:
                return ConsciousnessState.DROWSY
        elif self.state == ConsciousnessState.DROWSY:
            if self.cognitive_fatigue >= self.rest_threshold:
                return ConsciousnessState.RESTING
            if self.consolidation_pressure >= self.consolidation_threshold:
                return ConsciousnessState.RESTING
            if self.cognitive_fatigue < self.drowsy_threshold * 0.8:
                return ConsciousnessState.AWAKE
        elif self.state == ConsciousnessState.RESTING:
            if time_in_state > 120.0:
                return ConsciousnessState.DEEP_REST
            if self.cognitive_fatigue < self.wake_threshold and self.consolidation_pressure < 0.3:
                return ConsciousnessState.WAKING
        elif self.state == ConsciousnessState.DEEP_REST:
            if self.cognitive_fatigue < self.wake_threshold and self.consolidation_pressure < 0.2:
                return ConsciousnessState.WAKING
            if time_in_state > self.optimal_rest_duration and self.cognitive_fatigue < 0.3:
                return ConsciousnessState.WAKING
        elif self.state == ConsciousnessState.WAKING:
            if time_in_state > 10.0:
                return ConsciousnessState.AWAKE
        return self.state
    def _transition_to_state(self, new_state: ConsciousnessState) -> None:
        old_state = self.state
        self.state = new_state
        self.state_entered_time = time.time()
        print(f'🌙 Consciousness State Transition: {old_state.value} → {new_state.value}')
        if new_state == ConsciousnessState.RESTING:
            print(f'   💤 Initiating rest cycle (fatigue: {self.cognitive_fatigue:.2f}, consolidation pressure: {self.consolidation_pressure:.2f})')
            print(f'   ⏱️  Activity duration: {self.activity_duration / 60:.1f} minutes')
        elif new_state == ConsciousnessState.WAKING:
            print(f'   ☀️  Waking up (fatigue: {self.cognitive_fatigue:.2f}, rest duration: {self.rest_duration / 60:.1f} minutes)')
            self.total_cycles += 1
            self._update_optimal_durations()
        elif new_state == ConsciousnessState.AWAKE:
            print(f'   ✨ Fully awake and ready (cycle #{self.total_cycles})')
    def _update_optimal_durations(self) -> None:
        alpha = 0.1
        avg_quality = sum(self.recent_processing_quality) / max(1, len(self.recent_processing_quality))
        if avg_quality > 0.7:
            self.optimal_activity_duration = (1 - alpha) * self.optimal_activity_duration + alpha * self.activity_duration
            self.optimal_rest_duration = (1 - alpha) * self.optimal_rest_duration + alpha * self.rest_duration
        else:
            self.optimal_activity_duration *= 0.95
            self.optimal_rest_duration *= 1.05
    def should_rest_now(self) -> bool:
        return self.cognitive_fatigue >= self.rest_threshold or self.consolidation_pressure >= self.consolidation_threshold
    def should_wake_now(self) -> bool:
        return self.state in [ConsciousnessState.RESTING, ConsciousnessState.DEEP_REST] and self.cognitive_fatigue < self.wake_threshold and (self.consolidation_pressure < 0.3)
    def get_fatigue_metrics(self) -> CognitiveFatigueMetrics:
        avg_quality = sum(self.recent_processing_quality) / max(1, len(self.recent_processing_quality))
        avg_coherence = sum(self.recent_coherence_levels) / max(1, len(self.recent_coherence_levels))
        response_latency = 1.0 + self.cognitive_fatigue * 2.0
        error_rate = self.cognitive_fatigue * 0.3
        attention_span = 1.0 - self.cognitive_fatigue * 0.8
        return CognitiveFatigueMetrics(processing_quality=avg_quality, coherence_level=avg_coherence, response_latency=response_latency, error_rate=error_rate, attention_span=attention_span)
    def get_consolidation_metrics(self) -> MemoryConsolidationMetrics:
        unconsolidated = int(self.consolidation_pressure * 100)
        buffer_util = self.consolidation_pressure
        quality = 1.0 - self.consolidation_pressure * 0.5
        return MemoryConsolidationMetrics(unconsolidated_memories=unconsolidated, consolidation_pressure=self.consolidation_pressure, memory_buffer_utilization=buffer_util, last_consolidation_time=self.state_entered_time if self.state == ConsciousnessState.RESTING else 0, consolidation_quality=quality)
    def get_metrics_summary(self) -> Dict:
        fatigue = self.get_fatigue_metrics()
        consolidation = self.get_consolidation_metrics()
        return {'consciousness_state': self.state.value, 'time_in_state_minutes': (time.time() - self.state_entered_time) / 60.0, 'cognitive_fatigue': self.cognitive_fatigue, 'consolidation_pressure': self.consolidation_pressure, 'total_cycles': self.total_cycles, 'optimal_activity_minutes': self.optimal_activity_duration / 60.0, 'optimal_rest_minutes': self.optimal_rest_duration / 60.0, 'fatigue_metrics': {'processing_quality': fatigue.processing_quality, 'coherence_level': fatigue.coherence_level, 'response_latency': fatigue.response_latency, 'error_rate': fatigue.error_rate, 'attention_span': fatigue.attention_span}, 'consolidation_metrics': {'unconsolidated_memories': consolidation.unconsolidated_memories, 'consolidation_pressure': consolidation.consolidation_pressure, 'buffer_utilization': consolidation.memory_buffer_utilization, 'consolidation_quality': consolidation.consolidation_quality}}
    def force_rest(self) -> None:
        self._transition_to_state(ConsciousnessState.RESTING)
    def force_wake(self) -> None:
        self._transition_to_state(ConsciousnessState.AWAKE)
        self.cognitive_fatigue = 0.0
        self.consolidation_pressure = 0.0