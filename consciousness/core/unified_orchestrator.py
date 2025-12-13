"""
Unified Orchestrator for Deep Tree Echo

Coordinates all major subsystems:
- EchoBeats 12-step cognitive loop
- EchoDream knowledge consolidation
- Embodied emotion dynamics
- Theory of mind updates
- Interest pattern tracking
- Wisdom cultivation
- Autonomous wake/rest cycles
"""

import time
import asyncio
from typing import Dict, List, Optional, Any
from dataclasses import dataclass
from enum import Enum
import json

# Import subsystem components (these would be actual imports in production)
# For now, we'll define interfaces


class OrchestrationPhase(Enum):
    """Phases of the orchestration cycle."""
    PERCEPTION = "perception"  # Gather inputs and context
    COGNITION = "cognition"  # Process through EchoBeats
    EMOTION = "emotion"  # Update emotional state
    SOCIAL = "social"  # Update theory of mind models
    LEARNING = "learning"  # Update interest patterns and wisdom
    CONSOLIDATION = "consolidation"  # Memory consolidation (if resting)
    INTEGRATION = "integration"  # Integrate all updates


@dataclass
class OrchestrationContext:
    """Context passed between orchestration phases."""
    timestamp: float
    phase: OrchestrationPhase
    inputs: Dict[str, Any]
    outputs: Dict[str, Any]
    metadata: Dict[str, Any]


class UnifiedOrchestrator:
    """
    Master orchestrator that coordinates all Deep Tree Echo subsystems.
    
    The orchestrator runs a continuous cycle that:
    1. Monitors wake/rest state
    2. Coordinates EchoBeats cognitive processing
    3. Updates emotional state based on processing
    4. Maintains theory of mind models
    5. Tracks interest patterns
    6. Measures wisdom cultivation
    7. Manages memory consolidation
    8. Integrates all subsystem outputs
    """
    
    def __init__(self,
                 echobeats=None,
                 echodream=None,
                 emotion_system=None,
                 theory_of_mind=None,
                 interest_tracker=None,
                 wisdom_metrics=None,
                 wake_rest_controller=None):
        """
        Initialize the unified orchestrator.
        
        Args:
            echobeats: EchoBeats three-phase cognitive loop
            echodream: EchoDream knowledge integration system
            emotion_system: Embodied emotion system
            theory_of_mind: Theory of mind module
            interest_tracker: Echo interest pattern tracker
            wisdom_metrics: Wisdom cultivation metrics
            wake_rest_controller: Autonomous wake/rest controller
        """
        # Subsystems
        self.echobeats = echobeats
        self.echodream = echodream
        self.emotion_system = emotion_system
        self.theory_of_mind = theory_of_mind
        self.interest_tracker = interest_tracker
        self.wisdom_metrics = wisdom_metrics
        self.wake_rest_controller = wake_rest_controller
        
        # Orchestration state
        self.running = False
        self.cycle_count = 0
        self.current_phase = OrchestrationPhase.PERCEPTION
        
        # Performance metrics
        self.cycle_times: List[float] = []
        self.phase_times: Dict[OrchestrationPhase, List[float]] = {
            phase: [] for phase in OrchestrationPhase
        }
        
        # Integration state
        self.current_context: Optional[OrchestrationContext] = None
        self.context_history: List[OrchestrationContext] = []
        
        # Goals and priorities
        self.active_goals: List[Dict] = []
        self.goal_priorities: Dict[str, float] = {}
        
    async def start(self) -> None:
        """Start the unified orchestration loop."""
        if self.running:
            print("⚠️  Orchestrator already running")
            return
            
        self.running = True
        print("🎭 ═══════════════════════════════════════════════════════")
        print("🎭 Unified Orchestrator: Starting Deep Tree Echo")
        print("🎭 ═══════════════════════════════════════════════════════")
        print("🎭 Coordinating subsystems:")
        print(f"🎭   - EchoBeats: {'✓' if self.echobeats else '✗'}")
        print(f"🎭   - EchoDream: {'✓' if self.echodream else '✗'}")
        print(f"🎭   - Emotion System: {'✓' if self.emotion_system else '✗'}")
        print(f"🎭   - Theory of Mind: {'✓' if self.theory_of_mind else '✗'}")
        print(f"🎭   - Interest Tracker: {'✓' if self.interest_tracker else '✗'}")
        print(f"🎭   - Wisdom Metrics: {'✓' if self.wisdom_metrics else '✗'}")
        print(f"🎭   - Wake/Rest Controller: {'✓' if self.wake_rest_controller else '✗'}")
        print("🎭 ═══════════════════════════════════════════════════════\n")
        
        # Start subsystems
        await self._start_subsystems()
        
        # Run orchestration loop
        await self._orchestration_loop()
        
    async def stop(self) -> None:
        """Stop the unified orchestration loop."""
        if not self.running:
            print("⚠️  Orchestrator not running")
            return
            
        self.running = False
        print("\n🎭 Stopping Unified Orchestrator...")
        
        # Stop subsystems
        await self._stop_subsystems()
        
        # Print summary
        self._print_summary()
        
    async def _start_subsystems(self) -> None:
        """Start all available subsystems."""
        if self.echobeats:
            try:
                await asyncio.to_thread(self.echobeats.Start)
                print("✓ EchoBeats started")
            except Exception as e:
                print(f"✗ EchoBeats start failed: {e}")
                
        if self.echodream:
            try:
                await asyncio.to_thread(self.echodream.Start)
                print("✓ EchoDream started")
            except Exception as e:
                print(f"✗ EchoDream start failed: {e}")
                
    async def _stop_subsystems(self) -> None:
        """Stop all running subsystems."""
        if self.echobeats:
            try:
                await asyncio.to_thread(self.echobeats.Stop)
            except Exception as e:
                print(f"✗ EchoBeats stop failed: {e}")
                
        if self.echodream:
            try:
                await asyncio.to_thread(self.echodream.Stop)
            except Exception as e:
                print(f"✗ EchoDream stop failed: {e}")
                
    async def _orchestration_loop(self) -> None:
        """Main orchestration loop."""
        while self.running:
            cycle_start = time.time()
            
            try:
                # Check wake/rest state
                await self._check_wake_rest_state()
                
                # Run orchestration cycle
                await self._run_orchestration_cycle()
                
                # Update metrics
                cycle_time = time.time() - cycle_start
                self.cycle_times.append(cycle_time)
                if len(self.cycle_times) > 100:
                    self.cycle_times = self.cycle_times[-100:]
                    
                self.cycle_count += 1
                
                # Adaptive sleep based on wake/rest state
                if self.wake_rest_controller:
                    state = self.wake_rest_controller.state
                    if state.value in ['resting', 'deep_rest']:
                        await asyncio.sleep(5.0)  # Slower cycles when resting
                    else:
                        await asyncio.sleep(1.0)  # Normal cycle rate
                else:
                    await asyncio.sleep(1.0)
                    
            except Exception as e:
                print(f"❌ Orchestration cycle error: {e}")
                await asyncio.sleep(1.0)
                
    async def _check_wake_rest_state(self) -> None:
        """Check and update wake/rest state."""
        if not self.wake_rest_controller:
            return
            
        # Get current processing quality and coherence
        processing_quality = 0.8  # Default
        coherence_level = 0.8  # Default
        
        if self.wisdom_metrics:
            scores = self.wisdom_metrics.calculate_composite_wisdom_score()
            coherence_level = scores.get('coherence', 0.8)
            
        # Update wake/rest controller
        new_state = self.wake_rest_controller.update(
            processing_quality=processing_quality,
            coherence_level=coherence_level,
            new_memories=0,  # Would be tracked from actual memory system
            consolidation_occurred=False
        )
        
        # Handle state-specific actions
        if new_state.value == 'resting' and self.echodream:
            # Ensure EchoDream is running for consolidation
            if not self.echodream.running:
                await asyncio.to_thread(self.echodream.Start)
                
        elif new_state.value == 'awake' and self.echodream:
            # Stop EchoDream when fully awake
            if self.echodream.running:
                await asyncio.to_thread(self.echodream.Stop)
                
    async def _run_orchestration_cycle(self) -> None:
        """Run one complete orchestration cycle through all phases."""
        context = OrchestrationContext(
            timestamp=time.time(),
            phase=OrchestrationPhase.PERCEPTION,
            inputs={},
            outputs={},
            metadata={'cycle': self.cycle_count}
        )
        
        # Phase 1: Perception
        context = await self._phase_perception(context)
        
        # Phase 2: Cognition (EchoBeats processing)
        context = await self._phase_cognition(context)
        
        # Phase 3: Emotion update
        context = await self._phase_emotion(context)
        
        # Phase 4: Social (Theory of Mind)
        context = await self._phase_social(context)
        
        # Phase 5: Learning (Interest patterns and wisdom)
        context = await self._phase_learning(context)
        
        # Phase 6: Consolidation (if resting)
        context = await self._phase_consolidation(context)
        
        # Phase 7: Integration
        context = await self._phase_integration(context)
        
        # Store context
        self.current_context = context
        self.context_history.append(context)
        if len(self.context_history) > 100:
            self.context_history = self.context_history[-100:]
            
    async def _phase_perception(self, context: OrchestrationContext) -> OrchestrationContext:
        """Perception phase: Gather inputs and context."""
        phase_start = time.time()
        context.phase = OrchestrationPhase.PERCEPTION
        
        # Gather current state from all subsystems
        if self.wake_rest_controller:
            context.inputs['wake_rest_state'] = self.wake_rest_controller.state.value
            context.inputs['cognitive_fatigue'] = self.wake_rest_controller.cognitive_fatigue
            
        if self.interest_tracker:
            context.inputs['active_interests'] = self.interest_tracker.active_interests
            context.inputs['exploration_goals'] = len(self.interest_tracker.get_exploration_priorities())
            
        if self.wisdom_metrics:
            context.inputs['wisdom_scores'] = self.wisdom_metrics.calculate_composite_wisdom_score()
            
        # Record phase time
        phase_time = time.time() - phase_start
        self.phase_times[OrchestrationPhase.PERCEPTION].append(phase_time)
        
        return context
        
    async def _phase_cognition(self, context: OrchestrationContext) -> OrchestrationContext:
        """Cognition phase: Process through EchoBeats."""
        phase_start = time.time()
        context.phase = OrchestrationPhase.COGNITION
        
        # EchoBeats processes autonomously, just check status
        if self.echobeats:
            # Get current step and phase from EchoBeats
            context.outputs['echobeats_step'] = getattr(self.echobeats, 'currentStep', 0)
            context.outputs['echobeats_phase'] = str(getattr(self.echobeats, 'currentPhase', 'unknown'))
            
        phase_time = time.time() - phase_start
        self.phase_times[OrchestrationPhase.COGNITION].append(phase_time)
        
        return context
        
    async def _phase_emotion(self, context: OrchestrationContext) -> OrchestrationContext:
        """Emotion phase: Update emotional state."""
        phase_start = time.time()
        context.phase = OrchestrationPhase.EMOTION
        
        if self.emotion_system:
            # Emotion system would update based on cognitive processing
            # For now, just record that it's available
            context.outputs['emotion_system_active'] = True
            
        phase_time = time.time() - phase_start
        self.phase_times[OrchestrationPhase.EMOTION].append(phase_time)
        
        return context
        
    async def _phase_social(self, context: OrchestrationContext) -> OrchestrationContext:
        """Social phase: Update theory of mind models."""
        phase_start = time.time()
        context.phase = OrchestrationPhase.SOCIAL
        
        if self.theory_of_mind:
            # Theory of mind would update agent models
            context.outputs['tom_active'] = True
            
        phase_time = time.time() - phase_start
        self.phase_times[OrchestrationPhase.SOCIAL].append(phase_time)
        
        return context
        
    async def _phase_learning(self, context: OrchestrationContext) -> OrchestrationContext:
        """Learning phase: Update interest patterns and wisdom."""
        phase_start = time.time()
        context.phase = OrchestrationPhase.LEARNING
        
        # Update interest patterns based on recent activity
        if self.interest_tracker:
            # Would record topic encounters from cognitive processing
            context.outputs['interest_tracking_active'] = True
            
        # Update wisdom metrics
        if self.wisdom_metrics:
            scores = self.wisdom_metrics.calculate_composite_wisdom_score()
            context.outputs['current_wisdom'] = scores['composite_wisdom']
            
        phase_time = time.time() - phase_start
        self.phase_times[OrchestrationPhase.LEARNING].append(phase_time)
        
        return context
        
    async def _phase_consolidation(self, context: OrchestrationContext) -> OrchestrationContext:
        """Consolidation phase: Memory consolidation if resting."""
        phase_start = time.time()
        context.phase = OrchestrationPhase.CONSOLIDATION
        
        if self.wake_rest_controller and self.wake_rest_controller.state.value in ['resting', 'deep_rest']:
            if self.echodream:
                # EchoDream handles consolidation autonomously
                context.outputs['consolidation_active'] = True
                
        phase_time = time.time() - phase_start
        self.phase_times[OrchestrationPhase.CONSOLIDATION].append(phase_time)
        
        return context
        
    async def _phase_integration(self, context: OrchestrationContext) -> OrchestrationContext:
        """Integration phase: Integrate all subsystem outputs."""
        phase_start = time.time()
        context.phase = OrchestrationPhase.INTEGRATION
        
        # Integrate outputs from all phases
        context.metadata['integration_complete'] = True
        context.metadata['cycle_complete_time'] = time.time()
        
        phase_time = time.time() - phase_start
        self.phase_times[OrchestrationPhase.INTEGRATION].append(phase_time)
        
        return context
        
    def get_metrics_summary(self) -> Dict:
        """Get comprehensive orchestration metrics."""
        avg_cycle_time = sum(self.cycle_times) / max(1, len(self.cycle_times))
        
        phase_avg_times = {
            phase.value: sum(times) / max(1, len(times))
            for phase, times in self.phase_times.items()
        }
        
        summary = {
            'running': self.running,
            'total_cycles': self.cycle_count,
            'average_cycle_time': avg_cycle_time,
            'phase_times': phase_avg_times,
            'subsystems': {
                'echobeats': self.echobeats is not None,
                'echodream': self.echodream is not None,
                'emotion_system': self.emotion_system is not None,
                'theory_of_mind': self.theory_of_mind is not None,
                'interest_tracker': self.interest_tracker is not None,
                'wisdom_metrics': self.wisdom_metrics is not None,
                'wake_rest_controller': self.wake_rest_controller is not None
            }
        }
        
        # Add subsystem metrics
        if self.wake_rest_controller:
            summary['wake_rest'] = self.wake_rest_controller.get_metrics_summary()
            
        if self.interest_tracker:
            summary['interests'] = self.interest_tracker.get_metrics_summary()
            
        if self.wisdom_metrics:
            summary['wisdom'] = self.wisdom_metrics.get_metrics_summary()
            
        return summary
        
    def _print_summary(self) -> None:
        """Print orchestration summary."""
        summary = self.get_metrics_summary()
        
        print("\n🎭 ═══════════════════════════════════════════════════════")
        print("🎭 Unified Orchestrator: Session Summary")
        print("🎭 ═══════════════════════════════════════════════════════")
        print(f"🎭 Total Cycles: {summary['total_cycles']}")
        print(f"🎭 Average Cycle Time: {summary['average_cycle_time']:.3f}s")
        print("🎭 Phase Times:")
        for phase, avg_time in summary['phase_times'].items():
            print(f"🎭   - {phase}: {avg_time:.3f}s")
        print("🎭 ═══════════════════════════════════════════════════════\n")
