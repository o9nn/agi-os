import time
import asyncio
from typing import Dict, List, Optional, Any
from dataclasses import dataclass
from enum import Enum
import json
class OrchestrationPhase(Enum):
    PERCEPTION = 'perception'
    COGNITION = 'cognition'
    EMOTION = 'emotion'
    SOCIAL = 'social'
    LEARNING = 'learning'
    CONSOLIDATION = 'consolidation'
    INTEGRATION = 'integration'
@dataclass
class OrchestrationContext:
    timestamp: float
    phase: OrchestrationPhase
    inputs: Dict[str, Any]
    outputs: Dict[str, Any]
    metadata: Dict[str, Any]
class UnifiedOrchestrator:
    def __init__(self, echobeats=None, echodream=None, emotion_system=None, theory_of_mind=None, interest_tracker=None, wisdom_metrics=None, wake_rest_controller=None):
        self.echobeats = echobeats
        self.echodream = echodream
        self.emotion_system = emotion_system
        self.theory_of_mind = theory_of_mind
        self.interest_tracker = interest_tracker
        self.wisdom_metrics = wisdom_metrics
        self.wake_rest_controller = wake_rest_controller
        self.running = False
        self.cycle_count = 0
        self.current_phase = OrchestrationPhase.PERCEPTION
        self.cycle_times: List[float] = []
        self.phase_times: Dict[OrchestrationPhase, List[float]] = {phase: [] for phase in OrchestrationPhase}
        self.current_context: Optional[OrchestrationContext] = None
        self.context_history: List[OrchestrationContext] = []
        self.active_goals: List[Dict] = []
        self.goal_priorities: Dict[str, float] = {}
    async def start(self) -> None:
        if self.running:
            print('⚠️  Orchestrator already running')
            return
        self.running = True
        print('🎭 ═══════════════════════════════════════════════════════')
        print('🎭 Unified Orchestrator: Starting Deep Tree Echo')
        print('🎭 ═══════════════════════════════════════════════════════')
        print('🎭 Coordinating subsystems:')
        print(f"🎭   - EchoBeats: {('✓' if self.echobeats else '✗')}")
        print(f"🎭   - EchoDream: {('✓' if self.echodream else '✗')}")
        print(f"🎭   - Emotion System: {('✓' if self.emotion_system else '✗')}")
        print(f"🎭   - Theory of Mind: {('✓' if self.theory_of_mind else '✗')}")
        print(f"🎭   - Interest Tracker: {('✓' if self.interest_tracker else '✗')}")
        print(f"🎭   - Wisdom Metrics: {('✓' if self.wisdom_metrics else '✗')}")
        print(f"🎭   - Wake/Rest Controller: {('✓' if self.wake_rest_controller else '✗')}")
        print('🎭 ═══════════════════════════════════════════════════════\n')
        await self._start_subsystems()
        await self._orchestration_loop()
    async def stop(self) -> None:
        if not self.running:
            print('⚠️  Orchestrator not running')
            return
        self.running = False
        print('\n🎭 Stopping Unified Orchestrator...')
        await self._stop_subsystems()
        self._print_summary()
    async def _start_subsystems(self) -> None:
        if self.echobeats:
            try:
                await asyncio.to_thread(self.echobeats.Start)
                print('✓ EchoBeats started')
            except Exception as e:
                print(f'✗ EchoBeats start failed: {e}')
        if self.echodream:
            try:
                await asyncio.to_thread(self.echodream.Start)
                print('✓ EchoDream started')
            except Exception as e:
                print(f'✗ EchoDream start failed: {e}')
    async def _stop_subsystems(self) -> None:
        if self.echobeats:
            try:
                await asyncio.to_thread(self.echobeats.Stop)
            except Exception as e:
                print(f'✗ EchoBeats stop failed: {e}')
        if self.echodream:
            try:
                await asyncio.to_thread(self.echodream.Stop)
            except Exception as e:
                print(f'✗ EchoDream stop failed: {e}')
    async def _orchestration_loop(self) -> None:
        while self.running:
            cycle_start = time.time()
            try:
                await self._check_wake_rest_state()
                await self._run_orchestration_cycle()
                cycle_time = time.time() - cycle_start
                self.cycle_times.append(cycle_time)
                if len(self.cycle_times) > 100:
                    self.cycle_times = self.cycle_times[-100:]
                self.cycle_count += 1
                if self.wake_rest_controller:
                    state = self.wake_rest_controller.state
                    if state.value in ['resting', 'deep_rest']:
                        await asyncio.sleep(5.0)
                    else:
                        await asyncio.sleep(1.0)
                else:
                    await asyncio.sleep(1.0)
            except Exception as e:
                print(f'❌ Orchestration cycle error: {e}')
                await asyncio.sleep(1.0)
    async def _check_wake_rest_state(self) -> None:
        if not self.wake_rest_controller:
            return
        processing_quality = 0.8
        coherence_level = 0.8
        if self.wisdom_metrics:
            scores = self.wisdom_metrics.calculate_composite_wisdom_score()
            coherence_level = scores.get('coherence', 0.8)
        new_state = self.wake_rest_controller.update(processing_quality=processing_quality, coherence_level=coherence_level, new_memories=0, consolidation_occurred=False)
        if new_state.value == 'resting' and self.echodream:
            if not self.echodream.running:
                await asyncio.to_thread(self.echodream.Start)
        elif new_state.value == 'awake' and self.echodream:
            if self.echodream.running:
                await asyncio.to_thread(self.echodream.Stop)
    async def _run_orchestration_cycle(self) -> None:
        context = OrchestrationContext(timestamp=time.time(), phase=OrchestrationPhase.PERCEPTION, inputs={}, outputs={}, metadata={'cycle': self.cycle_count})
        context = await self._phase_perception(context)
        context = await self._phase_cognition(context)
        context = await self._phase_emotion(context)
        context = await self._phase_social(context)
        context = await self._phase_learning(context)
        context = await self._phase_consolidation(context)
        context = await self._phase_integration(context)
        self.current_context = context
        self.context_history.append(context)
        if len(self.context_history) > 100:
            self.context_history = self.context_history[-100:]
    async def _phase_perception(self, context: OrchestrationContext) -> OrchestrationContext:
        phase_start = time.time()
        context.phase = OrchestrationPhase.PERCEPTION
        if self.wake_rest_controller:
            context.inputs['wake_rest_state'] = self.wake_rest_controller.state.value
            context.inputs['cognitive_fatigue'] = self.wake_rest_controller.cognitive_fatigue
        if self.interest_tracker:
            context.inputs['active_interests'] = self.interest_tracker.active_interests
            context.inputs['exploration_goals'] = len(self.interest_tracker.get_exploration_priorities())
        if self.wisdom_metrics:
            context.inputs['wisdom_scores'] = self.wisdom_metrics.calculate_composite_wisdom_score()
        phase_time = time.time() - phase_start
        self.phase_times[OrchestrationPhase.PERCEPTION].append(phase_time)
        return context
    async def _phase_cognition(self, context: OrchestrationContext) -> OrchestrationContext:
        phase_start = time.time()
        context.phase = OrchestrationPhase.COGNITION
        if self.echobeats:
            context.outputs['echobeats_step'] = getattr(self.echobeats, 'currentStep', 0)
            context.outputs['echobeats_phase'] = str(getattr(self.echobeats, 'currentPhase', 'unknown'))
        phase_time = time.time() - phase_start
        self.phase_times[OrchestrationPhase.COGNITION].append(phase_time)
        return context
    async def _phase_emotion(self, context: OrchestrationContext) -> OrchestrationContext:
        phase_start = time.time()
        context.phase = OrchestrationPhase.EMOTION
        if self.emotion_system:
            context.outputs['emotion_system_active'] = True
        phase_time = time.time() - phase_start
        self.phase_times[OrchestrationPhase.EMOTION].append(phase_time)
        return context
    async def _phase_social(self, context: OrchestrationContext) -> OrchestrationContext:
        phase_start = time.time()
        context.phase = OrchestrationPhase.SOCIAL
        if self.theory_of_mind:
            context.outputs['tom_active'] = True
        phase_time = time.time() - phase_start
        self.phase_times[OrchestrationPhase.SOCIAL].append(phase_time)
        return context
    async def _phase_learning(self, context: OrchestrationContext) -> OrchestrationContext:
        phase_start = time.time()
        context.phase = OrchestrationPhase.LEARNING
        if self.interest_tracker:
            context.outputs['interest_tracking_active'] = True
        if self.wisdom_metrics:
            scores = self.wisdom_metrics.calculate_composite_wisdom_score()
            context.outputs['current_wisdom'] = scores['composite_wisdom']
        phase_time = time.time() - phase_start
        self.phase_times[OrchestrationPhase.LEARNING].append(phase_time)
        return context
    async def _phase_consolidation(self, context: OrchestrationContext) -> OrchestrationContext:
        phase_start = time.time()
        context.phase = OrchestrationPhase.CONSOLIDATION
        if self.wake_rest_controller and self.wake_rest_controller.state.value in ['resting', 'deep_rest']:
            if self.echodream:
                context.outputs['consolidation_active'] = True
        phase_time = time.time() - phase_start
        self.phase_times[OrchestrationPhase.CONSOLIDATION].append(phase_time)
        return context
    async def _phase_integration(self, context: OrchestrationContext) -> OrchestrationContext:
        phase_start = time.time()
        context.phase = OrchestrationPhase.INTEGRATION
        context.metadata['integration_complete'] = True
        context.metadata['cycle_complete_time'] = time.time()
        phase_time = time.time() - phase_start
        self.phase_times[OrchestrationPhase.INTEGRATION].append(phase_time)
        return context
    def get_metrics_summary(self) -> Dict:
        avg_cycle_time = sum(self.cycle_times) / max(1, len(self.cycle_times))
        phase_avg_times = {phase.value: sum(times) / max(1, len(times)) for phase, times in self.phase_times.items()}
        summary = {'running': self.running, 'total_cycles': self.cycle_count, 'average_cycle_time': avg_cycle_time, 'phase_times': phase_avg_times, 'subsystems': {'echobeats': self.echobeats is not None, 'echodream': self.echodream is not None, 'emotion_system': self.emotion_system is not None, 'theory_of_mind': self.theory_of_mind is not None, 'interest_tracker': self.interest_tracker is not None, 'wisdom_metrics': self.wisdom_metrics is not None, 'wake_rest_controller': self.wake_rest_controller is not None}}
        if self.wake_rest_controller:
            summary['wake_rest'] = self.wake_rest_controller.get_metrics_summary()
        if self.interest_tracker:
            summary['interests'] = self.interest_tracker.get_metrics_summary()
        if self.wisdom_metrics:
            summary['wisdom'] = self.wisdom_metrics.get_metrics_summary()
        return summary
    def _print_summary(self) -> None:
        summary = self.get_metrics_summary()
        print('\n🎭 ═══════════════════════════════════════════════════════')
        print('🎭 Unified Orchestrator: Session Summary')
        print('🎭 ═══════════════════════════════════════════════════════')
        print(f"🎭 Total Cycles: {summary['total_cycles']}")
        print(f"🎭 Average Cycle Time: {summary['average_cycle_time']:.3f}s")
        print('🎭 Phase Times:')
        for phase, avg_time in summary['phase_times'].items():
            print(f'🎭   - {phase}: {avg_time:.3f}s')
        print('🎭 ═══════════════════════════════════════════════════════\n')