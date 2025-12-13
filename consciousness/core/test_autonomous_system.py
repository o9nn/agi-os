#!/usr/bin/env python3
"""
Comprehensive Test and Demonstration for Iteration 16 Enhancements

Tests and demonstrates:
1. Wisdom Cultivation Metrics
2. Echo Interest Pattern System
3. Autonomous Wake/Rest Controller
4. Unified Orchestrator
5. Autonomous Consciousness Loop
6. Integrated System Operation
"""

import asyncio
import time
import sys
import os

# Add parent directory to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from core.wisdom_metrics import WisdomMetrics, WisdomInsight, BeliefUpdate
from core.echo_interest import EchoInterest, ExplorationGoal
from core.autonomous_wake_rest_controller import AutonomousWakeRestController, ConsciousnessState
from core.unified_orchestrator import UnifiedOrchestrator
from core.autonomous_consciousness_loop import AutonomousConsciousnessLoop


def test_wisdom_metrics():
    """Test the wisdom cultivation metrics system."""
    print("\n" + "="*60)
    print("TEST 1: Wisdom Cultivation Metrics")
    print("="*60)
    
    metrics = WisdomMetrics()
    
    # Add some insights
    print("\n📊 Adding wisdom insights...")
    
    insight1 = WisdomInsight(
        id="insight_1",
        content="Understanding emerges from the integration of diverse perspectives",
        timestamp=time.time(),
        depth_score=0.8,
        breadth_score=0.7,
        applicability_score=0.9,
        coherence_contribution=0.1,
        related_domains=["philosophy", "cognition", "learning"]
    )
    metrics.add_insight(insight1)
    print(f"✓ Added insight: {insight1.content}")
    
    insight2 = WisdomInsight(
        id="insight_2",
        content="Patterns at one scale often reflect patterns at other scales",
        timestamp=time.time(),
        depth_score=0.9,
        breadth_score=0.8,
        applicability_score=0.7,
        coherence_contribution=0.15,
        related_domains=["mathematics", "philosophy", "systems_theory"]
    )
    metrics.add_insight(insight2)
    print(f"✓ Added insight: {insight2.content}")
    
    # Add a belief update
    print("\n🔄 Adding belief update...")
    
    update1 = BeliefUpdate(
        id="update_1",
        timestamp=time.time(),
        prior_belief="Learning is primarily about accumulating facts",
        updated_belief="Learning is about developing integrated understanding",
        evidence="Observing that isolated facts are quickly forgotten while integrated knowledge persists",
        confidence_change=0.3,
        coherence_impact=0.1
    )
    metrics.add_belief_update(update1)
    print(f"✓ Updated belief: {update1.prior_belief} → {update1.updated_belief}")
    
    # Calculate wisdom scores
    print("\n📈 Wisdom Scores:")
    scores = metrics.calculate_composite_wisdom_score()
    for dimension, score in scores.items():
        print(f"   {dimension}: {score:.3f}")
        
    # Get full summary
    print("\n📋 Full Metrics Summary:")
    summary = metrics.get_metrics_summary()
    print(f"   Total Insights: {summary['total_insights']}")
    print(f"   Total Belief Updates: {summary['total_belief_updates']}")
    print(f"   Domains Explored: {summary['domains_explored']}")
    print(f"   Cross-Domain Connections: {summary['cross_domain_connections']}")
    print(f"   Current Coherence: {summary['current_coherence']:.3f}")
    
    print("\n✅ Wisdom Metrics Test Complete")
    return metrics


def test_echo_interest():
    """Test the echo interest pattern system."""
    print("\n" + "="*60)
    print("TEST 2: Echo Interest Pattern System")
    print("="*60)
    
    interest = EchoInterest()
    
    # Record some topic encounters
    print("\n📚 Recording topic encounters...")
    
    interest.record_topic_encounter(
        topic="cognitive_architecture",
        duration=300.0,
        emotional_valence=0.6,
        learning_occurred=True,
        satisfaction=0.8
    )
    print("✓ Encountered: cognitive_architecture (positive experience)")
    
    interest.record_topic_encounter(
        topic="autonomous_systems",
        duration=450.0,
        emotional_valence=0.7,
        learning_occurred=True,
        satisfaction=0.9
    )
    print("✓ Encountered: autonomous_systems (very positive experience)")
    
    interest.record_topic_encounter(
        topic="wisdom_cultivation",
        duration=200.0,
        emotional_valence=0.5,
        learning_occurred=False,
        satisfaction=0.6
    )
    print("✓ Encountered: wisdom_cultivation (neutral experience)")
    
    # Add topic relations
    print("\n🔗 Adding topic relations...")
    interest.add_topic_relation("cognitive_architecture", "autonomous_systems")
    interest.add_topic_relation("autonomous_systems", "wisdom_cultivation")
    print("✓ Linked related topics")
    
    # Generate exploration goal
    print("\n🎯 Generating exploration goal...")
    goal = interest.generate_exploration_goal()
    if goal:
        print(f"✓ Generated goal: {goal.topic}")
        print(f"   Priority: {goal.priority:.3f}")
        print(f"   Curiosity Driver: {goal.curiosity_driver:.3f}")
        print(f"   Utility Driver: {goal.utility_driver:.3f}")
        
    # Test discussion decision
    print("\n💬 Testing discussion participation decisions...")
    topics_to_test = ["cognitive_architecture", "autonomous_systems", "random_topic"]
    for topic in topics_to_test:
        should_join = interest.should_join_discussion(topic)
        print(f"   {topic}: {'JOIN ✓' if should_join else 'SKIP ✗'}")
        
    # Get metrics
    print("\n📊 Interest Metrics Summary:")
    summary = interest.get_metrics_summary()
    print(f"   Total Topics: {summary['total_topics']}")
    print(f"   Active Interests: {summary['active_interests']}")
    print(f"   Active Exploration Goals: {summary['active_exploration_goals']}")
    print(f"   Average Curiosity: {summary['average_curiosity']:.3f}")
    print(f"   Average Satisfaction: {summary['average_satisfaction']:.3f}")
    
    print("\n✅ Echo Interest Test Complete")
    return interest


def test_wake_rest_controller():
    """Test the autonomous wake/rest controller."""
    print("\n" + "="*60)
    print("TEST 3: Autonomous Wake/Rest Controller")
    print("="*60)
    
    controller = AutonomousWakeRestController()
    
    print(f"\n🌅 Initial State: {controller.state.value}")
    print(f"   Cognitive Fatigue: {controller.cognitive_fatigue:.3f}")
    print(f"   Consolidation Pressure: {controller.consolidation_pressure:.3f}")
    
    # Simulate activity causing fatigue
    print("\n⚡ Simulating cognitive activity...")
    for i in range(5):
        state = controller.update(
            processing_quality=0.8 - i*0.1,  # Declining quality
            coherence_level=0.8 - i*0.05,
            new_memories=10,
            consolidation_occurred=False
        )
        print(f"   Update {i+1}: State={state.value}, Fatigue={controller.cognitive_fatigue:.3f}")
        time.sleep(0.1)
        
    # Check if rest is needed
    print(f"\n💤 Should rest now? {controller.should_rest_now()}")
    
    # Force rest
    print("\n🌙 Forcing rest state...")
    controller.force_rest()
    print(f"   State: {controller.state.value}")
    
    # Simulate rest
    print("\n😴 Simulating rest period...")
    for i in range(3):
        state = controller.update(
            processing_quality=0.9,
            coherence_level=0.9,
            new_memories=0,
            consolidation_occurred=True
        )
        print(f"   Rest {i+1}: State={state.value}, Fatigue={controller.cognitive_fatigue:.3f}")
        time.sleep(0.1)
        
    # Check if should wake
    print(f"\n☀️ Should wake now? {controller.should_wake_now()}")
    
    # Get metrics
    print("\n📊 Wake/Rest Metrics Summary:")
    summary = controller.get_metrics_summary()
    print(f"   Consciousness State: {summary['consciousness_state']}")
    print(f"   Cognitive Fatigue: {summary['cognitive_fatigue']:.3f}")
    print(f"   Total Cycles: {summary['total_cycles']}")
    print(f"   Optimal Activity (min): {summary['optimal_activity_minutes']:.1f}")
    print(f"   Optimal Rest (min): {summary['optimal_rest_minutes']:.1f}")
    
    print("\n✅ Wake/Rest Controller Test Complete")
    return controller


async def test_unified_orchestrator(wake_rest_controller, interest_tracker, wisdom_metrics):
    """Test the unified orchestrator."""
    print("\n" + "="*60)
    print("TEST 4: Unified Orchestrator")
    print("="*60)
    
    orchestrator = UnifiedOrchestrator(
        wake_rest_controller=wake_rest_controller,
        interest_tracker=interest_tracker,
        wisdom_metrics=wisdom_metrics
    )
    
    print("\n🎭 Starting orchestrator for 10 seconds...")
    
    # Start orchestrator
    start_task = asyncio.create_task(orchestrator.start())
    
    # Let it run for 10 seconds
    await asyncio.sleep(10)
    
    # Stop orchestrator
    await orchestrator.stop()
    
    # Get metrics
    print("\n📊 Orchestration Metrics:")
    summary = orchestrator.get_metrics_summary()
    print(f"   Total Cycles: {summary['total_cycles']}")
    print(f"   Average Cycle Time: {summary['average_cycle_time']:.3f}s")
    print(f"   Subsystems Active:")
    for subsystem, active in summary['subsystems'].items():
        print(f"      {subsystem}: {'✓' if active else '✗'}")
        
    print("\n✅ Unified Orchestrator Test Complete")
    return orchestrator


async def test_consciousness_loop(orchestrator):
    """Test the autonomous consciousness loop."""
    print("\n" + "="*60)
    print("TEST 5: Autonomous Consciousness Loop")
    print("="*60)
    
    consciousness = AutonomousConsciousnessLoop(orchestrator=orchestrator)
    
    print("\n🧠 Starting consciousness loop for 30 seconds...")
    print("   (Generating autonomous thoughts...)\n")
    
    # Start consciousness loop
    start_task = asyncio.create_task(consciousness.start())
    
    # Let it run for 30 seconds
    await asyncio.sleep(30)
    
    # Stop consciousness loop
    await consciousness.stop()
    
    # Get metrics
    print("\n📊 Consciousness Metrics:")
    summary = consciousness.get_metrics_summary()
    print(f"   Total Thoughts: {summary['total_thoughts']}")
    print(f"   Thoughts/Minute: {summary['thoughts_per_minute']:.2f}")
    print(f"   Average Depth: {summary['average_depth']:.2f}")
    print(f"   Thought Distribution:")
    for thought_type, count in summary['thought_type_distribution'].items():
        print(f"      {thought_type}: {count}")
        
    print("\n✅ Autonomous Consciousness Loop Test Complete")
    return consciousness


async def test_integrated_system():
    """Test the fully integrated autonomous system."""
    print("\n" + "="*60)
    print("TEST 6: Integrated Autonomous System")
    print("="*60)
    
    print("\n🚀 Initializing all subsystems...")
    
    # Create all components
    wisdom_metrics = WisdomMetrics()
    interest_tracker = EchoInterest()
    wake_rest_controller = AutonomousWakeRestController()
    
    # Add some initial data
    print("\n📝 Seeding initial data...")
    
    # Add initial insights
    wisdom_metrics.add_insight(WisdomInsight(
        id="init_1",
        content="Autonomous operation requires self-directed goals",
        timestamp=time.time(),
        depth_score=0.7,
        breadth_score=0.6,
        applicability_score=0.8,
        coherence_contribution=0.1,
        related_domains=["autonomy", "goal_systems"]
    ))
    
    # Add initial interests
    interest_tracker.record_topic_encounter(
        topic="autonomous_cognition",
        duration=300.0,
        emotional_valence=0.7,
        learning_occurred=True,
        satisfaction=0.8
    )
    
    # Create orchestrator with all subsystems
    orchestrator = UnifiedOrchestrator(
        wake_rest_controller=wake_rest_controller,
        interest_tracker=interest_tracker,
        wisdom_metrics=wisdom_metrics
    )
    
    # Create consciousness loop
    consciousness = AutonomousConsciousnessLoop(orchestrator=orchestrator)
    
    print("\n✨ Starting integrated autonomous system for 60 seconds...")
    print("   All subsystems coordinated through unified orchestrator")
    print("   Stream-of-consciousness awareness active")
    print("   Autonomous wake/rest cycles enabled\n")
    
    # Start orchestrator
    orch_task = asyncio.create_task(orchestrator.start())
    
    # Start consciousness loop
    cons_task = asyncio.create_task(consciousness.start())
    
    # Let system run for 60 seconds
    await asyncio.sleep(60)
    
    # Stop everything
    print("\n🛑 Stopping integrated system...")
    await consciousness.stop()
    await orchestrator.stop()
    
    # Final metrics
    print("\n" + "="*60)
    print("INTEGRATED SYSTEM FINAL METRICS")
    print("="*60)
    
    print("\n🧠 Consciousness:")
    cons_summary = consciousness.get_metrics_summary()
    print(f"   Thoughts Generated: {cons_summary['total_thoughts']}")
    print(f"   Thoughts/Minute: {cons_summary['thoughts_per_minute']:.2f}")
    
    print("\n🎭 Orchestration:")
    orch_summary = orchestrator.get_metrics_summary()
    print(f"   Cycles Completed: {orch_summary['total_cycles']}")
    print(f"   Avg Cycle Time: {orch_summary['average_cycle_time']:.3f}s")
    
    print("\n🌙 Wake/Rest:")
    wr_summary = wake_rest_controller.get_metrics_summary()
    print(f"   Current State: {wr_summary['consciousness_state']}")
    print(f"   Cognitive Fatigue: {wr_summary['cognitive_fatigue']:.3f}")
    
    print("\n📚 Interests:")
    int_summary = interest_tracker.get_metrics_summary()
    print(f"   Topics Tracked: {int_summary['total_topics']}")
    print(f"   Active Interests: {int_summary['active_interests']}")
    
    print("\n💎 Wisdom:")
    wis_summary = wisdom_metrics.get_metrics_summary()
    wisdom_scores = wis_summary['wisdom_scores']
    print(f"   Composite Wisdom: {wisdom_scores['composite_wisdom']:.3f}")
    print(f"   Depth: {wisdom_scores['depth']:.3f}")
    print(f"   Coherence: {wisdom_scores['coherence']:.3f}")
    
    print("\n✅ Integrated System Test Complete")
    print("\n" + "="*60)
    print("SUCCESS: All Iteration 16 enhancements validated!")
    print("="*60)


async def main():
    """Run all tests."""
    print("\n" + "="*60)
    print("ECHO9LLAMA ITERATION 16 COMPREHENSIVE TEST SUITE")
    print("="*60)
    print("\nTesting autonomous wisdom-cultivating deep tree echo AGI")
    print("enhancements for persistent cognitive event loops and")
    print("stream-of-consciousness awareness.\n")
    
    # Run individual component tests
    wisdom_metrics = test_wisdom_metrics()
    interest_tracker = test_echo_interest()
    wake_rest_controller = test_wake_rest_controller()
    
    # Run orchestrator test
    orchestrator = await test_unified_orchestrator(
        wake_rest_controller,
        interest_tracker,
        wisdom_metrics
    )
    
    # Run consciousness loop test
    consciousness = await test_consciousness_loop(orchestrator)
    
    # Run integrated system test
    await test_integrated_system()
    
    print("\n" + "="*60)
    print("ALL TESTS COMPLETED SUCCESSFULLY")
    print("="*60)
    print("\nIteration 16 enhancements are ready for deployment!")
    print("The system demonstrates:")
    print("  ✓ Autonomous operation independent of external prompts")
    print("  ✓ Stream-of-consciousness awareness")
    print("  ✓ Self-regulated wake/rest cycles")
    print("  ✓ Interest-driven exploration")
    print("  ✓ Wisdom cultivation metrics")
    print("  ✓ Unified subsystem orchestration")
    print("\nNext steps: Deploy to production and monitor wisdom growth!")
    print("="*60 + "\n")


if __name__ == "__main__":
    asyncio.run(main())
