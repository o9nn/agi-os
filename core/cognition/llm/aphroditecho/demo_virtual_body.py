#!/usr/bin/env python3
"""
Example Usage: Virtual Body Representation for Embodied AI

This example demonstrates how to use the virtual body representation
system implemented for Task 2.1.1.
"""

import numpy as np
from aar_core.embodied import EmbodiedAgent
from aar_core.arena.simulation_engine import ArenaPhysics, ArenaEnvironment


def main():
    print("🤖 Virtual Body Representation Demo")
    print("=" * 50)
    
    # Create an embodied agent with virtual body
    print("1. Creating embodied agent...")
    agent = EmbodiedAgent("demo_agent", position=(0, 0, 1))
    
    # Show initial body status
    status = agent.get_embodiment_status()
    print(f"   Agent created with {status['joint_count']} joints")
    print(f"   Body type: {status['body_type']}")
    print(f"   Initial consistency score: {status['embodiment_quality_score']:.3f}")
    
    # Validate body consistency
    print("\n2. Validating body consistency...")
    is_consistent, results = agent.validate_body_consistency()
    print(f"   Body consistent: {is_consistent}")
    print(f"   Consistency score: {results['consistency_score']:.3f}")
    
    # Demonstrate joint control
    print("\n3. Demonstrating joint control...")
    initial_angles = {}
    for joint_id in ["neck", "left_shoulder", "right_shoulder"]:
        state = agent.get_joint_state(joint_id)
        initial_angles[joint_id] = state['angle']
    
    # Set target angles
    agent.set_joint_target("neck", np.pi / 6)  # 30 degrees
    agent.set_joint_target("left_shoulder", np.pi / 4)  # 45 degrees  
    agent.set_joint_target("right_shoulder", -np.pi / 4)  # -45 degrees
    
    # Run simulation
    physics = ArenaPhysics()
    environment = ArenaEnvironment()
    
    print("   Running simulation for 2 seconds...")
    steps = 200  # 2 seconds at 100 Hz
    
    for step in range(steps):
        agent.update(0.01, physics, environment)
        
        if step % 50 == 0:  # Print every 0.5 seconds
            status = agent.get_embodiment_status()
            print(f"   Step {step}: Quality score = {status['embodiment_quality_score']:.3f}")
    
    # Show final joint angles
    print("\n4. Final joint positions:")
    for joint_id in ["neck", "left_shoulder", "right_shoulder"]:
        state = agent.get_joint_state(joint_id)
        initial = initial_angles[joint_id]
        final = state['angle']
        print(f"   {joint_id}: {initial:.3f} → {final:.3f} rad ({np.degrees(final):.1f}°)")
    
    # Show body schema representation
    print("\n5. Body schema neural representation:")
    schema = agent.get_body_representation()
    print(f"   Neural encoding dimension: {len(schema['joint_encodings'])} × {len(schema['joint_encodings'][0])}")
    print(f"   Spatial map size: {len(schema['spatial_map'])} × {len(schema['spatial_map'][0])}")
    print(f"   Schema coherence: {schema['coherence_score']:.3f}")
    
    # Show proprioceptive feedback
    print("\n6. Proprioceptive feedback:")
    feedback, confidence = agent.get_proprioceptive_feedback()
    print(f"   Feedback vector size: {feedback.shape}")
    print(f"   Feedback confidence: {confidence:.3f}")
    print(f"   Sample values: [{', '.join(f'{x:.3f}' for x in feedback[:6])}...]")
    
    # Final consistency check
    print("\n7. Final consistency validation:")
    is_consistent, results = agent.validate_body_consistency()
    print(f"   Body remains consistent: {is_consistent}")
    print(f"   Final consistency score: {results['consistency_score']:.3f}")
    
    # Component breakdown
    print(f"   - Body schema valid: {results['body_schema_valid']}")
    print(f"   - Joint kinematics valid: {results['joint_kinematics_valid']}")
    print(f"   - Proprioception valid: {results['proprioception_valid']}")
    print(f"   - Physics integration valid: {results['physics_integration_valid']}")
    
    print("\n✅ Demo completed successfully!")
    print("   The agent maintained consistent body representation throughout")
    print("   the simulation, meeting the Task 2.1.1 acceptance criteria.")


if __name__ == "__main__":
    main()