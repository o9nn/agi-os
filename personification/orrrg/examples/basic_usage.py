#!/usr/bin/env python3
"""Example usage of ORRRG autognosis system via Python API."""

import asyncio
from core import SelfOrganizingCore


async def main():
    # Initialize ORRRG with autognosis
    soc = SelfOrganizingCore(autognosis_levels=5)
    await soc.initialize()
    
    # Run a few autognosis cycles
    for i in range(3):
        print(f"Running cycle {i+1}...")
        result = await soc.run_autognosis_cycle()
        print(f"  Duration: {result['duration_seconds']:.3f}s")
    
    # Get autognosis status
    status = soc.get_autognosis_status()
    print(f"\nAutognosis Status:")
    print(f"  Running: {status['running']}")
    print(f"  Cycles: {status['cycle_count']}")
    print(f"  Levels: {status['max_levels']}")
    
    # Access current self-images
    print(f"\nSelf-Images:")
    for level, self_image in soc.autognosis.current_self_images.items():
        print(f"  Level {level}: confidence={self_image.confidence:.2f}, "
              f"patterns={len(self_image.behavioral_patterns)}, "
              f"reflections={len(self_image.meta_reflections)}")
    
    # Get self-awareness assessment
    highest_level = max(soc.autognosis.current_self_images.keys())
    self_image = soc.autognosis.current_self_images[highest_level]
    assessment = soc.autognosis.processor.get_self_awareness_assessment(self_image)
    
    print(f"\nSelf-Awareness Assessment:")
    for metric, value in assessment.items():
        print(f"  {metric}: {value:.3f}")
    
    # Shutdown
    await soc.shutdown()
    print("\nSystem shutdown complete.")


if __name__ == "__main__":
    asyncio.run(main())
