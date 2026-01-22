import asyncio
from core import SelfOrganizingCore
async def main():
    soc = SelfOrganizingCore(autognosis_levels=5)
    await soc.initialize()
    for i in range(3):
        print(f'Running cycle {i + 1}...')
        result = await soc.run_autognosis_cycle()
        print(f"  Duration: {result['duration_seconds']:.3f}s")
    status = soc.get_autognosis_status()
    print(f'\nAutognosis Status:')
    print(f"  Running: {status['running']}")
    print(f"  Cycles: {status['cycle_count']}")
    print(f"  Levels: {status['max_levels']}")
    print(f'\nSelf-Images:')
    for level, self_image in soc.autognosis.current_self_images.items():
        print(f'  Level {level}: confidence={self_image.confidence:.2f}, patterns={len(self_image.behavioral_patterns)}, reflections={len(self_image.meta_reflections)}')
    highest_level = max(soc.autognosis.current_self_images.keys())
    self_image = soc.autognosis.current_self_images[highest_level]
    assessment = soc.autognosis.processor.get_self_awareness_assessment(self_image)
    print(f'\nSelf-Awareness Assessment:')
    for metric, value in assessment.items():
        print(f'  {metric}: {value:.3f}')
    await soc.shutdown()
    print('\nSystem shutdown complete.')
if __name__ == '__main__':
    asyncio.run(main())