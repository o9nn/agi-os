import asyncio
import random
class ESMWorker:
    def __init__(self, pattern_name, initial_value=0.0):
        self.pattern_name = pattern_name
        self.state = initial_value
        self.iteration = 0
    async def evolve(self, constraints):
        improvement = random.uniform(-0.1, 0.5)
        constraint_factor = sum(constraints) / (len(constraints) or 1)
        self.state = self.state + improvement + constraint_factor * 0.1
        self.iteration += 1
        print(f'[{self.pattern_name}] Cycle {self.iteration}: state updated to {self.state:.2f} (improvement: {improvement:.2f}, constraint factor: {constraint_factor:.2f})')
        await asyncio.sleep(0.1)
        return self.state
class ConstraintEmitter:
    def __init__(self):
        self.emitter_values = {}
    def update(self, pattern_name, value):
        self.emitter_values[pattern_name] = value
    def get_constraints(self, excluding=None):
        return [value for name, value in self.emitter_values.items() if name != excluding]
async def run_cycle(workers, emitter):
    tasks = []
    for worker in workers:
        constraints = emitter.get_constraints(excluding=worker.pattern_name)
        tasks.append(asyncio.create_task(worker.evolve(constraints)))
    results = await asyncio.gather(*tasks)
    for worker, result in zip(workers, results):
        emitter.update(worker.pattern_name, result)
async def main():
    worker_patterns = ['Differential Gear', 'Epicyclic Train', 'Zodiac Dial']
    workers = [ESMWorker(name, initial_value=random.uniform(0, 1)) for name in worker_patterns]
    emitter = ConstraintEmitter()
    for worker in workers:
        emitter.update(worker.pattern_name, worker.state)
    for cycle in range(5):
        print(f'\n=== Global Cycle {cycle + 1} ===')
        await run_cycle(workers, emitter)
        await asyncio.sleep(0.5)
    print('\nFinal states:')
    for worker in workers:
        print(f'{worker.pattern_name}: {worker.state:.2f}')
if __name__ == '__main__':
    asyncio.run(main())