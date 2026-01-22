import logging
import asyncio
from typing import Dict, List
from datetime import datetime
import random
from echo_evolution import EchoAgent, EvolutionNetwork, EvolutionMemory
from cognitive_architecture import CognitiveArchitecture, Memory, Goal, MemoryType
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger('cognitive_evolution')
class CognitiveEvolutionBridge:
    def __init__(self, network: EvolutionNetwork):
        self.network = network
        self.cognitive = CognitiveArchitecture()
        self.evolution_memory = EvolutionMemory()
        logger.info('CognitiveEvolutionBridge initialized')
    def evolution_cycle_to_experience(self, cycle_data: Dict) -> Dict:
        avg_state = sum(cycle_data['agents'].values()) / len(cycle_data['agents'])
        evolution_success = avg_state > 0.5
        significance = min(1.0, abs(avg_state) * 2)
        experience = {'type': 'adaptation' if evolution_success else 'challenge', 'description': f"Evolution cycle {('succeeded' if evolution_success else 'struggled')} with avg state {avg_state:.2f}", 'effectiveness': avg_state, 'resolution': avg_state if evolution_success else 0.5 - avg_state, 'importance': significance, 'emotional_impact': significance * (1 if evolution_success else -1) * 0.5, 'area': 'self_evolution', 'timestamp': datetime.now().timestamp(), 'details': {'agents': cycle_data['agents'], 'resources': cycle_data.get('resource_metrics', {}), 'duration': cycle_data.get('duration', 0)}}
        return experience
    def job_cycle_to_experience(self, job_data: Dict) -> Dict:
        success_rate = job_data.get('success_rate', 0)
        job_success = success_rate > 0.7
        significance = success_rate
        experience = {'type': 'learning' if job_success else 'challenge', 'description': f'Processing jobs with {success_rate:.0%} success rate', 'success': success_rate, 'importance': significance, 'emotional_impact': (success_rate - 0.5) * 2, 'area': 'job_processing', 'timestamp': datetime.now().timestamp(), 'details': {'results': job_data.get('job_results', {}), 'duration': job_data.get('duration', 0)}}
        return experience
    def create_memory_from_evolution(self, cycle_data: Dict) -> Memory:
        experience = self.evolution_cycle_to_experience(cycle_data)
        memory = Memory(content=experience['description'], memory_type=MemoryType.EPISODIC, timestamp=experience['timestamp'], emotional_valence=experience['emotional_impact'], importance=experience['importance'], context=experience)
        for agent_name in cycle_data['agents']:
            agent = self.network.agents.get(agent_name)
            if agent:
                memory.associations.add(agent.domain)
        return memory
    def update_personality_from_evolution(self, cycle_data: Dict) -> None:
        avg_state = sum(cycle_data['agents'].values()) / len(cycle_data['agents'])
        max_state = max(cycle_data['agents'].values()) if cycle_data['agents'] else 0
        self.cognitive.personality_traits['adaptability'].update(avg_state, {'source': 'evolution', 'data': cycle_data})
        self.cognitive.personality_traits['creativity'].update(max_state, {'source': 'evolution', 'data': cycle_data})
        if 'resource_metrics' in cycle_data:
            resource_load = (cycle_data['resource_metrics'].get('cpu_usage', 50) + cycle_data['resource_metrics'].get('memory_usage', 50)) / 200
            self.cognitive.personality_traits['persistence'].update(1.0 - resource_load, {'source': 'evolution', 'resources': cycle_data['resource_metrics']})
    def generate_evolution_goals(self) -> List[Goal]:
        goals = []
        summary = self.network.get_summary()
        for name, info in summary['agents'].items():
            if info['state'] < 0.5:
                goals.append(Goal(description=f'Improve evolution of {name} agent', priority=0.9 - info['state'], deadline=None, context={'type': 'evolution_improvement', 'agent': name, 'current_state': info['state'], 'error_rate': info['error_rate']}))
        if summary['average_state'] > 0.7:
            goals.append(Goal(description='Explore new evolution patterns', priority=0.7, deadline=None, context={'type': 'evolution_exploration', 'current_avg_state': summary['average_state']}))
        return goals
    def create_evolution_constraints_from_cognition(self) -> Dict[str, float]:
        constraints = {}
        adaptability = self.cognitive.personality_traits['adaptability'].current_value
        creativity = self.cognitive.personality_traits['creativity'].current_value
        persistence = self.cognitive.personality_traits['persistence'].current_value
        for agent_name, agent in self.network.agents.items():
            if 'Cognitive' in agent_name:
                constraints[agent_name] = creativity
            elif 'Memory' in agent_name:
                constraints[agent_name] = persistence
            elif 'Sensory' in agent_name:
                constraints[agent_name] = adaptability
            else:
                constraints[agent_name] = (adaptability + creativity + persistence) / 3
        for goal in self.cognitive.active_goals:
            if goal.context.get('type') == 'evolution_improvement':
                target_agent = goal.context.get('agent')
                if target_agent in constraints:
                    constraints[target_agent] += 0.2 * goal.priority
        return constraints
    async def process_evolution_cycle(self, cycle_data: Dict) -> None:
        memory = self.create_memory_from_evolution(cycle_data)
        self.cognitive.enhanced_memory_management(memory)
        self.update_personality_from_evolution(cycle_data)
        experience = self.evolution_cycle_to_experience(cycle_data)
        self.cognitive.learn_from_experience(experience)
        if experience['importance'] > 0.7:
            goals = self.generate_evolution_goals()
            for goal in goals:
                self.cognitive.enhanced_goal_management(goal)
        self.cognitive.save_state()
    async def process_job_cycle(self, job_data: Dict) -> None:
        experience = self.job_cycle_to_experience(job_data)
        self.cognitive.learn_from_experience(experience)
        success_rate = job_data.get('success_rate', 0)
        self.cognitive.personality_traits['persistence'].update(success_rate, {'source': 'job_processing', 'success_rate': success_rate})
        self.cognitive.save_state()
    async def apply_cognitive_constraints(self) -> None:
        constraints = self.create_evolution_constraints_from_cognition()
        for agent_name, constraint in constraints.items():
            agent = self.network.agents.get(agent_name)
            if agent:
                adjustment = (constraint - 0.5) * 0.2
                agent.state = max(0, agent.state + adjustment)
                logger.info(f'Applied cognitive constraint to {agent_name}: adjustment {adjustment:.2f}, new state: {agent.state:.2f}')
    async def run_integrated_evolution(self, cycles: int=5) -> Dict:
        results = {'evolution_cycles': [], 'job_cycles': [], 'cognitive_updates': [], 'start_time': datetime.now().isoformat()}
        for cycle in range(cycles):
            logger.info(f'\n=== Integrated Evolution Cycle {cycle + 1}/{cycles} ===')
            await self.apply_cognitive_constraints()
            evo_result = await self.network.run_cycle()
            results['evolution_cycles'].append(evo_result)
            await self.process_evolution_cycle(evo_result)
            logger.info(f'=== Integrated Job Cycle {cycle + 1}/{cycles} ===')
            job_result = await self.network.run_job_cycle()
            results['job_cycles'].append(job_result)
            await self.process_job_cycle(job_result)
            cognitive_state = {'personality': {trait: value.current_value for trait, value in self.cognitive.personality_traits.items()}, 'active_goals': len(self.cognitive.active_goals), 'memories': len(self.cognitive.memories)}
            results['cognitive_updates'].append(cognitive_state)
            await asyncio.sleep(0.5)
        results['end_time'] = datetime.now().isoformat()
        return results
async def main():
    domains = [('CognitiveAgent', 'Cognitive Architecture'), ('MemoryAgent', 'Memory Management'), ('SensoryAgent', 'Sensory Processing'), ('IntegrationAgent', 'System Integration')]
    network = EvolutionNetwork()
    for name, domain in domains:
        agent = EchoAgent(name, domain, initial_state=random.uniform(0, 1))
        network.add_agent(agent)
    bridge = CognitiveEvolutionBridge(network)
    logger.info('Starting Integrated Cognitive Evolution System')
    results = await bridge.run_integrated_evolution(cycles=3)
    logger.info('\n=== Cognitive Evolution Summary ===')
    logger.info(f"Evolution Cycles: {len(results['evolution_cycles'])}")
    logger.info(f"Job Cycles: {len(results['job_cycles'])}")
    logger.info('Final Personality State:')
    for trait, value in results['cognitive_updates'][-1]['personality'].items():
        logger.info(f'  {trait}: {value:.2f}')
    network_summary = network.get_summary()
    logger.info('\n=== Evolution Network Summary ===')
    logger.info(f"Average Agent State: {network_summary['average_state']:.2f}")
    return results
if __name__ == '__main__':
    asyncio.run(main())