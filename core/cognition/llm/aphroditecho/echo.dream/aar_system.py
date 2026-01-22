import numpy as np
from abc import ABC, abstractmethod
class AARComponent(ABC):
    def __init__(self, name):
        self.name = name
        self.state = {}
        self.relations = []
    @abstractmethod
    def process(self, input_data):
        pass
    def add_relation(self, relation):
        self.relations.append(relation)
    def get_state(self):
        return self.state
class Agent(AARComponent):
    def __init__(self, name, capabilities=None):
        super().__init__(name)
        self.capabilities = capabilities or []
        self.state = {'active': True, 'energy': 100, 'perception': {}, 'memory': []}
    def process(self, input_data):
        self.state['perception'] = input_data
        action = None
        for capability in self.capabilities:
            if np.random.random() < 0.3:
                action = {'type': capability, 'intensity': np.random.uniform(0.1, 1.0), 'target': input_data.get('targets', [None])[0]}
                break
        self.state['energy'] -= 5
        if self.state['energy'] <= 0:
            self.state['active'] = False
        if len(self.state['memory']) >= 10:
            self.state['memory'].pop(0)
        self.state['memory'].append({'perception': input_data, 'action': action})
        return action
class Arena(AARComponent):
    def __init__(self, name, size=(10, 10)):
        super().__init__(name)
        self.size = size
        self.state = {'grid': np.zeros(size), 'entities': {}, 'resources': {}, 'rules': []}
    def process(self, input_data):
        actions = input_data.get('actions', [])
        for action in actions:
            if not action:
                continue
            action_type = action.get('type')
            target = action.get('target')
            intensity = action.get('intensity', 0.5)
            if action_type == 'modify' and target:
                x, y = target
                if 0 <= x < self.size[0] and 0 <= y < self.size[1]:
                    self.state['grid'][x, y] += intensity
            elif action_type == 'create' and target:
                entity_id = f"entity_{len(self.state['entities']) + 1}"
                self.state['entities'][entity_id] = {'position': target, 'strength': intensity * 10}
            elif action_type == 'destroy' and target:
                entities_to_remove = []
                for entity_id, entity in self.state['entities'].items():
                    if entity['position'] == target:
                        entities_to_remove.append(entity_id)
                for entity_id in entities_to_remove:
                    del self.state['entities'][entity_id]
        if np.random.random() < 0.2:
            x = np.random.randint(0, self.size[0])
            y = np.random.randint(0, self.size[1])
            resource_id = f"resource_{len(self.state['resources']) + 1}"
            self.state['resources'][resource_id] = {'position': (x, y), 'value': np.random.uniform(1, 10)}
        return self.state
class Relation(AARComponent):
    def __init__(self, name, source, target, relation_type='bidirectional'):
        super().__init__(name)
        self.source = source
        self.target = target
        self.relation_type = relation_type
        self.state = {'active': True, 'strength': 1.0, 'type': relation_type, 'history': []}
        source.add_relation(self)
        target.add_relation(self)
    def process(self, input_data):
        self.source.get_state()
        self.target.get_state()
        if len(self.state['history']) > 0:
            self.state['strength'] *= 0.99
        interaction = {'timestamp': input_data.get('timestamp', 0), 'source_action': input_data.get('source_action'), 'target_response': input_data.get('target_response')}
        if len(self.state['history']) >= 20:
            self.state['history'].pop(0)
        self.state['history'].append(interaction)
        if self.state['strength'] < 0.1:
            self.state['active'] = False
        return self.state
class AARTriad:
    def __init__(self):
        self.agents = {}
        self.arenas = {}
        self.relations = {}
        self.timestamp = 0
    def create_agent(self, name, capabilities=None):
        agent = Agent(name, capabilities)
        self.agents[name] = agent
        return agent
    def create_arena(self, name, size=(10, 10)):
        arena = Arena(name, size)
        self.arenas[name] = arena
        return arena
    def create_relation(self, name, source_name, target_name, relation_type='bidirectional'):
        source = self.agents.get(source_name) or self.arenas.get(source_name)
        target = self.agents.get(target_name) or self.arenas.get(target_name)
        if not source or not target:
            raise ValueError('Source or target component not found')
        relation = Relation(name, source, target, relation_type)
        self.relations[name] = relation
        return relation
    def step(self):
        self.timestamp += 1
        agent_actions = {}
        for name, agent in self.agents.items():
            if agent.state.get('active', False):
                perceptual_input = self._gather_agent_input(agent)
                action = agent.process(perceptual_input)
                agent_actions[name] = action
        arena_states = {}
        for name, arena in self.arenas.items():
            arena_input = {'actions': [action for agent_name, action in agent_actions.items()]}
            state = arena.process(arena_input)
            arena_states[name] = state
        for name, relation in self.relations.items():
            if relation.state.get('active', False):
                source_name = relation.source.name
                target_name = relation.target.name
                relation_input = {'timestamp': self.timestamp, 'source_action': agent_actions.get(source_name), 'target_response': arena_states.get(target_name)}
                relation.process(relation_input)
        return {'timestamp': self.timestamp, 'agents': {name: agent.get_state() for name, agent in self.agents.items()}, 'arenas': {name: arena.get_state() for name, arena in self.arenas.items()}, 'relations': {name: relation.get_state() for name, relation in self.relations.items()}}
    def _gather_agent_input(self, agent):
        perceptual_input = {'timestamp': self.timestamp, 'environments': {}, 'targets': []}
        for relation in agent.relations:
            if isinstance(relation.target, Arena) and relation.state.get('active', False):
                arena = relation.target
                perceptual_input['environments'][arena.name] = arena.get_state()
                for entity_id, entity in arena.state.get('entities', {}).items():
                    perceptual_input['targets'].append(entity.get('position'))
                for resource_id, resource in arena.state.get('resources', {}).items():
                    perceptual_input['targets'].append(resource.get('position'))
        return perceptual_input
    def get_system_state(self):
        return {'timestamp': self.timestamp, 'agents': {name: agent.get_state() for name, agent in self.agents.items()}, 'arenas': {name: arena.get_state() for name, arena in self.arenas.items()}, 'relations': {name: relation.get_state() for name, relation in self.relations.items()}}
if __name__ == '__main__':
    aar = AARTriad()
    aar.create_agent('Explorer', ['move', 'observe', 'modify'])
    aar.create_agent('Creator', ['create', 'destroy', 'modify'])
    aar.create_arena('PhysicalSpace', (20, 20))
    aar.create_arena('ConceptualSpace', (10, 10))
    aar.create_relation('ExplorerInPhysical', 'Explorer', 'PhysicalSpace')
    aar.create_relation('CreatorInConceptual', 'Creator', 'ConceptualSpace')
    aar.create_relation('PhysicalToConceptual', 'PhysicalSpace', 'ConceptualSpace')
    states = []
    for _ in range(10):
        state = aar.step()
        states.append(state)
    print(f'Simulation completed with {len(states)} states')