import logging
import threading
import time
import random
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Callable
from database import db
from models_memory import MemoryCycle, MemoryNode, MemoryAssociation, PatternTemplate, DreamState
logger = logging.getLogger(__name__)
class TemporalProcessor:
    def __init__(self):
        self.running = False
        self.thread = None
        self.cycle_handlers = {}
        self.default_cycles = {'sensory': {'name': 'sensory_processing', 'cycle_type': 'fast', 'duration_ms': 50, 'variance_percent': 10.0, 'description': 'Fast sensory processing cycle for immediate perceptual input', 'function': self._process_sensory_cycle}, 'working_memory': {'name': 'working_memory', 'cycle_type': 'medium', 'duration_ms': 1000, 'variance_percent': 15.0, 'description': 'Medium-speed cycle for working memory operations', 'function': self._process_working_memory_cycle}, 'consolidation': {'name': 'memory_consolidation', 'cycle_type': 'slow', 'duration_ms': 30000, 'variance_percent': 20.0, 'description': 'Slow cycle for memory consolidation and organization', 'function': self._process_consolidation_cycle}, 'sleep': {'name': 'sleep_cycle', 'cycle_type': 'ultra-slow', 'duration_ms': 300000, 'variance_percent': 30.0, 'description': 'Ultra-slow cycle for REM-like sleep processing and dreaming', 'function': self._process_sleep_cycle}}
        self.active_cycles = {}
        self.recent_activations = []
        self.max_recent = 100
        self.current_dream_state = None
        self.system_load = 0.0
    def register_handler(self, cycle_name: str, handler: Callable):
        self.cycle_handlers[cycle_name] = handler
    def initialize_default_cycles(self):
        from app import app
        with app.app_context():
            for cycle_key, cycle_data in self.default_cycles.items():
                existing = MemoryCycle.query.filter_by(name=cycle_data['name']).first()
                if not existing:
                    cycle = MemoryCycle(name=cycle_data['name'], cycle_type=cycle_data['cycle_type'], duration_ms=cycle_data['duration_ms'], variance_percent=cycle_data['variance_percent'], description=cycle_data['description'])
                    self.register_handler(cycle_data['name'], cycle_data['function'])
                    db.session.add(cycle)
            db.session.commit()
            logger.info('Default temporal cycles initialized')
            self._load_active_cycles()
    def _load_active_cycles(self):
        active_cycles = MemoryCycle.query.filter_by(enabled=True).all()
        for cycle in active_cycles:
            if not cycle.next_scheduled or cycle.next_scheduled < datetime.utcnow():
                cycle.calculate_next_execution()
                db.session.commit()
            self.active_cycles[cycle.id] = cycle.next_scheduled
            if cycle.name in self.default_cycles and cycle.name not in self.cycle_handlers:
                self.register_handler(cycle.name, self.default_cycles[cycle.name]['function'])
        logger.info(f'Loaded {len(self.active_cycles)} active temporal cycles')
    def start(self):
        if self.running:
            logger.warning('Temporal processor already running')
            return
        self.running = True
        self.thread = threading.Thread(target=self._run_loop)
        self.thread.daemon = True
        self.thread.start()
        logger.info('Temporal processor started')
    def stop(self):
        self.running = False
        if self.thread:
            self.thread.join(timeout=1.0)
            self.thread = None
        logger.info('Temporal processor stopped')
    def _run_loop(self):
        from app import app
        logger.info('Temporal processor loop started')
        while self.running:
            with app.app_context():
                current_time = datetime.utcnow()
                ready_cycles = []
                for cycle_id, next_time in list(self.active_cycles.items()):
                    if next_time <= current_time:
                        ready_cycles.append(cycle_id)
                for cycle_id in ready_cycles:
                    try:
                        self._execute_cycle(cycle_id)
                    except Exception as e:
                        logger.error(f'Error executing cycle {cycle_id}: {e}')
                self.system_load = min(1.0, len(ready_cycles) / max(1, len(self.active_cycles)))
            time.sleep(0.01)
    def _execute_cycle(self, cycle_id: int):
        cycle = MemoryCycle.query.get(cycle_id)
        if not cycle or not cycle.enabled:
            if cycle_id in self.active_cycles:
                del self.active_cycles[cycle_id]
            return
        start_time = datetime.utcnow()
        result = None
        try:
            if cycle.name in self.cycle_handlers:
                handler = self.cycle_handlers[cycle.name]
                result = handler(cycle)
            else:
                result = self._default_cycle_handler(cycle)
            cycle.last_execution = start_time
            cycle.execution_count += 1
            end_time = datetime.utcnow()
            execution_time_ms = (end_time - start_time).total_seconds() * 1000
            if cycle.execution_count > 1:
                cycle.avg_execution_time_ms = (cycle.avg_execution_time_ms * (cycle.execution_count - 1) + execution_time_ms) / cycle.execution_count
            else:
                cycle.avg_execution_time_ms = execution_time_ms
            if result:
                cycle.set_last_result(result)
            cycle.calculate_next_execution()
            self.active_cycles[cycle_id] = cycle.next_scheduled
            db.session.commit()
            logger.debug(f'Executed cycle {cycle.name} in {execution_time_ms:.2f}ms')
        except Exception as e:
            logger.error(f'Error in cycle {cycle.name}: {e}')
            cycle.calculate_next_execution()
            self.active_cycles[cycle_id] = cycle.next_scheduled
            db.session.commit()
    def _default_cycle_handler(self, cycle: MemoryCycle) -> Dict:
        return {'status': 'default_executed', 'cycle_name': cycle.name, 'timestamp': datetime.utcnow().isoformat()}
    def _process_sensory_cycle(self, cycle: MemoryCycle) -> Dict:
        active_nodes = MemoryNode.query.filter(MemoryNode.activation_level > 0).all()
        for node in active_nodes:
            node.decay()
        db.session.commit()
        return {'status': 'processed', 'active_nodes_count': len(active_nodes), 'timestamp': datetime.utcnow().isoformat()}
    def _process_working_memory_cycle(self, cycle: MemoryCycle) -> Dict:
        nodes = MemoryNode.query.all()
        if nodes:
            random_node = random.choice(nodes)
            random_node.activate()
            self.recent_activations.append({'node_id': random_node.id, 'timestamp': datetime.utcnow().isoformat(), 'cycle_name': cycle.name})
            if len(self.recent_activations) > self.max_recent:
                self.recent_activations = self.recent_activations[-self.max_recent:]
            db.session.commit()
            return {'status': 'processed', 'activated_node': random_node.id, 'memory_type': random_node.memory_type, 'timestamp': datetime.utcnow().isoformat()}
        return {'status': 'no_nodes', 'timestamp': datetime.utcnow().isoformat()}
    def _process_consolidation_cycle(self, cycle: MemoryCycle) -> Dict:
        recent_node_ids = [a['node_id'] for a in self.recent_activations[-20:]]
        if not recent_node_ids:
            return {'status': 'no_recent_activations', 'timestamp': datetime.utcnow().isoformat()}
        nodes = MemoryNode.query.filter(MemoryNode.id.in_(recent_node_ids)).all()
        if len(nodes) < 2:
            return {'status': 'insufficient_nodes', 'timestamp': datetime.utcnow().isoformat()}
        new_associations = []
        for i, node1 in enumerate(nodes):
            for node2 in nodes[i + 1:]:
                existing = MemoryAssociation.query.filter((MemoryAssociation.source_id == node1.id) & (MemoryAssociation.target_id == node2.id) | (MemoryAssociation.source_id == node2.id) & (MemoryAssociation.target_id == node1.id)).first()
                if not existing:
                    assoc = MemoryAssociation(source_id=node1.id, target_id=node2.id, association_type='temporal_co_activation', strength=0.5, bidirectional=True)
                    db.session.add(assoc)
                    new_associations.append({'source_id': node1.id, 'target_id': node2.id})
        for node in nodes:
            node.consolidation_stage += 1
        db.session.commit()
        return {'status': 'processed', 'nodes_consolidated': len(nodes), 'new_associations': len(new_associations), 'timestamp': datetime.utcnow().isoformat()}
    def _process_sleep_cycle(self, cycle: MemoryCycle) -> Dict:
        if self.current_dream_state:
            dream = DreamState.query.get(self.current_dream_state)
            if dream:
                dream.finalize()
                db.session.commit()
                result = {'status': 'dream_completed', 'dream_id': dream.id, 'duration_seconds': dream.duration_seconds, 'timestamp': datetime.utcnow().isoformat()}
                self.current_dream_state = None
                return result
        dream = DreamState(title=f"Dream {datetime.utcnow().strftime('%Y%m%d-%H%M%S')}", dream_type='rem', start_time=datetime.utcnow())
        memory_sources = MemoryNode.query.filter((MemoryNode.activation_level > 0.7) | (MemoryNode.last_activated > datetime.utcnow() - timedelta(hours=24))).limit(20).all()
        source_ids = [node.id for node in memory_sources]
        dream.set_source_memories(source_ids)
        patterns = PatternTemplate.query.all()
        pattern_activations = {}
        for pattern in patterns:
            match_score = pattern.matches(memory_sources)
            if match_score >= pattern.activation_threshold:
                pattern_activations[pattern.id] = match_score
        dream.set_pattern_activations(pattern_activations)
        insights = [f'Insight from pattern {pattern_id}: Connection strength {activation:.2f}' for pattern_id, activation in pattern_activations.items()]
        dream.set_insights(insights)
        new_associations = []
        if pattern_activations and memory_sources:
            for i, node1 in enumerate(memory_sources):
                for node2 in memory_sources[i + 1:]:
                    if random.random() < 0.2:
                        existing = MemoryAssociation.query.filter((MemoryAssociation.source_id == node1.id) & (MemoryAssociation.target_id == node2.id) | (MemoryAssociation.source_id == node2.id) & (MemoryAssociation.target_id == node1.id)).first()
                        if not existing:
                            assoc = MemoryAssociation(source_id=node1.id, target_id=node2.id, association_type='dream_integration', strength=0.3, bidirectional=True)
                            db.session.add(assoc)
                            new_associations.append({'source_id': node1.id, 'target_id': node2.id})
        dream.set_new_associations(new_associations)
        dream.set_content({'narrative': 'Dream narrative would be generated here', 'symbols': ['symbol1', 'symbol2', 'symbol3'], 'emotions': {'joy': 0.3, 'fear': 0.1, 'curiosity': 0.6}})
        dream.emotional_tone = random.uniform(-0.3, 0.7)
        dream.coherence = random.uniform(0.3, 0.8)
        db.session.add(dream)
        db.session.commit()
        self.current_dream_state = dream.id
        return {'status': 'dream_started', 'dream_id': dream.id, 'source_memories': len(source_ids), 'patterns_activated': len(pattern_activations), 'insights_generated': len(insights), 'new_associations': len(new_associations), 'timestamp': datetime.utcnow().isoformat()}
    def create_memory_node(self, base_node_id: int, memory_type: str, activation: float=0.5, **kwargs) -> MemoryNode:
        memory_node = MemoryNode(node_id=base_node_id, memory_type=memory_type, activation_level=activation, **kwargs)
        db.session.add(memory_node)
        db.session.commit()
        return memory_node
    def get_active_memories(self, threshold: float=0.3, limit: int=10) -> List[MemoryNode]:
        return MemoryNode.query.filter(MemoryNode.activation_level >= threshold).order_by(MemoryNode.activation_level.desc()).limit(limit).all()
    def get_memory_by_type(self, memory_type: str, limit: int=10) -> List[MemoryNode]:
        return MemoryNode.query.filter_by(memory_type=memory_type).limit(limit).all()
    def activate_memory(self, memory_id: int, amount: float=0.5) -> float:
        memory_node = MemoryNode.query.get(memory_id)
        if memory_node:
            activation = memory_node.activate(amount)
            db.session.commit()
            self.recent_activations.append({'node_id': memory_node.id, 'timestamp': datetime.utcnow().isoformat(), 'cycle_name': 'explicit_activation'})
            if len(self.recent_activations) > self.max_recent:
                self.recent_activations = self.recent_activations[-self.max_recent:]
            return activation
        return 0.0
    def create_pattern_template(self, name: str, pattern_type: str, structure: Dict, rules: Dict, user_id: Optional[int]=None) -> PatternTemplate:
        pattern = PatternTemplate(name=name, pattern_type=pattern_type, user_id=user_id)
        pattern.set_structure(structure)
        pattern.set_rules(rules)
        db.session.add(pattern)
        db.session.commit()
        return pattern
    def get_dream_history(self, limit: int=10) -> List[DreamState]:
        return DreamState.query.order_by(DreamState.start_time.desc()).limit(limit).all()
    def get_system_state(self) -> Dict:
        cycle_counts = {}
        for cycle_id in self.active_cycles:
            cycle = MemoryCycle.query.get(cycle_id)
            if cycle:
                cycle_type = cycle.cycle_type
                cycle_counts[cycle_type] = cycle_counts.get(cycle_type, 0) + 1
        memory_counts = {}
        memory_types = db.session.query(MemoryNode.memory_type, db.func.count(MemoryNode.id)).group_by(MemoryNode.memory_type).all()
        for memory_type, count in memory_types:
            memory_counts[memory_type] = count
        active_count = MemoryNode.query.filter(MemoryNode.activation_level > 0).count()
        current_dream = None
        if self.current_dream_state:
            dream = DreamState.query.get(self.current_dream_state)
            if dream:
                current_dream = {'id': dream.id, 'title': dream.title, 'start_time': dream.start_time.isoformat() if dream.start_time else None, 'duration_so_far': (datetime.utcnow() - dream.start_time).total_seconds() if dream.start_time else 0, 'type': dream.dream_type}
        return {'running': self.running, 'system_load': self.system_load, 'active_cycles': len(self.active_cycles), 'cycle_counts': cycle_counts, 'memory_counts': memory_counts, 'active_memories': active_count, 'recent_activations': len(self.recent_activations), 'current_dream': current_dream, 'timestamp': datetime.utcnow().isoformat()}
temporal_processor = TemporalProcessor()