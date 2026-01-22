import logging
import uuid
from datetime import datetime
from typing import Dict, List, Any
from collections import defaultdict
logger = logging.getLogger(__name__)
class SystemEntelecho:
    def __init__(self):
        self.goals = {}
        self.intentions = {}
        self.strategies = {}
        self.fulfillments = []
        self.goal_parents = defaultdict(set)
        self.goal_children = defaultdict(set)
        self.goal_dependencies = defaultdict(set)
        self.goal_dependents = defaultdict(set)
        self.goal_intentions = defaultdict(set)
        self.intention_goals = defaultdict(set)
        self.goal_progress = {}
        self.intention_progress = {}
        self.goal_metrics = defaultdict(dict)
        self.system_metrics = {}
    def create_goal(self, name: str, description: str, priority: float=1.0, parent_goal_id: str=None, attributes: Dict[str, Any]=None) -> str:
        goal_id = str(uuid.uuid4())
        created_at = datetime.now()
        self.goals[goal_id] = {'id': goal_id, 'name': name, 'description': description, 'priority': priority, 'status': 'active', 'created_at': created_at, 'updated_at': created_at, 'attributes': attributes or {}}
        if parent_goal_id and parent_goal_id in self.goals:
            self.goal_parents[goal_id].add(parent_goal_id)
            self.goal_children[parent_goal_id].add(goal_id)
        self.goal_progress[goal_id] = 0.0
        logger.info(f"Created goal '{name}' with ID {goal_id}")
        return goal_id
    def update_goal(self, goal_id: str, name: str=None, description: str=None, priority: float=None, status: str=None, attributes: Dict[str, Any]=None) -> bool:
        if goal_id not in self.goals:
            logger.error(f'Cannot update goal: {goal_id} not found')
            return False
        goal = self.goals[goal_id]
        if name is not None:
            goal['name'] = name
        if description is not None:
            goal['description'] = description
        if priority is not None:
            goal['priority'] = priority
        if status is not None:
            goal['status'] = status
        if attributes is not None:
            goal['attributes'].update(attributes)
        goal['updated_at'] = datetime.now()
        logger.info(f"Updated goal '{goal['name']}' ({goal_id})")
        return True
    def create_intention(self, name: str, description: str, goal_id: str=None, strategy: str=None, attributes: Dict[str, Any]=None) -> str:
        intention_id = str(uuid.uuid4())
        created_at = datetime.now()
        self.intentions[intention_id] = {'id': intention_id, 'name': name, 'description': description, 'strategy': strategy, 'status': 'active', 'created_at': created_at, 'updated_at': created_at, 'attributes': attributes or {}}
        if goal_id and goal_id in self.goals:
            self.goal_intentions[goal_id].add(intention_id)
            self.intention_goals[intention_id].add(goal_id)
        self.intention_progress[intention_id] = 0.0
        logger.info(f"Created intention '{name}' with ID {intention_id}")
        return intention_id
    def create_strategy(self, name: str, description: str, steps: List[str]=None, goal_ids: List[str]=None, attributes: Dict[str, Any]=None) -> str:
        strategy_id = str(uuid.uuid4())
        created_at = datetime.now()
        self.strategies[strategy_id] = {'id': strategy_id, 'name': name, 'description': description, 'steps': steps or [], 'status': 'active', 'created_at': created_at, 'updated_at': created_at, 'attributes': attributes or {}}
        if goal_ids:
            for goal_id in goal_ids:
                if goal_id in self.goals:
                    self.strategies[strategy_id].setdefault('goals', []).append(goal_id)
        logger.info(f"Created strategy '{name}' with ID {strategy_id}")
        return strategy_id
    def add_goal_dependency(self, goal_id: str, prerequisite_goal_id: str) -> bool:
        if goal_id not in self.goals or prerequisite_goal_id not in self.goals:
            return False
        self.goal_dependencies[goal_id].add(prerequisite_goal_id)
        self.goal_dependents[prerequisite_goal_id].add(goal_id)
        logger.info(f'Added dependency: goal {goal_id} requires {prerequisite_goal_id}')
        return True
    def remove_goal_dependency(self, goal_id: str, prerequisite_goal_id: str) -> bool:
        if goal_id not in self.goal_dependencies:
            return False
        if prerequisite_goal_id not in self.goal_dependencies[goal_id]:
            return False
        self.goal_dependencies[goal_id].remove(prerequisite_goal_id)
        self.goal_dependents[prerequisite_goal_id].remove(goal_id)
        logger.info(f'Removed dependency: goal {goal_id} no longer requires {prerequisite_goal_id}')
        return True
    def set_goal_progress(self, goal_id: str, progress: float) -> bool:
        if goal_id not in self.goals:
            return False
        progress = max(0.0, min(1.0, progress))
        previous = self.goal_progress.get(goal_id, 0.0)
        self.goal_progress[goal_id] = progress
        if progress >= 1.0 and self.goals[goal_id]['status'] != 'completed':
            self.goals[goal_id]['status'] = 'completed'
            self.goals[goal_id]['completed_at'] = datetime.now()
        logger.info(f"Updated goal '{self.goals[goal_id]['name']}' progress: {previous:.2f} -> {progress:.2f}")
        for parent_id in self.goal_parents.get(goal_id, set()):
            self._update_parent_goal_progress(parent_id)
        return True
    def set_intention_progress(self, intention_id: str, progress: float) -> bool:
        if intention_id not in self.intentions:
            return False
        progress = max(0.0, min(1.0, progress))
        previous = self.intention_progress.get(intention_id, 0.0)
        self.intention_progress[intention_id] = progress
        if progress >= 1.0 and self.intentions[intention_id]['status'] != 'completed':
            self.intentions[intention_id]['status'] = 'completed'
            self.intentions[intention_id]['completed_at'] = datetime.now()
        logger.info(f"Updated intention '{self.intentions[intention_id]['name']}' progress: {previous:.2f} -> {progress:.2f}")
        for goal_id in self.intention_goals.get(intention_id, set()):
            self._update_goal_progress_from_intentions(goal_id)
        return True
    def record_fulfillment(self, description: str, goal_id: str=None, intention_id: str=None, value: float=None, details: Dict[str, Any]=None) -> str:
        fulfillment_id = str(uuid.uuid4())
        timestamp = datetime.now()
        fulfillment = {'id': fulfillment_id, 'description': description, 'timestamp': timestamp, 'value': value, 'details': details or {}}
        if goal_id:
            fulfillment['goal_id'] = goal_id
        if intention_id:
            fulfillment['intention_id'] = intention_id
        self.fulfillments.append(fulfillment)
        if goal_id and goal_id in self.goals:
            self.set_goal_progress(goal_id, 1.0)
        if intention_id and intention_id in self.intentions:
            self.set_intention_progress(intention_id, 1.0)
        logger.info(f'Recorded fulfillment: {description}')
        return fulfillment_id
    def update_goal_metric(self, goal_id: str, metric_name: str, value: Any) -> bool:
        if goal_id not in self.goals:
            return False
        previous = self.goal_metrics[goal_id].get(metric_name, None)
        self.goal_metrics[goal_id][metric_name] = value
        logger.info(f"Updated metric '{metric_name}' for goal {goal_id}: {previous} -> {value}")
        return True
    def update_system_metric(self, metric_name: str, value: Any) -> bool:
        previous = self.system_metrics.get(metric_name, None)
        self.system_metrics[metric_name] = value
        logger.info(f"Updated system metric '{metric_name}': {previous} -> {value}")
        return True
    def get_goal(self, goal_id: str) -> Dict[str, Any]:
        if goal_id not in self.goals:
            return None
        goal = dict(self.goals[goal_id])
        goal['progress'] = self.goal_progress.get(goal_id, 0.0)
        goal['metrics'] = dict(self.goal_metrics.get(goal_id, {}))
        goal['parent_goals'] = list(self.goal_parents.get(goal_id, set()))
        goal['child_goals'] = list(self.goal_children.get(goal_id, set()))
        goal['prerequisites'] = list(self.goal_dependencies.get(goal_id, set()))
        goal['dependents'] = list(self.goal_dependents.get(goal_id, set()))
        goal['intentions'] = list(self.goal_intentions.get(goal_id, set()))
        return goal
    def get_intention(self, intention_id: str) -> Dict[str, Any]:
        if intention_id not in self.intentions:
            return None
        intention = dict(self.intentions[intention_id])
        intention['progress'] = self.intention_progress.get(intention_id, 0.0)
        intention['goals'] = list(self.intention_goals.get(intention_id, set()))
        return intention
    def get_strategy(self, strategy_id: str) -> Dict[str, Any]:
        return self.strategies.get(strategy_id)
    def get_system_metrics(self) -> Dict[str, Any]:
        return dict(self.system_metrics)
    def get_fulfillments(self, limit: int=None, goal_id: str=None, intention_id: str=None) -> List[Dict[str, Any]]:
        fulfillments = self.fulfillments
        if goal_id:
            fulfillments = [f for f in fulfillments if f.get('goal_id') == goal_id]
        if intention_id:
            fulfillments = [f for f in fulfillments if f.get('intention_id') == intention_id]
        fulfillments = sorted(fulfillments, key=lambda f: f['timestamp'], reverse=True)
        if limit:
            fulfillments = fulfillments[:limit]
        return fulfillments
    def get_entelecho_state(self) -> Dict[str, Any]:
        active_goals = [g for g in self.goals.values() if g['status'] == 'active']
        completed_goals = [g for g in self.goals.values() if g['status'] == 'completed']
        root_goals = [goal_id for goal_id in self.goals if not self.goal_parents.get(goal_id)]
        if self.goals:
            overall_progress = sum(self.goal_progress.values()) / len(self.goals)
            goals_complete_pct = len(completed_goals) / len(self.goals) if self.goals else 0
        else:
            overall_progress = 0
            goals_complete_pct = 0
        state = {'goals_count': len(self.goals), 'intentions_count': len(self.intentions), 'strategies_count': len(self.strategies), 'fulfillments_count': len(self.fulfillments), 'active_goals_count': len(active_goals), 'completed_goals_count': len(completed_goals), 'overall_progress': overall_progress, 'goals_complete_percentage': goals_complete_pct, 'root_goals': root_goals, 'recent_fulfillments': self.get_fulfillments(limit=5), 'system_metrics': self.get_system_metrics()}
        return state
    def _update_parent_goal_progress(self, parent_id: str):
        if parent_id not in self.goals:
            return
        child_ids = self.goal_children.get(parent_id, set())
        if not child_ids:
            return
        child_progress = [self.goal_progress.get(cid, 0.0) for cid in child_ids]
        average_progress = sum(child_progress) / len(child_progress)
        self.goal_progress[parent_id] = average_progress
        if average_progress >= 1.0 and self.goals[parent_id]['status'] != 'completed':
            self.goals[parent_id]['status'] = 'completed'
            self.goals[parent_id]['completed_at'] = datetime.now()
        for grand_parent_id in self.goal_parents.get(parent_id, set()):
            self._update_parent_goal_progress(grand_parent_id)
    def _update_goal_progress_from_intentions(self, goal_id: str):
        if goal_id not in self.goals:
            return
        intention_ids = self.goal_intentions.get(goal_id, set())
        if not intention_ids:
            return
        intention_progress = [self.intention_progress.get(iid, 0.0) for iid in intention_ids]
        average_progress = sum(intention_progress) / len(intention_progress)
        self.set_goal_progress(goal_id, average_progress)
system_entelecho = SystemEntelecho()
def get_entelecho() -> SystemEntelecho:
    return system_entelecho