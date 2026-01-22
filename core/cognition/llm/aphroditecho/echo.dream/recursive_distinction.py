import re
import json
from typing import Union, List, Dict, Any, Optional, Callable
from models import RecursiveDistinction, SelfReferentialNode, NodeConnection, HyperGNN
from database import db
class DistinctionParser:
    def __init__(self):
        self.tokens = []
        self.position = 0
    def tokenize(self, expression: str) -> List[str]:
        expression = re.sub('\\s+', ' ', expression)
        expression = expression.replace('(', ' ( ').replace(')', ' ) ')
        self.tokens = [token for token in expression.split(' ') if token]
        self.position = 0
        return self.tokens
    def parse(self, expression: str) -> Any:
        self.tokenize(expression)
        return self._parse_expression()
    def _parse_expression(self) -> Any:
        if self.position >= len(self.tokens):
            raise SyntaxError('Unexpected end of expression')
        token = self.tokens[self.position]
        self.position += 1
        if token == '(':
            sub_expr = []
            while self.position < len(self.tokens) and self.tokens[self.position] != ')':
                sub_expr.append(self._parse_expression())
            if self.position >= len(self.tokens):
                raise SyntaxError('Unmatched opening parenthesis')
            self.position += 1
            return sub_expr
        elif token == ')':
            raise SyntaxError('Unexpected closing parenthesis')
        else:
            try:
                return int(token)
            except ValueError:
                try:
                    return float(token)
                except ValueError:
                    return token
class DistinctionEvaluator:
    def __init__(self):
        self.env = {}
        self._init_primitives()
    def _init_primitives(self):
        self.env['identity'] = lambda x: x
        self.env['negate'] = lambda x: not x if isinstance(x, bool) else not bool(x)
        self.env['K'] = lambda x, y=None: x
        self.env['S'] = lambda x, y, z: self.apply(self.apply(x, z), self.apply(y, z))
        self.env['I'] = lambda x: x
    def evaluate(self, expr: Any, env: Optional[Dict]=None) -> Any:
        if env is None:
            env = self.env.copy()
        if isinstance(expr, list):
            if not expr:
                return None
            first = expr[0]
            if first == 'define' or first == 'def':
                if len(expr) != 3:
                    raise SyntaxError(f'Invalid define expression: {expr}')
                symbol, value = (expr[1], self.evaluate(expr[2], env))
                env[symbol] = value
                return value
            elif first == 'lambda' or first == 'λ':
                if len(expr) != 3:
                    raise SyntaxError(f'Invalid lambda expression: {expr}')
                params, body = (expr[1], expr[2])
                if not isinstance(params, list):
                    params = [params]
                return lambda *args: self.evaluate(body, {**env, **dict(zip(params, args))})
            elif first == 'if':
                if len(expr) != 4:
                    raise SyntaxError(f'Invalid if expression: {expr}')
                condition = self.evaluate(expr[1], env)
                if condition:
                    return self.evaluate(expr[2], env)
                else:
                    return self.evaluate(expr[3], env)
            elif first == 'quote' or first == "'":
                if len(expr) != 2:
                    raise SyntaxError(f'Invalid quote expression: {expr}')
                return expr[1]
            else:
                fn = self.evaluate(first, env)
                args = [self.evaluate(arg, env) for arg in expr[1:]]
                return self.apply(fn, *args)
        elif isinstance(expr, str):
            if expr in env:
                return env[expr]
            raise NameError(f"Symbol '{expr}' not found")
        else:
            return expr
    def apply(self, fn: Callable, *args) -> Any:
        if callable(fn):
            return fn(*args)
        raise TypeError(f'Cannot apply non-function: {fn}')
class RecursiveDistinctionManager:
    def __init__(self):
        self.parser = DistinctionParser()
        self.evaluator = DistinctionEvaluator()
    def create_distinction(self, name: str, expression: str, description: Optional[str]=None, user_id: Optional[int]=None, parent_id: Optional[int]=None) -> RecursiveDistinction:
        try:
            parsed = self.parser.parse(expression)
        except SyntaxError as e:
            raise ValueError(f'Invalid expression: {e}')
        distinction = RecursiveDistinction(name=name, expression=expression, description=description, user_id=user_id, parent_id=parent_id)
        metrics = self._calculate_metrics(parsed)
        distinction.set_metrics(metrics)
        db.session.add(distinction)
        db.session.commit()
        return distinction
    def evaluate_distinction(self, distinction_id: int) -> Any:
        distinction = RecursiveDistinction.query.get(distinction_id)
        if not distinction:
            raise ValueError(f'No distinction found with id {distinction_id}')
        parsed = self.parser.parse(distinction.expression)
        result = self.evaluator.evaluate(parsed)
        return result
    def _calculate_metrics(self, parsed_expr: Any) -> Dict[str, Any]:
        metrics = {}
        def get_depth(expr):
            if not isinstance(expr, list):
                return 0
            if not expr:
                return 1
            return 1 + max([get_depth(e) for e in expr], default=0)
        metrics['depth'] = get_depth(parsed_expr)
        def count_distinctions(expr):
            if not isinstance(expr, list):
                return 0
            return 1 + sum((count_distinctions(e) for e in expr))
        metrics['distinctions'] = count_distinctions(parsed_expr)
        metrics['complexity'] = metrics['depth'] * metrics['distinctions']
        return metrics
class HyperGNNManager:
    def __init__(self):
        self.active_networks = {}
    def create_hypergnn(self, name: str, structure: Dict, weights: Optional[Dict]=None, parameters: Optional[Dict]=None, user_id: Optional[int]=None) -> HyperGNN:
        hypergnn = HyperGNN(name=name, user_id=user_id)
        hypergnn.set_structure(structure)
        if weights:
            hypergnn.set_weights(weights)
        if parameters:
            hypergnn.set_parameters(parameters)
        hypergnn.set_loss_history([])
        db.session.add(hypergnn)
        db.session.commit()
        self.active_networks[hypergnn.id] = {'structure': structure, 'weights': weights or {}, 'parameters': parameters or {}, 'epochs': 0, 'loss_history': []}
        return hypergnn
    def load_hypergnn(self, hypergnn_id: int) -> Dict:
        if hypergnn_id in self.active_networks:
            return self.active_networks[hypergnn_id]
        hypergnn = HyperGNN.query.get(hypergnn_id)
        if not hypergnn:
            raise ValueError(f'No HyperGNN found with id {hypergnn_id}')
        network = {'structure': hypergnn.get_structure(), 'weights': hypergnn.get_weights(), 'parameters': hypergnn.get_parameters(), 'epochs': hypergnn.epochs_trained, 'loss_history': hypergnn.get_loss_history()}
        self.active_networks[hypergnn_id] = network
        return network
    def sync_to_db(self, hypergnn_id: int) -> None:
        if hypergnn_id not in self.active_networks:
            raise ValueError(f'No active network with id {hypergnn_id}')
        network = self.active_networks[hypergnn_id]
        hypergnn = HyperGNN.query.get(hypergnn_id)
        if not hypergnn:
            raise ValueError(f'No HyperGNN found in database with id {hypergnn_id}')
        hypergnn.set_structure(network['structure'])
        hypergnn.set_weights(network['weights'])
        hypergnn.set_parameters(network['parameters'])
        hypergnn.epochs_trained = network['epochs']
        hypergnn.set_loss_history(network['loss_history'])
        db.session.commit()
    def update_weights(self, hypergnn_id: int, new_weights: Dict) -> None:
        if hypergnn_id not in self.active_networks:
            self.load_hypergnn(hypergnn_id)
        self.active_networks[hypergnn_id]['weights'] = new_weights
    def record_training(self, hypergnn_id: int, epochs: int, loss: Union[float, List[float]]) -> None:
        if hypergnn_id not in self.active_networks:
            self.load_hypergnn(hypergnn_id)
        network = self.active_networks[hypergnn_id]
        network['epochs'] += epochs
        if isinstance(loss, list):
            network['loss_history'].extend(loss)
        else:
            network['loss_history'].append(loss)
class SelfReferentialNodeManager:
    def __init__(self):
        self.parser = DistinctionParser()
        self.evaluator = DistinctionEvaluator()
    def create_node(self, name: str, node_type: str, expression: Optional[str]=None, value: Optional[Any]=None, parent_id: Optional[int]=None, user_id: Optional[int]=None) -> SelfReferentialNode:
        if expression:
            try:
                self.parser.parse(expression)
            except SyntaxError as e:
                raise ValueError(f'Invalid expression: {e}')
        value_str = None
        if value is not None:
            if isinstance(value, (dict, list, int, float, bool, str)):
                value_str = json.dumps(value)
            else:
                value_str = str(value)
        node = SelfReferentialNode(name=name, node_type=node_type, expression=expression, value=value_str, parent_id=parent_id, user_id=user_id)
        db.session.add(node)
        db.session.commit()
        return node
    def connect_nodes(self, source_id: int, target_id: int, connection_type: str='default', weight: float=1.0, metadata: Optional[Dict]=None) -> NodeConnection:
        source = SelfReferentialNode.query.get(source_id)
        target = SelfReferentialNode.query.get(target_id)
        if not source or not target:
            missing = 'source' if not source else 'target'
            raise ValueError(f'No {missing} node found with provided id')
        connection = NodeConnection(source_id=source_id, target_id=target_id, connection_type=connection_type, weight=weight)
        if metadata:
            connection.conn_data = json.dumps(metadata)
        db.session.add(connection)
        db.session.commit()
        return connection
    def evaluate_node(self, node_id: int, args: Optional[List]=None) -> Any:
        node = SelfReferentialNode.query.get(node_id)
        if not node:
            raise ValueError(f'No node found with id {node_id}')
        if not node.expression:
            return node.get_value()
        parsed = self.parser.parse(node.expression)
        env = self.evaluator.env.copy()
        for child in node.children:
            env[child.name] = child.get_value()
        for conn in node.connections:
            if conn.source_id == node_id:
                env[f'_{conn.target.name}'] = conn.target.get_value()
            else:
                env[f'_{conn.source.name}'] = conn.source.get_value()
        if args:
            env['args'] = args
        result = self.evaluator.evaluate(parsed, env)
        if isinstance(result, (dict, list, int, float, bool, str)):
            node.value = json.dumps(result)
        else:
            node.value = str(result)
        db.session.commit()
        return result
    def build_recursive_system(self, structure: Dict) -> int:
        root = self.create_node(name=structure['name'], node_type=structure['type'], expression=structure.get('expression'), value=structure.get('value'))
        nodes = {structure['name']: root}
        if 'children' in structure:
            for child_struct in structure['children']:
                child = self.build_recursive_system(child_struct)
                child_node = SelfReferentialNode.query.get(child)
                child_node.parent_id = root.id
                nodes[child_struct['name']] = child_node
        if 'connections' in structure:
            for conn in structure['connections']:
                source = nodes[conn['from']]
                target = nodes[conn['to']]
                self.connect_nodes(source_id=source.id, target_id=target.id, connection_type=conn.get('type', 'default'), weight=conn.get('weight', 1.0), metadata=conn.get('metadata'))
        db.session.commit()
        return root.id