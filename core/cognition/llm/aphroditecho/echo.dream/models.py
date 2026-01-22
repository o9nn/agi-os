from database import db
from datetime import datetime
from flask_login import UserMixin
from werkzeug.security import generate_password_hash, check_password_hash
import json
class User(UserMixin, db.Model):
    id = db.Column(db.Integer, primary_key=True)
    username = db.Column(db.String(64), unique=True, nullable=False, index=True)
    email = db.Column(db.String(120), unique=True, nullable=False, index=True)
    password_hash = db.Column(db.String(256))
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    last_login = db.Column(db.DateTime)
    projects = db.relationship('Project', backref='creator', lazy='dynamic')
    simulations = db.relationship('Simulation', backref='creator', lazy='dynamic')
    def set_password(self, password):
        self.password_hash = generate_password_hash(password)
    def check_password(self, password):
        return check_password_hash(self.password_hash, password)
class Project(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(128), nullable=False)
    description = db.Column(db.Text)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    user_id = db.Column(db.Integer, db.ForeignKey('user.id'))
    namespace = db.Column(db.String(64), nullable=False, default='default')
    simulations = db.relationship('Simulation', backref='project', lazy='dynamic')
class Simulation(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(128), nullable=False)
    description = db.Column(db.Text)
    engine_type = db.Column(db.String(64), nullable=False, default='dte')
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    user_id = db.Column(db.Integer, db.ForeignKey('user.id'))
    project_id = db.Column(db.Integer, db.ForeignKey('project.id'))
    config = db.Column(db.Text)
    snapshots = db.relationship('SimulationSnapshot', backref='simulation', lazy='dynamic')
    def get_config(self):
        if self.config:
            return json.loads(self.config)
        return {}
    def set_config(self, config_dict):
        self.config = json.dumps(config_dict)
class SimulationSnapshot(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    simulation_id = db.Column(db.Integer, db.ForeignKey('simulation.id'))
    timestamp = db.Column(db.DateTime, default=datetime.utcnow)
    state_data = db.Column(db.Text)
    def get_state(self):
        if self.state_data:
            return json.loads(self.state_data)
        return {}
    def set_state(self, state_dict):
        self.state_data = json.dumps(state_dict)
class RecursivePattern(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(128), nullable=False)
    pattern_type = db.Column(db.String(64), nullable=False)
    code = db.Column(db.Text, nullable=False)
    description = db.Column(db.Text)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    user_id = db.Column(db.Integer, db.ForeignKey('user.id'), nullable=True)
    is_builtin = db.Column(db.Boolean, default=False)
class DiaryEntry(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    title = db.Column(db.String(256), nullable=False)
    content = db.Column(db.Text, nullable=False)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    modified_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    tags = db.Column(db.Text)
    def get_tags(self):
        if self.tags:
            return json.loads(self.tags)
        return []
    def set_tags(self, tags_list):
        self.tags = json.dumps(tags_list)
class AARComponent(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(128), nullable=False)
    component_type = db.Column(db.String(64), nullable=False)
    config = db.Column(db.Text)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    def get_config(self):
        if self.config:
            return json.loads(self.config)
        return {}
    def set_config(self, config_dict):
        self.config = json.dumps(config_dict)
class RecursiveDistinction(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(128), nullable=False)
    expression = db.Column(db.Text, nullable=False)
    description = db.Column(db.Text)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    user_id = db.Column(db.Integer, db.ForeignKey('user.id'), nullable=True)
    parent_id = db.Column(db.Integer, db.ForeignKey('recursive_distinction.id'), nullable=True)
    children = db.relationship('RecursiveDistinction', backref=db.backref('parent', remote_side=[id]), lazy='dynamic')
    metrics = db.Column(db.Text)
    def get_metrics(self):
        if self.metrics:
            return json.loads(self.metrics)
        return {}
    def set_metrics(self, metrics_dict):
        self.metrics = json.dumps(metrics_dict)
class HyperGNN(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(128), nullable=False)
    structure = db.Column(db.Text)
    weights = db.Column(db.Text)
    parameters = db.Column(db.Text)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    user_id = db.Column(db.Integer, db.ForeignKey('user.id'), nullable=True)
    epochs_trained = db.Column(db.Integer, default=0)
    loss_history = db.Column(db.Text)
    def get_structure(self):
        if self.structure:
            return json.loads(self.structure)
        return {}
    def set_structure(self, structure_dict):
        self.structure = json.dumps(structure_dict)
    def get_weights(self):
        if self.weights:
            return json.loads(self.weights)
        return {}
    def set_weights(self, weights_dict):
        self.weights = json.dumps(weights_dict)
    def get_parameters(self):
        if self.parameters:
            return json.loads(self.parameters)
        return {}
    def set_parameters(self, params_dict):
        self.parameters = json.dumps(params_dict)
    def get_loss_history(self):
        if self.loss_history:
            return json.loads(self.loss_history)
        return []
    def set_loss_history(self, loss_list):
        self.loss_history = json.dumps(loss_list)
class SelfReferentialNode(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(128), nullable=False)
    node_type = db.Column(db.String(64), nullable=False)
    expression = db.Column(db.Text)
    value = db.Column(db.Text)
    parent_id = db.Column(db.Integer, db.ForeignKey('self_referential_node.id'), nullable=True)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    user_id = db.Column(db.Integer, db.ForeignKey('user.id'), nullable=True)
    children = db.relationship('SelfReferentialNode', backref=db.backref('parent', remote_side=[id]), lazy='dynamic')
    connections = db.relationship('NodeConnection', primaryjoin='or_(SelfReferentialNode.id==NodeConnection.source_id, SelfReferentialNode.id==NodeConnection.target_id)', lazy='dynamic', overlaps='incoming_connections,outgoing_connections')
    def get_value(self):
        if self.value:
            try:
                return json.loads(self.value)
            except json.JSONDecodeError:
                return self.value
        return None
class NodeConnection(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    source_id = db.Column(db.Integer, db.ForeignKey('self_referential_node.id'), nullable=False)
    target_id = db.Column(db.Integer, db.ForeignKey('self_referential_node.id'), nullable=False)
    connection_type = db.Column(db.String(64), default='default')
    weight = db.Column(db.Float, default=1.0)
    conn_data = db.Column(db.Text)
    source = db.relationship('SelfReferentialNode', foreign_keys=[source_id], backref='outgoing_connections', overlaps='connections')
    target = db.relationship('SelfReferentialNode', foreign_keys=[target_id], backref='incoming_connections', overlaps='connections')
    def get_conn_data(self):
        if self.conn_data:
            return json.loads(self.conn_data)
        return {}
class SimulationThought(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    content = db.Column(db.Text, nullable=False)
    thought_type = db.Column(db.String(64), default='thought')
    timestamp = db.Column(db.DateTime, default=datetime.utcnow)
    state = db.Column(db.String(128))
    recursion_level = db.Column(db.Integer, default=0)
    engine_type = db.Column(db.String(64), default='dte')
    session_id = db.Column(db.String(64))
    def to_dict(self):
        return {'id': self.id, 'content': self.content, 'type': self.thought_type, 'timestamp': self.timestamp.isoformat(), 'state': self.state, 'recursion_level': self.recursion_level}