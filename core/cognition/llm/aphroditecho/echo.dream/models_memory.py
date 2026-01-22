from datetime import datetime
import json
from database import db
from models import User
memory_cycle_association = db.Table('memory_cycle_association', db.Column('memory_node_id', db.Integer, db.ForeignKey('memory_node.id'), primary_key=True), db.Column('memory_cycle_id', db.Integer, db.ForeignKey('memory_cycle.id'), primary_key=True))
pattern_node_association = db.Table('pattern_node_association', db.Column('memory_node_id', db.Integer, db.ForeignKey('memory_node.id'), primary_key=True), db.Column('pattern_template_id', db.Integer, db.ForeignKey('pattern_template.id'), primary_key=True))
class MemoryCycle(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(64), nullable=False)
    cycle_type = db.Column(db.String(32), nullable=False)
    duration_ms = db.Column(db.Integer, nullable=False)
    variance_percent = db.Column(db.Float, default=10.0)
    last_execution = db.Column(db.DateTime)
    next_scheduled = db.Column(db.DateTime)
    priority = db.Column(db.Integer, default=5)
    enabled = db.Column(db.Boolean, default=True)
    description = db.Column(db.Text)
    user_id = db.Column(db.Integer, db.ForeignKey('user.id'))
    memory_nodes = db.relationship('MemoryNode', secondary=memory_cycle_association, back_populates='processing_cycles')
    function_code = db.Column(db.Text)
    execution_count = db.Column(db.Integer, default=0)
    avg_execution_time_ms = db.Column(db.Float, default=0.0)
    last_result = db.Column(db.Text)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    def get_last_result(self):
        if self.last_result:
            return json.loads(self.last_result)
        return {}
    def set_last_result(self, result_dict):
        self.last_result = json.dumps(result_dict)
    def calculate_next_execution(self):
        import random
        from datetime import timedelta
        variance_factor = 1.0 + random.uniform(-self.variance_percent, self.variance_percent) / 100.0
        actual_duration = self.duration_ms * variance_factor
        if self.last_execution:
            self.next_scheduled = self.last_execution + timedelta(milliseconds=actual_duration)
        else:
            self.next_scheduled = datetime.utcnow() + timedelta(milliseconds=actual_duration)
        return self.next_scheduled
class MemoryNode(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    node_id = db.Column(db.Integer, db.ForeignKey('self_referential_node.id'), nullable=False)
    memory_type = db.Column(db.String(32), nullable=False)
    activation_level = db.Column(db.Float, default=0.0)
    decay_rate = db.Column(db.Float, default=0.05)
    consolidation_stage = db.Column(db.Integer, default=0)
    emotional_valence = db.Column(db.Float, default=0.0)
    emotional_arousal = db.Column(db.Float, default=0.0)
    salience = db.Column(db.Float, default=0.5)
    context = db.Column(db.Text)
    timestamp = db.Column(db.DateTime, default=datetime.utcnow)
    source = db.Column(db.String(64))
    base_node = db.relationship('SelfReferentialNode', foreign_keys=[node_id])
    processing_cycles = db.relationship('MemoryCycle', secondary=memory_cycle_association, back_populates='memory_nodes')
    pattern_templates = db.relationship('PatternTemplate', secondary=pattern_node_association, back_populates='memory_nodes')
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    last_activated = db.Column(db.DateTime)
    activation_count = db.Column(db.Integer, default=0)
    def get_context(self):
        if self.context:
            return json.loads(self.context)
        return {}
    def set_context(self, context_dict):
        self.context = json.dumps(context_dict)
    def activate(self, amount=0.5, record=True):
        self.activation_level = min(1.0, self.activation_level + amount)
        if record:
            self.last_activated = datetime.utcnow()
            self.activation_count += 1
        return self.activation_level
    def decay(self, amount=None):
        if amount is None:
            amount = self.decay_rate
        self.activation_level = max(0.0, self.activation_level - amount)
        return self.activation_level
class MemoryAssociation(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    source_id = db.Column(db.Integer, db.ForeignKey('memory_node.id'), nullable=False)
    target_id = db.Column(db.Integer, db.ForeignKey('memory_node.id'), nullable=False)
    association_type = db.Column(db.String(64), default='default')
    strength = db.Column(db.Float, default=0.5)
    bidirectional = db.Column(db.Boolean, default=True)
    association_metadata = db.Column(db.Text)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    source = db.relationship('MemoryNode', foreign_keys=[source_id], backref='outgoing_associations')
    target = db.relationship('MemoryNode', foreign_keys=[target_id], backref='incoming_associations')
    def get_metadata(self):
        if self.association_metadata:
            return json.loads(self.association_metadata)
        return {}
    def set_metadata(self, metadata_dict):
        self.association_metadata = json.dumps(metadata_dict)
    def strengthen(self, amount=0.1):
        self.strength = min(1.0, self.strength + amount)
        self.updated_at = datetime.utcnow()
        return self.strength
    def weaken(self, amount=0.1):
        self.strength = max(0.0, self.strength - amount)
        self.updated_at = datetime.utcnow()
        return self.strength
class PatternTemplate(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    name = db.Column(db.String(128), nullable=False)
    pattern_type = db.Column(db.String(64), nullable=False)
    structure = db.Column(db.Text)
    rules = db.Column(db.Text)
    activation_threshold = db.Column(db.Float, default=0.7)
    description = db.Column(db.Text)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)
    updated_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)
    user_id = db.Column(db.Integer, db.ForeignKey('user.id'))
    memory_nodes = db.relationship('MemoryNode', secondary=pattern_node_association, back_populates='pattern_templates')
    def get_structure(self):
        if self.structure:
            return json.loads(self.structure)
        return {}
    def set_structure(self, structure_dict):
        self.structure = json.dumps(structure_dict)
    def get_rules(self):
        if self.rules:
            return json.loads(self.rules)
        return {}
    def set_rules(self, rules_dict):
        self.rules = json.dumps(rules_dict)
    def matches(self, memory_nodes):
        return 1.0 if memory_nodes else 0.0
class DreamState(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    title = db.Column(db.String(256))
    start_time = db.Column(db.DateTime, default=datetime.utcnow)
    end_time = db.Column(db.DateTime)
    duration_seconds = db.Column(db.Integer)
    content = db.Column(db.Text)
    source_memories = db.Column(db.Text)
    pattern_activations = db.Column(db.Text)
    insights_generated = db.Column(db.Text)
    new_associations = db.Column(db.Text)
    dream_type = db.Column(db.String(64))
    emotional_tone = db.Column(db.Float, default=0.0)
    coherence = db.Column(db.Float, default=0.5)
    user_id = db.Column(db.Integer, db.ForeignKey('user.id'))
    def get_content(self):
        if self.content:
            return json.loads(self.content)
        return {}
    def set_content(self, content_dict):
        self.content = json.dumps(content_dict)
    def get_source_memories(self):
        if self.source_memories:
            return json.loads(self.source_memories)
        return []
    def set_source_memories(self, memory_ids):
        self.source_memories = json.dumps(memory_ids)
    def get_pattern_activations(self):
        if self.pattern_activations:
            return json.loads(self.pattern_activations)
        return {}
    def set_pattern_activations(self, activations_dict):
        self.pattern_activations = json.dumps(activations_dict)
    def get_insights(self):
        if self.insights_generated:
            return json.loads(self.insights_generated)
        return []
    def set_insights(self, insights_list):
        self.insights_generated = json.dumps(insights_list)
    def get_new_associations(self):
        if self.new_associations:
            return json.loads(self.new_associations)
        return []
    def set_new_associations(self, associations_list):
        self.new_associations = json.dumps(associations_list)
    def finalize(self):
        self.end_time = datetime.utcnow()
        if self.start_time:
            delta = self.end_time - self.start_time
            self.duration_seconds = delta.total_seconds()
        return self.duration_seconds
User.memory_cycles = db.relationship('MemoryCycle', backref='user', lazy='dynamic')
User.pattern_templates = db.relationship('PatternTemplate', backref='user', lazy='dynamic')
User.dream_states = db.relationship('DreamState', backref='user', lazy='dynamic')