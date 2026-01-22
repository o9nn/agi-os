from datetime import datetime
import json
from database import db
class ThoughtLog(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    timestamp = db.Column(db.DateTime, default=datetime.utcnow, index=True)
    content = db.Column(db.Text, nullable=False)
    thought_type = db.Column(db.String(64), index=True)
    source = db.Column(db.String(64))
    state_before = db.Column(db.Text)
    state_after = db.Column(db.Text)
    generation_time_ms = db.Column(db.Float)
    recursive_depth = db.Column(db.Integer)
    tags = db.Column(db.Text)
    session_id = db.Column(db.String(64), index=True)
    analysis = db.Column(db.Text)
    flagged = db.Column(db.Boolean, default=False)
    flag_reason = db.Column(db.String(128))
    def get_state_before(self):
        if self.state_before:
            return json.loads(self.state_before)
        return {}
    def set_state_before(self, state_dict):
        self.state_before = json.dumps(state_dict)
    def get_state_after(self):
        if self.state_after:
            return json.loads(self.state_after)
        return {}
    def set_state_after(self, state_dict):
        self.state_after = json.dumps(state_dict)
    def get_tags(self):
        if self.tags:
            return json.loads(self.tags)
        return []
    def set_tags(self, tags_list):
        self.tags = json.dumps(tags_list)
    def get_analysis(self):
        if self.analysis:
            return json.loads(self.analysis)
        return {}
    def set_analysis(self, analysis_dict):
        self.analysis = json.dumps(analysis_dict)
class DreamLog(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    dream_id = db.Column(db.Integer, index=True)
    timestamp = db.Column(db.DateTime, default=datetime.utcnow, index=True)
    title = db.Column(db.String(256))
    content = db.Column(db.Text, nullable=False)
    dream_type = db.Column(db.String(64), index=True)
    start_time = db.Column(db.DateTime)
    end_time = db.Column(db.DateTime)
    duration_seconds = db.Column(db.Integer)
    source_memories = db.Column(db.Text)
    pattern_activations = db.Column(db.Text)
    insights_generated = db.Column(db.Text)
    new_associations = db.Column(db.Text)
    emotional_tone = db.Column(db.Float)
    coherence = db.Column(db.Float)
    analysis = db.Column(db.Text)
    flagged = db.Column(db.Boolean, default=False)
    flag_reason = db.Column(db.String(128))
    session_id = db.Column(db.String(64), index=True)
    def get_content(self):
        if self.content:
            try:
                return json.loads(self.content)
            except:
                return self.content
        return {}
    def set_content(self, content):
        if isinstance(content, dict):
            self.content = json.dumps(content)
        else:
            self.content = content
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
    def get_analysis(self):
        if self.analysis:
            return json.loads(self.analysis)
        return {}
    def set_analysis(self, analysis_dict):
        self.analysis = json.dumps(analysis_dict)
class ChatLog(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    timestamp = db.Column(db.DateTime, default=datetime.utcnow, index=True)
    message_type = db.Column(db.String(64), index=True)
    content = db.Column(db.Text, nullable=False)
    conversation_id = db.Column(db.String(64), index=True)
    parent_message_id = db.Column(db.Integer)
    user_id = db.Column(db.Integer, db.ForeignKey('user.id'), nullable=True)
    system_state = db.Column(db.Text)
    processing_time_ms = db.Column(db.Float)
    response_to = db.Column(db.Integer)
    response_type = db.Column(db.String(64))
    analysis = db.Column(db.Text)
    flagged = db.Column(db.Boolean, default=False)
    flag_reason = db.Column(db.String(128))
    def get_system_state(self):
        if self.system_state:
            return json.loads(self.system_state)
        return {}
    def set_system_state(self, state_dict):
        self.system_state = json.dumps(state_dict)
    def get_analysis(self):
        if self.analysis:
            return json.loads(self.analysis)
        return {}
    def set_analysis(self, analysis_dict):
        self.analysis = json.dumps(analysis_dict)
class DiagnosticConfig(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    last_updated = db.Column(db.DateTime, default=datetime.utcnow)
    enabled = db.Column(db.Boolean, default=True)
    log_thoughts = db.Column(db.Boolean, default=True)
    log_dreams = db.Column(db.Boolean, default=True)
    log_chats = db.Column(db.Boolean, default=True)
    retention_days = db.Column(db.Integer, default=90)
    thought_sampling_rate = db.Column(db.Float, default=1.0)
    perform_analysis = db.Column(db.Boolean, default=False)
    analysis_delay_seconds = db.Column(db.Integer, default=3600)
    flagging_enabled = db.Column(db.Boolean, default=True)
    flagging_criteria = db.Column(db.Text)
    def get_flagging_criteria(self):
        if self.flagging_criteria:
            return json.loads(self.flagging_criteria)
        return {}
    def set_flagging_criteria(self, criteria_dict):
        self.flagging_criteria = json.dumps(criteria_dict)