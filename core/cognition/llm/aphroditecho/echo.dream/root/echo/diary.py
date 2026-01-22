import logging
import uuid
from datetime import datetime
from typing import Dict, List, Any
from collections import defaultdict
logger = logging.getLogger(__name__)
class WorkspaceDiary:
    def __init__(self):
        self.journals = {}
        self.entries = {}
        self.reflections = {}
        self.insights = {}
        self.journal_entries = defaultdict(list)
        self.entry_reflections = defaultdict(list)
        self.reflection_insights = defaultdict(list)
        self.journal_types = defaultdict(list)
        self.entry_types = defaultdict(list)
        self.reflection_types = defaultdict(list)
        self.tags = defaultdict(list)
        self.entity_tags = defaultdict(list)
        self.entry_importance = {}
        self.insight_relevance = {}
    def create_journal(self, name: str, journal_type: str='generic', tags: List[str]=None, description: str=None, attributes: Dict[str, Any]=None) -> str:
        journal_id = str(uuid.uuid4())
        created_at = datetime.now()
        self.journals[journal_id] = {'id': journal_id, 'name': name, 'type': journal_type, 'description': description, 'created_at': created_at, 'updated_at': created_at, 'entry_count': 0}
        self.journal_types[journal_type].append(journal_id)
        if tags:
            for tag in tags:
                self.tags[tag].append(('journal', journal_id))
                self.entity_tags[journal_id].append(tag)
        if attributes:
            self.journals[journal_id]['attributes'] = attributes
        logger.info(f"Created journal '{name}' of type '{journal_type}' with ID {journal_id}")
        return journal_id
    def create_entry(self, title: str, content: str, journal_id: str, entry_type: str='note', tags: List[str]=None, importance: float=0.5, attributes: Dict[str, Any]=None) -> str:
        if journal_id not in self.journals:
            logger.error(f'Cannot create entry: journal {journal_id} not found')
            return None
        entry_id = str(uuid.uuid4())
        created_at = datetime.now()
        self.entries[entry_id] = {'id': entry_id, 'title': title, 'content': content, 'journal_id': journal_id, 'type': entry_type, 'created_at': created_at, 'updated_at': created_at}
        self.journal_entries[journal_id].append(entry_id)
        self.journals[journal_id]['entry_count'] += 1
        self.journals[journal_id]['updated_at'] = created_at
        self.entry_types[entry_type].append(entry_id)
        if tags:
            for tag in tags:
                self.tags[tag].append(('entry', entry_id))
                self.entity_tags[entry_id].append(tag)
        self.entry_importance[entry_id] = importance
        if attributes:
            self.entries[entry_id]['attributes'] = attributes
        logger.info(f"Created entry '{title}' in journal '{self.journals[journal_id]['name']}'")
        return entry_id
    def create_reflection(self, title: str, content: str, reference_entries: List[str]=None, reflection_type: str='synthesis', tags: List[str]=None, attributes: Dict[str, Any]=None) -> str:
        reflection_id = str(uuid.uuid4())
        created_at = datetime.now()
        self.reflections[reflection_id] = {'id': reflection_id, 'title': title, 'content': content, 'type': reflection_type, 'created_at': created_at, 'updated_at': created_at, 'reference_entries': reference_entries or []}
        if reference_entries:
            for entry_id in reference_entries:
                if entry_id in self.entries:
                    self.entry_reflections[entry_id].append(reflection_id)
        self.reflection_types[reflection_type].append(reflection_id)
        if tags:
            for tag in tags:
                self.tags[tag].append(('reflection', reflection_id))
                self.entity_tags[reflection_id].append(tag)
        if attributes:
            self.reflections[reflection_id]['attributes'] = attributes
        logger.info(f"Created reflection '{title}' of type '{reflection_type}'")
        return reflection_id
    def create_insight(self, title: str, content: str, source_reflections: List[str]=None, relevance: float=0.5, tags: List[str]=None, attributes: Dict[str, Any]=None) -> str:
        insight_id = str(uuid.uuid4())
        created_at = datetime.now()
        self.insights[insight_id] = {'id': insight_id, 'title': title, 'content': content, 'created_at': created_at, 'updated_at': created_at, 'source_reflections': source_reflections or []}
        if source_reflections:
            for reflection_id in source_reflections:
                if reflection_id in self.reflections:
                    self.reflection_insights[reflection_id].append(insight_id)
        if tags:
            for tag in tags:
                self.tags[tag].append(('insight', insight_id))
                self.entity_tags[insight_id].append(tag)
        self.insight_relevance[insight_id] = relevance
        if attributes:
            self.insights[insight_id]['attributes'] = attributes
        logger.info(f"Created insight '{title}'")
        return insight_id
    def update_entry(self, entry_id: str, title: str=None, content: str=None, importance: float=None, tags: List[str]=None, attributes: Dict[str, Any]=None) -> bool:
        if entry_id not in self.entries:
            logger.error(f'Cannot update entry: {entry_id} not found')
            return False
        entry = self.entries[entry_id]
        if title is not None:
            entry['title'] = title
        if content is not None:
            entry['content'] = content
        if importance is not None:
            importance = max(0.0, min(1.0, importance))
            self.entry_importance[entry_id] = importance
        if tags is not None:
            for tag in self.entity_tags.get(entry_id, []):
                if ('entry', entry_id) in self.tags[tag]:
                    self.tags[tag].remove(('entry', entry_id))
            self.entity_tags[entry_id] = []
            for tag in tags:
                self.tags[tag].append(('entry', entry_id))
                self.entity_tags[entry_id].append(tag)
        if attributes is not None:
            entry.setdefault('attributes', {}).update(attributes)
        entry['updated_at'] = datetime.now()
        if 'journal_id' in entry:
            self.journals[entry['journal_id']]['updated_at'] = datetime.now()
        logger.info(f"Updated entry '{entry['title']}' ({entry_id})")
        return True
    def get_journal(self, journal_id: str) -> Dict[str, Any]:
        if journal_id not in self.journals:
            return None
        journal = dict(self.journals[journal_id])
        journal['entries'] = list(self.journal_entries.get(journal_id, []))
        journal['tags'] = list(self.entity_tags.get(journal_id, []))
        return journal
    def get_entry(self, entry_id: str) -> Dict[str, Any]:
        if entry_id not in self.entries:
            return None
        entry = dict(self.entries[entry_id])
        entry['reflections'] = list(self.entry_reflections.get(entry_id, []))
        entry['tags'] = list(self.entity_tags.get(entry_id, []))
        entry['importance'] = self.entry_importance.get(entry_id, 0.5)
        return entry
    def get_reflection(self, reflection_id: str) -> Dict[str, Any]:
        if reflection_id not in self.reflections:
            return None
        reflection = dict(self.reflections[reflection_id])
        reflection['insights'] = list(self.reflection_insights.get(reflection_id, []))
        reflection['tags'] = list(self.entity_tags.get(reflection_id, []))
        return reflection
    def get_insight(self, insight_id: str) -> Dict[str, Any]:
        if insight_id not in self.insights:
            return None
        insight = dict(self.insights[insight_id])
        insight['tags'] = list(self.entity_tags.get(insight_id, []))
        insight['relevance'] = self.insight_relevance.get(insight_id, 0.5)
        return insight
    def get_entries_by_tag(self, tag: str) -> List[str]:
        return [entity_id for entity_type, entity_id in self.tags.get(tag, []) if entity_type == 'entry']
    def get_entries_by_importance(self, min_importance: float=0.7) -> List[str]:
        return [entry_id for entry_id, importance in self.entry_importance.items() if importance >= min_importance]
    def get_insights_by_relevance(self, min_relevance: float=0.7) -> List[str]:
        return [insight_id for insight_id, relevance in self.insight_relevance.items() if relevance >= min_relevance]
    def search_entries(self, query: str, journal_id: str=None) -> List[str]:
        results = []
        for entry_id, entry in self.entries.items():
            if journal_id and entry.get('journal_id') != journal_id:
                continue
            if query.lower() in entry.get('title', '').lower() or query.lower() in entry.get('content', '').lower():
                results.append(entry_id)
        return results
    def get_diary_state(self) -> Dict[str, Any]:
        total_journals = len(self.journals)
        total_entries = len(self.entries)
        total_reflections = len(self.reflections)
        total_insights = len(self.insights)
        total_tags = len(self.tags)
        avg_importance = sum(self.entry_importance.values()) / len(self.entry_importance) if self.entry_importance else 0
        avg_relevance = sum(self.insight_relevance.values()) / len(self.insight_relevance) if self.insight_relevance else 0
        tag_counts = {tag: len(entities) for tag, entities in self.tags.items()}
        top_tags = sorted(tag_counts.items(), key=lambda x: x[1], reverse=True)[:10]
        journal_stats = {}
        for j_type, j_ids in self.journal_types.items():
            total_type_entries = sum((len(self.journal_entries.get(j_id, [])) for j_id in j_ids))
            journal_stats[j_type] = {'count': len(j_ids), 'entries': total_type_entries}
        state = {'journal_count': total_journals, 'entry_count': total_entries, 'reflection_count': total_reflections, 'insight_count': total_insights, 'tag_count': total_tags, 'average_entry_importance': avg_importance, 'average_insight_relevance': avg_relevance, 'journal_types': journal_stats, 'top_tags': dict(top_tags), 'recent_update': max([j.get('updated_at', datetime.min) for j in self.journals.values()], default=datetime.min)}
        return state
workspace_diary = WorkspaceDiary()
def get_diary() -> WorkspaceDiary:
    return workspace_diary