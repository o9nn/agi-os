import os
import logging
import json
from datetime import datetime
logger = logging.getLogger(__name__)
class NamespaceManager:
    def __init__(self):
        self.namespaces = {}
        self.active_namespace = None
        self.namespace_dir = os.path.dirname(os.path.abspath(__file__))
        self.load_namespaces()
    def load_namespaces(self):
        for ns_dir in os.listdir(self.namespace_dir):
            ns_path = os.path.join(self.namespace_dir, ns_dir)
            if os.path.isdir(ns_path) and (not ns_dir.startswith('__')):
                self.namespaces[ns_dir] = {'path': ns_path, 'last_accessed': None, 'projects': []}
                projects_file = os.path.join(ns_path, 'projects.json')
                if os.path.exists(projects_file):
                    try:
                        with open(projects_file, 'r') as f:
                            self.namespaces[ns_dir]['projects'] = json.load(f)
                    except Exception as e:
                        logger.error(f'Error loading projects for {ns_dir}: {e}')
    def activate_namespace(self, namespace):
        if namespace in self.namespaces:
            self.active_namespace = namespace
            self.namespaces[namespace]['last_accessed'] = datetime.now().isoformat()
            return True
        return False
    def create_project(self, name, description, namespace=None):
        ns = namespace or self.active_namespace
        if not ns or ns not in self.namespaces:
            return False
        project = {'id': f"{ns}_{name.lower().replace(' ', '_')}", 'name': name, 'description': description, 'created': datetime.now().isoformat(), 'last_modified': datetime.now().isoformat(), 'artifacts': []}
        self.namespaces[ns]['projects'].append(project)
        self._save_namespace_data(ns)
        return project
    def _save_namespace_data(self, namespace):
        if namespace in self.namespaces:
            projects_file = os.path.join(self.namespaces[namespace]['path'], 'projects.json')
            try:
                with open(projects_file, 'w') as f:
                    json.dump(self.namespaces[namespace]['projects'], f, indent=2)
            except Exception as e:
                logger.error(f'Error saving projects for {namespace}: {e}')
    def get_namespace_status(self):
        return {'active': self.active_namespace, 'namespaces': {k: {'project_count': len(v['projects']), 'last_accessed': v['last_accessed']} for k, v in self.namespaces.items()}}
namespace_manager = NamespaceManager()