import logging
import uuid
from datetime import datetime
from typing import Dict, List, Any
logger = logging.getLogger(__name__)
class UserProjects:
    def __init__(self):
        self.user_id = 'default_user'
        self.containers = {}
        self.categories = {}
        self.projects = {}
        self.resources = {}
        self.container_categories = {}
        self.category_projects = {}
        self.project_resources = {}
        logger.info('User projects module initialized')
    def create_container(self, name: str, description: str=None, tags: List[str]=None, attributes: Dict[str, Any]=None) -> str:
        container_id = str(uuid.uuid4())
        self.containers[container_id] = {'id': container_id, 'name': name, 'user_id': self.user_id, 'description': description or f'Container: {name}', 'tags': tags or [], 'attributes': attributes or {}, 'created_at': datetime.now(), 'updated_at': datetime.now()}
        self.container_categories[container_id] = []
        logger.info(f"Created project container '{name}' with ID {container_id}")
        return container_id
    def create_category(self, name: str, parent_id: str, description: str=None, tags: List[str]=None, attributes: Dict[str, Any]=None) -> str:
        if parent_id not in self.containers:
            logger.error(f'Container {parent_id} not found')
            return None
        category_id = str(uuid.uuid4())
        self.categories[category_id] = {'id': category_id, 'name': name, 'parent_id': parent_id, 'user_id': self.user_id, 'description': description or f'Category: {name}', 'tags': tags or [], 'attributes': attributes or {}, 'created_at': datetime.now(), 'updated_at': datetime.now()}
        self.container_categories[parent_id].append(category_id)
        self.category_projects[category_id] = []
        logger.info(f"Created project category '{name}' in container '{self.containers[parent_id]['name']}'")
        return category_id
    def create_project(self, name: str, category_id: str, description: str=None, status: str='active', priority: str='medium', tags: List[str]=None, attributes: Dict[str, Any]=None) -> str:
        if category_id not in self.categories:
            logger.error(f'Category {category_id} not found')
            return None
        project_id = str(uuid.uuid4())
        self.projects[project_id] = {'id': project_id, 'name': name, 'category_id': category_id, 'user_id': self.user_id, 'description': description or f'Project: {name}', 'status': status, 'priority': priority, 'progress': 0.0, 'tags': tags or [], 'attributes': attributes or {}, 'created_at': datetime.now(), 'updated_at': datetime.now(), 'start_date': None, 'end_date': None, 'completed_at': None}
        self.category_projects[category_id].append(project_id)
        self.project_resources[project_id] = []
        logger.info(f"Created project '{name}' in category '{self.categories[category_id]['name']}'")
        return project_id
    def add_project_resource(self, project_id: str, name: str, resource_type: str, location: str=None, description: str=None, attributes: Dict[str, Any]=None) -> str:
        if project_id not in self.projects:
            logger.error(f'Project {project_id} not found')
            return None
        resource_id = str(uuid.uuid4())
        self.resources[resource_id] = {'id': resource_id, 'name': name, 'project_id': project_id, 'user_id': self.user_id, 'resource_type': resource_type, 'location': location, 'description': description or f'Resource: {name}', 'attributes': attributes or {}, 'created_at': datetime.now(), 'updated_at': datetime.now()}
        self.project_resources[project_id].append(resource_id)
        logger.info(f"Added resource '{name}' to project '{self.projects[project_id]['name']}'")
        return resource_id
    def update_project_status(self, project_id: str, status: str) -> bool:
        if project_id not in self.projects:
            logger.error(f'Project {project_id} not found')
            return False
        old_status = self.projects[project_id]['status']
        self.projects[project_id]['status'] = status
        self.projects[project_id]['updated_at'] = datetime.now()
        if status == 'completed' and old_status != 'completed':
            self.projects[project_id]['completed_at'] = datetime.now()
        elif status != 'completed' and old_status == 'completed':
            self.projects[project_id]['completed_at'] = None
        logger.info(f"Updated project '{self.projects[project_id]['name']}' status to '{status}'")
        return True
    def update_project_progress(self, project_id: str, progress: float) -> bool:
        if project_id not in self.projects:
            logger.error(f'Project {project_id} not found')
            return False
        progress = max(0.0, min(1.0, progress))
        self.projects[project_id]['progress'] = progress
        self.projects[project_id]['updated_at'] = datetime.now()
        logger.info(f"Updated project '{self.projects[project_id]['name']}' progress to {progress:.1%}")
        return True
    def get_container(self, container_id: str) -> Dict[str, Any]:
        if container_id not in self.containers:
            logger.error(f'Container {container_id} not found')
            return None
        return self.containers[container_id]
    def get_category(self, category_id: str) -> Dict[str, Any]:
        if category_id not in self.categories:
            logger.error(f'Category {category_id} not found')
            return None
        return self.categories[category_id]
    def get_project(self, project_id: str) -> Dict[str, Any]:
        if project_id not in self.projects:
            logger.error(f'Project {project_id} not found')
            return None
        return self.projects[project_id]
    def get_resource(self, resource_id: str) -> Dict[str, Any]:
        if resource_id not in self.resources:
            logger.error(f'Resource {resource_id} not found')
            return None
        return self.resources[resource_id]
    def get_container_categories(self, container_id: str) -> List[Dict[str, Any]]:
        if container_id not in self.containers:
            logger.error(f'Container {container_id} not found')
            return []
        category_ids = self.container_categories.get(container_id, [])
        return [self.categories[cat_id] for cat_id in category_ids if cat_id in self.categories]
    def get_category_projects(self, category_id: str) -> List[Dict[str, Any]]:
        if category_id not in self.categories:
            logger.error(f'Category {category_id} not found')
            return []
        project_ids = self.category_projects.get(category_id, [])
        return [self.projects[proj_id] for proj_id in project_ids if proj_id in self.projects]
    def get_project_resources(self, project_id: str) -> List[Dict[str, Any]]:
        if project_id not in self.projects:
            logger.error(f'Project {project_id} not found')
            return []
        resource_ids = self.project_resources.get(project_id, [])
        return [self.resources[res_id] for res_id in resource_ids if res_id in self.resources]
    def get_projects_state(self) -> Dict[str, Any]:
        return {'container_count': len(self.containers), 'category_count': len(self.categories), 'project_count': len(self.projects), 'resource_count': len(self.resources), 'active_projects': sum((1 for p in self.projects.values() if p['status'] == 'active')), 'completed_projects': sum((1 for p in self.projects.values() if p['status'] == 'completed')), 'high_priority_projects': sum((1 for p in self.projects.values() if p['priority'] == 'high')), 'updated_at': datetime.now()}
    def search_projects(self, query: str) -> List[Dict[str, Any]]:
        query = query.lower()
        results = []
        for project in self.projects.values():
            if query in project['name'].lower() or (project['description'] and query in project['description'].lower()):
                results.append(project)
        return results
    def find_projects_by_tag(self, tag: str) -> List[Dict[str, Any]]:
        tag = tag.lower()
        return [p for p in self.projects.values() if tag in [t.lower() for t in p['tags']]]
    def find_projects_by_status(self, status: str) -> List[Dict[str, Any]]:
        return [p for p in self.projects.values() if p['status'] == status]
_projects_instance = UserProjects()
def get_projects() -> UserProjects:
    return _projects_instance