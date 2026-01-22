import logging
import uuid
from datetime import datetime
from typing import Dict, List, Any, Tuple
from collections import defaultdict
logger = logging.getLogger(__name__)
class WorkspaceArchitecture:
    def __init__(self):
        self.elements = {}
        self.root_elements = []
        self.element_parents = {}
        self.element_children = defaultdict(list)
        self.element_types = defaultdict(list)
        self.element_categories = defaultdict(list)
        self.element_tags = defaultdict(list)
        self.element_attributes = defaultdict(dict)
        self.element_connections = defaultdict(list)
        self.element_connection_types = {}
        self.element_permissions = defaultdict(dict)
    def create_root_element(self, name: str, element_type: str, attributes: Dict[str, Any]=None) -> str:
        element_id = str(uuid.uuid4())
        created_at = datetime.now()
        self.elements[element_id] = {'id': element_id, 'name': name, 'type': element_type, 'parent_id': None, 'created_at': created_at, 'updated_at': created_at, 'position': (0, 0), 'size': (100, 100), 'active': True}
        self.root_elements.append(element_id)
        self.element_types[element_type].append(element_id)
        if attributes:
            self.element_attributes[element_id] = attributes
            if 'category' in attributes:
                categories = attributes['category']
                if isinstance(categories, str):
                    categories = [categories]
                for category in categories:
                    self.element_categories[category].append(element_id)
            if 'tags' in attributes:
                tags = attributes['tags']
                if isinstance(tags, str):
                    tags = [tags]
                for tag in tags:
                    self.element_tags[tag].append(element_id)
        logger.info(f"Created root element '{name}' of type '{element_type}' with ID {element_id}")
        return element_id
    def create_element(self, name: str, parent_id: str, element_type: str, position: Tuple[float, float]=None, size: Tuple[float, float]=None, attributes: Dict[str, Any]=None) -> str:
        if parent_id not in self.elements:
            logger.error(f'Cannot create element: parent {parent_id} not found')
            return None
        element_id = str(uuid.uuid4())
        created_at = datetime.now()
        self.elements[element_id] = {'id': element_id, 'name': name, 'type': element_type, 'parent_id': parent_id, 'created_at': created_at, 'updated_at': created_at, 'position': position or (0, 0), 'size': size or (100, 100), 'active': True}
        self.element_parents[element_id] = parent_id
        self.element_children[parent_id].append(element_id)
        self.element_types[element_type].append(element_id)
        if attributes:
            self.element_attributes[element_id] = attributes
            if 'category' in attributes:
                categories = attributes['category']
                if isinstance(categories, str):
                    categories = [categories]
                for category in categories:
                    self.element_categories[category].append(element_id)
            if 'tags' in attributes:
                tags = attributes['tags']
                if isinstance(tags, str):
                    tags = [tags]
                for tag in tags:
                    self.element_tags[tag].append(element_id)
        logger.info(f"Created element '{name}' of type '{element_type}' under parent {parent_id}")
        return element_id
    def update_element(self, element_id: str, name: str=None, position: Tuple[float, float]=None, size: Tuple[float, float]=None, active: bool=None, attributes: Dict[str, Any]=None) -> bool:
        if element_id not in self.elements:
            logger.error(f'Cannot update element: {element_id} not found')
            return False
        element = self.elements[element_id]
        if name is not None:
            element['name'] = name
        if position is not None:
            element['position'] = position
        if size is not None:
            element['size'] = size
        if active is not None:
            element['active'] = active
        if attributes is not None:
            self.element_attributes[element_id].update(attributes)
        element['updated_at'] = datetime.now()
        logger.info(f"Updated element '{element['name']}' ({element_id})")
        return True
    def delete_element(self, element_id: str) -> bool:
        if element_id not in self.elements:
            return False
        for child_id in list(self.element_children.get(element_id, [])):
            self.delete_element(child_id)
        parent_id = self.element_parents.get(element_id)
        if parent_id and element_id in self.element_children[parent_id]:
            self.element_children[parent_id].remove(element_id)
        if element_id in self.root_elements:
            self.root_elements.remove(element_id)
        element_type = self.elements[element_id]['type']
        if element_id in self.element_types[element_type]:
            self.element_types[element_type].remove(element_id)
        for category, elements in self.element_categories.items():
            if element_id in elements:
                elements.remove(element_id)
        for tag, elements in self.element_tags.items():
            if element_id in elements:
                elements.remove(element_id)
        if element_id in self.element_connections:
            del self.element_connections[element_id]
        for source_id, targets in self.element_connections.items():
            if element_id in targets:
                targets.remove(element_id)
                if (source_id, element_id) in self.element_connection_types:
                    del self.element_connection_types[source_id, element_id]
        if element_id in self.element_parents:
            del self.element_parents[element_id]
        if element_id in self.element_children:
            del self.element_children[element_id]
        if element_id in self.element_attributes:
            del self.element_attributes[element_id]
        if element_id in self.element_permissions:
            del self.element_permissions[element_id]
        element_name = self.elements[element_id]['name']
        del self.elements[element_id]
        logger.info(f"Deleted element '{element_name}' ({element_id})")
        return True
    def connect_elements(self, source_id: str, target_id: str, connection_type: str='generic') -> bool:
        if source_id not in self.elements or target_id not in self.elements:
            return False
        if target_id not in self.element_connections[source_id]:
            self.element_connections[source_id].append(target_id)
            self.element_connection_types[source_id, target_id] = connection_type
            source_name = self.elements[source_id]['name']
            target_name = self.elements[target_id]['name']
            logger.info(f"Connected '{source_name}' to '{target_name}' with type '{connection_type}'")
            return True
        return False
    def disconnect_elements(self, source_id: str, target_id: str) -> bool:
        if source_id not in self.element_connections:
            return False
        if target_id not in self.element_connections[source_id]:
            return False
        self.element_connections[source_id].remove(target_id)
        if (source_id, target_id) in self.element_connection_types:
            del self.element_connection_types[source_id, target_id]
        source_name = self.elements[source_id]['name']
        target_name = self.elements[target_id]['name']
        logger.info(f"Disconnected '{source_name}' from '{target_name}'")
        return True
    def move_element(self, element_id: str, new_parent_id: str) -> bool:
        if element_id not in self.elements or new_parent_id not in self.elements:
            return False
        current_parent_id = self.element_parents.get(element_id)
        if current_parent_id is None:
            if element_id in self.root_elements:
                self.root_elements.remove(element_id)
        elif element_id in self.element_children[current_parent_id]:
            self.element_children[current_parent_id].remove(element_id)
        self.element_parents[element_id] = new_parent_id
        self.element_children[new_parent_id].append(element_id)
        self.elements[element_id]['parent_id'] = new_parent_id
        element_name = self.elements[element_id]['name']
        new_parent_name = self.elements[new_parent_id]['name']
        logger.info(f"Moved element '{element_name}' to new parent '{new_parent_name}'")
        return True
    def set_element_permission(self, element_id: str, user_id: str, permission: str) -> bool:
        if element_id not in self.elements:
            return False
        self.element_permissions[element_id][user_id] = permission
        return True
    def get_element(self, element_id: str) -> Dict[str, Any]:
        if element_id not in self.elements:
            return None
        element = dict(self.elements[element_id])
        element['attributes'] = dict(self.element_attributes.get(element_id, {}))
        element['children'] = list(self.element_children.get(element_id, []))
        element['connections'] = list(self.element_connections.get(element_id, []))
        element['connection_details'] = []
        for target_id in element['connections']:
            conn_type = self.element_connection_types.get((element_id, target_id), 'generic')
            element['connection_details'].append({'target_id': target_id, 'type': conn_type})
        return element
    def get_elements_by_type(self, element_type: str) -> List[str]:
        return list(self.element_types.get(element_type, []))
    def get_elements_by_category(self, category: str) -> List[str]:
        return list(self.element_categories.get(category, []))
    def get_elements_by_tag(self, tag: str) -> List[str]:
        return list(self.element_tags.get(tag, []))
    def get_architecture_state(self) -> Dict[str, Any]:
        type_counts = {element_type: len(elements) for element_type, elements in self.element_types.items()}
        total_connections = sum((len(targets) for targets in self.element_connections.values()))
        active_workspaces = [eid for eid, element in self.elements.items() if element.get('type') == 'workspace' and element.get('active', False)]
        state = {'element_count': len(self.elements), 'root_element_count': len(self.root_elements), 'connection_count': total_connections, 'element_types': type_counts, 'active_workspace_count': len(active_workspaces), 'root_elements': self.root_elements, 'categories': list(self.element_categories.keys()), 'tags': list(self.element_tags.keys())}
        return state
workspace_architecture = WorkspaceArchitecture()
def get_architecture() -> WorkspaceArchitecture:
    return workspace_architecture