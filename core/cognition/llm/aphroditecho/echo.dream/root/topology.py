import uuid
import logging
from typing import Dict, List, Tuple, Any, Optional
from collections import defaultdict
logger = logging.getLogger(__name__)
class TopologyNode:
    def __init__(self, id: str=None, name: str='', node_type: str='generic', position: Tuple[float, float, float]=(0.0, 0.0, 0.0), attributes: Dict[str, Any]=None):
        self.id = id or str(uuid.uuid4())
        self.name = name
        self.node_type = node_type
        self.position = position
        self.attributes = attributes or {}
        self.created_at = None
        self.updated_at = None
    def to_dict(self) -> Dict[str, Any]:
        return {'id': self.id, 'name': self.name, 'type': self.node_type, 'position': self.position, 'attributes': self.attributes, 'created_at': self.created_at, 'updated_at': self.updated_at}
class SystemTopology:
    def __init__(self):
        self.nodes = {}
        self.connections = defaultdict(set)
        self.regions = {}
        self.tags = defaultdict(set)
    def add_node(self, node: TopologyNode) -> str:
        self.nodes[node.id] = node
        logger.info(f'Added node {node.name} ({node.id}) to topology')
        return node.id
    def remove_node(self, node_id: str) -> bool:
        if node_id not in self.nodes:
            return False
        self.connections.pop(node_id, None)
        for source_id, targets in self.connections.items():
            targets.discard(node_id)
        for region_nodes in self.regions.values():
            region_nodes.discard(node_id)
        for tag_nodes in self.tags.values():
            tag_nodes.discard(node_id)
        node_name = self.nodes[node_id].name
        del self.nodes[node_id]
        logger.info(f'Removed node {node_name} ({node_id}) from topology')
        return True
    def get_node(self, node_id: str) -> Optional[TopologyNode]:
        return self.nodes.get(node_id)
    def connect_nodes(self, source_id: str, target_id: str) -> bool:
        if source_id not in self.nodes or target_id not in self.nodes:
            return False
        self.connections[source_id].add(target_id)
        logger.info(f'Connected nodes {source_id} -> {target_id}')
        return True
    def disconnect_nodes(self, source_id: str, target_id: str) -> bool:
        if source_id not in self.connections:
            return False
        if target_id not in self.connections[source_id]:
            return False
        self.connections[source_id].remove(target_id)
        logger.info(f'Disconnected nodes {source_id} -> {target_id}')
        return True
    def get_connected_nodes(self, node_id: str, outgoing: bool=True) -> List[str]:
        if outgoing:
            return list(self.connections.get(node_id, set()))
        else:
            return [src for src, targets in self.connections.items() if node_id in targets]
    def create_region(self, region_name: str, node_ids: List[str]) -> bool:
        if not node_ids or not all((nid in self.nodes for nid in node_ids)):
            return False
        self.regions[region_name] = set(node_ids)
        logger.info(f'Created region {region_name} with {len(node_ids)} nodes')
        return True
    def add_to_region(self, region_name: str, node_id: str) -> bool:
        if region_name not in self.regions or node_id not in self.nodes:
            return False
        self.regions[region_name].add(node_id)
        return True
    def remove_from_region(self, region_name: str, node_id: str) -> bool:
        if region_name not in self.regions:
            return False
        if node_id not in self.regions[region_name]:
            return False
        self.regions[region_name].remove(node_id)
        return True
    def tag_node(self, node_id: str, tag: str) -> bool:
        if node_id not in self.nodes:
            return False
        self.tags[tag].add(node_id)
        return True
    def untag_node(self, node_id: str, tag: str) -> bool:
        if tag not in self.tags or node_id not in self.tags[tag]:
            return False
        self.tags[tag].remove(node_id)
        return True
    def get_nodes_by_tag(self, tag: str) -> List[str]:
        return list(self.tags.get(tag, set()))
    def get_node_tags(self, node_id: str) -> List[str]:
        return [tag for tag, nodes in self.tags.items() if node_id in nodes]
    def get_topology_state(self) -> Dict[str, Any]:
        nodes_dict = {node_id: node.to_dict() for node_id, node in self.nodes.items()}
        connections_list = []
        for source_id, targets in self.connections.items():
            for target_id in targets:
                connections_list.append({'source': source_id, 'target': target_id})
        regions_dict = {name: list(nodes) for name, nodes in self.regions.items()}
        tags_dict = {tag: list(nodes) for tag, nodes in self.tags.items()}
        return {'nodes': nodes_dict, 'connections': connections_list, 'regions': regions_dict, 'tags': tags_dict}
system_topology = SystemTopology()
def get_topology() -> SystemTopology:
    return system_topology