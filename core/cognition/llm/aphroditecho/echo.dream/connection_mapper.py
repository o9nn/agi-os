import json
import logging
import uuid
import re
from collections import defaultdict
import numpy as np
import networkx as nx
logger = logging.getLogger(__name__)
_CONNECTION_MAPPER = None
def get_connection_mapper():
    global _CONNECTION_MAPPER
    if _CONNECTION_MAPPER is None:
        _CONNECTION_MAPPER = DynamicConnectionMapper()
    return _CONNECTION_MAPPER
class KnowledgeDomain:
    def __init__(self, name, description=None, domain_type='general', parent_domain=None, attributes=None):
        self.id = str(uuid.uuid4())
        self.name = name
        self.description = description or ''
        self.domain_type = domain_type or 'general'
        self.parent_domain_id = parent_domain
        self.attributes = attributes or {}
        self.concepts = {}
        self.connections = {}
        self.terminology = set()
        self.created_at = None
        self.updated_at = None
    def add_concept(self, concept_id, concept_data):
        self.concepts[concept_id] = concept_data
    def add_connection(self, target_domain_id, connection_data):
        self.connections[target_domain_id] = connection_data
    def add_terminology(self, terms):
        if isinstance(terms, str):
            self.terminology.add(terms)
        elif isinstance(terms, (list, set)):
            self.terminology.update(terms)
    def to_dict(self):
        return {'id': self.id, 'name': self.name, 'description': self.description, 'domain_type': self.domain_type, 'parent_domain_id': self.parent_domain_id, 'attributes': self.attributes, 'terminology': list(self.terminology), 'concept_count': len(self.concepts), 'connection_count': len(self.connections), 'created_at': self.created_at, 'updated_at': self.updated_at}
class ConceptNode:
    def __init__(self, name, domain_id=None, description=None, node_type='concept', attributes=None):
        self.id = str(uuid.uuid4())
        self.name = name
        self.domain_id = domain_id
        self.description = description or ''
        self.node_type = node_type or 'concept'
        self.attributes = attributes or {}
        self.connections = {}
        self.vector = None
        self.created_at = None
        self.updated_at = None
    def add_connection(self, target_concept_id, connection_data):
        self.connections[target_concept_id] = connection_data
    def set_vector(self, vector):
        self.vector = vector
    def to_dict(self):
        return {'id': self.id, 'name': self.name, 'domain_id': self.domain_id, 'description': self.description, 'node_type': self.node_type, 'attributes': self.attributes, 'has_vector': self.vector is not None, 'connection_count': len(self.connections), 'created_at': self.created_at, 'updated_at': self.updated_at}
    def similarity(self, other_concept):
        if self.vector is None or other_concept.vector is None:
            return 0.0
        dot_product = np.dot(self.vector, other_concept.vector)
        norm_a = np.linalg.norm(self.vector)
        norm_b = np.linalg.norm(other_concept.vector)
        if norm_a == 0 or norm_b == 0:
            return 0.0
        return dot_product / (norm_a * norm_b)
class DynamicConnectionMapper:
    def __init__(self):
        self.domains = {}
        self.concepts = {}
        self.domain_graph = nx.DiGraph()
        self.concept_graph = nx.DiGraph()
        self.domain_index = defaultdict(list)
        self.concept_index = defaultdict(list)
        self.discovered_patterns = []
        self.domain_types = ['general', 'specialized', 'interdisciplinary', 'applied', 'theoretical', 'emerging']
        self.relation_types = ['related', 'part_of', 'contains', 'depends_on', 'influences', 'derived_from', 'opposes']
        self._load_from_database()
        logger.info('Dynamic Connection Mapper initialized')
    def _load_from_database(self):
        try:
            from models_mapper import KnowledgeDomain as DBKnowledgeDomain
            from models_mapper import DomainConnection
            db_domains = DBKnowledgeDomain.query.all()
            for db_domain in db_domains:
                domain = KnowledgeDomain(name=db_domain.name, description=db_domain.description, domain_type=db_domain.domain_type, attributes=db_domain.attributes)
                domain.id = db_domain.id
                domain.created_at = db_domain.created_at
                domain.updated_at = db_domain.updated_at
                self.domains[domain.id] = domain
                self.domain_graph.add_node(domain.id, name=domain.name, domain_type=domain.domain_type, description=domain.description)
                self._index_domain(domain)
            db_connections = DomainConnection.query.all()
            for db_connection in db_connections:
                source_id = db_connection.source_domain_id
                target_id = db_connection.target_domain_id
                if source_id not in self.domains or target_id not in self.domains:
                    continue
                self.domains[source_id].add_connection(target_id, {'id': db_connection.id, 'source_domain_id': source_id, 'target_domain_id': target_id, 'connection_type': db_connection.connection_type, 'strength': db_connection.strength, 'description': db_connection.description, 'bidirectional': db_connection.bidirectional, 'attributes': db_connection.attributes or {}, 'created_at': db_connection.created_at, 'updated_at': db_connection.updated_at})
                self.domain_graph.add_edge(source_id, target_id, id=db_connection.id, type=db_connection.connection_type, strength=db_connection.strength, bidirectional=db_connection.bidirectional, description=db_connection.description)
            logger.info(f'Loaded {len(self.domains)} domains and {len(db_connections)} domain connections from database')
        except Exception as e:
            logger.error(f'Error loading from database: {str(e)}')
    def create_domain(self, name, description=None, domain_type='general', parent_domain=None, attributes=None) -> str:
        domain = KnowledgeDomain(name=name, description=description, domain_type=domain_type, parent_domain=parent_domain, attributes=attributes)
        import datetime
        domain.created_at = datetime.datetime.now()
        domain.updated_at = domain.created_at
        self.domains[domain.id] = domain
        self.domain_graph.add_node(domain.id, name=name, domain_type=domain_type, description=description)
        if parent_domain and parent_domain in self.domains:
            self.connect_domains(domain.id, parent_domain, connection_type='child_of', bidirectional=False)
            self.connect_domains(parent_domain, domain.id, connection_type='parent_of', bidirectional=False)
        self._index_domain(domain)
        logger.info(f"Created domain '{name}' with ID {domain.id}")
        return domain.id
    def create_concept(self, name, domain_id, description=None, node_type='concept', attributes=None) -> str:
        if domain_id not in self.domains:
            raise ValueError(f'Domain with ID {domain_id} not found')
        concept = ConceptNode(name=name, domain_id=domain_id, description=description, node_type=node_type, attributes=attributes)
        import datetime
        concept.created_at = datetime.datetime.now()
        concept.updated_at = concept.created_at
        self.concepts[concept.id] = concept
        domain = self.domains[domain_id]
        domain.add_concept(concept.id, {'id': concept.id, 'name': name, 'description': description, 'domain_id': domain_id, 'concept_type': node_type, 'importance': 0.5, 'attributes': attributes, 'created_at': concept.created_at, 'updated_at': concept.updated_at})
        self.concept_graph.add_node(concept.id, name=name, domain_id=domain_id, node_type=node_type, description=description)
        self._index_concept(concept)
        logger.info(f"Created concept '{name}' with ID {concept.id} in domain {domain_id}")
        return concept.id
    def connect_domains(self, source_domain_id, target_domain_id, connection_type='related', strength=0.5, description=None, bidirectional=True, attributes=None) -> str:
        if source_domain_id not in self.domains:
            raise ValueError(f'Source domain with ID {source_domain_id} not found')
        if target_domain_id not in self.domains:
            raise ValueError(f'Target domain with ID {target_domain_id} not found')
        if source_domain_id == target_domain_id:
            raise ValueError('Cannot connect a domain to itself')
        connection_id = str(uuid.uuid4())
        connection_data = {'id': connection_id, 'source_domain_id': source_domain_id, 'target_domain_id': target_domain_id, 'connection_type': connection_type, 'strength': strength, 'description': description, 'bidirectional': bidirectional, 'attributes': attributes or {}, 'created_at': None, 'updated_at': None}
        import datetime
        connection_data['created_at'] = datetime.datetime.now()
        connection_data['updated_at'] = connection_data['created_at']
        source_domain = self.domains[source_domain_id]
        source_domain.add_connection(target_domain_id, connection_data)
        self.domain_graph.add_edge(source_domain_id, target_domain_id, id=connection_id, type=connection_type, strength=strength, bidirectional=bidirectional, description=description)
        if bidirectional:
            reverse_connection_id = str(uuid.uuid4())
            reverse_connection_data = {'id': reverse_connection_id, 'source_domain_id': target_domain_id, 'target_domain_id': source_domain_id, 'connection_type': connection_type, 'strength': strength, 'description': description, 'bidirectional': False, 'attributes': attributes or {}, 'created_at': connection_data['created_at'], 'updated_at': connection_data['updated_at']}
            target_domain = self.domains[target_domain_id]
            target_domain.add_connection(source_domain_id, reverse_connection_data)
            self.domain_graph.add_edge(target_domain_id, source_domain_id, id=reverse_connection_id, type=connection_type, strength=strength, bidirectional=False, description=description)
        logger.info(f'Created connection from domain {source_domain_id} to domain {target_domain_id}')
        return connection_id
    def connect_concepts(self, source_concept_id, target_concept_id, relation_type='related', strength=0.5, description=None, bidirectional=True, attributes=None) -> str:
        if source_concept_id not in self.concepts:
            raise ValueError(f'Source concept with ID {source_concept_id} not found')
        if target_concept_id not in self.concepts:
            raise ValueError(f'Target concept with ID {target_concept_id} not found')
        if source_concept_id == target_concept_id:
            raise ValueError('Cannot connect a concept to itself')
        connection_id = str(uuid.uuid4())
        connection_data = {'id': connection_id, 'source_concept_id': source_concept_id, 'target_concept_id': target_concept_id, 'relation_type': relation_type, 'strength': strength, 'description': description, 'bidirectional': bidirectional, 'attributes': attributes or {}, 'created_at': None, 'updated_at': None}
        import datetime
        connection_data['created_at'] = datetime.datetime.now()
        connection_data['updated_at'] = connection_data['created_at']
        source_concept = self.concepts[source_concept_id]
        source_concept.add_connection(target_concept_id, connection_data)
        self.concept_graph.add_edge(source_concept_id, target_concept_id, id=connection_id, type=relation_type, strength=strength, bidirectional=bidirectional, description=description)
        if bidirectional:
            reverse_connection_id = str(uuid.uuid4())
            reverse_connection_data = {'id': reverse_connection_id, 'source_concept_id': target_concept_id, 'target_concept_id': source_concept_id, 'relation_type': relation_type, 'strength': strength, 'description': description, 'bidirectional': False, 'attributes': attributes or {}, 'created_at': connection_data['created_at'], 'updated_at': connection_data['updated_at']}
            target_concept = self.concepts[target_concept_id]
            target_concept.add_connection(source_concept_id, reverse_connection_data)
            self.concept_graph.add_edge(target_concept_id, source_concept_id, id=reverse_connection_id, type=relation_type, strength=strength, bidirectional=False, description=description)
        source_domain_id = self.concepts[source_concept_id].domain_id
        target_domain_id = self.concepts[target_concept_id].domain_id
        if source_domain_id and target_domain_id and (source_domain_id != target_domain_id):
            if source_domain_id in self.domains and target_domain_id in self.domains:
                if target_domain_id not in self.domains[source_domain_id].connections:
                    self.connect_domains(source_domain_id, target_domain_id, connection_type='concept_bridge', strength=strength, description=f'Bridge via concepts: {source_concept.name} -> {self.concepts[target_concept_id].name}', bidirectional=bidirectional, attributes={'bridged_by_concepts': [source_concept_id, target_concept_id]})
        logger.info(f'Created connection from concept {source_concept_id} to concept {target_concept_id}')
        return connection_id
    def search_domains(self, query, max_results=10):
        search_terms = self._extract_search_terms(query)
        domain_scores = defaultdict(float)
        for term in search_terms:
            if term in self.domain_index:
                for domain_id in self.domain_index[term]:
                    domain_scores[domain_id] += 1.0
                    if term.lower() == self.domains[domain_id].name.lower():
                        domain_scores[domain_id] += 3.0
                    if term.lower() == self.domains[domain_id].domain_type.lower():
                        domain_scores[domain_id] += 2.0
        top_domains = sorted(domain_scores.items(), key=lambda x: x[1], reverse=True)[:max_results]
        results = []
        for domain_id, score in top_domains:
            if domain_id in self.domains:
                domain = self.domains[domain_id]
                domain_data = domain.to_dict()
                domain_data['search_score'] = score
                results.append(domain_data)
        return results
    def search_concepts(self, query, max_results=20):
        search_terms = self._extract_search_terms(query)
        concept_scores = defaultdict(float)
        for term in search_terms:
            if term in self.concept_index:
                for concept_id in self.concept_index[term]:
                    concept_scores[concept_id] += 1.0
                    if term.lower() == self.concepts[concept_id].name.lower():
                        concept_scores[concept_id] += 3.0
                    if term.lower() == self.concepts[concept_id].node_type.lower():
                        concept_scores[concept_id] += 2.0
        top_concepts = sorted(concept_scores.items(), key=lambda x: x[1], reverse=True)[:max_results]
        results = []
        for concept_id, score in top_concepts:
            if concept_id in self.concepts:
                concept = self.concepts[concept_id]
                concept_data = concept.to_dict()
                concept_data['search_score'] = score
                if concept.domain_id and concept.domain_id in self.domains:
                    domain = self.domains[concept.domain_id]
                    concept_data['domain_name'] = domain.name
                    concept_data['domain_type'] = domain.domain_type
                results.append(concept_data)
        return results
    def find_interdisciplinary_paths(self, source_domain_id, target_domain_id, max_paths=3, max_length=4):
        if source_domain_id not in self.domains:
            raise ValueError(f'Source domain with ID {source_domain_id} not found')
        if target_domain_id not in self.domains:
            raise ValueError(f'Target domain with ID {target_domain_id} not found')
        if source_domain_id == target_domain_id:
            return [{'length': 0, 'domains': [self.domains[source_domain_id].to_dict()], 'connections': []}]
        try:
            all_paths = list(nx.all_simple_paths(self.domain_graph, source=source_domain_id, target=target_domain_id, cutoff=max_length))
        except (nx.NetworkXNoPath, nx.NodeNotFound):
            return []
        if not all_paths:
            return []
        all_paths.sort(key=len)
        results = []
        for path in all_paths[:max_paths]:
            domains = []
            for domain_id in path:
                if domain_id in self.domains:
                    domain_data = self.domains[domain_id].to_dict()
                    domains.append(domain_data)
            connections = []
            for i in range(len(path) - 1):
                source_id = path[i]
                target_id = path[i + 1]
                if self.domain_graph.has_edge(source_id, target_id):
                    edge_data = self.domain_graph.get_edge_data(source_id, target_id)
                    connection = {'source_domain_id': source_id, 'target_domain_id': target_id, 'type': edge_data.get('type', 'related'), 'strength': edge_data.get('strength', 0.5), 'description': edge_data.get('description', ''), 'id': edge_data.get('id', '')}
                    connections.append(connection)
            results.append({'length': len(path) - 1, 'domains': domains, 'connections': connections})
        return results
    def discover_connection_patterns(self):
        patterns = []
        if len(self.domains) < 3 or len(self.concepts) < 5:
            logger.info('Not enough data to discover connection patterns')
            return patterns
        if len(self.domains) >= 3:
            centrality = nx.degree_centrality(self.domain_graph)
            top_domains = sorted(centrality.items(), key=lambda x: x[1], reverse=True)[:5]
            for domain_id, centrality_score in top_domains:
                if centrality_score > 0.1 and domain_id in self.domains:
                    domain = self.domains[domain_id]
                    patterns.append({'pattern_type': 'central_domain', 'domain_id': domain_id, 'domain_name': domain.name, 'centrality_score': centrality_score, 'connection_count': len(domain.connections), 'domain_type': domain.domain_type, 'description': f'Central hub domain with high connectivity ({len(domain.connections)} connections)'})
        if len(self.domains) >= 5:
            try:
                betweenness = nx.betweenness_centrality(self.domain_graph)
                top_bridges = sorted(betweenness.items(), key=lambda x: x[1], reverse=True)[:5]
                for domain_id, betweenness_score in top_bridges:
                    if betweenness_score > 0.1 and domain_id in self.domains:
                        domain = self.domains[domain_id]
                        patterns.append({'pattern_type': 'bridge_domain', 'domain_id': domain_id, 'domain_name': domain.name, 'betweenness_score': betweenness_score, 'domain_type': domain.domain_type, 'description': 'Bridge domain connecting disparate knowledge areas'})
            except:
                pass
        if len(self.domains) >= 6:
            try:
                clusters = list(nx.community.greedy_modularity_communities(self.domain_graph.to_undirected()))
                for i, cluster in enumerate(clusters[:5]):
                    if len(cluster) >= 2:
                        cluster_domains = []
                        for domain_id in cluster:
                            if domain_id in self.domains:
                                cluster_domains.append({'id': domain_id, 'name': self.domains[domain_id].name})
                        patterns.append({'pattern_type': 'domain_cluster', 'cluster_id': i, 'size': len(cluster), 'domains': cluster_domains, 'description': f'Cluster of {len(cluster)} related knowledge domains'})
            except:
                pass
        if len(self.concepts) >= 5:
            domain_concept_map = defaultdict(list)
            for concept_id, concept in self.concepts.items():
                if concept.domain_id:
                    domain_concept_map[concept.domain_id].append(concept_id)
            top_concepts = []
            for domain_id, domain_concepts in domain_concept_map.items():
                for concept_id in domain_concepts:
                    concept = self.concepts[concept_id]
                    connected_domains = set()
                    for connected_concept_id in concept.connections:
                        if connected_concept_id in self.concepts:
                            connected_concept = self.concepts[connected_concept_id]
                            if connected_concept.domain_id and connected_concept.domain_id != concept.domain_id:
                                connected_domains.add(connected_concept.domain_id)
                    if len(connected_domains) >= 2:
                        top_concepts.append((concept_id, len(connected_domains)))
            top_concepts.sort(key=lambda x: x[1], reverse=True)
            for concept_id, domain_count in top_concepts[:5]:
                concept = self.concepts[concept_id]
                domain_names = []
                for connected_concept_id in concept.connections:
                    if connected_concept_id in self.concepts:
                        connected_concept = self.concepts[connected_concept_id]
                        if connected_concept.domain_id and connected_concept.domain_id != concept.domain_id:
                            if connected_concept.domain_id in self.domains:
                                domain_names.append(self.domains[connected_concept.domain_id].name)
                patterns.append({'pattern_type': 'interdisciplinary_concept', 'concept_id': concept_id, 'concept_name': concept.name, 'domain_count': domain_count, 'domain_names': list(set(domain_names)), 'description': f'Concept that bridges {domain_count} different knowledge domains'})
        self.discovered_patterns = patterns
        return patterns
    def generate_domain_network(self, include_connections=True):
        nodes = []
        edges = []
        for domain_id, domain in self.domains.items():
            node_data = {'id': domain_id, 'name': domain.name, 'domain_type': domain.domain_type, 'concept_count': len(domain.concepts), 'connection_count': len(domain.connections)}
            nodes.append(node_data)
        if include_connections:
            for source_id in self.domain_graph.nodes():
                for target_id in self.domain_graph[source_id]:
                    edge_data = self.domain_graph.get_edge_data(source_id, target_id)
                    edge = {'source': source_id, 'target': target_id, 'type': edge_data.get('type', 'related'), 'weight': edge_data.get('strength', 0.5)}
                    edges.append(edge)
        return {'nodes': nodes, 'edges': edges}
    def export_data(self, format_type='json'):
        export_data = {'domains': [domain.to_dict() for domain in self.domains.values()], 'concepts': [concept.to_dict() for concept in self.concepts.values()], 'domain_connections': [], 'concept_connections': []}
        for source_id, targets in self.domain_graph.adjacency():
            for target_id, edge_data in targets.items():
                connection = {'source_domain_id': source_id, 'target_domain_id': target_id, 'connection_type': edge_data.get('type', 'related'), 'strength': edge_data.get('strength', 0.5), 'bidirectional': edge_data.get('bidirectional', True), 'description': edge_data.get('description', '')}
                export_data['domain_connections'].append(connection)
        for source_id, targets in self.concept_graph.adjacency():
            for target_id, edge_data in targets.items():
                connection = {'source_concept_id': source_id, 'target_concept_id': target_id, 'relation_type': edge_data.get('type', 'related'), 'strength': edge_data.get('strength', 0.5), 'bidirectional': edge_data.get('bidirectional', True), 'description': edge_data.get('description', '')}
                export_data['concept_connections'].append(connection)
        if format_type == 'json':
            return json.dumps(export_data, indent=2)
        elif format_type == 'xml':
            xml_data = ['<?xml version="1.0" encoding="UTF-8"?>']
            xml_data.append('<ConnectionMapper>')
            xml_data.append('  <Domains>')
            for domain in export_data['domains']:
                xml_data.append(f'''    <Domain id="{domain['id']}">''')
                xml_data.append(f"      <Name>{domain['name']}</Name>")
                xml_data.append(f"      <DomainType>{domain['domain_type']}</DomainType>")
                xml_data.append(f"      <Description>{domain.get('description', '')}</Description>")
                xml_data.append('    </Domain>')
            xml_data.append('  </Domains>')
            xml_data.append('  <Concepts>')
            for concept in export_data['concepts']:
                xml_data.append(f'''    <Concept id="{concept['id']}">''')
                xml_data.append(f"      <Name>{concept['name']}</Name>")
                xml_data.append(f"      <DomainId>{concept.get('domain_id', '')}</DomainId>")
                xml_data.append(f"      <NodeType>{concept['node_type']}</NodeType>")
                xml_data.append(f"      <Description>{concept.get('description', '')}</Description>")
                xml_data.append('    </Concept>')
            xml_data.append('  </Concepts>')
            xml_data.append('  <DomainConnections>')
            for conn in export_data['domain_connections']:
                xml_data.append(f'''    <Connection source="{conn['source_domain_id']}" target="{conn['target_domain_id']}">''')
                xml_data.append(f"      <Type>{conn['connection_type']}</Type>")
                xml_data.append(f"      <Strength>{conn['strength']}</Strength>")
                xml_data.append(f"      <Bidirectional>{str(conn['bidirectional']).lower()}</Bidirectional>")
                xml_data.append('    </Connection>')
            xml_data.append('  </DomainConnections>')
            xml_data.append('  <ConceptConnections>')
            for conn in export_data['concept_connections']:
                xml_data.append(f'''    <Connection source="{conn['source_concept_id']}" target="{conn['target_concept_id']}">''')
                xml_data.append(f"      <Type>{conn['relation_type']}</Type>")
                xml_data.append(f"      <Strength>{conn['strength']}</Strength>")
                xml_data.append(f"      <Bidirectional>{str(conn['bidirectional']).lower()}</Bidirectional>")
                xml_data.append('    </Connection>')
            xml_data.append('  </ConceptConnections>')
            xml_data.append('</ConnectionMapper>')
            return '\n'.join(xml_data)
        else:
            raise ValueError(f'Unsupported export format: {format_type}')
    def _index_domain(self, domain):
        terms = self._extract_search_terms(domain.name, domain.description or '')
        for term in terms:
            self.domain_index[term].append(domain.id)
        if domain.domain_type:
            self.domain_index[domain.domain_type].append(domain.id)
        for key, value in domain.attributes.items():
            attribute_terms = self._extract_search_terms(str(key), str(value))
            for term in attribute_terms:
                self.domain_index[term].append(domain.id)
        for term in domain.terminology:
            normalized_term = term.lower()
            if normalized_term not in self.domain_index:
                self.domain_index[normalized_term] = [domain.id]
            else:
                self.domain_index[normalized_term].append(domain.id)
    def _index_concept(self, concept):
        terms = self._extract_search_terms(concept.name, concept.description or '')
        for term in terms:
            self.concept_index[term].append(concept.id)
        if concept.node_type:
            self.concept_index[concept.node_type].append(concept.id)
        for key, value in concept.attributes.items():
            attribute_terms = self._extract_search_terms(str(key), str(value))
            for term in attribute_terms:
                self.concept_index[term].append(concept.id)
    def _extract_search_terms(self, *texts):
        combined_text = ' '.join([t for t in texts if t])
        normalized = combined_text.lower()
        words = re.findall('\\b\\w+\\b', normalized)
        stopwords = {'the', 'a', 'an', 'and', 'or', 'but', 'is', 'are', 'of', 'in', 'to', 'for', 'with', 'on', 'at', 'by', 'this', 'that', 'it', 'as'}
        terms = [word for word in words if word not in stopwords and len(word) > 1]
        return set(terms)
    def reload_from_database(self):
        from models_mapper import KnowledgeDomain, ConceptNode, DomainConnection, ConceptConnection
        self.domains = {}
        self.concepts = {}
        self.domain_graph.clear()
        self.concept_graph.clear()
        self.domain_index.clear()
        self.concept_index.clear()
        domains = KnowledgeDomain.query.all()
        for db_domain in domains:
            domain = KnowledgeDomain(name=db_domain.name, description=db_domain.description, domain_type=db_domain.domain_type, parent_domain=db_domain.parent_domain_id, attributes=db_domain.attributes)
            domain.id = db_domain.id
            domain.created_at = db_domain.created_at
            domain.updated_at = db_domain.updated_at
            if db_domain.terminology:
                domain.terminology = set(db_domain.terminology)
            self.domains[domain.id] = domain
            self.domain_graph.add_node(domain.id)
            self._index_domain(domain)
        concepts = ConceptNode.query.all()
        for db_concept in concepts:
            concept = ConceptNode(name=db_concept.name, description=db_concept.description, node_type=db_concept.node_type, attributes=db_concept.attributes)
            concept.id = db_concept.id
            concept.created_at = db_concept.created_at
            concept.updated_at = db_concept.updated_at
            for db_domain in db_concept.domains:
                if db_domain.id in self.domains:
                    concept.domain_id = db_domain.id
                    break
            if db_concept.vector_data:
                try:
                    vector = np.array(json.loads(db_concept.vector_data), dtype=float)
                    concept.set_vector(vector)
                except (ValueError, TypeError, json.JSONDecodeError) as e:
                    logger.warning(f'Error loading vector for concept {concept.id}: {str(e)}')
            self.concepts[concept.id] = concept
            self.concept_graph.add_node(concept.id)
            self._index_concept(concept)
        domain_connections = DomainConnection.query.all()
        for conn in domain_connections:
            if conn.source_domain_id in self.domains and conn.target_domain_id in self.domains:
                source = self.domains[conn.source_domain_id]
                source.connections[conn.target_domain_id] = {'id': conn.id, 'type': conn.connection_type, 'strength': conn.strength, 'description': conn.description, 'bidirectional': conn.bidirectional, 'attributes': conn.attributes}
                self.domain_graph.add_edge(conn.source_domain_id, conn.target_domain_id, weight=1.0 - conn.strength, type=conn.connection_type)
        concept_connections = ConceptConnection.query.all()
        for conn in concept_connections:
            if conn.source_concept_id in self.concepts and conn.target_concept_id in self.concepts:
                source = self.concepts[conn.source_concept_id]
                source.connections[conn.target_concept_id] = {'id': conn.id, 'type': conn.relation_type, 'strength': conn.strength, 'description': conn.description, 'bidirectional': conn.bidirectional, 'attributes': conn.attributes}
                self.concept_graph.add_edge(conn.source_concept_id, conn.target_concept_id, weight=1.0 - conn.strength, type=conn.relation_type)
        logger.info(f'Reloaded connection mapper data from database: {len(self.domains)} domains, {len(self.concepts)} concepts, {self.domain_graph.number_of_edges()} domain connections, {self.concept_graph.number_of_edges()} concept connections')