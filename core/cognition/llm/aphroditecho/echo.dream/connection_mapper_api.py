import logging
import json
import uuid
from collections import defaultdict
import numpy as np
from flask import Blueprint, request, jsonify
from datetime import datetime
from database import db
from models_mapper import KnowledgeDomain, ConceptNode, DomainConnection, ConceptConnection, ConnectionPattern
from connection_mapper import get_connection_mapper
logger = logging.getLogger(__name__)
mapper_api = Blueprint('mapper_api', __name__)
@mapper_api.route('/domains', methods=['GET'])
def get_domains():
    query = request.args.get('query')
    if query:
        mapper = get_connection_mapper()
        results = mapper.search_domains(query)
        return jsonify({'domains': results, 'count': len(results)})
    else:
        domains = KnowledgeDomain.query.all()
        return jsonify({'domains': [domain.to_dict() for domain in domains], 'count': len(domains)})
@mapper_api.route('/domains', methods=['POST'])
def create_domain():
    data = request.json
    if not data or 'name' not in data:
        return (jsonify({'error': 'Domain name is required'}), 400)
    mapper = get_connection_mapper()
    domain_id = mapper.create_domain(name=data['name'], description=data.get('description'), domain_type=data.get('domain_type', 'general'), parent_domain=data.get('parent_domain_id'), attributes=data.get('attributes'))
    domain = KnowledgeDomain(id=domain_id, name=data['name'], description=data.get('description'), domain_type=data.get('domain_type', 'general'), parent_domain_id=data.get('parent_domain_id'), attributes=data.get('attributes', {}))
    if 'terminology' in data and isinstance(data['terminology'], list):
        domain.terminology = data['terminology']
    db.session.add(domain)
    db.session.commit()
    logger.info(f"Created domain '{data['name']}' with ID {domain_id}")
    return jsonify({'domain': domain.to_dict(), 'message': 'Domain created successfully'})
@mapper_api.route('/domains/<domain_id>', methods=['GET'])
def get_domain(domain_id):
    domain = KnowledgeDomain.query.get(domain_id)
    if not domain:
        return (jsonify({'error': f'Domain with ID {domain_id} not found'}), 404)
    response = domain.to_dict()
    response['connections'] = [conn.to_dict() for conn in domain.outgoing_connections]
    response['concepts'] = [concept.to_dict() for concept in domain.concepts]
    return jsonify({'domain': response})
@mapper_api.route('/domains/<domain_id>', methods=['PUT'])
def update_domain(domain_id):
    domain = KnowledgeDomain.query.get(domain_id)
    if not domain:
        return (jsonify({'error': f'Domain with ID {domain_id} not found'}), 404)
    data = request.json
    if not data:
        return (jsonify({'error': 'No update data provided'}), 400)
    if 'name' in data:
        domain.name = data['name']
    if 'description' in data:
        domain.description = data['description']
    if 'domain_type' in data:
        domain.domain_type = data['domain_type']
    if 'parent_domain_id' in data:
        domain.parent_domain_id = data['parent_domain_id']
    if 'attributes' in data:
        domain.attributes = data['attributes']
    if 'terminology' in data and isinstance(data['terminology'], list):
        domain.terminology = data['terminology']
    domain.updated_at = datetime.now()
    db.session.commit()
    mapper = get_connection_mapper()
    if domain_id in mapper.domains:
        mapper_domain = mapper.domains[domain_id]
        if 'name' in data:
            mapper_domain.name = data['name']
        if 'description' in data:
            mapper_domain.description = data['description']
        if 'domain_type' in data:
            mapper_domain.domain_type = data['domain_type']
        if 'attributes' in data:
            mapper_domain.attributes = data['attributes']
        if 'terminology' in data and isinstance(data['terminology'], list):
            mapper_domain.terminology.update(data['terminology'])
        mapper_domain.updated_at = datetime.now()
    logger.info(f"Updated domain '{domain.name}' (ID: {domain_id})")
    return jsonify({'domain': domain.to_dict(), 'message': 'Domain updated successfully'})
@mapper_api.route('/domains/<domain_id>', methods=['DELETE'])
def delete_domain(domain_id):
    domain = KnowledgeDomain.query.get(domain_id)
    if not domain:
        return (jsonify({'error': f'Domain with ID {domain_id} not found'}), 404)
    domain_name = domain.name
    db.session.delete(domain)
    db.session.commit()
    mapper = get_connection_mapper()
    if domain_id in mapper.domains:
        del mapper.domains[domain_id]
        if domain_id in mapper.domain_graph:
            mapper.domain_graph.remove_node(domain_id)
    logger.info(f"Deleted domain '{domain_name}' (ID: {domain_id})")
    return jsonify({'message': f"Domain '{domain_name}' deleted successfully"})
@mapper_api.route('/concepts', methods=['GET'])
def get_concepts():
    query = request.args.get('query')
    domain_id = request.args.get('domain_id')
    if query:
        mapper = get_connection_mapper()
        results = mapper.search_concepts(query)
        if domain_id:
            results = [r for r in results if domain_id in [d.id for d in r.get('domains', [])]]
        return jsonify({'concepts': results, 'count': len(results)})
    elif domain_id:
        domain = KnowledgeDomain.query.get(domain_id)
        if not domain:
            return (jsonify({'error': f'Domain with ID {domain_id} not found'}), 404)
        concepts = domain.concepts
        return jsonify({'concepts': [concept.to_dict() for concept in concepts], 'count': len(concepts)})
    else:
        concepts = ConceptNode.query.all()
        return jsonify({'concepts': [concept.to_dict() for concept in concepts], 'count': len(concepts)})
@mapper_api.route('/concepts', methods=['POST'])
def create_concept():
    data = request.json
    if not data or 'name' not in data:
        return (jsonify({'error': 'Concept name is required'}), 400)
    if 'domain_id' not in data:
        return (jsonify({'error': 'Domain ID is required'}), 400)
    domain = KnowledgeDomain.query.get(data['domain_id'])
    if not domain:
        return (jsonify({'error': f"Domain with ID {data['domain_id']} not found"}), 404)
    mapper = get_connection_mapper()
    concept_id = mapper.create_concept(name=data['name'], domain_id=data['domain_id'], description=data.get('description'), node_type=data.get('node_type', 'concept'), attributes=data.get('attributes'))
    concept = ConceptNode(id=concept_id, name=data['name'], description=data.get('description'), node_type=data.get('node_type', 'concept'), attributes=data.get('attributes', {}))
    concept.domains.append(domain)
    if 'vector' in data and isinstance(data['vector'], list):
        try:
            vector = np.array(data['vector'], dtype=float)
            concept.vector_data = json.dumps(vector.tolist())
            if concept_id in mapper.concepts:
                mapper.concepts[concept_id].set_vector(vector)
        except (ValueError, TypeError) as e:
            logger.warning(f'Invalid vector data: {str(e)}')
    db.session.add(concept)
    db.session.commit()
    logger.info(f"Created concept '{data['name']}' with ID {concept_id}")
    return jsonify({'concept': concept.to_dict(), 'message': 'Concept created successfully'})
@mapper_api.route('/concepts/<concept_id>', methods=['GET'])
def get_concept(concept_id):
    concept = ConceptNode.query.get(concept_id)
    if not concept:
        return (jsonify({'error': f'Concept with ID {concept_id} not found'}), 404)
    response = concept.to_dict()
    response['connections'] = [conn.to_dict() for conn in concept.outgoing_connections]
    response['domains'] = [domain.to_dict() for domain in concept.domains]
    return jsonify({'concept': response})
@mapper_api.route('/concepts/<concept_id>', methods=['PUT'])
def update_concept(concept_id):
    concept = ConceptNode.query.get(concept_id)
    if not concept:
        return (jsonify({'error': f'Concept with ID {concept_id} not found'}), 404)
    data = request.json
    if not data:
        return (jsonify({'error': 'No update data provided'}), 400)
    if 'name' in data:
        concept.name = data['name']
    if 'description' in data:
        concept.description = data['description']
    if 'node_type' in data:
        concept.node_type = data['node_type']
    if 'attributes' in data:
        concept.attributes = data['attributes']
    if 'domain_ids' in data and isinstance(data['domain_ids'], list):
        concept.domains = []
        for domain_id in data['domain_ids']:
            domain = KnowledgeDomain.query.get(domain_id)
            if domain:
                concept.domains.append(domain)
    if 'vector' in data and isinstance(data['vector'], list):
        try:
            vector = np.array(data['vector'], dtype=float)
            concept.vector_data = json.dumps(vector.tolist())
            mapper = get_connection_mapper()
            if concept_id in mapper.concepts:
                mapper.concepts[concept_id].set_vector(vector)
        except (ValueError, TypeError) as e:
            logger.warning(f'Invalid vector data: {str(e)}')
    concept.updated_at = datetime.now()
    db.session.commit()
    mapper = get_connection_mapper()
    if concept_id in mapper.concepts:
        mapper_concept = mapper.concepts[concept_id]
        if 'name' in data:
            mapper_concept.name = data['name']
        if 'description' in data:
            mapper_concept.description = data['description']
        if 'node_type' in data:
            mapper_concept.node_type = data['node_type']
        if 'attributes' in data:
            mapper_concept.attributes = data['attributes']
        mapper_concept.updated_at = datetime.now()
    logger.info(f"Updated concept '{concept.name}' (ID: {concept_id})")
    return jsonify({'concept': concept.to_dict(), 'message': 'Concept updated successfully'})
@mapper_api.route('/domain-connections', methods=['POST'])
def create_domain_connection():
    data = request.json
    if not data or 'source_domain_id' not in data or 'target_domain_id' not in data:
        return (jsonify({'error': 'Source and target domain IDs are required'}), 400)
    source_domain = KnowledgeDomain.query.get(data['source_domain_id'])
    if not source_domain:
        return (jsonify({'error': f"Source domain with ID {data['source_domain_id']} not found"}), 404)
    target_domain = KnowledgeDomain.query.get(data['target_domain_id'])
    if not target_domain:
        return (jsonify({'error': f"Target domain with ID {data['target_domain_id']} not found"}), 404)
    mapper = get_connection_mapper()
    connection_id = mapper.connect_domains(source_domain_id=data['source_domain_id'], target_domain_id=data['target_domain_id'], connection_type=data.get('connection_type', 'related'), strength=data.get('strength', 0.5), description=data.get('description'), bidirectional=data.get('bidirectional', True), attributes=data.get('attributes'))
    connection = DomainConnection(id=connection_id, source_domain_id=data['source_domain_id'], target_domain_id=data['target_domain_id'], connection_type=data.get('connection_type', 'related'), strength=data.get('strength', 0.5), description=data.get('description'), bidirectional=data.get('bidirectional', True), attributes=data.get('attributes', {}))
    db.session.add(connection)
    if data.get('bidirectional', True):
        reverse_connection_id = None
        if data['target_domain_id'] in mapper.domains and data['source_domain_id'] in mapper.domains[data['target_domain_id']].connections:
            reverse_connection_id = mapper.domains[data['target_domain_id']].connections[data['source_domain_id']].get('id')
        if not reverse_connection_id:
            reverse_connection_id = str(uuid.uuid4())
        reverse_connection = DomainConnection(id=reverse_connection_id, source_domain_id=data['target_domain_id'], target_domain_id=data['source_domain_id'], connection_type=data.get('connection_type', 'related'), strength=data.get('strength', 0.5), description=data.get('description'), bidirectional=False, attributes=data.get('attributes', {}))
        db.session.add(reverse_connection)
    db.session.commit()
    logger.info(f"Created connection from domain {data['source_domain_id']} to domain {data['target_domain_id']}")
    return jsonify({'connection': connection.to_dict(), 'message': 'Domain connection created successfully'})
@mapper_api.route('/connections/domains', methods=['POST'])
def api_create_domain_connection():
    return create_domain_connection()
@mapper_api.route('/concept-connections', methods=['POST'])
def create_concept_connection():
    data = request.json
    if not data or 'source_concept_id' not in data or 'target_concept_id' not in data:
        return (jsonify({'error': 'Source and target concept IDs are required'}), 400)
    source_concept = ConceptNode.query.get(data['source_concept_id'])
    if not source_concept:
        return (jsonify({'error': f"Source concept with ID {data['source_concept_id']} not found"}), 404)
    target_concept = ConceptNode.query.get(data['target_concept_id'])
    if not target_concept:
        return (jsonify({'error': f"Target concept with ID {data['target_concept_id']} not found"}), 404)
    mapper = get_connection_mapper()
    connection_id = mapper.connect_concepts(source_concept_id=data['source_concept_id'], target_concept_id=data['target_concept_id'], relation_type=data.get('relation_type', 'related'), strength=data.get('strength', 0.5), description=data.get('description'), bidirectional=data.get('bidirectional', True), attributes=data.get('attributes'))
    connection = ConceptConnection(id=connection_id, source_concept_id=data['source_concept_id'], target_concept_id=data['target_concept_id'], relation_type=data.get('relation_type', 'related'), strength=data.get('strength', 0.5), description=data.get('description'), bidirectional=data.get('bidirectional', True), attributes=data.get('attributes', {}))
    db.session.add(connection)
    if data.get('bidirectional', True):
        reverse_connection_id = None
        if data['target_concept_id'] in mapper.concepts and data['source_concept_id'] in mapper.concepts[data['target_concept_id']].connections:
            reverse_connection_id = mapper.concepts[data['target_concept_id']].connections[data['source_concept_id']].get('id')
        if not reverse_connection_id:
            reverse_connection_id = str(uuid.uuid4())
        reverse_connection = ConceptConnection(id=reverse_connection_id, source_concept_id=data['target_concept_id'], target_concept_id=data['source_concept_id'], relation_type=data.get('relation_type', 'related'), strength=data.get('strength', 0.5), description=data.get('description'), bidirectional=False, attributes=data.get('attributes', {}))
        db.session.add(reverse_connection)
    db.session.commit()
    logger.info(f"Created connection from concept {data['source_concept_id']} to concept {data['target_concept_id']}")
    return jsonify({'connection': connection.to_dict(), 'message': 'Concept connection created successfully'})
@mapper_api.route('/connections/concepts', methods=['POST'])
def api_create_concept_connection():
    return create_concept_connection()
@mapper_api.route('/domain-connections/<connection_id>', methods=['DELETE'])
def delete_domain_connection(connection_id):
    connection = DomainConnection.query.get(connection_id)
    if not connection:
        return (jsonify({'error': f'Connection with ID {connection_id} not found'}), 404)
    source_id = connection.source_domain_id
    target_id = connection.target_domain_id
    bidirectional = connection.bidirectional
    db.session.delete(connection)
    if bidirectional:
        reverse_connection = DomainConnection.query.filter_by(source_domain_id=target_id, target_domain_id=source_id).first()
        if reverse_connection:
            db.session.delete(reverse_connection)
    db.session.commit()
    mapper = get_connection_mapper()
    if source_id in mapper.domains and target_id in mapper.domains[source_id].connections:
        del mapper.domains[source_id].connections[target_id]
    if bidirectional and target_id in mapper.domains and (source_id in mapper.domains[target_id].connections):
        del mapper.domains[target_id].connections[source_id]
    if mapper.domain_graph.has_edge(source_id, target_id):
        mapper.domain_graph.remove_edge(source_id, target_id)
    if bidirectional and mapper.domain_graph.has_edge(target_id, source_id):
        mapper.domain_graph.remove_edge(target_id, source_id)
    logger.info(f'Deleted connection from domain {source_id} to domain {target_id}')
    return jsonify({'message': 'Domain connection deleted successfully'})
@mapper_api.route('/concept-connections/<connection_id>', methods=['DELETE'])
def delete_concept_connection(connection_id):
    connection = ConceptConnection.query.get(connection_id)
    if not connection:
        return (jsonify({'error': f'Connection with ID {connection_id} not found'}), 404)
    source_id = connection.source_concept_id
    target_id = connection.target_concept_id
    bidirectional = connection.bidirectional
    db.session.delete(connection)
    if bidirectional:
        reverse_connection = ConceptConnection.query.filter_by(source_concept_id=target_id, target_concept_id=source_id).first()
        if reverse_connection:
            db.session.delete(reverse_connection)
    db.session.commit()
    mapper = get_connection_mapper()
    if source_id in mapper.concepts and target_id in mapper.concepts[source_id].connections:
        del mapper.concepts[source_id].connections[target_id]
    if bidirectional and target_id in mapper.concepts and (source_id in mapper.concepts[target_id].connections):
        del mapper.concepts[target_id].connections[source_id]
    if mapper.concept_graph.has_edge(source_id, target_id):
        mapper.concept_graph.remove_edge(source_id, target_id)
    if bidirectional and mapper.concept_graph.has_edge(target_id, source_id):
        mapper.concept_graph.remove_edge(target_id, source_id)
    logger.info(f'Deleted connection from concept {source_id} to concept {target_id}')
    return jsonify({'message': 'Concept connection deleted successfully'})
@mapper_api.route('/network/domains', methods=['GET'])
def get_domain_network():
    include_connections = request.args.get('include_connections', 'true').lower() == 'true'
    mapper = get_connection_mapper()
    mapper.generate_domain_network(include_connections)
    nodes = []
    edges = []
    for domain_id, domain in mapper.domains.items():
        nodes.append({'id': domain_id, 'name': domain.name, 'domain_type': domain.domain_type, 'description': domain.description, 'concept_count': len(domain.concepts) if hasattr(domain, 'concepts') else 0, 'connection_count': len(domain.connections) if hasattr(domain, 'connections') else 0})
    for source_id, source in mapper.domains.items():
        for target_id, connection in source.connections.items():
            if source_id != target_id:
                edges.append({'source': source_id, 'target': target_id, 'type': connection.get('type', 'related'), 'strength': connection.get('strength', 0.5), 'weight': connection.get('strength', 0.5), 'description': connection.get('description', '')})
    return jsonify({'nodes': nodes, 'edges': edges})
@mapper_api.route('/network/concepts', methods=['GET'])
def get_concept_network():
    domain_id = request.args.get('domain_id')
    request.args.get('include_connections', 'true').lower() == 'true'
    get_connection_mapper()
    nodes = []
    edges = []
    if domain_id:
        domain = KnowledgeDomain.query.get(domain_id)
        if not domain:
            return (jsonify({'error': f'Domain with ID {domain_id} not found'}), 404)
        concepts = ConceptNode.query.filter(ConceptNode.domains.any(id=domain_id)).all()
        for concept in concepts:
            nodes.append({'id': concept.id, 'name': concept.name, 'node_type': concept.node_type, 'description': concept.description, 'domain_count': len(concept.domains), 'connection_count': len(concept.outgoing_connections) + len(concept.incoming_connections)})
        for concept in concepts:
            for connection in concept.outgoing_connections:
                if connection.target_concept_id in [c.id for c in concepts]:
                    edges.append({'source': connection.source_concept_id, 'target': connection.target_concept_id, 'type': connection.relation_type, 'strength': connection.strength, 'weight': connection.strength, 'description': connection.description or ''})
    else:
        concepts = ConceptNode.query.all()
        for concept in concepts:
            nodes.append({'id': concept.id, 'name': concept.name, 'node_type': concept.node_type, 'description': concept.description, 'domain_count': len(concept.domains), 'connection_count': len(concept.outgoing_connections) + len(concept.incoming_connections)})
        connections = ConceptConnection.query.all()
        for connection in connections:
            edges.append({'source': connection.source_concept_id, 'target': connection.target_concept_id, 'type': connection.relation_type, 'strength': connection.strength, 'weight': connection.strength, 'description': connection.description or ''})
    return jsonify({'nodes': nodes, 'edges': edges})
@mapper_api.route('/connection-patterns', methods=['GET'])
def discover_patterns():
    mapper = get_connection_mapper()
    patterns = mapper.discover_connection_patterns()
    for pattern in patterns:
        existing = ConnectionPattern.query.filter_by(pattern_type=pattern['pattern_type'], description=pattern.get('description', '')).first()
        if not existing:
            entities = []
            if 'domain_id' in pattern:
                entities.append(pattern['domain_id'])
            if 'concept_id' in pattern:
                entities.append(pattern['concept_id'])
            if 'domains' in pattern:
                entities.extend([d.get('id') for d in pattern['domains'] if d.get('id')])
            score = pattern.get('centrality_score', pattern.get('betweenness_score', 1.0))
            db_pattern = ConnectionPattern(pattern_type=pattern['pattern_type'], description=pattern.get('description', ''), score=score, entities=entities, pattern_metadata={k: v for k, v in pattern.items() if k not in ('pattern_type', 'description', 'domain_id', 'concept_id')})
            db.session.add(db_pattern)
    db.session.commit()
    return jsonify({'patterns': patterns, 'count': len(patterns)})
@mapper_api.route('/interdisciplinary-paths', methods=['GET'])
def find_interdisciplinary_paths():
    source_domain_id = request.args.get('source_domain_id')
    target_domain_id = request.args.get('target_domain_id')
    max_paths = int(request.args.get('max_paths', 3))
    max_length = int(request.args.get('max_length', 4))
    if not source_domain_id or not target_domain_id:
        return (jsonify({'error': 'Source and target domain IDs are required'}), 400)
    mapper = get_connection_mapper()
    try:
        paths = mapper.find_interdisciplinary_paths(source_domain_id, target_domain_id, max_paths, max_length)
    except ValueError as e:
        return (jsonify({'error': str(e)}), 404)
    return jsonify({'paths': paths, 'count': len(paths)})
@mapper_api.route('/export', methods=['GET'])
def export_data():
    format_type = request.args.get('format', 'json')
    if format_type not in ('json', 'xml'):
        return (jsonify({'error': f'Unsupported format: {format_type}'}), 400)
    mapper = get_connection_mapper()
    exported_data = mapper.export_data(format_type)
    if format_type == 'json':
        return jsonify(json.loads(exported_data))
    else:
        return (exported_data, 200, {'Content-Type': 'application/xml'})
@mapper_api.route('/import', methods=['POST'])
def import_data():
    if not request.is_json:
        return (jsonify({'error': 'Request must contain JSON data'}), 400)
    data = request.json
    counters = {'domains_created': 0, 'concepts_created': 0, 'domain_connections_created': 0, 'concept_connections_created': 0}
    domain_id_map = {}
    if 'domains' in data and isinstance(data['domains'], list):
        for domain_data in data['domains']:
            if not isinstance(domain_data, dict) or 'name' not in domain_data:
                continue
            domain = KnowledgeDomain(id=str(uuid.uuid4()), name=domain_data['name'], description=domain_data.get('description'), domain_type=domain_data.get('domain_type', 'general'), attributes=domain_data.get('attributes', {}))
            if 'terminology' in domain_data and isinstance(domain_data['terminology'], list):
                domain.terminology = domain_data['terminology']
            db.session.add(domain)
            if 'id' in domain_data:
                domain_id_map[domain_data['id']] = domain.id
            counters['domains_created'] += 1
    concept_id_map = {}
    if 'concepts' in data and isinstance(data['concepts'], list):
        for concept_data in data['concepts']:
            if not isinstance(concept_data, dict) or 'name' not in concept_data:
                continue
            concept = ConceptNode(id=str(uuid.uuid4()), name=concept_data['name'], description=concept_data.get('description'), node_type=concept_data.get('node_type', 'concept'), attributes=concept_data.get('attributes', {}))
            if 'domain_ids' in concept_data and isinstance(concept_data['domain_ids'], list):
                for orig_domain_id in concept_data['domain_ids']:
                    if orig_domain_id in domain_id_map:
                        domain = KnowledgeDomain.query.get(domain_id_map[orig_domain_id])
                        if domain:
                            concept.domains.append(domain)
            if 'vector' in concept_data and isinstance(concept_data['vector'], list):
                try:
                    vector = np.array(concept_data['vector'], dtype=float)
                    concept.vector_data = json.dumps(vector.tolist())
                except (ValueError, TypeError) as e:
                    logger.warning(f'Invalid vector data: {str(e)}')
            db.session.add(concept)
            if 'id' in concept_data:
                concept_id_map[concept_data['id']] = concept.id
            counters['concepts_created'] += 1
    db.session.commit()
    seen_domain_connections = defaultdict(set)
    if 'domain_connections' in data and isinstance(data['domain_connections'], list):
        for conn_data in data['domain_connections']:
            if not isinstance(conn_data, dict) or 'source_domain_id' not in conn_data or 'target_domain_id' not in conn_data:
                continue
            source_id = domain_id_map.get(conn_data['source_domain_id'])
            target_id = domain_id_map.get(conn_data['target_domain_id'])
            if not source_id or not target_id:
                continue
            if target_id in seen_domain_connections[source_id]:
                continue
            connection = DomainConnection(id=str(uuid.uuid4()), source_domain_id=source_id, target_domain_id=target_id, connection_type=conn_data.get('connection_type', 'related'), strength=conn_data.get('strength', 0.5), description=conn_data.get('description'), bidirectional=conn_data.get('bidirectional', True), attributes=conn_data.get('attributes', {}))
            db.session.add(connection)
            seen_domain_connections[source_id].add(target_id)
            if conn_data.get('bidirectional', True):
                reverse_connection = DomainConnection(id=str(uuid.uuid4()), source_domain_id=target_id, target_domain_id=source_id, connection_type=conn_data.get('connection_type', 'related'), strength=conn_data.get('strength', 0.5), description=conn_data.get('description'), bidirectional=False, attributes=conn_data.get('attributes', {}))
                db.session.add(reverse_connection)
                seen_domain_connections[target_id].add(source_id)
            counters['domain_connections_created'] += 1
    seen_concept_connections = defaultdict(set)
    if 'concept_connections' in data and isinstance(data['concept_connections'], list):
        for conn_data in data['concept_connections']:
            if not isinstance(conn_data, dict) or 'source_concept_id' not in conn_data or 'target_concept_id' not in conn_data:
                continue
            source_id = concept_id_map.get(conn_data['source_concept_id'])
            target_id = concept_id_map.get(conn_data['target_concept_id'])
            if not source_id or not target_id:
                continue
            if target_id in seen_concept_connections[source_id]:
                continue
            connection = ConceptConnection(id=str(uuid.uuid4()), source_concept_id=source_id, target_concept_id=target_id, relation_type=conn_data.get('relation_type', 'related'), strength=conn_data.get('strength', 0.5), description=conn_data.get('description'), bidirectional=conn_data.get('bidirectional', True), attributes=conn_data.get('attributes', {}))
            db.session.add(connection)
            seen_concept_connections[source_id].add(target_id)
            if conn_data.get('bidirectional', True):
                reverse_connection = ConceptConnection(id=str(uuid.uuid4()), source_concept_id=target_id, target_concept_id=source_id, relation_type=conn_data.get('relation_type', 'related'), strength=conn_data.get('strength', 0.5), description=conn_data.get('description'), bidirectional=False, attributes=conn_data.get('attributes', {}))
                db.session.add(reverse_connection)
                seen_concept_connections[target_id].add(source_id)
            counters['concept_connections_created'] += 1
    db.session.commit()
    mapper = get_connection_mapper()
    mapper.reload_from_database()
    logger.info(f'Imported data: {counters}')
    return jsonify({'message': 'Data imported successfully', 'stats': counters})