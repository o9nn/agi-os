#!/usr/bin/env python3
"""
Sync Deep Tree Echo Hypergraph to Neon and Supabase Databases
"""

import json
import os
from datetime import datetime

def load_hypergraph_data():
    """Load the Deep Tree Echo hypergraph data"""
    with open('/home/ubuntu/analysis/deep_tree_echo_identity_hypergraph.json', 'r') as f:
        return json.load(f)

def generate_insert_statements(hypergraph_data):
    """Generate SQL INSERT statements for the hypergraph data"""
    statements = []
    
    # Insert echoself hypernodes
    for node_id, node_data in hypergraph_data['hypernodes'].items():
        identity_seed = json.dumps(node_data['identity_seed']).replace("'", "''")
        current_role = node_data['current_role']
        
        # Format entropy trace array
        entropy_trace = node_data['entropy_trace']
        if entropy_trace:
            entropy_str = "ARRAY[" + ",".join(str(e) for e in entropy_trace) + "]"
        else:
            entropy_str = "ARRAY[]::DECIMAL[]"
        
        role_probs = json.dumps(node_data['role_transition_probabilities']).replace("'", "''")
        activation = node_data['activation_level']
        
        stmt = f"""
INSERT INTO echoself_hypernodes (id, identity_seed, current_role, entropy_trace, role_transition_probabilities, activation_level)
VALUES ('{node_id}', '{identity_seed}'::jsonb, '{current_role}'::identity_role, {entropy_str}, '{role_probs}'::jsonb, {activation});
"""
        statements.append(stmt.strip())
        
        # Insert memory fragments for this node
        for fragment in node_data['memory_fragments']:
            frag_id = fragment['id']
            memory_type = fragment['memory_type']
            content = json.dumps(fragment['content']).replace("'", "''")
            
            # Format associations array
            associations = fragment['associations']
            if associations:
                assoc_str = "ARRAY['" + "','".join(associations) + "']::UUID[]"
            else:
                assoc_str = "ARRAY[]::UUID[]"
            
            frag_activation = fragment['activation_level']
            
            frag_stmt = f"""
INSERT INTO memory_fragments (id, hypernode_id, memory_type, content, associations, activation_level)
VALUES ('{frag_id}', '{node_id}', '{memory_type}'::memory_type, '{content}'::jsonb, {assoc_str}, {frag_activation});
"""
            statements.append(frag_stmt.strip())
    
    # Insert echoself hyperedges
    for edge_id, edge_data in hypergraph_data['hyperedges'].items():
        source_ids = "ARRAY['" + "','".join(edge_data['source_node_ids']) + "']::UUID[]"
        target_ids = "ARRAY['" + "','".join(edge_data['target_node_ids']) + "']::UUID[]"
        edge_type = edge_data['edge_type']
        weight = edge_data['weight']
        metadata = json.dumps(edge_data['metadata']).replace("'", "''")
        
        edge_stmt = f"""
INSERT INTO echoself_hyperedges (id, source_node_ids, target_node_ids, edge_type, weight, metadata)
VALUES ('{edge_id}', {source_ids}, {target_ids}, '{edge_type}'::hyperedge_type, {weight}, '{metadata}'::jsonb);
"""
        statements.append(edge_stmt.strip())
    
    # Calculate and insert synergy metrics for each node
    metrics = hypergraph_data['synergy_metrics']
    for node_id in hypergraph_data['hypernodes'].keys():
        novelty = metrics['novelty_score']
        priority = metrics['priority_score']
        synergy = metrics['synergy_index']
        
        metrics_stmt = f"""
INSERT INTO synergy_metrics (hypernode_id, novelty_score, priority_score, synergy_index)
VALUES ('{node_id}', {novelty}, {priority}, {synergy});
"""
        statements.append(metrics_stmt.strip())
    
    return statements

def main():
    print("=" * 80)
    print("Deep Tree Echo Hypergraph Database Sync")
    print("=" * 80)
    
    # Load hypergraph data
    print("\nLoading hypergraph data...")
    hypergraph_data = load_hypergraph_data()
    
    print(f"Loaded {len(hypergraph_data['hypernodes'])} hypernodes")
    print(f"Loaded {len(hypergraph_data['hyperedges'])} hyperedges")
    print(f"Loaded {len(hypergraph_data['pattern_language_mappings'])} pattern mappings")
    
    # Generate INSERT statements
    print("\nGenerating SQL INSERT statements...")
    insert_statements = generate_insert_statements(hypergraph_data)
    
    print(f"Generated {len(insert_statements)} INSERT statements")
    
    # Save to file
    output_file = '/home/ubuntu/analysis/deep_tree_echo_data_inserts.sql'
    with open(output_file, 'w') as f:
        f.write("-- Deep Tree Echo Hypergraph Data Inserts\n")
        f.write(f"-- Generated: {datetime.now().isoformat()}\n\n")
        for stmt in insert_statements:
            f.write(stmt + "\n\n")
    
    print(f"\nSQL INSERT statements saved to: {output_file}")
    
    print("\n" + "=" * 80)
    print("Database sync preparation complete!")
    print("=" * 80)
    print("\nNext steps:")
    print("1. Execute the migration schema on Neon database")
    print("2. Execute the data inserts on Neon database")
    print("3. Repeat for Supabase database")

if __name__ == "__main__":
    main()

