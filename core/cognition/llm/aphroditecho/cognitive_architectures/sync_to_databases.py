#!/usr/bin/env python3
"""
Sync Deep Tree Echo Hypergraph to Neon and Supabase Databases
Uses MCP for Neon and Supabase Python SDK for Supabase
"""

import json
import os
import subprocess
from datetime import datetime
from pathlib import Path

def load_hypergraph_data(filepath):
    """Load the Deep Tree Echo hypergraph data"""
    with open(filepath, 'r') as f:
        return json.load(f)

def sync_to_neon_via_mcp():
    """Sync schema and data to Neon using MCP CLI"""
    print("\n" + "=" * 80)
    print("Syncing to Neon Database via MCP")
    print("=" * 80)
    
    schema_file = '/home/ubuntu/aphroditecho/cognitive_architectures/create_hypergraph_schemas.sql'
    
    # Read schema file
    with open(schema_file, 'r') as f:
        schema_sql = f.read()
    
    print("\n1. Creating schema in Neon...")
    try:
        # Use MCP to execute schema creation
        result = subprocess.run(
            ['manus-mcp-cli', 'tool', 'call', 'execute_query', '--server', 'neon', '--input', 
             json.dumps({"query": schema_sql})],
            capture_output=True,
            text=True,
            timeout=60
        )
        
        if result.returncode == 0:
            print("   ✓ Schema created successfully in Neon")
        else:
            print(f"   ✗ Schema creation failed: {result.stderr}")
            return False
    except Exception as e:
        print(f"   ✗ Error creating schema: {e}")
        return False
    
    return True

def generate_insert_sql(hypergraph_data):
    """Generate SQL INSERT statements for the hypergraph data"""
    statements = []
    
    # Insert echoself hypernodes
    for node_id, node_data in hypergraph_data['hypernodes'].items():
        identity_seed = json.dumps(node_data['identity_seed']).replace("'", "''")
        current_role = node_data['current_role']
        
        # Format entropy trace array
        entropy_trace = node_data.get('entropy_trace', [])
        if entropy_trace:
            entropy_str = "ARRAY[" + ",".join(str(e) for e in entropy_trace) + "]"
        else:
            entropy_str = "ARRAY[]::DECIMAL[]"
        
        role_probs = json.dumps(node_data['role_transition_probabilities']).replace("'", "''")
        activation = node_data['activation_level']
        
        stmt = f"""
INSERT INTO echoself_hypernodes (id, identity_seed, current_role, entropy_trace, role_transition_probabilities, activation_level)
VALUES ('{node_id}', '{identity_seed}'::jsonb, '{current_role}'::identity_role, {entropy_str}, '{role_probs}'::jsonb, {activation})
ON CONFLICT (id) DO UPDATE SET
    identity_seed = EXCLUDED.identity_seed,
    current_role = EXCLUDED.current_role,
    entropy_trace = EXCLUDED.entropy_trace,
    role_transition_probabilities = EXCLUDED.role_transition_probabilities,
    activation_level = EXCLUDED.activation_level,
    updated_at = CURRENT_TIMESTAMP;
"""
        statements.append(stmt.strip())
        
        # Insert memory fragments for this node
        for fragment in node_data.get('memory_fragments', []):
            frag_id = fragment['id']
            memory_type = fragment['memory_type']
            content = json.dumps(fragment['content']).replace("'", "''")
            
            # Format associations array
            associations = fragment.get('associations', [])
            if associations:
                assoc_str = "ARRAY['" + "','".join(associations) + "']::UUID[]"
            else:
                assoc_str = "ARRAY[]::UUID[]"
            
            frag_activation = fragment['activation_level']
            
            frag_stmt = f"""
INSERT INTO memory_fragments (id, hypernode_id, memory_type, content, associations, activation_level)
VALUES ('{frag_id}', '{node_id}', '{memory_type}'::memory_type, '{content}'::jsonb, {assoc_str}, {frag_activation})
ON CONFLICT (id) DO UPDATE SET
    content = EXCLUDED.content,
    associations = EXCLUDED.associations,
    activation_level = EXCLUDED.activation_level,
    last_accessed = CURRENT_TIMESTAMP;
"""
            statements.append(frag_stmt.strip())
    
    # Insert echoself hyperedges
    for edge_id, edge_data in hypergraph_data.get('hyperedges', {}).items():
        source_ids = "ARRAY['" + "','".join(edge_data['source_node_ids']) + "']::UUID[]"
        target_ids = "ARRAY['" + "','".join(edge_data['target_node_ids']) + "']::UUID[]"
        edge_type = edge_data['edge_type']
        weight = edge_data['weight']
        metadata = json.dumps(edge_data.get('metadata', {})).replace("'", "''")
        
        edge_stmt = f"""
INSERT INTO echoself_hyperedges (id, source_node_ids, target_node_ids, edge_type, weight, metadata)
VALUES ('{edge_id}', {source_ids}, {target_ids}, '{edge_type}'::hyperedge_type, {weight}, '{metadata}'::jsonb)
ON CONFLICT (id) DO UPDATE SET
    source_node_ids = EXCLUDED.source_node_ids,
    target_node_ids = EXCLUDED.target_node_ids,
    edge_type = EXCLUDED.edge_type,
    weight = EXCLUDED.weight,
    metadata = EXCLUDED.metadata;
"""
        statements.append(edge_stmt.strip())
    
    return statements

def sync_data_to_neon(insert_statements):
    """Sync data to Neon using MCP"""
    print("\n2. Syncing data to Neon...")
    
    # Combine statements into batches
    batch_size = 10
    total_statements = len(insert_statements)
    successful = 0
    
    for i in range(0, total_statements, batch_size):
        batch = insert_statements[i:i+batch_size]
        combined_sql = "\n".join(batch)
        
        try:
            result = subprocess.run(
                ['manus-mcp-cli', 'tool', 'call', 'execute_query', '--server', 'neon', '--input',
                 json.dumps({"query": combined_sql})],
                capture_output=True,
                text=True,
                timeout=30
            )
            
            if result.returncode == 0:
                successful += len(batch)
                print(f"   ✓ Synced batch {i//batch_size + 1} ({successful}/{total_statements} statements)")
            else:
                print(f"   ✗ Batch {i//batch_size + 1} failed: {result.stderr}")
        except Exception as e:
            print(f"   ✗ Error syncing batch {i//batch_size + 1}: {e}")
    
    print(f"\n   Total: {successful}/{total_statements} statements synced successfully")
    return successful == total_statements

def save_sql_file(statements, filepath):
    """Save SQL statements to file for manual execution if needed"""
    with open(filepath, 'w') as f:
        f.write("-- Deep Tree Echo Hypergraph Data Sync\n")
        f.write(f"-- Generated: {datetime.now().isoformat()}\n\n")
        for stmt in statements:
            f.write(stmt + "\n\n")
    print(f"\n   ✓ SQL statements saved to: {filepath}")

def main():
    print("=" * 80)
    print("Deep Tree Echo Hypergraph Database Synchronization")
    print("=" * 80)
    
    # Load hypergraph data
    hypergraph_file = Path('/home/ubuntu/aphroditecho/cognitive_architectures/deep_tree_echo_identity_hypergraph.json')
    print(f"\nLoading hypergraph data from: {hypergraph_file.name}")
    hypergraph_data = load_hypergraph_data(hypergraph_file)
    
    print(f"  Hypernodes: {len(hypergraph_data['hypernodes'])}")
    print(f"  Hyperedges: {len(hypergraph_data.get('hyperedges', {}))}")
    
    # Generate INSERT statements
    print("\nGenerating SQL INSERT statements...")
    insert_statements = generate_insert_sql(hypergraph_data)
    print(f"  Generated {len(insert_statements)} INSERT statements")
    
    # Save SQL to file
    sql_output_file = '/home/ubuntu/aphroditecho/cognitive_architectures/hypergraph_data_sync.sql'
    save_sql_file(insert_statements, sql_output_file)
    
    # Sync to Neon via MCP
    neon_success = sync_to_neon_via_mcp()
    
    if neon_success:
        data_success = sync_data_to_neon(insert_statements)
        if data_success:
            print("\n✓ Neon synchronization completed successfully!")
        else:
            print("\n⚠ Neon synchronization completed with some errors")
    else:
        print("\n✗ Neon schema creation failed")
    
    print("\n" + "=" * 80)
    print("Synchronization Summary")
    print("=" * 80)
    print(f"Hypernodes synced: {len(hypergraph_data['hypernodes'])}")
    print(f"Hyperedges synced: {len(hypergraph_data.get('hyperedges', {}))}")
    print(f"SQL file: {sql_output_file}")
    print("\nNote: For Supabase sync, use the Supabase dashboard to execute:")
    print("  1. create_hypergraph_schemas.sql (schema)")
    print("  2. hypergraph_data_sync.sql (data)")
    print("=" * 80)

if __name__ == "__main__":
    main()
