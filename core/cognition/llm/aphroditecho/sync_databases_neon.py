import os
import json
import asyncio
import asyncpg
from datetime import datetime
from pathlib import Path
NEON_CONNECTION_STRING = 'postgresql://neondb_owner:npg_2VJFqYZcAGM9@ep-calm-math-ae0a6p4o-pooler.c-2.us-east-2.aws.neon.tech/neondb?channel_binding=require&sslmode=require'
async def create_neon_connection():
    try:
        conn = await asyncpg.connect(NEON_CONNECTION_STRING)
        print('✓ Connected to Neon database (deep-tree-echo-hypergraph)')
        return conn
    except Exception as e:
        print(f'❌ Could not connect to Neon: {e}')
        return None
async def execute_schema_sql(conn, sql_file_path):
    with open(sql_file_path, 'r') as f:
        sql = f.read()
    statements = [s.strip() for s in sql.split(';') if s.strip()]
    success_count = 0
    for statement in statements:
        if not statement:
            continue
        try:
            await conn.execute(statement)
            success_count += 1
        except Exception as e:
            if 'already exists' not in str(e).lower():
                print(f'  ⚠️  Warning: {e}')
    print(f'✓ Executed {success_count} SQL statements from schema')
    return True
async def sync_hypergraph_data_to_neon(conn, hypergraph_file):
    print('\n📊 Syncing hypergraph data to Neon...')
    with open(hypergraph_file, 'r') as f:
        data = json.load(f)
    hypernodes = data.get('hypernodes', {})
    hyperedges = data.get('hyperedges', {})
    pattern_mappings = data.get('pattern_language_mappings', {})
    print(f'\nInserting {len(hypernodes)} hypernodes...')
    inserted_nodes = 0
    for node_id, node_data in hypernodes.items():
        try:
            await conn.execute('\n                INSERT INTO echoself_hypernodes \n                (id, identity_seed, current_role, entropy_trace, role_transition_probabilities, activation_level, created_at, updated_at)\n                VALUES ($1, $2, $3, $4, $5, $6, $7, $8)\n                ON CONFLICT (id) DO UPDATE SET\n                    identity_seed = EXCLUDED.identity_seed,\n                    current_role = EXCLUDED.current_role,\n                    entropy_trace = EXCLUDED.entropy_trace,\n                    role_transition_probabilities = EXCLUDED.role_transition_probabilities,\n                    activation_level = EXCLUDED.activation_level,\n                    updated_at = EXCLUDED.updated_at\n            ', node_id, json.dumps(node_data['identity_seed']), node_data['current_role'], node_data.get('entropy_trace', []), json.dumps(node_data.get('role_transition_probabilities', {})), float(node_data.get('activation_level', 0.5)), datetime.fromisoformat(node_data['created_at']), datetime.fromisoformat(node_data['updated_at']))
            inserted_nodes += 1
            for fragment in node_data.get('memory_fragments', []):
                await conn.execute('\n                    INSERT INTO memory_fragments\n                    (id, hypernode_id, memory_type, content, associations, activation_level, created_at, last_accessed)\n                    VALUES ($1, $2, $3, $4, $5, $6, $7, $8)\n                    ON CONFLICT (id) DO UPDATE SET\n                        content = EXCLUDED.content,\n                        associations = EXCLUDED.associations,\n                        activation_level = EXCLUDED.activation_level,\n                        last_accessed = EXCLUDED.last_accessed\n                ', fragment['id'], node_id, fragment['memory_type'], json.dumps(fragment['content']), fragment.get('associations', []), float(fragment.get('activation_level', 0.5)), datetime.fromisoformat(fragment['created_at']), datetime.fromisoformat(fragment['last_accessed']))
        except Exception as e:
            print(f'  ⚠️  Error inserting hypernode {node_id}: {e}')
    print(f'✓ Inserted/updated {inserted_nodes} hypernodes')
    print(f'\nInserting {len(hyperedges)} hyperedges...')
    inserted_edges = 0
    for edge_id, edge_data in hyperedges.items():
        try:
            await conn.execute('\n                INSERT INTO echoself_hyperedges\n                (id, source_node_ids, target_node_ids, edge_type, weight, metadata, created_at)\n                VALUES ($1, $2, $3, $4, $5, $6, $7)\n                ON CONFLICT (id) DO UPDATE SET\n                    source_node_ids = EXCLUDED.source_node_ids,\n                    target_node_ids = EXCLUDED.target_node_ids,\n                    edge_type = EXCLUDED.edge_type,\n                    weight = EXCLUDED.weight,\n                    metadata = EXCLUDED.metadata\n            ', edge_id, edge_data['source_node_ids'], edge_data['target_node_ids'], edge_data['edge_type'], float(edge_data['weight']), json.dumps(edge_data.get('metadata', {})), datetime.fromisoformat(edge_data['created_at']))
            inserted_edges += 1
        except Exception as e:
            print(f'  ⚠️  Error inserting hyperedge {edge_id}: {e}')
    print(f'✓ Inserted/updated {inserted_edges} hyperedges')
    print(f'\nInserting {len(pattern_mappings)} pattern mappings...')
    inserted_patterns = 0
    for oeis_num, description in pattern_mappings.items():
        try:
            await conn.execute('\n                INSERT INTO pattern_language_mappings\n                (oeis_number, pattern_description, created_at)\n                VALUES ($1, $2, $3)\n                ON CONFLICT (oeis_number) DO UPDATE SET\n                    pattern_description = EXCLUDED.pattern_description\n            ', int(oeis_num), description, datetime.now())
            inserted_patterns += 1
        except Exception as e:
            print(f'  ⚠️  Error inserting pattern mapping {oeis_num}: {e}')
    print(f'✓ Inserted/updated {inserted_patterns} pattern mappings')
    synergy = data.get('synergy_metrics', {})
    if synergy and hypernodes:
        print('\nCalculating synergy metrics...')
        inserted_metrics = 0
        for node_id in hypernodes.keys():
            try:
                await conn.execute('\n                    INSERT INTO synergy_metrics\n                    (hypernode_id, novelty_score, priority_score, synergy_index, calculated_at)\n                    VALUES ($1, $2, $3, $4, $5)\n                ', node_id, float(synergy.get('novelty_score', 0.0)), float(synergy.get('priority_score', 0.0)), float(synergy.get('synergy_index', 0.0)), datetime.now())
                inserted_metrics += 1
            except Exception as e:
                print(f'  ⚠️  Error inserting synergy metric for {node_id}: {e}')
        print(f'✓ Inserted {inserted_metrics} synergy metrics')
    print('\n✅ Neon database sync complete!')
    print('\n📊 Verifying synced data...')
    node_count = await conn.fetchval('SELECT COUNT(*) FROM echoself_hypernodes')
    edge_count = await conn.fetchval('SELECT COUNT(*) FROM echoself_hyperedges')
    fragment_count = await conn.fetchval('SELECT COUNT(*) FROM memory_fragments')
    pattern_count = await conn.fetchval('SELECT COUNT(*) FROM pattern_language_mappings')
    print(f'  Hypernodes in database: {node_count}')
    print(f'  Hyperedges in database: {edge_count}')
    print(f'  Memory fragments in database: {fragment_count}')
    print(f'  Pattern mappings in database: {pattern_count}')
async def main():
    print('=' * 80)
    print('Deep Tree Echo Hypergraph - Neon Database Sync')
    print('=' * 80)
    print()
    schema_file = Path('/home/ubuntu/aphroditecho/cognitive_architectures/create_hypergraph_schemas.sql')
    hypergraph_file = Path('/home/ubuntu/aphroditecho/cognitive_architectures/deep_tree_echo_identity_hypergraph.json')
    if not schema_file.exists():
        print(f'❌ Schema file not found: {schema_file}')
        return
    if not hypergraph_file.exists():
        print(f'❌ Hypergraph file not found: {hypergraph_file}')
        return
    print('Connecting to Neon database...')
    neon_conn = await create_neon_connection()
    if not neon_conn:
        print('❌ Failed to connect to Neon database')
        return
    print()
    print('=' * 80)
    print('Creating/Updating Database Schema')
    print('=' * 80)
    await execute_schema_sql(neon_conn, schema_file)
    print()
    print('=' * 80)
    print('Syncing Hypergraph Data')
    print('=' * 80)
    await sync_hypergraph_data_to_neon(neon_conn, hypergraph_file)
    await neon_conn.close()
    print('\n✓ Neon connection closed')
    print()
    print('=' * 80)
    print('✅ Database sync complete!')
    print('=' * 80)
    print()
    print('Summary:')
    print(f'  Database: deep-tree-echo-hypergraph (Neon)')
    print(f'  Schema file: {schema_file.name}')
    print(f'  Hypergraph file: {hypergraph_file.name}')
    print(f'  Status: ✓ Successfully synced')
if __name__ == '__main__':
    asyncio.run(main())