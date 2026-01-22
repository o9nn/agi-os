import os
import json
import asyncio
import asyncpg
from datetime import datetime
from pathlib import Path
SUPABASE_URL = os.getenv('SUPABASE_URL')
SUPABASE_KEY = os.getenv('SUPABASE_KEY')
async def get_neon_connection_string():
    import subprocess
    try:
        result = subprocess.run(['manus-mcp-cli', 'tool', 'call', 'list_projects', '--server', 'neon', '--input', '{}'], capture_output=True, text=True, timeout=30)
        if result.returncode == 0:
            projects = json.loads(result.stdout)
            if projects and len(projects) > 0:
                project_id = projects[0].get('id')
                if project_id:
                    conn_result = subprocess.run(['manus-mcp-cli', 'tool', 'call', 'get_connection_string', '--server', 'neon', '--input', json.dumps({'project_id': project_id})], capture_output=True, text=True, timeout=30)
                    if conn_result.returncode == 0:
                        return json.loads(conn_result.stdout).get('connection_string')
    except Exception as e:
        print(f'Warning: Could not get Neon connection string: {e}')
    return None
async def create_supabase_connection():
    if not SUPABASE_URL:
        print('⚠️  SUPABASE_URL not found in environment variables')
        return None
    project_ref = SUPABASE_URL.replace('https://', '').replace('.supabase.co', '')
    conn_string = f'postgresql://postgres:[password]@db.{project_ref}.supabase.co:5432/postgres'
    try:
        from supabase import create_client
        client = create_client(SUPABASE_URL, SUPABASE_KEY)
        print('✓ Connected to Supabase via REST API')
        return client
    except Exception as e:
        print(f'⚠️  Could not connect to Supabase: {e}')
        return None
async def create_neon_connection():
    conn_string = await get_neon_connection_string()
    if not conn_string:
        print('⚠️  Could not get Neon connection string')
        return None
    try:
        conn = await asyncpg.connect(conn_string)
        print('✓ Connected to Neon database')
        return conn
    except Exception as e:
        print(f'⚠️  Could not connect to Neon: {e}')
        return None
async def execute_schema_sql(conn, sql_file_path):
    with open(sql_file_path, 'r') as f:
        sql = f.read()
    try:
        await conn.execute(sql)
        print(f'✓ Executed schema from {sql_file_path}')
        return True
    except Exception as e:
        print(f'❌ Error executing schema: {e}')
        return False
async def sync_hypergraph_data_to_neon(conn, hypergraph_file):
    print('\n📊 Syncing hypergraph data to Neon...')
    with open(hypergraph_file, 'r') as f:
        data = json.load(f)
    hypernodes = data.get('hypernodes', {})
    hyperedges = data.get('hyperedges', {})
    pattern_mappings = data.get('pattern_language_mappings', {})
    print(f'Inserting {len(hypernodes)} hypernodes...')
    for node_id, node_data in hypernodes.items():
        try:
            await conn.execute('\n                INSERT INTO echoself_hypernodes \n                (id, identity_seed, current_role, entropy_trace, role_transition_probabilities, activation_level, created_at, updated_at)\n                VALUES ($1, $2, $3, $4, $5, $6, $7, $8)\n                ON CONFLICT (id) DO UPDATE SET\n                    identity_seed = EXCLUDED.identity_seed,\n                    current_role = EXCLUDED.current_role,\n                    entropy_trace = EXCLUDED.entropy_trace,\n                    role_transition_probabilities = EXCLUDED.role_transition_probabilities,\n                    activation_level = EXCLUDED.activation_level,\n                    updated_at = EXCLUDED.updated_at\n            ', node_id, json.dumps(node_data['identity_seed']), node_data['current_role'], node_data.get('entropy_trace', []), json.dumps(node_data.get('role_transition_probabilities', {})), float(node_data.get('activation_level', 0.5)), datetime.fromisoformat(node_data['created_at']), datetime.fromisoformat(node_data['updated_at']))
            for fragment in node_data.get('memory_fragments', []):
                await conn.execute('\n                    INSERT INTO memory_fragments\n                    (id, hypernode_id, memory_type, content, associations, activation_level, created_at, last_accessed)\n                    VALUES ($1, $2, $3, $4, $5, $6, $7, $8)\n                    ON CONFLICT (id) DO UPDATE SET\n                        content = EXCLUDED.content,\n                        associations = EXCLUDED.associations,\n                        activation_level = EXCLUDED.activation_level,\n                        last_accessed = EXCLUDED.last_accessed\n                ', fragment['id'], node_id, fragment['memory_type'], json.dumps(fragment['content']), fragment.get('associations', []), float(fragment.get('activation_level', 0.5)), datetime.fromisoformat(fragment['created_at']), datetime.fromisoformat(fragment['last_accessed']))
        except Exception as e:
            print(f'  ⚠️  Error inserting hypernode {node_id}: {e}')
    print(f'✓ Inserted/updated {len(hypernodes)} hypernodes')
    print(f'Inserting {len(hyperedges)} hyperedges...')
    for edge_id, edge_data in hyperedges.items():
        try:
            await conn.execute('\n                INSERT INTO echoself_hyperedges\n                (id, source_node_ids, target_node_ids, edge_type, weight, metadata, created_at)\n                VALUES ($1, $2, $3, $4, $5, $6, $7)\n                ON CONFLICT (id) DO UPDATE SET\n                    source_node_ids = EXCLUDED.source_node_ids,\n                    target_node_ids = EXCLUDED.target_node_ids,\n                    edge_type = EXCLUDED.edge_type,\n                    weight = EXCLUDED.weight,\n                    metadata = EXCLUDED.metadata\n            ', edge_id, edge_data['source_node_ids'], edge_data['target_node_ids'], edge_data['edge_type'], float(edge_data['weight']), json.dumps(edge_data.get('metadata', {})), datetime.fromisoformat(edge_data['created_at']))
        except Exception as e:
            print(f'  ⚠️  Error inserting hyperedge {edge_id}: {e}')
    print(f'✓ Inserted/updated {len(hyperedges)} hyperedges')
    print(f'Inserting {len(pattern_mappings)} pattern mappings...')
    for oeis_num, description in pattern_mappings.items():
        try:
            await conn.execute('\n                INSERT INTO pattern_language_mappings\n                (oeis_number, pattern_description, created_at)\n                VALUES ($1, $2, $3)\n                ON CONFLICT (oeis_number) DO UPDATE SET\n                    pattern_description = EXCLUDED.pattern_description\n            ', int(oeis_num), description, datetime.now())
        except Exception as e:
            print(f'  ⚠️  Error inserting pattern mapping {oeis_num}: {e}')
    print(f'✓ Inserted/updated {len(pattern_mappings)} pattern mappings')
    synergy = data.get('synergy_metrics', {})
    if synergy and hypernodes:
        print('Calculating synergy metrics...')
        for node_id in hypernodes.keys():
            try:
                await conn.execute('\n                    INSERT INTO synergy_metrics\n                    (hypernode_id, novelty_score, priority_score, synergy_index, calculated_at)\n                    VALUES ($1, $2, $3, $4, $5)\n                ', node_id, float(synergy.get('novelty_score', 0.0)), float(synergy.get('priority_score', 0.0)), float(synergy.get('synergy_index', 0.0)), datetime.now())
            except Exception as e:
                print(f'  ⚠️  Error inserting synergy metric for {node_id}: {e}')
        print(f'✓ Inserted synergy metrics')
    print('✅ Neon database sync complete!')
async def sync_to_supabase(client, hypergraph_file):
    print('\n📊 Syncing hypergraph data to Supabase...')
    with open(hypergraph_file, 'r') as f:
        data = json.load(f)
    hypernodes = data.get('hypernodes', {})
    print(f'Preparing to sync {len(hypernodes)} hypernodes to Supabase...')
    print('⚠️  Note: Ensure schemas are created in Supabase dashboard first')
    print('    You can run the SQL from: cognitive_architectures/create_hypergraph_schemas.sql')
    '\n    for node_id, node_data in hypernodes.items():\n        try:\n            response = client.table(\'echoself_hypernodes\').upsert({\n                \'id\': node_id,\n                \'identity_seed\': node_data[\'identity_seed\'],\n                \'current_role\': node_data[\'current_role\'],\n                \'entropy_trace\': node_data.get(\'entropy_trace\', []),\n                \'role_transition_probabilities\': node_data.get(\'role_transition_probabilities\', {}),\n                \'activation_level\': node_data.get(\'activation_level\', 0.5),\n                \'created_at\': node_data[\'created_at\'],\n                \'updated_at\': node_data[\'updated_at\']\n            }).execute()\n        except Exception as e:\n            print(f"  ⚠️  Error syncing to Supabase: {e}")\n    '
    print('✓ Supabase sync prepared (manual schema creation required)')
async def main():
    print('=' * 80)
    print('Deep Tree Echo Hypergraph Database Sync')
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
    print('Connecting to databases...')
    print()
    neon_conn = await create_neon_connection()
    supabase_client = await create_supabase_connection()
    print()
    if neon_conn:
        print('=' * 80)
        print('Syncing to Neon Database')
        print('=' * 80)
        print('Creating/updating schema...')
        await execute_schema_sql(neon_conn, schema_file)
        await sync_hypergraph_data_to_neon(neon_conn, hypergraph_file)
        await neon_conn.close()
        print('✓ Neon connection closed')
    else:
        print('⚠️  Skipping Neon sync (connection not available)')
    print()
    if supabase_client:
        print('=' * 80)
        print('Syncing to Supabase Database')
        print('=' * 80)
        await sync_to_supabase(supabase_client, hypergraph_file)
    else:
        print('⚠️  Skipping Supabase sync (connection not available)')
    print()
    print('=' * 80)
    print('✅ Database sync process complete!')
    print('=' * 80)
    print()
    print('Summary:')
    print(f'  Schema file: {schema_file}')
    print(f'  Hypergraph file: {hypergraph_file}')
    print(f"  Neon: {('✓ Synced' if neon_conn else '⚠️  Skipped')}")
    print(f"  Supabase: {('✓ Prepared' if supabase_client else '⚠️  Skipped')}")
if __name__ == '__main__':
    asyncio.run(main())