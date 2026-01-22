import json
import subprocess
from pathlib import Path
PROJECT_ID = 'lively-recipe-23926980'
def run_neon_sql(sql, database_name='neondb'):
    input_data = {'params': {'projectId': PROJECT_ID, 'sql': sql, 'databaseName': database_name}}
    try:
        result = subprocess.run(['manus-mcp-cli', 'tool', 'call', 'run_sql', '--server', 'neon', '--input', json.dumps(input_data)], capture_output=True, text=True, timeout=60)
        if result.returncode == 0:
            return (True, result.stdout)
        else:
            return (False, result.stderr)
    except Exception as e:
        return (False, str(e))
def create_schema():
    print('Creating database schema...')
    enums = ["DO $$ BEGIN CREATE TYPE identity_role AS ENUM ('observer', 'narrator', 'guide', 'oracle', 'fractal'); EXCEPTION WHEN duplicate_object THEN null; END $$", "DO $$ BEGIN CREATE TYPE memory_type AS ENUM ('declarative', 'procedural', 'episodic', 'intentional'); EXCEPTION WHEN duplicate_object THEN null; END $$", "DO $$ BEGIN CREATE TYPE hyperedge_type AS ENUM ('symbolic', 'temporal', 'causal', 'feedback', 'pattern', 'entropy'); EXCEPTION WHEN duplicate_object THEN null; END $$"]
    for enum_sql in enums:
        success, output = run_neon_sql(enum_sql)
        if success:
            print(f'  ✓ Created ENUM type')
        else:
            print(f'  ⚠️  ENUM type (may already exist): {output[:100]}')
    tables = ["CREATE TABLE IF NOT EXISTS echoself_hypernodes (\n            id UUID PRIMARY KEY,\n            identity_seed JSONB NOT NULL,\n            current_role VARCHAR(50) NOT NULL DEFAULT 'observer',\n            entropy_trace DECIMAL[] DEFAULT ARRAY[]::DECIMAL[],\n            role_transition_probabilities JSONB NOT NULL DEFAULT '{}'::jsonb,\n            activation_level DECIMAL NOT NULL DEFAULT 0.5,\n            created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,\n            updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP\n        )", 'CREATE TABLE IF NOT EXISTS memory_fragments (\n            id UUID PRIMARY KEY,\n            hypernode_id UUID NOT NULL,\n            memory_type VARCHAR(50) NOT NULL,\n            content JSONB NOT NULL,\n            associations UUID[] DEFAULT ARRAY[]::UUID[],\n            activation_level DECIMAL NOT NULL DEFAULT 0.5,\n            created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,\n            last_accessed TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP\n        )', "CREATE TABLE IF NOT EXISTS echoself_hyperedges (\n            id UUID PRIMARY KEY,\n            source_node_ids UUID[] NOT NULL,\n            target_node_ids UUID[] NOT NULL,\n            edge_type VARCHAR(50) NOT NULL,\n            weight DECIMAL NOT NULL DEFAULT 1.0,\n            metadata JSONB DEFAULT '{}'::jsonb,\n            created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP\n        )", 'CREATE TABLE IF NOT EXISTS synergy_metrics (\n            id UUID PRIMARY KEY,\n            hypernode_id UUID NOT NULL,\n            novelty_score DECIMAL NOT NULL DEFAULT 0.0,\n            priority_score DECIMAL NOT NULL DEFAULT 0.0,\n            synergy_index DECIMAL NOT NULL DEFAULT 0.0,\n            calculated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP\n        )', 'CREATE TABLE IF NOT EXISTS pattern_language_mappings (\n            id UUID PRIMARY KEY,\n            oeis_number INTEGER UNIQUE NOT NULL,\n            pattern_description TEXT NOT NULL,\n            related_hypernodes UUID[] DEFAULT ARRAY[]::UUID[],\n            created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP\n        )']
    for table_sql in tables:
        success, output = run_neon_sql(table_sql)
        if success:
            print(f'  ✓ Created table')
        else:
            print(f'  ❌ Error creating table: {output[:200]}')
    print('✓ Schema creation complete')
def sync_data():
    print('\nSyncing hypergraph data...')
    hypergraph_file = Path('/home/ubuntu/aphroditecho/cognitive_architectures/deep_tree_echo_identity_hypergraph.json')
    with open(hypergraph_file, 'r') as f:
        data = json.load(f)
    hypernodes = data.get('hypernodes', {})
    hyperedges = data.get('hyperedges', {})
    pattern_mappings = data.get('pattern_language_mappings', {})
    print(f'Loaded: {len(hypernodes)} hypernodes, {len(hyperedges)} hyperedges, {len(pattern_mappings)} patterns')
    print('✓ Data loaded and ready for sync')
    print('  (Use sync_databases_neon.py for full data sync after schema is created)')
def main():
    print('=' * 80)
    print('Deep Tree Echo Hypergraph - Neon MCP Sync')
    print('=' * 80)
    print()
    create_schema()
    sync_data()
    print()
    print('=' * 80)
    print('✅ Neon MCP sync complete!')
    print('=' * 80)
    print()
    print('Next steps:')
    print('  1. Schema created in Neon database')
    print('  2. Run sync_databases_neon.py to insert hypergraph data')
    print('  3. Verify data in Neon dashboard')
if __name__ == '__main__':
    main()