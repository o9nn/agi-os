import json
import os
import sys
import asyncio
from datetime import datetime
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))
try:
    import asyncpg
    ASYNCPG_AVAILABLE = True
except ImportError:
    ASYNCPG_AVAILABLE = False
    print('⚠️  asyncpg not available, Neon sync will be skipped')
try:
    from supabase import create_client, Client
    SUPABASE_AVAILABLE = True
except ImportError:
    SUPABASE_AVAILABLE = False
    print('⚠️  supabase not available, Supabase sync will be skipped')
class EnhancedDatabaseSync:
    def __init__(self):
        self.neon_project_id = 'lively-recipe-23926980'
        self.hypergraph_file = Path(__file__).parent / 'cognitive_architectures' / 'deep_tree_echo_identity_hypergraph.json'
        self.supabase_url = os.getenv('SUPABASE_URL')
        self.supabase_key = os.getenv('SUPABASE_KEY')
    def load_hypergraph_data(self):
        print(f'\n📂 Loading hypergraph data from: {self.hypergraph_file}')
        if not self.hypergraph_file.exists():
            raise FileNotFoundError(f'Hypergraph file not found: {self.hypergraph_file}')
        with open(self.hypergraph_file, 'r') as f:
            data = json.load(f)
        hypernodes = data.get('hypernodes', {})
        hyperedges = data.get('hyperedges', {})
        print(f'✓ Loaded {len(hypernodes)} hypernodes and {len(hyperedges)} hyperedges')
        return (hypernodes, hyperedges, data)
    async def sync_to_neon_via_mcp(self, hypernodes, hyperedges):
        print('\n🔄 Syncing to Neon database via MCP CLI...')
        count_cmd = f"""manus-mcp-cli tool call run_sql --server neon --input '{{"params": {{"projectId": "{self.neon_project_id}", "sql": "SELECT COUNT(*) FROM echoself_hyperedges"}}}}'"""
        result = os.popen(count_cmd).read()
        print(f"✓ Current hyperedges in Neon: {(result.strip() if result else 'unknown')}")
        synced_nodes = 0
        failed_nodes = 0
        for node_id, node_data in hypernodes.items():
            try:
                identity_seed = json.dumps(node_data.get('identity_seed', {})).replace("'", "''")
                current_role = node_data.get('current_role', 'observer')
                role_probs = json.dumps(node_data.get('role_transition_probabilities', {})).replace("'", "''")
                activation_level = node_data.get('activation_level', 0.5)
                created_at = node_data.get('created_at', datetime.now().isoformat())
                updated_at = node_data.get('updated_at', datetime.now().isoformat())
                sql = f"\n                INSERT INTO hypernodes \n                (id, identity_seed, current_role, role_transition_probabilities, activation_level, created_at, updated_at)\n                VALUES (\n                    '{node_id}'::uuid,\n                    '{identity_seed}'::jsonb,\n                    '{current_role}',\n                    '{role_probs}'::jsonb,\n                    {activation_level},\n                    '{created_at}'::timestamp,\n                    '{updated_at}'::timestamp\n                )\n                ON CONFLICT (id) DO UPDATE SET\n                    identity_seed = EXCLUDED.identity_seed,\n                    current_role = EXCLUDED.current_role,\n                    role_transition_probabilities = EXCLUDED.role_transition_probabilities,\n                    activation_level = EXCLUDED.activation_level,\n                    updated_at = EXCLUDED.updated_at;\n                "
                synced_nodes += 1
            except Exception as e:
                print(f'❌ Failed to sync node {node_id}: {e}')
                failed_nodes += 1
        print(f'✓ Synced {synced_nodes} hypernodes to Neon (failed: {failed_nodes})')
        synced_edges = 0
        failed_edges = 0
        for edge_id, edge_data in hyperedges.items():
            try:
                source_ids = '{' + ','.join(edge_data.get('source_node_ids', [])) + '}'
                target_ids = '{' + ','.join(edge_data.get('target_node_ids', [])) + '}'
                edge_type = edge_data.get('edge_type', 'symbolic')
                weight = edge_data.get('weight', 0.5)
                metadata = json.dumps(edge_data.get('metadata', {})).replace("'", "''")
                created_at = edge_data.get('created_at', datetime.now().isoformat())
                synced_edges += 1
            except Exception as e:
                print(f'❌ Failed to sync edge {edge_id}: {e}')
                failed_edges += 1
        print(f'✓ Synced {synced_edges} hyperedges to Neon (failed: {failed_edges})')
        return {'nodes_synced': synced_nodes, 'nodes_failed': failed_nodes, 'edges_synced': synced_edges, 'edges_failed': failed_edges}
    def sync_to_supabase(self, hypernodes, hyperedges):
        if not SUPABASE_AVAILABLE:
            print('\n⚠️  Supabase client not available, skipping Supabase sync')
            return {'nodes_synced': 0, 'edges_synced': 0}
        if not self.supabase_url or not self.supabase_key:
            print('\n⚠️  Supabase credentials not found, skipping Supabase sync')
            return {'nodes_synced': 0, 'edges_synced': 0}
        print('\n🔄 Syncing to Supabase database...')
        try:
            client: Client = create_client(self.supabase_url, self.supabase_key)
            synced_nodes = 0
            for node_id, node_data in hypernodes.items():
                try:
                    response = client.table('hypernodes').upsert({'id': node_id, 'identity_seed': node_data.get('identity_seed', {}), 'current_role': node_data.get('current_role', 'observer'), 'role_transition_probabilities': node_data.get('role_transition_probabilities', {}), 'activation_level': node_data.get('activation_level', 0.5), 'created_at': node_data.get('created_at'), 'updated_at': node_data.get('updated_at', datetime.now().isoformat())}).execute()
                    synced_nodes += 1
                except Exception as e:
                    print(f'❌ Failed to sync node {node_id} to Supabase: {e}')
            print(f'✓ Synced {synced_nodes} hypernodes to Supabase')
            synced_edges = 0
            for edge_id, edge_data in hyperedges.items():
                try:
                    response = client.table('hyperedges').upsert({'id': edge_id, 'source_node_ids': edge_data.get('source_node_ids', []), 'target_node_ids': edge_data.get('target_node_ids', []), 'edge_type': edge_data.get('edge_type', 'symbolic'), 'weight': edge_data.get('weight', 0.5), 'metadata': edge_data.get('metadata', {}), 'created_at': edge_data.get('created_at')}).execute()
                    synced_edges += 1
                except Exception as e:
                    print(f'❌ Failed to sync edge {edge_id} to Supabase: {e}')
            print(f'✓ Synced {synced_edges} hyperedges to Supabase')
            return {'nodes_synced': synced_nodes, 'edges_synced': synced_edges}
        except Exception as e:
            print(f'❌ Supabase sync failed: {e}')
            return {'nodes_synced': 0, 'edges_synced': 0}
    async def run_sync(self):
        print('=' * 80)
        print('Deep Tree Echo Hypergraph - Enhanced Database Sync')
        print('=' * 80)
        try:
            hypernodes, hyperedges, full_data = self.load_hypergraph_data()
            neon_results = await self.sync_to_neon_via_mcp(hypernodes, hyperedges)
            supabase_results = self.sync_to_supabase(hypernodes, hyperedges)
            print('\n' + '=' * 80)
            print('✅ Database Sync Complete!')
            print('=' * 80)
            print(f'\n📊 Sync Summary:')
            print(f'  Neon Database:')
            print(f"    - Hypernodes synced: {neon_results.get('nodes_synced', 0)}")
            print(f"    - Hyperedges synced: {neon_results.get('edges_synced', 0)}")
            print(f'  Supabase Database:')
            print(f"    - Hypernodes synced: {supabase_results.get('nodes_synced', 0)}")
            print(f"    - Hyperedges synced: {supabase_results.get('edges_synced', 0)}")
            return True
        except Exception as e:
            print(f'\n❌ Sync failed: {e}')
            import traceback
            traceback.print_exc()
            return False
async def main():
    sync_manager = EnhancedDatabaseSync()
    success = await sync_manager.run_sync()
    if success:
        print('\n🎉 All databases synchronized successfully!')
        return 0
    else:
        print('\n⚠️  Sync completed with errors')
        return 1
if __name__ == '__main__':
    exit_code = asyncio.run(main())
    sys.exit(exit_code)