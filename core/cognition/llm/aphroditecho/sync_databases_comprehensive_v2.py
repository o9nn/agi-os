import json
import os
import subprocess
from pathlib import Path
from typing import Dict, List, Any
class DatabaseSyncManager:
    def __init__(self):
        self.neon_project_id = 'lively-recipe-23926980'
        self.neon_database = 'neondb'
        self.hypergraph_file = Path('/home/ubuntu/aphroditecho/cognitive_architectures/deep_tree_echo_identity_hypergraph.json')
        self.supabase_url = os.getenv('SUPABASE_URL')
        self.supabase_key = os.getenv('SUPABASE_KEY')
        self.hypergraph_data = self.load_hypergraph_data()
    def load_hypergraph_data(self) -> Dict[str, Any]:
        if self.hypergraph_file.exists():
            with open(self.hypergraph_file, 'r') as f:
                return json.load(f)
        return {'hypernodes': {}, 'hyperedges': {}, 'metadata': {}}
    def run_mcp_command(self, tool_name: str, params: dict) -> dict:
        try:
            cmd = ['manus-mcp-cli', 'tool', 'call', tool_name, '--server', 'neon', '--input', json.dumps({'params': params})]
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=60)
            if result.returncode == 0:
                try:
                    return json.loads(result.stdout)
                except json.JSONDecodeError:
                    return {'success': True, 'raw_output': result.stdout}
            else:
                print(f'  ⚠️  MCP command failed: {result.stderr}')
                return {'success': False, 'error': result.stderr}
        except Exception as e:
            print(f'  ⚠️  Error running MCP command: {e}')
            return {'success': False, 'error': str(e)}
    def sync_hypernodes_neon(self) -> bool:
        print('\n🧠 Syncing hypernodes to Neon...')
        hypernodes = self.hypergraph_data.get('hypernodes', {})
        if not hypernodes:
            print('⚠️  No hypernodes to sync')
            return True
        batch_size = 10
        node_ids = list(hypernodes.keys())
        synced_count = 0
        for i in range(0, len(node_ids), batch_size):
            batch = node_ids[i:i + batch_size]
            values_list = []
            for node_id in batch:
                node_data = hypernodes[node_id]
                identity_seed = json.dumps(node_data.get('identity_seed', {})).replace("'", "''")
                current_role = node_data.get('current_role', 'observer')
                role_probs = json.dumps(node_data.get('role_transition_probabilities', {})).replace("'", "''")
                activation_level = node_data.get('activation_level', 0.5)
                created_at = node_data.get('created_at', 'NOW()')
                updated_at = node_data.get('updated_at', 'NOW()')
                values_list.append(f"(\n                    '{node_id}'::uuid,\n                    '{identity_seed}'::jsonb,\n                    '{current_role}'::identity_role,\n                    ARRAY[]::DECIMAL[],\n                    '{role_probs}'::jsonb,\n                    {activation_level},\n                    '{created_at}'::timestamp,\n                    '{updated_at}'::timestamp\n                )")
            sql = f"\n            INSERT INTO echoself_hypernodes \n            (id, identity_seed, current_role, entropy_trace, role_transition_probabilities, activation_level, created_at, updated_at)\n            VALUES {','.join(values_list)}\n            ON CONFLICT (id) DO UPDATE SET\n                identity_seed = EXCLUDED.identity_seed,\n                current_role = EXCLUDED.current_role,\n                role_transition_probabilities = EXCLUDED.role_transition_probabilities,\n                activation_level = EXCLUDED.activation_level,\n                updated_at = EXCLUDED.updated_at;\n            "
            result = self.run_mcp_command('run_sql', {'projectId': self.neon_project_id, 'databaseName': self.neon_database, 'sql': sql})
            if result.get('success', True):
                synced_count += len(batch)
                print(f'  ✓ Synced batch {i // batch_size + 1}: {len(batch)} nodes')
        print(f'✓ Synced {synced_count}/{len(hypernodes)} hypernodes to Neon')
        return synced_count > 0
    def sync_hyperedges_neon(self) -> bool:
        print('\n🔗 Syncing hyperedges to Neon...')
        hyperedges = self.hypergraph_data.get('hyperedges', {})
        if not hyperedges:
            print('⚠️  No hyperedges to sync')
            return True
        batch_size = 10
        edge_ids = list(hyperedges.keys())
        synced_count = 0
        edge_type_mapping = {'cognitive_persona_integration': 'symbolic', 'aar_embodiment_coupling': 'causal', 'triadic_identity_integration': 'pattern', 'feedback': 'feedback', 'symbolic': 'symbolic', 'temporal': 'temporal', 'causal': 'causal', 'pattern': 'pattern', 'entropy': 'entropy'}
        for i in range(0, len(edge_ids), batch_size):
            batch = edge_ids[i:i + batch_size]
            values_list = []
            for edge_id in batch:
                edge_data = hyperedges[edge_id]
                source_ids = edge_data.get('source_node_ids', [])
                target_ids = edge_data.get('target_node_ids', [])
                edge_type = edge_data.get('edge_type', 'symbolic')
                weight = edge_data.get('weight', 0.5)
                metadata = json.dumps(edge_data.get('metadata', {})).replace("'", "''")
                created_at = edge_data.get('created_at', 'NOW()')
                source_array = '{' + ','.join(source_ids) + '}'
                target_array = '{' + ','.join(target_ids) + '}'
                mapped_edge_type = edge_type_mapping.get(edge_type, 'symbolic')
                values_list.append(f"(\n                    '{edge_id}'::uuid,\n                    '{source_array}'::uuid[],\n                    '{target_array}'::uuid[],\n                    '{mapped_edge_type}'::hyperedge_type,\n                    {weight},\n                    '{metadata}'::jsonb,\n                    '{created_at}'::timestamp\n                )")
            sql = f"\n            INSERT INTO echoself_hyperedges \n            (id, source_node_ids, target_node_ids, edge_type, weight, metadata, created_at)\n            VALUES {','.join(values_list)}\n            ON CONFLICT (id) DO UPDATE SET\n                source_node_ids = EXCLUDED.source_node_ids,\n                target_node_ids = EXCLUDED.target_node_ids,\n                edge_type = EXCLUDED.edge_type,\n                weight = EXCLUDED.weight,\n                metadata = EXCLUDED.metadata;\n            "
            result = self.run_mcp_command('run_sql', {'projectId': self.neon_project_id, 'databaseName': self.neon_database, 'sql': sql})
            if result.get('success', True):
                synced_count += len(batch)
                print(f'  ✓ Synced batch {i // batch_size + 1}: {len(batch)} edges')
        print(f'✓ Synced {synced_count}/{len(hyperedges)} hyperedges to Neon')
        return synced_count > 0
    def verify_neon_sync(self) -> Dict[str, Any]:
        print('\n📊 Verifying Neon sync...')
        stats = {}
        result = self.run_mcp_command('run_sql', {'projectId': self.neon_project_id, 'databaseName': self.neon_database, 'sql': 'SELECT COUNT(*) as count FROM echoself_hypernodes;'})
        if result.get('success', True) and isinstance(result, list) and (len(result) > 0):
            stats['hypernodes'] = result[0].get('count', 0)
        result = self.run_mcp_command('run_sql', {'projectId': self.neon_project_id, 'databaseName': self.neon_database, 'sql': 'SELECT COUNT(*) as count FROM echoself_hyperedges;'})
        if result.get('success', True) and isinstance(result, list) and (len(result) > 0):
            stats['hyperedges'] = result[0].get('count', 0)
        print(f"  Hypernodes in Neon: {stats.get('hypernodes', 'unknown')}")
        print(f"  Hyperedges in Neon: {stats.get('hyperedges', 'unknown')}")
        return stats
    def sync_to_supabase(self) -> bool:
        print('\n☁️  Syncing to Supabase...')
        if not self.supabase_url or not self.supabase_key:
            print('⚠️  Supabase credentials not found in environment')
            return False
        try:
            from supabase import create_client, Client
            print('✓ Connected to Supabase')
            client: Client = create_client(self.supabase_url, self.supabase_key)
            hypernodes = self.hypergraph_data.get('hypernodes', {})
            synced_nodes = 0
            for node_id, node_data in hypernodes.items():
                try:
                    response = client.table('echoself_hypernodes').upsert({'id': node_id, 'identity_seed': node_data.get('identity_seed', {}), 'current_role': node_data.get('current_role', 'observer'), 'role_transition_probabilities': node_data.get('role_transition_probabilities', {}), 'activation_level': node_data.get('activation_level', 0.5), 'created_at': node_data.get('created_at'), 'updated_at': node_data.get('updated_at')}).execute()
                    synced_nodes += 1
                except Exception as e:
                    print(f'  ⚠️  Failed to sync node {node_id[:8]}...: {e}')
            print(f'✓ Synced {synced_nodes}/{len(hypernodes)} hypernodes to Supabase')
            hyperedges = self.hypergraph_data.get('hyperedges', {})
            synced_edges = 0
            for edge_id, edge_data in hyperedges.items():
                try:
                    response = client.table('echoself_hyperedges').upsert({'id': edge_id, 'source_node_ids': edge_data.get('source_node_ids', []), 'target_node_ids': edge_data.get('target_node_ids', []), 'edge_type': edge_data.get('edge_type', 'symbolic'), 'weight': edge_data.get('weight', 0.5), 'metadata': edge_data.get('metadata', {}), 'created_at': edge_data.get('created_at')}).execute()
                    synced_edges += 1
                except Exception as e:
                    print(f'  ⚠️  Failed to sync edge {edge_id[:8]}...: {e}')
            print(f'✓ Synced {synced_edges}/{len(hyperedges)} hyperedges to Supabase')
            return synced_nodes > 0 and synced_edges > 0
        except ImportError:
            print('⚠️  Supabase Python client not installed. Install with: pip3 install supabase')
            return False
        except Exception as e:
            print(f'❌ Supabase sync failed: {e}')
            return False
    def run_comprehensive_sync(self):
        print('=' * 80)
        print('Deep Tree Echo Hypergraph - Comprehensive Database Sync v2')
        print('=' * 80)
        print()
        print('🔷 Syncing to Neon Database...')
        neon_nodes_success = self.sync_hypernodes_neon()
        neon_edges_success = self.sync_hyperedges_neon()
        if neon_nodes_success or neon_edges_success:
            neon_stats = self.verify_neon_sync()
        print('\n' + '=' * 80)
        supabase_success = self.sync_to_supabase()
        print('\n' + '=' * 80)
        print('Sync Summary')
        print('=' * 80)
        print(f"  Neon Sync: {('✓ Success' if neon_nodes_success or neon_edges_success else '✗ Failed')}")
        print(f"  Supabase Sync: {('✓ Success' if supabase_success else '✗ Failed')}")
        print()
        print(f"  Total Hypernodes: {len(self.hypergraph_data.get('hypernodes', {}))}")
        print(f"  Total Hyperedges: {len(self.hypergraph_data.get('hyperedges', {}))}")
        print('=' * 80)
def main():
    manager = DatabaseSyncManager()
    manager.run_comprehensive_sync()
if __name__ == '__main__':
    main()