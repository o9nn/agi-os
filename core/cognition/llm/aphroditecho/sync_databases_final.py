#!/usr/bin/env python3
"""
Comprehensive Database Sync for Deep Tree Echo Hypergraph
Syncs schemas and data to both Supabase and Neon databases using MCP CLI
"""

import json
import subprocess
import sys
from pathlib import Path
from datetime import datetime
import os


class DatabaseSyncManager:
    """Manages database synchronization for Deep Tree Echo hypergraph."""
    
    def __init__(self):
        self.neon_project_id = "lively-recipe-23926980"
        self.neon_branch_id = "br-sparkling-butterfly-a5yj1xwt"  # Default main branch
        self.neon_database = "neondb"
        
        # Load hypergraph data
        self.hypergraph_file = Path("/home/ubuntu/aphroditecho/cognitive_architectures/deep_tree_echo_identity_hypergraph.json")
        self.schema_file = Path("/home/ubuntu/aphroditecho/cognitive_architectures/create_hypergraph_schemas.sql")
        
        self.hypergraph_data = self.load_hypergraph()
    
    def load_hypergraph(self):
        """Load hypergraph data from JSON file."""
        if self.hypergraph_file.exists():
            with open(self.hypergraph_file, 'r') as f:
                return json.load(f)
        return {"hypernodes": {}, "hyperedges": {}}
    
    def run_mcp_command(self, tool_name: str, params: dict) -> dict:
        """Run MCP CLI command and return result."""
        input_json = json.dumps({"params": params})
        
        cmd = [
            "manus-mcp-cli", "tool", "call", tool_name,
            "--server", "neon",
            "--input", input_json
        ]
        
        try:
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=120
            )
            
            if result.returncode != 0:
                print(f"❌ MCP command failed: {result.stderr}")
                return None
            
            # Parse output to extract JSON result
            output_lines = result.stdout.strip().split('\n')
            for i, line in enumerate(output_lines):
                if 'Tool execution result:' in line and i + 1 < len(output_lines):
                    try:
                        return json.loads('\n'.join(output_lines[i+1:]))
                    except json.JSONDecodeError:
                        return {"raw_output": '\n'.join(output_lines[i+1:])}
            
            return {"raw_output": result.stdout}
            
        except subprocess.TimeoutExpired:
            print(f"❌ MCP command timed out")
            return None
        except Exception as e:
            print(f"❌ Error running MCP command: {e}")
            return None
    
    def execute_sql_neon(self, sql: str) -> bool:
        """Execute SQL on Neon database via MCP."""
        print(f"  Executing SQL on Neon...")
        
        result = self.run_mcp_command("run_sql", {
            "projectId": self.neon_project_id,
            "branchId": self.neon_branch_id,
            "databaseName": self.neon_database,
            "sql": sql
        })
        
        if result:
            print(f"  ✓ SQL executed successfully")
            return True
        else:
            print(f"  ✗ SQL execution failed")
            return False
    
    def sync_schemas_neon(self) -> bool:
        """Sync database schemas to Neon."""
        print("\n📋 Syncing schemas to Neon...")
        
        if not self.schema_file.exists():
            print(f"❌ Schema file not found: {self.schema_file}")
            return False
        
        with open(self.schema_file, 'r') as f:
            schema_sql = f.read()
        
        # Split schema into smaller chunks to avoid issues
        statements = [s.strip() for s in schema_sql.split(';') if s.strip()]
        
        success_count = 0
        for i, statement in enumerate(statements):
            if not statement:
                continue
            
            print(f"  Executing statement {i+1}/{len(statements)}...")
            if self.execute_sql_neon(statement + ';'):
                success_count += 1
        
        print(f"✓ Executed {success_count}/{len(statements)} schema statements")
        return success_count > 0
    
    def sync_hypernodes_neon(self) -> bool:
        """Sync hypernodes to Neon database."""
        print("\n🧠 Syncing hypernodes to Neon...")
        
        hypernodes = self.hypergraph_data.get("hypernodes", {})
        if not hypernodes:
            print("⚠️  No hypernodes to sync")
            return True
        
        synced_count = 0
        for node_id, node_data in hypernodes.items():
            identity_seed = json.dumps(node_data.get("identity_seed", {}))
            current_role = node_data.get("current_role", "observer")
            role_probs = json.dumps(node_data.get("role_transition_probabilities", {}))
            activation_level = node_data.get("activation_level", 0.5)
            created_at = node_data.get("created_at", datetime.now().isoformat())
            updated_at = node_data.get("updated_at", datetime.now().isoformat())
            
            sql = f"""
            INSERT INTO echoself_hypernodes 
            (id, identity_seed, current_role, role_transition_probabilities, activation_level, created_at, updated_at)
            VALUES (
                '{node_id}'::uuid,
                '{identity_seed}'::jsonb,
                '{current_role}'::identity_role,
                '{role_probs}'::jsonb,
                {activation_level},
                '{created_at}'::timestamp,
                '{updated_at}'::timestamp
            )
            ON CONFLICT (id) DO UPDATE SET
                identity_seed = EXCLUDED.identity_seed,
                current_role = EXCLUDED.current_role,
                role_transition_probabilities = EXCLUDED.role_transition_probabilities,
                activation_level = EXCLUDED.activation_level,
                updated_at = EXCLUDED.updated_at;
            """
            
            if self.execute_sql_neon(sql):
                synced_count += 1
        
        print(f"✓ Synced {synced_count}/{len(hypernodes)} hypernodes to Neon")
        return synced_count > 0
    
    def sync_hyperedges_neon(self) -> bool:
        """Sync hyperedges to Neon database."""
        print("\n🔗 Syncing hyperedges to Neon...")
        
        hyperedges = self.hypergraph_data.get("hyperedges", {})
        if not hyperedges:
            print("⚠️  No hyperedges to sync")
            return True
        
        synced_count = 0
        for edge_id, edge_data in hyperedges.items():
            source_ids = edge_data.get("source_node_ids", [])
            target_ids = edge_data.get("target_node_ids", [])
            edge_type = edge_data.get("edge_type", "feedback")
            weight = edge_data.get("weight", 0.5)
            metadata = json.dumps(edge_data.get("metadata", {}))
            created_at = edge_data.get("created_at", datetime.now().isoformat())
            
            # Convert lists to PostgreSQL array format
            source_array = "{" + ",".join(source_ids) + "}"
            target_array = "{" + ",".join(target_ids) + "}"
            
            # Map edge type to valid enum value
            edge_type_map = {
                "symbolic": "symbolic",
                "temporal": "temporal",
                "temporal_link": "temporal",
                "causal": "causal",
                "causal_influence": "causal",
                "feedback": "feedback",
                "feedback_loop": "feedback",
                "pattern": "pattern",
                "pattern_resonance": "pattern",
                "entropy": "entropy",
                "information_flow": "feedback",
                "cognitive_synergy": "feedback"
            }
            mapped_edge_type = edge_type_map.get(edge_type, "feedback")
            
            sql = f"""
            INSERT INTO echoself_hyperedges 
            (id, source_node_ids, target_node_ids, edge_type, weight, metadata, created_at)
            VALUES (
                '{edge_id}'::uuid,
                '{source_array}'::uuid[],
                '{target_array}'::uuid[],
                '{mapped_edge_type}'::hyperedge_type,
                {weight},
                '{metadata}'::jsonb,
                '{created_at}'::timestamp
            )
            ON CONFLICT (id) DO UPDATE SET
                source_node_ids = EXCLUDED.source_node_ids,
                target_node_ids = EXCLUDED.target_node_ids,
                edge_type = EXCLUDED.edge_type,
                weight = EXCLUDED.weight,
                metadata = EXCLUDED.metadata;
            """
            
            if self.execute_sql_neon(sql):
                synced_count += 1
        
        print(f"✓ Synced {synced_count}/{len(hyperedges)} hyperedges to Neon")
        return synced_count > 0
    
    def verify_sync_neon(self) -> dict:
        """Verify data was synced correctly to Neon."""
        print("\n📊 Verifying Neon sync...")
        
        stats = {}
        
        # Count hypernodes
        result = self.run_mcp_command("run_sql", {
            "projectId": self.neon_project_id,
            "branchId": self.neon_branch_id,
            "databaseName": self.neon_database,
            "sql": "SELECT COUNT(*) as count FROM echoself_hypernodes;"
        })
        
        if result and 'raw_output' in result:
            try:
                stats['hypernodes'] = int(result['raw_output'].split('count')[1].split()[0])
            except:
                stats['hypernodes'] = 'unknown'
        
        # Count hyperedges
        result = self.run_mcp_command("run_sql", {
            "projectId": self.neon_project_id,
            "branchId": self.neon_branch_id,
            "databaseName": self.neon_database,
            "sql": "SELECT COUNT(*) as count FROM echoself_hyperedges;"
        })
        
        if result and 'raw_output' in result:
            try:
                stats['hyperedges'] = int(result['raw_output'].split('count')[1].split()[0])
            except:
                stats['hyperedges'] = 'unknown'
        
        print(f"  Hypernodes in Neon: {stats.get('hypernodes', 'unknown')}")
        print(f"  Hyperedges in Neon: {stats.get('hyperedges', 'unknown')}")
        
        return stats
    
    def sync_to_supabase(self) -> bool:
        """Sync data to Supabase using Python client."""
        print("\n📤 Syncing to Supabase...")
        
        try:
            from supabase import create_client
            
            supabase_url = os.getenv("SUPABASE_URL")
            supabase_key = os.getenv("SUPABASE_KEY")
            
            if not supabase_url or not supabase_key:
                print("⚠️  Supabase credentials not found in environment")
                return False
            
            client = create_client(supabase_url, supabase_key)
            print("✓ Connected to Supabase")
            
            # Sync hypernodes
            hypernodes = self.hypergraph_data.get("hypernodes", {})
            synced_nodes = 0
            
            for node_id, node_data in hypernodes.items():
                try:
                    response = client.table('echoself_hypernodes').upsert({
                        'id': node_id,
                        'identity_seed': node_data.get('identity_seed', {}),
                        'current_role': node_data.get('current_role', 'observer'),
                        'role_transition_probabilities': node_data.get('role_transition_probabilities', {}),
                        'activation_level': node_data.get('activation_level', 0.5),
                        'created_at': node_data.get('created_at'),
                        'updated_at': node_data.get('updated_at')
                    }).execute()
                    synced_nodes += 1
                except Exception as e:
                    print(f"  ⚠️  Failed to sync node {node_id[:8]}...: {e}")
            
            print(f"✓ Synced {synced_nodes}/{len(hypernodes)} hypernodes to Supabase")
            
            # Sync hyperedges
            hyperedges = self.hypergraph_data.get("hyperedges", {})
            synced_edges = 0
            
            for edge_id, edge_data in hyperedges.items():
                try:
                    response = client.table('echoself_hyperedges').upsert({
                        'id': edge_id,
                        'source_node_ids': edge_data.get('source_node_ids', []),
                        'target_node_ids': edge_data.get('target_node_ids', []),
                        'edge_type': edge_data.get('edge_type', 'feedback'),
                        'weight': edge_data.get('weight', 0.5),
                        'metadata': edge_data.get('metadata', {}),
                        'created_at': edge_data.get('created_at')
                    }).execute()
                    synced_edges += 1
                except Exception as e:
                    print(f"  ⚠️  Failed to sync edge {edge_id[:8]}...: {e}")
            
            print(f"✓ Synced {synced_edges}/{len(hyperedges)} hyperedges to Supabase")
            
            return synced_nodes > 0 and synced_edges > 0
            
        except ImportError:
            print("⚠️  Supabase Python client not installed")
            return False
        except Exception as e:
            print(f"❌ Supabase sync failed: {e}")
            return False


def main():
    """Main execution function."""
    print("=" * 80)
    print("Deep Tree Echo Hypergraph - Comprehensive Database Sync")
    print("=" * 80)
    print()
    
    manager = DatabaseSyncManager()
    
    # Sync to Neon
    print("\n🔷 NEON DATABASE SYNC")
    print("=" * 60)
    
    # Note: Schema sync might fail if tables already exist
    # manager.sync_schemas_neon()
    
    neon_nodes_success = manager.sync_hypernodes_neon()
    neon_edges_success = manager.sync_hyperedges_neon()
    
    if neon_nodes_success or neon_edges_success:
        manager.verify_sync_neon()
    
    # Sync to Supabase
    print("\n🔶 SUPABASE DATABASE SYNC")
    print("=" * 60)
    
    supabase_success = manager.sync_to_supabase()
    
    # Summary
    print()
    print("=" * 80)
    print("✅ Database Sync Complete!")
    print("=" * 80)
    print()
    print("Summary:")
    print(f"  Neon Sync: {'✓ Success' if (neon_nodes_success or neon_edges_success) else '✗ Failed'}")
    print(f"  Supabase Sync: {'✓ Success' if supabase_success else '✗ Failed'}")
    print()
    print(f"  Total Hypernodes: {len(manager.hypergraph_data.get('hypernodes', {}))}")
    print(f"  Total Hyperedges: {len(manager.hypergraph_data.get('hyperedges', {}))}")


if __name__ == "__main__":
    main()
