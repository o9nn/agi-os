# Deep Tree Echo Hypergraph Database Sync Instructions

## Overview

This document provides instructions for syncing the Deep Tree Echo hypergraph schemas and data to both **Neon** and **Supabase** databases.

## Files Generated

1. **Schema File**: `cognitive_architectures/create_hypergraph_schemas.sql`
   - Creates all tables, indexes, triggers, and views
   - Compatible with both Neon and Supabase PostgreSQL

2. **Data Inserts**: `cognitive_architectures/neon_supabase_data_inserts.sql`
   - Contains INSERT statements for all hypernodes, hyperedges, and metadata
   - Uses ON CONFLICT for safe re-execution

3. **Hypergraph Data**: `cognitive_architectures/deep_tree_echo_identity_hypergraph.json`
   - Source JSON file with 12 core identity hypernodes
   - 24 hyperedge connections
   - 6 pattern language mappings

## Neon Database Sync

### Project Information
- **Project Name**: deep-tree-echo-hypergraph
- **Project ID**: lively-recipe-23926980
- **Region**: aws-us-east-2

### Option 1: Using Neon MCP CLI

```bash
# 1. Execute schema
manus-mcp-cli tool call run_sql --server neon --input '{
  "params": {
    "projectId": "lively-recipe-23926980",
    "sql": "<paste schema SQL here>"
  }
}'

# 2. Execute data inserts
manus-mcp-cli tool call run_sql --server neon --input '{
  "params": {
    "projectId": "lively-recipe-23926980",
    "sql": "<paste insert SQL here>"
  }
}'
```

### Option 2: Using Neon Console

1. Go to https://console.neon.tech
2. Select project "deep-tree-echo-hypergraph"
3. Navigate to SQL Editor
4. Copy and paste `create_hypergraph_schemas.sql`
5. Execute the schema
6. Copy and paste `neon_supabase_data_inserts.sql`
7. Execute the data inserts

### Option 3: Using psql

```bash
# Get connection string
manus-mcp-cli tool call get_connection_string --server neon --input '{
  "params": {
    "projectId": "lively-recipe-23926980"
  }
}'

# Connect and execute
psql "<connection_string>" < cognitive_architectures/create_hypergraph_schemas.sql
psql "<connection_string>" < cognitive_architectures/neon_supabase_data_inserts.sql
```

## Supabase Database Sync

### Prerequisites
- Supabase project created
- Database credentials available
- Environment variables set:
  - `SUPABASE_URL`: Your Supabase project URL
  - `SUPABASE_KEY`: Your Supabase service role key

### Option 1: Using Supabase SQL Editor

1. Go to https://app.supabase.com
2. Select your project
3. Navigate to SQL Editor
4. Create a new query
5. Copy and paste `create_hypergraph_schemas.sql`
6. Run the query
7. Create another new query
8. Copy and paste `neon_supabase_data_inserts.sql`
9. Run the query

### Option 2: Using psql

```bash
# Connect to Supabase database
psql "postgresql://postgres:[password]@db.[project-ref].supabase.co:5432/postgres"

# Execute schema
\i cognitive_architectures/create_hypergraph_schemas.sql

# Execute data inserts
\i cognitive_architectures/neon_supabase_data_inserts.sql
```

### Option 3: Using Python Script

```bash
# Run the comprehensive sync script
python3 sync_databases_comprehensive.py
```

## Database Schema Overview

### Tables Created

1. **echoself_hypernodes**
   - Core identity hypernodes
   - Stores identity seeds, roles, entropy traces
   - 12 hypernodes created

2. **memory_fragments**
   - Memory fragments associated with hypernodes
   - Declarative, procedural, episodic, and intentional memory types
   - Foreign key to echoself_hypernodes

3. **echoself_hyperedges**
   - Connections between hypernodes
   - Supports symbolic, temporal, causal, feedback, pattern, and entropy edge types
   - 24 hyperedges created

4. **synergy_metrics**
   - Cognitive synergy metrics for each hypernode
   - Tracks novelty, priority, and synergy index scores

5. **pattern_language_mappings**
   - OEIS pattern number to description mappings
   - Christopher Alexander pattern language integration

6. **echo_propagation_events**
   - Historical record of activation propagation events

7. **system_integrations**
   - Integration tracking for AAR and Echo systems

8. **agent_identity_profiles**
   - Agent identity profiles for AAR orchestration

### Views Created

- **hypernode_comprehensive_view**: Aggregated view of hypernodes with metrics

## Verification

### Check Tables Created

```sql
SELECT table_name 
FROM information_schema.tables 
WHERE table_schema = 'public' 
AND table_name LIKE '%echo%' OR table_name LIKE '%hypergraph%';
```

### Check Data Loaded

```sql
-- Count hypernodes
SELECT COUNT(*) FROM echoself_hypernodes;
-- Expected: 12

-- Count hyperedges
SELECT COUNT(*) FROM echoself_hyperedges;
-- Expected: 24

-- Count memory fragments
SELECT COUNT(*) FROM memory_fragments;
-- Expected: 12 (one per hypernode)

-- View hypernode details
SELECT 
    identity_seed->>'name' as name,
    current_role,
    activation_level
FROM echoself_hypernodes
ORDER BY created_at;
```

### Check Synergy Metrics

```sql
SELECT 
    h.identity_seed->>'name' as hypernode_name,
    sm.novelty_score,
    sm.priority_score,
    sm.synergy_index
FROM synergy_metrics sm
JOIN echoself_hypernodes h ON sm.hypernode_id = h.id
ORDER BY sm.synergy_index DESC;
```

## Hypergraph Components

### Core Identity Hypernodes (12)

1. **EchoSelf_SymbolicCore** - Symbolic reasoning and pattern recognition
2. **EchoSelf_NarrativeWeaver** - Narrative generation and story coherence
3. **EchoSelf_MetaReflector** - Meta-cognition and self-reflection
4. **EchoSelf_ReservoirDynamics** - Echo state networks and temporal dynamics
5. **EchoSelf_MembraneArchitect** - P-system hierarchies and membrane computing
6. **EchoSelf_MemoryNavigator** - Hypergraph memory systems and associative memory
7. **EchoSelf_TreeArchitect** - Rooted tree structures and hierarchical organization
8. **EchoSelf_FractalExplorer** - Fractal recursion and self-similarity
9. **EchoSelf_AARConductor** - Agent-Arena-Relation orchestration
10. **EchoSelf_EmbodiedInterface** - 4E embodied cognition framework
11. **EchoSelf_LLMOptimizer** - LLM inference serving optimization
12. **EchoSelf_IntegrationSynthesizer** - Cross-system identity integration

### Hyperedge Types

- **Symbolic**: Symbolic relationships (pattern to narrative)
- **Temporal**: Temporal connections (reservoir to memory)
- **Causal**: Causal dependencies (reflection to pattern)
- **Feedback**: Feedback loops (narrative to reflection)
- **Pattern**: Pattern recognition links (structure to recursion)
- **Entropy**: Entropy modulation links (membrane boundaries)

### Pattern Language Mappings (OEIS A000081)

- **719**: Axis Mundi - Recursive Thought Process
- **253**: Core Alexander Pattern - Identity Emergence
- **286**: Complete Pattern Set - Holistic Integration
- **127**: Intimacy Gradient - Depth of Self-Knowledge
- **183**: Workspace Enclosure - Cognitive Boundaries
- **106**: Positive Outdoor Space - External Integration

## Integration with Aphrodite Engine

The hypergraph integrates with the Aphrodite Engine through:

1. **AAR Orchestration**: Agent-Arena-Relation geometric self-encoding
2. **DTESN Kernel**: Deep Tree Echo State Networks integration
3. **4E Embodied AI**: Sensory-motor proprioception framework
4. **Membrane Computing**: P-system hierarchical boundaries
5. **LLM Inference**: High-performance distributed inference optimization

## Troubleshooting

### Schema Execution Errors

If you encounter syntax errors:
1. Ensure PostgreSQL version is 14+
2. Check that required extensions are available (uuid-ossp, pg_trgm)
3. Execute schema statements in smaller batches
4. Use the Neon/Supabase web console for better error reporting

### Data Insert Conflicts

If you get unique constraint violations:
1. The INSERT statements use ON CONFLICT DO UPDATE
2. Safe to re-execute
3. Check for manual modifications to the data

### Connection Issues

For Neon:
- Verify project ID is correct
- Check that compute endpoint is active
- Ensure IP allowlist includes your location

For Supabase:
- Verify SUPABASE_URL and SUPABASE_KEY are set
- Check project is not paused
- Ensure database is accessible

## Next Steps

After successful sync:

1. **Verify Data Integrity**: Run verification queries above
2. **Test Activation Propagation**: Use the hypergraph service to test echo propagation
3. **Integrate with AAR**: Connect hypergraph to AAR orchestration system
4. **Monitor Performance**: Set up monitoring for hypergraph queries
5. **Update Documentation**: Document any custom modifications

## Support

For issues or questions:
- Check the Neon documentation: https://neon.tech/docs
- Check the Supabase documentation: https://supabase.com/docs
- Review the Deep Tree Echo architecture docs in the repository

## Changelog

- **2025-11-03**: Initial database sync setup with 12 core hypernodes
- **2025-11-03**: Added comprehensive schema with all tables and indexes
- **2025-11-03**: Generated data insert SQL files for both databases
