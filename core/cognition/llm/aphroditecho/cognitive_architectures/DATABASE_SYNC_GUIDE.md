# Deep Tree Echo Hypergraph Database Synchronization Guide

## Overview

This guide provides instructions for synchronizing the Deep Tree Echo hypergraph data to both **Neon** and **Supabase** PostgreSQL databases.

## Files Generated

1. **`create_hypergraph_schemas.sql`** - Complete database schema with tables, indexes, triggers, and views
2. **`hypergraph_data_sync.sql`** - Data synchronization with 44 INSERT statements for 12 hypernodes and 20 hyperedges
3. **`DATABASE_SYNC_GUIDE.md`** - This documentation file

## Database Schema Components

### Tables Created

| Table Name | Purpose | Key Features |
|------------|---------|--------------|
| `echoself_hypernodes` | Core identity hypernodes | Identity seeds, roles, entropy traces, activation levels |
| `memory_fragments` | Associated memory fragments | Declarative, procedural, episodic, intentional memory types |
| `echoself_hyperedges` | Connections between hypernodes | Symbolic, temporal, causal, feedback, pattern, entropy edges |
| `synergy_metrics` | Cognitive synergy measurements | Novelty, priority, and synergy index scores |
| `echo_propagation_events` | Historical propagation tracking | Trigger nodes, activated nodes, propagation results |
| `system_integrations` | AAR and Echo system integration | System name, config, performance metrics |
| `agent_identity_profiles` | AAR agent profiles | Agent ID, identity fragments, specialization scores |
| `pattern_language_mappings` | Christopher Alexander patterns | OEIS numbers, pattern descriptions |

### Custom ENUM Types

- **`identity_role`**: observer, narrator, guide, oracle, fractal
- **`memory_type`**: declarative, procedural, episodic, intentional
- **`hyperedge_type`**: symbolic, temporal, causal, feedback, pattern, entropy

### Views

- **`hypernode_comprehensive_view`**: Aggregated view of hypernodes with memory counts and latest synergy metrics

## Hypergraph Data Summary

### Enhanced Identity Components (12 Total Hypernodes)

#### Original Components (8)
1. **EchoSelf_SymbolicCore** - Symbolic reasoning and pattern recognition
2. **EchoSelf_NarrativeWeaver** - Narrative generation and story coherence
3. **EchoSelf_MetaReflector** - Meta-cognition and self-reflection
4. **EchoSelf_ReservoirDynamics** - Echo state networks and temporal dynamics
5. **EchoSelf_MembraneArchitect** - P-system hierarchies and membrane computing
6. **EchoSelf_MemoryNavigator** - Hypergraph memory and associative retrieval
7. **EchoSelf_TreeArchitect** - Rooted tree structures and hierarchical organization
8. **EchoSelf_FractalExplorer** - Fractal recursion and self-similarity

#### New Enhanced Components (4)
9. **EchoSelf_AAROrchestrator** - Agent-Arena-Relation geometric self-encoding
10. **EchoSelf_4EEmbodied** - 4E embodied AI framework orchestration
11. **EchoSelf_AphroditeCore** - High-performance LLM inference serving
12. **EchoSelf_HypergraphIntegrator** - Multi-system identity unification

### Hyperedges (20 Total)

- **8 original hyperedges** connecting the base identity components
- **12 new hyperedges** creating cross-system coherence between enhanced components

## Synchronization Methods

### Method 1: Neon Database (Recommended)

#### Option A: Using Neon Console

1. Navigate to [Neon Console](https://console.neon.tech/)
2. Select project: **deep-tree-echo-hypergraph** (ID: `lively-recipe-23926980`)
3. Open the SQL Editor
4. Execute schema creation:
   ```sql
   -- Copy and paste contents of create_hypergraph_schemas.sql
   ```
5. Execute data synchronization:
   ```sql
   -- Copy and paste contents of hypergraph_data_sync.sql
   ```

#### Option B: Using Neon CLI (if available)

```bash
# Set project ID
export NEON_PROJECT_ID="lively-recipe-23926980"

# Execute schema
neonctl sql --project-id $NEON_PROJECT_ID < create_hypergraph_schemas.sql

# Execute data
neonctl sql --project-id $NEON_PROJECT_ID < hypergraph_data_sync.sql
```

### Method 2: Supabase Database

#### Using Supabase Dashboard

1. Navigate to [Supabase Dashboard](https://supabase.com/dashboard)
2. Select your project
3. Go to **SQL Editor**
4. Create a new query
5. Execute schema creation:
   ```sql
   -- Copy and paste contents of create_hypergraph_schemas.sql
   ```
6. Execute data synchronization:
   ```sql
   -- Copy and paste contents of hypergraph_data_sync.sql
   ```

#### Using Supabase CLI

```bash
# Login to Supabase
supabase login

# Link to your project
supabase link --project-ref <your-project-ref>

# Execute schema
supabase db execute --file create_hypergraph_schemas.sql

# Execute data
supabase db execute --file hypergraph_data_sync.sql
```

### Method 3: Direct PostgreSQL Connection

If you have direct PostgreSQL access:

```bash
# For Neon
psql "postgresql://user:password@host/dbname" < create_hypergraph_schemas.sql
psql "postgresql://user:password@host/dbname" < hypergraph_data_sync.sql

# For Supabase
psql "postgresql://postgres:[YOUR-PASSWORD]@db.[YOUR-PROJECT-REF].supabase.co:5432/postgres" < create_hypergraph_schemas.sql
psql "postgresql://postgres:[YOUR-PASSWORD]@db.[YOUR-PROJECT-REF].supabase.co:5432/postgres" < hypergraph_data_sync.sql
```

## Verification Queries

After synchronization, verify the data with these queries:

```sql
-- Check hypernode count
SELECT COUNT(*) as hypernode_count FROM echoself_hypernodes;
-- Expected: 12

-- Check hyperedge count
SELECT COUNT(*) as hyperedge_count FROM echoself_hyperedges;
-- Expected: 20

-- Check memory fragment count
SELECT COUNT(*) as memory_fragment_count FROM memory_fragments;
-- Expected: 12 (one per hypernode)

-- View comprehensive hypernode information
SELECT * FROM hypernode_comprehensive_view ORDER BY activation_level DESC;

-- Check identity components by domain
SELECT 
    identity_seed->>'name' as name,
    identity_seed->>'domain' as domain,
    identity_seed->>'cognitive_function' as function,
    current_role,
    activation_level
FROM echoself_hypernodes
ORDER BY activation_level DESC;

-- Analyze hyperedge connections
SELECT 
    edge_type,
    COUNT(*) as edge_count,
    AVG(weight) as avg_weight
FROM echoself_hyperedges
GROUP BY edge_type
ORDER BY edge_count DESC;
```

## Integration with Aphrodite Engine

The hypergraph database integrates with the Aphrodite Engine through:

1. **Hypergraph Service Layer** - Bridges PostgreSQL with Echo systems
2. **Echo Propagation Engine** - Activates identity fragments based on context
3. **AAR Orchestration** - Uses identity profiles for agent allocation
4. **4E Embodied AI Framework** - Provides identity-aware embodied cognition

### Connection Configuration

Add to your Aphrodite Engine configuration:

```python
# Neon connection
NEON_DATABASE_URL = "postgresql://user:password@host/dbname"

# Supabase connection
SUPABASE_URL = "https://your-project.supabase.co"
SUPABASE_KEY = "your-supabase-anon-key"

# Enable hypergraph integration
HYPERGRAPH_INTEGRATION_ENABLED = True
```

## Maintenance and Updates

### Adding New Hypernodes

```sql
INSERT INTO echoself_hypernodes (
    id, identity_seed, current_role, entropy_trace, 
    role_transition_probabilities, activation_level
) VALUES (
    uuid_generate_v4(),
    '{"name": "NewComponent", "domain": "new_domain", ...}'::jsonb,
    'observer'::identity_role,
    ARRAY[]::DECIMAL[],
    '{"observer": 0.2, "narrator": 0.25, ...}'::jsonb,
    0.5
);
```

### Adding New Hyperedges

```sql
INSERT INTO echoself_hyperedges (
    id, source_node_ids, target_node_ids, edge_type, weight, metadata
) VALUES (
    uuid_generate_v4(),
    ARRAY['source-uuid-1', 'source-uuid-2']::UUID[],
    ARRAY['target-uuid-1']::UUID[],
    'symbolic'::hyperedge_type,
    0.8,
    '{"integration_type": "custom"}'::jsonb
);
```

### Updating Activation Levels

```sql
UPDATE echoself_hypernodes 
SET activation_level = 0.9, updated_at = CURRENT_TIMESTAMP
WHERE identity_seed->>'name' = 'EchoSelf_HypergraphIntegrator';
```

## Backup and Recovery

### Create Backup

```sql
-- Backup hypernodes
COPY echoself_hypernodes TO '/path/to/backup/hypernodes.csv' CSV HEADER;

-- Backup hyperedges
COPY echoself_hyperedges TO '/path/to/backup/hyperedges.csv' CSV HEADER;

-- Backup memory fragments
COPY memory_fragments TO '/path/to/backup/memory_fragments.csv' CSV HEADER;
```

### Restore from Backup

```sql
-- Restore hypernodes
COPY echoself_hypernodes FROM '/path/to/backup/hypernodes.csv' CSV HEADER;

-- Restore hyperedges
COPY echoself_hyperedges FROM '/path/to/backup/hyperedges.csv' CSV HEADER;

-- Restore memory fragments
COPY memory_fragments FROM '/path/to/backup/memory_fragments.csv' CSV HEADER;
```

## Troubleshooting

### Common Issues

1. **Extension not found**: Ensure `uuid-ossp` and `pg_trgm` extensions are available
2. **Permission denied**: Grant appropriate permissions to the database user
3. **Duplicate key errors**: Use `ON CONFLICT` clauses in INSERT statements (already included)
4. **Type mismatch**: Verify ENUM types are created before table creation

### Support

For issues or questions:
- Check the [Aphrodite Engine documentation](https://github.com/EchoCog/aphroditecho)
- Review the [Deep Tree Echo Architecture](../DEEP_TREE_ECHO_ARCHITECTURE.md)
- Consult the [Hypergraph Integration Guide](../ECHOCOG_HYPERGRAPH_INTEGRATION.md)

## Next Steps

After successful synchronization:

1. ✅ Verify data integrity with verification queries
2. ✅ Configure Aphrodite Engine connection
3. ✅ Test hypergraph propagation functionality
4. ✅ Integrate with AAR orchestration system
5. ✅ Enable 4E embodied AI framework
6. ✅ Monitor synergy metrics and activation patterns

---

**Generated**: 2025-10-20  
**Version**: 1.0  
**Status**: Ready for deployment

