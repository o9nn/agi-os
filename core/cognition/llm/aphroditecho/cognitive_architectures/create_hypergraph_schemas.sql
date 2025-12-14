-- Deep Tree Echo Hypergraph Database Schema
-- Compatible with both Neon and Supabase PostgreSQL

-- Enable required extensions
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pg_trgm";  -- For text similarity search

-- Create custom ENUM types
DO $$ BEGIN
    CREATE TYPE identity_role AS ENUM ('observer', 'narrator', 'guide', 'oracle', 'fractal');
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

DO $$ BEGIN
    CREATE TYPE memory_type AS ENUM ('declarative', 'procedural', 'episodic', 'intentional');
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

DO $$ BEGIN
    CREATE TYPE hyperedge_type AS ENUM ('symbolic', 'temporal', 'causal', 'feedback', 'pattern', 'entropy');
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

-- Echoself Hypernodes Table
CREATE TABLE IF NOT EXISTS echoself_hypernodes (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    identity_seed JSONB NOT NULL,
    current_role identity_role NOT NULL DEFAULT 'observer',
    entropy_trace DECIMAL[] DEFAULT ARRAY[]::DECIMAL[],
    role_transition_probabilities JSONB NOT NULL DEFAULT '{}'::jsonb,
    activation_level DECIMAL NOT NULL DEFAULT 0.5 CHECK (activation_level >= 0 AND activation_level <= 1),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Create indexes for efficient querying
CREATE INDEX IF NOT EXISTS idx_hypernodes_identity_seed ON echoself_hypernodes USING GIN (identity_seed);
CREATE INDEX IF NOT EXISTS idx_hypernodes_current_role ON echoself_hypernodes (current_role);
CREATE INDEX IF NOT EXISTS idx_hypernodes_activation ON echoself_hypernodes (activation_level DESC);
CREATE INDEX IF NOT EXISTS idx_hypernodes_created_at ON echoself_hypernodes (created_at DESC);

-- Memory Fragments Table
CREATE TABLE IF NOT EXISTS memory_fragments (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    hypernode_id UUID NOT NULL REFERENCES echoself_hypernodes(id) ON DELETE CASCADE,
    memory_type memory_type NOT NULL,
    content JSONB NOT NULL,
    associations UUID[] DEFAULT ARRAY[]::UUID[],
    activation_level DECIMAL NOT NULL DEFAULT 0.5 CHECK (activation_level >= 0 AND activation_level <= 1),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    last_accessed TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Create indexes for memory fragments
CREATE INDEX IF NOT EXISTS idx_memory_fragments_hypernode ON memory_fragments (hypernode_id);
CREATE INDEX IF NOT EXISTS idx_memory_fragments_type ON memory_fragments (memory_type);
CREATE INDEX IF NOT EXISTS idx_memory_fragments_content ON memory_fragments USING GIN (content);
CREATE INDEX IF NOT EXISTS idx_memory_fragments_activation ON memory_fragments (activation_level DESC);

-- Echoself Hyperedges Table
CREATE TABLE IF NOT EXISTS echoself_hyperedges (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    source_node_ids UUID[] NOT NULL,
    target_node_ids UUID[] NOT NULL,
    edge_type hyperedge_type NOT NULL,
    weight DECIMAL NOT NULL DEFAULT 1.0 CHECK (weight >= 0 AND weight <= 1),
    metadata JSONB DEFAULT '{}'::jsonb,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Create indexes for hyperedges
CREATE INDEX IF NOT EXISTS idx_hyperedges_source_nodes ON echoself_hyperedges USING GIN (source_node_ids);
CREATE INDEX IF NOT EXISTS idx_hyperedges_target_nodes ON echoself_hyperedges USING GIN (target_node_ids);
CREATE INDEX IF NOT EXISTS idx_hyperedges_type ON echoself_hyperedges (edge_type);
CREATE INDEX IF NOT EXISTS idx_hyperedges_weight ON echoself_hyperedges (weight DESC);

-- Synergy Metrics Table
CREATE TABLE IF NOT EXISTS synergy_metrics (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    hypernode_id UUID NOT NULL REFERENCES echoself_hypernodes(id) ON DELETE CASCADE,
    novelty_score DECIMAL NOT NULL DEFAULT 0.0,
    priority_score DECIMAL NOT NULL DEFAULT 0.0,
    synergy_index DECIMAL NOT NULL DEFAULT 0.0,
    calculated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Create indexes for synergy metrics
CREATE INDEX IF NOT EXISTS idx_synergy_metrics_hypernode ON synergy_metrics (hypernode_id);
CREATE INDEX IF NOT EXISTS idx_synergy_metrics_synergy_index ON synergy_metrics (synergy_index DESC);
CREATE INDEX IF NOT EXISTS idx_synergy_metrics_calculated_at ON synergy_metrics (calculated_at DESC);

-- Pattern Language Mappings Table
CREATE TABLE IF NOT EXISTS pattern_language_mappings (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    oeis_number INTEGER UNIQUE NOT NULL,
    pattern_description TEXT NOT NULL,
    related_hypernodes UUID[] DEFAULT ARRAY[]::UUID[],
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Create index for pattern mappings
CREATE INDEX IF NOT EXISTS idx_pattern_mappings_oeis ON pattern_language_mappings (oeis_number);

-- Echo Propagation Events Table (for tracking activation propagation)
CREATE TABLE IF NOT EXISTS echo_propagation_events (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    trigger_nodes UUID[] NOT NULL,
    activated_nodes UUID[] NOT NULL,
    max_depth INTEGER NOT NULL DEFAULT 3,
    min_weight DECIMAL NOT NULL DEFAULT 0.1,
    propagation_result JSONB NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Create indexes for propagation events
CREATE INDEX IF NOT EXISTS idx_propagation_events_trigger ON echo_propagation_events USING GIN (trigger_nodes);
CREATE INDEX IF NOT EXISTS idx_propagation_events_created_at ON echo_propagation_events (created_at DESC);

-- Integration Tracking Table (for AAR, Echo systems, etc.)
CREATE TABLE IF NOT EXISTS system_integrations (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    system_name VARCHAR(50) NOT NULL,
    hypernode_id UUID REFERENCES echoself_hypernodes(id) ON DELETE CASCADE,
    integration_config JSONB NOT NULL DEFAULT '{}'::jsonb,
    performance_metrics JSONB DEFAULT '{}'::jsonb,
    status VARCHAR(20) DEFAULT 'active',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Create indexes for system integrations
CREATE INDEX IF NOT EXISTS idx_system_integrations_name ON system_integrations (system_name);
CREATE INDEX IF NOT EXISTS idx_system_integrations_hypernode ON system_integrations (hypernode_id);
CREATE INDEX IF NOT EXISTS idx_system_integrations_status ON system_integrations (status);

-- Agent Identity Profiles Table (for AAR integration)
CREATE TABLE IF NOT EXISTS agent_identity_profiles (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    agent_id VARCHAR(100) NOT NULL UNIQUE,
    identity_fragments UUID[] NOT NULL,
    performance_history JSONB DEFAULT '{}'::jsonb,
    specialization_score DECIMAL DEFAULT 0.5,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Create indexes for agent profiles
CREATE INDEX IF NOT EXISTS idx_agent_profiles_agent_id ON agent_identity_profiles (agent_id);
CREATE INDEX IF NOT EXISTS idx_agent_profiles_fragments ON agent_identity_profiles USING GIN (identity_fragments);

-- Create function to update updated_at timestamp
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Create triggers for automatic timestamp updates
DROP TRIGGER IF EXISTS update_echoself_hypernodes_updated_at ON echoself_hypernodes;
CREATE TRIGGER update_echoself_hypernodes_updated_at
    BEFORE UPDATE ON echoself_hypernodes
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();

DROP TRIGGER IF EXISTS update_system_integrations_updated_at ON system_integrations;
CREATE TRIGGER update_system_integrations_updated_at
    BEFORE UPDATE ON system_integrations
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();

DROP TRIGGER IF EXISTS update_agent_identity_profiles_updated_at ON agent_identity_profiles;
CREATE TRIGGER update_agent_identity_profiles_updated_at
    BEFORE UPDATE ON agent_identity_profiles
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();

-- Create view for comprehensive hypernode information
CREATE OR REPLACE VIEW hypernode_comprehensive_view AS
SELECT 
    h.id,
    h.identity_seed,
    h.current_role,
    h.entropy_trace,
    h.activation_level,
    COUNT(DISTINCT mf.id) as memory_fragment_count,
    COALESCE(sm.synergy_index, 0) as latest_synergy_index,
    COALESCE(sm.novelty_score, 0) as latest_novelty_score,
    COALESCE(sm.priority_score, 0) as latest_priority_score,
    h.created_at,
    h.updated_at
FROM echoself_hypernodes h
LEFT JOIN memory_fragments mf ON h.id = mf.hypernode_id
LEFT JOIN LATERAL (
    SELECT synergy_index, novelty_score, priority_score
    FROM synergy_metrics
    WHERE hypernode_id = h.id
    ORDER BY calculated_at DESC
    LIMIT 1
) sm ON true
GROUP BY h.id, h.identity_seed, h.current_role, h.entropy_trace, h.activation_level,
         sm.synergy_index, sm.novelty_score, sm.priority_score, h.created_at, h.updated_at;

-- Grant permissions (adjust as needed for your environment)
-- For Supabase, you might want to grant to authenticated users
-- GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO authenticated;
-- GRANT USAGE ON ALL SEQUENCES IN SCHEMA public TO authenticated;

COMMENT ON TABLE echoself_hypernodes IS 'Core echoself hypernodes representing identity states in Deep Tree Echo';
COMMENT ON TABLE memory_fragments IS 'Memory fragments associated with echoself hypernodes';
COMMENT ON TABLE echoself_hyperedges IS 'Hyperedges connecting echoself hypernodes';
COMMENT ON TABLE synergy_metrics IS 'Cognitive synergy metrics for hypernodes';
COMMENT ON TABLE echo_propagation_events IS 'Historical record of echo propagation events';
COMMENT ON TABLE system_integrations IS 'Integration tracking for AAR and Echo systems';
COMMENT ON TABLE agent_identity_profiles IS 'Agent identity profiles for AAR orchestration';
