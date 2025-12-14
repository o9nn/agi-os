-- Deep Tree Echo Hypergraph Database Schema for Neon

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
    id UUID PRIMARY KEY,
    identity_seed JSONB NOT NULL,
    current_role identity_role NOT NULL DEFAULT 'observer',
    entropy_trace DECIMAL[] DEFAULT ARRAY[]::DECIMAL[],
    role_transition_probabilities JSONB NOT NULL DEFAULT '{}'::jsonb,
    activation_level DECIMAL NOT NULL DEFAULT 0.5,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Memory Fragments Table
CREATE TABLE IF NOT EXISTS memory_fragments (
    id UUID PRIMARY KEY,
    hypernode_id UUID NOT NULL REFERENCES echoself_hypernodes(id) ON DELETE CASCADE,
    memory_type memory_type NOT NULL,
    content JSONB NOT NULL,
    associations UUID[] DEFAULT ARRAY[]::UUID[],
    activation_level DECIMAL NOT NULL DEFAULT 0.5,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    last_accessed TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Echoself Hyperedges Table
CREATE TABLE IF NOT EXISTS echoself_hyperedges (
    id UUID PRIMARY KEY,
    source_node_ids UUID[] NOT NULL,
    target_node_ids UUID[] NOT NULL,
    edge_type hyperedge_type NOT NULL,
    weight DECIMAL NOT NULL DEFAULT 1.0,
    metadata JSONB DEFAULT '{}'::jsonb,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Synergy Metrics Table
CREATE TABLE IF NOT EXISTS synergy_metrics (
    id UUID PRIMARY KEY,
    hypernode_id UUID NOT NULL REFERENCES echoself_hypernodes(id) ON DELETE CASCADE,
    novelty_score DECIMAL NOT NULL DEFAULT 0.0,
    priority_score DECIMAL NOT NULL DEFAULT 0.0,
    synergy_index DECIMAL NOT NULL DEFAULT 0.0,
    calculated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Pattern Language Mappings Table
CREATE TABLE IF NOT EXISTS pattern_language_mappings (
    id UUID PRIMARY KEY,
    oeis_number INTEGER UNIQUE NOT NULL,
    pattern_description TEXT NOT NULL,
    related_hypernodes UUID[] DEFAULT ARRAY[]::UUID[],
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);
