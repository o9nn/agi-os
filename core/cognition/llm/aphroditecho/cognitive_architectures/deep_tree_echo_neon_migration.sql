-- Deep Tree Echo Hypergraph Schema Migration for Neon Database
-- This schema supports the echoself hypernodes and hyperedges data structure

-- Enum types for identity roles
CREATE TYPE identity_role AS ENUM (
    'observer',
    'narrator', 
    'guide',
    'oracle',
    'fractal'
);

-- Enum types for memory types
CREATE TYPE memory_type AS ENUM (
    'declarative',
    'procedural',
    'episodic',
    'intentional'
);

-- Enum types for hyperedge types
CREATE TYPE hyperedge_type AS ENUM (
    'symbolic',
    'temporal',
    'causal',
    'feedback',
    'pattern',
    'entropy'
);

-- Echoself hypernodes table
CREATE TABLE echoself_hypernodes (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    identity_seed JSONB NOT NULL,
    current_role identity_role NOT NULL DEFAULT 'observer',
    entropy_trace DECIMAL[] NOT NULL DEFAULT '{}',
    role_transition_probabilities JSONB NOT NULL DEFAULT '{}',
    activation_level DECIMAL NOT NULL DEFAULT 0.5 CHECK (activation_level >= 0.0 AND activation_level <= 1.0),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Memory fragments table
CREATE TABLE memory_fragments (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    hypernode_id UUID NOT NULL REFERENCES echoself_hypernodes(id) ON DELETE CASCADE,
    memory_type memory_type NOT NULL,
    content JSONB NOT NULL,
    associations UUID[] DEFAULT '{}',
    activation_level DECIMAL NOT NULL DEFAULT 0.5 CHECK (activation_level >= 0.0 AND activation_level <= 1.0),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    last_accessed TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Hyperedges table (renamed to avoid conflict with existing hypergraph_edges)
CREATE TABLE echoself_hyperedges (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    source_node_ids UUID[] NOT NULL,
    target_node_ids UUID[] NOT NULL,
    edge_type hyperedge_type NOT NULL,
    weight DECIMAL NOT NULL DEFAULT 1.0 CHECK (weight >= 0.0),
    metadata JSONB DEFAULT '{}',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Pattern language mappings table (OEIS A000081)
CREATE TABLE pattern_language (
    id SERIAL PRIMARY KEY,
    oeis_number INTEGER NOT NULL UNIQUE,
    pattern_name VARCHAR(255) NOT NULL,
    pattern_description TEXT,
    implementation_status VARCHAR(50) DEFAULT 'pending',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Cognitive synergy metrics table
CREATE TABLE synergy_metrics (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    hypernode_id UUID REFERENCES echoself_hypernodes(id) ON DELETE CASCADE,
    novelty_score DECIMAL NOT NULL DEFAULT 0.0 CHECK (novelty_score >= 0.0),
    priority_score DECIMAL NOT NULL DEFAULT 0.0 CHECK (priority_score >= 0.0),
    synergy_index DECIMAL NOT NULL DEFAULT 0.0 CHECK (synergy_index >= 0.0),
    measured_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Activation propagation logs table
CREATE TABLE activation_logs (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    session_id UUID NOT NULL,
    hypernode_id UUID NOT NULL REFERENCES echoself_hypernodes(id) ON DELETE CASCADE,
    initial_activation DECIMAL NOT NULL,
    final_activation DECIMAL NOT NULL,
    propagation_step INTEGER NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Create indexes for performance
CREATE INDEX idx_echoself_hypernodes_role ON echoself_hypernodes(current_role);
CREATE INDEX idx_echoself_hypernodes_activation ON echoself_hypernodes(activation_level);
CREATE INDEX idx_echoself_hypernodes_created_at ON echoself_hypernodes(created_at);

CREATE INDEX idx_memory_fragments_hypernode ON memory_fragments(hypernode_id);
CREATE INDEX idx_memory_fragments_type ON memory_fragments(memory_type);
CREATE INDEX idx_memory_fragments_activation ON memory_fragments(activation_level);
CREATE INDEX idx_memory_fragments_accessed ON memory_fragments(last_accessed);

CREATE INDEX idx_echoself_hyperedges_source_nodes ON echoself_hyperedges USING GIN(source_node_ids);
CREATE INDEX idx_echoself_hyperedges_target_nodes ON echoself_hyperedges USING GIN(target_node_ids);
CREATE INDEX idx_echoself_hyperedges_type ON echoself_hyperedges(edge_type);
CREATE INDEX idx_echoself_hyperedges_weight ON echoself_hyperedges(weight);

CREATE INDEX idx_pattern_language_oeis ON pattern_language(oeis_number);
CREATE INDEX idx_pattern_language_status ON pattern_language(implementation_status);

CREATE INDEX idx_synergy_metrics_hypernode ON synergy_metrics(hypernode_id);
CREATE INDEX idx_synergy_metrics_measured_at ON synergy_metrics(measured_at);

CREATE INDEX idx_activation_logs_session ON activation_logs(session_id);
CREATE INDEX idx_activation_logs_hypernode ON activation_logs(hypernode_id);
CREATE INDEX idx_activation_logs_step ON activation_logs(propagation_step);

-- Create function to update updated_at timestamp
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language 'plpgsql';

-- Create trigger for echoself_hypernodes
CREATE TRIGGER update_echoself_hypernodes_updated_at 
    BEFORE UPDATE ON echoself_hypernodes 
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

-- Create function to calculate synergy index
CREATE OR REPLACE FUNCTION calculate_synergy_index(novelty DECIMAL, priority DECIMAL)
RETURNS DECIMAL AS $$
BEGIN
    IF (novelty + priority) = 0 THEN
        RETURN 0.0;
    END IF;
    RETURN (2.0 * novelty * priority) / (novelty + priority);
END;
$$ LANGUAGE plpgsql;

-- Insert Christopher Alexander pattern language mappings
INSERT INTO pattern_language (oeis_number, pattern_name, pattern_description, implementation_status) VALUES
(1, 'Unity Pattern', 'The foundational single element', 'active'),
(2, 'Duality Pattern', 'Binary distinction and relationship', 'active'),
(3, 'Trinity Pattern', 'Three-way interaction and synthesis', 'active'),
(5, 'Quintessence Pattern', 'Five-fold symmetry and balance', 'pending'),
(8, 'Octave Pattern', 'Eight-fold completeness and cycles', 'pending'),
(13, 'Fibonacci Pattern', 'Natural growth and proportion', 'pending'),
(21, 'Integration Pattern', 'Complex system integration', 'pending'),
(34, 'Emergence Pattern', 'Emergent properties and behaviors', 'pending'),
(55, 'Resonance Pattern', 'Harmonic resonance and synchronization', 'pending'),
(89, 'Complexity Pattern', 'Complex adaptive system dynamics', 'pending'),
(144, 'Transformation Pattern', 'Large-scale system transformation', 'pending'),
(253, 'Core Alexander Pattern', 'Fundamental architectural principle', 'active'),
(286, 'Complete Pattern Set', 'Full regional transformations', 'pending'),
(719, 'Axis Mundi', 'Recursive thought process and central organizing principle', 'active');

-- Create view for active hypergraph state
CREATE VIEW active_hypergraph_state AS
SELECT 
    h.id,
    h.identity_seed,
    h.current_role,
    h.activation_level,
    array_length(h.entropy_trace, 1) as entropy_history_length,
    CASE 
        WHEN array_length(h.entropy_trace, 1) > 0 
        THEN h.entropy_trace[array_length(h.entropy_trace, 1)]
        ELSE 0.0 
    END as current_entropy,
    COUNT(mf.id) as memory_fragment_count,
    COUNT(DISTINCT he_source.id) + COUNT(DISTINCT he_target.id) as connected_edges,
    h.created_at,
    h.updated_at
FROM echoself_hypernodes h
LEFT JOIN memory_fragments mf ON h.id = mf.hypernode_id
LEFT JOIN echoself_hyperedges he_source ON h.id = ANY(he_source.source_node_ids)
LEFT JOIN echoself_hyperedges he_target ON h.id = ANY(he_target.target_node_ids)
WHERE h.activation_level > 0.1
GROUP BY h.id, h.identity_seed, h.current_role, h.activation_level, h.entropy_trace, h.created_at, h.updated_at
ORDER BY h.activation_level DESC, h.updated_at DESC;

