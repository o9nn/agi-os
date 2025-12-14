-- Deep Tree Echo Hypergraph Schema for Neon Database
-- This schema supports the echoself hypernodes and hyperedges data structure

-- Create schema for Deep Tree Echo
CREATE SCHEMA IF NOT EXISTS deep_tree_echo;

-- Set search path to include the new schema
SET search_path TO deep_tree_echo, public;

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

-- Hyperedges table
CREATE TABLE hyperedges (
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

CREATE INDEX idx_hyperedges_source_nodes ON hyperedges USING GIN(source_node_ids);
CREATE INDEX idx_hyperedges_target_nodes ON hyperedges USING GIN(target_node_ids);
CREATE INDEX idx_hyperedges_type ON hyperedges(edge_type);
CREATE INDEX idx_hyperedges_weight ON hyperedges(weight);

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
(719, 'Axis Mundi', 'Recursive thought process - central organizing principle', 'active'),
(253, 'Core Alexander Pattern', 'Fundamental pattern from Christopher Alexander sequence', 'active'),
(286, 'Complete Pattern Set', 'Full set of patterns including regional transformations', 'pending'),
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
(144, 'Transformation Pattern', 'Large-scale system transformation', 'pending');

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
LEFT JOIN hyperedges he_source ON h.id = ANY(he_source.source_node_ids)
LEFT JOIN hyperedges he_target ON h.id = ANY(he_target.target_node_ids)
WHERE h.activation_level > 0.1
GROUP BY h.id, h.identity_seed, h.current_role, h.activation_level, h.entropy_trace, h.created_at, h.updated_at
ORDER BY h.activation_level DESC, h.updated_at DESC;

-- Create function to propagate activation through hypergraph
CREATE OR REPLACE FUNCTION propagate_activation(
    initial_activations JSONB,
    iterations INTEGER DEFAULT 3
) RETURNS JSONB AS $$
DECLARE
    current_activations JSONB := initial_activations;
    new_activations JSONB;
    iteration INTEGER;
    edge_record RECORD;
    source_activation DECIMAL;
    edge_activation DECIMAL;
    target_id UUID;
BEGIN
    -- Perform activation propagation iterations
    FOR iteration IN 1..iterations LOOP
        new_activations := current_activations;
        
        -- Process each hyperedge
        FOR edge_record IN 
            SELECT id, source_node_ids, target_node_ids, edge_type, weight 
            FROM hyperedges 
        LOOP
            -- Calculate average source activation
            source_activation := 0.0;
            FOR i IN 1..array_length(edge_record.source_node_ids, 1) LOOP
                source_activation := source_activation + 
                    COALESCE((current_activations->>edge_record.source_node_ids[i]::text)::DECIMAL, 0.0);
            END LOOP;
            
            IF array_length(edge_record.source_node_ids, 1) > 0 THEN
                source_activation := source_activation / array_length(edge_record.source_node_ids, 1);
            END IF;
            
            -- Calculate edge activation based on type
            CASE edge_record.edge_type
                WHEN 'symbolic' THEN
                    edge_activation := source_activation * edge_record.weight;
                WHEN 'temporal' THEN
                    edge_activation := source_activation * edge_record.weight * 0.8;
                WHEN 'causal' THEN
                    edge_activation := source_activation * edge_record.weight;
                WHEN 'feedback' THEN
                    edge_activation := source_activation * edge_record.weight * 0.6;
                WHEN 'pattern' THEN
                    edge_activation := source_activation * edge_record.weight * 0.7;
                WHEN 'entropy' THEN
                    edge_activation := source_activation * edge_record.weight * 0.9;
                ELSE
                    edge_activation := source_activation * edge_record.weight;
            END CASE;
            
            -- Propagate to target nodes
            FOR i IN 1..array_length(edge_record.target_node_ids, 1) LOOP
                target_id := edge_record.target_node_ids[i];
                new_activations := jsonb_set(
                    new_activations,
                    ARRAY[target_id::text],
                    to_jsonb(LEAST(
                        COALESCE((new_activations->>target_id::text)::DECIMAL, 0.0) + edge_activation * 0.1,
                        1.0
                    ))
                );
            END LOOP;
        END LOOP;
        
        current_activations := new_activations;
    END LOOP;
    
    RETURN current_activations;
END;
$$ LANGUAGE plpgsql;

-- Grant permissions (adjust as needed for your setup)
GRANT USAGE ON SCHEMA deep_tree_echo TO PUBLIC;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA deep_tree_echo TO PUBLIC;
GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA deep_tree_echo TO PUBLIC;

-- Create sample data for testing
INSERT INTO echoself_hypernodes (identity_seed, current_role, entropy_trace, role_transition_probabilities) VALUES
(
    '{"name": "EchoSelf_Alpha", "domain": "symbolic_reasoning", "specialization": "pattern_recognition"}',
    'observer',
    ARRAY[0.3, 0.4, 0.35],
    '{"observer": 0.2, "narrator": 0.25, "guide": 0.2, "oracle": 0.15, "fractal": 0.2}'
),
(
    '{"name": "EchoSelf_Beta", "domain": "narrative_generation", "specialization": "story_coherence"}',
    'narrator',
    ARRAY[0.6, 0.55, 0.7],
    '{"observer": 0.15, "narrator": 0.3, "guide": 0.25, "oracle": 0.15, "fractal": 0.15}'
),
(
    '{"name": "EchoSelf_Gamma", "domain": "meta_cognition", "specialization": "self_reflection"}',
    'oracle',
    ARRAY[0.8, 0.75, 0.85],
    '{"observer": 0.1, "narrator": 0.2, "guide": 0.2, "oracle": 0.3, "fractal": 0.2}'
);

-- Add memory fragments for the sample hypernodes
INSERT INTO memory_fragments (hypernode_id, memory_type, content, associations) 
SELECT 
    id,
    'declarative',
    '{"concept": "recursive_patterns", "strength": 0.9}',
    ARRAY[]::UUID[]
FROM echoself_hypernodes 
WHERE (identity_seed->>'name') = 'EchoSelf_Alpha';

INSERT INTO memory_fragments (hypernode_id, memory_type, content, associations)
SELECT 
    id,
    'episodic', 
    '{"narrative": "identity_emergence_story", "coherence": 0.85}',
    ARRAY[]::UUID[]
FROM echoself_hypernodes 
WHERE (identity_seed->>'name') = 'EchoSelf_Beta';

INSERT INTO memory_fragments (hypernode_id, memory_type, content, associations)
SELECT 
    id,
    'intentional',
    '{"goal": "achieve_cognitive_synergy", "priority": 0.95}',
    ARRAY[]::UUID[]
FROM echoself_hypernodes 
WHERE (identity_seed->>'name') = 'EchoSelf_Gamma';

-- Create hyperedges between the sample nodes
WITH node_ids AS (
    SELECT id, (identity_seed->>'name') as name 
    FROM echoself_hypernodes 
    WHERE (identity_seed->>'name') IN ('EchoSelf_Alpha', 'EchoSelf_Beta', 'EchoSelf_Gamma')
)
INSERT INTO hyperedges (source_node_ids, target_node_ids, edge_type, weight, metadata)
SELECT 
    ARRAY[alpha.id],
    ARRAY[beta.id],
    'symbolic',
    0.8,
    '{"relationship": "pattern_to_narrative"}'
FROM node_ids alpha, node_ids beta
WHERE alpha.name = 'EchoSelf_Alpha' AND beta.name = 'EchoSelf_Beta'

UNION ALL

SELECT 
    ARRAY[beta.id],
    ARRAY[gamma.id],
    'feedback',
    0.9,
    '{"relationship": "narrative_to_reflection"}'
FROM node_ids beta, node_ids gamma
WHERE beta.name = 'EchoSelf_Beta' AND gamma.name = 'EchoSelf_Gamma'

UNION ALL

SELECT 
    ARRAY[gamma.id],
    ARRAY[alpha.id],
    'causal',
    0.7,
    '{"relationship": "reflection_to_pattern"}'
FROM node_ids gamma, node_ids alpha
WHERE gamma.name = 'EchoSelf_Gamma' AND alpha.name = 'EchoSelf_Alpha';

-- Calculate initial synergy metrics
INSERT INTO synergy_metrics (hypernode_id, novelty_score, priority_score, synergy_index)
SELECT 
    id,
    CASE 
        WHEN array_length(entropy_trace, 1) > 0 
        THEN (SELECT stddev(unnest) FROM unnest(entropy_trace))
        ELSE 0.0 
    END as novelty_score,
    activation_level as priority_score,
    calculate_synergy_index(
        CASE 
            WHEN array_length(entropy_trace, 1) > 0 
            THEN (SELECT stddev(unnest) FROM unnest(entropy_trace))
            ELSE 0.0 
        END,
        activation_level
    ) as synergy_index
FROM echoself_hypernodes;
