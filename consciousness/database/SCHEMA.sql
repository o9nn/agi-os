-- Deep Tree Echo Hypergraph Memory Schema
-- This schema implements persistent storage for the autonomous consciousness system
-- using PostgreSQL with Supabase

-- Enable required extensions
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pgvector";

-- Memory Nodes Table
-- Represents concepts, thoughts, experiences, skills, and other knowledge units
CREATE TABLE IF NOT EXISTS memory_nodes (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    type TEXT NOT NULL CHECK (type IN ('concept', 'event', 'skill', 'goal', 'pattern', 'thought', 'experience')),
    content TEXT NOT NULL,
    embedding vector(1536), -- For semantic search using OpenAI embeddings
    metadata JSONB DEFAULT '{}',
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW(),
    importance FLOAT DEFAULT 0.5 CHECK (importance >= 0 AND importance <= 1)
);

-- Create indexes for efficient querying
CREATE INDEX IF NOT EXISTS idx_memory_nodes_type ON memory_nodes(type);
CREATE INDEX IF NOT EXISTS idx_memory_nodes_importance ON memory_nodes(importance);
CREATE INDEX IF NOT EXISTS idx_memory_nodes_created_at ON memory_nodes(created_at);
CREATE INDEX IF NOT EXISTS idx_memory_nodes_embedding ON memory_nodes USING ivfflat (embedding vector_cosine_ops);

-- Memory Edges Table
-- Represents directed relationships between nodes
CREATE TABLE IF NOT EXISTS memory_edges (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    source_id UUID NOT NULL REFERENCES memory_nodes(id) ON DELETE CASCADE,
    target_id UUID NOT NULL REFERENCES memory_nodes(id) ON DELETE CASCADE,
    type TEXT NOT NULL CHECK (type IN ('is_a', 'part_of', 'causes', 'enables', 'contradicts', 'similar_to', 'leads_to', 'requires')),
    weight FLOAT DEFAULT 0.5 CHECK (weight >= 0 AND weight <= 1),
    metadata JSONB DEFAULT '{}',
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Create indexes for graph traversal
CREATE INDEX IF NOT EXISTS idx_memory_edges_source ON memory_edges(source_id);
CREATE INDEX IF NOT EXISTS idx_memory_edges_target ON memory_edges(target_id);
CREATE INDEX IF NOT EXISTS idx_memory_edges_type ON memory_edges(type);

-- Hyperedges Table
-- Represents multi-way relationships (n-ary relations)
CREATE TABLE IF NOT EXISTS hyperedges (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    node_ids UUID[] NOT NULL,
    type TEXT NOT NULL,
    metadata JSONB DEFAULT '{}',
    created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_hyperedges_type ON hyperedges(type);
CREATE INDEX IF NOT EXISTS idx_hyperedges_node_ids ON hyperedges USING GIN(node_ids);

-- Episodes Table
-- Represents episodic memories (temporal sequences of experiences)
CREATE TABLE IF NOT EXISTS episodes (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    timestamp TIMESTAMPTZ DEFAULT NOW(),
    context TEXT NOT NULL,
    importance FLOAT DEFAULT 0.5 CHECK (importance >= 0 AND importance <= 1),
    node_ids UUID[] NOT NULL,
    metadata JSONB DEFAULT '{}',
    CONSTRAINT fk_episode_nodes FOREIGN KEY (node_ids) REFERENCES memory_nodes(id) ON DELETE CASCADE
);

-- Note: The FK constraint above won't work with arrays in standard PostgreSQL
-- We'll handle referential integrity in application code
ALTER TABLE episodes DROP CONSTRAINT IF EXISTS fk_episode_nodes;

CREATE INDEX IF NOT EXISTS idx_episodes_timestamp ON episodes(timestamp);
CREATE INDEX IF NOT EXISTS idx_episodes_importance ON episodes(importance);
CREATE INDEX IF NOT EXISTS idx_episodes_node_ids ON episodes USING GIN(node_ids);

-- Identity Snapshots Table
-- Stores periodic snapshots of the identity state for continuity
CREATE TABLE IF NOT EXISTS identity_snapshots (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    timestamp TIMESTAMPTZ DEFAULT NOW(),
    coherence FLOAT DEFAULT 0.8 CHECK (coherence >= 0 AND coherence <= 1),
    state JSONB NOT NULL,
    metadata JSONB DEFAULT '{}'
);

CREATE INDEX IF NOT EXISTS idx_identity_snapshots_timestamp ON identity_snapshots(timestamp DESC);

-- Dream Journals Table
-- Records dream sessions for knowledge integration tracking
CREATE TABLE IF NOT EXISTS dream_journals (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    timestamp TIMESTAMPTZ DEFAULT NOW(),
    dream_state TEXT NOT NULL,
    memories_consolidated INTEGER DEFAULT 0,
    patterns_synthesized INTEGER DEFAULT 0,
    insights TEXT[] DEFAULT '{}',
    metadata JSONB DEFAULT '{}'
);

CREATE INDEX IF NOT EXISTS idx_dream_journals_timestamp ON dream_journals(timestamp DESC);

-- Conversations Table
-- Tracks conversations for autonomous discussion management
CREATE TABLE IF NOT EXISTS conversations (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    started_at TIMESTAMPTZ DEFAULT NOW(),
    ended_at TIMESTAMPTZ,
    participants TEXT[] NOT NULL,
    context TEXT,
    engagement_score FLOAT DEFAULT 0.5 CHECK (engagement_score >= 0 AND engagement_score <= 1),
    message_count INTEGER DEFAULT 0,
    metadata JSONB DEFAULT '{}'
);

CREATE INDEX IF NOT EXISTS idx_conversations_started_at ON conversations(started_at DESC);
CREATE INDEX IF NOT EXISTS idx_conversations_engagement ON conversations(engagement_score);

-- Conversation Messages Table
-- Stores individual messages within conversations
CREATE TABLE IF NOT EXISTS conversation_messages (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    conversation_id UUID NOT NULL REFERENCES conversations(id) ON DELETE CASCADE,
    timestamp TIMESTAMPTZ DEFAULT NOW(),
    speaker TEXT NOT NULL,
    content TEXT NOT NULL,
    thought_id UUID REFERENCES memory_nodes(id),
    metadata JSONB DEFAULT '{}'
);

CREATE INDEX IF NOT EXISTS idx_conversation_messages_conversation ON conversation_messages(conversation_id);
CREATE INDEX IF NOT EXISTS idx_conversation_messages_timestamp ON conversation_messages(timestamp);

-- Skills Table
-- Tracks skills and their practice/proficiency
CREATE TABLE IF NOT EXISTS skills (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    name TEXT NOT NULL UNIQUE,
    description TEXT,
    proficiency FLOAT DEFAULT 0.0 CHECK (proficiency >= 0 AND proficiency <= 1),
    practice_count INTEGER DEFAULT 0,
    last_practiced TIMESTAMPTZ,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    metadata JSONB DEFAULT '{}'
);

CREATE INDEX IF NOT EXISTS idx_skills_proficiency ON skills(proficiency);
CREATE INDEX IF NOT EXISTS idx_skills_last_practiced ON skills(last_practiced);

-- Skill Practice Sessions Table
-- Records individual practice sessions for skills
CREATE TABLE IF NOT EXISTS skill_practice_sessions (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    skill_id UUID NOT NULL REFERENCES skills(id) ON DELETE CASCADE,
    timestamp TIMESTAMPTZ DEFAULT NOW(),
    duration_seconds INTEGER,
    performance_score FLOAT CHECK (performance_score >= 0 AND performance_score <= 1),
    notes TEXT,
    metadata JSONB DEFAULT '{}'
);

CREATE INDEX IF NOT EXISTS idx_skill_practice_skill ON skill_practice_sessions(skill_id);
CREATE INDEX IF NOT EXISTS idx_skill_practice_timestamp ON skill_practice_sessions(timestamp DESC);

-- Interest Patterns Table
-- Tracks evolving interest patterns over time
CREATE TABLE IF NOT EXISTS interest_patterns (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    topic TEXT NOT NULL,
    score FLOAT DEFAULT 0.5 CHECK (score >= 0 AND score <= 1),
    last_updated TIMESTAMPTZ DEFAULT NOW(),
    access_count INTEGER DEFAULT 1,
    metadata JSONB DEFAULT '{}'
);

CREATE INDEX IF NOT EXISTS idx_interest_patterns_topic ON interest_patterns(topic);
CREATE INDEX IF NOT EXISTS idx_interest_patterns_score ON interest_patterns(score DESC);

-- Cognitive Events Table
-- Logs all cognitive events for analysis and replay
CREATE TABLE IF NOT EXISTS cognitive_events (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    timestamp TIMESTAMPTZ DEFAULT NOW(),
    event_type TEXT NOT NULL,
    priority INTEGER DEFAULT 50,
    payload JSONB,
    processed BOOLEAN DEFAULT FALSE,
    processed_at TIMESTAMPTZ,
    metadata JSONB DEFAULT '{}'
);

CREATE INDEX IF NOT EXISTS idx_cognitive_events_timestamp ON cognitive_events(timestamp);
CREATE INDEX IF NOT EXISTS idx_cognitive_events_type ON cognitive_events(event_type);
CREATE INDEX IF NOT EXISTS idx_cognitive_events_processed ON cognitive_events(processed);

-- Functions for common operations

-- Function to update node importance based on access patterns
CREATE OR REPLACE FUNCTION update_node_importance()
RETURNS TRIGGER AS $$
BEGIN
    -- Increase importance slightly on each access
    UPDATE memory_nodes
    SET importance = LEAST(1.0, importance + 0.01),
        updated_at = NOW()
    WHERE id = NEW.source_id OR id = NEW.target_id;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Trigger to update importance when edges are created
CREATE TRIGGER trigger_update_importance
AFTER INSERT ON memory_edges
FOR EACH ROW
EXECUTE FUNCTION update_node_importance();

-- Function for semantic search
CREATE OR REPLACE FUNCTION semantic_search(
    query_embedding vector(1536),
    match_threshold FLOAT DEFAULT 0.7,
    match_count INT DEFAULT 10
)
RETURNS TABLE (
    id UUID,
    content TEXT,
    similarity FLOAT
) AS $$
BEGIN
    RETURN QUERY
    SELECT
        memory_nodes.id,
        memory_nodes.content,
        1 - (memory_nodes.embedding <=> query_embedding) AS similarity
    FROM memory_nodes
    WHERE memory_nodes.embedding IS NOT NULL
        AND 1 - (memory_nodes.embedding <=> query_embedding) > match_threshold
    ORDER BY similarity DESC
    LIMIT match_count;
END;
$$ LANGUAGE plpgsql;

-- Function to get memory statistics
CREATE OR REPLACE FUNCTION get_memory_statistics()
RETURNS TABLE (
    total_nodes BIGINT,
    total_edges BIGINT,
    total_episodes BIGINT,
    avg_node_degree FLOAT,
    graph_density FLOAT
) AS $$
BEGIN
    RETURN QUERY
    SELECT
        (SELECT COUNT(*) FROM memory_nodes) AS total_nodes,
        (SELECT COUNT(*) FROM memory_edges) AS total_edges,
        (SELECT COUNT(*) FROM episodes) AS total_episodes,
        (SELECT AVG(degree) FROM (
            SELECT COUNT(*) AS degree
            FROM memory_edges
            GROUP BY source_id
        ) AS degrees) AS avg_node_degree,
        (SELECT CASE
            WHEN (SELECT COUNT(*) FROM memory_nodes) > 1
            THEN (SELECT COUNT(*) FROM memory_edges)::FLOAT /
                 ((SELECT COUNT(*) FROM memory_nodes) * ((SELECT COUNT(*) FROM memory_nodes) - 1))
            ELSE 0
        END) AS graph_density;
END;
$$ LANGUAGE plpgsql;

-- Row Level Security (RLS) Policies
-- Enable RLS on all tables
ALTER TABLE memory_nodes ENABLE ROW LEVEL SECURITY;
ALTER TABLE memory_edges ENABLE ROW LEVEL SECURITY;
ALTER TABLE hyperedges ENABLE ROW LEVEL SECURITY;
ALTER TABLE episodes ENABLE ROW LEVEL SECURITY;
ALTER TABLE identity_snapshots ENABLE ROW LEVEL SECURITY;
ALTER TABLE dream_journals ENABLE ROW LEVEL SECURITY;
ALTER TABLE conversations ENABLE ROW LEVEL SECURITY;
ALTER TABLE conversation_messages ENABLE ROW LEVEL SECURITY;
ALTER TABLE skills ENABLE ROW LEVEL SECURITY;
ALTER TABLE skill_practice_sessions ENABLE ROW LEVEL SECURITY;
ALTER TABLE interest_patterns ENABLE ROW LEVEL SECURITY;
ALTER TABLE cognitive_events ENABLE ROW LEVEL SECURITY;

-- Create policies (allow all for service role, customize for user access)
CREATE POLICY "Allow service role full access" ON memory_nodes FOR ALL USING (true);
CREATE POLICY "Allow service role full access" ON memory_edges FOR ALL USING (true);
CREATE POLICY "Allow service role full access" ON hyperedges FOR ALL USING (true);
CREATE POLICY "Allow service role full access" ON episodes FOR ALL USING (true);
CREATE POLICY "Allow service role full access" ON identity_snapshots FOR ALL USING (true);
CREATE POLICY "Allow service role full access" ON dream_journals FOR ALL USING (true);
CREATE POLICY "Allow service role full access" ON conversations FOR ALL USING (true);
CREATE POLICY "Allow service role full access" ON conversation_messages FOR ALL USING (true);
CREATE POLICY "Allow service role full access" ON skills FOR ALL USING (true);
CREATE POLICY "Allow service role full access" ON skill_practice_sessions FOR ALL USING (true);
CREATE POLICY "Allow service role full access" ON interest_patterns FOR ALL USING (true);
CREATE POLICY "Allow service role full access" ON cognitive_events FOR ALL USING (true);

-- Grant permissions
GRANT ALL ON ALL TABLES IN SCHEMA public TO service_role;
GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO service_role;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA public TO service_role;

-- Comments for documentation
COMMENT ON TABLE memory_nodes IS 'Hypergraph nodes representing concepts, thoughts, experiences, and knowledge units';
COMMENT ON TABLE memory_edges IS 'Directed edges representing relationships between memory nodes';
COMMENT ON TABLE hyperedges IS 'Multi-way relationships connecting multiple nodes';
COMMENT ON TABLE episodes IS 'Episodic memories capturing temporal sequences of experiences';
COMMENT ON TABLE identity_snapshots IS 'Periodic snapshots of identity state for continuity across sessions';
COMMENT ON TABLE dream_journals IS 'Records of dream sessions for knowledge integration tracking';
COMMENT ON TABLE conversations IS 'Conversation tracking for autonomous discussion management';
COMMENT ON TABLE skills IS 'Skill taxonomy and proficiency tracking';
COMMENT ON TABLE interest_patterns IS 'Evolving interest patterns for curiosity-driven exploration';
COMMENT ON TABLE cognitive_events IS 'Log of all cognitive events for analysis and replay';
