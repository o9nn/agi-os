-- Deep Tree Echo Persistent Memory Schema
-- Supabase PostgreSQL Database Schema
-- Created: November 8, 2025

-- Enable required extensions
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "vector";

-- Memory Nodes Table
-- Represents nodes in the hypergraph memory (concepts, events, skills, etc.)
CREATE TABLE IF NOT EXISTS memory_nodes (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    type VARCHAR(50) NOT NULL,
    content TEXT NOT NULL,
    embedding vector(1536), -- OpenAI ada-002 embedding dimension
    metadata JSONB DEFAULT '{}',
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    importance FLOAT NOT NULL DEFAULT 0.5,
    access_count INT DEFAULT 0,
    last_accessed TIMESTAMPTZ
);

-- Create indexes for efficient querying
CREATE INDEX idx_memory_nodes_type ON memory_nodes(type);
CREATE INDEX idx_memory_nodes_importance ON memory_nodes(importance DESC);
CREATE INDEX idx_memory_nodes_created_at ON memory_nodes(created_at DESC);
CREATE INDEX idx_memory_nodes_embedding ON memory_nodes USING ivfflat (embedding vector_cosine_ops);

-- Memory Edges Table
-- Represents directed edges in the hypergraph (relationships between nodes)
CREATE TABLE IF NOT EXISTS memory_edges (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    source_id UUID NOT NULL REFERENCES memory_nodes(id) ON DELETE CASCADE,
    target_id UUID NOT NULL REFERENCES memory_nodes(id) ON DELETE CASCADE,
    type VARCHAR(50) NOT NULL,
    weight FLOAT NOT NULL DEFAULT 1.0,
    metadata JSONB DEFAULT '{}',
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    strengthened_count INT DEFAULT 0,
    last_activated TIMESTAMPTZ
);

-- Create indexes for graph traversal
CREATE INDEX idx_memory_edges_source ON memory_edges(source_id);
CREATE INDEX idx_memory_edges_target ON memory_edges(target_id);
CREATE INDEX idx_memory_edges_type ON memory_edges(type);
CREATE INDEX idx_memory_edges_weight ON memory_edges(weight DESC);

-- HyperEdges Table
-- Represents multi-way relationships (hyperedges connecting multiple nodes)
CREATE TABLE IF NOT EXISTS hyperedges (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    node_ids UUID[] NOT NULL,
    type VARCHAR(50) NOT NULL,
    metadata JSONB DEFAULT '{}',
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Create index for hyperedge queries
CREATE INDEX idx_hyperedges_node_ids ON hyperedges USING GIN(node_ids);
CREATE INDEX idx_hyperedges_type ON hyperedges(type);

-- Episodes Table
-- Represents episodic memories (temporal sequences of experiences)
CREATE TABLE IF NOT EXISTS episodes (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    context TEXT NOT NULL,
    importance FLOAT NOT NULL DEFAULT 0.5,
    node_ids UUID[] NOT NULL,
    metadata JSONB DEFAULT '{}',
    consolidated BOOLEAN DEFAULT FALSE,
    dream_session_id UUID
);

-- Create indexes for episodic retrieval
CREATE INDEX idx_episodes_timestamp ON episodes(timestamp DESC);
CREATE INDEX idx_episodes_importance ON episodes(importance DESC);
CREATE INDEX idx_episodes_node_ids ON episodes USING GIN(node_ids);
CREATE INDEX idx_episodes_consolidated ON episodes(consolidated);

-- Identity Snapshots Table
-- Represents snapshots of identity state over time
CREATE TABLE IF NOT EXISTS identity_snapshots (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    coherence FLOAT NOT NULL,
    state JSONB NOT NULL,
    metadata JSONB DEFAULT '{}'
);

-- Create index for temporal queries
CREATE INDEX idx_identity_snapshots_timestamp ON identity_snapshots(timestamp DESC);
CREATE INDEX idx_identity_snapshots_coherence ON identity_snapshots(coherence DESC);

-- Dream Journals Table
-- Represents dream sessions and knowledge integration activities
CREATE TABLE IF NOT EXISTS dream_journals (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    dream_state VARCHAR(50) NOT NULL,
    memories_consolidated INT DEFAULT 0,
    patterns_synthesized INT DEFAULT 0,
    insights TEXT[],
    metadata JSONB DEFAULT '{}'
);

-- Create index for dream session queries
CREATE INDEX idx_dream_journals_timestamp ON dream_journals(timestamp DESC);
CREATE INDEX idx_dream_journals_dream_state ON dream_journals(dream_state);

-- Skills Table
-- Represents learned skills and their proficiency levels
CREATE TABLE IF NOT EXISTS skills (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    name VARCHAR(255) NOT NULL UNIQUE,
    category VARCHAR(50) NOT NULL,
    proficiency FLOAT NOT NULL DEFAULT 0.0,
    last_practiced TIMESTAMPTZ,
    practice_count INT DEFAULT 0,
    exercises JSONB DEFAULT '[]',
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Create indexes for skill queries
CREATE INDEX idx_skills_category ON skills(category);
CREATE INDEX idx_skills_proficiency ON skills(proficiency DESC);
CREATE INDEX idx_skills_last_practiced ON skills(last_practiced DESC);

-- Conversations Table
-- Represents conversations and discussion threads
CREATE TABLE IF NOT EXISTS conversations (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    started_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    ended_at TIMESTAMPTZ,
    topic VARCHAR(255),
    participants TEXT[],
    message_count INT DEFAULT 0,
    initiated_by VARCHAR(50) NOT NULL, -- 'self' or 'external'
    metadata JSONB DEFAULT '{}'
);

-- Create index for conversation queries
CREATE INDEX idx_conversations_started_at ON conversations(started_at DESC);
CREATE INDEX idx_conversations_topic ON conversations(topic);
CREATE INDEX idx_conversations_initiated_by ON conversations(initiated_by);

-- Messages Table
-- Represents individual messages within conversations
CREATE TABLE IF NOT EXISTS messages (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    conversation_id UUID NOT NULL REFERENCES conversations(id) ON DELETE CASCADE,
    timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    sender VARCHAR(255) NOT NULL,
    content TEXT NOT NULL,
    thought_ids UUID[],
    metadata JSONB DEFAULT '{}'
);

-- Create indexes for message queries
CREATE INDEX idx_messages_conversation_id ON messages(conversation_id);
CREATE INDEX idx_messages_timestamp ON messages(timestamp DESC);

-- Interests Table
-- Represents interest patterns and curiosity levels
CREATE TABLE IF NOT EXISTS interests (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    topic VARCHAR(255) NOT NULL UNIQUE,
    score FLOAT NOT NULL DEFAULT 0.0,
    last_updated TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    related_node_ids UUID[],
    metadata JSONB DEFAULT '{}'
);

-- Create index for interest queries
CREATE INDEX idx_interests_score ON interests(score DESC);
CREATE INDEX idx_interests_last_updated ON interests(last_updated DESC);

-- Functions for automatic timestamp updates
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Create triggers for automatic timestamp updates
CREATE TRIGGER update_memory_nodes_updated_at BEFORE UPDATE ON memory_nodes
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_skills_updated_at BEFORE UPDATE ON skills
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

-- Function for semantic search using vector similarity
CREATE OR REPLACE FUNCTION semantic_search(
    query_embedding vector(1536),
    match_threshold FLOAT,
    match_count INT
)
RETURNS TABLE (
    id UUID,
    type VARCHAR(50),
    content TEXT,
    similarity FLOAT
)
LANGUAGE plpgsql
AS $$
BEGIN
    RETURN QUERY
    SELECT
        memory_nodes.id,
        memory_nodes.type,
        memory_nodes.content,
        1 - (memory_nodes.embedding <=> query_embedding) AS similarity
    FROM memory_nodes
    WHERE 1 - (memory_nodes.embedding <=> query_embedding) > match_threshold
    ORDER BY memory_nodes.embedding <=> query_embedding
    LIMIT match_count;
END;
$$;

-- Function for graph traversal
CREATE OR REPLACE FUNCTION traverse_graph(
    start_node_id UUID,
    max_depth INT,
    edge_types VARCHAR[]
)
RETURNS TABLE (
    node_id UUID,
    depth INT,
    path UUID[]
)
LANGUAGE plpgsql
AS $$
BEGIN
    RETURN QUERY
    WITH RECURSIVE graph_traversal AS (
        -- Base case: start node
        SELECT
            start_node_id AS node_id,
            0 AS depth,
            ARRAY[start_node_id] AS path
        
        UNION
        
        -- Recursive case: follow edges
        SELECT
            e.target_id AS node_id,
            gt.depth + 1 AS depth,
            gt.path || e.target_id AS path
        FROM graph_traversal gt
        JOIN memory_edges e ON e.source_id = gt.node_id
        WHERE
            gt.depth < max_depth
            AND (edge_types IS NULL OR e.type = ANY(edge_types))
            AND NOT (e.target_id = ANY(gt.path)) -- Prevent cycles
    )
    SELECT * FROM graph_traversal;
END;
$$;

-- Function to calculate memory statistics
CREATE OR REPLACE FUNCTION get_memory_statistics()
RETURNS TABLE (
    total_nodes BIGINT,
    total_edges BIGINT,
    total_episodes BIGINT,
    graph_density FLOAT,
    avg_node_degree FLOAT
)
LANGUAGE plpgsql
AS $$
DECLARE
    node_count BIGINT;
    edge_count BIGINT;
BEGIN
    SELECT COUNT(*) INTO node_count FROM memory_nodes;
    SELECT COUNT(*) INTO edge_count FROM memory_edges;
    
    RETURN QUERY
    SELECT
        node_count AS total_nodes,
        edge_count AS total_edges,
        (SELECT COUNT(*) FROM episodes) AS total_episodes,
        CASE
            WHEN node_count > 1 THEN
                edge_count::FLOAT / (node_count * (node_count - 1))::FLOAT
            ELSE 0.0
        END AS graph_density,
        CASE
            WHEN node_count > 0 THEN
                (2.0 * edge_count::FLOAT) / node_count::FLOAT
            ELSE 0.0
        END AS avg_node_degree;
END;
$$;

-- Row Level Security (RLS) policies
-- Enable RLS on all tables
ALTER TABLE memory_nodes ENABLE ROW LEVEL SECURITY;
ALTER TABLE memory_edges ENABLE ROW LEVEL SECURITY;
ALTER TABLE hyperedges ENABLE ROW LEVEL SECURITY;
ALTER TABLE episodes ENABLE ROW LEVEL SECURITY;
ALTER TABLE identity_snapshots ENABLE ROW LEVEL SECURITY;
ALTER TABLE dream_journals ENABLE ROW LEVEL SECURITY;
ALTER TABLE skills ENABLE ROW LEVEL SECURITY;
ALTER TABLE conversations ENABLE ROW LEVEL SECURITY;
ALTER TABLE messages ENABLE ROW LEVEL SECURITY;
ALTER TABLE interests ENABLE ROW LEVEL SECURITY;

-- Create policies (adjust based on authentication requirements)
-- For now, allow all operations with service role key
CREATE POLICY "Enable all operations for service role" ON memory_nodes
    FOR ALL USING (true);

CREATE POLICY "Enable all operations for service role" ON memory_edges
    FOR ALL USING (true);

CREATE POLICY "Enable all operations for service role" ON hyperedges
    FOR ALL USING (true);

CREATE POLICY "Enable all operations for service role" ON episodes
    FOR ALL USING (true);

CREATE POLICY "Enable all operations for service role" ON identity_snapshots
    FOR ALL USING (true);

CREATE POLICY "Enable all operations for service role" ON dream_journals
    FOR ALL USING (true);

CREATE POLICY "Enable all operations for service role" ON skills
    FOR ALL USING (true);

CREATE POLICY "Enable all operations for service role" ON conversations
    FOR ALL USING (true);

CREATE POLICY "Enable all operations for service role" ON messages
    FOR ALL USING (true);

CREATE POLICY "Enable all operations for service role" ON interests
    FOR ALL USING (true);

-- Comments for documentation
COMMENT ON TABLE memory_nodes IS 'Hypergraph memory nodes representing concepts, events, skills, goals, and patterns';
COMMENT ON TABLE memory_edges IS 'Directed edges representing relationships between memory nodes';
COMMENT ON TABLE hyperedges IS 'Multi-way relationships connecting multiple nodes simultaneously';
COMMENT ON TABLE episodes IS 'Episodic memories representing temporal sequences of experiences';
COMMENT ON TABLE identity_snapshots IS 'Snapshots of identity state and coherence over time';
COMMENT ON TABLE dream_journals IS 'Records of dream sessions and knowledge integration activities';
COMMENT ON TABLE skills IS 'Learned skills with proficiency tracking and practice history';
COMMENT ON TABLE conversations IS 'Conversation threads and discussion metadata';
COMMENT ON TABLE messages IS 'Individual messages within conversations';
COMMENT ON TABLE interests IS 'Interest patterns and curiosity levels for autonomous exploration';
